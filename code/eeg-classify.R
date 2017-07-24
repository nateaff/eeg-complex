rm(list = ls())
# library(ggplot2)
library(dplyr)
library(ecomplex)
library(randomForest)
library(tssegment) # segmenetation
library(stargazer) # tables

#----------------------------------------------------------
# 1. Load derived features
# 2. Plot ecomplexity coefficients for each channel
# 3. Get combined complexity coefficients
# 4. Run palarm on combined coefficients A, B
# 4. Segment derived features based on the palarm 
#    features  
# 5. Classify on means of segments with 5-fold cross validation
#----------------------------------------------------------
prefix    = "eeg"
set.seed(2017)
chs       = 1:6
segch     = 4
from_cache = FALSE
window    = 240
len = window/2

# Derived features. 30 Trials : 6 channels : 15 features
feature_df <-readRDS(cache_file("mod_all_features", prefix))
# Load meta data 
trial_df <- readRDS(cache_file("mod_trial_segments", "eeg"))
# tdf <- trial_df[trial_df$window == window, ]
df <- do.call(rbind, feature_df) %>% 
      cbind(., trial_df) %>% 
      dplyr::filter(., window == 240)

df$trial <- 1:26


#----------------------------------------------------------
# Run cross validation segmenting on a single feature and 
# channel.
#----------------------------------------------------------
ch = 1
colnums = c(5:9,  13:18)
# Split on a feature
means_df <- segment_on_cols(df, ch, 
                          cols = c("ecomp.cspline_B","ecomp.cspline_A"), 
                          new_vec = F)
res <- run_cv(means_df, df$response, ch, colnums, from_cache=TRUE)
save_cache(res, "cv_results_ch_1", prefix)

# Split using a numeric vector instead of a column
means_df <- segment_on_cols(df, ch, vec = (1:120),
                          # cols = c("ecomp.cspline_B","ecomp.cspline_A"), 
                          new_vec = TRUE, m = 10)

res <- run_cv(means_df, df$response, ch, colnums, from_cache=TRUE)

#----------------------------------------------------------
# Run non bandpower features on each channel 50 times 
# and average accuracy
#----------------------------------------------------------
run_on_feat <- function(df_, ch, feat, m = 5, byname = TRUE){
  colnums = c(5:9, 10:11, 13:18)
  if(byname){
      means_df <- segment_on_cols(df_, ch, cols = feat, m = m)
    } else {
      means_df <- segment_on_cols(df_, ch, vec = feat, new_vec = TRUE, m = m) 
    }  
  cat("Number of segments: ", dim(means_df)[1], "\n")
  cat("Using channel: ", ch, "\n")
  cm <- run_cv(means_df, df$response, ch, colnums, TRUE)
}

# For a given feature compute replications on each channel
rep_feats <- function(df_, feat, m, reps = 50, byname = TRUE){
  ret <- list()
    for(ch in 1:6){
      res <- replicate(reps, run_on_feat(df_, ch, feat, m, byname = byname))
       ret[[ch]] <- do.call(rbind, res[1,])
  }
  ret
}

# Params 
#----------------------------------------------------------
colnums = c(5:9, 13:18) 
reps = 50
#----------------------------------------------------------
if(!from_cache){
  feats <- list("ecomp.cspline_A", 
                "ecomp.cspline_B", 
               c("ecomp.cspline_A", "ecomp.cspline_B"))
  # res <- run_on_feat(df, 1, feats[[1]], 3)
  # feat_one(df, feats, byname = TRUE)
  resAB = list() 
  for(k in 1:3){
    resAB[[k]] <- rep_feats(df, feats[[k]], 3, reps = 10, byname = TRUE)
  }

  ms = c(10,5,1)
  feats <- list((1:120), (1:120), (1:120)) 
  res_vec = list()
  for(k in 1:3){
    m = ms[k]
    res_vec[[k]] <- rep_feats(df, feats[[k]], m, reps = 10, byname = FALSE)
  }
save_cache(res_vec, "res_vec", prefix)
save_cache(resAB, "resAB", prefix)
}

res_vec <- readRDS(cache_file("res_vec", prefix))
resAB <- readRDS(cache_file("resAB", prefix)) 
#----------------------------------------------------------
# Run stats ... not used
#----------------------------------------------------------
# Compute statistic on a list of 
dflist_stat <- function(df_, stat, col = 2){
  lapply(df_, function(x) apply(x, col, stat)) %>% 
  do.call(rbind, .) %>% data.frame
}


resAB <- resAB[1:3]
ABmeans <- lapply(resAB, function(dfs) dflist_stat(dfs, mean)) %>%
           lapply(., function(x) do.call(rbind, x)) %>% 
           lapply(., data.frame)


vecmeans <- lapply(res_vec, function(dfs) dflist_stat(dfs, mean)) %>%
           lapply(., function(x) do.call(rbind, x)) %>% 
           lapply(., data.frame)

save_cache(vecmeans, "vecmeans", prefix)
save_cache(ABmeans, "ABmeans", prefix)



ABacc <- ABmeans %>% lapply(., function(x) x["Balanced.Accuracy", ]) %>% 
            bind_rows(.) %>% 
            data.frame
vecacc <- vecmeans %>% lapply(., function(x) x["Balanced.Accuracy", ]) %>% 
            bind_rows(.) %>% 
            data.frame
combacc <- rbind(ABacc, vecacc)
names(combacc) <- paste0("CH", 1:6)
methods <- c("A", "B", "A+B", "8", "15", "30")
row.names(combacc) <- methods
combacc <- apply(combacc, 2, round, digits = 2)


ABsp <- ABmeans %>% lapply(., function(x) x["Specificity", ]) %>% 
            bind_rows(.) %>% 
            data.frame
vecsp <- vecmeans %>% lapply(., function(x) x["Specificity", ]) %>% 
            bind_rows(.) %>% 
            data.frame
combsp <- rbind(ABsp, vecsp)
names(combsp) <- paste0("CH", 1:6)
methods <- c("A", "B", "A+B", "8", "15", "30")
row.names(combsp) <- methods
combsp <- apply(combsp, 2, round, digits = 2)

 
#----------------------------------------------------------
# Compute s. deviation of specificity and balanced 
# accuracy
#----------------------------------------------------------
parens <- function(x)  paste0("(", format(unlist(x)),")")
ABsd <- lapply(resAB, function(dfs) dflist_stat(dfs, sd)) %>%
           lapply(., function(x) do.call(rbind, x)) %>% 
           lapply(., data.frame)


vecsd  <- lapply(res_vec, function(dfs) dflist_stat(dfs, sd)) %>%
           lapply(., function(x) do.call(rbind, x)) %>% 
           lapply(., data.frame)


ABacc_sd <- ABsd %>% lapply(., function(x) x["Balanced.Accuracy", ]) %>% 
            bind_rows(.) %>% 
            data.frame
vecacc_sd <- vecsd %>% lapply(., function(x) x["Balanced.Accuracy", ]) %>% 
            bind_rows(.) %>% 
            data.frame



combacc_sd <- rbind(ABacc_sd, vecacc_sd)
names(combacc_sd) <- paste0("(SD", 1:6, ")")
methods <- c("A", "B", "A+B", "8", "15", "30")
row.names(combacc_sd) <- methods
combacc_sd <- combacc_sd %>% apply(., 2, round, digits = 2) %>% 
             apply(., 2, parens)
combacc_sd

 
#----------------------------------------------------------

ABsp_sd <- ABsd %>% lapply(., function(x) x["Specificity", ]) %>% 
            bind_rows(.) %>% 
            data.frame

vecsp_sd <- vecsd %>% lapply(., function(x) x["Specificity", ]) %>% 
            bind_rows(.) %>% 
            data.frame

combsp_sd <- rbind(ABsp_sd, vecsp_sd)
names(combsp_sd) <- paste0("(SD", 1:6, ")")
methods <- c("A", "B", "A+B", "8", "15", "30")
row.names(combsp_sd) <- methods
combsp_sd <- apply(combsp_sd, 2, round, digits = 2) %>% 
             apply(., 2, parens)

combsp_sd

# combine 

spdf <- cbind(combsp, combsp_sd)
spdf <- spdf[ , c(1,7,2,8,3,9,4,10,5,11,6,12)]
# interlaev

accdf <- cbind(combacc, combacc_sd)
accdf <- accdf[ , c(1,7,2,8,3,9,4,10,5,11,6,12)]

stargazer(spdf, summary = FALSE, label = "fig:all-specificity", 
                 title = "Mean and s.d. of specificity for all models.")

stargazer(accdf, summary = FALSE, label = "fig:all-accuracy", 
                 title = "Mean and s.d. of balanced accuacy for all models.")


save_cache(spdf, "specificity_df", prefix)
save_cache(accdf, "accuracy_df", prefix)


save_cache(combsp, "combsp_sd", prefix)
save_cache(combacc, "combsp_sd", prefix)
save_cache(combsp_sd, "combsp_sd", prefix)
save_cache(combacc_sd, "combsp_sd", prefix)







 
#----------------------------------------------------------
# Baseline prediction on all channels
#----------------------------------------------------------
colnums = c(5:9, 13:15)
allmean <- lapply(df[, 1:6], function(x) lapply(x, function(y) apply(y, 2, mean))) 
# res <- allmean %>% do.call(cbind, .)
res <- lapply(allmean, function(x) do.call(rbind, x)) %>% 
        lapply(., function(x) x[, colnums])
res2 <- do.call(cbind, res) %>% data.frame

res2$response <- as.factor(df$response)

rf <- randomForest::randomForest(response ~ ., data = res2)
# dfin <- res2[,colnums] 
rfmat <- replicate(10, randomForest::randomForest(response ~ ., data = res2))
cms <- apply(rfmat, 2, function(x) x$confusion) %>% data.frame

stats <- apply(cms, 1, mean)
sens <- 1 - stats[6]
spec <- 1 - stats[5]
bal.acc <- (sens + spec)/2

basedf <- data.frame(Sensitivity = sens, 
                     Specificity = spec, 
                     Balanced.Accuracy = bal.acc )

save_cache(basedf, "basestats", prefix)










