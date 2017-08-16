library(dplyr)
library(tssegment)
library(stargazer) 
#----------------------------------------------------------
# 1. Load derived features
# 2. Plot ecomplexity coefficients for each channel
# 3. Get combined complexity coefficients
# 4. Run palarm on combined coefficients A, B
# 4. Segment derived features based on the palarm 
#    features  
# 5. Classify on means of segments with 5-fold cross validation
#----------------------------------------------------------

if(!exists("from_cache")){
  from_cache = TRUE
}
prefix    = "eeg"
seed      = 2017

# Derived features. 30 Trials : 6 channels : 15 features
feature_df <-readRDS(cache_file("mod_all_features", prefix))
# Load meta data 
trial_df <- readRDS(cache_file("mod_trial_segments", "eeg"))
# tdf <- trial_df[trial_df$window == window, ]

# Select trials with 4 minute windows and 
# combine feature data frames with metadata

df <- do.call(rbind, feature_df) %>% 
      cbind(., trial_df) %>% 
      dplyr::filter(., window == 240)

df$trial <- 1:26

response <- rep(df$response, each = 120)
dfchs <-  lapply(df[ ,1:6], function(x) do.call(rbind, x))
# colnums = c(5:9,  13:15)

wc_test <- function(df_, response){
# Perform unparied Wilcox rank sum test = Mann-Whitney test
  tidx <- which(response)
  fidx <- which(!response)
  wc <- function(y){
     res <- (median(y[tidx]) > median(y[fidx]))
     type = ifelse(res, "g", "l")
      wt <- wilcox.test(x = y[tidx],  y = y[fidx], method = type) 
      wt$p.value
  }
  apply(df_[,colnums], 2, wc) %>% data.frame
}

wc_med <- function(df_, response){
  tidx <- which(response)
  fidx <- which(!response)
  colnums = c(5:9, 13:15)  
  med <- function(y){
     mt <- median(y[tidx]); mf <- median(y[fidx])
    data.frame(mt = mt , mf = mf)
  }
  apply(df_[,colnums], 2, med) %>% bind_cols
}
 
#----------------------------------------------------------
# Medians
#----------------------------------------------------------
trimmed_normalize <- function(x){
    x <- Trim(x, 0.01) 
    if (!max(x) == min(x)) {
        (x - min(x))/(max(x) - min(x))
    }
    else {
        rep(0, length(x))
    }
}

featnames <- c("Delta", "Theta", "Alpha", "Beta", 
                     "Gamma", "Variance", "Hurst", 
                     "Spectral Entropy")

meds <- lapply(dfchs, wc_med, response = response) %>% 
        bind_rows 
names(meds) <- paste0(rep(featnames, each = 2), rep(c("_T", "_F"), 8))
row.names(meds) <- paste0("Channel ", 1:6)

corrplot::corrplot(as.matrix(t(meds[, 1:10])), method = "circle", 
               col = gray.colors(10, start = 0.9, end = 0),
               is.corr = FALSE, 
               tl.col = "Black") 
 
#----------------------------------------------------------
# Differences
#----------------------------------------------------------
# variance, hurst, entropy
colnums = c(13:15)
normalized_dfchs <- lapply(dfchs, function(x) apply(x[,colnums], 2, trimmed_normalize))
meds <- lapply(dfchs, wc_med, response = response) %>% 
        bind_rows 
names(meds) <- paste0(rep(featnames, each = 2), rep(c("_T", "_F"), 8))
row.names(meds) <- paste0("Channel ", 1:6)

diff <- meds[, seq(1,16, by = 2)] - meds[, seq(2,16, by = 2)]
names(diff) <- c("Delta", "Theta", "Alpha", "Beta", 
                     "Gamma", "Variance", "Hurst", 
                     "Spec. Entropy")
diff <- apply(diff, 2, round, digits = 2) 

corrplot::corrplot(as.matrix(diff, method = "circle", 
               col = gray.colors(10, start = 0.9, end = 0),
               is.corr = FALSE, 
               tl.col = "Black") )

knitr::kable(t(diff))
stargazer(t(diff))2

 
#----------------------------------------------------------
# Logistic Regression
#----------------------------------------------------------
colnums = c(5:9,  13:15)
dfprepped <- lapply(dfchs, function(x) x[ ,colnums])
          
for( k in seq_along(dfprepped) ){
  dfprepped[[k]]$response <- response
}
dfprepped[[3]]
res <- lapply(dfprepped, 
              function(x) glm(response ~., 
                          family = binomial(link = "logit"), 
                          data = x)) %>% 
              lapply(., broom::tidy)


#----------------------------------------------------------
# P-values
#----------------------------------------------------------
res <- lapply(dfchs, wc_test, response = response) %>% 
       bind_cols 
names(res) <- paste0("Channel ", 1:6)
row.names(res) <- c("Delta", "Theta", "Alpha", "Beta", 
                     "Gamma", "Variance", "Hurst", 
                     "Spectral Entropy")

res <- apply(res, 2, round, digits = 4)

save_cache(res, "pvals", "eeg")


stargazer(res)
