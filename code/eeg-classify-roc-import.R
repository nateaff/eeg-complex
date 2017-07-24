
## @knitr importance-plot
library(ggplot2)
library(dplyr)
library(ecomplex)
library(randomForest)
library(tssegment)
library(caret)

prefix    = "eeg"
seed      = 2017
chs       = 1:6
from_cache = TRUE
window    = 240 
len = window/2


# Derived features. 30 Trials : 6 channels : 15 features
feature_df <-readRDS(cache_file("mod_all_features", prefix))
# Load meta data 
trial_df <- readRDS(cache_file("mod_trial_segments", "eeg"))
# tdf <- trial_df[trial_df$window == window, ]

# Select trials with 4 minute windows and 
# combine feature data frames with metadata
df <- do.call(rbind, feature_df)
df <- df %>% cbind(trial_df) 
df <- dplyr::filter(df, window == 240)
df$trial <- 1:26

#----------------------------------------------------------
# Run cross validation segmenting on a single feature and 
# channel.
#----------------------------------------------------------
run_on_feat <- function(df_, ch, feat, m = 5, byname = TRUE){
  colnums = c(5:9, 13:18)
  if(byname){
      means_df <- segment_on_cols(df_, ch, cols = feat, m = m)
    } else {
      means_df <- segment_on_cols(df_, ch, vec = feat, new_vec = TRUE, m = m) 
    }  
  cat("Number of segments: ", dim(means_df)[1], "\n")
  cat("Using channel: ", ch, "\n")
  cm <- run_cv(means_df, df$response, ch, colnums, TRUE)
}



library(corrplot) 
#----------------------------------------------------------
# Importance
#----------------------------------------------------------
resAB <- resvec <- list() 
feat <-  c("ecomp.cspline_B")
varnames <- c("Delta", "Theta", "Alpha", "Beta", "Gamma", 
                      "Variance", "Hurst", "Spectral Entropy")
for(ch in 1:6){
  resAB[[ch]]  <- run_on_feat(df, ch, feat, m = 3, byname = TRUE)
  resvec[[ch]] <- run_on_feat(df, ch, (1:120), m = 10, byname = FALSE)
}

ABimport <- lapply(resAB, function(x) x$importance) %>% 
            do.call(rbind, .) %>% 
            data.frame
vecimport <- lapply(resvec, function(x) x$importance) %>% 
             do.call(rbind, .) %>% 
             data.frame
names(ABimport) <- varnames
names(vecimport) <- varnames

ABnorm <- apply(ABimport, 1, ecomplex::normalize)
vecnorm <- apply(vecimport, 1, ecomplex::normalize)

c1 <- corrplot::corrplot(
               as.matrix(ABnorm),
               method = "circle", 
               # col = viridis::viridis(60)[55:2],
               # col = gray.colors(10, start = 0.9, end = 0),
               is.corr = FALSE, 
               tl.col = "Black"
               )
mtext("Parition on coefficient B change points.", side = 3, line = 3) 

corrplot::corrplot(
               as.matrix(vecnorm), 
               method = "circle", 
               # col = gray.colors(10, start = 0.9, end = 0),
               is.corr = FALSE, 
               tl.col = "Black"
               ) 
mtext("Uniform partition into 8 segments.", side = 3, line = 3)


## @ knitr ROC-plot

#----------------------------------------------------------
# ROC, Importance
#----------------------------------------------------------
library(pROC)
ABdf <- resAB[[1]]$df
vecdf <- resvec[[1]]$df

vecroc <- pROC::roc(df$response, vecdf$prob)
ABroc <- pROC::roc(df$response, ABdf$prob)

pdf(file.path(getwd(), paste0("figures/", prefix, "roc-comb.pdf")), 
    width = 8, height = 4)

par(mfrow = c(1,2))
plot(ABroc)
plot.roc(vecroc, add=TRUE, col="blue", lty = 3)
   legend("bottomright", legend=c("Parition on coefficient B", "8 regular paritions"),
          col=c(par("fg"), "blue"), lwd=2, lty = c(1,3))


plot(smooth(ABroc))
plot.roc(smooth(vecroc), add=TRUE, col="blue", lty = 3)
   legend("bottomright", legend=c("Parition on coefficient B", "8 regular paritions"),
          col=c(par("fg"), "blue"), lwd=2, lty = c(1,3))


par(mfrow = c(1,1))

dev.off()
