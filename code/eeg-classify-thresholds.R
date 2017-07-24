rm(list = ls())
# library(ggplot2)
library(dplyr)
library(ecomplex)
library(randomForest)
library(tssegment) # segmenetation

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

df <- do.call(rbind, feature_df) %>% 
      cbind(., trial_df) %>% 
      dplyr::filter(., window == 240)
df$trial <- 1:26

# FIXME : Check earlier versions of this method. 
boot_threshold <- function(df, ch, n, seed = 1){
  set.seed(seed)
  colnums = c(5:9,  13:16)
  means_df <- segment_on_cols(df, ch, 
                          cols = c("ecomp.cspline_B","ecomp.cspline_A"), 
                          new_vec = F)
  
  # run_cv2(means_df, df$response, ch, colnums, from_cache=TRUE)
  means_df$response <- as.factor(means_df$response)
  rows <- sample(1:nrow(means_df), 45, replace = TRUE)
  mod <- segmentRF(response ~., means_df[rows, colnums])

  # mod <- segmentRF(response ~., means_df)
  ground <- means_df$response[rows]
  roc_res <- pROC::roc(ground, mod$votes[,1])
  best = pROC::coords(roc_res, "best", ret = "threshold")
  best
}
 
thresholds <- lapply(1:6, function(x) boot_threshold(df, x, 45))
# Approximate thresholds from previous method
thresholds <- list(.40, .40, .50, .50, .55, .55)

save_cache(thresholds,"cvthresholds", prefix)
