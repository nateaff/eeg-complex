## @knitr eeg-importance-plot

library(ggplot2)
library(dplyr)
library(ecomplex)
library(randomForest)
library(tssegment)
library(caret)
library(corrplot) 

prefix    = "eeg"
seed      = 2017
chs       = 1:6
from_cache = TRUE
window    = 240 
len = window/2

feature_df <-readRDS(cache_file("mod_all_features", "eeg"))
trial_df <- readRDS(cache_file("mod_trial_segments", "eeg"))

# Select trials with 4 minute windows and 
# combine feature data frames with metadata
df <- do.call(rbind, feature_df)
df <- df %>% cbind(trial_df) 
df <- dplyr::filter(df, window == 240)
df$trial <- 1:26

# Run cross validation segmenting on a single feature and 
# channel.
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

# Compute importance
varnames <- c("Delta", "Theta", "Alpha", "Beta", "Gamma", 
              "Variance", "Hurst", "Spectral Entropy")
if(!from_cache){

  feat <-  c("ecomp.cspline_B")
  resAB_raw <- resvec_raw <- list() 
  for(ch in 1:6){
    resAB_raw[[ch]]  <- run_on_feat(df, ch, feat, m = 3, byname = TRUE)
    resvec_raw[[ch]] <- run_on_feat(df, ch, (1:120), m = 10, byname = FALSE)
  }
  save_cache(resAB_raw, "resAB_raw", "eeg")
  save_cache(resvec_raw, "resvec_raw", "eeg")
} else {
  resAB_raw <- readRDS(cache_file("resAB_raw", "eeg"))
  resvec_raw <- readRDS(cache_file("resvec_raw", "eeg"))
}


ABimport <- lapply(resAB_raw, function(x) x$importance) %>% 
            do.call(rbind, .) %>% 
            data.frame
vecimport <- lapply(resvec_raw, function(x) x$importance) %>% 
             do.call(rbind, .) %>% 
             data.frame
names(ABimport) <- varnames
names(vecimport) <- varnames

ABnorm <- apply(ABimport, 1, ecomplex::normalize)
vecnorm <- apply(vecimport, 1, ecomplex::normalize)

layout(matrix(c(1,2), 2, 2, byrow = TRUE))

c1  <- corrplot::corrplot(
               as.matrix(ABnorm),
               method = "circle", 
               col = viridis::viridis(30)[25:10],
               # col = gray.colors(10, start = 0.9, end = 0),
               is.corr = FALSE, 
               tl.col = "Black"
               )
# mtext("Parition on coefficient B change points.", side = 3, line = 3) 

corrplot::corrplot(
               as.matrix(vecnorm), 
               method = "circle", 
                col = viridis::viridis(30)[25:10],
               # col = gray.colors (10, start = 0.9, end = 0),
               is.corr = FALSE, 
               tl.col = "Black"
               ) 
# mtext("Uniform partition into 8 segments.", side = 3, line = 3)


