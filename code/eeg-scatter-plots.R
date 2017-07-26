# library(ggplot2)
library(dplyr)
library(ecomplex)

# Derived features. 30 Trials : 6 channels : 15 features
feature_df <-readRDS(cache_file("mod_all_features", prefix))
trial_df   <- readRDS(cache_file("mod_trial_segments", "eeg"))

# Labeled by hand and manually checked
id_vec <- as.factor(c(1,1,1,1,1,2,3,1,
                    1,1,1,1,1,1,1,1,1,1,1,1,1,
                    2,3,3,2,3,3,2,3,3))

df <- do.call(rbind, feature_df)

df <- df %>% cbind(., trial_df)
df$ids <- id_vec 
df <- dplyr::filter(df, window == 240)
df$trial <- 1:26
ids <- df$ids[means_df$trial]
 
ch = 3
means_df <- segment_on_col(df, ch, "ecomp.cspline_B") 
par(mfrow= c(2,3))
cex = 2
scatterplot_features("Theta", "Gamma", means_df, ids, cex = cex)
scatterplot_features("Beta", "Gamma", means_df, ids, cex = cex)
scatterplot_features("Entropy", "Gamma", means_df, ids, cex = cex)
scatterplot_features("Theta", "Beta", means_df, ids, cex = cex)
scatterplot_features("Alpha", "Entropy", means_df, ids, cex = cex)
scatterplot_features("Beta", "Hurst", means_df, ids, cex = cex)

