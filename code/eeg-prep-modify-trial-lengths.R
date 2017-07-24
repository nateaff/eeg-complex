#----------------------------------------------------------
# 2 trials have longer than 120 lengths segments and 1 trial has less
# than 120 length segment by only 1 observation.  I trim the two long
# trials and pad the short trial with a data point average of the
# previous two observations.
#----------------------------------------------------------

prefix = "eeg"

# All derived features. 30 Trials : 6 channels : 15 features
features_df <- readRDS(cache_file("all_features", prefix))

lfp_features_df <- readRDS(cache_file("lfp_eeg_features", prefix))
# Add another observation to a data frame that is an average of the
# last two observations
pad <- function(x) {
    cat(len)
    sum <- x[len, ] + x[(len - 1), ]
    res <- apply(sum, 2, mean)
    rbind(x, res)
}

# Data with all eeg channels
lens <- lapply(features_df, function(x) lapply(x, function(y) dim(y)[1]))

features_df[[16]] <- lapply(features_df[[16]], function(x) x[1:120, ])
features_df[[17]] <- lapply(features_df[[17]], function(x) x[1:120, ])

save_cache(features_df, "mod_all_features", prefix)

# Data with chs 1,2 LFP 
features_df <- lfp_features_df
lens <- lapply(features_df, function(x) lapply(x, function(y) dim(y)[1]))

features_df[[16]] <- lapply(features_df[[16]], function(x) x[1:120, ])
features_df[[17]] <- lapply(features_df[[17]], function(x) x[1:120, ])

save_cache(features_df, "mod_lfp_features", prefix)

# Trial info from meta.json file
trial_df <- readRDS(cache_file("trial_segments", "eeg"))

trial_df$window[13] <- 239
trial_df$window[16] <- 240
trial_df$window[17] <- 240

save_cache(trial_df, "mod_trial_segments", prefix)
