library(readr)
#----------------------------------------------------------
# Load specified eeg features
# Usese eeg-file-utiles.R
#----------------------------------------------------------
lfp_chs = 1:2
eeg_chs = 1:6
trials  = 1:30
prefix  = "eeg"

trial_df <- readRDS(cache_file("windows2", "meta"))

features <- names(feature_list())
lfpnames <- get_file_names(features, trial_df, (1:2), type = "LFFP")
eegnames <- get_file_names(features, trial_df, (1:6), type = "EEG")

lfp_df <- load_all_features(trials, lfp_chs, features, lfpnames)
eeg_df <- load_all_features(trials, eeg_chs, features, eegnames)

for(j in trials){
  for(k in 1:2){
    eeg_df[[j]][[k]] <- lfp_df[[j]][[k]]
  }
}

save_cache(eeg_df,"lfp_eeg_features", prefix) 

 
#----------------------------------------------------------
# Normalize bandpower
#----------------------------------------------------------
eeg_df <- load_all_features(trials, eeg_chs, features, eegnames)

relative_bp <- function(df_){
  colnums <- 5:9
  # for each channel compute total 
  row_tots <- exp(df_[colnums]) %>% apply(., 1, sum)
  df_[colnums] <- exp(df_[colnums]) / row_tots
  df_
}


for(j in 1:30){
  for(k in 1:6){
    eeg_df[[j]][[k]] <- relative_bp(eeg_df[[j]][[k]])
  }
}

save_cache(eeg_df,"all_features", prefix) 
