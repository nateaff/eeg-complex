
# Return list of features computed on raw data
feature_list <- function(){
  features <- list(wvar = gmwm::wvar, 
                 bandpower = bandpower,
                 ecomp = ecomp_lift, 
                 fd = fd_variogram,
                 # SampEn = sample_entropy, 
                 var = variance,
                 hurst = hurst, 
                 specEn  = spectral_entropy)
}

# Return a file name of feature data
file_name <- function(k, ch, type = 'EEG', feature, trial_df){
    sep <- '_'
    prefix <- trial_df$id[k]
    filename <- paste0(trial_df$trial_name[k], sep, 
          'SEG', trial_df$stim_num[k], sep, 'ch', ch, sep, feature, sep,  
          type)
    data_file(filename, prefix, ext = ".txt")
    # filename
}

# Return a nested list of the feature file names (trial_num -> channel))
# use names(feature_list()) for feat_names
get_file_names <- function(feat_names, trial_df, chs = (1:6), type = "EEG"){
  lapply(1:dim(trial_df)[1], function(i) lapply(chs,
                         function(j) unlist(lapply(1:length(feat_names), 
                         function(k) file_name(i,j, type, feat_names[k], trial_df)))))
}

# load single feature
load_feature <- function(trial, ch, feature, fnames){
  dfs <- lapply(1:length(features), 
                function(feat) readRDS(fnames[[trial]][[ch]][feat]))
  do.call(cbind, lapply(dfs, 
                function(x) subset(x, select = -id) ))
}

# load all features 
load_all_features <- function(ntrials, chs, features, fnames){
  lapply(ntrials, function(trial) lapply(chs, 
                  function(ch) load_feature(trial, ch, feature, fnames)))
}

# list of feature names
get_feature_names <- function(){
  c("wvar2", "wvar4", "wvar8", "wvar16", 
    "delta", "theta", "alpha", "beta", "gamma", 
    "ecompA","ecompB", "fd", "var", "hurst", "specEn")
}


