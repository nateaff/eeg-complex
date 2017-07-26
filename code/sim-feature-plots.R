library(ggplot2)
library(ecomplex)
library(dplyr)
library(ggthemes)

if(!exists("from_cache")){
  from_cache = TRUE
}
prefix = "sim"
n = 20 
len = 500
seed = 1

#----------------------------------------------------------
# Compare separation in each feature for complexity 
# features and variance 
#----------------------------------------------------------
comp_filename <- "complexity_features"
var_filename <- "variance_features"
if(from_cache) {
  comp_df <- readRDS(cache_file(comp_filename, prefix))
  var_df <- readRDS(cache_file(var_filename, prefix))
} else {
  # complexity related features
  comp_features <- c(ecomp_lift, 
                     permutation_entropy,
                     sample_entropy, 
                     hurst,
                     fd_variogram, 
                     variance) 
  complex_df <- make_sim_features(n, len, comp_features, seed)
  
  # variance features  
  var_features <- c(var, gmwm::wvar, bandpower)
  var_df <- make_sim_features(n, len, var_features, seed)

  save_cache(comp_df, comp_filename, prefix)
  save_cache(var_df, var_filename, prefix)
}

#----------------------------------------------------------
# plots
#----------------------------------------------------------
names(comp_df) <- c("EcompA", "EcompB", "PermEn", "SampEn", 
                        "Hurst", "FD", "Var", "id", "fnames")

# plot individual functions
n = 20
len = 500 
seed = 1
plot_names <- paste0("feature_density_", unique(comp_df$fnames))
for(k in 1:length(gs$group1)) {
  start <- (k-1)*(n*2) + 1
  end   <- start + n*2-1
  plot_features(comp_df[start:end, 1:8], type = "density", 
                                       title = plot_names[k], 
                                       ncol = 7, single = TRUE)
  # save_plot(plot_names[k], prefix)
}

#----------------------------------------------------------
# variance feature plots
#----------------------------------------------------------
names(var_df) <- c("Variance", "WavVar2", "WavVar4", "WavVar8", "WavVar16", 
                  "Delta", "Theta", "Alpha", "Beta", "Gamma", "id", "fnames")  


plot_names <- paste0("var_feature_density_", unique(var_df$fnames))
for(k in 1:length(gs$group1)) {
  start <- (k-1)*(n*2) + 1
  end   <- start + n*2-1
  plot_features(var_df[start:end, 1:11], type = "density", 
                                          title = plot_names[k], 
                                          ncol = 10, single = TRUE)
  save_plot(plot_names[k], prefix)
}
 
#----------------------------------------------------------
# Plot faceted version with view of all features
#----------------------------------------------------------
plot_name <- "all_variance_densities"
plot_features(var_df, type = "density", ncol = 10, single = FALSE)
save_plot(plot_name, prefix)
  
