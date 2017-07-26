library(dplyr)
library(ggplot2)
library(ecomplex)
# library(tsfeats) 
# library(tssims)
library(fArma)

if(!exists("from_cache")){
  from_cache = TRUE
}
prefix = "ecomplex"
set.seed(1)

# Generate complexity coefficients for all approximation methods
if(from_cache){
  sim_df1 <- readRDS(cache_file("approx_types1", prefix))
  sim_df <- readRDS(cache_file("approx_types", prefix))
  } else {
  ecomp_methods <- c( tsfeats::ecomp_lift, 
                      tsfeats::ecomp_cspline, 
                      tsfeats::ecomp_bspline, 
                      tsfeats::ecomp_all)
  sim_df <- make_sim_features(n = 30, len = 500, features = ecomp_methods)

  sim_df1  <- select(sim_df, -ends_with("A")) %>% reshape2::melt(.)
  sim_df2  <- select(sim_df, -ends_with("B")) %>% reshape2::melt(.)
  sim_df1$yvalue <- sim_df2$value
  save_cache(sim_df1, "approx_types1", prefix)
  save_cache(sim_df, "approx_types", prefix)

}

# Compute approximation error
if(from_cache){
  err_means <- readRDS(cache_file("ds_epsilons", prefix))
  all_err_means <- readRDS(cache_file("all_errs", prefix))
} else {

  epsilons <- function(mat, method){
    apply(mat, 2, function(x) ecomplex(x, method = method)) %>% 
             lapply(., function(x) x$epsilons) %>%
             do.call(rbind, .)  
  }

  n <- 30
  len = 500
  methods <- list("lift", "cspline", "bspline", "all")
  gs <- tssims::sim_suite()
  mats <- lapply(gs$group1, function(f) replicate(n, gen(jitter_params(f))(len)))  

  avg_err <- function(mat){
    # dataframe with cols = downsample, rows = method 
    fit_eps <- lapply(methods, function(method) epsilons(mat, method))
    err_means <- lapply(fit_eps, function(m) apply(m, 2, mean)) %>% 
                 do.call(cbind, .) %>%
                 data.frame

    names(err_means) <- methods
    row.names(err_means) <- paste0("downsample", (1:5))
    err_means <- apply(err_means, 2, function(x) round(x, 2)) %>%
                 data.frame
    err_means 
  }
  all_errs <- lapply(mats, avg_err)
  all_err_means <- lapply(all_errs, function(x) apply(x, 2, mean)) %>% 
                  do.call(rbind, .) %>% data.frame
  
  all_err_means 
  # save_cache(err_means, "ds_epsilons", prefix)
  save_cache(all_err_means, "all_errs", prefix)
} # end else

# df <- reshape2::melt(err_means)
# df$downsample <- rep((1:5),4)

# gp <- ggplot(df, aes(x = downsample, y = value, group = variable)) + 
#       geom_line(aes(color = variable)) + 
#       scale_color_manual(values = eegpalette(0.9)) + 
#       labs(x = "Downsample level", y = "Mean absolute error") + 
#       theme_base()
# gp

# save_plot("mae_errors", prefix)
# dev.off()
# gridExtra::grid.table(err_means)
# save_plot("err_means", prefix)
# dev.off()
# gridExtra::grid.table(all_err_means)
# save_plot("all_err_means")




# Classification error
if(from_cache){
  rf_errs <- readRDS(cache_file("rf_errs", prefix))
}
if(!from_cache){
  fns <- unique(sim_df$fnames)
  fdf <- lapply(fns, function(f) sim_df[sim_df$fnames == f,])  

  rf_accuracy <- function(df){
    rfs <- list( randomForest::randomForest(id ~ lift_A + lift_B, data = df),
                 randomForest::randomForest(id ~ cspline_A + cspline_B, data = df),
                 randomForest::randomForest(id ~ bspline_A +  bspline_B, data = df), 
                 randomForest::randomForest(id ~ ecomp_all_A +  ecomp_all_B, data = df)
             )
    acc <- lapply(rfs, function(rf) round((rf$confusion[2] + rf$confusion[3])/sum(rf$confusion), 3))
    ret <- unlist(t(acc)) %>% data.frame
    # names(ret) <- 
    ret
  }
  rf_errs <- lapply(fdf, rf_accuracy) %>% 
             do.call(cbind,. ) %>%  t %>% data.frame
  names(rf_errs) <- c("Lift", "Cspline", "Bpsline", "Combined")
  row.names(rf_errs) <- fns
  save_cache(rf_errs, "rf_errs", prefix)
}

rf_errs


