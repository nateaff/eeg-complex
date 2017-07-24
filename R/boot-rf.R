# get features from a list of matrices with with time 
# series as columns
#' @importFrom dplyr "%>%"
mc_features <- function(xs, feature) { 
   apply(xs, 2, function(mat) apply(mat, 2, 
                function(x) get_features(x, feature))) %>%
                lapply(., function(x) do.call(rbind, x))
} 

# get a statistic from a list of data frames, with each 
# data frame corresponding to a parameter set
mc_stat <- function(dfs, stat){
  lapply(dfs, function(mat) apply(mat, 2, stat)) %>% 
                do.call(rbind, .) %>%
                data.frame
}

# Create a non-parametric bootstrap functionj using indices
# for a a given statistics function. Used with boot()
boot_factory <- function(stat){
  function(x, ind){
    stat(x[ind])
  }
}

 
#----------------------------------------------------------
# Random forest resampling
#----------------------------------------------------------

# out-of-bag error given balanced classes
rf_errs <- function(rf){
  (rf$confusion[1] + rf$confusion[6])/2
}

tidy_confusion <- function(rf){
  ret <- t(c(rf$confusion))
  cbind(ret, (ret[1] + ret[4])/sum(ret[1:4]))
}

# compute mean error of bootstrap for a single formula
#' @importFrom randomForest randomForest
boot_rf_confusion <- function(df, formula, reps){
  ret <- replicate(reps, randomForest(formula, 
                         data = df, importance = FALSE)) %>% 
                         apply(., 2, function(rf) tidy_confusion(rf)) %>% 
                         t() %>% data.frame()
  
  names(ret) <- c("TP", "FN", "FP", "FN", "ERR_P", "ERR_N", "accuracy")
  ret
}

# boot multiple formulas
boot_formulas <- function(df, formulas, reps){
   lapply(formulas, function(f) boot_rf_confusion(df, f, reps))
}
