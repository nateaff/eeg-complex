
# Clean up caret::confusionMatrix
confusion_mat <- function(pred, truth, ...){
  cm <- suppressWarnings( 
        caret::confusionMatrix(pred, truth, ...)
        )
  df <- cm$byClass %>% t(.) %>% data.frame 
  df$accuracy <- cm$overall[1]
  df 
}

# Returns cross validation sets that 
# Make crosss validation sets 
make_cv_sets <- function(df){
  cat("CV sets only valid for 240 second window trials \n")
  # indices of true/false response sets
  indT <- list(1:2, 3:4, 5:6, 7:8, 9) 
  indF <- list(1:3, 4:6, 7:9, 10:12, 13:17)

  respT <- df %>% dplyr::filter(response == TRUE) %>% 
                           dplyr::select(trial) %>% unique
  respF <- df %>%  dplyr::filter(response == FALSE) %>% 
                            dplyr::select(trial) %>% unique
  randT <- sample(respT[ ,1], length(respT[,1]))
  randF <- sample(respF[ ,1], length(respF[,1]))

  # Create test and train sets
  f <- function(j){  
      test <- c(randT[indT[[j]]], randF[indF[[j]]]) 
      omit <- c(randT, randF) %in% test
      train <- c(randT, randF)[!omit]
      list(test = test, train = train)
  }  
  
  lapply(1:length(indT), function(k) f(k))
}

#----------------------------------------------------------
# Cross validation functions usnig cv_sets as hold-outs
#----------------------------------------------------------
# segmentRF takes formula method, I.e. x = formula, 
# data = data.frame with an id variable 
#----------------------------------------------------------
 
cv_one <- function(df, train, test, thresholds){

    traindf <- dplyr::filter(df, trial %in% train) %>% 
              select(-trial, -weights)
    traindf$response <- as.factor(traindf$response)
      
    mod <- tssegment::segmentRF(response ~., traindf)
    
  test_one <- function(k){
      testdf <- dplyr::filter(df, trial == k) 
      weights <- testdf$weights 
      # testdf
      testdf <- testdf %>% select(-trial, -weights)
      testdf$response <- as.factor(testdf$response)
      predict(mod, newdata = testdf, weights = weights, thresholds = thresholds)
    }
  res <- lapply(test, function(x) test_one(x))
  prob_true <- unlist(lapply(res,  function(x) x$prob[,2]))
  pred <- unlist(lapply(res,  function(x) x$pred))
  list(pred = pred, trials = test, prob = prob_true,  import = mod$importance)
}

# Segment features for ch = ch in df on column = colname
segment_on_cols <- function(df, ch, vec = NULL, 
                                    cols = NULL, 
                                    new_vec = FALSE, 
                                    m = 5){ 
  if(!new_vec) {
    cat("Segmenting on ", cols, "............ \n ")
  } else {
    cat("Segmenting on vector ............ \n ")         
  } 
  dfch <- df[,ch] 
  segment_one <- function(k){
    if(new_vec){
      y = vec 
    } else {
      y = dfch[[k]][ , cols]
    } 
    tssegment::segment(dfch[[k]], y, labels = df[k, c("response", "trial")], m = m )
  } 
  ret <- lapply(1:length(dfch), function(k) segment_one(k)) %>%
          do.call(rbind, .)
  ret
}


# Cross validation on fixed sets created in make_cv_sets 
cv_segments <- function(df, thresholds, colnums, ...){
  df <- df[colnums]
  cvsets <- make_cv_sets(df)
  #For each test element, predict using the train set
  res    <- lapply(cvsets, function(set) cv_one(df, set$train, set$test, thresholds))
  pred   <- unlist(lapply(res, function(x) x$pred))
  prob   <- unlist(lapply(res, function(x) x$prob))
  trial  <- unlist(lapply(cvsets, function(x) x$test))
  import <- lapply(res, function(x) x$import) %>% 
            do.call(cbind,.) %>%
            apply(., 1, mean)
  # resp   <- pred > thresh
  ret <- data.frame(  prediction = pred,
                   prob = prob,
                   trial = trial) %>% arrange(.,trial)

  list(df = arrange(ret, trial), import = import)
}
 
#
run_cv <- function(df_, ground, ch, colnums, from_cache){
  # Cross validation functions in eeg-rf-cv.R
  threshs <- readRDS(cache_file("cvthresholds", "eeg"))
  thresh <- c(1 - threshs[[ch]], threshs[[ch]])
  cat(sprintf("Threshold %f, channel %d \n", round(threshs[[ch]],2), ch))  
  
  # Cross validation on segment
  res <- cv_segments(df_, thresh, colnums)
  pred <- res$df$prediction
  cm <- confusion_mat(pred, ground)
  rc <- suppressWarnings(
        pROC::roc(ground, res$df$prob)
       )  
  cm$auc <- rc$auc
  # Trials not correctly classified
  which(ground != pred)

  if(!from_cache){
    save_cache(res, paste0("cv_results_ch_", ch), "eeg") 
    save_cache(cm, paste0("cv_confusion_mat_ch_", ch), "eeg") 
  }
  list(cm = cm, df = res$df, importance = res$import)
}


#----------------------------------------------------------
# Older Segment functions 
#----------------------------------------------------------

split_palarm <- function(x){
  means      <- lapply(x, function(y) y$means) 
  change_pts <- lapply(x, function(y) y$kout)
  list(means = means, change_pts = change_pts)
}


# Returns long data frame with complexity coefficients of all trials
get_complexity_coeffs <- function(df){
  # All complexity coefficients for all trials
  dflist <- lapply(1:6, 
                  function(ch) (df) ) 
  dfs    <- do.call(rbind, dflist) 
  dfs$channel <- factor(rep(1:6, each = 120))
  dfs
}















