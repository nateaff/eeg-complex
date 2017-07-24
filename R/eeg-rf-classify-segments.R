 
#----------------------------------------------------------
# To be deleted: older version
#----------------------------------------------------------

#' Leave one out classification of a single trial
#'
#'  Takes the mean of features on segmented data and
#'  predicts segments using random forest classifier
#'
#' @param  df: Features means for a given trial
#' @param data_ : Segment book-keeping data
#' @param tindex : The trial index (refers to the filtered index)
#' @param colnums: df_means columns to use in classification
#'
#' @return A list of 1. Prediction probability vector, 
#'                   2. Prediction vector
#'                   3. Weight vector
#' @export
predict_single_ch <- function(df_, data_, tnum,  train, colnums, thresh =.45){

    trial_ind <- data_$segtrials == tnum
    df_$trial <- data_$segtrials
    df_seg <- na.omit(df_[ ,colnums])

    df_in  <- df_seg %>% dplyr::filter(trial %in% train) %>% 
                      dplyr::select(-trial)  
    df_out <- df_seg %>% dplyr::filter(trial %in% tnum) %>% 
                      dplyr::select(-trial)

    rf_fit <- randomForest::randomForest(factor(response) ~ ., 
                            data = df_in,
                            ntree = 1000,
                            mtry = 5) 
    rf_fit
    # compute probabilities
    pred <- predict(rf_fit, df_out, type = "prob")
    wts <- lapply(data_$seglist[trial_ind], length) %>% unlist
    prob <- sum((pred[,2]*wts) / sum(wts))
    list(pred = pred[,2], 
         prob = prob, 
         wts = wts, 
         importance = rf_fit$importance)
}



#' Leave one out classification of a single trial
#'
#'  Takes the mean of features on segmented data and
#'  predicts segments using random forest classifier
#'
#' @param df: Features means for a given trial
#' @param colnums: df_means columns to use in classification
#'
#' @return A list of 1. Prediction probability vector, 
#'                    2. Prediction vector
#'                    3. Weight vector
#' @export
predict_all_segments <- function(df_, colnums, ...){
    # omit wavelet variance and complexity
    df_in <- na.omit(df_[ ,colnums])
    df_in <- 
    rf <- randomForest::randomForest(response ~ ., data = df_in, ...) 
   rf
}

predict_all_ch <- function(df_, 
                           data_, 
                           colnums, 
                           trialnums, 
                           thresh = 0.5){
  # ntrials <- dim(trialdata$trialdf)[1]
  predlist <- vector("list", length(trialnums))
  j = 1
  for(k in seq_along(trialnums)){
    train <- trialnums[-k]
    predlist[[j]] <- predict_single_ch(df_, 
                                       data_, 
                                       trialnums[k],
                                       train, 
                                       colnums)
    j = j + 1
  } 
  probvec <- unlist(lapply(predlist, function(x) x$prob))
  response <- probvec > thresh
  list(probvec = probvec, response = response, predlist  = predlist)
}


# Classify on all segments
classify_means_rf <- function(df, ind, response, thresh=.50){
  df <- df[c(ind),]
  probs <- double(length(ind))
  for(k in 1:24) {
    df_in <- df[-k, ]
    df_out <- df[k, ]
    # df_out <- df_in[df_in$trial == k, ]

    rf_fit <- randomForest(factor(id) ~ ., 
                        data = df_in,
                        ntree = 1000,
                        mtry = 10) 

    probs[k] <- predict(rf_fit, df_out, type = "prob")
    # wts <- lapply(segs[trial_ind], length) %>% unlist

    # probs[k] <- sum((as.numeric(pred)*wts) / sum(wts))
    pred_out[k] <- ifelse(probs[k] < thresh, TRUE, FALSE )
    cat("prob: ", probs[k], "\n")
  }
  ground <- df$id
  # 0 = TN, 1 = FP, 2 = FN, 3 = TP
  cm <- confusion_mat(ground, pred_out)
  cm        
  list(cm = cm, preds = pred_out, response = ground, probs = probs)
}

