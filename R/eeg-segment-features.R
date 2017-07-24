 
#----------------------------------------------------------
# To be deleted after plotting is updated
#----------------------------------------------------------

#----------------------------------------------------------
# Functions for filtering meta data for trials and 
# segmenting channels based on complexity coefficients
#----------------------------------------------------------
len_to_ranges <- function(vec){
  breaks <- c(0, cumsum(vec))
  lapply(1:(length(breaks)-1), function(k) (breaks[k]+1):breaks[k+1])
}

get_weights <- function(segdata, tnum){
  segs <- segdata$seglist[segdata$segdf$trialnum == tnum]
  unlist(lapply(segs, length))
}

segment_ranges <- function(change_pts){
  starts <- c(1, change_pts[-length(change_pts)] + 1)
  ends <- change_pts
  lapply(1:length(starts), function(k) starts[k]:ends[k])
}

get_trialnums <- function(change_pts, metadata){
  ends <- c(change_pts)
  trialnums <- c(ends %/% (metadata$len + 1) + 1)
  trialnums[length(trialnums)] <- metadata$ntrials
  trialnums
}

get_response <- function(metadata, trialnums){ 
  # correct last trial to 24
  response_map <- metadata$trialdf$response
  response <- unlist(lapply(trialnums, function(k) response_map[k])) 
  response
}

# Create list of dataframes of the mean value on each
# segment with segment info in segdata.
feature_mean_df <- function(metadata, segdata, ch){
  segs <- segdata$seglist
  response <- segdata$segresponse
  ind <- metadata$ind 
  # concatenate
  df_f <- lapply(ind, function(j) metadata$feature_df[[j]][[ch]]) %>% 
                      do.call(rbind, .)
  # take means over segments
  df_s <- lapply(segs, function(seg) apply(df_f[seg, ], 2, mean) ) %>% 
                          do.call(rbind, .) %>% data.frame
  df_s$response <- as.factor(response)
  df_s$trial <- segdata$segtrials
  df_s
}

segment_data <- function(metadata, change_pts){
  len <- metadata$len
  ind <- metadata$ind
  ntrials <- metadata$ntrials

  segs <- segment_ranges(change_pts)
  trialnums <- get_trialnums(change_pts, metadata)
  response <- get_response(metadata, trialnums) 
  segdf <- data.frame(trialnum = trialnums, response = response)
  list(seglist = segs, segtrials = trialnums, segresponse = response, segdf = segdf)
}


# Get trial data based on window length
get_trialdata <- function(feature_df, trial_df, window, segch, len, stim1 = FALSE){
    # indices of trials used
    
    if(stim1) { 
    trial_ind <- which(trial_df$window == window & (trial_df$response == FALSE | trial_df$stim_num ==1))
    } else {
      trial_ind <- which(trial_df$window == window)
    } 
    trialdf <- trial_df[trial_ind, ]
    ntrials <- length(trial_ind)

    # Meta data for trial test
    list(feature_df = feature_df,
        trialdf     = trialdf,
        len         = len, 
        segch       = segch, 
        ind         = trial_ind, 
        nch         = 6,
        ntrials     = ntrials,
        chlen       = ntrials*len)
}


 
#----------------------------------------------------------
#
#----------------------------------------------------------

palarm_means <- function(x){
  palarm(x)$means
}


ecomp_change_pts <- function(metadata){
  # df, metadf, ch, len, numch = 6
  ABlist <- lapply(1:(metadata$nch), 
                   function(ch) get_ecomp_df(metadata)) 
  dfs <- do.call(rbind, ABlist) 
  totalen <- metadata$ntrials*metadata$len
  dfs$channel <- factor(rep(1:6, each = totalen))
  get_change_pts(dfs, metadata)  
}


# get change_pts using comnbined pts for complexity coefficients A,B
get_change_pts <- function(dfs, metadata){ 
  ch = metadata$segch
  df <- subset(dfs, channel == ch)
  meansA <- palarm_means(df$A)
  meansB <- palarm_means(df$B)
  # combine and remove short segments
  means_comb <- meansA + meansB
  res <- remove_short_segments(means_comb, metadata, min_len = 2)
  palarm_means_AB <- res$means
  change_pts <- res$change_pts
  list(change_pts = change_pts, meansAB = palarm_means_AB)
 }
