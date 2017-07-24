#----------------------------------------------------------
# Comment : Script extracts set of features from the EEG
#  and LFP data. These are the same set of data with 
#  different filters. 
#  
#  Chs 1:2 Internal channels with LFP filter 
#  Chs 3:6 External EEG channels
#  
#  Note : The RCurl function downloads large files. 
#  This script takes 30 mins or so to extract LFP and
#  EEF features when run on a m4.xlarge AWS instance.
#----------------------------------------------------------

library(data.table)
library(ecomplex)
library(dplyr)
library(parallel)
library(readr )
library(RCurl)

# Take 2 second segments at 1220 samples/sec
fs <- 1220; sec <- 2
len <- fs*sec

# Set logging and
logdir <- "~/logs/"
save_dir <- "~/data"
# metadata file

# s3 <- "https://s3-us-west-1.amazonaws.com/ucsf-eeg/pre-stim/"

sep = "_"
save_file <- function(k, ch, type = "EEG", feature, dir){
    paste0(dir,wins$id[k], sep , wins$trial_name[k], sep, 
          "SEG", wins$stim_num[k], sep, "ch", ch, sep,
          feature, sep, type, ".txt")
}

get_file <- function(k, type = "EEG"){
   paste0(s3, wins$id[k], sep , wins$trial_name[k], sep, 
        "SEG", wins$stim_num[k], sep, type,".txt")
}

# Features extracted
features <- list(ecomp = ecomp_cspline, 
                 fd = fd_variogram,
                 SampEn = sample_entropy, 
                 var = variance, 
                 hurst = hurst, 
                 specEn = spectral_entropy, 
                 wvar = gmwm::wvar, 
                 bandpower = bandpower)

feat_names <- names(features)

logfile <- paste0(logdir, format(Sys.time(), "log_%m-%d-%Y_%H:%M.txt"))
sink(logfile, append = FALSE, split = TRUE)
cat(format(Sys.time(), "Date            :  %x   \n"))
cat(format(Sys.time(), "Start time      :  %H:%M   \n"))

# type <- "EEG"; chs = 3:6;
type <- "LFFP"; chs = 1:2;
for(i in 1:dim(wins)[1]){
  cur_file <- get_file(i, type)
  file <- getURL(cur_file)
  data <- t(data.matrix(fread(file)))
  N <- floor(dim(data)[1]/len)
  ends <- 1:N*len
  starts <- c(1, ends[1:(N-1)])
  ends[1] <- ends[1] + 1 
  stopifnot(max(ends) <= dim(data)[1])
  # load each channel 
  for( j in chs){
    r1 <- data[,j]
    # Chop up time series into matrix where each column is a segment
    tsm_all <- lapply(1:(length(ends)), function(x) c(starts[x]:ends[x])) %>% 
                lapply(., function(x) r1[x]) %>% 
                do.call(cbind, .)
    # extract features
    for(k in seq_along(features)){  
      fname <- save_file(i, j, feat_names[k], type)
      cat("..............................\n")
      cat("Extracting feature............", feat_names[k], "\n")
      
      tt <- system.time(
      df <- get_features(tsm_all, features[k], id = "1", 
                             ncores = 4, verbose = TRUE)
      ) # end timer

      cat("Time elapsed          : ", tt[[3]], "\n")
      cat("Writing feature       : ", feat_names[k], "\n")
          write_rds(df, fname)
    }
  }
}

#----------------------------------------------------------
# Get change points from raw data 
# Note: not used in analysis. No change points were 
# detected at this scale.
#----------------------------------------------------------
get_change_points <- function(x){
  ind <- which(diff(x) != 0)
  vals <- x[ind]
  vals
}

save_loc <- "~/palarm"
type <- "EEG"

get_palarm <- function(chs, type, save_loc){
  for(i in 1:dim(wins)[1]){
    cur_file <- get_file(i, type)
    file <- getURL(cur_file)
    data <- t(data.matrix(fread(file)))
    for(j in chs[1]:chs[2]){
      fname <- save_file(i, j, type, "palarm", dir = save_loc)
      cat("..............................\n")
      cat("Running palam..................\n")
      
      tt <- system.time(
      df <- data[ind, 4:5] %>% 
                apply(., 2, normalize) %>% 
                apply(., 2, function(x) palarm(x, m = 10, M = 1000)) %>%
                lapply( ., function(x) x$means) 

      ) #time
      names(df) <- paste0("ch", 3:6) 
        
      cat("Time elapsed          : ", tt[[3]], "\n")
      cat("System time : ", format(Sys.time(), "%M:%M \n"))
      cat("Writing feature       : ", "palarm", "\n")
      write_rds(df, fname)
    }
  }
}


logfile <- paste0(logdir, format(Sys.time(), "log_%m-%d-%Y_%H:%M.txt"))
sink(logfile  , append = FALSE, split = TRUE)
cat(format(Sys.time(), "Date            :  %x   \n"))
cat(format(Sys.time(), "Start time      :  %H:%M   \n"))