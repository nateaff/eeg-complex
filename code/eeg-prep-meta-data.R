rm(list = ls())
library(readr)
library(jsonlite)
library(tidyverse)
library(data.table)

prefix <- "eeg_process_metadata"

#----------------------------------------------------------
# This file generates the
#----------------------------------------------------------
to_mins <- function(s) {
    m <- s%/%60
    s <- s%%60
    sprintf("%d:%02d", m, s)
    
}

to_secs <- function(m, s = 0) {
    60 * m + s
}

json <- read_file(cache_file("meta", prefix = "eeg", ext = ".json"))

# read json
validate(json)
meta <- fromJSON(json)
ids <- meta$id
trials <- meta$trials
stims <- lapply(trials, function(x) x$stims)

# create data.frame
metadf <- data.frame()
for (k in seq_along(ids)) {
    temp <- do.call(rbind, stims[[k]])
    id <- rep(ids[[k]], dim(temp)[1])
    temp$id <- id
    metadf <- rbind(metadf, temp)
}
stopifnot(all(!is.na(metadf)))

summary(metadf)

# Look at number of trials, stimuli and response
ntrials <- dim(metadf)[1]
N <- subset(metadf, response == 1) %>% count()
nstims <- metadf %>% dplyr::filter(stim == TRUE) %>% count()
cat(sprintf("%d trials, %d with a stimulus and %d with a response \n", 
             ntrials, nstims[[1]], 
    N[[1]]))


# Not correct last time of previous trial is later than first time of next.
start_gaps <- function(x) {
    ifelse(x != 1 && metadf$stim_start[x] > metadf$stim_start[x - 1], 
        gap <- metadf$stim_start[x] - 
        metadf$stim_start[x - 1], gap <- metadf$stim_start[x])
}

# metadf$end_gap <- unlist(lapply(1:(dim(metadf)[1]), end_gaps))

metadf$stim_gap <- unlist(lapply(1:(dim(metadf)[1]), start_gaps))
metadf %>% dplyr::select(trial_name, stim_start, stim_gap)
stopifnot(all(!is.na(metadf$stim_gap)))

# window to use for trial stimulus
metadf <- mutate(metadf, window = ifelse(stim_gap %in% 1:119, 0, 
                                  ifelse(stim_gap %in% 120:240,  stim_gap, 
                                 ifelse(stim_gap == 0, 240, 240))))

# trials with no stimulus
nodf <- metadf %>% dplyr::filter(stim == FALSE)
stimdf <- metadf %>% dplyr::filter(stim == TRUE) %>% 
          mutate(win_start = stim_start - window)

 
#----------------------------------------------------------
# Intermediate save 
#----------------------------------------------------------
# tempstimdf <- stimdf %>% dplyr::filter(window > 239)
# # write_tsv(wins, save_dir)
# write_csv(tempstimdf, "inst/extdata/trials_with_stim.csv")

# check window length stimdf$stim_start - stimdf$win_start

wins <- stimdf %>% dplyr::filter(stim == TRUE, window > 119) %>% 
                   dplyr::select(win_start, stim_start)

winnames <- stimdf %>% 
            dplyr::filter(stim == TRUE, window > 119) %>% 
            dplyr::select(id, trial_name, stim_num, response, window)

 
#----------------------------------------------------------
# Second version used for making plot of full trials
# used in classification
#----------------------------------------------------------
win2 <- stimdf %>% 
            dplyr::filter(stim == TRUE, window >= 119) %>% 
            dplyr::select(id, trial_name, stim_num, stim_start, 
                          stim_end, response, window) 
            
win2 <- win2 %>%    dplyr::mutate(key = paste0(id, trial_name)) %>%
                 mutate(col_key = ifelse(window < 238, 4, 
                          ifelse(response == FALSE, 1, 
                          ifelse(stim_num == 1, 2, 3))))

save_cache(win2, "windows2", "meta") 
#----------------------------------------------------------

save_dir <- "~/data/eeg/windows.txt"
save_names <- "~/data/eeg/winnames.txt"

# write_tsv(wins, save_dir)
write_tsv(winnames, save_names)


#----------------------------------------------------------
# Adding windows after stims
#----------------------------------------------------------
enddf <- stimdf %>% dplyr::filter(., stim_gap > 240, response == FALSE)

end_gaps <- function(x) {
    ifelse(stimdf$stim_end[x + 1] > stimdf$stim_end[x], gap <- stimdf$stim_start[x + 
        1] - stimdf$stim_end[x], 0)
}
stimdf$end_gap <- unlist(lapply(1:(dim(stimdf)[1]), end_gaps))

stimdf %>% dplyr::filter(., response == FALSE, end_gap > 360)











