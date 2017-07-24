library(readr)
library(jsonlite)
library(tidyverse)
library(data.table)

#----------------------------------------------------------
# Plot window around recorded stimulus time. Visual check on stimulus
# window was accurately recorded.  Note: Not run as part of updated
# reproducible scripts.  Requires the raw EEG data files.
#----------------------------------------------------------

# Depends on eeg-trial-segments
prefix <- "eeg_check_segments"

name <- ""
metadf <- readRDS(cache_file(name, "eeg_trial_segments", sep = ""))

# check window length
summary(stimdf)

stim_dir <- "~/data/eeg/ucsf_stim"
after_dir <- "~/data/eeg/ucsf_after_stim"

# TODO: add seg name to files
get_fnames <- function(data_dir, type = "EEG", df) {
    sep <- "_"
    paste0(data_dir, "/", df$id, sep, df$trial_name, sep, "SEG", df$stim_num, 
        sep, type, ".txt")
}

poststim_fnames <- function(data_dir, type = "EEG", df) {
    sep <- "_"
    paste0(data_dir, "/", df$id, sep, df$trial_name, sep, "EEG_SEG", df$stim_num, 
        sep, type, ".txt")
}

# filenames
eegf <- get_fnames(stim_dir, "EEG", stimdf)
lfpf <- get_fnames(stim_dir, "LFFP", stimdf)

# after_eegf <- poststim_fnames(after_dir, 'EEG', stimdf) after_lfpf <-
# poststim_fnames(after_dir, 'LFFP', stimdf)

# examples with full window
select <- stimdf$window >= 120
test_eeg <- eegf[select]
# dim(t1lfp)



image_names <- paste0(stimdf$id, "_", stimdf$trial_name, "_SEG", stimdf$stim_num, 
    ".png")


check_images <- function() {
    for (k in 1:length(test_eeg)) {
        cat("loading...", test_eeg[k], "\n")
        t1 <- t(data.matrix(fread(test_eeg[k])))
        t1lfp <- t(data.matrix(fread(lfpf[k])))
        fs <- 1220.7
        stimdf$window[k] * fs
        dim(t1)
        
        plot.ts(t1[290000:310000, ], main = image_names[k])
        save_plot(image_names[k], prefix)
    }
}

check_images()
