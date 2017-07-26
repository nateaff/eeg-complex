#----------------------------------------------------------
# The stimplus files have an extra few seconds after 
# the application of the stimulus Stimulus times were 
# hand based on video. This script was used to
# verify visually that the stimulus times were correct.
#----------------------------------------------------------
stimdf <- readRDS(cache_file("windows2", "meta")) 

data_dir <- "~/data/eeg/ucsf_stimplus"
EEEG <- "EEG"
LFFP <- "LFP"

seg = "SEG1"
fEEG <- paste0(data_dir, "/", stimdf$id, "_", stimdf$trial_name, "_", seg, "_", EEEG, 
    ".txt")

fLFP <- paste0(data_dir, "/", stimdf$id, "_", stimdf$trial_name, "_", LFFP, ".txt")

#----------------------------------------------------------
# Test load data
#----------------------------------------------------------

t1 <- data.matrix(data.table::fread(fEEG[1]))
t1 <- t(t1)

save_cache(t1[, 250000:310000], "eegsnip", prefix = "eeg")
