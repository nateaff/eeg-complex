##@knitr eeg-boxplotpanel-ch1

library(dplyr) 
library(viridis)
prefix     = "eeg"
seed       = 2017
chs        = 1:6
segch      = 4
from_cache = TRUE
window     = 240 
len        = window/2
colnums    = c(5:9, 13:15, 21, 19)
 

feature_df <-readRDS(cache_file("mod_all_features", prefix))
trial_df   <- readRDS(cache_file("mod_trial_segments", "eeg"))

df <- do.call(rbind, feature_df)
df <- df %>% cbind(trial_df) 
df <- filter(df, window == 240)

palettte <- eegpalette(0.9)[c(1,3)]
# Combine all observation for each channel 
# Returns list of 6 data frames, one for each channel
chdf <- apply(df[1:6], 2, function(x) do.call(rbind, x))
colnums <- c(5:9, 13:15)
# names(chdf[[1]])[colnums]

# Add response column
response <- rep(df$response, each = 120)
response_id <- as.factor(c(1,1,1,1,1,2,3,1,
                    1,1,1,1,1,1,1,1,1,1,1,1,1,
                    2,3,3,2,3,3,2,3,3))

 
boxplot_panel(chdf[[1]], response, cex = 0.3)

##@knitr eeg-boxplotpanel-ch3
boxplot_panel(chdf[[3]], response, cex = 0.3)
