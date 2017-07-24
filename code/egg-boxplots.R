##@knitr beeboxplots
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
names(chdf[[1]])[colnums]

# Add response column
response <- rep(df$response, each = 120)
response_id <- as.factor(c(1,1,1,1,1,2,3,1,
                    1,1,1,1,1,1,1,1,1,1,1,1,1,
                    2,3,3,2,3,3,2,3,3))

 
boxplot_panel(chdf[[1]], response, cex = 0.3)
 
boxplot_panel(chdf[[2]], response, cex = 0.3)
 
boxplot_panel(chdf[[3]], response, cex = 0.3)

dev.off()

# @knitr save-boxplots
pdf(file.path(getwd(), paste0("figures/", prefix, "-boxplot1.pdf")), 
    width = 9, height = 4)
boxplot_panel(chdf[[1]], response, cex = 0.3)
dev.off()
 
#----------------------------------------------------------
pdf(file.path(getwd(), paste0("figures/", prefix, "-boxplotch2.pdf")), 
    width = 9, height = 4)
boxplot_panel(chdf[[2]], response, cex = 0.3)
dev.off()
 
#----------------------------------------------------------
pdf(file.path(getwd(), paste0("figures/", prefix, "-boxplot3.pdf")), 
    width = 9, height = 4)
boxplot_panel(chdf[[3]], response, cex = 0.3)
dev.off()

palette("default")


# boxplot_panel(chdf[[4]], response, cex = 0.3)

# boxplot_panel(chdf[[5]], response, cex = 0.3)

# boxplot_panel(chdf[[6]], response, cex = 0.3)
scatterplot_features("Theta", "Gamma", chdf[[1]], response_id, cex = 1)
# ###**Channel 1 Gamma - Entropy#**
scatterplot_features("Entropy", "Gamma", chdf[[1]], response_id, cex =1)

# Channel 3 
# **Channel 3 Gamma - Beta#**
scatterplot_features("Beta", "Gamma", chdf[[3]], response_id, cex=1)

# ## Channel 5 
# #**Channel 5 Gamma - Beta#**
scatterplot_features("Beta", "Gamma", chdf[[5]], response_id,cex = 1)
# #**Channel 5 Gamma - Theta#**

scatterplot_features("Theta", "Gamma", chdf[[5]], response_id, cex = 1)






