## @knitr eeg-segment-plot
library(ggplot2)
library(dplyr)
library(viridis)
library(ecomplex)
prefix    = "eeg"
seed      = 2017

# Derived features. 30 Trials : 6 channels : 15 features
feature_df <-readRDS(cache_file("mod_all_features", prefix))
# Load meta data 
trial_df <- readRDS(cache_file("mod_trial_segments", "eeg"))

df <- do.call(rbind, feature_df) %>% 
      cbind(., trial_df) %>% 
      dplyr::filter(., window == 240)

df$trial <- 1:26

ch = 1; colnums = c(5:9,  13:15); tnum = 8

dfch <- df[[ch]][[tnum]]
res <- palarm(dfch$ecomp.cspline_B)

seg_plot <- function(startn, stopn, title =""){
  segcol = adjustcolor("tomato3", 0.9)
  pal  = c("gray20", segcol, viridis::viridis(256)[c(1, 30, 60, 90, 120)])

  plot(dfch$ecomp.cspline_B + 3, 
                  type = 'l',  
                  col = adjustcolor("gray20", 0.6), 
                  lwd = 2,
                  lty = 1, 
                  ylim = c(0,2.2), 
                  xlim = c(0,120), 
                  xlab = "Time",
                  yaxt = "n", 
                  ylab = "", 
                  main = title)
  lines(res$means + 3, type = 'l', xlim=c(0,120), col=segcol, lwd=2, lty= 1 )
  for(k in startn:stopn) lines(dfch[ , 4 + k] + 0.5*(k-startn), 
                     lwd = 2, 
                     col = pal[k + 2])
  for(k in res$kout) abline(v = k, col = segcol, lwd = 1.5, lty = 2)
}

# pdf(file.path(getwd(), paste0("figures/", prefix, "-segment-plot.pdf")), 
#     width = 9, height = 4)

par(mfrow = c(1,2), mar = c(2,1,2,1))
seg_plot(1,2, "Segmentation of Delta, Theta")
seg_plot(3,5, "Segmentation of Alpha, Beta, Gamma")

# dev.off() 