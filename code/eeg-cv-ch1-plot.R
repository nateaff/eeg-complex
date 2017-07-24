##@knitr cv-ch1-plot
library(ggplot2)
prefix = "eeg"

palette(eegpalette())

meta_df <- readRDS(cache_file("windows2", "meta"))
predict <- readRDS(cache_file("cv_results_ch_1", "eeg") )
predict_df <- predict$df
colkey <- predict_df$trial_type <- meta_df %>% 
          dplyr::filter(window == 240) %>% 
          dplyr::select(col_key)
colkey <- colkey[[1]]
# colkey[colkey == 3] <- 2

thresholds <- readRDS(cache_file("cvthresholds", prefix))


tnames <- paste0("Trial ", 1:26)
tnames <- factor(tnames, levels = tnames)

names(predict_df) <- c("response","probability", "trialnum", "ground")

# Edited : Seizure vs No seizure
ggplot(predict_df, aes( x = tnames, y = probability)) + 
  geom_point(stat='identity', aes(col=as.factor(colkey)), size= 4) +
  scale_color_manual(name="", 
                     labels = c("No response", "Seizure response", "2nd response"), 
                     values = eegpalette()) +
                     labs(y = "Probability", x = "") +
                     coord_flip() + 
                     geom_hline(yintercept=thresholds[[1]], colour = "chartreuse3") +
                     theme_bw() + 
                     theme(text = element_text(size= 16),
                       axis.text.x = element_text(size = 16),  
                       panel.background = element_rect(fill = "transparent", colour = NA), 
                       # panel.background = element_blank(), 
                      plot.background = element_blank(), 
                      legend.background = element_rect(fill = "transparent", colour = NA))

   

palette("default")