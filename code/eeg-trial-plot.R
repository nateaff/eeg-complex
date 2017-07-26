##@knitr trialplot
library(dplyr)

if(!exists("from_cache")){
  from_cache = TRUE
}
plot_trial <- function(df_){
  plot( 
        y,  
        xlim = c(0, max_end), 
        ylim = c(-0.5, 0.5),
        lwd = 0.2,
        lty = 3, 
        col = "gray40",
        type = 'l', 
        xlab = '', 
        ylab = '', 
        yaxt = 'n',
        xaxt = 'n'
        )
  
  for(k in (1:dim(df_)[1])){         
         
          rcol <- keycols[df_$col_key[k]]
          if(df_$col_key[k]!=4){
          count <<- count + 1
          rect((
                df_$stim_start[k]-240), 
                -0.5, 
                df_$stim_start[k], 
                0.5, 
                col = rcol,   
                border = "transparent")
                text(df_$stim_start[k]-120, 0, paste0(count),
                cex = 1.3, 
                col = "gray90"
               )
        }

      abline(v = (df_$stim_start[k]+1), col = "chartreuse3", lwd = 3)
  }
 
}
count = 0
 
metadf <- readRDS(cache_file("windows2", "meta"))
# Fix errror: 
metadf$col_key[13] <- 4
trial_keys <- unique(metadf$key)
max_end <- max(metadf$stim_end) 

x <- (0:max_end)
y <- rep(0, length(x))

keycols <- eegpalette()
# pdf(file.path(getwd(), paste0("figures/", prefix, "-param-plots-notrandom.pdf")), 
#     width = 9, height = 4)
par(mfrow = c(8,2), mar = c(1, 0.5, 1, 0.5), cex.axis = 1.4, cex.lab = 1.4)


for(k in 1:length(trial_keys)){
  df_ <-  metadf %>% dplyr::filter(key == trial_keys[k])
  plot_trial(df_)
}

plot(1, type = 'n', axes = "FALSE")
legend(x = "center", inset = 0,
       c("No response", "1st response ", "2nd response"), 
        col = keycols, pch = 15, cex = 1.3, horiz = TRUE) 
        
plot(1, type = 'n', axes = "FALSE")

# dev.off()

## @knitr save-trial-plot
if(!from_cache){
  pdf(file.path(getwd(), paste0("figures/", prefix, "-param-plots-notrandom.pdf")), 
      width = 9, height = 4)

  par(mfrow = c(8,2), mar = c(1, 0.5, 1, 0.5), cex.axis = 1.4, cex.lab = 1.4)


  for(k in 1:length(trial_keys)){
    df_ <-  metadf %>% dplyr::filter(key == trial_keys[k])
    plot_trial(df_)
  }

  plot(1, type = 'n', axes = "FALSE")
  legend(x = "center", inset = 0,
         c("No response", "1st response ", "2nd response"), 
          col = keycols, pch = 15, cex = 1.3, horiz = TRUE) 
          
  plot(1, type = 'n', axes = "FALSE")

  dev.off()
}