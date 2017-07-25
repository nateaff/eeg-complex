## @knitr eeg-plot

eeg <- readRDS(cache_file("eegsnip", prefix = "eeg"))

# dim(eeg)
fs <- 1220.703

plot_range <- function(a,b){
  oldpar = par() 
  par(mfrow = c(6,  1), mar = c(1, 1, 1, 1))
  for(k in 1:6){
   plot(eeg[k, a:b], 
         type = 'l', 
         col = "gray30", 
         lwd = 1.5,
         axes = FALSE,
         frame.plot = FALSE)
  }
  
}

secs = 8
a = 37000
b = a + fs*secs
plot_range(a,b)

