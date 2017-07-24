rm(list = ls())
library(ggplot2) 
#----------------------------------------------------------
# Compare running times for ecomplexity methods
#----------------------------------------------------------
from_cache = TRUE
prefix = "benchmark"
filename = "lift_timing"

if(from_cache){
  time_elapsed <- readRDS(cache_file(filename = filename, prefix = prefix))
} else {

  N <- 3
  # short sequence for b-spline method
  seqs <- lapply(2^(7:11), rnorm)

  time_elapsed <- matrix(0, nrow = 8, ncol = 3)
  for(j in 1:length(seqs)){
  time_elapsed[j, 2]  <- mean(replicate(N, 
                          system.time(ecomplex(seqs[[j]], method = "bspline"))[3]), trim = 0.05)
  }

  # longer sequence 
  seqs <- lapply(2^(7:14), rnorm)
  for(j in 1:length(seqs)){
    time_elapsed[j, 1]  <- mean(replicate(N, 
                          system.time(ecomplex(seqs[[j]], method = "lift"))[3]), trim = 0.05)
    time_elapsed[j, 3]  <- mean(replicate(N, 
                          system.time(ecomplex(seqs[[j]], method = "cspline"))[3]), trim = 0.05)
  }
}

if(!from_cache){
  save_cache(time_elapsed, filename = filename, prefix = prefix)
}

#----------------------------------------------------------
# This shows that the lift method is linear but the 
# cubic spline method is too fast to really be comparable.
#----------------------------------------------------------


pdf(file.path(getwd(), paste0("figures/", prefix, "-benchmark.pdf")), 
    width = 6, height = 4)
par(mfrow = c(1,1))
pnt <- plot((7:13), time_elapsed[1:7, 1], 
                # cex = 0.9,
                pch = 2, 
                col = eegpalette(0.9)[2], 
                ylim = c(0,10), ylab = "seconds", 
                xlab = "log_2 ( N )")
      points((7:11), time_elapsed[1:5, 2], pch = 2, col = eegpalette(0.9)[1])
      points((7:13), time_elapsed[1:7, 3], pch = 2, col = eegpalette(0.9)[3], 
                                            lwd = "1.5")
      lines((7:11), time_elapsed[1:5, 2], col = eegpalette(0.9)[1], lwd = "1.5")
      lines((7:13), time_elapsed[1:7, 1], col = eegpalette(0.9)[2], lwd = "1.5")
      lines((7:13), time_elapsed[1:7, 3], col = eegpalette(0.9)[3], lwd = "1.5")

      legend(11, 9.5, c("B-splines", "Lifting", "Cubic-splines"),
                        lty=c(1,1), 
                        cex = 0.8,
                        lwd=c(2.5, 2.5), 
                        col= eegpalette(0.9))

dev.off()
# save data
