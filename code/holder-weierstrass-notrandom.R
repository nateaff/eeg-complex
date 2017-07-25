## @knitr weierstrass-notrandom-plot
# Weierstrass function without random phase
library(dplyr)

prefix = "holder_notrandom"
len = 1000
from_cache = TRUE
set.seed(seed)
alpha <- seq(0.1, 0.99, length.out = 20)
reps = 1
if(from_cache) {
  xs     <- readRDS(cache_file("weierstrass_xs", prefix)) 
  emeans <- readRDS(cache_file("weierstrass_ecomp", prefix))
  emc    <- readRDS(cache_file("weierstrass_emc", prefix))
  fdmc    <- readRDS(cache_file("weierstrass_fdmc", prefix))
  fdmeans    <- readRDS(cache_file("weierstrass_fdmean", prefix))

} else {
  # generate set of weierstrass functions for each alpha
  ws <- lapply(alpha, function(x) tssims::weierstrass(a = x, random = FALSE))
  xs <- replicate(reps, sapply(ws, function(w) tssims::gen(w)(1000)))
  xs <- xs[3:997, , ]
  # takes 3 dimensional array, with the third dimension the parameter
  ecomp <-  apply(xs, 2, ecomp_cspline)
  fd <- apply(xs, 2, fd_variogram)
  emeans <- lapply(ecomp, function(x) data.frame(A = x$A, B = x$B)) %>% 
            do.call(rbind, .)
  fdmeans <- lapply(fd, function(x) data.frame(fd = x$fd)) %>% 
            do.call(rbind, .)

  emeans$alpha <- alpha
  names(emeans) <- c("A", "B", "alpha")
  save_cache(xs, "weierstrass_xs", prefix)
  save_cache(emeans, "weierstrass_ecomp", prefix)
  save_cache(emc, "weierstrass_emc", prefix)
  save_cache(fdmeans, "weierstrass_fdmean", prefix)
  save_cache(fdmc, "weierstrass_fdmc", prefix)
}

# Plot time series and boxplots of coeffcients
cols <- seq(1, 19, by = 2)
cols = c(3, 8, 16)
# alpha[cols]

# Plot a single example from fbm with different parameters
plot.ts(xs[,cols], main = "Weierstrass function", col = viridis::viridis(30)[3])


## @knitr holder-coefficients
par(mfrow = c(1, 3))
plot(alpha[1:19],  emeans[1:19, 1], 
   xlab = "Alpha", 
     ylab = "A", 
     cex = 1.2,
     col = eegpalette(1)[1]
     )

plot(alpha[1:19], emeans[1:19, 2], 
     xlab = "Alpha", 
     ylab = "B", 
     cex = 1.3,
     col = eegpalette(1)[2]
     )
plot(alpha[1:19], fdmeans[1:19, 1], 
     xlab = "Alpha", 
     ylab = "Fractal Dimension", 
     cex = 1.3, 
     col = eegpalette(1)[3]
     )

## @knitr weierstrass-save-plots
# repeat plots if not from cache
if(!from_cache){
pdf(file.path(getwd(), paste0("figures/", prefix, "-param-plots-notrandom.pdf")), 
    width = 9, height = 4)
par(mfrow = c(1, 3))
plot(alpha[1:19],  emeans[1:19, 1], 
   xlab = "Alpha", 
     ylab = "A", 
     cex = 1.2, 
     col = eegpalette(1)[1]
     # pch = 16, 
     # col = adjustcolor("gray10", 0.8)
     )

plot(alpha[1:19], emeans[1:19, 2], 
     xlab = "Alpha", 
     ylab = "B", 
     cex = 1.3,
     col = eegpalette(1)[2] 
     # pch = 16, 
     # col = adjustcolor("gray10", 0.8)
     )
plot(alpha[1:19], fdmeans[1:19, 1], 
     xlab = "Alpha", 
     ylab = "Fractal Dimension", 
     cex = 1.3,
     col = eegpalette(1)[3] 
     # pch = 16, 
     # col = adjustcolor("gray10", 0.8)
     )
dev.off()

pdf(file.path(getwd(), paste0("figures/", prefix, "-weier-notrandom.pdf")))
plot.ts(xs[,cols], main = "Weierstrass function")
dev.off()

}

