library(ggplot2)
library(ggthemes)
library(dplyr)
library(ecomplex)
library(tsfeats)
library(fArma)
#----------------------------------------------------------
# Fractional Brownian Motion
# --------------------------
# Do a similar analysis as above but for fractional
# Brownian motion with H parameter in (0,1). The 
# Holder exponent is proportional to the Hurst 
# exponent. See Nualart, "Fractional Brownian motion: 
# stochastic calculus and applications."
#----------------------------------------------------------
seed = 2017
from_cache = TRUE
prefix <- "fBm-coeffs"
set.seed(seed)
alpha <- seq(0.1, 0.99, length.out = 20)

if(from_cache){
  xs <- readRDS(cache_file("xs", prefix))
  emc  <- readRDS(cache_file("ecomp_mc", prefix))
  emeans <-  readRDS(cache_file("ecomp_mean", prefix))
  hmeans <-  readRDS(cache_file("hurst_mean", prefix))
  fdmean <-  readRDS(cache_file("fdmean", prefix))
  fdmc <-  readRDS(cache_file("fdmc", prefix))
  
} else {
  set.seed(seed)
  # create random weierstrass functions for each alpha 
  fbms <- lapply(alpha, tssims::fBm) 
  xs <- replicate(50, sapply(fbms, function(f) tssims::gen(f)(1000)))
  # list of df frames, each represnting a paramter set
  emc <- mc_features(xs, ecomp_cspline)
  hurst_mc <- mc_features(xs, hurst)
  fdmc <- mc_features(xs, fd_variogram)

  emeans <- mc_stat(emc, mean)
  hmeans <- mc_stat(hurst_mc, mean)
  fdmeans <- mc_stat(fdmc, mean)
  emeans$alpha <- alpha
  names(emeans) <- c("A", "B", "alpha")

  if(!from_cache){
    # save_plot("coefficients", prefix)
    save_cache(xs, "xs", prefix)
    save_cache(emc, "emc", prefix)
    save_cache(emeans, "ecomp_mean", prefix)
    save_cache(hmeans, "hurst_mean", prefix)
    save_cache(fdmeans, "fdmean", prefix)
    save_cache(fdmc, "fdmc", prefix)
  }
}

 
#----------------------------------------------------------
# Boxplot of coefficients 
#----------------------------------------------------------

# Plot a single example from fbm with different parameters
plot.ts(xs[, cols, 1])

coeffA <- lapply(emc, function(x) x[,1]) %>% 
          do.call(cbind, .) %>%
          data.frame

coeffB <- lapply(emc, function(x) x[,2]) %>%
          do.call(cbind, .) %>%
          data.frame

coeffFD <- lapply(fdmc, function(x) x[,1]) %>%          
           do.call(cbind, .) %>%
           data.frame

colnames(coeffA) <- paste0(round(alpha, 2))
colnames(coeffB) <- paste0(round(alpha, 2))
colnames(coeffFD) <- paste0(round(alpha, 2))

# coeffA <- lapply(emc, function(x) x[,1]) 
# vioplot(coeffA[,1], coeffA[,2], coeffA[,3], coeffA[,4], col = "gray70")
pdf(file.path(getwd(), paste0("figures/", prefix, "-boxplots.pdf")), 
    width = 9, height = 4)
par(mfrow = c(1, 3))
boxplot(coeffA[, 1:19], xlab = "Alpha", ylab = "A", 
                        legend = c("alpha1", "alpha2"), 
                        fill = c("yellow", "pink4"))
boxplot(coeffB[, 1:19], xlab = "Alpha", ylab = "B")
boxplot(coeffFD[, 1:19], xlab = "Alpha", ylab = "Fractal Dimension")
dev.off()


 
#----------------------------------------------------------
# Time series plot 
#----------------------------------------------------------
if(from_cache){
  evarmc <- mc_features(xs, mean)
  evar <- mc_stat(evarmc, mean) 
  evar$H <- alpha 
  names(evar) <- c("variance", "H") 
}

with(evar, plot(H, variance))
df <- evar 

# cols <- seq(1, 19, by = 2)
cols = c(3, 8, 16)
# alpha[cols]
pdf(file.path(getwd(), paste0("figures/", prefix, "-plot.pdf")), 
    width = 5, height = 4)
plot.ts(xs[, cols, 1], main = "Fractional Brownian Motion")
dev.off()

names(df) <- c("y", "x")
plot_regression(df, xlab ="H", ylab = "Variance", title ="")     
#----------------------------------------------------------
# TODO : fix labels
#----------------------------------------------------------
gp <- ggplot() +
      theme_base() + 
      scale_x_continuous(name="log( S )", limits=c(0, 2)) +
      scale_y_continuous(name="log( epsilons )", limits=c(-15, -4)) + 
      # labs(x = "log(epsilon)",  y = "log(S)") +
      geom_abline(data =  emeans, 
        aes(intercept = A, slope = B, col = alpha))
      # scale_color_manual()
gp

save_plot("fbm_ecomp_fits", prefix)
#----------------------------------------------------------
# Regression plot
#----------------------------------------------------------
hmeans$H <- Hs
df <- hmeans
names(df) <- c("y", "x")
plot_regression(df, xlab = "H parameter", 
                    ylab = "Hurst estimate", 
                    title = "")



 



