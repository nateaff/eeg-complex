## @knitr cauchy-coefficients
library(ecomplex)
library(dplyr)
library(ecomplex)
library(tsfeats)

seed = 2017
from_cache = TRUE
len <- 500
reps <- 30
prefix <- "cauchy"
set.seed(seed)

alpha <- seq(0.01, 1.99, length.out = 10) 
beta <- seq(0.01, 1.5, length.out = 10)
ab <- expand.grid(alpha, beta)

if(!from_cache){
  ws <- apply(ab, 1, function(x) tssims::cauchy(alpha = x[1], beta = x[2]))
  xs <- replicate(reps, sapply(ws, function(w) tssims::gen(w)(len)))

  emc <- mc_features(xs, tsfeats::ecomp_cspline)
  fdmc <- mc_features(xs, tsfeats::fd_variogram)
  hmc <- mc_features(xs, tsfeats::hurst_pracma)

  save_cache(emc, "ecoeff", prefix) 
  save_cache(fdmc, "fd", prefix) 
  save_cache(hmc, "hurst", prefix) 
  save_cache(xs, "xs", prefix)
} else{
  xs  <- readRDS(cache_file("xs", prefix)) 
  emc<- readRDS(cache_file("ecoeff", prefix))
  fdmc <- readRDS(cache_file("fd", prefix))
  hmc <- readRDS(cache_file("hurst", prefix))
}

emean <- mc_stat(emc, mean)
varmean  <- mc_stat(emc, var)
fdmean <- mc_stat(fdmc, mean) 
hmean <- mc_stat(hmc, mean)
 
cols = c(22, 25, 29)

# Plot a single example from fbm with different parameters

# pdf(file.path(getwd(), paste0("figures/", prefix, "-plot.pdf")), 
    # width = 5, height = 4)
# plot.ts(xs[, cols, 1], main = "Cauchy process")
# dev.off() 

coeffA <- lapply(emc, function(x) x[,1]) %>% 
          do.call(cbind, .) %>%
          data.frame

coeffB <- lapply(emc, function(x) x[,2]) %>% 
          do.call(cbind, .) %>%
          data.frame

coeffFD <- lapply(fdmc, function(x) x[,1]) %>%          
           do.call(cbind, .) %>%
           data.frame

colnames(coeffA) <- paste0(rep(round(alpha, 2), 10))
colnames(coeffB) <- paste0(rep(round(alpha, 2), 10))
colnames(coeffFD) <- paste0(rep(round(alpha, 2),10))


ab_df  <- cbind(emean, ab, fdmean, hmean, varmean)
names(ab_df) <- c("A", "B", "Alpha", "Beta", "fd", "hurst")

# pdf(file.path(getwd(), paste0("figures/", prefix, "alpha-scatterplots.pdf")), 
    # width = 9, height = 4)
par(mfrow = c(1, 3))
with(ab_df, plot(Alpha, A,  col = eegpalette(1)[1], cex = 1.2))
with(ab_df, plot(Alpha, B,  col = eegpalette(1)[2], cex = 1.2))
with(ab_df, plot(Alpha, fd, ylab = "Fractal Dimension", col = eegpalette(1)[3], cex = 1.2))
# dev.off()


# pdf(file.path(getwd(), paste0("figures/", prefix, "beta-scatterplots.pdf")), 
#     width = 9, height = 4)
par(mfrow = c(1, 3))
with(ab_df, plot(Beta, A,  col = eegpalette(1)[1]) )
with(ab_df, plot(Beta, B,  col = eegpalette(1)[2]) )
with(ab_df, plot(Beta, fd, ylab = "Fractal Dimension", col = eegpalette(1)[3]) )
# dev.off() 


## @knitr cauchy-save

if(!from_cache){

  # Plot a single example from fbm with different parameters
  pdf(file.path(getwd(), paste0("figures/", prefix, "-plot.pdf")), 
      width = 5, height = 4)
  plot.ts(xs[, cols, 1], main = "Cauchy process")
  dev.off()
  pdf(file.path(getwd(), paste0("figures/", prefix, "-plot.pdf")), 
      width = 5, height = 4)
  plot.ts(xs[, cols, 1], main = "Cauchy process")
  dev.off()

  coeffA <- lapply(emc, function(x) x[,1]) %>% 
            do.call(cbind, .) %>%
            data.frame

  coeffB <- lapply(emc, function(x) x[,2]) %>% 
            do.call(cbind, .) %>%
            data.frame

  coeffFD <- lapply(fdmc, function(x) x[,1]) %>%          
             do.call(cbind, .) %>%
             data.frame

  colnames(coeffA) <- paste0(rep(round(alpha, 2), 10))
  colnames(coeffB) <- paste0(rep(round(alpha, 2), 10))
  colnames(coeffFD) <- paste0(rep(round(alpha, 2),10))

  # boxplot(coeffA[, 1:10])
  # coeffA <- lapply(emc, function(x) x[,1]) 
  # vioplot(coeffA[,1], coeffA[,2], coeffA[,3], coeffA[,4], col = "gray70")
  pdf(file.path(getwd(), paste0("figures/", prefix, "-boxplots.pdf")), 
      width = 9, height = 4)
  par(mfrow = c(1, 3))
  boxplot(coeffA[, 61:70], xlab = "Alpha", ylab = "A", 
                          legend = c("alpha1", "alpha2"), 
                          fill = c("yellow", "pink4"))
  boxplot(coeffB[, 61:70], xlab = "Alpha", ylab = "B")
  boxplot(coeffFD[, 61:70], xlab = "Alpha", ylab = "Fractal Dimension")
  dev.off()

  par(mfrow = c(1, 5))
  cols <- list(1:10, 21:30, 41:50, 61:70, 81:90)
  for(k in seq_along(cols)){
  boxplot(coeffB[, cols[[k]]], xlab = "Alpha", ylab = "A", 
                          legend = c("alpha1", "alpha2"), 
                          fill = c("yellow", "pink4"))
  }

  par(mfrow = c(1,1))

  ab_df  <- cbind(emean, ab, fdmean, hmean, varmean)
  names(ab_df) <- c("A", "B", "Alpha", "Beta", "fd", "hurst")

  # with(ab_df, plot(B, A,  col = "gray20") )

  pdf(file.path(getwd(), paste0("figures/", prefix, "alpha-scatterplots.pdf")), 
      width = 9, height = 4)
  par(mfrow = c(1, 3))
  with(ab_df, plot(Alpha, A,  col = "gray20" , cex = 1.2))
  with(ab_df, plot(Alpha, B,  col = "gray20" , cex = 1.2))
  with(ab_df, plot(Alpha, fd, ylab = "Fractal Dimension", col = "gray20", cex = 1.2))
  dev.off()


  pdf(file.path(getwd(), paste0("figures/", prefix, "beta-scatterplots.pdf")), 
      width = 9, height = 4)
  par(mfrow = c(1, 3))
  with(ab_df, plot(Beta, A,  col = "gray20") )
  with(ab_df, plot(Beta, B,  col = "gray20") )
  with(ab_df, plot(Beta, fd, ylab = "Fractal Dimension",  col = "gray20") )
  dev.off() 


}

