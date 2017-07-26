library(ecomplex)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(tssims) 
library(tsfeats)

if(!exists("from_cache")){
  from_cache = TRUE
}
set.seed(2017)
len = 500
reps = 30
prefix = "arma"

alpha <- seq(-0.5, 0.5, length.out = 10) 
ab <- expand.grid(alpha,alpha)
ind <- sort(sample(1:100, 20))


if(!from_cache){
  ws <- apply(ab[ind,], 1, function(x) tssims::arma(ar = x , ma = x ))
  xs <- replicate(reps, sapply(ws, function(w) tssims::gen(w)(len)))

  emc <- mc_features(xs, ecomp_cspline)
  fdmc <- mc_features(xs, fd_variogram)
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

  emean <- mc_stat(emc, mean)
  varmean  <- mc_stat(emc, var)
  fdmean <- mc_stat(fdmc, mean) 
  hmean <- mc_stat(hmc, mean)
}
 
cols = c(22, 25, 29)

coeffA <- lapply(emc, function(x) x[,1]) %>% 
          do.call(cbind, .) %>%
          data.frame

coeffB <- lapply(emc, function(x) x[,2]) %>% 
          do.call(cbind, .) %>%
          data.frame

coeffFD <- lapply(fdmc, function(x) x[,1]) %>%          
           do.call(cbind, .) %>%
           data.frame

# colnames(coeffA) <- paste0(rep(round(alpha, 2), a 20))

# boxplot(coeffA[, 1:10])
# coeffA <- lapply(emc, function(x) x[,1]) 
# vioplot(coeffA[,1], coeffA[,2], coeffA[,3], coeffA[,4], col = "gray70")
pdf(file.path(getwd(), paste0("figures/", prefix, "-boxplots.pdf")), 
    width = 9, height = 4)
par(mfrow = c(1, 3))
boxplot(coeffA, xlab = "Alpha", ylab = "A", 
                        legend = c("alpha1", "alpha2"), 
                        fill = c("yellow", "pink4"))
boxplot(coeffB, xlab = "Alpha", ylab = "B")
boxplot(coeffFD, xlab = "Alpha", ylab = "Fractal Dimension")
dev.off()

par(mfrow = c(2, 5))
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

