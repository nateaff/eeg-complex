## @knitr cauchy-acf
library(ecomplex)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(tssims) 

if(!exists("from_cache")){
  from_cache = TRUE
}
  
set.seed(2017)
len <- 150
reps <- 50
prefix <- "cauchy"

alpha <- seq(0.1, 1.3, length.out = 3) 
beta <- seq(0.01, .7, length.out = 3)

alpha_to_fd <- function(alpha) alpha + 1 - alpha/2
beta_to_hurst <- function(beta) 1 - beta/2

ab <- expand.grid(alpha, beta)
fh <- cbind(alpha_to_fd(ab[,1]), beta_to_hurst(ab[,2]))
names(ab) <- c("alpha", "beta")
names(fh) <- c("Fractal_dim", "Hurst")


ws <- apply(ab, 1, function(x) tssims::cauchy(alpha = x[1], beta = x[2]))
xs <- lapply(ws, function(w) tssims::gen(w)(len)) %>% 
      do.call(cbind, .) %>% 
      data.frame

acfs <- lapply(xs, acf, plot = FALSE)
par(mfrow = c(3,3))
# lapply(1:9, function(x) plot(acfs[[x]], 
#   main = paste0("alpha=", ab[k,1], " beta=", ab[k,2])))

for(k in 1:9) {
  stemplot(acfs[[k]]$lag, pch = '.', acfs[[k]]$acf, 
    ylab = "ACF", xlab = "Lag", 
    main = paste0("alpha=", ab[k,1], " beta=", ab[k,2]))
}
