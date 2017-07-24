## @knitr cauchy
library(ecomplex)
library(dplyr)
library(ggplot2)
library(ggthemes)

seed = 2017
from_cache = TRUE
len <- 150
reps <- 50
prefix <- "cauchy"
set.seed(seed)

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

comps <- lapply(xs, function(x) tsfeats::get_features(x, tsfeats::ecomp_cspline)) %>% 
         do.call(rbind, .) %>% round(., 2)


names(xs) <- paste0("Complexity 'B'=", comps[,2], " , ", "Hurst=", round(fh[,2], 2))
plot_ts(xs, ncol = 3)
