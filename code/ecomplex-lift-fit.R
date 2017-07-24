##@knitr lift-fit

library(ecomplex)
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(tssims) 
library(fArma)

prefix = "ecomplex_mae"
from_cache = TRUE
len = 500

fnames <- c("ARMA", "Logistic", "Weierstrass", "Cauchy", "FARIMA", "fBm")


plot_fit <- function(res, title){
  # cat(names(res), "\n")
  ylab = "log( S )"; xlab = "log( epsilons )"
  plot(res$x, res$y, pch = 2, ylab = ylab, xlab = xlab, main = title)
  fit = lm(y ~ x, data = res)
  abline(fit, col = eegpalette(1)[3])
}

len = 250
gs <- tssims::sim_suite()
fits1   <- lapply(gs$group1, function(g) gen(g)(len)) %>%
           lapply(., function(x) ecomplex::ecomplex(x, method = "lift")) 
eps <- lapply(fits1, function(x) data.frame(x = x$S, y = x$epsilons))


# pdf(file.path(getwd(), paste0("figures/", prefix, "lift-lm.pdf")), 
    # width = 8, height = 6)
par(mfrow = c(2,3))
for(x in 1:6) plot_fit(eps[[x]], title = fnames[[x]])

# dev.off()

 
  