## @knitr sim-example-create

library(ggplot2)
library(dplyr)
library(ggthemes)
library(tssims) 
library(ecomplex) 
library(tsfeats) 
library(fArma)

# Generate simulations
devtools::load_all()
prefix = "sim"
seed = 1

set.seed(seed)
len <- 200  
gs <- tssims::sim_suite()
gs  <- lapply(gs, function(x) lapply(x, tssims::jitter_params))
ys1 <- lapply(gs$group1, function(mod) tssims::gen(mod)(len))
ys2 <- lapply(gs$group2, function(mod) tssims::gen(mod)(len))

df <- data.frame(do.call(cbind, c(ys1, ys2)) %>% apply(., 2, normalize))
names(df) <- tssims::sim_names()
names(df) <- c("ARMA 1", "Logistic 1", "Weierstrass 1", "Cauchy 1", "FARIMA 1", "FBM 1",
              "ARMA 2", "Logistic 2", "Weierstrass 2", "Cauchy 2", "FARIMA 2", "FBM 2") 

palette(eegpalette()[c(1,3,4)])

nsims <- length(names(df))
id <- factor(rep(rep(c(1,2), each = 200), 6))

dfm <- df  %>%  tidyr::gather(variable, value) 
dfm$variable <- factor(dfm$variable)
gp <- ggplot(dfm, aes(x = rep((1:len), nsims), y = value)) + 
              labs(x = "", y = " ")

gp + geom_line(size = 0.6, alpha = 0.85, colour = id) +
     facet_wrap(~variable, scales = "free_y", ncol = 2) +
     # scale_color_manual(values = eegpalette()[c(1,3)]) +
     theme_minimal2() 

save_plot("jitter_timeseries", prefix)


## @knitr sim-example-plot
# Plot example fit for each function

plot_fit <- function(res, fname){
  # cat(names(res), "\n")
  xlab = "log( S )"; ylab = "log( epsilons )"
  plot(log(res$S), log(res$epsilons), pch = 2, ylab = ylab, xlab = xlab, 
    main = fname)
  # title = fname
  abline(res$fit, lwd = 1.5, col = eegpalette()[3])
}

# pdf(file.path(getwd(), paste0("figures/", prefix, "-fits.pdf")), 
#     width = 8, height = 6)

par(mfrow = c(2,3))
fits <- apply(df[1:6], 2, ecomplex::ecomplex, method = "lift") 
names(fits) <- c("ARMA", "Logistic", "Weierstrass", "Cauchy", "FARIMA", "FBM")
out <- Map(plot_fit, fits, names(fits))

# dev.off()



