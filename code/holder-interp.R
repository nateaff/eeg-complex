library(dplyr)
library(ggplot2)
library(tsfeats)
#----------------------------------------------------------
# Interpretation of complexity coefficients
# Do not update functions
#----------------------------------------------------------
if(!exists("from_cache")){
  from_cache = TRUE
}

prefix = "coeff-interp"

f1 <- function(x)  x
f2 <- function(x) sin(5 * x) 
f3 <- function(x) sin(5 * x)  + sin(17*x)
f4 <- function(x) sin(5 * x)  + sin(17 * x) + sin(53*x)

len = 1000
noise1 = rnorm(len, 0, 0.01)
noise2 = rnorm(len, 0, 0.03)

funcs <- list(f1, f2, f3, f4)
xx <- seq(0, 5, length.out = len)
yy <- lapply(funcs, function(x) x(xx)) %>% lapply(., ecomplex::normalize)
ymat0 <- do.call(cbind, yy) %>% data.frame 

fvar <- apply(ymat0, 2, var)
noise1 <- sqrt(fvar/20)
noise2 <- sqrt(fvar/5) 
n1 <- lapply(noise1, function(x) rnorm(len, 0, x)) 
n2 <- lapply(noise2, function(x) rnorm(len, 0, x))
ymat1 <- lapply(1:4, function(k) yy[[k]] + n1[[k]]) %>% 
				 do.call(cbind, .) %>% data.frame 
ymat2 <- lapply(1:4, function(k) yy[[k]] + n2[[k]]) %>% 
         do.call(cbind, .) %>% data.frame


#----------------------------------------------------------
# Base time-series plots
#----------------------------------------------------------
pdf(file.path(getwd(), paste0("figures/", prefix, "-simple-functions-comb.pdf")))
# par(mfrow = c(1,2), mar = c(2,1,2,1))
plot.ts(ymat0, main = "Functions without added noise")
dev.off() 
pdf(file.path(getwd(), paste0("figures/", prefix, "-simple-functions1.pdf")))
plot.ts(ymat1, main = "Functions with SNR = 5")
dev.off()
pdf(file.path(getwd(), paste0("figures/", prefix, "-simple-functions2.pdf")))
plot.ts(ymat2, main = "Functions with SNR = 20", axes = FALSE)
dev.off()
 
#----------------------------------------------------------
fnames <- c("y = x", "y = sin(5x)", "y = sin(5x) + sin(17x)", "y = sin(5x) + sin(17x) + sin(53x)" )
fnames <- c("F1", "F2", "F3", "F4")

names(ymat0) <- fnames 
names(ymat1) <- fnames 
names(ymat2) <- fnames 

matlist <- list(ymat0, ymat1, ymat2) 
#----------------------------------------------------------
# Example simple plot
#----------------------------------------------------------
plot_fit <- function(res, title = ""){
	# cat(names(res), "\n")
	ylab = "log( epsilons )"; xlab = "log( S )"
	plot(log(res$S), log(res$epsilons), pch = 2, ylab = ylab, xlab = xlab, 
    main = title)
	abline(res$fit, col = eegpalette(0.9)[3])
  
}

pdf(file.path(getwd(), paste0("figures/", prefix, "-simple-fits.pdf")), 
    width = 8, height = 3)
par(mfrow = c(1,4))
res2 <- apply(ymat0, 2, ecomplex::ecomplex)
lapply(1:4, function(x) plot_fit(res2[[x]], fnames[x]))
dev.off()

#----------------------------------------------------------
# save as simple plots
#----------------------------------------------------------
alldf <- data.frame(ymat0, ymat1, ymat2)
names(alldf) <- NULL
plot_ts(alldf, ncol = 4)
# compare to plot.ts 
# par(mfrow = c(1, 3))

#----------------------------------------------------------
# pdf(file.path(getwd(), paste0("figures/", prefix, "-simple-functions0.pdf")))
# plot_ts(ymat0, ncol = 1)
# dev.off()
# pdf(file.path(getwd(), paste0("figures/", prefix, "-simple-functions1.pdf")))
# plot_ts(ymat1, ncol = 1)
# dev.off()
# pdf(file.path(getwd(), paste0("figures/", prefix, "-simple-functions2.pdf")))
# plot_ts(ymat2, ncol = 1)

 
#----------------------------------------------------------
# Plot all together
#----------------------------------------------------------
ymat_all <- data.frame(ymat0, ymat1, ymat2)
names(ymat_all) <-  paste0(paste0("f", 1:4),  rep(c("_no_noise", "_noise1" , "_noise2"), 4))

pdf(file.path(getwd(), paste0("figures/", prefix, "-simple-functions.pdf")))
plot_ts(ymat_all, ncol = 4 )
dev.off()

mats <- list(ymat0, ymat1, ymat2)
feats <- lapply(mats, 
				 function(mat) get_features(mat, list(ecomp_cspline, fd_variogram, hurst, variance)))
feats <- lapply(feats, function(f) plyr::unrowname(f) )

prefix = "holder_interp"
save_cache(feats, "feature_lits", prefix)
save_cache(mats, "function_output", prefix)
