# @ knitr weierstrassplots
library(ecomplex)
library(tssims) 
library(ggplot2)
library(dplyr)
library(tsfeats)
library(ggthemes)
#----------------------------------------------------------
#  Random Weierstrass function
#  ---------------------------
#  Plot ecomplexity fits for sequence of alpha parameter for
#  random Weierstrass function. Parameters are in (0,1) 
#  and correspond to the Holder exponent of the function.
#----------------------------------------------------------
  
if(!exists("from_cache")){
  from_cache = TRUE
}

# With density parameter 250 time series length is 250*2 
len  = 2
reps = 2
prefix = "holder_coeffs"
set.seed(2017)
alpha <- seq(0.1, 0.99, length.out = 20)

if(from_cache) {
  xs     <- readRDS(cache_file("weierstrass_xs", prefix)) 
  emeans <- readRDS(cache_file("weierstrass_ecomp", prefix))
  emc    <- readRDS(cache_file("weierstrass_emc", prefix))
  fdmeans <- readRDS(cache_file("weierstrass_fdmean", prefix))
  fdmc    <- readRDS(cache_file("weierstrass_fdmc", prefix))

} else {
  # generate set of weierstrass functions for each alpha
  ws <- lapply(alpha, function(x) tssims::weierstrass(a = x, c = 3, 
                                                      addnorm = FALSE, 
                                                      random = TRUE, 
                                                      density = 500))
  xs <- replicate(reps, sapply(ws, function(w) tssims::gen(w)(len)))
  xs <- xs[3:997, , ]
  # takes 3 dimensional array, with the third dimension the parameter
  emc <- mc_features(xs, ecomp_cspline)
  fdmc <- mc_features(xs, fd_variogram)
  emeans <- mc_stat(emc, mean)
  fdmeans <- mc_stat(fdmc, mean)

  emeans$alpha <- alpha
  names(emeans) <- c("A", "B", "alpha")
  save_cache(xs, "weierstrass_xs", prefix)
  save_cache(emeans, "weierstrass_ecomp", prefix)
  save_cache(emc, "weierstrass_emc", prefix)
  save_cache(fdmeans, "weierstrass_fdmean", prefix)
  save_cache(fdmc, "weierstrass_fdmc", prefix)
}

#----------------------------------------------------------
# variance
# Redo: Normalize first
#----------------------------------------------------------
# if(from_cache){
#   evarmc <- mc_features(xs, mean)
#   evar <- mc_stat(evarmc, mean) 
#   evar$alpha <- alpha 
#   names(evar) <- c("variance", "alpha") 
# }

# with(evar, plot(alpha, variance))
 
#----------------------------------------------------------
# Plot time series and boxplots of coeffcients
#----------------------------------------------------------
cols = c(3, 8, 16)
alpha[cols]

pdf(file.path(getwd(), paste0("figures/", prefix, "-weier-random.pdf")))
plot.ts(xs[1:500, cols, 1], main = "Random-phase Weierstrass function")
dev.off()

# Plot a single example from fbm with different parameters
cols <- seq(1, 19, by = 2)
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

names(coeffA) <- paste0(round(alpha, 2))
names(coeffB) <- paste0(round(alpha, 2))
names(coeffFD) <- paste0(round(alpha, 2))

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

 
    
par(mfrow = c(1,1)) 
plot(fdmeans[1:19, 1], emeans[1:19, 2])
# plot(alpha[1:19], coeffFD[1:19])

# names(df) <- c("A", "y", "x")
# plot_regression(df, xlab ="a", ylab = "Variance", title ="")     


# Plot of log-log fit vs. alpha for random Weierstrass
gp <- ggplot() +
      theme_base() +
      scale_x_continuous(name="log( S )", limits=c(0,1)) +
      scale_y_continuous(name="log( epsilons )", limits=c(-5, -3)) + 
      geom_abline(data =  emeans, 
        aes(intercept = A, slope = B, col = alpha)) 
      # ggtitle("title")
gp
save_plot("all_fits_plot", prefix)
# Linear regression of mean alpha vs A
df <- emeans
names(df) <- c("y", "B", "x")
plot_regression(df, xlab =   expression(alpha), 
                    ylab = "B", 
                    title = "")
# Plot takes y variable as response
names(df) <- c("A", "y", "x")
plot_regression(df, xlab =   expression(alpha), 
                    ylab = "A", 
                    title = "")

save_plot("weierstrass_plot", prefix)
 

#----------------------------------------------------------
# Compare plot with some parameters 
#----------------------------------------------------------
#----------------------------------------------------------
# Boxplot of coefficients 
#----------------------------------------------------------
cols <- seq(1, 19, by = 2)
# Plot a single example from fbm with different parameters
plot.ts(ecomplex::normalize(xs[, cols, 1]))

coeffA <- lapply(emc, function(x) x[,1]) %>% do.call(cbind, .)
coeffB <- lapply(emc, function(x) x[,2]) %>% do.call(cbind, .)

# coeffA <- lapply(emc, function(x) x[,1]) 
# vioplot(coeffA[,1], coeffA[,2], coeffA[,3], coeffA[,4], col = "gray70")
boxplot(coeffA[, 1:19])
boxplot(coeffB[, 1:19])



 
#----------------------------------------------------------
#
#----------------------------------------------------------


pdf(file.path(getwd(), paste0("figures/", prefix, "-fd-vs-B.pdf")))
par(mfrow = c(1,1)) 
plot(fdmeans[1:19, 1], emeans[1:19, 2], 
     xlab = "Fractal Dimension", 
     ylab = "B", 
     cex = 1.3, 
     pch = 16, 
     col = adjustcolor("gray10", 0.8)
      )
dev.off()



 
#-------------------------------------------------------
# Distribution of epsilon-complexity intercept coefficient
# (A) for the alpha replications.
#----------------------------------------------------------
edf <- do.call(rbind, emc)  
edf$alpha <- factor(rep(round(alpha, 2), each = reps))
names(edf) <- c("A", "B", "alpha")
gp <- ggplot(data = edf, aes(x = alpha, y= A)) + 
        geom_boxplot(aes(fill= alpha)) + 
        scale_fill_manual(values = colorRampPalette(c("gray40", 
                                                    sf_pair()[2]))(20)) +
        theme_minimal2()
gp

save_plot("weierstrass_boxplots", prefix)

#----------------------------------------------------------
# Error bars
#----------------------------------------------------------
edf_se <- edf %>% select(A, alpha) %>% 
                  group_by(alpha) %>%
                  summarise(se = sd(A)/sqrt(n()), A = mean(A))

# edf_se$alpha <- as.numeric(edf_se$alpha)
ggplot(data = edf_se, aes(x = alpha, y = A)) + 
     geom_point() + 
     geom_smooth(method = lm, colour = sf_pair()[2], se = FALSE, 
                          size = 0.8, alpha = 0.8) + 
     geom_errorbar(aes(ymin = A - se, ymax = A + se), 
                       width = 0.2) +
     theme_minimal2()

  save_plot("weierstrass_errorbars", prefix)
 


 








