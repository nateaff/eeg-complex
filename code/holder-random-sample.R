library(ecomplex)
library(ggplot2)
library(ggthemes)
library(dplyr)

#----------------------------------------------------------
#  Random Weierstrass function
#  ---------------------------
#  Plot ecomplexity fits for sequence of parameters to 
#  random Weierstrass function. Parameters are in (0,1) 
#  and correspond to the Holder exponent of the function.
#----------------------------------------------------------

seed = 2017
from_cache = TRUE
len <- 500
reps <- 50

prefix <- "holder_coeffs"
set.seed(seed)
alphas <- seq(0.1, 0.99, length.out = 20)

if(from_cache) {
  xs     <- readRDS(cache_file("weierstrass_random_sample_xs", prefix)) 
  emeans <- readRDS(cache_file("weierstrass_random_sample_ecomp", prefix))
  emc    <- readRDS(cache_file("weierstrass_random_sample_emc", prefix))
} else {
  # generate set of weierstrass functions for each alpha
  ws <- lapply(alphas, function(x) tssims::weierstrass(a = x, random = TRUE, density = 1))
  xs <- replicate(reps, sapply(ws, function(w) tssims::gen(w)(len)))

  # takes 3 dimensional array, with the third dimension the parameter
  emc <- mc_features(xs, ecomp_cspline_rand)
  emeans <- mc_stat(emc, mean)

  emeans <- lapply(emc, function(x)  apply(x, 2, mean)) %>% 
           do.call(rbind, .) %>% 
           data.frame

  emeans$alpha <- alphas
  names(emeans) <- c("A", "B", "alpha")
}

#----------------------------------------------------------
# TODO: labels, text size
#----------------------------------------------------------
gp <- ggplot() +
      theme_base() + 
      scale_x_continuous(name="x", limits=c(0,1)) +
      scale_y_continuous(name="y", limits=c(-3.5, -2.5)) + 
      geom_abline(data =  emeans, 
        aes(intercept = A, slope = B, col = alphas))
gp

#-------------------------------------------------------
# Distribution of epsilon-complexity intercept coefficient
# (A) for the alpha replications.
#----------------------------------------------------------
edf <- do.call(rbind, emc)  
edf$alphas <- factor(rep(round(alphas, 2), each = reps))
names(edf) <- c("A", "B", "alphas")

gp <- ggplot(data = edf, aes(x = alphas, y= A)) + 
        geom_boxplot(aes(fill= alphas)) + 
        scale_fill_manual(values = colorRampPalette(c("gray40", 
                                                    sf_pair()[2]))(20)) +
        theme_minimal2()
gp

save_plot("weierstrass_random_boxplots", prefix)

#----------------------------------------------------------
# Error bars
#----------------------------------------------------------
edf_se <- edf %>% select(A, alphas) %>% 
                  group_by(alphas) %>%
                  summarise(se = sd(A)/sqrt(n()), A = mean(A))
edf_se$alphas <- as.numeric(edf_se$alphas)
ggplot(data = edf_se, aes(x = alphas, y = A)) + 
     geom_point() + 
     geom_smooth(method = lm, colour = sf_pair()[2], se = FALSE, 
                          size = 0.8, alpha = 0.8) + 
     geom_errorbar(aes(ymin = A - se, ymax = A + se), 
                       width = 0.2) +
     theme_minimal2()

save_plot("weierstrass_random_errorbars", prefix)
 
#----------------------------------------------------------
# Simple regression on means
#----------------------------------------------------------

df <- emeans
names(df) <- c("y", "A", "x")
plot_regression(df, xlab =   expression(alpha), 
                    ylab = "A", 
                    title = "")

 
# save_plot("weierstrass_regression_plot", prefix)
 
if(!from_cache){
  save_cache(xs, "weierstrass_random_sample_xs", prefix)
  save_cache(emeans, "weierstrass_random_sample_ecomp", prefix)
  save_cache(emc, "weierstrass_random_sample_emc", prefix)
}

