## @knitr cauchy-beta

prefix <- "cauchy"

alpha <- seq(0.01, 1.99, length.out = 10) 
beta <- seq(0.01, 1.5, length.out = 10)
ab <- expand.grid(alpha, beta)

xs  <- readRDS(cache_file("xs", prefix)) 
emc<- readRDS(cache_file("ecoeff", prefix))
fdmc <- readRDS(cache_file("fd", prefix))
hmc <- readRDS(cache_file("hurst", prefix))

emean <- mc_stat(emc, mean)
varmean  <- mc_stat(emc, var)
fdmean <- mc_stat(fdmc, mean) 
hmean <- mc_stat(hmc, mean)



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

par(mfrow = c(1, 3))
with(ab_df, plot(Beta, A,  col = "gray20") )
with(ab_df, plot(Beta, B,  col = "gray20") )
with(ab_df, plot(Beta, fd, ylab = "Fractal Dimension",  col = "gray20") )
