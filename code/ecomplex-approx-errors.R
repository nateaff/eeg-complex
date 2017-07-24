library(ecomplex)
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(tssims) 
library(fArma)
#----------------------------------------------------------
# 2. Compare mean absolute error for each of the 
#    functions in the test-suite for each of the 
#    ecomplex methods. 
#---------------------------------------------------------- 
prefix = "ecomplex"
from_cache = TRUE

if(from_cache){
  err_means <- readRDS(cache_file("ds_epsilons", prefix))
  all_err_means <- readRDS(cache_file("all_errs", prefix))
} else {
  epsilons <- function(mat, method){
    apply(mat, 2, function(x) ecomplex(x, method = method)) %>% 
             lapply(., function(x) x$epsilons) %>%
             do.call(rbind, .)  
  }

  n <- 30
  len = 500
  mat <- lapply(gs$group1, function(f) replicate(n, gen(jitter_params(f))(len)))  

  avg_err <- function(mat){
  # dataframe with cols = downsample, rows = method 
  fit_eps <- lapply(methods, function(method) epsilons(mat, method))
  err_means <- lapply(fit_eps, function(m) apply(m, 2, mean)) %>% 
               do.call(cbind, .) %>%
               data.frame

  names(err_means) <- methods
  row.names(err_means) <- paste0("downsample", (1:5))
  err_means <- apply(err_means, 2, function(x) round(x, 2)) %>%
               data.frame
  err_means 
  }
  all_errs <- lapply(mat, avg_err)
  all_err_means <- lapply(all_errs, function(x) apply(x, 2, mean)) %>% 
                  do.call(rbind, .) %>% data.frame
  
  all_err_means 
  save_cache(err_means, "ds_epsilons", prefix)
  save_cache(all_err_means, "all_errs", prefix)
} # end else

df <- reshape2::melt(err_means)
df$downsample <- rep((1:5),4)

gp <- ggplot(df, aes(x = downsample, y = value, group = variable)) + 
      geom_line(aes(color = variable)) + 
      scale_color_manual(values = eegpalette(0.9)) + 
      labs(x = "Downsample level", y = "Mean absolute error") + 
      theme_base()
gp

save_plot("mae_errors", prefix)
dev.off()
gridExtra::grid.table(err_means)
save_plot("err_means", prefix)
dev.off()
gridExtra::grid.table(all_err_means)
save_plot("all_err_means")

  

