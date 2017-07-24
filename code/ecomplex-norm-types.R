library(dplyr)
library(ggplot2)
library(ecomplex)
library(tssims)
library(tsfeats)
library(fArma)

prefix = "ecomplex_norm_types"
from_cache = TRUE
#----------------------------------------------------------
# 
#----------------------------------------------------------
if(from_cache){
  err_df <- readRDS(cache_file("sim_df", prefix))
} else {  ecomp_methods <- c( ecomp_cspline_mse, 
                      ecomp_cspline_mae, 
                      ecomp_cspline_max)
  sim_df <- make_sim_features(n = 30, len = 500, features = ecomp_methods)
  save_cache(sim_df, filename, prefix)
} 

#----------------------------------------------------------
# Plot feature dataframe
#----------------------------------------------------------

sim_df1  <- select(sim_df, -ends_with("A")) %>% reshape2::melt(.)
sim_df2  <- select(sim_df, -ends_with("B")) %>% reshape2::melt(.)
sim_df1$yvalue <- sim_df2$value

gp <- ggplot(data = sim_df1, aes(x  = value, y = yvalue))  

gp + facet_grid(~ variable, scales = "free_y") +
     geom_point(aes(colour = id, shape = fnames), size = 2) + 
     scale_color_manual(values = eegpalette(0.7)[c(1, 3)]) + 
     theme_minimal2() + 
     labs(x = "A", y = "B")


plot_name <- "norm_classification"
save_plot(plot_name, prefix)

#----------------------------------------------------------
# Compare error for each method
#----------------------------------------------------------
if(from_cache){
  err_table <- readRDS(cache_file("err_table", prefix))
  errs <- readRDS(cache_file("rf_confusion", prefix))

} else {
  # names(sim_df) <- c("ecomp_cspline_mse_A", "ecomp_cspline_mse_B", 
  #               "ecomp_cspline_mae_A", "ecomp_cspline_mae_B", 
  #               "ecomp_cspline_max_A", "ecomp_cspline_max_B",
  #                "id", "fname")

  reps <- 30
  method_names <- c("ecomp_cspline_mse", "ecomp_cspline_mae", "ecomp_cspline_max")

  # make formulas for each method
  formulas <- paste0("factor(id)", " ~ ", paste0(method_names, "_A"),
                                   " + ", paste0(method_names, "_B")) %>% 
              lapply(., as.formula) 
  # combined error on all simulated functions
  err_comb <- boot_formulas(sim_df, formulas, 5) %>% 
              lapply(., function(x) apply(x, 2, mean)) %>% 
              do.call(rbind, .)

  # Take the average classification error for each 
  # simulation and each norm type
  err_sim <- sim_df %>% split(.$fname)  %>% 
                    lapply(., function(x) boot_formulas(x, formulas, reps)) %>%
                    lapply(., function(x) lapply(x, function(y) apply(y, 2, mean))) %>%
                    lapply(., function(x) do.call(rbind, x)) 

  errs <- do.call(rbind, err_sim) %>% 
          data.frame %>% 
          dplyr::mutate(err_rate = round(1- accuracy, 2), method = rep(method_names, 6)) 
  errs$fname <- rep(sort(unique(sim_df$fname)), each = 3)

sort(unique(sim_df$fname))

  err_table <- errs %>% dplyr::select(err_rate, fname, method) %>%  
                       reshape2::dcast(., method ~ fname, value.var ="err_rate")

  errt <- as.matrix(err_table[2:7]) %>% t %>% data.frame
  names(errt) <- err_table$method
  err_table <- apply(errt, 2, round, digits = 2)
                       
  save_cache(errs, "rf_confusion", prefix)
  save_cache(err_table, "err_table", prefix)
  gridExtra::grid.table(err_table)
  save_plot("error_table", prefix)
}




