
## @knitr sim-feature-space

library(ggplot2)
library(dplyr)

prefix = "ecomplex"

sim_df1 <- readRDS(cache_file("approx_types1", prefix))
# sim_df <- readRDS(cache_file("approx_types", prefix))

sim_df1 <- sim_df1 %>% dplyr::filter(variable != "ecomp_all_B")

sim_df1$id2 <- as.factor(rep(c(1,2,1,3,1,4,1,5,1,6,1,7), each = 30))

# pal <- c("gray10", viridis::viridis(60)[c(5,10,15,20,25,30)])
pal <- eegpalette(0.9)[c(1,3)]
gp <- ggplot(data = sim_df1, aes(x  = value, y = yvalue))  

gp + facet_grid(~ variable, scales = "free_y") +
     geom_point(aes(colour = id, shape = fnames), size = 2) + 
     scale_color_manual(values = pal) + 
     theme_minimal() + 
     labs(x = "A", y = "B")

# save_plot("approx-feature-space", prefix)
