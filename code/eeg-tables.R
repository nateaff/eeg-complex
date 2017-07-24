## @knitr eeg-tables
library(dplyr)

prefix = "eeg"
vecmeans <- readRDS(cache_file("vecmeans", prefix))
ABmeans <- readRDS(cache_file("ABmeans", prefix))

ABacc <- ABmeans %>% lapply(., function(x) x["Balanced.Accuracy", ]) %>% 
            bind_rows(.) %>% 
            data.frame
vecacc <- vecmeans %>% lapply(., function(x) x["Balanced.Accuracy", ]) %>% 
            bind_rows(.) %>% 
            data.frame
combacc <- rbind(ABacc, vecacc)

names(combacc) <- paste0("ch", 1:6)
methods <- c("A", "B", "A+B", "8", "15", "30")
row.names(combacc) <- methods
combacc <- apply(combacc, 2, round, digits = 2)

names(combacc) <- paste0("Channel ", 1:6)
knitr::kable(combacc, caption = "Balanced accuracy of each models and channel combination.")

## @knitr baseline
basedf <- readRDS(cache_file("basestats", prefix))

names(basedf) <- c("Sensitivity", "Specificity", "Accuracy")
stargazer::stargazer(basedf, summary = FALSE, label = "fig:example", 
                 title = "Prediction performance of baseline classifier")

## @knitr comb-sd
# sd 
res_vec <- readRDS(cache_file("res_vec", prefix))
resAB <- readRDS(cache_file("resAB", prefix)) 

#
