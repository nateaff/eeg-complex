rm(list = ls())
library(ggplot2)
library(dplyr)
library(ecomplex)
library(ggthemes) 

 
prefix = 'eeg'

# Split length for regular splits
npts <- lapply(c(10,5,1), function(m) palarm(1:120, m = m)$kout) %>%
        lapply(., length) %>% unlist + 1

#----------------------------------------------------------
# Load data
#----------------------------------------------------------
reps = 10
methods <- c("A", "B", "A+B", "8", "15", "30")
# methods <- factor(methods, )
res_vec <- readRDS(cache_file("res_vec", prefix))
resAB  <- readRDS(cache_file("resAB", prefix))

ABdf <- resAB %>% lapply(., bind_rows) %>% bind_rows
ABdf$channel <- factor(rep(rep(paste0("ch", 1:6), each = reps), 3))
ABdf$method <- factor(rep(c("A", "B", "A+B"), each = reps*6), 
               levels = methods, ordered = TRUE)

vecdf <- res_vec %>% lapply(., bind_rows) %>% bind_rows
vecdf$channel <- factor(rep(rep(paste0("ch", 1:6), each = reps), 3))
vecdf$method <- factor(rep(c("8", "15", "30"), each = reps*6), 
               levels = methods, ordered = TRUE)

# vecsd <- readRDS(cache_file("vecsd", prefix))
# ABsd  <- readRDS(cache_file("ABsd", prefix))

# vecmeans <- readRDS(cache_file("vecmeans", prefix))
# ABmeans  <- readRDS(cache_file("ABmeans", prefix))
combdf <- bind_rows(vecdf, ABdf)
library(gridExtra)
# Use quantiles
plotdf <- combdf %>% filter(channel == "ch1") %>% 
                group_by(method) %>% 
                dplyr::select(auc, Sensitivity, Specificity, Balanced.Accuracy) %>% 
                dplyr::summarise(
                           acc05 = quantile(Balanced.Accuracy, probs = 0.0275), 
                           ACC = mean(Balanced.Accuracy),
                           acc95 = quantile(Balanced.Accuracy, probs = 0.975),
                           auc05 = quantile(auc, probs = 0.0275), 
                           AUC = mean(auc), 
                           auc95 = quantile(auc, probs = 0.975),
                           sp05 = quantile(Specificity, probs = 0.0275), 
                           Specificity = mean(Specificity),
                           sp95 = quantile(Specificity, probs = 0.975), 
                           se05 = quantile(Sensitivity, probs = 0.0275), 
                           Sensitivity = mean(Sensitivity),
                           se95 = quantile(Sensitivity, probs = 0.975)
                           )

gacc <- ggplot(plotdf, aes(
                            x = ACC, 
                            xmin = acc05, 
                            xmax = acc95, 
                            y = method 
                            )) +
      geom_point() + geom_segment( aes(
                                        x = acc05, 
                                        xend = acc95,
                                        y = method, 
                                        yend=method
                                        )) +
      theme_bw() + xlab("Percent") + 
      ylab("Partition Method") + 
      xlim(c(0.50,1)) + 
      ggtitle("Accuracy")

gauc <- ggplot(plotdf, aes(
                           x = AUC, 
                           xmin = auc05, 
                           xmax = auc95, 
                           y = method 
                           )) +
      geom_point() + 
      geom_segment( aes(
                        x = auc05, 
                        xend = auc95,
                        y = method, 
                        yend = method
                        )) +
      theme_bw() + xlab("Percent") +
      ylab("")+  
      xlim(c(0.50,1)) + 
      ggtitle("AUC")

gsp <- ggplot(plotdf, aes(
                           x = Specificity, 
                           xmin = sp05, 
                           xmax = sp95, 
                           y = method 
                           )) +
      geom_point() + 
      geom_segment( aes(
                        x    = sp05, 
                        xend = sp95,
                        y    = method, 
                        yend = method
                        )) +
      theme_bw() + xlab("Percent") + 
      ylab("")+
      xlim(c(0.50,1)) +
      ggtitle("Specificity")

gse <- ggplot(plotdf, aes(x = Sensitivity, 
                          xmin = se05, 
                          xmax = se95, 
                          y    = method 
                          )) +
      geom_point() + geom_segment( aes(
                                       x    = se05, 
                                       xend = se95,
                                       y    = method, 
                                       yend = method
                                       )) +
      theme_bw() + xlab("Percent") +
      ylab("") +  
      xlim(c(0.50,1)) + 
      ggtitle("Sensitivity")

g <- grid.arrange(gacc, gauc, gsp, gse, nrow = 1)
grid.arrange(gacc, gauc, gsp, gse, nrow = 1 )

ggsave("figures/eeg-partition-diagnostic.pdf", g)


