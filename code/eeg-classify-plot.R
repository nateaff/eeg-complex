ram(list = ls())
library(ggplot2)
library(dplyr)
library(ecomplex)
library(ggthemes) 

 
prefix = 'eeg'
#----------------------------------------------------------
# Notes on features used
#----------------------------------------------------------
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
# Both models predicted best on channel 1
plotdf <- combdf %>% filter(channel == "ch1") %>% 
          group_by(method) %>%
          dplyr::select(auc, Sensitivity, Specificity, Balanced.Accuracy) %>%  
          dplyr::summarise(SEacc = 1.96 * sd(Balanced.Accuracy)/sqrt(n()), 
                           ACC = mean(Balanced.Accuracy),
                          SEauc = 1.96 * sd(auc)/sqrt(n()), 
                           AUC = mean(auc), 
                           SEsp = 1.96 * sd(Specificity)/sqrt(n()), 
                           Specificity = mean(Specificity),
                           SEse = 1.96 * sd(Sensitivity)/sqrt(n()), 
                           Sensitivity = mean(Sensitivity)
                           ) 
# mdf <- reshape2::melt(plotdf)

# reshape2::melt(plotdf)

gacc <- ggplot(plotdf, aes(x = ACC, xmin = ACC - SEacc, xmax = ACC+SEacc, y = method )) +
      geom_point() + geom_segment( aes(x = ACC - SEacc, xend = ACC + SEacc,
                                       y = method, yend=method)) +
      theme_bw() + xlab("Percent") + 
      ylab("Partition Method") + 
      xlim(c(0.50,1)) + 
      ggtitle("Accuracy")

gauc <- ggplot(plotdf, aes(x = AUC, xmin = AUC-SEauc, xmax = AUC+SEauc, y = method )) +
      geom_point() + 
      geom_segment( aes(x = AUC-SEauc, xend = AUC+SEauc,
                                       y = method, yend=method)) +
      theme_bw() + xlab("Percent") +
      ylab("")+  
      xlim(c(0.50,1)) + 
      ggtitle("AUC")

gsp <- ggplot(plotdf, aes(x = Specificity, xmin = Specificity-SEsp, 
                                          xmax = Specificity+SEsp, 
                                          y = method )) +
      geom_point() + 
      geom_segment( aes(x    = Specificity-SEsp, 
                                       xend = Specificity+SEsp,
                                       y    = method, yend=method)) +
      theme_bw() + xlab("Percent") + 
      ylab("")+
      xlim(c(0.50,1)) +
      ggtitle("Specificity")

gse <- ggplot(plotdf, aes(x = Sensitivity, xmin = Sensitivity-SEse, 
                                          xmax = Specificity+SEse, 
                                          y    = method )) +
      geom_point() + geom_segment( aes(x    = Sensitivity-SEse, 
                                       xend = Sensitivity+SEse,
                                       y    = method, 
                                       yend = method)) +
      theme_bw() + xlab("Percent") +
      ylab("") +  
      xlim(c(0.50,1)) + 
      ggtitle("Sensitivity")

# pdf(file.path(getwd(), paste0("figures/", prefix, "-eeg-partition-diagnostic.pdf")), 
#     width = 9, height = 4)


g <- grid.arrange(gacc, gauc, gsp, gse, nrow = 1)
grid.arrange(gacc, gauc, gsp, gse, nrow = 1 )

ggsave("figures/eeg-partition-diagnostic.pdf", g)

