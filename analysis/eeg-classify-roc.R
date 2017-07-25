## @knitr ROC-plot
# ROC, Importance

library(pROC)

resAB_raw <- readRDS(cache_file("resAB_raw", "eeg"))
resvec_raw <- readRDS(cache_file("resvec_raw", "eeg"))


ABimport <- lapply(resAB_raw, function(x) x$importance) %>% 
            do.call(rbind, .) %>% 
            data.frame
vecimport <- lapply(resvec_raw, function(x) x$importance) %>% 
             do.call(rbind, .) %>% 
             data.frame
names(ABimport) <- varnames
names(vecimport) <- varnames

ABnorm <- apply(ABimport, 1, ecomplex::normalize)
vecnorm <- apply(vecimport, 1, ecomplex::normalize)


ABdf <- resAB_raw[[1]]$df
vecdf <- resvec_raw[[1]]$df

vecroc <- pROC::roc(df$response, vecdf$prob)
ABroc <- pROC::roc(df$response, ABdf$prob)

# pdf(file.path(getwd(), paste0("figures/", prefix, "roc-comb.pdf")), 
#     width = 8, height = 4)

par(mfrow = c(1,2))
plot(ABroc)
plot.roc(vecroc, add=TRUE, col="blue", lty = 3)
   legend("bottomright", legend=c("Parition on coefficient B", "8 regular paritions"),
          col=c(par("fg"), "blue"), lwd=2, lty = c(1,3))


plot(smooth(ABroc))
plot.roc(smooth(vecroc), add=TRUE, col="blue", lty = 3)
   legend("bottomright", legend=c("Parition on coefficient B", "8 regular paritions"),
          col=c(par("fg"), "blue"), lwd=2, lty = c(1,3))


par(mfrow = c(1,1))

# dev.off()
