# Plot predictions estimates for segmented data
plot_eeg_segments <- function(segdf, 
                              curdf, 
                              colnums, 
                              meanAB, 
                              ptAB, 
                              plotdata, 
                              predlist, 
                              plot_legend = TRUE){
    oldpar <- par()

    par(mar = c(05, 0.5, 2, 0.5))
    
    resA  <- palarm(segdf$ecomp.cspline_A)
    resB  <- palarm(segdf$ecomp.cspline_B)
    # ptA   <- resA$kouts
    meanA <- resA$means
    meanB <- resB$means
    ptAB  <- c(1, ptAB)
    #----------------------------------------------------------
    response = "No response"
    if(plotdata$response) response <- "Seizure response"
    # thresh = 0.43
    #----------------------------------------------------------
    
    pal = colorRampPalette(c("#75d7ff", "#f2ca18"))
    pal10 = pal(10)
    Acol = cbPalette()[4]
    Bcol = cbPalette()[2]
    Am  = "coral1"
    Bm = "tomato"
    Cm = "chocolate2"
    featcols = paste0("gray", seq(10,50, by = 5))
    legcols = c(Acol, Bcol, Am, Bm, Cm)
    predcol = c(adjustcolor("white", 0.2), adjustcolor(pal10[9], 0.5), adjustcolor(pal10[1], 0.4))

    # pdf("prediction_segments_ch4_t16.pdf", 
    ymax = 13.5
    ## @knitr segplot
    plot((normalize(segdf$ecomp.cspline_A) + 17), 
        type = 'l', 
        lwd = 3, 
        col = "gray20", 
        xlab = "",
        ylab = "", 
        ylim = c(0, ymax),
        xlim = c(0, 155), 
        axes = FALSE)
    Axis(side = 1, labels = TRUE, x = c(1,120))

    # Shade prediction areasa
    for(k in seq_along(1:(length(ptAB)-1))){  
        cind <- ceiling(predlist$pred[k]*10)
        rcol <- adjustcolor(pal10[cind], 0.3)
        rect(ptAB[k], -2, ptAB[k+1], ymax, col = rcol,   border = "transparent")
    }

    # Show segement breaks
    for(k in 1:(length(ptAB))){
        abline(v = ptAB[k], col = Cm, lty = 2)
    }
    lines((normalize(segdf$ecomp.cspline_A) + 12.4), 
        type = 'l', 
        lwd = 3, 
        col = cbPalette()[4])

    # lines((normalize(meanA[1:120]) + 12.4 ), 
    #     col = Am, 
    #     lwd = 2.7)

    lines((normalize((segdf$ecomp.cspline_B)) +  10.5), 
        type = 'l', 
        lwd = 2, 
        col = cbPalette()[2], 
        ylab = "B", 
        xlab = "time")

    # lines((normalize((meanB[1:120]))+ 11.2), 
    #     col = Bm, 
    #     lwd = 2.7)

    lines((normalize((meanAB[1:120])) + 11.5), 
        col = Cm, 
        lwd = 2.7)

    for(k in 1:length(colnums)){
      lines((normalize(curdf[,k]) + 10.1- 1.15*k), 
            type = 'l', 
            lwd = 2.7, 
            col = featcols[k],  
            ylim = c(0, 5) )
    }
    if(plot_legend){
    clabels <- c("Coefficient A", "Coefficient B", "Mean A", "Mean B", "Mean A + B")
    flabels <- c("Delta", "Theta", "Alpha", "Beta", "Gamma", "Variance", 
                "Hurst", "Spectral entropy")
    legend(123, 13.5, legend = c("Prediction", 
                     sprintf("Seizure: %.2f", predlist$prob), 
                     sprintf("No Seizure: %.2f", 1-predlist$prob)), 
                     col =  predcol, 
                     lwd = 5, 
                     lty = 1, 
                     bty = 'n')

    legend(123, 10.5, legend = clabels, col = legcols, lwd = 3, bty = 'n')
    legend(123, 6.7, legend = flabels, col = featcols, lwd = 3, bty = 'n')
    # title(main = sprintf("Trial: %d, ch: %d, %s", 
    #         plotdata$trialnum, 
    #         # plotdata$testch, 
    #         response),
    #         adj=0, line=1.0, cex = 0.7)
    mtext(sprintf("Trial %d, %s", 
            plotdata$trialnum, 
            # plotdata$testch,  
            response), line = 0, cex=1, font=1, at = 12) 
    }
    par(oldpar)
}