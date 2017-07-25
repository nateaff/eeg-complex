
boxplot_panel <- function(df, response, addbee = TRUE, cex = 0.5){

  # FIXME : use layout and add title as parameter
  par(mfrow = c(1,7), mar = c(4,0.05,0.05,0.05), cex.lab = 1.5, cex.axis = 1.2)

  # dfin <- chdf[[ch]]
  beeboxplot(df$bandpower.theta, response, "Theta", addbee = TRUE, cex = cex)
  beeboxplot(df$bandpower.alpha, response, "Alpha", addbee = TRUE, cex = cex)
  beeboxplot(df$bandpower.beta, response,"Beta", addbee = TRUE, cex = cex)
  beeboxplot(df$bandpower.gamma, response, "Gamma", addbee = TRUE, cex = cex)
  beeboxplot(df$var, response, "Variance", addbee = TRUE, cex = cex)
  beeboxplot(df$hurst, response, "Hurst", addbee = TRUE, cex = cex)
  beeboxplot(df$spec_entropy, response, "Entropy", addbee = TRUE, cex = cex)

  par(mfrow = c(1,1))
}

 
#' Box plot with data points plotted using beeswarm
#'
#' @param variable An independent factor variable
#' @param response The dependent (numerical) response variable
beeboxplot <- function(variable, response, xlab, addbee = TRUE, cex = 1){
  add = FALSE
  if(addbee){
    beeswarm::beeswarm(variable ~ response,
           # bins = 30, 
           # data = df, 
           pch = 16,
           # col = eegpalette()[c(1,3)], 
            col = c(adjustcolor(eegpalette()[1], 0.7), 
                    adjustcolor(eegpalette()[3], 0.7)),
           # xaxt = "n",
           method = "center",
           cex = cex,
           yaxt = "n",
           ylab = '',
           xlab = xlab
           )
  add = TRUE
  # addxaxt = 'n'
  }
  
  
  boxplot(variable ~ response, 
            # data = df, 
            outpch = NA ,
            notch = TRUE, 
            col = c(adjustcolor(eegpalette()[1], 0.4), 
                    adjustcolor(eegpalette()[3], 0.4)),
            add = add, 
            xaxt = 'n', 
            yaxt = 'n', 
            ylab = '')
}


#' Scatter plot of features
#'
#' @param xname Column name for x variable
#' @param yname Column name for y variable 
#' @param df_   A data frame with xname, yname columns
#' @param ids    
scatterplot_features <- function(xname, yname, df_, ids = NULL, cex = 1.2, leg = FALSE){
  palette(eegpalette())
  # means_df with lower case columns expected
  x <- df_[,grep(tolower(xname), colnames(df_))]
  y <- df_[,grep(tolower(yname), colnames(df_))]

  if(!is.numeric(x) | !is.numeric(y)) stop("Invalid column name or data") 
  

  if(is.null(ids)) ids <- palette()
  par(cex.lab = 1.3)
  plot(x, y, pch = 20, 
             col = ids, 
             cex = cex, 
             xlab = xname,
             ylab = yname) 

  xloc <- max(x) -  .3 * max(x)
  yloc <- max(y) -  .3 * min(y) 
  if(leg){
  legend(xloc, yloc, 
      c("No response", "1st Response", "2nd Reponse"), 
      col = palette(), 
      pch = 20)
  }
}


# Diagnostic plot of time series features  
#
# Data frame should be
#  formatted with features in columns and an id column:   
#   | feature_name1 | feature_name2 | ... | id | 
plot_features <- function(data, type = c("histogram", "density", "boxplot"), 
                                single = TRUE,
                                ncol = 5, 
                                title = ""){
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette

 
  if(single){
    data_long <- reshape2::melt(data, id.vars = "id")  
  } else {
    data_long <- reshape2::melt(data, id.vars = c("id", "fnames"))
  }

  type <- match.arg(type)
  switch(type,
    histogram = { gg <- ggplot(data_long, aes(x=value, fill=id)) + 
                              geom_histogram(alpha = 0.5, 
                                           aes(y = ..density..), 
                                           bins = 10, 
                                           position = "identity") },
    density   = { gg <- ggplot(data_long, aes(x=value, fill=id)) +
                                   geom_density(alpha = 0.5, 
                                          aes(y = ..density..), 
                                          color = NA, 
                                          position = "identity")}, 
    boxplot   = { gg <- ggplot(data_long, aes(x = id, y = value, fill=id)) + 
                        geom_boxplot()                     }
  )
  if(single){
    gg + facet_wrap(~variable, ncol = ncol, scales = "free") + 
          labs(x = "", y = "") + 
          scale_fill_manual(values = cbPalette()) +
          theme_minimal2() +
          ggtitle(title)
  } else {
    gg + facet_wrap(~fnames + variable, ncol = ncol, scales = "free") + 
          labs(x = "", y = "") + 
          scale_fill_manual(values = cbPalette()) +
          theme_minimal2()  
          # ggtitle(title) 
  }
}


 
#' Regression Plot
#'
#' @param df data.frame that includes columns x, y
#' @param xlab xlab
#' @param ylab ylab
#' @param title title
#' @param conifint Include confidience intervals expression(paste(alpha, " leq ", beta))
#' @param palette A color palette
plot_regression <- function(df, xlab, ylab, title, 
                            confint = TRUE, palette = cbPalette()[2]) {
  
  gp <- ggplot(df, aes(x = x, y = y), 
        colour = "gray20") +
        labs(x = xlab, y = ylab) + 
        ggtitle(title)

    gp + theme_base() +
         theme(plot.margin=unit(c(6.5, 6.5, 6.5, 6.5),"points")) + 
         # theme(panel.border    = element_rect()) + 
               # panel.spacing    = unit(2, "lines")) + 
         geom_smooth(method = lm, se = confint, colour = "gray20", size = 1) +  
         # geom_line(size = 0.7, alpha = 0.85, colour = "gray5") +
         geom_point(size = 2, alpha = 0.94, colour = palette)  
         
}

#' Plot a time series or grid of time series
#'
#'
#' @param df A data frame with columns x, y
#' @param xlab xlaba
#' @param ylab ylab
#' @param title title
#' @param ncol grid columns
plot_ts <- function(df, facet = TRUE, xlab = "", ylab = "", title ="", ncol = 2, col = "gray10"){
  
  len <- dim(df)[1]  
  width <- dim(df)[2]
  dfm <- reshape2::melt(df)
  gp <- ggplot(dfm, aes(x = rep(1:len, width), y = value)) +
                    geom_line(size = 0.5, alpha = 0.85) +
                    labs(x = "", y = " ") 
  if(facet){
    gp <- gp + geom_line(colour = col) +
               facet_wrap(~variable, scales = "free_y", ncol = ncol) 
  } else {
    gp <- gp + geom_line(aes(colour = variable), size = 0.8, alpha = 0.85) +
               geom_point(aes(colour = variable)) +
               # if(ci)stat_smooth(method = "lm")  
               scale_color_manual(values = c("gray30", cbPalette()[7]))
  }
    gp + theme_minimal2() 
}

# GGplot version of plot.ts
#
plot_ts2 <- function(df, xlab = "", ylab = "", title ="", 
                          ncol = 2, col = NULL){
  
  if(is.null(col)) col <- cbPalette()[7]
  len <- dim(df)[1]  
  width <- dim(df)[2]
  dfm <- reshape2::melt(df)
  gp <- ggplot(dfm, aes(x = rep(1:len, width), y = value)) +
                    labs(x = "", y = " ") 
  gp + geom_line(aes(colour = variable),size = 0.5, alpha = 0.6) +
       facet_wrap(~variable, scales = "free_y", ncol = ncol)+
       scale_color_manual(values = paired()) +
       theme_minimal2() + 
       theme(legend.position = "none")
}


#' Plot the linear fit corresponding a set of   
#'  ecomplexity coefficients. 
#'
#'@param fit An 'ecomplex' fit
plot.ecomplex <- function(fit){
  gp <- ggplot(data.frame(fit$S, fit$epsilons), aes(fit$S, fit$epsilons)) + 
        geom_point() + 
        stat_smooth(method = "lm", col = "blue") + 
        theme_bw()
  gp
}

#' Facet ploto of multiple ecomplex fits 
#'
#' @param  fits Dataframe with values 'x', 'y' and labels 'id'
#' @param title Title of plot
plot_complex_facet <- function(fits, title){

gp <- ggplot(fits, aes(x = S, y = epsilons)) + 
      geom_point(shape = 17, size = 3, color = cbPalette()[1])
gg <- gp + facet_wrap(~ id) + 
      stat_smooth(method = "lm", fill = "grey70", 
                  size = 0.8, alpha = 0.3, 
                  color = "gray5") + 
      xlab("log ( S )") + 
      ylab("log( epsilons )") + 
      theme_base() + 
      labs(title = title) 
      
gg
}  


#' Facet ploto of multiple ecomplex fits 
#'
#' @param  fits Dataframe with values 'x', 'y' and labels 'id'
#' @param title Title of plot
plot_error_facet <- function(fits, title){

gp <- ggplot(fits, aes(x = rep((2:6), 5), y = eps)) + 
      geom_point(color = "gray40")
gg <- gp + facet_wrap(~ id) + 
      # stat_smooth(method = "lm", fill = "grey60", 
      #             size = 0.9, alpha = 0.3, 
      #             color = "gray20") + 
      theme_tufte2( ) +
      labs(title = title)
          # axis.text.y = element_text()) 
gg
}  


#' Create Matlab-like stemplot
#'
#'
#' @param x A numeric vector in index points
#' @param y A numeric vector of function points
#' @param clinecol 
#' @param linecol 
#' @param pch Leaf symbol
stemplot <- function(x, y, pch=16, linecol = 1, ...) {
  # linecol <- 1
  clinecol <- linecol
  if (missing(y)) {
    y <- x
    x <- 1:length(x) 
  }
    plot(x, y, pch = pch, ..., axes = FALSE) 
    Axis(side = 1, labels = FALSE)
    for (i in 1:length(x)) {
       lines(c(x[i], x[i]), c(0, y[i]), col = linecol, lwd = 1.5)
    }
    lines(c(x[1] - 2, x[length(x)] + 2), c(0, 0), col = clinecol)
}


