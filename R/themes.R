# A minor modification of \code{tufte} theme
theme_tufte2 <- function(base_size = 12, base_family = "serif", ticks = FALSE) {
  ret <- theme_bw(base_family = base_family, base_size = base_size) +
    theme(
          legend.background = element_blank(),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          plot.background = element_blank(),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          text = element_text(size = 11),
          panel.grid.major.x = element_line(color = "gray50"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray50"),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text()
    )
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret
}

# A minor modification of \code{bw} theme
theme_bw2 <- function(base_size = 14, base_family = "Helvetica", ticks = FALSE){
  ret <- theme_bw(base_family = base_family) + 
         theme(axis.ticks = element_line(), 
               axis.line = element_line(size= .7, color = "gray5"), 
               axis.title = element_text(face = "bold",size = rel(1)),
               panel.grid = element_line(size = 0.6, color = "gray20"),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_blank(),
               strip.background = element_blank(),
               plot.background   = element_blank(),
               panel.background = element_blank()
        )
}

# A minor modification of the \code{minimal} theme
theme_minimal2 <- function( base_size = 12, base_family = "sans") {

  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major = element_line(color = "gray70", size = 0.2),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      axis.ticks        = element_line(),
      axis.ticks.x      = element_blank(),
      axis.ticks.length = unit(0, "lines"),
      panel.spacing.x   = unit(2, "lines"),
      panel.spacing.y   = unit(1, "lines")
      )
}