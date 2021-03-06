eegPair <- function(alpha = 0.6){
  c(adjustcolor("gray10",  alpha),  # no response
   adjustcolor("darkcyan", alpha))  # response 1st
}

eegpalette <- function(alpha = 0.6){
  c(
    adjustcolor("gray10", alpha), 
    # adjustcolor(viridis::viridis(30)[2], alpha),
    adjustcolor("tomato3", alpha),
    # adjustcolor(viridis::viridis(30)[20], alpha),
    adjustcolor(viridis::viridis(30)[12], alpha)
    ) 
}

viridisPalette <- function(){
  c("gray20", viridis::viridis(10)[10])
}

cbPalette <- function(){
  c("gray20",
    "#0072B2",
    "#E69F00",
    "#56B4E9", 
    "#009E73", 
    "#F0E442",  
    "#D55E00", 
    "#CC79A7", 
    "gray50")
}

# rearranged some colors
cbPalette2 <- function(){
  c("grey50",
    "#E69F00",
    "#56B4E9",
    "#0072B2",
    "#009E73", 
    "#F0E442",  
    "#D55E00", 
    "#CC79A7")
}

# purple and gray, used in presentation
sf_pair <- function(){
 c("#070511", "#6f4fff")
}


sf_gradient <- function(n = 20){
    pal <- colorRampPalette(colors = c("#070511", "#6f4fff"))
}

