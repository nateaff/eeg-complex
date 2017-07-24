
method_palette <- function(){
  c("gray10",
    "chocolate2", 
    "dodgerblue")
}

# eegpalette <- function(){
#   viridis::viridis(40)[c(1, 5, 10, 15, 20, 25, 30, 35)]
# }

eegPair <- function(alpha = 0.6){
  c(adjustcolor("gray10",  alpha),  # no response
   adjustcolor("darkcyan", alpha))       # response 1st
}

eegpalette <- function(alpha = 0.6){
  c(adjustcolor("gray10",  alpha),  # no response
   adjustcolor("coral2", alpha),    # response 2nd
   adjustcolor("darkcyan", alpha),       # response 1st
   adjustcolor("gray40",  alpha))     # not used
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

# contrasting pairs
paired <- function(){
  c("#422ca5", "gray35",
    "#325b9e", "gray20",
    "grey15", "chocolate", 
    "slategrey", "grey10",
    "dodgerblue", "gray40"
   )
} 

# purple and gray, used in presentation
sf_pair <- function(){
 c("#070511", "#6f4fff")
}


sf_gradient <- function(n = 20){
    pal <- colorRampPalette(colors = c("#070511", "#6f4fff"))
}

