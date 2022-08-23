#Visualization of Boxplots to get a bit of a feeling what they mean.
#Load packages
library(tidyverse)
library(reshape2)
library(ggsignif)
library(magick)

#set seed for random sampling
set.seed(1337)

#defining regular expressions for ggplot: 
apatheme <- theme(panel.background = element_blank(),
                  panel.grid = element_blank(), 
                  panel.border = element_blank(), 
                  axis.line.x.bottom = element_line(), 
                  axis.line.y.left = element_line(),
                  text = element_text(size = 12, family ="sans"))
#all used variable names in one vector 
iv_vec <- c("IV_1","IV_2","IV_3","IV_4","IV_5","IV_6","IV_7","IV_8")

#sourcing neighbor combinator function
source("https://raw.githubusercontent.com/K4tana/R_Helpers/master/neigh_combinator.R")

#combinations to t-test in a list
combs <- uniq_combinator(iv_vec)
comb_list <- list()
for (i in 1:nrow(combs)){
  a <- c(unname(unlist(combs[i,])))
  comb_list <- c(list(a),comb_list)
}

#create a data frame + plot + significance tests for neighboring variables. Do this for all N from 50 to 1000 in 50 increments. save plots as files and reference in a list.
images <- list()
for(i in 1:20){
  a <- data.frame(IV_1 = c(rnorm(i*50, mean = 2, sd= 1)),
             IV_2 = c(rnorm(i*50, mean = 2, sd= 2)), 
             IV_3 = c(rnorm(i*50, mean = 2, sd= 3)), 
             IV_4 = c(rnorm(i*50, mean = 2, sd= 4)),
             IV_5 = c(rnorm(i*50, mean = 2, sd= 5)),
             IV_6 = c(rnorm(i*50, mean = 2, sd= 6)),
             IV_7 = c(rnorm(i*50, mean = 2, sd= 7)),
             IV_8 = c(rnorm(i*50, mean = 2, sd= 8)),
             time=rep(i,i*50))
  a <- melt(a, id.var="time")
  b <- ggplot(a, aes(x=variable, y=value))+
    geom_boxplot(aes(fill = variable))+
    apatheme +
    geom_hline(yintercept = 2)+
    labs(x="Variable", y= "Value", caption = paste0("Observations per variable:",i*50, sep=" "))+
    theme(legend.position = "none")+
    geom_signif(comparisons = comb_list, map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05,"ns">0.05))
  
  name <- paste0("plot",i*50,".png")
  ggsave(filename = name,
         plot = b,
         device = "png",
         width = 10, height = 10, 
         units = "cm")
  images[[i]] <- name
  rm(a,b)
}

#this creates a list to animate from. Order from last to first has to be done for some reason unknown to me. 
frames <- c()
for (i in length(images):1){
  x <- image_read(images[[i]])
  x <- image_scale(x, "600")
  c(x,frames)-> frames
}
#The code below produces various images showing neighboring t-tests of different normal distribution draws in a boxplot. 
#This image demonstrates two things: Firstly, it shows how different standard deviations make boxplots wider. Secondly, it shows how false positives can still affect high N samples. Each variable's n is shown in the picture. Yet, over all images, about 5% of tests show a false positive result - a tiny bit below our expected alpha of 5%. Thus, this is acceptable. 
#
animation <- image_animate(frames, fps = 1)

#save it.
image_write(animation, "boxplots.gif")