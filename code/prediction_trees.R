#----------------------------------------------------------------------#
# A function to predict from a tree model                              #
# Bruna Wundervald                                                     #
#  December, 2018                                                      #
#----------------------------------------------------------------------#
library(tidyverse)

predict_tree <- function(model, da){
  vars <- model$vars
  cuts <- model$cuts
  nodes <- model$nodes
  parents <- model$parents
  means <- data.frame(
    node = nodes,
    prediction = model$means)
  
  da <- da %>% mutate(node = "root")
  counter <- 0
  
  for(i in 2:(length(vars)+1)){
    
    if(i == 2){
      da  <- da %>% 
        mutate(node = ifelse(!!sym(vars[i-1]) > cuts[i-1],
                             paste(node, vars[i-1], 1), 
                             paste(node, vars[i-1], 2)))
      counter <- counter + 1
    } else {
      # Creating the next splits and changing the auxiliar variables 
      da <- da %>% 
        mutate(node = ifelse(node == parents[i-1],
                             ifelse(!!sym(vars[i-1]) > cuts[i-1],
                                    paste(node, vars[i-1], 1), 
                                    paste(node, vars[i-1], 2)), 
                             paste(node)))
      counter = counter + 1
    }
  }
  
  predicted <- da %>% 
    full_join(means, by = "node") %>% 
    pull(prediction)
  
  return(predicted)
}