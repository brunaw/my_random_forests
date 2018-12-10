#----------------------------------------------------------------------#
# A function to predict from a tree model                              #
# Bruna Wundervald                                                     #
#  December, 2018                                                      #
#----------------------------------------------------------------------#
library(tidyverse)

predict_tree <- function(model, newdata){
  data <- newdata
  vars <- model$vars
  cuts <- model$cuts
  nodes <- model$nodes
  parents <- model$parents
  means <- data.frame(
    node = nodes,
    prediction = model$means)
  
  data$node <- "node 1"
  
  counter <- 0
  iter <-  2
  i = 2
  
  for(i in 2:(length(vars)+1)){
    
    if(i == 2){
      
      data <- data %>% 
        mutate(node = ifelse(!!sym(vars[i-1]) > cuts[i-1],
                             paste(node, iter + 1), paste(node, iter)))
      
      counter <- counter + 1
      iter <-  iter + 1
      
    } else {
      # Creating the next splits and changing the auxiliar variables 
      data <- data %>% 
        mutate(node = ifelse(node == parents[i-1],
                             ifelse(!!sym(vars[i-1]) > cuts[i-1],
                                    paste(node, iter+counter+1), paste(node, iter+counter)), 
                             paste(node)))
      counter = counter + 1
      iter = iter + 1
    }
  }
  
  
  
  predicted <- data %>% 
    full_join(means, by = "node") %>% 
    pull(prediction)
  
  return(predicted)
}

