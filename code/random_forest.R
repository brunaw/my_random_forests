#----------------------------------------------------------------------#
# A function to produce a random forest                                #
# Bruna Wundervald                                                     #
#  December, 2018                                                      #
#----------------------------------------------------------------------#

library(tidyverse)
library(parallel)
options(warn=-1)

rf <- function(formula, data, number_of_trees = 10){
  
  # Extracting the model structure
  m <- model.frame(formula, data = data)
  # Defining the response
  #response_name <- all.vars(formula)[1]
  y <- model.response(m)
  
  
  # Paralellizing all the trees 
  result <- mclapply(1:number_of_trees, 
                     FUN = function(i, formula, data){
                       trees <- my_trees(
                         formula = formula, data = data,
                         random_vars = TRUE)
                       Sys.sleep(0.2)
                       return(trees)
                     }, data = data, formula = formula)
  
  # Calculating predictions by extracting it from the list of trees
  fitted_values <- result %>% map("model_data") %>% 
    map(`[`, c("prediction")) %>% 
    bind_cols() %>% 
    mutate(final_prediction = rowSums(.)/number_of_trees) %>% 
    pull(final_prediction)
  
  
  sqr <- sum((y - fitted_values)^2)
  
  # Returning results 
  out <- list(trees_result = result, 
              prediction = predictions,
              sqr = sqr)
  class(out) <- "my_rf"
  return(out)
}