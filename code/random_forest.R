#----------------------------------------------------------------------#
# A function to produce a random forest                                #
# Bruna Wundervald                                                     #
#  December, 2018                                                      #
#----------------------------------------------------------------------#

library(tidyverse)
library(progress)
library(foreach)
library(doParallel)
library(doSNOW)
options(warn=-1)

# Creating function to be parallelized that just runs a tree
not_to_change <- function(rf_data, formula){
  trees <- my_trees(formula = formula, your_data = rf_data)
  Sys.sleep(0.2)
  return(trees)
}

rf <- function(formula, rf_data, number_of_trees = 15, 
               func = 
                 not_to_change(formula = formula, rf_data = rf_data)){
  
  # Setup parallel backend to use many processors
  cores <- parallel::detectCores()
  cl <- makeSOCKcluster(cores)
  registerDoSNOW(cl)
  
  # Creating the text bar 
  pb <- txtProgressBar(min = 0, max = number_of_trees, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  # Paralellizing all the trees 
  result <- 
    foreach(i = 1:number_of_trees, .packages="tidyverse",
            .options.snow = opts) %dopar% {
              func
            }
  # Closing parallelization
  
  close(pb)
  stopCluster(cl)
  
  # Calculating predictions by extracting it from the list of trees
  predictions <- result %>% map("model_data") %>% 
    map(`[`, c("prediction")) %>% 
    bind_cols() %>% 
    mutate(final_prediction = rowSums(.)/number_of_trees) %>% 
    pull(final_prediction)
  
  # Returning results 
  return(list(trees_result = result, prediction = predictions))
}
