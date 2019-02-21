#----------------------------------------------------------------------#
# A function to predict from a random forest                           #
# Bruna Wundervald                                                     #
#  December, 2018                                                      #
#----------------------------------------------------------------------#

predict_rf <- function(model, newdata){
  
  number_of_trees <- lengths(model)[1]
  prediction <- model$trees_result %>% 
    purrr::map(predict_tree, da = data) %>% 
    as.data.frame() %>% 
    mutate(final_prediction = rowSums(.)/number_of_trees) %>% 
    pull(final_prediction)
  
  return(prediction)
}

predict_rf(rf_cars, new_data)
