#----------------------------------------------------------------------#
# A function to produce a regression tree                              #
# Bruna Wundervald                                                     #
#  December, 2018                                                      #
#----------------------------------------------------------------------#

# Arguments
# formula: the specification of the model
# data: the data from where the variables come from
library(tidyverse)
library(furrr)


# Defining the function to create the minimizing criteria
sum_of_squares <- function(cut_point, variable_index, data_ss, X){
  
  # This function receives a variable and calculates the residual 
  # sum of squares if we partition the data accordingly to a cutpoint
  # in the chosen variable, considering all the pre-existent nodes
  
  cuts <- data_ss %>% 
    mutate(var = !!sym(variable_index)) %>% 
    group_by(node) %>% 
    mutate(criteria = ifelse(var > cut_point, "set1", "set2")) %>%
    mutate(mean_y = mean(!!sym(colnames(data_ss)[1]))) %>% 
    group_by(criteria) %>% 
    mutate(
      mean_new_node = mean(!!sym(colnames(data_ss)[1])), 
      resid = (!!sym(colnames(data_ss)[1]) - mean_new_node)^2)  %>%
    group_by(node) %>% 
    summarise(
      sqr_previous = sum((!!sym(colnames(data_ss)[1]) - mean_y)^2), 
      sqr_new_node = sum(resid)) 
  
  results <- cuts %>% 
    filter(sqr_new_node < sqr_previous) %>% 
    arrange(sqr_new_node) %>%
    slice(1) %>% 
    c()
  
  if(length(results[[1]]) > 0){
    return(results)
  } else { 
    results <- cuts %>% 
      arrange(sqr_previous) %>%
      slice(1) %>%  c()
    
    return(results)
  }
} 



my_trees <- function(formula, data, parallel = TRUE,
                     random_vars = FALSE){
  
  # Taking some action if the data has too many NAs
  if(nrow(na.omit(data))[1] < 10){
    stop("Your data has too many NAs.")
  }
  # Removing the intercept
  formula <- as.formula(paste(c(formula), "- 1"))
  
  # Extracting the model structure
  m <- model.frame(formula, data = data)
  # Defining the response
  response_name <- all.vars(formula)[1]
  y <- model.response(m)
  
  # Defining the matrix of predictor variables
  X <- model.matrix(formula, m)
  colnames(X) <- colnames(m)[-1]
  
  # Selecting m predictors randomly 
  if(random_vars){
    n_pred <- round(sqrt(ncol(X)))
    selected_predictors <- sample(1:ncol(X), size = n_pred, replace = FALSE)
  } else {
    n_pred <- ncol(X)
    selected_predictors <- 1:ncol(X)
  }
  node_num <- n_pred + 1
  
  # Creating the matrix to save the criteria for the cuts
  #criteria <- matrix(NA, ncol = 4, nrow = n_pred)
  criteria <- data.frame(i = NA, cut = NA, sqr = NA, node = NA)
  
  # Creating the minimum of observations that should be in each node
  keep_node <- round(nrow(data)*0.05)
  
  # Creating the node for each observation
  m$node<- "root"
  
  # Defining auxiliar variables 
  counter <- 0
  var  <-  vector("numeric", n_pred)
  cut_point <-  vector("numeric", n_pred)
  parents  <-  vector("numeric", n_pred)
  
  final_vars <-  c(0)
  final_cuts <-  c(0)
  final_parents <-  c(0)
  
  temps <- vector("numeric", n_pred)
  #iter = 2
  
  for(j in 2:node_num){
    
    # Finding the minimun sum of squares by testing all variables and different
    # cutpoints
    for(i in 1:n_pred){
      
      # Selecting a variable
      variable <- colnames(X)[selected_predictors[i]]
      # Defining sequence of cutpoints to be tested
      cut_points <- c(X[, variable])
      
      if(parallel){ plan(multiprocess) }
      # Mapping the RSS function to the cutpoints
      points <- cut_points %>% furrr::future_map(sum_of_squares, 
                                                 variable_index = variable,
                                                 data_ss = m, X = X) 
      
      # Saving the results
      sqrs <- points %>% map_dbl("sqr_new_node") 
      node_new <- points %>% map_chr("node") 
      cut <- sqrs %>% which.min()
      
      # Creating the criteria matrix to check for the variable that produced the
      #  smallest rss
      
      criteria[i, ] <- c(i, cut_points[cut], sqrs[cut], node_new[cut])
      
    }  
    
    # Verifying which variable and cutpoint gave the best results
    min_sqr <- which.min(criteria[,3])
    root <- selected_predictors[min_sqr]
    var[j-1] <- colnames(X)[min_sqr]
    cut_point[j-1] <- as.numeric(criteria[min_sqr, 2])
    parents[j-1] <- criteria[min_sqr, 4] 
    
    
    # Checking the dimension of the possible new nodes to see if  
    # they have the minimum of observations
    temps[j] <- m %>% 
      filter(node == parents[j-1]) %>% 
      mutate(groups =  ifelse(!!sym(var[j-1]) > cut_point[j-1], 'set1', 'set2')) %>% 
      count(groups) %>% 
      arrange(n) %>% slice(1) %>% pull()
    
    
    # Creating the very first split 
    if(j == 2 && temps[2] >= keep_node){
      
      m <- m %>% 
        mutate(node = ifelse(!!sym(var[j-1]) > cut_point[j-1],
                             paste(node, var[j-1], "1"), 
                             paste(node, var[j-1], "2")))
      
      counter <- counter + 1
      
      # Feeding the final vectors of variables and cutpoints
      final_vars[counter] <- var[j-1] 
      final_cuts[counter] <- cut_point[j-1] 
      final_parents[counter] <- parents[j-1]
      
    } else {
      # Creating the next splits and changing the auxiliar variables 
      if(temps[j] >= keep_node && temps[j] != temps[j-1]){
        m <- m %>% 
          mutate(node = ifelse(node == parents[j-1],
                               ifelse(!!sym(var[j-1]) > cut_point[j-1],
                                      paste(node, var[j-1], "1"), 
                                      paste(node, var[j-1], "2")), 
                               paste(node)))
        counter = counter + 1
        
        final_vars[counter] <- var[j-1] 
        final_cuts[counter] <- cut_point[j-1] 
        final_parents[counter] <- parents[j-1]
        
      } 
      #else { counter = counter }
    }
    
    # Updating objects 
    selected_predictors <- selected_predictors[-min_sqr]
    n_pred <- n_pred - 1
    criteria <- data.frame(i = NA, cut = NA, sqr = NA, node = NA)
    j <- j + 1
    
  }
  
  # Calculating the mean for each final node  
  m <- m %>% 
    group_by(node) %>% 
    mutate(prediction = round(mean(!!sym(response_name)), 3))
  
  # Calculating the means
  means <- m %>% 
    distinct(prediction) %>% 
    pull(prediction)
  
  nodes <- m %>% distinct(node)
  
  return(list(model_data = m, 
              vars = final_vars, 
              cuts = as.numeric(final_cuts),
              parents = final_parents,
              means = means, 
              nodes = nodes))
  
}