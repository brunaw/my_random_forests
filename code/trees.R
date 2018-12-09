#----------------------------------------------------------------------#
# A function to produce a regression tree                              #
# Bruna Wundervald                                                     #
#  December, 2018                                                      #
#----------------------------------------------------------------------#

# Arguments
# formula: the specification of the model
# data: the data from where the variables come from
library(tidyverse)

# Defining the function to create the minimizing criteria
sum_of_squares <- function(cut_point, variable_index, data_ss, X){
  
  # This function receives a variable and calculates the residual 
  # sum of squares if we partition the data accordingly to a cutpoint
  # in the chosen variable, considering all the pre-existent nodes
  
  cuts <- data_ss %>% 
    mutate(var = !!sym(colnames(X)[variable_index])) %>% 
    group_by(node) %>% 
    mutate(criteria = ifelse(var > cut_point, "set1", "set2")) %>%
    group_by(criteria) %>% 
    mutate(mean = mean(!!sym(colnames(data_ss)[1])), 
           sqr = (!!sym(colnames(data_ss)[1]) - mean)^2)  %>%
    group_by(node) %>% 
    summarise(sqr = sum(sqr)) %>% 
    arrange(sqr) %>% 
    slice(1) %>% 
    c()
} 



my_trees <- function(formula, your_data){
  
  # Taking some action if the data has too many NAs
  if(dim(na.omit(your_data)) < 10){
    stop("Your data has too many NAs.")
  }
  # Removing the intercept
  formula <- as.formula(paste(c(formula), "- 1"))
  
  # Extracting the model structure
  m <- model.frame(formula, data = your_data)
  response <- all.vars(formula)[1]
  
  # Defining the matrix of predictor variables
  X <- model.matrix(formula, m)
  colnames(X) <- colnames(m)[-1]
  
  # Defining the response
  y <- model.response(m)
  
  # Selecting m predictors 
  options <- 1:ncol(X)
  node_num <- round(sqrt(ncol(X))) + 1
  n_pred <- round(sqrt(ncol(X)))
  selected_predictors <- sample(options, size = n_pred, replace = FALSE)
  
  # Creating the matrix to save the criteria for the cuts
  criteria <- matrix(NA, ncol = 4, nrow = n_pred)
  
  # Creating the minimum of observations that should be in each node
  keep_node <- round(dim(your_data)[1]*0.05)
  
  # Creating the node for each observation
  m$node<- "node 1"
  
  
  # Defining auxiliar variables 
  counter <- 0
  var  <-  vector("numeric", n_pred)
  final_vars <-  c(0)
  cut_point <-  vector("numeric", n_pred)
  final_cuts <-  c(0)
  parents  <-  vector("numeric", n_pred)
  final_parents<-  c(0)
  
  temps <- vector("numeric", n_pred)
  iter <-  2
  
  for(j in 2:node_num){
    
    # Finding the minimun sum of squares by testing all variables and different
    # cutpoints
    for(i in 1:n_pred){
      
      # Selecting a variables
      variable <- selected_predictors[i]
      # Defining sequence of cutpoints to be tested
      cut_points <- c(X[, variable])
      
      # Mapping the RSS function to the cutpoints
      points <- cut_points %>% purrr::map(sum_of_squares, 
                                          variable_index = variable,
                                          data_ss = m, X = X) 
      
      # Saving the results
      sqrs <- points %>% map("sqr") %>% unlist()
      node_new <- points %>% map("node") %>% unlist()
      cut <- sqrs %>% which.min() %>% unlist()
      
      # Creating the criteria matrix to check for the variable that produced the
      #  smallest rss
      criteria[i, 1:4] <- c(i, cut_points[cut], sqrs[cut], node_new[cut])
    }  
    
    
    # Verifying which variable and cutpoint gave the best results
    min_sqr <- which.min(criteria[, 3])
    root <- selected_predictors[min_sqr]
    var[j-1] <- colnames(X)[root]
    cut_point[j-1] <- as.numeric(criteria[min_sqr, 2])
    parents[j-1] <- criteria[min_sqr, 4] 
    
    
    # Checking the dimension of the possible new nodes to see if it 
    # they have the minimum of observations
    temps[j] <- m %>% 
      filter(node == parents[j-1]) %>% 
      mutate(groups =  ifelse(!!sym(var[j-1]) > cut_point[j-1], 'set1', 'set2')) %>% 
      group_by(groups) %>% 
      count() %>% 
      ungroup() %>% 
      arrange(n) %>% slice(1) %>% pull()
    
    
    # Creating the very first split 
    if(j == 2 && temps[2] >= keep_node){
      
      m <- m %>% 
        mutate(node = ifelse(!!sym(var[j-1]) > cut_point[j-1],
                             paste(node, iter + 1), paste(node, iter)))
      
      counter <- counter + 1
      iter <-  iter + 1
      
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
                                      paste(node, iter+counter+1), paste(node, iter+counter)), 
                               paste(node)))
        counter = counter + 1
        iter = iter + 1
        
        final_vars[counter] <- var[j-1] 
        final_cuts[counter] <- cut_point[j-1] 
        final_parents[counter] <- parents[j-1]
        
      } else {
        counter = counter
        iter = iter
      }
    }
    
    # Updating objects 
    selected_predictors <- selected_predictors[-min_sqr]
    n_pred <- n_pred - 1
    criteria <- matrix(NA, ncol = 4, nrow = n_pred)
    j <- j + 1
    
  }
  
  # Calculating the mean for each final node  
  m <- m %>% 
    group_by(node) %>% 
    mutate(prediction = round(mean(!!sym(response)), 3))
  
  # Calculating the means
  means <- m %>% 
    group_by(node) %>% 
    summarise(means = round(mean(!!sym(response)), 3)) %>% 
    pull(means)
  
  nodes <- m %>% distinct(node)
  
  return(list(model_data = m, 
              vars = final_vars, 
              cuts = as.numeric(final_cuts),
              parents = final_parents,
              means = means, 
              nodes = nodes))
  
}