#----------------------------------------------------------------------#
# Using the mtcars data                                                #
# Bruna Wundervald                                                     #
#  December, 2018                                                      #
#----------------------------------------------------------------------#

# Load the functions & packages before running ------------------------
# Files: trees.R and random_forest.R

# Trees
# Using the function
tree_cars <- my_trees(formula = mpg ~ cyl + disp + hp + drat +
                        wt + qsec + am + gear + carb, 
                      mtcars)
tree_cars

# Finding the rss
sum((mtcars$mpg - tree_cars$model_data$prediction)^2)

# Random forests
# Using the rf function
rf_cars <- rf(formula = mpg ~ cyl + disp + hp + drat +
                wt + qsec + am + gear + carb, 
              rf_data = mtcars)

# Finding the rss
sum((mtcars$mpg - rf_cars$prediction)^2)

# Evaluating the number of trees
seq_number_trees <- seq(2, 100, by = 5)

sequence_rf <- seq_number_trees %>% map(rf, formula = mpg ~ cyl + disp + hp + drat +
                                          wt + qsec + am + gear + carb, rf_data = mtcars)

rss <- function(prediction) sum((mtcars$mpg - prediction)^2)

results <- sequence_rf %>% 
  map("prediction") %>% 
  map(rss) %>% 
  map_df(data.frame) %>% 
  setNames("rss") %>% 
  mutate(n = seq_number_trees)

results %>% 
  ggplot(aes(n, rss)) +
  geom_line(colour = '#c03728', linetype = 'dotted') +
  geom_point(colour = '#919c4c', size = 3) +
  labs(x = "Number of trees", y = "Residual sum of squares",
       title = "Results for the random forests applied to the mtcars data") +
  theme_bw()