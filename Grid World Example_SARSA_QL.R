#======================================================================
# Grid World : SARSA and Q learning
# Author : Junmo Nam
# BEGAS
#======================================================================



#======================================================================
# Set environment
#======================================================================

#reset session
rm(list = ls());gc(reset = T)

#load package
sapply(c('dplyr','ggplot2'),require,character.only = T)


#claim grid world environment
grid = matrix(0,5,5)
grid[2,3]=-1;grid[2,5]=-1;grid[3,4]=-1;grid[4,2]=-1;
grid[4,4] = 1


#claim SARSA parameters
params = list(
  epsilon = 0.1,
  discount = 0.9,
  learning_rate = 0.01
  
)

#sourcing function
dirname(rstudioapi::getActiveDocumentContext()$path) %>% paste0('/Grid World Solver.R') %>%
  source

#======================================================================
# do training : SARSA
#======================================================================

res = SARSA_grid(grid,params,100)


#======================================================================
# Visualizing policy
#======================================================================

draw_policy(grid,res$qpolicy,res$warning)


#======================================================================
# do training : QL
#======================================================================

res = QL_grid(grid,params,100)

#======================================================================
# Visualizing policy
#======================================================================

draw_policy(grid,res$qpolicy,res$warning)



#======================================================================
# More Complicated Grid
#======================================================================

#claim grid world environment
grid = matrix(0,7,9)
grid[2,3]=-1;grid[2,5]=-1;grid[3,4]=-1;grid[4,2]=-1;grid[4,6:9] = -1;grid[6:7,2] = -1;grid[5,5]=-1
grid[6,8] = 1


res = QL_grid(grid,params,100)
draw_policy(grid,res$qpolicy,res$warning)





