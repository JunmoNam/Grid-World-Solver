#======================================================================
# Grid World Solvers
#       Source Script for Grid World RL.R
# Author : Junmo Nam
# BEGAS
#======================================================================



#======================================================================
# For environment
#======================================================================


#step function for give information after do action
step = function(state,action){
  
  #x and y in state
  x = state$x
  y = state$y
  
  #define reward and next state by condition
  if(x==nrow(grid) & action == 'down' | y==ncol(grid) & action == 'right' |
     x==1 & action == 'up' | y==1 & action == 'left'){
    # action not allowed, stay
    reward = grid[x,y]
    next_state = state
  }else if(action == 'up'){
    reward = grid[x-1,y]  
    next_state = list(x = x-1,y = y)
  }else if(action == 'down'){
    reward = grid[x+1,y]
    next_state = list(x = x+1,y = y)
  }else if(action == 'left'){
    reward = grid[x,y-1]  
    next_state = list(x = x,y = y-1)
  }else{
    reward = grid[x,y+1]  
    next_state = list(x = x,y = y+1)
  }
  return(list(next_state = next_state,reward = reward))
}

#get action from given state
get_action = function(state,epsilon,qtable,actions){
  x = state$x
  y = state$y
  
  ifelse(sample(0:1,1,prob = c(epsilon,1-epsilon))==1, #epsilon check
         (filter(qtable,x==!!x,y==!!y) %>% #filter by state
            filter(q == max(q)) %>% #get maximum q
            sample_n(1) #sample one if there's multiple max q
         )$action, #do max q action
         sample(actions,1))#random action
  
}

#======================================================================
# Trainer : SARSA
#======================================================================

SARSA_grid = function(grid,params,n_iter){
  
  #allow only 4 actions
  actions = c('up','down','left','right')
  
  #get parameters
  epsilon = params$epsilon
  discount = params$discount
  learning_rate = params$learning_rate
  
  #claim q table
  qtable = expand.grid(x = 1:nrow(grid),y = 1:ncol(grid),action = actions,q = 0) %>%
    mutate(action = as.character(action))
  
  #do iteration
  p = progress_estimated(n_iter)
  
  for(i in 1:n_iter){
    
    warning = F
    
    #reset state
    state = list(x = sample(1:nrow(grid),1),y = sample(1:ncol(grid),1))
    
    #select action
    action = get_action(state,epsilon,qtable,actions)
    n_step = 0
    
    repeat{
      
      #define next state
      time_step = step(state,action)
      next_state = time_step$next_state
      reward = time_step$reward
      done = (reward ==1)
      next_action = get_action(next_state,epsilon,qtable,actions)
      
      #update q by SARSA : Bellman update
      q = filter(qtable,x==state$x,y==state$y,action==!!action)$q
      q2 = filter(qtable,x==next_state$x,y==next_state$y,action==!!next_action)$q
      new_q = q + learning_rate*(reward + discount*q2-q) 
      
      #get new q values to table
      qtable = qtable %>% 
        mutate(q = replace(q,x==state$x &
                             y==state$y &
                             action == !!action,new_q))
      
      #update states
      state = next_state
      action = next_action
      
      #step count
      n_step = n_step+1
      
      
      if(done){
        break
      }else if(n_step>100000){
        warning = T
        break
      }
      
    }#end repeat
    p$tick()$print()
    
    if(warning){
      cat('\n number of step is more than 100,000 : consider SARSA caught in inefficient loop \n break \n')
      break
    }
    
  }
  qpolicy = qtable %>% group_by(x,y) %>% filter(q == max(q)) %>% arrange(x,y)
  
  return(list(warning = warning, qpolicy = qpolicy,qtable = qtable))
}


#======================================================================
# Trainer : Q-learning
#======================================================================

QL_grid = function(grid,params,n_iter){
  
  #allow only 4 actions
  actions = c('up','down','left','right')
  
  #get parameters
  epsilon = params$epsilon
  discount = params$discount
  learning_rate = params$learning_rate
  
  #claim q table
  qtable = expand.grid(x = 1:nrow(grid),y = 1:ncol(grid),action = actions,q = 0) %>%
    mutate(action = as.character(action))
  
  #do iteration
  p = progress_estimated(n_iter)
  
  for(i in 1:n_iter){
    
    warning = F
    
    #reset state
    state = list(x = sample(1:nrow(grid),1),y = sample(1:ncol(grid),1))
    
    n_step = 0
    
    repeat{
      
      #select action
      action = get_action(state,epsilon,qtable,actions)
      
      #define next state
      time_step = step(state,action)
      next_state = time_step$next_state
      reward = time_step$reward
      done = (reward ==1)
      
      #update q by SARSA : Bellman update
      q = filter(qtable,x==state$x,y==state$y,action==!!action)$q
      q2 = reward + (filter(qtable,x==next_state$x,y==next_state$y) %>% filter(q==max(q)) %>% sample_n(1))$q
      new_q = q + learning_rate*(q2-q)
      
      #get new q values to table
      qtable = qtable %>% 
        mutate(q = replace(q,x==state$x &
                             y==state$y &
                             action == !!action,new_q))
      
      #update states
      state = next_state
      
      
      #step count
      n_step = n_step+1
      
      
      if(done){
        break
      }else if(n_step>100000){
        warning = T
        break
      }
      
    }#end repeat
    p$tick()$print()
    
    if(warning){
      cat('\n number of step is more than 100,000 : consider Q learning caught in inefficient loop \n break \n')
      break
    }
    
  }
  qpolicy = qtable %>% group_by(x,y) %>% filter(q == max(q)) %>% arrange(x,y)
  
  return(list(warning = warning, qpolicy = qpolicy,qtable = qtable))
}


#======================================================================
# Visualization
#======================================================================


#drawing
draw_policy = function(grid,qpolicy,warning){
  
  #mapping destination and obstacle 
  obstacle = which(grid==-1,arr.ind = T)
  destination = which(grid==1,arr.ind = T)
  
  if(warning){
    stop('Your policy has some problem : check object "warning" and policy before draw policy')
  }
  
  #check start point and action
  state = list(x=1,y=1)
  action = (qpolicy %>% filter(x==state$x,y==state$y))$action
  
  #make record dataframe
  record = data.frame(x=state$x,y=state$y)
  
  #do recording by given q policy
  repeat{
    
    #update state
    res = step(state,action)
    state = res$next_state
    done = (res$reward==1)
    
    #choose action by existing policy
    action = (qpolicy %>% filter(x==state$x,y==state$y))$action
    
    #update record
    record = rbind(record,data.frame(x=state$x,y=state$y))
    
    #done
    if(done){
      break
    }
  }
  
  #visualization : make empty grid object
  grid_plot = expand.grid(x=1:(ncol(grid)+1),y=1:(nrow(grid)+1)) %>% ggplot(aes(x,y))+
    geom_point(alpha=0)+
    geom_vline(xintercept = 1:(ncol(grid)+1))+
    geom_hline(yintercept = 1:(nrow(grid)+1))+
    ggtitle('Grid World Policy Viewer',
            subtitle = 'SARSA result')+
    theme_bw()
  
  #add obstacle and destination
  for(i in 1:nrow(obstacle)){
    x_add = obstacle[i,2]+0.5
    y_add = (nrow(grid)+1)-obstacle[i,1]+0.5
    grid_plot = grid_plot + 
      geom_point(aes(x = !!x_add, y= !!y_add),shape = 2,color = 'dark green',size = 3.5)
  }
  
  #add destination
  grid_plot = grid_plot + 
    geom_point(aes(x = destination[1,2]+0.5,
                   y= (nrow(grid)+1)-destination[1,1]+0.5),
               shape = 9,color = 'red',size = 4)
  
  #update empty plot
  for(i in 1:nrow(record)){
    x_add = record[i,2]+0.5
    y_add = (nrow(grid)+1)-record[i,1]+0.5
    
    grid_plot = grid_plot + 
      geom_point(aes(x = !!x_add,y = !!y_add),
                 color = 'blue',shape = 13, size = 1.8)
  }
  
  return(grid_plot)
  
}