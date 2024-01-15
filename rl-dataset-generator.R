# CMPT318 Term Project Part 3
# Authors: Rebecca Reedel (301454910), Asmita Srivastava (301436340), Mrinal Goshalia (301478325)
library(ReinforcementLearning)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(fmsb)


states <- as.character(seq(1, 100))
actions <- c("Stocks","Real_Estates","Commodities","Cryptocurrencies","Forex")

# profit is a portion of the invested money
profit <- function(investment)
{
  floor(2*runif(1) * investment)
  
}


market.env <- function(state, action)
{
  # agent's budgets
  budget <- as.numeric(state)
  
  # reward defined as a function below to calculate the new budget
  calculate_reward <- function(budget, investment, expected_profit)
  {
    # for affordable items
    if ((budget - investment) > 0)
    {
      reward = log(budget - investment + expected_profit)
    }
    
    # and if the agent wants to invest on something out of budget
    else
    {
      reward = budget - investment + expected_profit
    }
    
  } # Function R
  
  # initial new budget
  next_state <- budget
  
  if (state == state(budget) && action == "Stocks")
  {
    expected_profit <- profit(5)
    next_state <-
      state(budget - 5 + expected_profit)
    reward <- calculate_reward(budget, 5, expected_profit)
  }
  
  if (state == state(budget) && action == "Real_Estates")
  {
    expected_profit <- profit(17)
    next_state <-
      state(budget - 17 + expected_profit)
    reward <- calculate_reward(budget, 17, expected_profit)
  }
  
  if (state == state(budget) && action == "Commodities")
  {
    expected_profit <- profit(11)
    next_state <-
      state(budget - 11 + expected_profit)
    reward <- calculate_reward(budget, 11, expected_profit)
  }
  
  if (state == state(budget) && action == "Cryptocurrencies")
  {
    expected_profit <- profit(9)
    next_state <-
      state(budget - 9 + expected_profit)
    reward <- calculate_reward(budget, 9, expected_profit)
  }
  
  if (state == state(budget) && action == "Forex")
  {
    expected_profit <- profit(7)
    
    next_state <-
      state(budget - 7 + expected_profit)
    reward <- calculate_reward(budget, 7, expected_profit)
  }
  
  
  out <- list(NextState = as.character(next_state), Reward = reward)
  return(out)
}


data <- sampleExperience(
  N = 3000,
  env = market.env,
  states = states,
  actions = actions,
  actionSelection = "random",
)

# TASK: train an agent capable of making investment decisions regardless of the budget available.

head(data)

# Explain the hyper parameters of this model (alpha, gamma, epsilon) and how they influence the training process.
control <- list(alpha = 0.2, gamma = 0.6, epsilon = 0.2)

# Perform reinforcement learning
model <- ReinforcementLearning(data, 
                               s = "State", 
                               a = "Action", 
                               r = "Reward", 
                               s_new = "NextState", 
                               control = control)


#Obtain the Q-Table of the trained model and provide your interpretation of this table
#based on the values of the hyperparameters you chose
print(model)

#Compute the policy of the model and provide your interpretation.
computePolicy(model)


#### UNSEEN DATA ####

data_unseen <- data.frame(State = as.character(seq(15, 45)),
                          stringsAsFactors = FALSE)

#Apply this model to unseen data and extract the optimal action table for each state

# Pick optimal action
data_unseen$OptimalAction <- predict(model, data_unseen$State)

# Print only the reward to 2 decimal places
print(data_unseen)

