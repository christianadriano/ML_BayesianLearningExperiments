" 
Simulate Bayesian Gaussian updating
"
library(stringr)
library(tidyverse)
library(gridExtra)

source(".//util//Multiplot.R")

# Run for 100 samples (or in the multi-armed bandit, pull an armm a episode times)
episodes <- 100

"
###Sampling of rewards###
Sample  one reward at a time from one arm, then update the likelihood.
Sample with replacement now, because assuming stationary reward process.
For non-stationary the samples will be without-replacement and will follow the time series of rewards.
" 

#Normalized rewards
#TODO how to work with non-normalized and non-scaled?
reward_list <- c(1, 0, 0.5, 0.6, 0.7, 0.5, 0.4, 0.3, 0.3, 0.5, 0.4)
plot(reward_list)
true_mean_rewards <- mean(reward_list)
true_mean_variance <- var(reward_list)

#---------------------------

incremental_variance <- function(n,x,mean, current_variance){
  
  variance_new <- ((n-2)/(n-1)) *current_variance + ((x-mean)^2)/n
  return(variance_new)
}

"
###PRIOR FOR PROBABILITY FOR REWARDS###

##Choosing a prior##
If we have a few data points from the rewards, we can use them to choose a prior that is
better than the uninformative prior.
Look at https://a-little-book-of-r-for-bayesian-statistics.readthedocs.io/en/latest/src/bayesianstats.html

When I know nothing, I will assume uninformative prior, a beta(1,1)
see: https://stats.stackexchange.com/questions/297901/choosing-between-uninformative-beta-priors
"

prior_mean_reward <- rbeta(n=episodes,shape1=1,shape2=1)
mean(prior_mean_reward)
var(prior_mean_reward)
#plot(density(prior_mean_reward))

prior <- prior_mean_reward

max_index = length(reward_list)
collected_rewards <- vector("list",episodes) #creates empty list of size "episodes" 
previous_mean <- 0
previous_variance <- 0

#plot configurations
updates <- 8
myplots_list <- list(); #vector('list',updates)
plot_colors=sample(LETTERS[1:updates], 100, replace=TRUE)
posterior_matrix <- matrix(data=NA,nrow = episodes,ncol = updates)
par(mfrow=c(2,4))

for (i in 1:updates){
  #sample one reward with replacement
  sampled_index <- sample(1:max_index, 1) #sample one integer 
  sampled_reward <- reward_list[sampled_index] 

  #This code by using incremental mean and incremental variance
  #So we do not keep track of all datapoints.

  old_mean_reward <- mean_reward
  old_variance <- sd_reward^2
  if(i == 1){ #First round
    mean_reward <- sampled_reward
    sd_reward <- 1;
  } else {
    mean_reward <- old_mean_reward + (sampled_reward - old_mean_reward)/i
    sd_reward <-sqrt(incremental_variance(i,sampled_reward,old_mean_reward,old_variance))
  }

  reward_likelihood <- rnorm(100, mean=mean_reward, sd=sd_reward) 
  #plot(density(reward_likelihood))
  
  posterior <- reward_likelihood * prior
  posterior_matrix[,i] <- posterior
  #update the prior
  prior <-  posterior
  
  #plot
  plot(density(posterior), main=str_c("Iter=",i))
 # par(new=TRUE)
}

#-------------------------------------------------------------
#Better Plots
#TODO multiplot with density using ggplot and different colors

df <- data.frame(data=posterior_matrix)
#colnames(df) <- 

plot_data_column = function (data, column,plot_colors) {
    ggplot(data, aes_string(x=column, fill=plot_colors)) + 
    geom_density(alpha=0.5) +
    xlab(column)
}


myplots_list[[i]] <- local({
  i <- i
  p1 <-plot(density(posterior), main=str_c("Posterior, iteration=",i))
  print(p1)
})

multiplot(plotlist=myplots_list,cols=1)


