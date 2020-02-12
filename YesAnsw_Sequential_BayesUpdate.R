" 
Bayes update assuming each question has 21 possible hypotheses.

My distribution of answers is exchangeable, i.e., the probability of a sequence of
anwers is independent of their order.

H0 - zero number of YES's out of 20
H1 - One Ye's out of 20
H2 - two Yes' out of 20
...
H20 - all Ye's

Prior for each hypothesis= 1/21

"

library(stringr)

path <- "C://Users//Christian//Documents//GitHub//Complexity_Metrics//output//"
dataset_E2 <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))
df_E2 <- data.frame(dataset_E2)

task_id = 1
df <- df_E2[df_E2$microtask_id==task_id,]

#Since it is sampling without replacement, 
#I need to remove the answer from the list
sample_without_replacement <- function(sample_size){
  index = trunc(runif(1,1,20))
  
  sample <- df$answer[index]
  df <- df[-c(index),]
  if(sample=="YES_THERE_IS_AN_ISSUE")
    return(1)
  else
    return(0)
}

#Produces a likelihood function using the hypergeometric distribution
#

"Choices of hypergeometric functions
phyper gives the cumulative probability upto and including my observation,
e.g.,P(observing 3 or less yes's) 
dhyper gives me exactly the probability of P(Observing 3 yes's)
successes = how many white balls in the urn
fails = how many red balls in the urn
sample_size = how many balls to draw
"
initialize_likelihood_functions <- function(successes, unsuccesses, sample_size){
  x <- 0:(sample_size+1)
  return (dhyper(x,successes,answer_set-succcesses, sample_size))
}

sample_size=1 #how many answers to sample at each cycle
i=1

for(i in 1:6){
  n <- n+1
  h <- h + sample_without_replacement(sample_size)
  print(h)
  
  hypotheses_vec <- 
  likelihood_vec <- 
    
  #computes the likelihood for each hypothesis
  for(m in 0:20){
    likelihood_vec[m] <-compute_likelihood(m,20-m,sample_size) 
    
  }
  
  
}