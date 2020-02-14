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
  yes_answers=0
  for(n in 1:sample_size){
    index = trunc(runif(1,1,length(df$answer)))
    sample <- df$answer[index]
    df <- df[-c(index),] #remove answer from the list
    if(sample=="YES_THERE_IS_AN_ISSUE")
      yes_answers = yes_answers + 1;
  }
  return(yes_answers)
}

#Produces a likelihood function using the hypergeometric distribution
#

"Choices of hypergeometric functions
phyper gives the cumulative probability upto and including my observation,
e.g.,P(observing 3 or less yes's) 
dhyper gives me exactly the probability of P(Observing 3 yes's)
x = the exact number of yes's that I am interested in
successes = how many white balls in the urn
fails = how many red balls in the urn
sample_size = how many balls to draw
"
compute_likelihood_functions <- function(x,successes, unsuccesses, sample_size){
  return(dhyper(x,successes,unsuccesses, sample_size)[1])
}

sample_size=1 #how many answers to sample at each cycle
number_Yes=1
likelihood_matrix <- matrix(NA,2,21)
#computes the likelihood for each hypothesis (total_yes for a question)
for(total_yes in 0:20){
  likelihood_matrix[1,total_yes+1] <- paste0("H",total_yes)
  likelihood_matrix[2,total_yes+1] <-initialize_likelihood_functions(number_Yes,
                                                                     total_yes,
                                                                     20-total_yes,sample_size) 
}

likelihood_df <- data.frame(likelihood_matrix);
colnames(likelihood_df) <- likelihood_matrix[1,];


prior_vec = matrix()
yes_count <- 0;
for(i in 1:6){ #do 6 cycles of answering
  yes_count <- yes_count + sample_without_replacement(sample_size)
  
  #computes the likelihood for each hypothesis
  for(hypothesis in 0:20){
    likelihood_vec[hypothesis+1] <-compute_likelihood(yes_count,hypothesis,20-hypothesis,sample_size) 
  }
  
  #multiply by the likelihoods and prior
  posterior_vec = likelihood_vec * prior_vec
  
  #normalize posterior
  posterior_vec <- posterior_vec / sum(posterior_vec)
  
  #posterior becomes next prior
  prior_vec <- posterior_vec 
  
}