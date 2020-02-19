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

#Since it is sampling without replacement, 
#I need to remove the answer from the list
sample_without_replacement <- function(df_question,sample_size){
  yes_answers=0
  for(n in 1:sample_size){
    index = trunc(runif(1,1,length(df_question$answer)))
    sample <- df_question$answer[index]
    df_question <- df_question[-c(index),] #remove answer from the list
    if(sample=="YES_THERE_IS_AN_ISSUE")
      yes_answers = yes_answers + 1;
  }
  return(yes_answers)
}

#-------------------
#Produces a likelihood function using the hypergeometric distribution
"Choices of hypergeometric functions
phyper gives the cumulative probability upto and including my observation,
e.g.,P(observing 3 or less yes's) 
dhyper gives me exactly the probability of P(Observing 3 yes's)
x = the exact number of yes's that I am interested in
successes = how many white balls in the urn
fails = how many red balls in the urn
sample_size = how many balls to draw
"
compute_likelihood <- function(x,successes, unsuccesses, sample_size){
  return(dhyper(x,successes,unsuccesses, sample_size)[1])
}

#--
# Testing the likelihood function
sample_size=1 #how many answers to sample at each cycle
number_Yes=1
likelihood_matrix <- matrix(NA,2,21)
#computes the likelihood for each hypothesis (total_yes for a question)
for(total_yes in 0:20){
  likelihood_matrix[1,total_yes+1] <- paste0("H",total_yes)
  likelihood_matrix[2,total_yes+1] <- initialize_likelihood_functions(number_Yes,
                                                                     total_yes,
                                                                     20-total_yes,sample_size) 
}
#--------------------------------

likelihood_df <- data.frame(likelihood_matrix);
colnames(likelihood_df) <- likelihood_matrix[1,];

#---------------------------------
#Compute the posterior for one question
compute_posterior <- function(df_question,sample_size){
  posterior_matrix <- matrix(nrow = 21, ncol = 20)
  likelihood_vec <- matrix(rep(NA, 21),1, 21)
  prior_vec = matrix(rep(1/20, 21),1, 21)
  yes_count <- 0;
  for(i in 1:20){ #cycles of answering
    yes_count <- yes_count + sample_without_replacement(df_question,sample_size)
    
    #computes the likelihood for each hypothesis
    for(hypothesis in 0:20){
      likelihood_vec[hypothesis+1] <-compute_likelihood(yes_count,
                                                        hypothesis,20-hypothesis,
                                                        i) 
    }
    
    #multiply by the likelihoods and the prior
    posterior_vec = likelihood_vec * prior_vec
    
    #normalize the posterior so it ranges from 0 to 1
    posterior_vec <- posterior_vec / sum(posterior_vec)
    
    #posterior becomes next prior
    prior_vec <- posterior_vec 
    posterior_matrix[,i] <- posterior_vec
  }
  return(posterior_matrix)
}

#------------------------
"Redo this using regular expression"
create_labels <- function(){
  labels_vec <- vector("list", 21)
  for(i in 0:20) {
    labels_vec[i+1] <- paste0("H",i)
  }
  return(labels_vec);
}

#-------------------------
"Copy new set of posterior probabilities of a task"
copy_posterior_matrix <- function(df_posterior,posterior_matrix, 
                                  file_name,task_id){
  row_index <- dim(df_posterior)[1]

  for(i in 1:20){#traverses all columns of the matrix
    for(j in 1:21){#traverses all lines of the matrix
      #browser()
      df_posterior[row_index,j+2] <- posterior_matrix[j,i]
    }
    df_posterior[row_index,"file_name"] <- file_name
    df_posterior[row_index,"task_id"] <- task_id
    
    row_index <- row_index+1;
  }
  return(df_posterior)
}

#-------------------------
"Compute the posterior for all questions within a method"
compute_for_all_questions <- function(){
  java_methods = c("HIT01_8") #,"HIT02_4")
  start_task = 1
  end_task = 10
  sample_size=1
  df_posterior_instances <- data.frame(matrix(nrow = 1,ncol = 24))
  colnames(df_posterior_instances) <- c("file_name","task_id",create_labels())
  for(method in java_methods){
    df <- df_E2[df_E2$file_name == method,]
    for(task_id in start_task:end_task){
      df_question <- df[df$microtask_id == task_id,];
      posterior_matrix <- compute_posterior(df_question,sample_size);
      df_posterior_instances <- copy_posterior_matrix(df_posterior_instances,
                                                        posterior_matrix, 
                                                        method,task_id);
    }
    
  }
  return(df_posterior_instances)
}
---------------
#MAIN
df_posterior_instances <- compute_for_all_questions()

write.csv(df_posterior_instances,str_c("C://Users//Christian//Documents//GitHub//ML_BayesianLearningExperiments//output//", "E2_posterior_instances.csv"), 
               row.names = TRUE)

#---------------------------------------------------
#PLOTTING

matplot(posterior_matrix[,1:10], type = c("b"),pch=1,col = 1:10) #plot
legend("topleft", legend = 1:10, col=1:10, pch=1) # optional legend

matplot(posterior_matrix[,11:15], type = c("b"),pch=1,col = 1:5) #plot
legend("topleft", legend = 1:5, col=1:5, pch=1) # optional legend

matplot(posterior_matrix[,15:20], type = c("b"),pch=1,col = 1:6) #plot
legend("topleft", legend = 1:6, col=1:6, pch=1) # optional legend


matplot(posterior_matrix[,1:20], type = c("b"),pch=1,col = 1:20,
        xlab="Hypothesis on the number of Yes's", 
        ylab ="Probability of the hypothesis"
      ) #plot

legend("topleft", fill = 1:20, ncol = 5,
       cex = 0.75, legend = 1:20, col=1:20, pch=1) # optional legend
title("Posterior updated after 20 cycles of answers")
