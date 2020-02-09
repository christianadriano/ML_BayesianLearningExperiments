
"Approximate probability of YES using:
- Hypergeometric distribution as likelihood 
- Acceptance Probability method as Bayesian Update
- Markov Chain Monte Carlo as decision making

Adapted from the from https://www.r-bloggers.com/understanding-bayesian-inference-with-a-simple-example-in-r/

Hypergeometric distribution parameters:
nn = total number of answers
m = number of yes's (white balls in an urn model)
n = number of not yes's (black balls in an urn model)
k = number of answers sampled (balls drawn without replacement from an urn)
p = probability to obtain a positive answer (YES)

Query: 
Given a number of answers and the number of YESs (h), 
what is the probability p of obtaining a YES?

Bayes theorem tells me that my belief in my hypothesis AFTER seeing the data 
is proportional to how well that hypothesis explains the data times my INITIAL belief.

My hypothesis is the likelihood
My initial belief is the prior


"

library(stringr)

path <- "C://Users//Christian//Documents//GitHub//Complexity_Metrics//output//"
dataset_E2 <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))
df_E2 <- data.frame(dataset_E2)

task_id = 1
df <- df_E2[df_E2$microtask_id==task_id,]

# Step 1) Set an initial value for p (prior)
p <- runif(1, 0, 1)  #(random value from 0 to 1)

#Step 2) Propose a new value of p, called p-prime.
p_prime <- p + runif(1, -0.05, 0.05)

#Step 3) Compute the acceptance probability of this new value for the parameter. 
#We have to check if the new value improves the posterior probability given our data. 
#This can be seen as the ratio: Pr(p_prime|h) / Pr(p|h).

"The advantage of this method is that we avoid to compute the marginal likelihood, 
that is often difficult to obtain with more complex models.

Let's stop here a little bit to explain each term of this equation."

#Since it is sampling without replacement, 
#I need to remove the answer from the list
sample_without_replacement <- function(){
  index = trunc(runif(1,1,20))
  
  sample <- df$answer[index]
  df <- df[-c(index),]
  if(sample=="YES_THERE_IS_AN_ISSUE")
    return(1)
  else
    return(0)
}

"
Likehood as a binomial function
dhyper(x, m, n, k, log = FALSE)

x = size of the sample
nn = number of answers in the
n = number of black balls in the urn
m = number of white balls in the urn
m + n = total balls in the urn
k = number of balls drawn (sample size)
p = probability
"
likelihood <- function(x_vector_yes_answers, m_total_yes_answers, 
                       n_total_not_yes_answers, k_sample_size){  
  
  lh <- dhyper(x_vector_yes_answers, 
               m_total_yes_answers, 
               n_total_not_yes_answers, 
               k_sample_size)
  lh  
}  



# Set the numer of answer obtained (it inscreases as we sample more answers)
n <- 0
nn <- 20 #total number of answers per question

# Set the number of positive answers.  
total_yes_count <- 0  #successes

p <- 0.33 #probability  of one success

posterior <- data.frame() 
mean_posterior <- data.frame()

for(j in 1:20){
  n <- n+1
  
  #compute new ones
  total_yes_count <- total_yes_count + sample_without_replacement()
  not_yes_count <- abs(nn - total_yes_count)
  
  yes_vec <- total_yes_count:total_yes_count
  print(yes_vec)
  
  # Set the length of the loop (Markov Chain, number of iterations).  
  nrep <- 10
  #set the prior as the previous posterior
  
  # Start the loop (MCMC)  
  for (i in 1:nrep) {  
    #accumulated_yess <- accumulated_yess + sample_without_replacement()
    #n <- trunc( (accumulated_yess/i) * n)
    #if(n==0) n <- 1
    # Avoid values out of the range 0 - 1 
    
    # Obtain a new proposal value for p 
    p_prime <- p + runif(1, -0.05,0.05)  
    
    if (p_prime < 0) {p_prime <- abs(p_prime)}  
    if (p_prime > 1) {p_prime <- 2 - p_prime}  
    
    yes_count <- trunc(p_prime * nn)
    not_yes_count <- abs(nn - yes_count)
    old_yes_count <- trunc(p * nn)
    old_not_yes_count <- abs(nn - old_yes_count)
    print(paste("yes_count:",yes_count,"not_yes_count:",not_yes_count,
                "old_yes_count:",old_yes_count,"old_not_yes_count:",old_not_yes_count))

        # Compute the acceptance proability using our likelihood function and the  
    # beta(1,1) distribution as our prior probability.  
    R <- likelihood(yes_vec,yes_count,not_yes_count,1)/likelihood(yes_vec,old_yes_count,old_not_yes_count,1) * (dbeta(p_prime,1,1)/dbeta(p,1,1))  
    #print(paste0("R= ",R))
     # Accept or reject the new value of p  
    if (is.na(R) | R > 1) {R <- 1}  
    random <- runif (1,0,1)  
    if (random < R) {  
      p <- p_prime;  
      # Store the likelihood of the accepted p and its value
      posterior[i,1] <- log(likelihood(yes_vec,yes_count,not_yes_count,1));
    } 
    else{posterior[i,1] <- log(likelihood(yes_vec,old_yes_count,old_not_yes_count,1))}
      
    posterior[i,2] <- p  
    
    
    
    #print(i)  
    #print(normalized_yess)
  }  
  mean_posterior[j,1] <- mean(posterior[,2])
  mean_posterior[j,2] <- sd(posterior[,2])
  #
 # plot(mean_posterior[,1])
  
   par(mfrow= c(1,2))
   prior <- rbeta(nrep, 1,1)
   plot(1:nrep ,posterior$V2, cex=0, xlab = "generations", ylab = "p",
        main = "trace of MCMC\n accepted values of parameter p\n prior = beta(1,1) generations = 5000")
   lines(1:nrep, posterior$V2, cex=0)
   abline(h=mean(posterior$V2), col="red")
   plot(density(posterior$V2), xlim = c(min(min(prior),min((posterior$V2))), max(max(prior),max((posterior$V2)))),
        ylim = c(0, max(max(density(prior)$y),max((density(posterior$V2)$y)))), main= "prior VS posterior\n prior= beta(1,1)",
        lwd=3, col="red")
   lines(density(prior), lwd=3, lty=2, col="blue")
  # #legend("topleft", legend=c("prior density","posterior density"),
  #  #      col=c("blue","red"), lty=c(3,1), lwd=c(3,3), cex = 1)

  
} 

###################
# PLOTTING

plot(mean_posterior[,1])
hist(mean_posterior[,1])

par(mfrow= c(1,2))  
prior <- rbeta(5000, 1,1)  
plot(1:5000 ,posterior$V2, cex=0, xlab = "generations", ylab = "p",  
     main = "trace of MCMC\n accepted values of parameter p\n prior = beta(1,1) generations = 5000")  
lines(1:5000, posterior$V2, cex=0)  
abline(h=mean(posterior$V2), col="red")  
plot(density(posterior$V2), xlim = c(min(min(prior),min((posterior$V2))), max(max(prior),max((posterior$V2)))),   
     ylim = c(0, max(max(density(prior)$y),max((density(posterior$V2)$y)))), main= "prior VS posterior\n prior= beta(1,1)",  
     lwd=3, col="red")  
lines(density(prior), lwd=3, lty=2, col="blue")  
legend("topleft", legend=c("prior density","posterior density"),  
       col=c("blue","red"), lty=c(3,1), lwd=c(3,3), cex = 1)

###############################

