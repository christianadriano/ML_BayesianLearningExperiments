
"Approximate probability of YES using Bayesian Update and Acceptance Probability method

Example from https://www.r-bloggers.com/understanding-bayesian-inference-with-a-simple-example-in-r/

n = the number of answers
p = probability to obtain a positive answer (YES)

Query: 
Given a number of answers and the number of YESs (h), 
what is the probability p of obtaining a YES?

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

sample_yes <- function(){
  index = trunc(runif(1,1,20))
  
  sample <- df$answer[index]
  if(sample=="YES_THERE_IS_AN_ISSUE")
    return(1)
  else
    return(0)
}

#Likehood as a binomial function
likelihood <- function(h, n, p){  
  lh <- dbinom(h, n, p)  
  lh  
}  

#prior as a beta function
dbeta(p, 1, 1)

# Set the numer of tosses.  
n <- 20
# Set the number of positive answers.  
h <- 4 

"Now, the acceptance probability (R, see equations in Step 3) will
be the minimum value: 1 or the ratio of posterior
probabilities given the different p. We express this equation in R language as follows"

R <- likelihood(h,n,p_prime)/likelihood(h,n,p) * (dbeta(p_prime,1,1)/dbeta(p,1,1))  

posterior <- data.frame()  

accumulated_yess <- 0

p <- 0.3

# Set the length of the loop (Marcov Chain, number of iterations).  
nrep <- 5000  
# Start the loop (MCMC)  
for (i in 1:nrep) {  
  #accumulated_yess <- accumulated_yess + sample_yes()
  #n <- trunc( (accumulated_yess/i) * n)
  #if(n==0) n <- 1
  # Avoid values out of the range 0 - 1 
  
  # Obtain a new proposal value for p 
  p_prime <- p + runif(1, -0.05,0.05)  
  
  if (p_prime < 0) {p_prime <- abs(p_prime)}  
  if (p_prime > 1) {p_prime <- 2 - p_prime}  
  # Compute the acceptance proability using our likelihood function and the  
  # beta(1,1) distribution as our prior probability.  
  R <- likelihood(h,n,p_prime)/likelihood(h,n,p) * (dbeta(p_prime,1,1)/dbeta(p,1,1))  
  # Accept or reject the new value of p  
  if (R > 1) {R <- 1}  
  random <- runif (1,0,1)  
  if (random < R) {  
    p <- p_prime  
  }  
  # Store the likelihood of the accepted p and its value  
  posterior[i,1] <- log(likelihood(h, n, p))  
  posterior[i,2] <- p  
  print(i)  
  #print(normalized_yess)
}  

###################
# PLOTTING

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

