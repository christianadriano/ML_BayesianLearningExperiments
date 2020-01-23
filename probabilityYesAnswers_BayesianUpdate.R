" 
Simulate Bayesian Binomial updating
"
library(stringr)

path <- "C://Users//Christian//Documents//GitHub//Complexity_Metrics//output//"
dataset_E2 <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))
df_E2 <- data.frame(dataset_E2)

task_id = 1
df <- df_E2[df_E2$microtask_id==task_id,]

sim_bayes<-function(p=0.5,N=10,y_lim=15)
{
  success<-0
  curve(dbeta(x,1,1),xlim=c(0,1),ylim=c(0,y_lim),xlab='p',ylab='Posterior Density',lty=2)
  legend('topright',legend=c('Prior','Updated Posteriors','Final Posterior'),lty=c(2,1,1),col=c('black','black','red'))
  for(i in 1:N)
  {
    if(runif(1,0,1)<=p) #Substitute for if answer==YES
      success<-success+1
    
    curve(dbeta(x,success+1,(i-success)+1),add=TRUE)
    print(paste(success,"successes and ",i-success," failures"))
  }
  curve(dbeta(x,success+1,(i-success)+1),add=TRUE,col='red',lwd=1.5) #Final posterior
}

sim_bayes(p=0.6,N=90)

# True distribution
p <- 0.3
n <- 10

#add new answer
num_yes <- 1 #
#recompute probability (#Yes/Total Answers)

"
###Sampling of answers
Sample one answer at a time from the question, then update the likelihood.
Sample with replacement now, so I can use binomial, because I am only
interested in Yes's (success) and not-Yes (failure). Not-Yes are No and I don't know
answers.

In the future I will sample without replacement, hence will have to use hypergeometric distribution"

"PRIOR FOR PROBABILITY OF YES,s
When I know nothing, the probability of a YES answer is 1/3, 
because the answer options are 3 in E2.
"
prior_prob_yes <- rbeta(100,1,2)
mean(prior_prob_yes)
var(prior_prob_yes)
plot(density(prior_prob_yes))

prior <- prior_prob_yes

answerList <- df$answer
# Run for 20 samples
i <- 1
success = 0

i <- i+1
for (i in 1:length(answerList)) {
  if(answerList[i]=="YES_THERE_IS_AN_ISSUE"){
    success <- success + 1
  }
  p <- success / i
  p_likelihood <- rbinom(100, 1, p) 
  plot(density(p_likelihood))
  posterior <- p_likelihood * prior
  plot(density(posterior))
  
  prior <-  posterior
}

x <- rbeta(n=500, shape1=2, shape2=2)
est.par <- eBeta(x);
est.par
plot(est.par)