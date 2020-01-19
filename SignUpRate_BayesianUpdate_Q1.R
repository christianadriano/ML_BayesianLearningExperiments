"
Question I) Build a Bayesian model that answers the question: 
What would the rate of sign-up be if method A was used on a larger number of people?
source: http://www.sumsar.net/files/posts/2017-bayesian-tutorial-exercises/modeling_exercise1.html
"

# Number of random draws from the prior
n_draws <- 10000

prior <- runif(n_draws, min = 0, max = 1)# Here you sample n_draws draws from the prior  
hist(prior) # It's always good to eyeball the prior to make sure it looks ok.

# Here you define the generative model
generative_model <- function(prob_of_success){
  success <-  1
  trials <-  16
  subscribers <- rbinom(n=success, size=trials, prob=prob_of_success)
  print(subscribers)
}

# Here you simulate data using the parameters from the prior and the 
# generative model
sim_data <- rep(0, n_draws)
for(i in 1:n_draws) {
  sim_data[i] <-  generative_model(prior[i])
}

observed_data = 6 #people who signed up

# Here you filter off all draws that do not match the data.
posterior <- prior[sim_data == observed_data] 

hist(posterior) # Eyeball the posterior
length(posterior) # See that we got enought draws left after the filtering.
# There are no rules here, but you probably want to aim
# for >1000 draws.

# Now you can summarize the posterior, where a common summary is to take the mean
# or the median posterior, and perhaps a 95% quantile interval.
median(posterior)
quantile(posterior, c(0.025, 0.975))
