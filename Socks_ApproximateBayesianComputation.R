"
Approximate Bayesian Computation

Show how ABC can easily approximate a prior distribution when there is not enough data.
This is the case of my 10 to 20 answers per question.

"

n_picked <- 11 # The number of socks to pick out of the laundry

sock_sim <- replicate(100000, {
  # Generating a sample of the parameters from the priors
  prior_mu <- 30
  prior_sd <- 15
  prior_size <- -prior_mu^2 / (prior_mu - prior_sd^2)
  n_socks <- rnbinom(1, mu = prior_mu, size = prior_size)
  prop_pairs <- rbeta(1, shape1 = 15, shape2 = 2)
  n_pairs <- round(floor(n_socks / 2) * prop_pairs)
  n_odd <- n_socks - n_pairs * 2
  
  # Simulating picking out n_picked socks
  socks <- rep(seq_len(n_pairs + n_odd), rep(c(2, 1), c(n_pairs, n_odd)))
  picked_socks <- sample(socks, size =  min(n_picked, n_socks))
  sock_counts <- table(picked_socks)
  
  # Returning the parameters and counts of the number of matched 
  # and unique socks among those that were picked out.
  c(unique = sum(sock_counts == 1), pairs = sum(sock_counts == 2),
    n_socks = n_socks, n_pairs = n_pairs, n_odd = n_odd, prop_pairs = prop_pairs)
})

# just translating sock_sim to get one variable per column
sock_sim <- t(sock_sim)
head(sock_sim)

post_samples <- sock_sim[sock_sim[, "unique"] == 11 &
                           sock_sim[, "pairs" ] == 0 , ]

data= post_samples[,"n_socks"]
hist(data, col="lightblue")
abline(v=median(data),col="red")
