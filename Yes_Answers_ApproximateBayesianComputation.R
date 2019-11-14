"
Approximate Bayesian Computation, similar to Grid computation.

Trying to approximate the probability of one Yes answer after N answers from participants with
different programming skill (score).

Each question would have a posterior probability.

I want to estimate how many Yes´s would come out if I ask the question to N different participants
with different programming skills 

Priors:
- Prior probability on the distribution of skill within each question
- Prior probability on the average number of Yes. Since I don't whether the answer is a NO or IDK,
I can assume two types of answers (YES and NOT YES). This way, I can use a binomial distribution
as the generative process.

P()
"
n_picked <- 5 # The number of answers to sample from a question

answers_sim <- replicate(100000, {
  # Generating a sample of the parameters from the priors
  prior_mu <- 3 #average number of Yes 
  prior_sd <- 2
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
