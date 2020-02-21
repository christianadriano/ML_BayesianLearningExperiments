
"
Computes the Shannon Entropy for each question. 
Entropy is based on the proportion of Yes and Non-Yes answers for each question.
"

# load entropy library
install.packages("entropy")
library(entropy)

# observed counts for each bin
#position one = yes's, position two = no's
y = c(5, 5)
entropy(y, method="ML")
entropy(y, method="MM")
entropy(y, method="Jeffreys")
entropy(y, method="Laplace")
entropy(y, method="SG")
entropy(y, method="minimax")
entropy(y, method="CS")#
entropy(y, method="NSB")
entropy(y, method="shrink")

"Except for method=MM and CS, all the other methods
produced similar results for entropy."

#-----------------------------
#Dirichlet Prior Bayesian Estimators of Entropy

# Dirichlet estimate with a=1/2 (Jeffreys'prior)
entropy.Dirichlet(y, a=1/2)

# Dirichlet estimate with a=1 (Laplace prior)
entropy.Dirichlet(y, a=1)

"Also produced similar results to the Jeffrey's and Laplace method"