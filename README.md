This is a setup and example, also found in the vignette.

In the console run the following:

```{r }
library(devtools)
install_github("Arisina2309/BLB")
```

Here are some test codes to see how you can use the functions inside the BLB package for linear and quantile regression. 

# Linear regression

```{r }
n <- 1000 # Draws from X 
d <- 100 # Dimension of X
b = floor(n^(.7)) # subset size
X <- matrix(0, nrow = n, ncol = d)
for(i in 1:d){
  X[,i] <- rt(n, 3) # T3 generation
}
Y <- X %*% rep(1,d) + rnorm(n,mean = 0, sd = 10) # Linear generation of Y
r <- 50 # number of Monte Carlo iterations
s <- 10 # number of sampled subsets
blb_linear_regression(X, Y, b, s, r)
```

# Quantile regression

```{r }
n <- 1000 # Draws from X 
d <- 100 # Dimension of X
b = floor(n^(.7)) # subset size
X <- matrix(0, nrow = n, ncol = d)
for(i in 1:d){
  X[,i] <- rt(n, 1) # T1/Cauchy generation
}
Y <- X %*% rep(1,d) + rnorm(n,mean = 0, sd = 10) # Linear generation of Y
r <- 50 # number of Monte Carlo iterations
s <- 10 # number of sampled subsets
q <- 0.9 # quantile for quantile regression
blb_quantile_regression(X, Y, b, s, r, q)
```
