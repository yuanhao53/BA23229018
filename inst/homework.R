## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----iris---------------------------------------------------------------------
library(knitr)
kable(head(iris),row.names = TRUE, align="c")

## ----split--------------------------------------------------------------------
hist(iris$Sepal.Length, main = "Sepal.Length")
hist(iris$Sepal.Width, main = "Sepal.Width")
hist(iris$Petal.Length, main = "Petal.Length")
hist(iris$Petal.Width, main = "Petal.Width")

## ----women--------------------------------------------------------------------
library(knitr)
kable(women,row.names=TRUE,align="c")

## ----fit----------------------------------------------------------------------
fit <- lm(weight~height, data=women)
summary(fit)
plot(women$height,women$weight,xlab="Height",ylab="Weight")
abline(fit)

## ----ttest--------------------------------------------------------------------
library(knitr)
setosa <- iris[which(iris$Species=='setosa'),]
kable(head(setosa), row.names = TRUE, align = "c")
versicolor <- iris[which(iris$Species=='versicolor'),]
kable(head(versicolor), row.names = TRUE, align = "c")
t.test(setosa$Sepal.Length, versicolor$Sepal.Length)

## ----mysample-----------------------------------------------------------------
# Function Definition
mysample <- function(x, size = NULL, replace=FALSE, prob= NULL){
  if(is.null(size)|(!replace&&size>length(x))){
    size = length(x)
  } #size with (replace=FALSE) should not exceed length(x)
  if(is.null(prob)|(length(prob)!=length(x))){
    prob = rep(1/length(x),length(x))
  } #prob should meet the length with x
  prob = prob/sum(prob)# normalized
  if(replace){
    cp <- cumsum(prob)
    U <- runif(size)
    r <- x[findInterval(U,cp)+1]
    return(r)
  }
  else{
    r <- c()
    for(i in 1:size){
      cp <- cumsum(prob)
      U <- runif(1)
      idx <- findInterval(U,cp)+1
      temp <- x[idx]
      r <- append(r, temp)
      prob <- prob[-idx]
      prob <- prob/sum(prob)
      x <- x[-idx]
    }
    return(r)
  }
}
# Test of Validity
test_sample <- mysample(c(1,5,10,20), size = 1e5, replace = TRUE, prob = 1:4/10)
ct <- as.vector(table(test_sample))
ct/sum(ct)/(1:4/10)

## ----Permutation--------------------------------------------------------------
#Letters Permutation
test_sample <- mysample(letters, replace = FALSE)
test_sample

## -----------------------------------------------------------------------------
# Set rho values
rho <- c(0.001,0.5,1)
# Initialize
n <- 1e6
K <- 100
# Storage
var_results <- numeric(length(rho))
# Monte Carlo simulation
for(i in 1:length(rho)){
  rho_value <- rho[i]
  results <- replicate(K, expr = {
    X <- runif(n, 0, 1/2)
    Y <- runif(n, 0, pi/2)
    pi_hat <- 2*rho_value/mean(rho_value/2*sin(Y)>X)
    return(pi_hat)
  })
  var_results[i] <- var(results)
}

data.frame(Rho = rho, Variance = var_results)

## -----------------------------------------------------------------------------
cov <- exp(1)-(exp(1)-1)^2
cov
var <- (exp(2)-1)/2-(exp(1)-1)^2
var


## -----------------------------------------------------------------------------
var1 <- var/2
var2 <- (var+cov)/2
percent <- (var1-var2)/var1
percent

## -----------------------------------------------------------------------------
n <- 1e4
rep <- 1e4
# Replicate for variance calculation
simplemc <- replicate(rep, expr ={
  mean(exp(runif(n)))
})
antimc <- replicate(rep, expr = {
  u <- runif(n/2)
  v <- 1-u
  mean((exp(u)+exp(v))/2)
})
mean(simplemc)
mean(antimc)
percent <- (var(simplemc)-var(antimc))/var(simplemc)
percent


## -----------------------------------------------------------------------------
m <- 10000
theta.hat <- se <- numeric(2)

g <- function(x){
  x^2/sqrt(2*pi)*exp(-x^2/2)*(x>1)
}

# f1
f1 <- function(x){
  1/sqrt(2*pi)*exp(-x^2/2)
}
x <- rnorm(m)
fg <- g(x)/f1(x)
theta.hat[1] <- mean(fg)
se[1] <- sd(fg)

# f2
f2 <- function(x){
  0.5*exp(-0.5*x)
}
x <- rexp(m,0.5)
fg <- g(x)/f2(x)
theta.hat[2] <- mean(fg)
se[2] <- sd(fg)

table <- data.frame(
  Estimation = theta.hat,Variance = se
)
kable(table)

# Visualization
x <- seq(1.01,10,0.01)
plot(x,g(x), type = 'l', ylab = 'functions')
lines(x,f1(x),lty = 2)
lines(x,f2(x),lty = 3)
legend("topright", legend = c("g(x)","f1(x)","f2(x)"), lty = 1:3)

## -----------------------------------------------------------------------------
m <- 10000
mc1 <- replicate(1000, expr = {
  x <- rnorm(m)
  fg <- g(x)/f1(x)
  return(mean(fg))
})
mc2 <- replicate(1000, expr = {
  x <- rexp(m, 0.5)
  fg <- g(x)/f2(x)
  return(mean(fg))
})
c(mean(mc1), mean(mc2))
c(var(mc1),var(mc2))

## -----------------------------------------------------------------------------
true.theta <- integrate(g,lower = 1, upper = Inf)
abs(true.theta$value-mean(mc1))
abs(true.theta$value-mean(mc2))

## -----------------------------------------------------------------------------
m <- 10000
intervals <- 5
theta.hat <- numeric(intervals)
se <- numeric(intervals)

g <- function(x){
  exp(-x)/(1+x^2)
}
for(j in 1:intervals){
  u <- runif(m, (j-1)/intervals, j/intervals)
  x <- -log(1-(1-exp(-1))*u) #inversion
  f <- function(x){
    5/(1-exp(-1))*exp(-x)
  }
  fg <- g(x)/f(x)
  theta.hat[j] <- mean(fg)
  se[j] <- var(fg)
}

sum(theta.hat)
sqrt(mean(se))

# True value
true.value <- integrate(g,lower = 0, upper = 1)
1 - abs(true.value$value - sum(theta.hat))/abs(true.value$value - 0.5257801)
1 - sqrt(mean(se))/0.0970314

## -----------------------------------------------------------------------------
n <- 20
root.n <- sqrt(n)
t.interval <- qt(c(0.025,0.975), df = n-1)
CI <- replicate(10000, expr = {
  x <- rchisq(n, df = 2)
  ci <- mean(x) + t.interval * sd(x)/root.n
})
sum(CI[1, ] < 2 & CI[2, ] > 2)
mean(CI[1, ] < 2 & CI[2, ] > 2)

## -----------------------------------------------------------------------------
N <- 10000  
alpha <- 0.05 
mu0 <- 1 


#Chi-Square
df <- 1
type_I_errors <- 0
for (i in 1:N) {
  sample_data <- rchisq(n = 30, df = df)
  t_test <- t.test(sample_data, mu = mu0, alternative = "two.sided")
  if (t_test$p.value < alpha) {
    type_I_errors <- type_I_errors + 1
  }
}
empirical_type_I_error_rate <- type_I_errors / N
cat("Empirical Type I Error Rate with Chi-square(2):",  empirical_type_I_error_rate, "\n")

# Uniform(0,2)
type_I_errors <- 0
for (i in 1:N) {
  sample_data <- runif(n = 30, min = 0, max = 2)
  t_test <- t.test(sample_data, mu = mu0, alternative = "two.sided")
  if (t_test$p.value < alpha) {
    type_I_errors <- type_I_errors + 1
  }
}
empirical_type_I_error_rate <- type_I_errors / N
cat("Empirical Type I Error Rate with Uniform(0,2):",  empirical_type_I_error_rate, "\n")

# Exponential(1)
type_I_errors <- 0
for (i in 1:N) {
  sample_data <- rexp(n = 30, rate = 1)
  t_test <- t.test(sample_data, mu = mu0, alternative = "two.sided")
  if (t_test$p.value < alpha) {
    type_I_errors <- type_I_errors + 1
  }
}
empirical_type_I_error_rate <- type_I_errors / N
cat("Empirical Type I Error Rate with Exponential(1):",  empirical_type_I_error_rate, "\n")

## -----------------------------------------------------------------------------
M <- m <- 1000
alpha <- 0.1
FWER1 <- FDR1 <- TPR1 <- numeric(M)
FWER2 <- FDR2 <- TPR2 <- numeric(M)
# M Simulations
for(i in 1:M){
  p_value0 <- runif(0.95*m)
  p_value1 <- rbeta(0.05*m, 0.1, 1)
  
  # Bonferroni adjustment
  adj1 <- p.adjust(c(p_value0, p_value1), method = "bonferroni")
  # B-H
  adj2 <- p.adjust(c(p_value0, p_value1), method = "BH")
  
  result1 <- adj1 > 0.1
  result2 <- adj2 > 0.1
  true_result <- c(rep(TRUE, times = 0.95*m), rep(FALSE, times = 0.05*m))
  
  # All metrics
  FWER1[i] <- sum(result1[0:950]==FALSE)/950
  FWER2[i] <- sum(result2[0:950]==FALSE)/950
  FDR1[i] <- sum(result1[0:950]==FALSE)/sum(result1==FALSE)
  FDR2[i] <- sum(result2[0:950]==FALSE)/sum(result2==FALSE)
  TPR1[i] <- sum(result1[951:1000]==FALSE)/0.05/m
  TPR2[i] <- sum(result2[951:1000]==FALSE)/0.05/m
}

data.frame(FWER = c(sum(FWER1), sum(FWER2)), FDR = c(mean(FDR1),mean(FDR2)), TPR = c(mean(TPR1),mean(TPR2)))


## -----------------------------------------------------------------------------
library(boot)
library(MASS)

lambda <- 2
sample_size <- c(5,10,20)
B <- 1000
m <- 1000

bias_mean <- numeric(length(sample_size))
se_mean <- numeric(length(sample_size))

# Replicate process
for(n in 1:length(sample_size)){
  bias <- se <- numeric(m)
  for(i in 1:m){
    x <- rexp(sample_size[n], rate = lambda)
    theta <- 1/mean(x)
    thetastar <- numeric(B)
    for(b in 1:B){
      xstar <- sample(x, replace = TRUE)
      thetastar[b] <- 1/mean(xstar)
    }
    bias[i] <- mean(thetastar)-theta
    se[i] <- sd(thetastar)
  }
  bias_mean[n] <- mean(bias)
  se_mean[n] <- mean(se)
}

bias_mean
se_mean

## -----------------------------------------------------------------------------
# Theoretical Results
lambda/(sample_size-1)
lambda*sample_size/(sample_size-1)/sqrt(sample_size-2)

## -----------------------------------------------------------------------------
library(boot)
# apply law dataset
library(bootstrap)
b.cor <- function(x, i) cor(x[i,1], x[i,2])
# return both Cor and Var
b.cor_var <- function(x, i){
  sim <- boot(x[i,], b.cor, R = 100)
  c(sim$t0, var(sim$t)*(n-1)/n^2)
}
b <- boot(law, statistic = b.cor_var, R = 100)
boot.ci(b, type = "stud")

# Unstable
b <- boot(law, statistic = b.cor_var, R = 100)
boot.ci(b, type = "stud")
b <- boot(law, statistic = b.cor_var, R = 100)
boot.ci(b, type = "stud")


## -----------------------------------------------------------------------------
M <- m <- 1000
alpha <- 0.1
FWER1 <- FDR1 <- TPR1 <- numeric(M)
FWER2 <- FDR2 <- TPR2 <- numeric(M)
# M Simulations
for(i in 1:M){
  p_value0 <- runif(0.95*m)
  p_value1 <- rbeta(0.05*m, 0.1, 1)
  
  # Bonferroni adjustment
  adj1 <- p.adjust(c(p_value0, p_value1), method = "bonferroni")
  # B-H
  adj2 <- p.adjust(c(p_value0, p_value1), method = "BH")
  
  result1 <- adj1 > 0.1
  result2 <- adj2 > 0.1
  true_result <- c(rep(TRUE, times = 0.95*m), rep(FALSE, times = 0.05*m))
  
  # All metrics
  FWER1[i] <- sum(result1[0:950]==FALSE)/950
  FWER2[i] <- sum(result2[0:950]==FALSE)/950
  FDR1[i] <- sum(result1[0:950]==FALSE)/sum(result1==FALSE)
  FDR2[i] <- sum(result2[0:950]==FALSE)/sum(result2==FALSE)
  TPR1[i] <- sum(result1[951:1000]==FALSE)/0.05/m
  TPR2[i] <- sum(result2[951:1000]==FALSE)/0.05/m
}

data.frame(FWER = c(sum(FWER1), sum(FWER2)), FDR = c(mean(FDR1),mean(FDR2)), TPR = c(mean(TPR1),mean(TPR2)))


## -----------------------------------------------------------------------------
library(boot)
library(MASS)

lambda <- 2
sample_size <- c(5,10,20)
B <- 1000
m <- 1000

bias_mean <- numeric(length(sample_size))
se_mean <- numeric(length(sample_size))

# Replicate process
for(n in 1:length(sample_size)){
  bias <- se <- numeric(m)
  for(i in 1:m){
    x <- rexp(sample_size[n], rate = lambda)
    theta <- 1/mean(x)
    thetastar <- numeric(B)
    for(b in 1:B){
      xstar <- sample(x, replace = TRUE)
      thetastar[b] <- 1/mean(xstar)
    }
    bias[i] <- mean(thetastar)-theta
    se[i] <- sd(thetastar)
  }
  bias_mean[n] <- mean(bias)
  se_mean[n] <- mean(se)
}

bias_mean
se_mean

## -----------------------------------------------------------------------------
# Theoretical Results
lambda/(sample_size-1)
lambda*sample_size/(sample_size-1)/sqrt(sample_size-2)

## -----------------------------------------------------------------------------
library(boot)
# apply law dataset
library(bootstrap)
b.cor <- function(x, i) cor(x[i,1], x[i,2])
# return both Cor and Var
b.cor_var <- function(x, i){
  sim <- boot(x[i,], b.cor, R = 100)
  c(sim$t0, var(sim$t)*(n-1)/n^2)
}
b <- boot(law, statistic = b.cor_var, R = 100)
boot.ci(b, type = "stud")

# Unstable
b <- boot(law, statistic = b.cor_var, R = 100)
boot.ci(b, type = "stud")
b <- boot(law, statistic = b.cor_var, R = 100)
boot.ci(b, type = "stud")


## -----------------------------------------------------------------------------
attach(chickwts)
# Cramer-von Mises
Cramer.test <- function(x, y, R=999){
  m <- length(x)
  n <- length(y)
  z <- c(x,y)
  N <- m + n 
  Fm <- numeric(N)
  Gn <- numeric(N)
  for(i in 1:N){
    Fm[i] <- mean(as.integer(z[i] <= x))
    Gn[i] <- mean(as.integer(z[i] <= y))
  }
  Cramer0 <- ((m*n)/N) * sum((Fm-Gn)^2)
  Cramer <- replicate(R, expr = {
    k <- sample(1:N)
    Z <- z[k]
    X <- Z[1:n]
    Y <- Z[(n+1):N]
    for(i in 1:N){
      Fm[i] <- mean(as.integer(Z[i] <= X))
      Gn[i] <- mean(as.integer(Z[i] <= Y))
    }
    ((m*n)/N) * sum((Fm - Gn)^2)
  })
  Cramer1 <- c(Cramer, Cramer0)
  return(list(statistic = Cramer0, p.value = mean(Cramer1>=Cramer0)))
}
# Apply test
soybean <- as.vector(weight[feed == "soybean"])
sunflower <- as.vector(weight[feed == "sunflower"])
linseed <- as.vector(weight[feed == "linseed"])
Cramer.test(soybean, linseed)
Cramer.test(sunflower, linseed)

## -----------------------------------------------------------------------------
# Max Outliers
maxol <- function(x,y){
  X <- x - mean(x)
  Y <- y - mean(y)
  xout <- sum(X > max(Y)) + sum(X < min(Y))
  yout <- sum(Y > max(X)) + sum(Y < min(X))
  return(max(c(xout,yout)))
}

maxol.test <- function(x, y, R = 999){
  z <- c(x, y)
  n <- length(x)
  N <- length(z)
  maxout <- maxol(x,y)
  maxouts <- replicate(R, expr={
    k <- sample(1:N)
    k1 <- k[1:n]
    k2 <- k[(n+1):N]
    maxol(z[k1],z)
  })
  maxouts1 <- c(maxouts, maxout)
  return(list(estimate = maxout, p = mean(maxouts1 >= maxout)))
}

# Apply Test
## equal variance
x <- rnorm(10, 0, 1)
y <- rnorm(50, 0, 1)
maxol.test(x,y)
## unequal variance
x <- rnorm(10, 0, 1)
y <- rnorm(50, 0, 2)
maxol.test(x,y)

## -----------------------------------------------------------------------------
sol_alpha <- function(N, b1, b2, b3, f0){
  x1 <- rpois(N,1)
  x2 <- rexp(N)
  x3 <- sample(0:1, N, replace = TRUE)
  g <- function(alpha){
    tmp <- exp(-alpha-b1*x1-b2*x2-b3*x3);
    p <- 1/(1+tmp)
    mean(p)-f0
  }
  solution <- uniroot(g, c(-50,0))
  return(solution$root)
}

N <- 106
b1 <- 0
b2 <- 1
b3 <- -1
f0 <- c(0.1, 0.01, 0.001, 0.0001)
alpha <- numeric(length(f0))
for(i in 1:length(f0)){
  alpha[i] <- sol_alpha(N, b1, b2, b3, f0[i])
}

plot(-log10(f0),alpha, type = 'l')


## -----------------------------------------------------------------------------
# random walk Metropolis Sampler
Metropolis_RW <- function(N, x_init, sigma){
  x <- numeric(N)
  x[1] <- x_init
  # rej
  u <- runif(N)
  cnt <- 0
  for(i in 2:N){
    xt <- x[i-1]
    y <- rnorm(1, xt, sigma)
    if(exp(abs(xt)-abs(y))>=u[i]) x[i] <- y
    else{
      x[i] <- x[i-1]
      cnt <- cnt + 1
    }
  }
   return(list(x = x, cnt = cnt))
}

N <- 10000
sigma <- c(1, 2, 4, 8)
x0 <- 0
for(i in 1:length(sigma)){
  RW <- Metropolis_RW(N, x0, sigma[i])
  # rejection rate
  res <- list(sigma = sigma[i], count = RW$cnt, acc_rate = 1-RW$cnt/N)
  print(unlist(res))
}

## -----------------------------------------------------------------------------
N <- 10000
burn_in <- 1000
# Bivariate Chain
X <- matrix(0, nrow = N, ncol = 2)
# init
X[1,] <- c(0,0)
# Params
mu1 <- mu2 <- 0
corr <- 0.9
sigma1 <- sigma2 <- 1
for(i in 2:N){
  yt <- X[i-1,2]
  mu <- mu1 + corr*(yt-mu2)*sigma1/sigma2
  X[i,1] <- rnorm(1, mean = mu, sd = sqrt(1-corr^2)*sigma1)
  xt <- X[i,1]
  mu <- mu2 + corr*(xt-mu1)*sigma2/sigma1
  X[i,2] <- rnorm(1, mean = mu, sd = sqrt(1-corr^2)*sigma2)
}
X_burned <- X[(burn_in+1):N,]
Xt <- X_burned[,1]
Yt <- X_burned[,2]
L <- lm(Yt~Xt)
# Check all required properties
L
summary(L)
# plot all samples
plot(Xt, Yt, cex = 0.025)
abline(h=0, v=0)


## -----------------------------------------------------------------------------
library(coda)
# Rayleigh
Rayle <- function(x, sigma){
  if(any(x<0)) return(0)
  stopifnot(sigma>0)
  return((x/sigma^2)*exp(-x^2/(2*sigma^2)))
}
# M-H Chain
MH <- function(N, sigma, x_init){
  x <- numeric(N)
  x[1] <- x_init
  u <- runif(N)
  for(i in 2:N){
    xt <- x[i-1]
    y <- rchisq(1, df = xt)
    part1 <- Rayle(y, sigma) * dchisq(xt, df = y)
    part2 <- Rayle(xt, sigma) * dchisq(y, df = xt)
    if(part1/part2 >= u[i]) x[i] <- y
    else x[i] <- xt
  }
  return(x)
}

sigma <- 4
x_init <- rchisq(4, df=1)
N <- 1000
X <- matrix(0, nrow = length(x_init), ncol = N)
for(i in 1:length(x_init)) X[i,]<- MH(N, sigma, x_init[i])
Y <- mcmc.list(as.mcmc(X[1,]), as.mcmc(X[2,]), as.mcmc(X[3,]), as.mcmc(X[4,]))
print(gelman.diag(Y))

gelman.plot(Y)



## -----------------------------------------------------------------------------
library(stats4)

# All intervals
interval <- matrix(c(11, 12, 8, 9, 27, 28, 13, 14, 16, 17, 0, 1, 23, 24, 10, 11, 24, 25, 2, 3), ncol = 2, byrow = TRUE)

# E-step
E_step <- function(lambda, interval) {
  sapply(1:nrow(interval), function(i) {
    ui <- interval[i, 1]
    vi <- interval[i, 2]
    (integrate(function(x) x * lambda * exp(-lambda * x), lower = ui, upper = vi)$value) / 
    (integrate(function(x) lambda * exp(-lambda * x), lower = ui, upper = vi)$value)
  })
}

# M-step
M_step <- function(lambda, interval) {
  expect <- E_step(lambda, interval)
  n <- nrow(interval)
  lambda_upd <- n / sum(expect)
  return(lambda_upd)
}

# EM
EMproc <- function(interval, tolerance = 1e-6, max_iter = 1000) {
  lambda <- 10
  for (i in 1:max_iter) {
    lambda_upd <- M_step(lambda, interval)
    if (abs(lambda_upd - lambda) < tolerance) {
      break
    }
    lambda <- lambda_upd
  }
  return(lambda)
}

# negloglike
negloglike <- function(lambda, interval) {
  likelihood <- sapply(1:nrow(interval), function(i) {
    ui <- interval[i, 1]
    vi <- interval[i, 2]
    p <- integrate(function(x) lambda * exp(-lambda * x), lower = ui, upper = vi, rel.tol = 1e-8, abs.tol = 1e-10)$value
    max(p, .Machine$double.eps)
  })
  -sum(log(likelihood))
}
# maximize with "optim"
optimlambda <- function(interval) {
  result <- optim(par = 1, fn = negloglike, interval = interval, method = "L-BFGS-B", lower = 1e-2, upper = 1)
  return(result$par)
}

# EM Apply
lambda <- EMproc(interval)
print(lambda)

# Negloglike Apply
lambda <- optimlambda(interval)
print(lambda)



## -----------------------------------------------------------------------------
solve.game <- function(A) {
#solve the two player zero-sum game by simplex method
#optimize for player 1, then player 2
#maximize v subject to ...
#let x strategies 1:m, and put v as extra variable
#A1, the <= constraints
#
min.A <- min(A)
A <- A - min.A #so that v >= 0
max.A <- max(A)
A <- A / max(A)
m <- nrow(A)
n <- ncol(A)
it <- n^3
a <- c(rep(0, m), 1) #objective function
A1 <- -cbind(t(A), rep(-1, n)) #constraints <=
b1 <- rep(0, n)
A3 <- t(as.matrix(c(rep(1, m), 0))) #constraints sum(x)=1
b3 <- 1
sx <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3,
              maxi=TRUE, n.iter=it)
#the ’solution’ is [x1,x2,...,xm | value of game]
#
#minimize v subject to ...
#let y strategies 1:n, with v as extra variable
a <- c(rep(0, n), 1) #objective function
A1 <- cbind(A, rep(-1, m)) #constraints <=
b1 <- rep(0, m)
A3 <- t(as.matrix(c(rep(1, n), 0))) #constraints sum(y)=1
b3 <- 1
sy <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3,
maxi=FALSE, n.iter=it)
soln <- list("A" = A * max.A + min.A,
"x" = sx$soln[1:m],
"y" = sy$soln[1:n],
"v" = sx$soln[m+1] * max.A + min.A)
soln
}

# payoff matrix A
A <- matrix(c(0, -2, -2, 3, 0, 0, 4, 0, 0, 2, 0, 0, 0, -3, -3, 4, 0, 0, 2, 0, 0, 3, 0, 0, 0, -4, -4, -3, 0, -3, 0, 4, 0, 0, 5, 0, 0, 3, 0, -4, 0, -4, 0, 5, 0, 0, 3, 0, 0, 4, 0, -5, 0, -5, -4, -4, 0, 0, 0, 5, 0, 0, 6, 0, 0, 4, -5, -5, 0, 0, 0, 6, 0, 0, 4, 0, 0, 5, -6, -6, 0), nrow = 9, ncol = 9)

library(boot)

B <- A + 2
sol <- solve.game(B)
sol$v
round(cbind(sol$x, sol$y), 7)
round(sol$x * 61, 7)


## -----------------------------------------------------------------------------
mylist <- list(1, 2, 3)
my_unlist <- unlist(mylist)
my_asvector <- as.vector(mylist)
print(my_unlist)
print(my_asvector)

## -----------------------------------------------------------------------------
my_vector <- c(1,2,3)
dim(my_vector)

## -----------------------------------------------------------------------------
my_matrix <- matrix(1:4, nrow = 2, ncol = 2)
is.matrix(my_matrix)
is.array(my_matrix)

## -----------------------------------------------------------------------------
df <- data.frame(
  num = c(1, 2, 3),
  char = c("a", "b", "c"),
  bool = c(TRUE, FALSE, TRUE)
)
my_matrix <- as.matrix(df)
print(my_matrix)

## -----------------------------------------------------------------------------
df <- data.frame()
print(df)
nrow(df)
ncol(df)

## -----------------------------------------------------------------------------
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
# data frame
df <- data.frame(
  A = c(1, 2, 3),
  B = c(1, 4, 9),
  C = c(1, 10, 100)
)
# Apply scale01() to every column
scaled_df <- as.data.frame(lapply(df, scale01))
print(scaled_df)


## -----------------------------------------------------------------------------
df <- data.frame(
  A = c(1, 2, 3),
  B = c(1, 4, 9),
  C = c(1, 10, 100)
)
print(df)
# Compute the standard deviation
std_dev <- vapply(df, sd, numeric(1))
print(std_dev)

mix_df <- data.frame(
  A = c(1, 2, 3),
  B = c("A", "B", "C"),
  C = c(TRUE, FALSE, FALSE),
  D = c(1, 4, 9),
  E = c(1, 10, 100)
)
print(mix_df)
mix_numeric <- vapply(mix_df,function(x) all(is.numeric(x)),logical(1))
# Compute the standard deviation
std_dev <- vapply(mix_df[, mix_numeric], sd, numeric(1))
print(std_dev)


## -----------------------------------------------------------------------------
GibbsSamp <- function(n, a, b, iter) {
  # Init
  chain <- matrix(0, nrow = iter, ncol = 2)
  x <- 0
  y <- 0.5
  
  for (i in 1:iter) {
    # Update x from conditional B(n, y)
    x <- rbinom(1, n, y)
    
    # Update y from conditional Beta(x + a, n - x + b)
    y <- rbeta(1, x + a, n - x + b)
    
    chain[i, 1] <- x
    chain[i, 2] <- y
  }
  return(chain)
}

## -----------------------------------------------------------------------------
library(microbenchmark)

n <- 100
a <- 2
b <- 3
iter <- 1000

# R function
r_time <- microbenchmark(
  GibbsSamp(n, a, b, iter),
  times = 10
)

# Rcpp function
cpp_time <- microbenchmark(
  GibbsSampCpp(n, a, b, iter),
  times = 10
)

# Print the computation time of both functions
print(r_time)
print(cpp_time)

