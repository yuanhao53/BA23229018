## ----eval = FALSE-------------------------------------------------------------
#  train_inputs <- matrix(c(0, 0, 0, 1, 1, 0, 1, 1), ncol = 2, byrow = TRUE)
#  train_targets <- c(0, 1, 1, 0)
#  mlp_train(train_inputs, train_targets, hidden_size = 4, lr = 0.1, epochs = 1000, lambda = 0.01)

## ----eval = FALSE-------------------------------------------------------------
#  # Example usage of gradient_descent function
#  X <- matrix(runif(1000), ncol = 20)
#  y <- 2 * rbinom(50, 1, 0.5) - 1
#  lr <- 0.0001
#  num_iters <- 100
#  theta <- gradient_descent(X, y, lr, num_iters)

## ----eval = FALSE-------------------------------------------------------------
#  # Example usage of stochastic_gradient_descent function
#  X <- matrix(runif(1000), ncol = 20)
#  y <- 2 * rbinom(50, 1, 0.5) - 1
#  lr <- 0.0001
#  num_iters <- 100
#  theta <- stochastic_gradient_descent(X, y, lr, num_iters)
#  

## ----eval = FALSE-------------------------------------------------------------
#  # Example usage of minibatch_gradient_descent function
#  X <- matrix(runif(1000), ncol = 20)
#  y <- 2 * rbinom(50, 1, 0.5) - 1
#  lr <- 0.0001
#  batch_size <- 10
#  num_iters <- 100
#  theta <- minibatch_gradient_descent(X, y, lr, batch_size, num_iters)

## ----eval = FALSE-------------------------------------------------------------
#  # Example usage of sgd_with_momentum function
#  X <- matrix(runif(1000), ncol = 20)
#  y <- 2 * rbinom(50, 1, 0.5) - 1
#  lr <- 0.0001
#  beta <- 0.9
#  num_iters <- 100
#  theta <- sgd_with_momentum(X, y, lr, beta, num_iters)

## ----eval = FALSE-------------------------------------------------------------
#  # Example usage of adam_optimization function
#  X <- matrix(runif(1000), ncol = 20)
#  y <- 2 * rbinom(50, 1, 0.5) - 1
#  lr <- 0.0001
#  beta1 <- 0.9
#  beta2 <- 0.999
#  epsilon <- 1e-8
#  num_iters <- 100
#  theta <- adam_optimization(X, y, lr, beta1, beta2, epsilon, num_iters)

