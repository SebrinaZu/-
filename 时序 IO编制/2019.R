
library(dplyr)
library(readxl)
library(foreign)
library(sf)
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)
library(rmapshaper)
library(magic)
library(Matrix)
library(MASS)
library(data.table)
library(nloptr)
library(readxl)

X <- as.data.frame(read_excel("2019.xlsx", sheet = "x1", range = "P1:P154"))
va <- as.data.frame(read_excel("2019.xlsx", sheet = "v1", range = "G1:G154"))
tu <- as.data.frame(read_excel("2019.xlsx", sheet = "u1", range = "F2:G155"))

price_index <- as.matrix(read_excel("2019.xlsx",sheet = "A", range = "F3:G156"))
rownames(price_index) <- price_index[,1]
price_index <- price_index[, -1]

#-----------------------------------------------------------------------------#
# 1 usage 一致性调整  ----
#-----------------------------------------------------------------------------#

initial_fu <- as.vector(as.matrix(tu[,1]))
initial_im <- as.vector(as.matrix(tu[,2]))

# ----------------------------------- #
## 1a fu ----
# ----------------------------------- #

objective_history <- numeric()
constraint_history <- numeric()
iteration <- 0

# Initial values
x0 <- initial_fu
lower_bounds <- x0 - abs(x0) * 0.3
upper_bounds <- x0 + abs(x0) * 0.3

objective_function <- function(x) {
  fu_adj <- x
  obj_value <- sum(abs(fu_adj - initial_fu))
  
  # Log the objective function value
  objective_history <<- c(objective_history, obj_value)
  
  # Print the current iteration and objective function value
  iteration <<- iteration + 1
  cat(sprintf("Iteration: %d, Objective Value: %f\n", iteration, obj_value))
  
  return(obj_value)
}

constraint_function <- function(x) {
  fu_adj <- x
  constraint_value <- sum(fu_adj) - 1133962.1
  
  # Log the constraint function value
  constraint_history <<- c(constraint_history, constraint_value)
  
  # Print the current iteration and constraint function value
  cat(sprintf("Iteration: %d, Constraint Value: %f\n", iteration, constraint_value))
  
  return(constraint_value)
}

opts <- list("algorithm" = "NLOPT_LN_COBYLA", 
             "xtol_rel" = 1e-12, 
             "maxeval" = 20000)

result <- nloptr(x0 = x0, 
                 eval_f = objective_function, 
                 lb = lower_bounds, 
                 ub = upper_bounds, 
                 eval_g_eq = constraint_function, 
                 opts = opts)

fu_adj <- result$solution

# ----------------------------------- #
## 1b im ----
# ----------------------------------- #

objective_history <- numeric()
constraint_history <- numeric()
iteration <- 0

# Initial values
x0 <- initial_im 
lower_bounds <- x0 - abs(x0) * 0.3
upper_bounds <- x0 + abs(x0) * 0.3

weights <- c(rep(1.1, 108), rep(1, length(initial_im) - 108))

objective_function <- function(x) {
  im_adj <- x
  obj_value <- sum(weights * abs(im_adj - initial_im))
  
  # Log the objective function value
  objective_history <<- c(objective_history, obj_value)
  
  # Print the current iteration and objective function value
  iteration <<- iteration + 1
  cat(sprintf("Iteration: %d, Objective Value: %f\n", iteration, obj_value))
  
  return(obj_value)
}

constraint_function <- function(x) {
  im_adj <- x
  constraint_value <- sum(im_adj) - 143253.7
  
  # Log the constraint function value
  constraint_history <<- c(constraint_history, constraint_value)
  
  # Print the current iteration and constraint function value
  cat(sprintf("Iteration: %d, Constraint Value: %f\n", iteration, constraint_value))
  
  return(constraint_value)
}

opts <- list("algorithm" = "NLOPT_LN_COBYLA", 
             "xtol_rel" = 1e-12, 
             "maxeval" = 20000)

result <- nloptr(x0 = x0, 
                 eval_f = objective_function, 
                 lb = lower_bounds, 
                 ub = upper_bounds, 
                 eval_g_eq = constraint_function, 
                 opts = opts)

im_adj <- result$solution


#-----------------------------------------------------------------------------#
# 2 A matrix  ----
#-----------------------------------------------------------------------------#

IO_18 <- read_excel("IO表 02-20.xlsx", sheet = "2018", range = "C6:FM166")
IO_18 <- as.data.frame(IO_18)
rownames(IO_18) <- IO_18[[1]]
IO_18 <- IO_18[, -1]

IO_20 <- read_excel("IO表 02-20.xlsx", sheet = "2020", range = "C6:FM166")
IO_20 <- as.data.frame(IO_20)
rownames(IO_20) <- IO_20[[1]]
IO_20 <- IO_20[, -1]

X_18 <- IO_18[1:153,"GO"]

A_0 <- matrix(NA, nrow=153, ncol=153)
for (i in 1:153){
  for (j in 1:153){
    A_0[i,j] <- IO_18[i,j] / X_18[j]
  }
}

A_0_adjusted <- diag(as.numeric(price_index)/100) %*% A_0

Z_0 <- as.matrix(A_0_adjusted %*% diag(as.vector(as.matrix(X))))

# ----------------------------------- #
## 2a A_focus ----
# ----------------------------------- #

II_coefficient <- as.matrix(IO_18[1:153,1:153]) %*% solve(diag(IO_18[154,1:153]))

IU_coefficient <- matrix(NA, nrow=153, ncol=153)
for (i in 1:153){
  for (j in 1:153){
    IU_coefficient[i,j] <- IO_18[i,j] / IO_18[i,154]
  }
}

II_indices <- which(II_coefficient >= 0.7, arr.ind = TRUE)
IU_indices <- which(IU_coefficient >= 0.7, arr.ind = TRUE)

A_focus <- matrix(0, nrow=153, ncol=153)

for (idx in 1:nrow(II_indices)) {
  i <- II_indices[idx, 1]
  j <- II_indices[idx, 2]
  A_focus[i, j] <- A_0_adjusted[i, j]
}

for (idx in 1:nrow(IU_indices)) {
  i <- IU_indices[idx, 1]
  j <- IU_indices[idx, 2]
  A_focus[i, j] <- A_0_adjusted[i, j]
}

Z_1 <- as.matrix(A_focus %*% diag(as.vector(as.matrix(X))))

# ----------------------------------- #
## 2b A_unfocus ----
# ----------------------------------- #

A_unfocus <- A_0_adjusted

for (idx in 1:nrow(II_indices)) {
  i <- II_indices[idx, 1]
  j <- II_indices[idx, 2]
  A_unfocus[i, j] <- 0
}

for (idx in 1:nrow(IU_indices)) {
  i <- IU_indices[idx, 1]
  j <- IU_indices[idx, 2]
  A_unfocus[i, j] <- 0
}

Z_20 <- as.matrix(A_unfocus %*% diag(as.vector(as.matrix(X))))

r <- X - fu_adj + im_adj
s <- X - va
i_col <- as.matrix(rep(1,153), ncol = 1, nrow = 153)
r_bar <- as.matrix(r - A_focus %*% diag(as.vector(as.matrix(X))) %*% i_col)
s_bar <- as.matrix(s - t(i_col) %*% A_focus %*% diag(as.vector(as.matrix(X))))

GRAS <- function(A, r_bar, s_bar, iteration_maximum = 3000, accuracy = 1e-6, epsilon = 1e-8) {
  # Define the P and N matrices
  P <- ifelse(A > 0, A, 0)
  N <- ifelse(A < 0, -A, 0)
  
  # Initialize R and S as diagonal matrices
  m <- nrow(A)
  n <- ncol(A)
  R <- diag(1, m, m)
  S <- diag(1, n, n)
  
  # Define functions for updating r_i and s_j
  rho_i <- function(s, r_bar_i, p_i, n_i) {
    (r_bar_i + sqrt(r_bar_i^2 + 4 * p_i * n_i)) / (2 * p_i)
  }
 
  sigma_j <- function(r, s_bar_j, p_j, n_j) {
    (s_bar_j + sqrt(s_bar_j^2 + 4 * p_j * n_j)) / (2 * p_j)
  }
  
  # Iteration
  for (iteration in 1:iteration_maximum) {
    # Update S
    for (j in 1:n) {
      p_j <- sum(P[, j] * diag(R))
      n_j <- sum(N[, j] / diag(R))
      S[j, j] <- sigma_j(diag(R), s_bar[j], p_j, n_j)
      if (is.na(S[j, j]) || is.infinite(S[j, j]) || abs(S[j, j]) < epsilon) {
        S[j, j] <- epsilon
      }
    }
    
    # Update R
    for (i in 1:m) {
      p_i <- sum(P[i, ] * diag(S))
      n_i <- sum(N[i, ] / diag(S))
      R[i, i] <- rho_i(diag(S), r_bar[i], p_i, n_i)
      if (is.na(R[i, i]) || is.infinite(R[i, i]) || abs(R[i, i]) < epsilon) {
        R[i, i] <- epsilon
      }
    }
    
    # Check for convergence
    RS <- R %*% P %*% S - ginv(R + epsilon * diag(m)) %*% N %*% ginv(S + epsilon * diag(n))
    error_r <- norm(RS %*% rep(1, n) - matrix(r_bar, nrow = m, ncol = 1), "F") / norm(r_bar, "F")
    error_s <- norm(t(rep(1, m)) %*% RS - matrix(s_bar, nrow = 1, ncol = n), "F") / norm(s_bar, "F")
    if (error_r < accuracy && error_s < accuracy) {
      break
    }
    print(iteration)
  }
  
  # Calculate the updated matrix X with constraints
  X <- matrix(0, nrow = m, ncol = n)
  r <- diag(R)
  s <- diag(S)
  
  for (i in 1:m) {
    for (j in 1:n) {
      if (A[i, j] >= 0) {
        X[i, j] <- r[i] * A[i, j] * s[j]
      } else {
        X[i, j] <- (1 / r[i]) * A[i, j] * (1 / s[j])
      }
      # Ensure the updated values are within ±10% of the original values
      lower_bound <- A[i, j] - abs(A[i, j]) * 0.1
      upper_bound <- A[i, j] + abs(A[i, j]) * 0.1
      X[i, j] <- max(min(X[i, j], upper_bound), lower_bound)
    }
  }
  
  return(X)
}

Z_2 <- GRAS(A = Z_20, r_bar, s_bar)

#-----------------------------------------------------------------------------#
# 3 Z matrix  ----
#-----------------------------------------------------------------------------#

Z <- Z_1 + Z_2

fwrite(Z, "Z_0715.csv")

A_19 <- Z %*% solve(diag(as.vector(as.matrix(X))))
A_18 <- A_0
A_20 <- as.matrix(IO_20[1:153,1:153]) %*% solve(diag(as.vector(as.matrix(IO_20[1:153,"GO"]))))

I_diag <- diag(1,nrow=153)
L_19 <- solve(I_diag - A_19)
L_18 <- solve(I_diag - A_18)
L_20 <- solve(I_diag - A_20)
