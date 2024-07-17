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

X <- as.data.frame(read_excel("2021-2023.xlsx", sheet = "X", range = "L1:N154"))
va <- as.data.frame(read_excel("2021-2023.xlsx", sheet = "va", range = "I1:K154"))
fu <- as.data.frame(read_excel("2021-2023.xlsx", sheet = "fu", range = "L1:N154"))
im <- as.data.frame(read_excel("2021-2023.xlsx", sheet = "im", range = "K1:M154"))

price_index <- as.matrix(read_excel("2021-2023.xlsx",sheet = "Price", range = "C3:E156"))

#-----------------------------------------------------------------------------#
# 1 fu 一致性调整  ----
#-----------------------------------------------------------------------------#

fu_adj <- matrix(NA, nrow=153, ncol=3)
fu_total <- c(1318442.3,1383071.1,1438489.7)

for (i in 1:3) {
  
  initial_fu <- as.vector(as.matrix(fu[,i]))
  
  objective_history <- numeric()
  constraint_history <- numeric()
  iteration <- 0
  
  # Initial values
  x0 <- initial_fu
  lower_bounds <- x0 - abs(x0) * 0.2
  upper_bounds <- x0 + abs(x0) * 0.2
  
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
    constraint_value <- sum(fu_adj) - fu_total[i]
    
    # Log the constraint function value
    constraint_history <<- c(constraint_history, constraint_value)
    
    # Print the current iteration and constraint function value
    cat(sprintf("Iteration: %d, Constraint Value: %f\n", iteration, constraint_value))
    
    return(constraint_value)
  }
  
  opts <- list("algorithm" = "NLOPT_LN_COBYLA", 
               "xtol_rel" = 1e-8, 
               "maxeval" = 100000)
  
  result <- nloptr(x0 = x0, 
                   eval_f = objective_function, 
                   lb = lower_bounds, 
                   ub = upper_bounds, 
                   eval_g_eq = constraint_function, 
                   opts = opts)
  
  fu_adj[,i] <- result$solution
}

colnames(fu_adj) <- c("2021","2022","2023")

#-----------------------------------------------------------------------------#
# 2 im 一致性调整  ----
#-----------------------------------------------------------------------------#

im_adj <- matrix(NA, nrow=153, ncol=3)
im_total <- c(173159.4,180600.1,179842.4)
weights <- c(rep(1.1, 108), rep(1, 153 - 108))

for (i in 1:3) {
  
  initial_im <- as.vector(as.matrix(im[,i]))
  
  objective_history <- numeric()
  constraint_history <- numeric()
  iteration <- 0
  
  # Initial values
  x0 <- initial_im
  lower_bounds <- x0 - abs(x0) * 0.2
  upper_bounds <- x0 + abs(x0) * 0.2
  
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
    constraint_value <- sum(im_adj) - im_total[i]
    
    # Log the constraint function value
    constraint_history <<- c(constraint_history, constraint_value)
    
    # Print the current iteration and constraint function value
    cat(sprintf("Iteration: %d, Constraint Value: %f\n", iteration, constraint_value))
    
    return(constraint_value)
  }
  
  opts <- list("algorithm" = "NLOPT_LN_COBYLA", 
               "xtol_rel" = 1e-8, 
               "maxeval" = 100000)
  
  result <- nloptr(x0 = x0, 
                   eval_f = objective_function, 
                   lb = lower_bounds, 
                   ub = upper_bounds, 
                   eval_g_eq = constraint_function, 
                   opts = opts)
  
  im_adj[,i] <- result$solution
}

colnames(im_adj) <- c("2021","2022","2023")


#-----------------------------------------------------------------------------#
# 3 A matrix  ----
#-----------------------------------------------------------------------------#

IO_20 <- read_excel("IO表 02-20.xlsx", sheet = "2020", range = "C6:FM166")
IO_20 <- as.data.frame(IO_20)
rownames(IO_20) <- IO_20[[1]]
IO_20 <- IO_20[, -1]

X_20 <- IO_20[1:153,"GO"]

A_0 <- matrix(NA, nrow=153, ncol=153)
for (i in 1:153){
  for (j in 1:153){
    A_0[i,j] <- IO_20[i,j] / X_20[j]
  }
}

for (year in 2021:2023){
  price_index_year <- price_index[,as.character(year)]
  assign(paste0("A_0_", year), diag(as.numeric(price_index_year)/100) %*% A_0)
}

for (year in 2021:2023){
  X_year <- X[,as.character(year)]
  A_0_year <- get(paste0("A_0_",year))
  assign(paste0("Z_0_", year), as.matrix(A_0_year %*% diag(as.vector(as.matrix(X_year)))))
}

# ----------------------------------- #
## 3a A_focus ----
# ----------------------------------- #

II_coefficient <- as.matrix(IO_20[1:153,1:153]) %*% solve(diag(IO_20[154,1:153]))

IU_coefficient <- matrix(NA, nrow=153, ncol=153)
for (i in 1:153){
  for (j in 1:153){
    IU_coefficient[i,j] <- IO_20[i,j] / IO_20[i,154]
  }
}

II_indices <- which(II_coefficient >= 0.7, arr.ind = TRUE)
IU_indices <- which(IU_coefficient >= 0.7, arr.ind = TRUE)

for (year in 2021:2023){
  
  A_focus <- matrix(0, nrow=153, ncol=153)
  A_0_year <- get(paste0("A_0_",year))
  X_year <- X[,as.character(year)]
  
  for (idx in 1:nrow(II_indices)) {
    i <- II_indices[idx, 1]
    j <- II_indices[idx, 2]
    A_focus[i, j] <- A_0_year[i, j]
  }
  
  for (idx in 1:nrow(IU_indices)) {
    i <- IU_indices[idx, 1]
    j <- IU_indices[idx, 2]
    A_focus[i, j] <- A_0_year[i, j]
  }
  
  assign(paste0("A_focus_", year), A_focus)
  assign(paste0("Z_1_", year), as.matrix(A_focus %*% diag(as.vector(as.matrix(X_year)))))
  
}

# ----------------------------------- #
## 3b A_unfocus ----
# ----------------------------------- #

for (year in 2021:2023){
  
  A_unfocus <- get(paste0("A_0_",year))
  X_year <- X[,as.character(year)]
  
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
  
  assign(paste0("A_unfocus_", year), A_unfocus)
  assign(paste0("Z_20_", year), as.matrix(A_unfocus %*% diag(as.vector(as.matrix(X_year)))))
  
}

GRAS <- function(A, r_bar, s_bar, iteration_maximum = 5000, accuracy = 1e-6, epsilon = 1e-8) {
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
      # Ensure the updated values are within ±20% of the original values
      lower_bound <- A[i, j] - abs(A[i, j]) * 0.2
      upper_bound <- A[i, j] + abs(A[i, j]) * 0.2
      X[i, j] <- max(min(X[i, j], upper_bound), lower_bound)
    }
  }
  
  return(X)
}

for (year in 2021:2023){
  
  X_year <- X[,as.character(year)]
  fu_year <- fu_adj[,as.character(year)]
  im_year <- im_adj[,as.character(year)]
  va_year <- va[,as.character(year)]
  A_focus_year <- get(paste0("A_focus_",year))
  Z_20_year <- get(paste0("Z_20_",year))
  Z_1_year <- get(paste0("Z_1_",year))
  
  r <- X_year - fu_year + im_year
  s <- X_year - va_year
  i_col <- as.matrix(rep(1,153), ncol = 1, nrow = 153)
  r_bar <- as.matrix(r - A_focus_year %*% diag(as.vector(as.matrix(X_year))) %*% i_col)
  s_bar <- as.matrix(s - t(i_col) %*% A_focus_year %*% diag(as.vector(as.matrix(X_year))))
  
  Z_2_year <- GRAS(A = Z_20_year, r_bar, s_bar)
  assign(paste0("Z_2_", year), Z_2_year)
  
  Z_year <- Z_1_year + Z_2_year
  assign(paste0("Z_", year), Z_year)
  
  A_year <- Z_year %*% solve(diag(as.vector(as.matrix(X_year))))
  assign(paste0("A_", year), A_year)
  I_diag <- diag(1,nrow=153)
  assign(paste0("L_", year), solve(I_diag - A_year))

}
