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

X <- as.data.frame(read_excel("2019-2023.xlsx", sheet = "X", range = "B1:E154"))
va <- as.data.frame(read_excel("2019-2023.xlsx", sheet = "va", range = "B1:E154"))
fu <- as.data.frame(read_excel("2019-2023.xlsx", sheet = "fu", range = "B1:E154"))
im <- as.data.frame(read_excel("2019-2023.xlsx", sheet = "im", range = "B1:E154"))
price_index <- as.matrix(read_excel("2019-2023.xlsx",sheet = "Price", range = "B1:E154"))

IO_18 <- read_excel("IO表 02-20.xlsx", sheet = "2018", range = "C6:FM166")
IO_18 <- as.data.frame(IO_18)
rownames(IO_18) <- IO_18[[1]]
IO_18 <- IO_18[, -1]

IO_20 <- read_excel("IO表 02-20.xlsx", sheet = "2020", range = "C6:FM166")
IO_20 <- as.data.frame(IO_20)
rownames(IO_20) <- IO_20[[1]]
IO_20 <- IO_20[, -1]

#-----------------------------------------------------------------------------#
# 1 fu & im一致性调整  ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
## 1a part ----
# ----------------------------------- #

part_fu_all <- matrix(NA, nrow = 6, ncol = 4)
part_im_all <- matrix(NA, nrow = 6, ncol = 4)

# Sectors that should have X - fu + im = 0
sectors <- c(103:107, 146)

for (i in 1:4) {
  
  initial_fu <- as.vector(as.matrix(fu[sectors, i]))
  initial_im <- as.vector(as.matrix(im[sectors, i]))
  
  objective_history <- numeric()
  constraint_history <- numeric()
  iteration <- 0
  
  x0 <- c(initial_fu, initial_im)
  lower_bounds <- rep(0,12)
  upper_bounds <- c(initial_fu + 10*abs(initial_fu), initial_im + 10*abs(initial_im))
  
  objective_function <- function(x) {
    
    part_fu <- x[1:6]
    part_im <- x[7:12]
    obj_value <- sum(abs(part_fu - initial_fu)) + sum(abs(part_im - initial_im))
    
    objective_history <<- c(objective_history, obj_value)
    
    iteration <<- iteration + 1
    cat(sprintf("Iteration: %d, Objective Value: %f\n", iteration, obj_value))
    
    return(obj_value)
  }
  
  constraint_function_eq <- function(x) {
    
    part_fu <- x[1:6]
    part_im <- x[7:12]
    constraints <- numeric()
    
    # X - fu + im = 0 for specific sectors
    for (j in seq_along(sectors)) {
      sector_index <- sectors[j]
      constraints <- c(constraints, X[sector_index, i] - part_fu[j] + part_im[j])
    }
    
    return(constraints)
  }
  
  opts <- list("algorithm" = "NLOPT_LN_COBYLA", 
               "xtol_rel" = 1e-8, 
               "maxeval" = 10000)
  
  result <- nloptr(x0 = x0, 
                   eval_f = objective_function, 
                   lb = lower_bounds, 
                   ub = upper_bounds, 
                   eval_g_eq = constraint_function_eq, 
                   opts = opts)
  
  part_fu_all[, i] <- result$solution[1:6]
  part_im_all[, i] <- result$solution[7:12]
  
}

for (i in 1:4){
  fu[sectors, i] <- part_fu_all[, i]
  im[sectors, i] <- part_im_all[, i]
}

# ----------------------------------- #
## 1b full ----
# ----------------------------------- #

TU_18 <- IO_18[,"TIU"]
TU_20 <- IO_20[,"TIU"]

fu_adj_all <- matrix(NA, nrow = 147, ncol = 4)
im_adj_all <- matrix(NA, nrow = 147, ncol = 4)

fu_total <- c(1133962.1, 1318442.3, 1383071.1, 1438489.7)
im_total <- c(143253.7, 173159.4, 180600.1, 179842.4)

remaining_indices <- setdiff(1:153, sectors)
mapping <- data.frame(Sequential_Index = 1:length(remaining_indices), 
                      Original_Index = remaining_indices)

for (i in 1:4) {
  
  initial_fu <- as.vector(as.matrix(fu[-sectors, i]))
  initial_im <- as.vector(as.matrix(im[-sectors, i]))
  
  objective_history <- numeric()
  iteration <- 0
  
  x0 <- c(initial_fu, initial_im)
  lower_bounds <- c(
    ifelse(initial_fu >= 0, 0, 10 * initial_fu), 
    ifelse(initial_im >= 0, 0, 10 * initial_im)   
  )
  upper_bounds <- c(
    ifelse(initial_fu >= 0, 10 * initial_fu, 0),  
    ifelse(initial_im >= 0, 10 * initial_im, 0)  
  )
  
  objective_function <- function(x) {
    
    fu_adj <- x[1:147]
    im_adj <- x[148:294]
    obj_value <- sum(abs(fu_adj - initial_fu)) + sum(abs(im_adj - initial_im))
    
    objective_history <<- c(objective_history, obj_value)
    
    iteration <<- iteration + 1
    cat(sprintf("Iteration: %d, Objective Value: %f\n", iteration, obj_value))
    
    return(obj_value)
  }
  
  constraint_function_eq <- function(x) {
    
    fu_adj <- x[1:147]
    im_adj <- x[148:294]
    constraints <- c(sum(fu_adj) + sum(part_fu_all[,i]) - fu_total[i], 
                     sum(im_adj) + sum(part_im_all[,i]) - im_total[i])
    
    constraint_value_fu <- sum(fu_adj) + sum(part_fu_all[,i]) - fu_total[i]
    constraint_value_im <- sum(im_adj) + sum(part_im_all[,i]) - im_total[i]
    constraint_history <<- c(constraint_history, constraint_value_fu, constraint_value_im)
    
    # Print the current iteration and constraint function value
    cat(sprintf("Iteration: %d, Constraint Value fu: %f\n", iteration, constraint_value_fu))
    cat(sprintf("Iteration: %d, Constraint Value im: %f\n", iteration, constraint_value_im))
    
    return(constraints)
  }
  
  constraint_function_ineq <- function(x) {
    
    fu_adj <- x[1:147]
    im_adj <- x[148:294]
    constraints <- numeric()
    
    # X - fu + im >= 0.2 * TU & X - fu + im <= 5 * TU
    for (j in 1:147) {
      sector_index <- mapping$Original_Index[j]
      # constraints <- c(constraints, -(X[sector_index, i] - fu_adj[j] + im_adj[j]))
      
      if (i == 1){
        constraints <- c(constraints, -(X[sector_index, i] - fu_adj[j] + im_adj[j] - 0.2 * TU_18[sector_index]/10000))
        constraints <- c(constraints, X[sector_index, i] - fu_adj[j] + im_adj[j] - 5 * TU_18[sector_index]/10000)
      } else {
        constraints <- c(constraints, -(X[sector_index, i] - fu_adj[j] + im_adj[j] - 0.2 * TU_20[sector_index]/10000))
        constraints <- c(constraints, X[sector_index, i] - fu_adj[j] + im_adj[j] - 5 * TU_20[sector_index]/10000)
      }
    }
    
    return(constraints)
  }
  
  opts <- list("algorithm" = "NLOPT_LN_COBYLA", 
               "xtol_rel" = 1e-8, 
               "maxeval" = 10000000)
  
  result <- nloptr(x0 = x0, 
                   eval_f = objective_function, 
                   lb = lower_bounds, 
                   ub = upper_bounds, 
                   eval_g_ineq = constraint_function_ineq, 
                   eval_g_eq = constraint_function_eq, 
                   opts = opts)
  
  fu_adj_all[, i] <- result$solution[1:147]
  im_adj_all[, i] <- result$solution[148:294]
}

fu_adj <- matrix(NA, nrow = 153, ncol = 4)
im_adj <- matrix(NA, nrow = 153, ncol = 4)

k = 0
for (i in 1:153){
  if (i %in% sectors){
    for (j in 1:4){
      fu_adj[i,j] <- fu[i,j]
    }
    k <- k + 1
  } else {
    for (j in 1:4){
      fu_adj[i,j] <- fu_adj_all[i - k,j]
    }
  }
}

k = 0
for (i in 1:153){
  if (i %in% sectors){
    for (j in 1:4){
      im_adj[i,j] <- im[i,j]
    }
    k <- k + 1
  } else {
    for (j in 1:4){
      im_adj[i,j] <- im_adj_all[i - k,j]
    }
  }
}

colnames(fu_adj) <- c("2019", "2021", "2022", "2023")
colnames(im_adj) <- c("2019", "2021", "2022", "2023")

fwrite(fu_adj,"fu_adj.csv")
fwrite(im_adj,"im_adj.csv")

#-----------------------------------------------------------------------------#
# 2 Compile  ----
#-----------------------------------------------------------------------------#

fu_adj <- read.csv("fu_adj.csv")
im_adj <- read.csv("im_adj.csv")
colnames(fu_adj) <- colnames(im_adj) <- c(2019,2021:2023)

X_18 <- IO_18[1:153,"GO"]
X_20 <- IO_20[1:153,"GO"]

A_18 <- matrix(NA, nrow=153, ncol=153)
for (i in 1:153){
  for (j in 1:153){
    A_18[i,j] <- IO_18[i,j] / X_18[j]
  }
}

A_20 <- matrix(NA, nrow=153, ncol=153)
for (i in 1:153){
  for (j in 1:153){
    A_20[i,j] <- IO_20[i,j] / X_20[j]
  }
}

for (year in c(2019, 2021:2023)){
  price_index_year <- price_index[,as.character(year)]
  if (year == 2019){
    assign(paste0("A_0_", year), diag(as.numeric(price_index_year)/100) %*% A_18)
  } else {
    assign(paste0("A_0_", year), diag(as.numeric(price_index_year)/100) %*% A_20)
  }
}

for (year in c(2019, 2021:2023)){
  X_year <- X[,as.character(year)]
  A_0_year <- get(paste0("A_0_",year))
  assign(paste0("Z_0_", year), as.matrix(A_0_year %*% diag(as.vector(as.matrix(X_year)))))
}

# ----------------------------------- #
## 2a A_focus ----
# ----------------------------------- #

II_coefficient_18 <- as.matrix(IO_18[1:153,1:153]) %*% solve(diag(IO_18[154,1:153]))
II_coefficient_20 <- as.matrix(IO_20[1:153,1:153]) %*% solve(diag(IO_20[154,1:153]))

IU_coefficient_18 <- matrix(NA, nrow=153, ncol=153)
IU_coefficient_20 <- matrix(NA, nrow=153, ncol=153)
for (i in 1:153){
  for (j in 1:153){
    IU_coefficient_18[i,j] <- IO_18[i,j] / IO_18[i,154]
    IU_coefficient_20[i,j] <- IO_20[i,j] / IO_20[i,154]
  }
}

II_indices_18 <- which(II_coefficient_18 >= 0.7, arr.ind = TRUE)
IU_indices_18 <- which(IU_coefficient_18 >= 0.7, arr.ind = TRUE)
II_indices_20 <- which(II_coefficient_20 >= 0.7, arr.ind = TRUE)
IU_indices_20 <- which(IU_coefficient_20 >= 0.7, arr.ind = TRUE)

for (year in c(2019, 2021:2023)){
  
  A_focus <- matrix(0, nrow=153, ncol=153)
  A_0_year <- get(paste0("A_0_",year))
  X_year <- X[,as.character(year)]
  
  if (year == 2019){
    
    for (idx in 1:nrow(II_indices_18)) {
      i <- II_indices_18[idx, 1]
      j <- II_indices_18[idx, 2]
      A_focus[i, j] <- A_0_year[i, j]
    }
    
    for (idx in 1:nrow(IU_indices_18)) {
      i <- IU_indices_18[idx, 1]
      j <- IU_indices_18[idx, 2]
      A_focus[i, j] <- A_0_year[i, j]
    }
    
  } else {
    
    for (idx in 1:nrow(II_indices_20)) {
      i <- II_indices_20[idx, 1]
      j <- II_indices_20[idx, 2]
      A_focus[i, j] <- A_0_year[i, j]
    }
    
    for (idx in 1:nrow(IU_indices_20)) {
      i <- IU_indices_20[idx, 1]
      j <- IU_indices_20[idx, 2]
      A_focus[i, j] <- A_0_year[i, j]
    }
    
  }
  
  assign(paste0("A_focus_", year), A_focus)
  assign(paste0("Z_1_", year), as.matrix(A_focus %*% diag(as.vector(as.matrix(X_year)))))
  
}

# ----------------------------------- #
## 2b A_unfocus ----
# ----------------------------------- #

for (year in c(2019, 2021:2023)){
  
  A_unfocus <- get(paste0("A_0_",year))
  X_year <- X[,as.character(year)]
  
  if (year == 2019){
    
    for (idx in 1:nrow(II_indices_18)) {
      i <- II_indices_18[idx, 1]
      j <- II_indices_18[idx, 2]
      A_unfocus[i, j] <- 0
    }
    
    for (idx in 1:nrow(IU_indices_18)) {
      i <- IU_indices_18[idx, 1]
      j <- IU_indices_18[idx, 2]
      A_unfocus[i, j] <- 0
    }
    
  } else {
    
    for (idx in 1:nrow(II_indices_20)) {
      i <- II_indices_20[idx, 1]
      j <- II_indices_20[idx, 2]
      A_unfocus[i, j] <- 0
    }
    
    for (idx in 1:nrow(IU_indices_20)) {
      i <- IU_indices_20[idx, 1]
      j <- IU_indices_20[idx, 2]
      A_unfocus[i, j] <- 0
    }
    
  }
  
  assign(paste0("A_unfocus_", year), A_unfocus)
  assign(paste0("Z_20_", year), as.matrix(A_unfocus %*% diag(as.vector(as.matrix(X_year)))))
  
}

# ----------------------------------- #
## 2c GRAS ----
# ----------------------------------- #

GRAS <- function(A, r_bar, s_bar, iteration_maximum = 2000, accuracy = 1e-6, epsilon = 1e-8) {
  # Define the P and N matrices
  P <- as.matrix(ifelse(A > 0, A, 0),ncol=ncol(A), nrow= nrow(A))
  N <- as.matrix(ifelse(A < 0, -A, 0), nrow= nrow(A))
  
  # Initialize R and S as diagonal matrices
  m <- nrow(A)
  n <- ncol(A)
  R <- diag(1, m, m)
  S <- diag(NA, n, n)
  
  r_star <- r_bar * exp(1)
  s_star <- s_bar * exp(1)
  
  # Define functions for updating r_i and s_j
  rho_i <- function(s, r_star_i, p_i, n_i) {
    (r_star_i + sqrt(r_star_i^2 + 4 * p_i * n_i)) / (2 * p_i)
  }
  
  sigma_j <- function(r, s_star_j, p_j, n_j) {
    (s_star_j + sqrt(s_star_j^2 + 4 * p_j * n_j)) / (2 * p_j)
  }
  
  # Iteration
  for (iteration in 1:iteration_maximum) {
    # Update S
    for (j in 1:n) {
      p_j <- sum(P[, j] * diag(R))
      n_j <- sum(N[, j] / diag(R))
      S[j, j] <- sigma_j(diag(R), s_star[j], p_j, n_j)
      if (is.na(S[j, j]) || is.infinite(S[j, j]) || abs(S[j, j]) < epsilon) {
        S[j, j] <- epsilon
      }
    }
    
    # Update R
    for (i in 1:m) {
      p_i <- sum(P[i, ] * diag(S))
      n_i <- sum(N[i, ] / diag(S))
      R[i, i] <- rho_i(diag(S), r_star[i], p_i, n_i)
      if (is.na(R[i, i]) || is.infinite(R[i, i]) || abs(R[i, i]) < epsilon) {
        R[i, i] <- epsilon
      }
    }
    
    # Check for convergence
    i <- as.matrix(rep(1, n),ncol = 1, nrow = n)
    RS <- R %*% P %*% S
    RN <- tryCatch({
      solve(R) %*% N %*% solve(S)
    }, error = function(e) {
      ginv(R + epsilon * diag(m)) %*% N %*% ginv(S + epsilon * diag(n))
    })
    
    i <- as.matrix(rep(1, n),ncol = 1, nrow = n)  
    i_RS <- RS %*% i
    i_RN <- RN %*% i
    error_r <- norm(i_RS - i_RN - r_star, "F") / norm(as.matrix(r_star,nrow=153), "F")
    
    i <- as.matrix(rep(1, m),ncol = m, nrow = 1)
    i_RS_trans <- t(i) %*% RS
    i_RN_trans <- t(i) %*% RN
    error_s <- norm(i_RS_trans - i_RN_trans - s_star, "F") / norm(as.matrix(s_star,nrow=1), "F")
    
    print(paste("Iteration:", iteration, "Error_r:", error_r, "Error_s:", error_s))
    
    if (error_r < accuracy && error_s < accuracy) {
      break
    }
  }
  
  # Calculate the updated matrix X with constraints
  Z <- matrix(0, nrow = m, ncol = n)
  r <- diag(R)
  s <- diag(S)
  
  for (i in 1:m) {
    for (j in 1:n) {
      if (A[i, j] >= 0) {
        Z[i, j] <- r[i] * A[i, j] * s[j] / exp(1)
      } else {
        Z[i, j] <- (1 / r[i]) * A[i, j] * (1 / s[j]) / exp(1)
      }
    }
  }
  
  # Ensure the updated values are within ±20% of the original values
  lower_bound <- A - abs(A) * 0.2
  upper_bound <- A + abs(A) * 0.2
  Z <- pmin(pmax(Z, lower_bound), upper_bound)
  
  # Normalize rows and columns to satisfy constraints
  row_sums <- rowSums(Z)
  col_sums <- colSums(Z)
  a <- 0
  b <- 0
  
  for (i in 1:m) {
    if (row_sums[i] != r_bar[i]){
      Z[i, ] <- Z[i, ] * abs(r_bar[i] / row_sums[i])
      a <- a+1
    }
  }
  print(a)
  
  for (j in 1:n) {
    if (col_sums[j] != s_bar[j]){
      Z[, j] <- Z[, j] * abs(s_bar[j] / col_sums[j])
      b <- b+1
    }
  }
  print(b)
  
  return(Z)
}

for (year in c(2019, 2021:2023)){
  
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
  # assign(paste0("Z_2_", year), Z_2_year)
  Z_year <- Z_1_year + Z_2_year
  assign(paste0("Z_", year,"_G"), Z_year)
  
  # A_year <- Z_year %*% solve(diag(as.vector(as.matrix(X_year))))
  # assign(paste0("A_", year), A_year)
  # I_diag <- diag(1,nrow=153)
  # assign(paste0("L_", year), solve(I_diag - A_year))
  
}

fwrite(Z_2023_G,"Z_2023_G.csv")

# ----------------------------------- #
## 2d RAS ----
# ----------------------------------- #

library(lpSolve)
library(plot3D)
library(ioanalysis)

RS_label <- matrix(c(rep("China",153),1:153),ncol=2)
V_label <- matrix(1:153,ncol=1)

for (year in c(2019, 2021:2023)){
  
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
  
  IO_year <- as.inputoutput(Z = Z_20_year,RS_label = RS_label,
                            f = matrix(fu_year,ncol=1), f_label = V_label,
                            X = X_year,
                            V = matrix(va_year,nrow=1), V_label = t(V_label),
                            M = im_year, M_label = V_label)
  
  A_2_year <- ras(io = IO_year,
                  x1 = X_year,
                  u1 = r_bar,
                  v1 = s_bar,
                  verbose = T,
                  tol = 1e-15)
  
  Z_2_year <- A_2_year %*% diag(as.vector(as.matrix(X_year)))
  # assign(paste0("Z_2_", year), Z_2_year)
  Z_year <- Z_1_year + Z_2_year
  assign(paste0("Z_", year), Z_year)
  
  # A_year <- Z_year %*% solve(diag(as.vector(as.matrix(X_year))))
  # assign(paste0("A_", year), A_year)
  # I_diag <- diag(1,nrow=153)
  # assign(paste0("L_", year), solve(I_diag - A_year))
  
}

fwrite(Z_2023,"Z_2023.csv")
