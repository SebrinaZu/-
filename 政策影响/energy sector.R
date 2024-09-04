
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

library(devtools)
library(usethis)
require(devtools)
install_github("f1kidd/fmlogit")

library(maxLik)
library(miscTools)
library(fmlogit)
install_github("moodymudskipper/boomer")
library(boomer)

#-----------------------------------------------------------------------------#
# 1. compile dataset  ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
## 1a read IO tables ----
# ----------------------------------- #

read_province_data <- function(base_dir, year) {
  
  year_dir <- file.path(base_dir, sprintf("%d年各省份投入产出表", year))
  file_list <- list.files(year_dir, full.names = TRUE, pattern = "\\.xls$")
  indices <- as.numeric(sub("^(\\d+).*$", "\\1", basename(file_list)))
  ordered_files <- file_list[order(indices)]
  
  data_list <- list()
  coeff_list <- list()
  
  for (file_path in ordered_files) {
    
    file_name <- basename(file_path)
    province_index <- as.numeric(sub("^(\\d+).*$", "\\1", file_name))
    
    if (year == 2002){
      if (province_index == 0){
        data <- as.data.frame(read_excel(file_path, range = "C8:BG57"))
        rownames(data) <- data[[1]]
        data <- data[, -1]
      } else {
        data <- as.data.frame(read_excel(file_path, range = "C6:BG55"))
        rownames(data) <- data[[1]]
        data <- data[, -1]
      }
    }
    
    if (year == 2007){
      data <- as.data.frame(read_excel(file_path, range = "C8:BG57"))
      rownames(data) <- data[[1]]
      data <- data[, -1]
    }
    
    if (year == 2012){
      if (province_index == 0){
        data <- as.data.frame(read_excel(file_path, range = "C10:BG59"))
        rownames(data) <- data[[1]]
        data <- data[, -1]
      } else {
        data <- as.data.frame(read_excel(file_path, range = "C9:BI58"))
        rownames(data) <- data[[1]]
        data <- data[, -1]
      }
    }
    
    if (year == 2015){
      if (province_index == 0){
        data <- as.data.frame(read_excel(file_path, range = "C8:BG57"))
        rownames(data) <- data[[1]]
        data <- data[, -1]
      } else if (province_index == 2){
        data <- as.data.frame(read_excel(file_path, range = "C8:BH57"))
        rownames(data) <- data[[1]]
        data <- data[, -1]
      } else {
        data <- as.data.frame(read_excel(file_path, range = "C10:BI59"))
        rownames(data) <- data[[1]]
        data <- data[, -1]
      }
    }
    
    if (year == 2017){
      if (province_index == 0){
        data_raw <- as.data.frame(read_excel(file_path, range = "C6:FI162"))
        rownames(data_raw) <- data_raw[[1]]
        data_raw <- data_raw[, -1]
        data <- turn_into_42sectors(data_raw)
      } else {
        data <- as.data.frame(read_excel(file_path, range = "C8:BI57"))
        rownames(data) <- data[[1]]
        data <- data[, -1]
      }
    }
    
    if (year %in% 2018:2023){
      if (province_index == 0){
        data_raw <- as.data.frame(read_excel(file_path, range = "C6:FM166"))
        rownames(data_raw) <- data_raw[[1]]
        data_raw <- data_raw[, -1]
        data <- turn_into_42sectors(data_raw)
      } 
    }
    
    cat("Data for province index", province_index, ":\n")
    print(head(data))
    
    coeff <- matrix(NA, ncol = 2, nrow = 44)
    colnames(coeff) <- c("TU", "TI")
    all_data[["IO_2018"]][["IO_00_2018"]]["TI", ]
    for (i in 1:42) {
      coeff[i, 1] <- data[i, "TIU"] / data["TI", "TIU"]
      coeff[i, 2] <- data["TII", i] / data["TII", "GO"]
    }
    
    coeff[43, 1] <- data["VA001", "TIU"] / data["TI", "TIU"]
    coeff[43, 2] <- data["TII", "TFU"] / data["TII", "GO"]
    coeff[44, 1] <- (data["TVA", "TIU"] - data["VA001", "TIU"]) / data["TI", "TIU"]
    coeff[44, 2] <- 0
    
    data_list[[paste0("IO_", sprintf("%02d", province_index), "_", year)]] <- data
    coeff_list[[paste0("Coeff_", sprintf("%02d", province_index), "_", year)]] <- coeff
  }
  
  return(list(IO = data_list, Coeff = coeff_list))
}

turn_into_42sectors <- function(data_raw){
  
  group_list <- list()
  group_list[[1]] <- grep("^(01|02|03|04|05)", rownames(data_raw))
  group_list[[2]] <- grep("^(06)", rownames(data_raw))
  group_list[[3]] <- grep("^(07)", rownames(data_raw))
  group_list[[4]] <- grep("^(08|09)", rownames(data_raw))
  group_list[[5]] <- grep("^(10|11)", rownames(data_raw))
  group_list[[6]] <- grep("^(13|14|15|16)", rownames(data_raw))
  group_list[[7]] <- grep("^(17)", rownames(data_raw))
  group_list[[8]] <- grep("^(18|19)", rownames(data_raw))
  group_list[[9]] <- grep("^(20|21)", rownames(data_raw))
  group_list[[10]] <- grep("^(22|23|24)", rownames(data_raw))
  group_list[[11]] <- grep("^(25)", rownames(data_raw))
  group_list[[12]] <- grep("^(26|27|28)", rownames(data_raw))
  group_list[[13]] <- grep("^(29|30)", rownames(data_raw))
  group_list[[14]] <- grep("^(31|32)", rownames(data_raw))
  group_list[[15]] <- grep("^(33)", rownames(data_raw))
  group_list[[16]] <- grep("^(34)", rownames(data_raw))
  group_list[[17]] <- grep("^(35)", rownames(data_raw))
  group_list[[18]] <- grep("^(36|37)", rownames(data_raw))
  group_list[[19]] <- grep("^(38)", rownames(data_raw))
  group_list[[20]] <- grep("^(39)", rownames(data_raw))
  group_list[[21]] <- grep("^(40)", rownames(data_raw))
  group_list[[22]] <- grep("^(41)", rownames(data_raw))
  group_list[[23]] <- grep("^(42)", rownames(data_raw))
  group_list[[24]] <- grep("^(43)", rownames(data_raw))
  group_list[[25]] <- grep("^(44)", rownames(data_raw))
  group_list[[26]] <- grep("^(45)", rownames(data_raw))
  group_list[[27]] <- grep("^(46)", rownames(data_raw))
  group_list[[28]] <- grep("^(47|48|49|50)", rownames(data_raw))
  group_list[[29]] <- grep("^(51|52)", rownames(data_raw))
  group_list[[30]] <- grep("^(53|54|55|56|57|58|59|60)", rownames(data_raw))
  group_list[[31]] <- grep("^(61|62)", rownames(data_raw))
  group_list[[32]] <- grep("^(63|64|65)", rownames(data_raw))
  group_list[[33]] <- grep("^(66|67|68)", rownames(data_raw))
  group_list[[34]] <- grep("^(70)", rownames(data_raw))
  group_list[[35]] <- grep("^(71|72)", rownames(data_raw))
  group_list[[36]] <- grep("^(73|74|75)", rownames(data_raw))
  group_list[[37]] <- grep("^(76|77|78)", rownames(data_raw))
  group_list[[38]] <- grep("^(80|81)", rownames(data_raw))
  group_list[[39]] <- grep("^(83)", rownames(data_raw))
  group_list[[40]] <- grep("^(84|85)", rownames(data_raw))
  group_list[[41]] <- grep("^(86|87|88|89|90)", rownames(data_raw))
  group_list[[42]] <- grep("^(91|94)", rownames(data_raw))
  
  data <- matrix(NA, nrow = 49, ncol = 55)
  rownames(data) <- c(sprintf("%02d", 1:42),
                            "TII","VA001","VA002","VA003","VA004","TVA","TI")
  colnames(data) <- c(sprintf("%02d", 1:42),
                            "TIU",	"FU101",	"FU102",	"THC",	"FU103",	"TC",	"FU201",	"FU202",	"GCF",	"EX",	"TFU",	"IM",	"GO")
  
  for (i in 1:nrow(data)){
    for (j in 1: ncol(data)){
      if ((i %in% 1:42) & (j %in% 1:42)){
        data[i,j] <- sum(data_raw[group_list[[i]],group_list[[j]]],na.rm = T)
      } else if ((i %in% 1:42) & (j > 42)){
        data[i,j] <- sum(data_raw[group_list[[i]],colnames(data)[j]],na.rm = T)
      } else if ((j %in% 1:42) & (i > 42)){
        data[i,j] <- sum(data_raw[rownames(data)[i],group_list[[j]]],na.rm = T)
      } else if ((j == 43) & (i > 42)){
        data[i,j] <- data_raw[rownames(data)[i],colnames(data)[j]]
      } else if ((i == 43) & (j > 42)){
        data[i,j] <- data_raw[rownames(data)[i],colnames(data)[j]]
      } 
    }
  }
  
  return(data)
}

years <- c(2002, 2007, 2012, 2015, 2017:2023)

all_data <- list()

for (year in years) {

  base_dir <- "2002-2017分省投入产出表"
  data <- read_province_data(base_dir, year)
  
  IO_data <- data$IO
  Coeff_data <- data$Coeff
  
  cat("Adding data for year", year, ":\n")
  print(names(IO_data))
  print(names(Coeff_data))
  
  all_data[[paste0("IO_", year)]] <- IO_data
  all_data[[paste0("Coeff_", year)]] <- Coeff_data

}

# ----------------------------------- #
## 1b generate final dataset ----
# ----------------------------------- #

dataset <- as.data.frame(read_excel("energy consumption.xlsx", range = "A1:CU167"))
colnames(dataset)[13:54] <- paste0("S_",sprintf("%02d", 1:42))
colnames(dataset)[57:98] <- paste0("S_",sprintf("%02d", 1:42),"_2")
colnames(dataset)[99] <- "Other_2"

for (i in 1:nrow(dataset)) {
  
  province_index <- dataset$Province_index[i]
  year <- dataset$Year[i]
  Coeff_list <- all_data[[paste0("Coeff_", year)]]
  TU_coeff_year <- Coeff_list[[paste0("Coeff_", province_index, "_",year)]][,1]
  TI_coeff_year <- Coeff_list[[paste0("Coeff_", province_index, "_",year)]][,2]
  
  if (!is.null(TU_coeff_year)){
    dataset[i, 13:56] <- TU_coeff_year
  }
  if (!is.null(TI_coeff_year)){
    dataset[i, 57:98] <- TI_coeff_year[1:42]
    dataset[i, 99] <- 1- sum(TI_coeff_year[1:42])
  }
}

#-----------------------------------------------------------------------------#
# 2. fit model  ----
#-----------------------------------------------------------------------------#

dataset_cleaned <- na.omit(dataset)
fwrite(dataset_cleaned[,-2],"data.csv")

fmlogit_2 <- function(y, X, beta0 = NULL, MLEmethod = "NR", maxit = 5e+07, 
                      abstol = 1e-05, cluster = NULL, reps = 1000, ...){
  start.time = proc.time()
  
  if(length(cluster)!=nrow(y) & !is.null(cluster)){
    warning("Length of the cluster does not match the data. Cluster is ignored.")
    cluster = NULL
  }
  
  Xclass = sapply(X, class)
  Xfac = which(Xclass %in% c("factor", "character"))
  if (length(Xfac) > 0) {
    Xfacnames = colnames(X)[Xfac]
    strformFac = paste(Xfacnames, collapse = "+")
    Xdum = model.matrix(as.formula(paste("~", strformFac, 
                                         sep = "")), data = X)[, -1]
    X = cbind(X, Xdum)
    X = X[, -Xfac]
  }
  
  Xnames = colnames(X)
  ynames = colnames(y)
  X = as.matrix(X)
  y = as.matrix(y)
  n = dim(X)[1]
  j = dim(y)[2]
  k = dim(X)[2]
  xy = cbind(X, y)
  xy = na.omit(xy)
  row.remain = setdiff(1:n, attr(xy, "na.action"))
  X = xy[, 1:k]
  y = xy[, (k + 1):(k + j)]
  n = dim(y)[1]
  remove(xy)
  
  # adding in the constant term
  if(k==1){
    # check if the input X is constant
    if(length(unique(X))==1){ # X is constant
      Xnames = "constant"
      X = as.matrix(as.numeric(X),nrow=1)
      colnames(X) = Xnames
      X = as.matrix(X)
      k=0
    }else{ # one single variable of input
      Xnames = "X1"
      X = as.matrix(X)
      k = dim(X)[2]
      X = cbind(X, rep(1, n))
      Xnames = c(Xnames, "constant")
      colnames(X) = Xnames
    }
  }else{ # normal cases
    X = X[, apply(X, 2, function(x) length(unique(x)) != 1)]
    Xnames = colnames(X)
    k = dim(X)[2]
    X = cbind(X, rep(1, n))
    Xnames = c(Xnames, "constant")
    colnames(X) = Xnames
  }
  
  testcols <- function(X) {
    m = crossprod(as.matrix(X))
    ee = eigen(m)
    evecs <- split(zapsmall(ee$vectors), col(ee$vectors))
    mapply(function(val, vec) {
      if (val != 0) 
        NULL
      else which(vec != 0)
    }, zapsmall(ee$values), evecs)
  }
  
  collinear = unique(unlist(testcols(X)))
  while (length(collinear) > 0) {
    if (qr(X)$rank == dim(X)[2]) 
      print("Model may suffer from multicollinearity problems.")
    break
    if ((k + 1) %in% collinear) 
      collinear = collinear[-length(collinear)]
    X = X[, -collinear[length(collinear)]]
    Xnames = colnames(X)
    k = k - 1
    collinear = unique(unlist(testcols(X)))
  }
  
  QMLE <- function(betas) {
    betas = matrix(betas, nrow = j - 1, byrow = T)
    betamat = rbind(rep(0, k + 1), betas)
    llf = 0
    for (i in 1:j) {
      L = y[, i] * ((X %*% betamat[i, ]) - log(rowSums(exp(X %*% 
                                                             t(betamat)))))
      llf = llf + sum(L)
    }
    return(llf)
  }
  
  QMLE_Obs <- function(betas) {
    betas = matrix(betas, nrow = j - 1, byrow = T)
    betamat = rbind(rep(0, k + 1), betas)
    llf = rep(0, n)
    for (i in 1:j) {
      L = y[, i] * ((X %*% betamat[i, ]) - log(rowSums(exp(X %*% t(betamat)))))
      llf = llf + L
    }
    iteration_number <<- iteration_number + 1
    cat(sprintf("Iteration %d: log pseudolikelihood = %f\n", iteration_number, sum(llf)))
    return(llf)
  }
  
  if (length(beta0) == 0){
    beta0 = rep(0, (k + 1) * (j - 1))
  }
  
  if (length(beta0) != (k + 1) * (j - 1)) {
    beta0 = rep(0, (k + 1) * (j - 1))
    warning("Wrong length of beta0 given. Use default setting instead.")
  }
  
  iteration_number <<- 0
  opt <- maxLik(QMLE_Obs, start = beta0, method = MLEmethod, 
                control = list(iterlim = maxit, tol = abstol, printLevel = 2), ...)
  
  betamat = matrix(opt$estimate, ncol = k + 1, byrow = T)
  betamat_aug = rbind(rep(0, k + 1), betamat)
  colnames(betamat_aug) = Xnames
  rownames(betamat_aug) = ynames
  sigmat = matrix(nrow = j - 1, ncol = k + 1)
  vcov = list()
  
  ###insert--nonparametric bootstrap procedure (clustered SE and vcov)
  
  if(is.null(cluster) == F){
    cluster = cluster[row.remain]
    clusters <- names(table(cluster))
    for (i in 1:j) {
      # cluster should preferably be coming from a same data frame with the original y and X. 
      sterrs <- matrix(NA, nrow=reps, ncol=k + 1)
      vcov_j_list=list()
      
      b=1
      no_singular_error=c()
      while(b<=reps){
        
        index <- sample(1:length(clusters), length(clusters), replace=TRUE)
        aa <- clusters[index]
        bb <- table(aa)
        bootdat <- NULL
        dat=cbind(y,X)
        for(b1 in 1:max(bb)){
          cc <- dat[cluster %in% names(bb[bb %in% b1]),]
          for(b2 in 1:b1){
            bootdat <- rbind(bootdat, cc)
          }
        }
        
        bootdatX=matrix(bootdat[,(j+1):ncol(bootdat)],nrow=nrow(bootdat))
        bootdaty=bootdat[,1:j]
        
        sum_expxb = rowSums(exp(bootdatX %*% t(betamat_aug)))
        expxb = exp(bootdatX %*% betamat_aug[i, ])
        G = expxb/sum_expxb
        g = (expxb * sum_expxb - expxb^2)/sum_expxb^2
        X_a = bootdatX * as.vector(sqrt(g^2/(G * (1 - G))))
        A = t(X_a) %*% X_a
        mu = bootdaty[, i] - G
        X_b = bootdatX * as.vector(mu * g/G/(1 - G))
        B = t(X_b) %*% X_b
        
        a_solve_error = tryCatch(solve(A),error=function(e){NULL})
        if(is.null(a_solve_error)){
          no_singular_error=c(no_singular_error,b)
          next
        }
        
        Var_b = solve(A) %*% B %*% solve(A)
        std_b = sqrt(diag(Var_b))
        sterrs[b,]=std_b
        vcov_j_list[[b]]=Var_b
        
        b=b+1
      }
      if(length(no_singular_error)>0){
        warning(paste('Error in solve.default(A) : Lapack routine dgesv: system is exactly singular: U[28,28] = 0" Appeared',length(no_singular_error),'times within cluster bootstrap for outcome #',i))
      }
      std_b=apply(sterrs,2,mean)
      vcov[[i]] = Reduce("+", vcov_j_list) / length(vcov_j_list)
      if (i > 1) 
        sigmat[i - 1, ] = std_b
    }
  }else{
    
    for(i in 1:j){
      # start calculation  
      sum_expxb = rowSums(exp(X %*% t(betamat_aug))) # sum of the exp(x'b)s
      expxb = exp(X %*% betamat_aug[i,]) # individual exp(x'b)
      G = expxb / sum_expxb # exp(X'bj) / sum^J(exp(X'bj))
      g = (expxb * sum_expxb - expxb^2) / sum_expxb^2 # derivative of the logit function
      
      # Here the diagonal of A is the 'standard' standard error
      # hat(A) = sum hat(gi)^2 * xi'xi / hat(Gi)(1-hat(Gi))
      # or, Xtilde = X * sqrt(g^2/G(1-G)), A = Xtilde'Xtilde
      X_a = X * as.vector(sqrt(g^2/(G*(1-G))))
      A = t(X_a) %*% X_a
      
      # robust standard error, again following PW(1996)
      mu = y[,i] - G
      X_b = X * as.vector(mu * g / G / (1-G))
      B = t(X_b) %*% X_b
      
      a_solve = tryCatch({
        solve(A)
      }, error = function(e) {
        warning("Error in solve.default(A): Matrix A is computationally singular.")
        return(NULL)
      })
      
      if(!is.null(a_solve)){
        Var_b = a_solve %*% B %*% a_solve
        std_b = sqrt(diag(Var_b))
        vcov[[i]] = Var_b
        if(i>1) sigmat[i-1,] = std_b
      } else {
        next
      }
      
    }
  }
  
  ###end of insert--nonparametric bootstrap procedure (clustered SE and vcov)
  
  listmat = list()
  for (i in 1:(j - 1)) {
    tabout = matrix(ncol = 4, nrow = k + 1)
    tabout[, 1:2] = t(rbind(betamat[i, ], sigmat[i, ]))
    tabout[, 3] = tabout[, 1]/tabout[, 2]
    tabout[, 4] = 2 * (1 - pnorm(abs(tabout[, 3])))
    colnames(tabout) = c("estimate", "std", "z", "p-value")
    if (length(Xnames) > 0) 
      rownames(tabout) = Xnames
    listmat[[i]] = tabout
  }
  if (length(ynames) > 0) 
    names(listmat) = ynames[2:j]
  outlist = list()
  outlist$estimates = listmat
  outlist$baseline = ynames[1]
  outlist$likelihood = opt$maximum
  outlist$conv_code = opt$code
  outlist$convergence = paste(opt$type, paste(as.character(opt$iterations), 
                                              "iterations"), opt$message, sep = ",")
  outlist$count = c(Obs = n, Explanatories = k, Choices = j)
  outlist$y = y
  outlist$X = X
  outlist$rowNo = row.remain
  outlist$coefficient = betamat_aug
  names(vcov) = ynames
  outlist$vcov = vcov
  outlist$cluster = cluster
  outlist$reps=ifelse(is.null(cluster),0,reps)
  
  print(paste("Fractional logit model estimation completed. Time:", 
              round(proc.time()[3] - start.time[3], 1), "seconds"))
  return(structure(outlist, class = "fmlogit"))
}

y <- dataset_cleaned[,13:56]
x <- dataset_cleaned[,c(1,3:8,10,12)]

model <- fmlogit_2(y = y,X = x,cluster = dataset_cleaned$Province_index)

effects.fmlogit(model, effect = "marginal", se = T, varlist = colnames(x)[3])
predict.fmlogit(model,x[1,])

#-----------------------------------------------------------------------------#
# 3. predict results  ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
## 3a fetch r and s for 153 sectors ----
# ----------------------------------- #

initial_2023 <- as.data.frame(read_excel("2023.xls", sheet = "2023", range = "C6:FM166"))
rownames(initial_2023) <- initial_2023[[1]]
initial_2023 <- initial_2023[, -1]

Coeff_42sectors <- matrix(c(read_excel("results.xlsx", sheet = "Predicted results", range = "L5:CT5", col_names = F),0), 
       nrow = 44, ncol = 2)

turn_into_153sectors <- function(data_raw, results){
  
  group_list <- list()
  group_list[[1]] <- grep("^(01|02|03|04|05)", rownames(data_raw))
  group_list[[2]] <- grep("^(06)", rownames(data_raw))
  group_list[[3]] <- grep("^(07)", rownames(data_raw))
  group_list[[4]] <- grep("^(08|09)", rownames(data_raw))
  group_list[[5]] <- grep("^(10|11)", rownames(data_raw))
  group_list[[6]] <- grep("^(13|14|15|16)", rownames(data_raw))
  group_list[[7]] <- grep("^(17)", rownames(data_raw))
  group_list[[8]] <- grep("^(18|19)", rownames(data_raw))
  group_list[[9]] <- grep("^(20|21)", rownames(data_raw))
  group_list[[10]] <- grep("^(22|23|24)", rownames(data_raw))
  group_list[[11]] <- grep("^(25)", rownames(data_raw))
  group_list[[12]] <- grep("^(26|27|28)", rownames(data_raw))
  group_list[[13]] <- grep("^(29|30)", rownames(data_raw))
  group_list[[14]] <- grep("^(31|32)", rownames(data_raw))
  group_list[[15]] <- grep("^(33)", rownames(data_raw))
  group_list[[16]] <- grep("^(34)", rownames(data_raw))
  group_list[[17]] <- grep("^(35)", rownames(data_raw))
  group_list[[18]] <- grep("^(36|37)", rownames(data_raw))
  group_list[[19]] <- grep("^(38)", rownames(data_raw))
  group_list[[20]] <- grep("^(39)", rownames(data_raw))
  group_list[[21]] <- grep("^(40)", rownames(data_raw))
  group_list[[22]] <- grep("^(41)", rownames(data_raw))
  group_list[[23]] <- grep("^(42)", rownames(data_raw))
  group_list[[24]] <- grep("^(43)", rownames(data_raw))
  group_list[[25]] <- grep("^(44)", rownames(data_raw))
  group_list[[26]] <- grep("^(45)", rownames(data_raw))
  group_list[[27]] <- grep("^(46)", rownames(data_raw))
  group_list[[28]] <- grep("^(47|48|49|50)", rownames(data_raw))
  group_list[[29]] <- grep("^(51|52)", rownames(data_raw))
  group_list[[30]] <- grep("^(53|54|55|56|57|58|59|60)", rownames(data_raw))
  group_list[[31]] <- grep("^(61|62)", rownames(data_raw))
  group_list[[32]] <- grep("^(63|64|65)", rownames(data_raw))
  group_list[[33]] <- grep("^(66|67|68)", rownames(data_raw))
  group_list[[34]] <- grep("^(70)", rownames(data_raw))
  group_list[[35]] <- grep("^(71|72)", rownames(data_raw))
  group_list[[36]] <- grep("^(73|74|75)", rownames(data_raw))
  group_list[[37]] <- grep("^(76|77|78)", rownames(data_raw))
  group_list[[38]] <- grep("^(80|81)", rownames(data_raw))
  group_list[[39]] <- grep("^(83)", rownames(data_raw))
  group_list[[40]] <- grep("^(84|85)", rownames(data_raw))
  group_list[[41]] <- grep("^(86|87|88|89|90)", rownames(data_raw))
  group_list[[42]] <- grep("^(91|94)", rownames(data_raw))
  
  data <- matrix(NA, nrow = 153, ncol = 2)
  rownames(data) <- sprintf("%03d", 1:153)
  colnames(data) <- c("TU","TI")
  
  k <- 0
  for (i in 1:42){
    bin_1 <- data_raw[group_list[[i]],"TIU"]
    bin_2 <- as.numeric(data_raw["TII",group_list[[i]]])
    for (j in 1:length(bin_1)){ 
      k <- k + 1
      data[k,1] <- max(as.numeric(results[i,1]) / sum(bin_1) * bin_1[j],0)
      data[k,2] <- max(as.numeric(results[i,2]) / sum(bin_2) * bin_2[j],0)
    }
  }
  
  return(data)
}

Coeff_153sectors <- turn_into_153sectors(data_raw = initial_2023, 
                                         results = Coeff_42sectors)

# ----------------------------------- #
## 3b prepare data  ----
# ----------------------------------- #

prepare_data <- function(year, Z_S100, r, s){
  
  # base data
  
  X <- as.data.frame(read_excel("2019-2023.xlsx", sheet = "X", range = "B1:E154"))
  price_index <- as.matrix(read_excel("2019-2023.xlsx",sheet = "Price", range = "B1:E154"))
  
  IO_18 <- read_excel("IO表 02-20.xlsx", sheet = "2018", range = "C6:FM166")
  IO_18 <- as.data.frame(IO_18)
  rownames(IO_18) <- IO_18[[1]]
  IO_18 <- IO_18[, -1]
  
  IO_20 <- read_excel("IO表 02-20.xlsx", sheet = "2020", range = "C6:FM166")
  IO_20 <- as.data.frame(IO_20)
  rownames(IO_20) <- IO_20[[1]]
  IO_20 <- IO_20[, -1]
  
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
  
  price_index_year <- price_index[,as.character(year)]
  if (year == 2019){
    A_0 <- diag(as.numeric(price_index_year)/100) %*% A_18
  } else {
    A_0 <- diag(as.numeric(price_index_year)/100) %*% A_20
  }
  
  X_year <- X[,as.character(year)]
  Z_0 <- as.matrix(A_0 %*% diag(as.vector(as.matrix(X_year))))
  
  # A_focus and Z_1
  
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
  
  A_focus <- matrix(0, nrow=153, ncol=153)

  if (year == 2019){
    
    for (idx in 1:nrow(II_indices_18)) {
      i <- II_indices_18[idx, 1]
      j <- II_indices_18[idx, 2]
      A_focus[i, j] <- A_0[i, j]
    }
    
    for (idx in 1:nrow(IU_indices_18)) {
      i <- IU_indices_18[idx, 1]
      j <- IU_indices_18[idx, 2]
      A_focus[i, j] <- A_0[i, j]
    }
    
  } else {
    
    for (idx in 1:nrow(II_indices_20)) {
      i <- II_indices_20[idx, 1]
      j <- II_indices_20[idx, 2]
      A_focus[i, j] <- A_0[i, j]
    }
    
    for (idx in 1:nrow(IU_indices_20)) {
      i <- IU_indices_20[idx, 1]
      j <- IU_indices_20[idx, 2]
      A_focus[i, j] <- A_0[i, j]
    }
    
  }
  
  Z_1 <- as.matrix(A_focus %*% diag(as.vector(as.matrix(X_year))))
  Z_1[,100] <- Z_S100

  # A_unfocus and Z_20
  
  A_unfocus <- A_0
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
  
  Z_20 <- as.matrix(A_unfocus %*% diag(as.vector(as.matrix(X_year))))
  Z_20[,100] <- rep(0,153)
  
  i_col <- as.matrix(rep(1,153), ncol = 1, nrow = 153)
  r_bar <- as.matrix(r - Z_1 %*% i_col)
  s_bar <- as.matrix(s - t(i_col) %*% Z_1)
  
  return(list(Z_1, Z_20, r_bar, s_bar))
  
}

r <- as.numeric(Coeff_153sectors[,1] * initial_2023["TI","TIU"])
s <- as.numeric(Coeff_153sectors[,2] * initial_2023["TI","TIU"])

Z_S100 <- as.matrix(read_excel("Z_S100.xlsx", range = "C1:C153", col_names = F))

bin <- prepare_data(year = 2023, Z_S100, r, s)
Z_1 <- bin[[1]]
Z_20 <- bin[[2]]
r_bar <- bin[[3]]
s_bar <- bin[[4]]

# ----------------------------------- #
## 3c GRAS  ----
# ----------------------------------- #

GRAS <- function(A, r_bar, s_bar, 
                 iteration_maximum = 5000, accuracy = 1e-6, epsilon = 1e-8) {
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

Z_2 <- GRAS(A = Z_20, r_bar, s_bar)

for (i in 1:153){
  for (j in 1:153){
    if (is.nan(Z_2[i,j])){
      Z_2[i,j] <- 0
    }
  }
}
Z <- Z_1 + Z_2
fwrite(Z,"Z_2023_new.csv")

#-----------------------------------------------------------------------------#
# 4. 42 sectors  ----
#-----------------------------------------------------------------------------#

new_2023 <- as.data.frame(read_excel("2023.xls", sheet = "2023-new", range = "C6:FM166"))
rownames(new_2023) <- new_2023[[1]]
new_2023 <- new_2023[, -1]

initial_2023_42s <- turn_into_42sectors(initial_2023)
new_2023_42s <- turn_into_42sectors(new_2023)

fwrite(initial_2023_42s,"initial_2023_42s.csv",row.names = T)
fwrite(new_2023_42s,"new_2023_42s.csv",row.names = T)
