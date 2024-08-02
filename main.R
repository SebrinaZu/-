
library(devtools)
library(usethis)
require(devtools)
install_github("f1kidd/fmlogit")

library(maxLik)
library(miscTools)
library(fmlogit)
install_github("moodymudskipper/boomer")
library(boomer)

dataset <- read.csv("data.csv")
dataset$Country <- as.factor(dataset$Country)
dataset$Year <- as.factor(dataset$Year)

y <- dataset[,4:37]
x <- dataset[,c(2:3,38:52)]

model <- fmlogit(y,x,cluster = dataset$Country)
