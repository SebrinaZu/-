
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

#-----------------------------------------------------------------------------#
# 1 总量指标的确定  ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
## 1a GO ----
# ----------------------------------- #

# ----------------------------------- #
### GO_re  ----
# ----------------------------------- #

years <- c("2002", "2005", "2007", "2010", "2012", "2015", "2017", "2018", "2020")
row_names <- sprintf("%02d", 1:34)
TO_re <- data.frame(matrix(ncol = length(years), nrow = length(row_names)))
colnames(TO_re) <- years
rownames(TO_re) <- row_names

# 2002

IO_02 <- read_excel("IO表 02-20.xlsx",sheet = "2002",range = "C6:EI135")
IO_02 <- as.data.frame(IO_02)
rownames(IO_02) <- IO_02[[1]]
IO_02 <- IO_02[,-1]

TO_re["01", "2002"] <- sum(IO_02[grep("^(01|02|03|04|05)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["02", "2002"] <- sum(IO_02[grep("^(06)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["03", "2002"] <- sum(IO_02[grep("^(07)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["04", "2002"] <- sum(IO_02[grep("^(08|09)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["05", "2002"] <- sum(IO_02[grep("^(10)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["06", "2002"] <- sum(IO_02[grep("^(13|14|15|16)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["07", "2002"] <- sum(IO_02[grep("^(17)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["08", "2002"] <- sum(IO_02[grep("^(18|19)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["09", "2002"] <- sum(IO_02[grep("^(20|21)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["10", "2002"] <- sum(IO_02[grep("^(22|23|24)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["11", "2002"] <- sum(IO_02[grep("^(25)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["12", "2002"] <- sum(IO_02[grep("^(26|27|28|29|30)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["13", "2002"] <- sum(IO_02[grep("^(31)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["14", "2002"] <- sum(IO_02[grep("^(32|33)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["15", "2002"] <- sum(IO_02[grep("^(34)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["16", "2002"] <- sum(IO_02[grep("^(35|36)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["17", "2002"] <- sum(IO_02[grep("^(37)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["18", "2002"] <- sum(IO_02[grep("^(39)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["19", "2002"] <- sum(IO_02[grep("^(40)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["20", "2002"] <- sum(IO_02[grep("^(41)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["21", "2002"] <- sum(IO_02[grep("^(42|43)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["22", "2002"] <- sum(IO_02[grep("^(44)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["23", "2002"] <- sum(IO_02[grep("^(45)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["24", "2002"] <- sum(IO_02[grep("^(46)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["25", "2002"] <- sum(IO_02[grep("^(47)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["26", "2002"] <- sum(IO_02[grep("^(63)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["27", "2002"] <- sum(IO_02[grep("^(51|52|53|54|55|56|58|59)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["28", "2002"] <- sum(IO_02[grep("^(66|67)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["29", "2002"] <- sum(IO_02[grep("^(60|61)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["30", "2002"] <- sum(IO_02[grep("^(68|70)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["31", "2002"] <- sum(IO_02[grep("^(72)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["32", "2002"] <- sum(IO_02[grep("^(75|76)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["33", "2002"] <- sum(IO_02[grep("^(84|85|86|88|91|92)", rownames(IO_02)), "GO"], na.rm = TRUE)
TO_re["34", "2002"] <- sum(IO_02[grep("^(93)", rownames(IO_02)), "GO"], na.rm = TRUE)

# 2005
  
IO_05 <- read_excel("IO表 02-20.xlsx",sheet = "2005",range = "C6:BC55")
IO_05 <- as.data.frame(IO_05)
rownames(IO_05) <- IO_05[[1]]
IO_05 <- IO_05[,-1]

TO_re[1:20, "2005"] <- IO_05[1:20, "GO"]
TO_re[21, "2005"] <- IO_05[21, "GO"]+IO_05[22, "GO"]
TO_re[22:25, "2005"] <- IO_05[23:26, "GO"]
TO_re[26, "2005"] <- IO_05[30, "GO"]
TO_re[27, "2005"] <- IO_05[27, "GO"]+IO_05[28, "GO"]
TO_re[28, "2005"] <- IO_05[31, "GO"]
TO_re[29, "2005"] <- IO_05[29, "GO"]
TO_re[30:31, "2005"] <- IO_05[32:33, "GO"]
TO_re[32, "2005"] <- sum(IO_05[35:36, "GO"], na.rm = TRUE)
TO_re[33, "2005"] <- sum(IO_05[39:41, "GO"], na.rm = TRUE)
TO_re[34, "2005"] <- IO_05[42, "GO"]

# 2007

IO_07 <- read_excel("IO表 02-20.xlsx",sheet = "2007",range = "C6:EV148")
IO_07 <- as.data.frame(IO_07)
rownames(IO_07) <- IO_07[[1]]
IO_07 <- IO_07[,-1]

TO_re[1, "2007"] <- sum(IO_07[1:5, "GO"], na.rm = TRUE)
TO_re[2:3, "2007"] <- IO_07[6:7, "GO"]
TO_re[4, "2007"] <- sum(IO_07[8:9, "GO"], na.rm = TRUE)
TO_re[5, "2007"] <- sum(IO_07[10, "GO"], na.rm = TRUE)
TO_re[6, "2007"] <- sum(IO_07[11:24, "GO"], na.rm = TRUE)
TO_re[7, "2007"] <- sum(IO_07[25:29, "GO"], na.rm = TRUE)
TO_re[8, "2007"] <- sum(IO_07[30:31, "GO"], na.rm = TRUE)
TO_re[9, "2007"] <- sum(IO_07[32:33, "GO"], na.rm = TRUE)
TO_re[10, "2007"] <- sum(IO_07[34:36, "GO"], na.rm = TRUE)
TO_re[11, "2007"] <- sum(IO_07[37:38, "GO"], na.rm = TRUE)
TO_re[12, "2007"] <- sum(IO_07[39:49, "GO"], na.rm = TRUE)
TO_re[13, "2007"] <- sum(IO_07[50:56, "GO"], na.rm = TRUE)
TO_re[14, "2007"] <- sum(IO_07[57:62, "GO"], na.rm = TRUE)
TO_re[15, "2007"] <- sum(IO_07[63, "GO"], na.rm = TRUE)
TO_re[16, "2007"] <- sum(IO_07[64:72, "GO"], na.rm = TRUE)
TO_re[17, "2007"] <- sum(IO_07[73:76, "GO"], na.rm = TRUE)
TO_re[18, "2007"] <- sum(IO_07[77:81, "GO"], na.rm = TRUE)
TO_re[19, "2007"] <- sum(IO_07[82:87, "GO"], na.rm = TRUE)
TO_re[20, "2007"] <- sum(IO_07[88, "GO"], na.rm = TRUE)
TO_re[21, "2007"] <- sum(IO_07[89:91, "GO"], na.rm = TRUE)
TO_re[22:25, "2007"] <- sum(IO_07[92:95, "GO"], na.rm = TRUE)
TO_re[26, "2007"] <- sum(IO_07[108, "GO"], na.rm = TRUE)
TO_re[27, "2007"] <- sum(IO_07[97:104, "GO"], na.rm = TRUE)
TO_re[28, "2007"] <- sum(IO_07[109:110, "GO"], na.rm = TRUE)
TO_re[29, "2007"] <- sum(IO_07[105:107, "GO"], na.rm = TRUE)
TO_re[30, "2007"] <- sum(IO_07[111:112, "GO"], na.rm = TRUE)
TO_re[31, "2007"] <- sum(IO_07[113, "GO"], na.rm = TRUE)
TO_re[32, "2007"] <- sum(IO_07[117:118, "GO"], na.rm = TRUE)
TO_re[33, "2007"] <- sum(IO_07[126:134, "GO"], na.rm = TRUE)
TO_re[34, "2007"] <- sum(IO_07[135, "GO"], na.rm = TRUE)

# 2010

IO_10 <- read_excel("IO表 02-20.xlsx",sheet = "2010",range = "C6:BF54")
IO_10 <- as.data.frame(IO_10)
rownames(IO_10) <- IO_10[[1]]
IO_10 <- IO_10[,-1]

TO_re[1:20, "2010"] <- IO_10[1:20, "GO"]
TO_re[21, "2010"] <- IO_10[21, "GO"]
TO_re[22:25, "2010"] <- IO_10[22:25, "GO"]
TO_re[26, "2010"] <- IO_10[29, "GO"]
TO_re[27, "2010"] <- IO_10[26, "GO"] + IO_10[27, "GO"]
TO_re[28, "2010"] <- IO_10[30, "GO"]
TO_re[29, "2010"] <- IO_10[28, "GO"]
TO_re[30:31, "2010"] <- IO_10[31:32, "GO"]
TO_re[32, "2010"] <- sum(IO_10[34:35, "GO"], na.rm = TRUE)
TO_re[33, "2010"] <- sum(IO_10[38:40, "GO"], na.rm = TRUE)
TO_re[34, "2010"] <- IO_10[41, "GO"]

# 2012

IO_12 <- read_excel("IO表 02-20.xlsx",sheet = "2012",range = "C4:EY150")
IO_12 <- as.data.frame(IO_12)
rownames(IO_12) <- IO_12[[1]]
IO_12 <- IO_12[,-1]
colnames(IO_12)[151] <- "GO"
colnames(IO_12)[152] <- "ERR"

TO_re["01", "2012"] <- sum(IO_12[grep("^(01|02|03|04|05)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["02", "2012"] <- sum(IO_12[grep("^(06)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["03", "2012"] <- sum(IO_12[grep("^(07)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["04", "2012"] <- sum(IO_12[grep("^(08|09)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["05", "2012"] <- sum(IO_12[grep("^(10)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["06", "2012"] <- sum(IO_12[grep("^(13|14|15|16)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["07", "2012"] <- sum(IO_12[grep("^(17)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["08", "2012"] <- sum(IO_12[grep("^(18|19)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["09", "2012"] <- sum(IO_12[grep("^(20|21)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["10", "2012"] <- sum(IO_12[grep("^(22|23|24)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["11", "2012"] <- sum(IO_12[grep("^(25)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["12", "2012"] <- sum(IO_12[grep("^(26|27|28|29)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["13", "2012"] <- sum(IO_12[grep("^(30)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["14", "2012"] <- sum(IO_12[grep("^(31|32)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["15", "2012"] <- sum(IO_12[grep("^(33)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["16", "2012"] <- sum(IO_12[grep("^(34|35)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["17", "2012"] <- sum(IO_12[grep("^(36|37)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["18", "2012"] <- sum(IO_12[grep("^(38)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["19", "2012"] <- sum(IO_12[grep("^(39)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["20", "2012"] <- sum(IO_12[grep("^(40)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["21", "2012"] <- sum(IO_12[grep("^(41|42)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["22", "2012"] <- sum(IO_12[grep("^(44)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["23", "2012"] <- sum(IO_12[grep("^(45)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["24", "2012"] <- sum(IO_12[grep("^(46)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["25", "2012"] <- sum(IO_12[grep("^(47|48|49|50)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["26", "2012"] <- sum(IO_12[grep("^(51)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["27", "2012"] <- sum(IO_12[grep("^(53|54|55|56|58|59|60)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["28", "2012"] <- sum(IO_12[grep("^(61|62)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["29", "2012"] <- sum(IO_12[grep("^(63|65)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["30", "2012"] <- sum(IO_12[grep("^(66|67|68)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["31", "2012"] <- sum(IO_12[grep("^(70)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["32", "2012"] <- sum(IO_12[grep("^(73|74)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["33", "2012"] <- sum(IO_12[grep("^(82|83|84|85|86|88|89|93)", rownames(IO_12)), "GO"], na.rm = TRUE)
TO_re["34", "2012"] <- sum(IO_12[grep("^(90)", rownames(IO_12)), "GO"], na.rm = TRUE)

# 2015

IO_15 <- read_excel("IO表 02-20.xlsx", sheet = "2015", range = "C8:BG57")
IO_15 <- as.data.frame(IO_15)
rownames(IO_15) <- IO_15[[1]]
IO_15 <- IO_15[, -1]

TO_re[1:15, "2015"] <- IO_15[1:15, "GO"]
TO_re[16, "2015"] <- IO_15[16, "GO"]+IO_15[17, "GO"]
TO_re[17:20, "2015"] <- IO_15[18:21, "GO"]
TO_re[21, "2015"] <- IO_15[22, "GO"]+IO_15[23, "GO"]
TO_re[22:31, "2015"] <- IO_15[25:34, "GO"]
TO_re[32, "2015"] <- IO_15[36, "GO"]
TO_re[33, "2015"] <- sum(IO_15[39:41, "GO"], na.rm = TRUE)
TO_re[34, "2015"] <- IO_15[42, "GO"]

# 2017

IO_17 <- read_excel("IO表 02-20.xlsx", sheet = "2017", range = "C6:FI162")
IO_17 <- as.data.frame(IO_17)
rownames(IO_17) <- IO_17[[1]]
IO_17 <- IO_17[, -1]

TO_re["01", "2017"] <- sum(IO_17[grep("^(01|02|03|04|05)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["02", "2017"] <- sum(IO_17[grep("^(06)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["03", "2017"] <- sum(IO_17[grep("^(07)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["04", "2017"] <- sum(IO_17[grep("^(08|09)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["05", "2017"] <- sum(IO_17[grep("^(10)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["06", "2017"] <- sum(IO_17[grep("^(13|14|15|16)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["07", "2017"] <- sum(IO_17[grep("^(17)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["08", "2017"] <- sum(IO_17[grep("^(18|19)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["09", "2017"] <- sum(IO_17[grep("^(20|21)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["10", "2017"] <- sum(IO_17[grep("^(22|23|24)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["11", "2017"] <- sum(IO_17[grep("^(25)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["12", "2017"] <- sum(IO_17[grep("^(26|27|28|29)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["13", "2017"] <- sum(IO_17[grep("^(30)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["14", "2017"] <- sum(IO_17[grep("^(31|32)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["15", "2017"] <- sum(IO_17[grep("^(33)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["16", "2017"] <- sum(IO_17[grep("^(34|35)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["17", "2017"] <- sum(IO_17[grep("^(36|37)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["18", "2017"] <- sum(IO_17[grep("^(38)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["19", "2017"] <- sum(IO_17[grep("^(39)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["20", "2017"] <- sum(IO_17[grep("^(40)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["21", "2017"] <- sum(IO_17[grep("^(41|42)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["22", "2017"] <- sum(IO_17[grep("^(44)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["23", "2017"] <- sum(IO_17[grep("^(45)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["24", "2017"] <- sum(IO_17[grep("^(46)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["25", "2017"] <- sum(IO_17[grep("^(47|48|49|50)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["26", "2017"] <- sum(IO_17[grep("^(51|52)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["27", "2017"] <- sum(IO_17[grep("^(53|54|55|56|58|59|60)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["28", "2017"] <- sum(IO_17[grep("^(61|62)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["29", "2017"] <- sum(IO_17[grep("^(63|64|65)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["30", "2017"] <- sum(IO_17[grep("^(66|67|68)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["31", "2017"] <- sum(IO_17[grep("^(70)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["32", "2017"] <- sum(IO_17[grep("^(73|74)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["33", "2017"] <- sum(IO_17[grep("^(83|84|85|86|88|91)", rownames(IO_17)), "GO"], na.rm = TRUE)
TO_re["34", "2017"] <- sum(IO_17[grep("^(91)", rownames(IO_17)), "GO"], na.rm = TRUE)

# 2018

IO_18 <- read_excel("IO表 02-20.xlsx", sheet = "2018", range = "C6:FM166")
IO_18 <- as.data.frame(IO_18)
rownames(IO_18) <- IO_18[[1]]
IO_18 <- IO_18[, -1]

TO_re["01", "2018"] <- sum(IO_18[grep("^(01|02|03|04|05)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["02", "2018"] <- sum(IO_18[grep("^(06)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["03", "2018"] <- sum(IO_18[grep("^(07)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["04", "2018"] <- sum(IO_18[grep("^(08|09)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["05", "2018"] <- sum(IO_18[grep("^(10)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["06", "2018"] <- sum(IO_18[grep("^(13|14|15|16)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["07", "2018"] <- sum(IO_18[grep("^(17)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["08", "2018"] <- sum(IO_18[grep("^(18|19)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["09", "2018"] <- sum(IO_18[grep("^(20|21)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["10", "2018"] <- sum(IO_18[grep("^(22|23|24)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["11", "2018"] <- sum(IO_18[grep("^(25)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["12", "2018"] <- sum(IO_18[grep("^(26|27|28|29)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["13", "2018"] <- sum(IO_18[grep("^(30)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["14", "2018"] <- sum(IO_18[grep("^(31|32)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["15", "2018"] <- sum(IO_18[grep("^(33)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["16", "2018"] <- sum(IO_18[grep("^(34|35)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["17", "2018"] <- sum(IO_18[grep("^(36|37)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["18", "2018"] <- sum(IO_18[grep("^(38)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["19", "2018"] <- sum(IO_18[grep("^(39)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["20", "2018"] <- sum(IO_18[grep("^(40)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["21", "2018"] <- sum(IO_18[grep("^(41|42)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["22", "2018"] <- sum(IO_18[grep("^(44)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["23", "2018"] <- sum(IO_18[grep("^(45)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["24", "2018"] <- sum(IO_18[grep("^(46)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["25", "2018"] <- sum(IO_18[grep("^(47|48|49|50)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["26", "2018"] <- sum(IO_18[grep("^(51|52)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["27", "2018"] <- sum(IO_18[grep("^(53|54|55|56|58|59|60)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["28", "2018"] <- sum(IO_18[grep("^(61|62)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["29", "2018"] <- sum(IO_18[grep("^(63|64|65)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["30", "2018"] <- sum(IO_18[grep("^(66|67|68)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["31", "2018"] <- sum(IO_18[grep("^(70)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["32", "2018"] <- sum(IO_18[grep("^(73|74)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["33", "2018"] <- sum(IO_18[grep("^(83|84|85|86|87|88|89|90|94)", rownames(IO_18)), "GO"], na.rm = TRUE)
TO_re["34", "2018"] <- sum(IO_18[grep("^(91)", rownames(IO_18)), "GO"], na.rm = TRUE)

# 2020

IO_20 <- read_excel("IO表 02-20.xlsx", sheet = "2020", range = "C6:FM166")
IO_20 <- as.data.frame(IO_20)
rownames(IO_20) <- IO_20[[1]]
IO_20 <- IO_20[, -1]

TO_re["01", "2020"] <- sum(IO_20[grep("^(01|02|03|04|05)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["02", "2020"] <- sum(IO_20[grep("^(06)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["03", "2020"] <- sum(IO_20[grep("^(07)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["04", "2020"] <- sum(IO_20[grep("^(08|09)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["05", "2020"] <- sum(IO_20[grep("^(10)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["06", "2020"] <- sum(IO_20[grep("^(13|14|15|16)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["07", "2020"] <- sum(IO_20[grep("^(17)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["08", "2020"] <- sum(IO_20[grep("^(18|19)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["09", "2020"] <- sum(IO_20[grep("^(20|21)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["10", "2020"] <- sum(IO_20[grep("^(22|23|24)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["11", "2020"] <- sum(IO_20[grep("^(25)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["12", "2020"] <- sum(IO_20[grep("^(26|27|28|29)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["13", "2020"] <- sum(IO_20[grep("^(30)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["14", "2020"] <- sum(IO_20[grep("^(31|32)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["15", "2020"] <- sum(IO_20[grep("^(33)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["16", "2020"] <- sum(IO_20[grep("^(34|35)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["17", "2020"] <- sum(IO_20[grep("^(36|37)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["18", "2020"] <- sum(IO_20[grep("^(38)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["19", "2020"] <- sum(IO_20[grep("^(39)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["20", "2020"] <- sum(IO_20[grep("^(40)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["21", "2020"] <- sum(IO_20[grep("^(41|42)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["22", "2020"] <- sum(IO_20[grep("^(44)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["23", "2020"] <- sum(IO_20[grep("^(45)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["24", "2020"] <- sum(IO_20[grep("^(46)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["25", "2020"] <- sum(IO_20[grep("^(47|48|49|50)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["26", "2020"] <- sum(IO_20[grep("^(51|52)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["27", "2020"] <- sum(IO_20[grep("^(53|54|55|56|58|59|60)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["28", "2020"] <- sum(IO_20[grep("^(61|62)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["29", "2020"] <- sum(IO_20[grep("^(63|64|65)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["30", "2020"] <- sum(IO_20[grep("^(66|67|68)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["31", "2020"] <- sum(IO_20[grep("^(70)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["32", "2020"] <- sum(IO_20[grep("^(73|74)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["33", "2020"] <- sum(IO_20[grep("^(83|84|85|86|87|88|89|90|94)", rownames(IO_20)), "GO"], na.rm = TRUE)
TO_re["34", "2020"] <- sum(IO_20[grep("^(91)", rownames(IO_20)), "GO"], na.rm = TRUE)

# ----------------------------------- #
### GO_yr  ----
# ----------------------------------- #

# agr

agr_output_data <- as.data.frame(read_excel("adhoc.xlsx",sheet="1a 总产出",range = "D4:AA5"))
rownames(agr_output_data) <- "01"

# industry

indu_income_data <- as.data.frame(read_excel("adhoc.xlsx",sheet="1a 总产出",range = "C56:AA80"))
rownames(indu_income_data) <- sprintf("%02d", 2:25)
indu_income_data <- indu_income_data[, -c(1,2)]

reference_years <- c(2002, 2005, 2005, 2007, 2010, 2010, 2012, 2015, 2017, 2018, 2020, 2020)
compilation_years <- c(2003, 2004, 2006, 2008, 2009, 2011, 2013, 2014, 2016, 2019, 2021, 2022)

new_cols <- as.character(compilation_years)
new_rows <- rownames(indu_income_data)
indu_output_data <- as.data.frame(matrix(NA, nrow = length(new_rows), ncol = length(new_cols), dimnames = list(new_rows, new_cols)))

for (j in seq_along(compilation_years)) {
  ref_year <- reference_years[j]
  comp_year <- compilation_years[j]
  
  for (i in seq_along(new_rows)) {
    row_name <- new_rows[i]
    indu_output_data[row_name, as.character(comp_year)] <- 
      indu_income_data[row_name, as.character(comp_year)] * 
      TO_re[row_name, as.character(ref_year)] / 
      indu_income_data[row_name, as.character(ref_year)]
  }
}

# archi

archi_output_adhoc <- as.data.frame(read_excel("adhoc.xlsx",sheet="1a 总产出",range = "C88:Y89"))
rownames(archi_output_adhoc) <- "26"

reference_years <- c(2002, 2005, 2005, 2007, 2010, 2010, 2012, 2015, 2017, 2018, 2020, 2020)
compilation_years <- c(2003, 2004, 2006, 2008, 2009, 2011, 2013, 2014, 2016, 2019, 2021, 2022)
new_cols <- as.character(compilation_years)
new_rows <- rownames(archi_output_adhoc)

archi_output_data <- as.data.frame(matrix(NA, nrow = length(new_rows), ncol = length(new_cols), dimnames = list(new_rows, new_cols)))

for (j in seq_along(compilation_years)) {
  ref_year <- reference_years[j]
  comp_year <- compilation_years[j]
  
  for (i in seq_along(new_rows)) {
    row_name <- new_rows[i]
    archi_output_data[row_name, as.character(comp_year)] <- 
      archi_output_adhoc[row_name, as.character(comp_year)] * 
      TO_re[row_name, as.character(ref_year)] / 
      archi_output_adhoc[row_name, as.character(ref_year)]
  }
}

# ----------------------------------- #
## 1b VA ----
# ----------------------------------- #
years <- 2003:2022
row_names <- sprintf("%02d", 1:34)
va <- data.frame(matrix(ncol = length(years), nrow = length(row_names)))
colnames(va) <- years
rownames(va) <- row_names

va_adhoc <- as.data.frame(read_excel("adhoc.xlsx",sheet="1b 增加值",range = "B25:T39"))
rownames(va_adhoc) <- va_adhoc[,1]
va_adhoc <- va_adhoc[,-1]
rownames(va_adhoc)[1] <- "01"
rownames(va_adhoc)[5:14] <- sprintf("%02d", 25:34)

for (code in rownames(va_adhoc)) {
  for (year in colnames(va_adhoc)) {
    if (code %in% rownames(va)) {
      va[code, year] <- va_adhoc[code, year]
    }
  }
}

v_bar_in <- va_adhoc[2:4,]
v_bar_in <- as.data.frame(v_bar_in)
rownames(v_bar_in)[1:3] <- c("Mining", "Manufacturing", "Utilities")

# C

C_20 <- as.data.frame(read_excel("adhoc.xlsx",sheet="1b 增加值",range = "H93:K128"))
rownames(C_20) <- C_20[,1]
C_20 <- C_20[,-1]

C_18 <- as.data.frame(read_excel("adhoc.xlsx",sheet="1b 增加值",range = "H176:K211"))
rownames(C_18) <- C_18[,1]
C_18 <- C_18[,-1]

reference_years <- c(2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2020, 2020)
compilation_years <- c(2004, 2006, 2008, 2009, 2011, 2013, 2014, 2016, 2019, 2021)

v_in <- data.frame(matrix(ncol = length(2003:2022), nrow = length(rownames(C_20))))
colnames(v_in) <- 2003:2022

for (i in 1:length(compilation_years)) {
  year <- compilation_years[i]
  reference_year <- reference_years[i]
  C <- get(paste0("C_", substr(reference_year, 3, 4)))
  
  v_bar_in_year <- v_bar_in[, as.character(year)]
  v_in_year <- as.vector(t(as.matrix(v_bar_in_year)) %*% t(C))
  v_in[, as.character(year)] <- v_in_year
}

fwrite(v_in,"v_in.csv")

va["02",] <- v_in[1,]
va["03",] <- v_in[2,]
va["04",] <- colSums(v_in[3:4,])
va["05",] <- v_in[5,]
va["06",] <- colSums(v_in[6:9,])
va["07",] <- v_in[10,]
va["08",] <- colSums(v_in[11:12,])
va["09",] <- v_in[13,]
va["10",] <- colSums(v_in[14:15,])
va["11",] <- v_in[16,]
va["12",] <- colSums(v_in[17:20,])
va["13",] <- v_in[21,]
va["14",] <- colSums(v_in[22:23,])
va["15",] <- v_in[24,]
va["16",] <- colSums(v_in[25:26,])
va["17",] <- colSums(v_in[27:28,])
va["18",] <- v_in[29,]
va["19",] <- v_in[30,]
va["20",] <- v_in[31,]
va["21",] <- v_in[32,]
va["22",] <- v_in[33,]
va["23",] <- v_in[34,]
va["24",] <- v_in[35,]

#-----------------------------------------------------------------------------#
# 2 初始投入初始矩阵 ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
## 2a va_rowsum ----
# ----------------------------------- #

va_rowsum <- as.data.frame(read_excel("adhoc.xlsx",sheet="2 初始投入",range = "C24:V28"))
rownames(va_rowsum) <- va_rowsum[,1]
va_rowsum <- va_rowsum[,-1]

# ----------------------------------- #
## 2b VA_re ----
# ----------------------------------- #

# 2002

VA_re_02 <- as.data.frame(matrix(0, nrow = 4, ncol = 34))
rownames(VA_re_02) <- c("VA001", "VA002", "VA003", "VA004")
colnames(VA_re_02) <- sprintf("%02d", 1:34)

for (va in c("VA001", "VA002", "VA003", "VA004")) {
  VA_re_02[va, "01"] <- sum(IO_02[va, grep("^(01|02|03|04|05)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "02"] <- sum(IO_02[va, grep("^(06)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "03"] <- sum(IO_02[va, grep("^(07)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "04"] <- sum(IO_02[va, grep("^(08|09)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "05"] <- sum(IO_02[va, grep("^(10)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "06"] <- sum(IO_02[va, grep("^(13|14|15|16)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "07"] <- sum(IO_02[va, grep("^(17)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "08"] <- sum(IO_02[va, grep("^(18|19)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "09"] <- sum(IO_02[va, grep("^(20|21)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "10"] <- sum(IO_02[va, grep("^(22|23|24)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "11"] <- sum(IO_02[va, grep("^(25)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "12"] <- sum(IO_02[va, grep("^(26|27|28|29|30)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "13"] <- sum(IO_02[va, grep("^(31)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "14"] <- sum(IO_02[va, grep("^(32|33)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "15"] <- sum(IO_02[va, grep("^(34)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "16"] <- sum(IO_02[va, grep("^(35|36)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "17"] <- sum(IO_02[va, grep("^(37)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "18"] <- sum(IO_02[va, grep("^(39)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "19"] <- sum(IO_02[va, grep("^(40)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "20"] <- sum(IO_02[va, grep("^(41)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "21"] <- sum(IO_02[va, grep("^(42|43)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "22"] <- sum(IO_02[va, grep("^(44)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "23"] <- sum(IO_02[va, grep("^(45)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "24"] <- sum(IO_02[va, grep("^(46)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "25"] <- sum(IO_02[va, grep("^(47)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "26"] <- sum(IO_02[va, grep("^(63)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "27"] <- sum(IO_02[va, grep("^(51|52|53|54|55|56|58|59)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "28"] <- sum(IO_02[va, grep("^(66|67)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "29"] <- sum(IO_02[va, grep("^(60|61)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "30"] <- sum(IO_02[va, grep("^(68|70)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "31"] <- sum(IO_02[va, grep("^(72)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "32"] <- sum(IO_02[va, grep("^(75|76)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "33"] <- sum(IO_02[va, grep("^(84|85|86|88|91|92)", colnames(IO_02))], na.rm = TRUE)
  VA_re_02[va, "34"] <- sum(IO_02[va, grep("^(93)", colnames(IO_02))], na.rm = TRUE)
}

# 2005

VA_re_05 <- as.data.frame(matrix(0, nrow = 4, ncol = 34))
rownames(VA_re_05) <- c("VA001", "VA002", "VA003", "VA004")
colnames(VA_re_05) <- sprintf("%02d", 1:34)

for (va in rownames(VA_re_05)) {
  VA_re_05[va, 1:20] <- IO_05[va, 1:20]
  VA_re_05[va, 21] <- IO_05[va, 21] + IO_05[va, 22]
  VA_re_05[va, 22:25] <- IO_05[va, 23:26]
  VA_re_05[va, 26] <- IO_05[va, 30]
  VA_re_05[va, 27] <- IO_05[va, 27] + IO_05[va, 28]
  VA_re_05[va, 28] <- IO_05[va, 31]
  VA_re_05[va, 29] <- IO_05[va, 29]
  VA_re_05[va, 30:31] <- IO_05[va, 32:33]
  VA_re_05[va, 32] <- sum(IO_05[va, 35:36], na.rm = TRUE)
  VA_re_05[va, 33] <- sum(IO_05[va, 39:41], na.rm = TRUE)
  VA_re_05[va, 34] <- IO_05[va, 42]
}

# 2007

VA_re_07 <- as.data.frame(matrix(0, nrow = 4, ncol = 34))
rownames(VA_re_07) <- c("VA001", "VA002", "VA003", "VA004")
colnames(VA_re_07) <- sprintf("%02d", 1:34)

for (va in rownames(VA_re_07)) {
  VA_re_07[va, "01"] <- sum(IO_07[va, 1:5], na.rm = TRUE)
  VA_re_07[va, "02"] <- IO_07[va, 6]
  VA_re_07[va, "03"] <- IO_07[va, 7]
  VA_re_07[va, "04"] <- sum(IO_07[va, 8:9], na.rm = TRUE)
  VA_re_07[va, "05"] <- IO_07[va, 10]
  VA_re_07[va, "06"] <- sum(IO_07[va, 11:24], na.rm = TRUE)
  VA_re_07[va, "07"] <- sum(IO_07[va, 25:29], na.rm = TRUE)
  VA_re_07[va, "08"] <- sum(IO_07[va, 30:31], na.rm = TRUE)
  VA_re_07[va, "09"] <- sum(IO_07[va, 32:33], na.rm = TRUE)
  VA_re_07[va, "10"] <- sum(IO_07[va, 34:36], na.rm = TRUE)
  VA_re_07[va, "11"] <- sum(IO_07[va, 37:38], na.rm = TRUE)
  VA_re_07[va, "12"] <- sum(IO_07[va, 39:49], na.rm = TRUE)
  VA_re_07[va, "13"] <- sum(IO_07[va, 50:56], na.rm = TRUE)
  VA_re_07[va, "14"] <- sum(IO_07[va, 57:62], na.rm = TRUE)
  VA_re_07[va, "15"] <- IO_07[va, 63]
  VA_re_07[va, "16"] <- sum(IO_07[va, 64:72], na.rm = TRUE)
  VA_re_07[va, "17"] <- sum(IO_07[va, 73:76], na.rm = TRUE)
  VA_re_07[va, "18"] <- sum(IO_07[va, 77:81], na.rm = TRUE)
  VA_re_07[va, "19"] <- sum(IO_07[va, 82:87], na.rm = TRUE)
  VA_re_07[va, "20"] <- IO_07[va, 88]
  VA_re_07[va, "21"] <- sum(IO_07[va, 89:91], na.rm = TRUE)
  VA_re_07[va, "22"] <- IO_07[va, 92]
  VA_re_07[va, "23"] <- IO_07[va, 93]
  VA_re_07[va, "24"] <- IO_07[va, 94]
  VA_re_07[va, "25"] <- IO_07[va, 95]
  VA_re_07[va, "26"] <- IO_07[va, 108]
  VA_re_07[va, "27"] <- sum(IO_07[va, 97:104], na.rm = TRUE)
  VA_re_07[va, "28"] <- sum(IO_07[va, 109:110], na.rm = TRUE)
  VA_re_07[va, "29"] <- sum(IO_07[va, 105:107], na.rm = TRUE)
  VA_re_07[va, "30"] <- sum(IO_07[va, 111:112], na.rm = TRUE)
  VA_re_07[va, "31"] <- IO_07[va, 113]
  VA_re_07[va, "32"] <- sum(IO_07[va, 117:118], na.rm = TRUE)
  VA_re_07[va, "33"] <- sum(IO_07[va, 126:134], na.rm = TRUE)
  VA_re_07[va, "34"] <- IO_07[va, 135]
}

# 2010

VA_re_10 <- as.data.frame(matrix(0, nrow = 4, ncol = 34))
rownames(VA_re_10) <- c("VA001", "VA002", "VA003", "VA004")
colnames(VA_re_10) <- sprintf("%02d", 1:34)

for (va in rownames(VA_re_10)) {
  VA_re_10[va, 1:20] <- IO_10[va, 1:20] 
  VA_re_10[va, 21] <- IO_10[va, 21]
  VA_re_10[va, 22:25] <- IO_10[va, 22:25]
  VA_re_10[va, 26] <- IO_10[va, 29]
  VA_re_10[va, 27] <- IO_10[va, 26] + IO_10[va, 27]
  VA_re_10[va, 28] <- IO_10[va, 30]
  VA_re_10[va, 29] <- IO_10[va, 28]
  VA_re_10[va, 30:31] <- IO_10[va, 31:32]
  VA_re_10[va, 32] <- sum(IO_10[va, 34:35], na.rm = TRUE)
  VA_re_10[va, 33] <- sum(IO_10[va, 38:40], na.rm = TRUE)
  VA_re_10[va, 34] <- IO_10[va, 41]
}

# 2012
VA_re_12 <- as.data.frame(matrix(0, nrow = 4, ncol = 34))
rownames(VA_re_12) <- c("VA001", "VA002", "VA003", "VA004")
colnames(VA_re_12) <- sprintf("%02d", 1:34)

for (va in c("VA001", "VA002", "VA003", "VA004")) {
  VA_re_12[va, "01"] <- sum(IO_12[va, grep("^(01|02|03|04|05)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "02"] <- sum(IO_12[va, grep("^(06)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "03"] <- sum(IO_12[va, grep("^(07)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "04"] <- sum(IO_12[va, grep("^(08|09)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "05"] <- sum(IO_12[va, grep("^(10)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "06"] <- sum(IO_12[va, grep("^(13|14|15|16)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "07"] <- sum(IO_12[va, grep("^(17)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "08"] <- sum(IO_12[va, grep("^(18|19)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "09"] <- sum(IO_12[va, grep("^(20|21)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "10"] <- sum(IO_12[va, grep("^(22|23|24)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "11"] <- sum(IO_12[va, grep("^(25)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "12"] <- sum(IO_12[va, grep("^(26|27|28|29)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "13"] <- sum(IO_12[va, grep("^(30)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "14"] <- sum(IO_12[va, grep("^(31|32)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "15"] <- sum(IO_12[va, grep("^(33)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "16"] <- sum(IO_12[va, grep("^(34|35)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "17"] <- sum(IO_12[va, grep("^(36|37)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "18"] <- sum(IO_12[va, grep("^(38)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "19"] <- sum(IO_12[va, grep("^(39)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "20"] <- sum(IO_12[va, grep("^(40)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "21"] <- sum(IO_12[va, grep("^(41|42)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "22"] <- sum(IO_12[va, grep("^(44)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "23"] <- sum(IO_12[va, grep("^(45)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "24"] <- sum(IO_12[va, grep("^(46)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "25"] <- sum(IO_12[va, grep("^(47|48|49|50)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "26"] <- sum(IO_12[va, grep("^(51)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "27"] <- sum(IO_12[va, grep("^(53|54|55|56|58|59|60)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "28"] <- sum(IO_12[va, grep("^(61|62)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "29"] <- sum(IO_12[va, grep("^(63|65)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "30"] <- sum(IO_12[va, grep("^(66|67|68)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "31"] <- sum(IO_12[va, grep("^(70)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "32"] <- sum(IO_12[va, grep("^(73|74)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "33"] <- sum(IO_12[va, grep("^(82|83|84|85|86|88|89|93)", colnames(IO_12))], na.rm = TRUE)
  VA_re_12[va, "34"] <- sum(IO_12[va, grep("^(90)", colnames(IO_12))], na.rm = TRUE)
}

# 2015

VA_re_15 <- as.data.frame(matrix(0, nrow = 4, ncol = 34))
rownames(VA_re_15) <- c("VA001", "VA002", "VA003", "VA004")
colnames(VA_re_15) <- sprintf("%02d", 1:34)

for (va in rownames(VA_re_15)) {
  VA_re_15[va, 1:15] <- IO_15[va, 1:15]
  VA_re_15[va, 16] <- IO_15[va, 16] + IO_15[va, 17]
  VA_re_15[va, 17:20] <- IO_15[va, 18:21]
  VA_re_15[va, 21] <- IO_15[va, 22] + IO_15[va, 23]
  VA_re_15[va, 22:31] <- IO_15[va, 25:34]
  VA_re_15[va, 32] <- IO_15[va, 36]
  VA_re_15[va, 33] <- sum(IO_15[va, 39:41], na.rm = TRUE)
  VA_re_15[va, 34] <- IO_15[va, 42]
}

# 2017

VA_re_17 <- as.data.frame(matrix(0, nrow = 4, ncol = 34))
rownames(VA_re_17) <- c("VA001", "VA002", "VA003", "VA004")
colnames(VA_re_17) <- sprintf("%02d", 1:34)

for (va in rownames(VA_re_17)) {
  VA_re_17[va, "01"] <- sum(IO_17[va, grep("^(01|02|03|04|05)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "02"] <- sum(IO_17[va, grep("^(06)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "03"] <- sum(IO_17[va, grep("^(07)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "04"] <- sum(IO_17[va, grep("^(08|09)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "05"] <- sum(IO_17[va, grep("^(10)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "06"] <- sum(IO_17[va, grep("^(13|14|15|16)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "07"] <- sum(IO_17[va, grep("^(17)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "08"] <- sum(IO_17[va, grep("^(18|19)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "09"] <- sum(IO_17[va, grep("^(20|21)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "10"] <- sum(IO_17[va, grep("^(22|23|24)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "11"] <- sum(IO_17[va, grep("^(25)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "12"] <- sum(IO_17[va, grep("^(26|27|28|29)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "13"] <- sum(IO_17[va, grep("^(30)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "14"] <- sum(IO_17[va, grep("^(31|32)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "15"] <- sum(IO_17[va, grep("^(33)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "16"] <- sum(IO_17[va, grep("^(34|35)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "17"] <- sum(IO_17[va, grep("^(36|37)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "18"] <- sum(IO_17[va, grep("^(38)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "19"] <- sum(IO_17[va, grep("^(39)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "20"] <- sum(IO_17[va, grep("^(40)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "21"] <- sum(IO_17[va, grep("^(41|42)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "22"] <- sum(IO_17[va, grep("^(44)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "23"] <- sum(IO_17[va, grep("^(45)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "24"] <- sum(IO_17[va, grep("^(46)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "25"] <- sum(IO_17[va, grep("^(47|48|49|50)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "26"] <- sum(IO_17[va, grep("^(51|52)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "27"] <- sum(IO_17[va, grep("^(53|54|55|56|58|59|60)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "28"] <- sum(IO_17[va, grep("^(61|62)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "29"] <- sum(IO_17[va, grep("^(63|64|65)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "30"] <- sum(IO_17[va, grep("^(66|67|68)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "31"] <- sum(IO_17[va, grep("^(70)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "32"] <- sum(IO_17[va, grep("^(73|74)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "33"] <- sum(IO_17[va, grep("^(83|84|85|86|88|91)", rownames(IO_17))], na.rm = TRUE)
  VA_re_17[va, "34"] <- sum(IO_17[va, grep("^(91)", rownames(IO_17))], na.rm = TRUE)
}

# 2018

VA_re_18 <- as.data.frame(matrix(0, nrow = 4, ncol = 34))
rownames(VA_re_18) <- c("VA001", "VA002", "VA003", "VA004")
colnames(VA_re_18) <- sprintf("%02d", 1:34)

for (va in rownames(VA_re_18)) {
  VA_re_18[va, "01"] <- sum(IO_18[va, grep("^(01|02|03|04|05)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "02"] <- sum(IO_18[va, grep("^(06)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "03"] <- sum(IO_18[va, grep("^(07)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "04"] <- sum(IO_18[va, grep("^(08|09)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "05"] <- sum(IO_18[va, grep("^(10)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "06"] <- sum(IO_18[va, grep("^(13|14|15|16)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "07"] <- sum(IO_18[va, grep("^(17)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "08"] <- sum(IO_18[va, grep("^(18|19)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "09"] <- sum(IO_18[va, grep("^(20|21)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "10"] <- sum(IO_18[va, grep("^(22|23|24)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "11"] <- sum(IO_18[va, grep("^(25)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "12"] <- sum(IO_18[va, grep("^(26|27|28|29)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "13"] <- sum(IO_18[va, grep("^(30)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "14"] <- sum(IO_18[va, grep("^(31|32)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "15"] <- sum(IO_18[va, grep("^(33)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "16"] <- sum(IO_18[va, grep("^(34|35)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "17"] <- sum(IO_18[va, grep("^(36|37)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "18"] <- sum(IO_18[va, grep("^(38)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "19"] <- sum(IO_18[va, grep("^(39)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "20"] <- sum(IO_18[va, grep("^(40)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "21"] <- sum(IO_18[va, grep("^(41|42)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "22"] <- sum(IO_18[va, grep("^(44)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "23"] <- sum(IO_18[va, grep("^(45)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "24"] <- sum(IO_18[va, grep("^(46)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "25"] <- sum(IO_18[va, grep("^(47|48|49|50)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "26"] <- sum(IO_18[va, grep("^(51|52)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "27"] <- sum(IO_18[va, grep("^(53|54|55|56|58|59|60)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "28"] <- sum(IO_18[va, grep("^(61|62)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "29"] <- sum(IO_18[va, grep("^(63|64|65)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "30"] <- sum(IO_18[va, grep("^(66|67|68)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "31"] <- sum(IO_18[va, grep("^(70)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "32"] <- sum(IO_18[va, grep("^(73|74)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "33"] <- sum(IO_18[va, grep("^(83|84|85|86|87|88|89|90|94)", rownames(IO_18))], na.rm = TRUE)
  VA_re_18[va, "34"] <- sum(IO_18[va, grep("^(91)", rownames(IO_18))], na.rm = TRUE)
}

# 2020

VA_re_20 <- as.data.frame(matrix(0, nrow = 4, ncol = 34))
rownames(VA_re_20) <- c("VA001", "VA002", "VA003", "VA004")
colnames(VA_re_20) <- sprintf("%02d", 1:34)

for (va in rownames(VA_re_20)) {
  VA_re_20[va, "01"] <- sum(IO_20[va, grep("^(01|02|03|04|05)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "02"] <- sum(IO_20[va, grep("^(06)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "03"] <- sum(IO_20[va, grep("^(07)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "04"] <- sum(IO_20[va, grep("^(08|09)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "05"] <- sum(IO_20[va, grep("^(10)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "06"] <- sum(IO_20[va, grep("^(13|14|15|16)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "07"] <- sum(IO_20[va, grep("^(17)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "08"] <- sum(IO_20[va, grep("^(18|19)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "09"] <- sum(IO_20[va, grep("^(20|21)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "10"] <- sum(IO_20[va, grep("^(22|23|24)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "11"] <- sum(IO_20[va, grep("^(25)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "12"] <- sum(IO_20[va, grep("^(26|27|28|29)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "13"] <- sum(IO_20[va, grep("^(30)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "14"] <- sum(IO_20[va, grep("^(31|32)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "15"] <- sum(IO_20[va, grep("^(33)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "16"] <- sum(IO_20[va, grep("^(34|35)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "17"] <- sum(IO_20[va, grep("^(36|37)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "18"] <- sum(IO_20[va, grep("^(38)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "19"] <- sum(IO_20[va, grep("^(39)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "20"] <- sum(IO_20[va, grep("^(40)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "21"] <- sum(IO_20[va, grep("^(41|42)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "22"] <- sum(IO_20[va, grep("^(44)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "23"] <- sum(IO_20[va, grep("^(45)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "24"] <- sum(IO_20[va, grep("^(46)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "25"] <- sum(IO_20[va, grep("^(47|48|49|50)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "26"] <- sum(IO_20[va, grep("^(51|52)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "27"] <- sum(IO_20[va, grep("^(53|54|55|56|58|59|60)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "28"] <- sum(IO_20[va, grep("^(61|62)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "29"] <- sum(IO_20[va, grep("^(63|64|65)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "30"] <- sum(IO_20[va, grep("^(66|67|68)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "31"] <- sum(IO_20[va, grep("^(70)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "32"] <- sum(IO_20[va, grep("^(73|74)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "33"] <- sum(IO_20[va, grep("^(83|84|85|86|87|88|89|90|94)", rownames(IO_20))], na.rm = TRUE)
  VA_re_20[va, "34"] <- sum(IO_20[va, grep("^(91)", rownames(IO_20))], na.rm = TRUE)
}

# ----------------------------------- #
## 2c min ----
# ----------------------------------- #

# ----------------------------------- #
### VA_0  ----
# ----------------------------------- #

# 1b需要重新跑一下

reference_years <- c(2005, 2005, 2007, 2010, 2010, 2012, 2015, 2017, 2018, 2020)
compilation_years <- c(2004, 2006, 2008, 2009, 2011, 2013, 2014, 2016, 2019, 2021)

va_0_list <- list()

for (k in 1:length(compilation_years)) {
  
  year <- compilation_years[k]
  reference_year <- reference_years[k]
  va_re <- get(paste0("VA_re_", substr(reference_year, 3, 4)))
  va_0 <- as.data.frame(matrix(0, nrow = 4, ncol = 34))
  rownames(va_0) <- c("VA001", "VA002", "VA003", "VA004")
  colnames(va_0) <- sprintf("%02d", 1:34)

  for (j in 1:34){
    v_bar_j <- sum(va_re[1:4,j])
    v_j <- va[j,as.character(year)]
    for (i in 1:4){
      va_0[i,j] <- v_j * va_re[i,j] / v_bar_j
    }
  }

  va_0_list[[paste0("va_0_", substr(year, 3, 4))]] <- va_0
}


# ----------------------------------- #
### GRAS  ----
# ----------------------------------- #

# Define the GRAS function
GRAS <- function(A, u, v, iteration_maximum = 3000, accuracy = 1e-6) {
  # Define the P and N matrices
  P <- matrix(0, nrow = nrow(A), ncol = ncol(A))
  N <- matrix(0, nrow = nrow(A), ncol = ncol(A))
  P[A > 0] <- A[A > 0]
  N[A < 0] <- -A[A < 0]
  
  # Define u_star and v_star
  e <- exp(1)
  u_star <- e * u
  v_star <- e * v
  
  # Initialize R and S as diagonal matrices
  m <- nrow(A)
  n <- ncol(A)
  R <- diag(1, m, m)
  S <- diag(NA, n, n)  # Initialize S as a diagonal matrix

  # Define functions for updating r_i and s_j
  rho_i <- function(s, u_star_i, p_i, n_i) {
    (u_star_i + sqrt(u_star_i^2 + 4 * p_i * n_i)) / (2 * p_i)
  }
  
  sigma_j <- function(r, v_star_j, p_j, n_j) {
    (v_star_j + sqrt(v_star_j^2 + 4 * p_j * n_j)) / (2 * p_j)
  }
  
  # Iteration
  for (iteration in 1:iteration_maximum) {
    # Solve for S
    for (j in 1:n) {
      p_j <- sum(P[, j] * diag(R))
      n_j <- sum(N[, j] / diag(R))
      S[j, j] <- sigma_j(diag(R), v_star[j], p_j, n_j)
    }
    
    # Solve for R
    for (i in 1:m) {
      p_i <- sum(P[i, ] * diag(S))
      n_i <- sum(N[i, ] / diag(S))
      R[i, i] <- rho_i(diag(S), u_star[i], p_i, n_i)
    }
    
    # Check for convergence
    RS <- R %*% P %*% S - solve(R) %*% N %*% solve(S)
    error_u <- norm(RS %*% rep(1, n) - u_star, "F") / norm(u_star, "2")
    error_v <- norm(rep(1, m) %*% RS - v_star, "2") / norm(v_star, "2")
    if (error_u < accuracy && error_v < accuracy) {
      break
    }
  }
  
  # Calculate the updated matrix X
  X <- matrix(0, nrow = m, ncol = n)
  r <- diag(R)
  s <- diag(S)
  
  for (i in 1:m) {
    for (j in 1:n) {
      if (A[i, j] >= 0) {
        X[i, j] <- r[i] * A[i, j] * s[j] / e
      } else {
        X[i, j] <- (1 / r[i]) * A[i, j] * (1 / s[j]) / e
      }
    }
  }
  
  return(X)
}

Z <- GRAS(A = Z_0,u = r,v = s)

reference_years <- c(2005, 2005, 2007, 2010, 2010, 2012, 2015, 2017, 2018, 2020)
compilation_years <- c(2004, 2006, 2008, 2009, 2011, 2013, 2014, 2016, 2019, 2021)

va_list <- list()

for (i in 1:length(compilation_years)) {
  comp_year <- compilation_years[i]
  
  A <- va_0_list[[paste0("va_0_", substr(comp_year, 3, 4))]]
  u <- va_rowsum[, as.character(comp_year)] # rowsum
  v <- va[, as.character(comp_year)] # colsum
  
  X <- GRAS(A, u, v)
  va_list[[paste0("va_", substr(comp_year, 3, 4))]] <- X
}

print(va_list[["va_13"]])
