
library(readxl)

read_province_data <- function(year) {

  year_dir <- file.path(paste0(base_dir,sprintf("%d年各省份投入产出表", year)))
  file_list <- list.files(year_dir, full.names = TRUE, pattern = "\\.xls$")
  
  data_list <- list()
  coeff_list <- list()
  
  for (file_path in file_list) {
    
    file_name <- basename(file_path)
    province_index <- as.numeric(sub("^(\\d+).*$", "\\1", file_name))
    
    data <- read_excel(file_path, range = "C8:BG57")
    data <- as.data.frame(data)
    rownames(data) <- data[[1]]
    data <- data[, -1]
    
    coeff <- matrix(NA, ncol = 2, nrow = 44)
    colnames(coeff_data) <- c("TU","TI")
    for (i in 1:42){
      coeff[i,1] <- data[i,"TIU"] / data["TI","TIU"]
      coeff[i,2] <- data["TII",i] / data["TII","GO"]
    }
    coeff[43,1] <- data["VA001","TIU"] / data["TI","TIU"]
    coeff[43,2] <- 0
    coeff[44,1] <- (data["TVA","TIU"] - data["VA001","TIU"]) / data["TI","TIU"]
    coeff[44,2] <- data["TII","TFU"] / data["TII","GO"]
    
    data_list[[paste0("IO_", sprintf("%02d", province_index), "_", year)]] <- data
    coeff_list[[paste0("Coeff_", sprintf("%02d", province_index), "_", year)]] <- coeff
  }
  
  return(IO = data_list, Coeff = coeff_list)
}

years <- c(2002, 2007, 2012, 2015, 2017)

all_data <- list()

for (year in years) {
  
  base_dir <- "2002-2017分省投入产出表"
  IO_data <- read_province_data(base_dir, year)$IO
  Coeff_data <- read_province_data(base_dir, year)$Coeff
  all_data[[paste0("IO_province_", year)]] <- IO_data
  all_data[[paste0("Coeff_province_", year)]] <- Coeff_data
  
}

