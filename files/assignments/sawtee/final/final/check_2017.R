# .path <- "~/data/institutions/adb/nep_iot.xlsx"
# sapply(1:4, function(x) M[,x]/colSums(M)[x])
library(dplyr)

get_data <- function(year){
  
  O <- as.data.frame(readxl::read_excel(.path, range = 
                        paste0(year, "!A8:AS42"),
                        col_names = FALSE))
  
  column_names <- names(readxl::read_excel(.path,
                          range = paste0(year,"!D5:AS5")))
  industry_names <- column_names[1:35]
  consumption_names <- column_names[36:38]
  
  M <- O[,4:(ncol(O)-4)] # M: 35 X 38

  dimnames(M) <- list(industry_names,c(industry_names,consumption_names))

  Z <- M[,c(1:35)] # unscaled tech coefficients 35 X 35
  
  # which variables have ONLY zero values?
  determine_zero_vars <- Map(function(var) all(var == 0), Z)
  zero_vars <- which(unname(unlist(determine_zero_vars)))
  
  # remove the unnecessary variables from the M matrix and
  ## change industry_names variable to reflect that. New matrix is 33 X 33
  Z <- Z[-zero_vars, -zero_vars]
  industry_names <- colnames(Z)
  
  .T <- readxl::read_excel(
    .path, range = paste0(year, "!D86:AL86"),col_names = F)[-zero_vars]

  
  C <- M[-zero_vars,c(36:38)] # consumption matrix
  fC <- M[-zero_vars,]
  
  cT <- rowSums(Z)
  
  A <-  sweep(Z,2,cT,"/")
  
  cS <- colSums(A)
  
  # data.frame(var = names(cS), val = unname(cS) |> round(3))
  
  d <- as.numeric(.T); names(d) <- industry_names
  
  A <- sweep(Z,2,d,"/")
  
  data.frame(var = names(colSums(A)), val = unname(colSums(A)))

};get_data(2022) |> View()




