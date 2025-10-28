
# source("files/assignments/sawtee/final/read_data.R")

forwardLinkage <- function(year, intermediate = "true"){
  
  intermediate_mat <- read_data(year = year, linkage_type = "forward",
                                output = "data")
  
  final_dat <- readxl::read_excel(.path, range = paste0(year, "!AS43:AS77"), 
                                  col_names = FALSE)
  
  unnecessary_vars <- read_data(year = year, linkage_type = "forward",
                                output = "vars")
  
  
  final_mat <- matrix(unname(unlist(final_dat)),
                      nrow = 1,
                      dimnames = list(NULL, industry_names))
  
  
  ind_unnecessary <- which(dimnames(final_mat)[[2]] %in% unnecessary_vars)
  
  total_mat <- final_mat[,-ind_unnecessary] |> 
    matrix(nrow = 1, dimnames = list(NULL, industry_names_abb))
  
  # total_mat <- unlist(unname(total_mat)) |> 
  #   matrix(nrow = 33, dimnames = list(NULL, industry_names_abb))
  # total_mat
  
  switch(intermediate,
         true = intermediate_mat,
         false = total_mat)
  
  
  
}
# forwardLinkage(2019, intermediate = "true") |> dim()


iom_nepal <- function(year){
  
  fio::iom$new(
    "nepal_adb",
    intermediate_transactions = forwardLinkage(year,"true"),
    total_production = forwardLinkage(year,"false"))
}

fl_2019 <- iom_nepal(2019)
