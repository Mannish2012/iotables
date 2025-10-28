
get_data <- function(linkage_type, data_type){
  
  year = 2022
  column_names <- names(readxl::read_excel(.path, range = paste0(year,"!D5:AO5")))
  industry_names <- column_names[1:35]
  gross_consump_names <- column_names[36:38]
  
  int_range <- switch(linkage_type,
                      "backward" = "!A8:AL42",
                      "forward" = "!A43:AL77")
  
  gross_range <- switch(linkage_type,
                        "backward" = "!AM8:AO42",
                        "forward" = "!AM8:AO42")
  
  intermediate <- as.data.frame(
    readxl::read_excel(.path, range = paste0(year, int_range),
                       col_names = FALSE)[,-c(1:3)])
  

  dimnames(intermediate) <- list(industry_names, industry_names)
  
  # which variables have ONLY zero values?
  determine_zero_vars <- Map(function(var) all(var == 0), intermediate)
  zero_vars <- which(unname(unlist(determine_zero_vars)))
  
  intermediate <- intermediate[-zero_vars, -zero_vars]
  industry_names <- colnames(intermediate)
  

  gross <- as.data.frame(
    readxl::read_excel(.path,
                       range = paste0(year, gross_range),
                       col_names = FALSE))[-zero_vars,]
  
  dimnames(gross) <- list(industry_names, gross_consump_names)
  
  
  gross_sum <- switch(linkage_type,
                      "backward" = colSums(gross),
                      "forward" = rowSums(gross))
  
  direct <- intermediate/gross_sum # A/B matrix 
  total <- solve(diag(ncol(direct)) - direct) # Leontief/Ghosh inverse matrix
  
  switch(linkage_type,
         "backward" = colSums(direct),
         "forward" = colSums(total))
}

get_data("backward", "total")
