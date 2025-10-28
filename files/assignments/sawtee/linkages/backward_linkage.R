
industry_names <- readRDS(
  "files/assignments/sawtee/data/original_industry_names.RDS")
industry_names_abb <- readRDS(
  "files/assignments/sawtee/data/industry_names.RDS")

backward_linkage <- function(year, type){
  
  # this dataframe is 35 X 38
  dat <- readxl::read_excel(.path, 
                            range = paste0(year,"!A8:AL42"),
                            col_names = FALSE) |>
    as.data.frame()
  
  # find columns in which entries are all 0 and store those as a numeric vector
  allzero_cols <- Map(function(x) all(x == 0), dat) |> 
    unlist() |> unname() |> which() 
  
  # list all columns we want to remove
  unnecessary_cols <- c(1:3, allzero_cols)
  # then remove them
  
  # list all the rows we want to remove. First, find which variables we want
  ## to remove. We want to remove the variables that have all 0 values
  unnecessary_vars <- unlist(industry_names[allzero_cols - 3])
  unnecessary_rows <- unname(sapply(unnecessary_vars, function(x){
    which(x == industry_names)
  }))
  
  # This dataset is 33 X 33. 
  dat <- dat[-unnecessary_rows,-unnecessary_cols]
  dimnames(dat) <- list(industry_names_abb, industry_names_abb)
  
  # remove the associated industries with all zero values from the list
  ## of industry names
  industry_names <- industry_names[-unnecessary_rows]
  # determine the number of usable industries
  len <- length(industry_names)
  
  # Now create a data frame of intermediate values
  # create a temporary named list to store variable values
  temp_var_list <- lapply(names(dat), function(ind){
    dat[ind]/sum(dat[ind])
  }) |> setNames(industry_names_abb)
 
  # create a character data structure to index temp_var_list
  temp_var_index <- paste0("cbind(",
              paste0(sprintf("temp_var_list[[%i]]", 1:len), 
                     collapse = ","),")")

  input_coeff_matrix <- as.matrix(eval(str2expression(temp_var_index)))
  
  id_matrix <- diag(nrow(input_coeff_matrix)) # identity matrix of order 33
  # inverse of difference: Leontief inverse
  ## by multiplying the coefficients matrix by 0.99, we ensure that 
  ## we obtain an inverse, but our values are similar to the original
  leontief_inv_matrix <- solve(id_matrix - input_coeff_matrix*0.99)
  
  # Normalized linkages
  # normalized direct linkage
  sector_means_direct <- rowMeans(input_coeff_matrix)
  normalized_direct_linkages <- input_coeff_matrix/sector_means_direct
  
  # normalized total linkage
  sector_means_total <- rowMeans(leontief_inv_matrix)
  normalized_total_linkages <- leontief_inv_matrix/sector_means_total

  switch(type,
         original = dat,
         direct =  input_coeff_matrix,
         total = leontief_inv_matrix,
         normalized_direct = normalized_direct_linkages,
         normalized_total = normalized_total_linkages)
  

}; backward_linkage(2019, "normalized_total") |> View()
