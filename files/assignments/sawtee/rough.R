
industry_names_abb <- readRDS("files/assignments/sawtee/industry_names.RDS")

backward_linkage <- function(year, type){
  
  # this dataframe is 35 X 38
  dat <- readxl::read_excel(.path, 
                            range = paste0(year,"!A8:AL42"),
                            col_names = FALSE) 
  
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
  
  # remove the associated industries with all zero values from the list
  ## of industry names
  industry_names <- industry_names[-unnecessary_rows]
  # determine the number of usable industries
  len <- length(industry_names)
  
  # Now create a data frame of intermediate values

  temp_df <- as.data.frame(dat)
  colnames(temp_df) <- industry_names2
  # dimnames(temp_df) <- list(industry_names2, industry_names2)

  # create a temporary named list to store variable values
  temp_var_list <- lapply(names(temp_df), function(ind){
    temp_df[ind]/sum(temp_df[ind])
  }) |> setNames(industry_names_abb)
 
  # create a character data structure to index temp_var_list
  temp_var_index <- paste0("cbind(",
              paste0(sprintf("temp_var_list[[%i]]", 1:len), 
                     collapse = ","),")")

  input_coeff_matrix <- as.matrix(eval(str2expression(temp_var_index)))
  id_matrix <- diag(input_coeff_matrix) # identity matrix of order 33
  # inverse of difference: Leontief inverse
  leontief_inv_matrix <- solve(id_matrix - input_coeff_matrix) 
  # 

  switch(type,
         original = temp_df,
         direct =  input_coeff_matrix,
         total = leontief_inv_matrix)

  
}; coef_matrix2(2019, "direct") 