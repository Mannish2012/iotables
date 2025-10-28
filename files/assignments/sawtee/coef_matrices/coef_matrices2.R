
industry_names2 <- c("agriculture","mining",'food','textiles','leather',
                     'wood','pulp','coke','chemicals','rubber','other nonmetallic',
                     'metals','machinery','electrical','transport','manufacturing',
                     'utilities','construction','transport maintainance',
                     'wholesale trade','retail trade','hospitality','inland transport',
                     'water transport','air transport','other travel','telecom',
                     'finance','real estate','renting','public admin','education',
                     'health','community work','private HH')


coef_matrix2 <- function(year, scaled){
  
  # this dataframe is 35 X 38
  intermediate_dat <- readxl::read_excel(.path, 
                                         range = paste0(year,"!A8:AL42"),
                                         col_names = FALSE) 
  
  # find columns in which entries are all 0 and store those as a numeric vector
  allzero_cols <- Map(function(x) all(x == 0), intermediate_dat) |> 
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
  intermediate_dat <- intermediate_dat[-unnecessary_rows,-unnecessary_cols]
  
  # remove the associated industries with all zero values from the list
  ## of industry names
  industry_names <- industry_names[-unnecessary_rows]
  # determine the number of useable industries
  len <- length(industry_names)
  
  
  # Now create the MATRIX of intermediate values
  intermediate_mat <- matrix(unname(unlist(intermediate_dat)),
                             nrow = len,
                             dimnames = list(paste0("A",1:33),
                                             paste0("A",1:33)))
  
  # Scale the matrix
  
  switch(scaled,
         true =  as.data.frame(colSums(intermediate_mat)),
         false = intermediate_mat)

}; View(round(coef_matrix2(2019, "false"),2))


inverse_matrix2 <- function(year){
  
  m <- coef_matrix2(year = year)
  solve(diag(nrow(m))-m)
  
  # diag(nrow(m)) - (m^10)
  
}


backward_linkage2 <- function(year, method){
  if(method == "direct"){
    colSums(coeff_matrix2(year = year))
  } else{
    if(method == "total"){
      colSums(inverse_matrix2(year = year))
    }
  }
}