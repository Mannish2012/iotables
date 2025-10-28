
.path <- "~/data/institutions/adb/nep_iot.xlsx"
dat_dir <- "C:/users/hello/rprojects/upwork"

industry_names <- readRDS(paste0(dat_dir,"/data/institutes/adb/nepal_iot.RDS"))


coeff_matrix <- function(year, type){
  
  if(type == "backward"){
    m <- readxl::read_excel(.path, range = paste0(year,"!A8:AL42"), 
                    col_names = FALSE)
  } else if(type == "forward"){
    m <- readxl::read_excel(.path, range = paste0(year,"!A43:AL77"), 
                    col_names = FALSE)
  }
  
  
  m <- select(m,-c(2,3)) # remove irrelevant columns
  m <- unname(unlist(m[,-1])) # remove names and convert into a list of numbers only
  
  matrix(m, nrow = 35, dimnames = list(industry_names,industry_names))
  
}


# determine and remove all zero columns

all_zero <- function(year, type){
  
  # this dataframe is 35 X 38
  intermediate_dat <- readxl::read_excel(.path, 
                                         range = paste0(year,"!A8:AL42"),
                                         col_names = FALSE) 
  
  total.prod_dat <- readxl::read_excel(.path, 
                                       range = paste0(year, "!AS8:AS42"),
                                       col_names = FALSE)
  
  # find columns in which entries are all 0 and store those as a numeric vector
  allzero_cols <- Map(function(x) all(x == 0), intermediate_dat) |> 
    unlist() |> unname() |> which() 
  
  # list all columns we want to remove
  unnecessary_cols <- c(1:3, allzero_cols)
  # then remove them
  
  # list all the rows we want to remove. First, find which variables we want
  ## to remove
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
                             dimnames = list(industry_names,industry_names))

  # Similarly create a matrix of total production values
  total.prod_mat <- matrix(unname(unlist(total.prod_dat[-unnecessary_rows,])),
                           nrow = 1,
                           dimnames = list(NULL, industry_names))
  
  switch(type,
         intermediate = intermediate_mat,
         totalprod = total.prod_mat)
  
  
  

}; all_zero(2019, "totalprod")



mat2 <- function(year, type){
  
  intermediate_dat <- readxl::read_excel(.path, range = paste0(year,"!A8:AL42"),
                                    col_names = FALSE) 
  
  # find columns in which entries are all 0
  ind <- Map(function(x) !all(x == 0), intermediate_dat) |> 
    unlist() |> unname() |> which() 
  # then remove the all-zero columns from the data
  intermediate_dat <- Filter(function(x) !all(x == 0), intermediate_dat)
  # remove the zero-index variables from industry names
  industry_names <- industry_names[-ind]

  intermediate_mat <- matrix(unname(unlist(intermediate_dat[,-c(1:3)])),
                             nrow = 33,
                             dimnames = list(industry_names,industry_names))

  final_dat <- readxl::read_excel(.path, range = paste0(year, "!AS8:AS42"),
                          col_names = FALSE)

  final_mat <- matrix(unname(unlist(final_dat)),
                      nrow = 1,
                      dimnames = list(NULL, industry_names))

  switch(type,
         n = length(industry_names),
         intermediate = intermediate_mat,
         final = final_mat)
    
}; mat2(2019,"intermediate")



iom_nepal <- function(year){
  
  fio::iom$new(
    "nepal_adb",
    intermediate_transactions = mat2(year,"intermediate"),
    total_production = mat2(year,"final"))
}
  
bl <- iom_nepal(2017)


# Let's compare

p1 <- d$intermediate_transactions; p2 <- d$total_production
p3 <- fio::iom$new("check",
                   intermediate_transactions = p1,
                   total_production = p2)

p3$compute_tech_coeff()$compute_leontief_inverse()$leontief_inverse_matrix


bl <- iom_nepal(2017)
q1 <- bl$intermediate_transactions; q2 <- bl$total_production
q3 <- fio::iom$new("check2",
                   intermediate_transactions = q1,
                   total_production = q2)


q3$compute_tech_coeff()$compute_leontief_inverse()$leontief_inverse_matrix


.dat <- fio::import_element(file = .path,
                            sheet = 4,
                            range = "A8:AL42")



