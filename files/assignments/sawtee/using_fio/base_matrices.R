
.path <- "~/data/institutions/adb/nep_iot.xlsx"
dat_dir <- "C:/users/hello/rprojects/upwork"

industry_names <- readRDS(paste0(dat_dir,"/data/institutes/adb/nepal_iot.RDS"))


base_matrix <- function(year, type){
  
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

}

iom_nepal <- function(year){
  
  fio::iom$new(
    "nepal_adb",
    intermediate_transactions = base_matrix(year, "intermediate"),
    total_production = base_matrix(year, "totalprod"))
}
  
bl <- lapply(2010:2020, function(x) iom_nepal(x))


