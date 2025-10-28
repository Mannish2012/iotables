
# set directory paths to retrieve stored data or files
dat_dir <- "C:/users/hello/rprojects/upwork"

# The following code snippet transforms data set into matrix form and
## finds the necessary inverse

# get the names of the industry (previously stored)
industry_names <- readRDS(paste0(dat_dir,"/data/institutes/adb/nepal_iot.RDS"))

# Computing forward linkages
.path <- "~/data/institutions/adb/nep_iot.xlsx"

# Get the coefficients matrix. We can calculate inverse matrix from this

coeff_matrix <- function(year, type){
  
  if(type == "backward"){
    
    m <- read_excel(.path, range = paste0(year,"!A8:AL42"), 
                    col_names = FALSE)
    
    m <- select(m,-c(2,3)) # remove irrelevant columns
    m <- unname(unlist(m[,-1])) # remove names and convert into a list of numbers only
    
    M <- matrix(m, nrow = 35, dimnames = list(industry_names,industry_names))
    
    M <- M/colSums(M); M
    
    # Divide by the gross output of sector 'j'
    
    
  } else if(type == "forward"){
    
    m <- read_excel(.path, range = paste0(year,"!A43:AL77"), 
                    col_names = FALSE)
    
    # remove irrelevant columns remove names and convert into a list of numbers only
    m <- unlist(unname(select(m,-c(1:3)))) |>
    # then convert to a matrix
      matrix(nrow = 35, dimnames = list(industry_names,industry_names))
    # then rescale the rows by dividing by the row sums
    m/rowSums(m)
  }
  
}


inverse_matrix <- function(year, type){
  
  m <- coeff_matrix(year = year, type = type)
  solve(diag(nrow(m))-m)
  
}