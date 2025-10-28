
industry_names <- readRDS(
  "files/assignments/sawtee/data/original_industry_names.RDS")
industry_names_abb <- readRDS(
  "files/assignments/sawtee/data/industry_names.RDS")

.path <- "~/data/institutions/adb/nep_iot.xlsx"

read_data <- function(year, linkage_type){
  
  backward_linkage <- function(year){
    dat.bl <- readxl::read_excel(.path, 
                                 range = paste0(year,"!A8:AL42"),
                                 col_names = FALSE) |>
      as.data.frame()
    
    # colnames(dat.bl)[2] <- "c2"
    # dat.bl <- dplyr::filter(dat, c2 == "NEP")
    dat.bl
  }
  
  forward_linkage <- function(year){
    dat.fl <- readxl::read_excel(.path, 
                                 range = paste0(year,"!A43:AL77"),
                                 col_names = FALSE) |>
      as.data.frame()
    
    # colnames(dat.fl)[2] <- "c2"
    # dat.fl <- dplyr::filter(dat.fl, c2 == "IMP")
    dat.fl
  }
  
  dat <- switch(linkage_type,
                "forward" = forward_linkage(year),
                "backward" = backward_linkage(year))
  
  # determine which columns are zero
  
  determine_zero_vars <- Map(function(var) all(var == 0), dat)
  zero_vars <- which(unname(unlist(determine_zero_vars)))

  # list all columns we want to remove
  unnecessary_cols <- c(1:3, zero_vars)
  # then remove them
  
  # list all the rows we want to remove. First, find which variables we want
  ## to remove. We want to remove the variables that have all 0 values
  unnecessary_vars <- unlist(industry_names[zero_vars - 3])
  unnecessary_rows <- unname(sapply(unnecessary_vars, function(x){
    which(x == industry_names)
  }))
  
  # This dataset is 33 X 33. 
  dat <- as.matrix(dat[-unnecessary_rows,-unnecessary_cols])
  dimnames(dat) <- list(industry_names_abb, industry_names_abb)
  
  # remove the associated industries with all zero values from the list
  ## of industry names
  # industry_names <- industry_names[-unnecessary_rows]
  # determine the number of usable industries
  # len <- length(industry_names)
  
  dat
  
}
