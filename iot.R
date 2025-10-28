---
title: "Nepal Input Output Tables"
format: pdf
editor: visual
execute: 
  echo: false
  warning: false
---

#'

```{r init}
library(dplyr)
library(ggplot2)
library(purrr)
library(readxl)

```

```{r}


# set directory paths to retrieve stored data or files
dat_dir <- "C:/users/hello/rprojects/upwork"

.path <- "~/data/institutions/adb/nep_iot.xlsx"

iot13 <- read_excel(.path, range = "2013!A7:AL42")

names_iot3 <- pull(iot13,1)
iot13 <- select(iot13, -c(2,3))
# names(iot13) <- c("industry", names_iot3)

```

```{r}
# m <- iot13[,-1] %>% unlist() %>% unname()
# 
# m2 <- matrix(m, nrow = 35, dimnames = list(names_iot3, names_iot3))
# 
# actual_inv <- solve(diag(nrow(m2)) - m2) `
```

```{r}

#' The following code snippet transforms data set into matrix form and
##' finds the necessary inverse

#' get the names of the industry (previously stored)
industry_names <- readRDS(paste0(dat_dir,"/data/institutes/adb/nepal_iot.RDS"))

# Computing forward linkages
.path <- "~/data/institutions/adb/nep_iot.xlsx"

#' Get the coefficients matrix. We can calculate inverse matrix from this

coeff_matrix <- function(year, type){
  
  if(type == "backward"){
            m <- read_excel(.path, range = paste0(year,"!A8:AL42"), 
                          col_names = FALSE)
  } else if(type == "forward"){
           m <- read_excel(.path, range = paste0(year,"!A43:AL77"), 
                          col_names = FALSE)
  }

  #' remove irrelevant columns
  m <- select(m,-c(2,3)) 
  #' remove names and convert into a list of numbers only
  m <- unname(unlist(m[,-1])) 
  
  matrix(m, nrow = 35, dimnames = list(industry_names,industry_names))

}


inverse_matrix <- function(year, type){
  
  m <- coeff_matrix(year = year, type = type)
  solve(diag(nrow(m))-m)
  
}


