
source("coef_matrices.R")

backward_linkage <- function(year, method){
  if(method == "direct"){
    colSums(coeff_matrix(year = year, type = "backward"))
  } else{
    if(method == "total"){
      colSums(inverse_matrix(year=year,type ="backward"))
    }
  }
}

forward_linkage <- function(year, method){
  if(method == "direct"){
    rowSums(coeff_matrix(year = year, type = "forward"))
  } else{
    if(method == "total"){
      rowSums(inverse_matrix(year=year,type ="forward"))
    }
  }
}


linkage <- function(year, method, type){
  
  if(type == "backward"){
    A = backward_linkage(year,method)
  } else {
    if(type == "forward"){
      A = forward_linkage(year, method)
    }
  }
  as_tibble(cbind(industry = names(A), value = unname(A))) |>
    mutate(value = as.numeric(value),
           normalized_value = value/sum(value))
  
}

linkage2 <- function(year, method, type){
  years <- excel_sheets(.path)[-1]
  purrr::map(years, ~linkage(.x,method = method, type = type)) |> 
    set_names(years) |>
    bind_rows(.id = "year")
}
