
# source("files/assignments/sawtee/final/read_data.R")

backward_linkage <- function(year, output){
  
  #read the data, and store as the original data set
  original <- read_data(year, linkage_type = "backward")
  
  # Derive the input coefficients matrix, A
  A <- original/colSums(original)
  N <- ncol(A) # obtain order of the matrix
  
  # In matrix A (below), each column 'i' denotes the backward linkage of sector 'i', i.e,
  ## the direct impact on each sector in the column. Hence the sum of this column
  ## denotes the complete backward linkage of sector 'i'. Note that NaN values may be present. 
  ## Use simple imputation to replace them with column means
  
  A <- apply(A, 2, function(x){
    ifelse(is.nan(x), mean(na.omit(x)),x)
  }) |> as.matrix()
  # names
  dimnames(A) <- list(industry_names_abb, industry_names_abb)

  
  # Derive the Leontief inverse matrix (or requirements matrix), L
  ## This captures both direct and indirect impacts of an increase in the output
  ## of a sector 'i'. Multiply technical_coef matrix by 0.99 to ensure that
  ## inverse is obtained. 
  L <- solve(diag(N) - (A*0.99))
  dimnames(L) <- list(industry_names_abb, industry_names_abb)
  
  # (UNNORMALIZED) BACKWARD LINKAGES
  `direct linkage` <- colSums(A)
  `total linkage` <- colSums(L)
  
  ## NORMALIZED LINKAGES
  `normalized direct linkage` <- `direct linkage`/mean(`direct linkage`)
  `normalized total linkage` <- `total linkage`/mean(`total linkage`)
  
  or2 <- original[1:6,1:6]
  
  switch(output,
         "G" = A |> colSums(),
         "direct backward linkage" = `direct linkage`,
          "total backward linkage" = `total linkage`,
         "normalized direct bac
         kward linkage" = `normalized direct linkage`,
         "normalized total backward linkage" = `normalized total linkage`)
  
}; backward_linkage(2022, "G")
