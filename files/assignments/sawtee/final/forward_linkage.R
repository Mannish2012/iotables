
# source("files/assignments/sawtee/final/read_data.R")

# For forward linkage, read the appropriate data and then transform it to the
## output coefficients matrix by dividing original data by ROW sums. 


forward_linkage <- function(year, output){

  original <- read_data(year, linkage_type = "forward")
  # original <- t(original) # transpose (in R working with rows can be problematic)
  
  # output coefficients matrix (B)
  B <- original/rowSums(original)
  N <- nrow(B) # obtain order of the matrix
  
  B <- apply(B, 2, function(x){
    ifelse(is.nan(x), mean(na.omit(x)), x) })

  # in matrix output_coef, each row 'i' denotes the forward linkage of sector 'i', i.e,
  ## the direct impact on each sector in the row. Hence the sum of this row
  ## denotes the complete forward linkage of sector 'i'. 
  
  # Ghosh inverse matrix (G)
  ## This captures both direct and indirect impacts of an increase in the output
  ## of a sector 'i'. Multiply output_coef matrix by 0.99 to ensure that
  ## inverse is obtained. 
  G <- solve(diag(N) - (B*0.99))
  dimnames(G) <- list(industry_names_abb, industry_names_abb)
  
  ## NORMALIZED LINKAGES
  
  # Normalized direct linkage
  # we need the simple average of all direct forward linkages
  
  # (UNNORMALIZED) FORWARD LINKAGES
  `direct linkage` <- rowSums(B)
  `total linkage` <- rowSums(G)
  
  ## NORMALIZED LINKAGES
  `normalized direct linkage` <- `direct linkage`/mean(`direct linkage`)
  `normalized total linkage` <- `total linkage`/mean(`total linkage`)
  
  or2 <- original[1:6,1:6]
  
  switch(output,
         "G" =  rowSums(original/rowSums(original)),
         "H" = rowSums(original*rowMeans(original)),
         "direct forward linkage" = `direct linkage`,
         "total forward linkage" = `total linkage`,
         "normalized direct forward linkage" = `normalized direct linkage`,
         "normalized total forward linkage" = `normalized total linkage`)

}; forward_linkage(2022, "H")

