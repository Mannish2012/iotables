
# Normalized direct linkage
# sectoral_direct_average <- colMeans(technical_coef) # N X 1 column vector
# overall_direct_average <- mean(sectoral_direct_average) # 1 X 1 number
# normalized_direct <- technical_coef/overall_direct_average # N X N matrix

# Normalized total linkage (same methodology as normalized direct linkage)
# sectoral_total_average <- colMeans(leontief_inv) # N X 1 column vector
# overall_total_average <- mean(sectoral_total_average) # 1 X 1 number
# normalized_total <- leontief_inv/overall_total_average # N X N matrix

switch(output,
       # Matrices
       "original data" = original,
       "technical coef" = technical_coef,
       "leontief inverse" = leontief_inv,
       "normalized direct" = normalized_direct,
       "normalized total" = normalized_total,
       # Vectors
       "direct BL" = colSums(technical_coef),
       "total BL" = colSums(leontief_inv),
       "normalized direct BL" = colSums(normalized_direct),
       "normalized total BL" = colSums(normalized_total)
)





# Forward linkages

# sector i's direct forward linkage is a 1 X N row vector
## average of all direct forward linkages is obtained by taking the average of 
## the average direct forward linkages of ALL sectors 

# hence we first need to get the average of direct forward linkage of sector i
## that is a single number. The average of direct forward linkages of ALL sectors
## will be an N X 1 column vector
## Finally, the overall average is also a single number, and it is the average of
## the N X 1 column vector calculated earlier. 

sectoral_direct_average <- rowMeans(output_coef) # N X 1 column vector
overall_direct_average <- mean(sectoral_direct_average) # 1 X 1 number
normalized_direct <- output_coef/overall_direct_average # N X N matrix

# normalized_direct2 <- lapply(1:N, function(i){
#   # num <- N*(output_coef %*% output_coef[i,]); num
#   denom <- t(output_coef[i,]) %*% output_coef %*% output_coef[i,]
#   num/
# })

# Normalized total linkage (same methodology as normalized direct linkage)
sectoral_total_average <- rowMeans(ghosh_inv)
overall_total_average <- mean(sectoral_total_average)
normalized_total <- ghosh_inv/overall_total_average