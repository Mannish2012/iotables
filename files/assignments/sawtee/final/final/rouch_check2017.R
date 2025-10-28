# Create a 35 X 35 matrix of intermediate values
## (Later will be reduced to 33 X 33)

M <- as.data.frame(readxl::read_excel(.path,
                                      range = paste0(year,"!A8:AL42"),
                                      col_names = FALSE))[,-c(1:3)]


dimnames(M) <- list(industry_names, industry_names)

# which variables have ONLY zero values?
determine_zero_vars <- Map(function(var) all(var == 0), M)
zero_vars <- which(unname(unlist(determine_zero_vars)))

# remove the unnecessary variables from the M matrix and
## change industry_names variable to reflect that. New matrix is 33 X 33
M <- M[-zero_vars, -zero_vars]
industry_names <- colnames(M)

# Create a matrix of gross output values. Dimension: 33 X 3
gross <- as.data.frame(readxl::read_excel(.path, 
                                          range = paste0(year,"!AM8:AS42"), col_names = FALSE))[-zero_vars,]
dimnames(gross) <- list(industry_names, gross_consump_names)

total <- as.data.frame(readxl::read_excel(.path, 
                                          range = paste0(year,"!AS8:AS42"),
                                          col_names = FALSE))[-zero_vars,]

gross_output <- rowSums(M)

A <- M/gross_output; A[,1]


unname(rowSums(M)) |> sum()

sum(M1)
}

get_data(2016)

