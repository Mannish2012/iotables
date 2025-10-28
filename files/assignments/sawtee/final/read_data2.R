
# .path <- "~/data/institutions/adb/nep_iot.xlsx"


get_data <- function(year, linkage_type, data_type){
  
   column_names <- names(readxl::read_excel(.path, range = paste0(year,"!D5:AO5")))
   industry_names <- column_names[1:35]
   gross_consump_names <- column_names[36:38]
   
   backward_linkage <- function(year){
     
     # Create a 35 X 35 matrix of intermediate values
     ## (Later will be reduced to 33 X 33)
     intermediate <- as.data.frame(readxl::read_excel(.path, 
                        range = paste0(year,"!A8:AL42"),
                        col_names = FALSE))[,-c(1:3)]
     dimnames(intermediate) <- list(industry_names, industry_names)
     
     # which variables have ONLY zero values?
     determine_zero_vars <- Map(function(var) all(var == 0), intermediate)
     zero_vars <- which(unname(unlist(determine_zero_vars)))
     
     # remove the unnecessary variables from the intermediate matrix and
     ## change industry_names variable to reflect that. New matrix is 33 X 33
     intermediate <- intermediate[-zero_vars, -zero_vars]
     industry_names <- colnames(intermediate)
     
     # Create a matrix of gross output values. Dimension: 33 X 3
     gross <- as.data.frame(readxl::read_excel(.path,
                              range = paste0(year,"!AM8:AO42"),
                              col_names = FALSE))[-zero_vars,]
     dimnames(gross) <- list(industry_names, gross_consump_names)
     
     total <- as.data.frame(readxl::read_excel(.path,
                            range = paste0(year,"!AS8:AS42"),
                            col_names = FALSE))[-zero_vars,]

     
     gross_sum <- rowSums(gross)
     
     A <- intermediate/gross_sum
     L <- solve(diag(ncol(A)) - A)
     
     switch(data_type,
            "A" = A,
            "L" = L,
            "direct" = colSums(A),
            "total" = colSums(L),
            "normalized direct" = colSums(A)/mean(colSums(A)),
            "normalized total" = colSums(L)/mean(colSums(L)))
     
   }
   
   forward_linkage <- function(year){
     
     intermediate <- as.data.frame(readxl::read_excel(.path, 
                                 range = paste0(year,"!A43:AL77"),
                                 col_names = FALSE))[,-c(1:3)]
     dimnames(intermediate) <- list(industry_names, industry_names)
     
     # which variables have ONLY zero values?
     determine_zero_vars <- Map(function(var) all(var == 0), intermediate)
     zero_vars <- which(unname(unlist(determine_zero_vars)))
     
     intermediate <- intermediate[-zero_vars, -zero_vars]
     industry_names <- colnames(intermediate)
     
     gross <- as.data.frame(readxl::read_excel(.path,
                              range = paste0(year,"!AM8:AO42"),
                              col_names = FALSE))[-zero_vars,]
     dimnames(gross) <- list(industry_names, gross_consump_names)
     
     total <- as.data.frame(readxl::read_excel(.path,
                            range = paste0(year,"!AS8:AS42"),
                            col_names = FALSE))[-zero_vars,]
     
     
     gross_sum <- rowSums(gross)
     
     B <- intermediate/gross_sum
     G <- solve(diag(ncol(B)) - B)
     
     switch(data_type,
            "B" = B,
            "G" = G,
            "direct" = rowSums(B),
            "total" = rowSums(G),
            "normalized direct" = rowSums(B)/mean(rowSums(B)),
            "normalized total" = rowSums(G)/mean(rowSums(G)))
     
   }
   
   switch(linkage_type,
          "backward" = backward_linkage(year),
          "forward" = forward_linkage(year))
 }

get_data(2022, linkage_type = "backward", data_type = "normalized total")


