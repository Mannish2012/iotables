
.path <- "~/data/institutions/adb/nep_iot2.xlsx"
year <- 2022

column_names <- names(readxl::read_excel(.path, range = paste0(year,"!D5:AO5")))
industry_names <- column_names[1:35]
gross_consump_names <- column_names[36:38]

# intermediate consumption matrix
full_range <- "!A8:AL86"
full_mat <- as.data.frame(
  readxl::read_excel(.path, range = paste0(year,full_range),
                     col_names = FALSE))



# divide the data frame 
Z <- full_mat[1:35,-c(1:3)] # intermediate sales from sector i to sector j
Z2 <- full_mat[36:70,-c(1:3)]

dimnames(Z) <- list(industry_names, industry_names)
dimnames(Z2) <- list(industry_names, industry_names)

un_needed_vars <- unname(which(sapply(Z, function(var) all(var == 0))))

Z <- Z[-un_needed_vars,-un_needed_vars]

tot <- as.numeric(full_mat[nrow(full_mat),-c(1:3)])[-un_needed_vars] |>
  setNames(industry_names[-un_needed_vars])


# technical coefficients matrix
A <- sweep(Z,2,tot,"/")

# direct backward linkage
dbl <- unname(colSums(A))

# requirements matrix

L <- diag(33) - A
as.numeric(colSums(L))





int_consump_mat <- full_mat |> select()

full_mat[nrow(full_mat),4]

colnames(full_mat) <- industry_names

total_range <- "D86:AS86"
total <- as.data.frame(
  readxl::read_excel(.path, range = paste0(year,total_range),
                     col_names = FALSE))[,-c(1:3)]



