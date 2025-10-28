
# library(dplyr)
# .path <- "~/data/institutions/adb/nep_iot2.xlsx"
year <- 2010

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
dbl <- tibble(sector = names(colSums(A)), 
              unnormalized_dbl = unname(colSums(A)),
              normalized_dbl = unnormalized_dbl/mean(unnormalized_dbl)) |>
        arrange(desc(round(unnormalized_dbl,3)), 
                desc(round(normalized_dbl,3)))

# requirements matrix
L <- solve(diag(33) - A)

# total backward linkage
tbl <- tibble(sector = names(colSums(L)), 
              unnormalized_tbl = unname(colSums(L)),
              normalized_tbl = unnormalized_tbl/mean(unnormalized_tbl)) |> 
  arrange(desc(unnormalized_tbl), desc(normalized_tbl))

# ranking a sectors linkages with other sectors

top_ranking_A <- apply(A,2, function(x) rank(-x))
bottom_ranking_A <- apply(A,2, function(x) rank(x))


ranking <- function(sector){
  # n = which(sector == colnames(A))
  n <- grepl(sector, colnames(A), ignore.case = TRUE)
  tibble(sector = names(top_ranking_A[,n]),
         rank = unname(top_ranking_A[,n]),
         values = A[,n]) |>
    arrange(rank)
}

lapply(colnames(A), ranking) |> setNames(colnames(A)) |>
  bind_rows(.id = "target sector") |>
  rename("input sector" = "sector") -> ranking_2022
