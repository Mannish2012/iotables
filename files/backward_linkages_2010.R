

# In this example, I calculate different aspects related to backward linkages for Nepal
## for the year 2010. The data is available from ADB's website.

# The calculations I will make are as follows: 

# PART I

# Overall importance of each sector as a source of inputs for other industries
## (including itself)

# All sectors are ranked in descending order based on their importance as 
## downstream industries

## 1. unnormalized direct backward linkages
## 2. normalized direct backward linkages
## 3. unnormalized total backward linkages
## 4. normalized direct backward linkages

# PART II

# Part I provided an overview of the importance of each sector. In Part II, I look
## at the relationship between each sector 'j' and other sectors (including itself).
## In other words, for each industry, what are its most important downstream suppliers?


library(dplyr)
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

# ranking a sectors direct backward linkages with other sectors

top_ranking_A <- apply(A,2, function(x) rank(-x))
bottom_ranking_A <- apply(A,2, function(x) rank(x))

# ranking a sectors total backward linkages with other sectors
top_ranking_L <- apply(L,2, function(x) rank(-x))
bottom_ranking_L <- apply(L,2, function(x) rank(x))

ranking <- function(sector, type){
  
  dat <- switch(type,
                "direct" = top_ranking_A,
                "total" = top_ranking_L)

  n <- grepl(sector, colnames(A), ignore.case = TRUE)
  tibble(sector = names(dat[,n]),
         rank = unname(dat[,n]),
         values = A[,n]) |>
    arrange(rank)
}
# ranking('Inland transport', type = "direct")

# all sectors that have backward linkages to sector j are ranked in order of importance 
## and the unnormalized values of their contributions are shown

# direct backward linkages

lapply(colnames(A), function(x) ranking(x, "direct")) |> 
  setNames(colnames(A)) |>
  bind_rows(.id = "target sector") |>
  rename("input sector" = "sector") -> bl.ranking_2022_direct

# total backward linkages

lapply(colnames(L), function(x) ranking(x, "total")) |> 
  setNames(colnames(L)) |>
  bind_rows(.id = "target sector") |>
  rename("input sector" = "sector") -> bl.ranking_2022_total