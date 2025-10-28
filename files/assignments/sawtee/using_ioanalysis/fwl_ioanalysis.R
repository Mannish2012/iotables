

library(ioanalysis)

years <- c(2000,2007:2023)

# Backward linkages

fwl_intermediate <- readRDS("files/assignments/sawtee/data/fwd_linkage.RDS")
names(bwl_intermediate) <- years

region_sector <- data.frame(region = rep("NEP",33),
                            sector = paste0(industry_names_abb))


total_prod <- lapply(years, function(year){
  readxl::read_excel(.path, range = paste0(year, "!AS8:AS42"),
                     col_names = "total")[-c(23,35),]
}); names(total_prod) <- years

# year 2010

iom <- function(year){
  year <- as.character(year)
  as.inputoutput(
    Z = as.matrix(fwl_intermediate[[year]]),
    RS_label = region_sector,
    X = as.matrix(total_prod[[year]]))
}; iom(2011)


io3 <- as.inputoutput(Z = as.matrix(bwl_intermediate[["2000"]]),
                      RS_label = region_sector,
                      X = as.matrix(total_prod[["2000"]]))
