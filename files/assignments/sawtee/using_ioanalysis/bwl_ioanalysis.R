
library(ioanalysis)

.path <- "~/data/institutions/adb/nep_iot.xlsx"

years <- c(2000,2007:2023)

# Backward linkages

bwl_intermediate <- readRDS("files/assignments/sawtee/data/back_linkage.RDS")
names(bwl_intermediate) <- years

region_sector <- data.frame(region = rep("NEP",33),
                            sector = paste0(industry_names_abb))


bwl_total <- lapply(years, function(year){
  readxl::read_excel(.path, range = paste0(year, "!AS8:AS42"),
                     col_names = "total")[-c(23,35),]
}); names(bwl_total) <- years

# year 2010

iom.ioanalysis <- function(year){
  year <- as.character(year)
  ioanalysis::as.inputoutput(
                Z = data.matrix(bwl_intermediate[[year]]),
                RS_label = region_sector,
                X = data.matrix(bwl_total[[year]]))
}

iom.fio2 <- function(year){
  year <- as.character(year)
  fio::iom$new(
    "nepal_adb",
    intermediate_transactions = as.matrix(bwl_intermediate[[year]]),
    total_production = data.matrix(bwl_total[[year]]) |> t())
}

bwl_2022_ioa <- iom.ioanalysis(2022)

d <- bl.11.fio2 <- iom.fio2(2011)

d_12 <- iom.ioanalysis(2012)

