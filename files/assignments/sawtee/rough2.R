

readxl::excel_sheets("data/institutes/sawtee/linkages/excel/all.xlsx")

save_files2 <- function(year){
  y <- paste0("y",year)
  # .file <- sprintf("data/institutes/sawtee/linkages/excel/%s.xlsx",y)
  .file <- 
    openxlsx::write.xlsx(eval(str2expression(y)),.file)
}

lapply(2011:2021, save_files2)

new_dir <- "data/institutes/sawtee/linkages/excel"
openxlsx::write.xlsx(
  paste0(new_dir,"/",list.files(new_dir)),
  paste0(new_dir,"/allyears.xlsx")
)

x


dir_csv_files <- "data/institutes/sawtee/linkages/csv"

openxlsx::write.xlsx(paste0(dir_csv_files, "/", list.files(dir_csv_files)), 
                     file = paste0(dir_csv_files,"/dat.xlsx"))

openxlsx::write.xlsx("data/institutes/sawtee/linkages/csv/y2011.csv",
                     paste0(dir_csv_files,"/y2011.xlsx"))




# Rough


# save_files <- function(year){
#   y <- paste0("y",year)
#   .file <- sprintf("data/institutes/sawtee/linkages/csv/%s.csv",y)
#   write.csv(eval(str2expression(y)),.file)
# }
# 
# lapply(2011:2021, save_files)


# mass combine

combine_dat <- function(total){
  
  switch(total,
         yes = "total",
         no = "direct")
  
};combine_dat(total = "no")

combine_dat_yearwise <- function(year){
  bind_rows(map_df(yearly_data(year), ~bind_rows(.x$direct, .id = "linkage type"),
                   .id = "sector"),
            map_df(yearly_data(year), ~bind_rows(.x$total, .id = "linkage type"),
                   .id = "sector"),.id = "tot_dir") |> 
    mutate(tot_dir = if_else(tot_dir == 1,"tot","dir"))
}

y2020 <- combine_dat_yearwise(2020)

bind_rows(map_df(y2010, ~bind_rows(.x$direct, .id = "linkage type"),
                 .id = "sector"),
          map_df(y2010, ~bind_rows(.x$total, .id = "linkage type"),
                 .id = "sector"),.id = "tot_dir") |> 
  mutate(tot_dir = if_else(tot_dir == 1,"tot","dir")) |> View()




lapply()


