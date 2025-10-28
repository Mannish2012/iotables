
.path <- "~/data/institutions/adb/nep_iot.xlsx"

get_data <- function(year, linkage_type, data_type){
  
    column_names <- names(readxl::read_excel(.path, range = paste0(year,"!D5:AO5")))
    industry_names <- column_names[1:35]
    gross_consump_names <- column_names[36:38]
    
    int_range <- switch(linkage_type,
                        "backward" = "!A8:AL42",
                        "forward" = "!A43:AL77")
    
    gross_range <- switch(linkage_type,
                          "backward" = "!AM8:AO42",
                          "forward" = "!AM8:AO42")
    
    intermediate <- as.data.frame(readxl::read_excel(.path, 
                                                     range = paste0(year, int_range),
                                                     col_names = FALSE))[,-c(1:3)]
    dimnames(intermediate) <- list(industry_names, industry_names)
    
    # which variables have ONLY zero values?
    determine_zero_vars <- Map(function(var) all(var == 0), intermediate)
    zero_vars <- which(unname(unlist(determine_zero_vars)))
    
    intermediate <- intermediate[-zero_vars, -zero_vars]
    industry_names <- colnames(intermediate)
    
    gross <- as.data.frame(readxl::read_excel(.path,
                                              range = paste0(year, gross_range),
                                              col_names = FALSE))[-zero_vars,]
    dimnames(gross) <- list(industry_names, gross_consump_names)
    
    
    gross_sum <- switch(linkage_type,
                        "backward" = colSums(gross),
                        "forward" = rowSums(gross))
    
    direct <- intermediate/gross_sum # A/B matrix 
    total <- solve(diag(ncol(direct)) - direct) # Leontief/Ghosh inverse matrix
    
  
}

# get_data(2007, "backward", "total")


linkage <- function(year, linkage_type, data_type){
  
  eval(body(get_data))
  

    switch(data_type,

           "coeff" = direct,
           "inv" = total,

           "direct" = switch(linkage_type,
                             "backward" = colSums(direct),
                             "forward" = colSums(total)),

           "total" = switch(linkage_type,
                            "backward" = rowSums(direct),
                            "forward" = rowSums(total)),

           "normalized direct" = switch(linkage_type,
                                        "backward" = colSums(direct)/mean(colSums(direct)),
                                        "forward" = rowSums(direct)/mean(rowSums(direct))),

           "normalized total" = switch(linkage_type,
                                       "backward" = colSums(direct)/mean(colSums(direct)),
                                       "forward" = rowSums(direct)/mean(rowSums(direct)))
    )


}


# linkage(2021,"backward","total") 


sectoral_rankings <- function(year, linkage_type, sector, type){
  
    backward <- function(sector){
      switch(type,
             "direct" = linkage(year,"backward","normalized direct"),
             "total" = linkage(year,"backward","normalized total"))}
    
    forward <- function(sector){
      switch(type,
             "direct" = linkage(year,"forward","normalized direct"),
             "total" = linkage(year,"forward","normalized total"))}
    

     sorted <- switch(linkage_type,
           "backward" = backward(sector),
           "forward" = forward(sector)
           ) |> sort() 
     
     data.frame(sectors = names(sorted),
                   values = unname(sorted) |> round(2))
     
}

# sectoral_rankings(year = 2022,
#                   linkage_type = "backward",
#                   sector = "Education", 
#                   type = "direct")


sectoral_linkages <- function(year, linkage_type, sector, impact_type){
  
  eval(body(get_data))
  
  sorted <- sort(total[,sector], decreasing = TRUE) |> round(3)
  dat <- data.frame(sector = names(sorted), values = unname(sorted)) |>
    subset(values > 0)
  dat
  
  
  
}

# sectoral_linkages(2022, "backward", "Education", "direct")

industry_names <- industry_names <- readRDS(
  "files/assignments/sawtee/data/original_industry_names.RDS")
industry_names <- industry_names[-c(24,35)]
names(industry_names) <- as.character(1:33)

library(dplyr); library(purrr)

impact_types <- c("direct","total"); linkage_types <- c("backward","forward")
  
yearly_data <- function(year){
  map(industry_names, function(sector){
    map(impact_types, function(impact_type){
      map(linkage_types,function(linkage_type){
        sectoral_linkages(year,linkage_type,sector, impact_type) 
      }) |> set_names(linkage_types)
    }) |> set_names(impact_types)
  }) |> set_names(industry_names) 
}

y15 <- yearly_data(2015)
f15.1 <- list_flatten(y15)
f15.2 <- list_flatten(f15.1)

f15.2_separated <- bind_rows(f15.2, .id = "ID") |>
 tidyr::separate_wider_delim(cols = "ID", 
                              delim = "_",,
                             names = c("target_sector","type","linkage"))


flattened_list <- function(year){
  yearly_data(year) |> 
    list_flatten() |> 
    list_flatten() |>
    bind_rows(.id = "id") |>
    tidyr::separate_wider_delim(cols = "id", 
                                delim = "_",,
                                names = c("target_sector","type","linkage"))
}

# Get years

y2011 <- flattened_list(2011)
y2012 <- flattened_list(2012)
y2013 <- flattened_list(2013)
y2014 <- flattened_list(2014)
y2015 <- flattened_list(2015)
y2016 <- flattened_list(2016)
y2017 <- flattened_list(2017)
y2018 <- flattened_list(2018)
y2019 <- flattened_list(2019)
y2020 <- flattened_list(2020)
y2021<- flattened_list(2021)


save_files <- function(year){
  y <- paste0("y",year)
  .file <- sprintf("data/institutes/sawtee/linkages/csv/%s.csv",y)
  write.csv(eval(str2expression(y)),.file)
}

lapply(2011:2021, save_files)











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







