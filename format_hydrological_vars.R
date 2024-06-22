library(tidyverse)

# Read in excel data, convert to tibble, and snakecase the column names.
read_to_snake = function(filepath){
  if(grepl(pattern = "xls(x)?$", x = filepath)){
    res = openxlsx::read.xlsx(filepath) |> 
      tidyr::as_tibble() |> 
      purrr::set_names(snakecase::to_snake_case)
  }
  if(grepl(pattern = "csv$", x = filepath)){
    res = readr::read_csv(filepath) |> 
      purrr::set_names(snakecase::to_snake_case)
  }
  res
}

setwd(paste0(here::here(),'/data/raw/hydro_data/'))

# =======================
#  Read in data from 'hydro_data' folder      
# =======================

daily_dat = read_to_snake('daily data 1972 to 2016.csv')
# These look like daily flow readings for 366 hydat stations, many of which are discontinued.
# What's the overlap between these data and tidyhydat list of stations?
all_st = tidyhydat::hy_stations(prov_terr_state_loc = 'BC')
all_st[all_st$STATION_NUMBER %in% unique(daily_dat$id),]

sa_wood = read_to_snake('SAwood_update.xlsx')

clearcut = read_to_snake('section_clearcut_levels.xlsx')

storage = read_to_snake('Storage_compiled.xlsx')

topo = read_to_snake('Topo roughnes.csv')

modelled_dat = list.files('Modelled habitat data tables/', full.names = T) |> 
  lapply(\(x) read_to_snake(x))

# Rename the elements of this list of data tables with their file names, 
# removing the prefix of SA[number] and the suffix of file type.
names(modelled_dat) <- stringr::str_remove_all(list.files('Modelled habitat data tables/'),'\\..*')

mdat = dplyr::bind_rows(modelled_dat, .id = 'table_name')

# Make a 'var' column to describe what variable is being described.
mdat = mdat |> 
  dplyr::mutate(variable = stringr::str_remove_all(table_name, "SA[0-9]{1}")) |> 
  dplyr::select(-table_name)

# Drop the 'x_1' column which is just the row numbers.
mdat = mdat |> dplyr::select(-x_1)

# Pivot table long, splice the SA and variable columns into the percent MAD column names.
mdat = mdat |> 
  tidyr::pivot_longer(cols = -c(sa,dates,variable)) |> 
  dplyr::mutate(name = paste0('sa_',sa,'_',snakecase::to_snake_case(variable), '_',name))

# Try making this table VERY wide, to get one row per year? Yikes...
mdat_wide = mdat |> 
  dplyr::select(-sa,-variable) |> 
  tidyr::pivot_wider(names_from = name, values_from = value) |> 
  dplyr::rename(year = dates)

# =======================
#  Summarize datasets to one row per year (except 'daily_dat' flow/water level readings)  
# =======================

sa_wood_wide = sa_wood |> 
  pivot_longer(-c(year,sa)) |> 
  mutate(name = paste0("sa_",sa,"_",name)) |> 
  select(-sa) |> 
  pivot_wider(names_from = name, values_from = value)

storage = storage |> 
  pivot_longer(cols = -year) |> 
  mutate(name = paste0('storage_',name)) |> 
  pivot_wider()

topo = topo |> 
  dplyr::mutate(year = 1970 + row_number()) |> 
  dplyr::select(year, starts_with('sa')) |> 
  pivot_longer(-year) |> 
  mutate(name = paste0('topo_',name)) |> 
  pivot_wider()

# =======================
#  Combine datasets      
# =======================

hydro_data = storage |> 
  full_join(topo) |> 
  full_join(sa_wood_wide) |> 
  full_join(mdat_wide)

write.csv(hydro_data, '../../formatted/all_hydro_data_by_year.csv', row.names = F)

write.csv(hydro_data, '../../../app/www/all_hydro_data_by_year.csv', row.names = F)
