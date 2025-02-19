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

# Add mean value across SAs
add_mean_vars = function(dat){
  dat_l = dat |> 
    dplyr::mutate(sa = as.character(sa)) |> 
    pivot_longer(-c(year,sa))
  
  dat_mean = dat_l |> 
    dplyr::group_by(year, name) |> 
    dplyr::summarise(across(-c(sa), \(x) mean = mean(x,na.rm=T))) |> 
    dplyr::mutate(value = ifelse(is.nan(value), NA, value)) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(sa = 'mean')
  
  dat_w = dat_l |> 
    dplyr::bind_rows(dat_mean) |> 
    mutate(name = paste0("sa_",sa,"_",name)) |> 
    select(-sa) |> 
    pivot_wider(names_from = name, values_from = value)
  
  return(dat_w)
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

mdat = dplyr::bind_rows(modelled_dat, .id = 'table_name') |> 
  dplyr::select(-x_1) |> 
  dplyr::mutate(sa = as.character(sa))

# Make a 'var' column to describe what variable is being described.
mdat = mdat |> 
  dplyr::mutate(variable = stringr::str_remove_all(table_name, "SA[0-9]{1}")) |> 
  dplyr::select(-table_name,
                year = dates)

# Summarise all variables (described in the 'variable' column) across
# different SAs; this will give us an average across carnation creek by variable and year
mdat_mean = mdat |> 
  dplyr::group_by(variable, year) |> 
  dplyr::summarise(across(-c(sa), \(x) mean(x,na.rm=T))) |> 
  dplyr::ungroup() |>
  dplyr::mutate(sa = 'mean')

# Join these mean values onto the rest of the mdat dataset
mdat = mdat |> 
  dplyr::bind_rows(mdat_mean) |> 
  dplyr::arrange(year, sa)

# Pivot table long, splice the SA and variable columns into the percent MAD column names.
mdat_l = mdat |> 
  tidyr::pivot_longer(cols = -c(sa,year,variable)) |> 
  dplyr::mutate(name = paste0('sa_',sa,'_',snakecase::to_snake_case(variable), '_',name)) |> 
  dplyr::mutate(name = ifelse(stringr::str_detect(name, '^sa_mean'), stringr::str_remove(name, "^sa_"), name))

# Try making this table VERY wide, to get one row per year? Yikes...
mdat_w = mdat_l |> 
  dplyr::select(-sa,-variable) |> 
  tidyr::pivot_wider(names_from = name, values_from = value)

# =======================
#  Summarize datasets to one row per year (except 'daily_dat' flow/water level readings)  
# =======================

sa_wood_w = add_mean_vars(sa_wood)

storage_l = storage |> 
  pivot_longer(cols = -year) |> 
  dplyr::mutate(name = paste0(str_extract(name, 'sa_[0-9]'),"_storage"))

storage_mean = storage_l |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(value = mean(value, na.rm=T)) |> 
  dplyr::mutate(name = 'mean_storage')

storage_w = storage_l |> 
  dplyr::bind_rows(storage_mean) |> 
  arrange(year, name) |> 
  pivot_wider()

topo = topo |> 
  dplyr::mutate(year = 1970 + row_number()) |> 
  dplyr::select(year, starts_with('sa')) |> 
  pivot_longer(-year) |> 
  mutate(name = paste0('topo_',name)) |> 
  pivot_wider()

topo_l = topo |> 
  pivot_longer(cols = -year) |> 
  dplyr::mutate(name = paste0(str_extract(name, 'sa_[0-9]'),"_topo"))

topo_mean = topo_l |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(value = mean(value, na.rm=T)) |> 
  dplyr::mutate(name = 'mean_topo')

topo_w = topo_l |> 
  dplyr::bind_rows(topo_mean) |> 
  arrange(year, name) |> 
  pivot_wider()

# =======================
#  Combine datasets      
# =======================

hydro_data = storage_w |> 
  full_join(topo_w) |> 
  full_join(sa_wood_w) |> 
  full_join(mdat_w)

setwd(here::here())

write.csv(hydro_data, 'data/formatted/all_hydro_data_by_year.csv', row.names = F)

write.csv(hydro_data, 'app/www/all_hydro_data_by_year_2.csv', row.names = F)
