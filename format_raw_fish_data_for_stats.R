library(tidyverse)

# Read in excel data, convert to tibble, and snakecase the column names.
read_to_snake = function(filepath){
  openxlsx::read.xlsx(filepath) |> 
    tidyr::as_tibble() |> 
    purrr::set_names(snakecase::to_snake_case)
}

setwd(here::here())

# 4 fish variables (could be five if we include overwinter by age, which is currently incomplete).
# All but the 'coho_pop_sec' table are single row per section; the exception has separate rows by section.

coho_pop_tr = read_to_snake('data/raw/fish_data/coho_population_trends.xlsx')

coho_pop_sec = read_to_snake('data/raw/fish_data/late_summer_juv_coho_pop_ests_by_section.xlsx')

multiyear = read_to_snake('data/raw/fish_data/multiyear_summary_late_summer_coho_pops.xlsx') |> 
  dplyr::rowwise() |> 
  dplyr::mutate(total = sum(section_2 , section_3 , section_4 , section_5 , section_6 , section_8, na.rm=T)) |> 
  dplyr::ungroup()

overwinter = read_to_snake('data/raw/fish_data/overwinter_survival.xlsx')

overwinter_by_age = read_to_snake('data/raw/fish_data/overwinter_survival_by_age.xlsx')

# Simplify tables
coho_pop_sec_s = coho_pop_sec |> 
  dplyr::select(year, section, pop_estimate = population_estimate_n) |> 
  dplyr::group_by(year) |> 
  dplyr::mutate(total_juv_pop = sum(as.numeric(pop_estimate),na.rm=T)) |> 
  tidyr::pivot_wider(names_from = section, values_from = pop_estimate, names_prefix = 'juv_pop_in_section_')

# Combine the data with a single row per section.
dat_com = coho_pop_tr |> 
  dplyr::left_join(coho_pop_sec_s) |> 
  dplyr::left_join(overwinter) |> 
  dplyr::left_join(overwinter_by_age |> dplyr::select(-late_summer_coho_population_abundance)) |>
  dplyr::mutate(year = as.factor(year)) |> 
  dplyr::mutate(year = forcats::fct_inorder(year))

# Convert everything to numeric except the following columns: logging_phase
dat_com = dat_com |> 
  dplyr::mutate(across(-c(year,logging_phase), as.numeric))

# Write out this combined fish data
write.csv(dat_com, 'data/formatted/all_fish_data_by_year.csv', row.names = F)

write.csv(dat_com, 'app/www/all_fish_data_by_year.csv', row.names = F)

