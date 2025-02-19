library(tidyverse)

paths_to_DEM_bits = list.files(path = "data/raw/hydro_data/Channel bed DEMs/",
           recursive = T,
           full.names = T)


SA2_DEM_bits = list.files(path = "data/raw/hydro_data/Channel bed DEMs/SA2/",
                         recursive = T,
                         full.names = T)

all_max_heights = SA2_DEM_bits |> 
  purrr::map( ~ {
    the_rast = terra::rast(.x)
    names(the_rast) = "height"
    data.frame(
      the_min = min(terra::values(the_rast)),
      the_max = max(terra::values(the_rast))
    )
  }) |> 
  dplyr::bind_rows()

absolute_max_min = data.frame(abs_min = min(all_max_heights$the_min), abs_max = max(all_max_heights$the_max))

1:length(SA2_DEM_bits) |>
  lapply(\(x) {
    print(x)
    the_DEM = SA2_DEM_bits[[x]]
    file_name = stringr::str_extract(the_DEM, "[A-Z0-9]+\\.tif")
    the_year = stringr::str_extract(the_DEM,"[0-9]{4}(?=\\.tif)")
    p = ggplot() + 
      tidyterra::geom_spatraster(data = terra::rast(the_DEM)) + 
      scale_fill_viridis_c(limits = c(absolute_max_min$abs_min, absolute_max_min$abs_max)) +
      ggthemes::theme_map() + 
      labs(title = the_year, fill = "Height (m)")
    ggplot2::ggsave(filename = paste0("output/",stringr::str_remove(file_name,"\\.tif"),".jpg"), width = 4, height = 4)
  })

SA2_DEM_jpgs = list.files(path = 'output/', pattern = "SA2.*")

file_names = paste0("output/",stringr::str_extract(SA2_DEM_jpgs, "[A-Z0-9]+\\.jpg"))

m <- magick::image_read(file_names[1])
for (i in 2:length(file_names)) m <- c(m, magick::image_read(file_names[i]))
m <- magick::image_animate(m, fps = 10, loop = 1, dispose = "previous")
magick::image_write(m, "output/SA2_over_time.gif")

