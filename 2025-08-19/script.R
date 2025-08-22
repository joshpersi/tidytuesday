df <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-19/scottish_munros.csv'
)

df <- dplyr::filter(df, !is.na(Name))

df_sf <- sf::st_as_sf(df, coords = c("xcoord", "ycoord"), crs = "EPSG:27700")

ben_nevis <- dplyr::filter(df_sf, Name == "Ben Nevis")

df_sf <- sf::st_filter(
  df_sf,
  ben_nevis,
  .predicate = sf::st_is_within_distance,
  dist = units::set_units(5, "km")
)


points_vect <- terra::vect(df_sf, geom = c("x", "y"), crs = "EPSG:27700")

# Create a raster template
ext_buffer <- terra::ext(df_sf) + 0.01 # Add small buffer

r <- terra::rast(ext_buffer, resolution = 0.001) # Adjust resolution as needed

terra::crs(r) <- "EPSG:4326"

# Interpolate elevation values (using thin plate spline)
interpolated_raster <- terra::interpolate(
  r,
  points_vect,
  field = "elevation",
  method = "tps"
)

# Create contour lines
contour_lines <- as.contour(interpolated_raster)

# Convert to sf for ggplot
contours_sf <- st_as_sf(contour_lines)


ggplot2::ggplot() +
  ggplot2::geom_sf(data = df_sf) +
  ggplot2::geom_sf(data = ben_nevis, fill = "red", size = 4) +
  ggplot2::geom_contour(
    data = df,
    ggplot2::aes(xcoord, ycoord, z = Height_m)
  )
