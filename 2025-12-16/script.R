df <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-16/roundabouts_clean.csv'
)

df <- dplyr::filter(df, country == "United States", status == "Existing")

df <- dplyr::select(df, lat, long)

df <- dplyr::rename(df, lat = long, lon = lat)

df <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)

states <- sf::st_transform(tigris::states(), crs = 4326)

states <- dplyr::select(states, name = NAME, land_area = ALAND, geometry)

df <- sf::st_join(states, df, join = sf::st_intersects, left = FALSE)

df <- dplyr::summarise(df, n = dplyr::n(), .by = c(name, land_area, geometry))

dplyr::setdiff(df$name, state.name)

df <- dplyr::filter(
  df,
  !name %in% c("American Samoa", "Guam", "United States Virgin Islands")
)

df <- tigris::shift_geometry(df)

df <- dplyr::mutate(
  df,
  label = dplyr::case_when(
    dplyr::between(n, 0, 250) ~ "< 250",
    dplyr::between(n, 250, 500) ~ "250 - 500",
    dplyr::between(n, 500, 1000) ~ "500 - 1000",
    dplyr::between(n, 1000, 1500) ~ "1000 - 1500",
  )
)

ragg::agg_png("plot.png", width = 6, height = 4, units = "in", res = 300)

showtext::showtext_auto()

showtext::showtext_opts(dpi = 300)

sysfonts::font_add_google("EB Garamond")

p <- ggplot(df, aes(fill = label)) +
  geom_sf(color = "grey40", linewidth = 1 / 3) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_manual(
    values = c(
      "< 250" = "#FFFFFF",
      "250 - 500" = "#C4E2FF",
      "500 - 1000" = "#7DBFFF",
      "1000 - 1500" = "#1E90FF"
    ),
    breaks = c(
      "< 250",
      "250 - 500",
      "500 - 1000",
      "1000 - 1500"
    )
  ) +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(
      fill = "grey90",
      color = "grey90"
    ),
    plot.background = ggplot2::element_rect(
      fill = "grey90",
      color = "grey90"
    ),
    plot.margin = ggplot2::margin(0.25, 0.25, 0.375, 0.25, unit = "in"),
    plot.title = ggplot2::element_text(face = "bold", size = 24),
    plot.title.position = "plot",
    text = ggplot2::element_text(
      family = "EB Garamond",
      size = 12,
      color = "grey10"
    )
  ) +
  ggplot2::labs(
    title = "Florida has the most roundabouts\nof any US state",
    fill = "Number of\nroundabouts"
  )


print(p)

utilities::add_author_attribution(p)

utilities::add_source_attribution(
  p,
  "The {roundabouts} package by Emil Hvitfeldt"
)


dev.off()
