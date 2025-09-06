library(ggplot2)

frogID_data <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frogID_data.csv'
)
frog_names <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frog_names.csv'
)


top_10 <- frogID_data |>
  count(scientificName, sort = TRUE) |>
  head(10) |>
  pull(scientificName)

frogID_data <- dplyr::mutate(
  frogID_data,
  month = lubridate::month(eventDate),
  week = lubridate::week(eventDate)
)

plotting_data <- frogID_data |>
  dplyr::filter(scientificName %in% top_10) |>
  dplyr::summarize(count = n(), .by = week)


p <- ggplot(plotting_data, aes(week, count)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  scale_y_log10() +
  theme_light() +
  labs(
    title = "Observations of Australian frogs",
    x = "Week of the year",
    y = "Number of observations"
  )

ggplot2::ggsave(
  "plot.png",
  p,
  width = 5,
  height = 4,
  units = "in"
)
