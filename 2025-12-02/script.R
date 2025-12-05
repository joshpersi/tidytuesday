df <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-02/sechselaeuten.csv'
)

df <- dplyr::filter(df, year >= 1950)

temp_recipe <- recipes::recipe(tre200m0 ~ year, data = df) |>
  recipes::step_poly(year, degree = 2)

temp_spec <- parsnip::linear_reg() |>
  parsnip::set_engine("lm")

temp_workflow <- workflows::workflow() |>
  workflows::add_model(temp_spec) |>
  workflows::add_recipe(temp_recipe)

temp_fit <- temp_workflow |>
  parsnip::fit(data = df)

predictions <- parsnip::augment(temp_fit, df)

ragg::agg_png(
  "plot.png",
  width = 6,
  height = 6,
  units = "in",
  res = 300
)

utilities::setup_showtext()

p <- ggplot2::ggplot(df) +
  ggplot2::geom_point(
    ggplot2::aes(year, tre200m0),
    color = "grey40"
  ) +
  ggplot2::geom_line(
    data = predictions,
    ggplot2::aes(year, .pred),
    color = "#1d3557",
    linewidth = 3,
    lineend = "round"
  ) +
  ggplot2::theme_light() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
    panel.background = ggplot2::element_rect(
      fill = "#f1faee"
    ),
    plot.background = ggplot2::element_rect(
      fill = "#f1faee",
      color = "#f1faee"
    ),
    plot.caption = ggplot2::element_text(hjust = 0),
    plot.caption.position = "plot",
    plot.margin = ggplot2::margin(0.25, 0.25, 0.375, 0.25, unit = "in"),
    plot.title = ggplot2::element_text(face = "bold", size = 24),
    plot.title.position = "plot",
    text = ggplot2::element_text(
      family = "Noto Sans",
      size = 12,
      color = "#001219"
    )
  ) +
  ggplot2::labs(
    x = "Year",
    y = "Surface air temperature (°C)",
    title = "Zurich's Surface Air Temperature\nTrends Since 1950",
  )

base::print(p)

utilities::add_source_attribution(p, "OpenData for Zurich's Sechselaeuten")

utilities::add_author_attribution(p)

grDevices::dev.off()
