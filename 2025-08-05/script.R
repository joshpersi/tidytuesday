library(dplyr)
library(ggimage)
library(ggplot2)
library(ggtext)
library(showtext)

sysfonts::font_add_google("DM Serif Text", "dm_serif_text")

showtext::showtext_auto()

income_inequality_processed <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-05/income_inequality_processed.csv'
)

df <- income_inequality_processed |>
  dplyr::group_by(Entity) |>
  dplyr::slice_max(Year) |>
  dplyr::ungroup()

points <- df |>
  dplyr::arrange(gini_dhi_eq) |>
  dplyr::mutate(ecdf = stats::ecdf(gini_dhi_eq)(gini_dhi_eq)) |>
  dplyr::filter(
    Entity %in%
      base::c(
        "Canada",
        "Japan",
        "France",
        "Germany",
        "Italy",
        "United States",
        "United Kingdom"
      )
  ) |>
  dplyr::arrange(Code)

points <- dplyr::bind_cols(
  points,
  tibble::tibble(
    image = base::c(
      "www/icons8-canada-96.png",
      "www/icons8-germany-96.png",
      "www/icons8-france-96.png",
      "www/icons8-great-britain-96.png",
      "www/icons8-italy-96.png",
      "www/icons8-japan-96.png",
      "www/icons8-usa-96.png"
    )
  )
)

p <- ggplot2::ggplot(df, ggplot2::aes(gini_dhi_eq)) +
  ggplot2::stat_ecdf(geom = "step") +
  ggimage::geom_image(
    mapping = ggplot2::aes(
      gini_dhi_eq,
      ecdf,
      image = image
    ),
    data = points
  ) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.01)) +
  ggplot2::theme_light() +
  ggplot2::theme(
    text = ggplot2::element_text(
      family = "dm_serif_text",
      size = 32,
      lineheight = 1 / 3
    ),
    plot.caption = ggtext::element_markdown(
      color = "grey20",
      size = 16,
      hjust = 0
    ),
    plot.subtitle = ggplot2::element_text(color = "grey20", size = 24),
    plot.margin = ggplot2::margin(0.25, 0.5, 0.25, 0.5, unit = "cm")
  ) +
  ggplot2::labs(
    x = "Gini coefficient",
    y = "Cumulative Frequency",
    title = "Most G7 nations have moderate inequality",
    subtitle = "The Gini coefficient measures inequality on a scale from 0 to 1. Higher values indicate higher inequality.\nInequality is measured here in terms of income before and after taxes and benefits.",
    caption = "<b>Data source</b>: Luxembourg Income Study (2025)"
  )


ggplot2::ggsave(
  "plot.png",
  p,
  width = 16,
  height = 9,
  units = "cm",
  bg = "#FFFFFF"
)
