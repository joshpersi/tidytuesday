df <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-11/who_tb_data.csv'
)

response_columns <- base::c(
  "c_cdr",
  "c_newinc_100k",
  "cfr",
  "e_inc_100k",
  "e_inc_num",
  "e_mort_100k",
  "e_mort_exc_tbhiv_100k",
  "e_mort_exc_tbhiv_num",
  "e_mort_num",
  "e_mort_tbhiv_100k",
  "e_mort_tbhiv_num",
  "e_pop_num"
)

# plots <- purrr::map(
#   response_columns,
#   \(response) {
#     ggplot2::ggplot(df, ggplot2::aes(year, !!rlang::sym(response))) +
#       ggplot2::geom_line(ggplot2::aes(group = country)) +
#       ggplot2::geom_smooth(color = "blue", method = "lm") +
#       ggplot2::geom_smooth(color = "red", method = "loess")
#   }
# )

# patchwork::wrap_plots(plots)

ragg::agg_png(
  "plot.png",
  width = 6,
  height = 6,
  units = "in",
  res = 300
)

top_2_countries_names <- df |>
  dplyr::filter(year == 2000) |>
  dplyr::slice_max(e_mort_tbhiv_num, n = 2) |>
  dplyr::pull(country)

top_2_countries <- dplyr::filter(df, country %in% top_2_countries_names)

all_other_countries <- dplyr::filter(df, !country %in% top_2_countries_names)

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)
sysfonts::font_add_google("Domine")

p <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = all_other_countries,
    ggplot2::aes(x = year, y = e_mort_tbhiv_num, group = country),
    color = "grey80",
    size = 1
  ) +
  ggplot2::geom_line(
    data = top_2_countries,
    ggplot2::aes(
      x = year,
      y = e_mort_tbhiv_num,
      color = country,
      group = country
    ),
    size = 2
  ) +
  ggplot2::annotate(
    "label",
    x = 2003.5,
    y = 2.1e5,
    label = "India",
    family = "Domine",
    fontface = "bold",
    fill = "#f2ced7",
    linewidth = 1,
    label.padding = ggplot2::unit(0.5, "lines"),
    border.colour = "#df5165"
  ) +
  ggplot2::annotate(
    "label",
    x = 2010.75,
    y = 1.8e5,
    label = "South Africa",
    family = "Domine",
    fontface = "bold",
    fill = "#dee7ec",
    linewidth = 1,
    label.padding = ggplot2::unit(0.5, "lines"),
    border.colour = "#799fb4"
  ) +
  ggplot2::labs(
    title = "Fewer Indians and South Africans\nare dying of tuberculosis",
    subtitle = glue::trim(
      "The number of deaths from tuberculosis in all forms, excluding HIV, 
      has declined in India by 94% and in South Africa by 75% 
      from 2000 to 2023"
    ),
    caption = "This graphic is part of TidyTuesday, a weekly social data project, and may contain errors",
    x = "Year",
    y = "Estimated number of deaths"
  ) +
  ggplot2::scale_color_manual(
    values = base::c(
      "India" = "#df5165",
      "South Africa" = "#799fb4"
    )
  ) +
  ggplot2::scale_x_continuous(
    expand = ggplot2::expansion()
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = base::c(0, 0.1)),
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 0.1, unit = "in")
    ),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 0.1, unit = "in")
    ),
    legend.position = "none",
    plot.caption.position = "plot",
    plot.caption = ggplot2::element_text(hjust = 0, color = "grey50"),
    plot.margin = ggplot2::margin(0.25, 0.10, 0.375, 0.10, unit = "in"),
    plot.title.position = "plot",
    plot.title = ggplot2::element_text(face = "bold", size = 16),
    text = ggplot2::element_text(family = "Domine"),
  )

print(p)

source_caption <- grid::textGrob(
  label = bquote(
    bold("Data") *
      ":" ~ "World Health Organization"
  ),
  x = grid::unit(0.10, "in"),
  y = grid::unit(0.03, "npc"),
  hjust = 0,
  vjust = 1,
  gp = grid::gpar(fontfamily = "Domine", fontsize = 10)
)

grid::grid.draw(source_caption)

author_caption <- grid::textGrob(
  label = bquote(
    bold("Graphic") *
      ":" ~ "Josh Persi"
  ),
  x = grid::unit(5.90, "in"),
  y = grid::unit(0.03, "npc"),
  hjust = 1,
  vjust = 1,
  gp = grid::gpar(fontfamily = "Domine", fontsize = 10)
)

grid::grid.draw(author_caption)

dev.off()
