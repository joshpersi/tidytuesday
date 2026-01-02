# Code is very messy
christmas_novel_authors <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-30/christmas_novel_authors.csv'
)

christmas_novel_text <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-30/christmas_novel_text.csv'
)

christmas_novels <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-30/christmas_novels.csv'
)

df <- christmas_novel_text |>
  tidyr::drop_na() |>
  dplyr::summarize(
    text = stringr::str_c(text, collapse = " | "),
    .by = gutenberg_id
  ) |>
  dplyr::left_join(
    dplyr::distinct(christmas_novels, gutenberg_id, .keep_all = TRUE),
    by = "gutenberg_id"
  ) |>
  dplyr::left_join(christmas_novel_authors, by = "gutenberg_author_id")

sysfonts::font_add_google("Berkshire Swash")
sysfonts::font_add_google("Atkinson Hyperlegible")

utilities::setup_showtext()

ragg::agg_png(
  filename = "plot.png",
  width = 6,
  height = 6,
  units = "in",
  res = 300
)

p <- df |>
  tidytext::unnest_tokens(word, text) |>
  dplyr::anti_join(tidytext::stop_words, by = "word") |>
  dplyr::count(deathdate, title, word, sort = TRUE) |>
  dplyr::left_join(
    tidytext::get_sentiments("nrc") |> filter(sentiment == "joy"),
    by = "word"
  ) |>
  dplyr::summarize(
    num_words = sum(n),
    .by = c(title, sentiment)
  ) |>
  arrange(title, sentiment) |>
  tidyr::pivot_wider(names_from = sentiment, values_from = num_words) |>
  dplyr::mutate(prop = joy / (joy + `NA`), .keep = "unused") |>
  dplyr::arrange(desc(prop)) |>
  slice(c(1:5)) |>
  ggplot(aes(prop, reorder(title, prop))) +
  geom_col(fill = "#bc4749", color = "#386641") +
  geom_text(
    aes(
      0,
      reorder(title, prop),
      label = stringr::str_wrap(title, width = 30)
    ),
    position = position_nudge(x = 0.0025),
    hjust = 0,
    vjust = 0.5,
    family = "Atkinson Hyperlegible",
    color = "#f2e8cf",
    fontface = "bold"
  ) +
  labs(
    y = NULL,
    x = "Proportion of words associated with the feeling of joy",
    title = "The Joy of Christmas",
    subtitle = "The top five most joyous Christmas novels based on the proportion\nof their words associated with the feeling of joy"
  ) +
  scale_y_discrete(expand = expansion()) +
  scale_x_continuous(
    breaks = c(0, 0.05, 0.10, 0.15),
    limits = c(0, 0.15),
    expand = expansion(),
    labels = scales::label_percent()
  ) +
  ggplot2::theme_light() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),
    axis.title.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(
      fill = "#f2e8cf"
    ),
    plot.background = ggplot2::element_rect(
      fill = "#f2e8cf",
      color = "#f2e8cf"
    ),
    panel.grid = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(hjust = 0),
    plot.caption.position = "plot",
    plot.margin = ggplot2::margin(0.25, 0.40, 0.375, 0.25, unit = "in"),
    plot.title = ggplot2::element_text(face = "bold", size = 24),
    plot.subtitle = ggplot2::element_text(
      family = "Atkinson Hyperlegible"
    ),
    plot.title.position = "plot",
    text = ggplot2::element_text(
      family = "Berkshire Swash",
      size = 12,
      color = "#386641"
    )
  )

print(p)

caption <- grid::textGrob(
  label = bquote(bold("Data") * ":" ~ .("Project Gutenberg")),
  x = grid::unit(0.25, "in"),
  y = grid::unit(0.15, "in"),
  hjust = 0,
  vjust = 1,
  gp = grid::gpar(
    fontfamily = "Berkshire Swash",
    fontsize = 10,
    col = "#386641"
  )
)


grid::grid.draw(caption)

caption <- grid::textGrob(
  label = bquote(bold("Graphic") * ":" ~ .("Josh Persi")),
  x = grid::unit(5.6, "in"),
  y = grid::unit(0.15, "in"),
  hjust = 1,
  vjust = 1,
  gp = grid::gpar(
    fontfamily = "Berkshire Swash",
    fontsize = 10,
    col = "#386641"
  )
)

grid::grid.draw(caption)


dev.off()
