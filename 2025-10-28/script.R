library(ggplot2)

sysfonts::font_add(
  family = "Font Awesome 7 Brands",
  regular = "Font Awesome 7 Brands-Regular-400.otf"
)

showtext::showtext_auto()

showtext::showtext_opts(dpi = 300)

df <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-28/prizes.csv'
)

df <- dplyr::filter(
  df,
  prize_name == "Baillie Gifford Prize for Non-Fiction"
)

df <- dplyr::filter(
  df,
  gender %in% base::c("man", "woman")
)

df <- dplyr::summarize(
  df,
  n = dplyr::n(),
  .by = c(prize_year, gender)
)

df <- tidyr::pivot_wider(
  df,
  id_cols = prize_year,
  names_from = gender,
  values_from = n,
  values_fill = 0
)

df <- dplyr::mutate(df, diff = woman - man)

social_caption <- base::c(
  "
  <span style='font-family:\"Font Awesome 7 Brands\";'>&#xf09b;</span>
  <span style='color: #6c757d'>joshpersi</span>
  <span>&nbsp;</span>
  <span style='font-family:\"Font Awesome 7 Brands\";'>&#xe671;</span>
  <span style='color: #6c757d'>@joshpersi</span>
  "
)

a <- ggplot(df, aes(prize_year, diff)) +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_point() +
  geom_smooth(linewidth = 2, se = FALSE, color = "#7C109A") +
  labs(
    x = "Prize year",
    y = "Gender balance",
    title = "More woman have been nominated for the\nBaillie Gifford Prize for Non-Fiction in recent years",
    caption = social_caption
  ) +
  theme_light() +
  theme(
    text = element_text(family = "Segoe UI"),
    plot.caption = ggtext::element_textbox_simple()
  )

b <- ggplot() +
  annotate(
    "segment",
    x = 1,
    xend = 1,
    y = -1,
    yend = 1,
    arrow = arrow(length = unit(0.3, "cm"), ends = "both")
  ) +
  annotate(
    "text",
    x = 1,
    y = 1.1,
    label = "More\nwomen",
    size = 2
  ) +
  annotate(
    "text",
    x = 1,
    y = -1.1,
    label = "More\nmen",
    size = 2
  ) +
  theme_void()

library(patchwork)

layout <- c(
  area(t = 1, l = 1, b = 1, r = 90), # Main plot
  area(t = 1, l = 91, b = 1, r = 100) # Smaller plot
)

p <- a +
  b +
  plot_layout(
    design = layout,
    widths = c(90, 5)
  )


ggplot2::ggsave(
  "plot.png",
  p,
  width = 5,
  height = 4,
  units = "in"
)
