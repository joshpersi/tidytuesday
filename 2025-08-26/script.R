library(dplyr)
library(ggplot2)
library(ggtext)
library(lubridate)
library(showtext)

sysfonts::font_add(
  family = "Font Awesome 7 Brands",
  regular = "Font Awesome 7 Brands-Regular-400.otf"
)

sysfonts::font_add_google("Pacifico", "pacifico")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

billboard <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-26/billboard.csv'
)
topics <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-26/topics.csv'
)

df <- dplyr::mutate(
  billboard,
  songwriter_gender = dplyr::case_match(
    songwriter_male,
    1 ~ "Male",
    2 ~ "Female"
  )
)

social_caption <- base::c(
  "
  <span style='font-family:\"Font Awesome 7 Brands\";'>&#xf09b;</span>
  <span style='color: #6c757d'>joshpersi</span>
  <span>&nbsp;</span>
  <span style='font-family:\"Font Awesome 7 Brands\";'>&#xe671;</span>
  <span style='color: #6c757d'>@joshpersi</span>
  "
)

df <- df |>
  dplyr::filter(songwriter_gender %in% base::c("Male", "Female")) |>
  dplyr::group_by(decade = lubridate::year(date) %/% 10 * 10) |>
  dplyr::count(songwriter_gender, name = "num_songwriters")

df <- dplyr::mutate(
  df,
  songwriter_gender = forcats::fct_reorder(
    songwriter_gender,
    base::c("Male", "Female")
  )
)

p <- ggplot2::ggplot(
  df,
  ggplot2::aes(decade, num_songwriters, fill = songwriter_gender)
) +
  ggplot2::geom_col(position = "fill", width = 10, color = "black") +
  ggplot2::geom_hline(
    ggplot2::aes(yintercept = 0.5),
    linetype = "dashed",
    color = "#6c757d"
  ) +
  ggplot2::scale_fill_manual(
    values = base::c("Male" = "#3d405b", "Female" = "#e07a5f")
  ) +
  ggplot2::scale_x_continuous(
    breaks = base::c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020),
    labels = \(x) stringr::str_c(x, "s"),
    expand = ggplot2::expansion()
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_percent(),
    expand = ggplot2::expansion()
  ) +
  ggplot2::labs(
    title = "The rise of the female songwriter",
    subtitle = stringr::str_wrap(
      "The relative percentage of songs on the top 100 billboard written by women surpassed 50% for the first time during the 2010s",
      width = 80
    ),
    caption = social_caption,
    x = "Decade",
    y = "Relative percentage"
  ) +
  ggplot2::theme_light() +
  ggplot2::theme(
    axis.ticks = ggplot2::element_line(color = "#000000"),
    axis.line = ggplot2::element_line(color = "#000000"),
    axis.text = ggplot2::element_text(color = "#6c757d"),
    legend.position = "none",
    panel.border = ggplot2::element_rect(color = "#000000", linewidth = 1),
    plot.background = ggplot2::element_rect(fill = "#f4f1de"),
    plot.caption = ggtext::element_textbox_simple(),
    plot.caption.position = "plot",
    plot.margin = ggplot2::margin(0.1, 0.6, 0.1, 0.1, "in"),
    plot.subtitle = ggplot2::element_text(color = "#6c757d"),
    plot.title.position = "plot",
    text = ggplot2::element_text(family = "pacifico")
  )

ggplot2::ggsave(
  "plot.png",
  p,
  width = 5,
  height = 4,
  units = "in"
)
