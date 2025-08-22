df <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-19/scottish_munros.csv'
)

df <- dplyr::filter(df, !is.na(Name))

p <- ggplot2::ggplot(df, ggplot2::aes(Height_m)) +
  ggplot2::geom_histogram(bins = 30) +
  ggplot2::theme_light() +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion()) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = base::c(0, 0.1))
  ) +
  ggplot2::labs(
    title = "Most Scotish munros are smaller than 1000 m tall",
    x = "Height (m)",
    y = "Number of munros"
  ) +
  ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_blank()
  )

curl::curl_download(
  "https://upload.wikimedia.org/wikipedia/commons/thumb/1/10/Flag_of_Scotland.svg/1920px-Flag_of_Scotland.svg.png",
  destfile = "scottish_flag.png"
)

scottish_flag <- png::readPNG("scottish_flag.png")

png("plot.png", width = 1920, height = 1152, res = 300)

grid::grid.draw(grid::rasterGrob(scottish_flag))

print(p)

dev.off()
