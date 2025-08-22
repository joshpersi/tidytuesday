df <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-19/scottish_munros.csv'
)

df <- dplyr::filter(df, !is.na(Name))

df <- dplyr::filter(
  df,
  !dplyr::if_any(`1891`:`2021`, is.na)
)

df <- dplyr::select(df, `1891`:`2021`)

df <- tidyr::pivot_longer(
  df,
  dplyr::everything(),
  names_to = "year",
  values_to = "classification"
)

df <- dplyr::summarize(df, n = dplyr::n(), .by = c(year, classification))

df <- dplyr::arrange(df, year, classification)

ggplot2::ggplot(df, ggplot2::aes(year, n, fill = classification)) +
  ggplot2::geom_col(position = "fill")
