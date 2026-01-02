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

ggplot2::ggplot(
  df,
  ggplot2::aes(xmin = birthdate, xmax = deathdate, y = author)
) +
  ggplot2::geom_linerange()

df |>
  tidytext::unnest_tokens(word, text) |>
  dplyr::anti_join(tidytext::stop_words, by = "word") |>
  dplyr::summarize(n = dplyr::n(), .by = c(gutenberg_id, title, word)) |>
  dplyr::slice_max(
    order_by = n,
    n = 10,
    by = gutenberg_id,
    with_ties = FALSE
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = n,
    y = tidytext::reorder_within(word, n, title)
  )) +
  ggplot2::facet_wrap(
    ~title,
    nrow = 5,
    ncol = 8,
    scales = "free",
    labeller = ggplot2::as_labeller(\(x) stringr::str_wrap(x, width = 30))
  ) +
  ggplot2::geom_col(fill = "grey70", color = "grey30") +
  ggplot2::geom_text(
    ggplot2::aes(
      x = 0,
      y = tidytext::reorder_within(word, n, title),
      label = word
    ),
    hjust = 0,
    vjust = 0.5
  ) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion()) +
  tidytext::scale_y_reordered(expand = ggplot2::expansion()) +
  ggplot2::theme_void() +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(1, 1, 1, 1, unit = "cm"),
    strip.text = ggplot2::element_text(hjust = 0, vjust = 0)
  )

df |>
  tidytext::unnest_tokens(word, text) |>
  dplyr::anti_join(tidytext::stop_words, by = "word") |>
  dplyr::summarize(n = dplyr::n_distinct(word), .by = title) |>
  ggplot2::ggplot(ggplot2::aes(x = n, y = 0)) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_segment(
    data = tibble(ticks = seq(700, 9000, by = 500)),
    aes(x = ticks, xend = ticks, y = -0.025, yend = 0.025)
  ) +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(limits = c(-1, 1)) +
  ggplot2::theme_void()

df |>
  tidytext::unnest_tokens(word, text) |>
  dplyr::anti_join(tidytext::stop_words, by = "word") |>
  dplyr::summarize(n = dplyr::n_distinct(word), .by = title) |>
  ggplot2::ggplot() +
  ggplot2::geom_density(aes(n)) +
  ggplot2::geom_rug(aes(n)) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion())

df |>
  tidytext::unnest_tokens(word, text) |>
  dplyr::anti_join(tidytext::stop_words, by = "word") |>
  dplyr::count(title, word, sort = TRUE) |>
  tidytext::bind_tf_idf(word, title, n) |>
  dplyr::slice_max(order_by = n, n = 10, by = title, with_ties = FALSE) |>
  ggplot2::ggplot(ggplot2::aes(
    x = tf_idf,
    y = tidytext::reorder_within(word, tf_idf, title)
  )) +
  ggplot2::facet_wrap(
    ~title,
    nrow = 5,
    ncol = 8,
    scales = "free",
    labeller = ggplot2::as_labeller(\(x) stringr::str_wrap(x, width = 30))
  ) +
  ggplot2::geom_col(fill = "grey70", color = "grey30") +
  ggplot2::geom_text(
    ggplot2::aes(
      x = 0,
      y = tidytext::reorder_within(word, tf_idf, title),
      label = word
    ),
    hjust = 0,
    vjust = 0.5
  ) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion()) +
  tidytext::scale_y_reordered(expand = ggplot2::expansion()) +
  ggplot2::theme_void() +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(1, 1, 1, 1, unit = "cm"),
    strip.text = ggplot2::element_text(hjust = 0, vjust = 0)
  )

ggplot(df, aes(birthdate)) +
  geom_histogram(binwidth = 10) +
  theme_light() +
  scale_y_continuous(
    breaks = 0:11,
    labels = scales::label_number(accuracy = 1),
    expand = expansion(mult = c(0, 0.1))
  )

df |>
  tidytext::unnest_tokens(word, text) |>
  dplyr::anti_join(tidytext::stop_words, by = "word")
dplyr::count(title, word, sort = TRUE) |>
  dplyr::inner_join(
    tidytext::get_sentiments("nrc") |> filter(sentiment == "joy"),
    by = "word"
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

df |>
  tidytext::unnest_tokens(word, text) |>
  dplyr::anti_join(tidytext::stop_words, by = "word") |>
  dplyr::count(deathdate, title, word, sort = TRUE) |>
  dplyr::left_join(
    tidytext::get_sentiments("nrc") |> filter(sentiment == "surprise"),
    by = "word"
  ) |>
  dplyr::summarize(
    num_words = sum(n),
    .by = c(deathdate, title, sentiment)
  ) |>
  arrange(title, sentiment) |>
  tidyr::pivot_wider(names_from = sentiment, values_from = num_words) |>
  dplyr::mutate(prop = surprise / (surprise + `NA`), .keep = "unused") |>
  dplyr::mutate(decade = (deathdate %/% 10) * 10) |>
  dplyr::summarize(mean_prop = mean(prop), n = n(), .by = decade) |>
  ggplot(aes(decade, mean_prop)) +
  geom_point() +
  geom_label(aes(x = decade, y = mean_prop, label = n))
