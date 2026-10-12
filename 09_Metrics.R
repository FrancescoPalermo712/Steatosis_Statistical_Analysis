data_metrics <- df %>%
  dplyr::select(autore, confronto, var1, var2, data_as_factor) %>%
  dplyr::filter(var1 == "pdff") %>%
  dplyr::mutate(
    all_metrics = purrr::map(
      .x = data_as_factor,
      .f = ~ get_metrics(data = .x, eco_col = var2_steat, pdff_col = var1_steat)
    )
  ) |>
  dplyr::select(-data_as_factor) %>%
  tidyr::unnest(cols = all_metrics)

print(final_results_table)