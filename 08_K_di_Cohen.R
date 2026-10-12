df <- dati_plots |> 
  unnest(cols = c(data)) |> 
  dplyr::select(autore, var1, var2, var1_steat, var2_steat) |> 
  mutate(confronto = paste0(var1, " vs ", var2)) |>
  relocate(confronto, .after = autore) |> 
  group_by(autore, confronto) |> 
  mutate(across(c(var1_steat, var2_steat), ~ {
    cond <- (max(var1_steat, na.rm = TRUE) == 4 & max(var2_steat, na.rm = TRUE) == 3) | 
      (max(var1_steat, na.rm = TRUE) == 3 & max(var2_steat, na.rm = TRUE) == 4)
    
    case_when(
      cond & .x == 4 ~ 3,
      TRUE           ~ .x
    )
  })) |> 
  ungroup() |> 
  nest(data = -c(autore, confronto, var1, var2)) |>
  mutate(data_as_factor = map(.x = data,
                           .f = ~ ..1 |>
                             dplyr::mutate(across(c(var1_steat, var2_steat), \(x) factor(x, levels = 0:max(x, na.rm = TRUE), ordered = TRUE))))) |> 
  dplyr::select(-data) |> 
  mutate(kappa = map(.x = data_as_factor,
                     .f = ~ .x |> 
                       tibble_to_confmat(col1 = var1_steat, col2 = var2_steat) |>
                       kappa_single_output()
                     )) |> 
  tidyr::crossing(tibble(alpha = c(0.05, 0.01))) |>
  relocate(alpha, .after = confronto) |>
  mutate(
    ci_bounds = map2(data_as_factor, alpha, ~ get_bca_ci_fast(.x, col1 = var1_steat, col2 = var2_steat, .y, R = 2000))
  ) |>
  unnest(ci_bounds)

df_to_export <- df |> 
  mutate(kappa = unlist(kappa)) |> 
  dplyr::select(autore, confronto, alpha, kappa, ci_lower, ci_upper) |> 
  mutate(across(c(kappa, ci_lower, ci_upper), ~ round(x = .x, digits = 3)))

# Esportazione
openxlsx::write.xlsx(
  x = df_to_export,
  file = file.path(output, "Kappa di Cohen con IC.xlsx"),
  rowNames = TRUE
)
