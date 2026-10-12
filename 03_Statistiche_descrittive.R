# Statitiche di sintesi di variabili discrete
statistiche_descrittive_var_discrete <- data_clean |> 
  selezione_variabili_discrete() |> 
  pivot_longer(cols = everything(), names_to = "name", values_to = "value") |> 
  group_by(name, value) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  group_by(name) |> 
  mutate(percentuale = round(n/sum(n),digits = 2)) |> 
  mutate(across(c(value, n), ~ as.integer(.x))) |> 
  arrange(name, value)

# Statistiche di sintesi di variabili continue
statistiche_descrittive_var_continue <- data_clean |> 
  selezione_variabili_continue() |> 
  summarise(across(everything(), descr_stats)) |> 
  pivot_longer(cols = everything(), names_to = "name", values_to = "value") |> 
  mutate(var = str_sub(string = name, end = - 1 + stringi::stri_locate_last_fixed(str = name, pattern = "_")[,2]),
         stat = str_sub(string = name, start = 1 + stringi::stri_locate_last_fixed(str = name, pattern = "_")[,2])) |> 
  dplyr::select(-name) |> 
  pivot_wider(names_from = stat, values_from = value) |> 
  rename(second_quartile = secondquartile,
         fourth_quartile = fourthquartile) |> 
  relocate(second_quartile, median, fourth_quartile, .after = var)

# Esportazione
sheets <- list("Stat descr per var continue" = statistiche_descrittive_var_continue,
               "Stat descr per var discrete" = statistiche_descrittive_var_discrete)

sheets <- purrr::map(sheets, ~ as.data.frame(.x))

writexl::write_xlsx(
  x = sheets,
  path = file.path(output,"Tabelle descrittive.xlsx")
)
