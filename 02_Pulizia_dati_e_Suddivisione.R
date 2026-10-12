# Pulizia
data_clean <- data_orig |> 
  dplyr::filter(!is.na(Nome))  |>  
  dplyr::select(-c(Cognome, Nome, `Codice identificativo (CO-NO 001)`, `Data esami`, `Data di nascita`)) |>
  dplyr::mutate(maschio = ifelse(test = sesso == "M", yes = 1, no = 0)) |> 
  dplyr::select(c(maschio, eta,
                  pdff_roi_media_1 : pdff_roi_media_5, pdff_roi_sd_1 : pdff_roi_sd_5,
                  pdff_media, pdff_sd, pdff_sdbar, pdff_steat,
                  mref_roi_sd_1 : mref_roi_sd_4,
                  mref_media, mref_sd, mref_sdbar, mref_steat,
                  # mref_fibr, mref_infl, mref_ste_media, mref_cs_media, mref_vi_media,
                  fll_roi_sd_1 : fll_roi_sd_5,
                  fll_media, fll_sd, fll_sdbar, fll_steat)) |> 
  dplyr::mutate(pdff_steat_wangK = diagnosi(x = pdff_media, cutoff = dati_cutoff_orig[["Wang K. et al. (2026)"]]$pdff),
                mref_steat_wangK = diagnosi(x = mref_media, cutoff = dati_cutoff_orig[["Wang K. et al. (2026)"]]$eco),
                fll_steat_wangK = diagnosi(x = fll_media, cutoff = dati_cutoff_orig[["Wang K. et al. (2026)"]]$eco),
                pdff_steat_dag = diagnosi(x = pdff_media, cutoff = dati_cutoff_orig[["Dag et al. (2025)"]]$pdff),
                mref_steat_dag = diagnosi(x = mref_media, cutoff = dati_cutoff_orig[["Dag et al. (2025)"]]$eco),
                fll_steat_dag = diagnosi(x = fll_media, cutoff = dati_cutoff_orig[["Dag et al. (2025)"]]$eco),
                pdff_steat_wangM = diagnosi(x = pdff_media, cutoff = dati_cutoff_orig[["Wang M. et al. (2024)"]]$pdff),
                mref_steat_wangM = diagnosi(x = mref_media, cutoff = dati_cutoff_orig[["Wang M. et al. (2024)"]]$eco),
                fll_steat_wangM = diagnosi(x = fll_media, cutoff = dati_cutoff_orig[["Wang M. et al. (2024)"]]$eco),
                pdff_steat_yang = diagnosi(x = pdff_media, cutoff = dati_cutoff_orig[["Yang et al. (2023)"]]$pdff),
                mref_steat_yang = diagnosi(x = mref_media, cutoff = dati_cutoff_orig[["Yang et al. (2023)"]]$eco),
                fll_steat_yang = diagnosi(x = fll_media, cutoff = dati_cutoff_orig[["Yang et al. (2023)"]]$eco)) |> 
  mutate(across(everything(), ~ as.numeric(.x)))

# Suddivisione
dati_medie <- data_clean |> 
  dplyr::select(ends_with("_media"))

dati_sd <- data_clean |> 
  dplyr::select(ends_with("_sd"))

dati_cv <- data_clean |> 
  dplyr::select(ends_with("_cv"))

dati_sdbar <- data_clean |> 
  dplyr::select(ends_with("_sdbar"))

dati_steat <- data_clean |> 
  dplyr::select(ends_with("_steat"))

coordinate <- tibble(var1 = c("pdff", "pdff", "fll"),
                     var2 = c("mref", "fll", "mref"),
                     name_x = c("PDFF % Average", "PDFF % Average", "USAT (FLL) Average (dB/cm/MHz)"),
                     name_y = c("USAT (M−Ref.) Average (dB/cm/MHz)", "USAT (FLL) Average (dB/cm/MHz)", "USAT (M−Ref.) Average (dB/cm/MHz)"),
                     title = c("USAT (M-Ref.) vs PDFF", "USAT (FLL) vs PDFF", "USAT (M-Ref.) vs USAT (FLL)"))

dati_plots <- data_clean |> 
  dplyr::select(pdff_media, mref_media, fll_media,
                pdff_steat_vr = pdff_steat, mref_steat_vr = mref_steat, fll_steat_vr = fll_steat,
                pdff_steat_wangK, mref_steat_wangK, fll_steat_wangK,
                pdff_steat_dag, mref_steat_dag, fll_steat_dag,
                pdff_steat_wangM, mref_steat_wangM, fll_steat_wangM,
                pdff_steat_yang, mref_steat_yang, fll_steat_yang) |> 
  mutate(id = dplyr::row_number()) |> 
  relocate(id) |> 
  pivot_longer(
    cols = contains("steat"),
    names_to = c(".value", "autore"),
    names_pattern = "(.*_steat)_(.*)"
  ) |> 
  mutate(autore = case_when(autore == "dag" ~ "Dag et al. (2025)",
                            autore == "vr" ~ "Verona (2026)",
                            autore == "wangK" ~ "Wang K. et al. (2026)",
                            autore == "wangM" ~ "Wang M. et al. (2024)",
                            autore == "yang" ~ "Yang et al. (2023)")) |> 
  tidyr::crossing(coordinate) |>
  mutate(var1_media = dplyr::recode_values(var1,
                                 "pdff" ~ pdff_media,
                                 "mref" ~ mref_media,
                                 "fll" ~ fll_media),
         var2_media = dplyr::recode_values(var2,
                                 "pdff" ~ pdff_media,
                                 "mref" ~ mref_media,
                                 "fll" ~ fll_media),
         var1_steat = dplyr::recode_values(var1,
                                 "pdff" ~ pdff_steat,
                                 "mref" ~ mref_steat,
                                 "fll" ~ fll_steat),
         var2_steat = dplyr::recode_values(var2,
                                 "pdff" ~ pdff_steat,
                                 "mref" ~ mref_steat,
                                 "fll" ~ fll_steat),
         var1_cutoff = ifelse(test = var1 == "pdff", yes = var1, no = "eco"),
         var2_cutoff = ifelse(test = var2 == "pdff", yes = var2, no = "eco")) |> 
  group_by(autore, title) |>
  mutate(color_col = color_fun(var1_steat, var2_steat)) |>
  ungroup() |> 
  tidyr::nest(data = - c(autore, title, name_x, name_y, var1, var2, var1_cutoff, var2_cutoff)) |> 
  mutate(title = paste0(title, " - ", autore))

# dati_cutoff_vr <- list(pdff = dati_cutoff_orig$pdff_vr,
#                        eco = dati_cutoff_orig$eco_vr)
# 
# dati_cutoff_wangK <- list(pdff = dati_cutoff_orig$pdff_vr,
#                           eco = dati_cutoff_orig$eco_wangK)
# 
# dati_cutoff_dag <- list(pdff = dati_cutoff_orig$pdff_dag,
#                         eco = dati_cutoff_orig$eco_dag)
# 
# dati_cutoff_wangM <- list(pdff = dati_cutoff_orig$pdff_vr,
#                           eco = dati_cutoff_orig$eco_wangM)
# 
# nomi_cutoff_vr <- list(x_axis = "PDFF % Average",
#                        y_axis = "USAT (M-REF) Average (dB/cm/MHz)",
#                        title = "USAT (M-REF) vs PDFF")
