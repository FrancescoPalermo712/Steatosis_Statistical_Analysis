# DOMANDA: M-ref cambia significativamente per grado di steatosi vero (come da risonanza)?
kruskal.test(mref_media ~ pdff_steat, data = data_clean) # P = 0
kruskal.test(fll_media ~ pdff_steat, data = data_clean)  # p = 0
# RISPOSTA: Sì (inteso che c'è almeno un cambio significativo, cioè non causale)

# DOMANDA: Cambi significativi quanti ce ne sono? In un solo passaggio? In tutti? Una via di mezzo?
pairwise.wilcox.test(x = data_clean$mref_media,
                     g = data_clean$pdff_steat,
                     p.adjust.method = "bonferroni")
pairwise.wilcox.test(x = data_clean$fll_media,
                     g = data_clean$pdff_steat,
                     p.adjust.method = "bonferroni")
# RISPOSTA: In tutti

# DOMANDA: FLL sottostima, in mediANA, rispetto a M-Ref?
BlandAltmanLeh::bland.altman.plot(dplyr::filter(data_clean, pdff_steat >= 1)$fll_media,
                                  dplyr::filter(data_clean, pdff_steat >= 1)$mref_media,
                                  two = 1.96, mode = 1, graph.sys = "ggplot2", conf.int = 0, silent = TRUE, sunflower = FALSE, geom_count = FALSE)
# RISPOSTA: Sì, la mediana è "abbastanza sotto" lo zero

# DOMANDA: FLL sottostima significativamente, in media, rispetto a M-Ref, quando c'è della steatosi? 
wilcox.test(dplyr::filter(data_clean, pdff_steat %in% c(1,2,3))$fll_media,
            dplyr::filter(data_clean, pdff_steat %in% c(1,2,3))$mref_media,
            paired = TRUE)
# RISPOSTA: sì p < 0.1

# DOMANDA: i dati seguono distribuzione Gaussiana?
data_clean |> 
  selezione_variabili_continue() |> 
  map_dbl(.f = ~ shapiro.test(.x)$p.value)  # Ovviamente no, e non ce ne importa niente
# RISPOSTA: No, nessuno

# DOMANDA: sdbar è statisticamente inferiore in M-Ref che in FLL? INTRA-ROI c'è meno variabilità in M-Ref che in FLL? (Potrebbe significare meno risoluzione nelle ROI di M-Ref rispetto ad FLL)
wilcox.test(data_clean$mref_sdbar, data_clean$fll_sdbar, paired = TRUE)
# RISPOSTA: Sì, eclatantemente e significativamente inferiore: 0.2% contro 0.9&, meno di un quarto.
#           Quindi, M-Ref fa eno errori rispetto a FLL, ma ha delle ROI più omogenee al proprio interno. (Potrebbe significare meno risoluzione nelle ROI di M-Ref rispetto ad FLL)


# DOMANDA: La RM ha risoluzione maggiore rispetto alle due ecografie (abbiamo visto prima che FLL sembrerebbe avere più risoluzione rispetto ad M-Ref)?
pdff_cv <- data_clean |>
  dplyr::select(contains("pdff_roi_media_") , contains("pdff_roi_sd_")) |> 
  mutate(id = row_number()) |> 
  pivot_longer(cols = starts_with("pdff_roi_"),
               names_pattern = "pdff_roi_(media|sd)_(\\d+)",
               names_to = c("name", "roi"),
               values_to = "value") |> 
  pivot_wider(names_from = name, values_from = value) |> 
  dplyr::select(-roi) |> 
  group_by(id) |> 
  summarise(cv_medio_pdff = mean(sd / media, na.rm = TRUE))

mref_cv <- data_clean |> 
  dplyr::select(contains("mref_roi_"), mref_media) |> 
  mutate(id = row_number()) |> 
  pivot_longer(cols = starts_with("mref_roi_"),
               names_pattern = "mref_roi_(media|sd)_(\\d+)",
               names_to = c("name", "roi"),
               values_to = "sd") |> 
  dplyr::select(-c(roi, name)) |> 
  drop_na() |> 
  group_by(id) |> 
  summarise(cv_medio_mref = mean(sd / mref_media, na.rm = TRUE))

fll_cv <- data_clean |> 
  dplyr::select(contains("fll_roi_"), fll_media) |> 
  mutate(id = row_number()) |> 
  pivot_longer(cols = starts_with("fll_roi_"),
               names_pattern = "fll_roi_(media|sd)_(\\d+)",
               names_to = c("name", "roi"),
               values_to = "sd") |> 
  dplyr::select(-c(roi, name)) |> 
  drop_na() |> 
  group_by(id) |> 
  summarise(cv_medio_fll = mean(sd / fll_media, na.rm = TRUE))

dati_cv <- pdff_cv |> 
  left_join(y = mref_cv, by = "id") |> 
  left_join(y = fll_cv, by = "id")

dati_cv |> dplyr::select(-id) |> map_dbl(.f = mean)

wilcox.test(dati_cv$cv_medio_pdff, dati_cv$cv_medio_mref, paired = TRUE)
wilcox.test(dati_cv$cv_medio_pdff, dati_cv$cv_medio_fll, paired = TRUE)






















# # DOMANDA: sd (deviazione standard delle medie fra le ROI di ciascun paziente) è statisticamente SUPERIORE in M-Ref che in FLL?
# #          EXTRA-ROI c'è PIU' variabilità in M-Ref che in FLL? Cioè, le ROI sono "MENO COERENTI" tra loro in M-Ref che in FLL?
# wilcox.test(data_clean$mref_sd, data_clean$fll_sd, paired = TRUE)
# # RISPOSTA: Sì. Per ciascuna metodica e per ciascun paziente prendiamo le 5 ROI. Di ciascuna ROI calcoliamo la media, ottenendo 5 medie. Di queste 5 medie calcoliamo la deviazione standard.
# #           Otteniamo quindi, per ciascuna metodica, una deviazione standard per ciascun paziente. Abbiamo quindi due elenchi di queste deviazioni standard, una per metodica: il test confronta
# #           appunto questi due gruppi. Risulta che la deviazione standard è più alta in M-Ref (3,7% contro 3,2%, e questa differenza è statisticamente significativa, con p-value = 0.06).
# #           Ma è rilevante?

# # DOMANDA: Calcoliamo specificità, sensibilità ed accuratezza usando come target il gruppo S1
# dati_conf <- dati_steat |>
#   mutate(across(everything(), ~ ifelse(.x >= 1, 1, 0)))
# 
# conf_pdff_mref <- dati_conf |> 
#   dplyr::select(-fll_steat) |> 
#   table()
# 
# specificita_mref <- conf_pdff_mref[1,2]/sum(conf_pdff_mref[1,])
# sensibilita_mref <- conf_pdff_mref[2,2]/sum(conf_pdff_mref[2,])
# accuratezza_mref <- (conf_pdff_mref[1,1] + conf_pdff_mref[2,2]) / (conf_pdff_mref[1,1] + conf_pdff_mref[1,2] + conf_pdff_mref[2,1] + conf_pdff_mref[2,2])
# 
# conf_pdff_fll <- dati_conf |> 
#   dplyr::select(-mref_steat) |> 
#   table()
# 
# specificita_fll <- conf_pdff_fll[1,2]/sum(conf_pdff_fll[1,])
# sensibilita_fll <- conf_pdff_fll[2,2]/sum(conf_pdff_fll[2,])
# accuratezza_fll <- (conf_pdff_fll[1,1] + conf_pdff_fll[2,2]) / (conf_pdff_fll[1,1] + conf_pdff_fll[1,2] + conf_pdff_fll[2,1] + conf_pdff_fll[2,2])

