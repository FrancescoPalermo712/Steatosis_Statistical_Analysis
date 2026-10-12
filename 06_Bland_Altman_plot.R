# 1. Isolamento del dataset di interesse
df_ba <- data_clean %>%
  filter(pdff_steat >= 1) %>%
  mutate(
    mean = (fll_media + mref_media) / 2,
    diff  = fll_media - mref_media
  )

# 2. Calcolo dei parametri di Bland-Altman
mean_bias <- mean(df_ba$diff, na.rm = TRUE)
sd_diff   <- sd(df_ba$diff, na.rm = TRUE)
loa_upper <- mean_bias + (1.96 * sd_diff)
loa_lower <- mean_bias - (1.96 * sd_diff)

# 3. Grafico personalizzato per pubblicazione
bland_altman_plot <- ggplot(df_ba, aes(x = mean, y = diff)) +
  # Linea di zero (riferimento di assenza di bias)
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey60", linewidth = 0.5) +
  
  # Punti dati con leggera trasparenza per ridurre l'overplotting
  geom_point(alpha = 0.65, size = 2, color = "#1F4E78") +
  
  # Linea di Bias centrale
  geom_hline(yintercept = mean_bias, color = "#D95F02", linewidth = 0.8) +
  
  # Limiti di Accordo (95% LoA)
  geom_hline(yintercept = c(loa_lower, loa_upper), color = "#2B8CBE", linetype = "dashed", linewidth = 0.7) +
  
  # Etichette valori numerici sui margini
  annotate("text", x = max(df_ba$mean), y = mean_bias, label = paste0("Bias: ", sprintf("%.2f", mean_bias)), 
           hjust = 1, vjust = -0.6, fontface = "bold", size = 3.5, color = "#D95F02") +
  annotate("text", x = max(df_ba$mean), y = loa_upper, label = paste0("+1.96 SD: ", sprintf("%.2f", loa_upper)), 
           hjust = 1, vjust = -0.6, size = 3.2, color = "#2B8CBE") +
  annotate("text", x = max(df_ba$mean), y = loa_lower, label = paste0("-1.96 SD: ", sprintf("%.2f", loa_lower)), 
           hjust = 1, vjust = 1.5, size = 3.2, color = "#2B8CBE") +
  labs(
    title = "Agreement Between USAT (FLL) and USAT (M-Ref.)",
    subtitle = "Bland-Altman plot for patients with steatosis (Difference = FLL − M-Ref.)"
  ) +
  theme(
    # Centra il titolo principale e ne regola la dimensione
    plot.title = element_text(
      hjust = 0.5, 
      face = "bold", 
      size = 14,
      margin = margin(b = 5)
    ),
    # Centra il sottotitolo e regola lo spazio inferiore
    plot.subtitle = element_text(
      hjust = 0.5, 
      size = 11, 
      color = "gray30",
      margin = margin(b = 15)
    ),
    # Aumenta i margini esterni dell'immagine per evitare tagli in alto
    plot.margin = margin(t = 20, r = 15, b = 15, l = 15, unit = "pt")
  )

ggsave(
  filename = file.path(output, "Bland Altman (Confronto tra Mref e FLL tra pazienti steatosici).pdf"),
  plot = bland_altman_plot,
  width = 7, height = 6
)
