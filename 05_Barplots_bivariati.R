barplots <- dati_plots |> 
  dplyr::filter(var1 == "pdff" | var2 == "pdff") |> 
  dplyr::mutate(name_x = "PDFF",
                name_y = case_when(var2 == "fll" ~ "USAT (FLL)",
                                   var2 == "mref" ~ "USAT (M-Ref.)",
                                   TRUE ~ "Error")) |> 
  dplyr::mutate(plot = pmap(.l = list(data, name_x, name_y, autore),
                            .f = ~ ggplot(data = ..1,
                                        mapping = aes(x = var2_steat,
                                                      color = color_col,
                                                      fill = color_col)) +
                              geom_bar(width = 0.6) +
                              facet_grid(~ pdff_steat,
                                         labeller = labeller(pdff_steat = c("0" = "S0 (PDFF)",
                                                                            "1" = "S1 (PDFF)",
                                                                            "2" = "S2 (PDFF)",
                                                                            "3" = "S3 (PDFF)"))) +
                              scale_fill_identity() +
                              scale_color_identity() +
                              labs(title = paste0("Distribution of the grade of steatosis according to ", ..3, 
                                                  "\n by grade of steatosis according to ", ..2, " - " , ..4),
                                   x = paste0("Grade of steatosis (", ..2,")"),
                                   y = "Number of cases") +
                              theme(plot.title = element_text(hjust = 0.5)) +
                              scale_y_continuous(breaks = seq(from = 5, to = 70, by = 5)) +
                              coord_cartesian(ylim = c(0, 75))
                              ))

# Esportazione
# Define a 2x2 layout that leaves the bottom row empty
# 1, 2, 3 correspond to the plot sequence on each page
lay <- matrix(c(1, 2, 
                NA, NA), 
              nrow = 2, byrow = TRUE)

ggsave(
  filename = file.path(output, "barplots.pdf"),
  plot = marrangeGrob(barplots$plot, layout_matrix = lay),
  width = 15, height = 5
)
