scatterplots <- dati_plots |> 
  mutate(plot = pmap(list(data, name_x, name_y, title, autore, var1_cutoff, var2_cutoff), ~ 
                       ggplot(data = ..1, mapping = aes(x = var1_media, y = var2_media, color = color_col, fill = color_col)) +
                       geom_point(size = 2) + 
                       scale_color_identity() +
                       scale_fill_identity() +
                       scale_x_continuous(
                         name = if (..6 == "pdff") {paste0(..2, " (log scale)")} else {..2},
                         breaks = dati_cutoff_orig[[..5]][[..6]],
                         labels = {dati_cutoff_orig[[..5]][[..6]]},
                         limits = function(lims) {
                           c(
                             min(lims[1], min(dati_cutoff_orig[[..5]][[..6]])),
                             max(lims[2], max(dati_cutoff_orig[[..5]][[..6]]))
                           )
                           },
                         transform = if (..6 == "pdff") {"log"} else {"identity"}
                         ) +
                       scale_y_continuous(
                         name = ..3,
                         breaks = dati_cutoff_orig[[..5]][[..7]],
                         labels = {dati_cutoff_orig[[..5]][[..7]]},
                         limits = function(lims) {
                           c(
                             min(lims[1], min(dati_cutoff_orig[[..5]][[..7]])),
                             max(lims[2], max(dati_cutoff_orig[[..5]][[..7]]))
                           )
                         }
                       ) +
                         labs(title = ..4) +
                         theme(plot.title = element_text(hjust = 0.5),
                               panel.grid.minor = element_blank())
                       ))

# Esportazione
# Define a 2x2 layout that leaves the bottom-right corner empty
# 1, 2, 3 correspond to the plot sequence on each page
lay <- matrix(c(2, 1, 
                3, NA), 
              nrow = 2, byrow = TRUE)

ggsave(
  filename = file.path(output, "scatterplots.pdf"),
  plot = marrangeGrob(scatterplots$plot, layout_matrix = lay),
  width = 15, height = 9
)
