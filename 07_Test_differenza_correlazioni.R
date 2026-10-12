# Assicurando la presenza di 'dati_medie' nell'ambiente globale:

risultati <- compare_correlations(
  df = dati_medie, 
  x = "pdff_media", 
  y = "mref_media", 
  z = "fll_media",
  R_boot = 10000
)

print(risultati)
