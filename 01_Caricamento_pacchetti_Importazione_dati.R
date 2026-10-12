# Importazione dati
#####
input <- file.path("C:","Users","franc","Documents","Programmazione","R","Dati","Studio_1_Cate","Input settembre")
output <- file.path("C:","Users","franc","Documents","Programmazione","R","Dati","Studio_1_Cate","Output settembre")

data_orig <- readxl::read_xlsx(path = here(file.path("Input settembre", "Def1_PER_FRANCI_GIADA_MODIFIED_Final_Database_Fat_Fraction_mindray.xlsx")), skip = 2, sheet = "CALC_Nuovo_Database_1,5T")

# Importazione cutoff
dati_cutoff_orig <- list(`Verona (2026)` = list(pdff = c(0.05, 0.15, 0.25),
                                   eco = c(0.64, 0.69, 0.74, 0.84)),
                         `Wang K. et al. (2026)` = list(pdff = c(0.05, 0.15, 0.25),
                                      eco = c(0.60, 0.70, 0.81)),
                         `Dag et al. (2025)` = list(pdff = c(0.06, 0.17, 0.22),
                                    eco = c(0.57, 0.68, 0.76)),
                         `Wang M. et al. (2024)` = list(pdff = c(0.05, 0.15, 0.25),
                                      eco = c(0.53, 0.58, 0.72, 0.85)),
                         `Yang et al. (2023)` = list(pdff = c(0.05, 0.15, 0.25),
                                                        eco = c(0.61, 0.65, 0.71)))

