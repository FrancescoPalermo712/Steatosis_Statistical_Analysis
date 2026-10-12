Rcpp::sourceCpp(code = '
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double calc_single_kappa(const IntegerVector& x, const IntegerVector& y, int k, const IntegerVector& indices) {
    int n = indices.size();
    NumericMatrix O(k, k);
    NumericVector row_sum(k);
    NumericVector col_sum(k);

    for(int i = 0; i < n; ++i) {
        int idx = indices[i] - 1; 
        int x_val = x[idx] - 1;
        int y_val = y[idx] - 1;
        
        if (x_val >= 0 && y_val >= 0 && x_val < k && y_val < k) {
            O(x_val, y_val) += 1.0;
            row_sum[x_val] += 1.0;
            col_sum[y_val] += 1.0;
        }
    }

    double obs_disagreement = 0.0;
    double exp_disagreement = 0.0;
    
    for(int i = 0; i < k; ++i) {
        for(int j = 0; j < k; ++j) {
            if (i != j) {
                double weight = (i - j) * (i - j); 
                obs_disagreement += weight * O(i, j);
                exp_disagreement += weight * (row_sum[i] * col_sum[j]) / n;
            }
        }
    }

    if (exp_disagreement == 0.0) return NA_REAL;
    return 1.0 - (obs_disagreement / exp_disagreement);
}

// [[Rcpp::export]]
double calc_kappa_diff_cpp(IntegerVector x, IntegerVector y1, IntegerVector y2, int k, IntegerVector indices) {
    double k1 = calc_single_kappa(x, y1, k, indices);
    double k2 = calc_single_kappa(x, y2, k, indices);
    
    if (NumericVector::is_na(k1) || NumericVector::is_na(k2)) return NA_REAL;
    return k1 - k2;
}
')



























test_kappa_diff_bca <- function(df_sub, alpha_val = 0.05, R = 10000) {
  df_clean <- df_sub %>% 
    dplyr::filter(!is.na(pdff_steat) & !is.na(fll_steat) & !is.na(mref_steat))
  
  # Armonizzazione livelli (se max=4 e max=3 si converte 4 a 3)
  max_p <- max(df_clean$pdff_steat)
  max_f <- max(df_clean$fll_steat)
  max_m <- max(df_clean$mref_steat)
  
  if (any(c(max_p, max_f, max_m) == 4) && any(c(max_p, max_f, max_m) == 3)) {
    df_clean <- df_clean %>%
      dplyr::mutate(across(c(pdff_steat, fll_steat, mref_steat), ~ ifelse(.x == 4, 3, .x)))
  }
  
  all_levels <- sort(unique(c(df_clean$pdff_steat, df_clean$fll_steat, df_clean$mref_steat)))
  k <- length(all_levels)
  
  x_int  <- as.integer(factor(df_clean$pdff_steat, levels = all_levels))
  y1_int <- as.integer(factor(df_clean$fll_steat, levels = all_levels))
  y2_int <- as.integer(factor(df_clean$mref_steat, levels = all_levels))
  N <- length(x_int)
  
  # Valori osservati nel campione originale
  k_pdff_fll  <- calc_single_kappa(x_int, y1_int, k, seq_len(N))
  k_pdff_mref <- calc_single_kappa(x_int, y2_int, k, seq_len(N))
  diff_obs    <- k_pdff_fll - k_pdff_mref
  
  # Bootstrap appaiato
  stat_wrapper <- function(d, indices) {
    calc_kappa_diff_cpp(x_int, y1_int, y2_int, k, indices)
  }
  
  boot_out <- boot::boot(data = seq_len(N), statistic = stat_wrapper, R = R)
  
  # --- Test Non-Parametrico (Intervallo di Confidenza BCa e p-value empirico) ---
  ci_out <- tryCatch({
    boot::boot.ci(boot_out, conf = 1 - alpha_val, type = "bca")
  }, error = function(e) NULL)
  
  bca_low <- if (!is.null(ci_out) && !is.null(ci_out$bca)) ci_out$bca[1, 4] else NA_real_
  bca_up  <- if (!is.null(ci_out) && !is.null(ci_out$bca)) ci_out$bca[1, 5] else NA_real_
  
  # p-value non-parametrico empirico a due code
  p_nonparam <- 2 * min(
    mean(boot_out$t <= 0, na.rm = TRUE),
    mean(boot_out$t >= 0, na.rm = TRUE)
  )
  
  # --- Test Parametrico di Wald (Z-test con SE Bootstrap) ---
  se_diff <- sd(boot_out$t, na.rm = TRUE)
  z_stat  <- diff_obs / se_diff
  p_param <- 2 * (1 - pnorm(abs(z_stat)))
  
  tibble::tibble(
    kappa_pdff_fll   = k_pdff_fll,
    kappa_pdff_mref  = k_pdff_mref,
    diff_kappa       = diff_obs,
    se_diff          = se_diff,
    z_stat           = z_stat,
    p_value_param    = p_param,
    bca_ci_lower     = bca_low,
    bca_ci_upper     = bca_up,
    p_value_nonparam = p_nonparam
  )
}




















# Preparazione del dataset appaiato (3 misurazioni per paziente)
dati_paired <- data_clean %>%
  dplyr::select(
    pdff_steat_vr = pdff_steat, mref_steat_vr = mref_steat, fll_steat_vr = fll_steat,
    pdff_steat_wangK, mref_steat_wangK, fll_steat_wangK,
    pdff_steat_dag, mref_steat_dag, fll_steat_dag,
    pdff_steat_wangM, mref_steat_wangM, fll_steat_wangM,
    pdff_steat_yang, mref_steat_yang, fll_steat_yang
  ) %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  tidyr::pivot_longer(
    cols = contains("steat"),
    names_to = c("var", "autore"),
    names_pattern = "(.*_steat)_(.*)"
  ) %>%
  tidyr::pivot_wider(names_from = var, values_from = value) %>%
  dplyr::mutate(autore = case_when(
    autore == "dag"   ~ "Dag et al. (2025)",
    autore == "vr"    ~ "Verona (2026)",
    autore == "wangK" ~ "Wang K. et al. (2026)",
    autore == "wangM" ~ "Wang M. et al. (2024)",
    autore == "yang"  ~ "Yang et al. (2023)"
  ))

# Esecuzione del test per ogni autore (con alpha = 0.05 e R = 10000)
risultati_confronto_kappa <- dati_paired %>%
  nest(data = -autore) %>%
  mutate(test_res = map(data, ~ test_kappa_diff_bca(.x, alpha_val = 0.05, R = 10000))) %>%
  select(-data) %>%
  unnest(test_res)

# Visualizzazione dei risultati
print(risultati_confronto_kappa)
