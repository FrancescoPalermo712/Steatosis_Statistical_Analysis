# Caricamento pacchetti
#####
packages <- c("boot", "caret", "gridExtra", "psych", "Rcpp", "scales", "tidyverse", "vcd", "writexl", "yardstick")
install.packages(packages[!packages %in% installed.packages()[,"Package"]]) # si installano solo i pacchetti non ancora installati
invisible(lapply(X = packages, FUN = library, character.only = TRUE)) # caricamento pacchetti
#####


# (I) CUTOFF

diagnosi <- function(x, cutoff) {
  if(max(x) > 1) {
    x <- x/100
  }
  findInterval(x = x, vec = cutoff, left.open = TRUE)
}

# (II) Colore scatterplots
color_fun <- function(state_x, state_y) {
  max_x <- max(state_x, na.rm = TRUE)
  max_y <- max(state_y, na.rm = TRUE)
  
  condition <- if ((max_x == 4 && max_y == 4) || (max_x == 3 && max_y == 3)) {
    state_x == state_y
  } else if (max_x == 3 && max_y == 4) {
    (state_x == state_y) | (state_x == 3 & state_y == 4)
  } else if (max_x == 4 && max_y == 3) {
    (state_x == state_y) | (state_x == 4 & state_y == 3)
  } else {
    stop("Error: No condition was met.")
  }
  
  color = ifelse(test = condition, yes = "forestgreen", no = "firebrick")
  
  return(color)
}

# (III) selezione variabili per statistiche descrittive
selezione_variabili_continue <- function(x) {
  x |> 
    dplyr::select(eta,
                  pdff_media, pdff_sd, pdff_sdbar, 
                  mref_media, mref_sd, mref_sdbar,
                  fll_media, fll_sd, fll_sdbar
    )
}

selezione_variabili_discrete <- function(x) {
  x |> 
    dplyr::select(maschio, pdff_steat, mref_steat, fll_steat,
                  pdff_steat_wangK, mref_steat_wangK, fll_steat_wangK,
                  pdff_steat_dag, mref_steat_dag, fll_steat_dag,
                  pdff_steat_wangM, mref_steat_wangM, fll_steat_wangM,
                  pdff_steat_yang, mref_steat_yang, fll_steat_yang)
}

# (IV) Produzione statistiche di sintesi per variabili continue
descr_stats <- list(
  secondquartile = ~ round(quantile(.x, na.rm = TRUE)[2], digits = 5),
  fourthquartile = ~ round(quantile(.x, na.rm = TRUE)[4], digits = 5),
  median = ~ round(median(.x, na.rm = TRUE), digits = 5)
)

# (V) Define the helper function for the bootstrap and CI extraction
# --- 1. COMPILE THE C++ FUNCTION ---
Rcpp::cppFunction('
double calc_weighted_kappa_cpp(IntegerVector x, IntegerVector y, int k, IntegerVector indices) {
    int n = indices.size();
    NumericMatrix O(k, k);
    NumericVector row_sum(k);
    NumericVector col_sum(k);

    for(int i = 0; i < n; ++i) {
        int idx = indices[i] - 1; 
        int x_val = x[idx] - 1;
        int y_val = y[idx] - 1;
        
        if (x_val >= 0 && y_val >= 0) {
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
')

# --- 2. DEFINE THE R WRAPPER ---
get_bca_ci_fast <- function(data, col1, col2, alpha_val, R = 10000) {
  col1_sym <- rlang::ensym(col1)
  col2_sym <- rlang::ensym(col2)
  v1 <- data[[as.character(col1_sym)]]
  v2 <- data[[as.character(col2_sym)]]
  
  all_levels <- sort(unique(c(as.character(v1), as.character(v2))))
  v1 <- factor(v1, levels = all_levels)
  v2 <- factor(v2, levels = all_levels)
  
  k <- length(all_levels)
  x_int <- as.integer(v1)
  y_int <- as.integer(v2)
  N <- length(x_int)
  
  stat_wrapper <- function(d, indices) {
    calc_weighted_kappa_cpp(x_int, y_int, k, indices)
  }
  
  boot_out <- boot::boot(data = seq_len(N), statistic = stat_wrapper, R = R)
  
  ci_out <- tryCatch({
    boot::boot.ci(boot_out, conf = 1 - alpha_val, type = "bca")
  }, error = function(e) NULL)
  
  if (is.null(ci_out) || is.null(ci_out$bca)) {
    return(tibble(ci_lower = NA_real_, ci_upper = NA_real_))
  }
  
  tibble(
    ci_lower = ci_out$bca[1, 4],
    ci_upper = ci_out$bca[1, 5]
  )
}

# (VI) Da tibble a confusion matrix
tibble_to_confmat <- function(data, col1, col2) {
  data |> dplyr::count({{col1}}, {{col2}}, .drop = FALSE) |>
    tidyr::pivot_wider(names_from = {{col2}}, values_from = n, values_fill = 0) |>
    dplyr::select(-{{col1}}) |>
    as.matrix()
}


# (VII) Calcolo del Cohen's Kappa estraendo solo il valore isolato, ponderato con pesi quadratici
kappa_single_output <- function(confmat, weights = "Fleiss-Cohen", choice = "Weighted", stat = "value") {
  confmat |>
    vcd::Kappa(weights = weights) %>%
    .[[choice]] %>% .[[stat]]
}



f <- function(df, col1, col2, weights = "Fleiss-Cohen", choice = "Weighted", stat = "value") {
  df |> dplyr::count({{col1}}, {{col2}}, .drop = FALSE) |>
    tidyr::pivot_wider(names_from = {{col2}}, values_from = n, values_fill = 0) |>
    dplyr::select(-{{col1}}) |>
    as.matrix() |>
    vcd::Kappa(weights = weights) %>%
    .[[choice]] %>% .[[stat]]
}


# (VIII) Sensibilità specificità accuratezza
get_metrics <- function(data, eco_col, pdff_col) {
  
  # Helper to compute metrics for a single binary contrast
  calc_single_contrast <- function(sub_data, pos_levels) {
    
    # If the filtered subset is empty, return NAs to avoid errors
    if (nrow(sub_data) == 0) {
      return(tibble(Sensitivity = NA_real_, Specificity = NA_real_, Accuracy = NA_real_))
    }
    
    df_binary <- sub_data %>%
      mutate(
        eco_bin  = factor(if_else({{eco_col}}  %in% pos_levels, "Positive", "Negative"), levels = c("Positive", "Negative")),
        pdff_bin = factor(if_else({{pdff_col}} %in% pos_levels, "Positive", "Negative"), levels = c("Positive", "Negative"))
      )
    
    # Run confusion matrix
    cm <- confusionMatrix(df_binary$eco_bin, df_binary$pdff_bin, positive = "Positive")
    
    tibble(
      Sensitivity = cm$byClass["Sensitivity"],
      Specificity = cm$byClass["Specificity"],
      Accuracy    = cm$overall["Accuracy"]
    )
  }
  
  # 1. Level 0: S0 vs (S1, S2, S3)
  m0 <- calc_single_contrast(data, pos_levels = c(1, 2, 3))
  
  # 2. Level 1: S0 vs S1
  m1 <- data %>%
    filter({{pdff_col}} %in% c(0, 1), {{eco_col}} %in% c(0, 1)) %>%
    calc_single_contrast(pos_levels = 1)
  
  # 3. Level 2: S1 vs S2
  m2 <- data %>%
    filter({{pdff_col}} %in% c(1, 2), {{eco_col}} %in% c(1, 2)) %>%
    calc_single_contrast(pos_levels = 2)
  
  # 4. Level 3: S2 vs S3
  m3 <- data %>%
    filter({{pdff_col}} %in% c(2, 3), {{eco_col}} %in% c(2, 3)) %>%
    calc_single_contrast(pos_levels = 3)
  
  # Bind all levels into a single 4-row summary table
  bind_rows(
    "Level 0 (S0 vs S1-3)" = m0,
    "Level 1 (S0 vs S1)"   = m1,
    "Level 2 (S1 vs S2)"   = m2,
    "Level 3 (S2 vs S3)"   = m3,
    .id = "Comparison"
  )
}

# (IX) Test sulla differenza delle correlazioni (FLL vs pdff - M-Ref vs pdff): Williams e BCa Bootstrap CI 95%
# Funzione C++ per calcolare (r_XY - r_XZ) velocemente
cppFunction('
NumericVector fast_cor_diff(NumericMatrix data) {
  int n = data.nrow();
  
  double sum_x = 0, sum_y = 0, sum_z = 0;
  for (int i = 0; i < n; i++) {
    sum_x += data(i, 0);
    sum_y += data(i, 1);
    sum_z += data(i, 2);
  }
  
  double mean_x = sum_x / n;
  double mean_y = sum_y / n;
  double mean_z = sum_z / n;
  
  double sxx = 0, syy = 0, szz = 0;
  double sxy = 0, sxz = 0;
  
  for (int i = 0; i < n; i++) {
    double dx = data(i, 0) - mean_x;
    double dy = data(i, 1) - mean_y;
    double dz = data(i, 2) - mean_z;
    
    sxx += dx * dx;
    syy += dy * dy;
    szz += dz * dz;
    sxy += dx * dy;
    sxz += dx * dz;
  }
  
  double r_xy = sxy / std::sqrt(sxx * syy);
  double r_xz = sxz / std::sqrt(sxx * szz);
  
  // Restituisce r_xy, r_xz e la loro differenza
  return NumericVector::create(
    Named("r_XY") = r_xy,
    Named("r_XZ") = r_xz,
    Named("diff") = r_xy - r_xz
  );
}
')

# Test di Williams
williams_test <- function(data, x_col, y_col, z_col) {
  df <- data %>% select(all_of(c(x_col, y_col, z_col))) %>% drop_na()
  N <- nrow(df)
  
  # Calcolo matrice di correlazione
  R <- cor(df)
  r12 <- R[x_col, y_col]
  r13 <- R[x_col, z_col]
  r23 <- R[y_col, z_col]
  
  # Determinante della matrice 3x3
  det_R <- det(R)
  r_bar <- (r12 + r13) / 2
  
  # Statistica t di Williams
  num <- (r12 - r13) * sqrt((N - 1) * (1 + r23))
  den <- sqrt(2 * (((N - 1) / (N - 3)) * det_R + (r_bar^2) * ((1 - r23)^3)))
  
  t_stat <- num / den
  p_val <- 2 * pt(-abs(t_stat), df = N - 3)
  
  tibble(
    method = "Williams (1959) t-test",
    r_XY = r12,
    r_XZ = r13,
    diff = r12 - r13,
    statistic = t_stat,
    df = N - 3,
    p_value = p_val
  )
}

# Bootstrap non parametrico
bootstrap_bca_test <- function(data, x_col, y_col, z_col, R_boot = 5000, conf_level = 0.95, seed = 42) {
  set.seed(seed)
  
  mat_data <- data %>% 
    select(all_of(c(x_col, y_col, z_col))) %>% 
    drop_na() %>% 
    as.matrix()
  
  # Statistica boot per il pacchetto 'boot' (sfrutta Rcpp)
  boot_stat <- function(m, indices) {
    fast_cor_diff(m[indices, , drop = FALSE])["diff"]
  }
  
  # Esecuzione Bootstrap
  boot_res <- boot(data = mat_data, statistic = boot_stat, R = R_boot)
  
  # Calcolo intervallo BCa
  boot_ci <- boot.ci(boot_res, type = "bca", conf = conf_level)
  
  bca_lower <- boot_ci$bca[1, 4]
  bca_upper <- boot_ci$bca[1, 5]
  
  # P-value empirico ricavato dall'inversione degli intervalli/distribuzione bootstrap
  p_val_empirical <- mean(abs(boot_res$t - boot_res$t0) >= abs(boot_res$t0), na.rm = TRUE)
  
  tibble(
    method = paste0("Non-parametric Bootstrap BCa (R=", R_boot, ")"),
    r_XY = fast_cor_diff(mat_data)["r_XY"],
    r_XZ = fast_cor_diff(mat_data)["r_XZ"],
    diff = boot_res$t0,
    ci_lower = bca_lower,
    ci_upper = bca_upper,
    p_value_approx = p_val_empirical,
    significant_at_alpha = !(bca_lower <= 0 && bca_upper >= 0)
  )
}

# 4. Wrapper Principale ed Esecuzione
compare_correlations <- function(df, x, y, z, R_boot = 5000) {
  res_williams <- williams_test(df, x_col = x, y_col = y, z_col = z)
  res_bca <- bootstrap_bca_test(df, x_col = x, y_col = y, z_col = z, R_boot = R_boot)
  
  bind_rows(
    res_williams %>% select(method, diff, p_value),
    res_bca %>% select(method, diff, ci_lower, ci_upper, p_value = p_value_approx)
  )
}


