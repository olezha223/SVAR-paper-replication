library(vars)

load("russian_paper_results/model_rub/svar_results.RData")

H <- 15
RUNS <- 2000
SEED <- 42

# Используем recursive-design wild bootstrap.
# По умолчанию: один N(0, 1)-множитель на весь вектор остатков в момент t.
# При необходимости можно заменить на "rademacher".
WILD_WEIGHTS <- "gaussian"

SHOCKS <- c("delta_prod", "rea", "rpo")

shock_labels <- c(
  delta_prod = "Oil supply shock",
  rea = "Aggregate demand shock",
  rpo = "Oil-specific demand shock"
)


get_coef_matrix <- function(model) {
  coef_list <- lapply(model$varresult, coef)
  reg_names <- names(coef_list[[1]])

  same_names <- vapply(
    coef_list,
    function(x) identical(names(x), reg_names),
    logical(1)
  )

  if (!all(same_names)) {
    stop("В уравнениях VAR различается набор регрессоров")
  }

  B <- do.call(cbind, coef_list)
  rownames(B) <- reg_names
  colnames(B) <- names(model$varresult)
  B
}


get_lag_map <- function(reg_names, y_names, p) {
  lag_map <- data.frame(
    position = integer(0),
    variable = integer(0),
    lag = integer(0)
  )

  for (j in seq_along(reg_names)) {
    reg_name <- reg_names[j]

    if (!grepl("\\.l[0-9]+$", reg_name)) {
      next
    }

    variable_name <- sub("\\.l[0-9]+$", "", reg_name)
    lag_number <- as.integer(sub("^.*\\.l", "", reg_name))
    variable_number <- match(variable_name, y_names)

    if (!is.na(variable_number) && lag_number <= p) {
      lag_map <- rbind(
        lag_map,
        data.frame(
          position = j,
          variable = variable_number,
          lag = lag_number
        )
      )
    }
  }

  if (nrow(lag_map) != length(y_names) * p) {
    stop(
      "Не удалось распознать все лаговые переменные. ",
      "Проверь названия коэффициентов в var_model$varresult"
    )
  }

  lag_map
}


coef_to_A <- function(B, lag_map, K, p, y_names) {
  A <- array(
    0,
    dim = c(K, K, p),
    dimnames = list(
      response = y_names,
      variable = y_names,
      lag = paste0("L", seq_len(p))
    )
  )

  for (i in seq_len(nrow(lag_map))) {
    row_number <- lag_map$position[i]
    variable_number <- lag_map$variable[i]
    lag_number <- lag_map$lag[i]

    # B[row_number, j] — коэффициент при переменной variable_number
    # в уравнении для отклика j.
    A[, variable_number, lag_number] <- B[row_number, ]
  }

  A
}


structural_irf <- function(B, Sigma, lag_map, K, p, H, y_names) {
  A <- coef_to_A(B, lag_map, K, p, y_names)

  Phi <- array(
    0,
    dim = c(H + 1, K, K),
    dimnames = list(
      horizon = 0:H,
      response = y_names,
      innovation = y_names
    )
  )

  Phi[1, , ] <- diag(K)

  if (H >= 1) {
    for (h in seq_len(H)) {
      current_phi <- matrix(0, nrow = K, ncol = K)

      for (lag_number in seq_len(min(p, h))) {
        current_phi <- current_phi +
          A[,, lag_number] %*%
            Phi[h - lag_number + 1, , ]
      }

      Phi[h + 1, , ] <- current_phi
    }
  }

  Sigma <- (Sigma + t(Sigma)) / 2
  impact_matrix <- t(chol(Sigma))

  Theta <- array(
    0,
    dim = c(H + 1, K, K),
    dimnames = list(
      horizon = 0:H,
      response = y_names,
      impulse = y_names
    )
  )

  for (h in 0:H) {
    Theta[h + 1, , ] <- Phi[h + 1, , ] %*% impact_matrix
  }

  Theta
}


make_wild_weights <- function(n, type) {
  if (type == "gaussian") {
    return(rnorm(n))
  }

  if (type == "rademacher") {
    return(sample(c(-1, 1), size = n, replace = TRUE))
  }

  stop("WILD_WEIGHTS должен быть 'gaussian' или 'rademacher'")
}


recursive_design_wild_bootstrap <- function(
  model,
  H,
  runs,
  seed = 42,
  wild_weights = "gaussian"
) {
  Y <- as.matrix(model$y)
  y_names <- colnames(Y)
  K <- ncol(Y)
  p <- model$p
  T_total <- nrow(Y)
  n_effective <- T_total - p

  B_hat <- get_coef_matrix(model)

  if (!setequal(colnames(B_hat), y_names)) {
    stop("Названия уравнений VAR не совпадают с названиями переменных")
  }

  B_hat <- B_hat[, y_names, drop = FALSE]

  if (anyNA(B_hat)) {
    stop("В коэффициентах исходной VAR есть NA")
  }

  reg_names <- rownames(B_hat)
  q <- nrow(B_hat)

  datamat <- as.matrix(model$datamat)

  if (nrow(datamat) != n_effective) {
    stop("Число строк model$datamat не совпадает с T - p")
  }

  missing_regressors <- setdiff(reg_names, colnames(datamat))

  if (length(missing_regressors) > 0) {
    stop(
      "В model$datamat отсутствуют регрессоры: ",
      paste(missing_regressors, collapse = ", ")
    )
  }

  X_original <- datamat[, reg_names, drop = FALSE]
  Y_effective <- Y[(p + 1):T_total, , drop = FALSE]

  lag_map <- get_lag_map(reg_names, y_names, p)

  residuals_hat <- Y_effective - X_original %*% B_hat

  # При наличии константы среднее и так почти равно нулю.
  # Центрирование защищает от маленькой численной погрешности.
  residuals_for_bootstrap <- sweep(
    residuals_hat,
    MARGIN = 2,
    STATS = colMeans(residuals_hat),
    FUN = "-"
  )

  sigma_df <- n_effective - q

  if (sigma_df <= 0) {
    stop("Недостаточно наблюдений для оценки ковариационной матрицы")
  }

  Sigma_hat <- crossprod(residuals_hat) / sigma_df

  point_irf <- structural_irf(
    B = B_hat,
    Sigma = Sigma_hat,
    lag_map = lag_map,
    K = K,
    p = p,
    H = H,
    y_names = y_names
  )

  boot_irf <- array(
    NA_real_,
    dim = c(H + 1, K, K, runs),
    dimnames = list(
      horizon = 0:H,
      response = y_names,
      impulse = y_names,
      replication = seq_len(runs)
    )
  )

  build_design <- function(Y_star) {
    X_star <- X_original

    for (i in seq_len(nrow(lag_map))) {
      position <- lag_map$position[i]
      variable <- lag_map$variable[i]
      lag_number <- lag_map$lag[i]

      X_star[, position] <- Y_star[
        (p + 1 - lag_number):(T_total - lag_number),
        variable
      ]
    }

    X_star
  }

  set.seed(seed)

  completed <- 0
  attempts <- 0
  max_attempts <- runs * 5

  while (completed < runs && attempts < max_attempts) {
    attempts <- attempts + 1

    multipliers <- make_wild_weights(n_effective, wild_weights)

    # Один и тот же множитель используется для всего вектора ошибок в момент t.
    wild_residuals <- sweep(
      residuals_for_bootstrap,
      MARGIN = 1,
      STATS = multipliers,
      FUN = "*"
    )

    Y_star <- Y

    # Первые p наблюдений фиксируются на исходных значениях.
    for (s in seq_len(n_effective)) {
      t <- p + s
      x_t <- X_original[s, ]

      for (i in seq_len(nrow(lag_map))) {
        position <- lag_map$position[i]
        variable <- lag_map$variable[i]
        lag_number <- lag_map$lag[i]

        x_t[position] <- Y_star[t - lag_number, variable]
      }

      conditional_mean <- drop(x_t %*% B_hat)
      Y_star[t, ] <- conditional_mean + wild_residuals[s, ]
    }

    X_star <- build_design(Y_star)
    Y_star_effective <- Y_star[(p + 1):T_total, , drop = FALSE]

    fit_star <- lm.fit(x = X_star, y = Y_star_effective)

    if (fit_star$rank < q) {
      next
    }

    B_star <- fit_star$coefficients

    if (is.null(dim(B_star))) {
      B_star <- matrix(B_star, ncol = K)
    }

    rownames(B_star) <- reg_names
    colnames(B_star) <- y_names

    residuals_star <- fit_star$residuals

    if (is.null(dim(residuals_star))) {
      residuals_star <- matrix(residuals_star, ncol = K)
    }

    if (any(!is.finite(B_star)) || any(!is.finite(residuals_star))) {
      next
    }

    Sigma_star <- crossprod(residuals_star) / sigma_df

    irf_star <- try(
      structural_irf(
        B = B_star,
        Sigma = Sigma_star,
        lag_map = lag_map,
        K = K,
        p = p,
        H = H,
        y_names = y_names
      ),
      silent = TRUE
    )

    if (inherits(irf_star, "try-error") || any(!is.finite(irf_star))) {
      next
    }

    completed <- completed + 1
    boot_irf[,,, completed] <- irf_star

    if (completed %% 100 == 0 || completed == runs) {
      cat("Bootstrap:", completed, "из", runs, "\n")
    }
  }

  if (completed < runs) {
    stop(
      "Удалось получить только ",
      completed,
      " корректных репликаций из ",
      runs
    )
  }

  list(
    point = point_irf,
    boot = boot_irf,
    y_names = y_names,
    H = H,
    runs = runs,
    wild_weights = wild_weights
  )
}


get_bands <- function(
  bootstrap_result,
  response,
  shocks = SHOCKS,
  cumulative = FALSE,
  scale = 1
) {
  y_names <- bootstrap_result$y_names
  H <- bootstrap_result$H
  runs <- bootstrap_result$runs

  response_number <- match(response, y_names)
  shock_numbers <- match(shocks, y_names)

  if (is.na(response_number)) {
    stop("В VAR нет переменной response = ", response)
  }

  if (anyNA(shock_numbers)) {
    stop("В VAR отсутствует один или несколько шоков из SHOCKS")
  }

  point <- bootstrap_result$point[,
    response_number,
    shock_numbers,
    drop = FALSE
  ]
  dim(point) <- c(H + 1, length(shocks))
  colnames(point) <- shocks

  boot <- bootstrap_result$boot[,
    response_number,
    shock_numbers,
    ,
    drop = FALSE
  ]
  dim(boot) <- c(H + 1, length(shocks), runs)
  dimnames(boot) <- list(
    horizon = 0:H,
    shock = shocks,
    replication = seq_len(runs)
  )

  if (cumulative) {
    for (j in seq_along(shocks)) {
      point[, j] <- cumsum(point[, j])

      for (b in seq_len(runs)) {
        boot[, j, b] <- cumsum(boot[, j, b])
      }
    }
  }

  # В статье положительный oil supply shock трактуется как сокращение предложения,
  # поэтому меняем знак и у точки, и у всех bootstrap-репликаций.
  supply_number <- match("delta_prod", shocks)

  if (!is.na(supply_number)) {
    point[, supply_number] <- -point[, supply_number]
    boot[, supply_number, ] <- -boot[, supply_number, ]
  }

  point <- point * scale
  boot <- boot * scale

  standard_error <- apply(boot, c(1, 2), sd, na.rm = TRUE)

  bands <- vector("list", length(shocks))
  names(bands) <- shocks

  for (j in seq_along(shocks)) {
    bands[[j]] <- list(
      mean = point[, j],
      lo1 = point[, j] - standard_error[, j],
      hi1 = point[, j] + standard_error[, j],
      lo2 = point[, j] - 2 * standard_error[, j],
      hi2 = point[, j] + 2 * standard_error[, j]
    )
  }

  bands
}


plot_panel <- function(band, title, ylab, H) {
  h <- 0:H

  ylim <- range(
    band$mean,
    band$lo2,
    band$hi2,
    finite = TRUE
  )

  padding <- diff(ylim) * 0.08

  if (!is.finite(padding) || padding == 0) {
    padding <- 1
  }

  ylim <- ylim + c(-padding, padding)

  plot(
    h,
    band$mean,
    type = "l",
    lwd = 1.7,
    ylim = ylim,
    xlab = "Months",
    ylab = ylab,
    main = title,
    bty = "l",
    xaxs = "i",
    xaxt = "n"
  )

  axis(1, at = seq(0, H, by = 5))
  abline(h = 0, col = "grey65", lwd = 0.8)

  # Две стандартные ошибки — точечная линия.
  lines(h, band$lo2, lty = 3, lwd = 1.2)
  lines(h, band$hi2, lty = 3, lwd = 1.2)

  # Одна стандартная ошибка — штриховая линия.
  lines(h, band$lo1, lty = 2, lwd = 1.2)
  lines(h, band$hi1, lty = 2, lwd = 1.2)

  lines(h, band$mean, lwd = 1.7)
}


make_figure <- function(
  bootstrap_result,
  response,
  cumulative,
  ylab,
  file,
  scale = 1
) {
  bands <- get_bands(
    bootstrap_result = bootstrap_result,
    response = response,
    shocks = SHOCKS,
    cumulative = cumulative,
    scale = scale
  )

  pdf(file, width = 10, height = 3.5)

  par(
    mfrow = c(1, 3),
    mar = c(4, 4, 3, 1),
    oma = c(0, 0, 1.5, 0)
  )

  for (shock in SHOCKS) {
    plot_panel(
      band = bands[[shock]],
      title = shock_labels[shock],
      ylab = ylab,
      H = bootstrap_result$H
    )
  }

  dev.off()

  cat("Сохранено:", file, "\n")
}


bootstrap_result <- recursive_design_wild_bootstrap(
  model = var_model,
  H = H,
  runs = RUNS,
  seed = SEED,
  wild_weights = WILD_WEIGHTS
)

saveRDS(
  bootstrap_result,
  file = "russian_paper_results/model_rub/recursive_wild_bootstrap_irf.rds"
)

# Оставь scale = 1 для исходной шкалы.
# Если надо только визуально умножить значения на 19, поставь scale = 19.
make_figure(
  bootstrap_result = bootstrap_result,
  response = "rpo",
  cumulative = FALSE,
  ylab = "Real price of oil",
  file = "russian_paper_results/model_rub/irf_fig1_rpo.pdf",
  scale = 1
)

make_figure(
  bootstrap_result = bootstrap_result,
  response = "r",
  cumulative = TRUE,
  ylab = "Cumulative Real Stock Returns (%)",
  file = "russian_paper_results/model_rub/irf_fig3_stocks.pdf",
  scale = 1
)
