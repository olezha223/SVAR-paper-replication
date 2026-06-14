library(vars)
model_env <- new.env()

load(
  "russian_paper_results/model_1/svar_results.RData",
  envir = model_env
)

stopifnot(
  identical(model_env$model_id, "model_1"),
  identical(model_env$supply_source, "delta_world")
)

var_model <- model_env$var_model

rm(model_env)
H <- 15
RUNS <- 2000
SHOCKS <- c("delta_prod", "rea", "rpo")

shock_labels <- c(
  delta_prod = "Oil supply shock",
  rea = "Aggregate demand shock",
  rpo = "Oil-specific demand shock"
)

simulate_var_wild <- function(model, eta) {
  A <- Bcoef(model)
  E <- scale(residuals(model), center = TRUE, scale = FALSE) * eta
  y0 <- model$y
  p <- model$p
  K <- model$K
  n <- nrow(E)

  ys <- matrix(NA_real_, p + n, K, dimnames = list(NULL, colnames(y0)))
  ys[1:p, ] <- y0[1:p, ]

  for (t in (p + 1):(p + n)) {
    lags <- as.vector(t(ys[(t - 1):(t - p), , drop = FALSE]))
    ys[t, ] <- A %*% c(lags, 1) + E[t - p, ]
  }

  ys
}

calc_irf <- function(model, response, cumulative) {
  irf(
    model,
    impulse = SHOCKS,
    response = response,
    n.ahead = H,
    ortho = TRUE,
    boot = FALSE,
    cumulative = cumulative
  )
}

extract_curve <- function(irf_obj, shock, response) {
  m <- irf_obj$irf[[shock]][, response]
  if (shock == "delta_prod") {
    m <- -m
  }
  m
}

make_bands <- function(model, response, cumulative) {
  set.seed(42)

  point_obj <- calc_irf(model, response, cumulative)
  point <- matrix(NA_real_, length(SHOCKS), H + 1, dimnames = list(SHOCKS, 0:H))

  for (sh in SHOCKS) {
    point[sh, ] <- extract_curve(point_obj, sh, response)
  }

  n <- nrow(residuals(model))
  boot <- array(
    NA_real_,
    c(RUNS, length(SHOCKS), H + 1),
    dimnames = list(NULL, SHOCKS, 0:H)
  )

  for (b in 1:RUNS) {
    eta <- rnorm(n)
    yb <- simulate_var_wild(model, eta)
    mb <- VAR(yb, p = model$p, type = model$type)
    ib <- calc_irf(mb, response, cumulative)

    for (sh in SHOCKS) {
      boot[b, sh, ] <- extract_curve(ib, sh, response)
    }

    if (b %% 100 == 0) cat("bootstrap:", b, "/", RUNS, "\n")
  }

  se <- apply(boot, c(2, 3), sd)

  b1 <- list()
  b2 <- list()

  for (sh in SHOCKS) {
    b1[[sh]] <- list(
      mean = point[sh, ],
      lo = point[sh, ] - se[sh, ],
      hi = point[sh, ] + se[sh, ]
    )

    b2[[sh]] <- list(
      mean = point[sh, ],
      lo = point[sh, ] - 2 * se[sh, ],
      hi = point[sh, ] + 2 * se[sh, ]
    )
  }

  list(b1 = b1, b2 = b2)
}

plot_panel <- function(b1, b2, title, ylab) {
  h <- 0:H
  ylim <- range(b1$mean, b2$lo, b2$hi)
  ylim <- ylim + diff(ylim) * c(-0.1, 0.1)
  plot(
    h,
    b1$mean,
    type = "l",
    lwd = 1.5,
    ylim = ylim,
    xlab = "Months",
    ylab = ylab,
    main = title,
    bty = "l",
    xaxs = "i"
  )
  abline(h = 0, col = "grey60", lwd = 0.8)
  lines(h, b2$lo, lty = 3)
  lines(h, b2$hi, lty = 3)
  lines(h, b1$lo, lty = 2)
  lines(h, b1$hi, lty = 2)
  lines(h, b1$mean, lwd = 1.5)
}

make_figure <- function(response, cumulative, ylab, file) {
  bands <- make_bands(var_model, response, cumulative)
  pdf(file, width = 10, height = 3)
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 1.5, 0))
  for (sh in SHOCKS) {
    plot_panel(
      bands$b1[[sh]],
      bands$b2[[sh]],
      title = shock_labels[sh],
      ylab = ylab
    )
  }
  dev.off()
  cat("Сохранено:", file, "\n")
}

make_figure(
  "rpo",
  cumulative = FALSE,
  ylab = "Real price of oil",
  file = "russian_paper_results/model_1/irf_fig1_rpo.pdf"
)

make_figure(
  "r",
  cumulative = TRUE,
  ylab = "Cumulative Real Stock Returns (%)",
  file = "russian_paper_results/model_1/irf_fig3_stocks.pdf"
)
