library(vars)

industry_dir <- file.path("original_paper_results", "industry_results")

H <- 15
RUNS <- 2000

SHOCKS <- c("delta_prod", "rea", "rpo")
shock_labels <- c(
  delta_prod = "Oil supply shock",
  rea = "Aggregate demand shock",
  rpo = "Oil-specific demand shock"
)

# подписи на графиках
industries <- c(
  oil_industry = "Petroleum & Natural Gas",
  autos = "Automobiles & Trucks",
  rtail = "Retail",
  gold = "Precious Metals"
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

calc_irf <- function(model, response) {
  irf(
    model,
    impulse = SHOCKS,
    response = response,
    n.ahead = H,
    ortho = TRUE,
    boot = FALSE,
    cumulative = TRUE
  )
}

extract_curve <- function(irf_obj, shock, response) {
  m <- irf_obj$irf[[shock]][, response]
  if (shock == "delta_prod") {
    m <- -m
  }
  m
}

make_bands <- function(model, response) {
  set.seed(42)

  point_obj <- calc_irf(model, response)
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
    ib <- calc_irf(mb, response)

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

plot_panel <- function(b1, b2, title, ylab = "") {
  h <- 0:H
  ylim <- range(b1$mean, b2$lo, b2$hi, na.rm = TRUE)
  pad <- diff(ylim) * 0.12
  ylim <- ylim + c(-pad, pad)

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
    xaxs = "i",
    cex.axis = 0.8,
    cex.lab = 0.8,
    cex.main = 0.9
  )
  abline(h = 0, col = "grey60", lwd = 0.8)
  lines(h, b2$lo, lty = 3, lwd = 0.9)
  lines(h, b2$hi, lty = 3, lwd = 0.9)
  lines(h, b1$lo, lty = 2, lwd = 0.9)
  lines(h, b1$hi, lty = 2, lwd = 0.9)
  lines(h, b1$mean, lwd = 1.5)
}

bands_list <- list()

for (var_name in names(industries)) {
  rdata_file <- file.path(industry_dir, paste0(var_name, ".RData"))
  cat(" ->", var_name, ":", rdata_file, "\n")

  load(rdata_file)

  bands_list[[var_name]] <- make_bands(var_model, response = var_name)

  cat("    готово\n")
}

pdf(
  "original_paper_results/fig6_industry_irf.pdf",
  width = 9,
  height = 6,
  pointsize = 9
)
par(
  mfrow = c(3, 4),
  mar = c(4.5, 5.5, 3, 1),
  oma = c(0.5, 0.5, 1.5, 0),
  mgp = c(3.2, 0.8, 0),
  tcl = -0.25
)

for (sh in SHOCKS) {
  for (var_name in names(industries)) {
    b1 <- bands_list[[var_name]]$b1[[sh]]
    b2 <- bands_list[[var_name]]$b2[[sh]]

    title <- industries[var_name]
    ylab <- if (var_name == names(industries)[1]) shock_labels[sh] else ""

    plot_panel(b1, b2, title = title, ylab = ylab)
  }
}

dev.off()
