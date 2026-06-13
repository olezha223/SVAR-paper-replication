library(vars)

load("russian_paper_results/model_1/svar_results.RData")

H      <- 15
RUNS   <- 2000
SHOCKS <- c("delta_prod", "rea", "rpo")

shock_labels <- c(
  delta_prod = "Oil supply shock",
  rea        = "Aggregate demand shock",
  rpo        = "Oil-specific demand shock"
)

# --- 1. Bootstrap IRF -----------------------------------------
# ci = 0.682 ~ +-1 SE, ci = 0.954 ~ +-2 SE
get_irf <- function(response, cumulative, ci) {
  set.seed(42)
  irf(var_model,
      impulse    = SHOCKS,
      response   = response,
      n.ahead    = H,
      ortho      = TRUE,
      boot       = TRUE,
      ci         = ci,
      runs       = RUNS,
      cumulative = cumulative)
}

# --- 2. Извлечение одного отклика с нормировкой знака ---------
extract <- function(irf_obj, shock, response) {
  m  <- irf_obj$irf[[shock]][,   response]
  lo <- irf_obj$Lower[[shock]][, response]
  hi <- irf_obj$Upper[[shock]][, response]
  if (shock == "delta_prod") {            # знак: negative supply shock
    m   <- -m
    tmp <- lo
    lo  <- -hi
    hi  <- -tmp
  }
  list(mean = m, lo = lo, hi = hi)
}

# --- 3. Панель в стиле статьи ---------------------------------
plot_panel <- function(b1, b2, title, ylab) {
  h    <- 0:H
  ylim <- range(b1$mean, b2$lo, b2$hi)
  ylim <- ylim + diff(ylim) * c(-0.1, 0.1)
  plot(h, b1$mean, type = "l", lwd = 1.5, ylim = ylim,
       xlab = "Months", ylab = ylab, main = title, bty = "l", xaxs = "i")
  abline(h = 0, col = "grey60", lwd = 0.8)
  lines(h, b2$lo, lty = 3)                 # +-2 SE
  lines(h, b2$hi, lty = 3)
  lines(h, b1$lo, lty = 2)                 # +-1 SE
  lines(h, b1$hi, lty = 2)
  lines(h, b1$mean, lwd = 1.5)
}

make_figure <- function(response, cumulative, ylab, file, main) {
  irf1 <- get_irf(response, cumulative, 0.682)
  irf2 <- get_irf(response, cumulative, 0.954)
  pdf(file, width = 10, height = 3.5)
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 1.5, 0))
  for (sh in SHOCKS) {
    plot_panel(extract(irf1, sh, response),
               extract(irf2, sh, response),
               title = shock_labels[sh], ylab = ylab)
  }
  mtext(main, outer = TRUE, font = 2)
  dev.off()
  cat("Сохранено:", file, "\n")
}

# --- 4. Figure 1: отклик реальной цены нефти (НЕ кумулятивный)
make_figure("rpo", cumulative = FALSE,
            ylab = "Real price of oil",
            file = "russian_paper_results/model_1/irf_fig1_rpo.pdf",
            main = "Figure 1. Responses of the Real Price of Crude Oil (Russian Model v1)")

# --- 5. Figure 3: кумулятивный отклик реальных доходностей ----
make_figure("r", cumulative = TRUE,
            ylab = "Cumulative Real Stock Returns (%)",
            file = "russian_paper_results/model_1/irf_fig3_stocks.pdf",
            main = "Figure 3. Cumulative Responses of Russian Real Stock Returns (Russian Model v1)")
