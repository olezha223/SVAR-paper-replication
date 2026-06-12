# ============================================================
#  IRF — Kilian & Park (2009)
#  Читает svar_results.RData, строит кумулятивные IRF
#  как на рис. 1 и рис. 3 в статье:
#    - точечные оценки
#    - полосы ±1 и ±2 стандартных ошибки (bootstrap)
# ============================================================

library(vars)

# --- 0. Загрузка модели --------------------------------------
load("original_paper_results/svar_results.RData")   # объекты: var_model, svar_model

# --- 1. Параметры --------------------------------------------
n_ahead <- 15      # горизонт (как на графиках в статье)
n_boot  <- 2000    # число bootstrap-репликаций
seed    <- 42

shock_names <- c(
  "delta_prod" = "Oil supply shock",
  "rea"        = "Aggregate demand shock",
  "rpo"        = "Oil-specific demand shock",
  "r"          = "Other shocks to stock returns"
)

var_names <- c(
  "delta_prod" = "World Oil Production",
  "rea"        = "Real Economic Activity",
  "rpo"        = "Real Price of Oil",
  "r"          = "U.S. Stock Returns"
)

# --- 2. Bootstrap IRF ----------------------------------------
# ci = 0.682 даёт ±1 std.err. при нормальности,
# ci = 0.954 даёт ±2 std.err.
# Строим оба набора отдельно.

set.seed(seed)
irf_1se <- irf(svar_model,
               impulse   = names(shock_names),
               response  = names(var_names),
               n.ahead   = n_ahead,
               boot      = TRUE,
               ci        = 0.682,
               runs      = n_boot,
               cumulative = TRUE)

set.seed(seed)
irf_2se <- irf(svar_model,
               impulse   = names(shock_names),
               response  = names(var_names),
               n.ahead   = n_ahead,
               boot      = TRUE,
               ci        = 0.954,
               runs      = n_boot,
               cumulative = TRUE)

# --- 3. Функция построения одного графика --------------------
# Воспроизводит стиль рис. 1 / рис. 3:
#   сплошная линия  = точечная оценка
#   штриховая       = ±1 std.err.
#   пунктирная      = ±2 std.err.

plot_irf_panel <- function(irf1, irf2, shock, response,
                           shock_label, resp_label, n_ahead) {

  horizons <- 0:n_ahead

  y_mean  <- irf1$irf[[shock]][,   response]
  y_lo1   <- irf1$Lower[[shock]][, response]
  y_hi1   <- irf1$Upper[[shock]][, response]
  y_lo2   <- irf2$Lower[[shock]][, response]
  y_hi2   <- irf2$Upper[[shock]][, response]

  ylim <- range(c(y_mean, y_lo1, y_hi1, y_lo2, y_hi2), na.rm = TRUE)
  # небольшой отступ
  pad  <- diff(ylim) * 0.1
  ylim <- ylim + c(-pad, pad)

  plot(horizons, y_mean,
       type = "l", lwd = 1.5,
       ylim = ylim,
       xlab = "Months", ylab = "",
       main = paste0(shock_label, "\n", resp_label),
       bty  = "l", xaxs = "i")

  abline(h = 0, lty = 1, col = "grey60", lwd = 0.8)

  # ±2 std.err. — пунктир
  lines(horizons, y_lo2, lty = 3, lwd = 1)
  lines(horizons, y_hi2, lty = 3, lwd = 1)

  # ±1 std.err. — штриховая
  lines(horizons, y_lo1, lty = 2, lwd = 1)
  lines(horizons, y_hi1, lty = 2, lwd = 1)

  # точечная оценка поверх
  lines(horizons, y_mean, lwd = 1.5)
}

# --- 4. Рис. 1: реакция rpo на три нефтяных шока -------------
# (аналог Figure 1 в статье)

pdf("irf_fig1_rpo.pdf", width = 10, height = 3.5)
par(mfrow = c(1, 3), mar = c(4, 3, 3, 1), oma = c(0, 0, 1, 0))

for (sh in c("delta_prod", "rea", "rpo")) {
  plot_irf_panel(irf_1se, irf_2se,
                 shock     = sh,
                 response  = "rpo",
                 shock_label = shock_names[sh],
                 resp_label  = "Real price of oil",
                 n_ahead   = n_ahead)
}

mtext("Figure 1. Responses of the Real Price of Crude Oil",
      outer = TRUE, cex = 1, font = 2)
dev.off()
cat("Сохранено: irf_fig1_rpo.pdf\n")

# --- 5. Рис. 3 (верхняя панель): реакция r на три шока -------
# (аналог Figure 3, upper panel)

pdf("irf_fig3_stocks.pdf", width = 10, height = 3.5)
par(mfrow = c(1, 3), mar = c(4, 3, 3, 1), oma = c(0, 0, 1, 0))

for (sh in c("delta_prod", "rea", "rpo")) {
  plot_irf_panel(irf_1se, irf_2se,
                 shock     = sh,
                 response  = "r",
                 shock_label = shock_names[sh],
                 resp_label  = "Cumulative Real Stock Returns (%)",
                 n_ahead   = n_ahead)
}

mtext("Figure 3. Cumulative Responses of U.S. Real Stock Returns",
      outer = TRUE, cex = 1, font = 2)
dev.off()
cat("Сохранено: irf_fig3_stocks.pdf\n")

# --- 6. Полная матрица IRF (4×4) для справки -----------------

pdf("irf_full_matrix.pdf", width = 14, height = 14)
par(mfrow = c(4, 4), mar = c(3, 3, 2.5, 1))

for (sh in names(shock_names)) {
  for (resp in names(var_names)) {
    plot_irf_panel(irf_1se, irf_2se,
                   shock       = sh,
                   response    = resp,
                   shock_label = shock_names[sh],
                   resp_label  = var_names[resp],
                   n_ahead     = n_ahead)
  }
}
dev.off()
cat("Сохранено: irf_full_matrix.pdf\n")

cat("\nГотово.\n")
