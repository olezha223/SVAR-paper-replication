# ============================================================
#  Fig. 6 — Cumulative IRF: industry-level stock returns
#  Kilian & Park (2009)
#
#  Читает готовые модели из:
#    original_paper_results/industry_results/<var_name>.RData
#  Каждый .RData содержит var_model для соответствующей отрасли.
# ============================================================

library(vars)

industry_dir <- file.path("original_paper_results", "industry_results")

H    <- 15
RUNS <- 2000

SHOCKS <- c("delta_prod", "rea", "rpo")
shock_labels <- c(
  delta_prod = "Oil supply shock",
  rea        = "Aggregate demand shock",
  rpo        = "Oil-specific demand shock"
)

# Отрасли: имя переменной -> подпись на графике
industries <- c(
  oil_industry = "Petroleum & Natural Gas",
  autos        = "Automobiles & Trucks",
  rtail        = "Retail",
  gold         = "Precious Metals"
)

# --- 1. Bootstrap IRF для одной модели -----------------------
get_irf <- function(var_model, response, ci) {
  set.seed(42)
  irf(var_model,
      impulse    = SHOCKS,
      response   = response,
      n.ahead    = H,
      ortho      = TRUE,
      boot       = TRUE,
      ci         = ci,
      runs       = RUNS,
      cumulative = TRUE)   # Fig 6 — кумулятивные, как Fig 3
}

# --- 2. Извлечение отклика + нормировка знака ----------------
extract <- function(irf_obj, shock, response) {
  m  <- irf_obj$irf[[shock]][,   response]
  lo <- irf_obj$Lower[[shock]][, response]
  hi <- irf_obj$Upper[[shock]][, response]
  if (shock == "delta_prod") {   # отрицательный supply shock
    m   <- -m
    tmp <- lo; lo <- -hi; hi <- -tmp
  }
  list(mean = m, lo = lo, hi = hi)
}

# --- 3. Одна панель ------------------------------------------
plot_panel <- function(b1, b2, title, ylab = "") {
  h    <- 0:H
  ylim <- range(b1$mean, b2$lo, b2$hi, na.rm = TRUE)
  pad  <- diff(ylim) * 0.12
  ylim <- ylim + c(-pad, pad)

  plot(h, b1$mean,
       type = "l", lwd = 1.5,
       ylim = ylim,
       xlab = "Months", ylab = ylab,
       main = title,
       bty  = "l", xaxs = "i")
  abline(h = 0, col = "grey60", lwd = 0.8)
  lines(h, b2$lo, lty = 3, lwd = 0.9)   # ±2 SE — пунктир
  lines(h, b2$hi, lty = 3, lwd = 0.9)
  lines(h, b1$lo, lty = 2, lwd = 0.9)   # ±1 SE — штриховая
  lines(h, b1$hi, lty = 2, lwd = 0.9)
  lines(h, b1$mean, lwd = 1.5)
}

# --- 4. Загружаем модели и считаем IRF -----------------------
cat("Загружаем модели и считаем IRF...\n")

irf1_list <- list()   # ±1 SE
irf2_list <- list()   # ±2 SE

for (var_name in names(industries)) {
  rdata_file <- file.path(industry_dir, paste0(var_name, ".RData"))
  cat(" ->", var_name, ":", rdata_file, "\n")

  load(rdata_file)   # загружает var_model

  irf1_list[[var_name]] <- get_irf(var_model, response = var_name, ci = 0.682)
  irf2_list[[var_name]] <- get_irf(var_model, response = var_name, ci = 0.954)

  cat("    готово\n")
}

# --- 5. Figure 6: 3 строки (шоки) × 4 столбца (отрасли) -----
pdf("fig6_industry_irf.pdf", width = 14, height = 10)
par(mfrow = c(3, 4),
    mar   = c(3.5, 3.5, 2.5, 1),
    oma   = c(1, 1, 2.5, 0))

for (sh in SHOCKS) {
  for (var_name in names(industries)) {
    b1 <- extract(irf1_list[[var_name]], sh, var_name)
    b2 <- extract(irf2_list[[var_name]], sh, var_name)

    # Заголовок только в первой строке, метка шока — в первом столбце
    title <- industries[var_name]
    ylab  <- if (var_name == names(industries)[1]) shock_labels[sh] else ""

    plot_panel(b1, b2, title = title, ylab = ylab)
  }
}

mtext("Figure 6. Cumulative Responses of U.S. Real Stock Returns by Industry\nwith One- and Two-Standard Error Bands",
      outer = TRUE, font = 2, cex = 1)
dev.off()
cat("\nСохранено: fig6_industry_irf.pdf\n")
