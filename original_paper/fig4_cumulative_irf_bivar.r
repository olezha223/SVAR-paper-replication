# ============================================================
#  Fig. 4 — Cumulative response of U.S. real stock returns
#  to real oil price innovation (bivariate VAR)
#  Kilian & Park (2009)
# ============================================================

library(vars)

load("original_paper_results/bivariate_svar_results.RData")  # var_model

H    <- 15
RUNS <- 2000

# --- 1. Bootstrap IRF ----------------------------------------
get_irf <- function(ci) {
  set.seed(42)
  irf(var_model,
      impulse    = "rpo",
      response   = "r",
      n.ahead    = H,
      ortho      = TRUE,
      boot       = TRUE,
      ci         = ci,
      runs       = RUNS,
      cumulative = TRUE)
}

irf1 <- get_irf(0.682)   # ±1 SE
irf2 <- get_irf(0.954)   # ±2 SE

# --- 2. Извлекаем значения -----------------------------------
h      <- 0:H
y_mean <- irf1$irf[["rpo"]][,   "r"]
y_lo1  <- irf1$Lower[["rpo"]][, "r"]
y_hi1  <- irf1$Upper[["rpo"]][, "r"]
y_lo2  <- irf2$Lower[["rpo"]][, "r"]
y_hi2  <- irf2$Upper[["rpo"]][, "r"]

ylim <- range(y_mean, y_lo2, y_hi2)
ylim <- ylim + diff(ylim) * c(-0.1, 0.1)

# --- 3. График -----------------------------------------------
pdf("fig4_bivar_irf.pdf", width = 5, height = 4.5)
par(mar = c(4, 4, 3, 1))

plot(h, y_mean,
     type = "l", lwd = 1.5,
     ylim = ylim,
     xlab = "Months",
     ylab = "Cumulative Real Stock Returns (Percent)",
     main = "Real oil price shock",
     bty  = "l", xaxs = "i")

abline(h = 0, col = "grey60", lwd = 0.8)
lines(h, y_lo2, lty = 3, lwd = 0.9)   # ±2 SE — пунктир
lines(h, y_hi2, lty = 3, lwd = 0.9)
lines(h, y_lo1, lty = 2, lwd = 0.9)   # ±1 SE — штриховая
lines(h, y_hi1, lty = 2, lwd = 0.9)
lines(h, y_mean, lwd = 1.5)

dev.off()
cat("Сохранено: fig4_bivar_irf.pdf\n")
