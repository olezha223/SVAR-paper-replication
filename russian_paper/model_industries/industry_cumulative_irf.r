library(vars)

industry_dir <- file.path("russian_paper_results", "industry_results")

H <- 15
RUNS <- 2000

SHOCKS <- c("delta_prod", "rea", "rpo")
shock_labels <- c(
  delta_prod = "Oil supply shock",
  rea = "Aggregate demand shock",
  rpo = "Oil-specific demand shock"
)

industries <- c(
  oil_gas_log_return = "Oil & Gas",
  metals_mining_log_return = "Metals & Mining",
  consumer_log_return = "Consumer Goods"
)

get_irf <- function(var_model, response, ci) {
  set.seed(42)
  irf(var_model,
      impulse = SHOCKS,
      response = response,
      n.ahead = H,
      ortho = TRUE,
      boot = TRUE,
      ci = ci,
      runs = RUNS,
      cumulative = TRUE)
}

extract <- function(irf_obj, shock, response) {
  m <- irf_obj$irf[[shock]][, response]
  lo <- irf_obj$Lower[[shock]][, response]
  hi <- irf_obj$Upper[[shock]][, response]
  if (shock == "delta_prod") {
    m   <- -m
    tmp <- lo; lo <- -hi; hi <- -tmp
  }
  list(mean = m, lo = lo, hi = hi)
}

plot_panel <- function(b1, b2, title, ylab = "") {
  h <- 0:H
  ylim <- range(b1$mean, b2$lo, b2$hi, na.rm = TRUE)
  pad <- diff(ylim) * 0.12
  ylim <- ylim + c(-pad, pad)

  plot(h, b1$mean,
       type = "l", lwd = 1.5,
       ylim = ylim,
       xlab = "Months", ylab = ylab,
       main = title,
       bty = "l", xaxs = "i")
  abline(h = 0, col = "grey60", lwd = 0.8)
  lines(h, b2$lo, lty = 3, lwd = 0.9)
  lines(h, b2$hi, lty = 3, lwd = 0.9)
  lines(h, b1$lo, lty = 2, lwd = 0.9)
  lines(h, b1$hi, lty = 2, lwd = 0.9)
  lines(h, b1$mean, lwd = 1.5)
}

cat("Загружаем модели и считаем IRF...\n")

irf1_list <- list()
irf2_list <- list()

for (var_name in names(industries)) {
  rdata_file <- file.path(industry_dir, paste0(var_name, ".RData"))
  cat(" ->", var_name, ":", rdata_file, "\n")

  load(rdata_file)

  irf1_list[[var_name]] <- get_irf(var_model, response = var_name, ci = 0.682)
  irf2_list[[var_name]] <- get_irf(var_model, response = var_name, ci = 0.954)

  cat("    готово\n")
}

pdf("russian_paper_results/industry_results/fig6_industry_irf.pdf", width = 14, height = 10)
par(mfrow = c(3, 3),
    mar = c(3.5, 3.5, 2.5, 1),
    oma = c(1, 1, 2.5, 0))

for (sh in SHOCKS) {
  for (var_name in names(industries)) {
    b1 <- extract(irf1_list[[var_name]], sh, var_name)
    b2 <- extract(irf2_list[[var_name]], sh, var_name)

    title <- industries[var_name]
    ylab <- if (var_name == names(industries)[1]) shock_labels[sh] else ""

    plot_panel(b1, b2, title = title, ylab = ylab)
  }
}

mtext("Figure 6. Cumulative Responses of Russian Real Stock Returns by Industry\nwith One- and Two-Standard Error Bands",
      outer = TRUE, font = 2, cex = 1)
dev.off()
