library(readxl)
library(vars)

df <- read_excel("russian_data/processed/DATASET-rub.xlsx")

yt <- as.matrix(df[, c("delta_non_rus", "IGREA", "rpo" ,"ret_t")])
colnames(yt) <- c("delta_prod", "rea", "rpo", "r")

stopifnot(!anyNA(yt))
cat("Наблюдений:", nrow(yt), "\n")

lag_selection <- VARselect(
  yt,
  lag.max = 24,
  type = "const"
)

print(lag_selection)

cat("\nОптимальные лаги:\n")
print(lag_selection$selection)

# AIC
p_aic <- lag_selection$selection["AIC(n)"]

cat("\nAIC выбрал p =", p_aic, "\n")

var_model <- VAR(
  yt,
  p = 12,
  type = "const"
)

cat("\nМаксимальный модуль корня:\n")
print(max(Mod(roots(var_model))))
sort(Mod(roots(var_model)), decreasing = TRUE)

P <- t(chol(summary(var_model)$covres))
print(round(P, 4))

model_id <- "model_rub"
supply_source <- "delta_non_rus"

save(
  var_model,
  model_id,
  supply_source,
  file = "russian_paper_results/model_rub/svar_results_without_cur.RData"
)
