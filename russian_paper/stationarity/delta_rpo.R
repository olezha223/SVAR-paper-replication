library(readxl)
library(urca)


var_name <- "delta_rpo" # название колонки в датасете
specification <- "drift" # "none", "drift", "trend"
selectlags <- "BIC" # "BIC", "AIC", "Fixed"

df <- read_excel("russian_data/processed/DATASET-stationary.xlsx")
df$date <- as.Date(df$date)

series <- df[[var_name]]
T_eff <- sum(!is.na(series))
p_max <- min(24, floor(T_eff^(1/3)))

cat(sprintf("Переменная: %s\n", var_name))
cat(sprintf("Спецификация: %s\n", specification))
cat(sprintf("Выбор лага: %s  (p_max = %d)\n", selectlags, p_max))

adf <- ur.df(na.omit(series),
             type       = specification,
             lags       = p_max,
             selectlags = selectlags)

print(summary(adf))

tau_col <- rownames(adf@cval)[1]
tau_stat <- adf@teststat[1, tau_col]
cv <- adf@cval[tau_col, ]

reject_1pct <- tau_stat < cv["1pct"]
reject_5pct <- tau_stat < cv["5pct"]
reject_10pct <- tau_stat < cv["10pct"]

lag_used <- adf@lags

result <- data.frame(
  variable = var_name,
  specification = specification,
  selectlags = selectlags,
  lag_used  = lag_used,
  T_obs = T_eff,
  tau_stat = round(tau_stat, 4),
  cv_1pct = cv["1pct"],
  cv_5pct = cv["5pct"],
  cv_10pct = cv["10pct"],
  reject_1pct = reject_1pct,
  reject_5pct = reject_5pct,
  reject_10pct = reject_10pct,
  stringsAsFactors = FALSE
)

out_dir <- file.path("russian_paper_results", "stationarity")
out_file <- file.path(out_dir, paste0(var_name, "-ADF.csv"))
write.csv(result, out_file, row.names = FALSE)
