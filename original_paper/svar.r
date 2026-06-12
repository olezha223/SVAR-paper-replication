# --- 0. Пакеты -----------------------------------------------
if (!require("readxl")) install.packages("readxl")
if (!require("vars"))   install.packages("vars")

library(readxl)
library(vars)

# --- 1. Данные -----------------------------------------------
df <- read_excel("original_data/processed/DATASET.xlsx")

cat("Колонки:", paste(names(df), collapse = ", "), "\n")
cat("Наблюдений:", nrow(df), "\n")

# Порядок переменных строго соответствует уравнению (2) в статье:
#   delta_world  = Δprod_t   % change in global crude oil production
#   rea_t        = real economic activity index
#   rpo          = real price of oil
#   ret_t        = U.S. real stock returns
yt <- as.matrix(df[, c("delta_world", "rea_t", "rpo", "ret_t")])
colnames(yt) <- c("delta_prod", "rea", "rpo", "r")

plot(df$rpo, type = "l")
plot(df$ret_t, type = "l")

df$rpo = df$rpo |> diff()
df$rea_t = df$rea_t |> diff()



# --- 2. Приведённая форма VAR(24) ----------------------------
# p = 24 фиксировано согласно спецификации статьи (уравнение 1)
p_lag <- 24

var_model <- VAR(yt, p = p_lag, type = "const")
cat("\n--- Приведённая форма VAR(24) оценена ---\n")

# --- 3. SVAR: рекурсивная идентификация ----------------------
#
#  Идентифицирующие ограничения (уравнение 2 в статье):
#
#  e_t = A0^{-1} ε_t,  где A0 — нижнетреугольная матрица:
#
#        [ 1    0    0    0  ]
#  A0 =  [ a21  1    0    0  ]
#        [ a31  a32  1    0  ]
#        [ a41  a42  a43  1  ]
#
#  Нули в верхнем треугольнике:
#   (1,2),(1,3),(1,4) — нефтяное производство не реагирует
#                       на спрос внутри месяца
#   (2,3),(2,4)       — реальная активность не реагирует на
#                       цену нефти и акции внутри месяца
#   (3,4)             — цена нефти не реагирует на фондовый
#                       рынок внутри месяца
#
#  В пакете vars: Amat задаёт A0, NA = свободный параметр.

k <- ncol(yt)   # k = 4

Amat <- matrix(c(
   1,   0,   0,   0,
  NA,   1,   0,   0,
  NA,  NA,   1,   0,
  NA,  NA,  NA,   1
), nrow = k, byrow = TRUE)

# Bmat = NULL соответствует Cov(ε_t) = I (единичная дисперсия шоков)
Bmat <- NULL

cat("\n--- Оценка SVAR (метод maximum likelihood, прямая параметризация) ---\n")
svar_model <- SVAR(var_model,
                   estmethod = "direct",
                   Amat      = Amat,
                   Bmat      = Bmat,
                   max.iter  = 1000,
                   conv.crit = 1e-8)

# --- 4. Результаты -------------------------------------------
cat("\n--- Оценённая матрица A0 ---\n")
print(round(svar_model$A, 4))

cat("\n--- A0^{-1} (impact matrix: столбец j = реакция e_t на ε_j) ---\n")
print(round(solve(svar_model$A), 4))

cat("\n--- Логарифм функции правдоподобия ---\n")
cat("LR-статистика:", svar_model$LR$statistic, "\n")
cat("p-value:       ", svar_model$LR$p.value, "\n")

cat("\n--- Полная сводка SVAR ---\n")
print(summary(svar_model))

# --- 5. Сохранение -------------------------------------------
save(var_model, svar_model, file = "original_paper_results/svar_results.RData")
cat("\nОбъекты сохранены в original_paper_results/svar_results.RData\n")
