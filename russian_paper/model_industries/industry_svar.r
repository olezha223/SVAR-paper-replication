library(readxl)
library(vars)

df <- read_excel("russian_data/processed/DATASET-industries.xlsx")

# Возможные варианты: consumer_log_return,metals_mining_log_return,oil_gas_log_return
var_name <- "oil_gas_log_return"   # МЕНЯТЬ ЗДЕСЬ НАЗВАНИЕ ПЕРЕМЕННОЙ
df[[var_name]] <- df[[var_name]] * 100

yt <- as.matrix(df[, c("delta_non_rus", "rea_t", "rpo", var_name)])
colnames(yt) <- c("delta_prod", "rea", "rpo", var_name)

stopifnot(!anyNA(yt))
cat("Наблюдений:", nrow(yt), "\n")

var_model <- VAR(yt, p = 24, type = "const")

P <- t(chol(summary(var_model)$covres))
print(round(P, 4))

industry_dir <- file.path("russian_paper_results", "industry_results")
output_file <- file.path(industry_dir, paste0(var_name, ".RData"))
save(var_model, file = output_file)
