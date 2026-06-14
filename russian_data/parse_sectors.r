# ============================================================
# ОТРАСЛЕВЫЕ ИНДЕКСЫ MOEX
#
# MOEXOG — нефть и газ
# MOEXCN — потребительский сектор
# MOEXMM — металлы и добыча
#
# Сохраняются только:
# 1. дневные уровни и лог-доходности
# 2. месячные уровни и лог-доходности
# ============================================================

# ------------------------------------------------------------
# ПАКЕТЫ
# ------------------------------------------------------------

packages <- c(
  "httr",
  "jsonlite",
  "dplyr",
  "tidyr",
  "purrr",
  "readr"
)

new_packages <- packages[
  !(packages %in% installed.packages()[, "Package"])
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)


# ------------------------------------------------------------
# ПАРАМЕТРЫ
# ------------------------------------------------------------

date_from <- as.Date("2000-01-01")
date_to <- as.Date("2014-12-31")

output_dir <- "./SVAR-paper-replication/russian_data"

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)


# ------------------------------------------------------------
# ИНДЕКСЫ
# ------------------------------------------------------------

indices <- tibble(
  variable = c(
    "oil_gas",
    "consumer",
    "metals_mining"
  ),

  sector = c(
    "Нефть и газ",
    "Потребительский сектор",
    "Металлы и добыча"
  ),

  secid = c(
    "MOEXOG",
    "MOEXCN",
    "MOEXMM"
  )
)


# ------------------------------------------------------------
# ПРЕОБРАЗОВАНИЕ СТРОК JSON В ОБЫЧНУЮ ТАБЛИЦУ
# ------------------------------------------------------------

history_rows_to_tibble <- function(rows, columns) {
  if (length(rows) == 0) {
    return(tibble())
  }

  matrix_data <- do.call(
    rbind,
    lapply(
      rows,
      function(row) {
        vapply(
          seq_along(columns),
          function(i) {
            value <- row[[i]]

            if (
              is.null(value) ||
                length(value) == 0
            ) {
              return(NA_character_)
            }

            as.character(value[[1]])
          },
          character(1)
        )
      }
    )
  )

  result <- as.data.frame(
    matrix_data,
    stringsAsFactors = FALSE
  )

  names(result) <- columns

  as_tibble(result)
}


# ------------------------------------------------------------
# ЗАГРУЗКА ОДНОГО ИНДЕКСА
# ------------------------------------------------------------

load_moex_index <- function(secid, from, till) {
  url <- paste0(
    "https://iss.moex.com/iss/history/",
    "engines/stock/",
    "markets/index/",
    "boards/SNDX/",
    "securities/",
    secid,
    ".json"
  )

  parts <- list()
  start <- 0

  repeat {
    response <- RETRY(
      verb = "GET",
      url = url,

      query = list(
        from = as.character(from),
        till = as.character(till),
        start = start,
        limit = 100,
        "iss.meta" = "off",
        "iss.only" = "history",
        "history.columns" = "TRADEDATE,CLOSE"
      ),

      user_agent(
        "R MOEX sector index downloader"
      ),

      timeout(60),

      times = 5,
      pause_base = 1
    )

    stop_for_status(response)

    response_text <- content(
      response,
      as = "text",
      encoding = "UTF-8"
    )

    result <- fromJSON(
      response_text,
      simplifyVector = FALSE
    )

    if (is.null(result$history)) {
      stop(
        paste(
          "MOEX не вернул раздел history для",
          secid
        )
      )
    }

    rows <- result$history$data

    if (
      is.null(rows) ||
        length(rows) == 0
    ) {
      break
    }

    columns <- unlist(
      result$history$columns,
      use.names = FALSE
    )

    part <- history_rows_to_tibble(
      rows = rows,
      columns = columns
    )

    if (nrow(part) == 0) {
      break
    }

    parts[[length(parts) + 1]] <- part

    message(
      secid,
      ": загружено строк ",
      start + 1,
      "–",
      start + nrow(part)
    )

    start <- start + nrow(part)

    Sys.sleep(0.1)
  }

  if (length(parts) == 0) {
    stop(
      paste(
        "Для индекса",
        secid,
        "данные не найдены"
      )
    )
  }

  result <- bind_rows(parts)

  result %>%
    transmute(
      TRADEDATE = as.Date(
        as.character(TRADEDATE),
        format = "%Y-%m-%d"
      ),

      CLOSE = as.numeric(
        as.character(CLOSE)
      )
    ) %>%
    filter(
      !is.na(TRADEDATE),
      !is.na(CLOSE),
      CLOSE > 0,
      TRADEDATE >= from,
      TRADEDATE <= till
    ) %>%
    arrange(TRADEDATE) %>%
    distinct(
      TRADEDATE,
      .keep_all = TRUE
    )
}


# ------------------------------------------------------------
# ЗАГРУЗКА ВСЕХ ТРЁХ ИНДЕКСОВ
# ------------------------------------------------------------

indices_long <- pmap_dfr(
  indices,
  function(variable, sector, secid) {
    message("")

    message(
      "Загружается: ",
      sector,
      " — ",
      secid
    )

    load_moex_index(
      secid = secid,
      from = date_from,
      till = date_to
    ) %>%
      mutate(
        variable = variable
      )
  }
)


# ------------------------------------------------------------
# ПРОВЕРКА ПЕРИОДОВ
# ------------------------------------------------------------

coverage <- indices_long %>%
  group_by(variable) %>%
  summarise(
    first_date = min(TRADEDATE),
    last_date = max(TRADEDATE),
    observations = n(),
    .groups = "drop"
  )

message("")
message("Доступные периоды:")

print(coverage)


# ------------------------------------------------------------
# ДНЕВНЫЕ ЛОГАРИФМИЧЕСКИЕ ДОХОДНОСТИ
# ------------------------------------------------------------

indices_daily_long <- indices_long %>%
  group_by(variable) %>%
  arrange(
    TRADEDATE,
    .by_group = TRUE
  ) %>%
  mutate(
    log_return = log(CLOSE) -
      log(lag(CLOSE))
  ) %>%
  ungroup()


# ------------------------------------------------------------
# ДНЕВНАЯ ИТОГОВАЯ ТАБЛИЦА
# ------------------------------------------------------------

daily_levels <- indices_daily_long %>%
  select(
    TRADEDATE,
    variable,
    CLOSE
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = CLOSE
  )

daily_returns <- indices_daily_long %>%
  select(
    TRADEDATE,
    variable,
    log_return
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = log_return,
    names_glue = "{variable}_log_return"
  )

indices_daily <- daily_levels %>%
  left_join(
    daily_returns,
    by = "TRADEDATE"
  ) %>%
  arrange(TRADEDATE)


# ------------------------------------------------------------
# МЕСЯЧНЫЕ УРОВНИ
#
# Берётся последнее торговое значение каждого месяца.
# ------------------------------------------------------------

indices_monthly_long <- indices_long %>%
  mutate(
    date = as.Date(
      format(
        TRADEDATE,
        "%Y-%m-01"
      )
    )
  ) %>%
  group_by(
    variable,
    date
  ) %>%
  slice_max(
    order_by = TRADEDATE,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  group_by(variable) %>%
  arrange(
    date,
    .by_group = TRUE
  ) %>%
  mutate(
    log_return = log(CLOSE) -
      log(lag(CLOSE))
  ) %>%
  ungroup()


# ------------------------------------------------------------
# МЕСЯЧНАЯ ИТОГОВАЯ ТАБЛИЦА
# ------------------------------------------------------------

monthly_levels <- indices_monthly_long %>%
  select(
    date,
    variable,
    CLOSE
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = CLOSE
  )

monthly_returns <- indices_monthly_long %>%
  select(
    date,
    variable,
    log_return
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = log_return,
    names_glue = "{variable}_log_return"
  )

indices_monthly <- monthly_levels %>%
  left_join(
    monthly_returns,
    by = "date"
  ) %>%
  arrange(date)


# ------------------------------------------------------------
# СОХРАНЕНИЕ ТОЛЬКО ДВУХ ТАБЛИЦ
# ------------------------------------------------------------

write_csv(
  indices_daily,
  file.path(
    output_dir,
    "moex_sector_indices_daily_2000_2014.csv"
  )
)

write_csv(
  indices_monthly,
  file.path(
    output_dir,
    "moex_sector_indices_monthly_2000_2014.csv"
  )
)


# ------------------------------------------------------------
# РЕЗУЛЬТАТ
# ------------------------------------------------------------

message("")
message("Готово.")

message(
  "Файлы сохранены в папку: ",
  normalizePath(output_dir)
)

message("")
message("Дневная таблица:")

print(
  head(indices_daily)
)

message("")
message("Месячная таблица:")

print(
  head(indices_monthly)
)
