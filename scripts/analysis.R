# ============================================================
# PROJEKT: Bank Marketing
# ETAP II: czyszczenie i analiza danych
# ============================================================

# 1. Pakiety ---------------------------------------------------

library(readr)
library(dplyr)
library(magrittr)
library(janitor)
library(ggplot2)
library(corrplot)
library(randomForest)

# 2. Wczytanie danych ------------------------------------------

bank <- read_csv2("data/bank-full.csv", show_col_types = FALSE)

# 3. Czyszczenie nazw kolumn -----------------------------------
# clean_names() upraszcza nazwy kolumn: usuwa spacje,
# wielkie litery i znaki specjalne.

bank <- bank %>%
  clean_names()

# 4. Zamiana zmiennych tekstowych na jakościowe -----------------
# Zmienne tekstowe, np. job, marital, education i y,
# zamieniamy na factor, czyli zmienne jakościowe.

bank <- bank %>%
  mutate(across(where(is.character), as.factor))

# 5. Podstawowy podgląd danych ---------------------------------

glimpse(bank)
summary(bank)

# 6. Sprawdzenie wymiarów i nazw kolumn -------------------------

dim(bank)
names(bank)

# 7. Sprawdzenie braków danych ---------------------------------

missing_values <- colSums(is.na(bank))
missing_values

# 8. Sprawdzenie wartości "unknown" -----------------------------
# W tym zbiorze brak informacji często zapisano jako "unknown",
# a nie jako klasyczne NA.

unknown_values <- bank %>%
  summarise(across(where(is.factor), ~ sum(. == "unknown", na.rm = TRUE)))

unknown_values

# 9. Zapis oczyszczonego zbioru --------------------------------

write_csv(bank, "data/bank_clean.csv")

# ============================================================
# 10. Statystyki opisowe dla zmiennych ilościowych z hipotez
# ============================================================

# Funkcja pomocnicza do wyznaczania mody.
mode_value <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Zmienne ilościowe analizowane w hipotezach.
quant_vars_hyp <- bank %>%
  select(age, duration, campaign)

# Statystyki opisowe w układzie szerokim.
descriptive_stats <- quant_vars_hyp %>%
  summarise(across(
    everything(),
    list(
      n = ~sum(!is.na(.)),
      mean = ~mean(., na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      mode = ~mode_value(.),
      min = ~min(., na.rm = TRUE),
      max = ~max(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      var = ~var(., na.rm = TRUE)
    )
  ))

descriptive_stats

write_csv(descriptive_stats, "output/tables/descriptive_stats_quant_vars.csv")

# Statystyki opisowe w układzie tabelarycznym do raportu.
descriptive_stats_long <- data.frame(
  zmienna = c("age", "duration", "campaign"),
  n = c(
    sum(!is.na(bank$age)),
    sum(!is.na(bank$duration)),
    sum(!is.na(bank$campaign))
  ),
  srednia = c(
    mean(bank$age, na.rm = TRUE),
    mean(bank$duration, na.rm = TRUE),
    mean(bank$campaign, na.rm = TRUE)
  ),
  mediana = c(
    median(bank$age, na.rm = TRUE),
    median(bank$duration, na.rm = TRUE),
    median(bank$campaign, na.rm = TRUE)
  ),
  moda = c(
    mode_value(bank$age),
    mode_value(bank$duration),
    mode_value(bank$campaign)
  ),
  minimum = c(
    min(bank$age, na.rm = TRUE),
    min(bank$duration, na.rm = TRUE),
    min(bank$campaign, na.rm = TRUE)
  ),
  maksimum = c(
    max(bank$age, na.rm = TRUE),
    max(bank$duration, na.rm = TRUE),
    max(bank$campaign, na.rm = TRUE)
  ),
  odchylenie_std = c(
    sd(bank$age, na.rm = TRUE),
    sd(bank$duration, na.rm = TRUE),
    sd(bank$campaign, na.rm = TRUE)
  ),
  wariancja = c(
    var(bank$age, na.rm = TRUE),
    var(bank$duration, na.rm = TRUE),
    var(bank$campaign, na.rm = TRUE)
  )
)

descriptive_stats_long

write_csv(descriptive_stats_long, "output/tables/descriptive_stats_quant_vars.csv")
