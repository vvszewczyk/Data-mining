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


# ============================================================
# 11. Tabele liczności dla zmiennych jakościowych z hipotez
# ============================================================

freq_y <- bank %>% tabyl(y)
freq_poutcome <- bank %>% tabyl(poutcome)
freq_housing <- bank %>% tabyl(housing)
freq_loan <- bank %>% tabyl(loan)
freq_job <- bank %>% tabyl(job)
freq_marital <- bank %>% tabyl(marital)
freq_education <- bank %>% tabyl(education)

freq_y
freq_poutcome
freq_housing
freq_loan
freq_job
freq_marital
freq_education

write_csv(freq_y, "output/tables/freq_y.csv")
write_csv(freq_poutcome, "output/tables/freq_poutcome.csv")
write_csv(freq_housing, "output/tables/freq_housing.csv")
write_csv(freq_loan, "output/tables/freq_loan.csv")
write_csv(freq_job, "output/tables/freq_job.csv")
write_csv(freq_marital, "output/tables/freq_marital.csv")
write_csv(freq_education, "output/tables/freq_education.csv")

# ============================================================
# 12. Tabele wielodzielcze dla zmiennych jakościowych
# ============================================================

cross_y_poutcome <- bank %>%
  tabyl(poutcome, y) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2)

cross_housing_marital <- bank %>%
  tabyl(marital, housing) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2)

cross_loan_education <- bank %>%
  tabyl(education, loan) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2)

cross_y_poutcome
cross_housing_marital
cross_loan_education

write.csv(cross_y_poutcome, "output/tables/cross_y_poutcome.csv", row.names = FALSE)
write.csv(cross_housing_marital, "output/tables/cross_housing_marital.csv", row.names = FALSE)
write.csv(cross_loan_education, "output/tables/cross_loan_education.csv", row.names = FALSE)

# ============================================================
# 13. Histogramy skategoryzowane
# ============================================================

# Histogram czasu kontaktu względem decyzji o założeniu lokaty
hist_duration_y <- ggplot(bank, aes(x = duration)) +
  geom_histogram(bins = 40) +
  facet_wrap(~ y) +
  labs(
    title = "Rozkład czasu kontaktu względem decyzji o założeniu lokaty",
    x = "Czas ostatniego kontaktu [sekundy]",
    y = "Liczba obserwacji"
  )

hist_duration_y

ggsave(
  "output/figures/hist_duration_y.png",
  plot = hist_duration_y,
  width = 8,
  height = 5
)

# Histogram wieku względem posiadania kredytu mieszkaniowego
hist_age_housing <- ggplot(bank, aes(x = age)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ housing) +
  labs(
    title = "Rozkład wieku względem posiadania kredytu mieszkaniowego",
    x = "Wiek",
    y = "Liczba obserwacji"
  )

hist_age_housing

ggsave(
  "output/figures/hist_age_housing.png",
  plot = hist_age_housing,
  width = 8,
  height = 5
)

# Histogram wieku względem posiadania pożyczki osobistej
hist_age_loan <- ggplot(bank, aes(x = age)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ loan) +
  labs(
    title = "Rozkład wieku względem posiadania pożyczki osobistej",
    x = "Wiek",
    y = "Liczba obserwacji"
  )

hist_age_loan

ggsave(
  "output/figures/hist_age_loan.png",
  plot = hist_age_loan,
  width = 8,
  height = 5
)

# Histogram liczby kontaktów względem decyzji o założeniu lokaty
hist_campaign_y <- ggplot(bank, aes(x = campaign)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ y) +
  labs(
    title = "Rozkład liczby kontaktów względem decyzji o założeniu lokaty",
    x = "Liczba kontaktów w kampanii",
    y = "Liczba obserwacji"
  )

hist_campaign_y

ggsave(
  "output/figures/hist_campaign_y.png",
  plot = hist_campaign_y,
  width = 8,
  height = 5
)

# ============================================================
# 14. Wykresy średnich w grupach
# ============================================================

mean_duration_y <- ggplot(bank, aes(x = y, y = duration)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(
    title = "Średni czas kontaktu względem decyzji o założeniu lokaty",
    x = "Założenie lokaty",
    y = "Średni czas kontaktu [sekundy]"
  )

mean_duration_y

ggsave(
  "output/figures/mean_duration_y.png",
  plot = mean_duration_y,
  width = 7,
  height = 5
)

mean_campaign_y <- ggplot(bank, aes(x = y, y = campaign)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(
    title = "Średnia liczba kontaktów względem decyzji o założeniu lokaty",
    x = "Założenie lokaty",
    y = "Średnia liczba kontaktów"
  )

mean_campaign_y

ggsave(
  "output/figures/mean_campaign_y.png",
  plot = mean_campaign_y,
  width = 7,
  height = 5
)

mean_age_housing <- ggplot(bank, aes(x = housing, y = age)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(
    title = "Średni wiek względem posiadania kredytu mieszkaniowego",
    x = "Kredyt mieszkaniowy",
    y = "Średni wiek"
  )

mean_age_housing

ggsave(
  "output/figures/mean_age_housing.png",
  plot = mean_age_housing,
  width = 7,
  height = 5
)

mean_age_loan <- ggplot(bank, aes(x = loan, y = age)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(
    title = "Średni wiek względem posiadania pożyczki osobistej",
    x = "Pożyczka osobista",
    y = "Średni wiek"
  )

mean_age_loan

ggsave(
  "output/figures/mean_age_loan.png",
  plot = mean_age_loan,
  width = 7,
  height = 5
)

mean_tables <- list(
  duration_by_y = bank %>% group_by(y) %>% summarise(mean_duration = mean(duration)),
  campaign_by_y = bank %>% group_by(y) %>% summarise(mean_campaign = mean(campaign)),
  age_by_housing = bank %>% group_by(housing) %>% summarise(mean_age = mean(age)),
  age_by_loan = bank %>% group_by(loan) %>% summarise(mean_age = mean(age))
)

mean_tables

# ============================================================
# 15. Macierz korelacji dla zmiennych ilościowych
# ============================================================

numeric_vars <- bank %>%
  select(where(is.numeric))

cor_matrix <- cor(numeric_vars, use = "complete.obs")

cor_matrix

write.csv(
  cor_matrix,
  "output/tables/correlation_matrix.csv"
)

png(
  filename = "output/figures/correlation_matrix.png",
  width = 1000,
  height = 800
)

corrplot(
  cor_matrix,
  method = "number",
  type = "upper",
  tl.cex = 0.9,
  number.cex = 0.8
)

dev.off()

cor_matrix

png(
  filename = "output/figures/correlation_matrix_readable.png",
  width = 1000,
  height = 800
)

corrplot(
  cor_matrix,
  method = "number",
  type = "upper",
  col = "black",
  number.cex = 0.9,
  tl.cex = 1,
  diag = FALSE
)

dev.off()