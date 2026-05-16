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

# ============================================================
# 16. Testy Chi-kwadrat dla zmiennych z hipotez
# ============================================================

# ----------------------------
# Hipoteza 1: y ~ duration + campaign + poutcome
# duration i campaign są ilościowe, więc trzeba je skategoryzować
# poutcome jest jakościowe
# ----------------------------

bank <- bank %>%
  mutate(
    duration_group = cut(
      duration,
      breaks = quantile(duration, probs = seq(0, 1, 0.25), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("niski", "sredni_niski", "sredni_wysoki", "wysoki")
    ),
    campaign_group = cut(
      campaign,
      breaks = c(0, 1, 2, 3, Inf),
      labels = c("1", "2", "3", "4+"),
      include.lowest = TRUE
    )
  )

chi_y_poutcome <- stats::chisq.test(table(bank$y, bank$poutcome))
chi_y_duration <- stats::chisq.test(table(bank$y, bank$duration_group))
chi_y_campaign <- stats::chisq.test(table(bank$y, bank$campaign_group))

chi_y_poutcome
chi_y_duration
chi_y_campaign


# ----------------------------
# Hipoteza 2: housing ~ age + job + marital
# age jest ilościowe, więc tworzymy grupy wieku
# job i marital są jakościowe
# ----------------------------

bank <- bank %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(17, 30, 45, 60, Inf),
      labels = c("18-30", "31-45", "46-60", "60+"),
      include.lowest = TRUE
    )
  )

chi_housing_age <- stats::chisq.test(table(bank$housing, bank$age_group))
chi_housing_job <- stats::chisq.test(table(bank$housing, bank$job))
chi_housing_marital <- stats::chisq.test(table(bank$housing, bank$marital))

chi_housing_age
chi_housing_job
chi_housing_marital


# ----------------------------
# Hipoteza 3: loan ~ age + education + job
# age jest ilościowe, więc używamy age_group
# education i job są jakościowe
# ----------------------------

chi_loan_age <- stats::chisq.test(table(bank$loan, bank$age_group))
chi_loan_education <- stats::chisq.test(table(bank$loan, bank$education))
chi_loan_job <- stats::chisq.test(table(bank$loan, bank$job))

chi_loan_age
chi_loan_education
chi_loan_job

chi_results <- data.frame(
  hipoteza = c(
    "H1", "H1", "H1",
    "H2", "H2", "H2",
    "H3", "H3", "H3"
  ),
  zmienna_zalezna = c(
    "y", "y", "y",
    "housing", "housing", "housing",
    "loan", "loan", "loan"
  ),
  predyktor = c(
    "poutcome", "duration_group", "campaign_group",
    "age_group", "job", "marital",
    "age_group", "education", "job"
  ),
  chi_square = c(
    as.numeric(chi_y_poutcome$statistic),
    as.numeric(chi_y_duration$statistic),
    as.numeric(chi_y_campaign$statistic),
    as.numeric(chi_housing_age$statistic),
    as.numeric(chi_housing_job$statistic),
    as.numeric(chi_housing_marital$statistic),
    as.numeric(chi_loan_age$statistic),
    as.numeric(chi_loan_education$statistic),
    as.numeric(chi_loan_job$statistic)
  ),
  df = c(
    as.numeric(chi_y_poutcome$parameter),
    as.numeric(chi_y_duration$parameter),
    as.numeric(chi_y_campaign$parameter),
    as.numeric(chi_housing_age$parameter),
    as.numeric(chi_housing_job$parameter),
    as.numeric(chi_housing_marital$parameter),
    as.numeric(chi_loan_age$parameter),
    as.numeric(chi_loan_education$parameter),
    as.numeric(chi_loan_job$parameter)
  ),
  p_value = c(
    chi_y_poutcome$p.value,
    chi_y_duration$p.value,
    chi_y_campaign$p.value,
    chi_housing_age$p.value,
    chi_housing_job$p.value,
    chi_housing_marital$p.value,
    chi_loan_age$p.value,
    chi_loan_education$p.value,
    chi_loan_job$p.value
  )
)

chi_results

write.csv(
  chi_results,
  "output/tables/chi_square_results.csv",
  row.names = FALSE
)

# ============================================================
# 17. Diagram ważności zmiennych na podstawie testu Chi-kwadrat
# ============================================================

cramers_v <- function(x, y) {
  tab <- table(x, y)
  chi <- stats::chisq.test(tab)
  n <- sum(tab)
  k <- min(nrow(tab) - 1, ncol(tab) - 1)
  sqrt(as.numeric(chi$statistic) / (n * k))
}

chi_importance <- data.frame(
  hipoteza = c(
    "H1", "H1", "H1",
    "H2", "H2", "H2",
    "H3", "H3", "H3"
  ),
  zmienna_zalezna = c(
    "y", "y", "y",
    "housing", "housing", "housing",
    "loan", "loan", "loan"
  ),
  predyktor = c(
    "poutcome", "duration_group", "campaign_group",
    "age_group", "job", "marital",
    "age_group", "education", "job"
  ),
  v_cramera = c(
    cramers_v(bank$poutcome, bank$y),
    cramers_v(bank$duration_group, bank$y),
    cramers_v(bank$campaign_group, bank$y),
    cramers_v(bank$age_group, bank$housing),
    cramers_v(bank$job, bank$housing),
    cramers_v(bank$marital, bank$housing),
    cramers_v(bank$age_group, bank$loan),
    cramers_v(bank$education, bank$loan),
    cramers_v(bank$job, bank$loan)
  )
)

chi_importance

write.csv(
  chi_importance,
  "output/tables/chi_square_importance.csv",
  row.names = FALSE
)

importance_plot <- ggplot(
  chi_importance,
  aes(x = reorder(paste(hipoteza, predyktor, sep = ": "), v_cramera),
      y = v_cramera)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Diagram ważności predyktorów na podstawie testu Chi-kwadrat",
    x = "Predyktor",
    y = "V Craméra"
  )

importance_plot

ggsave(
  "output/figures/chi_square_importance.png",
  plot = importance_plot,
  width = 9,
  height = 6
)

# ============================================================
# 18. Wykresy ramka-wąsy
# ============================================================

# Boxplot dla wieku
box_age <- ggplot(bank, aes(y = age)) +
  geom_boxplot() +
  labs(
    title = "Wykres ramka-wąsy dla wieku",
    y = "Wiek"
  )

box_age

ggsave(
  "output/figures/box_age.png",
  plot = box_age,
  width = 6,
  height = 5
)

# Boxplot dla czasu kontaktu
box_duration <- ggplot(bank, aes(y = duration)) +
  geom_boxplot() +
  labs(
    title = "Wykres ramka-wąsy dla czasu ostatniego kontaktu",
    y = "Czas kontaktu [sekundy]"
  )

box_duration

ggsave(
  "output/figures/box_duration.png",
  plot = box_duration,
  width = 6,
  height = 5
)

# Boxplot dla liczby kontaktów
box_campaign <- ggplot(bank, aes(y = campaign)) +
  geom_boxplot() +
  labs(
    title = "Wykres ramka-wąsy dla liczby kontaktów",
    y = "Liczba kontaktów"
  )

box_campaign

ggsave(
  "output/figures/box_campaign.png",
  plot = box_campaign,
  width = 6,
  height = 5
)

# Boxplot skategoryzowany: duration względem y
box_duration_y <- ggplot(bank, aes(x = y, y = duration)) +
  geom_boxplot() +
  labs(
    title = "Czas kontaktu względem decyzji o założeniu lokaty",
    x = "Założenie lokaty",
    y = "Czas kontaktu [sekundy]"
  )

box_duration_y

ggsave(
  "output/figures/box_duration_y.png",
  plot = box_duration_y,
  width = 7,
  height = 5
)

# Boxplot skategoryzowany: age względem housing
box_age_housing <- ggplot(bank, aes(x = housing, y = age)) +
  geom_boxplot() +
  labs(
    title = "Wiek względem posiadania kredytu mieszkaniowego",
    x = "Kredyt mieszkaniowy",
    y = "Wiek"
  )

box_age_housing

ggsave(
  "output/figures/box_age_housing.png",
  plot = box_age_housing,
  width = 7,
  height = 5
)

# Opcjonalnie: age względem loan
box_age_loan <- ggplot(bank, aes(x = loan, y = age)) +
  geom_boxplot() +
  labs(
    title = "Wiek względem posiadania pożyczki osobistej",
    x = "Pożyczka osobista",
    y = "Wiek"
  )

box_age_loan

ggsave(
  "output/figures/box_age_loan.png",
  plot = box_age_loan,
  width = 7,
  height = 5
)

# ============================================================
# 19. Test normalności i wartości odstające
# ============================================================

# Test Shapiro-Wilka działa w R maksymalnie dla 5000 obserwacji,
# dlatego losujemy próbkę z każdej zmiennej.

set.seed(123)

shapiro_age <- shapiro.test(sample(bank$age, 5000))
shapiro_duration <- shapiro.test(sample(bank$duration, 5000))
shapiro_campaign <- shapiro.test(sample(bank$campaign, 5000))

shapiro_age
shapiro_duration
shapiro_campaign

# Tabela wyników testu normalności

normality_results <- data.frame(
  zmienna = c("age", "duration", "campaign"),
  W = c(
    as.numeric(shapiro_age$statistic),
    as.numeric(shapiro_duration$statistic),
    as.numeric(shapiro_campaign$statistic)
  ),
  p_value = c(
    shapiro_age$p.value,
    shapiro_duration$p.value,
    shapiro_campaign$p.value
  )
)

normality_results

write.csv(
  normality_results,
  "output/tables/normality_results.csv",
  row.names = FALSE
)

# Wykresy Q-Q

png("output/figures/qq_age.png", width = 800, height = 600)
qqnorm(bank$age, main = "Wykres Q-Q dla zmiennej age")
qqline(bank$age)
dev.off()

png("output/figures/qq_duration.png", width = 800, height = 600)
qqnorm(bank$duration, main = "Wykres Q-Q dla zmiennej duration")
qqline(bank$duration)
dev.off()

png("output/figures/qq_campaign.png", width = 800, height = 600)
qqnorm(bank$campaign, main = "Wykres Q-Q dla zmiennej campaign")
qqline(bank$campaign)
dev.off()

# Funkcja do wykrywania wartości odstających metodą IQR

detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  
  lower_bound <- q1 - 1.5 * iqr_value
  upper_bound <- q3 + 1.5 * iqr_value
  
  x < lower_bound | x > upper_bound
}

# Oznaczenie wartości odstających

bank <- bank %>%
  mutate(
    outlier_age = detect_outliers(age),
    outlier_duration = detect_outliers(duration),
    outlier_campaign = detect_outliers(campaign)
  )

# Tabela liczby wartości odstających

outlier_results <- data.frame(
  zmienna = c("age", "duration", "campaign"),
  liczba_odstajacych = c(
    sum(bank$outlier_age, na.rm = TRUE),
    sum(bank$outlier_duration, na.rm = TRUE),
    sum(bank$outlier_campaign, na.rm = TRUE)
  ),
  udzial_odstajacych = c(
    mean(bank$outlier_age, na.rm = TRUE),
    mean(bank$outlier_duration, na.rm = TRUE),
    mean(bank$outlier_campaign, na.rm = TRUE)
  )
)

outlier_results

write.csv(
  outlier_results,
  "output/tables/outlier_results.csv",
  row.names = FALSE
)

# ============================================================
# 20. Wykresy rozrzutu dla par zmiennych skorelowanych
# ============================================================

# 1. pdays - previous
scatter_pdays_previous <- ggplot(bank, aes(x = pdays, y = previous)) +
  geom_point(alpha = 0.2) +
  labs(
    title = "Wykres rozrzutu: pdays i previous",
    x = "Liczba dni od poprzedniego kontaktu",
    y = "Liczba wcześniejszych kontaktów"
  )

scatter_pdays_previous

ggsave(
  "output/figures/scatter_pdays_previous.png",
  plot = scatter_pdays_previous,
  width = 7,
  height = 5
)

# 2. day - campaign
scatter_day_campaign <- ggplot(bank, aes(x = day, y = campaign)) +
  geom_point(alpha = 0.2) +
  labs(
    title = "Wykres rozrzutu: day i campaign",
    x = "Dzień miesiąca",
    y = "Liczba kontaktów w kampanii"
  )

scatter_day_campaign

ggsave(
  "output/figures/scatter_day_campaign.png",
  plot = scatter_day_campaign,
  width = 7,
  height = 5
)

# 3. duration - campaign
scatter_duration_campaign <- ggplot(bank, aes(x = duration, y = campaign)) +
  geom_point(alpha = 0.2) +
  labs(
    title = "Wykres rozrzutu: duration i campaign",
    x = "Czas ostatniego kontaktu [sekundy]",
    y = "Liczba kontaktów w kampanii"
  )

scatter_duration_campaign

ggsave(
  "output/figures/scatter_duration_campaign.png",
  plot = scatter_duration_campaign,
  width = 7,
  height = 5
)

# ============================================================
# 21. Wykresy rozrzutu skategoryzowane względem zmiennej y
# ============================================================

scatter_pdays_previous_y <- ggplot(bank, aes(x = pdays, y = previous)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ y) +
  labs(
    title = "Wykres rozrzutu: pdays i previous względem y",
    x = "Liczba dni od poprzedniego kontaktu",
    y = "Liczba wcześniejszych kontaktów"
  )

scatter_pdays_previous_y

ggsave(
  "output/figures/scatter_pdays_previous_y.png",
  plot = scatter_pdays_previous_y,
  width = 8,
  height = 5
)

scatter_day_campaign_y <- ggplot(bank, aes(x = day, y = campaign)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ y) +
  labs(
    title = "Wykres rozrzutu: day i campaign względem y",
    x = "Dzień miesiąca",
    y = "Liczba kontaktów w kampanii"
  )

scatter_day_campaign_y

ggsave(
  "output/figures/scatter_day_campaign_y.png",
  plot = scatter_day_campaign_y,
  width = 8,
  height = 5
)

scatter_duration_campaign_y <- ggplot(bank, aes(x = duration, y = campaign)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ y) +
  labs(
    title = "Wykres rozrzutu: duration i campaign względem y",
    x = "Czas ostatniego kontaktu [sekundy]",
    y = "Liczba kontaktów w kampanii"
  )

scatter_duration_campaign_y

ggsave(
  "output/figures/scatter_duration_campaign_y.png",
  plot = scatter_duration_campaign_y,
  width = 8,
  height = 5
)

# ============================================================
# III. Indukcja drzew decyzyjnych
# ============================================================

library(rpart)
library(rpart.plot)

# ------------------------------------------------------------
# Drzewo dla hipotezy 1: y ~ duration + campaign + poutcome
# ------------------------------------------------------------

tree_h1 <- rpart(
  y ~ duration + campaign + poutcome,
  data = bank,
  method = "class",
  control = rpart.control(
    cp = 0.01,
    minsplit = 100,
    minbucket = 50
  )
)

tree_h1

png(
  filename = "output/figures/tree_h1_y.png",
  width = 1200,
  height = 800
)

rpart.plot(
  tree_h1,
  type = 3,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Drzewo decyzyjne dla zmiennej y"
)

dev.off()


# ------------------------------------------------------------
# Drzewo dla hipotezy 2: housing ~ age + job + marital
# ------------------------------------------------------------

tree_h2 <- rpart(
  housing ~ age + job + marital,
  data = bank,
  method = "class",
  control = rpart.control(
    cp = 0.01,
    minsplit = 100,
    minbucket = 50
  )
)

tree_h2

png(
  filename = "output/figures/tree_h2_housing.png",
  width = 1300,
  height = 800
)

rpart.plot(
  tree_h2,
  type = 3,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Drzewo decyzyjne dla zmiennej housing"
)

dev.off()


# ------------------------------------------------------------
# Drzewo dla hipotezy 3: loan ~ age + education + job
# ------------------------------------------------------------

tree_h3 <- rpart(
  loan ~ age + education + job,
  data = bank,
  method = "class",
  parms = list(prior = c("no" = 0.5, "yes" = 0.5)),
  control = rpart.control(
    cp = 0.001,
    minsplit = 100,
    minbucket = 50,
    maxdepth = 4
  )
)

tree_h3

png(
  filename = "output/figures/tree_h3_loan.png",
  width = 1600,
  height = 700
)

rpart.plot(
  tree_h3,
  type = 3,
  extra = 104,
  fallen.leaves = TRUE,
  cex = 0.6,
  tweak = 1.1,
  main = "Drzewo decyzyjne dla zmiennej loan"
)

dev.off()

# ============================================================
# Ocena drzew: macierze klasyfikacji i błąd całkowity
# ============================================================

evaluate_tree <- function(model, data, target_var) {
  predicted <- predict(model, data, type = "class")
  actual <- data[[target_var]]
  
  confusion_matrix <- table(
    Rzeczywiste = actual,
    Przewidywane = predicted
  )
  
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  error_rate <- 1 - accuracy
  
  list(
    confusion_matrix = confusion_matrix,
    accuracy = accuracy,
    error_rate = error_rate
  )
}

eval_h1 <- evaluate_tree(tree_h1, bank, "y")
eval_h2 <- evaluate_tree(tree_h2, bank, "housing")
eval_h3 <- evaluate_tree(tree_h3, bank, "loan")

eval_h1
eval_h2
eval_h3

# ============================================================
# Ważność predyktorów w drzewach decyzyjnych
# ============================================================

importance_h1 <- data.frame(
  predyktor = names(tree_h1$variable.importance),
  waznosc = as.numeric(tree_h1$variable.importance),
  hipoteza = "H1"
)

importance_h2 <- data.frame(
  predyktor = names(tree_h2$variable.importance),
  waznosc = as.numeric(tree_h2$variable.importance),
  hipoteza = "H2"
)

importance_h3 <- data.frame(
  predyktor = names(tree_h3$variable.importance),
  waznosc = as.numeric(tree_h3$variable.importance),
  hipoteza = "H3"
)

importance_all <- rbind(importance_h1, importance_h2, importance_h3)

importance_h1
importance_h2
importance_h3

write.csv(importance_all, "output/tables/tree_variable_importance.csv", row.names = FALSE)

importance_plot_tree <- ggplot(
  importance_all,
  aes(x = reorder(paste(hipoteza, predyktor, sep = ": "), waznosc),
      y = waznosc)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Ważność predyktorów w drzewach decyzyjnych CART",
    x = "Predyktor",
    y = "Ważność"
  )

importance_plot_tree

ggsave(
  "output/figures/tree_variable_importance.png",
  plot = importance_plot_tree,
  width = 9,
  height = 6
)

# ============================================================
# Reguły decyzyjne z drzew
# ============================================================

rules_h1 <- rpart.rules(tree_h1)
rules_h2 <- rpart.rules(tree_h2)
rules_h3 <- rpart.rules(tree_h3)

rules_h1
rules_h2
rules_h3

capture.output(rules_h1, file = "output/tables/rules_tree_h1.txt")
capture.output(rules_h2, file = "output/tables/rules_tree_h2.txt")
capture.output(rules_h3, file = "output/tables/rules_tree_h3.txt")

# ============================================================
# Wykresy macierzy klasyfikacji
# ============================================================

plot_confusion_matrix <- function(eval_object, title, file_name) {
  cm_df <- as.data.frame(eval_object$confusion_matrix)
  
  plot <- ggplot(cm_df, aes(x = Przewidywane, y = Rzeczywiste, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), size = 6) +
    labs(
      title = title,
      x = "Klasa przewidywana",
      y = "Klasa rzeczywista"
    )
  
  ggsave(
    file_name,
    plot = plot,
    width = 6,
    height = 5
  )
  
  plot
}

conf_plot_h1 <- plot_confusion_matrix(
  eval_h1,
  "Macierz klasyfikacji dla drzewa H1",
  "output/figures/confusion_tree_h1.png"
)

conf_plot_h2 <- plot_confusion_matrix(
  eval_h2,
  "Macierz klasyfikacji dla drzewa H2",
  "output/figures/confusion_tree_h2.png"
)

conf_plot_h3 <- plot_confusion_matrix(
  eval_h3,
  "Macierz klasyfikacji dla drzewa H3",
  "output/figures/confusion_tree_h3.png"
)