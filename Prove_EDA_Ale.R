# ========== INIZIO EDA ==========

# ============================================================
# 📊 Analisi esplorativa - Visualizzazioni principali
# Solo per gender: "Male" e "Female"
# ============================================================

# 11. 📦 Boxplot globale dei tre indicatori
ggplot(df_alcohol_clean %>% filter(gender %in% c("Male", "Female")),
       aes(x = indicator_short, y = value, fill = indicator_short)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribuzione globale dei 3 indicatori sull'alcol (solo M/F)",
       x = "Indicatore", y = "Valore") +
  theme_minimal() +
  theme(legend.position = "none")

# 12. 💰 Boxplot per income group
ggplot(df_alcohol_clean %>% filter(gender %in% c("Male", "Female")),
       aes(x = income_group, y = value, fill = indicator_short)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  labs(title = "Distribuzione degli indicatori per gruppo di reddito (solo M/F)",
       x = "Gruppo di reddito", y = "Valore", fill = "Indicatore") +
  theme_minimal() +
  coord_flip()

# 13. 📈 Trend globale nel tempo (media annuale per indicatore)
df_alcohol_clean %>%
  filter(gender %in% c("Male", "Female")) %>%
  group_by(year, indicator_short) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_value, color = indicator_short)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Trend globale nel tempo (media per indicatore)",
       subtitle = "Solo M/F", x = "Anno", y = "Valore medio", color = "Indicatore") +
  theme_minimal()

# 14. 🌍 Trend per regione OMS - solo % consumatori
df_alcohol_clean %>%
  filter(indicator_short == "Perc_Cons", gender %in% c("Male", "Female")) %>%
  group_by(year, who_region, gender) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_value, color = gender)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ who_region) +
  labs(
    title = "% di consumatori di alcol per regione OMS",
    subtitle = "Suddiviso per genere (escluso Both sexes)",
    x = "Anno", y = "% Consumatori", color = "Genere"
  ) +
  theme_minimal()

# 15. 🔥 Heatmap per % consumo alcol - separata per genere

# Funzione per creare la heatmap
plot_heatmap_by_gender <- function(g) {
  df_alcohol_clean %>%
    filter(indicator_short == "Perc_Cons", gender == g) %>%
    group_by(year, who_region) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = who_region, values_from = mean_value) %>%
    pivot_longer(cols = -year, names_to = "region", values_to = "value") %>%
    ggplot(aes(x = region, y = factor(year), fill = value)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(name = "%", na.value = "grey90") +
    labs(
      title = paste("Heatmap: % consumatori di alcol -", g),
      x = "Regione OMS", y = "Anno", fill = "% Consumatori"
    ) +
    theme_minimal()
}

# 📊 Heatmap per ciascun genere
plot_heatmap_by_gender("Male")
plot_heatmap_by_gender("Female")

# Funzione per calcolare variazioni anno su anno e creare heatmap
plot_heatmap_delta_by_gender <- function(g) {
  df_delta <- df_alcohol_clean %>%
    filter(indicator_short == "Perc_Cons", gender == g) %>%
    group_by(year, who_region) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(who_region, year) %>%
    group_by(who_region) %>%
    mutate(delta = mean_value - lag(mean_value)) %>%   # differenza assoluta
    ungroup()
  
  # Preparazione formato long per heatmap
  df_delta_wide <- df_delta %>%
    select(year, who_region, delta) %>%
    pivot_wider(names_from = who_region, values_from = delta)
  
  df_delta_long <- df_delta_wide %>%
    pivot_longer(cols = -year, names_to = "region", values_to = "delta")
  
  # Grafico heatmap
  ggplot(df_delta_long, aes(x = region, y = factor(year), fill = delta)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      name = "Δ% rispetto all'anno precedente",
      low = "blue", mid = "white", high = "red",
      midpoint = 0, na.value = "grey90"
    ) +
    labs(
      title = paste("Heatmap variazione % consumatori rispetto all'anno precedente -", g),
      x = "Regione OMS", y = "Anno"
    ) +
    theme_minimal()
}

# Esegui per entrambi i generi
plot_heatmap_delta_by_gender("Male")
plot_heatmap_delta_by_gender("Female")

# Funzione per creare heatmap delle variazioni annuali per astemi
plot_heatmap_delta_astemi_by_gender <- function(g) {
  df_delta <- df_alcohol_clean %>%
    filter(indicator_short == "Perc_Ast", gender == g) %>%
    group_by(year, who_region) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(who_region, year) %>%
    group_by(who_region) %>%
    mutate(delta = mean_value - lag(mean_value)) %>%   # differenza rispetto all'anno precedente
    ungroup()
  
  # Preparazione formato lungo
  df_delta_long <- df_delta %>%
    select(year, region = who_region, delta)
  
  # Heatmap
  ggplot(df_delta_long, aes(x = region, y = factor(year), fill = delta)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      name = "Δ% astemi rispetto anno prec.",
      low = "blue", mid = "white", high = "red",
      midpoint = 0, na.value = "grey90"
    ) +
    labs(
      title = paste("Heatmap variazione % astemi -", g),
      x = "Regione OMS", y = "Anno"
    ) +
    theme_minimal()
}

# 🔁 Esegui per entrambi i generi
plot_heatmap_delta_astemi_by_gender("Male")
plot_heatmap_delta_astemi_by_gender("Female")

# Funzione per heatmap variazioni consumo tra i soli bevitori
plot_heatmap_delta_litres_by_gender <- function(g) {
  df_delta <- df_alcohol_clean %>%
    filter(indicator_short == "Litres_Cons", gender == g) %>%
    group_by(year, who_region) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(who_region, year) %>%
    group_by(who_region) %>%
    mutate(delta = mean_value - lag(mean_value)) %>%   # differenza anno su anno
    ungroup()
  
  df_delta_long <- df_delta %>%
    select(year, region = who_region, delta)
  
  ggplot(df_delta_long, aes(x = region, y = factor(year), fill = delta)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      name = "Δ litri/anno rispetto anno prec.",
      low = "blue", mid = "white", high = "red",
      midpoint = 0, na.value = "grey90"
    ) +
    labs(
      title = paste("Heatmap variazione consumo alcol tra bevitori -", g),
      x = "Regione OMS", y = "Anno"
    ) +
    theme_minimal()
}

# 🔁 Esegui per "Male" e "Female"
plot_heatmap_delta_litres_by_gender("Male")
plot_heatmap_delta_litres_by_gender("Female")


library(dplyr)
library(tidyr)
library(ggplot2)

df_gendergap_region <- df_alcohol_clean %>%
  filter(indicator_short == "Perc_Cons", gender %in% c("Male", "Female")) %>%
  select(country, year, who_region, gender, value) %>%
  pivot_wider(names_from = gender, values_from = value) %>%
  mutate(gender_gap = Male - Female) %>%
  drop_na(gender_gap) %>%
  group_by(year, who_region) %>%
  summarise(
    mean_gap = mean(gender_gap, na.rm = TRUE),
    .groups = "drop"
  )

# Lineplot: 1 riga per ogni regione OMS
ggplot(df_gendergap_region, aes(x = year, y = mean_gap, color = who_region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Evoluzione del gender gap medio (% consumatori) per regione OMS",
    subtitle = "Gap = Male - Female",
    x = "Anno", y = "Gap medio (%)",
    color = "Regione OMS"
  ) +
  theme_minimal()

# Calcolo del gender gap per anno e regione
df_gendergap_delta <- df_alcohol_clean %>%
  filter(indicator_short == "Perc_Cons", gender %in% c("Male", "Female")) %>%
  select(country, year, who_region, gender, value) %>%
  pivot_wider(names_from = gender, values_from = value) %>%
  mutate(gender_gap = Male - Female) %>%
  drop_na(gender_gap) %>%
  group_by(year, who_region) %>%
  summarise(
    mean_gap = mean(gender_gap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(who_region, year) %>%
  group_by(who_region) %>%
  mutate(
    gap_lag = lag(mean_gap),
    delta_rel = (mean_gap - gap_lag) / abs(gap_lag)
  ) %>%
  ungroup()

# Rimuove variazioni infinite o NA (es. primo anno o cambio di segno con 0)
df_gendergap_delta <- df_gendergap_delta %>%
  filter(is.finite(delta_rel), !is.na(delta_rel))

# Visualizzazione
ggplot(df_gendergap_delta, aes(x = year, y = delta_rel, color = who_region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Variazione relativa del gender gap (% consumatori) per regione",
    subtitle = "Δrel = (gap_t - gap_t-1) / |gap_t-1|",
    x = "Anno", y = "Variazione relativa del gap",
    color = "Regione OMS"
  ) +
  theme_minimal()

df_litres_gap <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons", gender %in% c("Male", "Female")) %>%
  select(country, year, who_region, gender, value) %>%
  pivot_wider(names_from = gender, values_from = value) %>%
  mutate(litres_gap = Male - Female) %>%
  drop_na(litres_gap) %>%
  group_by(year, who_region) %>%
  summarise(
    mean_gap = mean(litres_gap, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(df_litres_gap, aes(x = year, y = mean_gap, color = who_region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Gender gap nel consumo di alcol tra i bevitori (litri pro capite)",
    subtitle = "Gap = Male - Female (Litres_Cons)",
    x = "Anno", y = "Differenza media in litri",
    color = "Regione OMS"
  ) +
  theme_minimal()

df_litres_gap_delta <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons", gender %in% c("Male", "Female")) %>%
  select(country, year, who_region, gender, value) %>%
  pivot_wider(names_from = gender, values_from = value) %>%
  mutate(litres_gap = Male - Female) %>%
  drop_na(litres_gap) %>%
  group_by(year, who_region) %>%
  summarise(mean_gap = mean(litres_gap, na.rm = TRUE), .groups = "drop") %>%
  arrange(who_region, year) %>%
  group_by(who_region) %>%
  mutate(delta_rel = (mean_gap - lag(mean_gap)) / abs(lag(mean_gap))) %>%
  ungroup() %>%
  filter(is.finite(delta_rel))

ggplot(df_litres_gap_delta, aes(x = year, y = delta_rel, color = who_region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Variazione relativa del gender gap su Litres_Cons",
    subtitle = "Δrel = (gap_t - gap_t-1) / |gap_t-1|",
    x = "Anno", y = "Variazione relativa", color = "Regione OMS"
  ) +
  theme_minimal()

library(dplyr)
library(tidyr)
library(scales)  # per normalizzazione

# Seleziona l'ultimo anno disponibile e solo maschi o femmine (qui facciamo generic, es. "Male")
ultimo_anno <- max(df_alcohol_clean$year, na.rm = TRUE)

df_profile <- df_alcohol_clean %>%
  filter(year == ultimo_anno, gender == "Male") %>%
  select(country, indicator_short, value) %>%
  pivot_wider(names_from = indicator_short, values_from = value) %>%
  drop_na()  # serve avere tutti e tre gli indicatori

# Normalizza su range [0,1] – min‑max per ciascun indicatore
df_profile_norm <- df_profile %>%
  mutate(
    Perc_Cons_norm = rescale(Perc_Cons),
    Litres_Cons_norm = rescale(Litres_Cons),
    Perc_Ast_norm = rescale(Perc_Ast)
  ) %>%
  select(country, Perc_Cons_norm, Litres_Cons_norm, Perc_Ast_norm)

library(fmsb)
library(scales)
library(dplyr)

# Paesi da confrontare
paesi <- c("Italy", "United States")

# Filtra e normalizza
df_radar <- df_profile_norm %>%
  filter(country %in% paesi) %>%
  select(-country) %>%
  as.data.frame()  # 🔧 fondamentale

# Verifica il numero di righe attese
n <- nrow(df_radar)

# Crea riga max e min
max_row <- rep(1, ncol(df_radar))
min_row <- rep(0, ncol(df_radar))

# Unisci
radar2 <- rbind(max_row, min_row, df_radar)

# Assegna rownames correttamente
rownames(radar2) <- c("Max", "Min", paesi[1:n])

# Radar chart
radarchart(
  radar2,
  pcol = rainbow(n),
  plwd = 2,
  plty = 1:n,
  pfcol = scales::alpha(rainbow(n), 0.3),
  title = paste("Profilo alcolico –", paste(paesi[1:n], collapse = " vs "))
)

legend("topright", legend = paesi[1:n], col = rainbow(n), pch = 15, pt.cex = 1.2, bty = "n")


