# 1. Installa i pacchetti necessari (solo la prima volta)
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("writexl")

# 2. Carica le librerie
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(tidyr)


# 3. Controlla i fogli con percorso assoluto
sheets <- excel_sheets("C:/Users/rugge/Documents/progettoDSHS/data.xlsx")
print(sheets)

# 4. Importa il primo foglio
df <- read_excel("C:/Users/rugge/Documents/progettoDSHS/data.xlsx", sheet = sheets[1])



# 5. Esplora il dataset

# Mostra le prime righe
head(df)

# Visualizza i nomi delle colonne
colnames(df)

# Struttura generale delle variabili
str(df)

# Riepilogo statistico delle variabili numeriche
summary(df)

# Conta dei valori mancanti per colonna
colSums(is.na(df))

# Mostra i nomi unici degli indicatori
unique(df$indicator_name)


# Filtra solo le righe con uno dei 3 indicatori scelti
df_alcohol <- df %>%
  filter(indicator_name %in% c(
    "Alcohol, consumers past 12 months (%)",
    "Alcohol, drinkers only per capita (15+) consumption (in litres of pure alcohol)",
    "Alcohol, abstainers lifetime (%)"
  ))

# Controlla quante righe rimangono
nrow(df_alcohol)

# Dai un’occhiata ai primi risultati
head(df_alcohol)

# Seleziona solo le colonne necessarie e le rinomina
df_alcohol_clean <- df_alcohol %>%
  select(
    country       = setting,
    year          = date,
    indicator     = indicator_name,
    gender        = subgroup,
    value         = estimate,
    who_region    = whoreg6,
    income_group  = wbincome2024
  )

# Visualizza struttura e prime righe per verifica
glimpse(df_alcohol_clean)
head(df_alcohol_clean)

df_alcohol_clean <- df_alcohol_clean %>%
  mutate(indicator_short = case_when(
    indicator == "Alcohol, consumers past 12 months (%)" ~ "Perc_Cons",
    indicator == "Alcohol, drinkers only per capita (15+) consumption (in litres of pure alcohol)" ~ "Litres_Cons",
    indicator == "Alcohol, abstainers lifetime (%)" ~ "Perc_Ast",
    TRUE ~ NA_character_
  ))

table(df_alcohol_clean$indicator_short, useNA = "ifany")

# Salva attuale dataset
write_xlsx(df_alcohol_clean, "v2_df_clean1.xlsx")


# Controlla se ci sono valori percentuali fuori dal range 0-100
df_check_range <- df_alcohol_clean %>%
  filter(indicator_short %in% c("Perc_Cons", "Perc_Ast")) %>%   # Solo indicatori in percentuale
  filter(value < 0 | value > 100)

# Mostra le righe problematiche, se esistono
print(df_check_range)

# Quante sono?
nrow(df_check_range)

if (nrow(df_check_range) == 0) {
  cat("✅ Tutti i valori percentuali (P_Cons e P_Abst) sono nel range corretto [0, 100].\n")
} else {
  cat("⚠️ Attenzione: trovati", nrow(df_check_range), "valori percentuali fuori range!\n")
}

ggplot(df_alcohol_clean, aes(x = indicator_short, y = value, fill = indicator_short)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribuzioni a confronto", x = "Indicatore", y = "Valore") +
  theme_minimal()


#controllo valori NA nei 3 indicatori

df_alcohol_clean %>%
  group_by(indicator_short) %>%
  summarise(
    totale = n(),
    n_missing = sum(is.na(value)),
    percentuale_missing = round(100 * sum(is.na(value)) / n(), 2)
  )

#elimino i valori NA dato che sono pochi nei 3 indicatori

df_alcohol_clean <- df_alcohol_clean %>%
  filter(!(is.na(value) & indicator_short %in% c("Litres_Cons", "Perc_Ast", "Perc_Cons")))

#RI-controllo valori NA nei 3 indicatori

df_alcohol_clean %>%
  group_by(indicator_short) %>%
  summarise(
    totale = n(),
    n_missing = sum(is.na(value)),
    percentuale_missing = round(100 * sum(is.na(value)) / n(), 2)
  )

#salvo il nuovo df pulito come file

write_xlsx(df_alcohol_clean, "v3_df_clean_senza_NA.xlsx")


################################ valori anomali Litres_Cons

# Controlla il range dei valori per Litres_Cons (litri per capita tra i bevitori)
df_litres <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons")

# Mostra statistiche descrittive
summary(df_litres$value)

# Mostra i valori più alti (>50 litri)
df_litres %>%
  filter(value > 50) %>%
  arrange(desc(value)) %>%
  select(country, year, gender, value)

# Boxplot per individuare eventuali outlier visivamente
ggplot(df_litres, aes(x = gender, y = value)) +
  geom_boxplot() +
  labs(
    title = "Distribuzione del consumo alcolico tra i soli bevitori (L_Cons)",
    x = "Genere", y = "Litri per capita (15+)"
  ) +
  theme_minimal()

##########################################

#conto i valori nulli nelle colonne
colSums(is.na(df_alcohol_clean))

# ho valori nulli solo su income_group e li metto in categoria "unknown"
# così da non eliminarli perchè sono comunque utili per test dove non conta il gruppo di reddito

#lista dei paesi con categoria Unknown associata: Cook Islands, Niue e Venezuela
df_alcohol_clean <- df_alcohol_clean %>%
  mutate(income_group = ifelse(is.na(income_group), "Unknown", income_group))

#controllo i valori unici per ogni colonna (importante per i dati categorici e ordinali, no per i dati quantitativi (value))

df_alcohol_clean %>%
  summarise(across(everything(), n_distinct))


#stampo i valori unici presenti nelle colonne, per procedere alle visualizzazioni grafiche in maniera corretta

# Country (solo primi 10 per leggibilità)
cat("🌍 Paesi (n =", n_distinct(df_alcohol_clean$country), "):\n")
print(sort(unique(df_alcohol_clean$country))[1:10])
cat("... (totale 189 paesi)\n\n")

# Year
cat("📅 Anni disponibili (n =", n_distinct(df_alcohol_clean$year), "):\n")
print(sort(unique(df_alcohol_clean$year)))
cat("\n")

# Gender
cat("🧍‍♂️ Gender:\n")
print(unique(df_alcohol_clean$gender))
cat("\n")

# WHO Region
cat("🌐 WHO Regions:\n")
print(unique(df_alcohol_clean$who_region))
cat("\n")

# Income Group
cat("💰 Income Groups:\n")
print(unique(df_alcohol_clean$income_group))
cat("\n")

# Indicator Short
cat("📊 Indicatori analizzati:\n")
print(unique(df_alcohol_clean$indicator_short))
cat("\n")

#=================INIZIO EDA=====================

# Carica i pacchetti necessari
library(dplyr)
library(gridExtra)
library(grid)

# ===== primo Riepilogo statistico per indicatore e genere =====
# Questo blocco di codice calcola la media e la deviazione standard del consumo di alcol 
# per ciascun indicatore e genere nel dataset pulito. 
# I risultati sono raggruppati per 'indicator_short' e 'gender'.

summary_gender <- df_alcohol_clean %>%
  group_by(indicator_short, gender) %>%
  summarise(
    media = round(mean(value, na.rm = TRUE), 2),
    sd = round(sd(value, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Crea una tabella visualizzabile come plot
table_plot <- tableGrob(summary_gender, rows = NULL)

# Mostra la tabella su una nuova pagina grafica
grid.newpage()
grid.draw(table_plot)

# ===== Matrice di correlazione tra indicatori (Spearman) =====
# Calcola la correlazione di Spearman tra:
# - Perc_Cons (percentuale di consumatori)
# - Litres_Cons (litri pro capite tra i bevitori)
# - Perc_Ast (percentuale di astinenti a vita)
# e la mostra come tabella a schermo.

# Librerie necessarie
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)

# Calcolo media per ciascun indicatore e paese
df_corr <- df_alcohol_clean %>%
  group_by(country, indicator_short) %>%
  summarise(media = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicator_short, values_from = media) %>%
  drop_na()

# Calcolo della matrice di correlazione (Spearman)
corr_matrix <- df_corr %>%
  select(Perc_Cons, Litres_Cons, Perc_Ast) %>%
  cor(method = "spearman", use = "complete.obs")

# Conversione in data frame e arrotondamento
corr_df <- as.data.frame(round(corr_matrix, 2))

# Visualizzazione come tabella
corr_table <- tableGrob(corr_df, rows = NULL)

# Mostra la tabella a schermo
grid.newpage()
grid.draw(corr_table)


# ===== Grafico a barre della media per indicatore e genere con deviazione standard =====
# Questo plot mostra la media del valore di consumo per ciascun indicatore, separata per genere.
# Le barre rappresentano la media, mentre le linee di errore indicano la deviazione standard.

ggplot(summary_gender, aes(x = indicator_short, y = media, fill = gender)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = media - sd, ymax = media + sd),
                position = position_dodge(0.8), width = 0.2) +
  labs(
    title = "Media e deviazione standard per indicatore e genere",
    x = "Indicatore", y = "Media",
    fill = "Genere"
  ) +
  theme_minimal()

# ===== Trend globale nel tempo per indicatore (solo maschi e femmine) =====
# Questo grafico mostra l'evoluzione temporale della media globale dei tre indicatori principali,
# considerando solo i dati relativi a genere "Male" e "Female".
# Le linee rappresentano la media annuale del valore per ciascun indicatore.

library(ggplot2)
library(dplyr)
library(patchwork)
library(stringr)

# Filtra dati validi solo per M/F
df_filtered <- df_alcohol_clean %>%
  filter(gender %in% c("Male", "Female"))

# Plot 1: Percentuali
p_percentuali <- df_filtered %>%
  filter(indicator_short %in% c("Perc_Cons", "Perc_Ast")) %>%
  group_by(year, indicator_short) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_value, color = indicator_short)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Percentuali: astemi e consumatori",
    x = "Anno", y = "% Popolazione", color = "Indicatore"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box.just = "center"
  )

# Plot 2: Litri
p_litri <- df_filtered %>%
  filter(str_detect(indicator_short, "Lit")) %>%
  group_by(year, indicator_short) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_value, color = indicator_short)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Litri di alcol pro capite",
    x = "Anno", y = "Litri", color = "Indicatore"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box.just = "center"
  )

# Combina i due grafici con una sola legenda
(p_percentuali | p_litri) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")


# ===== Calcolo della media annuale per indicatore e genere =====
# Raggruppa i dati per indicatore, genere e anno, calcolando la media.

summary_by_year_gender <- df_alcohol_clean %>%
  group_by(indicator_short, gender, year) %>%
  summarise(media = mean(value), .groups = "drop")

# ===== Trend del consumo di alcol per anno, genere e indicatore =====
# Questo grafico mostra l'evoluzione temporale di tre indicatori di consumo alcolico:
# 1. Litri consumati per bevitore
# 2. Percentuale di astinenti a vita
# 3. Percentuale di consumatori negli ultimi 12 mesi
# I dati sono divisi per genere e disposti in pannelli separati.

# =======================
# LIBRERIE
# =======================
library(dplyr)
library(ggplot2)
library(stringr)

# =======================
# RICODIFICA REGIONI WHO CON SIGLE
# =======================
df_alcohol_clean <- df_alcohol_clean %>%
  mutate(
    who_region = case_when(
      str_detect(who_region, "Africa") ~ "AFRO",
      str_detect(who_region, "Americas") ~ "AMRO",
      str_detect(who_region, "South-East Asia") ~ "SEARO",
      str_detect(who_region, "European") ~ "EURO",
      str_detect(who_region, "Eastern Mediterranean") ~ "EMRO",
      str_detect(who_region, "Western Pacific") ~ "WPRO",
      TRUE ~ who_region
    )
  )

# =======================
# 1. TREND TEMPORALI PER GENERE E INDICATORE
# =======================
# Rinomina indicatori per leggibilità
summary_by_year_gender <- summary_by_year_gender %>%
  mutate(indicatore = recode(indicator_short,
                             "Litres_Cons" = "Litri per bevitore",
                             "Perc_Ast" = "Astinenti a vita",
                             "Perc_Cons" = "Consumatori 12 mesi"))

# Filtra gli indicatori di interesse
df_plot <- summary_by_year_gender %>%
  filter(indicator_short %in% c("Litres_Cons", "Perc_Ast", "Perc_Cons"))

# Plot temporale con facet per indicatore
ggplot(df_plot, aes(x = year, y = media, color = gender)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ indicatore, scales = "free_y") +
  labs(
    title = "Andamento del consumo di alcol nel tempo per genere",
    x = "Anno", y = "Valore medio", color = "Genere"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))


# =======================
# 2. VIOLIN PLOT: LITRI PER BEVITORE
# =======================
plot_litres <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons") %>%
  mutate(who_region = recode(who_region, !!!who_codes)) %>%
  ggplot(aes(x = who_region, y = value, fill = who_region)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "Distribuzione Litri per Bevitore per Regione WHO ",
    x = "Regione WHO", y = "Litri per capita (bevitori)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")

print(plot_litres)


# =======================
# 3. VIOLIN PLOT: % CONSUMATORI
# =======================
plot_cons <- df_alcohol_clean %>%
  filter(indicator_short == "Perc_Cons") %>%
  mutate(who_region = recode(who_region, !!!who_codes)) %>%
  ggplot(aes(x = who_region, y = value, fill = who_region)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "% Consumatori negli ultimi 12 mesi per Regione WHO (Sigle)",
    x = "Regione WHO", y = "% Consumatori"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")

print(plot_cons)


# =======================
# 4. VIOLIN PLOT: % ASTINENTI A VITA
# =======================
plot_abst <- df_alcohol_clean %>%
  filter(indicator_short == "Perc_Ast") %>%
  mutate(who_region = recode(who_region, !!!who_codes)) %>%
  ggplot(aes(x = who_region, y = value, fill = who_region)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "% Astinenti a Vita per Regione WHO",
    x = "Regione WHO", y = "% Astinenti"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")

print(plot_abst)



# ===== Riepilogo statistico per indicatore e fascia di reddito (income group) =====
# Questo blocco filtra i dati escludendo i record con income group sconosciuto,
# ordina le fasce di reddito in modo coerente, e calcola media e deviazione standard
# del valore per ciascuna combinazione di indicatore e income group.

summary_income <- df_alcohol_clean %>%
  filter(income_group != "Unknown") %>%
  mutate(income_group = factor(income_group, levels = c(
    "Low income",
    "Lower middle income",
    "Upper middle income",
    "High income"
  ))) %>%
  group_by(indicator_short, income_group) %>%
  summarise(
    media = mean(value),
    sd = sd(value),
    .groups = "drop"
  )

# ===== Barplot con media e deviazione standard per income group =====
# Il grafico mostra le medie dei valori per ciascun indicatore, separati per fascia di reddito.
# Le barre di errore rappresentano la deviazione standard, evitando valori negativi.
# Abbrevia i nomi dei gruppi income
summary_income$income_group <- recode(summary_income$income_group,
                                      "Low income" = "Low",
                                      "Lower middle income" = "Lower mid",
                                      "Upper middle income" = "Upper mid",
                                      "High income" = "High"
)

# Barplot aggiornato
ggplot(summary_income, aes(x = indicator_short, y = media, fill = income_group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = pmax(0, media - sd), ymax = media + sd),
    position = position_dodge(width = 0.8), width = 0.2
  ) +
  labs(
    title = "Media e deviazione standard per indicatore e income group",
    x = "Indicatore", y = "Valore medio",
    fill = "Income Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.title.align = 0.5,
    legend.justification = "center",
    legend.margin = margin(t = 10),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )


# ===== Evoluzione temporale dei valori medi per income group =====
# Questo grafico mostra come variano nel tempo i valori medi dei tre indicatori principali 
# (litri consumati, % consumatori e % astinenti), suddivisi per fascia di reddito (income group).
# Ogni indicatore è mostrato in un pannello separato tramite facet_wrap.
df_alcohol_clean %>%
  filter(income_group != "Unknown") %>%
  mutate(income_group = recode(income_group,
                               "Low income" = "Low",
                               "Lower middle income" = "Lower mid",
                               "Upper middle income" = "Upper mid",
                               "High income" = "High"
  )) %>%
  group_by(indicator_short, income_group, year) %>%
  summarise(media = mean(value), .groups = "drop") %>%
  ggplot(aes(x = year, y = media, color = income_group)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ indicator_short, scales = "free_y") +
  labs(
    title = "Evoluzione temporale per income group",
    x = "Anno", y = "Media", color = "Income group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.title.align = 0.5,
    legend.justification = "center",
    legend.margin = margin(t = 10),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )


# ===== Funzione: Heatmap variazione annuale % consumatori per regione WHO =====
# Questa funzione crea una heatmap che mostra, per ogni regione WHO, la variazione
# (delta) percentuale annuale dei consumatori di alcol (indicator_short == "Perc_Cons").
# Le variazioni sono calcolate rispetto all'anno precedente.
library(stringr)

plot_heatmap_delta_global <- function() {
  
  # 1. Calcolo della variazione annuale per regione
  df_delta <- df_alcohol_clean %>%
    filter(indicator_short == "Perc_Cons") %>%
    group_by(year, who_region) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(who_region, year) %>%
    group_by(who_region) %>%
    mutate(delta = mean_value - lag(mean_value)) %>%  # differenza rispetto all’anno precedente
    ungroup()
  
  # 2. Rinomina le regioni con le sigle ufficiali WHO
  df_delta <- df_delta %>%
    mutate(who_region = case_when(
      str_detect(who_region, "Africa") ~ "AFRO",
      str_detect(who_region, "Amer") ~ "AMRO",
      str_detect(who_region, "South.*Asia") ~ "SEARO",
      str_detect(who_region, "Europe") ~ "EURO",
      str_detect(who_region, "Eastern Med") ~ "EMRO",
      str_detect(who_region, "Pacific") ~ "WPRO",
      TRUE ~ who_region
    ))
  
  # 3. Riorganizzazione in formato lungo per heatmap
  df_delta_long <- df_delta %>%
    select(year, who_region, delta) %>%
    pivot_wider(names_from = who_region, values_from = delta) %>%
    pivot_longer(cols = -year, names_to = "region", values_to = "delta")
  
  # 4. Creazione della heatmap
  p <- ggplot(df_delta_long, aes(x = region, y = factor(year), fill = delta)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      name = "Δ% rispetto all'anno precedente",
      low = "blue", mid = "white", high = "red",
      midpoint = 0, na.value = "grey90"
    ) +
    labs(
      title = "Variazione % consumatori rispetto all'anno precedente",
      x = "Regione WHO", y = "Anno"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.title.align = 0.5,
      legend.justification = "center",
      legend.margin = margin(t = 10),
      axis.title.x = element_text(hjust = 0.5),
      axis.title.y = element_text(hjust = 0.5)
    ) +
    coord_fixed(ratio = 0.3)
  
  print(p)
}

# Esecuzione della funzione
plot_heatmap_delta_global()


# ===== Funzione: Heatmap variazione annuale % astinenti per regione WHO =====
# Questa funzione crea una heatmap che mostra la variazione della percentuale di astinenti a vita
# (indicator_short == "Perc_Ast") per ciascuna regione WHO, confrontando ogni anno col precedente.
# I colori indicano se la % di astinenti è aumentata (rosso) o diminuita (blu).
plot_heatmap_delta_astemi_global <- function() {
  library(stringr)
  
  # 1. Calcolo variazione annuale per regione
  df_delta <- df_alcohol_clean %>%
    filter(indicator_short == "Perc_Ast") %>%
    group_by(year, who_region) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(who_region, year) %>%
    group_by(who_region) %>%
    mutate(delta = mean_value - lag(mean_value)) %>%
    ungroup()
  
  # 2. Conversione nomi regioni in sigle WHO
  df_delta <- df_delta %>%
    mutate(who_region = case_when(
      str_detect(who_region, "Africa") ~ "AFRO",
      str_detect(who_region, "Amer") ~ "AMRO",
      str_detect(who_region, "South.*Asia") ~ "SEARO",
      str_detect(who_region, "Europe") ~ "EURO",
      str_detect(who_region, "Eastern Med") ~ "EMRO",
      str_detect(who_region, "Pacific") ~ "WPRO",
      TRUE ~ who_region
    ))
  
  # 3. Riorganizzazione per heatmap
  df_delta_long <- df_delta %>%
    select(year, region = who_region, delta)
  
  # 4. Heatmap finale
  p <- ggplot(df_delta_long, aes(x = region, y = factor(year), fill = delta)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      name = "Δ% astinenti rispetto anno prec.",
      low = "blue", mid = "white", high = "red",
      midpoint = 0, na.value = "grey90"
    ) +
    labs(
      title = "Variazione % astinenti rispetto all'anno precedente",
      x = "Regione WHO", y = "Anno"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),  # 🔹 centra il titolo
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "center",              # 🔹 centra la legenda
      legend.title.align = 0.5,                # 🔹 centra il titolo della legenda
      legend.justification = "center",
      legend.margin = margin(t = 10),
      axis.title.x = element_text(hjust = 0.5), # 🔹 centra label x (non sempre serve)
      axis.title.y = element_text(hjust = 0.5)  # 🔹 centra label y (non sempre serve)
    )+ 
    coord_fixed(ratio = 0.3)
  print(p)  # <- NECESSARIO per vedere il plot
}


# Esecuzione
plot_heatmap_delta_astemi_global()


# ===== Scatterplot: consumo vs astinenza per tutti i paesi =====
# Questo plot mostra la relazione tra la % di astinenti e il consumo di alcol (tra i bevitori)
# per ciascun paese. Le regioni WHO sono indicate per colore.

df_bubble <- df_paradox  # già pronto e normalizzato

ggplot(df_bubble, aes(x = Perc_Ast, y = Litres_Cons, color = who_region)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Astinenti (%) vs Litri pro capite (tra i bevitori)",
    x = "% Astinenti a vita", y = "Litri pro capite",
    color = "Regione WHO"
  ) +
  theme_minimal()

# ===== Analisi del paradosso tra consumo e astinenza =====
# Questo script confronta il consumo medio di alcol tra i bevitori (Litres_Cons)
# e la percentuale di astinenti a vita (Perc_Ast) per ciascun paese.
# Si calcola un "indice di paradosso": maggiore è il valore, più alto è il consumo
# anche a fronte di un'elevata astinenza, suggerendo una polarizzazione nel consumo.

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# 1. Calcolo media per indicatore e paese
df_paradox <- df_alcohol_clean %>%
  filter(indicator_short %in% c("Perc_Ast", "Litres_Cons")) %>%
  group_by(country, who_region, indicator_short) %>%
  summarise(media = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicator_short, values_from = media) %>%
  drop_na()

# 2. Conversione nomi delle regioni in sigle ufficiali WHO
df_paradox <- df_paradox %>%
  mutate(who_region = case_when(
    str_detect(who_region, "Africa") ~ "AFRO",
    str_detect(who_region, "Amer") ~ "AMRO",
    str_detect(who_region, "South.*Asia") ~ "SEARO",
    str_detect(who_region, "Europe") ~ "EURO",
    str_detect(who_region, "Eastern Med") ~ "EMRO",
    str_detect(who_region, "Pacific") ~ "WPRO",
    TRUE ~ who_region
  ))

# 3. Calcolo dell'indice di paradosso: consumo / (100 - % astinenti)
df_paradox <- df_paradox %>%
  mutate(paradox_index = Litres_Cons / (100 - Perc_Ast)) %>%
  filter(!is.infinite(paradox_index)) %>%
  arrange(desc(paradox_index))

# 4. Visualizzazione: Top 10 paesi con paradosso più elevato
df_paradox %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(country, paradox_index), y = paradox_index, fill = who_region)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 paesi con il maggior paradosso consumo/astinenza",
    x = "Paese", y = "Indice di paradosso",
    fill = "Regione WHO"
  ) +
  theme_minimal()




#====== Confronto litri consumati (media anni) per regione e genere ==========
df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons", gender %in% c("Male", "Female")) %>%
  group_by(who_region, gender) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = who_region, y = mean_value, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title = "Confronto litri consumati (media anni) per regione e genere",
    subtitle = "Media su tutti gli anni disponibili, suddiviso per genere",
    x = "Regione WHO", y = "% Consumatori", fill = "Genere"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# ===== Analisi del gender gap nel consumo di alcol (litri per bevitori) =====
# Questo blocco calcola la differenza media tra uomini e donne (Male - Female)
# nel consumo di alcol pro capite tra i bevitori (indicator_short == "Litres_Cons"),
# per ciascun anno e regione WHO.
# Il risultato è una curva temporale che mostra come varia il gap di genere nel tempo.

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)  # Per str_detect

# 1. Calcolo del gender gap
df_litres_gap <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons", gender %in% c("Male", "Female")) %>%
  select(country, year, who_region, gender, value) %>%
  pivot_wider(names_from = gender, values_from = value) %>%
  mutate(
    litres_gap = Male - Female,
    
    # Conversione dei nomi regione in sigle WHO
    who_region = case_when(
      str_detect(who_region, regex("African", ignore_case = TRUE))             ~ "AFRO",
      str_detect(who_region, regex("Americas", ignore_case = TRUE))           ~ "AMRO",
      str_detect(who_region, regex("Eastern Mediterranean", ignore_case = TRUE)) ~ "EMRO",
      str_detect(who_region, regex("European", ignore_case = TRUE))           ~ "EURO",
      str_detect(who_region, regex("South[- ]?East Asia", ignore_case = TRUE)) ~ "SEARO",
      str_detect(who_region, regex("Western Pacific", ignore_case = TRUE))    ~ "WPRO",
      TRUE ~ who_region  # fallback
    )
  ) %>%
  drop_na(litres_gap) %>%
  group_by(year, who_region) %>%
  summarise(mean_gap = mean(litres_gap, na.rm = TRUE), .groups = "drop")

# 2. Visualizzazione del gender gap nel tempo per regione
ggplot(df_litres_gap, aes(x = year, y = mean_gap, color = who_region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Gender gap nel consumo di alcol tra i bevitori (litri pro capite)",
    subtitle = "Gap = Male - Female (Litres_Cons)",
    x = "Anno", y = "Differenza media in litri",
    color = "Regione WHO"
  ) +
  theme_minimal()



