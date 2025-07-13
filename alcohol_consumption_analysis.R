# ========================================================================================================================================
# Progetto: Analisi esplorativa e statistica del consumo globale di alcol: evidenze da dati WHO disaggregati per fattori socio-demografici
# Autori: Daniele Angeloni, Alessandra Ruggeri
# Università degli Studi di Perugia - Corso "Data Science for Health Systems" - Professore: Alessio De Angelis
# Email: daniele.angeloni@studenti.unipg.it, alessandra.ruggeri@studenti.unipg.it
# Link repository Github: https://github.com/arlerug/DSHS
# ========================================================================================================================================

# Installazione packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("writexl")
install.packages("tidyr")
install.packages("tibble")
install.packages("PMCMRplus")
install.packages("ggpubr")
install.packages("FSA")
install.packages("rcompanion")
install.packages("gridExtra")
install.packages("patchwork")

# Importazione e scrittura dati
library(readxl)
library(writexl)

# Manipolazione e analisi
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)

# Visualizzazione
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
library(patchwork)

# Test
library(PMCMRplus)
library(FSA)
library(rcompanion)

# Imposta manualmente la working directory
setwd("insert/your/path/of/project_DSHS")

# Controllo dei fogli nel file
sheets <- excel_sheets("DATASET/data.xlsx")
print(sheets)

# Importazione dataset 
df <- read_excel("DATASET/data.xlsx", sheet = sheets[1])

# ====== Esplorazione dataset ===============

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

# Controllo righe attuali
nrow(df_alcohol)
head(df_alcohol)

# Selezione colonne necessarie e assegnazione etichette sintetiche e immediate 
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

# Rinomina dei tre indicatori in etichette sintetiche per uso interno
df_alcohol_clean <- df_alcohol_clean %>%
  mutate(indicator_short = case_when(
    indicator == "Alcohol, consumers past 12 months (%)" ~ "Perc_Cons",
    indicator == "Alcohol, drinkers only per capita (15+) consumption (in litres of pure alcohol)" ~ "Litres_Cons",
    indicator == "Alcohol, abstainers lifetime (%)" ~ "Perc_Ast",
    TRUE ~ NA_character_
  ))

table(df_alcohol_clean$indicator_short, useNA = "ifany")

# Controllo presenza valori percentuali fuori dal range 0-100
df_check_range <- df_alcohol_clean %>%
  filter(indicator_short %in% c("Perc_Cons", "Perc_Ast")) %>%   # Solo indicatori in percentuale
  filter(value < 0 | value > 100)

# Visualizzazione e conteggio righe problematiche, se esistono
print(df_check_range)
nrow(df_check_range)

if (nrow(df_check_range) == 0) {
  cat("Tutti i valori percentuali (P_Cons e P_Ast) sono nel range corretto [0, 100].\n")
} else {
  cat("Attenzione: trovati", nrow(df_check_range), "valori percentuali fuori range!\n")
}

plot_indicatori <- ggplot(df_alcohol_clean, aes(x = indicator_short, y = value, fill = indicator_short)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribuzioni a confronto", x = "Indicatore", y = "Valore") +
  theme_minimal()

dir.create("img_eda", showWarnings = FALSE, recursive = TRUE)

ggsave("img_eda/boxplot_indicatori.png",
       plot = plot_indicatori, width = 8, height = 5, bg = "white")

# Controllo valori NA nei 3 indicatori
df_alcohol_clean %>%
  group_by(indicator_short) %>%
  summarise(
    totale = n(),
    n_missing = sum(is.na(value)),
    percentuale_missing = round(100 * sum(is.na(value)) / n(), 2)
  )

# Rimozione valori NA dato il numero non consistente
df_alcohol_clean <- df_alcohol_clean %>%
  filter(!(is.na(value) & indicator_short %in% c("Litres_Cons", "Perc_Ast", "Perc_Cons")))

# Nuovo controllo valori NA nei 3 indicatori
df_alcohol_clean %>%
  group_by(indicator_short) %>%
  summarise(
    totale = n(),
    n_missing = sum(is.na(value)),
    percentuale_missing = round(100 * sum(is.na(value)) / n(), 2)
  )

# Salvataggio dataset preprocessato e pulito come file .xlsx
write_xlsx(df_alcohol_clean, "dataset_preprocessed.xlsx")

# Controllo valori anomali Litres_Cons
df_litres <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons")

# Mostra statistiche descrittive
summary(df_litres$value)

# Mostra i valori più alti (>50 litri)
df_litres %>%
  filter(value > 50) %>%
  arrange(desc(value)) %>%
  select(country, year, gender, value)

# Conteggio valori nulli nelle colonne (gruppi)
colSums(is.na(df_alcohol_clean))

# Presenti valori nulli solo su income_group e li metto in categoria "Unknown"
# così da non eliminarli perchè sono comunque utili per test dove non conta il gruppo di reddito
#Lista dei paesi con categoria Unknown associata: Cook Islands, Niue e Venezuela

df_alcohol_clean <- df_alcohol_clean %>%
  mutate(income_group = ifelse(is.na(income_group), "Unknown", income_group))

# Controllo valori unici per ogni colonna (importante per i dati categorici e ordinali, no per i dati quantitativi (value))

df_alcohol_clean %>%
  summarise(across(everything(), n_distinct))

# Visualizzazione valori unici presenti nelle colonne, per procedere alle visualizzazioni grafiche in maniera corretta

# Country (solo primi 10 per leggibilità)
cat("Paesi (n =", n_distinct(df_alcohol_clean$country), "):\n")
print(sort(unique(df_alcohol_clean$country))[1:10])
cat("... (totale 189 paesi)\n\n")

# Year
cat("Anni disponibili (n =", n_distinct(df_alcohol_clean$year), "):\n")
print(sort(unique(df_alcohol_clean$year)))
cat("\n")

# Gender
cat("Gender:\n")
print(unique(df_alcohol_clean$gender))
cat("\n")

# WHO Region
cat("WHO Regions:\n")
print(unique(df_alcohol_clean$who_region))
cat("\n")

# Income Group
cat("Income Groups:\n")
print(unique(df_alcohol_clean$income_group))
cat("\n")

# Indicator Short
cat("Indicatori analizzati:\n")
print(unique(df_alcohol_clean$indicator_short))
cat("\n")

#=================INIZIO EDA=====================

# Riepilogo media e deviazione standard per indicatore e genere
summary_gender <- df_alcohol_clean %>%
  group_by(indicator_short, gender) %>%
  summarise(
    media = round(mean(value, na.rm = TRUE), 2),
    sd = round(sd(value, na.rm = TRUE), 2),
    .groups = "drop"
  )

table_plot <- tableGrob(summary_gender, rows = NULL)


png("img_eda/summary_gender_table.png", width = 800, height = 400)
grid.newpage()
grid.draw(table_plot)
dev.off()
grid.newpage()
grid.draw(table_plot)

# Matrice di correlazione Spearman tra i 3 indicatori principali

# Calcola media per paese e indicatore
df_corr <- df_alcohol_clean %>%
  group_by(country, indicator_short) %>%
  summarise(media = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicator_short, values_from = media) %>%
  drop_na()

# Calcola matrice di correlazione Spearman
corr_matrix <- df_corr %>%
  select(Perc_Cons, Litres_Cons, Perc_Ast) %>%
  cor(method = "spearman", use = "complete.obs")

# Prepara tabella
corr_df <- as.data.frame(round(corr_matrix, 2))
corr_table <- tableGrob(corr_df, rows = NULL)

# Salva immagine
png("img_eda/correlazione_spearman.png", width = 800, height = 500)
grid.newpage()
grid.draw(corr_table)
dev.off()

# Visualizzazione
grid.newpage()
grid.draw(corr_table)

# ====Barplot con media e deviazione standard per indicatore e genere====

p <- ggplot(summary_gender, aes(x = indicator_short, y = media, fill = gender)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = media - sd, ymax = media + sd),
                position = position_dodge(0.8), width = 0.2) +
  labs(
    title = "Barplot media e SD per indicatore e genere",
    x = "Indicatore", y = "Media",
    fill = "Genere"
  ) +
  theme_minimal()

# Salvataggio e visualizzazione
ggsave("img_eda/barplot_media_sd_genere.png",
       plot = p, width = 8, height = 5, bg = "white")

print(p)

#====Trend temporali: % consumatori/astinenti e litri pro capite (solo M/F)====

# Filtra solo M/F
df_filtered <- df_alcohol_clean %>% filter(gender %in% c("Male", "Female"))

# Trend percentuali
p_percentuali <- df_filtered %>%
  filter(indicator_short %in% c("Perc_Cons", "Perc_Ast")) %>%
  group_by(year, indicator_short) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_value, color = indicator_short)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Percentuali: astemi e consumatori", x = "Anno", y = "% Popolazione", color = "Indicatore") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Trend litri
p_litri <- df_filtered %>%
  filter(str_detect(indicator_short, "Lit")) %>%
  group_by(year, indicator_short) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_value, color = indicator_short)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Litri di alcol pro capite", x = "Anno", y = "Litri", color = "Indicatore") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Combinazione, salvataggio e visualizzazione
plot_combined <- (p_percentuali | p_litri) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

ggsave("img_eda/trend_temporali_alcol_mf.png", 
       plot = plot_combined, width = 12, height = 5, bg = "white")

print(plot_combined)

# ====Trend temporali per genere e indicatore====

# Calcola media per anno, genere e indicatore
summary_by_year_gender <- df_alcohol_clean %>%
  filter(gender %in% c("Male", "Female")) %>%
  group_by(year, gender, indicator_short) %>%
  summarise(media = mean(value, na.rm = TRUE), .groups = "drop")

plot_trend <- summary_by_year_gender %>%
  mutate(indicatore = recode(indicator_short,
                             "Litres_Cons" = "Litri per bevitore",
                             "Perc_Ast" = "Astinenti a vita",
                             "Perc_Cons" = "Consumatori 12 mesi")) %>%
  ggplot(aes(x = year, y = media, color = gender)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ indicatore, scales = "free_y") +
  labs(
    title = "Trend temporali per genere dei tre indicatori di consumo alcolico",
    x = "Anno", y = "Valore medio", color = "Genere"
  ) +
  theme_minimal()

ggsave("img_eda/trend_per_genere.png", plot = plot_trend, width = 10, height = 5, bg = "white")
print(plot_trend)

# ====Violin plot – Litri per bevitore====
plot_litres <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons") %>%
  ggplot(aes(x = who_region, y = value, fill = who_region)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "Distribuzione litri pro capite (bevitori) per regione WHO",
    x = "Regione WHO", y = "Litri"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")

ggsave("img_eda/violin_litres.png", plot = plot_litres, width = 8, height = 5, bg = "white")
print(plot_litres)

# ====Violin plot – % consumatori====
plot_cons <- df_alcohol_clean %>%
  filter(indicator_short == "Perc_Cons") %>%
  ggplot(aes(x = who_region, y = value, fill = who_region)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "Distribuzione % consumatori ultimi 12 mesi per regione WHO",
    x = "Regione WHO", y = "% Consumatori"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")

ggsave("img_eda/violin_consumatori.png", plot = plot_cons, width = 8, height = 5, bg = "white")
print(plot_cons)

# ====Violin plot – % astinenti====
plot_ast <- df_alcohol_clean %>%
  filter(indicator_short == "Perc_Ast") %>%
  ggplot(aes(x = who_region, y = value, fill = who_region)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "Distribuzione % astinenti a vita per regione WHO",
    x = "Regione WHO", y = "% Astinenti"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")

ggsave("img_eda/violin_astinenti.png", plot = plot_ast, width = 8, height = 5, bg = "white")
print(plot_ast)

#====Media e deviazione standard per indicatore e income group==== 

# Calcolo media e deviazione standard per indicatore e income group
summary_income <- df_alcohol_clean %>%
  filter(income_group != "Unknown") %>%
  mutate(income_group = factor(income_group, levels = c(
    "Low income", "Lower middle income", "Upper middle income", "High income"
  ))) %>%
  group_by(indicator_short, income_group) %>%
  summarise(
    media = mean(value),
    sd = sd(value),
    .groups = "drop"
  )

# Abbreviazione nomi income_group per visualizzazione
summary_income$income_group <- recode(summary_income$income_group,
                                      "Low income" = "Low",
                                      "Lower middle income" = "Lower mid",
                                      "Upper middle income" = "Upper mid",
                                      "High income" = "High"
)

# Plot
plot_income <- ggplot(summary_income, aes(x = indicator_short, y = media, fill = income_group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = pmax(0, media - sd), ymax = media + sd),
    position = position_dodge(width = 0.8), width = 0.2
  ) +
  labs(
    title = "Barplot media e SD per indicatore e livello di reddito",
    x = "Indicatore", y = "Valore medio",
    fill = "Income Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box.just = "center",
    legend.title.align = 0.5,
    legend.margin = margin(t = 10),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )

# Salvataggio e visualizzazione
ggsave("img_eda/barplot_media_sd_income.png",
       plot = plot_income, width = 10, height = 6, bg = "white")

print(plot_income)

# ====Trend nel tempo per income group (3 indicatori alcol)====

# Preparazione e generazione grafico
plot_income_trend <- df_alcohol_clean %>%
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
    title = "Trend nel tempo per income group (3 indicatori alcol)",
    x = "Anno", y = "Media", color = "Income group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
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

# Salvataggio e visualizzazione
ggsave("img_eda/trend_income_group.png",
       plot = plot_income_trend, width = 10, height = 6, bg = "white")

print(plot_income_trend)

# ====Δ % consumatori per regione WHO (anno precedente)====

plot_heatmap_delta_global <- function() {
  df_delta <- df_alcohol_clean %>%
    filter(indicator_short == "Perc_Cons") %>%
    group_by(year, who_region) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(who_region, year) %>%
    group_by(who_region) %>%
    mutate(delta = mean_value - lag(mean_value)) %>%
    ungroup() %>%
    mutate(who_region = case_when(
      str_detect(who_region, "Africa") ~ "AFRO",
      str_detect(who_region, "Amer") ~ "AMRO",
      str_detect(who_region, "South.*Asia") ~ "SEARO",
      str_detect(who_region, "Europe") ~ "EURO",
      str_detect(who_region, "Eastern Med") ~ "EMRO",
      str_detect(who_region, "Pacific") ~ "WPRO",
      TRUE ~ who_region
    ))
  
  df_delta_long <- df_delta %>%
    select(year, who_region, delta) %>%
    pivot_wider(names_from = who_region, values_from = delta) %>%
    pivot_longer(cols = -year, names_to = "region", values_to = "delta")
  
  p <- ggplot(df_delta_long, aes(x = region, y = factor(year), fill = delta)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      name = "Δ% rispetto all'anno precedente",
      low = "blue", mid = "white", high = "red",
      midpoint = 0, na.value = "grey90"
    ) +
    labs(
      title = "Δ % consumatori per regione WHO (anno precedente)",
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
  
  ggsave("img_eda/heatmap_delta_consumatori.png",
         plot = p, width = 10, height = 6, bg = "white")
  print(p)
}

plot_heatmap_delta_global()

#====Δ % astinenti per regione WHO (anno precedente)====

plot_heatmap_delta_astemi_global <- function() {
  df_delta <- df_alcohol_clean %>%
    filter(indicator_short == "Perc_Ast") %>%
    group_by(year, who_region) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(who_region, year) %>%
    group_by(who_region) %>%
    mutate(delta = mean_value - lag(mean_value)) %>%
    ungroup() %>%
    mutate(who_region = case_when(
      str_detect(who_region, "Africa") ~ "AFRO",
      str_detect(who_region, "Amer") ~ "AMRO",
      str_detect(who_region, "South.*Asia") ~ "SEARO",
      str_detect(who_region, "Europe") ~ "EURO",
      str_detect(who_region, "Eastern Med") ~ "EMRO",
      str_detect(who_region, "Pacific") ~ "WPRO",
      TRUE ~ who_region
    ))
  
  df_delta_long <- df_delta %>%
    select(year, region = who_region, delta)
  
  p <- ggplot(df_delta_long, aes(x = region, y = factor(year), fill = delta)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      name = "Δ% astinenti rispetto anno prec.",
      low = "blue", mid = "white", high = "red",
      midpoint = 0, na.value = "grey90"
    ) +
    labs(
      title = "Δ % astinenti per regione WHO (anno precedente)",
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
  
  ggsave("img_eda/heatmap_delta_astinenti.png",
         plot = p, width = 10, height = 6, bg = "white")
  
  print(p)
}

plot_heatmap_delta_astemi_global()

# Calcolo indice di paradosso per paese
df_paradox <- df_alcohol_clean %>%
  filter(indicator_short %in% c("Perc_Ast", "Litres_Cons")) %>%
  group_by(country, who_region, indicator_short) %>%
  summarise(media = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicator_short, values_from = media) %>%
  drop_na() %>%
  mutate(
    who_region = case_when(
      str_detect(who_region, "Africa") ~ "AFRO",
      str_detect(who_region, "Amer") ~ "AMRO",
      str_detect(who_region, "South.*Asia") ~ "SEARO",
      str_detect(who_region, "Europe") ~ "EURO",
      str_detect(who_region, "Eastern Med") ~ "EMRO",
      str_detect(who_region, "Pacific") ~ "WPRO",
      TRUE ~ who_region
    ),
    paradox_index = Litres_Cons / (100 - Perc_Ast)
  ) %>%
  filter(!is.infinite(paradox_index)) %>%
  arrange(desc(paradox_index))

# ====Relazione tra % astinenti e litri pro capite (per regione WHO)====
plot_paradox <- ggplot(df_paradox, aes(x = Perc_Ast, y = Litres_Cons, color = who_region)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Relazione tra % astinenti e litri pro capite (per regione WHO)",
    x = "% Astinenti a vita", y = "Litri pro capite",
    color = "Regione WHO"
  ) +
  theme_minimal()

ggsave("img_eda/scatter_astinenti_vs_litri.png",
       plot = plot_paradox, width = 8, height = 6, bg = "white")

print(plot_paradox)

# ====Top 10 paesi con elevato indice paradosso: alto consumo + alta astinenza====
plot_paradox_top10 <- df_paradox %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(country, paradox_index), y = paradox_index, fill = who_region)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 paesi con elevato indice paradosso: alto consumo + alta astinenza",
    x = "Paese", y = "Indice di paradosso",
    fill = "Regione WHO"
  ) +
  theme_minimal()

ggsave("img_eda/paradox_index_top10.png",
       plot = plot_paradox_top10, width = 9, height = 6, bg = "white")

print(plot_paradox_top10)

#====Litri medi pro capite per regione WHO e genere====
plot_litres_region_gender <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons", gender %in% c("Male", "Female")) %>%
  group_by(who_region, gender) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = who_region, y = mean_value, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title = "Litri medi pro capite per regione WHO e genere",
    subtitle = "Media su tutti gli anni disponibili",
    x = "Regione WHO", y = "Litri pro capite (bevitori)", fill = "Genere"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("img_eda/barplot_litres_regione_genere.png",
       plot = plot_litres_region_gender, width = 9, height = 6, bg = "white")

print(plot_litres_region_gender)

#====Gender gap (M − F) nei litri pro capite tra i bevitori – per regione WHO====
# Calcola gender gap per regione e anno
df_litres_gap <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons", gender %in% c("Male", "Female")) %>%
  select(country, year, who_region, gender, value) %>%
  pivot_wider(names_from = gender, values_from = value) %>%
  mutate(
    litres_gap = Male - Female,
    who_region = case_when(
      str_detect(who_region, regex("African", ignore_case = TRUE))             ~ "AFRO",
      str_detect(who_region, regex("Americas", ignore_case = TRUE))           ~ "AMRO",
      str_detect(who_region, regex("Eastern Mediterranean", ignore_case = TRUE)) ~ "EMRO",
      str_detect(who_region, regex("European", ignore_case = TRUE))           ~ "EURO",
      str_detect(who_region, regex("South[- ]?East Asia", ignore_case = TRUE)) ~ "SEARO",
      str_detect(who_region, regex("Western Pacific", ignore_case = TRUE))    ~ "WPRO",
      TRUE ~ who_region
    )
  ) %>%
  drop_na(litres_gap) %>%
  group_by(year, who_region) %>%
  summarise(mean_gap = mean(litres_gap, na.rm = TRUE), .groups = "drop")

# Creazione, salvataggio e visualizzazione
plot_gap <- ggplot(df_litres_gap, aes(x = year, y = mean_gap, color = who_region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Gender gap (M − F) nei litri pro capite tra i bevitori – per regione WHO",
    subtitle = "Differenza media annuale: Male - Female",
    x = "Anno", y = "Differenza media (litri)",
    color = "Regione WHO"
  ) +
  theme_minimal()

ggsave("img_eda/gender_gap_litres.png",
       plot = plot_gap, width = 9, height = 6, bg = "white")

print(plot_gap)

# ===== Test Statistici =====

# ===== TEST DI ADATTAMENTO (verifica normalità dati) =====


# Creazione cartelle di output
dir.create("results_testNormalita", showWarnings = FALSE)
dir.create("img_qqplot", showWarnings = FALSE)
dir.create("results_testNonParametrici", showWarnings = FALSE)
dir.create("results_testPostHoc", showWarnings = FALSE)
dir.create("img_compareTest", showWarnings = FALSE)

# Lista dei confronti da testare
normality_tests <- list(
  list(indicatore = "Litres_Cons", gruppo = "gender"),
  list(indicatore = "Litres_Cons", gruppo = "income_group"),
  list(indicatore = "Perc_Ast",     gruppo = "who_region"),
  list(indicatore = "Perc_Cons",    gruppo = "income_group"),
  list(indicatore = "Perc_Cons",    gruppo = "gender")
)

# Loop per ogni test di normalità
for (test in normality_tests) {
  
  indicatore <- test$indicatore
  gruppo <- test$gruppo
  nome_file <- paste0("Shapiro_", indicatore, "_by_", gruppo)
  
  cat("\n🔬 Test di normalità:", indicatore, "~", gruppo, "\n")
  
  df_filtered <- df_alcohol_clean %>%
    filter(indicator_short == indicatore) %>%
    filter(!is.na(.data[[gruppo]])) %>%
    {
      if (gruppo == "income_group") {
        filter(., .data[[gruppo]] != "Unknown")
      } else {
        .
      }
    }
  
  livelli <- unique(df_filtered[[gruppo]])
  risultati <- list()
  
  for (liv in livelli) {
    df_sub <- df_filtered %>% filter(.data[[gruppo]] == liv)
    
    if (nrow(df_sub) >= 3) {
      shapiro <- shapiro.test(df_sub$value)
      
      risultati[[length(risultati) + 1]] <- data.frame(
        gruppo = gruppo,
        categoria = liv,
        n = nrow(df_sub),
        W = round(shapiro$statistic, 3),
        p_value = signif(shapiro$p.value, 3)
      )
    } else {
      risultati[[length(risultati) + 1]] <- data.frame(
        gruppo = gruppo,
        categoria = liv,
        n = nrow(df_sub),
        W = NA,
        p_value = NA
      )
    }
  }
  
  shapiro_df <- do.call(rbind, risultati)
  print(shapiro_df)
  
  # Salvataggio tabella dei risultati dei test Shapiro-Wilk in .csv
  write.csv(shapiro_df, paste0("results_testNormalita/", nome_file, ".csv"), row.names = FALSE)
  
  # Generazione QQ plot + salvataggio grafici in .png
  gg <- ggqqplot(
    df_filtered,
    x = "value",
    facet.by = gruppo,
    title = paste("QQ-plot:", indicatore, "~", gruppo),
    xlab = "Quantili teorici", ylab = "Quantili osservati"
  ) +
    xlim(-3, 3) + 
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      strip.text = element_text(size = 15),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  
  ggsave(paste0("img_qqplot/QQplot_", indicatore, "_by_", gruppo, ".png"), plot = gg, width = 8, height = 5, bg = "white")
}

# ===== FUNZIONI PER TEST STATISTICI NON PARAMETRICI + SALVATAGGI + CREAZIONE GRAFICI =====

# ===== Funzione per Wilcoxon Test ======
wilcox_and_plot <- function(data, formula, gruppo, ylab, title, filename_prefix) {
  result <- wilcox.test(formula, data = data)
  print(result)
  
  # Salvataggio risultato in .csv
  write.csv(data.frame(result = capture.output(result)), 
            file = paste0("results_testNonParametrici/Wilcoxon_", filename_prefix, ".csv"),
            row.names = FALSE)
  
  # Generazione, plot e salvataggio grafici in .png
  p <- ggplot(data, aes_string(x = gruppo, y = "value", fill = gruppo)) +
    geom_boxplot(alpha = 0.7) +
    stat_compare_means(method = "wilcox.test", label = "p.signif") +
    labs(title = title, y = ylab, x = gruppo) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction = "horizontal")
  
  
  print(p)
  ggsave(paste0("img_compareTest/plot_", filename_prefix, ".png"), plot = p, width = 8, height = 5, bg = "white")
  
}

# ==== Funzione per Test di Kruskal-Wallis =====
kruskal_and_plot <- function(data, formula, gruppo, ylab, title, filename_prefix) {
  
  # Gestione dei gruppi di income_group
  if (gruppo == "income_group") {
    data <- data %>%
      filter(.data[[gruppo]] != "Unknown") %>%
      mutate(income_group = factor(income_group, levels = c(
        "Low income", "Lower middle income", "Upper middle income", "High income"
      )))
  }
  
  # Esecuzione Kruskal-Wallis test
  result <- kruskal.test(formula, data = data)
  print(result)
  
  # Salvataggio risultato del Kruskal-Wallis
  write.csv(data.frame(result = capture.output(result)),
            file = paste0("results_testNonParametrici/Kruskal_", filename_prefix, ".csv"),
            row.names = FALSE)
  
  # Se significativo, esecuzione Post-hoc Dunn test con correzione di Bonferroni
  if (result$p.value < 0.05) {
    dunn <- dunnTest(formula, data = data, method = "bonferroni")
    print(dunn)
    write.csv(dunn$res,
              file = paste0("results_testPostHoc/Dunn_", filename_prefix, ".csv"),
              row.names = FALSE)
  }
  
  
  # Generazione plot e salvataggio grafici in .png
  if (gruppo == "who_region") {
    data <- data %>%
      mutate(who_region = recode(who_region,
                                 "African" = "AFRO",
                                 "Americas" = "AMRO",
                                 "Eastern Mediterranean" = "EMRO",
                                 "European" = "EURO",
                                 "South-East Asia" = "SEARO",
                                 "Western Pacific" = "WPRO"
      ))
  }
  
  p <- ggplot(data, aes(x = forcats::fct_reorder(.data[[gruppo]], value, .fun = mean), 
                        y = value, fill = .data[[gruppo]])) +
    geom_boxplot(alpha = 0.7) +
    stat_compare_means(method = "kruskal.test", label = "p.signif") +
    labs(title = title, y = ylab, x = "") +
    scale_x_discrete(labels = c(
      "Low income" = "Low",
      "Lower middle income" = "Lower middle",
      "Upper middle income" = "Upper middle",
      "High income" = "High"
    )) +
    scale_fill_discrete(labels = c(
      "Low income" = "Low",
      "Lower middle income" = "Lower middle",
      "Upper middle income" = "Upper middle",
      "High income" = "High"
    )) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  
  print(p)
  ggsave(paste0("img_compareTest/plot_", filename_prefix, ".png"), plot = p, width = 8, height = 5, bg = "white")
  
}

# ==== TEST NON PARAMETRICI =====

## --- Test 1: Litres_Cons ~ gender (Wilcoxon)
df1 <- df_alcohol_clean %>% filter(indicator_short == "Litres_Cons")
wilcox_and_plot(df1, value ~ gender, "gender", "Litri", "Litres_Cons per Genere", "Litres_Cons_by_gender")


## --- Test 2: Litres_Cons ~ income_group (Kruskal-Wallis)
df2 <- df_alcohol_clean %>% filter(indicator_short == "Litres_Cons", income_group != "Unknown") %>%
  mutate(income_group = factor(income_group, levels = c(
    "Low income", "Lower middle income", "Upper middle income", "High income"
  )))
kruskal_and_plot(df2, value ~ income_group, "income_group", "Litri", "Litres_Cons per Income Group", "Litres_Cons_by_income_group")


## --- Test 3: Perc_Ast ~ who_region (Kruskal-Wallis)
df3 <- df_alcohol_clean %>% filter(indicator_short == "Perc_Ast")
kruskal_and_plot(df3, value ~ who_region, "who_region", "% Astinenti", "Perc_Ast per Regione OMS", "Perc_Ast_by_who_region")


## --- Test 4: Perc_Cons ~ income_group (Kruskal-Wallis)
df4 <- df_alcohol_clean %>% filter(indicator_short == "Perc_Cons", income_group != "Unknown") %>%
  mutate(income_group = factor(income_group, levels = c(
    "Low income", "Lower middle income", "Upper middle income", "High income"
  )))
kruskal_and_plot(df4, value ~ income_group, "income_group", "% Consumatori", "Perc_Cons per Income Group", "Perc_Cons_by_income_group")


## --- Test 5: Perc_Cons ~ gender (Wilcoxon)
df5 <- df_alcohol_clean %>% filter(indicator_short == "Perc_Cons")
wilcox_and_plot(df5, value ~ gender, "gender", "% Consumatori", "Perc_Cons per Genere", "Perc_Cons_by_gender")


## --- Test 6: Litres_Cons ~ year in Italy, France, Germany (Friedman)
# Preprocessing pre-test di Friedman
df_filtered <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons", country %in% c("Italy", "Germany", "France"))

df_filtered %>% count(country, year) %>% print(n = Inf)

df_wide <- df_filtered %>%
  select(country, year, value) %>%
  group_by(country, year) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = value)

mat <- df_wide %>%
  column_to_rownames("country") %>%
  as.matrix()

# Esecuzione Test di Friedman
cat("\n Test di Friedman: Litres_Cons tra Italy, France, Germany (2000–2020)\n")
friedman_result <- friedman.test(mat)
print(friedman_result)

# Salva risultati test Friedman in .csv
write.csv(
  data.frame(Result = capture.output(friedman_result)),
  "results_testNonParametrici/friedman_litres_italy_france_germany.csv",
  row.names = FALSE
)

# Post-hoc se significativo
if (friedman_result$p.value < 0.05) {
  posthoc <- frdAllPairsExactTest(mat, p.adjust.method = "bonferroni")
  
  posthoc_matrix <- posthoc$p.value
  posthoc_df <- as.data.frame(as.table(posthoc_matrix))
  colnames(posthoc_df) <- c("Group1", "Group2", "p_value")
  print(posthoc_df)
  
  # Salvataggio risultati post-hoc in .csv
  write.csv(
    posthoc_df,
    "results_testPostHoc/friedman_posthoc_litres_italy_france_germany.csv",
    row.names = FALSE
  )
}

# Generazione e salvataggio dei plot temporali in .png
p <- df_filtered %>%
  group_by(country, year) %>%
  summarise(media = mean(value), .groups = "drop") %>%
  ggplot(aes(x = year, y = media, color = country, group = country)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Trend Litres_Cons in Italia, Francia, Germania (2000–2020)",
    x = "Anno", y = "Litri (bevitori)", color = "Paese"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "horizontal")


print(p)
ggsave("img_compareTest/plot_friedman_litres_italy_france_germany.png", plot = p, width = 8, height = 5, bg = "white")








