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


# 3. Controlla i fogli presenti nel file Excel
sheets <- excel_sheets("data.xlsx")
print(sheets)

# 4. Importa il primo foglio (oppure sostituisci con il nome esatto se serve)
df <- read_excel("data.xlsx", sheet = 1)

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

####FINE FASE PULIZIA E PREPARAZIONE DATASET

#################################################################

#### INIZIO FASE EDA CON VISUALIZZAZIONI VARIE

#statistiche descrittive classiche (mean,median,sd,min,max) per ogni indicatori in base alle features

# Calcola le statistiche (se non l'hai già salvato in un oggetto)
summary_gender <- df_alcohol_clean %>%
  group_by(indicator_short, gender) %>%
  summarise(
    media = mean(value),
    sd = sd(value),
    .groups = "drop"
  )

print(summary_gender)

#grafico associato: Crea il barplot con barre d'errore
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

######################

# Calcolo della media annuale per indicatore e genere
summary_by_year_gender <- df_alcohol_clean %>%
  group_by(indicator_short, gender, year) %>%
  summarise(media = mean(value), .groups = "drop")

# ---- 1. Litres_Cons ----
df_litres <- summary_by_year_gender %>%
  filter(indicator_short == "Litres_Cons")

ggplot(df_litres, aes(x = year, y = media, color = gender)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Consumo medio di alcol (litri per bevitori)",
    x = "Anno", y = "Litri", color = "Genere"
  ) +
  theme_minimal()

# ---- 2. Perc_Ast ----
df_ast <- summary_by_year_gender %>%
  filter(indicator_short == "Perc_Ast")

ggplot(df_ast, aes(x = year, y = media, color = gender)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Percentuale di astinenti a vita",
    x = "Anno", y = "% Astinenti", color = "Genere"
  ) +
  theme_minimal()

# ---- 3. Perc_Cons ----
df_cons <- summary_by_year_gender %>%
  filter(indicator_short == "Perc_Cons")

ggplot(df_cons, aes(x = year, y = media, color = gender)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Percentuale di consumatori (ultimi 12 mesi)",
    x = "Anno", y = "% Consumatori", color = "Genere"
  ) +
  theme_minimal()

#############################

# indicatori e who_Region distribuzione su 3 grafici divisi


df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons") %>%
  ggplot(aes(x = who_region, y = value, fill = who_region)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "Distribuzione Litres_Cons per regione WHO",
    x = "Regione WHO", y = "Litri per capita (bevitori)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")

df_alcohol_clean %>%
  filter(indicator_short == "Perc_Cons") %>%
  ggplot(aes(x = who_region, y = value, fill = who_region)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "Distribuzione % consumatori (ultimi 12 mesi)",
    x = "Regione WHO", y = "% Consumatori"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")

df_alcohol_clean %>%
  filter(indicator_short == "Perc_Ast") %>%
  ggplot(aes(x = who_region, y = value, fill = who_region)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "Distribuzione % astinenti a vita",
    x = "Regione WHO", y = "% Astinenti"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")

ggsave("violin_litres.png", width = 8, height = 5)
ggsave("violin_cons.png", width = 8, height = 5)
ggsave("violin_abst.png", width = 8, height = 5)


#####################################################

# indicatori e income_group

# Calcolo statistiche senza "Unknown", con ordine logico
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

# Barplot con SD corretta (non sotto zero)
ggplot(summary_income, aes(x = indicator_short, y = media, fill = income_group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = pmax(0, media - sd), ymax = media + sd),
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(
    title = "Media e deviazione standard per indicatore e income group",
    x = "Indicatore", y = "Valore medio",
    fill = "Income Group"
  ) +
  theme_minimal()



#############   PROVE 



df_alcohol_clean %>%
  filter(income_group != "Unknown") %>%
  group_by(indicator_short, income_group, year) %>%
  summarise(media = mean(value), .groups = "drop") %>%
  ggplot(aes(x = year, y = media, color = income_group)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ indicator_short, scales = "free_y") +
  labs(
    title = "Evoluzione temporale per income group",
    x = "Anno", y = "Media", color = "Income group"
  ) +
  theme_minimal()

#############################

# Top 10 paesi con astinenza media più alta (Perc_Ast), colorati per regione WHO
df_alcohol_clean %>%
  filter(indicator_short == "Perc_Ast") %>%
  group_by(country, who_region) %>%
  summarise(media = mean(value), .groups = "drop") %>%
  arrange(desc(media)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(country, media), y = media, fill = who_region)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 paesi per astensione media dall'alcol",
    x = "Paese", y = "% Astinenti",
    fill = "Regione WHO"
  ) +
  theme_minimal()

#####################################################


install.packages("tidyr")
library(tidyr)


# Calcola media di astinenza e consumo tra bevitori per ogni paese
df_dual <- df_alcohol_clean %>%
  filter(indicator_short %in% c("Perc_Ast", "Litres_Cons")) %>%
  group_by(country, indicator_short) %>%
  summarise(media = mean(value), .groups = "drop") %>%
  pivot_wider(names_from = indicator_short, values_from = media)

# Scatter plot: Astinenti vs Litri (solo bevitori)
ggplot(df_dual, aes(x = Perc_Ast, y = Litres_Cons)) +
  geom_point(alpha = 0.7, color = "#0072B2") +
  geom_text(aes(label = country), size = 2.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "Astinenza (%) vs Litri per capita (tra bevitori)",
    x = "% Astinenti", y = "Litri per capita (15+, solo bevitori)"
  ) +
  theme_minimal()


########################################



# Calcola consumo medio stimato sulla popolazione generale
df_combined <- df_dual %>%
  mutate(Litres_total_pop = Litres_Cons * (100 - Perc_Ast) / 100)

# Top 10 paesi per consumo totale medio (stimato)
df_combined %>%
  arrange(desc(Litres_total_pop)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(country, Litres_total_pop), y = Litres_total_pop)) +
  geom_col(fill = "#D55E00") +
  coord_flip() +
  labs(
    title = "Top 10 paesi per consumo medio stimato (popolazione totale)",
    x = "Paese", y = "Litri per capita (stimati)"
  ) +
  theme_minimal()

#### Questa non è una moltiplicazione per la popolazione reale, ma solo una media stimata sul 100% degli adulti (15+).

####################################

# Assicurati di avere i dati aggregati con Litres_Cons e Perc_Ast
df_paradox <- df_alcohol_clean %>%
  filter(indicator_short %in% c("Perc_Ast", "Litres_Cons")) %>%
  group_by(country, who_region, indicator_short) %>%
  summarise(media = mean(value), .groups = "drop") %>%
  pivot_wider(names_from = indicator_short, values_from = media) %>%
  drop_na()

# Calcolo dell'indice di paradossalità
df_paradox <- df_paradox %>%
  mutate(paradox_index = Litres_Cons / (100 - Perc_Ast)) %>%
  filter(!is.infinite(paradox_index)) %>%
  arrange(desc(paradox_index))

# Visualizzazione: top 10 paesi più "paradossali"
df_paradox %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(country, paradox_index), y = paradox_index, fill = who_region)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 paesi con il maggior paradosso consumo/astinenza",
    x = "Paese", y = "Indice paradosso",
    fill = "Regione WHO"
  ) +
  theme_minimal()

####################################

df_bubble <- df_alcohol_clean %>%
  filter(indicator_short %in% c("Perc_Ast", "Litres_Cons")) %>%
  group_by(country, who_region, indicator_short) %>%
  summarise(media = mean(value), .groups = "drop") %>%
  pivot_wider(names_from = indicator_short, values_from = media) %>%
  drop_na()

ggplot(df_bubble, aes(x = Perc_Ast, y = Litres_Cons, color = who_region)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Astinenti (%) vs Litri per capita (bevitori)",
    x = "% Astinenti", y = "Litri tra i bevitori", color = "WHO Region"
  ) +
  theme_minimal()




library(tidyr)

# Riorganizza i dati in formato wide
df_corr <- df_alcohol_clean %>%
  filter(indicator_short %in% c("Perc_Cons", "Litres_Cons", "Perc_Ast")) %>%
  group_by(country, indicator_short) %>%
  summarise(media = mean(value), .groups = "drop") %>%
  pivot_wider(names_from = indicator_short, values_from = media) %>%
  drop_na()

# Scatter plot
ggplot(df_corr, aes(x = Perc_Cons, y = Litres_Cons, color = Perc_Ast)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_gradient(low = "#56B4E9", high = "#D55E00", name = "% Astinenti") +
  labs(
    title = "Relazione tra % consumatori e litri medi (colorato per % astinenti)",
    x = "% Consumatori (ultimi 12 mesi)",
    y = "Litri per capita (solo bevitori)"
  ) +
  theme_minimal()


# Calcola matrice di correlazione --> c'è correlazione negativa tra Perc_Cons e Perc_Ast
df_corr %>%
  select(Perc_Cons, Litres_Cons, Perc_Ast) %>%
  cor(method = "spearman", use = "complete.obs")





#################################################
#################################################

## TEST DI ADATTAMENTO


install.packages(c("ggpubr", "cowplot"), repos = "https://cloud.r-project.org")

# Installa il pacchetto per il QQ plot se non presente

library(ggpubr)
colnames(df_alcohol_clean)
library(dplyr)

# Filtro dati per Litres_Cons
df_litres <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons")

# Test di Shapiro per ogni genere
df_litres %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    shapiro_p = shapiro.test(value)$p.value,
    W = shapiro.test(value)$statistic
  ) -> shapiro_results

print(shapiro_results)

# QQ plot per ogni genere
ggqqplot(
  df_litres, x = "value", facet.by = "gender",
  title = "QQ-plot Litres_Cons per Genere",
  xlab = "Quantili teorici", ylab = "Quantili osservati"
)


#################### TUTTO UNITO

# Carica i pacchetti
library(dplyr)
library(ggplot2)
library(ggpubr)

# Lista dei confronti da testare
normality_tests <- list(
  list(indicatore = "Litres_Cons", gruppo = "gender"),
  list(indicatore = "Litres_Cons", gruppo = "income_group"),
  list(indicatore = "Perc_Ast",     gruppo = "who_region"),
  list(indicatore = "Perc_Cons",    gruppo = "income_group"),
  list(indicatore = "Perc_Cons",    gruppo = "gender")
)

# Loop sui confronti
for (test in normality_tests) {
  
  indicatore <- test$indicatore
  gruppo <- test$gruppo
  
  cat("\n🔬 Test di normalità:", indicatore, "~", gruppo, "\n")
  
  # Filtro dei dati con gestione specifica per income_group
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
  
  # Inizializza lista per i risultati
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
  
  # Stampa tabella riassuntiva
  shapiro_df <- do.call(rbind, risultati)
  print(shapiro_df)
  
  # Mostra QQ-plot unico con facet_wrap
  gg <- ggqqplot(
    df_filtered,
    x = "value",
    facet.by = gruppo,
    title = paste("QQ-plot:", indicatore, "~", gruppo),
    xlab = "Quantili teorici",
    ylab = "Quantili osservati"
  ) + theme_minimal()
  
  print(gg)
}



############# TEST STATISTICI NON PARAMETRICI + BOXPLOT CON SIGNIFICATIVITÀ

install.packages("FSA")      
install.packages("rcompanion")  

library(dplyr)
library(ggpubr)
library(FSA)
library(ggplot2)

# 1. Litres_Cons ~ gender → Wilcoxon + Boxplot
df1 <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons", gender %in% c("Male", "Female"))

cat("\n🔹 Wilcoxon test: Litres_Cons ~ gender\n")
print(wilcox.test(value ~ gender, data = df1))

ggplot(df1, aes(x = gender, y = value, fill = gender)) +
  geom_boxplot(alpha = 0.7) +
  stat_compare_means(method = "wilcox.test", label = "p.signif") +
  labs(title = "Litres_Cons per Gender", y = "Litri", x = "Genere") +
  theme_minimal()

# 2. Litres_Cons ~ income_group → Kruskal + Dunn + Boxplot
df2 <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons", income_group != "Unknown")

cat("\n🔹 Kruskal-Wallis: Litres_Cons ~ income_group\n")
kruskal2 <- kruskal.test(value ~ income_group, data = df2)
print(kruskal2)
if (kruskal2$p.value < 0.05) {
  cat("✅ Post-hoc Dunn test:\n")
  print(dunnTest(value ~ income_group, data = df2, method = "bonferroni"))
}

ggplot(df2, aes(x = income_group, y = value, fill = income_group)) +
  geom_boxplot(alpha = 0.7) +
  stat_compare_means(method = "kruskal.test", label = "p.signif") +
  labs(title = "Litres_Cons per Income Group", y = "Litri", x = "Reddito") +
  theme_minimal()

# 3. Perc_Ast ~ who_region → Kruskal + Dunn + Boxplot
df3 <- df_alcohol_clean %>%
  filter(indicator_short == "Perc_Ast")

cat("\n🔹 Kruskal-Wallis: Perc_Ast ~ who_region\n")
kruskal3 <- kruskal.test(value ~ who_region, data = df3)
print(kruskal3)
if (kruskal3$p.value < 0.05) {
  cat("✅ Post-hoc Dunn test:\n")
  print(dunnTest(value ~ who_region, data = df3, method = "bonferroni"))
}

ggplot(df3, aes(x = who_region, y = value, fill = who_region)) +
  geom_boxplot(alpha = 0.7) +
  stat_compare_means(method = "kruskal.test", label = "p.signif") +
  labs(title = "Perc_Ast per WHO Region", y = "% Astinenti", x = "Regione OMS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# 4. Perc_Cons ~ income_group → Kruskal + Dunn + Boxplot
df4 <- df_alcohol_clean %>%
  filter(indicator_short == "Perc_Cons", income_group != "Unknown")

cat("\n🔹 Kruskal-Wallis: Perc_Cons ~ income_group\n")
kruskal4 <- kruskal.test(value ~ income_group, data = df4)
print(kruskal4)
if (kruskal4$p.value < 0.05) {
  cat("✅ Post-hoc Dunn test:\n")
  print(dunnTest(value ~ income_group, data = df4, method = "bonferroni"))
}

ggplot(df4, aes(x = income_group, y = value, fill = income_group)) +
  geom_boxplot(alpha = 0.7) +
  stat_compare_means(method = "kruskal.test", label = "p.signif") +
  labs(title = "Perc_Cons per Income Group", y = "% Consumatori", x = "Reddito") +
  theme_minimal()

# 5. Perc_Cons ~ gender → Wilcoxon + Boxplot
df5 <- df_alcohol_clean %>%
  filter(indicator_short == "Perc_Cons", gender %in% c("Male", "Female"))

cat("\n🔹 Wilcoxon test: Perc_Cons ~ gender\n")
print(wilcox.test(value ~ gender, data = df5))

ggplot(df5, aes(x = gender, y = value, fill = gender)) +
  geom_boxplot(alpha = 0.7) +
  stat_compare_means(method = "wilcox.test", label = "p.signif") +
  labs(title = "Perc_Cons per Gender", y = "% Consumatori", x = "Genere") +
  theme_minimal()
