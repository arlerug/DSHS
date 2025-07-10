#################################################
#################################################


# dopo parte EDA -  Inizio Parte Test (Adattamento --> Non Parametrici --> Grafici allegati al risultato)

# tutti i RISULTATI , sia dei test di adattamento (normalità  quindi Shapiro-Wilk e 
# i relativi qqplot per vedere non normalità in forma grafica,  test parametrici, 
# test post-hoc con correzione  e grafici finali di conferma dei risultati 
#sono salvati in file .csv o .png nelle rispettive cartelle)


##############################################
## TEST DI ADATTAMENTO (NORMALITA') + SALVATAGGIO RISULTATI
##############################################

library(dplyr)
library(ggpubr)
library(ggplot2)

# Crea cartelle di output se non esistono
dir.create("results_testNormalita", showWarnings = FALSE)
dir.create("img_qqplot", showWarnings = FALSE)
dir.create("results_testStatistici", showWarnings = FALSE)
dir.create("results_testPostHoc", showWarnings = FALSE)
dir.create("img_TestStatistici", showWarnings = FALSE)

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
  
  # Salva la tabella in CSV
  write.csv(shapiro_df, paste0("results_testNormalita/", nome_file, ".csv"), row.names = FALSE)
  
  # QQ plot
  gg <- ggqqplot(
    df_filtered,
    x = "value",
    facet.by = gruppo,
    title = paste("QQ-plot:", indicatore, "~", gruppo),
    xlab = "Quantili teorici", ylab = "Quantili osservati"
  ) + theme_minimal()
  
  ggsave(paste0("img_qqplot/QQplot_", indicatore, "_by_", gruppo, ".png"), plot = gg, width = 8, height = 5)
}

###############################################################################
## FUNZIONI PER TEST STATISTICI NON PARAMETRICI + SALVATAGGI + CREAZIONE GRAFICO
###############################################################################

library(FSA)
library(rcompanion)
library(ggpubr)

######
## Funzione per Wilcoxon Test
######

wilcox_and_plot <- function(data, formula, gruppo, ylab, title, filename_prefix) {
  result <- wilcox.test(formula, data = data)
  print(result)
  
  # Salva risultato
  write.csv(data.frame(result = capture.output(result)), 
            file = paste0("results_testStatistici/Wilcoxon_", filename_prefix, ".csv"),
            row.names = FALSE)
  
  p <- ggplot(data, aes_string(x = gruppo, y = "value", fill = gruppo)) +
    geom_boxplot(alpha = 0.7) +
    stat_compare_means(method = "wilcox.test", label = "p.signif") +
    labs(title = title, y = ylab, x = gruppo) +
    theme_minimal()
  
  print(p)
  ggsave(paste0("img_TestStatistici/plot_", filename_prefix, ".png"), plot = p, width = 8, height = 5)
  
}

#######
## funzione per Test di Kruskal-Wallis
#######

kruskal_and_plot <- function(data, formula, gruppo, ylab, title, filename_prefix) {
  
  # Ordina i livelli di income_group se presente
  if (gruppo == "income_group") {
    data <- data %>%
      filter(.data[[gruppo]] != "Unknown") %>%
      mutate(income_group = factor(income_group, levels = c(
        "Low income", "Lower middle income", "Upper middle income", "High income"
      )))
  }
  
  # Esegui Kruskal-Wallis test
  result <- kruskal.test(formula, data = data)
  print(result)
  
  # Salva il risultato del Kruskal-Wallis
  write.csv(data.frame(result = capture.output(result)),
            file = paste0("results_testStatistici/Kruskal_", filename_prefix, ".csv"),
            row.names = FALSE)
  
  # Se significativo → Post-hoc Dunn test
  if (result$p.value < 0.05) {
    dunn <- dunnTest(formula, data = data, method = "bonferroni")
    print(dunn)
    write.csv(dunn$res,
              file = paste0("results_testPostHoc/Dunn_", filename_prefix, ".csv"),
              row.names = FALSE)
  }
  
  
  # Crea e ritorna il grafico (con ordinamento visivo per media crescente)
  
  p <- ggplot(data, aes(x = forcats::fct_reorder(.data[[gruppo]], value, .fun = mean), 
                        y = value, fill = .data[[gruppo]])) +
    geom_boxplot(alpha = 0.7) +
    stat_compare_means(method = "kruskal.test", label = "p.signif") +
    labs(title = title, y = ylab, x = gruppo) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  
  print(p)
  ggsave(paste0("img_TestStatistici/plot_", filename_prefix, ".png"), plot = p, width = 8, height = 5)
  
}

######
# ##Funzione per Test di Friedman
# ######





#######################
## TEST NON PARAMETRICI
#######################


## --- Test 1: Litres_Cons ~ gender
df1 <- df_alcohol_clean %>% filter(indicator_short == "Litres_Cons")
wilcox_and_plot(df1, value ~ gender, "gender", "Litri", "Litres_Cons per Genere", "Litres_Cons_by_gender")


## --- Test 2: Litres_Cons ~ income_group
df2 <- df_alcohol_clean %>% filter(indicator_short == "Litres_Cons", income_group != "Unknown") %>%
  mutate(income_group = factor(income_group, levels = c(
    "Low income", "Lower middle income", "Upper middle income", "High income"
  )))
kruskal_and_plot(df2, value ~ income_group, "income_group", "Litri", "Litres_Cons per Income Group", "Litres_Cons_by_income_group")


## --- Test 3: Perc_Ast ~ who_region
df3 <- df_alcohol_clean %>% filter(indicator_short == "Perc_Ast")
kruskal_and_plot(df3, value ~ who_region, "who_region", "% Astinenti", "Perc_Ast per Regione OMS", "Perc_Ast_by_who_region")


## --- Test 4: Perc_Cons ~ income_group
df4 <- df_alcohol_clean %>% filter(indicator_short == "Perc_Cons", income_group != "Unknown") %>%
  mutate(income_group = factor(income_group, levels = c(
    "Low income", "Lower middle income", "Upper middle income", "High income"
  )))
kruskal_and_plot(df4, value ~ income_group, "income_group", "% Consumatori", "Perc_Cons per Income Group", "Perc_Cons_by_income_group")


## --- Test 5: Perc_Cons ~ gender
df5 <- df_alcohol_clean %>% filter(indicator_short == "Perc_Cons")
wilcox_and_plot(df5, value ~ gender, "gender", "% Consumatori", "Perc_Cons per Genere", "Perc_Cons_by_gender")


## --- Test 6: Litres_Cons ~ year in Italy, France, Germany (Friedman)

library(tidyr)
library(dplyr)
library(PMCMRplus)
library(ggplot2)
library(ggpubr)
library(tibble)

# 0. Crea le cartelle se non esistono
dir.create("results_testStatistici", showWarnings = FALSE)
dir.create("results_testPostHoc", showWarnings = FALSE)
dir.create("img_TestStatistici", showWarnings = FALSE)

# 1. Filtro e controllo
df_filtered <- df_alcohol_clean %>%
  filter(indicator_short == "Litres_Cons", country %in% c("Italy", "Germany", "France"))

# 2. Controlla numero osservazioni per combinazione
df_filtered %>% count(country, year) %>% print(n = Inf)

# 3. Pivot: righe = paesi, colonne = anni
df_wide <- df_filtered %>%
  select(country, year, value) %>%
  group_by(country, year) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = value)

# 4. Converte in matrice
mat <- df_wide %>%
  column_to_rownames("country") %>%
  as.matrix()

# 5. Verifica struttura
print(dim(mat))          # deve essere 3 x 21
print(is.numeric(mat))   # deve essere TRUE
print(mat[1:3, 1:5])     # preview

# 6. Test di Friedman
cat("\n📊 Test di Friedman: Litres_Cons tra Italy, France, Germany (2000–2020)\n")
friedman_result <- friedman.test(mat)
print(friedman_result)

# ⬇️ Salva risultati test Friedman
write.csv(
  data.frame(Result = capture.output(friedman_result)),
  "results_testStatistici/friedman_litres_italy_france_germany.csv",
  row.names = FALSE
)

# 7. Post-hoc se significativo
if (friedman_result$p.value < 0.05) {
  posthoc <- frdAllPairsExactTest(mat, p.adjust.method = "bonferroni")
  
  # Trasforma in tabella leggibile
  posthoc_matrix <- posthoc$p.value
  posthoc_df <- as.data.frame(as.table(posthoc_matrix))
  colnames(posthoc_df) <- c("Group1", "Group2", "p_value")
  print(posthoc_df)
  
  # Salva il post-hoc
  write.csv(
    posthoc_df,
    "results_testPostHoc/friedman_posthoc_litres_italy_france_germany.csv",
    row.names = FALSE
  )
}

# 8. Plot dei trend temporali
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
  theme_minimal()

# Mostra e salva
print(p)
ggsave("img_TestStatistici/plot_friedman_litres_italy_france_germany.png", plot = p, width = 8, height = 5)





