# Analisi esplorativa e statistica del consumo globale di alcol: evidenze da dati WHO disaggregati per fattori socio-demografici
---
## Autori

- **Daniele Angeloni** – `daniele.angeloni@studenti.unipg.it`  
- **Alessandra Ruggeri** – `alessandra.ruggeri@studenti.unipg.it`  

*Università degli Studi di Perugia*  
*Corso Data Science for Health Systems – A.A. 2024/2025*
---
## Descrizione del progetto

Questo progetto analizza le disuguaglianze nel consumo di alcol utilizzando dati disaggregati provenienti dall’**Health Inequality Data Repository** dell’Organizzazione Mondiale della Sanità (WHO).  
L’obiettivo è esaminare la diffusione, l’intensità e l’astensione dal consumo di alcol secondo variabili socio-demografiche come **genere**, **gruppo di reddito**, **regione WHO** e **trend temporali**.

L’analisi si è focalizzata su tre indicatori principali:
- `Alcohol, consumers past 12 months (%)` → **Perc_Cons**
- `Alcohol, drinkers only per capita (15+) consumption (litres of pure alcohol)` → **Litres_Cons**
- `Alcohol, abstainers lifetime (%)` → **Perc_Ast**

L’intero workflow è stato implementato in **linguaggio R**, includendo fasi di preprocessing del dataset, analisi esplorativa dei dati (EDA), test statistici e visualizzazioni comparative.

---

## Metodologia statistica

Per verificare l’ipotesi di disuguaglianza tra gruppi socio-demografici sono stati applicati:

- **Test di normalità**: Shapiro-Wilk + QQ-plot
- **Test non parametrici**:
  - Wilcoxon (tra due gruppi: genere)
  - Kruskal-Wallis (tra più gruppi: reddito, regione)
  - Friedman (su dati longitudinali: Italia, Francia, Germania)
- **Test post-hoc** con correzione di Bonferroni (Dunn e Friedman)

---

## Contenuti principali del progetto

- `dataset_preprocessed.xlsx` – Dataset pulito e preprocessato
- `alcohol_consumption_analysis.R` – Codice R completo del progetto
- `img_eda/` – Visualizzazioni EDA (boxplot, trend, violin plot, heatmap ecc.)
- `img_qqplot/` – QQ-plot dei test di normalità
- `img_compareTest/` – Grafici per il confronto visivo con i test statistici 
- `results_testNormalita/` – Risultati test di normalità Shapiro-Wilk (.csv)
- `results_testNonParametrici/` – Risultati Wilcoxon, Kruskal, Friedman (.csv)
- `results_testPostHoc/` – Post-hoc test (.csv)

---

## Requisiti R

Assicurati di avere installato i seguenti pacchetti:

```r
install.packages(c("dplyr", "ggplot2", "readxl", "writexl", "ggpubr",
                   "PMCMRplus", "FSA", "rcompanion", "patchwork", 
                   "tibble", "gridExtra", "tidyr", "stringr"))
```
**Nota**: nel file `alcohol_consumption_analysis.R` ricordati di **modificare il percorso assoluto della working directory**
alla riga:
```
setwd("insert/your/path/of/project_DSHS")
```


