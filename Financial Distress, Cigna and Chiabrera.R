library(tidyverse)
library(readxl)
library (dplyr)

leggi_orbis <- function(path) {
  read_excel(path, sheet = "Risultati", na = "n.d.") %>%
    mutate(across(everything(), as.character))
}

# Attive — distress = 0
df_a1 <- leggi_orbis("Attive definitivo, 1.xlsx") %>% mutate(distress = "0")
df_a2 <- leggi_orbis("Attive definitivo, 2.xlsx") %>% mutate(distress = "0")
df_a3 <- leggi_orbis("Attive definitivo, 3.xlsx") %>% mutate(distress = "0")
df_a4 <- leggi_orbis("Attive definitivo, 4.xlsx") %>% mutate(distress = "0")

# Fallite — distress = 1
df_f1 <- leggi_orbis("Fallite definitivo, 1.xlsx") %>% mutate(distress = "1")
df_f2 <- leggi_orbis("Fallite definitivo, 2.xlsx") %>% mutate(distress = "1")
df_f3 <- leggi_orbis("Fallite definitivo, 3.xlsx") %>% mutate(distress = "1")
df_f4 <- leggi_orbis("Fallite definitivo, 4.xlsx") %>% mutate(distress = "1")


df_unito <- bind_rows(df_a1, df_a2, df_a3, df_a4,
                      df_f1, df_f2, df_f3, df_f4) %>%
  filter(!if_all(everything(), is.na))

cat("Righe totali dopo import:", nrow(df_unito), "\n")
cat("Colonne totali:", ncol(df_unito), "\n")
cat("Distress = 0:", sum(df_unito$distress == "0"), "\n")
cat("Distress = 1:", sum(df_unito$distress == "1"), "\n")

n_prima <- nrow(df_unito)
df_unito <- df_unito %>% distinct(`Numero BvD ID`, .keep_all = TRUE)
cat("Duplicati rimossi:", n_prima - nrow(df_unito), "\n")
cat("Righe dopo deduplicazione:", nrow(df_unito), "\n")

cat("Distress = 0:", sum(df_unito$distress == "0"), "\n")
cat("Distress = 1:", sum(df_unito$distress == "1"), "\n")

keep_char <- c(
  "Ragione socialeCaratteri latini",
  "Numero BvD ID",
  "Area geografica",
  "BvD sectors",
  "NACE Rev. 2, sezione principale",
  "NACE Rev. 2, codici/e primari/o",
  "Anno fiscale Ultimo anno disp.",
  "Data di costituzione",
  "Classificazione per dimensione",
  "distress"
)

nace_col <- "NACE Rev. 2, codici/e primari/o"
n_prima  <- nrow(df_unito)

df_unito <- df_unito %>%
  filter(is.na(.data[[nace_col]]) |
           !substr(as.character(.data[[nace_col]]), 1, 2) %in% c("64", "65", "66"))

cat("Righe rimosse per settore finanziario:", n_prima - nrow(df_unito), "\n")
cat("Righe rimaste:", nrow(df_unito), "\n")

df_finale <- df_unito %>%
  mutate(across(
    where(is.character) & !all_of(keep_char),
    ~ suppressWarnings(as.numeric(.))
  ))


na_col <- df_finale %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variabile", values_to = "pct_na") %>%
  arrange(desc(pct_na))

cat("\nTop 30 variabili per % NA:\n")
print(na_col, n = 30)

na_riga <- rowMeans(is.na(df_finale))
summary(na_riga)
hist(na_riga, main = "NA per row — before filtering",
     xlab = "% NA per row", col = "steelblue")


col_ok      <- colMeans(is.na(df_finale)) <= 0.50
df_filtrato <- df_finale[, col_ok]
cat("\nColonne rimosse (>50% NA):", sum(!col_ok), "\n")

df_filtrato <- df_filtrato %>%
  mutate(eliminata = rowMeans(is.na(.)) > 0.20)
sum(df_filtrato$eliminata == 1)

cat("\nEliminazione per area geografica:\n")
df_filtrato %>%
  group_by(`Area geografica`, eliminata) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = eliminata, values_from = n, names_prefix = "elim_") %>%
  mutate(pct_eliminata = round(elim_TRUE / (elim_TRUE + elim_FALSE), 3)) %>%
  arrange(desc(pct_eliminata)) %>%
  print()

cat("\nEliminazione per dimensione:\n")
df_filtrato %>%
  group_by(`Classificazione per dimensione`, eliminata) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = eliminata, values_from = n, names_prefix = "elim_") %>%
  mutate(pct_eliminata = round(elim_TRUE / (elim_TRUE + elim_FALSE), 3)) %>%
  arrange(desc(pct_eliminata)) %>%
  print()

cat("\nEliminazione per classe distress:\n")
df_filtrato %>%
  group_by(distress, eliminata) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = eliminata, values_from = n, names_prefix = "elim_") %>%
  mutate(pct_eliminata = round(elim_TRUE / (elim_TRUE + elim_FALSE), 3)) %>%
  print()

chisq.test(table(df_filtrato$`Area geografica`, df_filtrato$eliminata))
chisq.test(table(df_filtrato$`Classificazione per dimensione`, df_filtrato$eliminata))
chisq.test(table(df_filtrato$distress, df_filtrato$eliminata))

df_filtrato <- df_filtrato %>% 
  select(-eliminata) %>% 
  filter(`Area geografica` %in% c("Western Europe", "Eastern Europe", "Africa",
                                  "Far East and Central Asia", "North America"))

active     <- df_filtrato %>% filter(distress == "0")
distressed <- df_filtrato %>% filter(distress == "1")

filtra_righe <- function(df, soglia = 0.20) df[rowMeans(is.na(df)) <= soglia, ]

active     <- filtra_righe(active)
distressed <- filtra_righe(distressed)

cat("Active dopo filtro righe:    ", nrow(active), "\n")
cat("Distressed dopo filtro righe:", nrow(distressed), "\n")
cat("Sbilanciamento: 1 distressed ogni",
    round(nrow(active) / nrow(distressed), 1), "active\n")

df <- bind_rows(active, distressed) %>%
  mutate(distress = as.factor(distress))

col_ok2 <- colMeans(is.na(df)) <= 0.25
df2 <- df[, col_ok2]
cat("\nColonne rimosse (>25% NA):", sum(!col_ok2), "\n")

na_col2 <- df2 %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variabile", values_to = "pct_na") %>%
  arrange(desc(pct_na))

cat("\nTop 30 variabili per % NA:\n")
print(na_col2, n = 30)


active     <- df2 %>% filter(distress == "0")
distressed <- df2 %>% filter(distress == "1")

filtra_righe <- function(df, soglia = 0.10) df[rowMeans(is.na(df)) <= soglia, ]

active     <- filtra_righe(active)
distressed <- filtra_righe(distressed)

cat("Active dopo filtro righe:    ", nrow(active), "\n")
cat("Distressed dopo filtro righe:", nrow(distressed), "\n")
cat("Sbilanciamento: 1 distressed ogni",
    round(nrow(active) / nrow(distressed), 1), "active\n")

df2 <- bind_rows(active, distressed) %>%
  mutate(distress = as.factor(distress))


na_riga2 <- rowMeans(is.na(df2))
summary(na_riga2)
hist(na_riga2, main = "NA distribution per row — after filtering",
     xlab = "% NA per row", col = "steelblue")
sum(is.na(df2))/(30977*130)

df2 <- df2 %>%
  filter(`Totale Attivo migl USD Ultimo anno disp.` > 0) %>% 
  filter(`Passività correnti migl USD Ultimo anno disp.` +
           `Passività non correnti migl USD Ultimo anno disp.` > 0) %>% 
  filter(`Totale Attivo migl USD Anno - 1` > 0, `Totale Attivo migl USD Anno - 2` > 0,
         `Passività correnti migl USD Anno - 1` +
           `Passività non correnti migl USD Anno - 1` > 0,
         `Passività correnti migl USD Anno - 2` +
           `Passività non correnti migl USD Anno - 2` > 0)
cat("Dopo filtro TA>0:", nrow(df2), "\n")

df2 <- df2 %>%
  mutate(
    anno_costituzione = case_when(
      grepl("/", `Data di costituzione`) ~
        year(dmy(`Data di costituzione`)),
      nchar(`Data di costituzione`) == 4 ~
        as.integer(`Data di costituzione`),
      TRUE ~
        year(as.Date(as.numeric(`Data di costituzione`),
                     origin = "1899-12-30"))
    ),
    anno_costituzione = if_else(
      anno_costituzione < 1800 | anno_costituzione > 2025,
      NA_integer_,
      anno_costituzione
    ),
    eta_impresa = as.integer(`Ultimo anno disp.`) - anno_costituzione
  ) %>%
  mutate(eta_impresa = if_else(eta_impresa < 0, NA_integer_, eta_impresa))

cat("\nAnno costituzione — NA:", sum(is.na(df2$anno_costituzione)), "\n")
print(summary(df2$eta_impresa))


col_categoriche <- c(
  "...1",
  "Ragione socialeCaratteri latini",
  "Numero BvD ID",
  "Area geografica",
  "BvD sectors",
  "NACE Rev. 2, sezione principale",
  "NACE Rev. 2, codici/e primari/o",
  "Classificazione per dimensione",
  "Data di costituzione",
  "anno_costituzione",
  "eta_impresa",
  "N° partecipate registrate",
  "distress",
  "Ultimo anno disp.",
  "Anno fiscale Ultimo anno disp."
)

cols_t0 <- names(df)[grepl("Ultimo anno disp\\.", names(df))]
cols_t1 <- names(df)[grepl("Anno - 1", names(df))]
cols_t2 <- names(df)[grepl("Anno - 2", names(df))]
cols_t3 <- names(df)[grepl("Anno - 3", names(df))]

cat("\nColonne per periodo: t0=", length(cols_t0),
    "t1=", length(cols_t1),
    "t2=", length(cols_t2),
    "t3=", length(cols_t3), "\n")


na_count <- colSums(is.na(df2))
na_percent <- round((na_count / nrow(df2)) * 100, 2)

na_report <- data.frame(
  variabile = names(na_count),
  totale_na = na_count,
  percentuale = na_percent
) %>%
  arrange(desc(percentuale))


df2 <- df2 %>%
  mutate(
    passivita_t0 = `Passività correnti migl USD Ultimo anno disp.` +
      `Passività non correnti migl USD Ultimo anno disp.`,
    passivita_t1 = `Passività correnti migl USD Anno - 1` +
      `Passività non correnti migl USD Anno - 1`,
    passivita_t2 = `Passività correnti migl USD Anno - 2` +
      `Passività non correnti migl USD Anno - 2`,
    
    ebit_t0 = `Utile/Perdita prima delle imposte migl USD Ultimo anno disp.` -
      `Proventi/oneri finanziari migl USD Ultimo anno disp.`,
    ebit_t1 = `Utile/Perdita prima delle imposte migl USD Anno - 1` -
      `Proventi/oneri finanziari migl USD Anno - 1`,
    ebit_t2 = `Utile/Perdita prima delle imposte migl USD Anno - 2` -
      `Proventi/oneri finanziari migl USD Anno - 2`,
    X1 = (`Attività correnti migl USD Anno - 2` - 
            `Passività correnti migl USD Anno - 2`)/`Totale Attivo migl USD Anno - 2`,
    X2 = `Patrimonio netto migl USD Anno - 2` /
      `Totale Attivo migl USD Anno - 2`,
    X3 = ebit_t2 / `Totale Attivo migl USD Anno - 2`,
    X4 = `Patrimonio netto migl USD Anno - 2` / passivita_t2,
    X5 = `Ricavi vendite e prestazioni migl USD Anno - 2` / `Totale Attivo migl USD Anno - 2`,
    
    Z_score = 0.717*X1 + 0.847*X2 + 3.107*X3 + 0.420*X4 + 0.998*X5,
    
    zona_altman = case_when(
      Z_score >= 2.90  ~ "Sana",
      Z_score >= 1.23 & Z_score < 2.90 ~ "Grigia",
      Z_score < 1.23    ~ "Distress"
    )
  )


winsorize <- function(x, p = 0.01) {
  q <- quantile(x, c(p, 1 - p), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

cols_winsorize_df <- intersect(c(
  "X1", "X2", "X3", "X4", "Z_score",
  "Redditività del totale Attivo (ROA) - Lordo Ultimo anno disp.",
  "Redditività del capitale proprio (ROE) - Lordo Ultimo anno disp.",
  "Rendimento del capitale investito (ROCE) - Lordo Ultimo anno disp.",
  "Indice di leva (gearing) Ultimo anno disp.",
  "Margine di profitto Ultimo anno disp.",
  "Margine EBIT Ultimo anno disp.",
  "Margine EBITDA Ultimo anno disp.",
  "Copertura degli interessi Ultimo anno disp.",
  "Coefficiente di solvibilità (sulla base del patrimonio) Ultimo anno disp.",
  "Coefficiente di solvibilità (sulla base della liquidità) Ultimo anno disp.",
  "Indice di disponibilità Ultimo anno disp....41",
  "Indice di liquidità Ultimo anno disp."
), names(df2))

df2 <- df2 %>% mutate(across(all_of(cols_winsorize_df), winsorize))
cat("Winsorizzazione su df — colonne:", length(cols_winsorize_df), "\n")
print(summary(df2$Z_score))
print(table(df2$zona_altman, useNA = "ifany"))

saveRDS(df2, "df2.rds")
######################IMPORTANTE UNA VOLTA ARRIVATI QUA SPLITTARE TRAIN - TEST#################
# ----------- IMPUTAZIONE MEAN + NOISE ------------

library(caret)

set.seed(123)
train_index <- createDataPartition(df2$distress, p = 0.7, list = FALSE)

train <- df2[train_index, ]
test  <- df2[-train_index, ]

cat("Train:", nrow(train), " | Test:", nrow(test), "\n")

colonne_da_imputare <- train %>%
  summarise(across(where(is.numeric), ~ sum(is.na(.)))) %>%
  pivot_longer(everything()) %>%
  filter(value > 0,
         name != "anno_costituzione",
         name != "eta_impresa") %>%
  pull(name)

cat("Variabili da imputare:", length(colonne_da_imputare), "\n")

parametri_imputazione <- train %>%
  summarise(across(all_of(colonne_da_imputare),
                   list(mu = ~mean(.x, na.rm = TRUE),
                        sigma = ~sd(.x, na.rm = TRUE))))

imputa_con_parametri <- function(x, mu, sigma) {
  if(all(!is.na(x))) return(x)
  x[is.na(x)] <- mu + rnorm(sum(is.na(x)), 0, sigma)
  return(x)
}

train_imputato <- train

for (col in colonne_da_imputare) {
  mu <- parametri_imputazione[[paste0(col, "_mu")]]
  sigma <- parametri_imputazione[[paste0(col, "_sigma")]]
  
  train_imputato[[col]] <- imputa_con_parametri(train[[col]], mu, sigma)
}

test_imputato <- test

for (col in colonne_da_imputare) {
  mu <- parametri_imputazione[[paste0(col, "_mu")]]
  sigma <- parametri_imputazione[[paste0(col, "_sigma")]]
  
  test_imputato[[col]] <- imputa_con_parametri(test[[col]], mu, sigma)
}

cat("NA train dopo imputazione:", sum(is.na(train_imputato)), "\n")
cat("NA test dopo imputazione:", sum(is.na(test_imputato)), "\n")

saveRDS(train_imputato, "train_imputato.rds")
saveRDS(test_imputato,  "test_imputato.rds")


### Sottrazioni
sottrazioni <- function(data){
  col_categoriche <- c(
    "Ragione socialeCaratteri latini",
    "Numero BvD ID",
    "Area geografica",
    "BvD sectors",
    "NACE Rev. 2, sezione principale",
    "NACE Rev. 2, codici/e primari/o",
    "Anno fiscale Ultimo anno disp.",
    "Data di costituzione",
    "Classificazione per dimensione",
    "distress",
    "N° partecipate registrate"
  )
  periodi_correnti  <- c("Ultimo anno disp.", "Anno - 1", "Anno - 2")
  periodi_precedenti <- c("Anno - 1",         "Anno - 2", "Anno - 3")
  data <- data[-1]
  df <- data[, !(names(data) %in% "Totale valore della produzione migl USD Ultimo anno disp....13")]
  df <- rename(df, `Totale valore della produzione migl USD Ultimo anno disp.` = `Totale valore della produzione migl USD Ultimo anno disp....12`)
  # Parti dal df con solo le colonne categoriche
  risultato <- df %>% select(all_of(col_categoriche))
  
  # Subtractions
  for (i in seq_along(periodi_correnti)) {
    
    attuale   <- periodi_correnti[i]
    precedente <- periodi_precedenti[i]
    
    # Trova le colonne del periodo attuale (escluse quelle categoriche)
    col_attuali <- names(df)[grepl(attuale, names(df), fixed = TRUE)]
    
    for (col_att in col_attuali) {
      
      col_prec <- sub(attuale, precedente, col_att, fixed = TRUE)
      nome_ratio <- paste0("sub_T", i, "_", col_att)
      
      if (col_prec %in% names(df)) {
        risultato[[nome_ratio]] <- df[[col_att]] - df[[col_prec]]
      } else {
        risultato[[nome_ratio]] <- NA_real_
      }
    }
  }
  
  # Filtra colonne (max 70% NA)
  col_ok      <- colMeans(is.na(risultato)) <= 0.99
  data <- risultato[, col_ok]
  df <- df%>%select(-(1:4))
  df <- df%>%select(-(2:6))
  data <- full_join(data, df, "Numero BvD ID")
  return(data)
}

train<- sottrazioni(train_imputato)
test <- sottrazioni(test_imputato)

saveRDS(train, "train.rds")
saveRDS(test, "test.rds")

### Clustering
library(cluster)
library(factoextra)
library(NbClust)

# Corr matrix and distances
quant <- train[,12:209]
cor_matrix <- cor(quant, method = "pearson")
distance_R2 <-  1- (cor_matrix^2)
distance_R <- 1-abs(cor_matrix)

eu_2 <- as.dist(distance_R2)
eu_1 <- as.dist(distance_R)

# Dendrograms
hc_compl_2 <- hclust(eu_2, method = 'complete')
hc_sing_1 <- hclust(eu_1, method = 'single')
hc_compl_1 <- hclust(eu_1, method = 'complete')
plot(hc_compl_1, labels=FALSE)
plot(hc_compl_2, labels = FALSE)


# ELBOW?
#fviz_nbclust(distance_R2, hcut, method = 'wss')
fviz_nbclust(distance_R2, kmeans, method = 'wss')
#fviz_nbclust(distance_R, hcut, method = 'wss')
fviz_nbclust(distance_R, kmeans, method = 'wss')

fviz_nbclust(distance_R2, hcut, method = "silhouette")
fviz_nbclust(distance_R2, kmeans, method = "silhouette")
#kmsol <- fviz_nbclust(quant, kmeans, 'silhouette')

fviz_nbclust(distance_R, hcut, method = "silhouette")
fviz_nbclust(distance_R, kmeans, method = "silhouette")

optimal_1 <- NbClust(distance_R, distance = "euclidean", min.nc = 2,
                     max.nc = 20, method = "complete", index = "silhouette")
# 2 clusters are useless
# Let us do it artigianally
# The plot for the R looks like the most informative
plot(hc_compl_1, labels=FALSE)
# We identify 14 big branches
groups <- cutree(hc_compl_1, k = 15)

# Let's extract
mappa <- data.frame(variable = names(groups), Cluster = groups)
mappa <- mappa[order(mappa$Cluster), ]
#write_xlsx(mappa, "second clusters.xlsx")
groups <- split(mappa$variable, mappa$Cluster)
quant <- as.data.frame(quant)

# Groups
quant_1 <- quant %>% select(groups[[1]])
quant_2 <- quant %>% select(groups[[2]])
quant_3 <- quant %>% select(groups[[3]])
quant_4 <- quant %>% select(groups[[4]])
quant_5 <- quant %>% select(groups[[5]])
quant_6 <- quant %>% select(groups[[6]])
quant_7 <- quant %>% select(groups[[7]])
quant_8 <- quant %>% select(groups[[8]])
quant_9 <- quant %>% select(groups[[9]])
quant_10 <- quant %>% select(groups[[10]])
quant_11 <- quant %>% select(groups[[11]])
quant_12 <- quant %>% select(groups[[12]])
quant_13 <- quant %>% select(groups[[13]])
quant_14 <- quant %>% select(groups[[14]])
quant_15 <- quant %>% select(groups[[15]])

# PCA
PCA_1 <- prcomp(quant_1, scale. = TRUE)
PCA_2 <- prcomp(quant_2, scale. = TRUE)
PCA_3 <- prcomp(quant_3, scale. = TRUE)
PCA_4 <- prcomp(quant_4, scale. = TRUE)
PCA_5 <- prcomp(quant_5, scale. = TRUE)
PCA_6 <- prcomp(quant_6, scale. = TRUE)
PCA_7 <- prcomp(quant_7, scale. = TRUE)
PCA_8 <- prcomp(quant_8, scale. = TRUE)
PCA_9 <- prcomp(quant_9, scale. = TRUE)
PCA_10 <- prcomp(quant_10, scale. = TRUE)
PCA_11 <- prcomp(quant_11, scale. = TRUE)
PCA_12 <- prcomp(quant_12, scale. = TRUE)
PCA_13 <- prcomp(quant_13, scale. = TRUE)
PCA_14 <- prcomp(quant_14, scale. = TRUE)
PCA_15 <- prcomp(quant_15, scale. = TRUE)

# Variance
get_eig(PCA_1)
get_eig(PCA_2)
get_eig(PCA_3)
get_eig(PCA_4)
get_eig(PCA_5)
get_eig(PCA_6)
get_eig(PCA_7)
get_eig(PCA_8)
get_eig(PCA_9)
get_eig(PCA_10)
get_eig(PCA_11)
get_eig(PCA_12)
get_eig(PCA_13)
get_eig(PCA_14)
get_eig(PCA_15)

# Graphing loadings
fviz_pca_var(PCA_1, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_2, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_3, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_4, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_5, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_6, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_7, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_8, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_9, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_10, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_11, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_12, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_13, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_14, col.var = 'contrib', repel=TRUE)
fviz_pca_var(PCA_15, col.var = 'contrib', repel=TRUE)

#Saving on excel
#writexl::write_xlsx(quant, "quant.xlsx")

# Computing loadings
get_eig(PCA_1)
var_contrib <- get_pca_var(PCA_1)$contrib
sort(var_contrib[,1], decreasing = TRUE)

get_eig(PCA_2)
var_contrib <- get_pca_var(PCA_2)$contrib
sort(var_contrib[,1], decreasing = TRUE)
sort(var_contrib[,2], decreasing = TRUE)
sort(var_contrib[,3], decreasing = TRUE)
sort(var_contrib[,4], decreasing = TRUE)

get_eig(PCA_3)
var_contrib <- get_pca_var(PCA_3)$contrib
sort(var_contrib[,1], decreasing = TRUE)
sort(var_contrib[,2], decreasing = TRUE)
sort(var_contrib[,3], decreasing = TRUE)
sort(var_contrib[,4], decreasing = TRUE)

get_eig(PCA_4)
var_contrib <- get_pca_var(PCA_4)$contrib
sort(var_contrib[,1], decreasing = TRUE)

get_eig(PCA_5)
var_contrib <- get_pca_var(PCA_5)$contrib
sort(var_contrib[,1], decreasing = TRUE)
sort(var_contrib[,2], decreasing = TRUE)
sort(var_contrib[,3], decreasing = TRUE)
sort(var_contrib[,4], decreasing = TRUE)

get_eig(PCA_6)
var_contrib <- get_pca_var(PCA_6)$contrib
sort(var_contrib[,1], decreasing = TRUE)
sort(var_contrib[,2], decreasing = TRUE)
sort(var_contrib[,3], decreasing = TRUE)

get_eig(PCA_7)
var_contrib <- get_pca_var(PCA_7)$contrib
sort(var_contrib[,1], decreasing = TRUE)
sort(var_contrib[,2], decreasing = TRUE)

get_eig(PCA_8)
var_contrib <- get_pca_var(PCA_8)$contrib
sort(var_contrib[,1], decreasing = TRUE)
sort(var_contrib[,2], decreasing = TRUE)
sort(var_contrib[,3], decreasing = TRUE)
sort(var_contrib[,4], decreasing = TRUE)

get_eig(PCA_9)
var_contrib <- get_pca_var(PCA_9)$contrib
sort(var_contrib[,1], decreasing = TRUE)
sort(var_contrib[,2], decreasing = TRUE)

get_eig(PCA_10)
var_contrib <- get_pca_var(PCA_10)$contrib
sort(var_contrib[,1], decreasing = TRUE)
sort(var_contrib[,2], decreasing = TRUE)

get_eig(PCA_11)
var_contrib <- get_pca_var(PCA_11)$contrib
sort(var_contrib[,1], decreasing = TRUE)
sort(var_contrib[,2], decreasing = TRUE)
sort(var_contrib[,3], decreasing = TRUE)
sort(var_contrib[,4], decreasing = TRUE)
sort(var_contrib[,5], decreasing = TRUE)

get_eig(PCA_12)
var_contrib <- get_pca_var(PCA_12)$contrib
sort(var_contrib[,1], decreasing = TRUE)

get_eig(PCA_13)
var_contrib <- get_pca_var(PCA_13)$contrib
sort(var_contrib[,1], decreasing = TRUE)
sort(var_contrib[,2], decreasing = TRUE)

get_eig(PCA_14)
var_contrib <- get_pca_var(PCA_14)$contrib
sort(var_contrib[,1], decreasing = TRUE)
sort(var_contrib[,2], decreasing = TRUE)
sort(var_contrib[,3], decreasing = TRUE)
sort(var_contrib[,4], decreasing = TRUE)

get_eig(PCA_15)
var_contrib <- get_pca_var(PCA_15)$contrib
sort(var_contrib[,1], decreasing = TRUE)
sort(var_contrib[,2], decreasing = TRUE)
sort(var_contrib[,3], decreasing = TRUE)
sort(var_contrib[,4], decreasing = TRUE)
sort(var_contrib[,5], decreasing = TRUE)

# Selecting the variables
train <- train%>%select(
  #"Ragione socialeCaratteri latini",
  #"Numero BvD ID",
  "Area geografica",
  #"BvD sectors",
  #"NACE Rev. 2, sezione principale",
  #"NACE Rev. 2, codici/e primari/o",
  "Anno fiscale Ultimo anno disp.",
  #"Data di costituzione",
  "Classificazione per dimensione",
  "distress.x",
  "N° partecipate registrate.x",
  eta_impresa,
  `sub_T1_Totale valore della produzione migl USD Ultimo anno disp.`,
  `Capitale sociale migl USD Anno - 3`,
  `Coefficiente di solvibilità (sulla base del patrimonio) Anno - 2`,
  `Immobilizzazioni immateriali migl USD Anno - 1`,
  `Immobilizzazioni materiali migl USD Anno - 1`,
  `Indice di disponibilità Ultimo anno disp....33`,
  `Indice di leva (gearing) Anno - 1`,
  `Indice di liquidità Anno - 1`,
  `Indice di liquidità Ultimo anno disp.`,
  `Margine EBIT Anno - 2`,
  `Margine EBIT Anno - 3`,
  `Margine EBIT Ultimo anno disp.`,
  `Proventi/oneri finanziari migl USD Ultimo anno disp.`,
  `Redditività del capitale proprio (ROE) - Lordo Ultimo anno disp.`,
  `Redditività del totale Attivo (ROA) - Lordo Anno - 3`,
  `Redditività del totale Attivo (ROA) - Lordo Ultimo anno disp.`,
  `Ricavi vendite e prestazioni migl USD Anno - 2`,
  `Rimanenze migl USD Ultimo anno disp.`,
  `sub_T1_Attività correnti migl USD Ultimo anno disp.`,
  `sub_T1_Capitale circolante migl USD Ultimo anno disp.`,
  `sub_T1_Coefficiente di solvibilità (sulla base del patrimonio) Ultimo anno disp.`,
  `sub_T1_Disponibilità liquide e mezzi equivalenti migl USD Ultimo anno disp.`,
  `sub_T1_Margine sui consumi migl USD Ultimo anno disp.`,
  `sub_T1_Redditività del capitale proprio (ROE) - Lordo Ultimo anno disp.`,
  `sub_T1_Rimanenze migl USD Ultimo anno disp.`,
  `sub_T1_Utile/perdita di esercizio [utile netto] migl USD Ultimo anno disp.`,
  `sub_T2_Coefficiente di solvibilità (sulla base del patrimonio) Anno - 1`,
  `sub_T2_Crediti verso clienti migl USD Anno - 1`,
  `sub_T2_Immobilizzazioni immateriali migl USD Anno - 1`,
  `sub_T2_Indice di leva (gearing) Anno - 1`,
  `sub_T2_Proventi/oneri finanziari migl USD Anno - 1`,
  `sub_T2_Redditività del capitale proprio (ROE) - Lordo Anno - 1`,
  `sub_T2_Utile/Perdita prima delle imposte migl USD Anno - 1`,
  `sub_T3_Attività correnti migl USD Anno - 2`,
  `sub_T3_Capitale circolante migl USD Anno - 2`,
  `sub_T3_Coefficiente di solvibilità (sulla base del patrimonio) Anno - 2`,
  `sub_T3_Indice di leva (gearing) Anno - 2`,
  `sub_T3_Indice di liquidità Anno - 2`,
  `sub_T3_Margine sui consumi migl USD Anno - 2`,
  `sub_T3_Redditività del totale Attivo (ROA) - Lordo Anno - 2`,
  `Totale Attivo migl USD Anno - 1`,
  `Utile/Perdita prima delle imposte migl USD Ultimo anno disp.`
)

# Nuovo Clustering
quant <- scale(train[,12:208])
eu <- dist(quant)
fviz_nbclust(quant, 
             FUNcluster = function(x, k) hcut(eu, k = k, hc_method = "ward.D2"),
             method = 'wss')

#fviz_nbclust(quant, hcut, method = 'wss')
fviz_nbclust(quant, kmeans, method = 'wss')
fviz_nbclust(quant, hcut, method = "silhouette")
fviz_nbclust(quant, kmeans, method = "silhouette")

set.seed(42)

train_sorted <- train[sample(nrow(train), 7000), ]
table(train_sorted$distress.x)
table(train_sorted$distress.x)
quant <- scale(train_sorted[,12:208])
quant <- as.data.frame(quant)
fviz_nbclust(quant, kmeans, method = 'wss')
fviz_nbclust(quant, kmeans, method = "silhouette")


quant <- scale(train[,12:208])
cluster_labels <- kmeans(quant, centers = 2, nstart = 50)$cluster
tab <- table(Cluster = cluster_labels, Variabile = train$distress.x)
print(tab)
prop.table(tab, margin = 1)
chisq.test(tab)

cluster_1 <- train[cluster_labels == 1, ]
cluster_2 <- quant[cluster_labels == 2, ]

set.seed(42)
quant_c1 <- quant[cluster_labels == 2, ]
distress_c1 <- train$distress.x[cluster_labels == 2]
cluster_labels2 <- kmeans(quant_c1, centers = 2, nstart = 50)$cluster
tab2 <- table(Cluster = cluster_labels2, Variabile = distress_c1)
print(tab2)
prop.table(tab2, margin = 1)
chisq.test(tab2)

normalise_by_ta <- function(df, year_tag, ta_col, prefix = "") {
  pattern <- paste0("^", prefix, ".*migl USD ", year_tag)
  target_cols <- grep(pattern, names(df), value = TRUE)
  
  ta <- df[[ta_col]]
  ta[ta == 0 | is.na(ta)] <- NA
  
  df[target_cols] <- lapply(df[target_cols], function(x) x / ta)
  return(df)
}

train_norm <- train
train_norm <- normalise_by_ta(train_norm, "Ultimo anno disp.", "Totale Attivo migl USD Ultimo anno disp.", prefix = "")
train_norm <- normalise_by_ta(train_norm, "Anno - 1",          "Totale Attivo migl USD Anno - 1",          prefix = "")
train_norm <- normalise_by_ta(train_norm, "Anno - 2",          "Totale Attivo migl USD Anno - 2",          prefix = "")
train_norm <- normalise_by_ta(train_norm, "Anno - 3",          "Totale Attivo migl USD Anno - 3",          prefix = "")
train_norm <- normalise_by_ta(train_norm, "Ultimo anno disp.", "Totale Attivo migl USD Ultimo anno disp.", prefix = "sub_T1_")
train_norm <- normalise_by_ta(train_norm, "Anno - 1",          "Totale Attivo migl USD Anno - 1",          prefix = "sub_T2_")
train_norm <- normalise_by_ta(train_norm, "Anno - 2",          "Totale Attivo migl USD Anno - 2",          prefix = "sub_T3_")
train_norm <- train_norm[, !grepl("^Totale Attivo migl USD", names(train_norm))]
train_norm <- train_norm[, !grepl("^sub_T._Totale Attivo migl USD", names(train_norm))]

quant <- scale(train_norm[,12:201])
cluster_labels <- kmeans(quant, centers = 2, nstart = 50)$cluster
tab <- table(Cluster = cluster_labels, Variabile = train$distress.x)
print(tab)
prop.table(tab, margin = 1)
chisq.test(tab)
train[cluster_labels==2,]$`Classificazione per dimensione`
quant_c1 <- quant[cluster_labels == 1, ]
distress_c1 <- train$distress.x[cluster_labels == 1]
cluster_labels2 <- kmeans(quant_c1, centers = 2, nstart = 50)$cluster
tab2 <- table(Cluster = cluster_labels2, Variabile = distress_c1)
print(tab2)
prop.table(tab2, margin = 1)
chisq.test(tab2)



### LASSO
library(glmnet)
library(caret)
library(ggplot2)
library(corrplot)
library(mvtnorm)
library(pROC)

X <- data.matrix(train[-(1:6)])
Y <- data.matrix(train[[4]])
X <- scale(X)

lasso1 <- glmnet(X,Y, alpha = 1, family = 'binomial')
cv_lasso1 <- cv.glmnet(X,Y,alpha=1, family= 'binomial')

lbs_fun <- function(fit, offset_x=1) {
  L <- length(fit$lambda)
  x <- log(fit$lambda [L])+ offset_x
  y <- fit$beta[, L]
  labs <- names (y)
  text(x, y, labels=labs, cex=0.75)
}
plot(cv_lasso1)
cv_lasso1

plot(lasso1,xvar = 'lambda', label = T)
lbs_fun(lasso1)
abline(v=log(cv_lasso1$lambda.min), col = 'red', lty = 2)
abline(v=log(cv_lasso1$lambda.1se), col = 'blue', lty = 2)

coef(cv_lasso1, s = 'lambda.1se')

new_vars <- coef(cv_lasso1, s = 'lambda.1se')
new_vars <- row.names(new_vars)[which(as.matrix(new_vars) != 0)]
new_vars <- new_vars[new_vars != "(Intercept)"]
data_ridotto <- train %>% 
  dplyr::select(1:6, all_of(new_vars))


#------------------- prove + tabelline (check vari) --------------
lassine <- train %>% 
  dplyr::select(all_of(new_vars), distress.x, `Area geografica`, `Anno fiscale Ultimo anno disp.`)
logit <- glm(distress.x ~.,
             data = lassine,
             family = "binomial")
summary(logit)
prob_test <- predict(logit, newdata = test_set, type = "response")
pred_class <- ifelse(prob_test > 0.4, 1, 0)
pred_class <- factor(pred_class, levels = levels(as.factor(test_set$distress.x)))
real_class <- as.factor(test_set$distress.x)
results_lasso <- confusionMatrix(pred_class, real_class, positive = "1")
print(results_lasso)

#-----------------------------------------------

modello_lasso <- glm(distress.x ~ .,
                     data = data_ridotto,
                     family = "binomial")
summary(modello_lasso)
library(car)
vif_values <- vif(modello_lasso)
print(vif_values)

thresholds <- seq(0, 1, by = 0.01)

sens <- numeric(length(thresholds))
spec <- numeric(length(thresholds))
acc  <- numeric(length(thresholds))

real <- factor(test_set$distress.x, levels = c(0,1))

for(i in seq_along(thresholds)) {
  
  th <- thresholds[i]
  
  pred <- ifelse(prob_test >= th, 1, 0)
  pred <- factor(pred, levels = c(0,1))
  
  cm <- confusionMatrix(pred, real, positive = "1")
  
  sens[i] <- cm$byClass["Sensitivity"]
  spec[i] <- cm$byClass["Specificity"]
  acc[i]  <- cm$overall["Accuracy"]
}

threshold_table <- data.frame(
  threshold = thresholds,
  sensitivity = sens,
  specificity = spec,
  accuracy = acc
)

threshold_table$youden <- 
  threshold_table$sensitivity +
  threshold_table$specificity - 1

which.max(threshold_table$youden)
print(threshold_table)


test_set <- test%>%dplyr::select(-c(1:2,4:6))
test_set <- test_set[-3]
test_set <- test_set[-c(203:205, 207:218)]
prob_test <- predict(modello_lasso, newdata = test_set, type = "response")
pred_class <- ifelse(prob_test > 0.4, 1, 0)
pred_class <- factor(pred_class, levels = levels(as.factor(test_set$distress.x)))
real_class <- as.factor(test_set$distress.x)
results_lasso <- confusionMatrix(pred_class, real_class, positive = "1")
print(results_lasso)


modello_logistico <- glm(distress.x ~ ., 
                         data = train, 
                         family = "binomial")


prob_test <- predict(modello_lasso, newdata = test_set, type = "response")
pred_class <- ifelse(prob_test > 0.4, 1, 0)
pred_class <- factor(pred_class, levels = levels(as.factor(test_set$distress.x)))
real_class <- as.factor(test_set$distress.x)
results_tutto <- confusionMatrix(pred_class, real_class, positive = "1")
print(results_tutto)


roc_obj <- roc(real_class, prob_test)
coords <- as.data.frame(coords(roc_obj,
                               seq(0,1,0.05),
                               ret = c("threshold","sensitivity","specificity")))

coords$youden <- coords$sensitivity +
  coords$specificity - 1


coords[which.max(coords$youden), ]

plot(roc_obj,
     col = "blue",
     lwd = 2,
     main = paste("ROC Curve - AUC =", 
                  round(auc(roc_obj), 3)))

abline(a = 0, b = 1, lty = 2)


# Qui facciamo che è sana solo se è in zona sana di altman
zona_altman <- ifelse(test$zona_altman == 'Sana', 0, 1)
altman <- as.data.frame(zona_altman)
altman$zona_altman <- factor(as.character(altman$zona_altman), levels = c("0", "1"))
altman$distress.x <- factor(as.character(test$distress.x), levels = c("0", "1"))
results_altman <- confusionMatrix(altman$zona_altman, altman$distress.x, positive = "1")
print(results_altman)

# Qui facciamo che è distressed solo se è in zona distress di altman
zona_altman <- ifelse(test$zona_altman == 'Distress', 1, 0)
altman <- as.data.frame(zona_altman)
altman$zona_altman <- factor(as.character(altman$zona_altman), levels = c("0", "1"))
altman$distress.x <- factor(as.character(test$distress.x), levels = c("0", "1"))
results_altman <- confusionMatrix(altman$zona_altman, altman$distress.x, positive = "1")
print(results_altman)

# Ora togliamo le grigie
zona_altman <- test%>%filter(zona_altman!='Grigia')
zona_altman <- zona_altman%>%select(zona_altman,distress.x)
zona_altman$zona_altman <- ifelse(zona_altman$zona_altman == 'Distress', 1, 0)
altman <- as.data.frame(zona_altman)
altman$zona_altman <- factor(as.character(altman$zona_altman), levels = c("0", "1"))
altman$distress.x <- factor(as.character(altman$distress.x), levels = c("0", "1"))
results_altman <- confusionMatrix(altman$zona_altman, altman$distress.x, positive = "1")
print(results_altman)



var_1 <- "Coefficiente di solvibilità (sulla base del patrimonio) Anno - 2"
var_2 <- "Redditività del totale Attivo (ROA) - Lordo Ultimo anno disp."

data_plot <- data_ridotto %>%
  dplyr::select(distress.x, var_1, var_2) %>%
  rename(
    distress = distress.x,
    Solvency = var_1,
    ROA = var_2
  )

partimat(distress ~ Solvency + ROA, 
         data = data_plot, 
         method = "qda", 
         main = "Partition Plot",
         image.colors = c("honeydew", "lightpink"))

datapc <- data_ridotto %>% 
  dplyr::select(all_of(new_vars))
pca_lasso <- prcomp(datapc, scale. = TRUE)
data_ridotto <- data_ridotto %>% 
  na.omit()
df_plot_pca <- data.frame(pca_lasso$x[, 1:2], distress = data_ridotto$distress.x)

partimat(as.factor(distress) ~ PC1 + PC2, data = df_plot_pca, method = "qda")

### RANDOM FOREST
library(randomForest)

colonne_rf <- names(train)

train_rf <- train %>%
  dplyr::select(all_of(colonne_rf)) %>%
  na.omit() %>%
  mutate(across(where(is.character), as.factor))

test_rf <- test %>%
  dplyr::select(all_of(colonne_rf)) %>%
  na.omit() %>%
  mutate(across(where(is.character), as.factor))

names(train_rf) <- make.names(names(train_rf))
names(test_rf)  <- make.names(names(test_rf))

target_var <- make.names("distress.x")

train_rf2 <- train %>%
  dplyr::select(all_of(colonne_rf)) %>%
  dplyr::select(-`Anno fiscale Ultimo anno disp.`) %>% 
  na.omit() %>%
  mutate(across(where(is.character), as.factor))

test_rf2 <- test %>%
  dplyr::select(all_of(colonne_rf)) %>%
  dplyr::select(-`Anno fiscale Ultimo anno disp.`) %>%
  na.omit() %>%
  mutate(across(where(is.character), as.factor))

names(train_rf2) <- make.names(names(train_rf2))
names(test_rf2)  <- make.names(names(test_rf2))


set.seed(123)

rf_model <- randomForest(
  distress.x ~ .,
  data = train_rf,
  ntree = 500,
  mtry = 7, #empirical rule 
  importance = TRUE
)

print(rf_model)
plot(rf_model)

rf_model2 <- randomForest(
  distress.x ~ .,
  data = train_rf2,
  ntree = 500,
  mtry = 7, #empirical rule 
  importance = TRUE
)

print(rf_model2)
plot(rf_model2)

# -----------------------------------------------------------------------------
# FEATURE IMPORTANCE (Mean Decrease Accuracy)
# -----------------------------------------------------------------------------
clean_names <- c(
  "Anno.fiscale.Ultimo.anno.disp." = "Fiscal Year (T0)",
  "Area.geografica" = "Geographic Area",
  "Coefficiente.di.solvibilità..sulla.base.del.patrimonio..Anno...2" = "Solvency Ratio (T-2)",
  "Utile.Perdita.prima.delle.imposte.migl.USD.Ultimo.anno.disp." = "EBT [kUSD] (T0)",
  "sub_T1_Redditività.del.capitale.proprio..ROE....Lordo.Ultimo.anno.disp." = "ROE Gross [sub_T1] (T0)",
  "sub_T1_Coefficiente.di.solvibilità..sulla.base.del.patrimonio..Ultimo.anno.disp." = "Solvency Ratio [sub_T1] (T0)",
  "Redditività.del.totale.Attivo..ROA....Lordo.Ultimo.anno.disp." = "ROA Gross (T0)",
  "Immobilizzazioni.materiali.migl.USD.Anno...1" = "Tangible Assets [kUSD] (T-1)",
  "Redditività.del.capitale.proprio..ROE....Lordo.Ultimo.anno.disp." = "ROE Gross (T0)",
  "Margine.EBIT.Anno...2" = "EBIT Margin (T-2)",
  "Margine.EBIT.Ultimo.anno.disp." = "EBIT Margin (T0)",
  "sub_T1_Attività.correnti.migl.USD.Ultimo.anno.disp." = "Current Assets [kUSD] [sub_T1] (T0)",
  "sub_T2_Redditività.del.capitale.proprio..ROE....Lordo.Anno...1" = "ROE Gross [sub_T2] (T-1)",
  "Redditività.del.totale.Attivo..ROA....Lordo.Anno...3" = "ROA Gross (T-3)",
  "Ricavi.vendite.e.prestazioni.migl.USD.Anno...2" = "Sales Revenues [kUSD] (T-2)",
  
  # Nuove variabili dal Modello 2
  "Indice.di.leva..gearing..Anno...1" = "Gearing Ratio (T-1)",
  "sub_T2_Indice.di.leva..gearing..Anno...1" = "Gearing Ratio [sub_T2] (T-1)",
  "Proventi.oneri.finanziari.migl.USD.Ultimo.anno.disp." = "Net Financial Income/Exp. [kUSD] (T0)"
)
imp_data <- as.data.frame(importance(rf_model))

imp_df <- data.frame(
  
  Variabile = rownames(imp_data),
  
  Importanza = imp_data$MeanDecreaseAccuracy
  
) %>%
  
  arrange(desc(Importanza)) %>%
  
  slice_head(n = 15) # Top 15 

imp_df <- imp_df %>%
  
  mutate(Variabile = ifelse(Variabile %in% names(clean_names), clean_names[Variabile], Variabile))
ggplot(imp_df, aes(x = reorder(Variabile, Importanza), y = Importanza)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Feature Importance - Top 15 (Random Forest M1)",
    subtitle = "Metric: Mean Decrease Accuracy (With Fiscal Year)",
    x = "Variables",
    y = "Importance"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.major.y = element_blank()
  )

imp_data2 <- as.data.frame(importance(rf_model2))
imp_df2 <- data.frame(
  Variabile = rownames(imp_data2),
  Importanza = imp_data2$MeanDecreaseAccuracy
) %>%
  arrange(desc(Importanza)) %>%
  slice_head(n = 15) # Top 15 
imp_df2 <- imp_df2 %>%
  mutate(Variabile = ifelse(Variabile %in% names(clean_names), clean_names[Variabile], Variabile))

ggplot(imp_df2, aes(x = reorder(Variabile, Importanza), y = Importanza)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Feature Importance - Top 15 (Random Forest)",
    subtitle = "Metric: Mean Decrease Accuracy (Pre-LASSO Pool)",
    x = "Variables",
    y = "Importance"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.major.y = element_blank()
  )
#-----------------------------------------------
prob_rf <- predict(rf_model, newdata = test_rf, type = "prob")[, "1"]

thresholds <- seq(0, 1, by = 0.01)
sens <- numeric(length(thresholds))
spec <- numeric(length(thresholds))
acc  <- numeric(length(thresholds))
real <- factor(test_rf[[target_var]], levels = c("0", "1"))

for(i in seq_along(thresholds)) {
  th <- thresholds[i]
  pred <- factor(ifelse(prob_rf >= th, "1", "0"), levels = c("0", "1"))
  cm <- confusionMatrix(pred, real, positive = "1")
  
  sens[i] <- cm$byClass["Sensitivity"]
  spec[i] <- cm$byClass["Specificity"]
  acc[i]  <- cm$overall["Accuracy"]
}

threshold_table <- data.frame(
  threshold = thresholds,
  sensitivity = sens,
  specificity = spec,
  accuracy = acc,
  youden = sens + spec - 1
)

best_th_idx <- which.max(threshold_table$youden)
best_threshold <- threshold_table$threshold[best_th_idx]


pred_rf_opt <- factor(ifelse(prob_rf >= best_threshold, "1", "0"), levels = c("0", "1"))

results_rf <- confusionMatrix(pred_rf_opt, real, positive = "1")
print(results_rf)

cm_rf_df <- as.data.frame(results_rf$table)
colnames(cm_rf_df) <- c("Predicted", "Reference", "Frequency")

ggplot(cm_rf_df, aes(x = Reference, y = Predicted, fill = Frequency)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Frequency), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "grey95", high = "steelblue") +
  labs(
    title = "Random Forest - Confusion Matrix (Optimal Threshold)",
    x = "Actual Class (Reference)",
    y = "Predicted Class"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text = element_text(face = "bold")
  )


roc_rf <- roc(as.factor(test_rf[[target_var]]), prob_rf)

plot(roc_rf, col = "darkgreen", lwd = 2,
     main = paste("Random Forest ROC - AUC =", round(auc(roc_rf), 3)))
abline(a = 0, b = 1, lty = 2)


prob_rf2 <- predict(rf_model2, newdata = test_rf2, type = "prob")[, "1"]


real2 <- factor(test_rf2[[target_var]], levels = c("0", "1"))

for(i in seq_along(thresholds)) {
  th <- thresholds[i]
  pred <- factor(ifelse(prob_rf2 >= th, "1", "0"), levels = c("0", "1"))
  cm <- confusionMatrix(pred, real2, positive = "1")
  
  sens[i] <- cm$byClass["Sensitivity"]
  spec[i] <- cm$byClass["Specificity"]
  acc[i]  <- cm$overall["Accuracy"]
}

threshold_table <- data.frame(
  threshold = thresholds,
  sensitivity = sens,
  specificity = spec,
  accuracy = acc,
  youden = sens + spec - 1
)

best_th_idx <- which.max(threshold_table$youden)
best_threshold <- threshold_table$threshold[best_th_idx]


pred_rf_opt2 <- factor(ifelse(prob_rf2 >= best_threshold, "1", "0"), levels = c("0", "1"))

results_rf2 <- confusionMatrix(pred_rf_opt2, real2, positive = "1")
print(results_rf2)

prob_rf1 <- predict(rf_model,  newdata = test_rf,  type = "prob")[, "1"]
prob_rf2 <- predict(rf_model2, newdata = test_rf2, type = "prob")[, "1"]

roc_rf1 <- roc(test_rf$distress.x, prob_rf1)
roc_rf2 <- roc(test_rf2$distress.x, prob_rf2)


plot(roc_rf1, col = "darkred", lwd = 2, main = "ROC Comparison: With vs Without Fiscal Year")
plot(roc_rf2, col = "darkblue", lwd = 2, add = TRUE)
legend("bottomright", legend = c(paste("With Year (AUC =", round(auc(roc_rf1), 3), ")"),
                                 paste("Without Year (AUC =", round(auc(roc_rf2), 3), ")")),
       col = c("darkred", "darkblue"), lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")

