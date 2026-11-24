# ============================================================
# ANALYSE DE LA BASE - ÉTUDE DE CAS SCORING
# ============================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(pROC)

# ============================================================
# 1. IMPORTATION DE LA BASE
# ============================================================

base <- read_excel(
  "C:\\Users\\ibrah\\Documents\\Scoring\\base2.xlsx",
  col_types = c(
    "text",     # CalYear
    "text",     # Gender
    "text",     # Type
    "text",     # Category
    "text",     # Occupation
    "numeric",  # Age
    "numeric",  # Group1
    "numeric",  # Bonus
    "numeric",  # Poldur
    "numeric",  # Value
    "numeric",  # Adind
    "text",     # Group2
    "numeric",  # Density
    "text"      # Surv1
  )
)

# Vérification de la dimension
dim(base)

# Noms des variables
names(base)

# Premières observations
head(base)


# ============================================================
# 2. VÉRIFICATION DES VALEURS MANQUANTES
# ============================================================

colSums(is.na(base))


# ============================================================
# 3. VÉRIFICATION DES DOUBLONS
# ============================================================

# Nombre de doublons
sum(duplicated(base))

# Visualisation des doublons
doublons <- base[
  duplicated(base) | duplicated(base, fromLast = TRUE),
]

# View(doublons)  # désactivé en exécution non interactive


# ============================================================
# 4. SUPPRESSION DES DOUBLONS
# ============================================================

base <- base %>%
  distinct()

# Vérification
sum(duplicated(base))

# Nouvelle dimension
dim(base)


# ============================================================
# 5. VÉRIFICATION DES MODALITÉS
# ============================================================

table(base$Gender)
table(base$Type)
table(base$Category)
table(base$Occupation)
table(base$Adind)
table(base$Group2)
table(base$Surv1, useNA = "always")


# ============================================================
# 6. NETTOYAGE DES VARIABLES
# ============================================================

# Harmonisation de Gender
base <- base %>%
  mutate(
    Gender = case_when(
      Gender == "M" ~ "Male",
      Gender == "F" ~ "Female",
      TRUE ~ Gender
    )
  )

# Harmonisation de Surv1
base <- base %>%
  mutate(
    Surv1 = case_when(
      Surv1 == "yes" ~ 1,
      Surv1 == "no" ~ 0,
      Surv1 == "1" ~ 1,
      Surv1 == "0" ~ 0,
      TRUE ~ NA_real_
    )
  )

# Vérification de Surv1
table(base$Surv1, useNA = "always")


# ============================================================
# 7. CORRECTION DES ERREURS DE SAISIE DE AGE
# ============================================================

# Vérification des valeurs extrêmes
base %>%
  filter(Age < 18 | Age > 80)

# Correction des 8 erreurs de saisie identifiées
base <- base %>%
  mutate(
    Age = ifelse(
      Age > 80,
      floor(Age / 10),
      Age
    )
  )

# Vérification après correction
base %>%
  filter(Age < 18 | Age > 80)

# Résumé de Age
summary(base$Age)


# ============================================================
# 8. VÉRIFICATION FINALE DES VALEURS MANQUANTES
# ============================================================

colSums(is.na(base))


# ============================================================
# 9. VÉRIFICATION DES VARIABLES QUANTITATIVES
# ============================================================

summary(
  base[, c(
    "Age",
    "Bonus",
    "Poldur",
    "Value",
    "Density"
  )]
)

# Vérification des bornes du Bonus
range(base$Bonus, na.rm = TRUE)

# Vérification de Poldur
range(base$Poldur, na.rm = TRUE)

# Vérification de Value
range(base$Value, na.rm = TRUE)

# Vérification de Density
range(base$Density, na.rm = TRUE)


# ============================================================
# 10. VÉRIFICATION DE LA VARIABLE Adind
# ============================================================

table(base$Adind, useNA = "always")

range(base$Adind, na.rm = TRUE)


# ============================================================
# 11. VÉRIFICATION DE Group2
# ============================================================

table(base$Group2, useNA = "always")


# ============================================================
# 12. Group1
# ============================================================
# Group1 n'est pas utilisé dans l'analyse car sa définition
# n'est pas clairement indiquée dans le dictionnaire fourni.
#
# On le conserve dans la base originale mais on ne l'utilise
# pas dans les statistiques descriptives ni l'analyse bivariée.


# ============================================================
# 13. VÉRIFICATION FINALE DE LA BASE
# ============================================================

str(base)

summary(base)

nrow(base)

# ============================================================
# 14. CRÉATION DES ÉCHANTILLONS APPRENTISSAGE / TEST
# ============================================================

# Séparation stratifiée selon Surv1
# afin de conserver une proportion similaire d'accidents
# dans les deux échantillons.

set.seed(123)

idx_train <- createDataPartition(
  y = base$Surv1,
  p = 0.70,
  list = FALSE
)

train <- base[idx_train, ]
test <- base[-idx_train, ]

# ============================================================
# 15. VÉRIFICATION DES ÉCHANTILLONS
# ============================================================

cat(
  "Nombre d'observations dans l'échantillon complet :",
  nrow(base), "\n"
)

cat(
  "Nombre d'observations dans l'échantillon d'apprentissage :",
  nrow(train), "\n"
)

cat(
  "Nombre d'observations dans l'échantillon test :",
  nrow(test), "\n"
)


# Vérification de la proportion de Surv1

cat("\nProportion de Surv1 - Base complète :\n")
print(
  round(
    prop.table(table(base$Surv1)) * 100,
    2
  )
)

cat("\nProportion de Surv1 - Échantillon d'apprentissage :\n")
print(
  round(
    prop.table(table(train$Surv1)) * 100,
    2
  )
)

cat("\nProportion de Surv1 - Échantillon test :\n")
print(
  round(
    prop.table(table(test$Surv1)) * 100,
    2
  )
)


# ============================================================
# 16. STATISTIQUES DESCRIPTIVES
# Échantillon d'apprentissage
# ============================================================

str(train)

summary(train)


# ============================================================
# 17. VARIABLES QUANTITATIVES
# ============================================================

vars_quanti <- c(
  "Age",
  "Bonus",
  "Poldur",
  "Value",
  "Density"
)


# Statistiques descriptives
stats_quanti <- data.frame(
  
  Variable = vars_quanti,
  
  Moyenne = sapply(
    train[vars_quanti],
    mean,
    na.rm = TRUE
  ),
  
  Ecart_type = sapply(
    train[vars_quanti],
    sd,
    na.rm = TRUE
  ),
  
  Minimum = sapply(
    train[vars_quanti],
    min,
    na.rm = TRUE
  ),
  
  Q1 = sapply(
    train[vars_quanti],
    quantile,
    probs = 0.25,
    na.rm = TRUE
  ),
  
  Mediane = sapply(
    train[vars_quanti],
    median,
    na.rm = TRUE
  ),
  
  Q3 = sapply(
    train[vars_quanti],
    quantile,
    probs = 0.75,
    na.rm = TRUE
  ),
  
  Maximum = sapply(
    train[vars_quanti],
    max,
    na.rm = TRUE
  )
)

stats_quanti


# ============================================================
# 18. VARIABLES QUALITATIVES
# ============================================================

vars_quali <- c(
  "Gender",
  "Type",
  "Category",
  "Occupation",
  "Adind",
  "Group2",
  "CalYear"
)


# Affichage des effectifs et pourcentages
for (v in vars_quali) {
  
  cat("\n====================================\n")
  cat("Variable :", v, "\n")
  cat("====================================\n")
  
  print(table(train[[v]], useNA = "always"))
  
  print(
    round(
      prop.table(table(train[[v]])) * 100,
      2
    )
  )
}


# ============================================================
# 19. VARIABLE À EXPLIQUER : SURV1
# ============================================================

# Effectifs
table(train$Surv1)

# Pourcentages
round(
  prop.table(table(train$Surv1)) * 100,
  2
)

# Taux de sinistre
taux_sinistre <- mean(
  train$Surv1,
  na.rm = TRUE
) * 100

cat(
  "Taux de sinistre dans l'échantillon d'apprentissage :",
  round(taux_sinistre, 2),
  "%\n"
)


# ============================================================
# 20. GRAPHIQUES DES VARIABLES QUANTITATIVES
# ============================================================

# Age
ggplot(train, aes(x = Age)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution de l'âge des conducteurs",
    x = "Âge",
    y = "Nombre d'assurés"
  ) +
  theme_minimal()


# Bonus
ggplot(train, aes(x = Bonus)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution du coefficient bonus-malus",
    x = "Bonus-malus",
    y = "Nombre d'assurés"
  ) +
  theme_minimal()


# Poldur
ggplot(train, aes(x = Poldur)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution de l'ancienneté du contrat",
    x = "Ancienneté du contrat",
    y = "Nombre d'assurés"
  ) +
  theme_minimal()


# Value
ggplot(train, aes(x = Value)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution de la valeur du véhicule",
    x = "Valeur du véhicule",
    y = "Nombre d'assurés"
  ) +
  theme_minimal()


# Density
ggplot(train, aes(x = Density)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution de la densité de population",
    x = "Densité de population",
    y = "Nombre d'assurés"
  ) +
  theme_minimal()


# ============================================================
# 21. GRAPHIQUE DE SURV1
# ============================================================

ggplot(train, aes(x = factor(Surv1))) +
  geom_bar() +
  labs(
    title = "Répartition de la survenue d'un accident responsable",
    x = "Survenue d'un accident",
    y = "Nombre d'assurés"
  ) +
  theme_minimal()


# ============================================================
# 22. GRAPHIQUES DES VARIABLES QUALITATIVES
# ============================================================

# Gender
ggplot(train, aes(x = Gender)) +
  geom_bar() +
  labs(
    title = "Répartition des conducteurs selon le genre",
    x = "Genre",
    y = "Nombre d'assurés"
  ) +
  theme_minimal()


# Type
ggplot(train, aes(x = Type)) +
  geom_bar() +
  labs(
    title = "Répartition selon le type de véhicule",
    x = "Type de véhicule",
    y = "Nombre d'assurés"
  ) +
  theme_minimal()


# Category
ggplot(train, aes(x = Category)) +
  geom_bar() +
  labs(
    title = "Répartition selon la catégorie du véhicule",
    x = "Catégorie",
    y = "Nombre d'assurés"
  ) +
  theme_minimal()


# Occupation
ggplot(train, aes(x = Occupation)) +
  geom_bar() +
  labs(
    title = "Répartition selon la profession",
    x = "Profession",
    y = "Nombre d'assurés"
  ) +
  theme_minimal()


# ============================================================
# 23. VÉRIFICATION FINALE DE L'ÉCHANTILLON D'APPRENTISSAGE
# ============================================================

nrow(train)

colSums(is.na(train))

table(train$Surv1)

round(
  prop.table(table(train$Surv1)) * 100,
  2
)


# ============================================================
# 24. ANALYSE BIVARIÉE
# SURV1 ET VARIABLES QUANTITATIVES
# ============================================================

resultats_quanti <- data.frame()

for (v in vars_quanti) {

  # Moyennes
  moyennes <- tapply(
    train[[v]],
    train$Surv1,
    mean,
    na.rm = TRUE
  )

  # Médianes
  medianes <- tapply(
    train[[v]],
    train$Surv1,
    median,
    na.rm = TRUE
  )

  # Test de Student classique (comparaison de deux moyennes sous
  # l'hypothèse de variances égales entre les deux groupes Surv1=0
  # et Surv1=1). Par défaut, t.test() applique le test de Welch
  # (var.equal = FALSE), qui ne suppose pas l'égalité des variances.
  # On impose ici var.equal = TRUE pour revenir au test de Student
  # "classique" (test bilatéral à variance commune).
  test <- t.test(
    train[[v]] ~ train$Surv1,
    var.equal = TRUE
  )

  # Ajout des résultats
  resultats_quanti <- rbind(
    resultats_quanti,
    data.frame(
      Variable = v,
      Moyenne_Surv0 = moyennes["0"],
      Moyenne_Surv1 = moyennes["1"],
      Mediane_Surv0 = medianes["0"],
      Mediane_Surv1 = medianes["1"],
      Stat_t = as.numeric(test$statistic),
      Ddl = as.numeric(test$parameter),
      P_value = test$p.value
    )
  )
}

resultats_quanti


# ============================================================
# 25. V DE CRAMER
# ============================================================

cramer_v <- function(tab) {
  
  test <- suppressWarnings(
    chisq.test(tab)
  )
  
  chi2 <- as.numeric(test$statistic)
  n <- sum(tab)
  
  k <- min(
    nrow(tab),
    ncol(tab)
  )
  
  sqrt(
    chi2 / (n * (k - 1))
  )
}


# ============================================================
# 26. ANALYSE BIVARIÉE
# SURV1 ET VARIABLES QUALITATIVES
# ============================================================

resultats_quali <- data.frame()

for (v in vars_quali) {
  
  cat("\n====================================\n")
  cat("Variable :", v, "\n")
  cat("====================================\n")
  
  # Tableau de contingence
  tab <- table(
    train[[v]],
    train$Surv1
  )
  
  print(tab)
  
  # Pourcentages par modalité
  pourcentages <- round(
    prop.table(tab, margin = 1) * 100,
    2
  )
  
  print(pourcentages)
  
  # Test du Chi-2
  test <- suppressWarnings(
    chisq.test(tab)
  )
  
  print(test)
  
  # Vérification des effectifs théoriques
  if (any(test$expected < 5)) {
    
    cat(
      "ATTENTION : certains effectifs théoriques sont inférieurs à 5.\n"
    )
    
  }
  
  # V de Cramer
  v_cramer <- cramer_v(tab)
  
  cat(
    "V de Cramer :",
    round(v_cramer, 4),
    "\n"
  )
  
  # Résultats récapitulatifs
  resultats_quali <- rbind(
    resultats_quali,
    data.frame(
      Variable = v,
      Chi2 = as.numeric(test$statistic),
      P_value = test$p.value,
      V_Cramer = v_cramer
    )
  )
}


# ============================================================
# 27. TABLEAU RÉCAPITULATIF - VARIABLES QUALITATIVES
# ============================================================

resultats_quali


# ============================================================
# 27 bis. DÉTAIL PAR MODALITÉ (toutes les modalités, pas
# seulement les deux modalités extrêmes) - sert à construire le
# tableau 3.2 complet du rapport (taux de sinistre par modalité,
# avec le Chi² et le V de Cramer rappelés une fois par variable)
# ============================================================

detail_quali <- data.frame()

for (v in vars_quali) {

  tab <- table(train[[v]], train$Surv1)
  pct <- round(prop.table(tab, margin = 1) * 100, 2)
  eff <- rowSums(tab)

  ligne_var <- resultats_quali[resultats_quali$Variable == v, ]

  detail_v <- data.frame(
    Variable = v,
    Modalite = rownames(tab),
    Effectif = as.integer(eff),
    Taux_sinistre = pct[, "1"],
    Chi2 = c(round(ligne_var$Chi2, 2), rep(NA, nrow(tab) - 1)),
    P_value = c(ligne_var$P_value, rep(NA, nrow(tab) - 1)),
    V_Cramer = c(round(ligne_var$V_Cramer, 4), rep(NA, nrow(tab) - 1))
  )

  detail_quali <- rbind(detail_quali, detail_v)
}

print(detail_quali, row.names = FALSE)


# ============================================================
# 28. TABLEAUX DE RÉSULTATS
# ============================================================

# Variables quantitatives
print(resultats_quanti)

# Variables qualitatives
print(resultats_quali)

# ============================================================
# 30. PRÉPARATION DES VARIABLES POUR LA MODÉLISATION
# ============================================================

set.seed(123)

idx_train <- createDataPartition(
  y = base$Surv1,
  p = 0.70,
  list = FALSE
)

train <- base[idx_train, ]
test <- base[-idx_train, ]

# ============================================================
# MODÈLE FINAL : LOGIT NON LINÉAIRE
# ============================================================

# Préparation des variables qualitatives
train_model <- train %>%
  mutate(
    Gender = factor(Gender),
    Type = factor(Type),
    Category = factor(Category),
    Occupation = factor(Occupation),
    Adind = factor(Adind),
    Group2 = factor(Group2)
  )

test_model <- test %>%
  mutate(
    Gender = factor(Gender, levels = levels(train_model$Gender)),
    Type = factor(Type, levels = levels(train_model$Type)),
    Category = factor(Category, levels = levels(train_model$Category)),
    Occupation = factor(
      Occupation,
      levels = levels(train_model$Occupation)
    ),
    Adind = factor(Adind, levels = levels(train_model$Adind)),
    Group2 = factor(Group2, levels = levels(train_model$Group2))
  )


# ============================================================
# 1. ESTIMATION DU MODÈLE FINAL
# ============================================================

modele_final <- glm(
  Surv1 ~
    Gender +
    Type +
    Category +
    Occupation +
    poly(Age, 2, raw = TRUE) +
    poly(Bonus, 2, raw = TRUE) +
    poly(Poldur, 2, raw = TRUE) +
    poly(Value, 2, raw = TRUE) +
    Adind +
    Group2 +
    poly(Density, 2, raw = TRUE),
  data = train_model,
  family = binomial(link = "logit")
)

summary(modele_final)


# ============================================================
# 2. PROBABILITÉS PRÉDITES SUR LE TRAIN
# ============================================================

train_model$proba <- predict(
  modele_final,
  newdata = train_model,
  type = "response"
)


# ============================================================
# 3. CHOIX DU SEUIL SUR LE TRAIN
# ============================================================

roc_train <- roc(
  train_model$Surv1,
  train_model$proba,
  quiet = TRUE
)

seuil_optimal <- coords(
  roc_train,
  x = "best",
  best.method = "youden",
  ret = "threshold"
)

seuil <- as.numeric(seuil_optimal)

cat("Seuil optimal :", round(seuil, 4), "\n")

cat(
  "AUC train :",
  round(as.numeric(auc(roc_train)), 4),
  "\n"
)


# ============================================================
# 4. VALIDATION SUR L'ÉCHANTILLON TEST
# ============================================================

test_model$proba <- predict(
  modele_final,
  newdata = test_model,
  type = "response"
)

# AUC sur le test
roc_test <- roc(
  test_model$Surv1,
  test_model$proba,
  quiet = TRUE
)

auc_test <- auc(roc_test)

cat(
  "AUC test :",
  round(as.numeric(auc_test), 4),
  "\n"
)


# ============================================================
# 5. CLASSIFICATION SUR LE TEST
# ============================================================

test_model$prediction <- ifelse(
  test_model$proba >= seuil,
  1,
  0
)


# ============================================================
# 6. MATRICE DE CONFUSION
# ============================================================

matrice_test <- confusionMatrix(
  factor(
    test_model$prediction,
    levels = c(0, 1)
  ),
  factor(
    test_model$Surv1,
    levels = c(0, 1)
  ),
  positive = "1"
)

matrice_test


# ============================================================
# 7. COURBE ROC DU MODÈLE FINAL
# ============================================================

plot(
  roc_test,
  main = "Courbe ROC - Modèle Logit non linéaire - Test"
)
