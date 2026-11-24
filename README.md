# Projet de scoring : risque d'accident en assurance automobile

Projet de Master 2 Sciences des données (Université de Lille), réalisé en binôme avec Amadou Diop sous la direction de Yves Arrighi.

## Objectif

Construire une règle qui classe chaque assuré comme à risque ou non à risque d'accident responsable, à partir de ses caractéristiques et de celles de son véhicule.

## Données

71 196 assurés d'une compagnie d'assurance, années 2009 et 2010. Variables : âge, genre, profession, coefficient bonus-malus, ancienneté du contrat, type, catégorie et valeur du véhicule, garantie tous risques, région, densité de population. La variable cible vaut 1 si l'assuré a eu au moins un accident responsable dans l'année (13,3 % des assurés).

## Démarche

1. Nettoyage sous R : suppression de 14 doublons, recodage des modalités mal saisies (genre, variable cible), correction de 8 âges aberrants.
2. Découpage en échantillon d'apprentissage (70 %) et échantillon test (30 %).
3. Statistiques descriptives et tests bivariés (Student, Chi², V de Cramér).
4. Modèle logit linéaire puis non linéaire (termes quadratiques), comparés par AIC, BIC et AUC.
5. Choix du seuil de classement par l'indice de Youden, puis évaluation sur l'échantillon test.

## Résultats sur l'échantillon test

| Indicateur | Valeur |
|---|---|
| AUC | 0,754 |
| Sensibilité | 67,8 % |
| Spécificité | 70,7 % |
| Seuil retenu | 0,140 |

Le modèle détecte environ 68 % des assurés sinistrés. Le bonus-malus et l'âge sont les variables les plus discriminantes.

## Limites et pistes

La valeur prédictive positive reste faible (26 %), ce qui est attendu avec un événement rare. Des modèles plus flexibles (forêts aléatoires, boosting) et un seuil ajusté au coût des erreurs pour l'assureur pourraient améliorer la règle.

## Contenu du dépôt

- `scoring.R` : code complet (nettoyage, analyses, modélisation, évaluation)
- `rapport_scoring.pdf` : rapport du projet

Les données de l'assureur ne sont pas publiées.
