# Act-R_Jenga

## Pour tester le fonctionnement du jenga en tant qu'humain 
```
1 > (play-jenga)
```

## Pour faire jouer le modèle: 
### Parameters
1. Le nombre d'expériences distincte (les résultats finaux vont faire la moyenne entres les expériences)
2. (Optionelle) Le nombre d'ensembles d'essaie à faire 
3. (Optionelle) Le nombre d'essaie à faire par ensembles

```
1 > (jenga-model *nb_exeriments* *nb_sets* *nb_trials*)
```

Exemple de résultat avec seulement 1 expérience et pas vraiment
d'améliorations:
```
1 > (model-jenga 1 4 12)
1 experiment(s) of 4 set(s) of 12 trial(s)
#Set      #Block removed
1         7.00
2         12.50
3         8.75
4         12.33
```