# Exercice 1

  #1

df = read.csv(file = "L:/BUT/SD/Promo 2023/targoud/Semestre 2/R2.04  progra statistiques/velov.csv",  
              header = TRUE,
              sep = ";",      #importe le chemin d'accés
              dec = "," )

  #2

summary(df)         #fais un resumé des données
class(df$status)    
class(df$CodePostal)

  #3

df$status = as.factor(df$status)  #convertit en type factor
df$CodePostal = as.factor(df$CodePostal)

  #4

df$bornes = ifelse(df$capacity != (df$bikes + df$stands), "KO" , "OK") #verifie si il y a des bornes indisponibles sur les stations


#EXERCICE 2

  #1

hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations")  #histogramme de la capacité

  #2

hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations", #histogramme mais avec 6 classes
     breaks = 6)


  #3

hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",  #histogramme avec 6 classes mais cela met en couleur rouge
     breaks = 6,
     col = "red")

  #4

hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations", #meme histogramme que avant mais on met un nom capacité en absicce
     breaks = 6,
     col = "red",
     xlab = "capacité")

  #5

abline(h = 100, col = "blue", lty = 2) #ajoute une ligne horizontale avec une ligne bleu 


  #6

hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     col = "red",                 #construit le meme graphique que précedement mais avec la densité plutot que les effectifs, et supprime l'argument break
     probability = TRUE,
     xlab = "Capacity")

  #7

lines(density(df$capacity), # ici on ajoute une courbe de densité a l'histogramme
      lty = 2, #type de la courbe
      col = "blue",     
      lwd =4) #lwd chane la taille de la courbe

  #8

hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations", #meme graphique que precedemment mais avec les axes en y que on a modifier de 0 à 0.08
     col = "red",
     probability = TRUE,
     xlab = "Capacity",
     ylim = c(0,0.08)) #ici on reduit les axes de y, de 0 à 0.08

lines(density(df$capacity),
      lty = 2,
      col = "blue",
      lwd = 4)


#EXERCICE 3

  #1

boxplot(x =df$capacity, main = "boite a moustache") # créer une boite a moustache pour la capcité avec un titre

  #2

boxplot(x =df$capacity, main = "boite a moustache", #créer la meme boite a moustache mais horizontalement
        horizontal = TRUE)

  #3

boxplot(x =df$capacity, main = "boite a moustache", #boite a moustache vertical, en n'affichant pas les valeurs atypiques
        horizontal = FALSE,
        outline = FALSE)

  #4

points(mean(df$capacity), col = "red", pch = 15, cex= 2) # ajoute un carre rouge qui correspond a la moyenne de la série
                                                        #cex change la taille du carré, et pch change l'orientation de la figure

  #5

par(mfrow=c(1,2)) #fenêtre sur 1 ligne et 2 colonnes
#7ème
df7 = subset(df, CodePostal == "69007")
boxplot(x = df7$bikes, 
        main = "Boxplot nb vélos \n 69007",
        ylim = c(0,40))
#8ème
df8 = subset(df, CodePostal == "69008")
boxplot(x = df8$bikes, 
        main = "Boxplot nb vélos \n 69008",
        ylim = c(0,40))

  #6

par(mfrow=c(1,1)) #fenêtre sur 1 ligne et 1 colonne
# Tracer le graphique boxplot
boxplot(formula = bikes ~ bonus,
        data = df, 
        main = "Dispo vélos vs Stations Bonus")

  #7

# Calculer les moyennes de chaque groupe
means <- tapply(X = df$bikes, 
                INDEX = df$bonus, 
                FUN = function(X) mean(X))
print(means)
# Ajouter les moyennes de chaque groupe au graphique
points(means, col = "red", pch = 19)


#EXERCICE 4

  #1

effectif = table(df$bonus)
barplot(height = effectif,
        main = "repartition du nombre \ n stations")

  #2


barplot(height = effectif,                  #meme graphique que précedemment mais horizntalement
        main = "Répartition du nombre \n de station bonus",  
        horiz = TRUE)

  #3

frequence = prop.table(effectif)         #meme graphique mais en poucentage 
barplot(height = frequence,
        main = "Repartition en % du nombre \ n sations bonus",
        horiz = TRUE)

  #4

effectif = table(df$banking, df$bonus)
print(effectif)
barplot(height = effectif,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?")
#On remarque qu'on ne sait pas distinguer les deux modalités car il n'y a pas de légende.

  #5

#Calcul des pourcentages
frequence = prop.table(x = effectif)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)

  #6
