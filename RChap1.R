
################### Importation des données ###################


# 1)/2)  Téléchargement et importation des jeux de données avec setwd et read.csv


ancien=read.csv("C:/Users/Utilisateur/Documents/Cours BUT 2éme année/R studio/dpe-v2-logements-existants .csv",
             header=TRUE, dec=".",sep=",")
View(ancien)

neuf=read.csv("C:/Users/Utilisateur/Documents/Cours BUT 2éme année/R studio/dpe-v2-logements-existants .csv",
              header = TRUE , dec = ".", sep = ",")
View(neuf)


# 3) Affichiage des dimensions des deux datasets avec dim()

dim(ancien)
dim(neuf)


# 4) Création de la colonne Logement avec comme valeur "ancien" ou "neuf" selon la source concernée


ancien$Logement="ancien"
neuf$Logement="neuf"

# 5)  Ajout de la colonne Année_construction dans df2

neuf$Année_construction=2024


# 6) Fusion des deux dataframes en gardant les mêmes colonnes en utilisant d'abord intersect() pour récupérer les colonens communes, puis on transforme 
#    les colonnes des deux dataframes avec celles qu'elles ont en commun, pour enfin ajouter dans le df1 les lignes du df2 avec rbind()
#       
   

col_communes=intersect(colnames(ancien),colnames(neuf))

ancien=ancien[,col_communes]
neuf=neuf[,col_communes]

df=rbind(ancien,neuf) 

View(df)



# 7) Création de la colonne Date_réception_DPE en convertissant d'abord la colonne Date_réception_DPE en format date, puis on extrait l'année de cette même colonne dans une 
# nouvelle colonne Année


df$Date_réception_DPE=as.Date(df$Date_réception_DPE, format="%Y-%m-%d")
df$Année=format(df$Date_réception_DPE, "%Y")


# 8) Création d'une colonne qui vérifie si le coût total des cinq usages est vraie à l'aide de valeurs booléens, vérification avec une condition qui print "Somme fausée" si 
# au moins une ligne de la colonne crée précédement contient une valeur "FALSE" à l'aide de la fonction any()

df$verif_somme = df$Coût_total_5_usages == (df$Coût_chauffage + df$Coût_éclairage + df$Coût_ECS + df$Coût_refroidissement + df$Coût_auxiliaires)

if(any(df$verif_somme==FALSE)) {
  print("Somme faussée")
} else {
  print("Somme vérifiée")
}


# 9) Création d'une colonne contenant le coût du chauffage en %

df$Coût_chauffage_p=(df$Coût_chauffage / df$Coût_total_5_usages) * 100


# 10) Création de la colonne Periode_construction avec la fonction cut() permettant de classer les date de construction selon la période, en incluant la borne supérieure


df$Periode_construction <- cut(df$Année_construction,
                               breaks = c(-Inf, 1960, 1970, 1980, 1990, 2000, 2010, Inf),
                               labels = c("Avant 1960", "1961 - 1970", "1971 - 1980", 
                                          "1981 - 1990", "1991 - 2000", "2001 - 2010", "Après 2010"),
                               right = TRUE)






################### Statistiques générales ###################


# 1) Répartition des logements par Etiquette_DPE

table(df$Etiquette_DPE)

# 2) Répartition des logements par Date_réception_DPE

table(df$Date_réception_DPE)

# 3) Répartition des logements par type

table(df$Logement)

# 4) Répartition des bâtiments par type

table(df$Type_bâtiment)

# 5) Répartition des logements par type d'installation chauffage en pourcentage

prop.table(table(df$Type_installation_chauffage))

# 6) Répartition des logements par période de construction

table(df$Periode_construction)

# 7) Calcul de la surface habitable moyenne des logements

mean(df$Surface_habitable_logement, na.rm = TRUE)

# 8) Calcul moyenne du coût_chauffage

mean(df$Coût_chauffage, na.rm = TRUE)

# 9) Calcul des quartiles et des déciles du Coût_ECS

quantile(df$Coût_ECS, probs = seq(0, 1, 0.25), na.rm = TRUE)  
quantile(df$Coût_ECS, probs = seq(0, 1, 0.1), na.rm = TRUE)  

# 10) Répartition des logements par Etiquette_DPE

cor(df$Surface_habitable_logement, df$Coût_chauffage, use = "complete.obs")

# 12) Répartition des logements par Etiquette_DPE

install.packages("corrplot")
library(corrplot)


variables=df[, c("Coût_total_5_usages", "Coût_chauffage", "Coût_éclairage", "Coût_ECS", 
                    "Coût_refroidissement", "Coût_auxiliaires", "Surface_habitable_logement", "Emission_GES_5_usages")]

cor_matrix=cor(variables, use = "complete.obs")

corrplot(cor_matrix, method = "circle")







################### Manipulation des données ###################


# Installation du package
install.packages("dplyr")
library(dplyr)

# Filtre #


# 1)

df_apartment=df %>%
  filter(Type_bâtiment == "appartement")

# 2)

df_DPE=df %>%
  filter(Etiquette_DPE %in% c("D", "E", "F", "G"))

# 3)

df_ancien=df %>%
  filter(Periode_construction == "Avant 1960")

# 4)
moyenne_surface=mean(df$Surface_habitable_immeuble, na.rm = TRUE)

df_surface_superieure=df %>%
  filter(Surface_habitable_immeuble > moyenne_surface)

# 5)

df_energie_tri=df %>%
  arrange(desc(Conso_5_usages.m._é_finale))

# 6)

df_tri=df %>%
  arrange(Etiquette_DPE, Periode_construction, desc(Coût_chauffage))


# Agréagation #


# 1)

df_cout_chauffage=df %>%
  group_by(Etiquette_DPE) %>%
  summarise(moyenne_cout_chauffage = mean(Coût_chauffage, na.rm = TRUE))


# 2)

df_conso_par_periode=df %>%
  group_by(Periode_construction) %>%
  summarise(moyenne_conso_5_usages = mean(Conso_5_usages.m._é_finale, na.rm = TRUE))

# 3)

df_conso_par_type_et_dpe=df %>%
  group_by(Type_bâtiment, Etiquette_DPE) %>%
  summarise(moyenne_conso_5_usages = mean(Conso_5_usages.m._é_finale, na.rm = TRUE))


################### Création de graphique ###################


#Graphiques élémentaires#


# 1)

hist(df$Surface_habitable_logement, 
     main = "Distribution des surfaces habitables", 
     xlab = "Surface habitable (m^2)", 
     ylab = "Fréquence", 
     col = "red")




# 2)

barplot(table(df$Periode_construction), 
        main = "Nombre de logements par période de construction", 
        xlab = "Période de construction", 
        ylab = "Nombre de logements", 
        col = "red", 
        las = 2)

# 3)

pie(table(df$Type_énergie_n.1), 
    main = "Répartition du type d'énergie", 
    col = rainbow(length(unique(df$Type_énergie_n.1))))




#Régression linéaire#


# 1)

plot(df$Surface_habitable_logement, df$Coût_chauffage, 
     main = "Nuage de points : Surface habitable vs Coût du chauffage", 
     xlab = "Surface habitable (m^2)", 
     ylab = "Coût du chauffage (Euros)", 
     pch = 19, col = "blue")

# 2)

cor(df$Surface_habitable_logement, df$Coût_chauffage, use = "complete.obs")

# 3)

modele=lm(df$Coût_chauffage ~ df$Surface_habitable_logement, data = df)
summary(modele)

# 4) selon summary(modele) : y=ax+b <=> y=4.80125*x+250.04927 (Cout chauffage=4.80125*(Surface Logement en m^2)+250.04927), plus la surface habitable du logement est grande, plus le coût du chauffage augmentera d'environ 4.8 euros par m^2

# 5)

plot(df$Surface_habitable_logement, df$Coût_chauffage, 
     main = "Surface habitable vs Coût du chauffage avec droite de régression", 
     xlab = "Surface habitable (m^2)", 
     ylab = "Coût du chauffage (Euros)", 
     pch = 19, col = "blue")

abline(modele, col = "pink", lwd = 2)




#Cartographie#


# 1)/2)

dff=read.csv("C:/Users/Utilisateur/Documents/Cours BUT 2éme année/R studio/adresses-69.csv", header=TRUE, sep=";", dec=".")
View(dff)

# 3)

df_joint=merge(df, dff[, c("id", "lat", "lon")], by.x="Identifiant__BAN", by.y="id", all.x=TRUE)
View(df_joint)

# 4)Construire une carte simple à partir de l'exemple de SD1

install.packages("leaflet")
install.packages("ggplot2")

library(leaflet)
library(dplyr)
library(ggplot2)

leaflet(data = df_joint) %>%
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~paste("Identifiant:", Identifiant_BAN))
  

# 5)