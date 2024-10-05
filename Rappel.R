# Importer les jeux de données
df_ancien <- read.csv("dpe-v2-logements-existants.csv", sep = ",",dec = ".", header = TRUE)
df_neuf <- read.csv("dpe-v2-logements-neufs.csv", sep = ",",dec = ".", header = TRUE)

# Afficher la dimension des deux datasets
dim(df_ancien)
dim(df_neuf)

df_ancien$Logement <- "ancien"
df_neuf$Logement <- "neuf"
df_neuf$Année_construction = "2024"

# Trouver les colonnes communes
common_cols <- intersect(names(df_ancien), names(df_neuf))

# Fusionner les deux dataframes
df_combined <- rbind(df_ancien[, common_cols], df_neuf[, common_cols])

# Extraire l'année de la colonne Date de réception du DPE
df_combined$Date_réception_DPE <- as.Date(df_combined$Date_réception_DPE)
df_combined$Date_réception_DPE <- format(df_combined$Date_réception_DPE, "%Y")
# Créer une colonne de vérification
df_combined$Verification_Cout <- with(df_combined, Coût_total_5_usages == (Coût_chauffage + Coût_éclairage + Coût_ECS + Coût_refroidissement + Coût_auxiliaires))

# Calculer la part du coût du chauffage dans le coût total
df_combined$Cout_chauffage_pourcentage <- with(df_combined, (Coût_chauffage / Coût_total_5_usages) * 100)

table(df_combined$Etiquette_DPE)
table(df_combined$Date_réception_DPE)
table(df_combined$Logement)
table(df_combined$Type_bâtiment)
prop.table(table(df_combined$Type_installation_chauffage))
mean(df_combined$Surface_habitable_logement, na.rm = TRUE)
mean(df_combined$Cout_chauffage_pourcentage, na.rm = TRUE) 


install.packages("dplyr")
library(dplyr)

df1 = filter(df_combined, Type_bâtiment == "appartement")
df2 = filter(df_combined, Etiquette_DPE == c("D","E","F","G"))
df3 = filter(df_combined, Année_construction < 1948)
df4 = filter(df_combined, Surface_habitable_logement >mean(df_combined$Surface_habitable_logement, na.rm = TRUE) )
df5 = arrange(df_combined, Conso_5_usages.m._é_finale)
df6 = arrange(df_combined, Etiquette_DPE, Année_construction, Cout_chauffage_pourcentage)



aggregate(x = Cout_chauffage_pourcentage ~ Etiqette_DPE, df_combined, FUN = function(x) mean(x))


hist(df_combined$Surface_habitable_logement, main = ("Histogramme de la surface habitale"), xlab = "Surface habitable") 
df_combined$Année_construction = as.integer(df_combined$Année_construction)
barplot(df_combined$Année_construction, main = "Nombre de logement par année de construction", xlab = df_combined$Année_construction)





























