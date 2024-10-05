install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)

adresses = read.csv("C:/Users/Utilisateur/Documents/Cours BUT 2éme année/R studio/adresses-69.csv",
                    sep = ";", dec = ".", header = TRUE)



cp = sort(unique(adresses$code_postal))     
année = c("2021","2022","2023","2024")

base_url_existant <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"
base_url_neuf <-  "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-neufs/lines"

df_ancien = data.frame()
df_neuf = data.frame()



for (i in 1:length(cp)){
  for (j in 1:length(année)){
    # Paramètres de la requête
    params <- list(
      page = 1,
      size = 10000,
      select = "Etiquette_DPE,Année_construction",
      q = cp[i],
      q_fields = "Code_postal_(BAN)",
      qs = paste("Date_réception_DPE:[", année[j], "-01-01 TO ", année[j], "-12-31]", sep = "")
    ) 
    print(cp[i])
    # Encodage des paramètres
    url_encoded <- modify_url(base_url_existant, query = params)
    
    # Effectuer la requête
    response <- GET(url_encoded)
    
    # On convertit le contenu brut (octets) en une chaîne de caractères (texte). Cela permet de transformer les données reçues de l'API, qui sont généralement au format JSON, en une chaîne lisible par R
    content = fromJSON(rawToChar(response$content), flatten = FALSE)
    
    df_ancien = rbind(df_ancien, content$results)
    
  }
}


for (i in 1:length(cp)){
  for (j in 1:length(année)){
    # Paramètres de la requête
    params <- list(
      page = 1,
      size = 10000,
      select ="Etiquette_DPE,Année_construction",
      q = cp[i],
      q_fields = "Code_postal_(BAN)",
      qs = paste("Date_réception_DPE:[", année[j], "-01-01 TO ", année[j], "-12-31]", sep = "")
    ) 
    print(cp[i])
    # Encodage des paramètres
    url_encoded <- modify_url(base_url_neuf, query = params)
    
    # Effectuer la requête
    response <- GET(url_encoded)
    
    # On convertit le contenu brut (octets) en une chaîne de caractères (texte). Cela permet de transformer les données reçues de l'API, qui sont généralement au format JSON, en une chaîne lisible par R
    content = fromJSON(rawToChar(response$content), flatten = FALSE)
    
    df_neuf = rbind(df_neuf, content$results)
    
  }
}
response <- GET(url_encoded)
content <- fromJSON(rawToChar(response$content), flatten = FALSE)
df_neuf <- content$results
str(df_ancien)
str(df_neuf)

finale = rbind(df_ancien,df_neuf)

View(finale)
  
