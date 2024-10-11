###### PROBLEME #######

#Je n'arrive pas a afficher l'histogramme sur le nombre de logement par code_postal_ban ( pourtant le nom de colonne est correcte)
 # Plot 2: Histogramme du nombre de logements par code postal (Code_postal_.BAN.)
  output$hist_code_postal <- renderPlot({
    data <- selected_data()
    req(data)  # Vérifie que les données existent
    
    # Vérifier si la colonne 'Code_postal_.BAN.' existe
    if ("Code_postal_.BAN." %in% colnames(data)) {
      
      # Supprimer les lignes avec des valeurs manquantes ou égales à 0
      data <- data %>% filter(!is.na(Code_postal_.BAN.) & Code_postal_.BAN. != 0)
      
      # Afficher l'histogramme
      ggplot(data, aes(x = Code_postal_.BAN.)) +
        geom_histogram(binwidth = 1, fill = "blue", color = "white") +
        theme_minimal() +
        labs(title = "Nombre de logements par code postal", x = "Code postal", y = "Nombre de logements")
      
    } else {
      print("La colonne 'Code_postal_.BAN.' n'existe pas.")
    }
  })


# La carte ne s'affiche toujours pas : ( ca me fais beuger le shinny et le freeze)

 # Cartographie Leaflet
  output$carte <- renderLeaflet({
    data <- selected_data()
    req(data)  # Vérifie que les données existent
    if (all(c("lon", "lat") %in% colnames(data))) {
      leaflet(data) %>% 
        addTiles() %>% 
        addMarkers(~lon, ~lat, popup = ~paste(Surface_habitable_logement, "m²"))
    } else {
      print("Les colonnes 'lon' ou 'lat' n'existent pas.")
    }
  })
}
