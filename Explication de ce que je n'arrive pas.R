# EXPLICATION DE MES DIFFICULTES

# POur LE SHINNY ET LA Partie graphique, je n'arrive pas a afficher le premier
#graphique avec le nombre de logement par code postal

#j'ai changer le code de l'api, j'ai 2 fichiers CSV, neuf et ancien et on
# peut choisir le fichier si on veut les données pour les logements neufs ou ancien

-# j'ai du mal a afficher la carte , je souhaite afficher la carte de lyon,
# carte des logements de lyon selon le type de logement ( appartement ou immeuble)
  # et avoir des infos quand on clique sur un points gps (etiquette_dpe, code postal(ban),...)
# il faudrait pouvoir aussi actualiser les données ( raffraichir)
  
# troisieme onglet, si tu peux demander de creer un tableau avec les données utiliser 

# Chargement des données selon le fichier sélectionné
data <- reactive({
  if(input$type_logement == "Neuf") {
    read.csv("neufs_69.csv")
  } else {
    read.csv("existants_69.csv")
  }
})

# Créer un graphique avec ggplot2
output$plot_nb_logements <- renderPlot({
  df <- data()
  ggplot(df, aes(x = Code_postal_(BAN))) +
    geom_bar() +
    labs(x = "Code Postal", y = "Nombre de Logements", title = "Nombre de Logements par Code Postal")
})

# Carte Leaflet
output$map <- renderLeaflet({
  df <- data()
  leaflet(df) %>%
    addTiles() %>%
    setView(lng = 4.8357, lat = 45.7640, zoom = 12) %>%
    addCircleMarkers(
      ~lon, ~lat,
      color = ~ifelse(Type_bâtiment == "appartement", "blue", "red"),
      popup = ~paste("DPE:", Etiquette_DPE, "<br>Code Postal:", Code_postal_(BAN), "<br>Coût Total:", Coût_total_5_usages)
    )
})

# Bouton de rafraîchissement
observeEvent(input$refresh, {
  data() # Recharger les données dans le tableau de données
  output$plot_nb_logements <- renderPlot({...}) # Redessiner le graphique
  output$map <- renderLeaflet({...}) # Redessiner la carte
})

# Affichage des données sous forme de tableau
output$table <- DT::renderDataTable({
  df <- data()
  datatable(df, options = list(pageLength = 10, autoWidth = TRUE))
})
