# Chargement des bibliothèques
library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)
library(shinythemes)

# Définir l'URL du fichier ZIP sur GitHub
url_zip <- "https://github.com/lgcgwen/iut_sd2_rshiny_enedis/raw/main/logement_finale%20-%20Copie.zip"

# Télécharger le fichier ZIP
download.file(url_zip, destfile = "logement_finale.zip", mode = "wb")

# Décompresser le fichier ZIP
unzip("logement_finale.zip")

# Charger les données
df_logement <- read.csv("logement_finale.csv", sep = ",", dec = ".", header = TRUE)


# Interface utilisateur (UI)
ui <- tagList(
  shinythemes::themeSelector(),
  navbarPage(
    "shinythemes",
    # Page de connexion
    tabPanel("Connexion",
             sidebarPanel(
               textInput("username", "Nom d'utilisateur"),
               passwordInput("password", "Mot de passe"),
               actionButton("login", "Se connecter"),
               textOutput("login_status")
             ),
             mainPanel(
               h3("Veuillez vous connecter pour accéder à l'application.")
             )
    ),
    # Autres onglets
    tabPanel("Statistiques générales", 
             sidebarPanel(
               textInput("txt", "Text input:", "general"),
               sliderInput("slider", "Slider input:", 1, 100, 30),
               tags$h5("Default actionButton:"),
               actionButton("action", "Search"),
               tags$h5("actionButton with CSS class:"),
               actionButton("action2", "Action button", class = "btn-primary")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Répartition des types de bâtiments", 
                          plotOutput("hist_type_batiment"),
                          downloadButton("download_hist_type_batiment", "Télécharger le graphique (.png)")
                 ),
                 tabPanel("Nuage de points - Coût de chauffage",  
                          plotOutput("scatter_coût_chauffage"),
                          verbatimTextOutput("debug_info"),  # Afficher les informations de débogage
                          downloadButton("download_scatter_coût_chauffage", "Télécharger le graphique (.png)")
                 ),
                 tabPanel("Nb logement / Etiquette DPE", 
                          plotOutput("hist_dpe"),
                          downloadButton("download_hist_dpe", "Télécharger le graphique (.png)")
                 )
               )
             )
    ),
    # Ajout de l'onglet "Contexte"
    tabPanel("Contexte", 
             sidebarPanel(),
             mainPanel(
               h3("Aperçu des données"),
               tableOutput("data_preview"),
               h3("Structure des données"),
               verbatimTextOutput("data_structure")
             )
    ),
    tabPanel("Cartographie", 
             sidebarPanel(
               actionButton("refresh_data", "Actualiser les données")
             ),
             mainPanel(
               leafletOutput("carte")
             )
    ),
    tabPanel("KPI et graphiques", 
             fluidRow(
               # Sélecteur pour le code postal
               column(12, 
                      h3("Filtrer par Code Postal"),
                      selectInput("code_postal", "Choisir un code postal :", 
                                  choices = unique(df_logement$Code_postal_.BAN.),  # Remplacez par le nom de votre colonne de code postal
                                  selected = unique(df_logement$Code_postal_.BAN.)[1],
                                  multiple = FALSE)  # Permettre la sélection d'un seul code postal
               )
             ),
             fluidRow(
               # Ajout des KPI en haut
               column(12, h3("KPI"),
                      textOutput("kpi_nb_logements"),
                      textOutput("kpi_surface_moyenne")
                      # textOutput("kpi_logements_neufs")  # Si vous avez besoin de ce KPI, décommentez cette ligne
               )
             ),
             fluidRow(
               # Ajout des graphiques en bas
               column(6, h3("Diagramme Circulaire - Types de bâtiments"),
                      plotOutput("pie_chart"),
                      downloadButton("download_pie_chart", "Télécharger le graphique (.png)")
               ),
               column(6, h3("Boîte à Moustaches - Surface par DPE"),
                      plotOutput("boxplot_dpe"),
                      downloadButton("download_boxplot_dpe", "Télécharger le graphique (.png)")
               )
             )
    ),
    #onglet pour la régression linéaire
    tabPanel("Régression Linéaire",
             sidebarPanel(
               selectInput("var_x", "Choisissez la variable X :", choices = names(df_logement), selected = names(df_logement)[1]),
               selectInput("var_y", "Choisissez la variable Y :", choices = names(df_logement), selected = names(df_logement)[2]),
               actionButton("calculate", "Calculer la régression")
             ),
             mainPanel(
               plotOutput("scatter_regression"),
               verbatimTextOutput("regression_summary"),  # Affichage du résumé de la régression
               verbatimTextOutput("correlation_text")      # Affichage du coefficient de corrélation
             )
    )
  )
)


# Serveur (server)
server <- function(input, output, session) {
  
  # Variables réactives pour la connexion
  user_authenticated <- reactiveVal(FALSE)
  
  # Gestion de la connexion
  observeEvent(input$login, {
    # Autoriser toute combinaison de nom d'utilisateur et mot de passe
    user_authenticated(TRUE)
    output$login_status <- renderText("Connexion réussie!")
  })
  
  # Sélection des données
  selected_data <- reactive({
    req(user_authenticated())  # Nécessite une connexion réussie
    df_logement
  })
  
  # Aperçu des données
  output$data_preview <- renderTable({
    req(user_authenticated())
    head(selected_data(), 10)  # Affiche les 10 premières lignes
  })
  
  # Structure des données
  output$data_structure <- renderPrint({
    req(user_authenticated())
    str(selected_data())  # Affiche la structure des colonnes du jeu de données
  })
  
  # Histogramme des types de bâtiments
  output$hist_type_batiment <- renderPlot({
    req(user_authenticated())
    data <- selected_data()
    ggplot(data, aes(x = Type_bâtiment)) +
      geom_bar(fill = "red", color = "white") +
      theme_minimal() +
      labs(title = "Répartition des types de bâtiments", x = "Type de bâtiment", y = "Nombre de logements")
  })
  
  # Téléchargement de l'histogramme
  output$download_hist_type_batiment <- downloadHandler(
    filename = function() { "hist_type_batiment.png" },
    content = function(file) {
      png(file)
      print(ggplot(selected_data(), aes(x = Type_bâtiment)) +
              geom_bar(fill = "red", color = "white") +
              theme_minimal() +
              labs(title = "Répartition des types de bâtiments", x = "Type de bâtiment", y = "Nombre de logements"))
      dev.off()
    }
  )
  
  # Vérifiez les noms des colonnes et les valeurs manquantes
  print(colnames(df_logement))
  print(head(df_logement))
  
  # Nettoyage des données : filtre des valeurs manquantes pour la consommation de chauffage et la surface habitable
  df_logement_clean <- df_logement %>%
    filter(!is.na(Coût_chauffage) & !is.na(Surface_habitable_logement))
  
  # Créer le nuage de points pour le coût de chauffage par type de bâtiment
  output$scatter_coût_chauffage <- renderPlot({
    req(user_authenticated())
    ggplot(df_logement_clean, aes(x = Surface_habitable_logement, y = Coût_chauffage)) +
      geom_point(alpha = 0.6, color = "blue") +
      theme_minimal() +
      labs(title = "Nuage de points - Coût de chauffage vs Surface habitable",
           x = "Surface Habitable (m²)",
           y = "Coût de Chauffage (€)") +
      theme(legend.position = "none")
  })
  
  # Téléchargement du nuage de points
  output$download_scatter_coût_chauffage <- downloadHandler(
    filename = function() { "scatter_coût_chauffage.png" },
    content = function(file) {
      png(file)
      print(ggplot(df_logement_clean, aes(x = Surface_habitable_logement, y = Coût_chauffage)) +
              geom_point(alpha = 0.6, color = "blue") +
              theme_minimal() +
              labs(title = "Nuage de points - Coût de chauffage vs Surface habitable", 
                   x = "Surface Habitable (m²)", 
                   y = "Coût de Chauffage (€)") +
              theme(legend.position = "none"))
      dev.off()
    }
  )
  
  # Histogramme DPE
  output$hist_dpe <- renderPlot({
    req(user_authenticated())
    data <- selected_data()
    ggplot(data, aes(x = Etiquette_DPE)) +
      geom_bar(fill = "red", color = "white") +
      theme_minimal() +
      labs(title = "Nombre de logements par étiquette DPE", x = "Étiquette DPE", y = "Nombre de logements")
  })
  
  # Téléchargement de l'histogramme DPE
  output$download_hist_dpe <- downloadHandler(
    filename = function() { "hist_dpe.png" },
    content = function(file) {
      png(file)
      print(ggplot(selected_data(), aes(x = Etiquette_DPE)) +
              geom_bar(fill = "red", color = "white") +
              theme_minimal() +
              labs(title = "Nombre de logements par étiquette DPE", x = "Étiquette DPE", y = "Nombre de logements"))
      dev.off()
    }
  )
  
  # Aperçu des données
  output$data_preview <- renderTable({
    req(user_authenticated())
    head(selected_data(), 10)  # Affiche les 10 premières lignes
  })
  
  # Structure des données
  output$data_structure <- renderPrint({
    req(user_authenticated())
    str(selected_data())  # Affiche la structure des colonnes du jeu de données
  })
  # Simuler une fonction qui récupère les nouvelles données via une API
  get_new_data <- function() {
    # Simulez la récupération de données avec un délai
    Sys.sleep(2)  # Simule un temps de récupération de données
    
    # Remplacez par l'appel API réel avec httr, jsonlite, etc.
    new_data <- df_logement  # Simule l'obtention de nouvelles données
    
    # Ajoutez du bruit aux données pour simuler des changements
    new_data$Surface_habitable_logement <- new_data$Surface_habitable_logement + rnorm(nrow(new_data), mean = 0, sd = 5)
    
    return(new_data)
  }
  
 # Créer une variable réactive pour les données de la carte
carte_data <- reactiveVal(df_logement)  # Initialise avec les données existantes

# Observer l'événement d'actualisation des données
observeEvent(input$refresh_data, {
  # Affiche un message pour informer que les données sont en cours d'actualisation
  showModal(modalDialog("Mise à jour des données, veuillez patienter...", footer = NULL))
  
  # Appeler la fonction pour récupérer les nouvelles données
  new_data <- get_new_data()
  
  # Mettre à jour la variable réactive avec les nouvelles données
  carte_data(new_data)
  
  # Fermer le modal une fois les données mises à jour
  removeModal()
})

# Mise à jour de la carte avec les nouvelles données
output$carte <- renderLeaflet({
  req(user_authenticated())
  
  # Récupérer les données mises à jour
  data <- carte_data()
  
  # Filtrer les données pour supprimer les lignes avec des valeurs manquantes pour lon et lat
  data_filtered <- data %>%
    filter(!is.na(lon) & !is.na(lat) & lon != 0 & lat != 0)  # Suppression des valeurs NA et 0
  
  # Définir les limites de la zone de Lyon et de ses arrondissements
  lyon_lat_min <- 45.70
  lyon_lat_max <- 45.85
  lyon_lon_min <- 4.80
  lyon_lon_max <- 4.90  # Limite ajustée pour Lyon uniquement
  
  # Filtrer pour ne garder que les données de Lyon et ses arrondissements
  data_filtered <- data_filtered %>%
    filter(lat >= lyon_lat_min & lat <= lyon_lat_max &
             lon >= lyon_lon_min & lon <= lyon_lon_max)
  
  # Limiter les points à 1000 en prenant un échantillon aléatoire
  if (nrow(data_filtered) > 1000) {
    data_filtered <- data_filtered %>% sample_n(1000)
  }
  
  # Vérifier s'il y a des données à afficher
  if (nrow(data_filtered) > 0) {
    # Palette de couleurs
    color_pal <- colorFactor(
      palette = c("lightgreen", "yellow", "orange", "darkorange", "red", "darkred"),
      levels = c("A", "B", "C", "D", "E", "F", "G")
    )
    
    leaflet(data_filtered) %>%
      addTiles() %>%
      setView(lng = 4.85, lat = 45.75, zoom = 12) %>%
      addCircleMarkers(~lon, ~lat,
                       radius = 5,
                       color = ~color_pal(Etiquette_DPE),
                       fill = TRUE,
                       fillOpacity = 0.8,
                       popup = ~paste(
                         "Surface :", Surface_habitable_logement, "m²", "<br>",
                         "Type de bâtiment :", Type_bâtiment, "<br>",
                         "Etiquette DPE :", Etiquette_DPE, "<br>",
                         "Code postal :", Code_postal_.BAN.
                       )
      ) %>%
      addLegend("bottomright", pal = color_pal, values = ~Etiquette_DPE,
                title = "Etiquette DPE", opacity = 1)
  } else {
    leaflet() %>%
      addTiles() %>%
      setView(lng = 4.85, lat = 45.75, zoom = 12) %>%
      addPopups(lng = 4.85, lat = 45.75, "Aucune donnée valide à afficher.")
  }
})
  
   # Calcul des KPI
  output$kpi_nb_logements <- renderText({
    req(user_authenticated())  # Vérifie si l'utilisateur est authentifié
    data <- selected_data()     # Sélection des données
    
    # Affiche le nombre total de logements
    paste("Nombre total de logements :", nrow(data))
  })
  
  output$kpi_surface_moyenne <- renderText({
    req(user_authenticated())  # Vérifie si l'utilisateur est authentifié
    data <- selected_data()     # Sélection des données
    
    # Calcul de la surface habitable moyenne
    surface_moyenne <- round(mean(data$Surface_habitable_logement, na.rm = TRUE), 2)
    
    # Affiche la surface habitable moyenne
    paste("Surface habitable moyenne :", surface_moyenne, "m²")
  })
  # Diagramme circulaire des types de bâtiments
  output$pie_chart <- renderPlot({
    req(user_authenticated())
    data <- selected_data()
    type_counts <- as.data.frame(table(data$Type_bâtiment))
    colnames(type_counts) <- c("Type de bâtiment", "Nombre")
    
    ggplot(type_counts, aes(x = "", y = Nombre, fill = `Type de bâtiment`)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Répartition des types de bâtiments")
  })
  
  # Téléchargement du diagramme circulaire
  output$download_pie_chart <- downloadHandler(
    filename = function() { "pie_chart.png" },
    content = function(file) {
      png(file)
      print(ggplot(as.data.frame(table(selected_data()$Type_bâtiment)), aes(x = "", y = Freq, fill = Var1)) +
              geom_bar(width = 1, stat = "identity") +
              coord_polar(theta = "y") +
              theme_void() +
              labs(title = "Répartition des types de bâtiments"))
      dev.off()
    }
  )
  
  # Boîte à moustaches de surface par DPE
  output$boxplot_dpe <- renderPlot({
    req(user_authenticated())
    data <- selected_data()
    ggplot(data, aes(x = Etiquette_DPE, y = Surface_habitable_logement)) +
      geom_boxplot(fill = "lightblue") +
      theme_minimal() +
      labs(title = "Boîte à Moustaches - Surface par Étiquette DPE", 
           x = "Étiquette DPE", 
           y = "Surface Habitable (m²)")
  })
  
  # Téléchargement de la boîte à moustaches
  output$download_boxplot_dpe <- downloadHandler(
    filename = function() { "boxplot_dpe.png" },
    content = function(file) {
      png(file)
      print(ggplot(selected_data(), aes(x = Etiquette_DPE, y = Surface_habitable_logement)) +
              geom_boxplot(fill = "lightblue") +
              theme_minimal() +
              labs(title = "Boîte à Moustaches - Surface par Étiquette DPE", 
                   x = "Étiquette DPE", 
                   y = "Surface Habitable (m²)"))
      dev.off()
    }
  )
}
# Régression linéaire et affichage des résultats
observeEvent(input$calculate, {
  req(user_authenticated())
  
  # Sélection des variables
  var_x <- input$var_x
  var_y <- input$var_y
  
  # Nettoyage des données : enlever les NA
  data_clean <- selected_data() %>%
    select(all_of(c(var_x, var_y))) %>%
    na.omit()
  
  # Vérifiez si le dataframe est vide après nettoyage
  if(nrow(data_clean) == 0) {
    output$regression_summary <- renderPrint({"Aucune donnée valide pour effectuer la régression."})
    output$scatter_regression <- renderPlot({ ggplot() + labs(title = "Aucune donnée à afficher") })
    output$correlation_text <- renderText({"Aucune donnée valide pour calculer la corrélation."})
    return()
  }
  
  # Calcul de la régression
  lm_model <- lm(as.formula(paste(var_y, "~", var_x)), data = data_clean)
  
  # Résumé du modèle
  output$regression_summary <- renderPrint({
    summary(lm_model)
  })
  
  # Calcul du coefficient de corrélation
  correlation_coefficient <- cor(data_clean[[var_x]], data_clean[[var_y]], use = "complete.obs")
  
  # Affichage du coefficient de corrélation
  output$correlation_text <- renderText({
    paste("Coefficient de corrélation entre", var_x, "et", var_y, ":", round(correlation_coefficient, 4))
  })
  
  # Graphique de régression
  output$scatter_regression <- renderPlot({
    ggplot(data_clean, aes_string(x = var_x, y = var_y)) +
      geom_point(alpha = 0.6, color = "blue") +
      geom_smooth(method = "lm", col = "red") +
      theme_minimal() +
      labs(title = paste("Régression Linéaire:", var_y, "~", var_x),
           x = var_x,
           y = var_y)
  })
})

# Exécution de l'application
shinyApp(ui = ui, server = server)
