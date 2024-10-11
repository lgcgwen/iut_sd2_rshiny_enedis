library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)
library(shinythemes)

# Chargement des données
df_neufs <- read.csv("C:/Users/Utilisateur/Documents/Cours BUT 2éme année/R studio/neufs_69.csv", sep = ",", dec = ".", header = TRUE)
df_existants <- read.csv("C:/Users/Utilisateur/Documents/Cours BUT 2éme année/R studio/existants_69.csv", sep = ",", dec = ".", header = TRUE)

# Interface utilisateur
ui <- tagList(
  shinythemes::themeSelector(),
  navbarPage(
    "shinythemes",
    tabPanel("Statistiques générales", 
             sidebarPanel(
               radioButtons("dataset", "Sélectionner le jeu de données:",
                            choices = list("Logements Neufs" = "neufs", "Logements Existants" = "existants")),
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
                          plotOutput("hist_type_batiment")
                 ),
                 tabPanel("Nombre de logements par code postal", 
                          plotOutput("hist_code_postal")
                 ),
                 tabPanel("Nb logement / Etiquette DPE", 
                          plotOutput("hist_dpe")
                 )
               )
             )
    ),
    tabPanel("Cartographie", 
             leafletOutput("carte")
    ),
    tabPanel("Navbar 3", "This panel is intentionally left blank")
  )
)

# Serveur
server <- function(input, output) {
  
  # Sélection des données en fonction du choix de l'utilisateur
  selected_data <- reactive({
    if (input$dataset == "neufs") {
      df_neufs
    } else {
      df_existants
    }
  })
  
  # Plot 1: Histogramme de la répartition des types de bâtiments
  output$hist_type_batiment <- renderPlot({
    data <- selected_data()
    req(data)  # Vérifie que les données existent
    if ("Type_bâtiment" %in% colnames(data)) {
      ggplot(data, aes(x = Type_bâtiment)) +
        geom_bar(fill = "red", color = "white") +
        theme_minimal() +
        labs(title = "Répartition des types de bâtiments", x = "Type de bâtiment", y = "Nombre de logements")
    } else {
      print("La colonne 'Type_bâtiment' n'existe pas.")
    }
  })
  
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
  
  # Plot 3: Histogramme du nombre de logements par étiquette DPE
  output$hist_dpe <- renderPlot({
    data <- selected_data()
    req(data)  # Vérifie que les données existent
    if ("Etiquette_DPE" %in% colnames(data)) {
      ggplot(data, aes(x = Etiquette_DPE)) +
        geom_bar(fill = "red", color = "white") +
        theme_minimal() +
        labs(title = "Nombre de logements par étiquette DPE", x = "Étiquette DPE", y = "Nombre de logements")
    } else {
      print("La colonne 'Etiquette_DPE' n'existe pas.")
    }
  })
  
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

# Exécution de l'application
shinyApp(ui = ui, server = server)
