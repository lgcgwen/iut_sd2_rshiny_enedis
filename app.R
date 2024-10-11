library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)
library(shinythemes)

# Chargement des données
df_neufs <- read.csv("C:/Users/Utilisateur/Documents/Cours BUT 2éme année/R studio/neufs_69.csv", sep = ",", dec = ".", header = TRUE)
df_existants <- read.csv("C:/Users/Utilisateur/Documents/Cours BUT 2éme année/R studio/existants_69.csv", sep = ",", dec = ".", header = TRUE)

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
                 tabPanel("Nuage de points - Code postal",  # Nouvel onglet pour le nuage de points
                          plotOutput("scatter_code_postal")
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
    tabPanel("KPI et graphiques", 
             fluidRow(
               column(4, h3("KPI"),
                      textOutput("kpi_nb_logements"),
                      textOutput("kpi_surface_moyenne"),
                      textOutput("kpi_logements_neufs")
               ),
               column(4, h3("Diagramme Circulaire - Types de bâtiments"),
                      plotOutput("pie_chart")
               ),
               column(4, h3("Boîte à Moustaches - Surface par DPE"),
                      plotOutput("boxplot_dpe")
               )
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
    updateTabsetPanel(session, "tabs", selected = "Statistiques générales")  # Aller à l'onglet Statistiques
  })
  
  # Sélection des données en fonction du choix de l'utilisateur
  selected_data <- reactive({
    req(user_authenticated())  # Nécessite une connexion réussie
    if (input$dataset == "neufs") {
      df_neufs
    } else {
      df_existants
    }
  })
  
  # Plots et autres fonctions de l'application, conditionnés par l'authentification
  output$hist_type_batiment <- renderPlot({
    req(user_authenticated())
    data <- selected_data()
    req(data)
    if ("Type_bâtiment" %in% colnames(data)) {
      ggplot(data, aes(x = Type_bâtiment)) +
        geom_bar(fill = "red", color = "white") +
        theme_minimal() +
        labs(title = "Répartition des types de bâtiments", x = "Type de bâtiment", y = "Nombre de logements")
    } else {
      print("La colonne 'Type_bâtiment' n'existe pas.")
    }
  })
  
  
  # Nuage de points pour les codes postaux
  output$scatter_code_postal <- renderPlot({
    req(user_authenticated())
    data <- selected_data()
    req(data)
    if ("Code_postal_.BAN." %in% colnames(data) && "Surface_habitable_logement" %in% colnames(data)) {
      ggplot(data, aes(x = Code_postal_.BAN., y = Surface_habitable_logement)) +
        geom_point(alpha = 0.6, color = "blue") +
        theme_minimal() +
        labs(title = "Nuage de points - Code postal vs Surface habitable", 
             x = "Code Postal", 
             y = "Surface Habitable (m²)") +
        theme(legend.position = "none")
    } else {
      print("Les colonnes 'Code_postal_.BAN.' ou 'Surface_habitable_logement' n'existent pas.")
    }
  })
  
  output$hist_dpe <- renderPlot({
    req(user_authenticated())
    data <- selected_data()
    req(data)
    if ("Etiquette_DPE" %in% colnames(data)) {
      ggplot(data, aes(x = Etiquette_DPE)) +
        geom_bar(fill = "red", color = "white") +
        theme_minimal() +
        labs(title = "Nombre de logements par étiquette DPE", x = "Étiquette DPE", y = "Nombre de logements")
    } else {
      print("La colonne 'Etiquette_DPE' n'existe pas.")
    }
  })
  
  output$carte <- renderLeaflet({
    req(user_authenticated())
    data <- selected_data()
    req(data)
    if (all(c("lon", "lat", "Etiquette_DPE", "Code_postal_.BAN.") %in% colnames(data))) {
      color_pal <- colorFactor(
        palette = c("green", "lightgreen", "yellow", "orange", "darkorange", "red", "darkred"),
        levels = c("A", "B", "C", "D", "E", "F", "G")
      )
      
      leaflet(data) %>%
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
      print("Les colonnes 'lon', 'lat', 'Etiquette_DPE' ou 'Code_postal_.BAN.' n'existent pas.")
    }
  })
  
  output$kpi_nb_logements <- renderText({
    req(user_authenticated())
    data <- selected_data()
    req(data)
    paste("Nombre total de logements :", nrow(data))
  })
  
  output$kpi_surface_moyenne <- renderText({
    req(user_authenticated())
    data <- selected_data()
    req(data)
    paste("Surface habitable moyenne :", round(mean(data$Surface_habitable_logement, na.rm = TRUE), 2), "m²")
  })
  
  output$kpi_logements_neufs <- renderText({
    req(user_authenticated())
    data <- selected_data()
    req(data)
    nb_neufs <- nrow(df_neufs)
    nb_total <- nrow(df_neufs) + nrow(df_existants)
    pourcentage_neufs <- round((nb_neufs / nb_total) * 100, 2)
    paste("Pourcentage de logements neufs :", pourcentage_neufs, "%")
  })
  
  output$pie_chart <- renderPlot({
    req(user_authenticated())
    data <- selected_data()
    req(data)
    if ("Type_bâtiment" %in% colnames(data)) {
      type_counts <- data %>%
        count(Type_bâtiment) %>%
        mutate(percentage = n / sum(n) * 100) # Calcul des pourcentages
      
      ggplot(type_counts, aes(x = "", y = percentage, fill = Type_bâtiment)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        theme_void() +
        labs(title = "Répartition des types de bâtiments") +
        geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) # Ajout des étiquettes de pourcentage
    } else {
      print("La colonne 'Type_bâtiment' n'existe pas.")
    }
  })
  
  output$boxplot_dpe <- renderPlot({
    req(user_authenticated())
    data <- selected_data()
    req(data)
    if (all(c("Surface_habitable_logement", "Etiquette_DPE") %in% colnames(data))) {
      ggplot(data, aes(x = Etiquette_DPE, y = Surface_habitable_logement)) +
        geom_boxplot(fill = "lightblue", color = "darkblue") +
        theme_minimal() +
        labs(title = "Distribution des surfaces par étiquette DPE", x = "Étiquette DPE", y = "Surface habitable (m²)")
    } else {
      print("Les colonnes 'Surface_habitable_logement' ou 'Etiquette_DPE' n'existent pas.")
    }
  })
}

# Exécution de l'application
shinyApp(ui = ui, server = server)
