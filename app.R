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
  
  # Plot 2: Histogramme du nombre de logements par code postal (Code_postal_.BAN.)
  output$hist_code_postal <- renderPlot({
    data <- selected_data()
    req(data)
    if ("Code_postal_.BAN." %in% colnames(data)) {
      data <- data %>% filter(!is.na(Code_postal_.BAN.) & Code_postal_.BAN. != 0)
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
  
  # Cartographie Leaflet avec légende des couleurs DPE
  output$carte <- renderLeaflet({
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
  
  # KPI: Calcul du nombre total de logements, surface moyenne et proportion des logements neufs
  output$kpi_nb_logements <- renderText({
    data <- selected_data()
    req(data)
    paste("Nombre total de logements :", nrow(data))
  })
  
  output$kpi_surface_moyenne <- renderText({
    data <- selected_data()
    req(data)
    paste("Surface habitable moyenne :", round(mean(data$Surface_habitable_logement, na.rm = TRUE), 2), "m²")
  })
  
  output$kpi_logements_neufs <- renderText({
    data <- selected_data()
    req(data)
    nb_neufs <- nrow(df_neufs)
    nb_total <- nrow(df_neufs) + nrow(df_existants)
    pourcentage_neufs <- round((nb_neufs / nb_total) * 100, 2)
    paste("Pourcentage de logements neufs :", pourcentage_neufs, "%")
  })
  
  # Graphique en camembert (pie chart) pour la répartition des types de bâtiments
  output$pie_chart <- renderPlot({
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
  
  # Boîte à moustaches (boxplot) de la surface en fonction de l'étiquette DPE
  output$boxplot_dpe <- renderPlot({
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
