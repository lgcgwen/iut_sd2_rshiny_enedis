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
                            choices = c("Logements Neufs" = "neufs", "Logements Existants" = "existants")),
               textInput("txt", "Text input:", "general"),
               sliderInput("slider", "Slider input:", 1, 100, 30),
               tags$h5("Default actionButton:"),
               actionButton("action", "Search"),
               
               tags$h5("actionButton with CSS class:"),
               actionButton("action2", "Action button", class = "btn-primary")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Nombre de logements par code postal", 
                          plotOutput("hist_code_postal")
                 ),
                 tabPanel("Surface habitable", 
                          plotOutput("hist_surface_habitable")
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
  
  # Filtrage des données selon le type de bâtiment sélectionné
  filtered_data <- reactive({
    selected_data() %>% filter(Type_bâtiment %in% c("maison", "appartement"))
  })
  
  # Filtrage des logements du département du Rhône (code postal commençant par '69')
  rhone_data <- reactive({
    filtered_data() %>% filter(grepl("^69", Code_postal))
  })
  
  # Histogramme du nombre de logements par code postal
  output$hist_code_postal <- renderPlot({
    ggplot(filtered_data(), aes(x = Code_postal)) +
      geom_bar(fill = "green", color = "black") +
      labs(title = "Nombre de logements par code postal", x = "Code postal", y = "Nombre de logements") +
      theme_minimal()
  })
  
  # Histogramme de la surface habitable
  output$hist_surface_habitable <- renderPlot({
    ggplot(filtered_data(), aes(x = Surface_habitable)) +
      geom_histogram(bins = 30, fill = "blue", color = "black") +
      labs(title = "Histogramme de la surface habitable des logements", x = "Surface habitable (m²)", y = "Nombre de logements") +
      theme_minimal()
  })
  
  # Histogramme des étiquettes DPE
  output$hist_dpe <- renderPlot({
    ggplot(filtered_data(), aes(x = Etiquette_DPE)) +
      geom_bar(fill = "red", color = "black") +
      labs(title = "Répartition des étiquettes DPE", x = "Étiquette", y = "Nombre de bâtiments")
  })
  
  # Carte interactive pour le département du Rhône
  output$carte <- renderLeaflet({
    leaflet(rhone_data()) %>%
      addTiles() %>%
      addCircleMarkers(~long, ~lat, popup = ~paste("Code postal:", Code_postal, "<br>Étiquette DPE:", Etiquette_DPE, "<br>Consommation:", Conso_5_usages_é_finale),
                       radius = 5, color = "blue", stroke = FALSE, fillOpacity = 0.7)
  })
}

# Exécution de l'application
shinyApp(ui = ui, server = server)
