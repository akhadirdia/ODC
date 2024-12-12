library(shiny)
library(shinydashboard)
library(htmltools)
library(rmarkdown)
library(knitr)
library(readxl)
library(writexl)
library(shinyjs)
library(tidyverse)
library(ggplot2)
library(shinyBS)
library(officer)
library(pdftools)

# Define UI for application that draws a histogram
ui <-
  dashboardPage(
    
    # Application title
    dashboardHeader(title = "Calcul de Charge"),
    
    # Sidebar with menu
    dashboardSidebar(
      sidebarMenu(
        id="tabs",
        # Menu items
        tags$img(src = "image001.png", width = "180px", height = "55px"),
        menuItem("Calcul de charge", tabName = "calcharge", icon = icon("bolt")),
        menuItem("Importer Données", tabName = "impdata", icon = icon("area-chart")),
        menuItem("Menu 3", tabName = "Menu3", icon = icon("database")),
        menuItem("Aide", tabName = "aide", icon = icon("question-circle"))
      )
    ),
    
    # Main content
    dashboardBody(
      useShinyjs(), 
      tabItems(
        # Tab items
        tabItem(tabName = "calcharge",
                fluidRow(
                  # Affichage de la fenêtre modale
                  bsModal("erreurModal", "Erreur", "modal-error", size = "small", 
                          textOutput("messageErreur"),
                          actionButton("fermerModal", "Fermer")
                  ),
                  # Row 2
                  box(width = 6, height = '610px', title = "Données de  l'entrée electrique", background = "light-blue", status = "info",solidHeader = TRUE, collapsible = TRUE,
                      icon = icon("bolt"),
                      column (width = 4,
                              numericInput("tension", "Tension entrée electrique:", value = 0, min = 0),
                              numericInput("intensite", "Intensité entrée electrique:", value = 0, min = 0),
                              selectInput("monotri", "Monophasée/Triphasée:", choices = c("triphasée", "monophasée")),
                              numericInput("apppuiss", "Appel de Puissance:", value = 0, min = 0),
                              selectInput("protect", "Protection:", choices = c("20%", "40%"))
                      ),
                      column(width = 4, numericInput(inputId = "num_transformateurs", label = "Entrez le nombre de type de transformateurs :", value = 1, min = 0, max = 20),
                             uiOutput("inputs_transformateurs")),
                      column(width = 4, numericInput(inputId = "num_sectionneur", label = "Entrez le nombre de Sectionneur :", value = 1, min = 0, max = 20),
                             uiOutput("inputsectionneur"),
                             verbatimTextOutput("result")),
                      
                      fluidRow(
                        column (width = 6, textInput("nom_visit", "Nom du responsable de la visite")),
                        column (width = 6, dateInput("date", "Entrez la date de la visite", value = "2023-01-01"))
                      )),
                  box(width = 6, height = '610px', title = "Informations sur les bornes", background = "light-blue", status = "info",solidHeader = FALSE, collapsible = TRUE,
                      
                      br(),
                      br(),
                      tags$p(strong("Importer un fichier contenant les données de l'entrée electrique")),
                      br(),
                      br(),
                      column (width=3, 
                              numericInput("B7.2kW", "Borne de 7.2 kW:", value = 0, min=0),
                              numericInput("B24kW3ph", "Borne de 24 kW 3ph:", value = 0, min=0),
                              numericInput("B400kW", "Borne de 400 kW:", value = 0, min=0)),
                      column(width = 3, 
                             numericInput("B11.5kW", "Borne de 11.5 kW:", value = 0, min=0),
                             numericInput("B25kW", "Borne de 25 kW:", value = 0, min=0),
                             numericInput("B600kW", "Borne de 600 kW:", value = 0, min=0),
                             numericInput("B9.6kW", "Borne de 9.6 kW:", value = 0, min=0)
                      ),
                      column(width=3,
                             numericInput("B19.2kW", "Borne de 19.2 kW:", value = 0, min=0),
                             numericInput("B50kW", "Borne de 50 kW:", value = 0, min=0),
                             numericInput("B7.2kWPart", label =tags$p("Borne de 7.2 kW avec Partage de Puissance :", style="font-size:12.5px;"), value = 0, min=0)),
                      
                      column(width = 3, 
                             numericInput("B24kW1ph", "Borne de 24 kW 1ph:", value = 0, min=0),
                             numericInput("B200kW", "Borne de 200 kW:", value = 0, min=0),
                             numericInput(inputId = "part_borne", label =tags$p("Si partage de Puissance de borne 7.2 kW, Entrez le nombre de fois :", style="font-size:10px;"), value = 0, min = 0, max = 10)),
                      fluidRow(
                        column (width = 6, textInput("adressbat", "Adresse du Batiment")),
                        column (width = 6, textInput("num_compteur", "Numero du Compteur"))
                      ),
                      br(),
                      br(),
                      br()
                  )),
                
                
                fluidRow(
                  tags$p(strong("Pour tous les calculs, on a considéré une protection de 80% sur l'entrée electrique, 20% sur les bornes de niveau 2 et 20% sur les bornes de niveau 3"))),
                
                
                fluidRow(
                  # Row 1
                  ##  tags$head(tags$style(HTML(".small-box {height: 50px}"))),
                  
                  box(width = 6,title = "Résultats du Calcul de Charge", background = "light-blue", status = "info",
                      solidHeader = TRUE, collapsible = TRUE,
                      
                      tags$div(
                        style = "display: flex; align-items: center;",  # Alignement horizontal des éléments
                        tags$div(
                          style = "flex: 1;",  # Utilisez une fraction égale de l'espace disponible
                          
                          selectInput("choicephase", "Choisir les resultats de la phase à afficher", choices = c("1", "2", "3","4","5"), width = '300px')
                          
                        ),
                        tags$div(
                          style = "flex: 1; margin-left: 20px;",  # Utilisez une fraction égale de l'espace disponible et ajoutez une marge à gauche
                          actionButton("affich", "Afficher les résultats", icon = icon("bolt"))
                        )
                      ),
                      tags$h4("  Methode de Calcul basée sur les transformateurs et sectionneurs"),
                      ## hr(),
                      valueBoxOutput("box1", width = 3), 
                      valueBoxOutput("box4", width = 3), 
                      valueBoxOutput("box2", width = 3),
                      valueBoxOutput("box3", width = 3),
                      br(),
                      br(),
                      br(),
                      tags$p(" "),
                      tags$h4("          Methode de Calcul basée sur l'appel de puissance"),
                      
                      valueBoxOutput("box8", width = 3),
                      valueBoxOutput("box5", width = 3),
                      valueBoxOutput("box6", width = 3),
                      valueBoxOutput("box7", width = 3)),
                  box(width = 6,title = "Dimensionnement du Transformateur", background = "light-blue", status = "info",
                      solidHeader = TRUE, collapsible = TRUE, height = '440px',
                      tags$h4("Borne de niveau 2 / 240 V"),
                      valueBoxOutput("box9", width = 6),
                      valueBoxOutput("box10", width = 6),
                      tags$h4("Borne de niveau 3 / 480 V"),
                      valueBoxOutput("box11", width = 6),
                      valueBoxOutput("box12", width = 6),
                      tags$h4("Nouvelle entrèe electrique"),
                      valueBoxOutput("box15", width = 6),
                      valueBoxOutput("box16", width = 6))
                  
                ),
                fluidRow(
                  # Row 2
                  
                  box(width = 6,title = "Telecharger les résultats du Calcul de Charge", background = "light-blue", status = "info",
                      solidHeader = TRUE, collapsible = TRUE,
                      tags$div(
                        style = "display: flex; align-items: center;",  # Alignement horizontal des éléments
                        tags$div(
                          style = "flex: 1;",  # Utilisez une fraction égale de l'espace disponible
                          
                          textInput("batiment", "Nom du batiment")
                          
                        ),
                        tags$div(
                          style = "flex: 1; margin-left: 20px;",  # Utilisez une fraction égale de l'espace disponible et ajoutez une marge à gauche
                          downloadButton("telecharger_excel", "Télécharger les données de l'entrée electrique")
                        )
                      ),
                      tags$div(
                        style = "display: flex; align-items: center;",  # Alignement horizontal des éléments
                        tags$div(
                          style = "flex: 1;",  # Utilisez une fraction égale de l'espace disponible
                          
                          downloadButton("download_data_borne", "Telecharger les données des bornes")
                          
                        ),
                        tags$div(
                          style = "flex: 1; margin-left: 20px;",  # Utilisez une fraction égale de l'espace disponible et ajoutez une marge à gauche
                          downloadButton("report", "Telecharger le rapport")
                        )
                      ),
                      
                      
                      br()
                  ),
                  box(width = 6,title = "Ajout de charge", background = "light-blue", status = "info",
                      solidHeader = TRUE, collapsible = TRUE, height = '192px',
                      valueBoxOutput("box13", width = 6),
                      valueBoxOutput("box14", width = 6)
                  )
                )
        ),
        
        tabItem(tabName = "impdata",
                
                fluidRow(
                  box(width = 4, title = "Données de l'entrée electrique", background = "light-blue", status = "info",solidHeader = FALSE, collapsible = TRUE,
                      tags$p(strong("Importer un fichier contenant les données de l'entrée electrique")),
                      hr(),
                      column(width = 12, fileInput("load", "Choisir un fichier Excel (.xlsx ou .xls)"))
                      
                  ),
                  box(width = 4, title = "Données sur la consommation d'energie", background = "light-blue", status = "info",solidHeader = FALSE, collapsible = TRUE,
                      tags$p(strong("Importer le fichier contenant la consommation d'energie par 15 minutes")),
                      hr(),
                      column(width = 12, fileInput("loadappui", "Choisir un fichier Excel"))
                  ),
                  box(width = 4, title = "Données sur les bornes de recharge", background = "light-blue", status = "info",solidHeader = FALSE, collapsible = TRUE,
                      tags$p(strong("Importer un fichier contenant les quantités et puissances de bornes souhaitées")),
                      hr(),
                      column(width = 12, fileInput("loadborne", "Choisir un fichier Excel (.xlsx ou .xls)"))
                  )
                ),
                fluidRow(
                  tags$strong("\t Pour afficher les resultats du calcul de charge "),
                  actionButton("btn_menu", " Cliquez ici")
                ),
                fluidRow(
                  box(title = "Heure de la journée où la consommation est plus elevée", status = "primary", solidHeader = TRUE, width=12,
                      plotOutput("plot1"))
                )
        ),
        
        tabItem(tabName = "Menu3",
                "En construction"
                
        ),
        
        tabItem(tabName = "aide",
                
                "En construction"
                
        )
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  # Server logic
  
  # output$inputs_transformateurs <- renderUI({
  #   num_transformateurs <- input$num_transformateurs
  # 
  #   inputs <- tagList()
  # 
  #   if(num_transformateurs>0){
  # 
  #     for(i in seq_len(num_transformateurs)) {
  #       inputs[[paste0("tension_", i)]] <- numericInput(
  #         inputId = paste0("tension_", i),
  #         label = paste0("Tension du transformateur ", i, " :"),
  #         value = 0
  #       )
  # 
  #       inputs[[paste0("puissance_", i)]] <- numericInput(
  #         inputId = paste0("puissance_", i),
  #         label = paste0("Puissance du transformateur ", i, " :"),
  #         value = 0
  #       )
  #     }
  #   }
  # 
  #   return(inputs)
  # })
  
  
  output$inputs_transformateurs <- renderUI({
    num_transformateurs <- input$num_transformateurs
    if(!is.na(num_transformateurs) && length(num_transformateurs) > 0 && num_transformateurs > 0){
      lapply(seq_len(num_transformateurs), function(i) {
        tagList(
          numericInput(paste0("tension_", i), paste0("Tension du transformateur ", i, " :"), value = 0, min = 0),
          numericInput(paste0("puissance_", i), paste0("Puissance du transformateur ", i, " :"), value = 0, min = 0),
          selectInput(paste0("phase_", i), paste0("Phase du transformateur ", i, " :"), choices = c("triphasée", "monophasée")),
          numericInput(paste0("tension_sorti_", i),paste0("Tension de sortie du transformateur", i), value = 0, min = 0)
        )
      })
    }
  })
  
  
  
  output$inputsectionneur <- renderUI({
    num_sectionneur <- input$num_sectionneur
    
    inputs <- tagList()
    
    if(!is.na(num_sectionneur) && num_sectionneur>0){
      
      for(i in seq_len(num_sectionneur)) {
        inputs[[paste0("tensionsec_", i)]] <- numericInput(
          inputId = paste0("tensionsec_", i),
          label = paste0("Tension du Sectionneur ", i, " :"),
          value = 0, min = 0
        )
        
        inputs[[paste0("intensite_", i)]] <- numericInput(
          inputId = paste0("intensite_", i),
          label = paste0("Intensite du sectionneur ", i, " :"),
          value = 0, min = 0
        )
        
        inputs[[paste0("nbredefois_", i)]] <- numericInput(
          inputId = paste0("nbredefois_", i),
          label = paste0("Nombre de fois ", i, " :"),
          value = 1, min = 1
        )
      }
    }
    
    return(inputs)
  })
  
  ##Telecharger les donnees de l'entree dans un fichier Excel
  
  saisie_donnees <- reactive({
    num_transformateurs <- input$num_transformateurs
    num_sectionneur <- input$num_sectionneur
    donnees = data.frame(
      tension = input$tension,
      intensite = input$intensite,
      Phasage = input$monotri,
      num_transformateurs = num_transformateurs,
      num_sectionneur = num_sectionneur,
      Date_visite = input$date,
      Responsable_Visite = input$nom_visit
    )
    
    
    for(i in seq_len(num_transformateurs)) {
      if (!is.null(input[[paste0("tension_", i)]])){
        
        donnees[paste0("tension_", i)]<- input[[paste0("tension_", i)]]
        donnees[paste0("puissance_", i)]<- input[[paste0("puissance_", i)]]
        donnees[paste0("phase_", i)] <- input[[paste0("phase_", i)]]
        donnees[paste0("tension_sorti_", i)] <- input[[paste0("tension_sorti_", i)]]
        
      }
    }
    
    for(i in seq_len(num_sectionneur)) {
      if (!is.null(input[[paste0("tensionsec_", i)]])){
        
        donnees[paste0("tensionsec_", i)] <- input[[paste0("tensionsec_", i)]]
        donnees[paste0("intensite_", i)]<- input[[paste0("intensite_", i)]]
        donnees[paste0("nbredefois_", i)] <- input[[paste0("nbredefois_", i)]]
        
      }
    }
    donnees
  })
  
  # Télécharger les données dans un fichier Excel
  output$telecharger_excel <- downloadHandler(
    filename = function() {
      paste("Entree electrique ",input$batiment, Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(saisie_donnees(), file)
    }
  )
  
  ### Telecharger les donnees des bornes dans un fichier EXCEL
  
  saisie_donnees_data_borne <- reactive({
    
    donneesborne = data.frame(
      Annee = "",
      B7.2kW = input$B7.2kW,
      B7.2kWPart = input$B7.2kWPart,
      B9.6kW = input$B9.6kW,
      part_borne = input$part_borne,
      B11.5kW = input$B11.5kW,
      B19.2 = input$B19.2kW,
      B24kW1ph = input$B24kW1ph,
      B24kW3ph = input$B24kW3ph,
      B25kW = input$B25kW,
      B50kW = input$B50kW,
      B200kW = input$B200kW,
      B400kW = input$B400kW,
      B600kW = input$B600kW,
      phase = input$choicephase
    )
    
    donneesborne
  })
  # Télécharger les données dans un fichier Excel
  output$download_data_borne <- downloadHandler(
    filename = function() {
      paste("Nbre_et_type_de_borne_",input$batiment, Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(saisie_donnees_data_borne(), file)
    }
  )
  
  observe({
    voltage <- input$tension
    current <- input$intensite
    num_transformateurs <- input$num_transformateurs
    num_sectionneur <- input$num_sectionneur
    nbborne7.2P <- input$B7.2kWPart
    nbborne7.2 <- input$B7.2kW
    nbborne9.6 <- input$B9.6kW
    nbborne11.5 <- input$B11.5kW
    nbborne19.2 <- input$B19.2kW
    nbborne24_1ph <- input$B24kW1ph
    nbborne24_3ph <- input$B24kW3ph
    nbborne25 <- input$B25kW
    nbborne50 <- input$B50kW
    nbborne200 <- input$B200kW
    nbborne400 <- input$B400kW
    nbborne600 <-input$B600kW
    ajout_intens <-0
    puisstotal_borne <- 0 
    intentransfo <-0
    intensite_sec <-0
    puissdispo <- 0
    puissdispo2 <-0
    power1 <- 0
    intendispo <- 0
    puissance_sec <-0
    protect <- input$protect
    apuiss <- input$apppuiss
    if (protect=="40%") {
      appuiss <- input$apppuiss*1.4
    }else{
      appuiss <- input$apppuiss*1.2
    }
    
    inten_nv2transfo <-0
    inten_nv3transfo <-0
    puissnv3transfo <-0
    puissnv2transfo <- 0
    puissnv3transfo2 <-0
    puissnv2transfo2 <-0
    Ampere240<-0
    Ampere480<-0
    puissnv23ptransfo <-0
    puissnv31ptransfo <-0
    I_resid<-0
    I_borneN2 <-0
    I_borneN3 <-0
    I_appuiss <-0
    I_residA <- 0
    intendispoA <-0
    I_entreN2 <- 0
    I_entreN3 <- 0
    Ampere240Ap <- 0
    Ampere480Ap <- 0
    puissdispoAPB <- 0
    puissdispoAPBAP <- 0
    adressbat <- input$adressbat
    num_compteur <- input$num_compteur
    
    
    voltage <- input$tension
    current <- input$intensite
    num_transformateurs <- input$num_transformateurs
    num_sectionneur <- input$num_sectionneur
    if ( !is.na(num_sectionneur) ) {
      tension_sec <- numeric(num_sectionneur)
      intensite_sec <- numeric(num_sectionneur)
      nbredefois <- numeric(num_sectionneur) 
    }
    
    if (!is.na(num_transformateurs)) {
      tensions <- numeric(num_transformateurs)
      puissances <- numeric(num_transformateurs)
      phase <- numeric(num_transformateurs)
      intentransfo <-numeric(num_transformateurs)
    }
    
    
    if (!is.na(num_transformateurs) && num_transformateurs>0) {
      for (i in  1: num_transformateurs) {
        updateNumericInput(session, paste0("tension_", i), value = voltage)
      }
    }
    
    if (!is.na(num_sectionneur) && num_sectionneur>0) {
      for (i in  1: num_sectionneur) {
        updateNumericInput(session, paste0("tensionsec_", i), value = voltage)
      }
    }
    
    
    nbpart <-0
    if (!is.na(input$part_borne) && input$part_borne>0 ) {
      nbpart <- input$part_borne
    }else {
      nbpart <- 1
    }
    
    
    
    P_borne7.2 <- 7.2 * nbborne7.2*1.2
    I_borne7.2 <- 36 * nbborne7.2
    
    P_borne9.6 <- 7.2 * nbborne9.6*1.2
    I_borne9.6 <- 40 * nbborne9.6
    
    P_borne11.5 <- 11.5 * nbborne11.5*1.2
    I_borne11.5 <- 58*nbborne11.5
    
    
    P_borne19.2 <- 19.2 * nbborne19.2*1.2
    I_borne19.2 <- 96*nbborne19.2
    
    
    P_borne24_1ph <- 24 * nbborne24_1ph*1.2
    I_borne24_1ph <- 120*nbborne24_1ph
    
    P_borne24_3ph <- 24 * nbborne24_3ph*1.2
    I_borne24_3ph <- 34*nbborne24_3ph
    
    P_borne25 <- 25 * nbborne25*1.2
    I_borne25 <-35*nbborne25
    
    
    P_borne50 <- 50 * nbborne50*1.2
    I_borne50 <- 71 * nbborne50
    
    if (!is.na(input$part_borne)  && input$part_borne %% 2 == 0) {
      nb7.2 <- nbborne7.2P
      P_borne7.2P <- (7.2 * nb7.2*1.2)/nbpart
      I_borne7.2P <- 36*(nb7.2/nbpart)
      
    }else if ( input$part_borne %% 2 != 0) {
      nb7.2 <-nbborne7.2P
      valeurdiv = nb7.2 - (trunc(nb7.2/nbpart)*nbpart)
      P_borne7.2P <- 7.2 * ((trunc(nb7.2/nbpart)+valeurdiv)*1.2)
      I_borne7.2P <- 36*((trunc(nb7.2/nbpart)+valeurdiv))
    }
    
    P_borne200 <- 200 * nbborne200*1.2
    I_borne200 <- 285*nbborne200
    
    P_borne400 <- 400 * nbborne400*1.2
    I_borne400 <- 570*nbborne400
    
    
    P_borne600 <- 600 * nbborne600*1.2
    I_borne600 <- 854*nbborne600
    
    
    inten_nv2transfo = I_borne7.2+I_borne7.2P+I_borne9.6+I_borne11.5+I_borne19.2+I_borne24_1ph
    inten_nv3transfo = I_borne24_3ph+I_borne25+I_borne50+I_borne200+I_borne400+I_borne600
    puisstotal_borne <- P_borne7.2P+P_borne7.2 + P_borne9.6 + P_borne11.5 + P_borne19.2 + P_borne24_1ph + P_borne24_3ph + P_borne50 + P_borne25+P_borne200+P_borne400+P_borne600
    ## Récupération des valeurs du transformateurs
    
    kVAN3 <-c(0,6,10,15,30,45,50,75,112.5,150,225,300,450,500,600,750,1000,1500,2000)
    kvAN2 <- c(0,3,5,7.5,10,15,25,37.5,50,75,100,150,200,250,333)
    intendispo = (current*0.8)-sum(intentransfo)-sum(intensite_sec)
    puissdispo = (intendispo*voltage)/1000
    puissdispo2 <- power1 - (appuiss)
    
    
    
    
    ########## CALCUL TRIPHASEE
    
    if (input$monotri=="triphasée") {
      
      if (!is.na(num_transformateurs) ) {
        for(i in seq_len(num_transformateurs)) {
          if (!is.null(input[[paste0("tension_", i)]])){
            
            tensions[i] <- input[[paste0("tension_", i)]]
            ## tensions[i] <- input$tension
            puissances[i] <- input[[paste0("puissance_", i)]]
            phase[i] <- input[[paste0("phase_", i)]]
            if (phase[i]=="triphasée") {
              intentransfo[i] <- (puissances[i]*1000)/(tensions[i]*1.73)
            }else{
              intentransfo[i] <- (puissances[i]*1000)/(tensions[i])
            }
            
          }
        }
      }
      
      if (!is.na(num_sectionneur)) {
        for(i in seq_len(num_sectionneur)) {
          if (!is.null(input[[paste0("tensionsec_", i)]])){
            
            tension_sec[i] <- input[[paste0("tensionsec_", i)]]
            intensite_sec[i] <- input[[paste0("intensite_", i)]]*input[[paste0("nbredefois_", i)]]
            puissance_sec[i] <- (intensite_sec[i]*tension_sec[i]*1.73*0.8)/1000
            nbredefois[i] <- input[[paste0("nbredefois_", i)]]
          }
        }
      }
      
      intendispo = (current*0.8)-sum(intentransfo)-sum(intensite_sec)
      puiss_sec_total <- sum(puissance_sec)
      
      power1 <- (voltage * current * 1.73*0.8)/1000
      puissdispo2 <- power1 - appuiss
      puissdispo = (intendispo*voltage*1.73)/1000
      puissnv2transfo = (inten_nv2transfo*240*1.73)/1000
      puissnv3transfo = (inten_nv3transfo*480*1.73)/1000
      
      # if (length(inten_nv2transfo)>0 && !is.na(inten_nv2transfo) &&  inten_nv2transfo>0) {
      #   Ampere240 <-(puissdispo*1000)/(1.732*240)
      #   Ampere240Ap <- (puissdispo2*1000)/(1.732*240)
      # }else{
      #   Ampere240 <-0
      #   Ampere240Ap <-0
      # }
      # if (length(inten_nv3transfo)>0 && !is.na(inten_nv3transfo) && inten_nv3transfo>0) {
      #   Ampere480<-(puissdispo*1000)/(1.732*480)
      #   Ampere480Ap<-(puissdispo2*1000)/(1.732*480)
      # }else{
      #   Ampere480<- 0
      #   Ampere480Ap<- 0
      # }
      
      
      
      
      ##puissnv2transfo = (I_borneN2*240*1.732)/1000
      ##puissnv3transfo = (I_borneN3*480*1.732)/1000
      I_appuiss <- (appuiss*1000)/(1.732*voltage)
      
      
      ##On convertit l'intensité des bornes à l'intensité à l'entrée
      
      
      I_entreN2 <-inten_nv2transfo/2.5
      I_entreN3 <-inten_nv3transfo/1.25
      # I_entreN2 <- (puissnv2transfo*1000)/(1.732*voltage)
      # I_entreN3 <- (puissnv3transfo*1000)/(1.732*voltage)
      I_resid = intendispo - I_entreN2 - I_entreN3
      ## I_resid <- (Ampere480 + Ampere240) - (inten_nv2transfo + inten_nv3transfo)
      intendispoA <- (current*0.8) - I_appuiss
      I_residA <-intendispoA - I_entreN2 - I_entreN3
      puissdispoAPB = (voltage * I_resid * 1.73)/1000 #puissance dispo apres ajout bornes methode transformateurs
      puissdispoAPBAP = (voltage * I_residA * 1.73)/1000 #puissance dispo apres ajout bornes methode appel puissance
      ## I_residA <-(Ampere480Ap + Ampere240Ap) - (inten_nv2transfo + inten_nv3transfo)
      
      for (x in kVAN3) {
        if(!is.na(puissnv2transfo) && length(puissnv2transfo)>0 && x >= puissnv2transfo){
          puissnv2transfo2=x          
          break
        }
      } 
      for (Y in kVAN3) {
        if(!is.na(puissnv3transfo) &&length(puissnv3transfo) && Y >= puissnv3transfo){
          puissnv3transfo2= Y
          break
        }
      }
      
      ajout_intens <- (inten_nv2transfo/2.5) + (inten_nv3transfo/1.25)
      ajout_intensNE <- ((inten_nv2transfo/2.5) + (inten_nv3transfo/1.25))*1.2
      # ajout_intens <- (((puissnv2transfo2 + puissnv3transfo2)*1000)/(1.732*voltage))
      # ajout_intensNE <- (((puissnv2transfo2 + puissnv3transfo2)*1000)/(1.732*voltage))*1.2
    } 
    
    
    
    ########## CALCUL MONOPHASEE
    
    else {
      
      for(i in seq_len(num_transformateurs)) {
        if (!is.null(input[[paste0("tension_", i)]])){
          tensions[i] <- input[[paste0("tension_", i)]]
          puissances[i] <- input[[paste0("puissance_", i)]]
          phase[i] <- input[[paste0("phase_", i)]]
          
          if (phase[i]=="triphasée") {
            intentransfo[i] <- (puissances[i]*1000)/(tensions[i]*1.73)
          }else{
            intentransfo[i] <- (puissances[i]*1000)/(tensions[i])
          }
        }}
      
      for(i in seq_len(num_sectionneur)) {
        if (!is.null(input[[paste0("tensionsec_", i)]])){
          
          tension_sec[i] <- input[[paste0("tensionsec_", i)]]
          intensite_sec[i] <- input[[paste0("intensite_", i)]]*input[[paste0("nbredefois_", i)]]
          puissance_sec[i] <- (intensite_sec[i]*tension_sec[i]*0.8)/1000
          nbredefois[i] <- input[[paste0("nbredefois_", i)]]
        }
      }
      
      power1 = (voltage * current*0.8)/1000
      puissdispo2 <- power1 - appuiss
      
      intendispo = (current*0.8)-sum(intentransfo)-sum(intensite_sec)
      puiss_sec_total <- sum(puissance_sec)
      puissdispo <- (intendispo*voltage)/1000
      puissnv2transfo = (inten_nv2transfo*240)/1000
      puissnv3transfo = (inten_nv3transfo*480)/1000
      I_appuiss <- (appuiss*1000)/(voltage)
      
      # if (length(inten_nv2transfo)>0 && !is.na(inten_nv2transfo) &&  inten_nv2transfo>0) {
      #   Ampere240 <-(puissdispo*1000)/(240)
      #   Ampere240Ap <-(puissdispo2*1000)/(240)
      # }else{
      #   Ampere240 <-0
      #   Ampere240Ap <-0
      # }
      # if (length(inten_nv3transfo)>0 && !is.na(inten_nv3transfo) && inten_nv3transfo>0) {
      #   Ampere480<-(puissdispo*1000)/(480)
      #   Ampere480Ap<-(puissdispo2*1000)/(480)
      # }else{
      #   Ampere480<- 0
      #   Ampere480Ap<- 0
      # }
      
      
      
      I_entreN2 <-inten_nv2transfo/2.5
      I_entreN3 <-inten_nv3transfo/1.25
      # I_entreN2 <- (puissnv2transfo*1000)/(voltage)
      # I_entreN3 <- (puissnv3transfo*1000)/(voltage)
      I_resid = intendispo- I_entreN2 - I_entreN3
      ##I_resid = (Ampere480 + Ampere240) - (inten_nv2transfo + inten_nv3transfo)
      intendispoA <- (current*0.8) - I_appuiss
      I_residA <- intendispoA - I_entreN2 - I_entreN3
      puissdispoAPB = (voltage * I_resid)/1000 #puissance dispo apres ajout bornes methode transformateurs
      puissdispoAPBAP = (voltage * I_residA)/1000 #puissance dispo apres ajout bornes methode appel puissance
      ##I_residA <- (Ampere480Ap + Ampere240Ap) - (inten_nv2transfo + inten_nv3transfo)
      
      for (Y in kvAN2) {
        if(!is.na(puissnv2transfo) && length(puissnv2transfo) && Y >=puissnv2transfo){
          puissnv2transfo2= Y
          break
        }
      }
      for (Y in kvAN2) {
        if(!is.na(puissnv3transfo) && length(puissnv3transfo) && Y >=puissnv3transfo){
          puissnv3transfo2= Y
          break
        }
      }
      ajout_intens <- (inten_nv2transfo/2.5) + (inten_nv3transfo/1.25)
      ajout_intensNE <- ((inten_nv2transfo/2.5) + (inten_nv3transfo/1.25))*1.2
      
      # ajout_intens <- (((puissnv2transfo2 + puissnv3transfo2)*1000)/voltage)
      # ajout_intensNE <- (((puissnv2transfo2 + puissnv3transfo2)*1000)/voltage)*1.2
    }
    
    output$result <- renderText(
      
      paste0("Intensité Transfo: \n",voltage, " V @ ", round(sum(intentransfo),2), " A")
    )
    
    
    if (!is.na(appuiss) && appuiss==0) {
      intendispoA = 0
      I_residA = 0
      puissdispo2 = 0
    }
    if ( !is.na(current) && !is.na(I_resid) && !is.na(puisstotal_borne) && puisstotal_borne>0   && current == 0) {
      ajout_intens = 0
      I_resid = 0
    }
    
    if (!is.na(voltage) && voltage == 240) {
      updateNumericInput(session, "B24kW3ph", value = 0 )
      updateNumericInput(session, "B25kW", value = 0 )
      updateNumericInput(session, "B50kW", value = 0 )
      updateNumericInput(session, "B200kW", value = 0 )
      updateNumericInput(session, "B400kW", value = 0 )
      updateNumericInput(session, "B600kW", value = 0 )
      inten_nv2transfo = 0
      puissnv2transfo2 = 0
    }
    
    # if (!is.na(puissdispo) && !is.na(puissdispo2) && puissdispo < 0 | puissdispo2 < 0) {
    #   puissdispo = 0
    #   puissdispo2 = 0
    # }
    
    
    if (!is.na(num_transformateurs) && num_transformateurs==0) {
      if (!is.na(I_resid) && !is.na(I_residA) && appuiss > 0 && I_residA < I_resid) {
        showModal(modalDialog(
          title = "Erreur",
          tags$p("Erreur: L'intensité residuelle par la methode de l'appel de puissance ne doit pas être inferieure
      à l'intensité residuelle par la methode des transformateurs. Veuillez revoir les informations entrées.",
                 align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: red;"),
          easyClose = FALSE,
          footer = modalButton("Dismiss"),
          fade = TRUE
        ))
        puissdispo = 0
        puissdispo2 = 0
      }
    }else {
      if (!is.na(I_resid) && !is.na(I_residA) && !is.na(sum(intentransfo)) && sum(intentransfo) > 0 && I_residA < I_resid) {
        showModal(modalDialog(
          title = "Erreur",
          tags$p("Erreur: L'intensité residuelle par la methode de l'appel de puissance ne doit pas être inferieure
      à l'intensité residuelle par la methode des transformateurs. Veuillez revoir les informations entrées.",
                 align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: red;"),
          easyClose = FALSE,
          footer = modalButton("Dismiss"),
          fade = TRUE
        ))
        puissdispo = 0
        puissdispo2 = 0
      }
    }
    
   
    
    # if (!is.na(I_resid) && !is.null(I_resid) && I_resid < 0 ) {
    #   showModal(modalDialog(
    #     title = "ATTENTION",
    #     tags$p("Attention: L'entrée electrique est pleine. Veuillez envisager l'ajout de charge ou 
    #            installer une nouvelle entrée electrique",
    #            align="center", style = "font-family: 'Times', Helvetica ;
    #   font-weight: 700; font-size: 12px;color: red;"),
    #     easyClose = FALSE,
    #     footer = modalButton("Dismiss"),
    #     fade = TRUE
    #   ))
    # }
    
    # Réaction au bouton "Fermer" de la fenêtre modale
    # observeEvent(input$fermerModal, {
    #   # Fermeture de la fenêtre modale
    #   removeModal()
    # })
    
    # Rendu du message d'erreur dans la fenêtre modale
    # output$messageErreur <- renderText({
    #   tags$p("Erreur: L'intensité residuelle par la methode de l'appel de puissance ne doit pas être superieure
    #     à l'intensité residuelle par la methode des transformateurs. Veuillez revoir les informations entrées.",
    #     align="center", style = "font-family: 'Times', Helvetica ;
    #   font-weight: 700; font-size: 12px;color: red;")
    #   
    # })
    
    #Box 1
    output$box1 <- renderValueBox({
      if (!is.null(puissdispo)){
        valueBox(
          value = tags$p(paste0(round(puissdispo), " kVA"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
          subtitle = tags$p("Puissance Disponible",style= "color:black;"),
          icon = icon("bolt"),
          color = "green"
        )}
    })
    
    output$box2 <- renderValueBox({
      if (!is.null(puisstotal_borne)){
        valueBox(
          ##   value = paste0(round(inten_nv2transfo+inten_nv3transfo,2), " A"),
          value = tags$p(paste0(round(inten_nv2transfo+inten_nv3transfo), " A"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
          subtitle = tags$p("Intensité des bornes", style = "color:black;"),
          icon=icon("plug"),
          color = "green"
        )}
    })
    
    
    output$box3 <- renderValueBox({
      if (!is.null(puissdispo)){
        valueBox(
          ##    value = paste0(voltage, " V @ ",round(I_resid,2), " A"),
          value = tags$p(paste0(round(I_resid), " A@", voltage, " V"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
          subtitle = tags$p("Intensité Residuelle", style = "color:black;"),
          icon = icon("bolt"),
          color = "green"
        )}
    })
    
    output$box4 <- renderValueBox({
      if (!is.null(power)){
        valueBox(
          ##value = paste0(round(power1,2), " kVA"),
          ##value=paste0(voltage," V ","@ ",round(intendispo,2)," A"," \n"),
          value = tags$p(paste0(round(intendispo)," A@", voltage," V"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
          subtitle = tags$p("Intensité disponible", style = "color:black"),
          icon = icon("bolt"),
          color = "green"
        )}
    })
    
    
    
    output$box5 <- renderValueBox({
      
      if (!is.null(power1)){
        valueBox(
          ## value = paste0(voltage," V ","@ ",round(intendispoA,2)," A"," \n"),
          value = tags$p(paste0(round(intendispoA)," A@", voltage," V"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"), 
          subtitle = tags$p("Intensité disponible", style = "color:black"),
          icon = icon("bolt"),
          color = "yellow"
        )}
    })
    
    output$box6 <- renderValueBox({
      if (!is.null(puisstotal_borne)){
        valueBox(
          ##value = paste0(round(inten_nv2transfo+inten_nv3transfo,2), " A"),
          value = tags$p(paste0(round(inten_nv2transfo+inten_nv3transfo), " A"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
          subtitle = tags$p("Intensité des bornes", style = "color:black"),
          icon=icon("plug"),
          color = "yellow"
        )}
    })
    
    output$box7 <- renderValueBox({
      if (!is.null(puissdispo2)){
        valueBox(
          ##value = paste0(voltage, " V @ ",round(I_residA,2), " A"),
          value = tags$p(paste0(round(I_residA), " A@", voltage, " V"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
          subtitle = tags$p("Intensité Residuelle", style = "color:black"),
          icon = icon("bolt"),
          color = "yellow"
        )}
    })
    
    output$box8 <- renderValueBox({
      if (!is.null(puissdispo2)){
        valueBox(
          ## value = paste0(round(puissdispo2,2), " kVA"),
          value = tags$p(paste0(round(puissdispo2), " kVA"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
          subtitle = tags$p("Puissance disponible", style = "color:black"),
          icon = icon("bolt"),
          color = "yellow"
        )}
    })
    
    
    
    output$box9 <- renderValueBox({
      
      valueBox(
        ## value = paste0(round(puissnv2transfo2,2), " kVA"),
        value = tags$p(paste0(round(puissnv2transfo2), " kVA"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
        subtitle = tags$p("Puissance du transformateur", style = "color:black"),
        color = "green"
      )
    })
    
    output$box10 <- renderValueBox({
      if (!is.null(inten_nv2transfo)){
        valueBox(
          ##value = paste0(round(inten_nv2transfo,2), " A"),
          value = tags$p(paste0(round(inten_nv2transfo), " A"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
          subtitle = tags$p("Intensité du transformateur", style = "color:black"),
          color = "green"
        )}
    })
    
    output$box11 <- renderValueBox({
      
      valueBox(
        ##value = paste0(round(puissnv3transfo2,2), " kVA"),
        value = tags$p(paste0(round(puissnv3transfo2), " kVA"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
        subtitle = tags$p("Puissance du transformateur", style = "color:black"),
        color = "yellow"
      ) 
    })
    
    output$box12 <- renderValueBox({
      if (!is.null(inten_nv3transfo)){
        valueBox(
          ##  value = paste0(round(inten_nv3transfo,2), " A"),
          value = tags$p(paste0(round(inten_nv3transfo), " A"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
          subtitle = tags$p("Intensité du transformateur", style = "color:black"),
          color = "yellow"
        )}
    })
    
    if (!is.null(I_resid) && !is.na(I_resid) && I_resid<0 | !is.null(I_residA) && !is.na(I_residA) && appuiss>0 && I_residA<0 ) {
      ajout_intens2 <- ajout_intens
    }else{
      
      ajout_intens2 <- 0
    }
    output$box13 <- renderValueBox({
      valueBox(
        value = tags$p(paste0(round(ajout_intens2), " A"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
        subtitle = tags$p("Intensité", style = "color:black"),
        color = "blue"
      )
    })
    
    output$box14 <- renderValueBox({
      valueBox(
        value = tags$p(paste0(round(voltage), " V"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
        subtitle = tags$p("Tension", style = "color:black"),
        color = "blue"
      )
    })
    
    if (!is.na(puisstotal_borne) && !is.null(puisstotal_borne) && !is.null(current) && !is.na(current) && current==0 && puisstotal_borne>0 ) {
      ajout_intensNE2 <-ajout_intensNE
    }else{
      ajout_intensNE2 <- 0
    }
    
    output$box15 <- renderValueBox({
      valueBox(
        value = tags$p(paste0(round(ajout_intensNE2), " A"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
        subtitle = tags$p("Intensité", style = "color:black"),
        color = "blue"
      )
    })
    
    output$box16 <- renderValueBox({
      valueBox(
        value = tags$p(paste0(round(voltage), " V"), align="center", style = "font-family: 'Times', Helvetica ;
    font-weight: 700; font-size: 12px;color: black;"),
        subtitle = tags$p("Tension", style = "color:black"),
        color = "blue"
      )
    })
    
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      ## filename = paste0(input$batiment,".pdf"),
      filename = function() {
        paste("Calcul de charge ",input$batiment, ".pdf", sep = "")
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(b1 = round(puissdispo), b2=round(inten_nv2transfo+inten_nv3transfo), b3=round(I_resid), b4=round(intendispo),
                       b5=round(intendispoA), b6=round(I_residA), b7=voltage, b8=round(puissdispo2), b9=round(puissnv2transfo2), b10= round(inten_nv2transfo),
                       b11=round(puissnv3transfo2), b12=round(inten_nv3transfo), b14=nbborne7.2, b14bis=nb7.2,  b15=nbpart, b16=nbborne11.5,
                       b17=nbborne19.2, b18=nbborne24_1ph, b19=nbborne24_3ph, b20=nbborne25, b21=nbborne50, b22=nbborne200, b23=nbborne400,
                       b24=nbborne600, b25=round(puisstotal_borne), b26=round(ajout_intens2), b27=round(ajout_intensNE2), b28 = current, b29=voltage, num_transformateurs = num_transformateurs,
                       tensions = tensions, puissances = puissances, num_sectionneur = num_sectionneur, intensite_sec = intensite_sec/nbredefois, nbredefois_sec = nbredefois,
                       tension_sec = tension_sec, b30 = apuiss, b31 = input$choicephase, b32 = input$date, b33 = round(sum(intentransfo)), b34 = sum(intensite_sec), b35 = round(intendispo),
                       b36 = round(intendispoA), b37 = round(puissdispoAPB), b38 = round(puissdispoAPBAP), b39=adressbat, b40 = num_compteur)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    # Fonction pour charger les données de l'entrée electrique dans l'interface
    loadData <- function() {
      # Ouvrir une boîte de dialogue pour sélectionner le fichier Excel
      ## file <- file.choose()
      # Charger la base de données à partir du fichier Excel
      ## data <- read_excel(file)
      data <- read_excel(input$load$datapath)
      # Affecter les valeurs de la base de données dans les champs de l'interface Shiny
      # updateTextInput(session, "tension", value = as.character(data$tension))
      # updateTextInput(session, "intensite", value = as.character(data$intensite))
      # Affecter les valeurs des champs de l'interface avec les données importées
      updateNumericInput(session, "tension", value = as.numeric(data$tension))
      updateNumericInput(session, "intensite", value = as.numeric(data$intensite))
      updateSelectInput(session, "monotri", selected = as.character(data$monotri))
      updateNumericInput(session, "num_transformateurs", value = as.numeric(data$num_transformateurs))
      updateNumericInput(session, "num_sectionneur", value = as.numeric(data$num_sectionneur))
      
      
      for (i in  1: data$num_transformateurs) {
        updateNumericInput(session, paste0("tension_",i), value = as.numeric(data[[paste0("tension_",i)]]))
        updateNumericInput(session, paste0("puissance_",i), value = as.numeric(data[[paste0("puissance_",i)]]))
        updateNumericInput(session, paste0("tension_sorti_",i), value = as.numeric(data[[paste0("tension_sorti_",i)]]))
        ## updateNumericInput(session, paste0("tension_", i), value = as.numeric(data[[paste0("puissance_",i)]]))
      }
      
      
      for (i in seq_len(data$num_sectionneur)) {
        updateNumericInput(session, paste0("tensionsec_", i), value = as.numeric(data[[paste0("tensionsec_",i)]]))
        updateNumericInput(session, paste0("intensite_", i), value = as.numeric(data[[paste0("intensite_",i)]]))
        updateNumericInput(session, paste0("nbredefois_", i), value = as.numeric(data[[paste0("nbredefois_",i)]]))
      }
    }
    
    
    
    reactiveInput <- reactive({
      input$choicephase
    })
    
    donnees <- reactive({
      req(input$loadappui)
      fichier <- input$loadappui
      if (is.null(fichier))
        return(NULL)
      read_excel(fichier$datapath)
    })
    
    if(!is.null(input$loadappui))
    {
      # Plot 1
      output$plot1 <- renderPlot({
        donnees <- donnees()
        donnees <- na.omit(donnees)
        donnees$heure <- format(donnees[['Date et heure']], "%H")
        Q3 <- quantile(donnees$`Puissance réelle (kW)`, probs=0.75, na.rm = TRUE)
        datagraph <- filter(donnees, `Puissance réelle (kW)` >= Q3)
        ggplot(datagraph) + geom_line(aes(x = heure, y = `Puissance réelle (kW)`), color="darkblue") +
          theme_minimal()
      })
    }
    
    # Fonction pour charger les donnees de l'appel de puissance
    loadDatapuiss <- function() {
      datapui <- read_excel(input$loadappui$datapath)
      datapui <- na.omit(datapui)
      updateNumericInput(session, "apppuiss", value = max(as.numeric(datapui[[3]])))
      
    }
    
    ### Charger les donnees des bornes
    
    choiceborne <- c()
    intenPhase1 <- reactiveValues(intens = NULL)
    intenPhase2 <- reactiveValues(intens2 = NULL)
    bornedispo <- c("B7.2kW","B7.2kWPart", "part_borne",	"B11.5kW",	"B19.2kW",	"B24kW1ph",	"B24kW3ph",	"B25kW",	"B50kW",	"B200kW",	"B400kW",	"B600kW")
    loadDataborne <- function(phase) {
      choicephase2 <- reactiveInput()
      databorne <- read_excel(input$loadborne$datapath)
      updateSelectInput(session, "choicephase2", choices =unique(databorne$phase))
      colname <- colnames(databorne)
      for (i in 2:13) {
        if (sum((databorne[[colname[i]]]), na.rm = TRUE)>0) {
          choiceborne[i] = colname[i]
        }
      }
      choiceborne = as.character(na.omit(choiceborne))
      for (j in 1:length(bornedispo)) {
        for (i in 1:length(choiceborne)) {
          if (bornedispo[j]==choiceborne[i]) {
            updateNumericInput(session, bornedispo[j] , value = sum(databorne[[bornedispo[j]]][as.character(databorne$phase) == phase], na.rm = TRUE))
          }
          
        }
      } 
      
      intenPhase1$intens <- round(I_resid)
      intenPhase2$intens2 <- round(I_residA)
      
    }
    
    
    # observeEvent(input$loadborne, {
    #   databorne <- read_excel(input$loadborne$datapath)
    #   updateSelectInput(session, "choicephase", choices =unique(databorne$phase))
    #   loadDataborne(phase = input$choicephase)
    #   uniqueborne <- as.character(unique(databorne$phase))
    #   for (i in 1:length(uniqueborne) ) {
    #     if (input$choicephase==uniqueborne[i]) {
    #       loadDataborne(phase = uniqueborne[i])
    #     }
    #   }
    # })
    
    # Charger les donnees de l'entree electrique
    observeEvent(input$load, {
      loadData()
    })
    
    # Charger les donnees de l'appel de puissance
    observeEvent(input$loadappui, {
      loadDatapuiss()
    })
    observeEvent(input$btn_menu, {
      # Passer à l'onglet 2 en utilisant la fonction `showTab`
      updateTabItems(session, "tabs", "calcharge")
    })
    
    if (!is.null(input$loadborne)) {
      observeEvent(input$affich, {
        choicephase2 <- reactiveInput()
        
        
        if (choicephase2==1) {
          loadDataborne(phase=1)
          
        }else {
          updateNumericInput(session, "intensite", value =intenPhase1$intens )
          updateNumericInput(session, "num_transformateurs", value = 1)
          updateNumericInput(session, "puissance_1", value = 0)
          I_residA <- 0
          loadDataborne(phase=choicephase2)
        }
        
        
      })
    }
    
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
