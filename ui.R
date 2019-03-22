library(shiny)
library(leaflet)
library(magrittr)
library(shinydashboard)
library(flexdashboard)


targetFile = 'C:\\Users\\saidk\\Desktop\\workspace\\R\\training\\data\\apae-data.csv'

# targetFile = 'C:\Users\saidk\Desktop\workspace\R\training\data\apae-data.csv'


data <- read.csv2(targetFile, sep = ";", encoding = "UTF-8")
# 
# print(typeof(data$longitude))
# print(data$latitude[1:10])
# 
# as.numeric(as.character(df$longitude))
# 
# print(is.numeric(df$longitude))
# print(data$secteur[1:10])

ui <- fluidPage(
  titlePanel("La carte d'identité scolaire"),
  sidebarLayout(
    sidebarPanel(
    textInput("adresse", "Adresse : ", value = "126 Rue du Dr Albert Barraud, 33000 Bordeaux"),
      
      sliderInput("year", 
                  "Année : ",
                  min = min(data[6]),
                  max = max(data[6]),
                  value = min(data[6]),
                  step = 1, sep=""),
     sliderInput("rayon",
                  "Rayon :",
                  min = 100,
                  max = 10000,
                  value = 1000),
      radioButtons("theme", "choisir le theme pour les collegues", choices = c("identity", "RH", "performances")),
    
      checkboxGroupInput("secteur",
                         "Secteur du collègue : ",
                         c("prive", "public"), selected = "public"),
      selectInput("collegue",
                  "Selectioner le collègue que vous souhaitez voir : ",
                  choices = sort(unique(unlist(data[9]))),
                  selected = NULL,
                  multiple = TRUE),
    h3("Selectionner les 2 collègues à comparer : "),
      selectInput("college1",
                  "Selectioner le 1er collègue : ",
                  choices = sort(unique(unlist(data[9])))),
      selectInput("college2",
                  "Selectioner le 2em collègue : ",
                  choices = sort(unique(unlist(data[9]))))
  
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("map", h1("carte interactive"), leafletOutput("leafletMap", width = "100%", height="600")),
        tabPanel("comparaison", h1("comparaison des écoles"), dashboardBody(
          fluidRow(
          column(4,box(flexdashboard::gaugeOutput("plt1"),flexdashboard::gaugeOutput("plt12"),flexdashboard::gaugeOutput("plt13"),width=10,title="Indetite Ecole",background ="green")),
          column(4,box(flexdashboard::gaugeOutput("plt2"),flexdashboard::gaugeOutput("plt22"),flexdashboard::gaugeOutput("plt23"),width=10,title="Ressources Humaines",background ="green")),
          column(4,box(flexdashboard::gaugeOutput("plt3"),flexdashboard::gaugeOutput("plt32"),flexdashboard::gaugeOutput("plt33"),width=10,title="Performances Eleves",background ="green"))))),
        tabPanel("collegiens", h1("Performances des collégiens"), plotOutput("bar",height = 500))
      )
    )
  )
) 