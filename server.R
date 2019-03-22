library(shiny)
library(leaflet)
library(magrittr)
library(shinydashboard)
library(flexdashboard)
library(geosphere)
library(opencage)
library(sf)
library(dplyr)


#options(encoding = "UTF-8")

targetFile = 'C:\\Users\\saidk\\Desktop\\workspace\\R\\training\\data\\apae-data.csv'

df <- read.csv2(file = targetFile, header = TRUE, sep = ";", encoding = "UTF-8")
# print(typeof(df$longitude))
# print(df$latitude[1:10])
# df$longitude <- as.numeric(df$longitude)
# df$latitude <- as.numeric(df$latitude)
# print(is.numeric(df$longitude))

# print(typeof(df$longitude))
# print(df$longitude[1:10])

findCoordinate <- function(adresse){
  query_adresse = opencage_forward(adresse
                                   , key = "96743c95bc754fc6be57987a4a0da611"
                                   , countrycode = "FR")
  adresse_mercator = data.frame(lat = as.integer(query_adresse$results$annotations.Mercator.x[1])
                                , lng = as.integer(query_adresse$results$annotations.Mercator.y[1]))
  adresses_wgs84 =st_coordinates(st_transform(st_as_sf(adresse_mercator
                                                       , coords = c("lat","lng")
                                                       , crs = 54004)
                                              ,4326))
  return(adresses_wgs84)
}

getIndicatorsSet <- function(dfIn, theme, anneeIn){
  #print(paste0("Current working dir: ", anneeIn, typeof(anneeIn), sapply(dfIn, class)))
  if (theme == 'identity'){
    dfSelected = dfIn %>%
             select(X.U.FEFF.etab, longitude, coordonnee_x, latitude, coordonnee_y, annee, commune, secteur, nom_norme, ep, effectif_1er_cycle, nb_el_6eme, nb_el_5eme)
    dfFiltred = subset(dfSelected, annee==anneeIn)
    #print(paste0("6 th column dfFiltred: ", dfFiltred[9], colnames(dfFiltred)))
    return(dfFiltred)
  } else if (theme == 'RH'){
    dfSelected = dfIn %>%
             select(X.U.FEFF.etab, longitude, coordonnee_x, latitude, coordonnee_y, annee, commune, secteur, nom_norme, ep, etp_enseignants, age_ens_moy, ens_inf_35_ans)
    dfFiltred = subset(dfSelected, annee == anneeIn)
    return(dfFiltred)
  } else {
    dfSelected = dfIn %>%
             select(X.U.FEFF.etab, longitude, coordonnee_x, latitude, coordonnee_y, annee, commune, secteur, nom_norme, ep, taux_reussite_dnb, note_ecrit_dnb, redoublement_6eme)
    dfFiltred = subset(dfSelected, annee == anneeIn)
    return(dfFiltred)
  }
}


getIndicatorsSetColleges <- function(dfInCollege1, theme, anneeIn, dfInCollege2){
  # print(paste0("Current working dir: ", dfInCollege1[9], colnames(dfInCollege1)))
  #dfIn = rbind(dfInCollege1, dfInCollege2)
  #print(paste0("Current working dir: ", dfIn, typeof(dfIn)))
  if (theme == 'identity'){
    dfSelectedCol1 = dfInCollege1 %>%
      select(X.U.FEFF.etab, longitude, coordonnee_x, latitude, coordonnee_y, annee, commune, secteur, nom_norme, ep, effectif_1er_cycle, nb_el_6eme, nb_el_5eme)
    dfFiltredCol1 = subset(dfSelectedCol1, annee==anneeIn)
    dfSelectedCol2 = dfInCollege2 %>%
      select(X.U.FEFF.etab, longitude, coordonnee_x, latitude, coordonnee_y, annee, commune, secteur, nom_norme, ep, effectif_1er_cycle, nb_el_6eme, nb_el_5eme)
    dfFiltredCol2 = subset(dfSelectedCol2, annee==anneeIn)
    #print(paste0("6 th column dfFiltred1: ", dfFiltredCol1[9], colnames(dfFiltredCol1)))
    return(c(dfFiltredCol1, dfFiltredCol2))
  } else if (theme == 'RH'){
    dfSelectedCol1 = dfInCollege1 %>%
      select(X.U.FEFF.etab, longitude, coordonnee_x, latitude, coordonnee_y, annee, commune, secteur, nom_norme, ep, etp_enseignants, age_ens_moy, ens_inf_35_ans)
    dfFiltredCol1 = subset(dfSelectedCol1, annee == anneeIn)
    dfSelectedCol2 = dfInCollege2 %>%
      select(X.U.FEFF.etab, longitude, coordonnee_x, latitude, coordonnee_y, annee, commune, secteur, nom_norme, ep, etp_enseignants, age_ens_moy, ens_inf_35_ans)
    dfFiltredCol2 = subset(dfSelectedCol2, annee == anneeIn)
    return(c(dfFiltredCol1, dfFiltredCol2))
  } else {
    dfSelectedCol1 = dfInCollege1 %>%
      select(X.U.FEFF.etab, longitude, coordonnee_x, latitude, coordonnee_y, annee, commune, secteur, nom_norme, ep, taux_reussite_dnb, note_ecrit_dnb, redoublement_6eme)
    dfFiltredCol1 = subset(dfSelectedCol1, annee == anneeIn)
    dfSelectedCol2 = dfInCollege2 %>%
      select(X.U.FEFF.etab, longitude, coordonnee_x, latitude, coordonnee_y, annee, commune, secteur, nom_norme, ep, taux_reussite_dnb, note_ecrit_dnb, redoublement_6eme)
    dfFiltredCol2 = subset(dfSelectedCol2, annee == anneeIn)
    return(c(dfFiltredCol1, dfFiltredCol2))
  }
}
# 
# getDfSelectTheme <- function(theme){
#   if (theme == 'identity'){
#     return(df %>%
#              select(X.U.FEFF.etab, longitude, coordonnee_x, latitude, coordonnee_y, annee, commune, secteur, nom_norme, ep, effectif_1er_cycle, nb_el_6eme, nb_el_5eme))
#   } else if (theme == 'RH'){
#     return(dfSelectTheme = df %>%
#              select(X.U.FEFF.etab, longitude, coordonnee_x, latitude, coordonnee_y, annee, commune, secteur, nom_norme, ep, etp_enseignants, age_ens_moy, ens_inf_35_ans))
#   } else {
#     return(dfSelectTheme = df %>%
#              select(X.U.FEFF.etab, longitude, coordonnee_x, latitude, coordonnee_y, annee, commune, secteur, nom_norme, ep, taux_reussite_dnb, note_ecrit_dnb, redoublement_6eme))
#   }
# }

server <- function(input, output, session){
  
  datasetInput <- reactive({
    dfSecteur <- df %>% filter(secteur %in% input$secteur)
    dfOutput <- getIndicatorsSet(dfSecteur, input$theme, input$year)

  })
  
  getDataEcole <- reactive({
    
  })
  datasetInputGauge <- reactive({
    # print(paste0("College 1: ", input$college1, colnames(df)))
    uaiCollege1 <- df$X.U.FEFF.etab[df$nom_norme==input$college1]
    # print(paste0("uai College 1: ", uaiCollege1))
    dfCollege1 <- df %>% filter(X.U.FEFF.etab %in% uaiCollege1)
    # print(paste0("df College 1: ", dfCollege1))
    uaiCollege2 <- df$X.U.FEFF.etab[df$nom_norme==input$college2]
    dfCollege2 <- df %>% filter(X.U.FEFF.etab %in% uaiCollege2)
    dfColleges <- getIndicatorsSetColleges(dfCollege1, input$theme, input$year, dfCollege2)
    
  })
  
  # dfSelectTheme <- reactive({
  #   DfSelect <- getDfSelectTheme(input$theme)
  # })
  
  # description <- reactive({
  #   
  # })
  output$leafletMap <- renderLeaflet({
    coordinate <- findCoordinate(input$adresse)
    latitudeInit <- coordinate[2]
    longitudeInit <- coordinate[1]
    
    dfInput <- datasetInput()
    col1 <- colnames(dfInput)[11]
    col2 <- colnames(dfInput)[12]
    col3<- colnames(dfInput)[13]
    #print(dfInput$nom_norme)
    #print(head(dfInput, 15))
    

    map <- leaflet() %>% 
      addTiles() %>% 
      setView(longitudeInit, latitudeInit, zoom = 13) %>% 
      addCircles(lat = as.numeric(latitudeInit)
                 , lng = as.numeric(longitudeInit)
                 , radius = input$rayon)%>%
      addMarkers(data = dfInput, lng = ~ dfInput$longitude, lat = ~ dfInput$latitude, popup = paste("College:", dfInput$nom_norme, 
                                                                                                    "<br>", paste0(col1, ' : ', sep = ""), unlist(dfInput[11]),
                                                                                                    "<br>", paste0(col2, ' : ', sep = ""), unlist(dfInput[12]), 
                                                                                                    "<br>", paste0(col3, ' : ', sep = ""), unlist(dfInput[13])))
    
    map
  })
  

  output$plt1 <- flexdashboard::renderGauge({
    datasetInputColleges <- datasetInputGauge()
    #dfSelectedTheme <- dfSelectTheme()
    value <- as.double(unlist(datasetInputColleges[11]))
    valueMax <- as.double(max(df[11], na.rm = TRUE))
    valueMin <- as.double(min(df[11], na.rm = TRUE))
    print(paste0("Dataset input College: ", datasetInputColleges$nom_norme, c(valueMin, valueMax)))
    gauge(value, min = valueMin, max = valueMax, symbol = '', label = paste("Eleve"),gaugeSectors(
      success = c(valueMax*0.5, valueMax), warning = c(valueMax*0.2,valueMax*0.5), danger = c(valueMin, valueMax*0.2)
    ))
    
  })
}