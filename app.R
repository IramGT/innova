## LIBRARIES

library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(viridis)
library(readr)
library(utils)
library(shiny)
library(bslib)
library(shinyjs)
library(RColorBrewer)
library(scales)
library(lattice)
library(DT)

## DATA BASES IN GOOGLE SHEETS

seccionCSV <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSRiEMncSYN59z1lcOSDVmfm7bi1l3esl5Wc8fUMWW7c6ZFgUJaX3tZetQQKmB4Vgi87XF8V96KEQMZ/pub?gid=1588389477&single=true&output=csv")
Objetivos <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRLELZEMRj9seabP6vCTMvfZ4Z4RMvMtW_NwGduzmT8QS9oRhHZaE6m_9ea9aLeYK6EWoRDaCwIKjRZ/pub?gid=0&single=true&output=csv")
Revocacion <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRK5RiWe_Ijt1KV1wfIpM24tc1EimnzsEuvfFMLIIT0uxjGxGwTxZFPqLC2bR1WALQ4N3Qy_gJlTBgc/pub?gid=843665158&single=true&output=csv")
seccionesHidalgo <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS7bZ9fVqxd2Yl5HtsS-6kXvWAkv0vgBaN6Zb9v8cQzwZSwLumSBSY-rATijw5NDg_1IcipjeDoprBL/pub?gid=0&single=true&output=csv")
seccionApp <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQlBcu3mg4wYIR2YPAeoV1S8oHRmaLk_g75J-eOeIVs8g2f0u5HpAXmmfQRbHG7d_lp2WnI1qba1frj/pub?gid=0&single=true&output=csv")


## SHAPE FILE
shp_seccion <- read_sf(dsn = "seccionHidalgo", layer = "SeccionHidalgo")

shp_seccion_rev <- read_sf(dsn = "seccionHidalgo", layer = "SeccionHidalgo")

## CHANGE THE NAME OF THE SECCION DATA BASE

seccionCSV <- seccionCSV %>% 
  rename(MarcaTemporal = `Marca temporal`, Promovente = `Nombre completo del promotor`, Promovido = `Nombre completo del promovido en orden de APELLIDO PATERNO, MATERNO Y NOMBRE(S)`,
         Celular = `Celular del promovido`, CPostal = `CODIGO POSTAL de la credencial de elector`) 

## THE VARIABLE IS NEEDED AS NUMERIC FOR THE LEFT JOIN

seccionCSV$seccion <- as.numeric(seccionCSV$seccion)
seccionCSV$Celular <- as.numeric(seccionCSV$Celular)
seccionApp$Celular <- as.numeric(seccionApp$Celular)
seccionCSV$CPostal <- as.numeric(seccionCSV$CPostal)
seccionApp$CPostal <- as.numeric(seccionApp$CPostal)

seccionesHidalgo$sede <- as.numeric(seccionesHidalgo$sede)

seccionCSV <- bind_rows(seccionCSV, seccionApp)


## LEFT JOIN DATA BASE AND THE SHAPEFILE

shp_seccion <- seccionCSV %>%
  group_by(seccion) %>% 
  summarise(RESULTADOS = n()) %>% 
  left_join(shp_seccion, ., by = 'seccion') %>% 
  left_join(Objetivos, by = 'seccion')

shp_seccion_rev <- Revocacion %>% 
  group_by(seccion) %>% 
  summarise(Siga = sum(Siga), Revoque = sum(Revoque), Total = sum(Total), ListaNominal = sum(ListaNominal)) %>% 
  mutate(Participacion = round( 100*Total / ListaNominal , 1)) %>% 
  left_join(seccionesHidalgo, ., by = c("sede" = "seccion")) %>%
  dplyr::select(-MunicipioN) %>% 
  left_join(shp_seccion_rev, ., by = 'seccion')


#### UI ####


ui <- (
  # Choices for drop-downs
  navbarPage(h4("INNOVA", style='color:#B3282D'), 
             id="nav",
             tabPanel("PROMOVIDOS", 
                      div(class="outer",
                          # Include our custom CSS
                          tags$head(includeCSS("styles.css") ),
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("map", width="100%", height="100%"),
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                                        draggable = TRUE, top = 60, left = 40, right = "auto", bottom = "auto",
                                        width = 260, height = "auto",
                                        h3(img(src = "JM.jpg", height = 94, width = 186)),
                                        selectizeInput(inputId = "select_mun_1", label = "", selected = "MINERAL DE LA REFORMA", choices = sort(unique(na.omit(seccionCSV$MunicipioN)))),
                                        uiOutput(outputId = 'select_sec_1')),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                                        draggable = TRUE, top = "auto", left = "auto", right = 40, bottom = 20,
                                        width = 270, height = 560,
                                        h3("Resultado Municipal", style='color:#B3282D'),
                                        plotOutput("plot1", height = 250),
                                        plotOutput("plot2", height = 250))
                      ) ),
             tabPanel("REVOCACION", 
                      div(class="outer",
                          # Include our custom CSS
                          tags$head(includeCSS("styles.css") ),
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("mapR", width="100%", height="100%"),
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                                        draggable = TRUE, top = 60, left = 40, right = "auto", bottom = "auto",
                                        width = 260, height = "auto",
                                        h3(img(src = "JM.jpg", height = 94, width = 186)),
                                        selectizeInput(inputId = "select_mun_2", label = "", selected = "MINERAL DE LA REFORMA", choices = sort(unique(na.omit(seccionesHidalgo$MunicipioN)))),
                                        uiOutput(outputId = 'select_sec_2')),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                                        draggable = TRUE, top = "auto", left = "auto", right = 40, bottom = 20,
                                        width = 330, height = 320,
                                        h3("Resultado Municipal", style='color:#B3282D'),
                                        plotOutput("plot3", height = 250) )
                      ) ),
             tabPanel("RESULTADOS", h3(img(src = "JM.jpg", height = 94, width = 186)) , tableOutput("Total"), DT::dataTableOutput("ziptable"))
  ) ) 




#### SERVER ####

server <- function(input, output) {
  
  output$select_sec_1 <- renderUI({
    
    # check whether user wants to filter by cyl;
    # if not, then filter by selection
    if ('TODOS' %in% input$select_mun_1) {
      df <- seccionCSV
    } else {
      df <- seccionCSV %>%
        filter(
          MunicipioN %in% input$select_mun_1)
    }
    
    # get available carb values
    seccion_1 <- sort(unique(df$seccion))
    
    # render selectizeInput
    selectizeInput(
      inputId = 'select_sec_1',
      label = '',
      choices = c(input$select_mun_1, seccion_1),
      multiple = FALSE,
      selected = input$select_mun_1)
  })
  
  # render the child dropdown menu
  output$select_sec_2 <- renderUI({
    
    # check whether user wants to filter by cyl;
    # if not, then filter by selection
    if ('TODOS' %in% input$select_mun_2) {
      df_1 <- seccionesHidalgo
    } else {
      df_1 <- seccionesHidalgo %>%
        filter(
          MunicipioN %in% input$select_mun_2)
    }
    
    # get available carb values
    seccion_2 <- sort(unique(df_1$seccion))
    
    # render selectizeInput
    selectizeInput(
      inputId = 'select_sec_2',
      label = '',
      choices = c(input$select_mun_2, seccion_2),
      multiple = FALSE,
      selected = input$select_mun_2)
  })
  

    output$map <- renderLeaflet({
      tm <- shp_seccion %>% 
        mutate(ResultadosOb = round(100 * RESULTADOS / objetivo, 0)) %>% 
        filter(MunicipioN == input$select_sec_1) %>% 
        tm_shape() +
        tm_polygons(col = "ResultadosOb", id = "seccion", midpoint =50, style = "fixed", breaks = c(0,5,10,15,20,60), alpha = .5, palette = c("#D6E2FF", "#3D31F6", "#FFFB92", "#FEA601", "#AA282D"), title = "Objetivo %", popup.vars = c("Objetivo" = "objetivo", "Avance %" = "ResultadosOb", "Promovidos" = "RESULTADOS")) +
        tm_basemap("OpenStreetMap")
      tmap_leaflet(tm, in.shiny = TRUE)
    })
    
    output$mapR <- renderLeaflet({
      tm <- shp_seccion_rev %>% 
        filter(MunicipioN == input$select_sec_2) %>% 
        tm_shape() +
        tm_polygons(col = "Participacion", id = "seccion", midpoint =16, n = 5, alpha = .6, palette = c("#D6E2FF", "#3D31F6", "#FFFB92", "#FEA601", "#AA282D"), title = "Logrado %", popup.vars = c("Objetivo" = "ListaNominal", "Avance %" = "Participacion", "Siga" = "Siga")) +
        tm_basemap("OpenStreetMap")
      tmap_leaflet(tm, in.shiny = TRUE)
    })
    
    output$plot1 <- renderPlot({
      seccionCSV %>%  
        filter(MunicipioN == input$select_mun_1) %>% 
        group_by(seccion) %>% 
        summarise(Resultados = n()) %>% 
        na.omit() %>% 
        left_join(Objetivos, by = "seccion") %>% 
        mutate(ResultadosOb = round( 100*Resultados / objetivo , 1)) %>% 
        top_n(7) %>% 
        ggplot(aes(x = reorder(seccion, ResultadosOb), y = ResultadosOb)) + 
        geom_bar(stat="identity", fill="#B3282D", alpha = 0.7) +
        coord_flip()+
        geom_text(aes(label = Resultados), hjust= 1.2, color="#FFFFFF")+
        theme_minimal() +
        scale_y_continuous() +
        theme(plot.title = element_text("PLot 2 title"), axis.title.x = element_text("Promovente"), axis.title.y = element_blank()) +
        ylab("Mayor avance")  %>% 
        print()
    })
    
    output$plot2 <- renderPlot({
      
      seccionCSV %>% 
        filter(MunicipioN == input$select_mun_1) %>% 
        group_by(seccion) %>% 
        summarise(Resultados = n()) %>% 
        top_n(-7) %>% 
        na.omit() %>% 
        left_join(Objetivos, by = "seccion") %>% 
        mutate(ResultadosOb = round( 100*Resultados / objetivo , 1)) %>% 
        ggplot(aes(x = reorder(seccion, ResultadosOb), y = ResultadosOb)) + 
        geom_bar(stat="identity", fill="#B3282D", alpha = 0.7) +
        coord_flip()+
        geom_text(aes(label = Resultados), hjust=- 0.6, color="#B3282D")+
        theme_minimal() +
        scale_y_continuous() +
        theme(plot.title = element_text("PLot 2 title"), axis.title.x = element_text("Promovente"), axis.title.y = element_blank()) +
        ylab("Menor avance")  %>% 
        print()
    })
    
    output$plot3 <- renderPlot({
      Revocacion %>%  
        group_by(MunicipioN) %>% 
        summarise(Siga = sum(Siga), Revoque = sum(Revoque), Total = sum(Total), ListaNominal = sum(ListaNominal)) %>% 
        mutate(Morenistas = round( 100* Siga / ListaNominal , 1)) %>% 
        filter(ListaNominal != 0) %>%
        top_n(10) %>%  
        ggplot(aes(x = reorder(MunicipioN, Morenistas), y = Morenistas)) + 
        geom_bar(stat="identity", fill="#B3282D", alpha = 0.7) +
        coord_flip()+
        geom_text(aes(label = Morenistas), hjust= 1.6, color="#FFFFFF")+
        theme_minimal() +
        scale_y_continuous() +
        theme(plot.title = element_text("PLot 2 title"), axis.title.x = element_text("Promovente"), axis.title.y = element_blank()) +
        ylab("Resultados") %>% 
        print()
    })
    
    output$Total <- renderTable({
     Total <- seccionCSV %>% 
       group_by(seccion) %>% 
       summarise(suma = n()) %>%
       na.omit() %>% 
       summarise(Total = sum(suma))
    }    )
    
    output$ziptable <- DT::renderDataTable({
      tabla <- seccionCSV %>% 
        group_by(Promovente) %>% 
        summarise(Resultados = n()) %>% 
        mutate(Porcentaje = round(100*Resultados / sum(Resultados),1)) %>% 
        na.omit() %>% 
        arrange(-Resultados) 

      DT::datatable(tabla, options = list(pageLength = 50))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
