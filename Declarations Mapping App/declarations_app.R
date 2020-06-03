library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(tidyverse)

#rq : certaines fonctionnalites prennent du temps mais marchent
#User interface pour creer :
ui <- fluidPage(
  #un titre
  titlePanel('Analyse des Declarations - Paris :'),
  #un theme
  shinythemes::shinytheme("paper") ,
  #des inputs et outputs organises :
  sidebarLayout(
    sidebarPanel(
      selectInput('Type',"Type de declaration : ",choices = dansmarue %>% distinct(TYPE)),
      selectInput('p_code',"Code Postal :", choices = dansmarue %>% distinct(CODE_POSTAL)),
      dateRangeInput('date','Periode :',
                     start = min(dansmarue$DATEDECL),end = "2018-07-04",
                     min = min(dansmarue$DATEDECL),max = max(dansmarue$DATEDECL),
                     format = "yyyy-mm-dd")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Dispertion des declarations',leafletOutput('pts_map')),
        tabPanel('Densite des declarations',leafletOutput('circles_map')),
        tabPanel('Nombre de declarations',plotOutput("counts")),
        tabPanel('Data',DT::DTOutput('table'))
      )
    )
  )
)

#Un serveur pour definir les actions qui suivent le choix de l utilisateur 
server <- function(input, output,session) {
  #mise a jour des donnees avec une fonction reactive 
  data_rue <- reactive(dansmarue[dansmarue$TYPE == input$Type,]%>% 
                         filter(as.double(DATEDECL)  >= min(input$date),
                                as.double(DATEDECL) <= max(input$date),
                                CODE_POSTAL == input$p_code
                         )
  )
  data_rue.sf <- reactive(dansmarue.sf[dansmarue.sf$TYPE == input$Type,]%>% 
                            filter(as.double(DATEDECL)  >= min(input$date),
                                   as.double(DATEDECL) <= max(input$date)  
                            )
  )
  
  densit <- reactive(sapply(st_contains( x=gr, y=data_rue.sf()$geometry), length))
  data_gr_p <- reactive(gr_p %>%  add_column(dens  = densit()))
  #resultats des outputs
  output$circles_map <- renderLeaflet(
    #une map interactive
    leaflet(data=data_gr_p() %>% st_transform(4326)) %>% 
      addTiles() %>% 
      addCircleMarkers(radius =~ sqrt(dens/10)*1.5,
                       fillColor = "purple",
                       stroke=FALSE,
                       fillOpacity = 0.5,
                       popup = ~paste("Nombre de plaintes:",dens))
  )
  #un tableau interactif
  output$table <- DT::renderDT(
    data_rue()
  )
  #une map de pts interactive
  output$pts_map <- renderLeaflet(
    
    leaflet(data = data_rue()) %>% 
      addTiles() %>% 
      addCircles(fillOpacity = 0.5)
  )
  #un bar plot 
  output$counts <- renderPlot(
    ggplot(data_rue() %>% count(TYPE,SOUSTYPE, DATEDECL) , aes(x=SOUSTYPE, y=n)) +
      geom_bar(stat="identity", fill = "lightgrey")   +
      theme_minimal()+ 
      coord_flip() 
  )
  
}

#Execution de l'app
shinyApp(ui=ui, server=server)