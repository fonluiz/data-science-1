#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(highcharter)
library(shinythemes)
library(readr)
library(tidyverse)

dados.series <- read_csv("data/dados.series-2.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   theme = shinytheme("cerulean"),
   
   # Application title
   h2("Avaliações de séries de TV", align = "center"),
   tags$br(),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("serie", "Escolha uma série:", 
                    choices = dados.series$series_name,
                    selected = 1),
        p("Para a visualização ao lado, as notas foram classificadas da seguinte forma: "),
        tags$ul(
          tags$li("notas péssimas: 1, 2 e 3;"),
          tags$li("notas ruins: 4 e 5;"),
          tags$li("notas medianas: 6 e 7;"),
          tags$li("notas boas: 8 e 9;"),
          tags$li("nota máxima: 10."))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        p("O gráfico abaixo mostra a qualidade e quantidade de notas recebidas 
          por conhecidas séries de televisão. Os dados vem do maior site de avaliações de shows de televisão,
          o IMDB. No IMDB os usuários podem avaliar episódios de séries com notas de 1 a 10."),
        p("As avaliações estão agrupadas por temporadas e você pode selecionar uma série
          na caixa ao lado para ver suas avaliações."),
         highchartOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$distPlot <- renderHighchart2({
     
     plot.data <- dados.series %>%
       filter(series_name == input$serie)
     
       chart <-highchart() %>%
       hc_title(text = paste("Avaliações recebidas por", input$serie)) %>%
       hc_subtitle(text = "Fonte: www.imdb.com") %>%
       hc_xAxis(categories = as.character(plot.data$season), title = list(text = "Temporada")) %>%
         hc_yAxis(title = list(text = "Quantidade de avaliações")) %>%
       hc_series(list(type = "column",
                      name = "Péssimas",
                      data = plot.data$pessimas),
                 list(type = "column",
                      name = "Ruins",
                      data = plot.data$ruins),
                 list(type = "column",
                      name = "Medianas",
                      data = plot.data$medianas),
                 list(type = "column",
                      name = "Boas",
                      data = plot.data$boas),
                 list(type = "column",
                      name = "Máxima",
                      data = plot.data$maxima)
       )
     chart
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

