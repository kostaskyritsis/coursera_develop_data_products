#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(plotly)
library(tidyr)
library(dplyr)
library(reshape2)

imdb.data = read.csv("movie_metadata.csv", header = T)
imdb.data = as.data.table(imdb.data)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Quick Exploratory Analysis for a subset of IMDB data"),
  
  sidebarLayout(
    
    sidebarPanel(
      # selectInputs
      selectInput("movie", "Movie:", imdb.data$movie_title),
      selectInput("director", "Director:", imdb.data$director_name),
      selectInput("actor1", "First Actor:", imdb.data$actor_1_name)
    ),
    # selectOutput
    mainPanel(
      tabsetPanel(
        tabPanel("Movie Barplot",plotOutput("movie.out")),
        tabPanel("Director Plot",plotlyOutput("director.out")),
        tabPanel("First Actor Plot",plotlyOutput("actor1.out")),
        h3("Explaining the usage of this tool"),
        p("In the side panel you can see the presence of three scroll-down menus.
          By selecting a name for the Movie, the Director and/or the Actor a barplot is created at the respective tabPanel."),
        
        p("In these tabPanels one can see the differences in 1) a specific movie's gross (profit) vs its budget (cost), or the sums of the grosses and budgets for movies per year, regarding a specific director and actor, that participated in that film.")
        
        
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Movie barplot
  output$movie.out <- renderPlot({
    
    # index movie
    ind = which(imdb.data$movie_title == input$movie)
    m = imdb.data[ind,]
    
    b.data = c(gross=as.numeric(as.character(m$gross)),
               budget=as.numeric(as.character(m$budget)))
    
    # 
    bp = barplot(b.data/10e+06,
                 col = "lightblue", ylab = "Millions of Dollars ($)", 
                 main = paste0(input$movie, ": Gross and Budget")
    )
    text(bp, (b.data/10e+06)*0.5, labels = round(b.data/10e+06,2))
    
  })
  
  # Director plot_ly
  output$director.out = renderPlotly({
    
    # index director name
    ind.D = which(imdb.data$director_name == input$director)
    
    # index grass and budget values
    money_data = imdb.data[ind.D, list(gross, budget)] %>%
      gather() %>%
      mutate(title_year = rep(imdb.data[ind.D,title_year],2))
    money_data = money_data[!is.na(money_data$value),]
    money_data = money_data[order(money_data$title_year),]
    
    # use plot_ly for interactive graphics
    plot_ly(money_data, x = ~I(as.character(title_year)), 
            y = ~I(as.numeric(value)),
            color = ~key, 
            type = "bar",
            hoverinfo = 'text',
            text = ~paste('Million Dollars ($):', 
                          I(round(as.numeric(value)/10e+6,2)))) %>%
      layout(title = paste0(input$director,": Gross and Budget for Movies per Year"),
             xaxis = list(title = 'Date in Years'),
             yaxis = list(title = '')
      )
    
  })
  
  # First actor plot_ly
  output$actor1.out = renderPlotly({
    
    # index actor1 name
    ind.D = which(imdb.data$actor_1_name == input$actor1)
    
    # index grass and budget values
    money_data = imdb.data[ind.D, list(gross, budget)] %>%
      gather() %>%
      mutate(title_year = rep(imdb.data[ind.D,title_year],2))
    money_data = money_data[!is.na(money_data$value),]
    money_data = money_data[order(money_data$title_year),]
    
    # use plot_ly for interactive graphics
    plot_ly(money_data, x = ~I(as.character(title_year)), 
            y = ~I(as.numeric(value)),
            color = ~key, 
            type = "bar",
            hoverinfo = 'text',
            text = ~paste('Million Dollars ($):', 
                          I(round(as.numeric(value)/10e+6,2)))) %>%
      layout(title = paste0(input$actor1,": Gross and Budget for Movies per Year"),
             xaxis = list(title = 'Date in Years'),
             yaxis = list(title = '')
      )
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

