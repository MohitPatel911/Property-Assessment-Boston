library(shiny)
library(ggplot2)
library(dplyr)
## Building new data frame to build plots
p_data <- Newdf2
## I have build new column to get the uique count of commercial and industrial 
## properties for years rebuilt
p_data1 <- p_data %>%
  group_by(YR_REMOD, LU) %>%
  summarise(counts = length(LU))

land <- unique(p_data1$LU)

ui <- fluidPage(
  
  titlePanel("Year in which most commercial and Industrial properties Re-build"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = 'land1',
                  label = 'Land 1',
                  choices = land,
                  selected = 'I'),
      
      selectInput(inputId = 'land2',
                  label = 'Land 2',
                  choices = land,
                  selected = 'C'),

      sliderInput(inputId = 'year',
                  label = 'Year',
                  min = 1980,
                  max = 2017,
                  value = 1980,
                  sep = '')
      
    ),
    
    # Show a plot of the generated comparison
    mainPanel(
      fluidRow(
        plotOutput("barPlot", width = "60%"),
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$barPlot <- renderPlot({
    
    # Set the x values (the land) populate the dataframe
    x <- c(input$land1, input$land2)
    df <- subset(p_data1, YR_REMOD== input$year & LU%in%x)
    
    # Plot the bar chart for year built
    ggplot(data = df, aes(x=LU, y= counts, fill= LU)) + 
      geom_col() +
      theme_minimal() + 
      labs(fill='LU') #+ coord_flip()
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

