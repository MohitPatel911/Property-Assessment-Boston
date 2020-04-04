library(shiny)
library(ggplot2)
library(dplyr)
## Building new data frame to build plots
pop_data <- Newdf1
## I have build new column to get the uique count of commercial and industrial 
## properties for years built
pop_data1 <- pop_data %>%
   group_by(YR_BUILT, LU) %>%
   summarise(counts = length(LU))

land <- unique(pop_data1$LU)

ui <- fluidPage(
   
   titlePanel("Year in which most commercial and Industrial properties build"),
   
 
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
  
         radioButtons(inputId = "year", label = "Select Year",
                      choices = list("1899" = 1899, "1900" = 1900, "1910" = 1910,
                                     "1920" = 1920, "1930" = 1930, "1940" = 1940,
                                     "1950" = 1950, "1960" = 1960, "1970" = 1970), 
                      selected = 1899),
         
      ),
      
      # Show a plot of the generated comparison
      mainPanel(
         fluidRow(
            plotOutput("barPlot", width = "60%"),
            plotOutput("linePlot", height = "200px")
         )
      )
   )
)

# Define server logic
server <- function(input, output) {
   
   output$barPlot <- renderPlot({
      
     # Set the x values (the land) populate the dataframe
      x <- c(input$land1, input$land2)
      df <- subset(pop_data1, YR_BUILT== input$year & LU%in%x)
      
      # Plot the bar chart for year built
      ggplot(data = df, aes(x=LU, y= counts, fill= LU)) + 
        geom_col() +
        theme_minimal() + 
        labs(fill='LU') #+ coord_flip()
   })

   
}

# Run the application 
shinyApp(ui = ui, server = server)

