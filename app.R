library(shiny)
library(dplyr)
library(ggmap)
library(ggplot2)
library(graphics)
library(maps)
library(viridis)

load(tampaData.RData)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Filter"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("variable", "Crime Type for first map: ", 
                     c("Petty Theft" = "PETTY THEFT",
                       "Residential/Commercial Alarm" = "RESID/COMMERIAL ALARM",
                       "Robbery"= "ROBBERY"), multiple = TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("map"),
         plotOutput("linkage1"),
         plotOutput("linkage2"),
         tableOutput("table")
      )
   )
)

# Server
server <- function(input, output) {
  
  
   
  newData = reactive ({
    filter(totalData, Description %in% input$variable)
  })
   output$map <- renderPlot({
      ggmap(mapTampa)+geom_point(data=newData(), aes(x=lon, y=lat, color=Description)) + 
       stat_density2d(data = newData(), aes(x=lon, y=lat, fill= ..density..), geom = 'tile',
                     contour = F, alpha=.5) + scale_fill_viridis(option = 'inferno')
  
   })
   output$linkage1 <- renderPlot({
     fields::image.plot(1:113, 1:113, pp[1:113, 1:113], xlab = 'Crime', ylab = 'Unsolved Crime', main = 'Probability that crimes are linked')
     
   })
   
   output$linkage2 <- renderPlot(plot(1:113, unsolved_probs, xlab = 'Unsolved Crimes', ylab = 'Probability of Linkage'))
   
   # output$table <- renderTable(max(pp))
}

# Run the application 
shinyApp(ui = ui, server = server)

