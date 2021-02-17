#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(reactable)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MASH for Three Locations"),

    fluidRow(
        column(
            2,
            "People",
            numericInput("n1", label="n1", value = 10, width = "80px"),
            numericInput("n2", label="n2", value = 5, width = "80px"),
            numericInput("n3", label="n3", value = 15, width = "80px")
            ),
        column(
            2,
            "PfPR",
            numericInput("n1", label="n1", value = 10, width = "80px"),
            numericInput("n2", label="n2", value = 5, width = "80px"),
            numericInput("n3", label="n3", value = 15, width = "80px")
        ),
        column(
            4,
            "Travel",
            numericInput("travel1", label="travel1", value = 0.2),
            numericInput("travel2", label="travel2", value = 0.3),
            numericInput("travel3", label="travel3", value = 0.2)
        ),
        column(
            2,
            "Mosquitoes",
            numericInput("mosy1", label="mosy1", value = 200),
            numericInput("mosy2", label="mosy2", value = 300),
            numericInput("mosy3", label="mosy3", value = 100)
        ),
        column(
            2,
            "Z",
            numericInput("mosy1", label="mosy1", value = 20),
            numericInput("mosy2", label="mosy2", value = 30),
            numericInput("mosy3", label="mosy3", value = 10)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application
shinyApp(ui = ui, server = server)
