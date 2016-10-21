
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
    
    # Application title
    titlePanel("Drift Correction & Normalization Utility"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            p(strong("First, select data file")),
            fileInput('raw', 'Select Data File'),
            hr(),
            p(tags$strong("Next apply settings:")),
            p("To be added..."),
            hr(),
            p(strong("Download Data after checking tabs.")),
            downloadButton("dow", "Download Results")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Raw Data",
                    dataTableOutput('preview')
                ),
                tabPanel("Check Standards",
                         dataTableOutput('checks')
                ),
                tabPanel("Drift Corrected",
                         dataTableOutput('final')
                ),
                tabPanel("Normalized Data",
                         dataTableOutput('norm')
                )
            )
        )
    )
))
