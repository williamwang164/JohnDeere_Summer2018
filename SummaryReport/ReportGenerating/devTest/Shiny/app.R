library(shiny)
options(shiny.maxRequestSize = 30*1024^4)
# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  verticalLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("asHarvest", "As Harvested CSV Files",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      fileInput("simHarvest", "Simulated CSV Files",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      downloadButton("report", "Generate report")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    output$report <- downloadHandler(
      filename = "report.pdf",
      content = function(file){
        if(is.null(input$asHarvest) || is.null(input$simHarvest)){
          return(NULL)
        }else{
          asHarvFiles <- data.frame( path = input$asHarvest$datapath, name = input$asHarvest$name )
          simFiles <- data.frame( path = input$simHarvest$datapath, name = input$simHarvest$name )
          params <- list(hFiles = asHarvFiles,
                         sFiles = simFiles)
          
          rmarkdown::render("shinySummaryReport.Rmd", output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv()))
        }
      }
      
    )
}
# Run the app ----
shinyApp(ui, server)