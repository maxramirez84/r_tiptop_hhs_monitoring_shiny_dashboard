library(shiny)

source("../tiptop_hhs_quality.R")
source("../tokens.R")

# Define UI ----
ui <- fluidPage(
  h1("TIPTOP Baseline HHS Data Quality Report: MADAGASCAR"),
  em("Máximo Ramírez Robles"),
  br(),
  em(Sys.time()),
  br(),
  img(src = "github_icon.png", width = 30),
  a("Dashboard GitHub Repository", href = "https://github.com/maxramirez84/r_tiptop_hhs_monitoring_shiny_dashboard"),
      
  h2("FIELD DATA COLLECTION PROGRESS"),
  helpText(textOutput("records_summary")),
  span("Data collection by "),
  a("MANISA", href =  "http://www.manisa.mg/")
)

# Define server logic ----
server <- function(input, output) {
  api_url   <- redcap_api_url
  api_token <- api_token_test # Testing project
  
  hhs_data <- readData(api_url, api_token)
  
  number_of_records = numberOfRecords(hhs_data)
  last_record_date = lastRecordDate(hhs_data)
  output$records_summary <- renderText({
    paste0("The database contains ", number_of_records, " records ", 
           "(last record from ", last_record_date, ").")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

