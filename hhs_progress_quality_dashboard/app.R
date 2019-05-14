library(shiny)

source("../tiptop_hhs_quality.R")
source("../tokens.R")

# Define UI ----
ui <- fluidPage(
  h1("TIPTOP Baseline HHS Data Quality Report: MADAGASCAR"),
      
  h2("FIELD DATA COLLECTION PROGRESS"),
  helpText(textOutput("records_summary"))
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

