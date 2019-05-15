library(shiny)

source("../tiptop_hhs_quality.R")
source("../tokens.R")

# Study area description
study_areas                    <- c("Mananjary", "Toliary 2")
study_areas_ids                <- c("mananjary", "toliary_2")
sample_size_area_1             <- 284
household_to_be_visited_area_1 <- 1278
sample_size_area_2             <- 284
household_to_be_visited_area_2 <- 1278

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
  a("MANISA", href =  "http://www.manisa.mg/"),
  
  h3("General Progress"),
  fluidRow(
    column(6,
      div(align = "center",
        span(paste("Women interviewed @", study_areas[1])),
        div(helpText(textOutput("recruited_area_1", inline = T)), 
            style = "font-size: 95px"),
        helpText(textOutput("interviewed_out_of_area1"))
      )
    ),
    column(6,
      div(align = "center",
        span(paste("Women interviewed @", study_areas[2])),
        div(helpText(textOutput("recruited_area_2", inline = T)), 
            style = "font-size: 95px"),
        helpText(textOutput("interviewed_out_of_area2"))
      )
    )
  )
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
  
  recruitment <- recruitmentRate(hhs_data, sample_size_area_1, 
                                sample_size_area_2)
  output$recruited_area_1 <- renderText({ paste0(recruitment[1], "%") })
  output$recruited_area_2 <- renderText({ paste0(recruitment[2], "%") })
  
  consented <- numberOfparticipantsWhoConsented(hhs_data)
  output$interviewed_out_of_area1 <- renderText({ 
    paste(consented[1], "/", sample_size_area_1) 
  })
  output$interviewed_out_of_area2 <- renderText({ 
    paste(consented[2], "/", sample_size_area_2) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

