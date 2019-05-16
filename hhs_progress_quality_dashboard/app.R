# Copyright (c) 2019 Máximo Ramírez
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# maxramirez84
# 
# This script builds a Shiny dashboard for monitoring the progress and data 
# quality of the TIPTOP (https://www.tiptopmalaria.org/) household surveys. 
#
# INPUTS : REDCap API access information and study areas descriptions
# OUTPUTS: A Shiny App composing the dashboard
#
source("../tiptop_hhs_quality.R")
source("../tokens.R")

library(shiny)

# API access information
kApiUrl   <- redcap_api_url
kApiToken <- api_token_test  # Testing project

# Study area description
kStudyAreas                 <- c("Mananjary", "Toliary 2")
kStudyAreasIds              <- c("mananjary", "toliary_2")
kSampleSizeArea1            <- 284
kHouseholdsToBeVisitedArea1 <- 1278
kSampleSizeArea2            <- 284
kHouseholdsToBeVisitedArea2 <- 1278

# Define UI for the monitoring dashboard app
ui <- fluidPage(
  # Header
  h1("TIPTOP Baseline HHS Data Quality Report: MADAGASCAR"),
  em("Máximo Ramírez Robles"),
  br(),
  em(Sys.time()),
  br(),
  img(src = "github_icon.png", width = 30),
  a("Dashboard GitHub Repository", 
    href = "https://github.com/maxramirez84/r_tiptop_hhs_monitoring_shiny_dashboard"),
      
  # Field data collection progress section
  h2("FIELD DATA COLLECTION PROGRESS"),
  helpText(textOutput("records.summary")),
  span("Data collection by "),
  a("MANISA", href =  "http://www.manisa.mg/"),
  
  # General progress subsection
  h3("General Progress"),
  
  # General progress indicators
  fluidRow(
    column(6,
      div(align = "center",
        span(paste("Women interviewed @", kStudyAreas[1])),
        div(helpText(textOutput("recruited.area1", inline = T)), 
            style = "font-size: 95px"),
        helpText(textOutput("interviewed.out.of.area1"))
      )
    ),
    column(6,
      div(align = "center",
        span(paste("Women interviewed @", kStudyAreas[2])),
        div(helpText(textOutput("recruited.area2", inline = T)), 
            style = "font-size: 95px"),
        helpText(textOutput("interviewed.out.of.area2"))
      )
    )
  ),
  
  plotOutput("visited.households.per.area")
)

# Define server logic for the monitoring dashboard app
server <- function(input, output) {
  hhs.data <- ReadData(kApiUrl, kApiToken)
  
  # Total number of records in the database when user access the shiny app
  number.of.records <- NumberOfRecords(hhs.data)
  last.record.date <- LastRecordDate(hhs.data)
  output$records.summary <- renderText({
    paste0("The database contains ", number.of.records, " records ", 
           "(last record from ", last.record.date, ").")
  })
  
  # Percentage of recruited women when user access the shiny app
  recruitment <- RecruitmentRate(hhs.data, kSampleSizeArea1, kSampleSizeArea2)
  output$recruited.area1 <- renderText({
    paste0(recruitment[1], "%")
  })
  output$recruited.area2 <- renderText({
    paste0(recruitment[2], "%")
  })
  
  # Number of interviewed women versus the number of planned interviews when 
  # user access the shiny app
  consented <- NumberOfparticipantsWhoConsented(hhs.data)
  output$interviewed.out.of.area1 <- renderText({ 
    paste(consented[1], "/", kSampleSizeArea1) 
  })
  output$interviewed.out.of.area2 <- renderText({ 
    paste(consented[2], "/", kSampleSizeArea2) 
  })
  
  output$visited.households.per.area <- renderPlot({
    visitedHouseholdsArea(
      hhs.data, 
      kHouseholdsToBeVisitedArea1, 
      kHouseholdsToBeVisitedArea2, 
      kSampleSizeArea1, 
      kSampleSizeArea2, 
      kStudyAreas)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

