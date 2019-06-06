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

# Colors
kBlueEmphasis <- "#31708f"

# CSS rules
kCSSMediumNumber <- "font-size: 40px;"
kCSSBigNumber <- "font-size: 95px;"
kCSSEmphasis <- paste("color:", kBlueEmphasis, ";")
kCSSLeftIndicatorBox <- paste(
  "text-align: right;", 
  "position: absolute;", 
  "bottom: 0px;",
  "width: 50%;",
  "padding: 0 15px 0 15px;"
)
kCSSRightIndicatorBox <- paste(
  "text-align: left;",
  "float: right;",
  "width: 50%;",
  "padding: 0 15px 0 15px;"
)
kCSSIndicatorsWidget <- paste(
  "position: relative;", 
  "overflow: auto;"
)
kCSSWarning <- paste(
  "color: red;", 
  "font-weight: bold;"
)

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
            style = kCSSBigNumber),
        helpText(textOutput("interviewed.out.of.area1"))
      )
    ),
    column(6,
      div(align = "center",
        span(paste("Women interviewed @", kStudyAreas[2])),
        div(helpText(textOutput("recruited.area2", inline = T)), 
            style = kCSSBigNumber),
        helpText(textOutput("interviewed.out.of.area2"))
      )
    )
  ),
  
  plotOutput("visited.households.per.area"),
  
  # Progress in first area subsection
  h3(paste("Progress in", kStudyAreas[1])),
  
  plotOutput("progress.of.area1"),
  
  # Progress in second area subsection
  h3(paste("Progress in", kStudyAreas[2])),
  
  plotOutput("progress.of.area2"),
  
  # Study profile section
  h2("STUDY PROFILE"),
  
  # Profile of first area subsection
  h3(paste("Profile of", kStudyAreas[1])),
  
  htmlOutput("profile.of.area1"),
  
  # Profile of second area subsection
  h3(paste("Profile of", kStudyAreas[2])),
  
  htmlOutput("profile.of.area2"),
  
  # Duplicates section
  h2("DUPLICATES"),
  
  # Summary of duplicates in first area subsection
  h3(paste("Summary of duplicates in", kStudyAreas[1])),
  
  htmlOutput("summary.duplicates.of.area1"),
  
  # Summary of duplicates in second area subsection
  h3(paste("Summary of duplicates in", kStudyAreas[2])),
  
  htmlOutput("summary.duplicates.of.area2"),
  
  # Main indicators section
  h2("MAIN INDICATORS"),
  span("Important:", style = kCSSWarning),
  span("These indicators are computed by using raw data. Therefore, data has not 
       passed any verification and/or cleaning process. They should not be used 
       for analysis purposes. They are presented to address any possible data 
       issue."),
  
  # SP indicators subsection
  h3("SP Indicators"),
  
  fluidRow(
    column(6,
      # SP service provided in first area widget
      div(align = "center",
        p(paste("SP service provided in", kStudyAreas[1], "by CHW (c-IPTp)")),
        div(
          div(
            helpText(
              textOutput("ciptp.knowledge.area1"), 
              style = kCSSMediumNumber
            )
          ),
          span("Women who know"),
          style = kCSSLeftIndicatorBox
        ),
        div(
          div(
            helpText(
              textOutput("ciptp.admin.area1"),
              style = paste(kCSSBigNumber, kCSSEmphasis)
            )
          ),
          span("Women who took", style =  paste("color:", kBlueEmphasis)),
          style = kCSSRightIndicatorBox
        ),
        style = kCSSIndicatorsWidget
      ),
      br(),
      # SP service provided in second area widget
      div(align = "center",
        p(paste("SP service provided in", kStudyAreas[2], "by CHW (c-IPTp)")),
        div(
          div(
            helpText(
              textOutput("ciptp.knowledge.area2"), 
              style = kCSSMediumNumber
            )
          ),
          span("Women who know"),
          style = kCSSLeftIndicatorBox
        ),
        div(
          div(
            helpText(
              textOutput("ciptp.admin.area2"),
              style = paste(kCSSBigNumber, kCSSEmphasis)
            )
          ),
          span("Women who took", style =  paste("color:", kBlueEmphasis)),
          style = kCSSRightIndicatorBox
        )
      ),
      style = kCSSIndicatorsWidget
    ),
    column(6,
           div(align = "center",
               "Column2"
           )
    )
  )
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
  
  # Indicator: Number of interviewed women versus the number of planned 
  # interviews when user access the shiny app
  consented <- NumberOfparticipantsWhoConsented(hhs.data)
  output$interviewed.out.of.area1 <- renderText({ 
    paste(consented[1], "/", kSampleSizeArea1) 
  })
  output$interviewed.out.of.area2 <- renderText({ 
    paste(consented[2], "/", kSampleSizeArea2) 
  })
  
  # Horizontal Bar Plot: Number of interviewed versus visited households when 
  # user access the shiny app
  output$visited.households.per.area <- renderPlot({
    VisitedHouseholdsArea(
      hhs.data, 
      kHouseholdsToBeVisitedArea1, 
      kHouseholdsToBeVisitedArea2, 
      kSampleSizeArea1, 
      kSampleSizeArea2, 
      kStudyAreas)
  })
  
  # Bar Plot: Number of interviewed versus visited households per cluster in
  # first area when user access the shiny app
  output$progress.of.area1 <- renderPlot({
    ProgressOfArea(
      hhs.data, 
      study.area.id        = kStudyAreasIds[1],
      study.area.name      = kStudyAreas[1], 
      interval             = 10, 
      required.visits.mean = 60)
  })
  
  # Bar Plot: Number of interviewed versus visited households per cluster in
  # second area when user access the shiny app
  output$progress.of.area2 <- renderPlot({
    ProgressOfArea(
      hhs.data, 
      study.area.id        = kStudyAreasIds[2],
      study.area.name      = kStudyAreas[2], 
      interval             = 10, 
      required.visits.mean = 60)
  })
  
  # Table: Study profile of first area when user access the shiny app
  output$profile.of.area1 <- renderText({
    StudyProfileOfArea(
      hhs.data, 
      study.area.id = kStudyAreasIds[1]
    )
  })
  
  # Table: Study profile of second area when user access the shiny app
  output$profile.of.area2 <- renderText({
    StudyProfileOfArea(
      hhs.data, 
      study.area.id = kStudyAreasIds[2]
    )
  })
  
  # Table: Summary of duplicates in first area when user access the shiny app
  output$summary.duplicates.of.area1 <- renderText({
    DuplicatesSummary(
      hhs.data, 
      study.area.id = kStudyAreasIds[1]
    )
  })
  
  # Table: Summary of duplicates in second area when user access the shiny app
  output$summary.duplicates.of.area2 <- renderText({
    DuplicatesSummary(
      hhs.data, 
      study.area.id = kStudyAreasIds[2]
    )
  })
  
  # Indicator: Percentage of women who know about c-IPTp when user access the 
  # shiny app
  ciptp.knowledge <- CIPTpKnowledgeRate(hhs.data)
  output$ciptp.knowledge.area1 <- renderText({
    paste0(ciptp.knowledge[1], "%")
  })
  output$ciptp.knowledge.area2 <- renderText({
    paste0(ciptp.knowledge[2], "%")
  })
  
  # Indicator: Percentage of women who took c-IPTp when user access the shiny 
  # app
  ciptp.admin <- CIPTpAdministrationRate(hhs.data)
  output$ciptp.admin.area1 <- renderText({
    paste0(ciptp.admin[1], "%")
  })
  output$ciptp.admin.area2 <- renderText({
    paste0(ciptp.admin[2], "%")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

