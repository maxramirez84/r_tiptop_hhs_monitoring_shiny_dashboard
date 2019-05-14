library(redcapAPI)

# Read data downloaded from REDCap directly through the API
readData <- function(api_url = "", api_token = "") {
  rcon <- redcapConnection(api_url, api_token)
  
  exportRecords(rcon, factors = F)
}

# Get the number of records uploaded to REDCap
numberOfRecords = function(hhs_data) {
  nrow(hhs_data)
}

# Get the timestamp of the last collected record
lastRecordDate = function(hhs_data) {
  max(as.character(hhs_data$interview_date), na.rm = T)
}