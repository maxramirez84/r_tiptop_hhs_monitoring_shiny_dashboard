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

# Compute number of participants who consented the interview
numberOfparticipantsWhoConsented = function(hhs_data) {
  #browser()
  consented_area_1 = table(
    hhs_data$ended_pregnancy[hhs_data$district == 1], 
    hhs_data$resident_during_pregnancy[hhs_data$district == 1]
  )
  consented_area_2 = table(
    hhs_data$ended_pregnancy[hhs_data$district == 2], 
    hhs_data$resident_during_pregnancy[hhs_data$district == 2]
  )
  consented = c(
    if(is.na(consented_area_1)) 0 else consented_area_1,
    if(is.na(consented_area_2)) 0 else consented_area_2
  )
  names(consented) = c(1, 2)
  
  return(consented)
}

# Compute recruitment rate
recruitmentRate = function(hhs_data, sample_size_area_1, sample_size_area_2) {
  consented = numberOfparticipantsWhoConsented(hhs_data)
  
  recruitment = c(
    if(is.na(consented[1])) 0 else 
      floor((consented[1] / sample_size_area_1) * 100),
    if(is.na(consented[2])) 0 else 
      floor((consented[2] / sample_size_area_2) * 100)
  )
  names(recruitment) = c(1, 2)
  
  return(recruitment)
}