library(redcapAPI)

# Read data downloaded from REDCap directly through the API
ReadData <- function (api.url = "", api.token = "") {
  rcon <- redcapConnection(api.url, api.token)
  
  exportRecords(rcon, factors = F)
}

# Get the number of records uploaded to REDCap
NumberOfRecords <- function (hhs.data) {
  nrow(hhs.data)
}

# Get the timestamp of the last collected record
LastRecordDate <- function (hhs.data) {
  max(as.character(hhs.data$interview_date), na.rm = T)
}

# Compute number of participants who consented the interview
NumberOfparticipantsWhoConsented <- function (hhs.data) {
  #browser()
  consented.area1 <- table(
    hhs.data$ended_pregnancy[hhs.data$district == 1], 
    hhs.data$resident_during_pregnancy[hhs.data$district == 1]
  )
  consented.area2 <- table(
    hhs.data$ended_pregnancy[hhs.data$district == 2], 
    hhs.data$resident_during_pregnancy[hhs.data$district == 2]
  )
  consented <- c(
    if (is.na(consented.area1)) {
      0 
    } else { 
      consented.area1
    },
    if (is.na(consented.area2)) {
      0 
    } else { 
      consented.area2
    }
  )
  names(consented) <- c(1, 2)
  
  consented
}

# Compute recruitment rate
RecruitmentRate <- function (hhs.data, sample.size.area1, sample.size.area2) {
  consented <- NumberOfparticipantsWhoConsented(hhs.data)
  
  recruitment <- c(
    if (is.na(consented[1])) {
      0 
    } else { 
      floor((consented[1] / sample.size.area1) * 100)
    },
    if (is.na(consented[2])) {
      0 
    } else { 
      floor((consented[2] / sample.size.area2) * 100)
    }
  )
  names(recruitment) <- c(1, 2)
  
  recruitment
}