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
# This script (or library) contains the auxiliar functions used to build the 
# data quality and progress dashboard for monitoring data collection during the 
# execution of the household surveys of the TIPTOP project 
# (https://www.tiptopmalaria.org/).
#
library(redcapAPI)

ReadData <- function(api.url, api.token) {
  # Export dataset from the REDCap project identified by the token and through
  # the API.
  #
  # Args:
  #   api.url: URL of the REDCap API.
  #   api.token: Token for accessing the project to export the data.
  #
  # Returns:
  #   A data frame containing all the records data for the specified project.
  rcon <- redcapConnection(api.url, api.token)
  
  exportRecords(rcon, factors = F)
}

NumberOfRecords <- function(hhs.data) {
  # Compute the number of records uploaded to REDCap from the dataset previously
  # exported from REDCap.
  #
  # Args:
  #   hhs.data: Data frame containing all the records of a REDCap project.
  #
  # Returns:
  #   Number of records (observations) that the dataset contains.
  nrow(hhs.data)
}

LastRecordDate <- function(hhs.data) {
  # Get the timestamp of the last collected record from the dataset previously
  # exported from REDCap.
  #
  # Args:
  #   hhs.data: Data frame containing all the records of a REDCap project.
  #
  # Returns:
  #   Timestamp of the last collected record (observation) in the dataset.
  max(as.character(hhs.data$interview_date), na.rm = T)
}

NumberOfparticipantsWhoConsented <- function(hhs.data) {
  # Compute the number of participants who consented the interview from the 
  # dataset previously exported from REDCap.
  #
  # Args:
  #   hhs.data: Data frame containing all the records of a REDCap project.
  #
  # Returns:
  #   A vector containg the number of participants who consented the interview
  #   for each study area.
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

RecruitmentRate <- function(hhs.data, sample.size.area1, sample.size.area2) {
  # Compute the recruitment rate from the dataset previously exported from 
  # REDCap.
  #
  # Args:
  #   hhs.data: Data frame containing all the records of a REDCap project.
  #   sample.size.area1: Expected sample size in first area.
  #   sample.size.area2: Expected sample size in second area.
  #
  # Returns:
  #   A vector with the recruitment rate of first and second study areas.
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