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