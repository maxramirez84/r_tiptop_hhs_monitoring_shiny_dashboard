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

# Color palette
kColorPalette <- c("gray8", "gray35", "gray90")

ReadData <- function(api.url, api.token) {
  # Export dataset from the REDCap project identified by the token and through
  # the API.
  #
  # Args:
  #   api.url:   URL of the REDCap API.
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
  #   hhs.data:          Data frame containing all the records of a REDCap 
  #                      project.
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

MySQLUnion <- function(...) {
  # Behaves as MySQL UNION statement. Appends a list just below the other.
  #
  # Args:
  #   ...: A list of lists to appen one after the other.
  #
  # Returns:
  #   A data frame in which each row is a parameters passed list.
  aux <- list(...)
  dat <- data.frame()
  for (i in seq(along = aux)) {
    if (length(aux[[i]]) == 0) {
      dat[i, ] <- rep(0, ncol(dat))
    } else {
      for (j in names(aux[[i]]))
        dat[i, j] <- aux[[i]][j] 
    }
  }
  dat <- rapply(
    object = dat, 
    f      = function(x) { 
      ifelse (is.na(x), 0, x)
    }, 
    how = "replace"
  )
  
  dat
}

# Plots ------------------------------------------------------------------------
VisitedHouseholdsArea <- function(hhs.data, households.to.be.visited.area1, 
                                  households.to.be.visited.area2, 
                                  sample.size.area1, sample.size.area2, 
                                  study.areas) {
  # Build an horizontal bar plot showing the relationship between the number of
  # visited versus interviewed households. On the x-axis it has Number of 
  # households and on the y-axis the study areas. The legend has two different
  # items: Interviewed and Visited.
  #
  # Args:
  #   hhs.data:                       Data frame containing all the records of a 
  #                                   REDCap project.
  #   households.to.be.visited.area1: Expected number of households to visit in
  #                                   first area in order to find the required 
  #                                   number of eligible women.
  #   households.to.be.visited.area2: Expected number of households to visit in
  #                                   second area in order to find the required 
  #                                   number of eligible women.
  #   sample.size.area1:              Expected sample size in first area.
  #   sample.size.area2:              Expected sample size in second area.
  #   study.areas:                    Vector containing the name of the study
  #                                   areas.
  #
  #   Returns:
  #     It does not return anything directly but it plots the described Vis.
  #browser()
  kInterval <- 100
  max.x.axis <- max(
    households.to.be.visited.area1, 
    households.to.be.visited.area2
  ) + 
    kInterval * 5
  
  consented <- NumberOfparticipantsWhoConsented(hhs.data)
  recruitment <- RecruitmentRate(hhs.data, sample.size.area1, sample.size.area2)
  
  visits.area1 <- table(hhs.data$district)['1']
  visits.area2 <- table(hhs.data$district)['2']
  visits.number <- c(
    if (is.na(visits.area1)) {
      0
    } else {
      visits.area1
    },
    if (is.na(visits.area2)) {
      0
    } else {
      visits.area2
    }
  )
  completeness <- c(
    if (is.na(visits.number[1])) {
      0
    } else {
      floor((visits.number[1] / households.to.be.visited.area1) * 100)
    }, 
    if (is.na(visits.number[2])) {
      0
    } else {
      floor((visits.number[2] / households.to.be.visited.area2) * 100)
    }
  )
  names(completeness) <- c(1, 2)
  par(cex.lab = 1.5, cex.main = 2, cex.axis = 1.05)
  visits.progress <- barplot(
    height      = matrix(
      data = c(consented, visits.number), 
      nrow = 2, 
      ncol = 2, 
      byrow = T
    ), 
    horiz       = T, 
    names.arg   = study.areas, 
    main        = "Visited Households per Area",
    xlab        = "Number of households",
    ylab        = "Study areas",
    xlim        = c(0, max.x.axis),
    axes        = F,
    beside      = T,
    col         = kColorPalette[2:3]
  )
  axis(1, seq(0, max.x.axis, kInterval))
  legend(
    x      = "topright", 
    legend = c("Interviewed", "Visited"), 
    fill   = kColorPalette[2:3], 
    cex    = 1.5
  )
  text(
    x      = c(visits.number, consented), 
    y      = c(2.5, 5.5, 1.5, 4.5), 
    labels = paste0(c(completeness, recruitment), '%'), 
    pos    = 4, 
    col    = "red",
    cex    = 1.5
  )
}

# Visited Households per Cluster in a concrete Area
ProgressOfArea <- function(hhs.data, study.area.id, study.area.name, interval, 
                           required.visits.mean) {
  # Build a bar plot showing the number of visited versus interviewed households
  # in each of the clusters of the specified study area. On the x-axis it has 
  # different clusters in the area and on the y-axis the number of households. 
  # The legend has two different items: Interviewed and Visited. It also plots
  # a straight line indicating the average number of households to be visited in
  # order to find the required number of women per cluster.
  #
  # Args:
  #   hhs.data:             Data frame containing all the records of a REDCap 
  #                         project.
  #   study.area.id:        Id of the study area to be plotted.
  #   study.area.name:      Name of the study area to be plotted.
  #   interval:             A number to control de interval of the y-axis.
  #   required.visits.mean: A number representing the average number of 
  #                         households to be visited in order to find the 
  #                         required number of women per cluster.
  #
  # Returns:
  #   It does not return anything directly but it plots the described Vis.
  #browser()
  study.area.column <- paste0("cluster_", study.area.id)
  
  visits.number <- table(hhs.data[study.area.column])
  if (length(visits.number) > 0) {
    max.y.axis <- max(visits.number) + interval
    consented.number <- table(hhs.data[
      hhs.data$ended_pregnancy == 1 & hhs.data$resident_during_pregnancy == 1, 
      study.area.column
    ])
    
    dat <- MySQLUnion(visits.number, consented.number)
    par(cex.lab = 1.5, cex.main = 2, cex.axis = 1.05, mar = c(8, 8, 4, 0))
    visits.progress = barplot(
      height = matrix(c(dat[2,], dat[1,] - dat[2,]), nrow = 2, byrow = T),
      main   = paste("Visited Households per Cluster in", study.area.name),
      xlab   = paste("Cluster in", study.area.name),
      ylab   = "Number of households",
      ylim   = c(0, max.y.axis),
      axes   = F,
      col    = kColorPalette[2:3],
      mgp    = c(4, 1, 0)
    )
    axis(1, visits.progress, paste0("C", rownames(visits.number)), las = 2)
    axis(2, seq(0, max.y.axis, interval))
    abline(h = required.visits.mean, lwd = 1, lty = 2, col = "red")
    legend(
      x      = "topright", 
      legend = c("Interviewed", "Visited"), 
      fill   = kColorPalette[2:3], 
      cex    = 1.5
    )
    text(
      x      = visits.progress, 
      y      = dat[2, ], 
      labels = dat[2, ], 
      pos    = 3, 
      col    = kColorPalette[1])
  } else {
    print("There is no data.") 
  }
}