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
library(kableExtra)

# Color palette, font size
kColorPalette <- c("gray8", "gray35", "gray90")
kFontSize     <- 10

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

Pivot <- function(indexes, index.column, value.column, df) {
  # Converts a data frame of two columns in a list in which one column is used 
  # as keys and the other as values.
  #
  # Args:
  #   indexes:      List of indexes to be looked for inside the data frame, if
  #                 some index does not exist in the data frame, it will be 
  #                 included in the list with a value of 0.
  #   index.column: Name of the data frame column in which indexes are stored.
  #   value.column: Name of the data frame column in which values are stored.
  #   df:           Data frame of two columns (index, value) to be converted in
  #                 a list.
  #
  # Returns:
  #   A list of the data frame values.
  l <- list()
  for (i in indexes) {
    if (length(df[value.column][df[index.column] == i]) == 0) {
      l[i] <- 0
    } else {
      l[i] <- df[value.column][df[index.column] == i]
    }
  }
  
  l
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

# Tables -----------------------------------------------------------------------
BuildStudyProfileTable <- function(hhs.data, study.area.column) {
  # Compute a data frame which represents the study profile of the study area
  # determined by a concrete dataset variable. I.e. the variable which stores
  # the cluster ID of the area.
  #
  # Args:
  #   hhs.data:          Data frame containing all the records of a REDCap 
  #                      project.
  #   study.area.column: Name of the hhs.data column which contains the cluster 
  #                      ID of the observations in a concrete study area.
  #
  # Returns:
  #   A data frame representing the study profile of the area. Or NULL if there
  #   is no observations in the area.
  
  # Households selected and visited, i.e. there's a record collected in it
  number.hh.selected.visited <- table(hhs.data[study.area.column])
  if (length(number.hh.selected.visited) == 0) {
    return(NULL)
  }
  
  # Households selected, visited and interviewed, i.e. there's an interview (the
  # head or representative of the household accepted to proceed) in it 
  number.hh.selected.interviewed <- table(
    hhs.data[hhs.data$hh_acceptance == 1, study.area.column]
  )
  
  # Sum of women in childbearing age
  number.women.childbearing.age.df <- setNames(
    object = aggregate(
      childbearing_age_women ~ get(study.area.column), 
      FUN  = sum, 
      data = hhs.data
    ), 
    nm     = c(study.area.column, "childbearing_age_women")
  )
  number.women.childbearing.age.list <- Pivot(
    indexes      = names(number.hh.selected.visited), 
    index.column = study.area.column, 
    value.column = "childbearing_age_women", 
    df           = number.women.childbearing.age.df
  )

  # Sum of eligible women
  number.eligible.women.df <- setNames(
    object = aggregate(
      residents_during_pregnancy ~ get(study.area.column), 
      FUN  = sum, 
      data = hhs.data
    ),
    nm     = c(study.area.column, "residents_during_pregnancy")
  )
  number.eligible.women.list <- Pivot(
    indexes      = names(number.hh.selected.visited),
    index.column = study.area.column,
    value.column = "residents_during_pregnancy",
    df           = number.eligible.women.df
  )
  
  # Data frame with two rows, one with the sum of women in childbearing age and
  # the other with the sum of elegible women. The substraction of the first row
  # minus the second is the sum of non elegible women
  childbearing.age.women.profile <- MySQLUnion(
    number.women.childbearing.age.list, 
    number.eligible.women.list
  )
  
  # Women interviewed, i.e. women who consented
  number.women.interviewed <- table(
    subset(
      x      = hhs.data, 
      subset = ended_pregnancy == 1 & resident_during_pregnancy == 1
    )[study.area.column]
  )
  
  # Women that interrupted interview, i.e. women who consented but didn't 
  # confirm the inclusion criteria
  number.women.interrupt.interview <- table(
    subset(
      x      = hhs.data, 
      subset = consent == 1 & (is.na(ended_pregnancy) | ended_pregnancy == 0 | 
                                 is.na(resident_during_pregnancy) | 
                                 resident_during_pregnancy == 0)
    )[study.area.column]
  )
  
  # Women NON interviewed, i.e. women who didn't sign the informed consent
  number.women.non.interviewed <- table(
    subset(
      x      = hhs.data, 
      subset = consent == 0
    )[study.area.column]
  )
  
  # Data frame with three rows, one with the women interviewed, other with the
  # women who interrupted the interview and the last with the women who didn't
  # consent. The addition of these three rows is the sum of selected eligible 
  # women 
  eligible.women.selected <- MySQLUnion(
    number.women.interviewed, 
    number.women.interrupt.interview, 
    number.women.non.interviewed
  )
  if (ncol(eligible.women.selected) > 0) {
    eligible.women.selected.totals <- eligible.women.selected[1, ] + 
      eligible.women.selected[2, ] + eligible.women.selected[3, ]
  } else {
    eligible.women.selected.totals <- number.women.interviewed  # empty table 
  }
 
  # Denied signed consent/assent, i.e. women who didn't sign the informed 
  # consent because they reject the interview
  number.women.denied.consent <- table(
    hhs.data[hhs.data$why_not_consent == 0, study.area.column]
  )
  
  # Absent, i.e. women who didn't sign the informed consent because they were
  # not around
  number.women.absent <- table(
    hhs.data[hhs.data$why_not_consent == 2, study.area.column]
  )
  
  # Not able to respond, i.e. women who didn't sign the informed consent because
  # they had some disability
  number.women.unabled <- table(
    hhs.data[hhs.data$why_not_consent == 1, study.area.column]
  )
  
  # Other reason, i.e. women who didn't sign the informed consent because any
  # other reason
  number.women.other.reason <- table(
    hhs.data[hhs.data$why_not_consent == 88, study.area.column]
  )
  
  # Empty/destroyed households
  number.hh.empty <- table(
    hhs.data[hhs.data$hh_available == 2, study.area.column]
  )
  
  # Household head not found although household head availability is not 
  # required to proceed with the interview as long as any other adult consents
  number.hh.head.not.found <- table(
    hhs.data[hhs.data$hh_available == 0, study.area.column]
  )
  
  # Household head/other refused to consent the interview
  number.hh.head.refused <- table(
    hhs.data[hhs.data$hh_acceptance == 0, study.area.column]
  )
  
  # Data frame with two rows, one with the emtpy/destroyed households, and the 
  # other with those households in which the head/other refused to consent the 
  # interview. The addition of these two rows is the sum of the households 
  # selected but not interviewed  
  hh.selected.not.interviewed <- MySQLUnion(
    number.hh.empty, 
    number.hh.head.refused
  )
  if (ncol(hh.selected.not.interviewed) > 0) {
    hh.selected.not.interviewed.totals <- hh.selected.not.interviewed[1, ] + 
      hh.selected.not.interviewed[2, ]
  } else {
    hh.selected.not.interviewed.totals <- number.hh.empty  # emty table
  }
    
  study.profile <- MySQLUnion(
    number.hh.selected.visited, 
    number.hh.selected.interviewed, 
    number.women.childbearing.age.list, 
    childbearing.age.women.profile[1, ] - childbearing.age.women.profile[2, ], 
    number.eligible.women.list,
    eligible.women.selected.totals,
    number.women.interviewed,
    number.women.interrupt.interview,
    number.women.non.interviewed,
    number.women.denied.consent,
    number.women.absent,
    number.women.unabled,
    number.women.other.reason,
    hh.selected.not.interviewed.totals,
    number.hh.empty,
    number.hh.head.not.found,
    number.hh.head.refused
  )
  row.names(study.profile) <- c(
    "HH selected visited", 
    "HH selected interviewed", 
    "Women of childbearing age",
    paste0("NON eligible women", footnote_marker_symbol(1, "html")),
    "Eligible women",
    "Eligible women selected",
    "Women interviewed",
    "Women that interrupted interview",
    "Women NON interviewed",
    "Denied signed consent/assent",
    "Absent",
    "Not able to respond",
    "Other reason",
    "HH selected NOT interviewed",
    "Empty/destroyed",
    paste0("HH head not found", footnote_marker_symbol(2, "html")),
    "HH head/other refused to consent the interview"
  )
  colnames(study.profile) <- paste0("C", colnames(study.profile))
  
  study.profile
}

CheckStudyProfile <- function(study.profile) {
  # Go through the study profile data frame executing defined consistency 
  # checks.
  #
  # Args:
  #   study.profile: A data frame representing the study profile of an study 
  #                  area.
  #
  # Returns:
  #   A data frame representing a styled study profile, i.e. values contained in 
  #   the data frame are encapsulated in HTML tags which style each cell.
  # Consistency checks within the study profile
  study.profile.checked <- study.profile
  for (i in colnames(study.profile)) {
    # non_interviewed HH = empty + refused
    study.profile.checked[c(14, 15, 17), i] <- cell_spec(
      x        = study.profile[c(14, 15, 17), i],
      format   = "html",
      color    = ifelse(
        test   = study.profile[15, i] + 
          study.profile[17, i] != study.profile[14, i], 
        yes    = "red", 
        no     = ""
      ),
      tooltip  = ifelse(
        test   = study.profile[15, i] + 
          study.profile[17, i] != study.profile[14, i], 
        yes    = paste("NOT interviewed HH must be equal to the sum of", 
                       "empty/destroyed + refused"), 
        no     = ""
      )
    )
    
    # women = eligible + non_eligible
    study.profile.checked[c(3, 4, 5), i] <- cell_spec(
      x        = study.profile[c(3, 4, 5), i],
      format   = "html",
      color    = ifelse(
        test   = study.profile[4, i] + 
          study.profile[5, i] != study.profile[3, i], 
        yes    = "red", 
        no     = ""
      ),
      tooltip  = ifelse(
        test   = study.profile[4, i] + 
          study.profile[5, i] != study.profile[3, i], 
        yes    = "Women must be equal to the sum of eligibles + NON eligibles", 
        no     = ""
      )
    )
    
    # non_interviwed women = denied + absent + unabled + other
    study.profile.checked[c(9, 10, 11, 12, 13), i] <- cell_spec(
      x        = study.profile[c(9, 10, 11, 12, 13), i],
      format   = "html",
      color    = ifelse(
        test   = study.profile[10, i] + study.profile[11, i] + 
          study.profile[12, i] + study.profile[13, i] != study.profile[9, i], 
        yes    = "red", 
        no     = ""
      ),
      tooltip  = ifelse(
        test   = study.profile[10, i] + study.profile[11, i] + 
          study.profile[12, i] + study.profile[13, i] != study.profile[9, i], 
        yes    = paste("NON interviewed women must be equal to the sum of", 
                       "denied + absent + unabled"), 
        no     = ""
      )
    )
    
    # women selected = interviewed + interrupted + non_interviewed
    study.profile.checked[c(6, 7, 8, 9), i] <- cell_spec(
      x        = study.profile[c(6, 7, 8, 9), i],
      format   = "html",
      color    = ifelse(
        test   = study.profile[7, i] + study.profile[8, i] + 
          study.profile[9, i] != study.profile[6, i], 
        yes    = "red", 
        no     = ""
      ),
      tooltip  = ifelse(
        test   = study.profile[7, i] + study.profile[8, i] + 
          study.profile[9, i] != study.profile[6, i], 
        yes    = paste("Women selected must be equal to the sum of", 
                       "interviewed + interrupted + NON interviewed"), 
        no     = ""
      )
    )
    
    # visited HH = interviewed + non_interviewed
    study.profile.checked[c(1, 2, 14), i] <- cell_spec(
      x        = study.profile[c(1, 2, 14), i],
      format   = "html",
      color    = ifelse(
        test   = study.profile[2, i] + 
          study.profile[14, i] != study.profile[1, i], 
        yes    = "red", 
        no     = ""
      ),
      tooltip  = ifelse(
        test   = study.profile[2, i] + 
          study.profile[14, i] != study.profile[1, i], 
        yes    = paste("Visited HH must be equal to the sum of", 
                       "interviewed + NOT interviewed"), 
        no     = ""
      )
    )
  }
  
  study.profile.checked
}

GenerateStudyProfileKable <- function(study.profile) {
  # Generate kable table (HTML output) to display the provided study profile.
  #
  # Args:
  #   study.profile: A data frame representing a styled or non-styled study 
  #                  profile.
  #
  # Returns:
  #   An HTML styled table representing the study profile.
  # TODO(maxramirez84): Manage profiles of study areas with a high number of
  # clusters.
  kable(
    x                 = study.profile, 
    format            = "html", 
    escape            = F
  ) %>% kable_styling(
    bootstrap_options = c("striped", "hover", "responsive"), 
    font_size         = kFontSize
  ) %>% row_spec(
    row               = 0, 
    bold              = T, 
    color             = "white", 
    background        = "#494949"
  ) %>% row_spec(
    row               = c(1, 2, 3, 14), 
    bold              = T
  ) %>% add_indent(
    positions         = c(10, 11, 12, 13)
  ) %>% footnote(
    general_title     = "Notes:",
    general           = paste("Colored cells are consistency errors.", 
                              "Hover over these cells to display a  tooltip", 
                              "with the error message. Please, refer to the", 
                              "provided Data Queries Sheet."), 
    symbol            = c(
      paste("Eligible woman: woman that meets selection criteria 1 and", 
            "selection criteria 2"),
      paste("HH head availability is not required to proceed with the", 
            "interview as long as any other adult consents")
    )
  )
}

StudyProfileOfArea <- function(hhs.data, study.area.id) {
  # Compute a table using kable (HTML output) representing the study profile of
  # the specified study area.
  #
  # Args:
  #   hhs.data:      Data frame containing all the records of a REDCap project.
  #   study.area.id: Id of the study area to be profiled.
  #
  # Returns:
  #   An HTML styled table representing the study profile of the area.
  study.area.column <- paste0("cluster_", study.area.id)
  
  study.profile.table <- BuildStudyProfileTable(hhs.data, study.area.column)
  if (is.null(study.profile.table)) {
    return("<span>There is no data.</span>")
  }
  
  study.profile.checked <- CheckStudyProfile(study.profile.table)
  
  GenerateStudyProfileKable(study.profile.checked)
}

DuplicatesSummary <- function(hhs.data, study.area.id) {
  # Compute a table using kable (HTML output) summarizing the duplicates found
  # in the data set of the specified study area. Duplicates may be duplicated
  # household, i.e. households recorded or interviewed more than once by the 
  # same or different field workers; or same household ID used more than once 
  # for different interviews. And they may be records in which all variables
  # are exactly the same, i.e. records sent more than once.
  #
  # Args:
  #   hhs.data:      Data frame containing all the records of a REDCap project.
  #   study.area.id: Id of the study area in which duplicates are identified.
  #
  # Returns:
  #   An HTML styled table summarizing the duplicates of the area.
  # TODO(maxramirez84): Refactor this function, it's too long!
  #browser()
  study.area.column <- paste0("cluster_", study.area.id)
  
  non.interviewed.visits.number.area <- table(
    hhs.data[is.na(hhs.data$consent) | hhs.data$consent != 1, study.area.column]
  )
  interviewed.number.area <- table(
    hhs.data[hhs.data$consent == 1, study.area.column]
  )
  
  if (length(non.interviewed.visits.number.area) > 0 | 
      length(interviewed.number.area) > 0) {
    duplicated.records <- hhs.data[duplicated(hhs.data[2:ncol(hhs.data)]), ]
    
    non.interviewed.duplicated.records.area <- table(
      duplicated.records[
        is.na(duplicated.records$consent) | duplicated.records$consent != 1,
        study.area.column
      ]
    )
    interviewed.duplicated.records.area <- table(
      duplicated.records[duplicated.records$consent == 1, study.area.column]
    )
    
    id.columns <- hhs.data[c(study.area.column, "household")]
    duplicated.hh <- hhs.data[
      duplicated(id.columns) | duplicated(id.columns, fromLast = T), 
    ]
    duplicated.records.from.last <- hhs.data[
      duplicated(hhs.data[2:ncol(hhs.data)]) | 
        duplicated(hhs.data[2:ncol(hhs.data)], fromLast = T), ]
    rerecorded.hh <- duplicated.hh[
      !(duplicated.hh$record_id %in% duplicated.records.from.last$record_id), ]
    
    rerecorded.hh.area <- table(rerecorded.hh[study.area.column])
    
    rerecorded.hh.interviewed <- rerecorded.hh[rerecorded.hh$consent == 1, ]
    rerecorded.hh.interviewed.area <- table(
      rerecorded.hh.interviewed[study.area.column]
    )
    
    non.interviewed <- MySQLUnion(
      non.interviewed.visits.number.area, 
      non.interviewed.duplicated.records.area
    )
    if (ncol(non.interviewed) > 0) {
      non.interviewed.totals <- non.interviewed[1, ] - non.interviewed[2, ]
    } else {  # empty table
      non.interviewed.totals <- non.interviewed.visits.number.area
    }
    
    interviewed <- MySQLUnion(
      interviewed.number.area, 
      interviewed.duplicated.records.area
    )
    if (ncol(interviewed) > 0) {
      interviewed.totals <- interviewed[1, ] - interviewed[2, ]
    } else {  # empty table
      interviewed.totals <- interviewed.number.area 
    }
      
    #browser()
    duplicates.summary <- MySQLUnion(
      non.interviewed.visits.number.area,
      interviewed.number.area,
      non.interviewed.duplicated.records.area,
      interviewed.duplicated.records.area,
      non.interviewed.totals,
      interviewed.totals,
      rerecorded.hh.area,
      rerecorded.hh.interviewed.area
    )
    row.names(duplicates.summary) <- c(
      "NON interviewed HH", 
      "Interviewed HH", 
      "Duplicated records in NON interviewed HH",
      "Duplicated records in interviewed HH",
      "NON interviewed HH without duplicated records",
      "Interviewed HH without duplicated records",
      "Reused HH IDs",
      "Reused HH IDs in interviewed HH"
    )
    colnames(duplicates.summary) <- paste0("C", colnames(duplicates.summary))
    
    duplicates.summary.reduced <- duplicates.summary[, 
      duplicates.summary[3, ] != 0 | duplicates.summary[4, ] != 0 | 
        duplicates.summary[7, ] != 0, drop = F]
    
    # TODO(maxramirez84): Manage tables of study areas with duplicates found in 
    # a high number of clusters.
    if (ncol(duplicates.summary.reduced) > 0) {
      kable(
        x      = duplicates.summary.reduced, 
        format = "html", 
        escape = F
      ) %>% kable_styling(
        bootstrap_options = c("striped", "hover", "responsive"), 
        font_size         = kFontSize
      ) %>% row_spec(
        row               = 0, 
        bold              = T, 
        color             = "white", 
        background        = "#494949"
      ) %>% row_spec(
        row               = c(2, 6), 
        bold              = T
      )
    } else {
      print("There are no duplicates.") 
    }
  } else {
    print("There is no data.")
  }
}