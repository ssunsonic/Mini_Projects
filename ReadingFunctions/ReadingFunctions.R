open_wea <- function(filepath) {
  data = read.table(filepath, skip = 6)
  colnames(data) = c(
    "Month",
    "Day",
    "Standard Time",
    "Direct Normal Irradiance",
    "Diffuse Horizontal Radiance"
  )
  data = data[ , colSums(is.na(data))==0] # checking for columns w/ NAs
  return(data)
  
}

# PVSYST function
open_pvsyst <- function(filepath) {
  data = read.table(filepath, sep = ",", skip = 14)
  colnames(data) = c(
    'Year',
    'Month',
    'Day',
    'Hour',
    'Minute',
    'GHI',
    'DHI',
    'DNI',
    'Tamb',
    'WindVel',
    'WindDir'
  )
  data = data[ , colSums(is.na(data))==0]
  return(data)
}

open_monthly_individual <- function(filepath, title) {
  unique_id = grep(title, readLines(filepath)) # grabs line number (position) of title in text file
  if (title == "Monthly Statistics for Dry Bulb temperatures") {
    data = read.table(
      filepath,
      sep = "\t",
      header = TRUE,
      skip = unique_id,
      nrows = 12
    )
  } else if (title == "Monthly Statistics for Dew Point temperatures" |
             title[i] == "Monthly Statistics for Wind Speed") {
    data = read.table(
      filepath,
      sep = "\t",
      header = TRUE,
      skip = unique_id,
      nrows = 5
    )
  } else {
    data = read.table(
      filepath,
      sep = "\t",
      header = TRUE,
      skip = unique_id,
      nrows = 16
    )
  }
  
  data = data[ , colSums(is.na(data))==0] 
  
  data = t(data) # transpose
  
  colnames(data) = data[1, ] # after transposing the matrix, previous row names that are now column names are read in as the first line of data, and we want these to be the actual column names of our data
  
  data = data[-1, ] # get rid of the column names contained within our data frame
  
  data = as.data.frame(data) 
  
  for (i in 1:ncol(data)) { 
    colnames(data)[i] = trimws(colnames(data)[i]) # trim whitespaces for column names in df
  }
  
  
  while (title != "Monthly Wind Direction") {
    for (i in 1:nrow(data)) {
      day_hour <- strsplit(as.character(data[i, 2]), ":")[[1]] # CHATGPT Assistance
      day <- as.numeric(day_hour[1])
      hour <- as.numeric(day_hour[2])
      
      data[i, "Datetime_Max"] <-
        as.POSIXct(paste0("2023-", i, "-", day, " ", hour, ":00:00"), format = "%Y-%m-%d %H:%M:%S")
    }
    
    for (i in 1:nrow(data)) {
      day_hour <- strsplit(as.character(data[i, 4]), ":")[[1]]
      day <- as.numeric(day_hour[1])
      hour <- as.numeric(day_hour[2])
      
      data[i, "Datetime_Min"] <-
        as.POSIXct(paste0("2023-", i, "-", day, " ", hour, ":00:00"), format = "%Y-%m-%d %H:%M:%S")
    }
    
    data[, 2] = data$Datetime_Max
    data[, 4] = data$Datetime_Min
    data = subset(data, select = -c(Datetime_Max, Datetime_Min))
    
    for (i in 1:ncol(data)) {
      if (colnames(data)[i] == "Day:Hour" |
          colnames(data)[i] == "Day:Hour.1") {
        next
      }
      data[, i] = as.numeric(data[, i])
    }
    break
  }
  
  return(data)
}

open_monthly <- function(filepath, titles) {
  data_list <- list()
  for (title in titles) {
    unique_id <- grep(title, readLines(filepath))
    if (title == "Monthly Statistics for Dry Bulb temperatures") {
      data <-
        read.table(
          filepath,
          sep = "\t",
          header = TRUE,
          skip = unique_id,
          nrows = 12
        )
    } else if (title == "Monthly Statistics for Dew Point temperatures" |
               title == "Monthly Statistics for Wind Speed") {
      data <-
        read.table(
          filepath,
          sep = "\t",
          header = TRUE,
          skip = unique_id,
          nrows = 5
        )
    } else {
      data <-
        read.table(
          filepath,
          sep = "\t",
          header = TRUE,
          skip = unique_id,
          nrows = 16
        )
    }
    
    data = data[ , colSums(is.na(data))==0] 
    
    data = t(data) # transpose
    
    colnames(data) = data[1, ] # after transposing the matrix, previous row names that are now column names are read in as the first line of data, and we want these to be the actual column names of our data
    
    data = data[-1, ] # get rid of the column names contained within our data frame
    
    data = as.data.frame(data) 
    
    for (i in 1:ncol(data)) {
      colnames(data)[i] <- trimws(colnames(data)[i]) # trim whitespaces in column names
    }
    
    while (title != "Monthly Wind Direction") { # not applied to monthly bc no time data
      for (i in 1:nrow(data)) {
        day_hour <- strsplit(as.character(data[i, 2]), ":")[[1]] # Chat GPT assisted in developing this code. Essentially the time data is being split by day and hours
        
        day <- as.numeric(day_hour[1])
        hour <- as.numeric(day_hour[2])
        
        data[i, "Datetime_Max"] <-
          as.POSIXct(paste0("2023-", i, "-", day, " ", hour, ":00:00"), format = "%Y-%m-%d %H:%M:%S")
      }
      
      for (i in 1:nrow(data)) {
        day_hour <- strsplit(as.character(data[i, 4]), ":")[[1]]
        day <- as.numeric(day_hour[1])
        hour <- as.numeric(day_hour[2])
        
        data[i, "Datetime_Min"] <-
          as.POSIXct(paste0("2023-", i, "-", day, " ", hour, ":00:00"), format = "%Y-%m-%d %H:%M:%S")
      }
      
      data[, 2] <- data$Datetime_Max # replace old column that contained time data with new column Datetime_Max created above
      data[, 4] <- data$Datetime_Min # replace with Datetime_Min
      data <- subset(data, select = -c(Datetime_Max, Datetime_Min)) # delete the new columns bc we already inputted the data into the original column indexes
      
      break
    }
    
    for (i in 1:ncol(data)) { # change every column into numeric aside for the time columns
      if (colnames(data)[i] == "Day:Hour" |
          colnames(data)[i] == "Day:Hour.1") {
        next
      }
      data[, i] <- as.numeric(data[, i])
    }
    
    data_list[[title]] <- data.frame(data) # returns a list of dataframes, based on the number of titles
  }
  return(data_list)
}

open_hourly_individual <- function(filepath, title) {
  unique_id = grep(title, readLines(filepath))
  data = read.table(
    filepath,
    sep = "\t",
    header = TRUE,
    skip = unique_id,
    nrows = 26
  )
  
  data = data[ , colSums(is.na(data))==0]
  data = as.data.frame(data)
  colnames(data)[1] = "Hours"
  colnames(data) = trimws(colnames(data))
  
  data[, 1] = gsub("[[:space:]]", "", data[, 1])
  
  temperatures = c()
  max_val = 0
  min_val = 0
  max_index = 0
  min_index = 0
  
  for (i in 2:ncol(data)) {
    if (class(data[25, i]) == "character") {
      data[25, i] <- as.numeric(gsub("[^0-9]", "", data[25, i]))
    }
    if (class(data[26, i]) == "character") {
      data[26, i] <- as.numeric(gsub("[^0-9]", "", data[26, i]))
    }
    
  }
  
  for (i in 2:(ncol(data))) {
    for (j in 1:(nrow(data))) {
      if (j == 25) {
        max_index = data[j, i]
      } else if (j == 26) {
        min_index = data[j, i]
      } else {
        temperatures = c(temperatures, data[j, i])
      }
    }
    max_val = max(temperatures)
    min_val = min(temperatures)
    
    # construct counters for correct max hours
    correct_max = 0
    incorrect_max = 0
    
    if (data[max_index, i] == max_val) {
      correct_max = correct_max + 1
    } else{
      incorrect_max = incorrect_max + 1
    }
    
    # counters for correct min hours
    correct_min = 0
    incorrect_min = 0
    
    if (data[min_index, i] == min_val) {
      correct_min = correct_min + 1
    } else{
      incorrect_in = incorrect_min + 1
    }
    temperatures = c()
  }
  
  data = data[-c(25, 26), ] # remove MAX and MIN rows
  
  Hours = rep(data[, 1], 12)
  Months = c()
  Temperature = c()
  
  for (i in 2:ncol(data)) {
    Temperature = c(Temperature, data[, i])
  }
  
  for (i in 2:ncol(data)) {
    Months = c(Months, rep(colnames(data)[i], 24))
  }
  
  data = cbind(Hours, Months, Temperature)
  data = as.data.frame(data)
  
  return(data)
}

open_hourly <- function(filepath, titles) {
  data_list <- list()
  
  # construct counters for correct max hours
  correct_max = 0
  incorrect_max = 0
  
  # counters for correct min hours
  correct_min = 0
  incorrect_min = 0
  
  for (title in titles) {
    unique_id = grep(title, readLines(filepath))
    data = read.table(
      filepath,
      sep = "\t",
      header = TRUE,
      skip = unique_id,
      nrows = 26
    )
    
    data = data[, colSums(is.na(data)) == 0] 
    data = as.data.frame(data)
    colnames(data)[1] = "Hours"
    colnames(data) = trimws(colnames(data)) 
    data[, 1] = gsub("[[:space:]]", "", data[, 1])
    
    for (i in 2:ncol(data)) { # skip the first column (time)
      if (class(data[25, i]) == "character") { # checking if value we want to compare (should be numerical) is not numerical for some reason
        data[25, i] <-
          as.numeric(gsub("[^0-9]", "", data[25, i])) # replaces character values other than numeric from 0-9
      }
      if (class(data[26, i]) == "character") {
        data[26, i] <- as.numeric(gsub("[^0-9]", "", data[26, i]))
      }
    }
    
    temperatures = c()
    max_val = 0
    min_val = 0
    max_index = 0
    min_index = 0
    
    for (i in 2:(ncol(data))) { # check every column besides for the first
      for (j in 1:(nrow(data))) { # checks every temp value per hour based on the ith column
        if (j == 25) { # the MAX Hour (variable we want to compare w/) lies on the 25th row
          max_index = data[j, i]
        } else if (j == 26) {
          min_index = data[j, i]
        } else {
          temperatures = c(temperatures, data[j, i]) # append all the temp values we want to compare w/ in a vector
        }
      }
      max_val = max(temperatures) # retrieve max temp for the ith column
      min_val = min(temperatures) # retrieve min temp for the ith column
      
      if (data[max_index,i] == max_val) { #
        correct_max = correct_max + 1
      } else{
        incorrect_max = incorrect_max + 1
      }
      
      if (data[min_index,i] == min_val) {
        correct_min = correct_min + 1
      } else{
        incorrect_min = incorrect_min + 1
      }
      temperatures = c()
    }
    
    data = data[-c(25, 26),] # remove MAX and MIN rows
    
    Hours = rep(data[, 1], 12) # repeat the hour column 12 times for each month = 288 total rows
    Months = c() 
    Temperature = c() # temperatures values based on title names (Dry, Dew, etc.)
    
    for (i in 2:ncol(data)) { # extract temp values
      Temperature = c(Temperature, data[, i])
    }
    
    for (i in 2:ncol(data)) { # extract months
      Months = c(Months, rep(colnames(data)[i], 24))
    }
    
    data = cbind(Hours, Months, Temperature) 
    data_list[[title]] <- data.frame(data) # put each df into a list
  }
  
  print(
    paste0(
      "I verified that out of the 60 maximum hours that had to be verified for the five hourly tables ",
      correct_max,
      " were correct."
    )
  )
  print(
    paste0(
      "I verified that out of the 60 minimum hours that had to be verified for the five hourly tables ",
      correct_min,
      " were correct."
    )
  )
  
  return(data_list)
}

merge_hourly_data <-
  function(filepath, titles) {
    # title is a vector that contains names of variables
    df = data.frame()
    
    hours_month = open_hourly_individual(filepath, titles[1]) # opens the hourly data only for the first df
    hours_month = hours_month[,-3] # removes the last column (temp_vals) and keeps the hour and month columns
    
    df = rbind(df, hours_month) # create a new df w/ only hour and months
    
    for (i in 1:length(titles)) {
      df = cbind(df, do.call(cbind, open_hourly_individual(filepath, titles[i])[3])) # column bind the 3rd column (temp_vals) of every df based on titles
    }
    
    colnames(df) = c(
      "Hours",
      "Months",
      "Dry_Bulb_Temp",
      "Dew_Point_Temp",
      "Relative_Humidity",
      "Direct_Normal_Solar_Radiation",
      "Wind_Speed"
    )
    
    hour_order <-
      c(
        "0:01-1:00",
        "1:01-2:00",
        "2:01-3:00",
        "3:01-4:00",
        "4:01-5:00",
        "5:01-6:00",
        "6:01-7:00",
        "7:01-8:00",
        "8:01-9:00",
        "9:01-10:00",
        "10:01-11:00",
        "11:01-12:00",
        "12:01-13:00",
        "13:01-14:00",
        "14:01-15:00",
        "15:01-16:00",
        "16:01-17:00",
        "17:01-18:00",
        "18:01-19:00",
        "19:01-20:00",
        "20:01-21:00",
        "21:01-22:00",
        "22:01-23:00",
        "23:01-24:00"
      )
    
    df$Hours <-
      factor(df$Hours, levels = hour_order) # set as factor, otherwise R doesn't read in by order but by char
    
    month_order <-
      c("Jan",
        "Feb",
        "Mar",
        "Apr",
        "May",
        "Jun",
        "Jul",
        "Aug",
        "Sep",
        "Oct",
        "Nov",
        "Dec")
    
    df$Months <- factor(df$Months, levels = month_order)
    
    for (i in 3:ncol(df)) {
      df[, i] = as.numeric(df[, i])
    }
    
    return(df)
  }

library(ggplot2)
library(scales)

plot_hourly <- function(filename, titles) {
  hourly_data = merge_hourly_data(filename, titles)
  
  for (i in 1:length(titles)) {
    if (titles[i] == "Average Hourly Statistics for Dry Bulb temperatures") {
      print(
        ggplot() +
          geom_point(data = hourly_data, aes(
            x = Hours, y = Dry_Bulb_Temp, color = Months
          )) + theme(axis.text.x = element_text(angle = 90)) # flips the x-axis names vertically for easier user read
      )
    } else if (titles[i] == "Average Hourly Statistics for Dew Point temperatures") {
      print(
        ggplot() +
          geom_point(data = hourly_data, aes(
            x = Hours, y = Dew_Point_Temp, color = Months
          )) + theme(axis.text.x = element_text(angle = 90))
      )
    } else if (titles[i] == "Average Hourly Relative Humidity") {
      print(
        ggplot() +
          geom_point(data = hourly_data, aes(
            x = Hours, y = Relative_Humidity, color = Months
          )) + theme(axis.text.x = element_text(angle = 90))
      )
    } else if (titles[i] == "Average Hourly Statistics for Direct Normal Solar Radiation") {
      print(
        ggplot() +
          geom_point(
            data = hourly_data,
            aes(x = Hours, y = Direct_Normal_Solar_Radiation, color = Months)
          ) + theme(axis.text.x = element_text(angle = 90))
      )
    } else {
      print(
        ggplot() +
          geom_point(data = hourly_data, aes(
            x = Hours, y = Wind_Speed, color = Months
          )) + theme(axis.text.x = element_text(angle = 90))
      )
    }
  }
}
