library(dplyr)

clean_data <- function(dataset, start.date, end.date) {
  # Create date time column
  dataset$strdatetime <- paste(dataset$Date, as.character(dataset$Time))
    
  # Exclude data that are not within the dates of interest
  dataset$Date <- as.Date(dataset$Date, format="%d/%m/%Y")
  dataset      <- subset(dataset, Date >= start.date & Date <= end.date)
  
  # Convert appropriate columns from Factors to numerics
  dataset$Global_active_power <- as.numeric(as.character(dataset$Global_active_power))
  
  dataset$timestamp <- strptime(dataset$strdatetime, "%d/%m/%Y %H:%M:%S", tz="GMT")
  
  dataset  
}

# Create the plot as a PNG file
create_plot2_png <- function(dataset, filename) {
  png(filename = filename, width = 480, height = 480, units = "px")
  plot(dataset$timestamp, dataset$Global_active_power, xlab="", ylab="Global Active Power (kilowatts)", type="l")
  dev.off()
}

# Download the zip file from the Internet which contains the raw data
# and then extract (unzip) the data from the zip file
download_raw_data <- function() {
  rawDataURL        <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  zipFileName       <- "household_power_consumption.zip"
  raw_data_filename <- "household_power_consumption.txt"
  
  if (file.exists(zipFileName)) {
    paste("Not downloading", zipFileName, "since it already exists")
  } else {
    download.file(rawDataURL, destfile = zipFileName, method="curl")
    unzip(zipFileName)
  }
  
  raw_data_filename
}

# Read the raw data from a file into a data frame
read_raw_data <- function(raw_data_filename) {
  household.power.consumption <- read.table(raw_data_filename, header=TRUE, sep=";")
}

filename   <- "plot2.png"
start.date <- "2007-02-01"
end.date   <- "2007-02-02"

raw_data_filename <- download_raw_data()
dataset <- read_raw_data(raw_data_filename) %>% clean_data(start.date, end.date)
create_plot2_png(dataset, filename)
