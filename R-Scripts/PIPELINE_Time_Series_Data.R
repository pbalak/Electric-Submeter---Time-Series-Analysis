# R-Pipeline-Filtering-TimeSeries

# Last updated: 2016-11-14

###############
# Housekeeping
###############

# Clear all variables from R
rm(list = ls())

# Set working directory
getwd()
setwd()
dir()


################################
## Install and load packages
################################

install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
require(lubridate) # work with dates
require(dplyr)     # data manipulation (filter, summarize, mutate)
require(tidyr)


###############
# Load dataset 
###############

hhpwr <- read.csv("household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE, header = T)
class(hhpwr)
str(hhpwr)


##################
# Pre-process DS 
##################

#------Create a DateTime col by using unite() in tidyr-------------#

# as.Date() is an R method [R - Date Class - as.Date]
# If use as.Date, will lose any time stamp; (time less than a day)
# as.POSIXct will preserve time stamp; [R - Date-Time - POSIX classes]
# as.POSIXct stores both a date and time with an associated time zone. 
# Default is tz of your computer.
# Be sure to keep the date format the same (e.g.,"%d/%m/%Y %H:%M:%S")

# combine Date and Time using unite in tidyr
hhpwrDT <- hhpwr %>% unite(DateTime, Date, Time, sep = " ", remove = FALSE)
# Could remove Date and Time by remove = TRUE and could add back using spread()
# convert DateTime to POSIXct
hhpwrDT$DateTime <- as.POSIXct(hhpwrDT$DateTime,
                               format = "%d/%m/%Y %H:%M:%S",
                               tz = "America/New_York")

class(hhpwrDT$DateTime) #[1] "POSIXct" "POSIXt" 
tz(hhpwrDT$DateTime) # "America/New_York"

# convert Date to as.Date
hhpwrDT$Date <- as.Date(hhpwrDT$Date, "%d/%m/%Y")

str(hhpwrDT)

##------- Change data types---------##

# Note: Understand the difference between as.numeric(as.character()) and as.numeric()

hhpwrDT$Global_active_power <- as.numeric(as.character(hhpwr$Global_active_power))
hhpwrDT$Global_reactive_power <- as.numeric(as.character(hhpwr$Global_reactive_power))
hhpwrDT$Voltage <- as.numeric(as.character(hhpwr$Voltage))
hhpwrDT$Global_intensity <- as.numeric(as.character(hhpwr$Global_intensity))
hhpwrDT$Sub_metering_1 <- as.numeric(as.character(hhpwr$Sub_metering_1))
hhpwrDT$Sub_metering_2 <- as.numeric(as.character(hhpwr$Sub_metering_2))
str(hhpwrDT)


## ------ Evaluate NA values ----------##

# Are there any NAs in df?
any(is.na(hhpwrDT)) 
# Count the number of values = NA
sum(is.na(hhpwrDT$Sub_metering_1)) # Review any metadata with dataset


## -------- Save pre-processed dataset --------##

# Save file, or
# Save object

#####################
# Filtering pipeline
#####################

##----Process Steps from Sub-setting to Graphing------#
# 1. dplyr::mutate(): filter for time interval (Yr/Mo/Day) using lubridate w/in dplyr mutate() to create col.
# 2. dplyr::filter(): select cols to filter by; full ds + col from mutate are avail to filter at this stage
# 3. dplyr::group_by(): select desired time interval to subset
# 4. dplyr::summarize(): select which vars and any calculations for the vars
# 5. dplyr::first(): add DateTime col to end summary() string using first() (not needed for forecasting)
# 6. dplyr::filter() to remove any NA or narrow data ranges 


#############
## DS ANNUAL 
#############

# MAIN DS
# Total kWh per SM by year 

yr.sum.wide <- hhpwrDT %>%
  mutate(Year = year(DateTime)) %>%  # Add col Year using dplyr and lubridate functions
  filter(Year==2007 | Year==2008 | Year==2009) %>%
  group_by(Year) %>%  # Group data by Year
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance
yr.sum.wide
names(yr.sum.wide)
class(yr.sum.wide)  

# Note: For time intervals less than a year, you may need to create a main dataset for that time interval (e.g., month, week, day), and then subset this main time interval subset to get the final results you need. 

