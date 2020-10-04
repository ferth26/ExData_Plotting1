library(lubridate)

dataset <-"./data/household_power_consumption.txt"
dataURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

# Check if the data directory exists
if (!dir.exists("./data")) {
  dir.create("./data")
}

# Check if dataset exists
if (!file.exists(dataset)) {
  # Download dataset
  download.file(url = dataURL, destfile = "./data/exdata.zip", method = "curl")
  # Unzip dataset
  unzip(zipfile = "./data/exdata.zip", exdir = "./data")
  file.remove("./data/exdata.zip")
}

# Loading the data
initial.data <- read.table(dataset, header = T, nrows = 10, sep = ";")
data.c <- sapply(initial.data, class)

data <-
  read.table(
    dataset,
    header = T,
    sep = ";",
    colClasses = data.c,
    na.strings = "?",
    comment.char = "",
    nrows = 2075259
  )

data$Date <- with(data, as.Date(Date, "%d/%m/%Y"))
# data$Time <- with(data, strptime(Time, format = "%H:%M:%S", ))

data.fil <- filter(data, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))


# Making plots
data.fil <- mutate(data.fil, DateTime = ymd_hms(paste(Date, Time, sep = " ")))


plot(
  formula = Global_active_power~DateTime,
  data = data.fil,
  type = "l",
  ylab = "Global Active Power (kilowatts)",
  xlab = NA
)

# Saving plot

dev.copy(png, "plot2.png", width  = 480, height = 480)
dev.off()





