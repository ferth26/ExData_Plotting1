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

# Multiple graphs in a single plot
par(
  mfrow = c(2, 2),
  mar = c(4, 4, 2, 1),
  oma = c(0, 0, 2, 0)
)

with(data = data.fil, expr = {
  plot(
    Global_active_power ~ DateTime,
    type = "l",
    ylab = "Global Active Power",
    xlab = NA
  )
  
  plot(Voltage ~ DateTime,
       type = "l",
       xlab = "datetime")
  
  plot(
    Sub_metering_1 ~ DateTime,
    type = "l",
    ylab = "Energy sub metering",
    xlab = NA
  )
  
  lines(Sub_metering_2 ~ DateTime, type = "l", col = "red")
  lines(Sub_metering_3 ~ DateTime, type = "l", col = "blue")
  
  legend(
    x = "topright",
    col = c("black", "red", "blue"),
    legend = c("Sub_metering_1",
               "Sub_metering_2",
               "Sub_metering_3"),
    lty = 1,
    bty = "n"
  )
  
  plot(Global_reactive_power ~ DateTime,
       type = "l", xlab = "datetime")
})


# Saving plot
dev.copy(png, "plot4.png", width  = 480, height = 480)
dev.off()
