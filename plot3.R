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

# png
png("plot3.png", width = 480, height = 480)

with(data.fil, {
  plot(
    Sub_metering_1 ~ DateTime,
    type = "l",
    ylab = "Energy sub metering",
    xlab = NA
  )
  lines(Sub_metering_2 ~ DateTime, type = "l", col = "red")
  lines(Sub_metering_3 ~ DateTime, type = "l", col = "blue")
})

legend(
  x = "topright",
  col = c("black", "red", "blue"),
  lty = 1,
  legend = c("Sub_metering_1",
             "Sub_metering_2",
             "Sub_metering_3"),
)


# Saving plot
dev.off()
