install.packages("dplyr")
library(dplyr)


temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", temp)
data <- read.csv2(unz(temp, "household_power_consumption.txt"), header=TRUE, sep=";", dec=",")
unlink(temp)

test6 <- select(filter(data, Date=="1/2/2007" | Date=="2/2/2007"),c(Date,Global_active_power))

test6$Global_active_power_num <- as.numeric(test6$Global_active_power)

png(file="plot1.png", width=480, height=480)

hist(test6$Global_active_power_num,col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)", ylim=c(0,1200))

dev.off()