install.packages("dplyr")
install.packages("lubricate")
install.packages("scales")
install.packages("ggplot2")
library(dplyr)
library(lubridate)
library(scales)
library(ggplot2)

temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", temp)
data <- read.csv2(unz(temp, "household_power_consumption.txt"), header=TRUE, sep=";", dec=",")
unlink(temp)


test7 <- select(filter(data, Date=="1/2/2007" | Date=="2/2/2007"),c(Date,Time,Global_active_power))

test7$Global_active_power_num <- as.numeric(test7$Global_active_power)

test7$DateTime <- as.POSIXct(paste(test7$Date, test7$Time), format="%d/%m/%Y %H:%M:%S")

test7$WorkDay <- wday(test7$DateTime)

png(file="plot2.png", width=480, height=480)

ggplot(test7, aes(x=DateTime, y=Global_active_power_num))+geom_line()+xlab("")+ ylab("Global Active Power (kilowatts)") + 
  scale_x_datetime(breaks =  seq.POSIXt( ceiling_date(min(test7$DateTime), unit = "day"),
                                      ceiling_date(max(test7$DateTime), unit = "day") , by="1 day"), labels=c("Thu","Fri","Sat")) +
	theme_classic()+
	theme(panel.background=element_rect(colour="black"))

dev.off()