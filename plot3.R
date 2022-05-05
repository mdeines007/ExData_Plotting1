install.packages("dplyr")
install.packages("lubricate")
install.packages("scales")
install.packages("cowplot")
install.packages("ggplot2")
library(dplyr)
library(lubridate)
library(scales)
library(ggplot2)
library(cowplot)

temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", temp)
data <- read.csv2(unz(temp, "household_power_consumption.txt"), header=TRUE, sep=";", dec=",")
unlink(temp)



test8 <- select(filter(data, Date=="1/2/2007" | Date=="2/2/2007"),c(Date,Time,Sub_metering_1, ,Sub_metering_2,Sub_metering_3))

test8$Sub_metering_1_num <- as.numeric(test8$Sub_metering_1)
test8$Sub_metering_2_num <- as.numeric(test8$Sub_metering_2)
test8$Sub_metering_3_num <- as.numeric(test8$Sub_metering_3)

test8$DateTime <- as.POSIXct(paste(test8$Date, test8$Time), format="%d/%m/%Y %H:%M:%S")


colors <- c("Sub_meeting_1" = "black", "Sub_meeting_2" = "red", "Sub_meeting_3" = "blue")

png(file="plot3.png", width=480, height=480)

ggplot(test8, aes(x=DateTime))+
  geom_line(aes(y=Sub_metering_1_num, color = "Sub_meeting_1"))+
  geom_line(aes(y=Sub_metering_2_num, color = "Sub_meeting_2"))+
  geom_line(aes(y=Sub_metering_3_num, color = "Sub_meeting_3"))+
  labs(x = "",
       y = "Energy sub metering",
	color = "Legend")+
  scale_color_manual(values = colors)+
  scale_x_datetime(breaks =  seq.POSIXt( ceiling_date(min(test8$DateTime), unit = "day"),
                                      ceiling_date(max(test8$DateTime), unit = "day") , by="1 day"), labels=c("Thu","Fri","Sat")) +
  theme_classic()+
  theme(panel.background=element_rect(colour="black"),
	legend.position = c(1,1),
	legend.justification = c(1,1),
	legend.title=element_blank(),
	legend.background = element_rect(fill = "white", colour = "black"))

dev.off()