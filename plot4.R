install.packages("dplyr")
install.packages("lubricate")
install.packages("scales")
install.packages("ggplot2")
install.packages("cowplot")
library(dplyr)
library(lubridate)
library(scales)
library(ggplot2)
library(cowplot)

temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", temp)
data <- read.csv2(unz(temp, "household_power_consumption.txt"), header=TRUE, sep=";", dec=",")
unlink(temp)

test9 <- select(filter(data, Date=="1/2/2007" | Date=="2/2/2007"),c(Date,Time,Voltage,Global_reactive_power,Global_active_power,Sub_metering_1, ,Sub_metering_2,Sub_metering_3))

test9$Voltage_num <- as.numeric(test9$Voltage)

test9$Global_reactive_power_num <- as.numeric(test9$Global_reactive_power)

test9$Global_active_power_num <- as.numeric(test9$Global_active_power)

test9$Sub_metering_1_num <- as.numeric(test9$Sub_metering_1)
test9$Sub_metering_2_num <- as.numeric(test9$Sub_metering_2)
test9$Sub_metering_3_num <- as.numeric(test9$Sub_metering_3)

test9$DateTime <- as.POSIXct(paste(test9$Date, test9$Time), format="%d/%m/%Y %H:%M:%S")

test9$WorkDay <- wday(test9$DateTime)

plot1 <- ggplot(test9, aes(x=DateTime, y=Voltage_num))+geom_line()+xlab("datetime")+ ylab("Voltage") + 
  scale_x_datetime(breaks =  seq.POSIXt(ceiling_date(min(test9$DateTime), unit = "day"),
                                      ceiling_date(max(test9$DateTime), unit = "day") , by="1 day"), labels=c("Thu","Fri","Sat")) + 
  scale_y_continuous(breaks = c(234,236,238,240,242,244,246),
					label=c("234","","238","","242","","246"))+
	theme_classic()+
	theme(panel.background=element_rect(colour="black"),
            axis.title.x = element_text(margin=margin(t=20)), 
      	axis.title.y = element_text(margin=margin(r=20)),
            plot.margin = margin(b=30, l=10, r=10))


plot2 <- ggplot(test9, aes(x=DateTime, y=Global_reactive_power_num))+geom_line()+xlab("datetime")+ylab("Global_reactive_power")+  
  scale_x_datetime(breaks =  seq.POSIXt(ceiling_date(min(test9$DateTime), unit = "day"),
                                      ceiling_date(max(test9$DateTime), unit = "day") , by="1 day"), labels=c("Thu","Fri","Sat")) + 
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5))+
	theme_classic()+
	theme(panel.background=element_rect(colour="black"),
            axis.title.x = element_text(margin=margin(t=20)), 
      	axis.title.y = element_text(margin=margin(r=20)),
            plot.margin = margin(b=30, l=20, r=10))


plot3 <- ggplot(test9, aes(x=DateTime, y=Global_active_power_num))+geom_line()+xlab("")+ ylab("Global Active Power (kilowatts)") + 
  scale_x_datetime(breaks =  seq.POSIXt( ceiling_date(min(test9$DateTime), unit = "day"),
                                      ceiling_date(max(test9$DateTime), unit = "day") , by="1 day"), labels=c("Thu","Fri","Sat")) +
	theme_classic()+
	theme(panel.background=element_rect(colour="black"),
            axis.title.x = element_text(margin=margin(t=20)), 
      	axis.title.y = element_text(margin=margin(r=20)),
            plot.margin = margin(b=30, l=20, r=10))



colors <- c("Sub_meeting_1" = "black", "Sub_meeting_2" = "red", "Sub_meeting_3" = "blue")

plot4 <- ggplot(test9, aes(x=DateTime))+
  geom_line(aes(y=Sub_metering_1_num, color = "Sub_meeting_1"))+
  geom_line(aes(y=Sub_metering_2_num, color = "Sub_meeting_2"))+
  geom_line(aes(y=Sub_metering_3_num, color = "Sub_meeting_3"))+
  labs(x = "",
       y = "Energy sub metering",
	color = "Legend")+
  scale_color_manual(values = colors)+
  scale_x_datetime(breaks =  seq.POSIXt( ceiling_date(min(test9$DateTime), unit = "day"),
                                      ceiling_date(max(test9$DateTime), unit = "day") , by="1 day"), labels=c("Thu","Fri","Sat")) +
  theme_classic()+
  theme(panel.background=element_rect(colour="black"),
	legend.position = c(0.993,0.994),
	legend.justification = c(1,1),
	legend.title=element_blank(),
	legend.background = element_rect(fill = "white", colour = "white"),
            axis.title.x = element_text(margin=margin(t=20)), 
      	axis.title.y = element_text(margin=margin(r=20)),
            plot.margin = margin(b=30, l=20, r=10))

png(file="plot4.png", width=480, height=480)

plot_grid(plot3, plot1, plot4, plot2, ncol=2, nrow=2)

dev.off()