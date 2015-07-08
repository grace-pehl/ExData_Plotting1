library(dplyr)
library(data.table)

plot4 <- function(){
   if (!file.exists("household_power_consumption.txt")) {
        dataUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        download.file(dataUrl, "household_power_consumption.zip")
        unzip("household_power_consumption.zip")
        powerdata <- fread("household_power_consumption.txt", verbose=TRUE)
        # since na.strings doesn't work, do it manually
        powerdata[powerdata == "?"] <- NA
        # convert to be able to use dplyr commands
        tbl_df(powerdata)
        # select only two days of interest out of huge file
        mypowerdata <- filter(powerdata, Date == "1/2/2007" | Date == "2/2/2007")
        # free up memory
        rm(powerdata)
        # convert columns to numeric from str
        mypowerdata <- mypowerdata[, Global_active_power:=as.numeric(Global_active_power)]
        mypowerdata <- mypowerdata[, Global_reactive_power:=as.numeric(Global_reactive_power)]
        mypowerdata <- mypowerdata[, Voltage:=as.numeric(Voltage)]
        mypowerdata <- mypowerdata[, Global_intensity:=as.numeric(Global_intensity)]
        mypowerdata <- mypowerdata[, Sub_metering_1:=as.numeric(Sub_metering_1)]
        mypowerdata <- mypowerdata[, Sub_metering_2:=as.numeric(Sub_metering_2)]
        mypowerdata <- mypowerdata[, Sub_metering_3:=as.numeric(Sub_metering_3)]
        # convert Date and Time to Datetime
        mypowerdata <- mutate(mypowerdata,
                              DateTime = as.POSIXct(strptime(paste(Date, Time), "%d/%m/%Y %T")))
    }
    x <- mypowerdata$DateTime
    y1 <- mypowerdata$Global_active_power
    y2 <- mypowerdata$Voltage
    y3a <- mypowerdata$Sub_metering_1
    y3b <- mypowerdata$Sub_metering_2
    y3c <- mypowerdata$Sub_metering_3
    y4 <- mypowerdata$Global_reactive_power
    par(mfrow=c(2, 2), lwd=0.1, cex=0.6)
    plot(x, y1, type="n", ylab = "Global Active Power", xlab = NA)
    lines(x, y1)
    plot(x, y2, type="n", ylab = "Voltage", xlab = "datetime")
    lines(x, y2)
    plot(x, y3a, type="n", ylab = "Energy sub metering", xlab = NA)
    lines(x, y3a)
    lines(x, y3b, col="red")
    lines(x, y3c, col="blue")
    legend(x="topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty = "n", lty=c(1,1,1), col=c("black", "red", "blue"))
    plot(x, y4, type="n", xlab="datetime", ylab="Global_reactive_power")
    lines(x, y4)
    dev.copy(png, "plot4.png", units = "px", width = 480, height = 480)
    dev.off()
}

