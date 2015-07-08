library(dplyr)
library(data.table)

plot1 <- function(){
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
    par(cex=0.75, mar=c(5,4,4,2) + 0.2) # reduce font size, increase margin
    plot1 <- hist(mypowerdata$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
    dev.copy(png, "plot1.png", units = "px", width = 480, height = 480)
    dev.off()
}

