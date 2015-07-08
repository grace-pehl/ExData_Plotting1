library(dplyr)
library(data.table)

plot1 <- function(){
    if (!file.exists("household_power_consumption.txt")) {
        dataUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        download.file(dataUrl, "household_power_consumption.zip")
        unzip("household_power_consumption.zip")
        powerdata <- fread("household_power_consumption.txt", verbose=TRUE)
        powerdata[powerdata == "?"] <- NA
        tbl_df(powerdata)
        mypowerdata <- filter(powerdata, Date == "1/2/2007" | Date == "2/2/2007")
        rm(powerdata)
        mypowerdata <- mypowerdata[, Global_active_power:=as.numeric(Global_active_power)]
    }
    plot1 <- hist(mypowerdata$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
    dev.copy(png, "plot1.png", units = "px", width = 480, height = 480)
    dev.off()
}

