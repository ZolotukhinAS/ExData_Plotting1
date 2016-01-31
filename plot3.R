# Reading the data. File should exists in current working directory
# Data read is cached
# FileName - Name of the file to read
# FilterDates - what dates to use in analysis
# Default values should be fine for this course assetment task

ReadData <- function (FileName = "household_power_consumption.txt", 
                      FilterDates = c("1/2/2007","2/2/2007")){
    if (exists("SourceData")) {
        
        return(SourceData)
    }
    SourceData <<- subset(read.table("household_power_consumption.txt", 
                                     header = T, sep = ";", stringsAsFactors = F, 
                                     na.strings = "?"),
                          Date %in% FilterDates)
    # Gathering Datetine from Date and Time columns
    SourceData$DateTime <<- with(SourceData, strptime(paste(Date,Time), 
                                                      format = "%d/%m/%Y %H:%M:%S"))
    # we don't need these columns anymore
    SourceData$Date <<- NULL
    SourceData$Time <<- NULL
    SourceData
}

Plot1Drawing <- function(){
    # getting data
    d<- ReadData()
    
    # opening a device
    png(filename = "plot1.png",width = 480, height = 480)
        
    hist(d$Global_active_power, main = "Global Active Power",
         xlab = "Global Active Power (kilowatts)",
         ylab = "Frequency", col = "red")
    dev.off()
}

Plot2Drawing <- function(){
    d<- ReadData()
    png(filename = "plot2.png",width = 480, height = 480)
    
    plot(x = d$DateTime, y = d$Global_active_power, type = "n", 
         ylab = "Global Active Power (kilowatts)", xlab = "")
    lines(d$DateTime,d$Global_active_power)
    dev.off()
}


Plot3Drawing <- function(){
    d<- ReadData()
    png(filename = "plot3.png",width = 480, height = 480)
    
    plot(x = d$DateTime, y = d$Sub_metering_1, type = "n", 
         ylab = "Energy sub metering", xlab = "")
    lines(d$DateTime,d$Sub_metering_1, col = "black")
    lines(d$DateTime,d$Sub_metering_2, col = "red")
    lines(d$DateTime,d$Sub_metering_3, col = "blue")
    legend("topright",lwd = 1, col = c("black","red","blue"), 
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    dev.off()
}


Plot4Drawing <- function(){
    d<- ReadData()
    png(filename = "plot4.png",width = 480, height = 480)
    
    OldPar <- par()
    par(mfcol = c(2,2))
    
    plot(x = d$DateTime, y = d$Global_active_power, type = "n", 
         ylab = "Global Active Power", xlab = "")
    lines(d$DateTime,d$Global_active_power)
    
    plot(x = d$DateTime, y = d$Sub_metering_1, type = "n", 
         ylab = "Energy sub metering", xlab = "")
    lines(d$DateTime,d$Sub_metering_1, col = "black")
    lines(d$DateTime,d$Sub_metering_2, col = "red")
    lines(d$DateTime,d$Sub_metering_3, col = "blue")
    legend("topright",lwd = 1, col = c("black","red","blue"), 
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
            bty = "n")
    
    plot(x = d$DateTime, y = d$Voltage, type = "n", 
         ylab = "Voltage", xlab = "datetime")
    lines(d$DateTime,d$Voltage)
    
    
    
    plot(x = d$DateTime, y = d$Global_reactive_power, type = "n", 
         ylab = "Global_reactive_power", xlab = "datetime")
    lines(d$DateTime,d$Global_reactive_power)
    
    par(OldPar)
    
    dev.off()
}