# Reading the data. File should exists in current working directory
# Data read is cached
# FileName - Name of the file to read
# FilterDates - what dates to use in analysis
# Default values should be fine for this course assetment task


# usage example:
# Plot1Drawing()


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
