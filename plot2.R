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


Plot2Drawing <- function(){
    d<- ReadData()
    png(filename = "plot2.png",width = 480, height = 480)
    
    plot(x = d$DateTime, y = d$Global_active_power, type = "n", 
         ylab = "Global Active Power (kilowatts)", xlab = "")
    lines(d$DateTime,d$Global_active_power)
    dev.off()
}
