# Plot histogram of global active power
# Assumes household_power_consumption.txt.


plot1 <- function(file = 'household_power_consumption.txt') {
    
    ## Data parser to read the Electric power consumption data
    ## Hardcoded to read only 2880 data entries, starting from 2007-02-01 00:00:00.
    ## 
    ## 
    ## ARGUMENTS
    ## file: string indicating file path to the txt file holding the data.
    ## 
    ## 
    ## 
    ## RETURNS
    ## data: parsed data stored in a table
    data_parser <- function(file = 'household_power_consumption.txt') {
        
        # Compute number of lines that must be skipped.
        txt <- readLines(file)
        
        # grep matching date string using regexp for both 2007-02-01 and 2007-02-02
        grep_result <- grep('^[12]/2/2007', txt)
        
        # number of lines to skip is right before the first matched index 
        skip_num <- grep_result[1] - 1
        
        # number of lines to read is just the number of index matched
        nrows <- length(grep_result)
        
        # Only really start reading when the line starts with the starting date.
        raw_data <- read.table(file, skip = skip_num, nrows = nrows, header = FALSE, 
                               sep = ';', na.strings = '?')
        
        # add the column names
        colnames(raw_data) <- names(read.table(file, nrows = 1, header = TRUE, 
                                               sep = ';'))
        
        # Create time stamps from raw_data using datetime objects
        Timestamps <- strptime(mapply(paste, raw_data$Date, raw_data$Time,
                                      MoreArgs = list(sep = ' '), SIMPLIFY = TRUE),
                               format = '%d/%m/%Y %H:%M:%S', tz = 'GMT')
        
        
        # Create new data frame with R datetime objects within range
        data <- data.frame(Timestamps, raw_data[3:length(raw_data)])
        
    }
    
    
    # Parse data
    data <- data_parser()
    
    # open PNG device
    png('plot1.png')
    
    # plot histogram
    hist(data$Global_active_power, col = 'red', main = 'Global Active Power', 
         xlab = 'Global Active Power (kilowatts)')
    
    # save and close device
    dev.off()
}