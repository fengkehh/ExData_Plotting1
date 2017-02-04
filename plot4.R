## (2,2) matrix plot of the following (top left to bottom right, row first)
## 1. Global Active Power vs datetime
## 2. Voltage vs datetime
## 3. Energy sub metering vs datetime
## 4. Global Active Power vs datetime
## Assumes household_power_consumption.txt, plot2.R and plot3.R are in the 
## current work folder.
## 
## ARGUMENTS:
## file: filepath to txt holding the data.
## 
## device: graphical output device (default to png). The only other accetable
## value is 'display'
## 
## data: optional argument to pass in formatted dataframe object for the plot

plot4 <- function(file = 'household_power_consumption.txt', device = 'png',
                  data = NULL) {
    
    ## Data parser to read the Electric power consumption data
    ## Hardcoded to start from the first entry on 2007-02-01 and stop at last 
    ## entry for 2007-02-02.
    ## 
    ## 
    ## ARGUMENTS
    ## file: string indicating file path to the txt file holding the data.
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
    
    # load required functions
    source('plot2.R')
    source('plot3.R')
    
    if (device == 'png' || device == 'display') {
        
        if (is.null(data)) {
            # Parse data
            data <- data_parser()
        }
        
        if (device == 'png') {
            # open png device
            png('plot4.png')
        }
        
        # set matrix format
        par(mfrow = c(2, 2))
        
        ## 1. Global Active Power vs datetime
        plot2(data = data, device = 'display', type = 'l', 
              ylab = 'Global Active Power', xlab = '')
        
        ## 2. Voltage vs datetime
        plot(data$Timestamps, data$Voltage, type = 'l', ylab = 'Voltage', 
             xlab = 'datetime')
        
        ## 3. Energy sub metering vs datetime
        plot3(data = data, device = 'display')
        
        ## 4. Global Active Power vs datetime
        plot(data$Timestamps, data$Global_reactive_power, type = 'l', 
             col = 'black', ylab = 'Global_reactive_power', xlab = 'datetime')
        
        
        if (device == 'png') {
            # save and close device
            dev.off()
        }
        
    } else {
        stop('invalid graphic device')
    }
    
    
}