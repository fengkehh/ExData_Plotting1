## Plot line graph of energy sub meterings vs datetimes
## Assumes household_power_consumption.txt is in the current work folder.
## 
## ARGUMENTS:
## file: filepath to txt holding the data.
## 
## device: graphical output device (default to png). The only other accetable
## value is 'display'
## 
## data: optional argument to pass in formatted dataframe object for the plot

plot3 <- function(file = 'household_power_consumption.txt', device='png', 
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
    
    
    if (device == 'png' || device == 'display') {
        
        if (is.null(data)) {
            # Parse data
            data <- data_parser()
        }
        
        if (device == 'png') {
            # open PNG device
            png('plot3.png')
        }
        
        # Plot line graphs
        
        # color array
        cols <- c('black', 'red', 'blue')
        
        # submeter 1 and set up y label
        plot(data$Timestamps, data$Sub_metering_1, type = 'l', col = cols[1], 
             ylab = 'Energy sub metering', xlab = '')
        
        # submeter 2
        lines(data$Timestamps, data$Sub_metering_2, type = 'l', col = cols[2])
        
        # submeter 3
        lines(data$Timestamps, data$Sub_metering_3, type = 'l', col = cols[3])
        
        # add legend
        legend('topright', legend = 
                   names(data)[(length(names(data)) - 2):length(names(data))], 
               col = cols, lty = 1)
        
        if (device == 'png') {
            # save and close device
            dev.off()
        }
    } else {
        stop('invalid graphic device')
    }
    
}