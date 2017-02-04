## Plot line graph of global active power vs datetimes
## Assumes household_power_consumption.txt is in the current work folder.
## 
## ARGUMENTS:
## file: filepath to txt holding the data.
## 
## device: graphical output device (default to png). The only other accetable
## value is 'display'
## 
## data: optional argument to pass in formatted dataframe object for the plot
##
## Note: this function allows optional arbitrary plot settings to be specified 
## in the arguments by the user.  This is meant to for label differences in 
## plot 4.


plot2 <- function(file = 'household_power_consumption.txt', device = 'png', 
                  data = NULL, ...) {
    
    # Date when data was downloaded if the internal download routine was used.
    file_name <- 'Existing data used.'
    
    # check if file exists. If it doesn't, try default. If still fails, 
    # download & extract, then set file back to default.
    if (!file.exists(file)) {
        if (!file.exists('household_power_consumption.txt')) {
            # Set file name to the download date.
            file_name <- paste(as.Date(Sys.time()), '.zip')
            
            download.file('https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip', 
                          destfile = file_name)
            
            unzip(file_name)
        }
        
        
        file <- 'household_power_consumption.txt'
        
    }
    
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
            data <- data_parser(file)
        }
        
        if (device == 'png') {
            # open PNG device
            png('plot2.png')
        }
        
        args <- list(...)
        
        if (length(args)) {
            # plot line graph with user specified setting
            plot(data$Timestamps, data$Global_active_power, ...)
            
            
        } else {
            # plot line graph with default setting
            plot(data$Timestamps, data$Global_active_power, type = 'l', 
                 ylab = 'Global Active Power (kilowatts)', xlab = '')
            
        }
        
        if (device == 'png') {
            # save and close device
            dev.off()
        }
        
    } else {
        stop('invalid graphic device')
    }
    
    # Return name of the current data zip or mention existing data used.
    file_name
}