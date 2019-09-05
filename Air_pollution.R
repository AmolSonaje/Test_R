pollutantmean <- function(directory, pollutant, id=1:332) {
    data <- data.frame()
    filesNames <- paste(directory, '/', formatC(id, width = 3, flag = '0'), '.csv', sep = '')
    for (file in filesNames){
        dat <- read.csv(file, as.is = T, header = T)
        data <- rbind(data,dat)  
    }
    mean(data[,pollutant], na.rm = TRUE)
}

