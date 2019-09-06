pollutantmean <- function(directory, pollutant, id=1:332) {
    data <- data.frame()
    filesNames <- paste(directory, '/', formatC(id, width = 3, flag = '0'), '.csv', sep = '')
    for (file in filesNames){
        dat <- read.csv(file, as.is = T, header = T)
        data <- rbind(data,dat)  
    }
    mean(data[,pollutant], na.rm = TRUE)
}

complete <- function(directory, id=1:332) {
    data <- data.frame()
    for (each in id){
        filesNames <- paste(directory, '/', formatC(each, width = 3, flag = '0'), '.csv', sep = '')
        dat <- read.csv(filesNames, as.is = T, header = T)
        nobs <- sum(complete.cases(dat))
        d2 <- data.frame(each, nobs)
        data <- rbind(data, d2)
    }
    data
}

corr <- function(directory, threshold=0){
    filesNames <- list.files(path = directory, pattern="*.csv")
    corr_vect <- NULL
    for (each in filesNames){
        dat <- read.csv(paste(directory, '/', each, sep = ''), as.is=T)
        dat <- dat[complete.cases(dat),]
        if (nrow(dat)>threshold){
            corr_vect <- c(corr_vect, cor(dat[,'sulfate'], dat[,'nitrate']))
            }
    }
    corr_vect
}

