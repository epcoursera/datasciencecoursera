pollutantmean <- function(directory, pollutant, id = 1:332) {
    # read in the file
    files <- list.files(directory, full.names = TRUE)

    data <- data.frame()
    for (i in id) {
        new_data <- read.csv(files[i])
        print(i)
        data <- rbind(data,new_data)
    }
    subset <- data[id,]
    return
    mean(data[, pollutant], na.rm = TRUE, trim = 0)
    if (pollutant == 'nitrate') {
        bad <- is.na(data$nitrate)
        mean(data[!bad,]$nitrate)
    } else if (pollutant == 'sulfate') {
        bad <- is.na(data$sulfate)
        mean(data[!bad,]$sulfate)
    }
}