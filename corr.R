corr <- function(directory, threshold = 0) {

    files <- list.files(directory, full.names = TRUE)
    results <- vector("numeric")
    for (i in files) {
        new_data <- read.csv(i)
        good <- complete.cases(new_data)
        if (nrow(new_data[good,]) > threshold) {
            dd <- new_data[good,]
            results <- c(results, cor(dd$sulfate, dd$nitrate))
        }
    }
    results
}