complete <- function(directory, id = 1:332) {
    files <- list.files(directory, full.names = TRUE)
    ids_list <- c()
    nobs_list <- c()
    for (i in id) {
        new_data <- read.csv(files[i])

        ids_list <- c(ids_list, i)
        good <- complete.cases(new_data)
        nobs_list <- c(nobs_list,nrow(new_data[good,]))
    }

    data.frame(id=ids_list, nobs=nobs_list)
}