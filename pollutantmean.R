pollutantmean <- function(directory, pollutant, id = 1:332) {
    # read in the file
    files <- list.files(directory, full.names = TRUE)

    for (i in id) {
        new_data <- read.csv(files[i])
        print(class())
        data <- rbind(data,new_data)
        #print(files[i])
    }

    #ldf <- lapply(files[id], read.csv)
    #print('ldf')
    #print(class(ldf))
    #print(class(data))

    #print(nrow(data))
    subset <- data[id,]
    mean(data[, pollutant], na.rm = TRUE, trim = 0)
    #print(mean(data[, pollutant], na.rm = TRUE))

    #print(pollutant)
    # remove the na columns from it
    if (pollutant == 'nitrate') {
        bad <- is.na(data$nitrate)
        mean(data[!bad,]$nitrate)
    } else if (pollutant == 'sulfate') {
        bad <- is.na(data$sulfate)
        mean(data[!bad,]$sulfate)
    }
}

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

corr <- function(directory, threshold = 0) {

    files <- list.files(directory, full.names = TRUE)
    # sulfates <- ()
    # nitrates <- ()
    results <- c()
    for (i in files) {
        new_data <- read.csv(i)
        good <- complete.cases(new_data)
        if (nrow(new_data[good,]) >= threshold) {
            dd <- new_data[good,]
            results <- c(results, cor(dd$sulfate, dd$nitrate))
            # ll <- data.frame(sulfate =dd$sulfate, nitrate = dd$nitrate)

            # add these into sulfates and nitrates
            # print('file is ')
            # print(i)
        }
        # data <- rbind(data,new_data)
        #print(files[i])
    }

    results

    # df <- complete(directory)
    # print(nrow(df[df$nobs > threshold,]))
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
}