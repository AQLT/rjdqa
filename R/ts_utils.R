ev <- function(x){
    UseMethod("ev", x)
}
#'@exportS3Method NULL
ev.ts <- function(x){
    result <- (x/stats::lag(x, k = -1) - 1) * 100
    return(result)
}
#'@exportS3Method NULL
ev.mts <- function(x){
    result <- (x/stats::lag(x, k = -1) - 1) * 100
    colnames(result) <- colnames(x)
    return(result)
}
#'@exportS3Method NULL
ev.xts <- function(x){
    result <- (x / stats::lag(x, k = 1) - 1) * 100
    return(result)
}

time_to_date <- function(x){
    time <- time(x)
    years <- as.integer(round(round(time,3) - (round(time,3) %% 1), 1))
    months <- as.integer((time %% 1) * 12 + 1)
    dates <- sprintf("%i-%02d-01", years, months)
    dates <- as.Date(dates)
    dates
}

