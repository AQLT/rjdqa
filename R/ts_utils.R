ev <- function(x){
    UseMethod("ev", x)
}
ev.ts <- function(x){
    result <- (x/stats::lag(x, k = -1) - 1) * 100
    return(result)
}
ev.mts <- function(x){
    result <- (x/stats::lag(x, k = -1) - 1) * 100
    colnames(result) <- colnames(x)
    return(result)
}
ev.xts <- function(x){
    result <- (x / stats::lag(x, k = 1) - 1) * 100
    return(result)
}

time_to_date <- function(x){
    time <- round(time(x),3)
    years <- as.integer(round(time - (time %% 1), 1))
    months <- as.integer((time %% 1) * 12 + 1)
    dates <- sprintf("%i-%02d-01", years, months)
    dates <- as.Date(dates)
    dates
}
sprintf("%08d", c(25499,25500,25501,25502,25503,25504))
class( c(25499,25500,25501,25502,25503,25504))
class(months)
months+1
