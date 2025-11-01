
seasonal_pattern <- function(x){
    data <- RJDemetra::get_indicators(x, c("s", "s_f", "y", "preprocessing.model.log"))
    s <- data[["s"]]
    last_date <- time(s)[length(time(s))]
    s <- window(s, start = last_date - 1 + deltat(s))
    freq <- frequency(s)
    estimated_values <- as.numeric(s)
    is_multiplicative <- as.logical(data[["preprocessing.model.log"]])
    evolution <- c(tail(s, 2),
                   head(data[["s_f"]],1))
    if(!is_multiplicative){
        series_mean <- mean(data[["y"]],
                            na.rm = TRUE)
        estimated_values <- estimated_values / series_mean + 1
        evolution <- evolution/series_mean + 1 
    }
    if(freq == 12){
        names(estimated_values) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                                     "Oct", "Nov", "Dec")[cycle(s)]
        period <- "Month"
    }else if (freq == 4){
        names(estimated_values) <- sprintf("Q%i",1:4)[cycle(s)]
        period <- "Quarter"
    } else {
        names(estimated_values) <- paste("P", cycle(s), sep = "")
        period <- "Period"
    }
    estimated_values <- (estimated_values - 1)*100
    evolution <- (evolution - 1)*100
    evolution <- data.frame(Evolution = evolution,
                            row.names = sprintf("%s %s",
                                                c("Previous", "Current", "Next"),
                                                period))
    result <- list(estimated_values = estimated_values, evolution = evolution)
    class(result) <- c("seasonal_pattern", class(result))
    
    result
}
#'@exportS3Method NULL
plot.seasonal_pattern <- function(x, ...){
    
    data_table <- round(x$evolution, 1)
    if(data_table$Evolution[3] > 0){
        title = expression("Seasonal Effects: " %dblup% " expected")
    }else if (data_table$Evolution[3] < 0){
        title = expression("Seasonal Effects: " %dbldown% " expected")
    }else{
        title = expression("Seasonal Effectss: " %=>% " expected")
    }
    data_table$Evolution <- sprintf("%+.1f%%",data_table$Evolution)
    
    mai <- par("mai") 
    barplot(x$estimated_values, names.arg=names(x$estimated_values),ann=FALSE,
            main = "Seasonal Pattern")
    par(mai = c(0,0,0,0))
    plot(0,type='n',axes=FALSE,ann = F, xlim = c(0,1), ylim = c(0,1))
    plotrix::addtable2plot(0.5, 0.5,
                  data_table[-3,, drop=FALSE], bty = "o", display.rownames = TRUE, hlines = TRUE,
                  display.colnames = FALSE,
                  vlines = TRUE,title = title, xjust = 0.5, yjust = 0.5)
    par(mai = mai)
}


