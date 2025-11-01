moving_holiday_pattern <- function(x){
    data <- RJDemetra::get_indicators(x, c("preprocessing.model.mhe", "preprocessing.model.mhe_f","y", "preprocessing.model.log"))
    
    is_multiplicative <- as.logical(data[["preprocessing.model.log"]])
    
    mhe <- data[["preprocessing.model.mhe"]]
    last_date <- time(mhe)[length(time(mhe))]
    mhe <- window(mhe, start = last_date - 1 + deltat(mhe))
    freq <- frequency(mhe)
    
    if (is_multiplicative) {
        mhe <- exp(mhe)
    }
    
    estimated_values <- as.numeric(mhe)
    evolution <- c(tail(mhe, 2),
                   head(data[["preprocessing.model.mhe_f"]],1))
    if (!is_multiplicative) {
        series_mean <- mean(data[["y"]],
                            na.rm = TRUE)
        estimated_values <- as.numeric(estimated_values) / series_mean + 1
        evolution <- evolution/series_mean + 1 
    }
    
    if(freq == 12){
        names(estimated_values) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                                     "Oct", "Nov", "Dec")[cycle(mhe)]
        period <- "Month"
    }else if (freq == 4){
        names(estimated_values) <- sprintf("Q%i",1:4)[cycle(mhe)]
        period <- "Quarter"
    } else {
        names(estimated_values) <- paste("P", cycle(mhe), sep = "")
        period <- "Period"
    }
    estimated_values <- (estimated_values - 1) * 100
    evolution <- (evolution - 1) * 100
    evolution <- data.frame(
        Evolution = evolution,
        row.names = sprintf(
            "%s %s",
            c("Previous", "Current", "Next"),
            period)
    )
    result <- list(estimated_values = estimated_values, evolution = evolution)
    class(result) <- c("moving_holiday_pattern", class(result))
    result
}
#'@exportS3Method NULL
plot.moving_holiday_pattern <- function(x, ...){
    if(is.null(x$evolution)){
        plot(0,type='n',axes=FALSE,main="Moving Holiday Pattern")
        plot.new()
        return(invisible(NULL))
    }
    
    data_table <- round(x$evolution, 1)
    if(data_table$Evolution[3] > 0){
        title = expression("Moving Holiday Effects: " %dblup% " expected")
    }else if (data_table$Evolution[3] < 0){
        title = expression("Moving Holiday Effects: " %dbldown% " expected")
    }else{
        title = expression("Moving Holiday Effects: " %=>% " expected")
    }
    data_table$Evolution <- sprintf("%+.1f%%",data_table$Evolution)
    
    mai <- par("mai")  
    barplot(x$estimated_values, names.arg=names(x$estimated_values),ann=FALSE,
            main = "Moving Holiday Pattern")
    par(mai = c(0,0,0,0))
    plot(0,type='n',axes=FALSE,ann = F, xlim = c(0,1), ylim = c(0,1))
    plotrix::addtable2plot(0.5, 0.5,
                           data_table[-3,, drop=FALSE], bty = "o", display.rownames = TRUE, hlines = TRUE,
                           display.colnames = FALSE,
                           vlines = TRUE,title = title, xjust = 0.5, yjust = 0.5)
    par(mai = mai)
    
}
