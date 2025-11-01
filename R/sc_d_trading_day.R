trading_day_pattern <- function(x){
    data <- RJDemetra::get_indicators(x, c("preprocessing.model.ntd", "preprocessing.model.tde", "y", "preprocessing.model.log", "preprocessing.model.tde_f"))
    is_multiplicative <- as.logical(data[["preprocessing.model.log"]])
    ntd <- data[["preprocessing.model.ntd"]]
    freq <- frequency(data[["preprocessing.model.tde"]])
    
    if(freq == 12){
        period <- "Month"
    }else{
        period <- "Quarter"
    }
    evolution_row_names <- sprintf("%s %s",
                                   c("Previous", "Current", "Next"),
                                   period)
    if(ntd == 0){
        result <- list(estimated_values = c(Monday = 0, Tuesday = 0, Wednesday = 0, Thursday = 0, Friday = 0, 
                                            Saturday = 0, Contrast = 0),
                       evolution = data.frame(Evolution = c(0,0,0),
                                              row.names = evolution_row_names))
    }else{
        data_td <- do.call(
            rbind, 
            RJDemetra::get_indicators(x, sprintf("preprocessing.model.td(%s)", seq_len(ntd)))
        )
        regression_var_names <- rownames(data_td)
        estimated_values <- data_td[,1]*100
        evolution <- c(tail(data[["preprocessing.model.tde"]], 2),
                       head(data[["preprocessing.model.tde_f"]],1))
        if (!is_multiplicative){
            series_mean <- mean(data[["y"]],
                                na.rm = TRUE)
            estimated_values <- estimated_values / series_mean
            evolution <- evolution/series_mean + 1 
        }else{
            evolution[1:2] <- exp(evolution[1:2])
        }
        
        estimated_values <- c(estimated_values, Contrast = -sum(estimated_values))
        evolution <- (evolution - 1)*100
        evolution <- data.frame(Evolution = evolution,
                                row.names = evolution_row_names)
        result <- list(estimated_values = estimated_values, evolution = evolution)
    }
    
    class(result) <- c("trading_day_pattern", class(result))
    result
}
#'@exportS3Method NULL
plot.trading_day_pattern <- function(x, decimal.mark = getOption("OutDec"),
                                     ...){
    if(is.null(x$evolution)){
        plot(0,type='n',axes=FALSE,main="Moving Holiday Pattern")
        plot.new()
        return(invisible(NULL))
    }
    data_table <- round(x$evolution, 1)
    if(data_table$Evolution[3] > 0){
        title = expression("Trading Day Effects: " %dblup% " expected")
    }else if (data_table$Evolution[3] < 0){
        title = expression("Trading Day Effects: " %dbldown% " expected")
    }else{
        title = expression("Trading Day Effects: " %=>% " expected")
    }
    data_table$Evolution <- sprintf("%+.1f%%",data_table$Evolution)
    
    mai <- par("mai")  
    barplot(x$estimated_values, names.arg=names(x$estimated_values),ann=FALSE,
            main = "Trading Day Pattern")
    par(mai = c(0,0,0,0))
    plot(0,type='n',axes=FALSE,ann = F, xlim = c(0,1), ylim = c(0,1))
    plotrix::addtable2plot(0.5, 0.5,
                  data_table[-3,, drop=FALSE], bty = "o", display.rownames = TRUE, hlines = TRUE,
                  display.colnames = FALSE,
                  vlines = TRUE,title = title, xjust = 0.5, yjust = 0.5)
    par(mai = mai)
}
