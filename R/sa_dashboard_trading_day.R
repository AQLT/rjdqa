trading_day_pattern <- function(x){
    UseMethod("trading_day_pattern", x)
}
trading_day_pattern.X13 <- function(x){
    if(!all(c("preprocessing.model.tde_f") %in% names(x$user_defined))){
        my_spec <- RJDemetra::x13_spec(x)
        x <- RJDemetra::x13(x$final$series[,"y"], my_spec,userdefined = "preprocessing.model.tde_f")
    }
    
    reg_coefficients <- x$regarima$regression.coefficients
    regression_var_names <- rownames(reg_coefficients)
    freq <- frequency(x$final$series[,"y"])
    td_reg <- grep("(Monday)|(Tuesday)|(Wednesday)|(Thursday)|(Friday)|(Saturday)|(Week days)",
                   regression_var_names)
    if(freq == 12){
        period <- "Month"
    }else{
        period <- "Quarter"
    }
    evolution_row_names <- sprintf("%s %s",
                                   c("Previous", "Current", "Next"),
                                   period)
    if(length(td_reg) == 0){
        result <- list(estimated_values = c(Monday = 0, Tuesday = 0, Wednesday = 0, Thursday = 0, Friday = 0, 
                                            Saturday = 0, Contrast = 0),
                       evolution = data.frame(Evolution = c(0,0,0),
                                              row.names = evolution_row_names))
    }else{
        estimated_values <- reg_coefficients[td_reg,1]*100
        is_multiplicative <- x$regarima$model$spec_rslt[, "Log transformation"]
        evolution <- c(tail(x$regarima$model$effects[,"tde"],2),
                       head(x$user_defined$preprocessing.model.tde_f,1))
        if (!is_multiplicative){
            series_mean <- mean(x$final$series[,"y"],
                                na.rm = TRUE)
            estimated_values <- estimated_values / series_mean
            evolution <- evolution/series_mean + 1 
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

trading_day_pattern.TRAMO_SEATS <- function(x){
    if(!all(c("preprocessing.model.tde_f") %in% names(x$user_defined))){
        my_spec <- RJDemetra::tramoseats_spec(x)
        x <- RJDemetra::tramoseats(x$final$series[,"y"], my_spec, userdefined = "preprocessing.model.tde_f")
    }
    
    reg_coefficients <- x$regarima$regression.coefficients
    regression_var_names <- rownames(reg_coefficients)
    freq <- frequency(x$final$series[,"y"])
    td_reg <- grep("(Monday)|(Tuesday)|(Wednesday)|(Thursday)|(Friday)|(Saturday)|(Week days)",
                   regression_var_names)
    if(freq == 12){
        period <- "Month"
    }else{
        period <- "Quarter"
    }
    evolution_row_names <- sprintf("%s %s",
                                   c("Previous", "Current", "Next"),
                                   period)
    if(length(td_reg) == 0){
        result <- list(estimated_values = c(Monday = 0, Tuesday = 0, Wednesday = 0, Thursday = 0, Friday = 0, 
                                            Saturday = 0, Contrast = 0),
                       evolution = data.frame(Evolution = c(0,0,0),
                                              row.names = evolution_row_names))
    }else{
        estimated_values <- reg_coefficients[td_reg,1]*100
        is_multiplicative <- x$regarima$model$spec_rslt[, "Log transformation"]
        evolution <- c(tail(x$regarima$model$effects[,"tde"],2),
                       head(x$user_defined$preprocessing.model.tde_f,1))
        if (!is_multiplicative){
            series_mean <- mean(x$final$series[,"y"],
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
