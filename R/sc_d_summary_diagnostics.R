summary_diagnostics <- function(x, digits = 2, decimal.mark = getOption("OutDec"),
                                colours = c(`1` = "red", `2` = "yellow", `3` = "#A0CD63")){
    UseMethod("summary_diagnostics", x)
}
summary_diagnostics.X13 <- function(x, digits = 2, decimal.mark = getOption("OutDec"),
                                    colours = c(`1` = "red", `2` = "yellow", `3` = "#A0CD63")){
    if (!all(c("decomposition.c17") %in% names(x$user_defined))) {
        my_spec <- x13_spec(x)
        x <- x13(x$final$series[,"y"], my_spec,userdefined = "decomposition.c17")
    }
    
    m7 <- x$decomposition$mstats["M(7)", ]
    m7_c <- cut(m7,
                breaks = c(0, 1.25, 1.75, Inf),
                labels = FALSE, right = FALSE)
    m7_c <- (3:1)[m7_c]
    m7 <- round(m7,2)
    
    qs_sa_on_sa <- x$diagnostics$residuals_test["qs test on sa", 2]
    f_test_sa_on_sa <- x$diagnostics$residuals_test["f-test on sa (seasonal dummies)", 2]
    f_test_td_on_sa <- x$diagnostics$residuals_test["f-test on sa (td)", 2]
    autocorrelation <-  x$regarima$residuals.stat$tests["ljung box",2]
    residuals_tests <- c(qs_sa_on_sa, f_test_sa_on_sa, f_test_td_on_sa, autocorrelation)
    residuals_tests_c <- cut(residuals_tests,
                             breaks = c(0, 0.01, 0.05, Inf),
                             labels = FALSE, right = FALSE)
    recent_outlier <- rbind(description_outlier(x, 0),description_outlier(x, 1))

    row.names <- c("Adjustability (M7)", "Residual Seasonality (qs-test on sa)",
                   "Residual Seasonality (f-test on sa)",
                   "Residual Trading-days effects (f-test on sa)",
                   "ARIMA autocorrelation (lb test)",
                   "Recent Outliers (current period)",
                   "Recent Outliers (previous period)"
                   # "Evolutive seasonality test"
    )
    value <- c(m7,
               residuals_tests,
               recent_outlier[,1]
               )
    value[1:length(c(m7, residuals_tests))] <-
        formatC(c(m7, residuals_tests),
                format = "f", digits = digits,
                decimal.mark = decimal.mark)
    colour <- colours[c(m7_c,
                        residuals_tests_c,
                        recent_outlier[,2])]
    
    summary_diagnostics <- data.frame(Value = value, Indicator = "",
                                      Colour = colour,
                                      row.names = row.names,
                                      stringsAsFactors = FALSE)
    class(summary_diagnostics) <- c("summary_diagnostics", class(summary_diagnostics))
    
    summary_diagnostics
}

summary_diagnostics.TRAMO_SEATS <- function(x, digits = 2, decimal.mark = getOption("OutDec"),
                                    colours = c(`1` = "red", `2` = "yellow", `3` = "green")){
    
    qs_sa_on_sa <- x$diagnostics$residuals_test["qs test on sa", 2]
    f_test_sa_on_sa <- x$diagnostics$residuals_test["f-test on sa (seasonal dummies)", 2]
    f_test_td_on_sa <- x$diagnostics$residuals_test["f-test on sa (td)", 2]
    autocorrelation <-  x$regarima$residuals.stat$tests["ljung box",2]
    residuals_tests <- c(qs_sa_on_sa, f_test_sa_on_sa, f_test_td_on_sa, autocorrelation)
    residuals_tests_c <- cut(residuals_tests,
                             breaks = c(0, 0.01, 0.05, Inf),
                             labels = FALSE, right = FALSE)
    recent_outlier <- rbind(description_outlier(x, 0),description_outlier(x, 1))

    row.names <- c("Residual Seasonality (qs-test on sa)",
                   "Residual Seasonality (f-test on sa)",
                   "Residual Trading-days effects (f-test on sa)",
                   "ARIMA autocorrelation (lb test)",
                   "Recent Outliers (current period)",
                   "Recent Outliers (previous period)"
    )
    value <- c(residuals_tests,
               recent_outlier[,1]
    )
    value[1:length(residuals_tests)] <-
        formatC(residuals_tests,
                format = "f", digits = digits,
                decimal.mark = decimal.mark)
    colour <- colours[c(residuals_tests_c,
                        recent_outlier[,2])]
    
    summary_diagnostics <- data.frame(Value = value, Indicator = "",
                                      Colour = colour,
                                      row.names = row.names,
                                      stringsAsFactors = FALSE)
    class(summary_diagnostics) <- c("summary_diagnostics", class(summary_diagnostics))
    
    summary_diagnostics
}
description_outlier <- function(x, nb_previous_periods = 0){
    UseMethod("description_outlier", x)
}
description_outlier.X13 <- function(x, nb_previous_periods = 0){

    regression_var_names <- rownames(x$regarima$regression.coefficients)
    freq <- frequency(x$final$series)
    date <- tail(time(x$final$series), nb_previous_periods + 1)[1]
    is_outlier <- length(grep(date_r2jd(date, freq), regression_var_names)) > 1
    
    c17 <- x$user_defined$decomposition.c17
    last_c17 <- window(c17, start = date)[1]
    
    if (is_outlier) {
        value <- c("Outlier",1)
    }else if (isTRUE(all.equal(last_c17, 0))) {
        value <- c("Extreme", 2)
    }else{
        value <- c("Regular", 3)
    }
    value
}
description_outlier.TRAMO_SEATS <- function(x, nb_previous_periods = 0){
    
    regression_var_names <- rownames(x$regarima$regression.coefficients)
    freq <- frequency(x$final$series)
    date <- tail(time(x$final$series), nb_previous_periods + 1)[1]
    is_outlier <- length(grep(date_r2jd(date, freq), regression_var_names)) > 1
    
    if (is_outlier) {
        value <- c("Outlier",1)
    } else {
        value <- c("Regular", 3)
    }
    value
}
recurring_outlier <- function(x){
    regression_var_names <- rownames(x$regarima$regression.coefficients)
    outliers <- grep("(AO)|(TC)|(LS)|(SO)", regression_var_names, value = TRUE)
    0
}
date_r2jd <- function(date, frequency){
    date_v <- start(ts(0, start = date, frequency = frequency))
    if (frequency == 12) {
        period <- date_v[2]
    }else{
        period <- as.roman(date_v[2])
    }
    sprintf("%s-%i", period, date_v[1])
}

plot.summary_diagnostics <- function(x, ...){
    bg_color <- cbind("white", x$Colour)
    
    row_names <- rownames(x)
    max_str_width <- max(strwidth(row_names))
    row_names <- sapply(row_names, function(n){
        while (strwidth(n) < max_str_width) {
            n <- paste0(" ", n)  
        }
        n
    })

    table <- x[, 1:2]
    rownames(table) <- row_names
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
         axes = FALSE)
    plotrix::addtable2plot(0.5, 0.5,
                  table, bty = "o", display.rownames = TRUE, hlines = TRUE,
                  vlines = TRUE,bg = bg_color, xjust = 0.5, yjust = 0.5)
    title("Summary of Key Diagnostics", line = -3)
}

#' @importFrom RJDemetra x13 x13_spec
#' @importFrom graphics arrows axis barplot box layout legend lines mtext par plot plot.new strwidth text title points
#' @importFrom stats cor cycle deltat frequency lag na.omit pchisq pt sd start time ts ts.union window
#' @importFrom utils as.roman head read.csv tail globalVariables head tail
utils::globalVariables(c("TD1", "TD2", "TD3", "TD4", "TD5", "TD6", "TD7",
                          "phi1", "phi2", "phi3", "phi4", "phi5", "phi6",
                         "regarima_coefs","theta1", "theta2", "theta3", "theta4", "theta5", "theta6"))

