summary_diagnostics <- function(x, digits = 2, decimal.mark = getOption("OutDec"),
                                colours = c(`1` = "red", `2` = "yellow", `3` = "#A0CD63")){
    
    m7 <- RJDemetra::get_indicators(x, "mstats.M(7)")[[1]]
    if (!is.null(m7)) {
        m7_c <- cut(m7,
                    breaks = c(0, 1.25, 1.75, Inf),
                    labels = FALSE, right = FALSE)
        m7_c <- (3:1)[m7_c]
        names(m7) <- "Adjustability (M7)"
    } else {
        m7 <- m7_c <- NULL
    }
    
    # residuals tests
    liste_ind <- c(
        "Residual Seasonality (qs-test on sa)" = "diagnostics.seas-sa-qs",
        "Residual Seasonality (f-test on sa)" = "diagnostics.seas-sa-f",
        "Residual Trading-days effects (f-test on sa)" = "diagnostics.td-sa-all",
        "ARIMA autocorrelation (lb test)" = "preprocessing.residuals.lb"
    )
    
    residuals_tests <- unlist(lapply(RJDemetra::get_indicators(x, liste_ind), `[[`, 2))
    names(residuals_tests) <- names(liste_ind)
    
    residuals_tests_c <- cut(residuals_tests,
                             breaks = c(0, 0.01, 0.05, Inf),
                             labels = FALSE, right = FALSE)
    recent_outlier <- rbind(description_outlier(x, 0),description_outlier(x, 1))
    rownames(recent_outlier) <- c("Recent Outliers (current period)",
                              "Recent Outliers (previous period)")
    
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
    
    summary_diagnostics <- data.frame(
        Value = value, Indicator = "",
        Colour = colour,
        row.names = names(value),
        stringsAsFactors = FALSE)
    class(summary_diagnostics) <- c("summary_diagnostics", class(summary_diagnostics))
    
    summary_diagnostics
}
description_outlier <- function(x, nb_previous_periods = 0){
    regression_var_names <- RJDemetra::get_indicators(x, "preprocessing.model.description")[[1]]
    y <- RJDemetra::get_indicators(x, c("y"))[[1]]
    freq <- frequency(y)
    date <- tail(time(y), nb_previous_periods + 1)[1]
    is_outlier <- length(grep(date_r2jd(date, freq), regression_var_names)) > 1
    c17 <- RJDemetra::get_indicators(x, "decomposition.c17")[[1]]
    if (!is.null(c17)) {
        last_c17 <- window(c17, start = date)[1]
    } else {
        last_c17 <- NULL
    }
    
    if (is_outlier) {
        value <- c("Outlier", 1)
    }else if (isTRUE(all.equal(last_c17, 0))) {
        value <- c("Extreme", 2)
    }else{
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
#'@exportS3Method NULL
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

