#' @importFrom rJava .jinstanceof
net_effect <- function(x){
    data <- RJDemetra::get_indicators(x, c(
        "y", "s", "sa", "preprocessing.model.tde",
        "preprocessing.model.mhe", "preprocessing.model.log")
    )
    
    is_tramo <- rJava::.jinstanceof(x[["spec"]], "jdr/spec/tramoseats/TramoSeatsSpec")
    
    sa <- data[["sa"]]
    y <- data[["y"]]
    s <- data[["s"]]

    tde <- data[["preprocessing.model.tde"]]
    mhe <- data[["preprocessing.model.mhe"]]
    is_multiplicative <- as.logical(data[["preprocessing.model.log"]])
    if (is_multiplicative & is_tramo) {
        tde <- exp(tde)
        mhe <- exp(mhe)
    }
    
    observed_sa <- tail(sa, 2)
    observed_y <- tail(y, 2)
    
    observed_tde <- tail(tde, 2)
    observed_mhe <- tail(mhe, 2)
    
    observed_sa_evol <- (observed_sa[2] / observed_sa[1] - 1) * 100
    observed_y_evol <- (observed_y[2] / observed_y[1] - 1) * 100
    
    neutral_line_sa <- rep(observed_sa[1], 2)
    
    if (is_multiplicative) {
        s <- s / (exp(tde + mhe))
        observed_td <- observed_y / exp(observed_tde)
        observed_cal <- observed_y / exp(observed_tde + observed_mhe)
        
        neutral_line_y <- observed_y * c(1, observed_sa[1] / observed_sa[2])
        neutral_line_td <- observed_td * c(1, observed_sa[1] / observed_sa[2])
        neutral_line_cal <- observed_cal * c(1, observed_sa[1] / observed_sa[2])
    }else{
        s <- s - tde - mhe
        observed_td <- observed_y - observed_tde
        observed_cal <- observed_y - observed_tde - observed_mhe
        
        neutral_line_y <- observed_y + c(0, observed_sa[1] - observed_sa[2])
        neutral_line_td <- observed_td + c(0, observed_sa[1] - observed_sa[2])
        neutral_line_cal <- observed_cal + c(0, observed_sa[1] - observed_sa[2])
    }
    observed_s <- tail(s, 2)
    observed_s_evol <- (observed_s[2] / observed_s[1] - 1) * 100
    
    EM <- sd(tail(ev(sa) / 100, 5 * frequency(sa)), na.rm = TRUE)
    
    upper_bound_sa <- neutral_line_sa * c(1, 1 + EM)
    lower_bound_sa <- neutral_line_sa * c(1, 1 - EM)
    upper_bound_y <- neutral_line_y * c(1, 1 + EM)
    lower_bound_y <- neutral_line_y * c(1, 1 - EM)
    
    y_movement <- c("decrease", "increase")[(observed_y_evol > 0) + 1]
    
    neutral_line_y_evol <- (neutral_line_y[2] / neutral_line_y[1] - 1) * 100
    neutral_line_td_evol <- (neutral_line_td[2] / neutral_line_td[1] - 1) * 100
    neutral_line_cal_evol <- (neutral_line_cal[2] / neutral_line_cal[1] - 1) * 100
    
    
    tde_contrib <- neutral_line_y_evol - neutral_line_td_evol
    mhe_contrib <- neutral_line_td_evol - neutral_line_cal_evol
    observed_s_evol <- neutral_line_cal_evol
    
    
    neutral_y_movement <- c("decrease", "increase")[(neutral_line_y_evol > 0) + 1]
    s_movement <- c("decrease", "increase")[(observed_s_evol > 0) + 1]
    
    neutral_cal_movement <- c("decrease", "increase")[(neutral_line_cal_evol > 0) + 1]
    
    line1 <- sprintf("Observed %.1f%% raw %s from last month",
                     abs(observed_y_evol), y_movement)
    line2 <- sprintf("Neutral result requires %.1f%% raw %s",
                     abs(neutral_line_y_evol), neutral_y_movement)
    line2 <- c(line2, "from last month:")
    if(round(tde_contrib, 1) == 0){
        line3 <- "No trading day effect"
    }else{
        tde_contrib_movement <- c("decrease", "increase")[(tde_contrib > 0) + 1]
        line3 <- sprintf("Trading day effects represent %.1f%% raw %s",
                         abs(tde_contrib), tde_contrib_movement)
    }
    if (round(mhe_contrib, 1) == 0) {
        line4 <- "No moving holiday effect"
    }else{
        mhe_contrib_movement <- c("decrease", "increase")[(mhe_contrib > 0) + 1]
        line4 <- sprintf("Moving holiday effects represent %.1f%% raw %s",
                         abs(mhe_contrib), mhe_contrib_movement)
    }
    line5 <- sprintf("Seasonal effects represent %.1f%% raw %s",
                     abs(observed_s_evol), s_movement)
    line6 <- sprintf("SA movement of %+.1f%% from last month",
                     observed_sa_evol)
    res <- list(raw_data = list(observed = observed_y, neutral_line = neutral_line_y,
                                lower_bound = lower_bound_y, upper_bound = upper_bound_y,
                                frequency = frequency(y)),
                sa_data = list(observed = observed_sa, neutral_line = neutral_line_sa,
                               lower_bound = lower_bound_sa, upper_bound = upper_bound_sa,
                               frequency = frequency(sa)),
                text = c(line1, line2,
                         line3, line4,
                         line5, line6)
    )
    class(res) <- c("net_effect", class(res))
    
    res
}
#'@exportS3Method NULL
plot.net_effect <- function(x,
                            raw_color = "#33A02C",
                            sa_color = "#E31A1C", ...){
    mai <- par('mai')
    
    par(mai = c(mai[1], mai[2], mai[3], 0.1))
    plot_net_graph(x$raw_data, title = "Unadjusted", subtitle = "Raw",
                   arrow_col = raw_color)
    par(mai = c(mai[1], 0, 0.1, 0.1))
    plot_net_effect_text(x$text)
    par(mai = c(mai[1], 0.2, mai[3], mai[4]))
    
    plot_net_graph(x$sa_data, title = "Seasonally Adjusted", subtitle = "SA",
                   arrow_col = sa_color)
    par(mai = mai)
}
plot_net_effect_text <- function(x){
    base_inset_x <- -0.00
    plot.new()
    box()
    legend("topleft", legend = x[1], 
           bty = "n", text.font =  2, inset = c(base_inset_x, 0))
    legend("topleft", legend = c(rep(NA, 3), x[2:3]), 
           bty = "n", text.font =  2, inset = c(base_inset_x, 0))
    legend("topleft", legend = c(rep(NA, 5), x[4:6]), 
           bty = "n", text.font =  1,inset = c(base_inset_x + 0.05,0))
    legend("topleft", legend = c(rep(NA, 10), x[7]), 
           bty = "n", text.font =  2, inset = c(base_inset_x, 0))
}
plot_net_graph <- function(x, title = "", subtitle = "", arrow_col = "black"){
    
    main <- sprintf("%s (%s)", title, subtitle)
    legend_text <- c(sprintf("%s, %+.1f%%",subtitle,
                             (x$observed[2]/ x$observed[1] - 1)*100), 
                     sprintf("%s %s",subtitle, c("Neutral", "Boundaries")))
    if(x$frequency == 12){
        x_lab <- c("m-1","m")
    }else{
        x_lab <- c("Q-1","Q")
    }
    plot(x = 1:2, y = x$neutral_line, type = "l",
         ylim = range(c(x$upper_bound, x$lower_bound)),
         lty = c("dotted"),xlim = c(0.8,2.1),
         xlab = "", ylab = "", main = main,
         xaxt = "n")
    lines(x = 1:2, y = x$upper_bound, lty = c("dashed"),lwd = 2)
    lines(x = 1:2, y = x$lower_bound, lty = c("dashed"),lwd = 2)
    arrows(1, x$observed[1], 2, x$observed[2], col = arrow_col,lwd = 2)
    axis(1, at = 1:2,
         labels = x_lab,
         las = 0)
    legend("topleft", legend = legend_text, pch = c(NA, NA,NA),
           lty = c("solid","dotted","dashed"), col = c(arrow_col, "black","black"),
           lwd = 3,cex = 0.8, bty="n") 
}
