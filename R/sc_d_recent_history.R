recent_history <- function(x, n_recent_obs = 24){
    result <- x$final$series[,c("y","t","sa")]
    if (!is.null(recent_history)) {
        start <- tail(time(result), n_recent_obs)[1]
        result <- ts(tail(result, n_recent_obs), start = start, frequency = frequency(result))
    }
    class(result) <- c("recent_history", class(result))
    result
}
#'@exportS3Method NULL
plot.recent_history <- function(x,
                                raw_color = "#33A02C",
                                sa_color = "#E31A1C",
                                trend_color = "black", ...){
    legend <- c("Series", "Trend","Seasonally adjusted")
    # legend <- c("y", "t", "sa")
    plot_date <- as.numeric(time(x))
    plot_date_lab <- time_to_date(x)
    freq <- frequency(x)
    if(freq == 12){
        plot_date_lab <- format(plot_date_lab, format = "%b %y")
    }else if(freq == 4){
        plot_date_lab <- sprintf("%s-%s",
                                 quarters(plot_date_lab),
                                 format(plot_date_lab, format = "%y"))
    }else{
        plot_date_lab <- as.character(plot_date_lab)
    }

    # par(mar = c(6,4,1,1))
    plot(x[,"y"],
         type = "p", pch = 4, col = raw_color,
         xlab = NULL, ylab = NULL,
         xaxt = "n", yaxt = "n")
    lines(x[,"t"],
          lty = c("dashed"), col = trend_color)
    lines(x[,"sa"],
          lty = c("solid"), col = sa_color)
    # axis(1, at = plot_date,
    #      labels = plot_date_lab, las = 2, cex.axis = 1)
    axis(1, at = plot_date,
         labels = FALSE)
    text(x = plot_date, y = par()$usr[3] - 0.03*(par()$usr[4] - par()$usr[3]),
         labels = plot_date_lab, srt = 40, adj = 1, xpd = TRUE)
    axis(2,cex.axis=1)
    legend("topleft", legend = legend,
           col = c(raw_color, trend_color, sa_color),
           lty = c("dashed","dashed","solid"),
           pch = c(4, NA, NA), bty="n",
           horiz=FALSE
    )
    
    title("Recent History")
}
