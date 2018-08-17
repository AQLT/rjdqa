recent_history <- function(x, nb_last_obs = 24){
    result <- x$final$series[,c("y","t","sa")]
    start <- tail(time(result), nb_last_obs)[1]
    result <- ts(tail(result, nb_last_obs), start = start, frequency = frequency(result))
    class(result) <- c("recent_history", class(result))
    result
}

plot.recent_history <- function(x, ...){
    x <- recent_history(mysa)
    plot_date <- as.numeric(time(x))
    plot_date_lab <- time_to_date(x)
    freq <- frequency(x)
    if(freq == 12){
        plot_date_lab <- format(plot_date_lab, format = "%b %Y")
    }else if(freq == 4){
        plot_date_lab <- sprintf("%s-%s",
                                 quarters(plot_date_lab),
                                 format(plot_date_lab, format = "%Y"))
    }else{
        plot_date_lab <- as.character(plot_date_lab)
    }

    # par(mar = c(6,4,1,1))
    plot(x[,"y"],
         type = "p", pch = 4, col = "green",
         xlab = NULL, ylab = NULL,
         xaxt = "n", yaxt = "n")
    lines(x[,"t"],
          lty = c("dashed"), col = "black")
    lines(x[,"sa"],
          lty = c("solid"), col = "red")
    # axis(1, at = plot_date,
    #      labels = plot_date_lab, las = 2, cex.axis = 1)
    axis(1, at = plot_date,
         labels = FALSE)
    text(x = plot_date, y = par()$usr[3] - 0.03*(par()$usr[4] - par()$usr[3]),
         labels = plot_date_lab, srt = 30, adj = 1, xpd = TRUE)
    axis(2,cex.axis=1)
    legend("topleft", legend = c("Series", "Trend","Seasonally adjusted"),
           col = c("green", "black", "red"),
           lty = c("dashed","dashed","solid"),
           pch = c(4, NA, NA), bty="n",
           horiz=FALSE
    )
    
    title("Recent History")
}
