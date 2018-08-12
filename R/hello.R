library(RJDemetra)
?x13
mysa <- x13_def(myseries, spec=c("RSA5c"),userdefined = )
x <- mysa
x <- extract_recent_history(mysa)
extract_recent_history <- function(x, nb_last_obs = 24){
    result <- x$final$series[,c("y","t","sa")]
    start <- tail(time(result), nb_last_obs)[1]
    ts(tail(result, nb_last_obs), start = start, frequency = frequency(result))
}
plot_recent_history <- function(x){
    plot_date <- as.numeric(time(x))
    plot_date_lab <- as.Date(time(x))
    freq <- frequency(x)
    if(freq == 12){
        plot_date_lab <- format(plot_date_lab, format = "%b. %Y")
    }else if(freq == 4){
        plot_date_lab <- sprintf("%s-%s",
                                 quarters(plot_date_lab),
                                 format(plot_date_lab, format = "%Y"))
    }else{
        plot_date_lab <- as.character(plot_date_lab)
    }

    plot(x[,"y"],
         type = "p", pch = 4, col = "green",
         xlab = NULL, ylab = NULL, main = "Recent History",
         xaxt = "n", yaxt = "n")
    lines(x[,"t"],
         lty = c("dashed"), col = "black")
    lines(x[,"sa"],
          lty = c("solid"), col = "red")
    legend("topleft", legend = c("Series", "Trend","Seasonally adjusted"),
           col = c("green", "black", "red"),
           lty = c("dashed","dashed","solid"),
           pch = c(4, NA, NA), bty="n")
    axis(1, at = plot_date,
         labels = plot_date_lab, las = 2, cex.axis = 0.8)
    axis(2,cex.axis=0.8)
}
