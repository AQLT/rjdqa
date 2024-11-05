
#' Compute data for a simple seasonal adjustment
#' 
#' Functions to compute the data to produce a simple seasonal adjustment dashboard.
#' `simple_dashboard2()` is a slightly variation of `simple_dashboard()` with smaller description
#' text to include a table with last outliers.
#' 
#' @inheritParams sc_dashboard
#' @param digits number of digits used in the tables.
#' @param scale_var_decomp boolean indicating if the variance decomposition table should be rescaled to 100.
#' @param remove_others_contrib boolean indication if the "Others" contribution (i.e.: the pre-adjustment contribution)
#' should be removed from the variance decomposition table.
#' @param add_obs_to_forecast Boolean indicating if the last observed values should be added to the forecast table (for the plot).
#' @param td_effect Boolean indicating if the residual trading days effect test should be printed.
#' By default (`td_effect = NULL`) the test is only printed for monthly series.
#' 
#' @examples
#' data <- window(RJDemetra::ipi_c_eu[, "FR"], start = 2003)
#' sa_model <- RJDemetra::jx13(data, "RSA5c")
#' dashboard_data <- simple_dashboard(sa_model)
#' plot(dashboard_data, main = "Simple dashboard IPI - FR")
#' dashboard_data2 <- simple_dashboard2(sa_model)
#' plot(dashboard_data2, main = "Simple dashboard with outliers IPI - FR")
#' @seealso \code{\link{plot.simple_dashboard}}.
#' @export
simple_dashboard <- function(x, digits = 2,
                             scale_var_decomp = FALSE,
                             remove_others_contrib = FALSE,
                             add_obs_to_forecast = TRUE,
                             td_effect = NULL) {
    if (inherits(x, "TRAMO_SEATS")) {
        x <- RJDemetra::jtramoseats(RJDemetra::get_ts(x), RJDemetra::tramoseats_spec(x))
    } else if (inherits(x, "X13")) {
        x <- RJDemetra::jx13(RJDemetra::get_ts(x), RJDemetra::x13_spec(x))
    }
    nb_format <- paste0("%.", digits, "f")
    
    # Raw, trend, sa
    data_plot <- RJDemetra::get_indicators(x, c("y", "t", "sa", "y_f", "t_f", "sa_f"))
    
    last_date <- tail(time_to_date(data_plot[[1]]), 1)
    last_date <- format(last_date, format = "%Y-%m")
    
    data_plot <- do.call(ts.union, data_plot)
    # add observed data for plots
    if (add_obs_to_forecast)
        data_plot[which(is.na(data_plot[,"y"]))[1]-1, c("y_f", "t_f", "sa_f")] <-
        data_plot[which(is.na(data_plot[,"y"]))[1]-1, c("y", "t", "sa")]

    # Global info on model
    arima_ord <- sprintf("ARIMA(%s)(%s)",
                         paste(unlist(RJDemetra::get_indicators(x, sprintf("preprocessing.arima.%s", c("p", "d", "q")))), collapse = ","),
                         paste(unlist(RJDemetra::get_indicators(x, sprintf("preprocessing.arima.%s", c("bp", "bd", "bq")))), collapse = ","))
    ntd <- RJDemetra::get_indicators(x, "preprocessing.model.ntd")[[1]] # nombre de JO
    nmh <- RJDemetra::get_indicators(x, "preprocessing.model.nmh")[[1]]
    is_easter <- (! is.null(nmh)) &&
        (nmh > 0)
    
    est_span <- sprintf("Estimation span: %s to %s\n%s observations",
                        RJDemetra::get_indicators(x, "preprocessing.model.espan.start")[[1]],
                        RJDemetra::get_indicators(x, "preprocessing.model.espan.end")[[1]],
                        RJDemetra::get_indicators(x, "preprocessing.model.espan.n")[[1]]
    )
    transform <- ifelse(RJDemetra::get_indicators(x, "preprocessing.model.log")[[1]] == "false",
                        "Series hasn't been transformed",
                        "Series has been log-transformed")
    tde <- sprintf("%s\n%s",
                   ifelse(ntd==0, "No trading days effect",
                          sprintf("Trading days effect (%s)", ntd)),
                   ifelse(is_easter, "Easter effect",
                          "No easter effect"))
    # nb outliers
    out <- sprintf("%s detected outliers", RJDemetra::get_indicators(x, "preprocessing.model.nout")[[1]])
    summary_text <- c(est_span, transform, tde, out, arima_ord)
    
    
    # Stats on quality of decomposition
    qstats <- list2DF(RJDemetra::get_indicators(x, "mstats.Q", "mstats.Q-M2"))
    colnames(qstats) <- c("Q", "Q-M2")
    # Stats on variance decomp
    var_decomp <- RJDemetra::get_indicators(x, "diagnostics.variancedecomposition")[[1]]
    names(var_decomp) <- c("Cycle", "Seasonal", "Irregular", "TDH", "Others", "Total")
    if (remove_others_contrib) {
        var_decomp <- var_decomp[-5]
        i_total <- length(var_decomp)
        var_decomp[i_total] <- sum(var_decomp[-i_total])
    }
    if (scale_var_decomp) {
        i_total <- length(var_decomp)
        var_decomp[-i_total] <- var_decomp[-i_total] / sum(var_decomp[-i_total])
        var_decomp[i_total] <- 1
    }
    
    var_decomp <- as.data.frame(t(data.frame(var_decomp*100)))
    var_decomp <- var_decomp
    # Tests on linearised series
    liste_ind_seas <- c("F-test" = "diagnostics.seas-lin-f",
                        "QS-test" = "diagnostics.seas-lin-qs",
                        "Kruskal-Wallis" = "diagnostics.seas-lin-kw",
                        "Friedman" = "diagnostics.seas-lin-friedman",
                        "Combined" = "diagnostics.combined.all.summary")
    # residuals tests
    liste_ind_res_seas <- c("F-test" = "diagnostics.seas-sa-f",
                            "QS-test" = "diagnostics.seas-sa-qs",
                            "Kruskal-Wallis" = "diagnostics.seas-sa-kw",
                            "Friedman" = "diagnostics.seas-sa-friedman",
                            "Combined" = "diagnostics.seas-sa-combined")
    liste_ind_res_jo <-
        c("Residual TD" = "diagnostics.td-sa-last")
    seas_test <- list2DF(lapply(RJDemetra::get_indicators(x, liste_ind_seas), function(x) {
        if(length(x) > 1)
            x <- sprintf(nb_format, x[2])
        x
    }))
    seas_res_test <- list2DF(lapply(RJDemetra::get_indicators(x, liste_ind_res_seas), function(x) {
        if(length(x) > 1)
            x <- sprintf(nb_format, x[2])
        x
    }))
    td_res_test <- data.frame(sprintf(nb_format, RJDemetra::get_indicators(x, liste_ind_res_jo)[[1]][2]),
                              "", "", "", "" )
    names(seas_test) <- names(seas_res_test) <-
        names(td_res_test) <- names(liste_ind_seas)
    all_tests <- rbind(seas_test, seas_res_test,
                       td_res_test)
    rownames(all_tests) <- c("Seasonality",
                             "Residual Seasonality",
                             "Residual TD effect")
    # On calcule les couleurs
    color_test <- rbind(c(ifelse(seas_test[,-5] < 0.05,  "#A0CD63", "red"),
                          switch(seas_test[,5], "Present" = "#A0CD63",
                                 "None" = "red", "orange")),
                        c(ifelse(seas_res_test[,-5] < 0.05,  "red", "#A0CD63"),
                          switch(seas_res_test[,5], "Present" = "red",
                                 "None" = "#A0CD63", "orange")),
                        c(ifelse(td_res_test[,1] < 0.05,  "red", "#A0CD63"),
                          rep("white", 4)))
    if (is.null(td_effect))
        td_effect <- frequency(data_plot) == 12
    if (!td_effect) {
        all_tests <- all_tests[-3,]
        color_test<- color_test[-3,]
    }
    decomp_stats_color <- c(sapply(qstats, function(x) ifelse(x < 1, "#A0CD63", "red")),
                         "white",
                         rep("grey90", ncol(var_decomp)
                         ))
    qstats[,] <- lapply(qstats, sprintf, fmt = nb_format)
    var_decomp[,] <- lapply(var_decomp, sprintf, fmt = nb_format)
    
    if (nrow(qstats) == 0) {
        # TRAMO-SEATS
        decomp_stats <- var_decomp
        decomp_stats_color <- unlist(decomp_stats_color[-c(1:3)])
    } else {
        # X-13
        decomp_stats <- cbind(qstats, "   " , var_decomp)
        colnames(decomp_stats)[ncol(qstats)+1] <- "   "
    }
    
    res <- list(main_plot = data_plot,
         siratio_plot = ggdemetra::siratio(x),
         summary_text = summary_text,
         decomp_stats = list(table = decomp_stats,
                                colors = decomp_stats_color),
         residuals_tests = list(table = all_tests,
                                colors = color_test),
         last_date = last_date)
    class(res) <- c("simple_dashboard")
    res
}
#' Plot a simple seasonal adjustment dashboard
#' 
#' Functions to plot a simple dashboard of a seasonal adjustment model.
#' 
#' @inheritParams plot.sc_dashboard 
#' @param color_series Color of the raw time series, the trend and the seasonally adjusted component.
#' 
#' @examples
#' data <- window(RJDemetra::ipi_c_eu[, "FR"], start = 2003)
#' sa_model <- RJDemetra::jx13(data, "RSA5c")
#' dashboard_data <- simple_dashboard(sa_model)
#' plot(dashboard_data, main = "Simple dashboard IPI - FR")
#' dashboard_data2 <- simple_dashboard2(sa_model)
#' plot(dashboard_data2, main = "Simple dashboard with outliers IPI - FR")
#' @seealso \code{\link{simple_dashboard}}.
#' @export
plot.simple_dashboard <- function(x, main = "Simple Dashboard with outliers",
                                  subtitle = NULL,
                                  color_series = c(y = "#F0B400", t = "#1E6C0B", sa = "#155692"),
                                  reference_date = TRUE,...){
    main_plot = x$main_plot
    siratio_plot = x$siratio_plot
    summary_text = x$summary_text
    decomp_stats = x$decomp_stats
    residuals_tests = x$residuals_tests
    last_date = x$last_date
    outliers = x$outliers
    
    # Check if it is simple_dashboard (TRUE) or simple_dashboard2
    is_sd <- is.null(outliers)
    def.par <- par(no.readonly = TRUE)    
    
    if (is_sd) {
        # simple_dashboard
        nf <- layout(matrix(c(rep(1,8),
                              rep(2,4),rep(3,4),
                              rep(4,3), rep(5,5),
                              rep(4,3), rep(6,5)),ncol = 8,byrow = T),
                     heights = c(0.2,2.5,0.5,1.3))
        cex <- 1
        y_res_test <- 0.5 # y position of the residuals tests table smaller
    } else {
        # simple_dashboard2
        nf <- layout(matrix(c(rep(1,8),
                              rep(2,4), rep(3,4),
                              rep(4,3), rep(5,5),
                              rep(4,3), rep(6,5),
                              rep(7,3), rep(8,5)),ncol = 8,byrow = T),
                     heights = c(0.2,2.2,0.5,0.2,0.7))
        cex <- 0.95 # smaller text
        y_res_test <- 0.8
    }
    
    # layout.show(nf)
    on.exit({
        par(def.par)
    })
    
    oma.saved <- par("oma")
    par(oma = rep.int(0, 4))
    # par(oma = oma.saved)
    o.par <- par(mar = rep.int(0, 4))
    plot.new()
    box(which = "inner")
    
    box()
    text(0.5, 0.5, main, font = 2,cex = 1.2)
    par(o.par)
    
    par(mai = c(0, 0.4, 0.2, 0.1))
    stats::plot.ts(main_plot,plot.type = "single",
                   col = rep(color_series, 2),
                   lty = rep(c(1,2), each = 3),
                   xlab = NULL,
                   ylab = NULL,
                   main = NULL
    )
    legend("bottomright", legend = names(color_series),
           col = color_series, lty = 1,
           pch = NA_integer_,
           inset = c(0,1), xpd = TRUE, horiz=TRUE, bty = "n")
    par(mai = c(0.0, 0.2, 0.2, 0.4))
    ggdemetra::siratioplot(siratio_plot, main = NULL)
    
    
    par(mai = c(0.4, 0.2, 0.2, 0))
    par(mar = rep.int(0.4, 4))
    plot.new()
    legend("topleft", legend = c(NA, summary_text), 
           bty = "n", text.font =  2, inset = c(0),
           cex = cex, 
           xpd = TRUE)
    
    
    par(mar = rep(2, 4))
    par(mai = c(0, 0.2, 0, 0.2))
    
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
         axes = FALSE)
    plotrix::addtable2plot(0.5, 0,
                           decomp_stats$table, bty = "o", display.rownames = FALSE, hlines = TRUE,
                           vlines = TRUE,bg = decomp_stats$colors, xjust = 0.5, yjust = 1)
    
    # Empty plot
    if(! is_sd) {
        plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
             axes = FALSE)
        plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
             axes = FALSE)
        par(mai = c(0, 0.2, 0.2, 0.2))
    }
    
    if(! is_sd && ! is.null(outliers$table)) {
        
        plotrix::addtable2plot(0.5, 1,
                               outliers$table, 
                               bg = outliers$colors,
                               bty = "o", 
                               display.rownames = FALSE, hlines = TRUE,
                               vlines = TRUE, 
                               xjust = 0.5, yjust = 0)
    }
    
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
         axes = FALSE)
    par(mai = c(0, 0.2, 0.2, 0.2))
    plotrix::addtable2plot(0.5, y_res_test,
                           residuals_tests$table, bty = "o", 
                           display.rownames = TRUE, hlines = TRUE,
                           vlines = TRUE,
                           bg = residuals_tests$colors, 
                           xjust = 0.5, yjust = 0.5)
    if (reference_date) 
        mtext(sprintf("Reference Date: %s",last_date), side = 3, line = -3, 
              outer = TRUE,font = 3,cex = 0.7,at = 0.95, adj = 1)
    mtext(subtitle, side = 3, line = -3, 
          outer = TRUE,font = 3,cex = 0.7,at = 0.1, adj = 1)
    invisible()
}
