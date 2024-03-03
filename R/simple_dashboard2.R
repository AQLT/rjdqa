

#' @param digits_outliers number of digits used in the table of outliers.
#' @param columns_outliers informations about outliers that should be printed in the summary table.
#' Can be either a vector of characters among `c("Estimate", "Std. Error", "T-stat", "Pr(>|t|)")`;
#' or an vector of integer: `1` corresponding to the estimate coefficient (`"Estimate"`),
#' `2` corresponding to the standard deviation error (`"Std. Error"`),
#' `3` corresponding to the t-statistic (`"T-stat"`) or
#' `4` corresponding to the p-value (`"Pr(>|t|)"`).
#' By default only the estimate coefficients and the t-statistics are printed 
#' (`columns_outliers = c("Estimate", "T-stat")`).
#' @param n_last_outliers number of last outliers to be printed (by default `n_last_outliers = 4`).
#' @param order_outliers order of the outliers in case of several outliers at the same date.
#' 
#' 
#' @rdname simple_dashboard
#' @export
simple_dashboard2 <- function(x, digits = 2,
                              scale_var_decomp = FALSE,
                              remove_others_contrib = FALSE,
                              digits_outliers = digits,
                              columns_outliers = c("Estimate", "T-stat"),
                              n_last_outliers = 4,
                              order_outliers = c("AO", "LS", "TC", "SO")) {
    if (inherits(x, "TRAMO_SEATS")) {
        x <- RJDemetra::jtramoseats(RJDemetra::get_ts(x), RJDemetra::tramoseats_spec(x))
    } else if (inherits(x, "X13")) {
        x <- RJDemetra::jx13(RJDemetra::get_ts(x), RJDemetra::x13_spec(x))
    }
    nb_format <- paste0("%.", digits, "f")
    if (is.numeric(columns_outliers)) {
        columns_outliers <- c("Estimate", "Std. Error", "T-stat", "Pr(>|t|)")[columns_outliers]
    } else {
        columns_outliers <- match.arg(columns_outliers,
                                      c("Estimate", "Std. Error", "T-stat", "Pr(>|t|)"), 
                                      several.ok = TRUE)
    }
    
    # Raw, trend, sa
    data_plot <- RJDemetra::get_indicators(x, c("y", "t", "sa", "y_f", "t_f", "sa_f"))
    
    last_date <- tail(time_to_date(data_plot[[1]]), 1)
    last_date <- format(last_date, format = "%Y-%m")
    
    data_plot <- do.call(ts.union, data_plot)
    # add observed data for plots
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
    
    est_span <- sprintf("Estimation span: %s to %s (%s obs)",
                        RJDemetra::get_indicators(x, "preprocessing.model.espan.start")[[1]],
                        RJDemetra::get_indicators(x, "preprocessing.model.espan.end")[[1]],
                        RJDemetra::get_indicators(x, "preprocessing.model.espan.n")[[1]]
    )
    transform <- ifelse(RJDemetra::get_indicators(x, "preprocessing.model.log")[[1]] == "false",
                        "Series hasn't been transformed",
                        "Series has been log-transformed")
    tde <- sprintf("%s, %s",
                   ifelse(ntd==0, "No trading days effect",
                          sprintf("Trading days effect (%s)", ntd)),
                   ifelse(is_easter, "easter effect",
                          "no easter effect"))
    # nb outliers
    nout <- RJDemetra::get_indicators(x, "preprocessing.model.nout")[[1]]
    out <- sprintf("%s detected outliers", nout)
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
    
    outliers <- outliers_color <- NULL
    if (nout > 0) {
        outliers <- do.call(rbind, RJDemetra::get_indicators(x, sprintf("preprocessing.model.out(%i)", seq_len(nout))))
        # sort outliers by dates
        dates_out <- outliers_to_dates(rownames(outliers))
        dates_out$type <- factor(dates_out$type, levels = order_outliers, ordered = TRUE)
        outliers <- outliers[order(dates_out$year, dates_out$period, dates_out$type, decreasing = TRUE), , drop = FALSE]
        outliers <- outliers[seq_len(min(n_last_outliers, nrow(outliers))), columns_outliers, drop = FALSE]
        outliers <- round(outliers, digits_outliers)
        outliers <- data.frame(rownames(outliers), 
                               outliers)
        colnames(outliers)[1] <- sprintf("Last %i outliers", n_last_outliers)
        rownames(outliers) <- NULL
        
        outliers_color <- 
            cbind(rep("grey90", nrow(outliers)),
                  matrix("white", ncol = ncol(outliers) - 1, nrow = nrow(outliers)))
    }
    
    res <- list(main_plot = data_plot,
                siratio_plot = ggdemetra::siratio(x),
                summary_text = summary_text,
                decomp_stats = list(table = decomp_stats,
                                    colors = decomp_stats_color),
                residuals_tests = list(table = all_tests,
                                       colors = color_test),
                last_date = last_date,
                outliers = list(table = outliers,
                                colors = outliers_color))
    class(res) <- c("simple_dashboard2")
    res
}
outliers_to_dates <- function(name_out){
    dates_out <- gsub("\\w. \\((.*)\\)", "\\1", name_out)
    types <- gsub(" .*", "", name_out)
    dates <- do.call(rbind, strsplit(dates_out, "-"))
    periods <- as.numeric(as.roman(dates[,1]))
    years <- as.numeric(dates[,2])
    data.frame(year = years, period = periods, type = types)
}
#' @rdname plot.simple_dashboard
#' @export
plot.simple_dashboard2 <- function(x, main = "Simple Dashboard with outliers",
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
    
    def.par <- par(no.readonly = TRUE)    
    
    nf <- layout(matrix(c(rep(1,8),
                          rep(2,4), rep(3,4),
                          rep(4,3), rep(5,5),
                          rep(4,3), rep(6,5),
                          rep(7,3), rep(8,5)),ncol = 8,byrow = T),
                 heights = c(0.2,2.2,0.5,0.2,0.7))
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
    # box()
    legend("topleft", legend = c(NA, summary_text), 
           bty = "n", text.font =  2, inset = c(0),
           cex = 0.95,
           xpd = TRUE)
    
    # plot.new()
    # # box()
    # legend("right", legend = c(arima_ord), 
    #        bty = "n", text.font =  2, inset = c(0),
    #        cex = 0.8)
    
    par(mar = rep(rep(2, 4)))
    par(mai = c(0, 0.2, 0, 0.2))
    
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
         axes = FALSE)
    plotrix::addtable2plot(0.5, 0,
                           decomp_stats$table, bty = "o", display.rownames = FALSE, hlines = TRUE,
                           vlines = TRUE,bg = decomp_stats$colors, xjust = 0.5, yjust = 1)
    
    # Empty plot
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
         axes = FALSE)
    
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
         axes = FALSE)
    par(mai = c(0, 0.2, 0.2, 0.2))
    
    if(! is.null(outliers$table)) {
        plotrix::addtable2plot(0.5, 0.7,
                               outliers$table, 
                               bg = outliers$colors,
                               bty = "o", 
                               display.rownames = FALSE, hlines = TRUE,
                               vlines = TRUE, 
                               xjust = 0.5, yjust = 0.5)
    }
    
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
         axes = FALSE)
    par(mai = c(0, 0.2, 0.2, 0.2))
    plotrix::addtable2plot(0.5, 0.8,
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
