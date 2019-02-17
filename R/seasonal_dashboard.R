#
#' @export
sa_dashboard <- function(x){
    if (inherits(x, "X13") && !all(c("decomposition.c17","preprocessing.model.tde_f",
                                     "preprocessing.model.mhe_f") %in% names(x$user_defined))) {
        my_spec <- RJDemetra::x13_spec(x)
        x <- RJDemetra::x13(x$final$series[,"y"], my_spec,
                            userdefined = c("decomposition.c17","preprocessing.model.tde_f",
                                            "preprocessing.model.mhe_f"))
    }
    if (inherits(x, "TRAMO_SEATS") && !all(c("preprocessing.model.tde_f",
                                     "preprocessing.model.mhe_f") %in% names(x$user_defined))) {
        my_spec <- RJDemetra::tramoseats_spec(x)
        x <- RJDemetra::tramoseats(x$final$series[,"y"], my_spec,
                                   userdefined = c("preprocessing.model.tde_f",
                                                   "preprocessing.model.mhe_f"))
    }
    
    last_date <- tail(time_to_date(x$final$series[,"y"]), 1)
    last_date <- format(last_date, format = "%Y-%m")
    res <- list(recent_history = recent_history(x),
         summary_diagnostics = summary_diagnostics(x),
         trading_day_pattern = trading_day_pattern(x),
         moving_holiday_pattern = moving_holiday_pattern(x),
         seasonal_pattern = seasonal_pattern(x),
         net_effect = net_effect(x), last_date = last_date)
    class(res) <- c("sa_dashboard")
    res
}
#' Plot a seasonal adjustment dashboard
#' @export
plot.sa_dashboard <- function(x, main = "Seasonal Adjustment Dashboard",
                              subtitle = "",
                              raw_color = "#33A02C",
                              sa_color = "#E31A1C",
                              trend_color = "black", ...){
    r_history <- (x$recent_history)
    diagnostics <- (x$summary_diagnostics)
    td <- (x$trading_day_pattern)
    mhe <- (x$moving_holiday_pattern)
    seas <- (x$seasonal_pattern)
    net_effects <- (x$net_effect)
    last_date <- x$last_date
    
    def.par <- par(no.readonly = TRUE)    
    
    # nf <- layout(matrix(c(rep(1,3),rep(2,3),
    #                       rep(3,2), rep(5,2), rep(7,2),
    #                       rep(4,2), rep(6,2), rep(8,2),
    #                       rep(9,2), rep(10,2), rep(11,2)),4,6,byrow = T),heights = c(4,4,1,4))
    nf <- layout(matrix(c(rep(1,6),
                          rep(2,3), rep(3,3),
                          rep(4,6),
                          rep(5,2), rep(7,2), rep(9,2),
                          rep(6,2), rep(8,2), rep(10,2),
                          rep(11,6),
                          rep(12,2), rep(13,2), rep(14,2)),7,6,byrow = T),
                 heights = c(0.3,4,0.3,4,1,0.3,4))
    on.exit({
        par(def.par)
    })
    # layout.show(nf)
    # grid.rect(x = unit(0.42, "npc"), y = unit(0.35, "npc"),
    #           width = unit(0.2, "npc"), height = unit(0.2, "npc"))
    oma.saved <- par("oma")
    par(oma = rep.int(0, 4))
    par(oma = oma.saved)
    o.par <- par(mar = rep.int(0, 4))
    plot.new()
    box(which = "inner")
    
    box()
    text(0.5, 0.5, main, font = 2,cex = 1.2)
    par(o.par)
    
    par(mai = c(0.5, 0.4, 0.4, 0.1))
    plot(r_history, raw_color = raw_color,
         sa_color = sa_color,
         trend_color = trend_color)
    # plot(0);
    # l=locator(2,type='n');
    # lines(l, type='l');
    # # plot.recent_history()
    par(mai = c(0.0, 0, 0, 0.42))
    plot(diagnostics)
   
    
    
    # oma.saved <- par("oma")
    # par(oma = rep.int(0, 4))
    # par(oma = oma.saved)
    
    o.par <- par(mai = c(1.02, 0.82, 0.82, 0.42),mar = rep.int(0, 4))
    plot.new()
    box()
    text(0.5, 0.5, "Net Effect of Seasonal Adjustment",font = 2,cex = 1.2)
    par(o.par)
    
    par(mai = c(0.4, 0.82, 0.5, 0.42))
    plot(td)
    # par(mai = c(0.2, 0.82, 0.5, 0.42))
    plot(mhe)
    # par(mai = c(0.2, 0.82, 0.5, 0.42))
    plot(seas)
    
    o.par <- par(mai = c(1.02, 0.82, 0.82, 0.42), mar = rep.int(0, 4))
    plot.new()
    box()
    text(0.5, 0.5, "Estimated Patterns and Anticipated Movements",font = 2,cex = 1.2)
    par(o.par)
    
    # mtext("Estimated Patterns and Anticipated Movements", side = 3, line = -20,
    #       outer = TRUE, font = 2)
    
    par(mai = c(0.4, 0.4, 0.5, 0.2))
    plot(net_effects, raw_color = raw_color,
         trend_color = trend_color)
    # mtext("Net Effect of Seasonal Adjustment", side = 3, line = -46,
    #       outer = TRUE,font = 2)
    # mtext("Seasonal Adjustment Dashboard", side = 3, line = -2,
    #       outer = TRUE,font = 2,cex = 1.2)
    mtext(sprintf("Reference Month: %s",last_date), side = 3, line = -3, 
          outer = TRUE,font = 3,cex = 0.7,at = 0.95, adj = 1)
    mtext(subtitle, side = 3, line = -3, 
          outer = TRUE,font = 3,cex = 0.7,at = 0.1, adj = 1)
    invisible()
}

add_box <- function(){
    o.par <- par(mai = c(1.02, 0.82, 0.82, 0.42),mar = rep.int(0, 4))
    box()
    par(o.par)
}
