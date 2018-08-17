
sa_dashboard <- function(x){
    if(!all(c("decomposition.c17","preprocessing.model.tde_f",
              "preprocessing.model.mhe_f") %in% names(x$user_defined))){
        my_spec <- x13_spec(x)
        x <- x13(x$final$series[,"y"], my_spec,userdefined = c("decomposition.c17","preprocessing.model.tde_f",
                                                               "preprocessing.model.mhe_f"))
    }
    last_date <- tail(time_to_date(x), 1)
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
plot.sa_dashboard <- function(x, main = "Seasonal Adjustment Dashboard", ...){
    recent_history <- recent_history(x$recent_history)
    diagnostics <- summary_diagnostics(x$summary_diagnostics)
    td <- trading_day_pattern(x$trading_day_pattern)
    mhe <- moving_holiday_pattern(x$moving_holiday_pattern)
    seas <- seasonal_pattern(x$seasonal_pattern)
    net_effects <- net_effect(x$net_effect)
    last_date <- x$last_date
    
    def.par <- par(no.readonly = TRUE)    
    
    nf <- layout(matrix(c(rep(1,3),rep(2,3),
                          rep(3,2), rep(5,2), rep(7,2),
                          rep(4,2), rep(6,2), rep(8,2),
                          rep(9,2), rep(10,2), rep(11,2)),4,6,byrow = T),heights = c(4,4,1,4))
    # layout.show(nf)
    # grid.rect(x = unit(0.42, "npc"), y = unit(0.35, "npc"),
    #           width = unit(0.2, "npc"), height = unit(0.2, "npc"))
    par(mai = c(0.8, 0.4, 0.4, 0.1))
    plot(r_history)
    par(mai = c(0.1, 0, 0, 0.42))
    plot(diagnostics)
    par(mai = c(0.2, 0.82, 0.5, 0.42))
    plot(td)
    par(mai = c(0.2, 0.82, 0.5, 0.42))
    plot(mhe)
    par(mai = c(0.2, 0.82, 0.5, 0.42))
    plot(seas)
    mtext("Estimated Patterns and Anticipated Movements", side = 3, line = -20,
          outer = TRUE, font = 2)
    
    par(mai = c(0.4, 0.4, 0.5, 0.2))
    plot(net_effects)
    
    mtext("Net Effect of Seasonal Adjustment", side = 3, line = -46,
          outer = TRUE,font = 2)
    mtext("Seasonal Adjustment Dashboard", side = 3, line = -2,
          outer = TRUE,font = 2,cex = 1.2)
    mtext(sprintf("Reference Month: %s",last_date), side = 3, line = -3, 
          outer = TRUE,font = 3,cex = 0.7,at = 0.95, adj = 1)
    
    
    par(def.par)
}