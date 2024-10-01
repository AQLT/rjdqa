# autoplot.simple_dashboard <- function(object, main = "Simple Dashboard with outliers",
#                                       color_series = c(y = "#F0B400", t = "#1E6C0B", sa = "#155692"),
#                                       reference_date = TRUE,
#                                       text_base_size = 10,
#                                       table_base_size = 10, ...) {
#     x <- object
#     main_plot <- x$main_plot
#     siratio_plot <- x$siratio_plot
#     summary_text <- x$summary_text
#     decomp_stats <- x$decomp_stats
#     residuals_tests <- x$residuals_tests
#     last_date <- x$last_date
#     outliers <- x$outliers
#     # Check if it is simple_dashboard (TRUE) or simple_dashboard2
#     is_sd <- is.null(outliers)
#     p_global <- ggplot(data = ggdemetra::ts2df(main_plot), aes(x = date)) +
#         geom_line(aes(y = y, color = "y"), na.rm = TRUE, show.legend = TRUE) +
#         geom_line(aes(y = t, color = "t"), na.rm = TRUE, show.legend = TRUE) +
#         geom_line(aes(y = sa, color = "sa"), na.rm = TRUE, show.legend = TRUE) +
#         geom_line(aes(y = y_f, color = "y"), na.rm = TRUE, linetype = 2) +
#         geom_line(aes(y = t_f, color = "t"), na.rm = TRUE, linetype = 2) +
#         geom_line(aes(y = sa_f, color = "sa"), na.rm = TRUE, linetype = 2) +
#         labs(x = NULL, y = NULL)
#     p_global <- p_global +
#         # on change l'ordre de la legende et les couleurs
#         scale_color_manual(breaks=names(color_series),
#                            values=color_series) +
#         theme_bw() +
#         # On met la legende en bas du graphique
#         theme(legend.position="bottom", legend.title = element_blank(),
#               legend.box.spacing = unit(0, "pt"))
#     p_siratio <- ggdemetra::ggsiratioplot(siratio_plot, main = NULL) + theme_bw()
# 
#     p_text_sum <- grid::textGrob(
#         paste(summary_text, collapse = "\n"),
#         gp=grid::gpar(fontsize=text_base_size),
#         hjust = 0, vjust = 1,
#         vp = grid::viewport(x = 0,y = 1))
# 
#     if(decomp_stats$colors[[3]] == "white") {
#         #X13
#         col_head <- c(rep("grey80", 2),
#                       "white",
#                       rep("grey80", length(decomp_stats$colors) - 3))
#     } else {
#         col_head <- rep("grey80", length(decomp_stats$colors))
#     }
#     th_decomp_stat <- gridExtra::ttheme_default(core=list(
#         bg_params = list(fill=decomp_stats$colors)
#     ),
#     colhead = list(bg_params = list(fill = col_head)),
#     base_size = table_base_size,
#     padding = unit(c(2, 1), "mm"))
#     p_tab_decomp_stat <- gridExtra::tableGrob(
#         decomp_stats$table,
#         theme = th_decomp_stat, rows = NULL,
#         vp = grid::viewport(x = .5,y = 1))
# 
#     th_res_test <- gridExtra::ttheme_default(core=list(
#         bg_params = list(fill=residuals_tests$colors)
#     ), base_size = table_base_size,
#     padding = unit(c(2, 1), "mm"))
#     p_tab_tests <- gridExtra::tableGrob(
#         residuals_tests$table,
#         theme = th_res_test,
#         vp = grid::viewport(x = .4,y = .5))
# 
#     if (is_sd) {
#         p_tab_outliers <- NULL
#         layout <- "
# AAAABBBB
# CCCDDDDD
# CCCEEEEE
# "
#         heights <- c(3,0.3,1.2)
#     } else {
#         th_outliers <- gridExtra::ttheme_default(core=list(
#             bg_params = list(fill=outliers$colors)
#         ), base_size = table_base_size,
#         padding = unit(c(2, 1), "mm"))
#         p_tab_outliers <- gridExtra::tableGrob(
#             outliers$table,
#             theme = th_outliers,
#             rows = NULL,
#             vp = grid::viewport(x = .4,y = .5))
#         layout <- "
# AAAABBBB
# CCCDDDDD
# EEEEFFFF
# "
#         heights <- c(3,1, 1)
#     }
#     all_plots <- list(p_global, p_siratio, p_text_sum, p_tab_decomp_stat,
#                       p_tab_outliers, p_tab_tests)
#     if (is_sd) {
#         all_plots[[5]] <- NULL
#     }
# 
#     if (reference_date)  {
#         subtitle <- sprintf("Reference Date: %s",last_date)
#     } else {
#         subtitle <- NULL
#     }
#     p_dashboard <-  wrap_plots(all_plots) +
#         plot_layout(design = layout, heights = heights) +
#         plot_annotation(
#             title = main,
#             subtitle = subtitle,
#             theme = theme(
#                 plot.title = element_text(hjust = 0.5, margin = margin()),
#                 plot.title.position = "plot",
#                 plot.subtitle = element_text(hjust = 1, margin = margin()),
#                 plot.caption.position = "panel",
#                 plot.background = element_rect(colour = "black",linewidth = .5)
#                 # plot.margin = unit(c(0, 0, 0, 0), "cm")
#             )
#         )
#     p_dashboard
# }
# autoplot(dashboard_data)
# autoplot(dashboard_data2)
