#' #' Extract a quality assessment matrix
#' #'
#' #' Functions to extracts a quality report assessment from a CSV file exported by JDemetra+ (or the JDemetra+ cruncher) or from a seasonal adjustment object create by the RJDemetra package.
#' #'
#' #' @param x a CSV file containing the diagnostics matrix or a \code{SA} object. By default a dialog box opens to select a CSV file.
#' #' @param sep the field separator character in the CSV file (\code{sep = ";"} by default).
#' #' @param dec the character used in the file for decimal points (\code{dec = ","} by default).
#' #' @param series_name The name of series for the \code{extract_QA.SA} method.
#' #' By default the name of the \code{x} parameter is used.
#' #' @param ... unused argument.
#' #'
#' #' @details The function allows to extract a quality assessment matrix from a CSV file containing all the diagnostics 
#' #' (usually the \emph{demetra_m.csv} file) or from a \code{SA} object create by the package \code{RJDemetra}).
#' #'
#' #' The result is a \code{\link{QA_matrix}} object which is a list of three parameters:
#' #' * le paramètre \code{modalities} est un \code{data.frame} contenant un ensemble de variables sous forme catégorielle
#' #'   (Good, Uncertain, Bad, Severe).
#' #' * le paramètre \code{values} est un \code{data.frame} contenant les valeurs associées aux indicateurs présents dans
#' #'   \code{modalities} (i.e. : p-valeurs, statistiques, etc.) ainsi que des variables qui n'ont pas
#' #'   de modalité (fréquence de la série et modèle ARIMA).
#' #' * le paramètre \code{score_formula} est initié à \code{NULL} : il contiendra la formule utilisée pour
#' #'   calculer le score (si le calcul est fait).
#' #'
#' #' @encoding UTF-8
#' #' @return A \code{\link{QA_matrix}} object.
#' #' @family QA_matrix functions
#' #' @examples \dontrun{
#' #' QA <- extract_QA()
#' #' QA
#' #' # To extract the modalities' matrix:
#' #' QA$modalities
#' #' # Ou :
#' #' QA[["modalities"]]
#' #' }
#' #' @export
#' #' @name extract_QA
#' #' @rdname extract_QA
#' extract_QA <- function(x, ...){
#'     UseMethod("extract_QA", x)
#' }
#' 
#' #' @name extract_QA
#' #' @rdname extract_QA
#' #' @export
#' extract_QA.SA <- function(x, series_name, ...){
#'     if(missing(series_name))
#'         series_name <- deparse(substitute(x))
#'     
#'     arima_tests <- arima_test(x)
#'     frequency <- frequency(x$final$series[,"y"])
#'     arima_model <- x$regarima$arma
#'     arima_model <- sprintf("(%i,%i,%i)(%i,%i,%i)",
#'                            arima_model["p"], arima_model["d"], arima_model["q"],
#'                            arima_model["bp"], arima_model["bd"], arima_model["bq"])
#'     
#'     ############################################
#'     ####### ARIMA residuals tests ##############
#'     ############################################
#'     arima_tests <- arima_tests[,c("skewness","kurtosis","mean","normality","lb","lb2")]
#'     colnames(arima_tests) <- 
#'         sprintf("residuals_%s",
#'                 c("skewness","kurtosis","mean",
#'                   "normality","independency",
#'                   "homoskedasticity"))
#'     arima_tests[,  sprintf("%s_modality", colnames(arima_tests))] <-
#'         cut(as.numeric(arima_tests),
#'             breaks = c(0, 0.01, 0.1, 1),
#'             labels = c("Bad", "Uncertain", "Good"),
#'             right = FALSE)
#'     ############################################
#'     ####### Diagnostics  #######################
#'     ############################################
#'     res_tests <- get_residual_tests(x)
#'     res_tests <- res_tests[,c("Cycle", "Seasonal", "Irregular", "TD & Hol.", "Others", "Total", 
#'                               "Combined test", "Kruskall-Wallis", "Stability test", "Evolutive test", 
#'                               "ResS_SA_QS", "ResS_SA_F", "ResS_I_QS", "ResS_I_F", "ResTD_SA_F", 
#'                               "ResTD_I_F")]
#'     colnames(res_tests) <- c("cycle_contrib_to_var", "seasonal_contrib_to_var", "irregular_contrib_to_var",
#'                              "td_hol_contrib_to_var", "others_contrib_to_var", "total_contrib_to_var", 
#'                              "combined_test", "combined_kw", "combined_stability_test", "combined_evolutive_test",
#'                              "qs_residual_sa_on_sa","f_residual_sa_on_sa",
#'                              "qs_residual_sa_on_i","f_residual_sa_on_i",
#'                              "f_residual_td_on_sa", "f_residual_td_on_i")
#'     res_tests[,sprintf("%s_modality",
#'                        c("qs_residual_sa_on_sa","f_residual_sa_on_sa",
#'                          "qs_residual_sa_on_i","f_residual_sa_on_i",
#'                          "f_residual_td_on_sa", "f_residual_td_on_i"))] <- 
#'         cut(as.numeric(res_tests[, c("qs_residual_sa_on_sa","f_residual_sa_on_sa",
#'                                      "qs_residual_sa_on_i","f_residual_sa_on_i",
#'                                      "f_residual_td_on_sa", "f_residual_td_on_i")]), 
#'             breaks = c(0,0.001, 0.01, 0.05, 1),
#'             labels = c("Severe", "Bad", "Uncertain", "Good"),
#'             right = FALSE)
#'     
#'     ###########################################
#'     ####### Decomposition  #####################
#'     ############################################
#'     decomposition <- get_decomposition_info(x)
#'     colnames(decomposition) <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", 
#'                                        "q", "q_m2","henderson","IC_ratio","Seasonal_filter","global_MSR_ratio")
#'     
#' 
#'     decomposition[, sprintf("%s_modality", c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", 
#'                                              "q", "q_m2"))] <- 
#'         cut(as.numeric(decomposition[,c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", 
#'                                         "q", "q_m2")]), 
#'                                  breaks = c(0, 1, 2, Inf),
#'                                  labels = c("Good", "Bad", "Severe"), right = FALSE)
#'     
#'     #############################################
#'     
#'     QA_global <- data.frame(series = series_name, frequency = frequency,
#'                             arima_model = arima_model, arima_tests,
#'                             decomposition, res_tests,
#'                             stringsAsFactors = FALSE,row.names = NULL)
#'     c("skewness","kurtosis","mean",
#'       "normality","independency",
#'       "homoskedasticity")
#'     c("cycle_contrib_to_var", "seasonal_contrib_to_var", "irregular_contrib_to_var",
#'       "td_hol_contrib_to_var", "others_contrib_to_var", "total_contrib_to_var", 
#'       "combined_test", "combined_kw", "combined_stability_test", "combined_evolutive_test",
#'       "qs_residual_sa_on_sa","f_residual_sa_on_sa",
#'       "qs_residual_sa_on_i","f_residual_sa_on_i",
#'       "f_residual_td_on_sa", "f_residual_td_on_i")
#'     c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", 
#'       "q_value", "q_m2_value")
#'     modalities_variables <- c("series", sprintf("%s_modality",c("qs_residual_sa_on_sa","f_residual_sa_on_sa",
#'                               "qs_residual_sa_on_i","f_residual_sa_on_i",
#'                               "f_residual_td_on_sa", "f_residual_td_on_i",
#'                               "residuals_skewness","residuals_kurtosis","residuals_mean",
#'                               "residuals_normality","residuals_independency",
#'                               "residuals_homoskedasticity",
#'                               "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", 
#'                               "q", "q_m2")))
#'     values_variables <- c("series", "qs_residual_sa_on_sa","f_residual_sa_on_sa",
#'                           "qs_residual_sa_on_i","f_residual_sa_on_i",
#'                           "f_residual_td_on_sa", "f_residual_td_on_i",
#'                           "residuals_skewness","residuals_kurtosis","residuals_mean",
#'                           "residuals_normality","residuals_independency",
#'                           "residuals_homoskedasticity",
#'                           "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", 
#'                           "q", "q_m2",
#'                           "henderson","IC_ratio","Seasonal_filter","global_MSR_ratio",
#'                           "cycle_contrib_to_var", "seasonal_contrib_to_var", "irregular_contrib_to_var",
#'                           "td_hol_contrib_to_var", "others_contrib_to_var", "total_contrib_to_var", 
#'                           "combined_test", "combined_kw", "combined_stability_test", "combined_evolutive_test",
#'                           "frequency","arima_model")
#' 
#'     QA_modalities <- QA_global[,modalities_variables]
#'     colnames(QA_modalities) <- gsub("_modality","",colnames(QA_modalities))
#'     QA_values <- QA_global[,values_variables]
#'     rownames(QA_modalities) <- rownames(QA_values) <- NULL
#'     
#'     QA_modalities[,-1] <- lapply(QA_modalities[,-1],factor,
#'                                  levels = c("Good", "Uncertain", "Bad","Severe"), ordered = TRUE)
#'     QA <- QA_matrix(modalities = QA_modalities, values = QA_values)
#'     QA
#' }
#' #' @name extract_QA
#' #' @rdname extract_QA
#' #' @export
#' extract_QA.default <- function(x, sep = ";", dec = ",", ...){
#'     if(missing(x) || is.null(x)){
#'         if(Sys.info()[['sysname']] == "Windows"){
#'             x <- utils::choose.files(caption = "Sélectionner le fichier contenant la matrice des paramètres",
#'                                              filters = c("Fichier CSV","*.csv"))
#'         }else{
#'             x <- base::file.choose()
#'         }
#'     }
#'     if(length(x) == 0)
#'         stop("Il faut choisir un fichier")
#'     if(!file.exists(x)|length(grep("\\.csv$",x))==0)
#'         stop("Le fichier n'existe pas ou n'est pas un fichier csv")
#' 
#'     demetra_m <- read.csv(file = x,
#'                       sep = sep, dec = dec, stringsAsFactors = FALSE,
#'                       na.strings = c("NA","?"))
#'     missing_variables <- which(is.na(match(c("qs.test.on.sa", "f.test.on.sa..seasonal.dummies.", "on.sa",
#'                                              "on.sa..last.3.years.","on.irregular","qs.test.on.i","f.test.on.i..seasonal.dummies.",
#'                                              "f.test.on.sa..td.", "f.test.on.i..td.","independence","normality"),
#'                                            colnames(demetra_m))))
#'     if(length(missing_variables)!=0){
#'         stop(paste0("Il manque le(s) variable(s) suivante(s) dans la matrice des diagnostics :\n"
#'                     ,c("qs.test.on.sa", "f.test.on.sa..seasonal.dummies.", "on.sa",
#'                        "on.sa..last.3.years.","on.irregular","qs.test.on.i","f.test.on.i..seasonal.dummies.",
#'                        "f.test.on.sa..td.", "f.test.on.i..td.","independence","normality")[missing_variables],
#'                     "\nRelancez l'export"))
#'     }
#' 
#'     demetra_m$series <- gsub("(^ *)|(* $)", "",
#'                              gsub("(^.* \\* )|(\\[frozen\\])", "", demetra_m[,1]))
#'     demetra_m$frequency <- extractFrequency(demetra_m)
#' 
#'     demetra_m <- cbind(demetra_m,
#'                        extractARIMA(demetra_m),
#'                        extractStatQ(demetra_m),
#'                        extractOOS_test(demetra_m))
#'     homoskedasticity_df <- 2 * demetra_m$frequency - demetra_m$arima_p-demetra_m$arima_q-demetra_m$arima_bp-demetra_m$arima_bq
#'     demetra_m$homoskedasticity_pvalue <- 1 - pchisq(demetra_m$lb2, df = homoskedasticity_df)
#'     demetra_m$homoskedasticity_modality <- cut(demetra_m$homoskedasticity_pvalue,
#'                                                breaks = c(0, 0.01, 0.1, 1),
#'                                                labels = c("Bad", "Uncertain", "Good"),
#'                                                right = FALSE)
#'     demetra_m$mean_residuals_pvalue <- 2 * (1 - pt(abs(demetra_m$mean), demetra_m$neffectiveobs))
#'     demetra_m$mean_residuals_modality <- cut(demetra_m$mean_residuals_pvalue,
#'                                              breaks = c(0, 0.01, 0.1, 1),
#'                                              labels = c("Bad", "Uncertain", "Good"),
#'                                              right = FALSE)
#'     demetra_m$pct_outliers_value <- demetra_m[,match("number.of.outliers",colnames(demetra_m))+1] * 100
#'     demetra_m$pct_outliers_modality <- demetra_m$number.of.outliers
#'     demetra_m$m7_modality <- cut(demetra_m$m7+0, #+0 pour forcer en numeric si que des NA
#'                                  breaks = c(0, 1, 2, Inf),
#'                                  labels = c("Good", "Bad", "Severe"), right = FALSE)
#' 
#'     colnames(demetra_m)[match(c("qs.test.on.sa", "f.test.on.sa..seasonal.dummies.", "on.sa",
#'                                 "on.sa..last.3.years.","on.irregular","qs.test.on.i","f.test.on.i..seasonal.dummies.",
#'                                 "f.test.on.sa..td.", "f.test.on.i..td.","independence","normality"),
#'                               colnames(demetra_m))+1] <- paste0(c("qs.test.on.sa", "f.test.on.sa..seasonal.dummies.", "on.sa",
#'                                                                   "on.sa..last.3.years.","on.irregular","qs.test.on.i","f.test.on.i..seasonal.dummies.",
#'                                                                   "f.test.on.sa..td.", "f.test.on.i..td.","independence","normality"),"_pvalue")
#'     modalities_variables <- c("series","qs.test.on.sa", "f.test.on.sa..seasonal.dummies.", "on.sa",
#'                               "on.sa..last.3.years.","on.irregular","qs.test.on.i","f.test.on.i..seasonal.dummies.",
#'                               "f.test.on.sa..td.", "f.test.on.i..td.","mean_residuals_modality",
#'                               "independence","homoskedasticity_modality","normality","oos_mean_modality",
#'                               "oos_mse_modality","m7_modality","q_modality","q_m2_modality","pct_outliers_modality")
#' 
#'     values_variables <- c("series","qs.test.on.sa_pvalue","f.test.on.sa..seasonal.dummies._pvalue","on.sa_pvalue",
#'                           "on.sa..last.3.years._pvalue","on.irregular_pvalue","qs.test.on.i_pvalue","f.test.on.i..seasonal.dummies._pvalue",
#'                           "f.test.on.sa..td._pvalue","f.test.on.i..td._pvalue","mean_residuals_pvalue",
#'                           "independence_pvalue","homoskedasticity_pvalue","normality_pvalue","oos_mean_pvalue",
#'                           "oos_mse_pvalue","m7","q_value","q_m2_value","pct_outliers_value",
#'                           "frequency","arima_model")
#' 
#'     if(!all(modalities_variables %in% colnames(demetra_m),
#'             values_variables %in% colnames(demetra_m))){
#'         missing_variables <- unique(c(modalities_variables[!modalities_variables %in% colnames(demetra_m)],
#'                                       values_variables[!values_variables %in% colnames(demetra_m)]))
#'         missing_variables <- paste(missing_variables,collapse = "\n")
#'         stop(paste0("Il manque le(s) variable(s) suivante(s) dans la matrice des diagnostics :\n"
#'                     ,missing_variables,"\nRelancez l'export"))
#'     }
#' 
#'     names_QA_variables <- c("series","qs_residual_sa_on_sa","f_residual_sa_on_sa","combined_residual_sa_on_sa",
#'                             "combined_residual_sa_on_sa_last_years","combined_residual_sa_on_i",
#'                             "qs_residual_sa_on_i","f_residual_sa_on_i",
#'                             "f_residual_td_on_sa","f_residual_td_on_i","residuals_mean",
#'                             "residuals_independency","residuals_homoskedasticity","residual_normality",
#'                             "oos_mean","oos_mse","m7","q","q_m2","pct_outliers")
#'     QA_modalities <- demetra_m[,modalities_variables]
#'     QA_values <- demetra_m[,values_variables]
#'     rownames(QA_modalities) <- rownames(QA_values) <- NULL
#'     colnames(QA_values)[1:length(names_QA_variables)] <- colnames(QA_modalities) <- names_QA_variables
#'     QA_modalities[,-1] <- lapply(QA_modalities[,-1],factor,
#'                                  levels = c("Good", "Uncertain", "Bad","Severe"), ordered = TRUE)
#'     QA <- QA_matrix(modalities = QA_modalities, values = QA_values)
#'     QA
#' }
#' 
#' extractARIMA <- function(demetra_m){
#'     q_possibles <- grep("(^q$)|(^q\\.\\d$)",colnames(demetra_m))
#'     bp_possibles <- grep("(^bp$)|(^bp\\.\\d$)",colnames(demetra_m))
#' 
#'     val_q <- demetra_m[, q_possibles]
#'     val_bq <- demetra_m[,bp_possibles]
#' 
#'     if(length(q_possibles) > 1){
#'         integer_col <- which(sapply(val_q, is.integer))
#'         if(length(integer_col) == 0){
#'             val_q <- rep(NA, nrow(val_q))
#'         }else{
#'             val_q <- val_q[, integer_col[1]]
#'         }
#' 
#'     }
#'     if(length(bp_possibles) > 1){
#'         integer_col <- which(sapply(val_bq,is.integer))
#'         if(length(integer_col) == 0){
#'             val_bq <- rep(NA, nrow(val_bq))
#'         }else{
#'             val_bq <- val_bq[, integer_col[1]]
#'         }
#' 
#'     }
#' 
#'     if(!all(is.integer(val_q) || all(is.na(val_q)),
#'            is.integer(val_bq) || all(is.na(val_q))))
#'         stop("Erreur dans l'extraction du paramètre q ou bq du modèle ARIMA : revoir l'extraction")
#'     arima <- data.frame(arima_p = demetra_m[,"p"], arima_d = demetra_m[,"d"], arima_q = val_q,
#'                         arima_bp = val_bq, arima_bd = demetra_m[,"bd"], arima_bq = demetra_m[,"bq"],
#'                         arima_model = demetra_m[,"arima"])
#'     return(arima)
#' }
#' extractOOS_test <- function(demetra_m){
#'     mean_possibles <- grep("(^mean$)|(^mean\\.\\d$)",colnames(demetra_m))
#'     col_mean <- mean_possibles
#'     if(length(mean_possibles) > 1){
#'         col_mean_possibles <- demetra_m[,mean_possibles]
#'         character_cols <- which(sapply(col_mean_possibles, is.character))
#'         if(length(character_cols) == 0){
#'             col_all_NA <- which(apply(is.na(col_mean_possibles),2,all))
#'             if(length(col_all_NA) == 0){
#'                 stop("Erreur dans l'extraction des diagnostics en out of sample")
#'             }else{
#'                 col_mean <- mean_possibles[col_all_NA[1]]
#'             }
#'         }else{
#'             col_mean <- mean_possibles[character_cols[1]]
#'         }
#'     }
#'     col_mse <- match("mse",colnames(demetra_m))[1]
#'     if(!all(is.character(demetra_m[,col_mean]) || all(is.na(demetra_m[,col_mean])),
#'             is.double(demetra_m[,col_mean+1]) || all(is.na(demetra_m[,col_mean+1])),
#'             is.character(demetra_m[,col_mse]) || all(is.na(demetra_m[,col_mse])),
#'             is.double(demetra_m[,col_mse+1]) || all(is.na(demetra_m[,col_mse+1]))
#'     ))
#'         stop("Erreur dans l'extraction des diagnostics en out of sample")
#' 
#'     stat_OOS <- data.frame(demetra_m[,col_mean+c(0,1)], demetra_m[,col_mse+c(0,1)],
#'                            stringsAsFactors = FALSE)
#'     colnames(stat_OOS) <- c("oos_mean_modality","oos_mean_pvalue","oos_mse_modality","oos_mse_pvalue")
#'     return(stat_OOS)
#' }
#' extractStatQ <- function(demetra_m){
#'     q_possibles <- grep("(^q$)|(^q\\.\\d$)",colnames(demetra_m))
#'     q_m2_possibles <- grep("(^q\\.m2$)|(^q\\.m2\\.\\d$)",colnames(demetra_m))
#'     col_q <- q_possibles
#'     col_q_m2 <- q_m2_possibles
#'     if(length(q_possibles) > 1){
#'         col_q_possibles <- demetra_m[,q_possibles]
#'         # col_q <- q_possibles[sapply(col_q_possibles,is.character)][1]
#'         character_cols <- which(sapply(col_q_possibles, is.character))
#'         if(length(character_cols) == 0){
#'             col_all_NA <- which(apply(is.na(col_q_possibles),2,all))
#'             if(length(col_all_NA) == 0){
#'                 stop("Erreur dans l'extraction des stats Q et Q-M2")
#'             }else{
#'                 col_q <- q_possibles[col_all_NA[1]]
#'             }
#'         }else{
#'             col_q <- q_possibles[character_cols[1]]
#'         }
#'     }
#'     if(length(q_m2_possibles) > 1){
#'         col_q_m2_possibles <- demetra_m[,q_m2_possibles]
#'         # col_q_m2 <- q_m2_possibles[sapply(col_q_m2_possibles,is.character)]
#'         character_cols <- which(sapply(col_q_m2_possibles, is.character))
#'         if(length(character_cols) == 0){
#'             col_all_NA <- which(apply(is.na(col_q_m2_possibles),2,all))
#'             if(length(col_all_NA) == 0){
#'                 stop("Erreur dans l'extraction des stats Q et Q-M2")
#'             }else{
#'                 col_q_m2 <- q_m2_possibles[col_all_NA[1]]
#'             }
#'         }else{
#'             col_q_m2 <- q_m2_possibles[character_cols[1]]
#'         }
#'     }
#'     if(!all(is.character(demetra_m[,col_q]) || all(is.na(demetra_m[,col_q])),
#'             is.double(demetra_m[,col_q+1]) || all(is.na(demetra_m[,col_q+1])),
#'             is.character(demetra_m[,col_q_m2]) || all(is.na(demetra_m[,col_q_m2])),
#'             is.double(demetra_m[,col_q_m2+1])) || all(is.na(demetra_m[,col_q_m2+1])))
#'         stop("Erreur dans l'extraction des stats Q et Q-M2")
#' 
#'     stat_Q <- data.frame(demetra_m[, col_q+c(0,1)], demetra_m[, col_q_m2+c(0,1)],
#'                          stringsAsFactors = FALSE)
#'     colnames(stat_Q) <- c("q_modality","q_value","q_m2_modality","q_m2_value")
#' 
#'     return(stat_Q)
#' }
#' extractFrequency <- function(demetra_m){
#'     if(any(is.na(match(c("start","end","n"),colnames(demetra_m)))))
#'         stop("Erreur lors de l'extraction de la fréquence (il manque la date de début, date de fin ou le nombre d'obs)")
#'     start <- as.Date(demetra_m$start,format="%Y-%m-%d")
#'     end <- as.Date(demetra_m$end,format="%Y-%m-%d")
#'     n <- demetra_m$n
#'     i <- 3
#' 
#'     start <- data.frame(y = as.numeric(format(start,"%Y")), m = as.numeric(format(start,"%m")))
#'     end <- data.frame(y = as.numeric(format(end,"%Y")), m = as.numeric(format(end,"%m")))
#'     freq <- c(12,6,4,3,2)
#'     nobs_compute <- matrix(sapply(freq,function(x){
#'         x * (end[,1] - start[,1]) + (end[,2] - start[,2])/(12/x)
#'     }),nrow = nrow(demetra_m))
#'     return(sapply(1:nrow(nobs_compute), function(i){
#'         freq[which((nobs_compute[i,] == n[i]) | (nobs_compute[i,] + 1 == n[i]) | (nobs_compute[i,] - 1 == n[i]))[1]]
#'     }))
#' }
#' 
