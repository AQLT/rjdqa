# quality_matrix <- function(x, series_name){
#     if(missing(series_name))
#         series_name <- deparse(substitute(x))
#     if(!all(c("preprocessing.residuals.nruns",
#               "preprocessing.residuals.lruns") %in% names(x$user_defined))){
#         my_spec <- RJDemetra::x13_spec(x)
#         x <- RJDemetra::x13(x$final$series[,"y"], my_spec,
#                  userdefined = c("preprocessing.residuals.nruns",
#                                  "preprocessing.residuals.lruns"))
#     }
# 
#     
#     dates <- time_to_date(x$final$series)
#     span <- x$regarima$model$spec_rslt["T.span"]
#     
#     regarima_coefs <- summary(x$regarima)
#     regarima_coefs <- rbind(regarima_coefs$coefficients[[1]],
#                             regarima_coefs$coefficients[[2]],
#                             regarima_coefs$coefficients[[3]],
#                             regarima_coefs$coefficients[[4]])
#     
#     period <- frequency(x$final$series)
#     n_obs <- length(dates)
#     n_efff_obs <- x$regarima$loglik["neffectiveobs",]
#     missing <- sum(is.na(x$final$series[,"y"]))
#     n_obs <- n_obs - missing
#     start <- gsub("(^from )|( to.*$)", "", span)
#     end <- gsub("^.* to ", "", span)
#     
#     method <- class(x)[2]
#     log_transformation <- x$regarima$model$spec_rslt["Log transformation"] == TRUE
#     nb_params <- x$regarima$loglik["np",]
#     arima_orders <- arima_order_coef(x, regarima_coefs)
#     
#     cste <- get_cste(x)
#     LY <- get_LY(x, regarima_coefs)
#     MH <- get_MH(x, regarima_coefs)
#     TD <- get_TD(x, regarima_coefs)
#     n_outliers <- n_out(regarima_coefs)
#     ic <- t(x$regarima$loglik[c("aic", "aicc", "bic", "bicc"), , drop = FALSE])
#     arima_tests <- arima_test(x)
#     
#     decomposition <- get_decomposition_info(x)
#     #OOS test
#     res_tests <- get_residual_tests(x)
#     
#     results <- data.frame(period = period, Nobs = n_obs, Missing = missing,
#                           Start = start, End = end, Method = method,
#                           Log_transformation = log_transformation,
#                           NEffObs = n_efff_obs, NbParams = nb_params,
#                           cste, arima_orders, LY, MH, TD, n_outliers, ic,
#                           arima_tests, res_tests, decomposition,
#                           row.names = series_name,
#                           check.names = FALSE,
#                           stringsAsFactors = FALSE
#     )
#     results
# 
# }
get_decomposition_info <- function(x){
    UseMethod("get_decomposition_info", x)
}
get_decomposition_info.X13 <- function(x){
    decomp <- x$decomposition
    mstats <- decomp$mstats
    
    trend_filter <- decomp$t_filter
    seas_filter <- decomp$s_filter
    ic_ratio <- NA
    global_msr_ratio <- NA
    result <- data.frame(t(mstats), trend_filter = trend_filter, ic_ratio = ic_ratio,
               seas_filter, global_msr_ratio, stringsAsFactors = FALSE
               )
    colnames(result) <- c(rownames(mstats), "Trend filter", "IC ratio", "Seasonal filter", "Global MSR Ratio")
    result
}

n_out <- function(regarima_coefs){
    variables <- rownames(regarima_coefs)
    n_AO <- length(grep("^AO (.*)$", variables))
    n_LS <- length(grep("^LS (.*)$", variables))
    n_TC <- length(grep("^TC (.*)$", variables))
    n_SO <- length(grep("^SO (.*)$", variables))
    result <- c(n_AO, n_LS, n_TC, n_SO)
    result <- c(sum(result), result)
    result <- as.data.frame(matrix(result, nrow = 1), row.names = NULL, stringsAsFactors = FALSE)
    names(result) <- c("Noutliers", "NoutAO", "NoutLS", "NoutTC", "NoutSO")
    result
}

arima_order_coef <- function(x, regarima_coefs){
    arima_orders <- x$regarima$arma
    
    for (i in 1:6) {
        if (i > arima_orders["p"]) {
            assign(sprintf("phi%i",i), c(Estimate = NA, `Pr(>|t|)` = NA) )
        }else{
            assign(sprintf("phi%i",i),
                   regarima_coefs[sprintf("Phi(%i)",i), c("Estimate", "Pr(>|t|)")])
        }
        if (i > arima_orders["q"]) {
            assign(sprintf("theta%i",i), c(Estimate = NA, `Pr(>|t|)` = NA) )
        }else{
            assign(sprintf("theta%i",i),
                   regarima_coefs[sprintf("Theta(%i)",i), c("Estimate", "Pr(>|t|)")])
        }
    }
    if (arima_orders["bp"] == 0) {
        bphi1 <- c(Estimate = NA, `Pr(>|t|)` = NA) 
    }else{
        bphi1 <- regarima_coefs["BPhi(1)", c("Estimate", "Pr(>|t|)")]
    }
    if (arima_orders["bq"] == 0) {
        btheta1 <- c(Estimate = NA, `Pr(>|t|)` = NA) 
    }else{
        btheta1 <- regarima_coefs["BTheta(1)", c("Estimate", "Pr(>|t|)")]
    }
    
    result <- matrix(c(arima_orders,
                phi1, phi2, phi3, phi4, phi5, phi6,
                theta1, theta2, theta3, theta4, theta5, theta6,
                bphi1, btheta1), nrow = 1)
    result <- as.data.frame(result,row.names = NULL)

    colnames(result) <- c(names(arima_orders), sprintf("Phi%i%s", rep(1:6,each = 2), c(""," (Pvalue)")),
                       sprintf("Theta%i%s",  rep(1:6,each = 2), c(""," (Pvalue)")),
                       "BPhi1", "BPhi1 (Pvalue)", "BTheta1", "BTheta1 (Pvalue)"
                       )
    result
}
# 
# regarima_coef <- function(x){
#     arima_coef <- x$regarima$arima.coefficients
#     reg_coef <- x$regarima$regression.coefficients
#     rslt_spec <- x$regarima$model$spec_rslt
#     loglik<- x$regarima$loglik
#     usr_spec <- x$regarima$specification$regression$userdef$specification
#     out <- s_preOut(x)
#     var <- s_preVar(x)$description
#     
#     fvar <- fout <- NULL
#     
#     if (!is.null(arima_coef)){
#         a_tvalues=matrix(2*(1 - pt(abs(arima_coef[,3]), loglik[3])),ncol=1)
#         colnames(a_tvalues)=c("Pr(>|t|)")
#         arima_coef <- cbind(arima_coef,a_tvalues)
#         
#     }
#     if (!is.null(reg_coef)){
#         r_tvalues=matrix(2*(1 - pt(abs(reg_coef[,3]), loglik[3])),ncol=1)
#         colnames(r_tvalues)=c("Pr(>|t|)")
#         reg_coef <- cbind(reg_coef,r_tvalues)
#     }
#     if (usr_spec[1]==TRUE & usr_spec[2]==TRUE){
#         out <- out[out[,3]!=0,]
#         if (dim(out)[1]!=0){
#             out_t <- as.character(out[,1])
#             out_y <- substr(out[,2],1,4)
#             out_m <- as.character(as.numeric(substr(out[,2],6,7)))
#             out_dsc <- paste(out_t," (",out_m,"-",out_y,")",sep = "")
#             colnames(out) <- c("","","Coefficients")
#             rownames(out) <- out_dsc
#             fout <- out[3]
#             fout <- cbind(fout, NA)
#             colnames(fout)[ncol(fout)] <- "Pr(>|t|)"
#         }
#     }
#     if (usr_spec[3]==TRUE & usr_spec[4]==TRUE){
#         nvar0 <-dim(var)[1]
#         var <- cbind(var,c(1:nvar0))
#         var <- var[var[,2]!=0,]
#         nvar <- dim(var)[1]
#         if (nvar!=0){
#             var_dsc <- if (nvar0==1){c("r.userdef")} else {paste("r.userdef",var[,3],sep="_")}
#             colnames(var) <- c("","Coefficients")
#             # rownames(var) <- var_dsc
#             fvar <- var[2]
#             rownames(fvar) <- sprintf("r.%s", rownames(fvar))
#             fvar <- cbind(fout, NA)
#             colnames(fvar)[ncol(fvar)] <- "Pr(>|t|)"
#         }
#     }
#     rbind(arima_coef, reg_coef, fout, fvar)
# }
arima_test <- function(x){
    residuals <- x$regarima$residuals
    residuals <- na.omit(ts.union(residuals, lag(residuals, -12)))
    
    tests <- x$regarima$residuals.stat[["tests"]]
    
    dh <- dh <- doornik_hansen(n = x$regarima$loglik["neffectiveobs", ],		
                               skewness = tests["skewness", "Statistic"],		
                               kurtosis = tests["kurtosis", "Statistic"])	
    nruns <- x$user_defined$preprocessing.residuals.nrun[2]
    lruns <- x$user_defined$preprocessing.residuals.lruns[2]
    if (is.null(nruns)) {
        nruns <- NA
    }
    if (is.null(lruns)) {
        lruns <- NA
    }
    result <- c(tests[c("skewness","kurtosis", "mean"), "P.value"],
                 dh[2], tests[c("ljung box",
                                "ljung box (squared residuals)",
                                "ljung box (residuals at seasonal lags)"), "P.value"],
                cor(residuals[,1], residuals[,2]), 
                nruns,
                lruns
    )
    result <- as.data.frame(matrix(result, nrow = 1),row.names = NULL)
    colnames(result) <- c("skewness", "kurtosis", "mean", "normality", "lb", "lb2",
                       "seaslb","autocorr12", "nruns", "lruns")
    result
}
# 
doornik_hansen <- function(n, skewness, kurtosis){
    beta <- (3 * (n^2 + 27*n - 70) * (n + 1) * (n+3)) / ((n-2)*(n+5)*(n+7)*(n+9))
    omega2 <- -1 + sqrt(2*(beta - 1))
    y <- skewness * sqrt(((omega2 - 1) * (n + 1) * (n+3))/(12*(n-2)))
    delta <- 1/sqrt(log(sqrt(omega2)))
    z1 <- delta * log(y+sqrt(y^2+1))

    delta <- (n-3)*(n+1)*(n^2+15*n-4)
    a <- (n-2)*(n+5)*(n+7)*(n^2+27*n-70)/(6*delta)
    c <- (n-7)*(n+5)*(n+7)*(n^2+2*n-5)/(6*delta)
    k <- (n+5)*(n+7)*(n^3+37*n^2+11*n-313)/(12*delta)
    alpha <- a+ c * skewness^2
    chi <- 2 * k *(kurtosis - 1 - skewness^2)
    z2 <- sqrt(9*alpha) *(1/(9*alpha) + (chi/(2*alpha))^(1/3)-1)

    DH <- z1^2+z2^2
    p_val <- 1 - pchisq(DH, 2)
    result <- c(DH, p_val)
    names(result) <- c("Estimate", "Pr(>|t|)")
    result
}

get_MH <- function(x, regarima_coefs){
    if(x$regarima$model$spec_rslt[, "Easter"]){
        MH_type <- grep("^Easter \\[.*\\]$", rownames(regarima_coefs),value = TRUE)
        if(length(MH_type) == 0){
            MH <- data.frame("", NA, NA,
                             stringsAsFactors = FALSE) 
        }else{
            MH <- data.frame(MH_type,
                             regarima_coefs[MH_type, c("Estimate", "Pr(>|t|)"), drop = FALSE],
                             stringsAsFactors = FALSE)
        }
    }else{
        MH <- data.frame("", NA, NA,
                         stringsAsFactors = FALSE) 
    }
    names(MH) <- c("MovingHoliday", "MH1", "PvalMH1")
    MH
}
get_cste <- function(x){
    if(x$regarima$model$spec_rslt[, "Mean"]){
        cste <- regarima_coefs["Mean", c("Estimate", "Pr(>|t|)"), drop = FALSE]
    }else{
        cste <- data.frame(Estimate = NA, `Pr(>|t|)` = NA)
        names(cste) <- c("Estimate", "Pr(>|t|)")
    }
    names(cste) <- c("Cste", "Cste (Pvalue)")
    cste
}
get_LY <- function(x, regarima_coefs){
    if(x$regarima$model$spec_rslt[, "Leap year"]){
        if("Length of period" %in% rownames(regarima_coefs)){
            LY <- data.frame("Length of period",
                             regarima_coefs["Length of period", c("Estimate", "Pr(>|t|)"), drop = FALSE],
                             stringsAsFactors = FALSE)
        }else{
            if("Leap year" %in% rownames(regarima_coefs)){
                LY <- data.frame("Leap year", regarima_coefs["Leap year", c("Estimate", "Pr(>|t|)"), drop = FALSE],
                                 stringsAsFactors = FALSE)
            }else{
                LY <- data.frame("", NA, NA,
                                 stringsAsFactors = FALSE)
            }
        }
    }else{
        LY <- data.frame("", NA, NA,
                         stringsAsFactors = FALSE) 
    }
    colnames(LY) <- c("LY","LeapYear", "PvalLeapYear")
    LY
}

get_TD <- function(x, regarima_coefs){
    td_reg <- grep("(^Monday$)|(^Tuesday$)|(^Wednesday$)|(^Thursday$)|(^Friday$)|(^Saturday$)|(^Week days$)",
                   rownames(regarima_coefs),value = TRUE)
    
    if(length(td_reg) > 0){
        for(i in 1:length(td_reg)){
            assign(sprintf("TD%i", i),
                   data.frame(td_reg[i],
                              regarima_coefs[td_reg[i], c("Estimate", "Pr(>|t|)"), drop = FALSE],
                              stringsAsFactors = FALSE))
        }
    }
    if(length(td_reg) < 7){
        for(i in (1+length(td_reg)):7){
            assign(sprintf("TD%i", i),
                   data.frame("", NA, NA,
                              stringsAsFactors = FALSE))
        }
    }
    td_joint_ftest <- NA
    result <- data.frame(TD1, TD2, TD3, TD4, TD5, TD6, TD7,
                         td_joint_ftest,
                         stringsAsFactors = FALSE)
    colnames(result) <- c("TradingDay1", "TD1", "TD1(Pvalue)", "TradingDay2", "TD2", 
                          "TD2(Pvalue)", "TradingDay3", "TD3", "TD3(Pvalue)", "TradingDay4", 
                          "TD4", "TD4(Pvalue)", "TradingDay5", "TD5", "TD5(Pvalue)", "TradingDay6", 
                          "TD6", "TD6(Pvalue)", "TradingDay7", "TD7", "TD7(Pvalue)",
                          "TDJointFTest")
    result
}

get_residual_tests <- function(x){
    variance_decomposition <- t(x$diagnostics$variance_decomposition)
    if(class(x)[2] == "X13"){
        result_combined_test <- x$diagnostics$combined_test$combined_seasonality_test
        combined_tests <- t(x$diagnostics$combined_test$tests_for_stable_seasonality[,"P.value", drop = FALSE])
    }else{
        result_combined_test <- ""
        combined_tests <- data.frame(NA, NA, NA)
    }
    
    res_tests <- t(x$diagnostics$residuals_test[c("qs test on sa", "f-test on sa (seasonal dummies)", "qs test on i", 
                                                  "f-test on i (seasonal dummies)", "f-test on sa (td)", "f-test on i (td)"),
                                                "P.value", drop = FALSE])
    res_tests <- data.frame(variance_decomposition,
                            result_combined_test, combined_tests, res_tests, stringsAsFactors = FALSE)
    colnames(res_tests) <- c(colnames(variance_decomposition), "Combined test",
                             "Kruskall-Wallis", "Stability test","Evolutive test",
                             "ResS_SA_QS", "ResS_SA_F", "ResS_I_QS", "ResS_I_F",
                             "ResTD_SA_F", "ResTD_I_F")
    res_tests
}
