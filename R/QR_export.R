#' #' Export QA_matrix objects to an Excel file
#' #'
#' #' Function to export a quality assessment matrix  (\code{QA_matrix}) to an Excel file.
#' #'
#' #' @param x a \code{QA_matrix} object.
#' #' @param layout QA_matrix' component to export. By default \code{layout = "all"}: the modalities matrix
#' #' (\code{"modalities"}) and the values matrix (\code{"values"}) are export. To add 
#' #' the additional variables of the values matrix to the modalities matrix, use \code{layout = "combined"}.
#' #' @param create boolean indicating whether to create a new Excel file or not (\code{TRUE} by default).
#' #' @param clear_sheet boolean indicating whether to clear the Excel's sheets before exporting  (\code{TRUE} by default); 
#' #' usefull when the Excel file already exist.
#' #' @param auto_format boolean indicating whether to format the ouptut or not (\code{TRUE} by default).
#' #' @param file_name argument optionnel indiquant chemin vers le fichier à exporter. Si no spécifié
#' #' alors un fichier *export.xls* est créé dans le working directory.
#' #' @param sheet_names nom des feuilles Excel en sortie. Si non spécifié alors le nom correspond à la composante exportée.
#' #' Si le paramètre est spécifié alors les éventuelles feuilles contenant ces noms sont supprimées.
#' #' @param ... autres paramètres non utilisés.
#' #' @family QA_matrix functions
#' #' @export
#' export_xlsx.QA_matrix <- function(x, layout = c("all","modalities", "values", "combined"),
#'                                   create = TRUE, clear_sheet = TRUE, auto_format = TRUE,
#'                                   file_name, sheet_names, ...) {
#'     layout <- match.arg(layout)
#'     file_name <- ifelse(missing(file_name), "export.xls", file_name)
#'     wb <- XLConnect::loadWorkbook(filename = file_name, create = create)
#'     sheets <- switch(layout, all = c("modalities", "values"),
#'                      combined = "values",
#'                      layout)
#'     exp_data <- switch(layout, combined = {
#'         data_v <- x[["values"]]
#'         data_m <- x[["modalities"]]
#'         joint_names <- colnames(data_m)[colnames(data_m) %in% colnames(data_v)]
#'         data_v[, joint_names] <- data_m[, joint_names]
#'         list(values = data_v)
#'     }, x)
#'     XLConnect::createSheet(wb, sheets)
#'     if (clear_sheet) {
#'         XLConnect::clearSheet(wb, sheets)
#'     }
#' 
#'     XLConnect::setStyleAction(wb,
#'                               XLConnect::XLC$STYLE_ACTION.DATA_FORMAT_ONLY)
#'     if (auto_format) {
#'         XLConnect::setDataFormatForType(wb,
#'                                         type = XLConnect::XLC$DATA_TYPE.NUMERIC,
#'                                         format = "0.000")
#' 
#'         cs <- XLConnect::createCellStyle(wb)
#'         XLConnect::setBorder(cs, side = "all",
#'                              type = XLConnect::XLC$BORDER.THIN,
#'                              color = XLConnect::XLC$COLOR.BLACK)
#'     }
#'     for (s in sheets) {
#'         data <- exp_data[[s]]
#'         XLConnect::writeWorksheet(wb, data = data, sheet = s, header = TRUE)
#'         if (auto_format) {
#'             XLConnect::setCellStyle(wb, sheet = s, row = 1:(nrow(data) + 1),
#'                                     col = 1:ncol(data), cellstyle = cs)
#'             XLConnect::setCellStyle(wb,
#'                                     formula = paste0(s, "!", "$A$1:",
#'                                                      XLConnect::idx2cref(c(nrow(data) + 1, ncol(data)))),
#'                                     cellstyle = cs)
#'             XLConnect::setColumnWidth(wb, sheet = s, column = 1:(ncol(data)),
#'                                       width = -1)
#'         }
#'     }
#'     if(!missing(sheet_names) && length(sheet_names) == length(sheets)){
#'         XLConnect::removeSheet(wb, sheet = sheet_names)
#'         XLConnect::renameSheet(wb, sheets, sheet_names)
#'     }
#'     XLConnect::saveWorkbook(wb)
#'     return(invisible(wb))
#' }
#' #' @export
#' export_xlsx <- function(x, ...){
#'     UseMethod("export_xlsx", x)
#' }
#' #' @export
#' export_xlsx.default <- function(x, ...){
#'     stop("Il faut un objet de type QA_matrix ou mQA_matrix")
#' }
#' 
#' #' Export mQA_matrix objects to an Excel file
#' #'
#' #' Function to export a list of quality assessment matrix  (\code{mQA_matrix}) to an Excel file.
#' #'
#' #' @param x a \code{mQA_matrix} object.
#' #' @param export_dir folder to export the result.
#' #' @param layout_file export parameter. By default (\code{layout_file = "ByComponent"}) an Excel by component
#' #' of the quality assessment matrix (modalities and/or values matrix) whose each sheet correspond to a QA_matrix. \code{layout_file = "ByQAMatrix"} to export 
#' #' one file per QA_matrix.
#' #' @param file_extension the files' extension (\code{".xls"} or \code{".xlsx"}).
#' #' @param ... other parameters of the \code{\link{export_xlsx.QA_matrix}} function.
#' #' @inheritParams export_xlsx.QA_matrix
#' #' @family QA_matrix functions
#' #' @export
#' export_xlsx.mQA_matrix <- function(x, export_dir = "./",
#'                                    layout_file = c("ByComponent","ByQAMatrix"),
#'                                    file_extension = c(".xls",".xlsx"),
#'                                    layout = c("all","modalities", "values", "combined"),
#'                                    ...){
#'     if(length(x) == 0)
#'         return(invisible(x))
#'     file_extension <- match.arg(file_extension)
#'     layout_file <- match.arg(layout_file)
#'     layout <- match.arg(layout)
#' 
#'     QA_matrix_names <- names(x)
#' 
#'     if(is.null(QA_matrix_names)){
#'         QA_matrix_names <- paste0("QA_",1:length(x))
#'     }else{
#'         QA_matrix_names[is.na(QA_matrix_names)] <- ""
#'         if(!is.na(match("", QA_matrix_names)))
#'             QA_matrix_names[match("", QA_matrix_names)] <- paste0("QA_",
#'                                                                   match("", QA_matrix_names))
#'     }
#' 
#' 
#'     if(layout_file == "ByQAMatrix"){
#'         #On exporte un fichier par bilan :
#'         files_name <- normalizePath(file.path(export_dir,
#'                                              paste0(QA_matrix_names, file_extension)),
#'             mustWork = FALSE)
#'         for(i in 1:length(x)){
#'             export_xlsx(x[[i]],layout = layout, file_name = files_name[i], ...)
#'         }
#'     }else{
#'         #On rassemble les bilans dans un fichier par composante
#'         files_name <- switch(layout,
#'                              all = c("modalities", "values"),
#'                              combined = "values",
#'                              layout)
#'         final_layout <- switch(layout,
#'                          all = c("modalities", "values"),
#'                          layout)
#' 
#'         files = normalizePath(
#'             file.path(export_dir,paste0(files_name, file_extension)),
#'             mustWork = FALSE)
#'         for(i in 1:length(x)){
#'             #Indice sur les QA_matrix
#'             for(j in 1:length(final_layout)){
#'                 #Indice sur les composantes
#'                 export_xlsx(x[[i]],layout = final_layout[j], file_name = files[j],
#'                             sheet_names = QA_matrix_names[i],
#'                             ...)
#'             }
#'         }
#' 
#'     }
#' 
#'     return(invisible(x))
#' }
#' 
