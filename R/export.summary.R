#' Data Summary
#'
#' Retorna una taula resum d'una variable numerica en funció de varies categoriques i ho exporta en format .csv
#' @param data El data-set que conte les dades
#' @param varname La variable numerica de la que obtenir la taula resum
#' @param groupnames La o les variables categoriques
#' @param ruta Ruta on s'exportara el document .csv
#' @return Una taula resum
#' @examples
#' data.summary(data_frame, "num_var", c("cat_var1", "cat_var2", "..."), "C:\\Users\\Sergi\\Desktop\\file_name.csv");
#' @export
export.summary <- function(data, varname, groupnames = NULL, ruta){
  require(plyr)

  export_func <- function(x, col){
    c(n = length(x[[col]]),
      mean.SE = paste(round(mean(x[[col]], na.rm=TRUE), 2),
                      "±",
                      round(sd(x[[col]], na.rm=TRUE) / sqrt(length(x[[col]])), 2)),
      mean.SD = paste(round(mean(x[[col]], na.rm=TRUE), 2),
                      "±",
                      round(sd(x[[col]], na.rm=TRUE), 2)),
      Min = round(min(x[[col]], na.rm = TRUE), 2),
      Max = round(max(x[[col]], na.rm = TRUE), 2))
  }

  data_export <- ddply(data, groupnames, .fun=export_func,
                       varname)

  write.csv(data_export, file = NULL, row.names = FALSE, fileEncoding = "latin1")
}
