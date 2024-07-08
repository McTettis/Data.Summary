#' Data Summary
#'
#' Retorna una taula resum d'una variable numerica en funció de varies categoriques
#' @param data El data-set que conte les dades
#' @param varname La variable numerica de la que obtenir la taula resum
#' @param groupnames La o les variables categoriques
#' @return Una taula resum
#' @examples
#' data.summary(data_frame, "num_var", c("cat_var1", "cat_var2", "..."));
#' @export

data.summary <- function(data, varname, groupnames = NULL){
  require(plyr)
  summary_func <- function(x, col){
    c(n = sum(!is.na(x[[col]]) & is.numeric(x[[col]])),
      mean = mean(x[[col]], na.rm = TRUE),
      SD = sd(x[[col]], na.rm=TRUE),
      SE = sd(x[[col]], na.rm=TRUE) / sqrt(length(x[[col]])),
      Min = min(x[[col]], na.rm = TRUE),
      Max = max(x[[col]], na.rm = TRUE))
  }
  data_sum <- ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
