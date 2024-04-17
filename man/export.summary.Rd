% Please edit documentation in R/Data_summary.R
\name{export.summary}
\alias{export.summary}
\title{Export Summary}
\usage{
export.summary(data, varname, groupnames = NULL, ruta)
}
\arguments{
\item{data}{El data-set que conte les dades}

\item{varname}{La variable numerica de la que obtenir la taula resum}

\item{groupnames}{La o les variables categoriques}

\item{ruta}{La ruta on es guardara el document .csv}
}
\value{
Una taula resum en forma de .csv
}
\description{
Retorna una taula resum d'una variable numerica en funció de varies categoriques amb format llest per publicar
}
\examples{
data.summary(data_frame, "num_var", c("cat_var1", "cat_var2"), "C:\\Users\\Sergi\\Desktop\\file_name.csv");
}
