% Please edit documentation in R/Data_summary.R
\name{data.summary}
\alias{data.summary}
\title{Data Summary}
\usage{
data.summary(data, varname, groupnames = NULL)
}
\arguments{
\item{data}{El data-set que conte les dades}

\item{varname}{La variable numerica de la que obtenir la taula resum}

\item{groupnames}{La o les variables categoriques}
}
\value{
Una taula resum
}
\description{
Retorna una taula resum d'una variable numerica en funció de varies categoriques
}
\examples{
data.summary(data_frame, "num_var", c("cat_var1", "cat_var2"));
}
