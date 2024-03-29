% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{co_ab_fr}
\alias{co_ab_fr}
\title{co_ab_fr}
\usage{
co_ab_fr(dataset, col_name_1, col_name_2)
}
\arguments{
\item{dataset}{the dataset for which you wish to retrieve comparative
absolute frequency.}

\item{col_name_1}{the first column name for which you wish to retrieve
comparative absolute frequency.}

\item{col_name_2}{the second column name for which you wish to retrieve
comparative absolute frequency.}
}
\value{
returns the comparative of the absolute frequency for the a variable
in different moments.
}
\description{
co_ab_fr
}
\examples{
\dontrun{
co_ab_fr(dataset = soccer,
        col_name_1 = result_19_20,
        col_name_2 = result_20_21)}
}
