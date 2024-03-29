% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{co_rl_fr}
\alias{co_rl_fr}
\title{co_rl_fr}
\usage{
co_rl_fr(dataset, col_name_1, col_name_2)
}
\arguments{
\item{dataset}{the dataset for which you wish to retrieve comparative
relative frequency.}

\item{col_name_1}{the first column name for which you wish to retrieve
comparative relative frequency.}

\item{col_name_2}{the second column name for which you wish to retrieve
comparative relative frequency.}
}
\value{
returns the comparative of the relative frequency for the a variable
in different moments.
}
\description{
co_rl_fr
}
\examples{
\dontrun{
co_rl_fr(dataset = soccer,
        col_name_1 = result_19_20,
        col_name_2 = result_20_21)}
}
