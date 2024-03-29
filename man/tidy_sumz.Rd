% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_sumz.R
\name{tidy_sumz}
\alias{tidy_sumz}
\title{Run metap::sumz to meta two p values with weights stored in a tibble}
\usage{
tidy_sumz(dt, pval1, pval2, wt1, wt2)
}
\arguments{
\item{dt}{input data table contains 4 columns representing two p values and two weights}

\item{pval1}{the column name of 1st p value}

\item{pval2}{the column name of 2nd p value}

\item{wt1}{the column name of weight of 1st p value}

\item{wt2}{the column name of weight of 2nd p value}
}
\value{
It returns the original input data table with one additional column \strong{sumz_pval} representing meta-ed p value from metap::sumz
}
\description{
Run metap::sumz to meta two p values with weights stored in a tibble
}
\examples{
dt <- tibble::tribble(
~p1, ~p2, ~w1, ~w2,
0.01, 0.0003, 1,1,
0.0003, 0.01, 10,1,
0.5, 0.0003, 2,5)

tidy_sumz(dt, p1, p2, w1, w2)


}
