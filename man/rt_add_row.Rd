% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrappers.R
\name{rt_add_row}
\alias{rt_add_row}
\title{Add rows to a reactibble}
\usage{
rt_add_row(.data, ..., .before = NULL, .after = NULL)
}
\arguments{
\item{.data}{Data frame to append to.}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}>
Name-value pairs, passed on to \code{\link[tibble:tibble]{tibble()}}. Values can be defined
only for columns that already exist in \code{.data} and unset columns will get an
\code{NA} value.}

\item{.before}{One-based row index where to add the new rows,
default: after last row.}

\item{.after}{One-based row index where to add the new rows,
default: after last row.}
}
\description{
Counterpart of \code{tibble::add_row} that works efficiently on \emph{"reactibble"}
objects. Beware of using \code{add_row()} instead as it would return an out of sync \code{reactibble}
}
