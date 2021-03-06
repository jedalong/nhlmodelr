% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/players.R
\name{players}
\alias{players}
\title{Player shooting analysis}
\usage{
players(model, shots)
}
\arguments{
\item{model}{a dataframe, specifically, ouput from the function \code{NHLmodel}.}

\item{shots}{the raw data shots file that was used as input into the function \code{shots2model}.}

\item{games}{the raw game data associated with \code{shots} (see details).}
}
\value{
The function returns a dataframe with spatially adjusted team shooting statistics.
}
\description{
Calculate spatially adjusted player-based shooting  statistics using spatially explicit model of NHL scoring probability.
}
\details{
Will be expanded later on.
}
