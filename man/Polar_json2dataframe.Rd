% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polar_utils.R
\name{Polar_json2dataframe}
\alias{Polar_json2dataframe}
\title{Polar json to dataframe}
\usage{
Polar_json2dataframe(file = NA)
}
\arguments{
\item{file}{a string indicating the path to a Polar .json file}
}
\value{
a dataframe with all relevant data processed
}
\description{
Takes a Polar .json file as input and returns a dataframe as output. The following four types of Polar data files are accepted as input: 1) heart rate (files starting with "247ohr-..."); 2) activity (files starting with "activity-..."); 3) temperature (files starting with "generic-period-..."); 4) PPI (files starting with "ppi_samples_...")
}
