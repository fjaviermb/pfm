% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phad-c32.R
\name{minMaxScaler}
\alias{minMaxScaler}
\title{Do MinMax scaler/normalizacion}
\usage{
minMaxScaler(x, minVal = 1, maxVal = 100)
}
\arguments{
\item{x:}{data. numeric vector of values to be scaled}

\item{minVal:}{minimum value}

\item{maxVal:}{maximum value}
}
\value{
scaled vector using minMax. Based on: https://gist.github.com/swayson/b5a6d3cd796ab1d08df1
}
\description{
Do MinMax scaler/normalizacion
}
