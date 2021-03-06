% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coef.R
\name{coef.galasso}
\alias{coef.galasso}
\title{Extract Coefficients From a 'galasso' Object}
\usage{
\method{coef}{galasso}(object, lambda, ...)
}
\arguments{
\item{object}{A 'galasso' fit}

\item{lambda}{Chosen value of lambda. Must be between 'min(lambda)' and
'max(lambda)'. Default is 'lambda.min'}

\item{...}{Additional unused arguments}
}
\value{
A numeric vector containing the coefficients from running
    \code{galasso} on \code{lambda}.
}
\description{
Extract Coefficients From a 'galasso' Object
}
