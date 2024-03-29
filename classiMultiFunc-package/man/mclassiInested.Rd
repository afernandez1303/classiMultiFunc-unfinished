% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mclassiNestedCV.R
\name{mclassiInested}
\alias{mclassiInested}
\title{mclassiNestedCV}
\usage{
mclassiInested(
  cl,
  task,
  id,
  knn,
  h = NULL,
  par.vals,
  par.vals.ensemble = NULL,
  subset,
  cv = 5,
  set.seed = NULL,
  xreg = NULL
)
}
\arguments{
\item{cl:}{classifier}

\item{task:}{define a task with makeClassifTask}

\item{id:}{name of semi-metrics}

\item{knn:}{value for nearest neighbor}

\item{h:}{value for kernel-based method}

\item{par.vals:}{hyperparameters}

\item{par.vals.ensemble:}{hyperparameters for either random forest or gradient boosting}

\item{subset:}{ith inner sample}

\item{cv:}{splits in the k-fold cross-validation in the inner loop. By default, 5-fold cross-validation is considered}

\item{set.seed:}{seed for the splits in the inner-loop}

\item{xreg:}{if covariates are further considered}
}
\description{
mclassiInested
}
