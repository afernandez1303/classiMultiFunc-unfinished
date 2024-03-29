% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mclassiEnsemble.R
\name{mclassiEnsembleLearner}
\alias{mclassiEnsembleLearner}
\title{mclassiEnsemble}
\usage{
mclassiEnsembleLearner(cl, task, id, knn, h = NULL, par.vals, subset)
}
\arguments{
\item{cl:}{classifier}

\item{task:}{define a task with makeClassifTask}

\item{id:}{name of semi-metrics}

\item{knn:}{value for nearest neighbor}

\item{h:}{Default NULL. If not, bandwidth parameter for the kernel method}

\item{par.vals:}{hyperparameters including metric name, mdist, kernel type, predict.type, set.seed (for ties)}

\item{subset:}{ith inner sample}
}
\description{
mclassiEnsemble
}
