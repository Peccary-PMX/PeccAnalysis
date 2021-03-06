% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peccary_NCA.R
\name{peccary_NCA}
\alias{peccary_NCA}
\title{Basic Non Compartimental Analysis}
\usage{
peccary_NCA(
  dataset,
  timecol,
  obscol,
  ...,
  evidcol = NULL,
  BLQcol = NULL,
  auc_0_X = 0,
  filter = "",
  keepcov = T,
  plotQC = F,
  outputExpr = F,
  IVbackextr = F,
  add_Obs0_Time0 = F,
  nIVback = 2
)
}
\arguments{
\item{dataset}{Dataset to analyze}

\item{timecol}{The name (unquoted) of the time column. Mandatory.}

\item{obscol}{The name  (unquoted) of the observation column. Mandatory.}

\item{...}{All the names (unquoted) of the grouping column (ID, YTYPE, nAdmin, any covariate....).}

\item{evidcol}{The name  (unquoted) of the evid column. Optional. Only line with evidcol = 0 will be kept}

\item{BLQcol}{The name  (unquoted) of the evid column. Optional. BLQ values are replaced by 0.}

\item{auc_0_X}{Numeric input. Compute AUC from time 0 to time auc_0_X}

\item{filter}{Optional filter to be applied on the dataset prior to NCA computation.}

\item{keepcov}{Boolean. If true (default), use \code{\link{pecc_search_cov}} to keep automatically all covariates}

\item{plotQC}{Boolean. If T, create plots to control the output  (default = F)}

\item{outputExpr}{Boolean. If F (default) returns directly NCA results. If T, return R code instead.}

\item{IVbackextr}{Boolean. If T, compute C0 based on the first nIVback points.}

\item{add_Obs0_Time0}{Boolean. If T, add observation 0 at times 0 for all patients. Usefull in case of non intravscular administration(default = F)}

\item{nIVback}{Numeric input. Number of point used for the backextrapolation. Default 2 as in PKAnalix}
}
\description{
\code{peccary_NCA} compute basic NCA analysis (no extrapolation)
}
\examples{
# To get the Peccary-independant code, add outputExpr = T
}
\author{
Thibaud Derippe (\email{Thibaud.Derippe@gmail.com})
}
