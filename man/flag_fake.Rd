% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/response_files.R
\name{flag_fake}
\alias{flag_fake}
\title{Flag Subdirectory as Having Simulated Data}
\usage{
flag_fake(path)
}
\arguments{
\item{path}{Path to subdirectory.}
}
\value{
No return value, called only for its side effect.
}
\description{
Flag Subdirectory as Having Simulated Data
}
\details{
This function tags data in a subdirectory as simulated so
  that it is not confused with genuine data. When an analysis
  report is compiled against data from that subdirectory, the
  report will contain a warning that the data is not real.
}
