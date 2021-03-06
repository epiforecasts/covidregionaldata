% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{make_github_workflow}
\alias{make_github_workflow}
\title{Create github action for a given source}
\usage{
make_github_workflow(
  source,
  workflow_path = paste0(".github/workflows/", source, ".yaml"),
  cron = "36 12 * * *"
)
}
\arguments{
\item{source}{character_array The name of the class to create the workflow
for.}

\item{workflow_path}{character_array The path to where the workflow file
should be saved. Defaults to '.github/workflows/'}

\item{cron}{character_array the cron time to run the tests, defaults to
36 12 * * *, following the minute, hour, day(month), month and day(week)
format.}
}
\description{
Makes a github workflow yaml file for a given source to be used
as an action to check the data as a github action.
}
\concept{utility}
