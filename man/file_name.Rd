% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other_functions.R
\name{file_name}
\alias{file_name}
\alias{file_type}
\title{Extract file raw name and type from file names}
\usage{
file_name(filepath)

file_type(filepath)
}
\arguments{
\item{filepath}{Character vector. File path(s) to get file raw names
without extension nor path OR extension without path nor raw name.}
}
\description{
Extract file raw name and type from file names

Get file extensions without file names
}
\examples{
file_name("file.aux")
file_name("temp/file.R")
file_name("/temp/temp3/music.mp3")
file_type("file.aux")
file_type("temp/file.R")
file_type("/temp/temp3/music.mp3")
}
