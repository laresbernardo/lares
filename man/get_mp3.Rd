% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/audio.R
\name{get_mp3}
\alias{get_mp3}
\title{Download MP3 from URL}
\usage{
get_mp3(
  id,
  mp3 = TRUE,
  params = "",
  start_time = 0,
  end_time = NA,
  overwrite = TRUE,
  info = TRUE,
  cover = FALSE,
  quiet = FALSE
)
}
\arguments{
\item{id}{Character. YouTube URL or ID to search for.}

\item{mp3}{Boolean. Add mp3 optimal parameters?}

\item{params}{Character. Additional parameters.}

\item{start_time, end_time}{Numeric. Start and end time
to trim the audio output in seconds.}

\item{overwrite}{Boolean. Overwrite original file?}

\item{info}{Boolean. Import and return metadata?}

\item{cover}{Boolean. Google Search its squared cover?}

\item{quiet}{Boolean. Keep quiet? If not, print messages.}
}
\value{
(Invisible) list with id's meta-data.
}
\description{
This function downloads YouTube videos or Soundcloud or any other
platform supported by the youtube-dl library, and converts them into
high quality MP3 files. The URL can be for a single video or a whole
playlist. It also returns metadata into an (invisible) list.
}
\section{youtube-dl}{

More info from the original developers and its code:
\href{https://github.com/ytdl-org/youtube-dl/}{youtube-dl's Github}
}

\examples{
# You must have "youtube-dl" installed in your OS:
\dontrun{
# Download video from YouTube and convert to MP3
get_mp3("https://www.youtube.com/watch?v=lrlKcCdVw9Q")
# OR simply
get_mp3("lrlKcCdVw9Q")
}
}
\seealso{
Other Scrapper: 
\code{\link{filesGD}()},
\code{\link{holidays}()},
\code{\link{ip_data}()},
\code{\link{readGS}()},
\code{\link{splot_etf}()},
\code{\link{stocks_hist}()},
\code{\link{stocks_quote}()},
\code{\link{trendsRelated}()},
\code{\link{writeGS}()}

Other Audio: 
\code{\link{trim_mp3}()}
}
\concept{Audio}
\concept{Scrapper}
