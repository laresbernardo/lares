% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/audio.R
\name{get_mp3}
\alias{get_mp3}
\title{Download MP3 from URL}
\usage{
get_mp3(url, mp3 = TRUE, params = "", quiet = FALSE)
}
\arguments{
\item{url}{Characeter. YouTube URL}

\item{mp3}{Boolean. Add mp3 parameters?}

\item{params}{Character. Additional parameters}

\item{quiet}{Boolean. Keep quiet (or show query)?}
}
\description{
This function downloads a YouTube video or Soundcloud or any other 
platform supported by the youtube-dl library, and converts it into a 
high quality MP3 file.
}
\section{youtube-dl}{

More info from the original developers and its code: 
\href{https://github.com/ytdl-org/youtube-dl/}{youtube-dl's Github}
}

\examples{
\dontrun{
 # Download video from YouTube and convert to MP3
 get_mp3("https://www.youtube.com/watch?v=lrlKcCdVw9Q")
}
}
\seealso{
Other Scrapper: 
\code{\link{geoStratum}()},
\code{\link{holidays}()},
\code{\link{ip_country}()},
\code{\link{readGS}()},
\code{\link{trendsRelated}()}
}
\concept{Audio}
\concept{Scrapper}