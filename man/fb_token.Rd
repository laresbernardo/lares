% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/facebook.R
\name{fb_token}
\alias{fb_token}
\title{Facebook's Long Life User Token}
\usage{
fb_token(app_id, app_secret, token, api_version = "v10.0")
}
\arguments{
\item{app_id, app_secret}{Character. Application ID and Secret.}

\item{token}{Character. User token, created with
\href{https://developers.facebook.com/tools/explorer}{API Graph}
or with this same \code{fb_token()}'s token.}

\item{api_version}{Character. Facebook API version}
}
\value{
Character. String with token requested.
}
\description{
Using a 1-hour generic user token you can generate a 60 day token.
You will need to have an App ID and App secret, and a valid token.
Generate a new valid User Token with the
\href{https://developers.facebook.com/tools/explorer}{API Graph}.
}
\details{
More info: \href{https://developers.facebook.com/docs/facebook-login/access-tokens/refreshing/}{Long-Lived Access Tokens}
}
\seealso{
Other API: 
\code{\link{bring_api}()},
\code{\link{fb_accounts}()},
\code{\link{fb_ads}()},
\code{\link{fb_creatives}()},
\code{\link{fb_insights}()},
\code{\link{fb_posts}()},
\code{\link{fb_post}()},
\code{\link{fb_process}()},
\code{\link{fb_rf}()},
\code{\link{li_auth}()},
\code{\link{li_profile}()},
\code{\link{queryGA}()},
\code{\link{slackSend}()}

Other Facebook: 
\code{\link{fb_accounts}()},
\code{\link{fb_ads}()},
\code{\link{fb_creatives}()},
\code{\link{fb_insights}()},
\code{\link{fb_posts}()},
\code{\link{fb_post}()},
\code{\link{fb_process}()},
\code{\link{fb_rf}()}
}
\concept{API}
\concept{Facebook}
