% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encryption.R
\name{encrypt_file}
\alias{encrypt_file}
\alias{read_encrypted}
\alias{write_encrypted}
\alias{hex_to_raw}
\alias{raw_to_hex}
\title{File Encryption and Decryption (AES-256-CBC)}
\usage{
encrypt_file(input_file, output_file, key)

read_encrypted(input_file, key)

write_encrypted(data, output_file = "encrypted.enc", key = NULL, quiet = FALSE)

hex_to_raw(x)

raw_to_hex(x)
}
\arguments{
\item{input_file}{Character string. The path to the file to be encrypted.}

\item{output_file}{Character string. The path where the encrypted
file will be saved.}

\item{key}{A raw vector of 32 bytes (256 bits) to be used as the encryption key.}

\item{data}{List, vector or any json-able object to write into encrypted file.}

\item{quiet}{Boolean. Keep quiet? If not, informative messages will be shown.}

\item{x}{Character string or hex vector to convert.}
}
\value{
No return value, called for side effects (writes to \code{output_file}).
}
\description{
This set of functions provides utilities for encrypting
and decrypting files using AES-256 in CBC mode, primarily for
handling key-value pair "secrets" files.
}
\details{
The encryption process pads the data, generates a random
Initialization Vector (IV), and encrypts the content. The IV is
prepended to the encrypted data in the output file.
Decryption involves extracting the IV, decrypting, and unpadding the data.
A specialized decryption function \code{read_encrypted} is provided
to directly parse decrypted content (assumed to be \code{key=value} pairs)
into a named R list. If no "=" sign found in the first line, text will be
imported as character vector.
}
\examples{
\dontrun{
# Create dummy files for demonstration
temp_input <- tempfile(fileext = ".txt")
temp_output <- tempfile(fileext = ".enc")
writeLines("library=lares \nversion=5.3.0 \n pass=123", temp_input)
readLines(temp_input)

# Generate a random 32-byte key (and save it in a secure place)
raw_key <- openssl::rand_bytes(32)
# You can convert from hex to raw and viceversa
raw_to_hex(raw_key)
hex_to_raw(raw_to_hex(raw_key))

# Encrypt the file
encrypt_file(temp_input, temp_output, raw_key)

# Import the data from encrypted file to list or vector
secrets <- read_encrypted(temp_output, raw_key)
unlist(secrets)

# Example using a string (JSON in this case)
writeLines(jsonlite::toJSON(list(a = 1, list(b = 2, c = 1:3))), temp_input)
encrypt_file(temp_input, temp_output, raw_key)
read_encrypted(temp_output, raw_key)

# Example writing a file from a list or vector directly from R
my_secrets <- list(
  api_key = "some_secret_api_key_123",
  username = "data_analyst",
  server = "production.server.com"
)
raw_key <- write_encrypted(my_secrets, temp_output, quiet = FALSE)
fromJSON(read_encrypted(temp_output, key = raw_key))
}
}
\seealso{
Other Credentials: 
\code{\link{db_download}()},
\code{\link{db_upload}()},
\code{\link{get_credentials}()},
\code{\link{get_tweets}()},
\code{\link{mail_send}()},
\code{\link{queryDB}()},
\code{\link{queryGA}()},
\code{\link{slackSend}()},
\code{\link{stocks_file}()}
}
\concept{Credentials}
\keyword{Decryption}
\keyword{Encryption}
\keyword{Security}
