# File Encryption and Decryption (AES-256-CBC)

This set of functions provides utilities for encrypting and decrypting
files using AES-256 in CBC mode, primarily for handling key-value pair
"secrets" files.

## Usage

``` r
encrypt_file(input_file, output_file, key)

read_encrypted(input_file, key)

write_encrypted(data, output_file = "encrypted.enc", key = NULL, quiet = FALSE)

hex_to_raw(x)

raw_to_hex(x)
```

## Arguments

- input_file:

  Character string. The path to the file to be encrypted.

- output_file:

  Character string. The path where the encrypted file will be saved.

- key:

  A raw vector of 32 bytes (256 bits) to be used as the encryption key.

- data:

  List, vector or any json-able object to write into encrypted file.

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- x:

  Character string or hex vector to convert.

## Value

No return value, called for side effects (writes to `output_file`).

## Details

The encryption process pads the data, generates a random Initialization
Vector (IV), and encrypts the content. The IV is prepended to the
encrypted data in the output file. Decryption involves extracting the
IV, decrypting, and unpadding the data. A specialized decryption
function `read_encrypted` is provided to directly parse decrypted
content (assumed to be `key=value` pairs) into a named R list. If no "="
sign found in the first line, text will be imported as character vector.

## See also

Other Credentials:
[`db_download()`](https://laresbernardo.github.io/lares/reference/db_download.md),
[`db_upload()`](https://laresbernardo.github.io/lares/reference/db_upload.md),
[`get_credentials()`](https://laresbernardo.github.io/lares/reference/get_credentials.md),
[`get_tweets()`](https://laresbernardo.github.io/lares/reference/get_tweets.md),
[`mail_send()`](https://laresbernardo.github.io/lares/reference/mail_send.md),
[`queryDB()`](https://laresbernardo.github.io/lares/reference/queryDB.md),
[`queryGA()`](https://laresbernardo.github.io/lares/reference/queryGA.md),
[`slackSend()`](https://laresbernardo.github.io/lares/reference/slackSend.md),
[`stocks_file()`](https://laresbernardo.github.io/lares/reference/stocks_report.md)

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
