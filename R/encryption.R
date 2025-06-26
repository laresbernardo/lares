####################################################################
#' File Encryption and Decryption (AES-256-CBC)
#' 
#' @description This set of functions provides utilities for encrypting
#' and decrypting files using AES-256 in CBC mode, primarily for
#' handling key-value pair "secrets" files.
#' @details The encryption process pads the data, generates a random
#' Initialization Vector (IV), and encrypts the content. The IV is
#' prepended to the encrypted data in the output file.
#' Decryption involves extracting the IV, decrypting, and unpadding the data.
#' A specialized decryption function (`decrypt_to_list`) is provided
#' to directly parse decrypted content (assumed to be `key=value` pairs)
#' into a named R list.
#' @keywords Encryption Decryption Security
#' @family Credentials
#' @param input_file Character string. The path to the file to be encrypted.
#' @param output_file Character string. The path where the encrypted
#' file will be saved.
#' @param key A raw vector of 32 bytes (256 bits) to be used as the encryption key.
#' @return No return value, called for side effects (writes to `output_file`).
#' @examples
#' \dontrun{
#' # Create dummy files for demonstration
#' temp_input <- tempfile(fileext = ".txt")
#' temp_output <- tempfile(fileext = ".enc")
#' writeLines("library=lares \nversion=5.3.0 \n pass=123", temp_input)
#' readLines(temp_input)
#'
#' # Generate a random 32-byte key (and save it in a secure place)
#' library(openssl)
#' raw_key <- rand_bytes(32)
#' raw_to_hex(raw_key)
#' hex_to_raw(raw_to_hex(raw_key))
#'
#' # Encrypt the file
#' encrypt_file(temp_input, temp_output, raw_key)
#'
#' # Decrypt the file directly to a list or vector
#' secrets <- read_encrypted(temp_output, raw_key)
#' unlist(secrets)
#' }
#' @export
encrypt_file <- function(input_file, output_file, key) {
  try_require("openssl")
  # Read binary file
  data <- readBin(input_file, what = "raw", n = file.info(input_file)$size)
  # Pad the data
  padded_data <- pad(data)
  # Generate random IV
  iv <- rand_bytes(16)
  # Encrypt using AES-256-CBC
  encrypted <- aes_cbc_encrypt(padded_data, key = key, iv = iv)
  # Write IV + encrypted data to output file
  writeBin(c(iv, encrypted), output_file)
}

#' Decript file to a list
#' @rdname encrypt_file
#' @export
read_encrypted <- function(input_file, key) {
  try_require("openssl")
  # Read binary file
  encrypted_data <- readBin(input_file, what = "raw", n = file.info(input_file)$size)
  # Extract IV (first 16 bytes) and encrypted content (remaining bytes)
  iv <- encrypted_data[1:16]
  encrypted_content <- encrypted_data[-(1:16)]
  # Decrypt using AES-256-CBC
  decrypted <- aes_cbc_decrypt(encrypted_content, key = key, iv = iv)
  # Remove padding
  unpadded_data <- unpad(decrypted)
  # Convert raw decrypted data into character format
  decrypted_text <- rawToChar(unpadded_data)
  # Split decrypted content into lines
  parsed_list <- lines <- strsplit(decrypted_text, "\n")[[1]]
  # Parse lines into key-value pairs
  if (grepl("=", lines[1])) {
    key_value_pairs <- lapply(lines, function(line) {
      parts <- strsplit(line, "=")[[1]]
      list(key = parts[1], value = parts[2])
    })
    # Create a named list with values and keys
    parsed_list <- as.list(setNames(
      trimws(lapply(key_value_pairs, `[[`, "value")),
      trimws(sapply(key_value_pairs, `[[`, "key"))
    ))
  }
  return(parsed_list)
}

# Convert Hexadecimal String to Raw Vector
#' @rdname encrypt_file
#' @param x Character string or hex vector to convert.
#' @export
hex_to_raw <- function(x) {
  try_require("openssl")
  stopifnot("character" %in% class(x))
  # Split hexadecimal string into pairs of two characters
  hex_pairs <- substring(
    x, seq(1, nchar(x), by = 2),
    seq(2, nchar(x), by = 2)
  )
  raw_vector <- as.raw(strtoi(hex_pairs, base = 16))
  return(raw_vector)
}

# Convert Raw Vector to Hexadecimal String
#' @rdname encrypt_file
raw_to_hex <- function(x) {
  stopifnot("raw" %in% class(x))
  paste0(as.character(x), collapse = "")
}

# Pad data to 16-byte AES block size
pad <- function(data) {
  pad_len <- 16 - (length(data) %% 16)
  c(data, rep(as.raw(pad_len), pad_len))
}

# Remove padding from decrypted data
unpad <- function(data) {
  pad_len <- as.integer(data[length(data)])
  data[1:(length(data) - pad_len)]
}

# Import decrypted file and convert to JSON
convert_to_list <- function(decrypted_file) {
  # Read the decrypted file
  lines <- readLines(decrypted_file)
  # Split each line into key-value pairs
  key_value_pairs <- lapply(lines, function(line) {
    parts <- strsplit(line, "=")[[1]]
    list(key = parts[1], value = parts[2])
  })
  # Create a named list or vector
  parsed_list <- setNames(
    trimws(lapply(key_value_pairs, `[[`, "value")), # Extract values
    trimws(sapply(key_value_pairs, `[[`, "key")) # Extract keys
  )
  return(parsed_list)
}
