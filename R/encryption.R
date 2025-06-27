####################################################################
#' File Encryption and Decryption (AES-256-CBC)
#'
#' @rdname encrypt_file
#' @description This set of functions provides utilities for encrypting
#' and decrypting files using AES-256 in CBC mode, primarily for
#' handling key-value pair "secrets" files.
#' @details The encryption process pads the data, generates a random
#' Initialization Vector (IV), and encrypts the content. The IV is
#' prepended to the encrypted data in the output file.
#' Decryption involves extracting the IV, decrypting, and unpadding the data.
#' A specialized decryption function \code{read_encrypted} is provided
#' to directly parse decrypted content (assumed to be \code{key=value} pairs)
#' into a named R list. If no "=" sign found in the first line, text will be
#' imported as character vector.
#' @keywords Encryption Decryption Security
#' @family Credentials
#' @param input_file Character string. The path to the file to be encrypted.
#' @param output_file Character string. The path where the encrypted
#' file will be saved.
#' @param key A raw vector of 32 bytes (256 bits) to be used as the encryption key.
#' @return No return value, called for side effects (writes to \code{output_file}).
#' @examples
#' \dontrun{
#' # Create dummy files for demonstration
#' temp_input <- tempfile(fileext = ".txt")
#' temp_output <- tempfile(fileext = ".enc")
#' writeLines("library=lares \nversion=5.3.0 \n pass=123", temp_input)
#' readLines(temp_input)
#'
#' # Generate a random 32-byte key (and save it in a secure place)
#' raw_key <- openssl::rand_bytes(32)
#' # You can convert from hex to raw and viceversa
#' raw_to_hex(raw_key)
#' hex_to_raw(raw_to_hex(raw_key))
#'
#' # Encrypt the file
#' encrypt_file(temp_input, temp_output, raw_key)
#'
#' # Import the data from encrypted file to list or vector
#' secrets <- read_encrypted(temp_output, raw_key)
#' unlist(secrets)
#'
#' # Example using a string (JSON in this case)
#' writeLines(jsonlite::toJSON(list(a = 1, list(b = 2, c = 1:3))), temp_input)
#' encrypt_file(temp_input, temp_output, raw_key)
#' read_encrypted(temp_output, raw_key)
#'
#' # Example writing a file from a list or vector directly from R
#' my_secrets <- list(
#'   api_key = "some_secret_api_key_123",
#'   username = "data_analyst",
#'   server = "production.server.com"
#' )
#' raw_key <- write_encrypted(my_secrets, temp_output, quiet = FALSE)
#' fromJSON(read_encrypted(temp_output, key = raw_key))
#' }
#' @export
encrypt_file <- function(input_file, output_file, key) {
  try_require("openssl")
  # Check key type
  if (!"raw" %in% class(key)) key <- hex_to_raw(key)
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
  # Check key type
  if (!"raw" %in% class(key)) key <- hex_to_raw(key)
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

#' Write a vector or list into an encoded file in JSON format
#' @inheritParams get_mp3
#' @param data List, vector or any json-able object to write into encrypted file.
#' @rdname encrypt_file
#' @export
write_encrypted <- function(data, output_file = "encrypted.enc", key = NULL, quiet = FALSE) {
  try_require("openssl")
  # 1. Convert R object to JSON string
  json_string <- jsonlite::toJSON(data, auto_unbox = TRUE, pretty = FALSE)
  # 2. Convert JSON string to raw vector
  data_raw <- charToRaw(json_string)
  # 3. Generate a new 32-byte (256-bit) AES key
  if (is.null(key)) {
    key <- rand_bytes(32)
  } else {
    if ("character" %in% class(key)) key <- hex_to_raw(key)
    quiet <- TRUE
  }
  # 4. Pad the raw data
  padded_data <- pad(data_raw)
  # 5. Generate random IV
  iv <- rand_bytes(16)
  # 6. Encrypt using AES-256-CBC
  encrypted <- aes_cbc_encrypt(padded_data, key = key, iv = iv)
  # 7. Write IV + encrypted data to output file
  writeBin(c(iv, encrypted), output_file)
  # 8. Return the hexadecimal key string
  if (!quiet) message("KEY: ", raw_to_hex(key))
  return(key)
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
#' @export
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
