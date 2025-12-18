

#' @export

clean_spinner <- function(text) {
  x <- paste(text, collapse = "\n")

  # Enlever les spinners braille
  x <- gsub("[\u2800-\u28FF]", "", x)

  # Enlever les codes ANSI du type \033[25h ou \u001B[?25h
  x <- gsub("\033\\[[0-9;?]*[A-Za-z]", "", x)
  x <- gsub("\u001B\\[[0-9;?]*[A-Za-z]", "", x)

  # Enlever résidus comme "25h"
  x <- gsub("\\b[0-9]{2}h\\b", "", x)

  # Enlever les astérisques Markdown
  x <- gsub("\\*", "", x)

  # Garder seulement après "début_de_ma_reponse:"
  if (grepl("début_de_ma_reponse:", x)) {
    x <- sub(".*début_de_ma_reponse:", "", x)
  }

  # Trim final
  x <- gsub("\n{2,}", "\n", x)
  trimws(x)
}


ollama_generate <- function(prompt, model = "gemma3:4b") {

  # On force une phrase clé en début de réponse
  prompt2 <- paste0(
    prompt,
    "\n\nRéponds en commençant EXACTEMENT par : début_de_ma_reponse:\n"
  )

  tmp <- tempfile(fileext = ".txt")
  writeLines(prompt2, tmp)

  cmd <- sprintf('type "%s" | ollama run %s', tmp, model)

  raw_output <- shell(cmd, intern = TRUE)

  clean_spinner(raw_output)
}















