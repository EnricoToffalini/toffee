
##########################################

#' Extract and Clean Text from a PDF
#' This function extracts text from a multi-page PDF and preprocesses it into a seamless, clean block of text suitable for downstream analysis or use with AI tools (e.g., via API). It removes repetitive headers/footers, page numbers, capitalized section titles, and common metadata lines. It also merges words split across lines by hyphenation.
#' @param pdf_path Character string. Full path to the PDF file to be processed.
#' @return A single character string containing the cleaned and flattened text from the PDF. UTF-8 encoding is enforced on the output.
#' @export
pdf2text = function(pdf_path = NA) {
  
  require(pdftools)
  require(stringr)
  
  if (is.na(pdf_path) || !file.exists(pdf_path)) {
    stop("Valid PDF file path must be provided.")
  }
  raw_pages = pdf_text(pdf_path)
  if (length(raw_pages) == 0) {
    warning("No pages found in PDF.")
    return("")
  }
  
  # Identify common headers and footers
  first_lines = sapply(raw_pages, function(p) str_trim(str_split(p, "\n")[[1]][1]))
  last_lines = sapply(raw_pages, function(p) str_trim(tail(str_split(p, "\n")[[1]], 1)))
  common_header = names(sort(table(first_lines), decreasing = TRUE))[1]
  common_footer = names(sort(table(last_lines), decreasing = TRUE))[1]
  cleaned_pages = vector("character", length(raw_pages))
  
  for (i in seq_along(raw_pages)) {
    page_text = raw_pages[[i]]
    
    lines = str_split(page_text, "\n")[[1]]
    lines = str_trim(lines)
    
    # Remove lines that are likely page numbers
    lines = lines[!str_detect(lines, "^\\s*\\d+\\s*$")]
    
    # Remove ALL CAPS lines (likely section headings or metadata)
    lines = lines[!str_detect(lines, "^[A-ZÀ-Ü ,\\-0-9]{5,}$")]
    
    # Remove inline metadata or intrusive headers (e.g., journal names mid-paragraph)
    lines = lines[!str_detect(lines, "^[A-ZÀ-Ü][A-ZÀ-Ü0-9 /,.:;\\-]{10,}$")]
    
    # Remove common header/footer
    if (length(lines) > 1 && str_trim(lines[1]) == common_header) {
      lines = lines[-1]
    }
    if (length(lines) > 1 && str_trim(tail(lines, 1)) == common_footer) {
      lines = head(lines, -1)
    }
    # Rejoin hyphenated line breaks
    j = 1
    while (j < length(lines)) {
      if (str_detect(lines[j], "-$") && str_detect(lines[j + 1], "^[a-zà-ü]")) {
        lines[j] = str_replace(lines[j], "-$", "") %>% paste0(lines[j + 1])
        lines = lines[-(j + 1)]
      } else {
        j = j + 1
      }
    }
    cleaned_pages[[i]] = paste(lines, collapse = " ")
  }
  full_text = paste(cleaned_pages, collapse = " ")
  full_text = str_replace_all(full_text, "\\s+", " ")
  full_text = str_trim(full_text)
  Encoding(full_text) = "UTF-8"

  return(full_text)
}


##########################################




