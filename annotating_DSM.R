# ------------------------------------------------------------------------------
# Annotating whether there is a DRM using LLMs
# Arnold & Schulz: "Reaching For The Threshold"
# Chris Arnold, University of Birmingham, October 2025
#===============================================================================


# -- Writing Out the treaties we want to annotate ------------------------------
run.DRM <- FALSE
if (run.DRM == TRUE){
  treaties.to.annotate <- dat.treaty$treaty_code[!is.na(dat.treaty$idealpoint.variance)]
  write.csv(treaties.to.annotate, 'data/treaties.to.annotate.csv')
}


# -- Housekeeping --------------------------------------------------------------


# -- Parse the data  -----------------------------------------------------------
# Long execution time due to OCR: About 30mins
# If you want access to these data reach out to Chris Arnold
# Due to size not uploaded to the repo (290MB)
if (run.DRM==TRUE) {
  # Set the path to your folder containing PDFs
  pdf_folder <- "../data/treaty_texts"
  
  # List all PDF files in the folder
  pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)
  
  # Function to extract text with OCR fallback
  extract_text <- function(file) {
    message("Processing: ", basename(file))
    
    # Try regular text extraction
    txt <- pdf_text(file)
    combined <- paste(txt, collapse = "\n")
    
    # Check if result is empty/whitespace only
    if (nchar(gsub("\\s+", "", combined)) == 0) {
      message("  -> Falling back to OCR for: ", basename(file))
      ocr <- pdf_ocr_text(file)
      return(paste(ocr, collapse = "\n"))
    } else {
      return(combined)
    }
  }
  
  # Extract text for all PDFs
  # This does take a while!!
  pdf_texts <- lapply(pdf_files, extract_text)
  
  # Get document names from filenames
  doc_names <- basename(pdf_files)
  
  # Create a quanteda corpus object
  corpus_obj <- corpus(unlist(pdf_texts), docnames = doc_names)
  # Save corpus for later use
  save(corpus_obj, file = "data/treaty_corpus.rdata")
}

load(file = "data/treaty_corpus.rdata")

# Optional: View a summary
# print(summary(corpus_obj))



# -- Query the LLM -------------------------------------------------------------

# Using my openai key
api_key <- Sys.getenv("OPENAI_API_KEY")

annotate_treaty <- function(treaty_text, model = "gpt-4o") {
  prompt <- paste(
    "# Background",
    "For a research project, I am studying treaties between states deposited with the United Nations.",
    "I want to know whether a treaty has any reference to any dispute resolution provision in it.",
    "",
    "# Instruction",
    "* Go through the Treaty text mentioned below under the section title # Context: Treaty Text.",
    "* Annotate whether you find any dispute resolution provision in it.",
    "* Return a json file that contains two entries:",
    "  * In the first field called 'waste', offer a quick justification for your decision in two to three sentences.",
    "  * In the second field called 'keep', 1 for the presence of a dispute resolution provision and 0 for no indication of a dispute resolution provision.",
    "",
    "# Context: Treaty Text",
    treaty_text,
    sep = "\n"
  )
  
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = model,
      messages = list(
        list(role = "system", content = "You are a legal assistant."),
        list(role = "user", content = prompt)
      ),
      temperature = 0.3
    )
  )
  
  # Check response and extract content
  if (response$status_code != 200) {
    warning("API request failed with status: ", response$status_code)
    return(NULL)
  }
  
  raw <- httr::content(response, as = "parsed", type = "application/json")
  raw$choices[[1]]$message$content
}




parse_keep_field <- function(json_text) {
  if (is.null(json_text) || nchar(trimws(json_text)) == 0) {
    return(NA)
  }
  # Remove code block fencing if present (```json ... ```)
  clean_text <- gsub("^```json\\s*|\\s*```$", "", json_text)
  # Find JSON bounds (fallback for safety)
  json_start <- regexpr("\\{", clean_text)
  json_end <- regexpr("\\}\\s*$", clean_text)
  if (json_start == -1 || json_end == -1 || json_start > json_end) {
    warning("Could not locate valid JSON structure.")
    return(NA)
  }
  json_str <- substr(clean_text, json_start, json_end + attr(json_end, "match.length") - 1)
  # Parse the JSON and return the 'keep' field
  parsed <- tryCatch(jsonlite::fromJSON(json_str), error = function(e) {
    warning("JSON parsing failed.")
    return(NULL)
  })
  if (!is.null(parsed) && !is.null(parsed$keep)) {
    return(as.integer(parsed$keep))
  } else {
    warning("'keep' field not found.")
    return(NA)
  }
}



# Prepare this for production
annotate_treaty_chunked <- function(treaty_text, model = "gpt-4o", chunk_size = 8000, max_queries_per_min = 20) {
  chunks <- ceiling(nchar(treaty_text) / chunk_size)
  results <- character(0)
  # Track number of queries made in the last minute
  if (!exists(".query_counter", envir = .GlobalEnv)) {
    assign(".query_counter", 0, envir = .GlobalEnv)
  }
  for (i in seq_len(chunks)) {
    # Rate limiting check
    .query_counter <<- get(".query_counter", envir = .GlobalEnv) + 1
    if (.query_counter > max_queries_per_min) {
      message("🔁 Query limit hit — waiting for 60 seconds...")
      Sys.sleep(60)
      assign(".query_counter", 1, envir = .GlobalEnv)  # reset
    }
    # Slice the text
    start <- ((i - 1) * chunk_size) + 1
    end <- min(i * chunk_size, nchar(treaty_text))
    chunk_text <- substr(treaty_text, start, end)
    # Call the original function
    result <- annotate_treaty(chunk_text)
    results <- c(results, result)
    message(sprintf("Processed chunk %d/%d", i, chunks))
  }
  return(results)
}
aggregate_keep_from_chunks <- function(result_list) {
  keep_values <- sapply(result_list, parse_keep_field)
  if (all(is.na(keep_values))) {
    return(NA)
  }
  return(max(keep_values, na.rm = TRUE))  # 1 if any chunk found a provision
}




annotate_doc <- function(doc_name, text) {
  cat("Annotating:", doc_name, "\n")
  chunk_results <- annotate_treaty_chunked(text)
  keep_value <- aggregate_keep_from_chunks(chunk_results)
  data.frame(treaty_name = sub("\\.pdf$", "", doc_name), keep = keep_value)
}


# This takes a loooong time
mpr.dsm.dat <- do.call(rbind, Map(annotate_doc, doc_names, 
                                 treaty_text))

if (run.DRM==TRUE) save(mpr.dsm.dat, file = 'data/mprdsm.rdata')



