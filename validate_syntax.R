#!/usr/bin/env Rscript

files <- sort(c(
  list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  list.files("analysis", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  list.files("tests", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
))

errors <- list()
for (file in files) {
  tryCatch(
    {
      parse(file = file)
      cat(sprintf("OK %s\n", file))
    },
    error = function(error) {
      errors[[file]] <<- conditionMessage(error)
      cat(sprintf("ERROR %s: %s\n", file, conditionMessage(error)))
    }
  )
}

if (length(errors)) quit(status = 1L)
cat(sprintf("Parsed %d R files successfully.\n", length(files)))
