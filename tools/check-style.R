# Basic automated checks for the tidyverse conventions used in this package.
# Run from the source root: Rscript tools/check-style.R
files <- unlist(lapply(c("R", "tests", "benchmarks", "tools"), function(dir) {
  list.files(dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
}))
problems <- character()
for (file in files) {
  lines <- readLines(file)
  bad <- which(nchar(lines, type = "width") > 80L |
    grepl("\t| +$", lines))
  if (length(bad)) {
    problems <- c(problems, paste0(file, ":", bad, ": width/whitespace"))
  }
  tokens <- utils::getParseData(parse(file, keep.source = TRUE))
  bad <- which(tokens$token %in% c("EQ_ASSIGN", "';'"))
  if (length(bad)) {
    problems <- c(problems, paste0(
      file, ":", tokens$line1[bad], ": assignment/semicolon"
    ))
  }
}
if (length(problems)) {
  stop(paste(problems, collapse = "\n"), call. = FALSE)
}
cat("Basic tidyverse style checks passed for", length(files), "R files.\n")
