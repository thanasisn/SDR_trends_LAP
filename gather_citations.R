


library(stringr)

sourcefile   <- "./Article.Rmd"
mainreposit  <- "./references2.bib"
localreposit <- "./references3.bib"

## gather citations
lines   <- paste(readLines(sourcefile), collapse = "")
entries <- unique(str_match_all(lines, "@([a-zA-Z0-9]+)[,\\. \\?\\!\\]\\;]")[[1]][, 2])
entries <- grep("gmail|auth", entries, value = TRUE, invert = TRUE)

bib <- readLines(mainreposit)

grep("Long2008", bib, value = T)
bib <- paste(bib, collapse = "\n")
bib <- unlist(strsplit(bib, "\n@"))

output <- sapply(entries, grep, bib, value = T)
grep("Long2008", output, value = T)
output <- paste("@", as.character(output), sep = "")

output <- gsub("^@c\\(\"", "@", output)
output <- gsub("\"\n\\)$",  "", output)
# output <- gsub("\\\\n",  "\n", output)


gsub("^@c\\(\"", "@", grep("Long2008", output, value = T))

gsub("\\\\n\"\n\\)$",  "", grep("Long2008", output, value = T))


writeLines(unlist(output), localreposit)
