


library(stringr)
library(bib2df)


setwd("~/MANUSCRIPTS/2022_sdr_trends/")
sourcefile   <- "./Article.Rmd"
mainreposit  <- "./references2.bib"
localreposit <- "./references3.bib"
folderlink   <- "./References"


## gather citations from file
lines   <- paste(readLines(sourcefile), collapse = "")
entries <- unique(str_match_all(lines, "@([a-zA-Z0-9]+)[,\\. \\?\\!\\]\\;]")[[1]][, 2])
entries <- grep("gmail|auth", entries, value = TRUE, invert = TRUE)


## get refs
bib <- bib2df(mainreposit)

## export sub file
output <- bib[bib$BIBTEXKEY %in% entries, ]
filesp <- output$FILE
output$ABSTRACT         <- NA
output$OWNER            <- NA
output$GROUPS           <- NA
output$MODIFICATIONDATE <- NA
output$TIMESTAMP        <- NA
output$FILE             <- NA
df2bib(output, localreposit)



# link files
filesp <- output$FILE
names(output)
basepath <- dirname(Sys.readlink(mainreposit))

for (af in strsplit(filesp, ":")) {
    cat(length(af),"\n")
    if (length(af) >= 2) {
        sourcefl <- paste0(basepath,"/", af[2])
        if (file.exists(sourcefl)) {
            dir.create(folderlink)
            file.symlink(sourcefl, paste0(folderlink, "/", basename(af[2])))
        }
    }
}


