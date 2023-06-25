## Build a single Rmd file

SHELL = /bin/bash

.DEFAULT_GOAL := render

all:       clean_all pdf html rtim
render:    pdf html rtim
pdf:       p1 p2 p3 Ap
#html:      h1 h2 h3
#html:      h2 h3
rtim:      r1 r2 r3
clean_all: clean_cache clean_data clean_pdfs

include .buildver.makefile

presentation = "../presentations/2023-01-18_LAP_GHI_trends/"
LIBRARY      = ~/LIBRARY/REPORTS/



###      Article
TARGET = Article
RMD    = $(TARGET).Rmd
PDF    = $(TARGET).pdf
SLIDY  = $(TARGET).html
Ap: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	@#-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	-Rscript -e "rmarkdown::find_pandoc(cache = FALSE, dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools') ;rmarkdown::render('$?', output_format='rticles::elsevier_article', output_file='$@')"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='Article_A.pdf')"
	@# echo "Changed:  $?"
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}


Ah: $(SLIDY)
$(SLIDY): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"
	@#echo "Changed:  $?"
	@# setsid mimeopen  $@ &



## Article pdf with build number
TARGET = Article
RMD    = $(TARGET).Rmd
PDF    = $(TARGET)_B$(shell cat $(BLD_FILE)).pdf
SLIDY  = $(TARGET).html
Apv: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='Article_A$(shell echo $$(($$(cat $(BLD_FILE)) + 1))).pdf')"
	-Rscript -e "rmarkdown::find_pandoc(cache = FALSE, dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='rticles::elsevier_article', output_file='Article_B$(shell echo $$(($$(cat $(BLD_FILE)) + 1))).pdf')"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::word_document2', output_file='Article_B$(shell echo $$(($$(cat $(BLD_FILE)) + 1))).docx')"
	-cp 'Article.Rmd' 'Article_B$(shell echo $$(($$(cat $(BLD_FILE)) + 1))).Rmd'
	$(call buildver)



###   1. DHI_GHI_longterm_trends  ####################################

TARGET := DHI_GHI_1_longterm_trends
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
SLIDY  := $(TARGET).html
RUNT   := ./runtime/$(TARGET).pdf

p1: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./DHI_GHI_*/figure-latex/ ./images
	#@echo "Building: $(SLIDY)"
	#-Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$(SLIDY)')"
	#@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.png' ./DHI_GHI_*/figure-latex/ ./images
	@#mkdir -p                   "$(presentation)/figures/"
	@#-cp -u "./figures/"*".dat" "$(presentation)/figures/"
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}

# h1: $(SLIDY)
# $(SLIDY): $(RMD)
# 	@echo "Building: $@"
# 	-Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"
# 	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.png' ./DHI_GHI_*/figure-latex/ ./images
# 	@#mkdir -p                   "$(presentation)/figures/"
# 	@#-cp -u "./figures/"*".dat" "$(presentation)/figures/"
# 	@# setsid mimeopen  $@ &

r1: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?
	#@mkdir -p                        "$(presentation)/figures/"
	#@cp -u "./figures/"*".dat"       "$(presentation)/figures/"



###   2. DHI_GHI_sza_trends  #########################################

TARGET := DHI_GHI_2_sza_trends
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
SLIDY  := $(TARGET).html
RUNT   := ./runtime/$(TARGET).pdf

p2: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./DHI_GHI_*/figure-latex/ ./images
	@#-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*.*' "$(basename $?)_files" "$(presentation)/images"
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}

h2: $(SLIDY)
$(SLIDY): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.png' ./DHI_GHI_*/figure-latex/ ./images
	@#-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*.*' "$(basename $?)_files" "$(presentation)/images"
	@#setsid mimeopen  $@ &

r2: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?



###   3. DHI_GHI_trends_consistency  #################################

TARGET := DHI_GHI_3_trends_consistency
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
SLIDY  := $(TARGET).html
RUNT   := ./runtime/$(TARGET).pdf

p3: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./DHI_GHI_*/figure-latex/ ./images
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}

h3: $(SLIDY)
$(SLIDY): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.png' ./DHI_GHI_*/figure-latex/ ./images
	@# setsid mimeopen  $@ &

r3: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?



clean_cache:
	rm -f -r ./Article_cache
	rm -f -r ./DHI_GHI_1_longterm_trends_cache
	rm -f -r ./DHI_GHI_1_longterm_trends_files
	rm -f -r ./DHI_GHI_2_sza_trends_cache
	rm -f -r ./DHI_GHI_2_sza_trends_files
	rm -f -r ./DHI_GHI_3_trends_consistency_cache
	rm -f -r ./DHI_GHI_3_trends_consistency_files
	rm -f -r ./runtime/DHI_GHI_1_longterm_trends.pdf
	rm -f -r ./runtime/DHI_GHI_2_sza_trends.pdf
	rm -f -r ./runtime/DHI_GHI_3_trends_consistency.pdf

clean_pdfs:
	rm -f    ./DHI_GHI_1_longterm_trends.pdf
	rm -f    ./DHI_GHI_2_sza_trends.pdf
	rm -f    ./DHI_GHI_3_trends_consistency.pdf

clean_data:
	rm -f    ./data/*.*

upload:
	./upload.sh

