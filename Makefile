## Build a single Rmd file

SHELL = /bin/bash

.DEFAULT_GOAL := render

all:       clean_all pdf rtim
render:    pdf rtim
Ap:        Ap1 Ap2
pdf:       p1 p2 p3 Ap
rtim:      r1 r2 r3
clean_all: clean_cache clean_data clean_pdfs

include .buildver.makefile

presentation = "../presentations/2023-01-18_LAP_GHI_trends/"
LIBRARY      = ~/LIBRARY/REPORTS/



### Article
## using rstudio pandoc
TARGET = Article
RMD    = $(TARGET).Rmd
PDF    = $(TARGET).pdf
Ap2: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	@#-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='rticles::elsevier_article', output_file='$@', clean = TRUE)" 
	@# echo "Changed:  $?"
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}


TARGET = Article
RMD    = $(TARGET).Rmd
PDF    = $(TARGET)_A.pdf 
Ap1: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	@#-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@', clean = TRUE)"
	@# echo "Changed:  $?"
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}




## Article pdf with build number
## using rstudio pandoc
TARGET = Article
RMD    = $(TARGET).Rmd
PDF    = $(TARGET)_B$(shell cat $(BLD_FILE)).pdf
SLIDY  = $(TARGET).html
Apv: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', clean = TRUE, output_format='bookdown::pdf_document2', output_file='Article_A$(shell echo $$(($$(cat $(BLD_FILE)) + 1))).pdf')"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', clean = TRUE, output_format='rticles::elsevier_article', output_file='Article_B$(shell echo $$(($$(cat $(BLD_FILE)) + 1))).pdf')"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', clean = TRUE, output_format='bookdown::word_document2', output_file='Article_B$(shell echo $$(($$(cat $(BLD_FILE)) + 1))).docx')"
	-mkdir -p "Build_$(shell echo $$(($$(cat $(BLD_FILE)) + 1)))"
	-cp 'Article.Rmd' 'Article_B$(shell echo $$(($$(cat $(BLD_FILE)) + 1))).Rmd'
	$(call buildver)



###   1. DHI_GHI_longterm_trends  ####################################

TARGET := DHI_GHI_1_longterm_trends
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p1: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./DHI_GHI_*/figure-latex/ ./images
	@-rsync -a "$@" ${LIBRARY}
	@-touch Article.Rmd

r1: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?
	#@mkdir -p                        "$(presentation)/figures/"
	#@cp -u "./figures/"*".dat"       "$(presentation)/figures/"


###   2. DHI_GHI_sza_trends  #########################################

TARGET := DHI_GHI_2_sza_trends
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p2: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./DHI_GHI_*/figure-latex/ ./images
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}
	@-touch Article.Rmd

r2: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?



TARGET := DHI_GHI_2_sza_trends_b
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p2b: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-touch Article.Rmd



###   3. DHI_GHI_trends_consistency  #################################

TARGET := DHI_GHI_3_trends_consistency
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
RUNT   := ./runtime/$(TARGET).pdf

p3: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::find_pandoc(dir = '/usr/lib/rstudio/resources/app/bin/quarto/bin/tools'); rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*' --include '*.pdf' --include '*.png' ./DHI_GHI_*/figure-latex/ ./images
	@#setsid evince    $@ &
	@-rsync -a "$@" ${LIBRARY}
	@-touch Article.Rmd

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

