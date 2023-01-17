## Build a single Rmd file

.DEFAULT_GOAL := pdf

all:       clean_all pdf rtim
pdf:       p1 p2 p3 Ap
html:      h1 h2 h3 Ah
rtim:      r1 r2 r3
clean_all: clean_cache clean_data clean_pdfs

presentation = "../presentations/2023-01-18_LAP_GHI_trends/"

###      Article
TARGET = Article
RMD    = $(TARGET).Rmd
PDF    = $(TARGET).pdf
SLIDY  = $(TARGET).html
Ap: $(PDF)
$(PDF): $(RMD)
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@echo "Building: $@"
	@echo "Changed:  $?"
#	setsid evince    $@ &
Ah: $(SLIDY)
$(SLIDY): $(RMD)
	-Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"
	@echo "Building: $@"
	@echo "Changed:  $?"
	# setsid mimeopen  $@ &



###   1. DHI_GHI_longterm_trends
TARGET := DHI_GHI_1_longterm_trends
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
SLIDY  := $(TARGET).html
RUNT   := ./runtime/$(TARGET).pdf

p1: $(PDF) 
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	## article files
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*.pdf' --include '*.pdf' ./DHI_GHI_*/figure-latex/ ./images
	## presentation files
	#@cp -r -u "$(basename $?)_files" "$(presentation)/images"
	#@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*.*' "$(basename $?)_files" "$(presentation)/images"
	#@mkdir -p                        "$(presentation)/figures/"
	#@cp -u "./figures/"*".dat"       "$(presentation)/figures/"
#	setsid evince    $@ &

h1: $(SLIDY)
$(SLIDY): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"
	## presentation files
	#@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*.*' "$(basename $?)_files" "$(presentation)/images"
	#@mkdir -p                        "$(presentation)/figures/"
	#@cp -u "./figures/"*".dat"       "$(presentation)/figures/"
	# setsid mimeopen  $@ &

r1: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?
	#@mkdir -p                        "$(presentation)/figures/"
	#@cp -u "./figures/"*".dat"       "$(presentation)/figures/"


###   2. DHI_GHI_sza_trends
TARGET := DHI_GHI_2_sza_trends
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
SLIDY  := $(TARGET).html
RUNT   := ./runtime/$(TARGET).pdf

p2: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	## article files
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*.pdf' --include '*.pdf' ./DHI_GHI_*/figure-latex/ ./images
	## presentation files
	#@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*.*'   "$(basename $?)_files" "$(presentation)/images"
#	setsid evince    $@ &

h2: $(SLIDY)
$(SLIDY): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"
	## presentation files
	#@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*.*' "$(basename $?)_files" "$(presentation)/images"
	# setsid mimeopen  $@ &

r2: $(RUNT)
$(RUNT): $(RMD)
	-Rscript $?




###   3. DHI_GHI_trends_consistency
TARGET := DHI_GHI_3_trends_consistency
RMD    := $(TARGET).R
PDF    := $(TARGET).pdf
SLIDY  := $(TARGET).html
RUNT   := ./runtime/$(TARGET).pdf

p3: $(PDF)
$(PDF): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	## article files
	@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*.pdf' --include '*.pdf' ./DHI_GHI_*/figure-latex/ ./images
	## presentation files
	#@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*.*'   "$(basename $?)_files" "$(presentation)/images"
#	setsid evince    $@ &

h3: $(SLIDY)
$(SLIDY): $(RMD)
	@echo "Building: $@"
	-Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"
	## presentation files
	#@-rsync -a --prune-empty-dirs --exclude 'unnamed-chunk*.*'   "$(basename $?)_files" "$(presentation)/images"
	# setsid mimeopen  $@ &

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
	rm -f    ./data/common_data.Rds
	rm -f    ./data/common_data_13.Rds
	rm -f    ./data/common_data_14.Rds
	


