## Build a single Rmd file

.DEFAULT_GOAL := pdf

all:  pdf html rtim
pdf:  p1 p2 p3 Ap
html: h1 h2 h3 Ah
rtim: r1 r2 r3


###      Article
TARGET = Article
RMD    = $(TARGET).Rmd
PDF    = $(TARGET).pdf
SLIDY  = $(TARGET).html
Ap: $(PDF)
$(PDF): $(RMD)
	Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@echo "Building: $@"
	@echo "Changed:  $?"
#	setsid evince    $@ &
Ah: $(SLIDY)
$(SLIDY): $(RMD)
	Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"
	@echo "Building: $@"
	@echo "Changed:  $?"
	# setsid mimeopen  $@ &



###   1. DHI_GHI_longterm_trends
TARGET = DHI_GHI_1_longterm_trends
RMD    = $(TARGET).R
PDF    = $(TARGET).pdf
SLIDY  = $(TARGET).html
RUNT   = ./runtime/$(TARGET).pdf
p1: $(PDF)
$(PDF): $(RMD)
	Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@echo "Building: $@"
	@echo "Changed:  $?"
#	setsid evince    $@ &
h1: $(SLIDY)
$(SLIDY): $(RMD)
	Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"
	@echo "Building: $@"
	@echo "Changed:  $?"
	# setsid mimeopen  $@ &
r1: $(RUNT)
$(RUNT): $(RMD)
	Rscript $?


###   2. DHI_GHI_sza_trends
TARGET = DHI_GHI_2_sza_trends
RMD    = $(TARGET).R
PDF    = $(TARGET).pdf
SLIDY  = $(TARGET).html
RUNT   = ./runtime/$(TARGET).pdf
p2: $(PDF)
$(PDF): $(RMD)
	Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@echo "Building: $@"
	@echo "Changed:  $?"
#	setsid evince    $@ &
h2: $(SLIDY)
$(SLIDY): $(RMD)
	Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"
	@echo "Building: $@"
	@echo "Changed:  $?"
	# setsid mimeopen  $@ &
r2: $(RUNT)
$(RUNT): $(RMD)
	Rscript $?




###   3. DHI_GHI_trends_consistency
TARGET = DHI_GHI_3_trends_consistency
RMD    = $(TARGET).R
PDF    = $(TARGET).pdf
SLIDY  = $(TARGET).html
RUNT   = ./runtime/$(TARGET).pdf
p3: $(PDF)
$(PDF): $(RMD)
	Rscript -e "rmarkdown::render('$?', output_format='bookdown::pdf_document2', output_file='$@')"
	@echo "Building: $@"
	@echo "Changed:  $?"
#	setsid evince    $@ &
h3: $(SLIDY)
$(SLIDY): $(RMD)
	Rscript -e "rmarkdown::render('$?', output_format='rmarkdown::html_document', output_file='$@')"
	@echo "Building: $@"
	@echo "Changed:  $?"
	# setsid mimeopen  $@ &
r2: $(RUNT)
$(RUNT): $(RMD)
	Rscript $?





