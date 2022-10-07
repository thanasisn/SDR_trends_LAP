## Build a single Rmd file

.DEFAULT_GOAL := pdf

all:  pdf html
pdf:  p1 p2 p3 Ap
html: h1 h2 h3 Ah


# ## targets
# TARGET = Article
# RMD    = $(TARGET).Rmd
# PDF    = $(TARGET).pdf
# SLIDY  = $(TARGET).html
# Ap: $(PDF)
# $(PDF): $(RMD)
# 	Rscript -e "rmarkdown::render( '$?' )"
# 	@echo "Building: $@"
# 	@echo "Changed:  $?"
# 	setsid evince    $@ &
# 
# ## not working perfect
# Ah: $(SLIDY)
# $(SLIDY): $(RMD)
# 	Rscript -e "bookdown::render_book( '$?' , 'rmarkdown::html_document')"
# 	@echo "Building: $@"
# 	@echo "Changed:  $?"
# 	# setsid mimeopen  $@ &
# 
# p1: DHI_GHI_longterm_trends.R
# DHI_GHI_longterm_trends.R: DHI_GHI_longterm_trends.pdf
# 	Rscript -e "rmarkdown::render( '$?' )"
# 	@echo "Building: $@"
# 	@echo "Changed:  $?"
# #	setsid evince    $@ &



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
TARGET = DHI_GHI_longterm_trends
RMD    = $(TARGET).R
PDF    = $(TARGET).pdf
SLIDY  = $(TARGET).html
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



###   2. DHI_GHI_sza_trends
TARGET = DHI_GHI_sza_trends
RMD    = $(TARGET).R
PDF    = $(TARGET).pdf
SLIDY  = $(TARGET).html
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



###   3. DHI_GHI_trends_consistency
TARGET = DHI_GHI_trends_consistency
RMD    = $(TARGET).R
PDF    = $(TARGET).pdf
SLIDY  = $(TARGET).html
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




