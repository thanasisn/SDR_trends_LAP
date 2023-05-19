
# File where to store auto increments
BLD_FILE ?= .buildver

# Initiate BLD_FILE if not exists
buildver_create := $(shell if ! test -f $(BLD_FILE); then echo 0 > $(BLD_FILE); fi)

# Prepare callable function. This function updates BLD_FILE
buildver = $(shell echo $$(($$(cat $(BLD_FILE)) + 1)) > $(BLD_FILE))

