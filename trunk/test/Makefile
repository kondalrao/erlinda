# $Id$
#
include ../build/otp.mk
include ../build/colors.mk
include ../build/buildtargets.mk

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------


MODULES = $(shell find . -name "*.erl" | sed s/.erl//)
MODULES_COMMA = $(shell find . -name "*.erl" | sed s/\\.erl/,/)

HRL_FILES=
ERL_FILES= $(MODULES:%=%.erl)
DOC_FILES=$(ERL_FILES)

BEAMS= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
TARGET_FILES= $(BEAMS) 

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_FLAGS +=
ERL_COMPILE_FLAGS += -I../include -I../lib/fslib/include -I../../system_status/include 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

all debug opt: $(EBIN) $(TARGET_FILES) 

#$(EBIN)/rm_logger.beam: $(APP_NAME).hrl
include ../build/docs.mk

# Note: In the open-source build clean must not destroy the preloaded
# beam files.
clean:
	rm -f $(TARGET_FILES)
	rm -f core
	rm -rf $(EBIN)
	rm -rf *html

$(EBIN):
	mkdir $(EBIN)

