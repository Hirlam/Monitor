ARCH    := ecgate
ROOTDIR := $(PWD)

export ARCH ROOTDIR

include $(ROOTDIR)/config/config.$(ARCH)

.DELETE_ON_ERROR:

GLLINK := src rdr mod src

DEFS     = verobs

LIBSGL  := $(patsubst %,$(ROOTDIR)/$(ARCH)/lib/%.a,$(GLLINK))

default: $(DEFS)

clean:
	-$(RM) -rf $(ARCH)

GLLIBS := mod rdr src

$(GLLIBS): depf90mod.x ./$(ARCH)/lib 
	test -d $(ARCH)/$@ || $(MKDIR) $(ARCH)/$@
	$(MAKE) -C $(ARCH)/$@ -f $(ROOTDIR)/makegl.mk ARCH=$(ARCH) TOROOT=.. $@

prg : depf90mod.x ./$(ARCH)/lib mod
	test -d $(ARCH)/$@ || $(MKDIR) $(ARCH)/$@
	$(MAKE) -C $(ARCH)/$@ -f $(ROOTDIR)/makegl.mk ARCH=$(ARCH) TOROOT=.. $@

.PHONY : $(GLLIBS) $(CMALIBS) prg

verobs: $(GLLIBS) ./$(ARCH)/prg
	$(MAKE) -C $(ARCH)/prg -f $(ROOTDIR)/makeexe.mk LIBS="$(LIBSGL)" LD="$(LD)" DEPS="$+" $@

# MISC tasks
./$(ARCH):
	test -d $@ || $(MKDIR) $@

./$(ARCH)/bin: ./$(ARCH)
	test -d $@ || $(MKDIR) $@

./$(ARCH)/lib: ./$(ARCH)
	test -d $@ || $(MKDIR) $@

./$(ARCH)/prg: ./$(ARCH) ./$(ARCH)/bin
	test -d $@ || $(MKDIR) $@
	-ln -sf   ./$(ARCH)/bin .

depf90mod.x: ./$(ARCH)/bin
	$(MAKE) -C tools -f Makefile ARCH=$(ARCH) ROOTDIR=$(ROOTDIR) $(ROOTDIR)/$(ARCH)/bin/depf90mod.x

install:
	install -D  $(ROOTDIR)/$(ARCH)/bin/verobs $(DESTDIR)/bin/verobs

uninstall:
	rm -f  $(DESTDIR)/bin/verobs
