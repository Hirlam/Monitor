ARCH    := ecgate
ROOTDIR := $(PWD)

export ARCH ROOTDIR

include $(ROOTDIR)/config/config.$(ARCH)

.DELETE_ON_ERROR:

ifeq ($(MAGICSFLAG),-DMAGICS)
   GLLINK := src plt rdr mod src
else
   GLLINK := src rdr mod src
endif
CMALINK := cmastat

LIBSGL  := $(patsubst %,$(ROOTDIR)/$(ARCH)/lib/%.a,$(GLLINK))
LIBSCMA := $(patsubst %,$(ROOTDIR)/$(ARCH)/lib/%.a,$(CMALINK))

default: verobs odbstat rejstat

clean:
	-$(RM) -rf $(ARCH)

ifeq ($(MAGICSFLAG),-DMAGICS)
   GLLIBS := mod rdr plt src
else
   GLLIBS := mod rdr src
endif
CMALIBS := cmastat

$(GLLIBS): depf90mod.x ./$(ARCH)/lib 
	test -d $(ARCH)/$@ || $(MKDIR) $(ARCH)/$@
	$(MAKE) -C $(ARCH)/$@ -f $(ROOTDIR)/makegl.mk ARCH=$(ARCH) TOROOT=.. $@

$(CMALIBS): depf90mod.x ./$(ARCH)/lib 
	test -d $(ARCH)/$@ || $(MKDIR) $(ARCH)/$@
	$(MAKE) -C $(ARCH)/$@ -f $(ROOTDIR)/makegl.mk ARCH=$(ARCH) TOROOT=.. $@

.PHONY : $(GLLIBS) $(CMALIBS)

verobs: $(GLLIBS) ./$(ARCH)/prg
        $(MAKE) -C $(ARCH)/prg -f $(ROOTDIR)/makeexe.mk LIBS="$(LIBSGL) $(EXTLIB)" LD="$(LD)" DEPS="$+" $@

odbstat: $(CMALIBS) ./$(ARCH)/prg
        $(MAKE) -C $(ARCH)/prg -f $(ROOTDIR)/makeexe.mk LIBS="$(LIBSCMA)" LD="$(LD)" DEPS="$+" $@

rejstat: $(CMALIBS) ./$(ARCH)/prg
        $(MAKE) -C $(ARCH)/prg -f $(ROOTDIR)/makeexe.mk LIBS="$(LIBSCMA)" LD="$(LD)" DEPS="$+" $@


# MISC tasks
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

