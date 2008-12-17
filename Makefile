ARCH   := ecgate

.DELETE_ON_ERROR:

ROOTDIR := $(PWD)

include $(ROOTDIR)/config/config.$(ARCH)


ifeq ($(MAGICSFLAG),-DMAGICS)
   GLLINK := src plt rdr mod src
else
   GLLINK := src rdr mod src
endif
CMALINK := cmastat

LIBSGL  := $(patsubst %,$(ROOTDIR)/$(ARCH)/lib/%.a,$(GLLINK))
LIBSCMA := $(patsubst %,$(ROOTDIR)/$(ARCH)/lib/%.a,$(CMALINK))

default: verobs odbstat rejstat

verobs: $(ROOTDIR)/$(ARCH)/bin/verobs 
odbstat: $(ROOTDIR)/$(ARCH)/bin/odbstat 
rejstat: $(ROOTDIR)/$(ARCH)/bin/rejstat

clean: clean_gl clean_cma allclean clean_arch

cleanwrk: cleanobj_gl cleanpp_gl cleanobj_cma cleanpp_cma

allclean: 
	$(MAKE) -C tools clean ARCH=$(ARCH) ROOTDIR=$(ROOTDIR)
	-$(RM) ./$(ARCH)/bin/*
	-$(RMDIR) ./$(ARCH)/bin
	-$(RM) ./$(ARCH)/lib/*
	-$(RMDIR) ./$(ARCH)/lib
	-$(RM) ./bin

clean_arch:
	-$(RMDIR) ./$(ARCH)

ifeq ($(MAGICSFLAG),-DMAGICS)
   GLDIRS := mod rdr plt src prg
else
   GLDIRS := mod rdr src prg
endif
   CMADIRS := cmastat prg

GLLIBS := $(patsubst %,$(ROOTDIR)/$(ARCH)/lib/%.a,$(GLDIRS))
CMALIBS := $(patsubst %,$(ROOTDIR)/$(ARCH)/lib/%.a,$(CMADIRS))

$(CMALIBS): $(ROOTDIR)/$(ARCH)/bin/depf90mod.x ./$(ARCH)/lib 
	-$(MKDIR)  $(ARCH)/$(patsubst $(ROOTDIR)/$(ARCH)/lib/%.a,%,$@)
	$(MAKE) -C $(ARCH)/$(patsubst $(ROOTDIR)/$(ARCH)/lib/%.a,%,$@) -f $(ROOTDIR)/makegl.mk ARCH=$(ARCH) TOROOT=.. $@

$(GLLIBS): $(ROOTDIR)/$(ARCH)/bin/depf90mod.x ./$(ARCH)/lib 
	-$(MKDIR)  $(ARCH)/$(patsubst $(ROOTDIR)/$(ARCH)/lib/%.a,%,$@)
	$(MAKE) -C $(ARCH)/$(patsubst $(ROOTDIR)/$(ARCH)/lib/%.a,%,$@) -f $(ROOTDIR)/makegl.mk ARCH=$(ARCH) TOROOT=.. $@

.PHONY : $(GLLIBS)

$(ROOTDIR)/$(ARCH)/bin/verobs: $(ARCH) $(GLLIBS)
	$(LD) $(ROOTDIR)/$(ARCH)/prg/verobs.o $(LIBSGL) $(EXTLIB) $(LDFLAGS) -o $@

$(ROOTDIR)/$(ARCH)/bin/odbstat: $(ARCH) $(CMALIBS)
	$(LD) $(ROOTDIR)/$(ARCH)/prg/odbstat.o $(LIBSCMA) $(LDFLAGS) -o $@

$(ROOTDIR)/$(ARCH)/bin/rejstat: $(ARCH) $(CMALIBS)
	$(LD) $(ROOTDIR)/$(ARCH)/prg/rejstat.o $(LIBSCMA) $(LDFLAGS) -o $@

clean_gl: cleanobj_gl cleanpp_gl
	-for dir in $(GLDIRS) ; do \
	$(RM) -rf $(ARCH)/$$dir ; \
	done ;

cleanobj_gl:
	-for dir in $(GLDIRS) ; do \
	$(RM) $(ARCH)/$$dir/*.o ; \
	done ;

cleanpp_gl:
	-for dir in $(GLDIRS) ; do \
	$(RM) $(ARCH)/$$dir/*_pp.* ; \
	done ;

clean_cma: cleanobj_cma cleanpp_cma
	-for dir in $(CMADIRS) ; do \
	$(RM) -rf $(ARCH)/$$dir ; \
	done ;

cleanobj_cma:
	-for dir in $(CMADIRS) ; do \
	$(RM) $(ARCH)/$$dir/*.o ; \
	done ;

cleanpp_cma:
	-for dir in $(CMADIRS) ; do \
	$(RM) $(ARCH)/$$dir/*_pp.* ; \
	done ;

# MISC tasks

./$(ARCH):
	-$(MKDIR) ./$(ARCH)
	-$(MKDIR) ./$(ARCH)/bin
	-$(MKDIR) ./$(ARCH)/lib
	-ln -sf   ./$(ARCH)/bin .

./$(ARCH)/bin: ./$(ARCH)
	-$(MKDIR) ./$(ARCH)/bin

./$(ARCH)/lib: ./$(ARCH)
	-$(MKDIR) ./$(ARCH)/lib

$(ROOTDIR)/$(ARCH)/bin/depf90mod.x: ./$(ARCH)
	$(MAKE) -C tools -f Makefile ARCH=$(ARCH) ROOTDIR=$(ROOTDIR) $(ROOTDIR)/$(ARCH)/bin/depf90mod.x
