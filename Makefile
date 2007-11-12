#ARCH   := hpce

.DELETE_ON_ERROR:

ROOTDIR := $(PWD)

include $(ROOTDIR)/config/config.$(ARCH)


ifeq ($(MAGICSFLAG),-DMAGICS)
   GLLINK := src plt rdr mod src
else
   GLLINK := src rdr mod src
endif


LIBSGL := $(patsubst %,$(ROOTDIR)/$(ARCH)/lib/%.a,$(GLLINK))

default: $(ROOTDIR)/$(ARCH)/bin/verobs 

clean: clean_gl allclean clean_arch

cleanwrk: cleanobj_gl cleanpp_gl

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

GLLIBS := $(patsubst %,$(ROOTDIR)/$(ARCH)/lib/%.a,$(GLDIRS))

$(GLLIBS): $(ROOTDIR)/$(ARCH)/bin/depf90mod.x ./$(ARCH)/lib 
	-$(MKDIR)  $(ARCH)/$(patsubst $(ROOTDIR)/$(ARCH)/lib/%.a,%,$@)
	$(MAKE) -C $(ARCH)/$(patsubst $(ROOTDIR)/$(ARCH)/lib/%.a,%,$@) -f $(ROOTDIR)/makegl.mk ARCH=$(ARCH) TOROOT=.. $@

.PHONY : $(GLLIBS)

$(ROOTDIR)/$(ARCH)/bin/verobs: $(ARCH) $(GLLIBS)
	$(LD) $(ROOTDIR)/$(ARCH)/prg/verobs.o $(LIBSGL) $(EXTLIB) $(LDFLAGS) -o $@

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
