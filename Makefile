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

ifeq "$(strip $(ODB_MONITOR))" "-DODB_MONITOR"
  DEFS     = verobs jbconv obsmon
  ifeq "$(strip $(MAKEUP))" "yes"
    ODBLIBS_PATH   = $(HM_LIB)/$(ARCH)/src
    ODB_GLUE       = $(HM_LIB)/src/odb/scripts/
    # AUXLIBS not from makeup at the moment
    ODBLIBS       := $(ODBLIBS_PATH)/libECMA.a $(ODBLIBS_PATH)/libCCMA.a $(ODBLIBS_PATH)/libodbmain.a  $(ODBLIBS_PATH)/libodbport.a $(ODBLIBS_PATH)/libodb.a $(ODBLIBS_PATH)/libodbdummy.a  $(ODBLIBS_PATH)/libifsaux.a $(AUXLIBS)/libmpidummyR64.a $(AUXLIBS)/libnetcdfdummyR64.a $(HM_LIB)/util/sqlite3/flibs/libfsqlite.a $(HM_LIB)/util/sqlite3/sqlite-autoconf-3080002/lib/libsqlite3.a -ldl $(ODBLIBS_EXTRA)
  else
    ODBLIBS_PATH   = $(HOMEPACK)/gmkpack_build/
    ODB_GLUE       = $(ODBLIBS_PATH)/src/main/odb/scripts/
    ODBLIBS       := $(ODBLIBS_PATH)/lib/libcma-odb.main.a $(ODBLIBS_PATH)/lib/libodb.main.a $(ODBLIBS_PATH)/lib/libcma-odb.main.a $(ODBLIBS_PATH)/lib/libifsaux.main.a $(LD_MPI_DUMMY) $(LD_NETCDF_DUMMY) $(HM_LIB)/util/sqlite3/flibs/libfsqlite.a $(HM_LIB)/util/sqlite3/sqlite-autoconf-3080002/lib/libsqlite3.a -ldl $(ODBLIBS_EXTRA)
  endif
else
  DEFS     = verobs jbconv
  ODBLIBS  =
endif

LIBSGL  := $(patsubst %,$(ROOTDIR)/$(ARCH)/lib/%.a,$(GLLINK))
LIBSCMA := $(patsubst %,$(ROOTDIR)/$(ARCH)/lib/%.a,$(CMALINK))

default: $(DEFS)

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
	sh $(ODB_GLUE)/create_odbglue ecma ccma
	cp -p _odb_glue.c $@/_odb_glue.c
	$(MAKE) -C $(ARCH)/$@ -f $(ROOTDIR)/makegl.mk ARCH=$(ARCH) TOROOT=.. $@

prg : depf90mod.x ./$(ARCH)/lib mod
	test -d $(ARCH)/$@ || $(MKDIR) $(ARCH)/$@
	$(MAKE) -C $(ARCH)/$@ -f $(ROOTDIR)/makegl.mk ARCH=$(ARCH) TOROOT=.. $@

.PHONY : $(GLLIBS) $(CMALIBS) prg

verobs: $(GLLIBS) ./$(ARCH)/prg
	$(MAKE) -C $(ARCH)/prg -f $(ROOTDIR)/makeexe.mk LIBS="$(LIBSGL)" LD="$(LD)" DEPS="$+" $@

jbconv: ./$(ARCH)/prg
	$(MAKE) -C $(ARCH)/prg -f $(ROOTDIR)/makeexe.mk LIBS= LD="$(LD)" DEPS="$+" $@

obsmon: mod $(CMALIBS) ./$(ARCH)/prg
	$(MAKE) -C $(ARCH)/prg -f $(ROOTDIR)/makeexe.mk LIBS="$(LIBSCMA) $(ODBLIBS) $(LIBSCMA) $(ODBLIBS)" LD="$(LD)" DEPS="$+" $@

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
