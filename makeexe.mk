.SUFFIXES: .F .F90 .f90 .c
TOROOT=..

include $(ROOTDIR)/config/config.$(ARCH)

VPATH=.:$(TOROOT)/../prg

MAIN := $(addsuffix .o, $(basename $(notdir $(MAKECMDGOALS))))

DEPLIBS := $(patsubst %,$(ROOTDIR)/$(ARCH)/lib/%.a,$(filter-out %prg,$(DEPS)))

TARGET := $(ROOTDIR)/$(ARCH)/bin/$(MAKECMDGOALS)

$(MAKECMDGOALS): $(TARGET)
	

$(TARGET): $(MAIN) $(DEPLIBS)
	$(LD) $+ $(LIBS) -o $@ $(LDFLAGS)

%.o: %.c
	$(CC) $(CCFLAGS) -c $<

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< $(*F)_pp.f90
	$(FC) $(FCFLAGS) $(FLUFLAG) $(FREE) -c $(*F)_pp.f90
	$(MV) $(*F)_pp.o $(*F).o

%.o: %.f90
	$(CPP) $(CPPFLAGS) $< $(*F)_pp.f90
	$(FC) $(FCFLAGS) $(FLUFLAG) $(FREE) -c $(*F)_pp.f90
	$(MV) $(*F)_pp.o $(*F).o

%.o: %.F
	$(CPP) $(CPPFLAGS) $< $(*F)_pp.f
	$(FC) $(FCFLAGS) $(FLUFLAG) $(FIXED) -c $(*F)_pp.f
	$(MV) $(*F)_pp.o $(*F).o

%.o: %.f
	$(CPP) $(CPPFLAGS) $< $(*F)_pp.f
	$(FC) $(FCFLAGS) $(FIXED) -c $(*F)_pp.f
	$(MV) $(*F)_pp.o $(*F).o
