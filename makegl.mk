.DEFAULT:
.SUFFIXES: .F .F90 .f90
.PHONY: cleanpp cleanobj clean depend

# SRCDIR should be CURDIR without the ARCH part
SRCDIR := $(subst $(ARCH)/,,$(CURDIR))

# ROOTDIR is where the first make is started
ROOTDIR := $(PWD)

VPATH := .:$(SRCDIR)

include $(ROOTDIR)/config/config.$(ARCH)

SRCSC   := $(notdir $(wildcard $(SRCDIR)/*.c))
SRCSF   := $(notdir $(wildcard $(SRCDIR)/*.F $(SRCDIR)/*.f))
SRCSF90 := $(notdir $(wildcard $(SRCDIR)/*.F90 $(SRCDIR)/*.f90))

OBJSC   := $(addsuffix .o, $(basename $(SRCSC)))
OBJSF   := $(addsuffix .o, $(basename $(SRCSF)))
OBJSF90 := $(addsuffix .o, $(basename $(SRCSF90)))

OBJS := $(OBJSF90) $(OBJSF) $(OBJSC) 
SRCS := $(SRCSF90) $(SRCSF) $(SRCSC) 

$(MAKECMDGOALS): $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

.c.o:
	$(CC) $(CCFLAGS) -c $<

.F90.o:
	$(CPP) $(CPPFLAGS) $< $(*F)_pp.f90
	$(FC) $(FCFLAGS) $(FREE) -c $(*F)_pp.f90
	$(MV) $(*F)_pp.o $(*F).o

.f90.o:
	$(CPP) $(CPPFLAGS) $< $(*F)_pp.f90
	$(FC) $(FCFLAGS) $(FREE) -c $(*F)_pp.f90
	$(MV) $(*F)_pp.o $(*F).o

.F.o:
	$(CPP) $(CPPFLAGS) $< $(*F)_pp.f
	$(FC) $(FCFLAGS) $(FIXED) -c $(*F)_pp.f
	$(MV) $(*F)_pp.o $(*F).o

.f.o:
	$(CPP) $(CPPFLAGS) $< $(*F)_pp.f
	$(FC) $(FCFLAGS) $(FIXED) -c $(*F)_pp.f
	$(MV) $(*F)_pp.o $(*F).o

cleanobj:
	$(RM) *.o

cleanpp: 
	$(RM) *_pp.f

clean: cleanobj cleanpp
	-$(RM) *.d work.pc

depend dependencies.inc: $(SRCS)
	cd $(SRCDIR) ; $(ROOTDIR)/$(ARCH)/bin/depf90mod.x $(MODEXTS) -M . -M $(TOROOT)/mod -I $(TOROOT)/inc $(SRCS)
	$(MV) $(SRCDIR)/dependencies.inc .

-include dependencies.inc

-include $(SRCDIR)/Makefile.qrk

