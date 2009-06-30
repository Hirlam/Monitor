.DEFAULT:
.SUFFIXES: .F .F90 .f90
.PHONY: depend

# ROOTDIR is where the first make is started
ROOTDIR := $(PWD)

# SRCDIR should be CURDIR without the ARCH part
SRCDIR := $(ROOTDIR)/$(MAKECMDGOALS)

VPATH := .:$(SRCDIR)

include $(ROOTDIR)/config/config.$(ARCH)

FLUFLAG := 
ifeq (flu,$(findstring flu,$(SRCDIR)))
   FLUFLAG := -r8
endif

SRCSC   := $(notdir $(wildcard $(SRCDIR)/*.c))
SRCSF   := $(notdir $(wildcard $(SRCDIR)/*.F $(SRCDIR)/*.f))
SRCSF90 := $(notdir $(wildcard $(SRCDIR)/*.F90 $(SRCDIR)/*.f90))

OBJSC   := $(addsuffix .o, $(basename $(SRCSC)))
OBJSF   := $(addsuffix .o, $(basename $(SRCSF)))
OBJSF90 := $(addsuffix .o, $(basename $(SRCSF90)))

OBJS := $(OBJSF90) $(OBJSF) $(OBJSC) 
SRCS := $(SRCSF90) $(SRCSF) $(SRCSC) 

TARGET := $(ROOTDIR)/$(ARCH)/lib/$(MAKECMDGOALS).a

$(MAKECMDGOALS): $(TARGET)

$(TARGET): $(OBJS)
	$(AR) $(ARFLAGS) $@ $?

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

depend dependencies.inc: $(SRCS)
	cd $(SRCDIR) ; $(ROOTDIR)/$(ARCH)/bin/depf90mod.x $(MODEXTS) -M . -M $(TOROOT)/mod -I $(TOROOT)/inc $(SRCS)
	$(MV) $(SRCDIR)/dependencies.inc .

-include dependencies.inc

-include $(SRCDIR)/Makefile.qrk

