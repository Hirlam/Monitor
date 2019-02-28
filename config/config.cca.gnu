ARCH=cca.gnu

# CPP
MACHINECPP :=
CPP        := /lib/cpp -E -traditional

# C
CC      := gcc 
CCFLAGS := -O3 -g  $(MACHINECPP) 

# Fortran
FC := gfortran
ifeq "$(strip $(MAKEUP))" "yes"
  MYFCFLAGS := -O3 -fdefault-real-8 -fbacktrace -g -Wall
else
  MYFCFLAGS := -O3 -fbacktrace -g -Wall
endif

# Modules
MODNAME = $(shell echo $(*F) | tr "[:upper:]" "[:lower:]")
MODEXT := mod

# Link
LD          := $(FC)
LDFLAGS     := -O3 -fbacktrace -g -Wall -Werror -fopenmp

# AR
AR      := ar
ARFLAGS := rv
MV      := mv
RM      := rm -f
MKDIR   := mkdir
RMDIR   := rmdir

# Flags
FCFLAGS := $(MYFCFLAGS) -I$(ROOTDIR)/$(ARCH)/mod
CPPFLAGS := 

# depf90mod.x search extensions ( -f is default )
MODEXTS=-E F90 -E F -E f90 -E -f 
