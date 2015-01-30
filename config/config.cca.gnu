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

ODB_MONITOR   := -DODB_MONITOR
ODBLIBS_EXTRA := -lpthread
LD_MPI_DUMMY    = $(AUXLIBS)/libmpi_serial.a
LD_NETCDF_DUMMY = -L/opt/cray/netcdf/4.3.0/GNU/48/lib/ -lnetcdf -lnetcdff

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
FCFLAGS := $(MYFCFLAGS) -I$(ROOTDIR)/inc -I$(ROOTDIR)/$(ARCH)/mod
CPPFLAGS := -I$(ROOTDIR)/inc $(ODB_MONITOR)

# depf90mod.x search extensions ( -f is default )
MODEXTS=-E F90 -E F -E f90 -E -f 
