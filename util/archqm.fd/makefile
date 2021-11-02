SHELL=/bin/sh
#

OBJS=	archqm.o

SRCS=	archqm.f

# Tunable parameters
#
# CF		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =		ftn
LDFLAGS =	
LIBS =  -L/nwprod/lib/graphics -lgph_8 -lutil \
	-L/nwprod/lib  -lw3nco_8 -lbufr_8_64 
CMD =		archqm
PROFLIB =	

FFLAGS =	  -r8 -i8 

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) $(FFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
