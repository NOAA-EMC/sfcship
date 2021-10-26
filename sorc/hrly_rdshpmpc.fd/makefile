SHELL=/bin/bash
#

SRCS=	rdshpmpc.f

OBJS=	rdshpmpc.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
#FC =          ifort
FC =          ftn
LDFLAGS =	
LIBS =	    ${W3NCO_LIB8} ${BUFR_LIB8}
CMD =		hrly_rdshpmpc
PROFLIB =	-lprof

FFLAGS =     -i8 -r8 -assume noold_ldout_format

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
