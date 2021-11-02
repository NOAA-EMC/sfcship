SHELL=/bin/sh

OBJS=	newstats.o 

SRCS=	newstats.f 

# Tunable parameters
#
#
FC =		ftn
LDFLAGS =	
#LIBS =          -L/nwprod/lib -lw3nco_8	
CMD =		marine_stats_newstats
PROFLIB =	-lprof

FFLAGS =	-g -r8 -i8 

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(W3NCO_LIB8)

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
