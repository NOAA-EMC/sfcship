SHELL=/bin/sh

OBJS=	eomqm.o 

SRCS=	eomqm.f 

# Tunable parameters
#
#
FC =		ifort
LDFLAGS =	
LIBS =	
CMD =		marine_stats_eomqm
PROFLIB =	-lprof

FFLAGS =	-g -i8 -r8 

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

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
