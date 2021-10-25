SHELL=/bin/sh

OBJS=	genstx.o 

SRCS=	genstx.f 

# Tunable parameters
#
#
FC =		ifort
LDFLAGS =	
LIBS =	
CMD =		marine_stats_genstx
PROFLIB =	-lprof

FFLAGS =	-g -r8 -i8 

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
