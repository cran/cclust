LIBNAME=cclust

OBJS=   cclust.o median.o 

$(LIBNAME).so: $(OBJS)
	@$(LD) $(SHLIBLDFLAGS) -o $(LIBNAME).so $(OBJS)

clean:
	@rm -f *.o *.so

realclean:
	@rm -f Makefile *.o *.so

