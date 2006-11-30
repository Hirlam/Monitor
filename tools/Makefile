.SUFFIXES: .F .F90

include $(ROOTDIR)/config/config.$(ARCH)

$(ROOTDIR)/$(ARCH)/bin/depf90mod.x : depf90mod.c
	$(CC) -g -o $@ $^ $(DEPF90MODLIBS) 

clean:
	rm -f depf90mod.x



