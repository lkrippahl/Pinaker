# Makefile for Pinaker
#
# Compiler: FreePascal

LAZARUS_VER=`ls /usr/lib/lazarus/|head -1`

all: 
	fpc -MObjFPC -Scgi -O1 -gl -WG -vewnhi -l -Fi/tmp/ -Fu./synapse/ -Fu/usr/lib/lazarus/$(LAZARUS_VER)/lcl/units/i386-linux/ -Fu/usr/lib/lazarus/$(LAZARUS_VER)/lcl/units/i386-linux/gtk2/ -Fu/usr/lib/lazarus/$(LAZARUS_VER)/packager/units/i386-linux/ -Fu. -FU/tmp/ -opinaker -dLCL -dLCLgtk2 pinaker.lpr

clean:
	rm -f pinaker

install:
	cp pinaker $(DESTDIR)/usr/bin/pinaker
