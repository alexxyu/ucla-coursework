# NAME: Alex Yu,Nicolas Trammer
# EMAIL: alexy23@g.ucla.edu,colet@g.ucla.edu
# ID: 105295708,005395690

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab3b-105295708.$(TAREXT)

TARGET = lab3b

default: $(TARGET)

$(TARGET): clean
	ln -s lab3b.py $(TARGET)
	chmod +x $(TARGET)

dist: $(DISTNAME)
dist-files = lab3b.py Makefile README
$(DISTNAME): 
	$(TAR) $(TARFLAGS) $@ $(dist-files)

clean:
	rm -rf *.$(TAREXT) $(TARGET)