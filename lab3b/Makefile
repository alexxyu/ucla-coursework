# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab3b-105295708.$(TAREXT)

TARGET = lab3b

default: $(TARGET)

$(TARGET): clean
	ln lab3b.py $(TARGET)
	chmod +x $(TARGET)

dist: $(DISTNAME)
dist-files = lab3b.py Makefile README
$(DISTNAME): 
	$(TAR) $(TARFLAGS) $@ $(dist-files)

clean:
	rm -rf *.$(TAREXT) $(TARGET)