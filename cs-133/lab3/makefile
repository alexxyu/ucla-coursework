SRCS = lib/macros.cuh lib/cnn.h lib/cnn.cu lib/wrapper.cu lib/main.cu kernel.cu

REQUIRED_FILES = cnn.h params.sh lab3-report.pdf
export KERNEL ?= kernel.cu

test: cnn $(KERNEL)
	. ./params.sh; ./$<

test-seq:
	@$(MAKE) --no-print-directory test SEQUENTIAL=

cnn: $(SRCS)
	$(CXX) $(CXXFLAGS) -o $@ $(filter %.cu %.a %.o, $^) $(LDFLAGS)

clean:
	$(RM) cnn

CXX = nvcc # specify your compiler here
LDFLAGS += # specify your library linking options here
CXXFLAGS += -std=c++11 -O3 $(LDFLAGS)
