CC_FILES += messagebody.pb.cc

all: $(CC_FILES)

%.pb.cc %.pb.h: %.proto
	protoc -I. --cpp_out . $<
