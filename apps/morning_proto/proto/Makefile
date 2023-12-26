CC_FILES += Login.pb.cc
CC_FILES += ClientCmdConstants.pb.cc
CC_FILES += Mission.pb.cc

all: $(CC_FILES)

%.pb.cc %.pb.h: %.proto
	protoc -I. --cpp_out . $<
