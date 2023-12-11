PB_PROTOS := ClientCmdConstants Login
PB_MORNING := ClientCmdConstants Login 

MORNING_SHARE_DIR := proto
MORNING_GPB_DIR := $(shell  echo ../../_build/default/lib; )

PB_ERLS := $(patsubst %, src/pb_%.erl, $(PB_PROTOS))
PB_HRLS := $(patsubst %, include/pb_%.hrl, $(PB_PROTOS))
PB_MORNING_PRO := $(patsubst %, %.proto, $(PB_MORNING))
$(PB_ERLS): src/pb_%.erl: $(MORNING_SHARE_DIR)/%.proto include
	cd $(MORNING_GPB_DIR)/gpb && make
	$(MORNING_GPB_DIR)/gpb/bin/protoc-erl -modprefix pb_  -strbin -I$(MORNING_SHARE_DIR) \
                          -o-erl src 	 \
                          -o-hrl include 	\
		$(patsubst $(MORNING_SHARE_DIR)/%.proto, %.proto, $(PB_MORNING_PRO))
	cd $(MORNING_GPB_DIR)/gpb && make clean

.PHONY:pb
pb:  $(PB_ERLS)

include:
	mkdir include

.PHONY:clean
clean:
	-@rm $(PB_ERLS) $(PB_HRLS) 2>/dev/null
