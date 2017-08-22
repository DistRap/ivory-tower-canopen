IVORY_REPO             ?= ../../ivory
TOWER_REPO             ?= ../../tower
IVORY_TOWER_STM32_REPO ?= ../../ivory-tower-stm32
IVORY_TOWER_CANOPEN    ?= ../../ivory-tower-canopen

GIDL ?= cidl # --debug
NAME ?= CANOpen # name prefix

default: canopen-schema-native
default: canopen-schema-tower
#default: canopen-schema-elm

test: canopen-schema-native-test
test: canopen-schema-tower-test
test: canopen-schema-elm-test

.PHONY: canopen-schema-native
canopen-schema-native:
	$(GIDL) -b haskell \
	     -o canopen-schema-native \
	     -p canopen-schema-native \
	     -n $(NAME)

.PHONY: canopen-schema-tower
canopen-schema-tower:
	$(GIDL) -b tower \
	     -o canopen-schema-tower \
	     -p canopen-schema-tower \
	     -n $(NAME) \
	     --ivory-repo=$(IVORY_REPO) \
	     --tower-repo=$(TOWER_REPO) \
	     --ivory-tower-stm32-repo=$(IVORY_TOWER_STM32_REPO) \
	     --canopen-repo=$(IVORY_TOWER_CANOPEN)

.PHONY: canopen-schema-elm
canopen-schema-elm:
	$(GIDL) -b elm \
	     -o canopen-schema-elm \
	     -p canopen-schema-elm \
	     -n $(NAME)

canopen-schema-native-test: canopen-schema-native
	make -C canopen-schema-native test

canopen-schema-tower-test: canopen-schema-tower
	make -C canopen-schema-tower test

canopen-schema-elm-test: canopen-schema-elm
	# no tests yet

clean:
	-rm -rf canopen-schema-native
	-rm -rf canopen-schema-tower
	-rm -rf canopen-schema-elm
