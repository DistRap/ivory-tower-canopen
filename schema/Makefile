IVORY_SRC               ?= ../../ivory
TOWER_SRC               ?= ../../tower
IVORY_TOWER_STM32_SRC   ?= ../../ivory-tower-stm32
IVORY_TOWER_CANOPEN_SRC ?= ../../ivory-tower-canopen

CIDL ?= cidl # --debug
NAME ?= CANOpen # name prefix

default: canopen-schema-native
default: canopen-schema-tower
#default: canopen-schema-elm

test: canopen-schema-native-test
test: canopen-schema-tower-test
test: canopen-schema-elm-test

.PHONY: canopen-schema-native
canopen-schema-native:
	$(CIDL) -b haskell \
	     -o canopen-schema-native \
	     -p canopen-schema-native \
	     -n $(NAME)

.PHONY: canopen-schema-tower
canopen-schema-tower:
	$(CIDL) -b tower \
	     -o canopen-schema-tower \
	     -p canopen-schema-tower \
	     -n $(NAME) \
	     --ivory-repo=$(IVORY_SRC) \
	     --tower-repo=$(TOWER_SRC) \
	     --ivory-tower-stm32-repo=$(IVORY_TOWER_STM32_SRC) \
	     --canopen-repo=$(IVORY_TOWER_CANOPEN_SRC)

.PHONY: canopen-schema-elm
canopen-schema-elm:
	$(CIDL) -b elm \
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
