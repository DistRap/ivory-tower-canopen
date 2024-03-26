CIDL ?= cidl # --debug
NAME ?= CANOpen # name prefix

default: canopen-schema-native
default: canopen-schema-tower

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
	     -n $(NAME)

clean:
	-rm -rf canopen-schema-native
	-rm -rf canopen-schema-tower
