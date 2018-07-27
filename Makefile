ARDUINO_DIR = /Users/stepcut/projects/arduino/Arduino.app/Contents/Resources/Java

TARGET = dmx
BOARD_TAG    = teensy31
ARDUINO_LIBS = Wire DmxSimple
F_CPU=72000000

include /Users/stepcut/projects/arduino/Arduino-Makefile/Teensy.mk
