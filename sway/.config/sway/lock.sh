#!/bin/sh

BLANK='#000000'
CLEAR='#000000'
DEFAULT='#999977'
TEXT='#ffffff'
RIGHT='#99ff99'
WRONG='#ff9999'
VERIFYING='#9999ff'

swaylock \
	-c "#000000" \
	--inside-ver-color=$CLEAR \
	--ring-ver-color=$VERIFYING \
	\
	--inside-wrong-color=$CLEAR \
	--ring-wrong-color=$WRONG \
	\
	--inside-color=$BLANK \
	--ring-color=$DEFAULT \
	--line-color=$BLANK \
	--separator-color=$DEFAULT \
	\
	--text-ver-color=$TEXT \
	--text-wrong-color=$TEXT \
	--layout-text-color=$TEXT \
	--key-hl-color=$RIGHT \
	--bs-hl-color=$WRONG
