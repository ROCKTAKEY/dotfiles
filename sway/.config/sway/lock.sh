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
	--insidever-color=$CLEAR \
	--ringver-color=$VERIFYING \
	\
	--insidewrong-color=$CLEAR \
	--ringwrong-color=$WRONG \
	\
	--inside-color=$BLANK \
	--ring-color=$DEFAULT \
	--line-color=$BLANK \
	--separator-color=$DEFAULT \
	\
	--verif-color=$TEXT \
	--wrong-color=$TEXT \
	--time-color=$TEXT \
	--date-color=$TEXT \
	--layout-color=$TEXT \
	--keyhl-color=$RIGHT \
	--bshl-color=$WRONG \
	\
	--clock \
	--indicator \
	--time-str="%H:%M:%S" \
	--date-str="%A, %Y-%m-%d" \
	--keylayout 1
