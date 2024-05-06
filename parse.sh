#!/bin/bash
lex ourlex.l

yacc -v -d --warning=none parser.y
gcc y.tab.c -lm

./a.out<quicksort.lf
