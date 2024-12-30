%:- use_module(library(readin)).

:- use_module(library(lists)).          %SICStus

%:- use_module('slibrary\lists.pl').

:-use_module(library(terms)).           %SICStus

:- op(1200, xfx,--->).

:- ensure_loaded('input').
:- ensure_loaded('driver').
:- ensure_loaded('items').
:- ensure_loaded('inference').
:- ensure_loaded('grammars').
:- ensure_loaded('utilities').
:- ensure_loaded('monitor').
