:- consult('menus.pl').
:- consult('board.pl').
:- consult('display.pl').
:- consult('logic.pl').
:- consult('input.pl').
:- consult('utils.pl').
:- use_module(library(system)).
:- use_module(library(lists)).

play :-
      mainMenu.