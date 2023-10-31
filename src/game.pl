:- consult('menus.pl').
:- consult('board.pl').
:- consult('display.pl').
:- consult('logic.pl').
:- consult('input.pl').
:- consult('choose_move.pl').
:- consult('value.pl').
:- consult('utils.pl').
:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(aggregate)).

play :-
      main_menu.