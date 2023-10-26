:- consult('menus.pl').
:- consult('board.pl').
:- consult('display.pl').
:- consult('logic.pl').
:- consult('input.pl').
:- consult('bot.pl').
:- consult('utils.pl').
:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(random)).

play :-
      main_menu.