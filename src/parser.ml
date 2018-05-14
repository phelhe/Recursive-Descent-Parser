(* file: parser.ml
  author: Bob Muller

  CS3366 Programming Languages

   This code implements a recursive descent parser for the mini-PL
   Mercury --- the simplest programming language with just integers.

  Terms:

  E ::= E + T | E - T | T
  T ::= T * F | T / F | T % F | F
  F ::= Integer | ( E )
*)

open Token
open Ast
open Debug

open Symbol (*i added this. do i need it???*)

let dbg = Debug.out "eval"
let fmt = Format.sprintf

(*App of {rator : Symbol.t; rands : t list}*)
(*(Ast.PLUS {left = ast; right = ast1})  what i had before on line 35*)

let rec expr tokens =    (*)(Ast.Literal 0, []) (* YOUR CODE HERE *)*)
  let (ast, tokens) = term tokens
  in
  exprprime ast tokens
and exprprime ast tokens =
  match tokens with
  | Token.PLUS :: tokens ->
    let (ast1, tokens) = term tokens
    in
    exprprime (App {rator= Symbol.fromString "+"; rands = [ast;ast1]}) tokens (*my attempt*)
  | Token.MINUS :: tokens ->
    let (ast1, tokens) = term tokens
    in
    exprprime (App {rator = Symbol.fromString "-"; rands = [ast;ast1]}) tokens
  | _ -> (ast, tokens)
and term tokens =
  let (ast, tokens) = factor tokens
  in
  termprime ast tokens
and termprime ast tokens =
  match tokens with
  | Token.TIMES :: tokens ->
    let (ast1, tokens) = factor tokens
    in
    termprime (App {rator = Symbol.fromString "*"; rands = [ast;ast1]}) tokens
  | Token.DIV :: tokens ->
    let (ast1, tokens) = factor tokens
    in
    termprime (App {rator = Symbol.fromString "/"; rands = [ast;ast1]}) tokens
  | Token.MOD :: tokens ->
    let (ast1, tokens) = factor tokens
    in
    termprime (App {rator = Symbol.fromString "%"; rands = [ast;ast1]}) tokens
  | _ -> (ast, tokens)
and factor tokens = (*needs to deal with literals and parens*)
  match tokens with
  | Token.INTEGER i :: tokens -> (Ast.Literal i, tokens)
  | Token.LPAR :: tokens ->
    let (ast, tokens) = expr tokens
    in
    (match tokens with
     | Token.RPAR :: tokens -> (ast, tokens)
     | _ -> failwith "missing closing paren")
  | _ -> (Ast.Literal 1, tokens)



let parser tokens =
  dbg (fmt "tokens = %s" (Token.toStrings tokens));
  match expr tokens with
  | (ast, []) -> ast
  | _ -> failwith "bad syntax, found a parse but there are leftover tokens."
