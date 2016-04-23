open Ast
open Cast
open Ctranslator
open Cprinter
open Helper

exception MainError

let () =
  let _ =
    if (Array.length Sys.argv < 2) || (Array.length Sys.argv > 2) then
      (Format.printf "Usage: autopar <file>\n";
       exit 0) in
  let filearg = 1 in
  let file = open_in (Sys.argv.(filearg)) in
  let lexbuf = Lexing.from_channel file in
  let (gammaenc, estmt) =
    try Parser.program Lexer.token lexbuf
    with Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Format.printf "Syntax error at line %d\n" pos.Lexing.pos_lnum;
      exit 1 in

  let eflatstmt = Helper.flattenseq estmt in
  let cntxt = Ctranslator.translatetoCcontext gammaenc in
  let _ = Cprinter.printCprog  (cntxt, cntxt) in
   ()
