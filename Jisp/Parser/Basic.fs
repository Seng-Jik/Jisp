module Jisp.Parser.Basic

open OurParserC
open Parser


let whitespace = Parsers.charInSeq [' ';'\t';'\r';'\n'] >> Parsed.ignore
let whitespace0 = zeroOrMore whitespace >> Parsed.ignore
let whitespace1 = oneOrMore whitespace >> Parsed.ignore


let lambda = Parsers.character 'λ' >> Parsed.ignore
let openBracket = Parsers.character '(' >> Parsed.ignore
let closeBracket = Parsers.character ')' >> Parsed.ignore
let bindBracket = Parsers.literal "($" >> Parsed.ignore

let alpha = Parsers.charInSeq (Seq.concat [['a'..'z']; ['A'..'Z']])
let number = Parsers.charInSeq ['0'..'9']
let punctuation = Parsers.charInSeq [
    '+';'-';'*';'/';'%';'\\';'>';'<';'=';'!';'|';'&';'#';'?';'_']

let charlist2str = Parsed.map (List.toArray >> fun x -> new System.String (x))

let identifier : string parser =
    (alpha <|> punctuation) <+..> zeroOrMore (alpha <|> number <|> punctuation)
    >> charlist2str
        
