module rec Jisp.Parser.Parser

let parse =
    Preprocessor.preprocess
    >> OurParserC.Input.create
    >> Expression.bareExpression