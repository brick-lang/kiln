
module Lexer = Lexer
module ParseTree = ParseTree
module Parser = struct
  include Parser
  module Error = ParserError
end
