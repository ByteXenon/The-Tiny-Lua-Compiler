--[[
  Today we're going to write a compiler for Lua 5.1 in Lua.
  But not just any compiler... a super duper easy and teeny tiny
  compiler! A compiler that is so small that if you remove all the
  comments this file would be ~1700 lines of actual code.

  The compiler will be able to tokenize, parse, and compile (almost)
  any Lua code you throw at it. It will even be able to compile itself!

  So, let's get started!

  ----------------------------------------------------------------------------

  Our journey will cover transforming Lua code into Lua bytecode, which the
  Lua Virtual Machine (VM) can understand and execute.

  Here's a quick breakdown of what we're doing:

   Tokenizer: Breaks down Lua code into tokens, the basic building blocks
    like numbers, strings, and keywords.

   Parser: Converts tokens into an Abstract Syntax Tree (AST), a tree
    representation showing the structure of the code.

   Instruction Generator: Transforms the AST into Lua VM instructions, the
    low-level commands that the Lua VM can execute.

   Compiler: Turns Lua VM instructions into Lua bytecode, ready for
    execution by the Lua VM.

  This process is a bit like translating a book from one language to another,
  then adapting it into a screenplay. Each step refines and transforms the
  content, making it ready for the final audience: the Lua VM.
--]]

--[[
Glossary:
  Token:
    The smallest element of programming language syntax that the compiler recognizes.
    Tokens are the building blocks of code, akin to words in a language, and include
    elements like numbers, strings, keywords (e.g., `if`, `while`), identifiers (variable names),
    and operators (`+`, `-`, `*`, `/`). The tokenizer, or lexical analyzer, scans the source code
    to identify and produce these tokens.

  AST (Abstract Syntax Tree):
    A hierarchical tree representation that outlines the grammatical structure of the code.
    Each node in the tree denotes a construct occurring in the source code. The AST is
    generated from the tokens produced by the tokenizer and serves as a crucial structure
    for further stages of compilation, such as optimization and code generation. It abstracts
    away the syntax details, focusing on the code's logical structure.

  VM (Virtual Machine):
    In the context of programming languages, a VM specifically refers to a runtime engine
    that executes bytecode or intermediate code. This VM is not to be confused with system
    virtual machines (like VirtualBox or VMWare) that emulate a full hardware system;
    it's a process virtual machine designed to execute code in a high-level, portable format.

  Bytecode:
    A form of instruction set designed for efficient execution by a software VM. Bytecode
    is more abstract than machine code and is not tied to any specific hardware architecture.
    It serves as an intermediate representation of the code, optimized for portability and
    quick execution. Bytecode is typically generated from the AST and is executed by the VM.
    Unlike human-readable source code or assembly language, bytecode is binary and is
    intended to be read and understood by the VM rather than humans.

  Proto (Function Prototype):
    In Lua, a function prototype is a data structure that contains metadata about a function,
    including its bytecode, the number of parameters it accepts, its local variables, and its
    upvalues (variables captured from the surrounding scope). The Lua VM uses this information
    to execute the function and manage its execution context. Each Lua function, whether
    defined in Lua or C, is represented internally by a function prototype.

  Scope:
    Defines the visibility and lifetime of variables and parameters in a program. In Lua,
    scope is determined by the location of variable declarations. Variables can be global,
    local, or upvalues. Global variables are accessible from anywhere in the code. Local
    variables have their visibility limited to the block where they are declared, enhancing
    modularity and preventing unintended modifications. Upvalues are local variables from
    an enclosing function's scope, which are captured by closures, allowing the closure to
    access and modify these variables even when the function is executed outside its original scope.
--]]

--[[
    ============================================================================
                                  (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧
                              THE HELPER FUNCTIONS!
    ============================================================================

    Building a compiler from scratch necessitates a set of utility functions to
    streamline common tasks, enhancing both efficiency and readability. Below,
    we introduce two essential helper functions pivotal to our compiler's core
    functionality.
--]]

-- Converts a list to a lookup table for O(1) element lookups
local function createLookupTable(list)
  local lookup = {}
  for _, value in ipairs(list) do
    lookup[value] = true
  end
  return lookup
end

-- Constructs a trie for efficient prefix-based operator searches
local function makeTrie(ops)
  -- Initialize the trie
  local trie = {}
  for _, op in ipairs(ops) do
    local node = trie
    -- Split the operator into individual characters
    for char in op:gmatch(".") do
      node[char] = node[char] or {}
      node = node[char]
    end
    node.Value = op
  end
  return trie
end

--[[
    ============================================================================
                                    (•_•)?!
                              TOKENIZER CONSTANTS
    ============================================================================

    Before diving into the tokenizer's implementation, let's explore the
    essential constants and lookup tables that will guide the tokenization
    process. These constants include Lua operators, escaped character
    sequences, reserved keywords, and Lua's boolean and nil constants.
    By defining these constants upfront, we can streamline the tokenization
    logic and ensure accurate identification and classification of tokens
    within the Lua code.
--]]

-- Lua operators for tokenization: arithmetic, comparison, logical.
local TOKENIZER_LUA_OPERATORS = {
  "^", "*", "/", "%",
  "+", "-", "<", ">",
  "#",

  "<=",  ">=", "==",  "~=",
  "and", "or", "not", ".."
}

-- Maps escaped sequences to characters for string literals.
local TOKENIZER_ESCAPED_CHARACTER_CONVERSIONS = {
  ["a"]  = "\a", -- bell
  ["b"]  = "\b", -- backspace
  ["f"]  = "\f", -- form feed
  ["n"]  = "\n", -- newline
  ["r"]  = "\r", -- carriage return
  ["t"]  = "\t", -- horizontal tab
  ["v"]  = "\v", -- vertical tab

  ["\\"] = "\\", -- backslash
  ["\""] = "\"", -- double quote
  ["\'"] = "\'", -- single quote
}

-- Lookup for Lua's boolean and nil constants.
local TOKENIZER_LUA_CONSTANTS_LOOKUP = createLookupTable({ "true", "false", "nil" })

-- Lookup for Lua's reserved keywords.
local TOKENIZER_RESERVED_KEYWORDS_LOOKUP = createLookupTable({
  "while",    "do",     "end",   "for",
  "local",    "repeat", "until", "return",
  "in",       "if",     "else",  "elseif",
  "function", "then",   "break"
})

-- Lookup for operators.
local TOKENIZER_LUA_OPERATORS_LOOKUP = createLookupTable(TOKENIZER_LUA_OPERATORS)

-- Trie for efficient operator searching.
local TOKENIZER_OPERATOR_TRIE = makeTrie(TOKENIZER_LUA_OPERATORS)

--[[
    ============================================================================
                                 (/^▽^)/
                              THE TOKENIZER!
    ============================================================================

    Imagine the tokenizer as a conveyor belt. You put Lua code in one end,
    and tokens come out the other end. The tokenizer will convert the input
    code into a list of tokens that the parser can understand.
    Tokens are the smallest building blocks of a programming language. They can be
    anything from a number, a string, a keyword, an identifier, or an operator.
    Typically, tokenizers strip out comments and whitespace, as they are not
    needed for the parsing phase, our tokenizer follows the same approach.

    Here's an example of how the tokenizer breaks down a simple Lua script:
    ```lua
    if (x == 10) then
      print("Hello, world!")
    end
    ```
    The resulting tokens would look like this:
    |-----------------|------------------|
    | Type            | Value            |
    |-----------------|------------------|
    | Keyword         | if               |
    | Character       | (                |
    | Identifier      | x                |
    | Operator        | ==               |
    | Number          | 10               |
    | Character       | )                |
    | Keyword         | then             |
    | Identifier      | print            |
    | Character       | (                |
    | String          | Hello, world!    |
    | Character       | )                |
    | Keyword         | end              |
    |-----------------|------------------|
--]]

local Tokenizer = {}
function Tokenizer.tokenize(code)
  --// LOCAL VARIABLES //--
  local charStream, charStreamLen = {}, 0
  for char in code:gmatch(".") do
    charStreamLen = charStreamLen + 1
    charStream[charStreamLen] = char
  end

  local curCharPos = 1
  local curChar = charStream[curCharPos]

  --// CHARACTER NAVIGATION //--
  local function lookAhead(n)
    local updatedCharPos = curCharPos + (n or 1)
    local updatedChar    = charStream[updatedCharPos] or "\0"
    return updatedChar
  end
  local function consume(n)
    local updatedCharPos = curCharPos + (n or 1)
    local updatedChar    = charStream[updatedCharPos] or "\0"
    curCharPos = updatedCharPos
    curChar    = updatedChar
    return updatedChar
  end

  --// CHECKERS //--
  local function isWhitespace(char)
    char = char or curChar
    return char:match("%s")
  end
  local function isNumber(char)
    char = char or curChar
    return char:match("%d")
  end
  local function isIdentifier(char)
    char = char or curChar
    return char:match("[%a%d_]")
  end
  local function isIdentifierStart(char)
    char = char or curChar
    return char:match("[%a_]")
  end
  local function isScientificNotationPrefix(char)
    char = char or curChar
    return char == "e" or char == "E"
  end
  local function isHexadecimalNumberPrefix()
    return curChar == "0" and (lookAhead() == "x" or lookAhead() == "X")
  end
  local function isVarArg()
    return curChar == "." and lookAhead(1) == "." and lookAhead(2) == "."
  end
  local function isComment()
    return curChar == "-" and lookAhead(1) == "-"
  end
  local function isString()
    return (curChar == '"' or curChar == "'")
        or (curChar == "[" and (lookAhead(1) == "[" or lookAhead(1) == "="))
  end

  --// CONSUMERS //--
  local function consumeWhitespace()
    while isWhitespace(lookAhead()) do
      consume()
    end
  end
  local function consumeIdentifier()
    local start = curCharPos
    while isIdentifier(lookAhead()) do
      consume()
    end
    return code:sub(start, curCharPos)
  end
  local function consumeInteger(maxLength)
    local start = curCharPos
    while lookAhead():match("%d") do
      if (maxLength and (curCharPos - start) >= maxLength) then
        break
      end
      consume()
    end
    return code:sub(start, curCharPos)
  end
  local function consumeNumber()
    local start = curCharPos

    -- Hexadecimal number case
    -- 0[xX][0-9a-fA-F]+
    if isHexadecimalNumberPrefix() then
      consume(2) -- Consume the "0x" part
      while isNumber(lookAhead()) or lookAhead():match("[a-fA-F]") do
        consume() -- Consume hexadecimal digits
      end
      return code:sub(start, curCharPos)
    end

    while isNumber(lookAhead()) do
      consume() -- Consume digits
    end

    -- Floating point number case
    -- [0-9]*\.[0-9]+
    if lookAhead() == "." then
      consume() -- Consume the "."
      while isNumber(lookAhead()) do
        consume()
      end
    end

    -- Exponential (scientific) notation case
    -- [eE][+-]?[0-9]+
    if isScientificNotationPrefix(lookAhead()) then
      consume() -- Consume the "e" or "E"
      if lookAhead() == "+" or lookAhead() == "-" then
        consume() -- Consume optional sign
      end
      while isNumber(lookAhead()) do
        consume() -- Consume exponent digits
      end
    end

    return code:sub(start, curCharPos)
  end
  local function consumeSimpleString()
    local delimiter = curChar
    local newString = { }
    consume() -- Consume the delimiter
    while curChar ~= delimiter do
      if curChar == "\\" then
        local nextChar = consume()
        if nextChar:match("%d") then -- Numeric escape sequence?
          local number = consumeInteger(3)
          table.insert(newString, string.char(tonumber(number)))
        elseif TOKENIZER_ESCAPED_CHARACTER_CONVERSIONS[nextChar] then
          table.insert(newString, TOKENIZER_ESCAPED_CHARACTER_CONVERSIONS[nextChar])
        else
          error("invalid escape sequence near '\\" .. nextChar .. "'")
        end
      else
        table.insert(newString, curChar)
      end
      consume()
    end
    return table.concat(newString)
  end
  local function consumeLongString()
    consume() -- Consume the "[" character
    local start = curCharPos
    local depth = 0
    while curChar == "=" do
      consume() -- Consume the "=" character
      depth = depth + 1
    end
    if curChar ~= "[" then
      error("invalid long string delimiter")
    end

    consume() -- Consume the "[" character
    while true do
      if curChar == "]" then
        consume() -- Consume the "]" character
        local closingDepth = 0
        while curChar == "=" do
          consume() -- Consume the "=" character
          closingDepth = closingDepth + 1
        end
        if closingDepth == depth and curChar == "]" then
          -- Exit the loop, as the closing delimiter is fully matched
          break
        end
      elseif curChar == "\0" then
        error("Unclosed long comment")
      end

      consume()
    end

    return code:sub(start + depth + 1, curCharPos - 2 - depth)
  end
  local function consumeString()
    if curChar == "[" then
      return consumeLongString()
    end
    return consumeSimpleString()
  end
  local function consumeOperator()
    local node  = TOKENIZER_OPERATOR_TRIE
    local operator

    -- Trie walker
    local index = 0
    while true do
      local character = lookAhead(index)
      node = node[character] -- Advance to the deeper node
      if not node then break end
      operator = node.Value
      index    = index + 1
    end
    if not operator then return end
    consume(#operator - 1)
    return operator
  end
  local function consumeShortComment()
    while curChar ~= "\0" and curChar ~= "\n" do
      consume()
    end
  end
  local function consumeLongComment()
    consume() -- Consumes the "[" character
    local depth = 0
    while curChar == "=" do
      consume() -- Consume the "=" character
      depth = depth + 1
    end
    if curChar ~= "[" then return consumeShortComment() end
    while true do
      if curChar == "]" then
        consume() -- Consume the "]" character
        local closingDepth = 0
        while curChar == "=" do
          consume() -- Consume the "=" character
          closingDepth = closingDepth + 1
        end
        if curChar == "]" and closingDepth == depth then break end
      elseif curChar == "\0" then
        error("Unclosed long comment")
      end
      consume()
    end
  end
  local function consumeComment()
    consume(2) -- Consume the "--"
    if curChar == "[" then
      return consumeLongComment()
    end
    return consumeShortComment()
  end

  --// TOKENIZERS //--
  local function getNextToken()
    if isWhitespace(curChar) then
      consumeWhitespace()
      return
    elseif isComment() then
      consumeComment()
      return
    elseif isNumber(curChar) then
      return { TYPE = "Number", Value = tonumber(consumeNumber()) }
    elseif isIdentifierStart(curChar) then
      local identifier = consumeIdentifier()
      if TOKENIZER_LUA_OPERATORS_LOOKUP[identifier] then
        return { TYPE = "Operator", Value = identifier }
      elseif TOKENIZER_RESERVED_KEYWORDS_LOOKUP[identifier] then
        return { TYPE = "Keyword", Value = identifier }
      elseif TOKENIZER_LUA_CONSTANTS_LOOKUP[identifier] then
        return { TYPE = "Constant", Value = identifier }
      end
      return { TYPE = "Identifier", Value = identifier }
    elseif isString() then
      return { TYPE = "String", Value = consumeString() }
    elseif isVarArg() then
      consume(2)
      return { TYPE = "VarArg" }
    end

    local operator = consumeOperator()
    if operator then
      return { TYPE = "Operator", Value = operator }
    end
    return { TYPE = "Character", Value = curChar }
  end

  --// MAIN //--
  local function tokenize()
    local tokens = {}
    while curChar ~= "\0" do
      local token = getNextToken()
      if token then
        table.insert(tokens, token)
      end
      consume()
    end
    return tokens
  end

  return tokenize()
end

--[[
    ============================================================================
                                  (•_•)?
                              PARSER CONSTANTS
    ============================================================================

    Before diving into the parser's implementation, let's explore the essential
    constants and lookup tables that will guide the parsing process. These
    constants include Lua operators, unary operators, and stop keywords.
    By defining these constants upfront, we can streamline the parsing logic
    and ensure accurate identification and classification of tokens within the
    Lua code.
--]]

local PARSER_UNARY_OPERATOR_PRECEDENCE = 8
local PARSER_MULTIRET_NODE_TYPES = createLookupTable({ "FunctionCall", "VarArg" })
local PARSER_LVALUE_NODE_TYPES   = createLookupTable({ "Variable", "TableIndex" })
local PARSER_STOP_KEYWORDS       = createLookupTable({ "end", "else", "elseif", "until" })
local PARSER_OPERATOR_PRECEDENCE = {
  ["+"]   = {6, 6},  ["-"]  = {6, 6},
  ["*"]   = {7, 7},  ["/"]  = {7, 7}, ["%"] = {7, 7},
  ["^"]   = {10, 9}, [".."] = {5, 4},
  ["=="]  = {3, 3},  ["~="] = {3, 3},
  ["<"]   = {3, 3},  [">"]  = {3, 3}, ["<="] = {3, 3}, [">="] = {3, 3},
  ["and"] = {2, 2},  ["or"] = {1, 1}
}
local PARSER_LUA_UNARY_OPERATORS  = createLookupTable({ "-", "#", "not" })
local PARSER_LUA_BINARY_OPERATORS = createLookupTable({
  "+",  "-",   "*",  "/",
  "%",  "^",   "..", "==",
  "~=", "<",   ">",  "<=",
  ">=", "and", "or"
})

--[[
    ============================================================================
                                  ヽ/❀o ل͜ o\ﾉ
                                 THE PARSER!!!
    ============================================================================

    The parser is responsible for converting the list of tokens into an
    Abstract Syntax Tree (AST). The AST is a tree representation of the
    structure of the code. Each node in the tree represents a different
    part of the code. For example, a node could represent a function call,
    a binary operation, or a variable declaration. The parser will also
    perform some basic syntax checking to ensure the code is valid.
    One of the most interesting parts of the parser is the expression parser,
    which is responsible for placing operators and operands in the correct
    order based on their precedence and associativity.

    Here's an example of how the parser converts a simple Lua script into an
    Abstract Syntax Tree (AST):
    ```lua
    local x = 10 + 20
    ```
    The resulting AST would look like this:

--]]

local Parser = {}
function Parser.parse(tokens)
  local currentToken      = tokens[1]
  local currentTokenIndex = 1
  local scopeStack        = {}
  local currentScope      = {}

  --// TOKEN NAVIGATION //--
  local function lookAhead(n)
    local updatedTokenIndex = currentTokenIndex + (n or 1)
    local updatedToken      = tokens[updatedTokenIndex]
    return updatedToken
  end
  local function consume(n)
    local updatedTokenIndex = currentTokenIndex + (n or 1)
    local updatedToken      = tokens[updatedTokenIndex]
    currentTokenIndex = updatedTokenIndex
    currentToken      = updatedToken
    return updatedToken
  end

  --// SCOPE MANAGEMENT //--
  local function enterScope(isFunctionScope)
    local scope = {
      localVariables  = {},
      isFunctionScope = isFunctionScope
    }
    table.insert(scopeStack, scope)
    currentScope = scope
    return scope
  end
  local function exitScope()
    scopeStack[#scopeStack] = nil
    currentScope = scopeStack[#scopeStack]
  end

  --// IN-SCOPE VARIABLE MANAGEMENT //--
  local function declareLocalVariable(variable)
    currentScope.localVariables[variable] = true
  end
  local function declareLocalVariables(variables)
    for _, variable in ipairs(variables) do
      declareLocalVariable(variable)
    end
  end
  local function getVariableType(variableName)
    local isUpvalue = false
    for scopeIndex = #scopeStack, 1, -1 do
      local scope = scopeStack[scopeIndex]
      if scope.localVariables[variableName] then
        local variableType = (isUpvalue and "Upvalue") or "Local"
        return variableType, scopeIndex
      elseif scope.isFunctionScope then
        isUpvalue = true
      end
    end
    return "Global"
  end

  --// TOKEN CHECKERS //--
  local function checkCharacter(character, token)
    token = token or currentToken
    return token
          and token.TYPE  == "Character"
          and token.Value == character
  end
  local function checkKeyword(keyword, token)
    token = token or currentToken
    return token
          and token.TYPE  == "Keyword"
          and token.Value == keyword
  end
  local function isComma(token)
    return token
          and token.TYPE == "Character"
          and token.Value == ","
  end
  local function isUnaryOperator(token)
    return token
          and token.TYPE == "Operator"
          and PARSER_LUA_UNARY_OPERATORS[token.Value]
  end
  local function isBinaryOperator(token)
    return token
          and token.TYPE == "Operator"
          and PARSER_LUA_BINARY_OPERATORS[token.Value]
  end

  --// NODE CHECKERS //--
  local function isValidAssignmentLvalue(node)
    return PARSER_LVALUE_NODE_TYPES[node.TYPE]
  end
  local function isMultiretNode(node)
    return PARSER_MULTIRET_NODE_TYPES[node.TYPE]
  end

  --// EXPECTORS //--
  local function expectTokenType(expectedType, skipConsume)
    local actualType = currentToken and currentToken.TYPE or "nil"
    assert(actualType == expectedType, string.format("Expected a %s, got: %s", expectedType, actualType))
    if not skipConsume then consume() end
    return currentToken
  end
  local function expectCharacter(character, skipConsume)
    local actualType = currentToken and currentToken.TYPE or "nil"
    assert(currentToken and currentToken.TYPE == "Character", "Expected a character, got: " .. actualType)
    assert(currentToken.Value == character, "Expected '" .. character .. "'")
    if not skipConsume then consume() end
    return currentToken
  end
  local function expectKeyword(keyword, skipConsume)
    local actualType = currentToken and currentToken.TYPE or "nil"
    assert(currentToken and currentToken.TYPE == "Keyword", "Expected a keyword, got: " .. actualType)
    assert(currentToken.Value == keyword, "Expected '" .. keyword .. "'")
    if not skipConsume then consume() end
    return currentToken
  end

  --// AUXILIARY FUNCTIONS //--
  local function createNilNode()
    return { TYPE = "Constant", Value = "nil" }
  end
  local function adjustMultiretNodes(nodeList, expectedReturnAmount)
    local lastNode = nodeList[#nodeList]
    local extraReturns = expectedReturnAmount - #nodeList
    if lastNode and isMultiretNode(lastNode) then
      extraReturns = math.max(extraReturns + 1, -1)
      -- Adjust the return value amount
      lastNode.ReturnValueAmount = extraReturns
    else
      for _ = 1, extraReturns do
        table.insert(nodeList, createNilNode())
      end
    end
  end

  --// PARSERS //--
  local getNextNode, parseCodeBlock
  local consumeExpression, consumeExpressions
  local function consumeIdentifierList()
    local identifiers = {}
    while currentToken.TYPE == "Identifier" do
      table.insert(identifiers, currentToken.Value)
      if not isComma(lookAhead()) then break end
      consume(2) -- Consume identifier and ","
    end
    return identifiers
  end
  local function consumeParameterList()
    expectCharacter("(")
    local parameters, isVarArg = {}, false
    while not checkCharacter(")") do
      if currentToken.TYPE == "Identifier" then
        table.insert(parameters, currentToken.Value)
      elseif currentToken.TYPE == "VarArg" then
        isVarArg = true
        consume() -- Consume the "..."
        break
      end
      consume() -- Consume the last token of the parameter
      if not isComma(currentToken) then break end
      consume() -- Consume the comma
    end
    expectCharacter(")")
    return parameters, isVarArg
  end
  local function consumeTableIndex(currentExpression)
    consume() -- Consume the "." symbol
    local indexToken = { TYPE = "String", Value = currentToken.Value }
    return { TYPE = "TableIndex", Index = indexToken, Expression = currentExpression }
  end
  local function consumeBracketTableIndex(currentExpression)
    consume() -- Consume the "[" symbol
    local indexExpression = consumeExpression()
    consume() -- Consume the last token of the index expression
    expectCharacter("]", true)
    return { TYPE = "TableIndex", Index = indexExpression, Expression = currentExpression }
  end
  local function consumeTable()
    consume() -- Consume the "{" symbol
    local elements            = {}
    local implicitElements    = {}
    local explicitElements    = {}
    local internalImplicitKey = 1

    -- Loop until we find a "}" (end of the table)
    while not checkCharacter("}") do
      local key, value
      local isImplicitKey = false
      if checkCharacter("[") then
        -- [<expression>] = <expression>
        consume() -- Consume "["
        key = consumeExpression()
        consume() -- Consume the last token of the key
        expectCharacter("]")
        expectCharacter("=")
        value = consumeExpression()
      elseif currentToken.TYPE == "Identifier" and checkCharacter("=", lookAhead()) then
        -- <identifier> = <expression>
        key = { TYPE = "String", Value = currentToken.Value }
        consume(2) -- Consume key and "="
        value = consumeExpression()
      else
        -- <expression>
        key = { TYPE = "Number", Value = internalImplicitKey }
        internalImplicitKey = internalImplicitKey + 1
        isImplicitKey = true
        value = consumeExpression()
      end
      local element = { Key = key, Value = value, IsImplicitKey = isImplicitKey }
      local tableToInsert = (isImplicitKey and implicitElements) or explicitElements
      table.insert(tableToInsert, element)
      table.insert(elements, element)

      consume() -- Consume the last token of the expression
      -- Table elements can be separated by "," or ";"
      local shouldContinue = checkCharacter(",") or checkCharacter(";")
      if not shouldContinue then break end
      consume() -- Consume ","
    end
    local lastElement = elements[#elements]
    if lastElement and lastElement.IsImplicitKey then
      local lastElementValue = lastElement.Value.Value
      if isMultiretNode(lastElementValue) then
        lastElementValue.ReturnValueAmount = -1
      end
    end

    return { TYPE = "Table",
      Elements         = elements,
      ImplicitElements = implicitElements,
      ExplicitElements = explicitElements }
  end
  local function consumeFunctionCall(currentExpression)
    consume() -- Consume the "("
    local arguments = consumeExpressions()
    adjustMultiretNodes(arguments, -1)
    consume() -- Consume the last token of the expression
    return { TYPE = "FunctionCall", Expression = currentExpression, Arguments = arguments, ReturnValueAmount = 1, WithSelf = false }
  end
  local function consumeImplicitFunctionCall(lvalue)
    local currentTokenType = currentToken.TYPE

    -- <string>?
    if currentTokenType == "String" then
      local arguments = { currentToken }
      return { TYPE = "FunctionCall", Expression = lvalue, Arguments = arguments, ReturnValueAmount = 1, WithSelf = false }
    end

    -- <table>
    local arguments = { consumeTable() }
    return { TYPE = "FunctionCall", Expression = lvalue, Arguments = arguments, ReturnValueAmount = 1, WithSelf = false }
  end
  local function consumeMethodCall(currentExpression)
    local methodIdentifier = consume().Value -- Consume the ":" character, and get the method identifier
    consume() -- Consume the method identifier
    local methodIndexNode = { TYPE = "TableIndex", Index = { TYPE = "String", Value = methodIdentifier }, Expression = currentExpression }
    local functionCallNode = consumeFunctionCall(methodIndexNode)
    functionCallNode.WithSelf = true -- Mark the function call as a method call
    return functionCallNode
  end
  local function consumeOptionalSemilcolon()
    local nextToken = lookAhead()
    if checkCharacter(";", nextToken) then
      consume()
    end
  end

  --// EXPRESSSION PARSERS //--
  local parsePrimaryExpression, parseSuffixExpression,
        parsePrefixExpression,  parseUnaryOperator,
        parseBinaryExpression
  function parsePrimaryExpression()
    if not currentToken then return end
    local tokenType = currentToken.TYPE
    local tokenValue = currentToken.Value

    if tokenType == "Number" or tokenType == "String" or tokenType == "Constant" then
      return currentToken
    elseif tokenType == "VarArg" then
      currentToken.ReturnValueAmount = 1
      return currentToken
    elseif tokenType == "Identifier" then
      local variableType = getVariableType(tokenValue)
      local variableNode = { TYPE = "Variable", Value = tokenValue, VariableType = variableType }
      return variableNode
    elseif tokenType == "Character" then
      if tokenValue == "(" then -- Parenthesized expression
        consume() -- Consume the parenthesis
        local expression = consumeExpression()
        consume() -- Consume the last token of the expression
        return expression
      elseif tokenValue == "{" then -- Table constructor
        return consumeTable()
      end
    elseif tokenType == "Keyword" then
      if tokenValue == "function" then
        consume() -- Consume the "function" token
        local parameters, isVarArg = consumeParameterList()
        local codeblock = parseCodeBlock(true, parameters)
        expectKeyword("end", true)
        return { TYPE = "Function", CodeBlock = codeblock, Parameters = parameters, IsVarArg = isVarArg }
      end
    end
    return nil
  end
  function parseSuffixExpression(primaryExpression)
    local nextToken = lookAhead()
    local nextTokenValue = nextToken and nextToken.Value
    if nextTokenValue == "(" then -- Function call
      consume()
      -- <expression> \( <args> \)
      return consumeFunctionCall(primaryExpression)
    elseif nextTokenValue == "." then -- Table access
      consume()
      -- <expression> \. <identifier>
      return consumeTableIndex(primaryExpression)
    elseif nextTokenValue == ":" then -- Method call
      consume()
      -- <expression> \: <identifier> \( <args> \)
      return consumeMethodCall(primaryExpression)
    elseif nextTokenValue == "[" then -- Table index
      consume()
      -- <expression> \[ <expression> \]
      return consumeBracketTableIndex(primaryExpression)
    elseif nextToken then
      -- In some edge cases, a user may call a function using only string,
      -- example: `print "Hello, World!"`. This is a valid Lua syntax.
      -- Let's handle both strings and tables here for that case.
      local nextTokenType = nextToken.TYPE
      if nextTokenType == "String" or (nextTokenValue == "{" and nextTokenType == "Character") then
        consume()
        return consumeImplicitFunctionCall(primaryExpression)
      end
    end
    return nil
  end
  function parsePrefixExpression()
    local primaryExpression = parsePrimaryExpression() -- <primary>
    if not primaryExpression then return end

    -- <suffix>*
    while (true) do
      local newExpression = parseSuffixExpression(primaryExpression)
      if not newExpression then break end
      primaryExpression = newExpression
    end

    return primaryExpression
  end
  function parseUnaryOperator()
    local unaryOperator = currentToken
    -- <unary> ::= <unary operator> <unary> | <primary>
    if not isUnaryOperator(currentToken) then
      return parsePrefixExpression()
    end

    -- <unary operator> <unary>
    consume() -- Consume the operator
    local expression = parseBinaryExpression(PARSER_UNARY_OPERATOR_PRECEDENCE)
    return { TYPE = "UnaryOperator", Operator = unaryOperator.Value, Operand = expression }
  end
  function parseBinaryExpression(minPrecedence)
    -- <binary> ::= <unary> <binary operator> <binary> | <unary>
    minPrecedence = minPrecedence or 0
    local expression = parseUnaryOperator() -- <unary>
    if not expression then return end

    -- [<binary operator> <binary>]
    while true do
      local operatorToken = lookAhead()
      local precedence = operatorToken and PARSER_OPERATOR_PRECEDENCE[operatorToken.Value]
      if not isBinaryOperator(operatorToken) or precedence[1] <= minPrecedence then
        break
      end

      -- The <binary operator> <binary> part itself
      local nextToken = consume(2) -- Advance to and consume the operator
      if not nextToken then error("Unexpected end") end

      local right = parseBinaryExpression(precedence[2])
      if not right then error("Unexpected end") end

      expression = { TYPE = "BinaryOperator",
        Operator = operatorToken.Value,
        Left = expression, Right = right }
    end
    return expression
  end
  function consumeExpression(returnRawNode)
    local expression = parseBinaryExpression(0)
    if not expression then
      -- Backtrack to the last token
      consume(-1)
      return
    end
    if returnRawNode then return expression end
    return { TYPE = "Expression", Value = expression }
  end
  function consumeExpressions()
    local expressions = { consumeExpression(true) }
    if #expressions == 0 then return {} end

    local nextToken = lookAhead()
    while isComma(nextToken) do
      consume(2) -- Consume the last token of the last expression and ","
      local expression = consumeExpression(true)
      table.insert(expressions, expression)
      nextToken = lookAhead()
    end

    return expressions
  end

  --// STATEMENT PARSERS //--
  local function parseLocal()
    consume() -- Consume the "local" token
    if checkKeyword("function") then
      consume() -- Consume the "function" token
      local name = currentToken.Value
      consume() -- Consume the last token of the identifier)
      local parameters, isVarArg = consumeParameterList()
      declareLocalVariable(name)
      local codeblock = parseCodeBlock(true, parameters)
      expectKeyword("end", true)
      return { TYPE = "LocalFunctionDeclaration", Name = name, CodeBlock = codeblock, Parameters = parameters, IsVarArg = isVarArg }
    end
    local variables = consumeIdentifierList()
    if checkCharacter("=", lookAhead()) then
      consume() -- Consume the last token of the last identifier
      expectCharacter("=")
      local expressions = consumeExpressions()
      adjustMultiretNodes(expressions, #variables)
      declareLocalVariables(variables)
      return { TYPE = "LocalDeclaration", Variables = variables, Expressions = expressions }
    end
    declareLocalVariables(variables)
    return { TYPE = "LocalDeclaration", Variables = variables, Expressions = {} }
  end
  local function parseWhile()
    consume() -- Consume the "while" token
    local condition = consumeExpression()
    consume() -- Consume the last token of the condition
    expectKeyword("do")
    local codeblock = parseCodeBlock()
    expectKeyword("end", true)
    return { TYPE = "WhileLoop", Condition = condition, CodeBlock = codeblock }
  end
  local function parseRepeat()
    consume() -- Consume the "repeat" token
    local codeblock = parseCodeBlock()
    expectKeyword("until")
    local condition = consumeExpression()
    return { TYPE = "RepeatLoop", CodeBlock = codeblock, Condition = condition }
  end
  local function parseDo()
    consume() -- Consume the "do" token
    local codeblock = parseCodeBlock()
    expectKeyword("end", true)
    return { TYPE = "DoBlock", CodeBlock = codeblock }
  end
  local function parseReturn()
    consume() -- Consume the "return" token
    local expressions = consumeExpressions()
    adjustMultiretNodes(expressions, -1)
    return { TYPE = "ReturnStatement", Expressions = expressions }
  end
  local function parseBreak()
    return { TYPE = "BreakStatement" }
  end
  local function parseIf()
    consume() -- Consume the "if" token
    local ifCondition = consumeExpression()
    consume() -- Consume the last token of the if condition
    expectKeyword("then")
    local ifCodeBlock = parseCodeBlock()
    local branches = { { Condition = ifCondition, CodeBlock = ifCodeBlock } }
    while checkKeyword("elseif") do
      consume() -- Consume the "elseif" token
      local elseifCondition = consumeExpression()
      consume() -- Consume the last token of the elseif condition
      expectKeyword("then")
      local elseifCodeBlock = parseCodeBlock()
      table.insert(branches, { Condition = elseifCondition, CodeBlock = elseifCodeBlock })
    end
    local elseCodeBlock
    if checkKeyword("else") then
      consume() -- Consume the "else" token
      elseCodeBlock = parseCodeBlock()
    end
    expectKeyword("end", true)
    return { TYPE = "IfStatement", Branches = branches, ElseCodeBlock = elseCodeBlock }
  end
  local function parseFor()
    consume() -- Consume the "for" token
    local variableName = expectTokenType("Identifier", true).Value
    consume() -- Consume the variable name
    if checkCharacter(",") or checkKeyword("in") then
      local iteratorVariables = { variableName }
      while checkCharacter(",") do
        consume() -- Consume the comma
        local newVariableName = expectTokenType("Identifier", true).Value
        table.insert(iteratorVariables, newVariableName)
        consume() -- Consume the variable name
      end
      expectKeyword("in")
      local expressions = consumeExpressions()
      adjustMultiretNodes(expressions, 3)
      consume() -- Consume the last token of the expressions
      expectKeyword("do")
      local codeblock = parseCodeBlock(false, iteratorVariables)
      expectKeyword("end", true)
      return { TYPE = "GenericForLoop", IteratorVariables = iteratorVariables, Expressions = expressions, CodeBlock = codeblock }
    end
    expectCharacter("=")
    local expressions = consumeExpressions()
    consume() -- Consume the last token of the expressions
    expectKeyword("do")
    local codeblock = parseCodeBlock(false, { variableName })
    expectKeyword("end", true)
    return { TYPE = "NumericForLoop", VariableName = variableName, Expressions = expressions, CodeBlock = codeblock }
  end
  local function parseFunction()
    consume() -- Consume the "function" token
    local variableName = expectTokenType("Identifier", true).Value
    local variableType = getVariableType(variableName)
    local expression = { TYPE = "Variable", Value = variableName, VariableType = variableType }
    local fields, isMethod = { }, false
    while consume() do
      if checkCharacter(".") then
        consume() -- Consume the "."
        local fieldName = expectTokenType("Identifier", true).Value
        table.insert(fields, fieldName)
      elseif checkCharacter(":") then
        consume() -- Consume the ":"
        local methodName = expectTokenType("Identifier", true).Value
        table.insert(fields, methodName)
        isMethod = true
        consume() -- Consume the method name
        break
      else break end
    end
    local parameters, isVarArg = consumeParameterList()
    if isMethod then
      table.insert(parameters, 1, "self")
    end
    local codeblock = parseCodeBlock(true, parameters)
    expectKeyword("end", true)
    return { TYPE = "FunctionDeclaration",
      Expression = expression,
      Fields = fields,
      IsMethod = isMethod,
      CodeBlock = codeblock,
      Parameters = parameters,
      IsVarArg = isVarArg
    }
  end
  local function parseAssignment(lvalue)
    local lvalues = { lvalue }
    consume() -- Consume the last token of the lvalue
    while isComma(currentToken) do
      consume() -- Consume the comma
      local nextLValue = parsePrefixExpression()
      if not nextLValue then error("Expected an lvalue") end
      if not isValidAssignmentLvalue(nextLValue) then
        error("Expected a variable or index, got: " .. nextLValue.TYPE)
      end
      table.insert(lvalues, nextLValue)
      consume() -- Consume the last token of the lvalue
    end
    expectCharacter("=")
    local expressions = consumeExpressions()
    adjustMultiretNodes(expressions, #lvalues)
    return { TYPE = "VariableAssignment", LValues = lvalues, Expressions = expressions }
  end
  local function parseFunctionCallOrVariableAssignment()
    local lvalue = parsePrefixExpression()
    local lvalueType = tostring(lvalue and lvalue.TYPE)
    if lvalue then
      if isValidAssignmentLvalue(lvalue) then
        return parseAssignment(lvalue)
      elseif lvalueType == "FunctionCall" then
        lvalue.ReturnValueAmount = 0
        return lvalue
      end
      error("Unexpected lvalue type: " .. lvalueType)
    end
    error("Expected an lvalue, got: " .. lvalueType)
  end

  --// CODE BLOCK PARSERS //--
  function getNextNode()
    local currentTokenValue = currentToken.Value
    local currentTokenType  = currentToken.TYPE
    local node
    if currentTokenType == "Keyword" then
      if PARSER_STOP_KEYWORDS[currentTokenValue] then return
      elseif currentTokenValue == "local"        then node = parseLocal()
      elseif currentTokenValue == "while"        then node = parseWhile()
      elseif currentTokenValue == "repeat"       then node = parseRepeat()
      elseif currentTokenValue == "do"           then node = parseDo()
      elseif currentTokenValue == "return"       then node = parseReturn()
      elseif currentTokenValue == "break"        then node = parseBreak()
      elseif currentTokenValue == "if"           then node = parseIf()
      elseif currentTokenValue == "for"          then node = parseFor()
      elseif currentTokenValue == "function"     then node = parseFunction()
      else error("Unsupported keyword: " .. currentTokenValue) end
    else
      node = parseFunctionCallOrVariableAssignment()
    end

    consumeOptionalSemilcolon()
    return node
  end
  function parseCodeBlock(isFunctionScope, variablesInCodeBlock)
    enterScope(isFunctionScope)
    if variablesInCodeBlock then
      declareLocalVariables(variablesInCodeBlock)
    end
    local nodeList = { TYPE = "Group" }
    while currentToken do
      local node = getNextNode()
      if not node then break end
      table.insert(nodeList, node)
      consume()
    end
    exitScope()
    return nodeList
  end

  --// MAIN //--
  local function parse()
    local ast = parseCodeBlock()
    ast.TYPE = "AST"
    return ast
  end

  return parse()
end

--[[
    ============================================================================
                                     (•_•)?
                          INSTRUCTION GENERATOR CONSTANTS
    ============================================================================

    Before diving into the compiler's implementation, let's explore the essential
    constants and lookup tables that will guide the compilation process. These
    constants include Lua operators, unary operators, and stop keywords.
    By defining these constants upfront, we can streamline the compilation logic
    and ensure accurate identification and classification of tokens within the
    Lua code.
--]]
local unpack = (unpack or table.unpack)

local COMPILER_SETLIST_MAX = 50
local COMPILER_SIMPLE_ARICHMETIC_OPERATOR_LOOKUP = {
  ["+"] = "ADD", ["-"] = "SUB",
  ["*"] = "MUL", ["/"] = "DIV",
  ["%"] = "MOD", ["^"] = "POW"
}
local COMPILER_UNARY_OPERATOR_LOOKUP = { ["-"] = "UNM", ["#"] = "LEN", ["not"] = "NOT" }
local COMPILER_COMPARISON_INSTRUCTION_LOOKUP = {
  ["=="] = {"EQ", 1}, ["~="] = {"EQ", 0},
  ["<"]  = {"LT", 1}, [">"]  = {"LT", 1},
  ["<="] = {"LE", 1}, [">="] = {"LE", 1}
}
local COMPILER_COMPARISON_OPERATOR_LOOKUP = createLookupTable({"==", "~=", "<", ">", "<=", ">="})
local COMPILER_CONTROL_FLOW_OPERATOR_LOOKUP = createLookupTable({"and", "or"})

--[[
    ============================================================================
                                 (づ｡◕‿‿◕｡)づ
                         THE INSTRUCTION GENERATOR!!!
    ============================================================================

    Possibly the most complex part of the compiler, the Instruction Generator is
    responsible for converting the AST into Lua instructions, which are
    similar to assembly instructions, but they are much higher level,
    because they're being executed in the Lua VM (Virtual Machine),
    not on a physical CPU. The Instruction Generator will also be responsible
    for generating the function prototypes, which are used to store
    information about the function, such as the number of arguments,
    the number of local variables, and the number of upvalues.

    Here's an example of how the Instruction Generator converts a simple AST
    into Lua instructions:
    ```lua
 
    ```
    The resulting proto would look like this:

--]]
local InstructionGenerator = {}
function InstructionGenerator.generate(ast)
  local breakInstructions
  local locals, currentScope
  local scopes = {}
  local currentProto
  local takenRegisters, code, constants,
        constantLookup, upvalues, upvalueLookup,
        protos

  --// PROTO MANAGEMENT //--
  local function setProto(proto)
    currentProto   = proto
    takenRegisters = proto.takenRegisters
    code           = proto.code
    constants      = proto.constants
    constantLookup = proto.constantLookup
    upvalues       = proto.upvalues
    upvalueLookup  = proto.upvalueLookup
    protos         = proto.protos
  end
  local function newProto()
    currentProto = {
      takenRegisters = {},
      code           = {},
      constants      = {},
      constantLookup = {},
      upvalues       = {},
      upvalueLookup  = {},
      protos         = {},
      numParams      = 0,
      isVarArg       = false,
      functionName   = "@tlc",
    }
    setProto(currentProto)
    return currentProto
  end

  --// REGISTER MANAGEMENT //--
  local function allocateRegister()
    local newRegister = (takenRegisters[0] and #takenRegisters + 1) or 0
    if newRegister > 255 then
      error("Out of registers")
    end
    takenRegisters[newRegister] = true
    return newRegister
  end
  local function deallocateRegister(register)
    takenRegisters[register] = nil
  end
  local function deallocateRegisters(registers)
    for _, register in ipairs(registers) do
      takenRegisters[register] = nil
    end
  end

  --// VARIABLE MANAGEMENT //--
  local function getVariableType(variableName)
    local scope = currentScope
    local isUpvalue = false
    while scope do
      if scope.locals[variableName] then
        return (isUpvalue and "Upvalue") or "Local"
      elseif scope.isFunctionScope then
        isUpvalue = true
      end
      scope = scope.previousScope
    end
    return "Global"
  end
  local function findVariableRegister(localName)
    local scope = currentScope
    while scope do
      local variableRegister = scope.locals[localName]
      if variableRegister then
        return variableRegister
      elseif scope.isFunctionScope then
        break
      end
      local previousScope = scope.previousScope
      scope = previousScope
    end
    error("Could not find variable: " .. localName)
  end
  local function registerVariable(localName, register)
    locals[localName] = register
  end
  local function unregisterVariable(localName)
    deallocateRegister(locals[localName])
    locals[localName] = nil
  end
  local function unregisterVariables(variables)
    for _, variable in ipairs(variables) do
      unregisterVariable(variable)
    end
  end

  --// SCOPE MANAGEMENT //--
  local function enterScope(isFunctionScope)
    local newScope = {
      locals = {},
      isFunctionScope = isFunctionScope,
      previousScope = scopes[#scopes]
    }
    locals = newScope.locals
    table.insert(scopes, newScope)
    currentScope = newScope
    return newScope
  end
  local function exitScope()
    table.remove(scopes)
    for variableName in pairs(currentScope.locals) do
      unregisterVariable(variableName)
    end
    if #scopes > 0 then
      currentScope = scopes[#scopes]
      locals = currentScope.locals
    end
  end

  --// UTILITY FUNCTIONS //--
  local function isMultiretNode(node)
    if not node then return false end
    return PARSER_MULTIRET_NODE_TYPES[node.TYPE]
  end
  local function updateJumpInstruction(instructionIndex)
    local currentInstructionIndex = #code
    local jumpDistance = currentInstructionIndex - instructionIndex
    local instruction = code[instructionIndex]
    instruction[3] = jumpDistance
  end
  local function updateJumpInstructions(list)
    for _, instructionIndex in ipairs(list) do
      updateJumpInstruction(instructionIndex)
    end
  end
  local function findOrCreateConstant(value)
    if constantLookup[value] then
      return constantLookup[value]
    end
    table.insert(constants, value)
    local constantIndex = -(#constants)
    constantLookup[value] = constantIndex
    return constantIndex
  end
  local function findOrCreateUpvalue(value)
    if upvalueLookup[value] then
      return upvalueLookup[value]
    end
    table.insert(upvalues, value)
    local upvalueIndex = #upvalues - 1
    upvalueLookup[value] = upvalueIndex
    return upvalueIndex
  end
  local function addInstruction(opname, a, b, c)
    local instruction = { opname, a, b, c }
    table.insert(code, instruction)
    return #code
  end

  local processExpressionNode, processExpressionNodes, processStatementNode,
        processCodeBlock, processFunctionCodeBlock, processFunction

  --// EXPRESSION COMPILERS //--
  local function compileNumberNode(node, expressionRegister)
    local constantIndex = findOrCreateConstant(node.Value)
    -- OP_LOADK [A, Bx]    R(A) := Kst(Bx)
    addInstruction("LOADK", expressionRegister, constantIndex)
    return expressionRegister
  end
  local function compileStringNode(node, expressionRegister)
    local constantIndex = findOrCreateConstant(node.Value)
    -- OP_LOADK [A, Bx]    R(A) := Kst(Bx)
    addInstruction("LOADK", expressionRegister, constantIndex)
    return expressionRegister
  end
  local function compileFunctionNode(node, expressionRegister)
    processFunction(node, expressionRegister)
    return expressionRegister
  end
  local function compileFunctionCallNode(node, expressionRegister)
    local selfArgumentRegister
    if node.WithSelf then
      local nodeExpressionIndex      = node.Expression.Index
      local nodeExpressionExpression = node.Expression.Expression
      processExpressionNode(nodeExpressionExpression, expressionRegister)
      selfArgumentRegister = allocateRegister()
      local nodeIndexRegister = processExpressionNode(nodeExpressionIndex)
      -- OP_SELF [A, B, C]    R(A+1) := R(B) R(A) := R(B)[RK(C)]
      addInstruction("SELF", expressionRegister, expressionRegister, nodeIndexRegister)
      deallocateRegister(nodeIndexRegister)
    else
      processExpressionNode(node.Expression, expressionRegister)
    end
    local argumentRegisters = processExpressionNodes(node.Arguments)
    if selfArgumentRegister then
      table.insert(argumentRegisters, 1, selfArgumentRegister)
    end
    local returnAmount   = math.max(0, node.ReturnValueAmount + 1)
    local argumentAmount = #argumentRegisters + 1
    if isMultiretNode(node.Arguments[#node.Arguments]) then
      argumentAmount = 0
    end
    -- OP_CALL [A, B, C]    R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
    addInstruction("CALL", expressionRegister, argumentAmount, returnAmount)
    deallocateRegisters(argumentRegisters)
    local returnRegisters = { expressionRegister }
    for _ = expressionRegister + 1, expressionRegister + node.ReturnValueAmount - 1 do
      table.insert(returnRegisters, allocateRegister())
    end
    return unpack(returnRegisters)
  end
  local function compileConstantNode(node, expressionRegister)
    local nodeValue = node.Value
    if nodeValue ~= "nil" then
      local secondValue = (nodeValue == "true" and 1) or 0
      -- OP_LOADBOOL [A, B, C]    R(A) := (Bool)B if (C) pc++
      addInstruction("LOADBOOL", expressionRegister, secondValue, 0)
    else
      -- OP_LOADNIL [A, B]    R(A) := ... := R(B) := nil
      addInstruction("LOADNIL", expressionRegister, expressionRegister)
    end
    return expressionRegister
  end
  local function compileVarArgNode(node, expressionRegister)
    local returnAmount = node.ReturnValueAmount + 1
    if returnAmount <= 0 then returnAmount = 0 end
    -- OP_VARARG [A, B]    R(A), R(A+1), ..., R(A+B-1) = vararg
    addInstruction("VARARG", expressionRegister, returnAmount)
    local returnRegisters = { expressionRegister }
    for _ = expressionRegister + 1, expressionRegister + node.ReturnValueAmount - 1 do
      table.insert(returnRegisters, allocateRegister())
    end
    return unpack(returnRegisters)
  end
  local function compileTableIndexNode(node, expressionRegister)
    processExpressionNode(node.Expression, expressionRegister)
    local indexRegister = processExpressionNode(node.Index)
    -- OP_GETTABLE [A, B, C]    R(A) := R(B)[RK(C)]
    addInstruction("GETTABLE", expressionRegister, expressionRegister, indexRegister)
    deallocateRegister(indexRegister)
    return expressionRegister
  end
  local function compileTableNode(node, expressionRegister)
    local implicitElements = node.ImplicitElements
    local explicitElements = node.ExplicitElements
    local sizeB = math.min(#implicitElements, 255)
    local sizeC = math.min(#explicitElements, 255)
    -- OP_NEWTABLE [A, B, C]    R(A) := {} (size = B,C)
    addInstruction("NEWTABLE", expressionRegister, sizeB, sizeC)
    for _, element in ipairs(explicitElements) do
      local valueRegister = processExpressionNode(element.Value)
      local keyRegister   = processExpressionNode(element.Key)
      deallocateRegisters({ valueRegister, keyRegister })
      -- OP_SETTABLE [A, B, C]    R(A)[RK(B)] := RK(C)
      addInstruction("SETTABLE", expressionRegister, keyRegister, valueRegister)
    end

    local pages = math.ceil(#implicitElements / COMPILER_SETLIST_MAX)
    for page = 1, pages do
      local startIndex = (page - 1) * COMPILER_SETLIST_MAX + 1
      local endIndex   = math.min(page * COMPILER_SETLIST_MAX, #implicitElements)

      local currentPageRegisters = {}
      for elementIndex = startIndex, endIndex do
        local element       = implicitElements[elementIndex]
        local elementValue  = element.Value
        local valueRegister = processExpressionNode(elementValue)

        table.insert(currentPageRegisters, valueRegister)
      end

      local lastElement               = implicitElements[endIndex]
      local lastElementValue          = lastElement.Value.Value
      local currentPageRegisterAmount = #currentPageRegisters
      if page == pages and isMultiretNode(lastElementValue) then
         -- B = 0: Doesn't have a fixed amount of keys (multiret)
         currentPageRegisterAmount = 0
      end

      -- OP_SETLIST [A, B, C]    R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B
      addInstruction("SETLIST", expressionRegister, currentPageRegisterAmount, page)
      deallocateRegisters(currentPageRegisters)
    end

    return expressionRegister
  end
  local function compileVariableNode(node, expressionRegister)
    local variableType = node.VariableType
    if variableType == "Global" then
      -- OP_GETGLOBAL [A, Bx]    R(A) := Gbl[Kst(Bx)]
      addInstruction("GETGLOBAL", expressionRegister, findOrCreateConstant(node.Value))
    elseif variableType == "Local" then
      local variableRegister = findVariableRegister(node.Value)
      -- OP_MOVE [A, B]    R(A) := R(B)
      addInstruction("MOVE", expressionRegister, variableRegister)
    elseif variableType == "Upvalue" then
      -- OP_GETUPVAL [A, B]    R(A) := UpValue[B]
      addInstruction("GETUPVAL", expressionRegister, findOrCreateUpvalue(node.Value))
    end
    return expressionRegister
  end
  local function compileBinaryOperatorNode(node, expressionRegister)
    local nodeOperator = node.Operator
    if COMPILER_SIMPLE_ARICHMETIC_OPERATOR_LOOKUP[nodeOperator] then
      local opcode = COMPILER_SIMPLE_ARICHMETIC_OPERATOR_LOOKUP[nodeOperator]
      local leftExpressionRegister = processExpressionNode(node.Left)
      local rightExpressionRegister = processExpressionNode(node.Right)
      addInstruction(opcode, expressionRegister, leftExpressionRegister, rightExpressionRegister)
      deallocateRegisters({ leftExpressionRegister, rightExpressionRegister })
    elseif COMPILER_CONTROL_FLOW_OPERATOR_LOOKUP[nodeOperator] then
      local leftExpressionRegister = processExpressionNode(node.Left, expressionRegister)
      local isConditionTrue = (nodeOperator == "and" and 0) or 1
      -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
      addInstruction("TEST", leftExpressionRegister, 0, isConditionTrue)
      -- OP_JMP [A, sBx]    pc+=sBx
      local jumpInstructionIndex = addInstruction("JMP", 0, 0)
      processExpressionNode(node.Right, expressionRegister)
      updateJumpInstruction(jumpInstructionIndex)
    elseif COMPILER_COMPARISON_OPERATOR_LOOKUP[nodeOperator] then
      local leftExpressionRegister = processExpressionNode(node.Left)
      local rightExpressionRegister = processExpressionNode(node.Right)
      local instruction, flag = unpack(COMPILER_COMPARISON_INSTRUCTION_LOOKUP[nodeOperator])
      if nodeOperator == ">" or nodeOperator == ">=" then
        leftExpressionRegister, rightExpressionRegister = rightExpressionRegister, leftExpressionRegister
      end
      addInstruction(instruction, flag, leftExpressionRegister, rightExpressionRegister)
      -- OP_JMP [A, sBx]    pc+=sBx
      addInstruction("JMP", 0, 1)
      -- OP_LOADBOOL [A, B, C]    R(A) := (Bool)B if (C) pc++
      addInstruction("LOADBOOL", expressionRegister, 0, 1)
      addInstruction("LOADBOOL", expressionRegister, 1, 0)
      deallocateRegisters({ leftExpressionRegister, rightExpressionRegister })
    elseif nodeOperator == ".." then
      local leftExpressionRegister = processExpressionNode(node.Left)
      local rightExpressionRegister = processExpressionNode(node.Right)
      if (rightExpressionRegister - leftExpressionRegister) ~= 1 then
        error("Concatenation requires consecutive registers")
      end
      -- OP_CONCAT [A, B, C]    R(A) := R(B).. ... ..R(C)
      addInstruction("CONCAT", expressionRegister, leftExpressionRegister, rightExpressionRegister)
      deallocateRegisters({ leftExpressionRegister, rightExpressionRegister })
    end
    return expressionRegister
  end
  local function compileUnaryOperatorNode(node, expressionRegister)
    local nodeOperator      = node.Operator
    local operatorOpcode    = COMPILER_UNARY_OPERATOR_LOOKUP[nodeOperator]
    local operandExpression = processExpressionNode(node.Operand)
    addInstruction(operatorOpcode, expressionRegister, operandExpression)
    deallocateRegister(operandExpression)
    return expressionRegister
  end

  --// STATEMENT COMPILERS //--
  local function compileBreakStatementNode()
    -- OP_JMP [A, sBx]    pc+=sBx
    local jumpInstructionIndex = addInstruction("JMP", 0, 0)
    table.insert(breakInstructions, jumpInstructionIndex)
  end
  local function compileLocalFunctionDeclarationNode(node)
    local name          = node.Name
    local localRegister = allocateRegister()
    registerVariable(name, localRegister)
    processFunction(node, localRegister, name)
  end
  local function compileFunctionDeclarationNode(node)
    local expression = node.Expression
    local fields     = node.Fields
    if #fields > 0 then
      local closureRegister = allocateRegister()
      local lastField = fields[#fields]
      processFunction(node, closureRegister, lastField)
      local expressionRegister = processExpressionNode(expression)
      for index, field in ipairs(fields) do
        local fieldRegister = allocateRegister()
        -- OP_LOADK [A, Bx]    R(A) := Kst(Bx)
        addInstruction("LOADK", fieldRegister, findOrCreateConstant(field))
        if index == #fields then
          -- OP_SETTABLE [A, B, C]    R(A)[RK(B)] := RK(C)
          addInstruction("SETTABLE", expressionRegister, fieldRegister, closureRegister)
        else
          -- OP_GETTABLE [A, B, C]    R(A) := R(B)[RK(C)]
          addInstruction("GETTABLE", expressionRegister, expressionRegister, fieldRegister)
        end
        deallocateRegister(fieldRegister)
      end
      deallocateRegisters({ expressionRegister, closureRegister })
      return
    end
    local variableName = expression.Value
    if expression.VariableType == "Local" then
      local localRegister = findVariableRegister(variableName)
      processFunction(node, localRegister, variableName)
    elseif expression.VariableType == "Upvalue" then
      local closureRegister = allocateRegister()
      processFunction(node, closureRegister, variableName)
      -- OP_SETUPVAL [A, B]    UpValue[B] := R(A)
      addInstruction("SETUPVAL", closureRegister, findOrCreateUpvalue(variableName))
      deallocateRegister(closureRegister)
    elseif expression.VariableType == "Global" then
      local globalRegister = allocateRegister()
      processFunction(node, globalRegister, variableName)
      -- OP_SETGLOBAL [A, Bx]    Gbl[Kst(Bx)] := R(A)
      addInstruction("SETGLOBAL", globalRegister, findOrCreateConstant(variableName))
      deallocateRegister(globalRegister)
    end
  end
  local function compileLocalDeclarationNode(node)
    local variableExpressionRegisters = {}
    for index, expression in ipairs(node.Expressions) do
      local expressionRegisters = { processExpressionNode(expression) }
      for index2, expressionRegister in ipairs(expressionRegisters) do
        table.insert(variableExpressionRegisters, expressionRegister)
        if not node.Variables[index + index2 - 1] then
          -- If this expression doesn't have a corresponding variable, deallocate it
          deallocateRegister(expressionRegister)
        end
      end
    end
    for index, localName in ipairs(node.Variables) do
      local expressionRegister = variableExpressionRegisters[index]
      if not expressionRegister then
        expressionRegister = allocateRegister()
        -- Load nil into the register
        -- OP_LOADNIL [A, B]    R(A) := ... := R(B) := nil
        addInstruction("LOADNIL", expressionRegister, expressionRegister)
      end
      registerVariable(localName, expressionRegister)
    end
  end
  local function compileNumericForLoopNode(node)
    local variableName  = node.VariableName
    local expressions   = node.Expressions
    local codeblock     = node.CodeBlock
    local startRegister = processExpressionNode(expressions[1])
    local endRegister   = processExpressionNode(expressions[2])
    local stepRegister  = allocateRegister()
    if expressions[3] then
      stepRegister = processExpressionNode(expressions[3], stepRegister)
    else
      -- OP_LOADK [A, Bx]    R(A) := Kst(Bx)
      addInstruction("LOADK", stepRegister, findOrCreateConstant(1))
    end
    -- OP_FORPREP [A, sBx]    R(A)-=R(A+2) pc+=sBx
    local forprepInstructionIndex = addInstruction("FORPREP", startRegister, 0)
    local loopStart = #code
    registerVariable(variableName, startRegister)
    local oldBreakInstructions = breakInstructions
    breakInstructions = {}
    processCodeBlock(codeblock)
    local loopEnd = #code
    updateJumpInstruction(forprepInstructionIndex)
    -- OP_FORLOOP [,A sBx]   R(A)+=R(A+2)
    --                       if R(A) <?= R(A+1) then { pc+=sBx R(A+3)=R(A) }
    addInstruction("FORLOOP", startRegister, loopStart - loopEnd - 1)
    updateJumpInstructions(breakInstructions)
    breakInstructions = oldBreakInstructions
    unregisterVariable(variableName)
    deallocateRegisters({ startRegister, endRegister, stepRegister })
  end
  local function compileGenericForLoopNode(node)
    local iteratorVariables   = node.IteratorVariables
    local expressions         = node.Expressions
    local codeblock           = node.CodeBlock
    local expressionRegisters = processExpressionNodes(expressions)
    -- OP_JMP [A, sBx]    pc+=sBx
    local startJmpInstructionIndex = addInstruction("JMP", 0, 0)
    local forGeneratorRegister = expressionRegisters[1]
    local forStateRegister     = expressionRegisters[2]
    local forControlRegister   = expressionRegisters[3]
    if not (forGeneratorRegister and forStateRegister and forControlRegister) then
      error("Expected 3 expression registers")
    end
    local loopStart = #code
    for _, iteratorVariable in ipairs(iteratorVariables) do
      local iteratorRegister = allocateRegister()
      registerVariable(iteratorVariable, iteratorRegister)
    end
    local oldBreakInstructions = breakInstructions
    breakInstructions = {}
    processCodeBlock(codeblock)
    updateJumpInstruction(startJmpInstructionIndex)
    -- OP_TFORLOOP [A, C]    R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2))
    --                       if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++
    addInstruction("TFORLOOP", forGeneratorRegister, 0, #iteratorVariables)
    -- OP_JMP [A, sBx]    pc+=sBx
    addInstruction("JMP", 0, loopStart - #code - 1)
    updateJumpInstructions(breakInstructions)
    breakInstructions = oldBreakInstructions
    deallocateRegisters(expressionRegisters)
    unregisterVariables(iteratorVariables)
  end
  local function compileReturnStatementNode(node)
    local expressionRegisters = processExpressionNodes(node.Expressions)
    local startRegister       = expressionRegisters[1] or 0
    local returnAmount        = #node.Expressions + 1
    local lastExpression      = node.Expressions[#node.Expressions]
    if isMultiretNode(lastExpression) then
      returnAmount = 0
    end
    -- OP_RETURN [A, B]    return R(A), ... ,R(A+B-2)
    addInstruction("RETURN", startRegister, returnAmount, 0)
    deallocateRegisters(expressionRegisters)
  end
  local function compileWhileLoopNode(node)
    local loopStart         = #code
    local conditionRegister = processExpressionNode(node.Condition)
    -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
    addInstruction("TEST", conditionRegister, 0, 0)
    -- OP_JMP [A, sBx]    pc+=sBx
    local jumpInstructionIndex = addInstruction("JMP", 0, 0)
    deallocateRegister(conditionRegister)
    local oldBreakInstructions = breakInstructions
    breakInstructions = {}
    processCodeBlock(node.CodeBlock)
    -- OP_JMP [A, sBx]    pc+=sBx
    addInstruction("JMP", 0, loopStart - #code - 1)
    updateJumpInstruction(jumpInstructionIndex)
    updateJumpInstructions(breakInstructions)
    breakInstructions = oldBreakInstructions
  end
  local function compileRepeatLoopNode(node)
    local loopStart = #code
    processCodeBlock(node.CodeBlock)
    local conditionRegister = processExpressionNode(node.Condition)
    -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
    addInstruction("TEST", conditionRegister, 0, 0)
    -- OP_JMP [A, sBx]    pc+=sBx
    addInstruction("JMP", 0, loopStart - #code - 1)
    deallocateRegister(conditionRegister)
  end
  local function compileDoBlockNode(node)
    processCodeBlock(node.CodeBlock)
  end
  local function compileIfStatementNode(node)
    local branches      = node.Branches
    local elseCodeBlock = node.ElseCodeBlock
    local jumpToEndInstructions = {}
    for index, branch in ipairs(branches) do
      local condition = branch.Condition
      local codeBlock = branch.CodeBlock
      local conditionRegister = processExpressionNode(condition)
      -- OP_TEST [A, C]    if not (R(A) <=> C) then pc++
      addInstruction("TEST", conditionRegister, 0, 0)
      -- OP_JMP [A, sBx]    pc+=sBx
      local conditionJumpInstructionIndex = addInstruction("JMP", 0, 0)
      deallocateRegister(conditionRegister)
      processCodeBlock(codeBlock)
      if index < #branches or elseCodeBlock then
        -- OP_JMP [A, sBx]    pc+=sBx
        local jumpInstructionIndex = addInstruction("JMP", 0, 0)
        table.insert(jumpToEndInstructions, jumpInstructionIndex)
      end
      updateJumpInstruction(conditionJumpInstructionIndex)
    end
    if elseCodeBlock then
      processCodeBlock(elseCodeBlock)
    end

    updateJumpInstructions(jumpToEndInstructions)
  end
  local function compileVariableAssignmentNode(node)
    local expressionRegisters = processExpressionNodes(node.Expressions)
    for index, lvalue in ipairs(node.LValues) do
      local lvalueType = lvalue.TYPE
      if lvalueType == "Variable" then
        local variableType = lvalue.VariableType
        local variableName = lvalue.Value
        local expressionRegister = expressionRegisters[index]
        if not expressionRegister then error("Expected an expression for assignment") end
        if variableType == "Local" then
          local variableRegister = findVariableRegister(variableName)
          -- OP_MOVE [A, B]    R(A) := R(B)
          addInstruction("MOVE", variableRegister, expressionRegister)
        elseif variableType == "Global" then
          -- OP_SETGLOBAL [A, Bx]    Gbl[Kst(Bx)] := R(A)
          addInstruction("SETGLOBAL", expressionRegister, findOrCreateConstant(variableName))
        elseif variableType == "Upvalue" then
          -- OP_SETUPVAL [A, B]    UpValue[B] := R(A)
          addInstruction("SETUPVAL", expressionRegister, findOrCreateUpvalue(variableName))
        end
      elseif lvalueType == "TableIndex" then
        local indexRegister = processExpressionNode(lvalue.Index)
        local tableExpressionRegister = processExpressionNode(lvalue.Expression)
        local expressionRegister = expressionRegisters[index]
        if not expressionRegister then error("Expected an expression for assignment") end
        -- OP_SETTABLE [A, B, C]    R(A)[RK(B)] := RK(C)
        addInstruction("SETTABLE", tableExpressionRegister, indexRegister, expressionRegister)
        deallocateRegisters({ indexRegister, expressionRegister, tableExpressionRegister })
      else
        error("Unsupported lvalue type: " .. lvalueType)
      end
    end
    deallocateRegisters(expressionRegisters)
  end

  --// CODE GENERATION //--
  function processExpressionNode(node, expressionRegister)
    expressionRegister = expressionRegister or allocateRegister()
    local nodeType = node.TYPE
    while nodeType == "Expression" do
      node = node.Value
      nodeType = node.TYPE
    end

    if     nodeType == "Number"         then return compileNumberNode(node, expressionRegister)
    elseif nodeType == "String"         then return compileStringNode(node, expressionRegister)
    elseif nodeType == "Function"       then return compileFunctionNode(node, expressionRegister)
    elseif nodeType == "FunctionCall"   then return compileFunctionCallNode(node, expressionRegister)
    elseif nodeType == "Constant"       then return compileConstantNode(node, expressionRegister)
    elseif nodeType == "VarArg"         then return compileVarArgNode(node, expressionRegister)
    elseif nodeType == "TableIndex"     then return compileTableIndexNode(node, expressionRegister)
    elseif nodeType == "Table"          then return compileTableNode(node, expressionRegister)
    elseif nodeType == "Variable"       then return compileVariableNode(node, expressionRegister)
    elseif nodeType == "BinaryOperator" then return compileBinaryOperatorNode(node, expressionRegister)
    elseif nodeType == "UnaryOperator"  then return compileUnaryOperatorNode(node, expressionRegister)
    end

    error("Unsupported expression node type: " .. tostring(nodeType))
  end
  function processStatementNode(node)
    local nodeType = node.TYPE
    if nodeType == "FunctionCall" then
      return deallocateRegisters({ processExpressionNode(node) })
    elseif nodeType == "BreakStatement"           then return compileBreakStatementNode()
    elseif nodeType == "LocalFunctionDeclaration" then return compileLocalFunctionDeclarationNode(node)
    elseif nodeType == "FunctionDeclaration"      then return compileFunctionDeclarationNode(node)
    elseif nodeType == "LocalDeclaration"         then return compileLocalDeclarationNode(node)
    elseif nodeType == "NumericForLoop"           then return compileNumericForLoopNode(node)
    elseif nodeType == "GenericForLoop"           then return compileGenericForLoopNode(node)
    elseif nodeType == "ReturnStatement"          then return compileReturnStatementNode(node)
    elseif nodeType == "WhileLoop"                then return compileWhileLoopNode(node)
    elseif nodeType == "RepeatLoop"               then return compileRepeatLoopNode(node)
    elseif nodeType == "DoBlock"                  then return compileDoBlockNode(node)
    elseif nodeType == "IfStatement"              then return compileIfStatementNode(node)
    elseif nodeType == "VariableAssignment"       then return compileVariableAssignmentNode(node)
    end

    error("Unsupported statement node type: " .. tostring(nodeType))
  end
  function processExpressionNodes(list)
    local registers = {}
    for _, node in ipairs(list) do
      local currentExpressionRegisters = { processExpressionNode(node) }
      for _, register in ipairs(currentExpressionRegisters) do
        table.insert(registers, register)
      end
    end
    return registers
  end
  function processCodeBlock(list)
    enterScope()
    for _, node in ipairs(list) do
      processStatementNode(node)
    end
    exitScope()
  end
  function processFunctionCodeBlock(list, parameters)
    enterScope(true) -- Enter with function scope
    for _, parameter in ipairs(parameters) do
      registerVariable(parameter, allocateRegister())
    end
    for _, node in ipairs(list) do
      processStatementNode(node)
    end
    exitScope()
  end
  function processFunction(node, expressionRegister, name)
    local codeBlock    = node.CodeBlock
    local parameters   = node.Parameters or {}
    local isVarArg     = node.IsVarArg
    local oldProto     = currentProto
    local proto        = newProto()
    proto.numParams    = #parameters
    proto.isVarArg     = isVarArg
    proto.functionName = (name and "@" .. name) or "@anonymous"

    processFunctionCodeBlock(codeBlock, parameters)

    -- OP_RETURN [A, B]    return R(A), ... ,R(A+B-2)
    addInstruction("RETURN", 0, 1) -- Default return statement
    setProto(oldProto)
    table.insert(protos, proto)
    -- R(A) := closure(KPROTO[Bx], R(A), ... ,R(A+n))
    addInstruction("CLOSURE", expressionRegister, #protos - 1)

    for _, upvalueName in ipairs(proto.upvalues) do
      local upvalueType = getVariableType(upvalueName)
      if upvalueType == "Local" then
        -- OP_MOVE [A, B]    R(A) := R(B)
        addInstruction("MOVE", 0, findVariableRegister(upvalueName))
      elseif upvalueType == "Upvalue" then
        -- OP_GETUPVAL [A, B]    R(A) := UpValue[B]
        addInstruction("GETUPVAL", 0, findOrCreateUpvalue(upvalueName))
      else error("Unsupported upvalue type: " .. upvalueType) end
    end
    return proto
  end

  --// MAIN //--
  local function generate()
    local proto = newProto()
    proto.isVarArg = true
    processCodeBlock(ast)
    -- OP_RETURN [A, B]    return R(A), ... ,R(A+B-2)
    addInstruction("RETURN", 0, 1) -- Default return statement
    return proto
  end

  return generate()
end

--[[
  ============================================================================
                                    (•_•)?
                              COMPILER CONSTANTS
  ============================================================================
--]]

local MODE_iABC = 0
local MODE_iABx = 1
local MODE_iAsBx = 2

local COMPILER_OPCODE_LOOKUP = {
  ["MOVE"]     = {0, MODE_iABC},  ["LOADK"]     = {1, MODE_iABx},  ["LOADBOOL"] = {2, MODE_iABC},  ["LOADNIL"]   = {3, MODE_iABC},
  ["GETUPVAL"] = {4, MODE_iABC},  ["GETGLOBAL"] = {5, MODE_iABx},  ["GETTABLE"] = {6, MODE_iABC},  ["SETGLOBAL"] = {7, MODE_iABx},
  ["SETUPVAL"] = {8, MODE_iABC},  ["SETTABLE"]  = {9, MODE_iABC},  ["NEWTABLE"] = {10, MODE_iABC}, ["SELF"]      = {11, MODE_iABC},
  ["ADD"]      = {12, MODE_iABC}, ["SUB"]       = {13, MODE_iABC}, ["MUL"]      = {14, MODE_iABC}, ["DIV"]       = {15, MODE_iABC},
  ["MOD"]      = {16, MODE_iABC}, ["POW"]       = {17, MODE_iABC}, ["UNM"]      = {18, MODE_iABC}, ["NOT"]       = {19, MODE_iABC},
  ["LEN"]      = {20, MODE_iABC}, ["CONCAT"]    = {21, MODE_iABC}, ["JMP"]      = {22, MODE_iAsBx},["EQ"]        = {23, MODE_iABC},
  ["LT"]       = {24, MODE_iABC}, ["LE"]        = {25, MODE_iABC}, ["TEST"]     = {26, MODE_iABC}, ["TESTSET"]   = {27, MODE_iABC},
  ["CALL"]     = {28, MODE_iABC}, ["TAILCALL"]  = {29, MODE_iABC}, ["RETURN"]   = {30, MODE_iABC}, ["FORLOOP"]   = {31, MODE_iAsBx},
  ["FORPREP"]  = {32, MODE_iAsBx},["TFORLOOP"]  = {33, MODE_iABC}, ["SETLIST"]  = {34, MODE_iABC}, ["CLOSE"]     = {35, MODE_iABC},
  ["CLOSURE"]  = {36, MODE_iABx}, ["VARARG"]    = {37, MODE_iABC}
}

--[[
  ============================================================================
                                   (۶* ‘ヮ’)۶”
                          !!!!!!!!THE COMPILER!!!!!!!!
  ============================================================================

  The final part of the compiler is the compiler itself (duh). The
  compiler is responsible for converting the given Lua Function Prototypes
  into Lua bytecode. The compiler will implement binary writing logic
  to write the bytecode to a file, which can then be executed by the
--]]

local Compiler = {}
function Compiler.compile(mainProto)
  --// BYTE MANIPULATION (needed for compiling to bytecode) //--
  local function toUnsigned(value)
    value = value or 0
    return math.max(value, -value - 1)
  end
  local function makeBytes(value, byteCount)
    local bytes = {}
    for i = 1, byteCount do
      bytes[i] = value % 256
      value = math.floor(value / 256)
    end
    return string.char(unpack(bytes))
  end
  local function makeOneByte(value)
    return string.char(value % 256)
  end
  local function makeFourBytes(value)
    return makeBytes(value, 4)
  end
  local function makeEightBytes(value)
    return makeBytes(value, 8)
  end
  local function makeDouble(value)
    local sign = (value < 0 and 1) or 0
    local mantissa, exponent = math.frexp(math.abs(value))

    if value == 0 then -- zero
      mantissa, exponent = 0, 0
    elseif value == 1/0 then -- infinity
      mantissa, exponent = 0, 2047
    else
      mantissa = (mantissa * 2 - 1) * (0.5 * (2 ^ 53))
      exponent = exponent + 1022
    end

    -- 52-bit mantissa
    local double = {}
    for index = 1, 6 do
      double[index] = mantissa % 256
      mantissa = math.floor(mantissa / 256)
    end

    -- exponent (11 bit)
    double[7] = ((mantissa % 16) + (exponent % 16) * 16) % 256
    double[8] = ((sign * 128) + math.floor(exponent / 16)) % 256
    return string.char(unpack(double))
  end

  --// BYTECODE GENERATION //--
  local makeString, makeConstant, makeInstruction, makeConstantSection,
        makeCodeSection, makeFunction, makeHeader
  function makeString(value)
    value = value .. "\0"
    local size = makeEightBytes(#value)
    return size .. value
  end
  function makeConstant(constantValue, constantType)
    if constantType == "number" then
      return makeOneByte(3) .. makeDouble(constantValue)
    elseif constantType == "string" then
      return makeOneByte(4) .. makeString(constantValue)
    elseif constantType == "boolean" then
      local secondByte = (constantValue and 1) or 0
      return makeOneByte(1) .. makeOneByte(secondByte)
    elseif constantType == "nil" then
      return makeOneByte(0)
    end
    error("Unsupported constant type: " .. constantType)
  end
  function makeInstruction(instruction)
    local opcodeTable = COMPILER_OPCODE_LOOKUP[instruction[1]]
    local opcode, opmode = unpack(opcodeTable)
    local a = toUnsigned(instruction[2])
    local instructionNumber = opcode
    instructionNumber = instructionNumber + (a * 64) -- a << 6
    if opmode == MODE_iABC then
      local b = toUnsigned(instruction[3])
      local c = toUnsigned(instruction[4])
      instructionNumber = instructionNumber + (b * 8388608) -- b << 23
      instructionNumber = instructionNumber + (c * 16384)   -- c << 14
    elseif opmode == MODE_iABx then
      local b = toUnsigned(instruction[3])
      instructionNumber = instructionNumber + (b * 16384) -- b << 14
    elseif opmode == MODE_iAsBx then
      local b = instruction[3]
      instructionNumber = instructionNumber + ((b + 131071) * 16384) -- (b + 131071) << 14
    end
    return makeFourBytes(instructionNumber)
  end
  function makeConstantSection(proto)
    local constantSection = makeFourBytes(#proto.constants) -- Number of constants
    for _, constant in ipairs(proto.constants) do
      local constantType = type(constant)
      constantSection = constantSection .. makeConstant(constant, constantType)
    end
    constantSection = constantSection .. makeFourBytes(#proto.protos) -- Number of protos
    for _, childProto in ipairs(proto.protos) do
      constantSection = constantSection .. makeFunction(childProto)
    end
    return constantSection
  end
  function makeCodeSection(proto)
    local codeSection = makeFourBytes(#proto.code) -- Number of instructions
    for _, instruction in ipairs(proto.code) do
      codeSection = codeSection .. makeInstruction(instruction)
    end
    return codeSection
  end
  function makeFunction(proto)
    local functionHeader = makeString(proto.functionName) -- Function name
    functionHeader = functionHeader .. makeFourBytes(0) -- Line defined
    functionHeader = functionHeader .. makeFourBytes(0) -- Last line defined
    functionHeader = functionHeader .. makeOneByte(#proto.upvalues) -- nups (Number of upvalues)
    functionHeader = functionHeader .. makeOneByte(proto.numParams) -- Number of parameters
    functionHeader = functionHeader .. makeOneByte((proto.isVarArg and 2) or 0) -- Is vararg
    functionHeader = functionHeader .. makeOneByte(128) -- Max stack size
    functionHeader = functionHeader .. makeCodeSection(proto)
    functionHeader = functionHeader .. makeConstantSection(proto)
    functionHeader = functionHeader .. makeFourBytes(0) -- Line info
    functionHeader = functionHeader .. makeFourBytes(0) -- Local variables
    functionHeader = functionHeader .. makeFourBytes(0) -- Upvalues
    return functionHeader
  end
  function makeHeader()
    local header = "\27Lua" -- Signature
    header = header .. string.char(0x51)  -- Version 5.1
    header = header .. "\0"  -- Format 0 (official)
    header = header .. "\1"  -- Little endian
    header = header .. "\4"  -- sizeof(int)
    header = header .. "\8"  -- sizeof(size_t)
    header = header .. "\4"  -- sizeof(Instruction)
    header = header .. "\8"  -- sizeof(lua_Number)
    header = header .. "\0"  -- Integral flag
    return header
  end

  --// MAIN //--
  local function compile()
    local header = makeHeader()
    local functionHeader = makeFunction(mainProto)
    return header .. functionHeader
  end

  return compile()
end

-- Now I'm just exporting everything...
return {
  Tokenizer            = Tokenizer,
  Parser               = Parser,
  InstructionGenerator = InstructionGenerator,
  Compiler             = Compiler
}