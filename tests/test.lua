--[[
========================================
~~  The Tiny Lua Compiler test suite  ~~
========================================
--]]

local tlc = require("the-tiny-lua-compiler")

--/// COMPARISONS HELPERS ///--
local function assertTableEquals(table1, table2, path)
  path = path or "" -- Initialize path if it's the first call
  for index, value in pairs(table1) do
    local newPath = path .. (path ~= "" and "." or "") .. tostring(index)
    local value2 = table2[index]
    if type(value) == "table" then
      if type(value2) ~= "table" then
        error("At path " .. newPath .. ", expected a table but got " .. type(value2))
      end
      assertTableEquals(value, value2, newPath)
    else
      assert(value == value2, "At path " .. newPath .. ", expected " .. tostring(value) .. " but got " .. tostring(value2))
    end
  end
  for index, _ in pairs(table2) do
    local newPath = path .. (path ~= "" and "." or "") .. tostring(index)
    if table1[index] == nil then
      error("At path " .. newPath .. ", expected nil but got a value")
    end
  end
end

--/// TOKENIZER TESTS ///--
local simpleTestScript = [[
  local a = 10 + 1;
  print(a)
  while true do
    print('test string')
  end
  if 1     then print(1)
  elseif 2 then print(2)
  else          print(3)
  end
  local function b()
    return a, 2, 3
  end
  b()
]]
local simpleTestTokens = {
  { TYPE = "Keyword",    Value = "local" },
  { TYPE = "Identifier", Value = "a" },
  { TYPE = "Character",  Value = "=" },
  { TYPE = "Number",     Value = 10 },
  { TYPE = "Operator",   Value = "+" },
  { TYPE = "Number",     Value = 1 },
  { TYPE = "Character",  Value = ";" },
  { TYPE = "Identifier", Value = "print" },
  { TYPE = "Character",  Value = "(" },
  { TYPE = "Identifier", Value = "a" },
  { TYPE = "Character",  Value = ")" },
  { TYPE = "Keyword",    Value = "while" },
  { TYPE = "Constant",   Value = "true" },
  { TYPE = "Keyword",    Value = "do" },
  { TYPE = "Identifier", Value = "print" },
  { TYPE = "Character",  Value = "(" },
  { TYPE = "String",     Value = "test string" },
  { TYPE = "Character",  Value = ")" },
  { TYPE = "Keyword",    Value = "end" },
  { TYPE = "Keyword",    Value = "if" },
  { TYPE = "Number",     Value = 1 },
  { TYPE = "Keyword",    Value = "then" },
  { TYPE = "Identifier", Value = "print" },
  { TYPE = "Character",  Value = "(" },
  { TYPE = "Number",     Value = 1 },
  { TYPE = "Character",  Value = ")" },
  { TYPE = "Keyword",    Value = "elseif" },
  { TYPE = "Number",     Value = 2 },
  { TYPE = "Keyword",    Value = "then" },
  { TYPE = "Identifier", Value = "print" },
  { TYPE = "Character",  Value = "(" },
  { TYPE = "Number",     Value = 2 },
  { TYPE = "Character",  Value = ")" },
  { TYPE = "Keyword",    Value = "else" },
  { TYPE = "Identifier", Value = "print" },
  { TYPE = "Character",  Value = "(" },
  { TYPE = "Number",     Value = 3 },
  { TYPE = "Character",  Value = ")" },
  { TYPE = "Keyword",    Value = "end" },
  { TYPE = "Keyword",    Value = "local" },
  { TYPE = "Keyword",    Value = "function" },
  { TYPE = "Identifier", Value = "b" },
  { TYPE = "Character",  Value = "(" },
  { TYPE = "Character",  Value = ")" },
  { TYPE = "Keyword",    Value = "return" },
  { TYPE = "Identifier", Value = "a" },
  { TYPE = "Character",  Value = "," },
  { TYPE = "Number",     Value = 2 },
  { TYPE = "Character",  Value = "," },
  { TYPE = "Number",     Value = 3 },
  { TYPE = "Keyword",    Value = "end" },
  { TYPE = "Identifier", Value = "b" },
  { TYPE = "Character",  Value = "(" },
  { TYPE = "Character",  Value = ")" }
}

local simpleTestTokensResult = tlc.Tokenizer.tokenize(simpleTestScript)
assertTableEquals(simpleTestTokens, simpleTestTokensResult)

--/// PARSER TESTS ///--
local expectedAST = {
  TYPE = "AST",
  {
    TYPE = "LocalDeclaration",
    Variables = { "a" },
    Expressions = {
      {
        TYPE = "BinaryOperator",
        Operator = "+",
        Left = { TYPE = "Number", Value = 10 },
        Right = { TYPE = "Number", Value = 1 }
      }
    }
  },
  {
    TYPE = "FunctionCall",
    Expression = { TYPE = "Variable", VariableType = "Global", Value = "print" },
    Arguments = { { TYPE = "Variable", VariableType = "Local", Value = "a" } },
    ReturnValueAmount = 0
  },
  {
    TYPE = "WhileLoop",
    Condition = {
      TYPE = "Expression",
      Value = { TYPE = "Constant", Value = "true" }
    },
    Codeblock = {
      TYPE = "Group",
      {
        TYPE = "FunctionCall",
        Expression = { TYPE = "Variable", VariableType = "Global", Value = "print" },
        Arguments = {
          { TYPE = "String", Value = "test string" },
        },
        ReturnValueAmount = 0,
      }
    }
  },
  {
    TYPE = "IfStatement",
    Condition = {
      TYPE = "Expression",
      Value = { TYPE = "Number", Value = 1 }
    },
    Codeblock = {
      TYPE = "Group",
      {
        TYPE = "FunctionCall",
        Expression = { TYPE = "Variable", VariableType = "Global", Value = "print" },
        Arguments = { { TYPE = "Number", Value = 1 } },
        ReturnValueAmount = 0
      }
    },
    ElseIfs = {
      {
        Condition = {
          TYPE = "Expression",
          Value = { TYPE = "Number", Value = 2 }
        },
        Codeblock = {
          TYPE = "Group",
          {
            TYPE = "FunctionCall",
            Expression = { TYPE = "Variable", VariableType = "Global", Value = "print" },
            Arguments = { { TYPE = "Number", Value = 2 } },
            ReturnValueAmount = 0
          }
        }
      }
    },
    ElseCodeblock = {
      TYPE = "Group",
      {
        TYPE = "FunctionCall",
        Expression = { TYPE = "Variable", VariableType = "Global", Value = "print" },
        Arguments = { { TYPE = "Number", Value = 3 } },
        ReturnValueAmount = 0
      }
    }
  },
  {
    TYPE = "LocalFunctionDeclaration",
    Name = "b",
    IsVarArg = false,
    Parameters = {},
    Codeblock = {
      TYPE = "Group",
      {
        TYPE = "ReturnStatement",
        Expressions = {
          { TYPE = "Variable", VariableType = "Upvalue", Value = "a" },
          { TYPE = "Number", Value = 2 },
          { TYPE = "Number", Value = 3 }
        }
      }
    }
  },
  {
    TYPE = "FunctionCall",
    Expression = { TYPE = "Variable", VariableType = "Local", Value = "b" },
    Arguments = {},
    ReturnValueAmount = 0
  }
}
local result = tlc.Parser.parse(simpleTestTokensResult)
assertTableEquals(expectedAST, result)

print("All tests passed successfully! :)")
return true