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
      if value2 == nil then
        error("At path " .. newPath .. ", expected a table but got nil")
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
local simpleTestScript = "local a = 10 + 1; while true do print('test string', ...) end"
local simpleTestTokens = {
  { TYPE = "Keyword",    Value = "local" },
  { TYPE = "Identifier", Value = "a" },
  { TYPE = "Character",  Value = "=" },
  { TYPE = "Number",     Value = "10" },
  { TYPE = "Operator",   Value = "+" },
  { TYPE = "Number",     Value = "1" },
  { TYPE = "Character",  Value = ";" },
  { TYPE = "Keyword",    Value = "while" },
  { TYPE = "Constant",   Value = "true" },
  { TYPE = "Keyword",    Value = "do" },
  { TYPE = "Identifier", Value = "print" },
  { TYPE = "Character",  Value = "(" },
  { TYPE = "String",     Value = "test string" },
  { TYPE = "Character",  Value = "," },
  { TYPE = "VarArg" },
  { TYPE = "Character",  Value = ")" },
  { TYPE = "Keyword",    Value = "end" }
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
        TYPE = "Expression",
        Value = {
          TYPE = "BinaryOperator",
          Operator = "+",
          Left = { TYPE = "Number", Value = "10" },
          Right = { TYPE = "Number", Value = "1" }
        }
      }
    }
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
        Expression = { TYPE = "Identifier", Value = "print" },
        Arguments = {
          { TYPE = "Expression", Value = { TYPE = "String", Value = "test string" } },
          { TYPE = "Expression", Value = { TYPE = "VarArg" } }
        }
      }
    }
  }
}
local result = tlc.Parser.parse(simpleTestTokensResult)
assertTableEquals(expectedAST, result)

print("All tests passed successfully! :)")
return true