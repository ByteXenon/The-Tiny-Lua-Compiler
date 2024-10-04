![Banner](https://github.com/ByteXenon/TinyLua/assets/125568681/41cf5285-e31d-4b27-a8a8-ee83a7300f1f)

***originally inspired by Jamie Kyle's [The Super Tiny Compiler](https://github.com/jamiebuilds/the-super-tiny-compiler) written in JavaScript***

**Welcome to The Tiny Lua Compiler!**

This is an ultra-simplified example of all the major pieces of a modern compiler
written in easy to read Lua code.

## Features

- **Zero dependencies**: This compiler is written in pure Lua and has no dependencies.
- **Educational**: Reading through the guided code will help you learn about how *most* compilers work from end to end.
- **Self-compiling**: This compiler can compile itself!

### [Want to jump into the code? Click here](the-tiny-lua-compiler.lua)

---

### Why should I care?

That's fair, most people don't really have to think about compilers in their day
jobs. However, compilers are all around you, tons of the tools you use are based
on concepts borrowed from compilers.

### Why Lua?

Lua is a simple programming language that is easy to learn and use. It doesn't
have complex syntax or a lot of features, which makes it a great language to
make a compiler for.

### But compilers are scary!

Yes, they are. But that's our fault (the people who write compilers), we've
taken something that is reasonably straightforward and made it so scary that
most think of it as this totally unapproachable thing that only the nerdiest of
the nerds are able to understand.

### Okay so where do I begin?

Awesome! Head on over to the [the-tiny-lua-compiler.lua](the-tiny-lua-compiler.lua)
file.

### Example usage?

Currently, the compiler only supports Lua 5.1. Here's an example of how you can use it:

```lua
local tlc = require("the-tiny-lua-compiler")

local inputCode = [[
  for index = 1, 10 do
    print(index)
  end
  print("Hello, World!")
]]

local tokens             = tlc.Tokenizer.tokenize(inputCode)
local abstractSyntaxTree = tlc.Parser.parse(tokens)
local prototype          = tlc.InstructionGenerator.generate(abstractSyntaxTree)
local bytecode           = tlc.Compiler.compile(prototype)

local compiledFunction = loadstring(bytecode)

-- Run the compiled function
compiledFunction()
```

### Tests

Run with `lua tests/test.lua`

---

[![cc-by-4.0](https://licensebuttons.net/l/by/4.0/80x15.png)](http://creativecommons.org/licenses/by/4.0/)