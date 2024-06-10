![Untitled](https://github.com/ByteXenon/TinyLua/assets/125568681/41cf5285-e31d-4b27-a8a8-ee83a7300f1f)

***originally inspired by [The Super Tiny Compiler](https://github.com/jamiebuilds/the-super-tiny-compiler)***

**Welcome to The Tiny Lua Compiler!**

This is an ultra-simplified example of all the major pieces of a modern compiler
written in easy to read Lua code.

## Features

- **Self-compiling**: This compiler can compile itself!
- **Educational**: Reading through the guided code will help you learn about how *most* compilers work from end to end.

### [Want to jump into the code? Click here](the-tiny-lua-compiler.lua)

---

### Why should I care?

That's fair, most people don't really have to think about compilers in their day
jobs. However, compilers are all around you, tons of the tools you use are based
on concepts borrowed from compilers.

### But compilers are scary!

Yes, they are. But that's our fault (the people who write compilers), we've
taken something that is reasonably straightforward and made it so scary that
most think of it as this totally unapproachable thing that only the nerdiest of
the nerds are able to understand.

### Okay so where do I begin?

Awesome! Head on over to the [the-tiny-lua-compiler.lua](the-tiny-lua-compiler.lua)
file.

### I'm back, that didn't make sense

Ouch, I'm really sorry. Let me know how it can be improved, by emailing me at ddavi142@asu.edu or opening an issue on this repo.

### Example usage?

Currently, the compiler only supports Lua 5.1. Here's an example of how you can use it:

```lua
local tlc = require("the-tiny-lua-compiler")

-- Compile the input code
local inputCode = [[
  for index = 1, 10 do
    print(index)
  end
  print("Hello, World!")
]]
local tokens = tlc.Tokenizer.tokenize(inputCode)
local ast = tlc.Parser.parse(tokens)
local outputBytecode = tlc.Compiler.compile(ast)

local compiledFunction = loadstring(outputBytecode)

-- Run the compiled function
compiledFunction()
```

### Tests

Run with `lua tests/test.lua`

---

[![cc-by-4.0](https://licensebuttons.net/l/by/4.0/80x15.png)](http://creativecommons.org/licenses/by/4.0/)
[![Open Source](https://badges.frapsoft.com/os/v1/open-source.svg?v=103)](https://opensource.org/)
[![LGBTQ+ Friendly](https://pride-badges.pony.workers.dev/static/v1?label=lgbtq%2B%20friendly&stripeWidth=6&stripeColors=E40303,FF8C00,FFED00,008026,24408E,732982)](https://lgbt.foundation/)
[![Transgender Flag](https://pride-badges.pony.workers.dev/static/v1?label=trans%20rights&stripeWidth=6&stripeColors=5BCEFA,F5A9B8,FFFFFF,F5A9B8,5BCEFA)](https://transequality.org/)
