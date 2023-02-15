# CurlyWas

CurlyWas is a (still WIP) curly-braces, infix synatx for WebAssembly.
The goal is to have as to a 1:1 mapping to the resulting wasm instructions
as possible while still being reasonably convenient to write.

For this reason alone (and in no way because I'm a little lazy) does this
compiler not implement any optimizations except for constant folding.

## Example

```rust
import "env.memory" memory(4);
import "env.sin" fn sin(f32) -> f32;

export fn tic(time: i32) {
    let i: i32;
    loop screen {
        let lazy t = time as f32 / 2000 as f32;
        let lazy o = sin(t) * 0.8;
        let lazy q = (i % 320) as f32 - 160.1;
        let lazy w = (i / 320 - 120) as f32;
        let lazy r = sqrt(q*q + w*w);
        let lazy z = q / r;
        let lazy s = z * o + sqrt(z * z * o * o + 1 as f32 - o * o);
        let lazy q2 = (z * s - o) * 10 as f32 + t;
        let lazy w2 = w / r * s * 10 as f32 + t;
        let lazy s2 = s * 50 as f32 / r;
        i?120 = max(
            0 as f32, 
            ((q2 as i32 ^ w2 as i32 & ((s2 + t) * 20 as f32) as i32) & 5) as f32 *
                (2 as f32 - s2) * 22 as f32
            ) as i32;
        branch_if (i := i + 1) < 320*240: screen
    }
}
```

You can compile this to `technotunnel.wasm` with the command

```
curlywas technotunnel.cwa
```

Then run it on [MicroW8](https://exoticorn.github.io/microw8/v0.1pre2)

## Syntax

### Comments

```
// This is a single line comment.

/*
   Multiline comments are also supported.
*/
```

### Include

Other sourcefiles can be included with the `include` top level statement:

```
include "platform_imports.cwa"
```

### Types

There are five types in WebAssembly and therefore CurlyWas:

* `i32`: 32bit integer
* `i64`: 64bit integer
* `f32`: 32bit float
* `f64`: 64bit float
* `v128`: 128bit vector

There are no unsigned types, but there are unsigned operators where it makes a difference.

### Literals

Integer numbers can be given either in decimal or hex:

```
123, -7878, 0xf00
```

64 or 128-bit integers are denoted using `i64` or `i128`:

```
0xabc0i64, 8i128
```

For floating point numbers, only the most basic decimal format is currently implemented (no scientific notation or hex-floats, yet):

```
0.464, 3.141, -10.0
```

String literals are used for include paths, import names and as literal strings in the data section. The following escapes are supported:
| Escape | Result | Comment                            |
| `\"`   | `"`    |                                    |
| `\'`   | `'`    |                                    |
| `\t`   | 8      |                                    |
| `\n`   | 10     |                                    |
| `\r`   | 13     |                                    |
| `\N`   | 0x0N   | (Can't be followed by a hex digit) |
| `\NN`  | 0xNN   |                                    |

```
"env.memory", "Hello World!"

"one line\nsecond line", "They said: \"Enough!\""
```

Character literals are enclosed in single quotes `'` and support the same escapes as strings. They can contain up to 4 characters and evaluate to the
little-endian representation of these characters. For examples: `'A'` evaluates to `0x41`, `'hi'` evaluates to 0x6968, and `'Crly'` to 0x7a6c7243.

### Imports

WebAssembly imports are specified with a module and a name. In CurlyWas you give them inside a single string literal, seperated by a dot. So a module `env` and name `printString` would be written `"env.printString"`.

Linear memory can be imported like this:

```
import "module.name" memory(min_pages);
```

giving the minimum required size as the number of 64KB pages.

Global variables can be imported with:

```
import "module.name" global var_name: type;     // const global
import "module.name" global mut var_name: type; // mutable global
```

`var_name` being the name you want to reference the variable by in your code.

Functions are imported like this:

```
import "module.name" fn fun_name(param_types) [-> return_type];

examples:

import "env.cls" cls(i32);         // no return type
import "env.random" rand() -> i32; // no params
import "env.atan2" atan2(f32, f32) -> f32;
```

### Global variables

Global variables are declare like this:

```
global name[: type] = value;             // immutable global value
global mut name[: type] = initial_value; // mutable variable
```

An immutable global is probably of very limited use, as usually you'd most often use it by exporting it so that some other module
can use it. However, exporting global variable is not yet supported in CurlyWas.

The type is optional, if missing it is inferred from the init value.

### Constants

Constants can be declared in the global scope:

```
const name[: type] = value;
```

`value` has to be an expression evaluating to a constant value. It may reference other constants.

The type is optional, but if given has to match the type of `value`.

### Functions

Functions look like this:

```
[export] fn name(param_list) [-> return_type] {
    [...]
}

exampels:

fn getPixel(x: i32, y: i32) -> i32 {
    ...
}

export fn upd() {
    ...
}
```

The body of a function is a block (see below), meaning a sequence of statements followed by an optional expression which gives the return value of the function.

#### Local variables

Variables are defined using `let`:

```
let name: type;
```

They can also be initialized to a value at the same time, in this case the type can be left out and will be infered:

```
let name = expression;
```

Local variables are lexically scoped and shadowing variables declared earlier is explicitely allowed.

`name = value;` assigns a new value to a (non-inline) variable.

There are two modifiers that change when the initializer expression is actually evaluated. They both can reduce the size of the resulting code (when used appropriately), but it is up to the coder to make sure that the delayed evaluation doesn't change the semantics of the code.

```
let inline name = expression;
```

The expression is evaluated (inlined) everytime you use the variable.

```
let lazy name = expression;
```

The expression is evaluated and assigned to the variable at the first place it is used (and only there).

`let lazy` uses the `local.tee` instruction which combines `local.set` and `local.get` and therefore saves on instruction (usually 2 bytes).

Examples of mistakes to watch out for:
```
let x = 4;
let inline y = 7;
print(y); // prints 11
x = x + 2;
print(y); // prints 13
```

```
let inline num_bytes = write(buffer, size);
print(num_bytes);
read(buffer, num_bytes); // calls write a second time
```

```
let lazy num_bytes = write(header, 8);
write(body, size);
print(num_bytes); // the header is only written now
```

```
let lazy foo = 42;
if rand() & 1 {
    printNumber(foo); // foo is initialized here
} else {
    printNumber(foo / 2 + 2); // foo is never initialized in the else branch
}
```

#### Expressions

Expressions are written in familiar infix operator and function call syntax.

The avaliable operators are:

| Precedence | Symbol           | WASM instruction(s)                    | Description                   |
| ---------- | ---------------- | -------------------------------------- | ----------------------------- |
| 1          | -                | fxx.neg, ixx.sub                       | Unary negate                  |
|            | !                | i32.eqz                                | Unary not / equal to zero     |
| 2          | as               | default signed casts                   | Type cast                     |
| 3          | ?, !             | i32.load_u8_u, i32.load                | load byte/word                |
| 4          | *                | ixx.mul, fxx.mul                       | Multiplication                |
|            | /, %             | ixx.div_s, fxx.div, ixx.rem_s          | signed division / remainder   |
|            | #/, #%           | ixx.div_u, ixx.rem_u                   | unsigned division / remainder |
| 5          | +, -             | xxx.add, xxx.sub                       | Addition, substraction        |
| 6          | <<, >>, #>>      | ixx.shl, ixx.shr_s, ixx.shr_u          | Shifts                        |
| 7          | ==, !=           | xxx.eq, xxx.ne                         | Equal, not equal              |
|            | <, <=, >, >=     | ixx.lt_s, ixx.le_s, ixx.gt_s, ixx.ge_s | signed comparison             |
|            |                  | fxx.lt, fxx.le, fxx.gt, fxx.ge         |                               |
|            | #<, #<=, #>, #>= | ixx.lt_u, ixx.le_u, ixx.gt_u, ixx.ge_u | unsigned comparison           |
| 8          | &, \|, ^         | ixx.and, ixx.or, ixx.xor               | Bitwise logic                 |
| 9          | <\|              | n/a                                    | take first, see sequencing    |

You can obviously group sub-expression using brackets `( )`.

Functions can be called using familiar `function_name(parameters)` syntax.

There are intrinsic functions for all WASM instructions that simply take a number of parameters and return a value. So you can, for example, do something like `i32.clz(value)` to use instructions that don't map to their own operator.

Some common float instructions have shortcuts, ie. they can be used without the `f32.` / `.f64.` prefix:

`sqrt, min, max, ceil, floor, trunc, nearest, abs, copysign`

`name := value` both assigns the value to the variable `name` and returns the value (using the `local.tee` WASM instruction).

Blocks are delimited by curly braces `{ }`. They contain zero or more statements, optionally followed by an expression. They evaluate to the
value of that final expression if it is there.

So for example this block evaluates to 12:

```
{
  let a = 5;
  let b = 7;
  a + b
}
```

Blocks are used as function bodies and in flow control (`if`, `block`, `loop`), but can also used at any point inside an expression.

Variable re-assignments of the form `name = name <op> expression` can be shortened to `name <op>= expression`, for example `x += 1` to increment `x` by one. This works for all arithmetic, bit and shift operators.
The same is allowed for `name := name <op> expression`, ie. `x +:= 1` increments `x` and returns the new value.

#### Flow control

`if condition_expression { if_true_block } [else {if_false_block}]` executes the `if_true_block` if the condition evaluates to a non-zero integer and
the `if_false_block` otherwise (if it exists). It can also be used as an expression, for example:

```
let a = if 0 { 2 } else { 3 }; // assigns 3 to a
```

If the `if_false_block` contains exactly one `if` expression or statement you may omit the curly braces, writing `else if` chains like:
```
if x == 0 {
    doOneThing()
} else if x == 1 {
    doThatOtherThing()
} else {
    keepWaiting()
}
```

`block name { ... }` opens a named block scope. A branch statement can be used to jump to the end of the block. Currently, `block` can only be used
as a statement, returning a value from the block is not yet supported.

`loop name { ... }` opens a named loop scope. A branch statement can be used to jump back to the beginning of the loop.

`branch name` jumps to the end/start of the named `block` or `loop` scope. `branch_if condition: name` does the same if the condition evaluates to a
non-zero integer.

`return [expression]` returns from the current function with the value of the optional expression.

#### Memory load/store

To read from memory you specify a memory location as `base?offset`, `base!offset` or `base$offset`. `?` reads a byte, `!` reads a 32bit word
and `$` reads a 32bit float.

`base` can be any expression that evaluates to an `i32` while `offset` has to be a constant `i32` value. The effective memory address is the sum of both.

Writing to memory looks just like an assignment to a memory location: `base?offset = expression`, `base!offset = expression` and `base$offset = expression`.

When reading/writing 32bit words you need to make sure the address is 4-byte aligned.

These compile to `i32.load8_u`, `i32.load`, `f32.load`, `i32.store8`, `i32.store` and `f32.store`.

In addition, all wasm memory instructions are available as intrinsics:

```
<load-ins>(<base-address>[, <offset>, [<align>]])

offset defaults to 0, align to the natural alignment: 0 for 8bit loads, 1 for 16bit, 2 for 32 bit and 3 for 64bit.
```

with `<load-ins>` being one of `i32.load`, `i32.load8_u`, `i32.load8_s`, `i32.load16_u`, `i32.load16_s`,
`i64.load`, `i64.load8_u`, `i64.load8_s`, `i64.load16_u`, `i64.load16_s`, `i32.load32_u`, `i32.load32_s`,
`f32.load` and `f64.load`.

```
<store-ins>(<value>, <base-address>[, <offset>, [<align>]])

offset and align defaults are the same as the load intrinsics.
```
with `<store-ins>` being one of `i32.store`, `i32.store8`, `i32.store16`, `i64.store`, `i64.store8`,
`i64.store16`, `i64.store32`, `f32.store` and `f64.store`.

#### Data

Data sections are written in `data` blocks:

```
data <address> {
    ...
}
```

The content of such a block is loaded at the given address at module start.

Inside the data block you can include 8, 16, 32, 64, f32 or f64 values:

```
i8(1, 255) i16(655350) i32(0x12345678) i64(0x1234567890abcdefi64) f32(1.0, 3.141) f64(0.5f64)
```

Strings:
```
"First line" i8(13, 10) "Second line"
```

And binary files:

```
file("font.bin")
```

#### SIMD

Intrinsics are available for all WASM SIMD instructions, except for the relaxed SIMD extensions.
For instructions that refer to lanes, i.e. `*.extract_lane*`, `*.replace_lane`, `v128.store*_lane`, and
`v128.load*_lane`, the lane number follows the vector argument. For example:

```
v128.store32_lane(<v128_value>, <lane>, <base-address>[, <offset>, [<align>]]);
v128.load32_splat(<v128_value>, <lane>, <base-address>[, <offset>, [<align>]]);
i32x4.extract_lane(<v128_value>, <lane>);
i32x4.replace_lane(<v128_value>, <lane>, <i32_value>);
```

The format for `i8x16.shuffle` is:

```
i8x16.shuffle(<v128_a>, <v128_b>, [<lane_0>[, <lane_1>[, ... <lane_16>]]])
```

Omitted lane arguments default to their index, so providing no lane arguments simply returns the value of `<v128_a>`.

#### Advanced sequencing

Sometimes when sizeoptimizing it helps to be able to execute some side-effecty code in the middle an expression.
Using a block scope, we can execute any number of statements before evaluating a final expression to an actual value. For example:

```
let x = { randomSeed(time); random() }; // set the random seed right before obtaining a random value
```

To execute something after evaluating the value we want to return, we can use the `<|` operator. Here is an example from the Wasm4 version of
Skip Ahead (see the example folder for the full source):

```
text(8000, set_color(c) <| rect(rx, y, rw, 1), set_color(4));
```

Here, we first set the color to `c`. `set_color` also happens to return the constant `6` which we want to use for the text x-position but only
after drawing a rectangle with color `c` and setting the color for the text to `4`. This line compiles to the following sequence:

* Push `8000` onto the stack
* Call `set_color(c)` which sets the drawing color and pushes 6 on the stack
* Call `rect` which draws a rectangle with the set color. This call doesn't affect the stack.
* Call `set_color(4)` which sets the drawing color to `4` and pushes another 6 on the stack.
* Call `text` with the parameters (`8000`, `6`, `6`) pushed on the stack. 

## Limitations

The idea of CurlyWas is to be able to hand-craft any valid WASM program, ie. having the same amount of control over the instruction sequence as if you would write in the web assembly text format (`.wat`) just with better ergonomics.

This goal is not yet fully reached, with the following being the main limitations:

* CurlyWas currently only targets MVP web assembly + non-trapping float-to-int conversions + SIMD. No other post-MVP features are currently supported. Especially "Multi-value" will be problematic as this allows programs that don't map cleanly to an expression tree.
* `block`s cannot return values, as the branch instructions are missing syntax to pass along a value.
* `br_table` and `call_indirect` are not yet implemented.
