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

### Types

There are four types in WebAssembly and therefore Curlywas:

* `i32`: 32bit integer
* `i64`: 64bit integer
* `f32`: 32bit float
* `f64`: 64bit float

There are no unsigned types, but there are unsigned operators where it makes a difference.

### Literals

Integer numbers can be given either in decimal or hex:

```
123, -7878, 0xf00
```

For floating point numbers, only the most basic decimal format is currently implemented (no scientific notation or hex-floats, yet):

```
0.464, 3.141, -10.0
```

String literals exist in a very basic form. No escape character implemented yet.

```
"env.memory", "Hello World!"

this does not work, yet:

"one line\nsecond line", "They said: \"Enough!\""
```

### Imports

WebAssembly imports are specified with a module and a name. In Curlywas you give them inside a single string literal, seperated by a dot. So a module `env` and name `printString` would be written `"env.printString"`.

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

#### Local variables

Variables are defined using `let`:

```
let name: type;
```

They can also be initialized to a value at the same time, in this case the type can be left out and will be infered:

```
let name = expression;
```

There are two modifiers that change when the initializer expression is actually evaluated.

They both can reduce the size of the resulting code, but it is up to the coder to make sure that the delayed evaluation doesn't change the semantics of the code.

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
