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