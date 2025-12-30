# Taffy
A malleable language where **t**ypes **a**re **f**unctions. Currently only does basic parsing; complete parsing, as well as an interpreter is in the works. The language is however designed to be AOT-compilable, and that is the end goal.

# Design
Types are unnamed; they are anonymous. Instead of `Vec2` being special and referring to some type, it is instead an expression that evaluates to a value that _just so happens to be usable as a type_. Generics are done by having functions accept types and return types. For example, `Pair(L, R)`, which groups two types together, can be defined as follows:
```
Pair(L, R) -> type = (L, R);
```

So if types are anonymous, how do you do methods? Interfaces? How do you keep information about what types actually represent? Markings are how Taffy does that. Markings start with a dollar sign (`$`).

Every type has a set of markings. `Vec2` can be defined as follows:
```
marking $Vec2;
Vec2 -> type = marked [$Vec2] { x: f32, y: f32 };
```
Then, a function parameter that uses that marked type will only match values that have the required markings, which doesn't actually require anything special: `length(this: Vec2) -> f32 = ...;`. `Vec2` returns the marked structure type, so anything that isn't marked as `$Vec2` won't work. Overloads are likely going to be allowed, and specialisation is done by having functions of the same name take differently marked types or different refutable patterns. Ambiguity resolution will likely be done based on how

I am hoping to add syntactical sugar to help simplify type/marking pair declaration. One idea is to make the above equivalent to the following:
```
Vec2 :: { x: f32, y: f32 };
```
This declares the `$Vec2` marking and `Vec2` which implicitly uses it.

Markings keep the richness of methods and the logic of traits, but allow malleability, subtyping, and type anonymity.

Identifiers. Almost every single identifier in the language can be based on an expression instead. `ident term` where `term` is some expression term will evaluate `term` until it becomes an identifier. `ident @str_to_ident(@concat("Hello", "World"))` is syntactically identical to `HelloWorld`. I'm hoping to add a macro system to the language to add even mroe metaprogramming capabilities. Identifiers can also be written like string literals by using the `` ` `` character: `` `main`() = {}; ``. Escaped identifiers are never interpreted as keywords.

Another thing Taffy has is write-only references. These are in the language for API design purposes (`copy(&w T, &r T)`) but also for variance. A read-only reference is covariant over `T` (rust's `&T`), while a write-only reference is contravariant over `T`; combining readability and writability creates a reference invariant over `T`, and this is rust's `&mut T`. While relatively minor, I'm curious to see what write-only references could do.

Operators are simply defined by a sequence of punctuation; there is no special treatment for `+`, `*`, or `/` over something like `<$>` or `|[*]|`. Well, unary operators have one punctuation token, while binary operators can have any number of them, so multi-character punctuation is disallowed in unary operations. A binary operator followed by a unary operator is whitespace-sensitive in the same way two identifiers are; `hello world` and `helloworld` are different like `2 + !condition` and `2 +!condition` are different. Operator overloading is really just defining a function whose name is made of punctuation, that takes one or two parameters, then giving it an opgroup. An opgroup bundles up a precedence group, and they are defined in relation to one another:
```
opgroup Multiplication; 
opgroup Addition after Multiplication; 
`*`(lhs: S(defines N), rhs: S(N)) in Multiplication -> S(N) = @signed_mul(N, lhs, rhs);
`+`(lhs: S(defines N), rhs: S(N)) in Addition -> S(N) = @signed_add(N, lhs, rhs);
```

Functions can have what are currently named specifiers (this will need more bikeshedding). Specifiers begin with an exclamation mark (`!`), and are used in a very similar form to Rust's `unsafe` specifier. In fact, Taffy's analogue to `unsafe` is `!ub`. Function specifiers come after the parameter list and before the return type signifier (`->`). Any function that has specifiers must be called under a function that has a superset of those specifiers. For example, `!ub` functions must be called under `!ub` contexts. You can create a context with a specifier with the `non` keyword, followed by an expression that is allowed to use that specifier. For example, `non!ub mem.bitcast(x)` is analogous to `unsafe { std::mem::transmute(x) }` in Rust. This is a **developer** guarantee, saying that an expression does not trigger the behaviour the specifier warns against. `main` can have any set of specifiers. There are built-in specifiers that mark functions that may perform I/O, filesystem operations, network communication, undefined behaviour, and more. Specifiers can be created (syntax WIP).

Finally, intrinsics. Nearly every single thing in the language boils down to either a call to an external function, or the evaluation of an intrinsic, denoted with an `@` symbol. For example, the 32-bit signed integer type, `s32`, is defined to be `S(32)`, which boils down to `marked $Signed @i(32)`. `@i` accepts any width from 0 to 65535, like Zig, and has no intrinsic operators; the `$Signed` marking does the heavy lifting there (which can also be changed to something like `$SatSigned` for saturating operations).

As seen above, even the basic operators are functions that call intrinsics, and they can be changed out for different schemes if so wanted. Implementations can change the pool of usable intrinsics, but generally there would be standard sets of intrinsics for different applications, from embedded scripts to native standalone projects.

# Todos

- [x] Tokenisation
- [x] Parsing PoC
- [ ] Complete Parsing
- [ ] Execution
- [ ] AOT Compilation
