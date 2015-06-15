- title : Introduction to Compilers in F#
- author : Dmytro Suvorov
- theme : solarized
- transition : default

***

## Changes to Our Small Language

- we now have 2 type of rules:
  * eval: "check Amount > 10"
  * exec: "if Amount > 10 then Refuse()"

---

### Changes to AST

---

### Changes to Interpreter


***

### Static vs Dynamic Typing

- Static Typing: symbols (var, funcs, etc) have type that are checked ahead-of-time (C#)
- (Strong) Dynamic Typing: symbols have types that are checked during execution (Python)
- (Weak) Dynamic Typing: "type" may be changed at runtime, strings become numerals, etc (Perl)

---

### The Main Property of a Sound Type System

- Evaluation should preserve types
  * Or put more simply: there should be no runtime type errors

***

### Why do we want static type in our rule engine?

- "if 1=1 then Action1(Created) and Action2(Amount)"
  - what if Action2 accepts string and not interger?
  - we will execute Action1 and then fail on Action2
  - essentially, we will execute half of our rule

---

### We want some better guarantees

- we want to be more transactional
  - either we do something fully, or fail early

---

### Side note:

- type checking would have also helped us with overload-resolution
  - what if our model has both Refuse() and Refuse(reason)?
- but we will not do it for simplicity sake

***

### Introduction to Typing Rules

**type judgements** are expressions of the following form:

$ \Gamma \vdash e : \tau $

Read as:

"expression $e$ has type $\tau$ in context $\Gamma$"

---

$ \emptyset \vdash \texttt{5} : int $

$ \emptyset \vdash \texttt{5 < 10} : bool $

$ \emptyset \vdash \texttt{check 5 < 10 and 2 = 3} : bool $

---

$ \{ Amount : int \,\} \vdash \texttt{ Amount < 10} : bool $

$ \{ HasTag : string \rightarrow bool \,\} \vdash \texttt{ HasTag('vip')} : bool $

***

### difference between type-checker and type-inferencer

- Type checking: given an expression $e$ and a type $\tau$, decide if $e : \tau$
- Type inference: given an expression $e$, find a type $\tau$ such that $e : \tau$

---

we don't really need type-inferencer

we don't even have variables in our language :)

but we will structure our code as if we are writing a type-inferencer

***

### inference rules

An **inference rule** is a set of premises $P_1, ..., P_n$ and one conclusion $C$, separated by a horizontal line

$ \frac{P_1 ... P_n}{C} $

Meaning: "to check $C$, you have to check $P_1, ..., P_n$ first"

***

### Examples

$ \frac{\emptyset\,\vdash\,a\,:\,bool \; \emptyset\,\vdash\,b\,:\,bool}{\emptyset\,\vdash\,a \texttt{ and } b\,:\,bool} $

---

#### Side note: Operational (Natural) Semantics

Compare this to

$ \frac{\emptyset\,\vdash\,a\,:\,true \; \emptyset\,\vdash\,b\,:\,false}{\emptyset\,\vdash\,a \texttt{ and } b\,:\,false} $

This is a way to specify semantics of programming languages

Called Natural (or Big-Step) Semantics

---

### Abstract interpretation

The process of breaking expression into sub-expressions, processing them separatelly and combining results
is called *structural recursion*

And using inference rules in such a way is called *abstract interpretation*

---

Rules of that form may be used to specify a lot of different (but similar in structure) processes:
- typing
- evaluation
- compilation
- optimization
- symbolic execution
- etc..

***

### back to typing rules for our language

Constants have their respective types:

- $ \frac{}{\Gamma\,\vdash\,i\,:\,int} $ when i is an integer constant
- $ \frac{}{\Gamma\,\vdash\,s\,:\,string} $ when s is a string constant
- $ \frac{}{\Gamma\,\vdash\,b\,:\,bool} $ when b is a boolean constant

in any context $\Gamma$

---

    let typeConstant = function
        | Int i -> typeof<int>
        | String s -> typeof<string>
        | Bool b -> typeof<bool>

---

$ \frac{}{\{ P\,:\,\tau \}\,\vdash\,P\,:\,\tau} $ when P is a property access

    | Property p ->
        match ctx.getPropType p with
            | Some t -> t
            | None ->
                failwith <| sprintf "undefined property %A" p

---

$ \frac{\texttt{a1}\,:\,a_1,\:...,\:\texttt{an}\,:\,a_n}{\Gamma\,\vdash\,\texttt{F(a1,...,an)}\,:\,\tau} $ when F is a method access

provided that $\Gamma$ containt $F\,:\,(\alpha_1,...,\alpha_n) \rightarrow \tau$

---
    | Func(fname, argExps) ->
        match ctx.getFuncType fname with
            | None -> failwith ...
            | Some(freturnType, fargTypes) ->
                let argTypes = argExps |> List.map typeValueExp
                checkArgs(argTypes, fargTypes)
                freturnType
