% Secure execution of untrusted scripts
% Gabriel Gonzalez
% July 29, 2015

# Poll

Who has ever done this?

    $ curl http://example.com/installer | sudo sh

I aim to fix this problem

# Overview

* Motivation
* Walkthrough
* Core calculus
* Front-end
* Back-end
* Conclusion

# The internet of code

<meta content='text/html; charset=UTF-8' http-equiv='Content-Type'/>

I've built a **distributed** and **typed** assembly language for functional
programs named **`morte`**

**`morte`** is **distributed**, which means you can host individual expressions
on network endpoints:

```haskell
$ curl 'http://sigil.place/talk/0/Bool/True'
λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True
```

... and any expression can embed remote sub-expressions by referencing their
URL:

```haskell
$ curl 'http://sigil.place/talk/0/Bool/(||)'
λ(b1 : #http://sigil.place/talk/0/Bool ) →
    b1 #http://sigil.place/talk/0/Bool
       #http://sigil.place/talk/0/Bool/True 
```

**`morte`** is **typed**, which means that you can enforce security policies at
compile time

# Example

Here are three network endpoints hosting code:

```haskell
$ curl 'http://sigil.place/talk/0/Bool/(||)'
λ(b1 : #http://sigil.place/talk/0/Bool ) →
    b1 #http://sigil.place/talk/0/Bool
       #http://sigil.place/talk/0/Bool/True 

$ curl 'http://sigil.place/talk/0/Bool/True'
λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True

$ curl 'http://sigil.place/talk/0/Bool/False'
λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False
```

I can compute `((||) True False)` by referencing all three URLs:

```haskell
$ morte
#http://sigil.place/talk/0/Bool/(||)
    #http://sigil.place/talk/0/Bool/True
    #http://sigil.place/talk/0/Bool/False
<Ctrl-D>
∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool

λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True
```

# Local imports

You can also import local files by just providing their relative path:

```haskell
$ echo '#http://sigil.place/talk/0/Bool'       > 'Bool'
$ echo '#http://sigil.place/talk/0/Bool/True'  > 'True'
$ echo '#http://sigil.place/talk/0/Bool/False' > 'False'
$ echo '#http://sigil.place/talk/0/Bool/(&&)'  > '(&&)'
$ echo '#http://sigil.place/talk/0/Bool/(||)'  > '(||)'
$ morte
#(||) #True #False
<Ctrl-D>
∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool

λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True
```

# Architecture

`morte` is a modular and reusable piece in a larger suite of projects:

* **`annah`** - Higher-level language that desugars to `morte`
* **`morte`** - Bare-bones language for type-checking, optimization, and linking
* **`nordom`** - Translates `morte` to executable machine code

All of these projects provide both a Haskell API and a command-line interface

Current progress:

* **`annah`**: almost done
* **`morte`**: done
* **`nordom`**: experimental, not yet publicly available

This talk explains how these projects work together to securely run untrusted scripts

# Challenge

Safely import and execute arbitrary remote code

... over an untrusted network

... without code signing

... even if the remote code is compromised by a hostile adversary!

Let's design a language where we treat the programmer as our adversary

We will only allow malicious code to waste CPU time, memory, or network
bandwidth (and we can limit those, too)

# Questions? - Walkthrough

* Motivation
* Walkthrough
* Core Calculus
* Front-end
* Back-end
* Conclusion

# Running example: Safe installation scripts

We want to sandbox installation scripts to only these side effects:

* Read a string
* Print a string
* Download a file from a URL
* Create a file from a string
* Test for a file's existence

```haskell
#IO/getLine    : #IO #String

#IO/putLine    : ∀(str : #String ) → #IO #Prod0

#IO/download   : ∀(url : #String ) → ∀(dest : #String ) → #IO #Prod0

#IO/createFile : ∀(payload : #String ) → ∀(dest : #String ) → #IO #Prod0

#IO/testFile   : ∀(path : #String ) → #IO #Bool
```

# Example script

```haskell
-- example.annah

#IO/Monad #Prod0 (do #IO {
    _       : #Prod0  <- #IO/putLine "Enter a file name:";
    dest    : #String <- #IO/getLine ;
    _       : #Prod0  <- #IO/putLine "Enter the file contents:";
    payload : #String <- #IO/getLine ;
    _       : #Prod0  <- #IO/createFile payload dest; })
```

```bash
$ run < example.annah
[+] Reading
[+] Parsing
[+] Desugaring
[+] Resolving imports
[+] Type-checking
[+] Compiling
[1 of 1] Compiling Main             ( tmp.hs, tmp.o )
Linking tmp ...
[+] Executing
Enter a file name:
output.txt<Enter>
Enter the file contents:
Hello, world!<Enter>
$ cat output.txt
Hello, world!
```

# Example - `annah` code

```haskell
$ cat example.annah
#IO/Monad #Prod0 (do #IO {
    _       : #Prod0  <- #IO/putLine "Enter a file name:";
    dest    : #String <- #IO/getLine ;
    _       : #Prod0  <- #IO/putLine "Enter the file contents:";
    payload : #String <- #IO/getLine ;
    _       : #Prod0  <- #IO/createFile payload dest; })
```

# Example - `morte` code (dynamically linked)

```haskell
$ cat example.annah | annah
#IO/Monad  #Prod0  (λ(Cmd : *) → λ(Bind : ∀(b : *) → #IO  b → (b → Cmd) → Cmd) →
λ(Pure : #Prod0  → Cmd) → Bind #Prod0  (#IO/putLine  (λ(S : *) → λ(N : S) → λ(C
: S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → C@69
(C@110 (C@116 (C@101 (C@114 (C@32 (C@97 (C@32 (C@102 (C@105 (C@108 (C@101 (C@32
(C@110 (C@97 (C@109 (C@101 (C@58 N))))))))))))))))))) (λ(_ : #Prod0 ) → Bind
#String  #IO/getLine  (λ(dest : #String ) → Bind #Prod0  (#IO/putLine  (λ(S : *)
→ λ(N : S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S
→ S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S)
→ λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C
: S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → C@69 (C@110 (C@116 (C@101 (C@114 (C@32 (C@116 (C@104 (C@101 (C@32
(C@102 (C@105 (C@108 (C@101 (C@32 (C@99 (C@111 (C@110 (C@116 (C@101 (C@110
(C@116 (C@115 (C@58 N))))))))))))))))))))))))) (λ(_ : #Prod0 ) → Bind #String
#IO/getLine  (λ(payload : #String ) → Bind #Prod0  (#IO/createFile  payload
dest) Pure)))))
```

# Example - `morte` code (statically linked)

```haskell
$ cat example.annah | annah | morte
λ(IO : *) → λ(PutLine_ : ∀(str : ∀(S : *) → ∀(N : S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → S) → IO → IO) → λ(GetLine_
: ((∀(S : *) → ∀(N : S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → S) → IO) → IO) → λ(Download_ : ∀(url : ∀(S : *) → ∀(N :
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → S) → ∀(dest : ∀(S : *) → ∀(N : S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → S) → IO → IO) → λ(CreateFile_ : ∀(payload
: ∀(S : *) → ∀(N : S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → S) → ∀(dest : ∀(S : *) → ∀(N : S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → S) → IO → IO) →
λ(TestFile_ : ∀(path : ∀(S : *) → ∀(N : S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → S) → ((∀(Bool : *) → ∀(True : Bool)
→ ∀(False : Bool) → Bool) → IO) → IO) → λ(Pure_ : ∀(r : ∀(Prod0 : *) → ∀(Make :
Prod0) → Prod0) → IO) → PutLine_ (λ(S : *) → λ(N : S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → C@69 (C@110 (C@116 (C@101
(C@114 (C@32 (C@97 (C@32 (C@102 (C@105 (C@108 (C@101 (C@32 (C@110 (C@97 (C@109
(C@101 (C@58 N)))))))))))))))))) (GetLine_ (λ(_ : ∀(S : *) → ∀(N : S) → ∀(C : S
→ S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S)
→ ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C
: S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → S) →
PutLine_ (λ(S : *) → λ(N : S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C
: S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → C@69 (C@110 (C@116 (C@101 (C@114 (C@32 (C@116
(C@104 (C@101 (C@32 (C@102 (C@105 (C@108 (C@101 (C@32 (C@99 (C@111 (C@110 (C@116
(C@101 (C@110 (C@116 (C@115 (C@58 N)))))))))))))))))))))))) (GetLine_ (λ(_ : ∀(S
: *) → ∀(N : S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → S) → CreateFile_ _ _@1 (Pure_ (λ(Prod0 : *) → λ(Make :
Prod0) → Make))))))
```

# Example - `nordom`-generated Haskell code

```haskell
$ cat example.annah | annah | morte | nordom
import Nordom.Haskell

main = io'ToIO (\iO0 -> \putLine_0 -> \getLine_0 -> \download_0 -> \createFile_0
-> \testFile_0 -> \pure_0 -> ((putLine_0) (\s7 -> \n7 -> \c896 -> \c897 -> \c898
-> \c899 -> \c900 -> \c901 -> \c902 -> \c903 -> \c904 -> \c905 -> \c906 -> \c907
-> \c908 -> \c909 -> \c910 -> \c911 -> \c912 -> \c913 -> \c914 -> \c915 -> \c916
-> \c917 -> \c918 -> \c919 -> \c920 -> \c921 -> \c922 -> \c923 -> \c924 -> \c925
-> \c926 -> \c927 -> \c928 -> \c929 -> \c930 -> \c931 -> \c932 -> \c933 -> \c934
-> \c935 -> \c936 -> \c937 -> \c938 -> \c939 -> \c940 -> \c941 -> \c942 -> \c943
-> \c944 -> \c945 -> \c946 -> \c947 -> \c948 -> \c949 -> \c950 -> \c951 -> \c952
-> \c953 -> \c954 -> \c955 -> \c956 -> \c957 -> \c958 -> \c959 -> \c960 -> \c961
-> \c962 -> \c963 -> \c964 -> \c965 -> \c966 -> \c967 -> \c968 -> \c969 -> \c970
-> \c971 -> \c972 -> \c973 -> \c974 -> \c975 -> \c976 -> \c977 -> \c978 -> \c979
-> \c980 -> \c981 -> \c982 -> \c983 -> \c984 -> \c985 -> \c986 -> \c987 -> \c988
-> \c989 -> \c990 -> \c991 -> \c992 -> \c993 -> \c994 -> \c995 -> \c996 -> \c997
-> \c998 -> \c999 -> \c1000 -> \c1001 -> \c1002 -> \c1003 -> \c1004 -> \c1005 ->
\c1006 -> \c1007 -> \c1008 -> \c1009 -> \c1010 -> \c1011 -> \c1012 -> \c1013 ->
\c1014 -> \c1015 -> \c1016 -> \c1017 -> \c1018 -> \c1019 -> \c1020 -> \c1021 ->
\c1022 -> \c1023 -> (c954) ((c913) ((c907) ((c922) ((c909) ((c991) ((c926)
((c991) ((c921) ((c918) ((c915) ((c922) ((c991) ((c913) ((c926) ((c914) ((c922)
((c965) (n7)))))))))))))))))))) ((getLine_0) (\_1031 -> ((putLine_0) (\s9 -> \n9
-> \c1152 -> \c1153 -> \c1154 -> \c1155 -> \c1156 -> \c1157 -> \c1158 -> \c1159
-> \c1160 -> \c1161 -> \c1162 -> \c1163 -> \c1164 -> \c1165 -> \c1166 -> \c1167
-> \c1168 -> \c1169 -> \c1170 -> \c1171 -> \c1172 -> \c1173 -> \c1174 -> \c1175
-> \c1176 -> \c1177 -> \c1178 -> \c1179 -> \c1180 -> \c1181 -> \c1182 -> \c1183
-> \c1184 -> \c1185 -> \c1186 -> \c1187 -> \c1188 -> \c1189 -> \c1190 -> \c1191
-> \c1192 -> \c1193 -> \c1194 -> \c1195 -> \c1196 -> \c1197 -> \c1198 -> \c1199
-> \c1200 -> \c1201 -> \c1202 -> \c1203 -> \c1204 -> \c1205 -> \c1206 -> \c1207
-> \c1208 -> \c1209 -> \c1210 -> \c1211 -> \c1212 -> \c1213 -> \c1214 -> \c1215
-> \c1216 -> \c1217 -> \c1218 -> \c1219 -> \c1220 -> \c1221 -> \c1222 -> \c1223
-> \c1224 -> \c1225 -> \c1226 -> \c1227 -> \c1228 -> \c1229 -> \c1230 -> \c1231
-> \c1232 -> \c1233 -> \c1234 -> \c1235 -> \c1236 -> \c1237 -> \c1238 -> \c1239
-> \c1240 -> \c1241 -> \c1242 -> \c1243 -> \c1244 -> \c1245 -> \c1246 -> \c1247
-> \c1248 -> \c1249 -> \c1250 -> \c1251 -> \c1252 -> \c1253 -> \c1254 -> \c1255
-> \c1256 -> \c1257 -> \c1258 -> \c1259 -> \c1260 -> \c1261 -> \c1262 -> \c1263
-> \c1264 -> \c1265 -> \c1266 -> \c1267 -> \c1268 -> \c1269 -> \c1270 -> \c1271
-> \c1272 -> \c1273 -> \c1274 -> \c1275 -> \c1276 -> \c1277 -> \c1278 -> \c1279
-> (c1210) ((c1169) ((c1163) ((c1178) ((c1165) ((c1247) ((c1163) ((c1175)
((c1178) ((c1247) ((c1177) ((c1174) ((c1171) ((c1178) ((c1247) ((c1180) ((c1168)
((c1169) ((c1163) ((c1178) ((c1169) ((c1163) ((c1164) ((c1221)
(n9)))))))))))))))))))))))))) ((getLine_0) (\_1288 -> (((createFile_0) (_1288))
(_1031)) ((pure_0) (\prod01 -> \make1 -> make1)))))))
```

# Example - Interpret with `ghc`

```haskell
$ cat example.annah | annah | morte | nordom | runhaskell
Enter a file name:
output.txt<Enter>
Enter the file contents:
Hello, world!<Enter>
$ cat output.txt
Hello, world!
```
# Questions? - `morte`

* Motivation
* Walkthrough
* Core calculus
* Front-end
* Back-end
* Conclusion

# `morte` - a bare-bones calculus of constructions

```haskell
-- | Syntax tree for expressions
data Expr a
    -- | > Const Star   ~  *
    = Const Const
    -- | > Var (V x 0)  ~  x
    --   > Var (V x n)  ~  x@n
    | Var Var
    -- | > Lam x A b    ~  λ(x : A) → b
    | Lam Text (Expr a) (Expr a)
    -- | > Pi  x A B    ~  ∀(x : A) → B
    | Pi  Text (Expr a) (Expr a)
    -- | > App f a      ~  f a
    | App (Expr a) (Expr a)
    -- | > Import path  ~  #path
    | Import a

-- | Resolve all imports
load :: Expr Path -> IO (Expr X)

-- | Type-check an expression
typeOf :: Expr X -> Either TypeError (Expr X)

-- | Reduce an expression to normal form
normalize :: Expr a -> Expr a
```

# Example `morte` optimization

```haskell
#(||) #True #False

-- Resolve imports
= ( λ(b1 : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool)
  → b1 (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool)
       (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)
  )
  (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True )
  (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False)

-- β-reduce
= (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True )
  (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool)
  (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)
  (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False)

-- β-reduce
= ( λ(True : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool)
  → λ(False : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool)
  → True
  )
  (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)
  (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False)

-- β-reduce
= ( λ(False : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool)
  → λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True
  )
  (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False)

-- β-reduce
= λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True
```

# Functional assembly language

Think of the calculus of constructions as an assembly language for functional
programs

* `morte` is a **total** programming language (i.e. non-Turing-complete)
* `morte` is 100% pure (meaning no built-in side effects)
* `morte` has no escape hatches or unsafe primitives

No native support for:

* strings
* numbers
* algebraic data types / pattern matching
* multiple top-level expressions
* recursion

We can implement those within the calculus of constructions using `annah`!

# Questions? - `annah`

* Motivation
* Walkthrough
* Core calculus
* Front-end
* Back-end
* Conclusion

# `let` - values

This `annah` code:

```haskell
let x : t = y
in  e
```

... desugars to this `morte` code:

```haskell
(λ(x : t) → e) y
```

This `annah` code:

```haskell
let x1 : t1 = y1
let x2 : t2 = y2
in  e
```

... desugars to this `morte` code:

```haskell
(λ(x1 : t1) → λ(x2 : t2) → e) y1 y2
```

# `let` - functions

This `annah` code:

```haskell
let f (x1 : t1) (x2 : t2) : t = y
in  e
```

... desugars to this `annah` code:

```haskell
let f : ∀(x1 : t1) → ∀(x2 : t2) → t =
        λ(x1 : t1) → λ(x2 : t2) → y
in  e
```

... which in turn desugars to this `morte` code:

```haskell
(\(f : ∀(x1 : t1) → ∀(x2 : t2) → t) → e)
      (λ(x1 : t1) → λ(x2 : t2) → y)
```

# Encode data types - `Bool`

```haskell
-- data Bool = True | False

type Bool
data True
data False
fold if
in   e
```

... desugars to this `annah` code:

```haskell
let Bool  : *     = ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool
let True  : Bool  = λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True
let False : Bool  = λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False
let if    : Bool  → ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool
    = λ(x : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) → x
in  e
```

... which in turns desugars to this `morte` code:

```haskell
(   λ(Bool  : *)
→   λ(True  : Bool)
→   λ(False : Bool)
→   λ(if : Bool  → ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool)
→   e
)
(∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool )
(λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True )
(λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False)
(λ(x : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) → x)
```

# Separate compilation of types and constructors

```haskell
$ curl http://sigil.place/talk/0/Bool/@.annah
type Bool
data True
data False
in   Bool

$ curl http://sigil.place/talk/0/Bool/@
∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool
```

```haskell
$ curl http://sigil.place/talk/0/Bool/True.annah
type Bool
data True
data False
in   True

$ curl http://sigil.place/talk/0/Bool/True
λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True
```

# Encode data types - `Pair`

This `annah` code:

```haskell
-- data Pair a b = MakePair { x : a, y : b }

given a : *
given b : *
type Pair
data MakePair (x : a) (y : b)
fold foldPair
in   e
```

... desugars to this `annah` code:

```haskell
let Pair (a : *) (b : *) : *
    = ∀(Pair : *) → ∀(MakePair : ∀(x : a) → ∀(y : b) → Pair) → Pair
let MakePair (a : *) (b : *) (x : a) (y : b) : Pair a b
    = λ(Pair : *) → λ(MakePair : ∀(x : a) → ∀(y : b) → Pair) → MakePair x y
let foldPair (a : *) (b : *)
    : Pair a b → ∀(Pair : *) → ∀(MakePair : ∀(x : a) → ∀(y : b) → Pair) → Pair
    = λ(p : ∀(Pair : *) → ∀(MakePair : ∀(x : a) → ∀(y : b) → Pair) → Pair) → p
in  e
```

... which in turn desugars to this `morte` code:

```haskell
(   λ(Pair : ∀(a : *) → ∀(b : *) → *
→   λ(MakePair : ∀(a : *) → ∀(b : *) → ∀(x : a) → ∀(y : b) → Pair a b)
→   λ(foldPair : ∀(a : *) → ∀(b : *) → Pair a b → ∀(Pair : *) → ∀(MakePair : ∀(x : a) → ∀(y : b) → Pair) → Pair)
→   e
)
(λ(a : *) → λ(b : *) → ∀(Pair : *) → ∀(MakePair : ∀(x : a) → ∀(y : b) → Pair) → Pair)
(λ(a : *) → λ(b : *) → λ(x : a) → λ(y : b) → λ(Pair : *) → λ(MakePair : ∀(x : a) → ∀(y : b) → Pair) → MakePair x y)
(λ(a : *) → λ(b : *) → λ(p : ∀(Pair : *) → ∀(MakePair : ∀(x : a) → ∀(y : b) → Pair) → Pair) → p)
```

# String literals

This `annah` string literal:

```haskell
"Hello, world!"
```

... desugars to this `morte` encoding as an ASCII string in lambda calculus:

```haskell
λ(S : *) → λ(N : S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S)
→ λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C
: S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → C@72 (C@101 (C@108 (C@108 (C@111 (C@44 (C@32 (C@119 (C@111
(C@114 (C@108 (C@100 (C@33 N))))))))))))
```

# `annah` - String operations

String concatenation:

```haskell
time echo '#String/(++) "Hello, " "world!"' | annah | morte
∀(S : *) → ∀(N : S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S)
→ ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C
: S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) →
∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C :
S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S → S) → ∀(C : S →
S) → ∀(C : S → S) → S

λ(S : *) → λ(N : S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S)
→ λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C
: S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) →
λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C :
S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S → S) → λ(C : S →
S) → λ(C : S → S) → C@72 (C@101 (C@108 (C@108 (C@111 (C@44 (C@32 (C@119 (C@111
(C@114 (C@108 (C@100 (C@33 N))))))))))))

real    0m1.270s
user    0m1.220s
sys     0m0.040s
```

# `annah` - Numeric literals

This `annah` numeric literal

```haskell
42
```

... desugars to this `morte` encoding as a binary numeral in lambda calculus:

```haskell
    λ(Bin : *)
→   λ(Zero : Bin)
→   λ(One :
        (   ∀(Bin_ : *)
        →   ∀(Nil_ : Bin_)
        →   ∀(Zero_ : Bin_ → Bin_)
        →   ∀(One_ : Bin_ → Bin_)
        →   Bin_
        )
    →   Bin
    )
→   One
    (   λ(Bin_ : *)
    →   λ(Nil_ : Bin_)
    →   λ(Zero_ : Bin_ → Bin_)
    →   λ(One_ : Bin_ → Bin_)
    →   Zero_ (One_ (Zero_ (One_ (Zero_ Nil_))))
    )
```

# `annah` - Arithmetic - Addition

64-bit addition

```haskell
time echo '#Bin/(+) 9223372036854775807 9223372036854775807' | annah | morte
∀(Bin : *) → ∀(Zero : Bin) → ∀(One : (∀(Bin_ : *) → ∀(Nil_ : Bin_) → ∀(Zero_ :
Bin_ → Bin_) → ∀(One_ : Bin_ → Bin_) → Bin_) → Bin) → Bin

λ(Bin : *) → λ(Zero : Bin) → λ(One : (∀(Bin_ : *) → ∀(Nil_ : Bin_) → ∀(Zero_ :
Bin_ → Bin_) → ∀(One_ : Bin_ → Bin_) → Bin_) → Bin) → One (λ(Bin_ : *) → λ(Nil_
: Bin_) → λ(Zero_ : Bin_ → Bin_) → λ(One_ : Bin_ → Bin_) → One_ (One_ (One_
(One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_
(One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_
(One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_
(One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_
(One_ (One_ (One_ (One_ (One_ (One_ (One_ (Zero_
Nil_)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

real    0m0.751s
user    0m0.708s
sys     0m0.044s
```

# `annah` - Arithmetic - Multiplication

64-bit multiplication

```haskell
gabriel@wind:~/proj/annah/Prelude$ time echo '#Bin/(*) 4294967295 4294967295' |
annah | morte
∀(Bin : *) → ∀(Zero : Bin) → ∀(One : (∀(Bin_ : *) → ∀(Nil_ : Bin_) → ∀(Zero_ :
Bin_ → Bin_) → ∀(One_ : Bin_ → Bin_) → Bin_) → Bin) → Bin

λ(Bin : *) → λ(Zero : Bin) → λ(One : (∀(Bin_ : *) → ∀(Nil_ : Bin_) → ∀(Zero_ :
Bin_ → Bin_) → ∀(One_ : Bin_ → Bin_) → Bin_) → Bin) → One (λ(Bin_ : *) → λ(Nil_
: Bin_) → λ(Zero_ : Bin_ → Bin_) → λ(One_ : Bin_ → Bin_) → One_ (One_ (One_
(One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_
(One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_ (One_
(One_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_
(Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_
(Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_ (Zero_
(One_ Nil_)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

real    0m9.887s
user    0m9.408s
sys     0m0.484s
```

# Questions? - `nordom`

* Motivation
* Walkthrough
* Core calculus
* Front-end
* Back-end
* Conclusion

# Encode data types - `IO`

This `annah` code:

```haskell
given a : *
type IO
data PutLine_ (str : #String ) IO
data GetLine_ (#String -> IO)
data Download_ (url : #String ) (dest : #String ) IO
data CreateFile_ (payload : #String ) (dest : #String ) IO
data TestFile_ (path : #String ) (#Bool -> IO)
data Pure_ (r : a)
in
IO
```

... desugars to this `morte` type:

```haskell
    λ(a : *)
→   ∀(IO : *)
→   ∀(PutLine_ : ∀(str : #String ) → IO → IO)
→   ∀(GetLine_ : (#String → IO) → IO)
→   ∀(Download_ : ∀(url : #String ) → ∀(dest : #String ) → IO → IO)
→   ∀(CreateFile_ : ∀(payload : #String ) → ∀(dest : #String ) → IO → IO)
→   ∀(TestFile_ : ∀(path : #String ) → (#Bool → IO) → IO)
→   ∀(Pure_ : ∀(r : a) → IO)
→   IO
```

# Types enforce policy 

Before generating code, `nordom` verifies that the program matches this type:

```haskell
    ∀(IO : *)
→   ∀(PutLine_ : ∀(str : #String ) → IO → IO)
→   ∀(GetLine_ : (#String → IO) → IO)
→   ∀(Download_ : ∀(url : #String ) → ∀(dest : #String ) → IO → IO)
→   ∀(CreateFile_ : ∀(payload : #String ) → ∀(dest : #String ) → IO → IO)
→   ∀(TestFile_ : ∀(path : #String ) → (#Bool → IO) → IO)
→   ∀(Pure_ : ∀(r : #Prod0 ) → IO)
→   IO
```

`nordom` rejects programs of any other type **at compile time**

# Customizable backends

`morte` has no built-in primitives or side effects

```haskell
data Expr a
    = Const Const
    | Var Var
    | Lam Text (Expr a) (Expr a)
    | Pi  Text (Expr a) (Expr a)
    | App (Expr a) (Expr a)
    | Import a
```

Instead, we specify the signature of our side effects **within the language**:

```haskell
    λ(a : *)
→   ∀(IO : *)
→   ∀(PutLine_ : ∀(str : #String ) → IO → IO)
→   ∀(GetLine_ : (#String → IO) → IO)
→   ∀(Download_ : ∀(url : #String ) → ∀(dest : #String ) → IO → IO)
→   ∀(CreateFile_ : ∀(payload : #String ) → ∀(dest : #String ) → IO → IO)
→   ∀(TestFile_ : ∀(path : #String ) → (#Bool → IO) → IO)
→   ∀(Pure_ : ∀(r : a) → IO)
→   IO
```

This means that we can easily customize the side effects our back-end permits

# Comparison to other security approaches

This approach is most similar to the sandbox approach to security

Unique features:

* Detect policy violations at **compile time**
* Pure code is automatically accepted.  You only need to whitelist side effects
* Extremely low barrier to code distribution
* Types-based policy

# Questions? - Conclusion

* Motivation
* Walkthrough
* Core calculus
* Front-end
* Back-end
* Conclusion

# Conclusion

These projects still have a long way to go before you should use them

* I need to provide high-efficiency numeric/string operations
* Type inference still missing
* Needs better tooling for distributing, discovering, and "installing" code

This code is up on Github at:

* https://github.com/Gabriel439/Haskell-Morte-Library
* https://github.com/Gabriel439/Haskell-Annah-Library
* Hacky nordom code is up on the `nordom` branch of the `annah` repository

You can reach me at:

[\@GabrielG439](https://twitter.com/GabrielG439)

[/u/Tekmo](http://www.reddit.com/user/Tekmo/) on [/r/haskell](http://www.reddit.com/r/haskell)

# Live demo + Questions

If time permits:

Browse: [http://sigil.place/talk/0](http://sigil.place/talk/0)

Run code provided by the audience
