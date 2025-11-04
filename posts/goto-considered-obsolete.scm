(use-modules (doclisp) (make))
(set-reader! doclisp-reader)
{{post {version #1}}
 {title Goto Considered Obsolete}
 {uuid 8e3c9e37-f7de-41f2-88c0-3b4ba546231a}
 {published #"2025/11/04 19:48 +0000"}
 {description
  The popular essay “Goto Considered Harmful” by Dijkstra has long inspired an almost religious hatred of the construct, but this is no longer justified.
  The GOTO of which Dijkstra spoke was unrestrained, and so programmers would often use it to jump from the body of one function into the body of another.
  Such reckless use of control flow is clearly objectionable.
  Modern goto is much less powerful than that, and mainly causes issues in languages with manual memory management due to variables being initialised to unexpected values (this makes its omission from Java even more questionable as Java doesn't have manual memory management).
  But I should like to make a stronger thesis, that goto as a programming construct is made entirely obsolete by common PL techniques and has been so for many years.
  I will show how these techniques can likily already be used in C using common compiler extensions.
  Finally, I shall present a rough draft of how such a design may be integrated well into a future C-style language.}
 {body
  #(link-heading 2 "functional-goto" "Functional Goto")
  {p
   My realisation stems from the Scheme language, which is a Lisp and based the lambda calculus.
   The primary abstraction in Scheme is the function.
   Almost all of the language constructs stem from functions in one way or another.
   For instance, the variable binding construct is often said to be equivalent to a function in the following way.}
  #(code-block "goto-considered-obsolete/let-binding.scm")
  {p
   This gives rise to Scheme's primary looping construct, the named let.
   Here, we bind the lambda to a name through {code letrec} (let recursive, this can also be turned into a lambda but doing so is more complicated) and then call that function with the supplied arguments.}
  #(code-block "goto-considered-obsolete/named-let.scm")
  {p
   This works because Scheme has tail call optimisation (TCO) which causes any statement of the form {code return f()\;} to compile into something along the lines of {code goto f\;} instead of {code call f\; return\;}.
   Doing it this way eliminates the stack overflow errors you'd otherwise get when a loop like this gets too deeply nested.}
  {p
   It's a fairly trivial to see how this can replace goto, since they are effectively doing the same thing under the hood.
   You just need to break each labelled block in the source code up into a function and replace gotos with calls to those functions.
   This is actually how most modern compilers represent gotos internally, by converting to something called static single assignment (SSA) form.
   Suffice to say that anything expressed with goto can be equally expressed in this way.}
  #(link-heading 2 "replacing-goto-in-c" "Replacing Goto in C")
  {p
   So why do we still have goto then?
   I think the way of doing things with return statements is generally much cleaner and removes the initialisation risks associated with using gotos as each function is forced to be a self-contained block.
   The GNU C compiler even has an extension that allows you to write that exact code.
   With optimisations enabled, it {{a {href https://godbolt.org/\#z:OYLghAFBqd5TKALEBjA9gEwKYFFMCWALugE4A0BIEAZgQDbYB2AhgLbYgDkAjF%2BTXRMiAZVQtGIHgBYBQogFUAztgAKAD24AGfgCsp5eiyagA%2BudTkVjVEQJDqzTAGF09AK5smBpwBkCTNgAcp4ARtikUrIADuhKxPZMrh5eBrHxdkL%2BgSFs4ZE8Vtg2mUwiRCykRMme3oXW2LaJ5ZVE2cFhEVFWFVU1qfW9bQEdeV08AJRW6O6kqJxcAKQATADMAageOADUi6vOSkSE6AB0SHu4i1oAgivrTJvuO3sHR/QEoWcXV9cBRNtsFgBCB/baVYCWbaoJCVbYAKjh4IAbhNdgB2ABCP2221BgPUu1WABEwSQCBBkYsAKwYnjUokTPZYm440H0dDoaIg4S48i4nlKTyoxaY7E43E0bYgwnOAEsdSMlninGkbBEWZMbbsznSlYY7aFbaCti7Zb6giM1bM67K4oqMXK1Xq0ia41MsUioli6KkP40CArZYrKmYanOJiBvnarlaPlaCaW60qtUa7Zad03T0/LhTejcKn8bxcHTkdDcWVKGZzbCm1Z8chEbQ5qYAaxAVNjea40n4bHbsaLJbLXH4ShAscbxZz5DgsBQGBw%2BGIZEo1DojFYHG49cEwjEEk4MjkwmUak0U/I%2BmWhmMZgsRRKiUcTBcblqIGWsb8I1y%2BQ/5HSBIhH6bxPwAuIgKYdpfy6a8GiaIQWj6N9UjA%2BDSiQ4Yck6SI4KGECP1jQ5WmgnCPymStZnmbg7g2LYaxeQ5ji%2BVZLkzNY6KeBj9iY95PnOVifjxIEmG5f5wUhaFYQRZFhVFJVhIJPYSRYMkKVIYAkWpWl6UTMVQWNQkSXTK0xUEUgpVBAgjLTJlcRlOV1DsvU9QtdEk3FQyXOJXEMxtdEvSVH0/QDZYg2WEMwwjMK%2BWNPSlSdVMTOtLMbmnLsC3IQd%2BGHCsq3mWtln4ScdATcgkGwFgcEiCBc24HtyD7DssqbUtuFHccGybMq22artVkLVrh2K7qZ0QWdkDQdA2GiBgIlXCAMBmubIlQYAeCpa91yICIxwgUJWtCAJKgAT23fgjtYUgToAeVCXRGkneslo4YQbqYegzovHBQncYBnAkegx14fgcEBExJG%2BghVSaJFsGBktsHURp3B287KGEYpWr40hTtcHBWqIX0%2BxB8g4dIUI4mwIlsHB4B3hMUaaCMYAlAANQIbAAHcbuiZh0d3URxEkI9BdPDRWqvG9GfMUxLD4sdICmTlSmBgBaG6itLcnfRwRXaofR6nwgJwCMKb9sLGApwIyRIzZtyDSKtgx0OafCULqQ2ELKIYnb/QZWnt4iqj98YKPyw86q4TLsrarhtnUAAOAA2NXk%2BkbYIVQA0qROZYpSXEgLLuSYRqnHr%2B0Mere0r2PhqsTqSunCaIHnabZsYCgqEW9uVrQdbNoEBgdtIPaDovS7TvRyfrruh7bHRl7mCId7Ptan6/oB%2BggfRsHb0hktCBhuw4YR/gkZRtHSb%2BLGLxxvGsAWEsiYIEn63JymVBpumGdAcuBBZuzTmPM%2BZFh3PIfcItZBixUBLC8%2BhChGBlveBW8BlbRFVtwDWWt0A6wIHrNBXtSjPlfCkT2FtRj%2BwdqUe2gFSih2tq7RC7syEu2KEbZhJEfxkQDshVhvCsKULDtMKinBlhRxjkNbgCcU5pwzlnHOecC6ECLoVCYZdSpTAqlVLoBsuwNSagOKRI4G4Tm6q2Su/VBoXnrk3MqXYtZ13al1cuUxybxAcNIIAA%3D}} compiles identically} to the loop-based equivalent.}
  #(code-block "goto-considered-obsolete/for-loop.c")
  {p
   While this can look better for some of the more complicated for loops you might write, it is quite ugly for the simple kind like this.
   So, let's see what this looks like when I use it as a replacement for a somewhat typical use of goto—breaking out of a loop.}
  #(code-block "goto-considered-obsolete/goto-replaced.c")
  {p
   The functional method has some readability advantages.
   The cleanup code can be placed next to the initialisation code, which I imagine helps to prevent bugs.
   There is no implicit fallthrough, so the order of the definitions of blocks doesn't affect how they execute, preventing potential errors similar to those you'll commonly see in switch statements.
   Fewer variables need to be shared between the blocks (only the stateful buffer, and not the result).
   Most importantly, I echo Dijkstra's original complaint about goto, that it can create spaghetti code.
   With the functional approach, each block must act as a function and so has a clear purpose and responsibility.
   This better encapsulates logical intent of labels as they are used in code, that is to delimit blocks of statements that perform a given function.
   As a final benefit, no one will be able to complain at you for using goto when you do it this way.
   You will instead get much more interesting and original complaints, possibly with the inclusion of epithets and general questions of sanity.}
  #(link-heading 2 "in-future-languages" "In Future Languages")
  {p
   I implore the designers of future languages: ditch harmful goto and adopt nested functions instead.
   I think this proposition is really a strict positive if you explicitly include it in the design of the language.
   Though it would benefit some additional rules and features to improve usability and ensure good compilation.}
  {p
   In the matter of usage, there would be some rules needed to turn these nested functions into things that work in a lower level language.
   You'll note from previous examples that they can form implicit closures and capture variables in the calling context.
   The capturing property of nested functions is also transitive—any nested function which calls a nested function must also be considered to capture whichever variables it does.
   This will require the language to forbid creating a function pointer to any capturing function.
   Non-capturing functions are just ordinary functions and so can be exempt from any special restriction.}
  {p
   Another important note regards usage of the stack.
   While it is possible to write head-recursive capturing functions, they will take up space on the stack, and special instructions will need to be emitted by the compiler that allow such functions to peer up the stack and gain access to their captured variables.
   This is generally undesirable for both the compiler writer who doesn't want to complicate their program, and the programmer who likely doesn't want nested functions that may cause overflows.
   So it would make sense to allow a call to a capturing function only in the tail position.
   This way it is truly guaranteed to compile to an efficient goto instruction.
   Though this limitation can be relaxed if a capturing function is only called once, as the functions body can be directly inserted into the call site in that case.}
  {p
   Finally, there is the matter of ergonomics.
   I believe the ergonomic issues of these functions in C-style languages can be alleviated with two small changes.
   First of all, allow them to be defined out of order and used without being pre-declared.
   Secondly, a new syntax for expressions using the keyword {code run} that allows a nested function to be defined and called simultaneously, similar to how {code let} worked in Scheme.
   Here's a short example showcasing the new feature.}
  #(code-block "goto-considered-obsolete/syntax-example.c")
  {p
   I find it particularly pleasing how the loop can be placed into the arguments of {code printf} as an expression.
   This {code run} syntax could even be extended to take an arbitrary statement following it, and introduce an implicit function when it does so.
   Combine that with a more advanced version of {code return}, and you can write some really elegant initialisation expressions.}
  #(code-block "goto-considered-obsolete/run-extended.c")
  {p
   I shall stop myself there before I am suckered, once again, into redesigning the entire C programming language.}
  {p
   Anyway, I hope I have here convinced you that there is really no need for new programming languages to contain a goto construct.
   I believe this alternative is both more expressive than goto in the contexts for which goto is used, and also more widely applicable to other areas.}}}
