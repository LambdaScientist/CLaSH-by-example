# CLaSH-by-example
This project is a collection of CLaSH examples.

Including :
- A conversion of source code from VHDL BY EXAMPLE by Blaine C. Readler
  - This code also includes some tools to test the logic
- Some modified version examples on CLaSH's website
  * https://hackage.haskell.org/package/clash-prelude-0.10.14/docs/CLaSH-Prelude-BlockRam.html#usingrams


Notes:
- For clarity, CLaSH is synthesizable to VHDL and Verilog. Code that is not synthesizable will be called Haskell.
- Proc defines timer tick events. According to WoWWiki -> "Proc is a common term used primarily in game programming to refer to an event triggered under particular circumstances."
- "St" refers to state.
- Typically a type with a single value constructor is labeled with the same name as the type, otherwise use different names for the type constructor 


Helpful links:
- CLaSH group - https://groups.google.com/forum/#!forum/clash-language
- Haskell reference -> http://learnyouahaskell.com/making-our-own-types-and-typeclasses
- CLaSH reference -> https://hackage.haskell.org/package/clash-prelude-0.10.14/docs/CLaSH-Tutorial.html
- Haskell reference -> http://cheatsheet.codeslower.com/CheatSheet.pdf
- Lens reference -> https://github.com/ekmett/lens/wiki/Operators
- https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/5-type-classes


Stack Overflow:

In Haskell how can I match a type class with an instance of that type class
http://stackoverflow.com/questions/41412958/in-haskell-how-can-i-match-a-type-class-with-an-instance-of-that-type-class

Haskell implementing different behavior for types to increase code resue
http://stackoverflow.com/questions/41400132/haskell-implementing-different-behavior-for-types-to-increase-code-resue

Haskell assert that a type will match another
http://stackoverflow.com/questions/41403208/haskell-assert-that-a-type-will-match-another
