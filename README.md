# CLaSH-by-example
This project is a collection of CLaSH examples. Code base is an expirment in diffrent ways to wirte Haskell for FPGAs

Including :
- A conversion of source code from VHDL BY EXAMPLE by Blaine C. Readler
- Some modified version examples on CLaSH's website
  * https://hackage.haskell.org/package/clash-prelude-0.10.14/docs/CLaSH-Prelude-BlockRam.html#usingrams


Notes:
- "Show" is used to Pretty-Print some of the records; while this is usually frowned upon, it proved effective. Pretty-Show is planned for future versions.
- For clarity, CLaSH is synthesizable to VHDL and Verilog. Code that is not synthesizable will be called Haskell.
- Proc defines timer tick events. According to WoWWiki -> "Proc is a common term used primarily in game programming to refer to an event triggered under particular circumstances."
- "St" refers to state.
- Typically a type with only one value constructor is labeled with the same name as the type. If there is more than one value constructor, different names will be used.


Helpful links:
- CLaSH group - https://groups.google.com/forum/#!forum/clash-language
- Haskell reference -> http://learnyouahaskell.com/making-our-own-types-and-typeclasses
- CLaSH reference -> https://hackage.haskell.org/package/clash-prelude-0.10.14/docs/CLaSH-Tutorial.html
- Haskell reference -> http://cheatsheet.codeslower.com/CheatSheet.pdf
- Lens reference -> https://github.com/ekmett/lens/wiki/Operators
- Boolean Blindness reference -> http://degoes.net/articles/destroy-all-ifs
- https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/5-type-classes


Stack Overflow:
- In Haskell how can I match a type class with an instance of that type class
  * http://stackoverflow.com/questions/41412958/in-haskell-how-can-i-match-a-type-class-with-an-instance-of-that-type-class
- Haskell implementing different behavior for types to increase code resue
  * http://stackoverflow.com/questions/41400132/haskell-implementing-different-behavior-for-types-to-increase-code-resue
- Haskell assert that a type will match another
  * http://stackoverflow.com/questions/41403208/haskell-assert-that-a-type-will-match-another

Language Extensions:
- RecordWildCards
  * Extracts each field name from a record as a variable of the same name
  * http://dev.stephendiehl.com/hask/#recordwildcards
- NoImplicitPrelude
  * Stops GHC from automatically importing prelude
- RankNTypes
  * Allows us to explicitly place quantifiers in the declaration of a type
  * http://dev.stephendiehl.com/hask/#rank-n-types
- ExistentialQuantification
  * Allows us to hide the type-values of a type from any consumers of that type
  * http://dev.stephendiehl.com/hask/#existential-quantification
- TemplateHaskell
  * Allows us to run Haskell code at compile time to generate new Haskell code
  * TemplateHaskell
- DataKinds
  * Allows us to use type constructors as types (e.g. BitVector 4)
  * http://dev.stephendiehl.com/hask/#data-kinds
- DataKinds
  * DefaultSignatures extension to allow the user to leave typeclass functions blank and defer to Generic or to define their own.
  * http://dev.stephendiehl.com/hask/#generic
