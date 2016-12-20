# CLaSH-by-example
This project is a collection of CLaSH examples.

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
