# CLaSH-by-example
This project is a collection of examples of CLaSH.

Including :
- A conversion of source code from VHDL BY EXAMPLE by Blaine C. Readler
- Some modified versions examples on CLaSH's website
  * https://hackage.haskell.org/package/clash-prelude-0.10.14/docs/CLaSH-Prelude-BlockRam.html#usingrams


Notes:
- I use Show to pretty-print some of the records. This is usualy frowned upon. I will evently look into things like pretty-show
- For clarity purposes I will referer to CLaSH as code that is synthesizable to VHDL and Verilog. Code that is not synthesizable will be called Haskell
- I am using Proc to define a what happened from a timmer tick. According to WoWWiki -> "Proc is a common term used primarily in game programming to refer to an event triggered under particular circumstances."
- We use St for stat normaly
- Usually when I write a type with only one value constructor I label the constructor with the same name as the type. If there is more then one value constructor I will not use the same name.


Helpful links:
- CLaSH group - https://groups.google.com/forum/#!forum/clash-language
- Haskell refence -> http://learnyouahaskell.com/making-our-own-types-and-typeclasses
- CLaSH refernce -> https://hackage.haskell.org/package/clash-prelude-0.10.14/docs/CLaSH-Tutorial.html
- Haskell refernce -> http://cheatsheet.codeslower.com/CheatSheet.pdf
- Lens refernce -> https://github.com/ekmett/lens/wiki/Operators
