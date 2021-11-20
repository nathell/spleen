# Spleen

Welcome to Spleen, a Scrabble playing engine written in Clojure.

Currently, it is in a very early stage of development.  However, most
of the game logic (including move lookup) is already in place.  It
currently supports Polish Scrabble, but support is planned for other
word games -- as a minimum, Scrabble and Literaxx in both Polish
and English variants will be available for play.

Other planned features:

  * a GUI
  * support for the proprietary Official Polish Scrabble Dictionary
    (OSPS)
  * human-like AI behaviour including psychological profiles

## Generating a dictionary

1. Have a \n-delimited list of words (Polish requires Latin-2);
   you can generate one for OSPS with [psps](https://github.com/nathell/psps)
2. Run: `clojure -X:build-dict :args '"fsa_compile -f CFSA2 -i dict.txt -o dict.cfsa"'`
