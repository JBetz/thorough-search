# thorough-search

## Why?

To generate a complete dataset of a given word.

## How it works

There are four stages
1. **Search**: It takes a search phrase (e.g., "sword of X") and generates search strings by replacing X with each letter of the alphabet. It then queries Google's autocomplete API with each of those strings, records the results, and recursively expands on any of them that return the max number of results using the same process for generating the original set of search strings. I.e., each letter of the alphabet is appended to the search string to generate a new set of search strings.
2. **Filter**: All of the results are filtered using the SCOWL word lists to get rid as much of the garbage results as possible.
3. **Sort**: Using the SCOWL word lists, the results are sorted by their commonality ranking.
4. **Commit**: The sorted and filtered results are then written to a file.

Note: Search results are cached in a SQLite database so that the thorough search for a given phrase can be resumed if the program is interrupted for any reason.

## How to run it

1. Clone this repository: `git clone git@github.com:JBetz/thorough-search.git`
2. Install nix: `curl https://nixos.org/nix/install | sh`
3. Enter shell: `nix-shell --attr env release.nix`
4. Create output directory: `mkdir output`
5. Run `cabal new-run ts <query>`, where \<query> is of the form "\<word> of X", "X \<word>", or "\<word> X". E.g., "sword of X", "X integrity", and "forgotten X" are all valid inputs.

## TODO
 - automatically email results
 - alternative sorting mechanism when over 30K results
 - add sqlite dependency to nix derivation