ghc -outputdir ghc-out -o Main -isrc src/Main.hs -prof -fprof-auto && ./Main +RTS -p
