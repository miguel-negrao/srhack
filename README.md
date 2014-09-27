srhack
======

srhack: change samplerate in header of wave file

usage: srhack sr file newfile
  sr: new sample rate
  file: file path
  newfile file path of converted file
  
Installing
==========

globally:

    cabal install
    
with sandbox:

```
cabal sandbox init
cabal install --only-dependencies
cabal build
```

Miguel Negrão 2014
GPL 3.0
  
