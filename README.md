# HORTH

Stack based programming language written in haskell


## example
```haskell
ghci> run [Push (MyNum 34), Push (MyNum 34), Push (MyNum 1), Add, Add]
Left [MyNum 69]
ghci> run [Push (MyStr "World"), Push (MyStr " "), Push (MyStr "Hello"), Add, Add]
Left [MyStr "Hello World"]
ghci> 
```
