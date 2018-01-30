First use alex to create a haskell file by using the command:
    alex a1.x

You can then open ghci by typing the following into the terminal:
    ghci

Once inside the following to complile:
    :l a1.hs

Then you can run the lexer by calling its function:
    lexer "filename goes here"

One thing to watch out for that the lexer won't catch for you is mismatched brackets.