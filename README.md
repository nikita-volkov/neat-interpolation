#NeatInterpolation

A quasiquoter for producing `Text` data with a 
simple interpolation of input values. It removes the excessive indentation 
from the input text and accurately manages the indentation of all lines of 
interpolated variables. But enough words, the code shows it better.

Consider the following declaration:

```haskell
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import NeatInterpolation
import qualified Data.Text.IO as Text

f :: Text -> Text -> Text
f a b = 
  [text|
    function(){
      function(){
        $a
      }
      return $b
    }
  |]
```

Executing the following:

```haskell
main = Text.putStrLn $ f "1" "2"
```

will produce this (notice the reduced indentation compared to how it was
declared):

```
function(){
  function(){
    1
  }
  return 2
}
```

Now let's test it with multiline text parameters:

```haskell
main = Text.putStrLn $ f 
  "{\n  indented line\n  indented line\n}" 
  "{\n  indented line\n  indented line\n}" 
```

We get

```
function(){
  function(){
    {
      indented line
      indented line
    }
  }
  return {
    indented line
    indented line
  }
}
```

See how it neatly preserved the indentation levels of lines the 
variable placeholders were at?  