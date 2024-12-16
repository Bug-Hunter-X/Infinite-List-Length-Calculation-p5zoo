The solution depends on what you want to achieve.  There is no way to directly compute the length of an infinite list.

**Option 1:  Finite List Processing**
If you only need to process a finite portion of the list, use `take` to limit the list's size before calculating its length.

```haskell
import Prelude hiding (length)

main :: IO ()
main = do
  let finiteList = take 10 [1..]  -- Take the first 10 elements
  print (length finiteList)  -- This will terminate
```

**Option 2:  Lazy Evaluation and Alternative Operations**
If you need to work with an infinite list without computing its length, consider using lazy evaluation to your advantage.  Many Haskell functions can operate on infinite lists without problems. For example, instead of calculating length, you might calculate the sum of the first N numbers using a function like `sum`.

```haskell
import Prelude hiding (length)

main :: IO ()
main = do
  let firstTenSum = sum (take 10 [1..])
  print firstTenSum
```

**Option 3:  Handling Lists of Unknown Length**
If you have a list of unknown length, you might need to reconsider your processing approach. Instead of relying on length, use recursive methods or other techniques suitable for handling lists of indeterminate size.