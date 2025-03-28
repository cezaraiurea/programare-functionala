module Cursor exposing (Cursor, back, current, forward, fromList, length, nonEmpty, toList, withSelectedElement)

{-| Data structure to efficiently navigate a list forward or backward.

It stores a non-empty list as two lists and one element that is currently "selected".

For example, the list `[1, 2, 3]`, when focused on the first element, would be stored as `Cursor [] 1 [2, 3]`.
To focus on the second element, the representation becomes `Cursor [1] 2 [3]`.
Finally, focusing on the third element is: `Cursor [2, 1] 3 []`.

**Note that the left part of the list is stored in reverse order!**

-}
import Char exposing (isOctDigit)

type Cursor a
    = Cursor (List a) a (List a)

withSelectedElement : List a -> a -> List a -> Cursor a
withSelectedElement left mid right =
    Cursor (List.reverse left) mid right

nonEmpty : a -> List a -> Cursor a
nonEmpty x xs =
    Cursor [] x xs

{-| Creates a `Cursor` from a `List`, if the list is not empty -}
fromList : List a -> Maybe (Cursor a)
fromList lista = 
    case lista of
      [] -> Nothing
      x :: xs -> Just (Cursor [] x xs)

{-| Convert the `Cursor` to a `List` -}
toList : Cursor a -> List a
toList (Cursor left mid right) =
    List.reverse left ++ mid :: right

{-| Get the current element from the cursor -}
current : Cursor a -> a
current (Cursor _ a _) =
    a

{-| Move the cursor forward -}
forward : Cursor a -> Maybe (Cursor a)
forward (Cursor left mid right) =
    case right of
      [] -> Nothing
      x :: xs -> Just (Cursor (mid::left) x xs)

{-| Move the cursor backward -}
back : Cursor a -> Maybe (Cursor a)
back (Cursor left mid right) =
    case left of 
      [] -> Nothing
      x :: xs -> Just (Cursor xs x (mid::right))

{-| Get the number of elements -}
length : Cursor a -> Int
length (Cursor left _ right) =
     List.length left + 1 + List.length right
