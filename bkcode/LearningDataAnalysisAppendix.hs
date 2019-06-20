{-

No functions were introduced in the Appendix.

Here are each of the lines of code used in this
chapter.

import Text.Regex.Posix
("My name is Jim." =~ "Jim") :: Bool
("My name is Frank." =~ "Jim") :: Bool
("a" =~ "a*") :: Bool
("aaaaaaaa" =~ "a*") :: Bool
("rabbit" =~ "a*") :: Bool
("cow" =~ "a*") :: Bool
("" =~ "a*") :: Bool
("a" =~ "a+") :: Bool
("aaaaaaaa" =~ "a+") :: Bool
("rabbit" =~ "a+") :: Bool
("cow" =~ "a+") :: Bool
("" =~ "a+") :: Bool
("color" =~ "colou?r") :: Bool
("colour" =~ "colou?r") :: Bool
("coluor" =~ "colou?r") :: Bool
("grandmother" =~ "^grand") :: Bool
("hundred grand" =~ "^grand") :: Bool
("writing" =~ "ing$") :: Bool
("zingers" =~ "ing$") :: Bool
("a" =~ "^a*$") :: Bool
("aaaaaaaa" =~ "^a*$") :: Bool
("rabbit" =~ "^a*$") :: Bool
("cow" =~ "^a*$") :: Bool
("" =~ "^a*$") :: Bool
("." =~ ".") :: Bool
("a" =~ ".") :: Bool
("." =~ "\\.") :: Bool
("a" =~ "\\.") :: Bool
("grey" =~ "gr[ae]y") :: Bool
("gray" =~ "gr[ae]y") :: Bool
("graey" =~ "gr[ae]y") :: Bool
("rabbit" =~ "^[^aeiou]+$") :: Bool
("cow" =~ "^[^aeiou]+$") :: Bool
("why" =~ "^[^aeiou]+$") :: Bool
("a" =~ "[a-z]") :: Bool
("A" =~ "[a-z]") :: Bool
("s" =~ "[A-Z]") :: Bool
("S" =~ "[A-Z]") :: Bool
("S" =~ "[a-zA-Z]") :: Bool
("row your boat" =~ "(row, )+row your boat") :: Bool
("row, row your boat" =~ "(row, )+row your boat") :: Bool
("row, row, row your boat" =~ "(row, )+row your boat") :: Bool
("row, row, row, row your boat" =~ "(row, )+row your boat") :: Bool
("row your boat" =~ "^(row, ){2}row your boat$") :: Bool
("row, row your boat" =~ "^(row, ){2}row your boat$") :: Bool
("row, row, row your boat" =~ "^(row, ){2}row your boat$") :: Bool
("row, row, row, row your boat" =~ "^(row, ){2}row your boat$") :: Bool
("1898" =~ "^1899|19[0-9][0-9]|20[0-9][0-9]$") :: Bool
("1899" =~ "^1899|19[0-9][0-9]|20[0-9][0-9]$") :: Bool
("1900" =~ "^1899|19[0-9][0-9]|20[0-9][0-9]$") :: Bool
("1999" =~ "^1899|19[0-9][0-9]|20[0-9][0-9]$") :: Bool
("2015" =~ "^1899|19[0-9][0-9]|20[0-9][0-9]$") :: Bool
("2115" =~ "^1899|19[0-9][0-9]|20[0-9][0-9]$") :: Bool

-}
