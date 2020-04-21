\documentclass[b5paper]{article}

\usepackage{../book/base}

\title{Largest palindrome product}
\author{Brandon Munshaw}
\date{March 24, 2020}

\begin{document}

\maketitle

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is $9009 = 91 \times 91$.\\

Find the largest palindrome made from the product of two 3-digit numbers.

\section{Generate palindromes then test}

I approach this by generating a descending list of palindromes, and testing for two 3-digit factors.

\begin{code}
main :: IO ()
main = print $ head $ filter hasThreeDigitFactors palindromes
\end{code}

Generating the palindromes is just a matter of manual digit manipulation.

\begin{code}
palindromes :: [Int]
palindromes =
   [ 1000 * n
   + 100  * rem n 10
   + 10   * rem (div n 10) 10
   + 1    * div n 100
   | n <- [997,996..100]]
\end{code}

Now we just need to test to see if a palindrome has two 3-digit factors. Luckily, there are a few things we can do to reduce the number of tests.

\begin{theorem}
3-digit palindromes are divisible by 11.
\end{theorem}

\begin{proof}
3-digit palindromes have the form $100000c+10000b+1000a+100a+10b+c = 100001c+10010b+1100a = 11(9091c+910b+100a)$
\end{proof}

So the idea is to test for divisibility starting at 990 with decrements of 11. We stop when the quotient is more than 3 digits.

\begin{code}
hasThreeDigitFactors :: Int -> Bool
hasThreeDigitFactors p =
    or $ ((== 0) . rem p) <$> [990,979..div p 999]
\end{code}

\end{document}