\documentclass[b5paper]{article}

\usepackage{../book/base}

\title{Summation of primes}
\author{Brandon Munshaw}
\date{April 13, 2020}

\begin{document}

\maketitle

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.\\

Find the sum of all the primes below two million.

\section{Turners Sieve}

We can reuse the sieve from problem 7.

\begin{code}
primesTo :: Int -> [Int]
primesTo m = sieve m [2..m]
  where
    sieve m (f:r)
        | f*f > m   = f : r
        | otherwise = f : sieve m [n | n <- r, rem n f > 0]

main :: IO ()
main = print $ sum . primesTo $ 2*10^6
\end{code}

\end{document}