\documentclass[b5paper]{article}

\usepackage{../book/base}

\title{10001st prime}
\author{Brandon Munshaw}
\date{March 25, 2020}

\begin{document}

\maketitle

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.\\

What is the 10 001st prime number?

\section{Turners Sieve}

In Turners sieve, when a prime number is read (starting at 2), it's multiples are (lazily) eliminated from the list of natural numbers, thus the next number will also be prime (fundamental theorem of arithmetic). It is currently featured on the \href{http://www.haskell.org}{Haskell homepage}.

\begin{spec}
primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]
\end{spec}

This may not be the fastest sieve, but we can improve it a little. Since factors come in pairs above and below the square root, we only need to test up to the square root of the largest prime we need.

\begin{code}
primesTo :: Int -> [Int]
primesTo m = sieve m [2..m]
  where
    sieve m (f:r)
        | f*f > m   = f : r
        | otherwise = f : sieve m [n | n <- r, rem n f > 0]
\end{code}

Where max is the largest prime we want to generate. Unfortunately, we don't know how large our answer is. We will need to guess our maximum value. After some experimentation (inspired by the Prime number theorem), I believe $n \ln(n\ln(n))$ is a good choice.

\begin{code}
guessPrimeN :: Int -> Int
guessPrimeN n = floor (n' * log (n' * log n'))
  where n' = fromIntegral n
\end{code}

Now we generate the list of primes up to our guess, and print the 10001st element if it exists.

\begin{code}
main :: IO ()
main = print $ primesTo (guessPrimeN 10000) !! 10000
\end{code}

This is fast enough for now, but we'll likely need a faster prime number generator later.

\end{document}