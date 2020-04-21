\documentclass[b5paper]{article}

\usepackage{../book/base}

\title{Largest prime factor}
\author{Brandon Munshaw}
\date{March 24, 2020}

\begin{document}

\maketitle

The prime factors of 13195 are 5, 7, 13 and 29.\\

What is the largest prime factor of the number 600851475143?

\section{Trial Division}

If we completely divide out the natural numbers in succession, the last number we divide out must be the largest prime factor. We can improve this in a couple of ways. After we divide out 2, we will not be able to divide out any multiples of 2. Thus, we only need to test 2 and the odd natural numbers. Also, since factors come in pairs above and below the square root, we only need to test up to the square root to confirm there are no other divisors.

\begin{code}
greatestPrimeFactor :: Int -> Int
greatestPrimeFactor n = gpf n $ 2:[3,5..]
  where
    gpf n twoAndOdds
       | m*m > n     = n
       | rem n m > 0 = gpf n $ tail twoAndOdds
       | otherwise   = gpf (div n m) twoAndOdds
         where m = head twoAndOdds

main :: IO ()
main = print $ greatestPrimeFactor 600851475143
\end{code}

\end{document}