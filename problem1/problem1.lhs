\documentclass[b5paper]{article}

\usepackage{../book/base}

\title{Multiples of 3 and 5}
\author{Brandon Munshaw}
\date{March 24, 2020}

\begin{document}

\maketitle

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.\\

Find the sum of all the multiples of 3 or 5 below 1000.\\

\section{Naive solution}

Since the limit is small, we can solve this directly with

\begin{spec}
sum [n | n <- [1..999], rem n 3 == 0 || rem n 5 == 0]
\end{spec}

This will run \emph{almost instantly} on modern hardware. But, suppose we wanted to improve this for limits much larger than 1000. We will define the function $\text{solve}(n)$ to be the sum of all the multiples of 3 and 5 below $n$.

\begin{code}
main :: IO ()
main = print $ solve 999
\end{code}

\section{Triangle numbers}

Lets break the problem down into separate sums: the sum of multiples of 3, plus the sum of multiples of 5. Since multiples of 15 are added twice, we need to subtract the sum of multiples of 15.

\begin{align*}
(3+6+\dots+999) + (5+10+\dots+995) - (15+30+\dots+985)\\
= 3(1+2+\dots+333) + 5(1+2+\dots+199) - 15(1+2+\dots+16)
\end{align*}

Numbers in the form $1+2+\dots+n$ are known as triangle numbers. If we had a function to compute them quickly, this can be solved as

\begin{code}
solve :: Int -> Int
solve n =
    3  * triangle (div n 3)
  + 5  * triangle (div n 5)
  - 15 * triangle (div n 15)
\end{code}

Luckily, we can use the following series to compute triangle numbers in constant time! The following is a proof to show the series is true, and you can use the technique from problem 6 to develop it.

\begin{theorem}
For $n\in\mathbb{N}$,
\[\sum_{i=1}^{n}i = \frac{n(n+1)}{2}\]
\end{theorem}

\begin{proof}
See that this holds for $n=1$. Suppose this holds for $n=k$.
\begin{align*}
\sum_{i=1}^{k}i = \frac{k(k+1)}{2}\\
\implies \sum_{i=1}^{k}i + (k+1) = k\frac{k+1}{2} + (k+1)\\
\implies \sum_{i=1}^{k+1}i = \frac{k(k+1)+2(k+1)}{2} = \frac{(k+1)(k+2)}{2}
\end{align*}
Thus, this holds for $n=k+1$. By mathematical induction, $n\in\mathbb{N}$ holds.
\end{proof}

Using that series, we can define our efficient triangle function.

\begin{code}
triangle :: Int -> Int
triangle n = div (n^2+n) 2
\end{code}

\end{document}