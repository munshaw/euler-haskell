\documentclass[b5paper]{article}

\usepackage{../book/base}

\title{Sum square difference}
\author{Brandon Munshaw}
\date{March 25, 2020}

\begin{document}

\maketitle

The sum of the squares of the first ten natural numbers is,

\begin{align*}
     1^2+2^2+\dots+10^2=385
\end{align*}

The square of the sum of the first ten natural numbers is,

\begin{align*}
    (1+2+\dots+10)^2=55^2=3025
\end{align*}

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025-385=2640\\

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

\section{Naive solution}

The limit is small enough that we can do this directly.

\begin{spec}
(sum [1..100])^2 - (sum $ (^2) <$> [1..100])
\end{spec}

We can reuse the same tricks from problem 1 to make this run in constant time! We just need to write an efficient implementation for $\text{squareOfSum}(n)$ and $\text{sumOfSquare}(n)$ and our solution is

\begin{code}
main :: IO ()
main = print $ squareOfSum 100 - sumOfSquare 100
\end{code}

\section{Using series}

We know that the sum of the first $n$ natural numbers is $n(n+1)/2$, so its square is simply

\begin{code}
squareOfSum :: Int -> Int
squareOfSum n = (div (n^2+n) 2)^2
\end{code}

Now we figure out the series for the sum of squares. The following is a technique to develop the series, and you can use the technique from problem 1 to prove it more rigorously.

\begin{theorem}
For $n\in\mathbb{N}$,
\[\sum_{i=1}^{n}i^2 = \frac{n(n+1)(2n-1)}{6}\]
\end{theorem}

\begin{proof}
Consider the first few terms of the sum.\\

\begin{tabular}{|c|c|c|c|c|}
\hline
i & $\sum i^2$ & 1st diff & 2nd diff & 3rd diff\\
\hline
0 & 0  & 1  & 3 & 2\\
1 & 1  & 4  & 5 & 2\\
2 & 5  & 9  & 7 &\\
3 & 14 & 16 &   &\\
4 & 30 &    &   &\\
\hline
\end{tabular}\\

This function seems to be cubic (a consecutive series of squares must be), so it must take the form of $an^3+bn^2+cn+d$. Now we solve the system of equations

\begin{align*}
d=0\\
a+b+c=1\\
8a+4b+2c=5\\
27a+9b+3c=14\\
\implies a=\frac{1}{3}, b=\frac{1}{2}, c=\frac{1}{6}\\
\implies \frac{n(n+1)(2n-1)}{6}
\end{align*}
\end{proof}

Now we can compute the som of square quickly.

\begin{code}
sumOfSquare :: Int -> Int
sumOfSquare n = div ((n^2+n)*(2*n+1)) 6
\end{code}

\end{document}