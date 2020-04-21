\documentclass[b5paper]{article}

\usepackage{../book/base}

\title{Smallest Multiple}
\author{Brandon Munshaw}
\date{March 25, 2020}

\begin{document}

\maketitle

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.\\

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

\section{Least common multiple}

Haskell provides a least common multiple primitive! Otherwise it would be a matter of writing lcm in terms of gcd and using the Euclidean algorithm.

\begin{code}
main :: IO ()
main = print $ foldr1 lcm [1..20]
\end{code}

\end{document}