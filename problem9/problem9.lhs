\documentclass[b5paper]{article}

\usepackage{../book/base}

\title{Special Pythagorean triplet}
\author{Brandon Munshaw}
\date{April 11, 2020}

\begin{document}

\maketitle

A Pythagorean triplet is a set of three natural numbers, $a<b<c$, for which,

\[
    a^2+b^2=c^2
\]

For example, $3^2 + 4^2 = 9 + 16 = 25 = 5^2$.\\

There exists exactly one Pythagorean triplet for which $a+b+c=1000$.
Find the product $abc$.

\section{Naive solution}

This could be computed reasonably fast using a direct approach. Without loss of generality, assume that $b \leq a$ and $c = 1000-a-b$. Now we search $a$ and $b$ until $a^2+b^2=c^2$.

\begin{spec}
head [ a*b*(1000-a-b)
     | a <- [1..]
     , b <- [1..a]
     , a^2+b^2 == (1000-a-b)^2]
\end{spec}

This works by generating triplets whose sum is 1000, and checking if they are Pythagorean Triples. Lets make this faster by generating Pythagorean Triplets, and checking if their sum is 1000.

\section{Generating Pythagorean triplets}

We need to be able to generate pythagorean triplets in linear time. If we were to write $c$ in terms of $a$, and $b$, we would still need to search for perfect squares. Not good enough, we need to try to eliminate the square root.

\begin{align*}
a^2+b^2=c^2\\
a^2=(c-b)(c+b)\\
\frac{a}{c-b}=\frac{c+b}{a}
\end{align*}

There is no meaningful way to manipulate this. Lets introduce some new variables. If we could get $a$ as a denominator, we could add and subtract these, so let's introduce the rational number $m/n$.

\begin{align*}
\frac{c-b}{a}=\frac{n}{m}, \frac{c+b}{a}=\frac{m}{n}\\
\frac{c}{a}-\frac{b}{a}=\frac{n^2}{mn}, \frac{c}{a}+\frac{b}{a}=\frac{m^2}{mn}\\
\frac{2c}{a}=\frac{m^2+n^2}{mn}, \frac{2b}{a} = \frac{m^2-n^2}{mn}\\
\end{align*}

If we could equate the numerators together and the denominators together, than we would have a nice parameterization! Of course this would be off by a factor for each equation. Intuitively, it looks like the factor is actually shared between $a,b,c$ given the similarities of the denominators and the properties of congruent triangles. So let's assume this factor is $\alpha$. We have,

\[
a=\alpha(2mn), b=\alpha(m^2-n^2), c=\alpha(m^2+n^2)\\
\]

And this can be shown by verifying

\[
(\alpha(2mn))^2 + (\alpha(m^2-n^2))^2 = (\alpha(m^2+n^2))^2
\]

We call a triplet without the $\alpha$ factor a base triplet.

\begin{code}
baseTriplet :: Int -> Int -> [Int]
baseTriplet m n = [2*m*n, m^2-n^2, m^2+n^2]
\end{code}

Now we need to test if a base triplet divides 1000. Notice that base triplet $a+b+c=2m(m+n)$.

\begin{code}
doesTripletDivide1000 :: Int -> Int -> Bool
doesTripletDivide1000 m n = rem (2*m*(m+n)) 1000 == 0
\end{code}

Now we find $\alpha$ to multiply our base triplet by to make it's sum 1000.

\begin{code}
multiplyTo1000 :: Int -> Int -> Int -> Int
multiplyTo1000 m n = (*) $ div (2*m*(m+n)) 1000
\end{code}

Our algorithm will generate natural numbers $n \leq m$, and search for a triplet whose sum divides 1000. Then we will multiply the triplet so it's sum is 1000. Finally, we print the product of the triplet.

\begin{code}
main :: IO ()
main = print $ product $ triplet
  where
    triplet = head [ multiplyTo1000 m n <$> baseTriplet m n
                   | m <- [1..]
                   , n <- [1..m]
                   , doesTripletDivide1000 m n]
\end{code}

It might be possible to optimise this further by considering the implications of the constraint $1000=a+b+c=\alpha(m^2+2mn)$, but the current solution is quick enough.

\end{document}