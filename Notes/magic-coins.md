Magic Coins
========================================================




### Required
* A set of coins to hand out to students.  You'll want to do with with at least 10 students.

### Introduction
I have a collection of coins from various countries and various eras, e.g. 100 Liras, 5 pesos, 10 kopecks.  I tell them that not all coins are fair.  Some coins, when you flip them, are more likely to come up heads, and some are more likely to come up tails.

I hand out the coins to students, taking care to refer to each coin individually as if it were something special.  Then I ask the students for their names and the kind of coin, and write a table on the board, like this:

Name   | Coin
------------
Julie  | 100 centavos
Mike   | 50 pence
Anne   | 1 deutschmark

and so on for at least 10 students --- more is better.

### Activity

Ask each student to flip his or her coin 6 times and count the number of heads.  Since not all the coins have an actual "head," they can decide for themselves what "head" means.  Then they report how many heads they got.

Ask the students, how many should they have gotten if the coin was fair?  Generally, students will say 3, but some will acknowledge that it might be 2 or 4.  Try to get a consensus from the class about what deviation from 5 is acceptable.

Now tally

Name   | Coin   |   Heads
--------------------------
Julie  | 100 centavos  | 4
Mike   | 50 pence      | 2
Anne   | 1 deutschmark | 5

and so on ....

Approach the student with the most extreme result, be it high or low.  Look at the coin.  Ask them why they think it wasn't fair.

### Discussion

Point out that there is no physical reason to think that flipping a coin --- whatever the coin --- doesn't give a fair result.  (Spinning the coin on edge is different.  Canadian pennies, for instance, have a bias due to the bevel on their edge.)

* How would they decide what's a reasonable deviation to expect?

Here is a formal way to approach the situation: simulation of 

```r
rbinom(10, prob = 0.5, size = 6)
```

```
##  [1] 3 2 2 2 2 5 3 3 4 2
```


If we do it many times, we can estimate the probability of each result:

```r
tally(rbinom(1e+05, prob = 0.5, size = 6))
```

```
## 
##      0      1      2      3      4      5      6  Total 
##   1525   9390  23555  31291  23527   9235   1477 100000 
```


It looks like a 2, 3, 4 should happen the large majority of times, about 75%.  There's only about a 3% chance of getting a 0 or 6.

* Ask someone who got a 0, 1, 5, or 6 what they think.  Is their coin fair?

* Point out that the real process is a bit more complicated.  I picked out the person with the most extreme result.  That's a more complicated process.  Here's an estimate of the result with 10 students flipping coins.

```r
tally(~result, data = do(100) * max({
    flips = rbinom(10, prob = 0.5, size = 6)
    pmax(6 - flips, flips)
}))
```

```
## 
##     4     5     6 Total 
##     8    59    33   100 
```

And with 20 students:

```r
tally(~result, data = do(100) * max({
    flips = rbinom(20, prob = 0.5, size = 6)
    pmax(6 - flips, flips)
}))
```

```
## 
##     4     5     6 Total 
##     2    49    49   100 
```


With 10 students flipping, I'm almost certain to get a result of 5 or 6.
With 20 students, even more so, and there is a very substantial chance of getting at least one 6.
With 100 students in a class, I'm almost certain to get a 6.

* How could I nontheless get a good idea about whether the coin I picked out is fair?  Ans: repeat the flipping on that coin!

This course will be about the analysis of data, not coin flips.  But the analogous phenomenon can happen: you draw a conclusion because of a chance fluctuation.  You need to know how to guard against this.


