# Problem 5.14 A car-insurance company classifies drivers in three categories: bad, neutral and good. The reclassification is done in January of each year and the probabilities for transitions between different categories is given by
P
=
  ⎡
⎢
⎣
1
/
  2
1
/
  2
0
1
/
  5
2
/
  5
2
/
  5
1
/
  5
1
/
  5
3
/
  5
⎤
⎥
⎦
,
where the first row/column corresponds to the bad category, the second to neutral and the third to good. The company started in January 1990 with 1400 drivers in each category. Estimate the number of drivers in each category in 2090. Assume that the total number of drivers does not change in time and use R for your computations.

P = matrix(
  c(1/2 , 1/2 , 0,
    1/5 , 2/5 , 2/5 ,
    1/5 , 1/5 , 3/5),
  byrow=T, ncol=3)

# a0 needs to be a row matrix
a0 = matrix(c(1/3, 1/3, 1/3), nrow=1)

P100 = diag(3) # the 3x3 identity matrix
for (i in 1:100)
  P100 = P100 %*% P

(a0 %*% P100) * 4200