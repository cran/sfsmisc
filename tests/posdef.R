library(sfsmisc)

options(digits=9)

set.seed(12)
m <- matrix(round(rnorm(25),2), 5, 5); m <- 1+ m + t(m); diag(m) <- diag(m) + 4
(mp  <- posdefify(m))
(mp. <- posdefify(m, method = "allEV"))

stopifnot(eigen(mp,  only.val=TRUE)$values > 0,
          eigen(mp., only.val=TRUE)$values > 0,
          all.equal(diag(m), diag(mp), tol= 1e-15),
          all.equal(diag(m), diag(mp.),tol= 1e-15),
          T)
