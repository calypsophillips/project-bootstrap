# project-bootstrap
Code used for bootstrap in portfolio management project

#Standard bootstrap methods:

#Finding bootstrap samples and replications using R

bootsamples <- lapply(1:B, function(i)sample(x, replace=T))

bootreps <- sapply(bootsamples, s)

#Alternatively

X.boot <- boot(data=X, statistic=s, R=n)

#Using these samples and the replication code, the standard error can then be found.

bootmeans <- sapply(bootsamples, mean)

sdot <- sum(bootmeans)/B

seboot <- ((sum(bootmeans-sdot)^2)/(B-1))^(1/2)

#Resampling vectors

f <- boot.array(X.boot)

#Non-overlapping block bootstrap with length 10, 1000 reps

fixedblockX <- tsboot(RDSBlogs, mu.fun, R=1000, l=10, sim="fixed")

#Stationary

Li <- rgeom(k, p) %Find lengths

Ii <- rdunif(k, n, a=1) %Find initial points of blocks

statr <- tsboot(RDSBlogs, mu.fun, R=1000, l=10, sim="geom")

#CAPM models:

#To find the coefficients alpha and beta for the CAPM models of each asset, where X is the vector of their stock prices in natural  logarithmic form, and FTSElogs is the market prices over the same time period, also in logarithmic form.

r.free <- 0.005

p <- Xlogs - r.free

q <- FTSElogs - r.free

capm <- lm(y~z)

#The bootstrap samples and bootstrap replicates of the beta values are then found using the following function, for n bootstrap samples, to create a list of n bootstrapped beta values.

Alphaboot <- function(x,y,z){
bs <- sample(x, 59, TRUE)
Bootres <- c(y+bs)
return(lm(Bootres~z)$coefficients[1])
}

alphasX <- replicate(n, Alphaboot(residX, p, q))

Betaboot <- function(x,y,z){
bs <- sample(x, 59, TRUE)
Bootres <- c(y+bs)
return(lm(Bootres~z)$coefficients[2])
}

residX <- capm$residuals

betasX <- replicate(n, Betaboot(residX, p, q))

#Finding log returns of asset X

Rl <- diff(Xlogs, lag=1, differences=1)

#Semiparametric bootstrap:

#To form 59x3 matrix of data for three assets (A,B,C) with 59 observed values each:

X <- matrix(c(A, B, C), nrow=60, ncol=3, byrow=FALSE)

#Transform X to log returns. The CAPM models of each asset are found as above, and the standardised residuals are found and then form another matrix. From this matrix, Z, the rank matrix R is found.

stdresX <- rstandard(capmX) 

Z <- matrix(c(stdresR, stdresC, stdresB), nrow=59, ncol=3, byrow=FALSE)

R <- matrix(c(order(stdresR), order(stdresC), order(stdresB)), nrow=59, ncol=3, byrow=FALSE)

#Generate 59 innovations from the distribution of each set of residuals and then order these according to the rank matrix R, and combine to form new matrix of values Z* then put back into CAPM to find X*:

spx1 <- rnorm(59, mean(stdresX), sd(stdresX))

ZX1 <- spx1[order(stdresX)]

Z1 <- matrix(c(ZR1, ZC1, ZB1), nrow=59, ncol=3, byrow=FALSE)

X1 <- matrix(c(alphaR+(betaR* q)+r.free+Z[,1], alphaC+(betaC* q)+r.free+Z[,2], alphaB+(betaB* q)+r.free+Z[,3]),nrow=59, ncol=3, byrow=FALSE)

#For B semiparametric efficient frontiers:

inovs <- function(a,b,c,d,e,f,g,h,i,q){
+ aa <- rnorm(59, mean(a), sd(a))
+ bb <- rnorm(59, mean(b), sd(b))
+ cc <- rnorm(59, mean(c), sd(c))
+ a1 <- aa[order(a)]
+ b1 <- bb[order(b)]
+ c1 <- cc[order(c)]
+ x <- d+(e* q)+0.005+a1
+ y <- f+(g* q)+0.005+b1
+ z <- h+(i* q)+0.005+c1
+ mic.fun(x,y,z)
}

semis <- function(a,b,c,d,e,f,g,h,i,q,B){
replicate(B, inovs(a,b,c,d,e,f,g,h,i,q))
}

#Michaud (Using portfolio.r package created by Eric Zivot)

covmatrix <- function(x,y,z){
+	cv <- matrix(c(cov(x,x),cov(x,y),cov(x,z),cov(y,x),cov(y,y),cov(y,z),cov(z,x),
+	cov(z,y),cov(z,z)),nrow=3, ncol=3)
+	return(cv)
}

means <- function(x,y,z){
+	mu <- c(mean(x),mean(y),mean(z))
+	return(mu)
}

mic.fun <- function(x,y,z){
+ er <- means(x,y,z)
+ covmat <- covmatrix(x,y,z)
+ ew <- rep(1,3)/3
+ r.free <- 0.005
+ equalWeight.portfolio <- getPortfolio(er=er,cov.mat=covmat,weights=ew)
+ gmin.port <- globalMin.portfolio(er, covmat)
+ target.return <- er[1]
+ e.port.msft = efficient.portfolio(er, covmat, target.return)
+ ef <- efficient.frontier(er, covmat, alpha.min=-2, alpha.max=1.5, nport=20)
+ return(ef)
}

mics <- function(a,b,c){
+ x <- sample(a, replace=T)
+ y <- sample(b, replace=T)
+ z <- sample(c, replace=T)
+ mic.fun(x,y,z)
}

micef <- function(p,q,r,B){
+ replicate(B, mics(p,q,r))
}

#Black-Litterman:

tau <- 1/60

Pi <- delta* covmat%* %weq

OI <- solve(Omega)

tSig <- tau* covmat

BLsd <- solve(solve(tSig)+t(P)%* %OI%* %P)

BLer <- BLsd%* %(solve(tSig)%* %Pi)+(t(P)%* %OI%* %Q)

BLsig <- covmat+BLsd

BLweights <- solve(delta* BLsig)%* %Pi




