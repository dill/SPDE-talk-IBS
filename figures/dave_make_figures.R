# make some figures for Dave


library(mgcv)
library(ggplot2)
library(gridExtra)

## smoothing parameter example

# hacked from the example in ?gam
set.seed(2) ## simulate some data... 
dat <- gamSim(1,n=50,dist="normal",scale=0.5, verbose=FALSE)
dat$y <- dat$f2 + rnorm(length(dat$f2), sd = sqrt(0.5))
f2 <- function(x) 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10-mean(dat$y)
ylim <- c(-4,6)

# fit some models
b.justright <- gam(y~s(x2),data=dat)
b.sp0 <- gam(y~s(x2, sp=0, k=50),data=dat)
b.spinf <- gam(y~s(x2),data=dat, sp=1e10)

pdf("penalty.pdf", width=10, height=5)
par(mfrow=c(1,3), cex.main=3.5, pch=19, lwd=2)

plot(b.justright, se=FALSE, ylim=ylim, main=expression(lambda*plain("= just right")),
     rug=FALSE, xlab="")
points(dat$x2, dat$y-mean(dat$y))
curve(f2,0,1, col="blue", add=TRUE)

plot(b.sp0, se=FALSE, ylim=ylim, main=expression(lambda*plain("=")*0),
     rug=FALSE, xlab="")
points(dat$x2, dat$y-mean(dat$y))
curve(f2,0,1, col="blue", add=TRUE)

plot(b.spinf, se=FALSE, ylim=ylim, main=expression(lambda*plain("=")*infinity),
     rug=FALSE, xlab="")
points(dat$x2, dat$y-mean(dat$y))
curve(f2,0,1, col="blue", add=TRUE)
dev.off()




## compare to linear model
set.seed(2) ## simulate some data...
dat <- gamSim(1, n=400, dist="normal", scale=1, verbose=FALSE)
dat <- dat[,c("y", "x0", "x1", "x2", "x3")]
p <- ggplot(dat,aes(y=y,x=x2)) +
  labs(x="", y="") +
  theme_minimal() +
  geom_point()
p1 <- p + geom_smooth(method="lm")
p2 <- p + geom_smooth(method="gam", formula=y~s(x))
pdf("lmorsmooth.pdf", width=12, height=5)
grid.arrange(p1,p2, ncol=2)
dev.off()


## approximations
library(raster)


require("rerddap")
sstInfo <- info('jplMURSST41')
# get latest daily sst
murSST <- griddap(sstInfo, latitude = c(56, 57), longitude = c(5, 7), time = c('last','last'), fields = 'analysed_sst')

mm <- raster(matrix(murSST$data$analysed_sst, 101, 101))
plot(mm)
s <- raster(nrow=50, ncol=50)
plot(resample(mm, s))
plot(mm)
plot(mm)
