

colTrans = function(col,perc) {
  rgb(t(grDevices::col2rgb(col)), maxColorValue=255, alpha = perc*255)
}

require(quantreg)
require(Rfit)

Cyan = "#235A97" 
Pink = "#EA4B68" 
Gray = "#606060" 
Green = "#6ABA9A"
yellow = "#EACC48"


# EXAMPLES ------------------------------------------------------------


plt = function(x, y, nam = c('x','y'),lab='A'){
  
  x = scale(x)
  y = scale(y)
  
  par(mar=c(5,5,2,2))
  plot.new();plot.window(range(x),range(y))
  usr = par()$usr
  xpos = usr[2] - .9 * diff(usr[1:2])
  ypos = usr[3] + .95 * diff(usr[3:4])
  text(xpos,ypos,1,labels=lab,cex=5,font=2,col=Green)
  points(x,y,col=Gray,pch=16,cex=3.5)
  lines(rep(min(x),2)-.02 * diff(range(x)), range(y)*1.042,lty=1,lwd=6,col='grey75')
  lines(range(x)*1.042, rep(min(y),2)-.02 * diff(range(y)),lty=1,lwd=6,col='grey75')
  mtext(nam, side=c(1,2),las=1,font=2,line=c(3.2,1.55),cex=5)
  lines(x, predict(lm(y~x)), lwd=11, col=Pink)
  }

n = 300

# heteroskedascity

set.seed(107)

het_x1 = rnorm(n,0,10) + runif(n,-3,3)
het_y = .85*het_x1 + rnorm(n,0,10 + (het_x1+ 30)/2)

# normality

set.seed(102)

nor_x1 = rnorm(n,0,10) + runif(n,-3,3)
nor_y = nor_x1 + rbeta(100,2,100) * 1000

# linearity

set.seed(101)

lin_x1 = rnorm(n,0,2) + runif(n,-3,3)
lin_y = lin_x1**2 + 2*lin_x1 + rnorm(n,0,7)

# linearity

set.seed(101)
lin2_x1 = sort(rnorm(n,0,1.5) + runif(n,-3,3))
lin2_x2 = rep(c(rep(-1,16), rep(1,80),rep(-1,16)),length(lin2_x1))[(1:length(lin2_x1))+60]
lin2_y = 2*lin2_x2 + .5*lin2_x1 + rnorm(n,0,2)

png('_sessions/RobustStats/image/assumptions.png',width=1200,height=1200)

par(mfrow=c(2,2))
plt(het_x1,het_y,lab='A')
plt(nor_x1,nor_y,lab='B')
plt(lin_x1,lin_y,lab='C')
plt(lin2_x1,lin2_y,lab='D')

dev.off()


# RESIDUAL ANALYSIS ------------------------------------------------------------

plt_res = function(x, y, nam = c(expression(bold(paste(x,"'",beta))),expression(bold('r'))),lab='A'){
  
  x = scale(x)
  y = scale(y)
  y = y - predict(lm(y ~ x))
  
  par(mar=c(5,5,2,2))
  plot.new();plot.window(range(x),range(y))
  usr = par()$usr
  xpos = usr[2] - .9 * diff(usr[1:2])
  ypos = usr[3] + .95 * diff(usr[3:4])
  points(x,y,col=Gray,pch=16,cex=3.5)
  lines(rep(min(x),2)-.02 * diff(range(x)), range(y)*1.042,lty=1,lwd=6,col='grey75')
  lines(range(x)*1.042, rep(min(y),2)-.02 * diff(range(y)),lty=1,lwd=6,col='grey75')
  mtext(nam, side=c(1,2),las=1,font=2,line=c(4.1,1.55),cex=5)
  lines(x, predict(lm(y~x)), lwd=11, col=Pink)
  text(xpos,ypos,1,labels=lab,cex=5,font=2,col=Green)
}


png('_sessions/RobustStats/image/assumptions_res.png',width=1200,height=1200)

par(mfrow=c(2,2))
plt_res(predict(lm(het_y~het_x1)), het_y,lab='A')
plt_res(predict(lm(nor_y~nor_x1)), nor_y,lab='B')
plt_res(predict(lm(lin_y~lin_x1)), lin_y,lab='C')
plt_res(predict(lm(lin2_y~lin2_x1)),lin2_y,lab='D')

dev.off()


# MISSING VARS -----------------------------------------------------------------


png('_sessions/RobustStats/image/assumptions_missingvar.png',width=1200,height=1200)

set.seed(101)

x1 = sort(rnorm(n,0,2) + runif(n,-3,3))
x2 = x1 ** 2
y = x2 + 2*x1 + rnorm(n,0,7)

x = scale(x)
y = scale(y)
#y = y - predict(lm(y ~ x1))

par(mar=c(5,5,2,2),mfcol=c(2,2))

plot.new();plot.window(range(x1),range(y))
usr = par()$usr
xpos = usr[2] - .9 * diff(usr[1:2])
ypos = usr[3] + .95 * diff(usr[3:4])
text(xpos,ypos,1,labels="C",cex=5,font=2,col=Green)
points(x1,y,col=Gray,pch=16,cex=3.5)
lines(rep(min(x1),2)-.02 * diff(range(x1)), range(y)*1.042,lty=1,lwd=6,col='grey75')
lines(range(x1)*1.042, rep(min(y),2)-.02 * diff(range(y)),lty=1,lwd=6,col='grey75')
mtext(c("Stress","Error rate"), side=c(1,2),las=0,font=2,line=c(3,.55),cex=4)
lines(x1, predict(lm(y~x1)), lwd=11, col=Pink)
lines(x1, predict(lm(y~x1+x2)), lwd=11, col=Green)

xpos = usr[1] + c(.525,.575) * diff(usr[1:2])
ypos = usr[3] + c(.06,.11) * diff(usr[3:4])

lines(xpos,rep(ypos[1],2),lwd=10,col=Green,lend=2)
lines(xpos,rep(ypos[2],2),lwd=10,col=Pink,lend=2)
text(rep(xpos[2]+.02*diff(usr[1:2])),rev(ypos),
     labels =c(expression(paste(r," ~ ",Stress)),
               expression(paste(r," ~ ",Stress,"+",Stress^2))),adj=0,cex=2.5)

set.seed(101)
x1 = sort(rnorm(n,0,1.5) + runif(n,-3,3))
x2 = rep(c(rep(-1,16), rep(1,80),rep(-1,16)),length(lin2_x1))[(1:length(lin2_x1))+60]
y = 2*x2 + .5*x1 + rnorm(n,0,2)

x = scale(x)
y = scale(y)
y = y - predict(lm(y ~ x1 + x2))

plot.new();plot.window(range(x1),range(y))
usr = par()$usr
xpos = usr[2] - .9 * diff(usr[1:2])
ypos = usr[3] + .95 * diff(usr[3:4])
text(xpos,ypos,1,labels="C",cex=5,font=2,col=Green)
points(x1,y,col=Gray,pch=16,cex=3.5)
lines(rep(min(x1),2)-.02 * diff(range(x1)), range(y)*1.042,lty=1,lwd=6,col='grey75')
lines(range(x1)*1.042, rep(min(y),2)-.02 * diff(range(y)),lty=1,lwd=6,col='grey75')
mtext(c(expression(bold(paste(beta[1],"Stress + ",beta[2],Stress^2))),
        expression(bold("Residuals"))), side=c(1,2),las=0,font=2,line=c(3.8,.55),cex=4)
lines(x1, predict(lm(y~x1)), lwd=11, col=Pink)



set.seed(101)
x1 = sort(rnorm(n,0,1.5) + runif(n,-3,3))
x2 = rep(c(rep(-1,16), rep(1,80),rep(-1,16)),length(lin2_x1))[(1:length(lin2_x1))+60]
y = 2*x2 + .5*x1 + rnorm(n,0,2)

x = scale(x)
y = scale(y)
#y = y - predict(lm(y ~ x1))

plot.new();plot.window(range(x1),range(y))
usr = par()$usr
xpos = usr[2] - .9 * diff(usr[1:2])
ypos = usr[3] + .95 * diff(usr[3:4])
text(xpos,ypos,1,labels="D",cex=5,font=2,col=Green)
points(x1,y,col=Gray,pch=16,cex=3.5)
lines(rep(min(x1),2)-.02 * diff(range(x1)), range(y)*1.042,lty=1,lwd=6,col='grey75')
lines(range(x1)*1.042, rep(min(y),2)-.02 * diff(range(y)),lty=1,lwd=6,col='grey75')
mtext(c("Days","Sales"), side=c(1,2),las=0,font=2,line=c(3,.55),cex=4)
lines(x1, predict(lm(y~x1)), lwd=11, col=Pink)
lines(x1, predict(lm(y~x1+x2)), lwd=11, col=Green)

xpos = usr[1] + c(.5,.55) * diff(usr[1:2])
ypos = usr[3] + c(.06,.11) * diff(usr[3:4])

lines(xpos,rep(ypos[1],2),lwd=10,col=Green,lend=2)
lines(xpos,rep(ypos[2],2),lwd=10,col=Pink,lend=2)
text(rep(xpos[2]+.02*diff(usr[1:2])),rev(ypos),labels =c(expression(paste(r," ~ ",days)),expression(paste(r," ~ ",days+weekend))),adj=0,cex=2.5)

set.seed(101)
x1 = sort(rnorm(n,0,1.5) + runif(n,-3,3))
x2 = rep(c(rep(-1,16), rep(1,80),rep(-1,16)),length(lin2_x1))[(1:length(lin2_x1))+60]
y = 2*x2 + .5*x1 + rnorm(n,0,2)

x = scale(x)
y = scale(y)
y = y - predict(lm(y ~ x1 + x2))

plot.new();plot.window(range(x1),range(y))
usr = par()$usr
xpos = usr[2] - .9 * diff(usr[1:2])
ypos = usr[3] + .95 * diff(usr[3:4])
text(xpos,ypos,1,labels="D",cex=5,font=2,col=Green)
points(x1,y,col=Gray,pch=16,cex=3.5)
lines(rep(min(x1),2)-.02 * diff(range(x1)), range(y)*1.042,lty=1,lwd=6,col='grey75')
lines(range(x1)*1.042, rep(min(y),2)-.02 * diff(range(y)),lty=1,lwd=6,col='grey75')
mtext(c(expression(bold(paste(beta[1],Days," + ",beta[2],Weekend))),expression(bold("Residuals"))), 
      side=c(1,2),las=0,font=2,line=c(3.8,.55),cex=4)
lines(x1, predict(lm(y~x1)), lwd=11, col=Pink)


dev.off()


# ROBUST REGRESSION ------------------------------------------------------------


png('_sessions/RobustStats/image/robust_reg.png',width=750,height=1400)


par(mfrow=c(2,1),mar=c(4,4,1,1))

set.seed(100)

x = scale(rnorm(300,0,1))
y = scale(x + rnorm(300,0,1))

x_extra = c(runif(10,3,4),runif(10,-4,-3))
y_extra = c(runif(10,-4,-3), runif(10,3,4))

x_c = c(x, x_extra)
y_c = c(y, y_extra)

plot.new();plot.window(xlim=range(x_c), ylim=range(y_c))

points(x, y, pch = 16, col = colTrans(Gray,.4),cex=3.5)

lines(x, predict(rq(y~x)),col=Pink,lwd=15)
lines(x, rfit(y~x)$fitted,col=Green,lwd=15)
lines(x, predict(lm(y~x)),col=Gray,lwd=15)

lines(rep(min(x_c),2)-.02 * diff(range(x_c)), range(y_c)*1.042,lty=1,lwd=6,col='grey75')
lines(range(x_c)*1.042, rep(min(y_c),2)-.02 * diff(range(y_c)),lty=1,lwd=6,col='grey75')

mtext(c("x","y"), side=c(1,2),las=1,font=2,line=c(2.2,.55),cex=4)

usr=par()$usr
xpos = usr[1] + c(.63,.68) * diff(usr[1:2])
ypos = usr[3] + c(.08,.13,.18) * diff(usr[3:4])

lines(xpos,rep(ypos[1],2),lwd=12,col=Green,lend=2)
lines(xpos,rep(ypos[2],2),lwd=12,col=Pink,lend=2)
lines(xpos,rep(ypos[3],2),lwd=12,col=Gray,lend=2)
text(rep(xpos[2]+.02*diff(usr[1:2])),rev(ypos),labels =c('OLS','Quantile','Rank-based'),adj=0,cex=2.5)


plot.new();plot.window(xlim=range(x_c), ylim=range(y_c))

points(x, y, pch = 16, col = colTrans(Gray,.4),cex=3.5)
points(x_extra, y_extra, pch = 16, col = colTrans(yellow,.8),cex=3.5)

lines(x_c, predict(lm(y_c~x_c)),col=Gray,lwd=15)
lines(x_c, predict(rq(y_c~x_c)),col=Pink,lwd=15)
lines(x_c, rfit(y_c~x_c)$fitted,col=Green,lwd=15)

lines(rep(min(x_c),2)-.02 * diff(range(x_c)), range(y_c)*1.042,lty=1,lwd=6,col='grey75')
lines(range(x_c)*1.042, rep(min(y_c),2)-.02 * diff(range(y_c)),lty=1,lwd=6,col='grey75')

mtext(c("x","y"), side=c(1,2),las=1,font=2,line=c(2.2,.55),cex=4)

dev.off()


# WILCOX ------------------------------------------------------------

gr1 = c(18, 24, 29, 12, 11, 31)
gr2 = c(27, 16, 23, 8, 15, 21)

gr1_r = rank(c(gr1, gr2))[1:6]
gr2_r = rank(c(gr1, gr2))[7:12]

sum(gr1_r) -  min(gr1)
sum(gr2_r) - min(gr2)

wilcox.test(gr1,gr2)

# BOOT ------------------------------------------------------------

d = c(18,63,25,53,34,64,24,12,23)
hist(d)

lapply(1:3,function(x) sample(d, replace=T))

png('_sessions/RobustStats/image/bootstrap_distr.png',width=1500,height=500,bg='transparent')

par(mar=c(6,0,0,0))
plot.new();plot.window(xlim=c(0,80),ylim=c(0,.062))

lines(density(d,bw=4,from=0,to=80),col=Gray,lwd=16)

set.seed(108)

b5 = sapply(1:5, function(x) mean(sample(d, replace = T)))
b5000 = sapply(1:5000, function(x) mean(sample(d, replace = T)))
lines(density(b5),col=memnet::cmix(Gray,Green,.5),lwd=16)
lines(density(b5000),col=Green,lwd=16)

axis(1,labels = F,lwd=8)
mtext(expression(bold(paste(x,", ",italic(f)[B]))),side=1,line=5,cex=5.5)


xpos = c(49,52)
ypos = rev(c(.032,.039,.046))
lines(xpos, rep(ypos[1],2),lwd=16,col=Gray,lend=2)
lines(xpos, rep(ypos[2],2),lwd=16,col=memnet::cmix(Gray,Green,.5),lend=2)
lines(xpos, rep(ypos[3],2),lwd=16,col=Green,lend=2)
text(rep(xpos[2]+1),ypos-c(0,.0002,.0002),
     labels = c('x',expression(italic(f)[paste(B,",",5)]),expression(italic(f)[paste(B,",",500)])),
     adj=0,cex=3.3)

dev.off()

