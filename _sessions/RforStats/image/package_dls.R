
l = length


stats = dlstats::cran_stats(c('stats','boot','quantreg','Rfit','moments',
                              'lme4','tidyverse','rstanarm','BayesFactor',
                              'pwr','sjstats','broom'))

counts = tapply(stats$downloads,stats$package,sum)
year = tapply(lubridate::year(stats$start),stats$package,min)
res = cbind(as.data.frame(counts),year)


res = res[!is.na(res[[1]]),,drop=F]
res = res[order(res[[1]],decreasing = F),,drop=F]

cols = c('#606060','#EA4B68')

png('_sessions/RforStats/image/stats_package_dl.png',width=1560,height=900)
par(mar=c(6,12,1,1))
plot.new();plot.window(xlim=c(0,7000000),ylim=c(.5,nrow(res) + .5))
sapply(seq(0,7000000,500000),function(x) lines(c(x,x),c(.5,nrow(res) + .5),col='grey85'))
pos = 1:nrow(res)
rect(rep(0,l(pos)),pos - .45,res[[1]], pos + .45,
     col=ifelse(rownames(res)!='caret',cols[1],cols[2]),border=NA)
mtext(rownames(res),at=pos,side=2,las=1,adj=1,line=-2.5,cex=2.5,font=1)
mtext(seq(0,7,.5),side=1,at=seq(0,7000000,500000),cex=2.5)
mtext('Downloads from CRAN since 2012 in mio.',side=1,cex=2.5,line=3)
text(res[[1]] + 50000,pos,labels = ifelse(res$year == 2012,"",paste0('since ',res$year)),adj = 0,cex=1.8,font=1)
dev.off()