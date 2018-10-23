#Insurance Case
library(evir)
##evir: a package for ExtremeValue Analysis
data(danish)
hist(danish,breaks=200,xlim=c(0,20))

sum(danish>20)/length(danish)
sum(danish[danish>20])/sum(danish)
emplot(danish)
emplot(danish,alog='xy')
qplot(danish,trim=100)

meplot(danish,omit=4)
gpdfit<- gpd(danish,threshold = 10)
plot(gpdfit)

tp <- tailplot(gpdfit)
gpd.q(tp,pp=0.999,ci.p=0.95)
quantile(danish,probs=0.999,type=1)

#Expected ShortFall
tp <- tailplot(gpdfit)
gpd.q(tp,pp=0.99)
gpd.sfall(tp,0.99)
