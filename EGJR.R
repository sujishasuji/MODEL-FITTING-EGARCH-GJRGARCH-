library(readxl)
A <- read_excel("~/project/FINAL  15 DATA SETS.xlsx", 
                sheet = "Sheet2")

library(tseries)
library(rugarch)
library(fGarch)
library(aTSA)
library(car)
library(FinTS)

########TIMEPLOT#####
x1=A$MTNL
xt=ts(x1,frequency=52,start=c(2016,1))

###########LOG RETURN#####
x1=as.vector(x1)
n=length(x1)
logreturn={}
for(i in 1:(n-1))
  logreturn[i]=log(x1[i+1])-log(x1[i])

#auto.arima(logreturn)

#######ARCH MODEL######

library(xts)
v=xts(logreturn,order.by =A$Date[1:nrow(A)-1] )
garch10.spec = ugarchspec(variance.model = list(garchOrder=c(1,0)),  
                          mean.model = list(armaOrder=c(0,0)))
MSFT.garch10.fit = ugarchfit(spec=garch10.spec, data=v,out.sample=20)
MSFT.garch10.fit
forc1= ugarchforecast(MSFT.garch10.fit, n.ahead=20)
m1=fpm(forc1)
a1=residuals(MSFT.garch10.fit,standardize=TRUE)
plot(residuals(MSFT.garch10.fit),type="l")
summary(MSFT.garch10.fit)
arch1=garchFit(~garch(1,0),logreturn);arch1
s1=summary(arch1);s1 
re1=residuals(arch1,standardize=T);re1
lb1=Box.test(re1,lag=round(2*sqrt(n)),type="Ljung-Box");lb1
sw1=shapiro.test(re1);sw1
jb1=jarque.bera.test(re1);jb1
info1=arch1@fit$ics[1];info1

Model=c("arch1");model
AIC=c(info1);AIC
s1$ics



#######garch######

library(xts)
v=xts(logreturn,order.by =A$Date[1:nrow(A)-1] )
garch11.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)),  
                          mean.model = list(armaOrder=c(0,0)))
MSFT.garch11.fit = ugarchfit(spec=garch11.spec, data=v,out.sample=20)
MSFT.garch11.fit
forc2= ugarchforecast(MSFT.garch11.fit, n.ahead=20)
m2=fpm(forc2)
a2=residuals(MSFT.garch11.fit,standardize=TRUE)
plot(residuals(MSFT.garch11.fit),type="l")

garch11=garchFit(~garch(1,1),logreturn);garch11
s11=summary(garch11);s11
re11=residuals(garch11,standardize=T);re11
lb11=Box.test(re11,lag=round(2*sqrt(n)),type="Ljung-Box");lb11
sw11=shapiro.test(re11);sw11
jb11=jarque.bera.test(re11);jb11
info11=garch11@fit$ics[1];info11


#######Nelson's egarch model#######
library(xts)
v=xts(logreturn,order.by =A$Date[1:nrow(A)-1] )
egarch11.spec = ugarchspec(variance.model=list(model="eGARCH",
                                               garchOrder=c(1,1)),
                           mean.model=list(armaOrder=c(0,0)))
MSFT.egarch11.fit = ugarchfit(egarch11.spec,data=v,out.sample=20)
MSFT.egarch11.fit

forc3= ugarchforecast(MSFT.egarch11.fit, n.ahead=20)
m3=fpm(forc3)
plot(residuals(MSFT.egarch11.fit),type="l")
a3=residuals(MSFT.egarch11.fit,standardize=TRUE)
is.vector(a3)
b1=as.vector(a3);b1
sw12=shapiro.test(b1);sw12
jb12=jarque.bera.test(b1);jb12
lb12=Box.test(a3,lag=round(2*sqrt(n)),type="Ljung-Box");lb12
info1=arch1@fit$ics[1];info1
info1=arch1@fit;info1
# GJR garch model

library(xts)
v=xts(logreturn,order.by =A$Date[1:nrow(A)-1] )
gjrgarch11.spec = ugarchspec(variance.model=list(model="gjrGARCH",
                                                 garchOrder=c(1,1)),
                             mean.model=list(armaOrder=c(0,0)))
MSFT.gjrgarch11.fit = ugarchfit(gjrgarch11.spec,v,out.sample=20)
MSFT.gjrgarch11.fit
a4=residuals(MSFT.gjrgarch11.fit,standardize=TRUE)
forc4= ugarchforecast(MSFT.gjrgarch11.fit, n.ahead=20)
m4=fpm(forc4)
is.vector(a4)
d1=as.vector(a4);d1
sw13=shapiro.test(d1);sw13
jb13=jarque.bera.test(d1);jb13
lb13=Box.test(a4,lag=round(2*sqrt(n)),type="Ljung-Box");lb13


#####TABLE OF TESTS####
Model=c("ARCH(1)","GARCH(1,1)","EGARCH(1,1)","GJRGARCH(1,1)");Model
AIC=c(info1,info11);AIC
LB=c(lb1$p.value,lb11$p.value,lb12$p.value,lb13$p.value);LB
SW=c(sw1$p.value,sw11$p.value,sw12$p.value,sw13$p.value);SW
JB=c(jb1$p.value,jb11$p.value,jb12$p.value,jb13$p.value);JB
Tabel=data.frame(Model,LB,SW,JB);Tabel
cat("\n The different combinations are given by\n")
print(Tabel)

####forcasted measure####
dd=data.frame(Model,c(m1[1],m2[1]),c(m2[1],m2[2]),c(m3[1],m3[2]),c(m4[1],m4[2]))
colnames(dd)=c("model",rownames(m1)[1:2])
dd

