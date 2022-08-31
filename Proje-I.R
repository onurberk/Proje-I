library(readr)
a <- read_csv("C:/Users/Onur/OneDrive/Masaüstü/xu100_week.csv")
summary(a)

xu100.ts <- ts(a$xu100)
cimsa.ts <- ts(a$cimsa)
usd.ts <- ts(a$usd_c)
for.ts <- ts(a$s_foreign)

#zaman serisi grafikleri
par(mfrow=c(2,2))
plot.ts(xu100.ts)
plot.ts(cimsa.ts)
plot.ts(usd.ts)
plot.ts(for.ts)

#getiri hesaplama
rbist <- 100*diff(xu100.ts, lag=1, diff=1)/lag(xu100.ts,-1)
rcimsa <- 100*diff(cimsa.ts, lag=1, diff=1)/lag(cimsa.ts,-1)
rusd <- 100*diff(usd.ts, lag=1, diff=1)/lag(usd.ts,-1)

par(mfrow=c(2,2))
plot.ts(rbist)
plot.ts(rcimsa)
plot.ts(rusd)

#modeli tahmin et
lreg <- lm(rcimsa~rbist+rusd)
summary(lreg)

#artiklari tahmin et
res <- ts(residuals(lreg))
#res <- ts(lreg$residuals)
res1 <- lag(res,-1)

#korelasyon
cor(res, res1)

dres <- data.frame(cbind(res, res1))
dres <- dres[-c(1,584),]
cor(dres$res, dres$res1)

#DW test
library(car)
durbinWatsonTest(lreg, max.lag=1)

#BG test
library(lmtest)
bgtest(rcimsa~rbist+rusd, order=4, type=c("Chisq", "F"))