---
title: "301 Final Project"
author: "Arjun Koshal"
date: "12/05/2022"
---
    
library(quantmod)
library(randomForest)
library(PerformanceAnalytics)
library(tree)
  
set.seed(123)

## Data Collection

# Get data for 20 ETFs and create table of daily log returns

# Obtain data for all 20 ETFs
tkrs = c("IVV", "IJH", "IJR", "SUSA", "VXUS", "SCHD", "VIG", "DNL", "PDBC", 
         "RYF", "RYH", "PBW", "XLK", "VGK", "FBND", "BSCQ", "TOTL", "ICSH", 
         "BKLN", "VTEB")

adj.list = lapply(tkrs, function(x) get (
  getSymbols(x, from = "2017-01-01", to = "2022-07-01")
))
daily.adj = lapply(adj.list, function(x) x[,6])
daily.adj = Reduce(merge, daily.adj)
daily.ret = na.omit(diff(log(daily.adj), lag=1))

tickers = c("IVV", "IJH", "IJR", "SUSA", "VXUS", "SCHD", "VIG", "DNL", "PDBC", 
            "RYF", "RYH", "PBW", "XLK", "VGK", "FBND", "BSCQ", "TOTL", "ICSH", 
            "BKLN", "VTEB")
getSymbols(tickers, from = "2017-01-01", to = "2022-07-01")

LogRet <- data.frame(as.numeric(diff(log(IVV$IVV.Adjusted)))[-1],
                     as.numeric(diff(log(IJH$IJH.Adjusted)))[-1],
                     as.numeric(diff(log(IJR$IJR.Adjusted)))[-1],
                     as.numeric(diff(log(SUSA$SUSA.Adjusted)))[-1],
                     as.numeric(diff(log(VXUS$VXUS.Adjusted)))[-1],
                     as.numeric(diff(log(SCHD$SCHD.Adjusted)))[-1],
                     as.numeric(diff(log(VIG$VIG.Adjusted)))[-1],
                     as.numeric(diff(log(DNL$DNL.Adjusted)))[-1],
                     as.numeric(diff(log(PDBC$PDBC.Adjusted)))[-1],
                     as.numeric(diff(log(RYF$RYF.Adjusted)))[-1],
                     as.numeric(diff(log(RYH$RYH.Adjusted)))[-1],
                     as.numeric(diff(log(PBW$PBW.Adjusted)))[-1],
                     as.numeric(diff(log(XLK$XLK.Adjusted)))[-1],
                     as.numeric(diff(log(VGK$VGK.Adjusted)))[-1],
                     as.numeric(diff(log(FBND$FBND.Adjusted)))[-1],
                     as.numeric(diff(log(BSCQ$BSCQ.Adjusted)))[-1],
                     as.numeric(diff(log(TOTL$TOTL.Adjusted)))[-1],
                     as.numeric(diff(log(ICSH$ICSH.Adjusted)))[-1],
                     as.numeric(diff(log(BKLN$BKLN.Adjusted)))[-1],
                     as.numeric(diff(log(VTEB$VTEB.Adjusted)))[-1])

colnames(LogRet) <- c("IVV", "IJH", "IJR", "SUSA", "VXUS", "SCHD", "VIG", 
                      "DNL", "PDBC", "RYF", "RYH", "PBW", "XLK", "VGK", "FBND", 
                      "BSCQ", "TOTL", "ICSH", "BKLN", "VTEB")
head(LogRet)

# Create a Statistics Summary for Mean Returns, Volatility, and Sharpe Ratio

# Create function that calculates: Q1, Median, Mean, and Q3 of given input
sum.func = function(x) {
  q1 = as.numeric(summary(x))[2]
  med = as.numeric(summary(x))[3]
  avg = as.numeric(summary(x))[4]
  q3 = as.numeric(summary(x))[5]
  return(rbind(avg,q1,med,q3))
}

# Create function that calls summary function on annual returns, sd, and sr data
sum.report = function(x) {
  Mean = sapply(x, mean)
  Vol = sapply(x, sd)
  SR = (sqrt(252) * Mean)/Vol
  return(cbind(252*sum.func(Mean), sqrt(252)*sum.func(Vol), sum.func(SR)))
}
perform.table = sum.report(daily.ret)
rownames(perform.table) = c("Mean", "Q1", "Median", "Q3")
colnames(perform.table) = c("Mean Returns", "Volatility", "Sharpe Ratio")
perform.table

# Plot the Mean Returns against Volatility

# Get mean and volatility data and create scatterplot
mean.table = sapply(daily.ret, mean)*252
vol.table = sapply(daily.ret, sd)*sqrt(252)
plot(vol.table, mean.table, main = "ETF Mean Returns vs Volatility",
     xlab = "Volatility", ylab = "Mean Returns")

# Get SPY data and merge data with adjusted prices

spy = lapply("SPY", function(x) get (
  getSymbols(x, from="2017-01-01", to = "2022-07-01")
))
spy.adj = lapply(spy, function(x) x[,6])
spy.adj = Reduce(merge, spy.adj)

adj.final = merge(daily.adj, spy.adj)

ret.data = na.omit(diff(log(adj.final), lag=1))

# Calculate 4 metrics (Jensen's alpha, Market B, TR, IR)

Alpha = c()
Beta = c()
Treynor = c()
IR = c()

etf.table = table.CAPM(ret.data[, 1:20], ret.data[,21])

for(i in 1:20) {
  metric.table = etf.table[i]
  Alpha = c(Alpha, metric.table["Annualized Alpha",])
  Beta = c(Beta, metric.table["Beta",])
  Treynor = c(Treynor, metric.table["Treynor Ratio",])
  IR = c(IR, metric.table["Information Ratio",])
}

# Use sum.func to get summary outputs for all 4 metrics and output as table

alpha.table = sum.func(Alpha)
beta.table = sum.func(Beta)
treynor.table = sum.func(Treynor)
IR.table = sum.func(IR)

metric.perform = cbind(alpha.table, beta.table, treynor.table, IR.table)
rownames(metric.perform) = c("Mean", "Q1", "Median","Q3")
colnames(metric.perform) = c("Annual Jensen Alpha", "Market Beta", "TR", "IR")
metric.perform

## Linear Regressions

rIVV = as.numeric(dailyReturn(IVV$IVV.Adjusted,type="log"))
rIJH = as.numeric(dailyReturn(IJH$IJH.Adjusted,type="log"))
rIJR = as.numeric(dailyReturn(IVV$IVV.Adjusted,type="log"))
rSUSA = as.numeric(dailyReturn(SUSA$SUSA.Adjusted,type="log"))
rVXUS = as.numeric(dailyReturn(VXUS$VXUS.Adjusted,type="log"))
rSCHD = as.numeric(dailyReturn(SCHD$SCHD.Adjusted,type="log"))
rVIG = as.numeric(dailyReturn(VIG$VIG.Adjusted,type="log"))
rDNL = as.numeric(dailyReturn(DNL$DNL.Adjusted,type="log"))
rPDBC = as.numeric(dailyReturn(PDBC$PDBC.Adjusted,type="log"))
rRYF = as.numeric(dailyReturn(RYF$RYF.Adjusted,type="log"))
rRYH = as.numeric(dailyReturn(RYH$RYH.Adjusted,type="log"))
rPBW = as.numeric(dailyReturn(PBW$PBW.Adjusted,type="log"))
rXLK = as.numeric(dailyReturn(XLK$XLK.Adjusted,type="log"))
rVGK = as.numeric(dailyReturn(VGK$VGK.Adjusted,type="log"))
rFBND = as.numeric(dailyReturn(FBND$FBND.Adjusted,type="log"))
rBSCQ = as.numeric(dailyReturn(BSCQ$BSCQ.Adjusted,type="log"))
rTOTL = as.numeric(dailyReturn(TOTL$TOTL.Adjusted,type="log"))
rICSH = as.numeric(dailyReturn(ICSH$ICSH.Adjusted,type="log"))
rBKLN = as.numeric(dailyReturn(BKLN$BKLN.Adjusted,type="log"))
rVTEB = as.numeric(dailyReturn(VTEB$VTEB.Adjusted,type="log"))

# By 2 lags
IVV_lag1 <- as.numeric(lag(rIVV), k=1)[-(1:2)]
IVV_lag2 <- as.numeric(lag(rIVV), k=2)[-(1:2)]

#IVV_lag1 <- as.numeric(lag(diff(log(IVV$IVV.Adjusted))), k=1)[-(1:2)]
#IVV_lag2 <- as.numeric(lag(diff(log(IVV$IVV.Adjusted))), k=2)[-(1:2)]

IJH_lag1 <- as.numeric(lag(rIJH), k=1)[-(1:2)]
IJH_lag2 <- as.numeric(lag(rIJH), k=2)[-(1:2)]

#IJH_lag1 <- as.numeric(lag(diff(log(IJH$IJH.Adjusted))), k=1)[-(1:2)]
#IJH_lag2 <- as.numeric(lag(diff(log(IJH$IJH.Adjusted))), k=2)[-(1:2)]

IJR_lag1 <- as.numeric(lag(rIJR), k=1)[-(1:2)]
IJR_lag2 <- as.numeric(lag(rIJR), k=2)[-(1:2)]

#IJR_lag1 <- as.numeric(lag(diff(log(IJR$IJR.Adjusted))), k=1)[-(1:2)]
#IJR_lag2 <- as.numeric(lag(diff(log(IJR$IJR.Adjusted))), k=2)[-(1:2)]

SUSA_lag1 <- as.numeric(lag(rSUSA), k=1)[-(1:2)]
SUSA_lag2 <- as.numeric(lag(rSUSA), k=2)[-(1:2)]

#SUSA_lag1 <- as.numeric(lag(diff(log(SUSA$SUSA.Adjusted))), k=1)[-(1:2)]
#SUSA_lag2 <- as.numeric(lag(diff(log(SUSA$SUSA.Adjusted))), k=2)[-(1:2)]

VXUS_lag1 <- as.numeric(lag(rVXUS), k=1)[-(1:2)]
VXUS_lag2 <- as.numeric(lag(rVXUS), k=2)[-(1:2)]

#VXUS_lag1 <- as.numeric(lag(diff(log(VXUS$VXUS.Adjusted))), k=1)[-(1:2)]
#VXUS_lag2 <- as.numeric(lag(diff(log(VXUS$VXUS.Adjusted))), k=2)[-(1:2)]

SCHD_lag1 <- as.numeric(lag(rSCHD), k=1)[-(1:2)]
SCHD_lag2 <- as.numeric(lag(rSCHD), k=2)[-(1:2)]

#SCHD_lag1 <- as.numeric(lag(diff(log(SCHD$SCHD.Adjusted))), k=1)[-(1:2)]
#SCHD_lag2 <- as.numeric(lag(diff(log(SCHD$SCHD.Adjusted))), k=2)[-(1:2)]

VIG_lag1 <- as.numeric(lag(rVIG), k=1)[-(1:2)]
VIG_lag2 <- as.numeric(lag(rVIG), k=2)[-(1:2)]

#VIG_lag1 <- as.numeric(lag(diff(log(VIG$VIG.Adjusted))), k=1)[-(1:2)]
#VIG_lag2 <- as.numeric(lag(diff(log(VIG$VIG.Adjusted))), k=2)[-(1:2)]

DNL_lag1 <- as.numeric(lag(rDNL), k=1)[-(1:2)]
DNL_lag2 <- as.numeric(lag(rDNL), k=2)[-(1:2)]

#DNL_lag1 <- as.numeric(lag(diff(log(DNL$DNL.Adjusted))), k=1)[-(1:2)]
#DNL_lag2 <- as.numeric(lag(diff(log(DNL$DNL.Adjusted))), k=2)[-(1:2)]

PDBC_lag1 <- as.numeric(lag(rPDBC), k=1)[-(1:2)]
PDBC_lag2 <- as.numeric(lag(rPDBC), k=2)[-(1:2)]

#PDBC_lag1 <- as.numeric(lag(diff(log(PDBC$PDBC.Adjusted))), k=1)[-(1:2)]
#PDBC_lag2 <- as.numeric(lag(diff(log(PDBC$PDBC.Adjusted))), k=2)[-(1:2)]

RYF_lag1 <- as.numeric(lag(rRYF), k=1)[-(1:2)]
RYF_lag2 <- as.numeric(lag(rRYF), k=2)[-(1:2)]

#RYF_lag1 <- as.numeric(lag(diff(log(RYF$RYF.Adjusted))), k=1)[-(1:2)]
#RYF_lag2 <- as.numeric(lag(diff(log(RYF$RYF.Adjusted))), k=2)[-(1:2)]

RYH_lag1 <- as.numeric(lag(rRYH), k=1)[-(1:2)]
RYH_lag2 <- as.numeric(lag(rRYH), k=2)[-(1:2)]

#RYH_lag1 <- as.numeric(lag(diff(log(RYH$RYH.Adjusted))), k=1)[-(1:2)]
#RYH_lag2 <- as.numeric(lag(diff(log(RYH$RYH.Adjusted))), k=2)[-(1:2)]

PBW_lag1 <- as.numeric(lag(rPBW), k=1)[-(1:2)]
PBW_lag2 <- as.numeric(lag(rPBW), k=2)[-(1:2)]

#PBW_lag1 <- as.numeric(lag(diff(log(PBW$PBW.Adjusted))), k=1)[-(1:2)]
#PBW_lag2 <- as.numeric(lag(diff(log(PBW$PBW.Adjusted))), k=2)[-(1:2)]

XLK_lag1 <- as.numeric(lag(rXLK), k=1)[-(1:2)]
XLK_lag2 <- as.numeric(lag(rXLK), k=2)[-(1:2)]

#XLK_lag1 <- as.numeric(lag(diff(log(XLK$XLK.Adjusted))), k=1)[-(1:2)]
#XLK_lag2 <- as.numeric(lag(diff(log(XLK$XLK.Adjusted))), k=2)[-(1:2)]

VGK_lag1 <- as.numeric(lag(rVGK), k=1)[-(1:2)]
VGK_lag2 <- as.numeric(lag(rVGK), k=2)[-(1:2)]

#VGK_lag1 <- as.numeric(lag(diff(log(VGK$VGK.Adjusted))), k=1)[-(1:2)]
#VGK_lag2 <- as.numeric(lag(diff(log(VGK$VGK.Adjusted))), k=2)[-(1:2)]

FBND_lag1 <- as.numeric(lag(rFBND), k=1)[-(1:2)]
FBND_lag2 <- as.numeric(lag(rFBND), k=2)[-(1:2)]

#FBND_lag1 <- as.numeric(lag(diff(log(FBND$FBND.Adjusted))), k=1)[-(1:2)]
#FBND_lag2 <- as.numeric(lag(diff(log(FBND$FBND.Adjusted))), k=2)[-(1:2)]

BSCQ_lag1 <- as.numeric(lag(rBSCQ), k=1)[-(1:2)]
BSCQ_lag2 <- as.numeric(lag(rBSCQ), k=2)[-(1:2)]

#BSCQ_lag1 <- as.numeric(lag(diff(log(BSCQ$BSCQ.Adjusted))), k=1)[-(1:2)]
#BSCQ_lag2 <- as.numeric(lag(diff(log(BSCQ$BSCQ.Adjusted))), k=2)[-(1:2)]

TOTL_lag1 <- as.numeric(lag(rTOTL), k=1)[-(1:2)]
TOTL_lag2 <- as.numeric(lag(rTOTL), k=2)[-(1:2)]

#TOTL_lag1 <- as.numeric(lag(diff(log(TOTL$TOTL.Adjusted))), k=1)[-(1:2)]
#TOTL_lag2 <- as.numeric(lag(diff(log(TOTL$TOTL.Adjusted))), k=2)[-(1:2)]

ICSH_lag1 <- as.numeric(lag(rICSH), k=1)[-(1:2)]
ICSH_lag2 <- as.numeric(lag(rICSH), k=2)[-(1:2)]

#ICSH_lag1 <- as.numeric(lag(diff(log(ICSH$ICSH.Adjusted))), k=1)[-(1:2)]
#ICSH_lag2 <- as.numeric(lag(diff(log(ICSH$ICSH.Adjusted))), k=2)[-(1:2)]

BKLN_lag1 <- as.numeric(lag(rBKLN), k=1)[-(1:2)]
BKLN_lag2 <- as.numeric(lag(rBKLN), k=2)[-(1:2)]

#BKLN_lag1 <- as.numeric(lag(diff(log(BKLN$BKLN.Adjusted))), k=1)[-(1:2)]
#BKLN_lag2 <- as.numeric(lag(diff(log(BKLN$BKLN.Adjusted))), k=2)[-(1:2)]

VTEB_lag1 <- as.numeric(lag(rVTEB), k=1)[-(1:2)]
VTEB_lag2 <- as.numeric(lag(rVTEB), k=2)[-(1:2)]

#VTEB_lag1 <- as.numeric(lag(diff(log(VTEB$VTEB.Adjusted))), k=1)[-(1:2)]
#VTEB_lag2 <- as.numeric(lag(diff(log(VTEB$VTEB.Adjusted))), k=2)[-(1:2)]

# Computed log returns on lags

IVVlog <- as.numeric(diff(log(IVV$IVV.Adjusted)))[-(1:2)]
IJHlog <- as.numeric(diff(log(IJH$IJH.Adjusted)))[-(1:2)]
IJRlog <- as.numeric(diff(log(IJR$IJR.Adjusted)))[-(1:2)]
SUSAlog <- as.numeric(diff(log(SUSA$SUSA.Adjusted)))[-(1:2)]
VXUSlog <- as.numeric(diff(log(VXUS$VXUS.Adjusted)))[-(1:2)]
SCHDlog <- as.numeric(diff(log(SCHD$SCHD.Adjusted)))[-(1:2)]
VIGlog <- as.numeric(diff(log(VIG$VIG.Adjusted)))[-(1:2)]
DNLlog <- as.numeric(diff(log(DNL$DNL.Adjusted)))[-(1:2)]
PDBClog <- as.numeric(diff(log(PDBC$PDBC.Adjusted)))[-(1:2)]
RYFlog <- as.numeric(diff(log(RYF$RYF.Adjusted)))[-(1:2)]
RYHlog <- as.numeric(diff(log(RYH$RYH.Adjusted)))[-(1:2)]
PBWlog <- as.numeric(diff(log(PBW$PBW.Adjusted)))[-(1:2)]
XLKlog <- as.numeric(diff(log(XLK$XLK.Adjusted)))[-(1:2)]
VGKlog <- as.numeric(diff(log(VGK$VGK.Adjusted)))[-(1:2)]
FBNDlog <- as.numeric(diff(log(FBND$FBND.Adjusted)))[-(1:2)]
BSCQlog <- as.numeric(diff(log(BSCQ$BSCQ.Adjusted)))[-(1:2)]
TOTLlog <- as.numeric(diff(log(TOTL$TOTL.Adjusted)))[-(1:2)]
ICSHlog <- as.numeric(diff(log(ICSH$ICSH.Adjusted)))[-(1:2)]
BKLNlog <- as.numeric(diff(log(BKLN$BKLN.Adjusted)))[-(1:2)]
VTEBlog <- as.numeric(diff(log(VTEB$VTEB.Adjusted)))[-(1:2)]

# Create a dataframe with log, lag1, and lag2

df2 <- data.frame(IVVlog, IVV_lag1, IVV_lag2, IJHlog, IJH_lag1, IJH_lag2, 
                  IJRlog, IJR_lag1, IJR_lag2, SUSAlog, SUSA_lag1, SUSA_lag2, 
                  VXUSlog, VXUS_lag1, VXUS_lag2, SCHDlog, SCHD_lag1, SCHD_lag2, 
                  VIGlog, VIG_lag1, VIG_lag2, DNLlog, DNL_lag1, DNL_lag2, 
                  PDBClog, PDBC_lag1, PDBC_lag2, RYFlog, RYF_lag1, RYF_lag2, 
                  RYHlog, RYH_lag1, RYH_lag2, PBWlog, PBW_lag1, PBW_lag2, 
                  XLKlog, XLK_lag1, XLK_lag2, VGKlog, VGK_lag1, VGK_lag2, 
                  FBNDlog, FBND_lag1, FBND_lag2, BSCQlog, BSCQ_lag1, BSCQ_lag2, 
                  TOTLlog, TOTL_lag1, TOTL_lag2, ICSHlog, ICSH_lag1, ICSH_lag2, 
                  BKLNlog, BKLN_lag1, BKLN_lag2, VTEBlog, VTEB_lag1, VTEB_lag2)

head(df2)

# Split data into training and testing sets (80% training and 20% test)

train1 <- sample(length(IVVlog), length(IVVlog)*.8, replace = FALSE)
train2 <- sample(length(IJHlog), length(IJHlog)*.8, replace = FALSE)
train3 <- sample(length(IJRlog), length(IJRlog)*.8, replace = FALSE)
train4 <- sample(length(SUSAlog), length(SUSAlog)*.8, replace = FALSE)
train5 <- sample(length(VXUSlog), length(VXUSlog)*.8, replace = FALSE)
train6 <- sample(length(SCHDlog), length(SCHDlog)*.8, replace = FALSE)
train7 <- sample(length(VIGlog), length(VIGlog)*.8, replace = FALSE)
train8 <- sample(length(DNLlog), length(DNLlog)*.8, replace = FALSE)
train9 <- sample(length(PDBClog), length(PDBClog)*.8, replace = FALSE)
train10 <- sample(length(RYFlog), length(RYFlog)*.8, replace = FALSE)
train11 <- sample(length(RYHlog), length(RYHlog)*.8, replace = FALSE)
train12 <- sample(length(PBWlog), length(PBWlog)*.8, replace = FALSE)
train13 <- sample(length(XLKlog), length(XLKlog)*.8, replace = FALSE)
train14 <- sample(length(VGKlog), length(VGKlog)*.8, replace = FALSE)
train15 <- sample(length(FBNDlog), length(FBNDlog)*.8, replace = FALSE)
train16 <- sample(length(BSCQlog), length(BSCQlog)*.8, replace = FALSE)
train17 <- sample(length(TOTLlog), length(TOTLlog)*.8, replace = FALSE)
train18 <- sample(length(ICSHlog), length(ICSHlog)*.8, replace = FALSE)
train19 <- sample(length(BKLNlog), length(BKLNlog)*.8, replace = FALSE)
train20 <- sample(length(VTEBlog), length(VTEBlog)*.8, replace = FALSE)

# IVV Model
IVV_model <- glm(IVVlog~., data = df2, subset = train1)
IVV_model

# IVV Testing MSE
IVV_pred <- predict(IVV_model, df2[-train1,])
IVV_MSE <- mean((IVV_pred - df2$IVVlog[-train1])^2 )
IVV_MSE

# IVV Training MSE
IVV_pred1 <- predict(IVV_model, df2[train1,])
IVV_MSE1 <- mean((IVV_pred1 - df2$IVVlog[train1])^2 )
IVV_MSE1

# IJH Model
IJH_model <- glm(IJHlog~., data = df2, subset = train2)
IJH_model

# IJH Testing MSE
IJH_pred <- predict(IJH_model, df2[-train2,])
IJH_MSE <- mean((IJH_pred - df2$IJHlog[-train2])^2 )
IJH_MSE

# IJH Training MSE
IJH_pred1 <- predict(IJH_model, df2[train2,])
IJH_MSE1 <- mean((IJH_pred1 - df2$IJHlog[train2])^2 )
IJH_MSE1

# IJR Model
IJR_model <- glm(IJRlog~., data = df2, subset = train3)
IJR_model

# IJR Testing MSE
IJR_pred <- predict(IJR_model, df2[-train3,])
IJR_MSE <- mean((IJR_pred - df2$IJRlog[-train3])^2 )
IJR_MSE

# IJR Training MSE
IJR_pred1 <- predict(IJR_model, df2[train3,])
IJR_MSE1 <- mean((IJR_pred1 - df2$IJRlog[train3])^2 )
IJR_MSE1

# SUSA Model
SUSA_model <- glm(SUSAlog~., data = df2, subset = train4)
SUSA_model

# SUSA Testing MSE
SUSA_pred <- predict(SUSA_model, df2[-train4,])
SUSA_MSE <- mean((SUSA_pred - df2$SUSAlog[-train4])^2 )
SUSA_MSE

# SUSA Training MSE
SUSA_pred1 <- predict(SUSA_model, df2[train4,])
SUSA_MSE1 <- mean((SUSA_pred1 - df2$SUSAlog[train4])^2 )
SUSA_MSE1

# VXUS Model
VXUS_model <- glm(VXUSlog~., data = df2, subset = train5)
VXUS_model

# VXUS Testing MSE
VXUS_pred <- predict(VXUS_model, df2[-train5,])
VXUS_MSE <- mean((VXUS_pred - df2$VXUSlog[-train5])^2 )
VXUS_MSE

# VXUS Training MSE
VXUS_pred1 <- predict(VXUS_model, df2[train5,])
VXUS_MSE1 <- mean((VXUS_pred1 - df2$VXUSlog[train5])^2 )
VXUS_MSE1

# SCHD Model
SCHD_model <- glm(SCHDlog~., data = df2, subset = train6)
SCHD_model

# SCHD Testing MSE
SCHD_pred <- predict(SCHD_model, df2[-train6,])
SCHD_MSE <- mean((SCHD_pred - df2$SCHDlog[-train6])^2 )
SCHD_MSE

# SCHD Training MSE
SCHD_pred1 <- predict(SCHD_model, df2[train6,])
SCHD_MSE1 <- mean((SCHD_pred1 - df2$SCHDlog[train6])^2 )
SCHD_MSE1

# VIG Model
VIG_model <- glm(VIGlog~., data = df2, subset = train7)
VIG_model

# VIG Testing MSE
VIG_pred <- predict(VIG_model, df2[-train7,])
VIG_MSE <- mean((VIG_pred - df2$VIGlog[-train7])^2 )
VIG_MSE

# VIG Training MSE
VIG_pred1 <- predict(VIG_model, df2[train7,])
VIG_MSE1 <- mean((VIG_pred1 - df2$VIGlog[train7])^2 )
VIG_MSE1

# DNL Model
DNL_model <- glm(DNLlog~., data = df2, subset = train8)
DNL_model

# DNL Testing MSE
DNL_pred <- predict(DNL_model, df2[-train8,])
DNL_MSE <- mean((DNL_pred - df2$DNLlog[-train8])^2 )
DNL_MSE

# DNL Training MSE
DNL_pred1 <- predict(DNL_model, df2[train8,])
DNL_MSE1 <- mean((DNL_pred1 - df2$DNLlog[train8])^2 )
DNL_MSE1

# PDBC Model
PDBC_model <- glm(PDBClog~., data = df2, subset = train9)
PDBC_model

# PDBC Testing MSE
PDBC_pred <- predict(PDBC_model, df2[-train9,])
PDBC_MSE <- mean((PDBC_pred - df2$PDBClog[-train9])^2 )
PDBC_MSE

# PDBC Training MSE
PDBC_pred1 <- predict(PDBC_model, df2[train9,])
PDBC_MSE1 <- mean((PDBC_pred1 - df2$PDBClog[train9])^2 )
PDBC_MSE1

# RYF Model
RYF_model <- glm(RYFlog~., data = df2, subset = train10)
RYF_model

# RYF Testing MSE
RYF_pred <- predict(RYF_model, df2[-train10,])
RYF_MSE <- mean((RYF_pred - df2$RYFlog[-train10])^2 )
RYF_MSE

# RYF Training MSE
RYF_pred1 <- predict(RYF_model, df2[train10,])
RYF_MSE1 <- mean((RYF_pred1 - df2$RYFlog[train10])^2 )
RYF_MSE1

# RYH Model
RYH_model <- glm(RYHlog~., data = df2, subset = train11)
RYH_model

# RYH Testing MSE
RYH_pred <- predict(RYH_model, df2[-train11,])
RYH_MSE <- mean((RYH_pred - df2$RYHlog[-train11])^2 )
RYH_MSE

# RYH Training MSE
RYH_pred1 <- predict(RYH_model, df2[train11,])
RYH_MSE1 <- mean((RYH_pred1 - df2$RYHlog[train11])^2 )
RYH_MSE1

# PBW Model
PBW_model <- glm(PBWlog~., data = df2, subset = train12)
PBW_model

# PBW Testing MSE
PBW_pred <- predict(PBW_model, df2[-train12,])
PBW_MSE <- mean((PBW_pred - df2$PBWlog[-train12])^2 )
PBW_MSE

# PBW Training MSE
PBW_pred1 <- predict(PBW_model, df2[train12,])
PBW_MSE1 <- mean((PBW_pred1 - df2$PBWlog[train12])^2 )
PBW_MSE1

# XLK Model
XLK_model <- glm(XLKlog~., data = df2, subset = train13)
XLK_model

# XLK Testing MSE
XLK_pred <- predict(XLK_model, df2[-train13,])
XLK_MSE <- mean((XLK_pred - df2$XLKlog[-train13])^2 )
XLK_MSE

# XLK Training MSE
XLK_pred1 <- predict(XLK_model, df2[train13,])
XLK_MSE1 <- mean((XLK_pred1 - df2$XLKlog[train13])^2 )
XLK_MSE1

# VGK Model
VGK_model <- glm(VGKlog~., data = df2, subset = train14)
VGK_model

# VGK testing MSE
VGK_pred <- predict(VGK_model, df2[-train14,])
VGK_MSE <- mean((VGK_pred - df2$VGKlog[-train14])^2 )
VGK_MSE

# VGK Training MSE
VGK_pred1 <- predict(VGK_model, df2[train14,])
VGK_MSE1 <- mean((VGK_pred1 - df2$VGKlog[train14])^2 )
VGK_MSE1

# FBND Model
FBND_model <- glm(FBNDlog~., data = df2, subset = train15)
FBND_model

# FBND Testing MSE
FBND_pred <- predict(FBND_model, df2[-train15,])
FBND_MSE <- mean((FBND_pred - df2$FBNDlog[-train15])^2 )
FBND_MSE

# FBND Training MSE
FBND_pred1 <- predict(FBND_model, df2[train15,])
FBND_MSE1 <- mean((FBND_pred1 - df2$FBNDlog[train15])^2 )
FBND_MSE1

# BSCQ Model
BSCQ_model <- glm(BSCQlog~., data = df2, subset = train16)
BSCQ_model

# BSCQ Testing MSE
BSCQ_pred <- predict(BSCQ_model, df2[-train16,])
BSCQ_MSE <- mean((BSCQ_pred - df2$BSCQlog[-train16])^2 )
BSCQ_MSE

# BSCQ Training MSE
BSCQ_pred1 <- predict(BSCQ_model, df2[train16,])
BSCQ_MSE1 <- mean((BSCQ_pred1 - df2$BSCQlog[train16])^2 )
BSCQ_MSE1

# TOTL Model
TOTL_model <- glm(TOTLlog~., data = df2, subset = train17)
TOTL_model

# TOTL Testing MSE
TOTL_pred <- predict(TOTL_model, df2[-train17,])
TOTL_MSE <- mean((TOTL_pred - df2$TOTLlog[-train17])^2 )
TOTL_MSE

# TOTL Training MSE
TOTL_pred1 <- predict(TOTL_model, df2[train17,])
TOTL_MSE1 <- mean((TOTL_pred1 - df2$TOTLlog[train17])^2 )
TOTL_MSE1

# ICSH Model
ICSH_model <- glm(ICSHlog~., data = df2, subset = train18)
ICSH_model

# ICSH Testing MSE
ICSH_pred <- predict(ICSH_model, df2[-train18,])
ICSH_MSE <- mean((ICSH_pred - df2$ICSHlog[-train18])^2 )
ICSH_MSE

# ICSH Training MSE
ICSH_pred1 <- predict(ICSH_model, df2[train18,])
ICSH_MSE1 <- mean((ICSH_pred1 - df2$ICSHlog[train18])^2 )
ICSH_MSE1

# BKLN Model
BKLN_model <- glm(BKLNlog~., data = df2, subset = train19)
BKLN_model

# BKLN Testing MSE
BKLN_pred <- predict(BKLN_model, df2[-train19,])
BKLN_MSE <- mean((BKLN_pred - df2$BKLNlog[-train19])^2 )
BKLN_MSE

# BKLN Training MSE
BKLN_pred1 <- predict(BKLN_model, df2[train19,])
BKLN_MSE1 <- mean((BKLN_pred1 - df2$BKLNlog[train19])^2 )
BKLN_MSE1

# VTEB Model
VTEB_model <- glm(VTEBlog~., data = df2, subset = train20)
VTEB_model

# VTEB Testing MSE
VTEB_pred <- predict(VTEB_model, df2[-train20,])
VTEB_MSE <- mean((VTEB_pred - df2$VTEBlog[-train20])^2 )
VTEB_MSE

# VTEB Training MSE
VTEB_pred1 <- predict(VTEB_model, df2[train20,])
VTEB_MSE1 <- mean((VTEB_pred1 - df2$VTEBlog[train20])^2 )
VTEB_MSE1

lintrainMSE = c(IVV_MSE1,
                IJH_MSE1,
                IJR_MSE1,
                SUSA_MSE1,
                VXUS_MSE1,
                SCHD_MSE1,
                VIG_MSE1,
                DNL_MSE1,
                PDBC_MSE1,
                RYF_MSE1,
                RYH_MSE1,
                PBW_MSE1,
                XLK_MSE1,
                VGK_MSE1,
                FBND_MSE1,
                BSCQ_MSE1,
                TOTL_MSE1,
                ICSH_MSE1,
                BKLN_MSE1,
                VTEB_MSE1)

lintestMSE = c(IVV_MSE,
               IJH_MSE,
               IJR_MSE,
               SUSA_MSE,
               VXUS_MSE,
               SCHD_MSE,
               VIG_MSE,
               DNL_MSE,
               PDBC_MSE,
               RYF_MSE,
               RYH_MSE,
               PBW_MSE,
               XLK_MSE,
               VGK_MSE,
               FBND_MSE,
               BSCQ_MSE,
               TOTL_MSE,
               ICSH_MSE,
               BKLN_MSE,
               VTEB_MSE)

linMSE = cbind(lintrainMSE, lintestMSE)
rownames(linMSE) = tickers
linMSE

## Logistic Regression

# Create directions
IVVdirection = (IVVlog > 0)+0
IJHdirection = (IJHlog > 0)+0
IJRdirection = (IJRlog > 0)+0
SUSAdirection = (SUSAlog > 0)+0
VXUSdirection = (VXUSlog > 0)+0
SCHDdirection = (SCHDlog > 0)+0
VIGdirection = (VIGlog > 0)+0
DNLdirection = (DNLlog > 0)+0
PDBCdirection = (PDBClog > 0)+0
RYFdirection = (RYFlog > 0)+0
RYHdirection = (RYHlog > 0)+0
PBWdirection = (PBWlog > 0)+0
XLKdirection = (XLKlog > 0)+0
VGKdirection = (VGKlog > 0)+0
FBNDdirection = (FBNDlog > 0)+0
BSCQdirection = (BSCQlog > 0)+0
TOTLdirection = (TOTLlog > 0)+0
ICSHdirection = (ICSHlog > 0)+0
BKLNdirection = (BKLNlog > 0)+0
VTEBdirection = (VTEBlog > 0)+0

# Logistic Regression for the direction of the stock as a function of 
# the 1 and 2 lagged returns

# IVV Logistic

dfIVV = data.frame(IVV_lag1, IVV_lag2)

logistic.reg = glm(IVVdirection ~ . , data=dfIVV , subset=train1, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfIVV[-train1,] , type="response")

y.logistic.pred=rep(0,length(IVVlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==IVVdirection[-train1])
logistic.acc

table(y.logistic.pred , IVVdirection[-train1])

# IJH Logistic

dfIJH = data.frame(IJH_lag1, IJH_lag2)

logistic.reg = glm(IJHdirection ~ . , data=dfIJH , subset=train2, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfIJH[-train2,] , type="response")

y.logistic.pred=rep(0,length(IJHlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==IJHdirection[-train2])
logistic.acc

table(y.logistic.pred , IJHdirection[-train2])

# IJR Logistic

dfIJR = data.frame(IJR_lag1, IJR_lag2)

logistic.reg = glm(IJRdirection ~ . , data=dfIJR , subset=train3, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfIJR[-train3,] , type="response")

y.logistic.pred=rep(0,length(IJRlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==IJRdirection[-train3])
logistic.acc

table(y.logistic.pred , IJRdirection[-train3])

# SUSA Logistic

dfSUSA = data.frame(SUSA_lag1, SUSA_lag2)

logistic.reg = glm(SUSAdirection ~ . , data=dfSUSA , subset=train4, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfSUSA[-train4,] , type="response")

y.logistic.pred=rep(0,length(SUSAlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==SUSAdirection[-train4])
logistic.acc

table(y.logistic.pred , SUSAdirection[-train4])

# VXUS Logistic

dfVXUS = data.frame(VXUS_lag1, VXUS_lag2)

logistic.reg = glm(VXUSdirection ~ . , data=dfVXUS , subset=train5, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfVXUS[-train5,] , type="response")

y.logistic.pred=rep(0,length(VXUSlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==VXUSdirection[-train5])
logistic.acc

table(y.logistic.pred , VXUSdirection[-train5])

# SCHD Logistic

dfSCHD = data.frame(SCHD_lag1, SCHD_lag2)

logistic.reg = glm(SCHDdirection ~ . , data=dfSCHD , subset=train6, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfSCHD[-train6,] , type="response")

y.logistic.pred=rep(0,length(SCHDlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==SCHDdirection[-train6])
logistic.acc

table(y.logistic.pred , SCHDdirection[-train6])

# VIG Logistic

dfVIG = data.frame(VIG_lag1, VIG_lag2)

logistic.reg = glm(VIGdirection ~ . , data=dfVIG , subset=train7, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfVIG[-train7,] , type="response")

y.logistic.pred=rep(0,length(VIGlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==VIGdirection[-train7])
logistic.acc

table(y.logistic.pred , VIGdirection[-train7])

# DNL Logistic

dfDNL = data.frame(DNL_lag1, DNL_lag2)

logistic.reg = glm(DNLdirection ~ . , data=dfDNL , subset=train8, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfDNL[-train8,] , type="response")

y.logistic.pred=rep(0,length(DNLlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==DNLdirection[-train8])
logistic.acc

table(y.logistic.pred , DNLdirection[-train8])

# PDBC Logistic

dfPDBC = data.frame(PDBC_lag1, PDBC_lag2)

logistic.reg = glm(PDBCdirection ~ . , data=dfPDBC , subset=train9, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfPDBC[-train9,] , type="response")

y.logistic.pred=rep(0,length(PDBClog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==PDBCdirection[-train9])
logistic.acc

table(y.logistic.pred , PDBCdirection[-train9])

# RYF Logistic

dfRYF = data.frame(RYF_lag1, RYF_lag2)

logistic.reg = glm(RYFdirection ~ . , data=dfRYF , subset=train10, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfRYF[-train10,] , type="response")

y.logistic.pred=rep(0,length(RYFlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==RYFdirection[-train10])
logistic.acc

table(y.logistic.pred , RYFdirection[-train10])

# RYH Logistic

dfRYH = data.frame(RYH_lag1, RYH_lag2)

logistic.reg = glm(RYHdirection ~ . , data=dfRYH , subset=train11, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfRYH[-train11,] , type="response")

y.logistic.pred=rep(0,length(RYHlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==RYHdirection[-train11])
logistic.acc

table(y.logistic.pred , RYHdirection[-train11])

# PBW Logistic

dfPBW = data.frame(PBW_lag1, PBW_lag2)

logistic.reg = glm(PBWdirection ~ . , data=dfPBW , subset=train12, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfPBW[-train12,] , type="response")

y.logistic.pred=rep(0,length(PBWlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==PBWdirection[-train12])
logistic.acc

table(y.logistic.pred , PBWdirection[-train12])

# XLK Logistic

dfXLK = data.frame(XLK_lag1, XLK_lag2)

logistic.reg = glm(XLKdirection ~ . , data=dfXLK , subset=train13, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfXLK[-train13,] , type="response")

y.logistic.pred=rep(0,length(XLKlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==XLKdirection[-train13])
logistic.acc

table(y.logistic.pred , XLKdirection[-train13])

# VGK Logistic

dfVGK = data.frame(VGK_lag1, VGK_lag2)

logistic.reg = glm(VGKdirection ~ . , data=dfVGK , subset=train14, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfVGK[-train14,] , type="response")

y.logistic.pred=rep(0,length(VGKlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==VGKdirection[-train14])
logistic.acc

table(y.logistic.pred , VGKdirection[-train14])

# FBND Logistic

dfFBND = data.frame(FBND_lag1, FBND_lag2)

logistic.reg = glm(FBNDdirection ~ . , data=dfFBND , subset=train15, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfFBND[-train15,] , type="response")

y.logistic.pred=rep(0,length(FBNDlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==FBNDdirection[-train15])
logistic.acc

table(y.logistic.pred , FBNDdirection[-train15])

# BSCQ Logistic

dfBSCQ = data.frame(BSCQ_lag1, BSCQ_lag2)

logistic.reg = glm(BSCQdirection ~ . , data=dfBSCQ , subset=train16, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfBSCQ[-train16,] , type="response")

y.logistic.pred=rep(0,length(BSCQlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==BSCQdirection[-train16])
logistic.acc

table(y.logistic.pred , BSCQdirection[-train16])

# TOTL Logistic

dfTOTL = data.frame(TOTL_lag1, TOTL_lag2)

logistic.reg = glm(TOTLdirection ~ . , data=dfTOTL , subset=train17, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfTOTL[-train17,] , type="response")

y.logistic.pred=rep(0,length(TOTLlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==TOTLdirection[-train17])
logistic.acc

table(y.logistic.pred , TOTLdirection[-train17])

# ICSH Logistic

dfICSH = data.frame(ICSH_lag1, ICSH_lag2)

logistic.reg = glm(ICSHdirection ~ . , data=dfICSH , subset=train18, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfICSH[-train18,] , type="response")

y.logistic.pred=rep(0,length(ICSHlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==ICSHdirection[-train18])
logistic.acc

table(y.logistic.pred , ICSHdirection[-train18])

# BKLN Logistic

dfBKLN = data.frame(BKLN_lag1, BKLN_lag2)

logistic.reg = glm(BKLNdirection ~ . , data=dfBKLN , subset=train19, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfBKLN[-train19,] , type="response")

y.logistic.pred=rep(0,length(BKLNlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==BKLNdirection[-train19])
logistic.acc

table(y.logistic.pred , BKLNdirection[-train19])

# VTEB Logistic

dfVTEB = data.frame(VTEB_lag1, VTEB_lag2)

logistic.reg = glm(VTEBdirection ~ . , data=dfVTEB , subset=train20, family=binomial)
summary(logistic.reg)

logistic.probs = predict(logistic.reg , newdata=dfVTEB[-train20,] , type="response")

y.logistic.pred=rep(0,length(VTEBlog)/5)
y.logistic.pred[logistic.probs>.5] = 1

logistic.acc = mean(y.logistic.pred==VTEBdirection[-train20])
logistic.acc

table(y.logistic.pred , VTEBdirection[-train20])

## Decision Tree

tree.reg1 = tree(IVVlog ~ IVV_lag1 + IVV_lag2 , data=dfIVV,
                 subset=train)
plot(tree.reg1)
text(tree.reg1)

mean((dfIVV$IVVlog[train] - predict(tree.reg1, df[train,]))^2)

pred=predict(tree.reg1,dfIVV[-train,])
tree.MSE1 = mean((pred-dfIVV$IVVlog[-train])^2)
tree.MSE1

tree.reg2 = tree(IJHlog ~ IJH_lag1 + IJH_lag2 , data=dfIJH,
                 subset=train)
plot(tree.reg2)
text(tree.reg2)

mean((dfIJH$IJHlog[train] - predict(tree.reg2, df[train,]))^2)

pred=predict(tree.reg2,dfIJH[-train,])
tree.MSE2 = mean((pred-dfIJH$IJHlog[-train])^2)
tree.MSE2

tree.reg3 = tree(IJRlog ~ IJR_lag1 + IJR_lag2 , data=dfIJR,
                 subset=train)
plot(tree.reg3)
text(tree.reg3)

mean((dfIJR$IJRlog[train] - predict(tree.reg3, df[train,]))^2)

pred=predict(tree.reg3,dfIJR[-train,])
tree.MSE3 = mean((pred-dfIJR$IJRlog[-train])^2)
tree.MSE3

tree.reg4 = tree(SUSAlog ~ SUSA_lag1 + SUSA_lag2 , data=dfSUSA,
                 subset=train)
plot(tree.reg4)
text(tree.reg4)

mean((dfSUSA$SUSAlog[train] - predict(tree.reg4, df[train,]))^2)

pred=predict(tree.reg4,dfSUSA[-train,])
tree.MSE4 = mean((pred-dfSUSA$SUSAlog[-train])^2)
tree.MSE4

tree.reg5 = tree(VXUSlog ~ VXUS_lag1 + VXUS_lag2 , data=dfVXUS,
                 subset=train)
plot(tree.reg5)
text(tree.reg5)

mean((dfVXUS$VXUSlog[train] - predict(tree.reg5, df[train,]))^2)

pred=predict(tree.reg5,dfVXUS[-train,])
tree.MSE5 = mean((pred-dfVXUS$VXUSlog[-train])^2)
tree.MSE5

tree.reg6 = tree(SCHDlog ~ SCHD_lag1 + SCHD_lag2 , data=dfSCHD,
                 subset=train)
plot(tree.reg6)
text(tree.reg6)

mean((dfSCHD$SCHDlog[train] - predict(tree.reg6, df[train,]))^2)

pred=predict(tree.reg6,dfSCHD[-train,])
tree.MSE6 = mean((pred-dfSCHD$SCHDlog[-train])^2)
tree.MSE6

tree.reg7 = tree(VIGlog ~ VIG_lag1 + VIG_lag2 , data=dfVIG,
                 subset=train)
plot(tree.reg7)
text(tree.reg7)

mean((dfVIG$VIGlog[train] - predict(tree.reg7, df[train,]))^2)

pred=predict(tree.reg7,dfVIG[-train,])
tree.MSE7 = mean((pred-dfVIG$VIGlog[-train])^2)
tree.MSE7

tree.reg8 = tree(DNLlog ~ DNL_lag1 + DNL_lag2 , data=dfDNL,
                 subset=train)
plot(tree.reg8)
text(tree.reg8)

mean((dfDNL$DNLlog[train] - predict(tree.reg8, df[train,]))^2)

pred=predict(tree.reg8,dfDNL[-train,])
tree.MSE8 = mean((pred-dfDNL$DNLlog[-train])^2)
tree.MSE8

tree.reg9 = tree(PDBClog ~ PDBC_lag1 + PDBC_lag2 , data=dfPDBC,
                 subset=train)
#plot(tree.reg9)
#text(tree.reg9)

mean((dfPDBC$PDBClog[train] - predict(tree.reg9, df[train,]))^2)

pred=predict(tree.reg9,dfPDBC[-train,])
tree.MSE9 = mean((pred-dfPDBC$PDBClog[-train])^2)
tree.MSE9

tree.reg10 = tree(RYFlog ~ RYF_lag1 + RYF_lag2 , data=dfRYF,
                  subset=train)
plot(tree.reg10)
text(tree.reg10)

mean((dfRYF$RYFlog[train] - predict(tree.reg10, df[train,]))^2)

pred=predict(tree.reg10,dfRYF[-train,])
tree.MSE10 = mean((pred-dfRYF$RYFlog[-train])^2)
tree.MSE10

tree.reg11 = tree(RYHlog ~ RYH_lag1 + RYH_lag2 , data=dfRYH,
                  subset=train)
plot(tree.reg11)
text(tree.reg11)

mean((dfRYH$RYHlog[train] - predict(tree.reg11, df[train,]))^2)

pred=predict(tree.reg11,dfRYH[-train,])
tree.MSE11 = mean((pred-dfRYH$RYHlog[-train])^2)
tree.MSE11

tree.reg12 = tree(PBWlog ~ PBW_lag1 + PBW_lag2 , data=dfPBW,
                  subset=train)
plot(tree.reg12)
text(tree.reg12)

mean((dfPBW$PBWlog[train] - predict(tree.reg12, df[train,]))^2)

pred=predict(tree.reg12,dfPBW[-train,])
tree.MSE12 = mean((pred-dfPBW$PBWlog[-train])^2)
tree.MSE12

tree.reg13 = tree(XLKlog ~ XLK_lag1 + XLK_lag2 , data=dfXLK,
                  subset=train)
plot(tree.reg13)
text(tree.reg13)

mean((dfXLK$XLKlog[train] - predict(tree.reg13, df[train,]))^2)

pred=predict(tree.reg13,dfXLK[-train,])
tree.MSE13 = mean((pred-dfXLK$XLKlog[-train])^2)
tree.MSE13

tree.reg14 = tree(VGKlog ~ VGK_lag1 + VGK_lag2 , data=dfVGK,
                  subset=train)
plot(tree.reg14)
text(tree.reg14)

mean((dfVGK$VGKlog[train] - predict(tree.reg14, df[train,]))^2)

pred=predict(tree.reg14,dfVGK[-train,])
tree.MSE14 = mean((pred-dfVGK$VGKlog[-train])^2)
tree.MSE14

tree.reg15 = tree(FBNDlog ~ FBND_lag1 + FBND_lag2 , data=dfFBND,
                  subset=train)
plot(tree.reg15)
text(tree.reg15)

mean((dfFBND$FBNDlog[train] - predict(tree.reg15, df[train,]))^2)

pred=predict(tree.reg15,dfFBND[-train,])
tree.MSE15 = mean((pred-dfFBND$FBNDlog[-train])^2)
tree.MSE15

tree.reg16 = tree(BSCQlog ~ BSCQ_lag1 + BSCQ_lag2 , data=dfBSCQ,
                  subset=train)
plot(tree.reg16)
text(tree.reg16)

mean((dfBSCQ$BSCQlog[train] - predict(tree.reg16, df[train,]))^2)

pred=predict(tree.reg16,dfBSCQ[-train,])
tree.MSE16 = mean((pred-dfBSCQ$BSCQlog[-train])^2)
tree.MSE16

tree.reg17 = tree(TOTLlog ~ TOTL_lag1 + TOTL_lag2 , data=dfTOTL,
                  subset=train)
plot(tree.reg17)
text(tree.reg17)

mean((dfTOTL$TOTLlog[train] - predict(tree.reg17, df[train,]))^2)

pred=predict(tree.reg17,dfTOTL[-train,])
tree.MSE17 = mean((pred-dfTOTL$TOTLlog[-train])^2)
tree.MSE17

tree.reg18 = tree(ICSHlog ~ ICSH_lag1 + ICSH_lag2 , data=dfICSH,
                  subset=train)
plot(tree.reg18)
text(tree.reg18)

mean((dfICSH$ICSHlog[train] - predict(tree.reg18, df[train,]))^2)

pred=predict(tree.reg18,dfICSH[-train,])
tree.MSE18 = mean((pred-dfICSH$ICSHlog[-train])^2)
tree.MSE18

tree.reg19 = tree(BKLNlog ~ BKLN_lag1 + BKLN_lag2 , data=dfBKLN,
                  subset=train)
plot(tree.reg19)
text(tree.reg19)

mean((dfBKLN$BKLNlog[train] - predict(tree.reg19, df[train,]))^2)

pred=predict(tree.reg19,dfBKLN[-train,])
tree.MSE19 = mean((pred-dfBKLN$BKLNlog[-train])^2)
tree.MSE19

tree.reg20 = tree(VTEBlog ~ VTEB_lag1 + VTEB_lag2 , data=dfVTEB,
                  subset=train)
plot(tree.reg20)
text(tree.reg20)

mean((dfVTEB$VTEBlog[train] - predict(tree.reg20, df[train,]))^2)

pred=predict(tree.reg20,dfVTEB[-train,])
tree.MSE20 = mean((pred-dfVTEB$VTEBlog[-train])^2)
tree.MSE20
```

```{r, warning = FALSE}
decisiontrainMSE = c(mean((dfIVV$IVVlog[train] - predict(tree.reg1, df[train,]))^2), 
                     mean((dfIJH$IJHlog[train] - predict(tree.reg2, df[train,]))^2), 
                     mean((dfIJR$IJRlog[train] - predict(tree.reg3, df[train,]))^2), 
                     mean((dfSUSA$SUSAlog[train] - predict(tree.reg4, df[train,]))^2), 
                     mean((dfVXUS$VXUSlog[train] - predict(tree.reg5, df[train,]))^2), 
                     mean((dfSCHD$SCHDlog[train] - predict(tree.reg6, df[train,]))^2), 
                     mean((dfVIG$VIGlog[train] - predict(tree.reg7, df[train,]))^2), 
                     mean((dfDNL$DNLlog[train] - predict(tree.reg8, df[train,]))^2), 
                     mean((dfPDBC$PDBClog[train] - predict(tree.reg9, df[train,]))^2), 
                     mean((dfRYF$RYFlog[train] - predict(tree.reg10, df[train,]))^2),
                     mean((dfRYH$RYHlog[train] - predict(tree.reg11, df[train,]))^2), 
                     mean((dfPBW$PBWlog[train] - predict(tree.reg12, df[train,]))^2), 
                     mean((dfXLK$XLKlog[train] - predict(tree.reg13, df[train,]))^2), 
                     mean((dfVGK$VGKlog[train] - predict(tree.reg14, df[train,]))^2), 
                     mean((dfFBND$FBNDlog[train] - predict(tree.reg15, df[train,]))^2), 
                     mean((dfBSCQ$BSCQlog[train] - predict(tree.reg16, df[train,]))^2), 
                     mean((dfTOTL$TOTLlog[train] - predict(tree.reg17, df[train,]))^2), 
                     mean((dfICSH$ICSHlog[train] - predict(tree.reg18, df[train,]))^2), 
                     mean((dfBKLN$BKLNlog[train] - predict(tree.reg19, df[train,]))^2), 
                     mean((dfVTEB$VTEBlog[train] - predict(tree.reg20, df[train,]))^2))

decisiontestMSE = c(tree.MSE1, 
                    tree.MSE2,
                    tree.MSE3, 
                    tree.MSE4, 
                    tree.MSE5, 
                    tree.MSE6, 
                    tree.MSE7, 
                    tree.MSE8, 
                    tree.MSE9, 
                    tree.MSE10, 
                    tree.MSE11, 
                    tree.MSE12, 
                    tree.MSE13, 
                    tree.MSE14, 
                    tree.MSE15,
                    tree.MSE16, 
                    tree.MSE17, 
                    tree.MSE18, 
                    tree.MSE19, 
                    tree.MSE20)

decisionMSE = cbind(decisiontrainMSE, decisiontestMSE)
rownames(decisionMSE) = tickers
decisionMSE

## Random Forests

dfIVV= data.frame(IVVlog, IVV_lag1, IVV_lag2)
dfIJH= data.frame(IJHlog, IJH_lag1,IJH_lag2)
dfIJR= data.frame(IJRlog, IJR_lag1,IJR_lag2)
dfSUSA= data.frame(SUSAlog, SUSA_lag1,SUSA_lag2)
dfVXUS= data.frame(VXUSlog, VXUS_lag1,VXUS_lag2)
dfSCHD= data.frame(SCHDlog, SCHD_lag1,SCHD_lag2)
dfVIG= data.frame(VIGlog, VIG_lag1,VIG_lag2)
dfDNL= data.frame(DNLlog, DNL_lag1,DNL_lag2)
dfPDBC= data.frame(PDBClog, PDBC_lag1,PDBC_lag2)
dfRYF= data.frame(RYFlog, RYF_lag1,RYF_lag2)
dfRYH= data.frame(RYHlog, RYH_lag1, RYH_lag2)
dfPBW= data.frame(PBWlog, PBW_lag1,PBW_lag2)
dfXLK= data.frame(XLKlog, XLK_lag1,XLK_lag2)
dfVGK= data.frame(VGKlog, VGK_lag1,VGK_lag2)
dfFBND= data.frame(FBNDlog, FBND_lag1,FBND_lag2)
dfBSCQ= data.frame(BSCQlog, BSCQ_lag1,BSCQ_lag2)
dfTOTL= data.frame(TOTLlog, TOTL_lag1,TOTL_lag2)
dfICSH= data.frame(ICSHlog, ICSH_lag1,ICSH_lag2)
dfBKLN= data.frame(BKLNlog, BKLN_lag1,BKLN_lag2)
dfVTEB= data.frame(VTEBlog, VTEB_lag1,VTEB_lag2)

rfIVV = randomForest(IVVlog~., data=dfIVV, subset=train, ntree=250, mtry=2, importance=TRUE) 
rfIVV
trainingIVV = mean((dfIVV$IVVlog[train] - predict(rfIVV, dfIVV[train,]))^2)
trainingIVV
predIVV=predict(rfIVV,dfIVV[-train,])
testIVV = mean((predIVV-dfIVV$IVVlog[-train])^2) 
testIVV

rfIJH = randomForest(IJHlog~., data=dfIJH, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfIJH
trainingIJH = mean((dfIJH$IJHlog[train] - predict(rfIJH, dfIJH[train,]))^2)
trainingIJH
predIJH=predict(rfIJH,dfIJH[-train,])
testIJH = mean((predIJH-dfIJH$IJHlog[-train])^2) 
testIJH

rfIJR = randomForest(IJRlog~., data=dfIJR, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfIJR
trainingIJR = mean((dfIJR$IJRlog[train] - predict(rfIJR, dfIJR[train,]))^2)
trainingIJR
predIJR=predict(rfIJR,dfIJR[-train,])
testIJR = mean((predIJR-dfIJR$IJRlog[-train])^2) 
testIJR

rfSUSA = randomForest(SUSAlog~., data=dfSUSA, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfSUSA
trainingSUSA = mean((dfSUSA$SUSAlog[train] - predict(rfSUSA, dfSUSA[train,]))^2)
trainingSUSA
predSUSA =predict(rfSUSA,dfSUSA[-train,])
testSUSA = mean((predSUSA-dfSUSA$SUSAlog[-train])^2) 
testSUSA

rfVXUS = randomForest(VXUSlog~., data=dfVXUS, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfVXUS
trainingVXUS = mean((dfVXUS$VXUSlog[train] - predict(rfVXUS, dfVXUS[train,]))^2)
trainingVXUS
predVXUS=predict(rfVXUS,dfVXUS[-train,])
testVXUS = mean((predVXUS-dfVXUS$VXUSlog[-train])^2) 
testVXUS

rfSCHD = randomForest(SCHDlog~., data=dfSCHD, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfSCHD
trainingSCHD = mean((dfSCHD$SCHDlog[train] - predict(rfSCHD, dfSCHD[train,]))^2)
trainingSCHD
predSCHD=predict(rfSCHD,dfSCHD[-train,])
testSCHD = mean((predSCHD-dfSCHD$SCHDlog[-train])^2) 
testSCHD

rfVIG = randomForest(VIGlog~., data=dfVIG, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfVIG
trainingVIG = mean((dfVIG$VIGlog[train] - predict(rfVIG, dfVIG[train,]))^2)
trainingVIG
predVIG=predict(rfVIG,dfVIG[-train,])
testVIG = mean((predVIG-dfVIG$VIGlog[-train])^2) 
testVIG

rfDNL = randomForest(DNLlog~., data=dfDNL, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfDNL
trainingDNL = mean((dfDNL$DNLlog[train] - predict(rfDNL, dfDNL[train,]))^2)
trainingDNL
predDNL=predict(rfDNL,dfDNL[-train,])
testDNL = mean((predDNL-dfDNL$DNLlog[-train])^2) 
testDNL

rfPDBC = randomForest(PDBClog~., data=dfPDBC, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfPDBC
trainingPDBC = mean((dfPDBC$PDBClog[train] - predict(rfPDBC, dfPDBC[train,]))^2)
trainingPDBC
predPDBC=predict(rfPDBC,dfPDBC[-train,])
testPDBC = mean((predPDBC-dfPDBC$PDBClog[-train])^2) 
testPDBC

rfRYF = randomForest(RYFlog~., data=dfRYF, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfRYF
trainingRYF = mean((dfRYF$RYFlog[train] - predict(rfRYF, dfRYF[train,]))^2)
trainingRYF
predRYF =predict(rfRYF,dfRYF[-train,])
testRYF = mean((predRYF-dfRYF$RYFlog[-train])^2) 
testRYF

rfRYH = randomForest(RYHlog~., data=dfRYH, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfRYH
trainingRYH = mean((dfRYH$RYHlog[train] - predict(rfRYH, dfRYH[train,]))^2)
trainingRYH
predRYH =predict(rfRYH,dfRYH[-train,])
testRYH = mean((predRYH-dfRYH$RYHlog[-train])^2) 
testRYH

rfPBW = randomForest(PBWlog~., data=dfPBW, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfPBW
trainingPBW = mean((dfPBW$PBWlog[train] - predict(rfPBW, dfPBW[train,]))^2)
trainingPBW
predPBW =predict(rfPBW,dfPBW[-train,])
testPBW = mean((predPBW-dfPBW$PBWlog[-train])^2) 
testPBW

rfXLK = randomForest(XLKlog~., data=dfXLK, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfXLK
trainingXLK = mean((dfXLK$XLKlog[train] - predict(rfXLK, dfXLK[train,]))^2)
trainingXLK
predXLK =predict(rfXLK,dfXLK[-train,])
testXLK = mean((predXLK-dfXLK$XLKlog[-train])^2) 
testXLK

rfVGK = randomForest(VGKlog~., data=dfVGK, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfVGK
trainingVGK = mean((dfVGK$VGKlog[train] - predict(rfVGK, dfVGK[train,]))^2)
trainingVGK
predVGK =predict(rfVGK,dfVGK[-train,])
testVGK = mean((predVGK-dfVGK$VGKlog[-train])^2) 
testVGK

rfFBND = randomForest(FBNDlog~., data=dfFBND, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfFBND
trainingFBND = mean((dfFBND$FBNDlog[train] - predict(rfFBND, dfFBND[train,]))^2)
trainingFBND
predFBND =predict(rfFBND,dfFBND[-train,])
testFBND = mean((predFBND-dfFBND$FBNDlog[-train])^2) 
testFBND

rfBSCQ = randomForest(BSCQlog~., data=dfBSCQ, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfBSCQ
trainingBSCQ = mean((dfBSCQ$BSCQlog[train] - predict(rfBSCQ, dfBSCQ[train,]))^2)
trainingBSCQ
predBSCQ =predict(rfBSCQ,dfBSCQ[-train,])
testBSCQ = mean((predBSCQ-dfBSCQ$BSCQlog[-train])^2) 
testBSCQ

rfTOTL = randomForest(TOTLlog~., data=dfTOTL, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfTOTL
trainingTOTL = mean((dfTOTL$TOTLlog[train] - predict(rfTOTL, dfTOTL[train,]))^2)
trainingTOTL
predTOTL =predict(rfTOTL,dfTOTL[-train,])
testTOTL = mean((predTOTL-dfTOTL$TOTLlog[-train])^2) 
testTOTL

rfICSH = randomForest(ICSHlog~., data=dfICSH, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfICSH
trainingICSH = mean((dfICSH$ICSHlog[train] - predict(rfICSH, dfICSH[train,]))^2)
trainingICSH
predICSH =predict(rfICSH,dfICSH[-train,])
testICSH = mean((predICSH-dfICSH$ICSHlog[-train])^2) 
testICSH

rfBKLN = randomForest(BKLNlog~., data=dfBKLN, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfBKLN
trainingBKLN = mean((dfBKLN$BKLNlog[train] - predict(rfBKLN, dfBKLN[train,]))^2)
trainingBKLN
predBKLN =predict(rfBKLN,dfBKLN[-train,])
testBKLN = mean((predBKLN-dfBKLN$BKLNlog[-train])^2) 
testBKLN

rfVTEB = randomForest(VTEBlog~., data=dfVTEB, subset=train,ntree=250, mtry=2, importance=TRUE) 
rfVTEB
trainingVTEB = mean((dfVTEB$VTEBlog[train] - predict(rfVTEB, dfVTEB[train,]))^2)
trainingVTEB
predVTEB =predict(rfVTEB,dfVTEB[-train,])
testVTEB = mean((predVTEB-dfVTEB$VTEBlog[-train])^2) 
testVTEB

trainingMSE = c(trainingIVV, trainingIJH, trainingIJR, trainingSUSA, trainingVXUS, trainingSCHD, trainingVIG, trainingDNL, trainingPDBC, trainingRYF,
                trainingRYH, trainingPBW, trainingXLK, trainingVGK, trainingFBND, trainingBSCQ, trainingTOTL, trainingICSH, trainingBKLN, trainingVTEB)

testMSE = c(testIVV, testIJH, testIJR, testSUSA, testVXUS, testSCHD, testVIG, testDNL, testPDBC, testRYF, testRYH, testPBW, testXLK, testVGK, testFBND,
            testBSCQ, testTOTL, testICSH, testBKLN, testVTEB)

MSE = cbind(trainingMSE, testMSE)
rownames(MSE) = tickers
MSE <- data.frame(MSE)

MSE$per_error <- abs((MSE$trainingMSE - MSE$testMSE)/(MSE$testMSE))
MSE[
  with(MSE, order(per_error)),
]
MSE