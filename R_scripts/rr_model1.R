
m1 <- asreml::asreml(fixed = resp ~ loc + gen,
                     random = ~ block + fa(loc, 1):id(gen) + loc:ddayc:gen,
                     residual = ~ dsum(~ante(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m2 <- asreml::asreml(fixed = resp ~ loc + gen,
                     random = ~ block + fa(loc, 1):id(gen) + loc:ddayc:gen + loc:ddayc_sq:gen,
                     residual = ~ dsum(~ante(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

infoCriteria.asreml(m1)
# fixedDF varDF NBound      AIC     BIC    loglik
# 1       0    68      4 2030.741 2487.81 -947.3705

infoCriteria.asreml(m2)
# fixedDF varDF NBound      AIC      BIC    loglik
# 1       0    67      6 1987.933 2438.281 -926.9667

lrt(m1, m2)
# df LR-statistic Pr(Chisq)    
# m2/m1  1       40.808 8.399e-11 ***

m3 <- asreml::asreml(fixed = resp ~ loc + gen,
                     random = ~ fa(loc, 1):id(gen) + loc:ddayc:gen + loc:ddayc_sq:gen + loc:block,
                     residual = ~ dsum(~ante(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

infoCriteria.asreml(m3)
# fixedDF varDF NBound      AIC      BIC    loglik
# 1       0    67      6 1989.777 2440.125 -927.8887
lrt(m2, m3, boundary = F)
# df LR-statistic Pr(Chisq)
# m3/m2  0       -1.844         1

lrt(m1, m3)
# df LR-statistic Pr(Chisq)    
# m3/m1  1       38.964 2.159e-10 ***

m4 <- asreml::asreml(fixed = resp ~ loc + gen,
                     random = ~ fa(loc, 1):id(gen) + loc:qi:gen + loc:block,
                     residual = ~ dsum(~ante(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

infoCriteria.asreml(m4)
# fixedDF varDF NBound      AIC      BIC    loglik
# 1       0    68      4 2030.697 2487.766 -947.3487

lrt(m2, m4)
# df LR-statistic Pr(Chisq)    
# m2/m4  1       40.764 8.588e-11 ***
data1 <- na.omit(data)

m5 <- asreml::asreml(fixed = resp ~ loc + gen,
                     random = ~ fa(loc, 1):id(gen) + diag(loc):spl(ddayc),
                     residual = ~ dsum(~ante(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

infoCriteria.asreml(m5)
# fixedDF varDF NBound      AIC      BIC    loglik
# 1       0    69      4 474.0091 937.7997 -168.0046
lrt(m1, m5)
# df LR-statistic Pr(Chisq)    
# m5/m1  1       1558.7 < 2.2e-16 ***

m6 <- asreml::asreml(fixed = resp ~ loc + gen,
                     random = ~ fa(loc, 1):id(gen) + diag(loc):spl(ddayc),
                     residual = ~ dsum(~corgh(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

infoCriteria.asreml(m6)
# fixedDF varDF NBound      AIC      BIC   loglik
# 1       0   172      6 96.93183 1253.047 123.5341

lrt.asreml(m5, m6)
# df LR-statistic Pr(Chisq)    
# m6/m5 105       583.08 < 2.2e-16 ***
head(data)

m7 <- asreml::asreml(fixed = resp ~ loc + gen,
                     random = ~ fa(loc, 1):id(gen) + diag(loc):spl(ddayc) + loc:block,
                     residual = ~ dsum(~corgh(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

infoCriteria.asreml(m7)
# fixedDF varDF NBound      AIC      BIC  loglik
# 1       0   173      6 149.8492 1312.686 98.0754

lrt.asreml(m6, m7, boundary = T)
# df LR-statistic Pr(Chisq)
# m7/m6  1      -50.917       0.5
lrt.asreml(m6, m7, boundary = F)
# df LR-statistic Pr(Chisq)
# m7/m6  1      -50.917         1

REMLRT.asreml(m6, m7, bound.test.parameters = "onlybound")
#   REMLRT        DF  p NBound.h0 NBound.h1
# 1 3.705009e-08  0   0         6         6
REMLRT.asreml(m2, m7, bound.test.parameters = "onlybound")

?as.asrtests
m7.wald <- wald.asreml(m7)
m7.wald
class(m7.wald)
summary(data$ddayc)

m8 <- asreml::asreml(fixed = resp ~ loc + gen,
                     random = ~ fa(loc, 1):id(gen) + corgh(loc):spl(ddayc),
                     residual = ~ dsum(~corgh(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

infoCriteria.asreml(m8)
# fixedDF varDF NBound      AIC      BIC   loglik
# 1       0   174      7 9.933791 1179.493 169.0331
levels(data$ddayc_sq)
m9 <- asreml::asreml(fixed = resp ~ loc + gen,
                     random = ~ fa(loc, 1):id(gen) + corgh(loc):spl(ddayc) + corgh(loc):spl(ddayc_sq),
                     residual = ~ dsum(~corgh(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

infoCriteria.asreml(m9)
# fixedDF varDF NBound       AIC     BIC   loglik
# 1       0   179      8 -220.2438 982.923 289.1219

lrt.asreml(m8, m9, boundary = F)


# df LR-statistic Pr(Chisq)    
# m8/m9 105       651.15 < 2.2e-16 ***

wald.asreml(m9)


m10 <- asreml::asreml(fixed = resp ~ loc + gen,
                     random = ~ loc:block + fa(loc, 1):id(gen) + corgh(loc):spl(ddayc) + corgh(loc):spl(ddayc_sq),
                     residual = ~ dsum(~corgh(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

infoCriteria.asreml(m10)
# fixedDF varDF NBound      AIC      BIC   loglik
# 1       0   180      8 -188.785 1021.103 274.3925


m11 <- asreml::asreml(fixed = resp ~ loc + gen,
                     random = ~ loc:block + fa(loc, 1):id(gen) + corgh(loc):spl(ddayc),
                     residual = ~ dsum(~corgh(year_cut):ar1(col):ar1(row) | loc),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

infoCriteria.asreml(m11)
# fixedDF varDF NBound      AIC     BIC   loglik
# 1       0   174      8 62.60105 1232.16 142.6995
summary(m11)$varcomp
dim(data)

m12 <- asreml::asreml(fixed = resp ~ loc + gen,
                      random = ~ loc:block + corgh(loc):spl(ddayc),
                      residual = ~ dsum(~corgh(year_cut):ar1(col):ar1(row) | loc),
                      data = data, 
                      na.action = list(x = "include", y = "include"))


infoCriteria.asreml(m12)
# fixedDF varDF NBound     AIC     BIC   loglik
# 1       0   171      5 5.04581 1154.44 168.4771

lrt.asreml(m8, m12, boundary = T)
# df LR-statistic Pr(Chisq)
# m8/m12  5        1.112    0.6359
wald.asreml(m12)
summary(m12)$varcomp

m13 <- asreml::asreml(fixed = resp ~ loc * gen,
                      random = ~ loc:block + corgh(loc):spl(ddayc),
                      residual = ~ dsum(~corgh(year_cut):ar1(col):ar1(row) | loc),
                      data = data,
                      na.action = list(x = "include", y = "include"))

wald.asreml(m13)
infoCriteria.asreml(m13)
# fixedDF varDF NBound       AIC      BIC  loglik
# 1       0   170      6 -150.1341 992.5383 245.067


# Load current fit into an asrtests object
current.asrt <- as.asrtests(m13, NULL, NULL)

current.asrt <- rmboundary.asrtests(m13)

blue <- predict.asreml(m12, classify='loc', vcov=TRUE)
blue$avsed
data$qi

diffs <- predictPlus(classify = "gen:loc", 
                     asreml.obj = m12, 
                     wald.tab = NULL, 
                     present = c("block","gen","loc","year_cut"))


asreml::asreml(fixed = resp ~ loc * gen,
               random = ~ loc:block + corgh(loc):spl(ddayc),
               residual = ~ dsum(~corgh(year_cut):ar1(col):ar1(row) | loc),
               data = data)
