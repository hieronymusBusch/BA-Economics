##########################################################
#                                                        #
# BA Econonomics:                                        #
# "The political economy of appliance replacement        #
#  programs for low-income households:                   #
#  An empirical study."                                  #
#                                                        #
# 2021/06                                                #
#                                                        #
# Alexander Busch (alexander.busch at stud.uni-hd dot de #
#                                                        #
# Data Analysis                                          #
#                                                        #
##########################################################

source("manipulation.R")



# Hypothesis 1.1 & 1.2 the political left
log1.1 <- glm(addSup ~ councilLeft, data=dfddProgNoSt, family=binomial)
summary (log1.1)
exp(coef(log1.1))
reg1.2.2 <- lm(addSupGRANTamountHH2 ~ councilLeft, data=dfddProgNoStAddSup)
summary (reg1.2.2)

# Hypothesis 1.3 & 1.4 female councilors
log1.3 <- glm(addSup ~ councilFem, data=dfddProgNoSt, family=binomial)
summary (log1.3)
exp(coef(log1.3))
reg1.4.2 <- lm(addSupGRANTamountHH2 ~ councilFem, data=dfddProgNoStAddSup)
summary (reg1.4.2)

# Hypothesis 1.5 & 1.6 share female parliamentarians 
log1.5 <- glm(addSup ~ parlFem, data=dfddProgNoSt, family=binomial)
summary (log1.5)
exp(coef(log1.5))
reg1.6.2 <- lm(addSupGRANTamountHH2 ~ parlFem, data=dfddProgNoStAddSup)
summary (reg1.6.2)

# Hypothesis 1.7 & 1.8 political instability
log1.7 <- glm(addSup ~ councilChange, data=dfddProgNoSt, family=binomial)
summary (log1.7)
exp(coef(log1.7))
reg1.8.2 <- lm(addSupGRANTamountHH2 ~ councilChange, data=dfddProgNoStAddSup)
summary (reg1.8.2)

# Hypothesis 1.9 & 1.10 political participation
log1.9 <- glm(addSup ~ turnout, data=dfddProgNoSt, family=binomial)
summary (log1.9)
exp(coef(log1.9))
reg1.10.2 <- lm(addSupGRANTamountHH2 ~ turnout, data=dfddProgNoStAddSup)
summary (reg1.10.2)

# Hyp. 1.1, 1.3, 1.5, 1.7, 1.9 combined 
log1 <- glm(addSup ~ councilLeft + parlFem + councilFem + councilChange + turnout
              , data=dfddProgNoSt, family=binomial)
summary (log1)
exp(coef(log1))

reg1.a.2 <- lm(addSupGRANTamountHH2 ~ councilLeft + parlFem + councilFem + councilChange + turnout, data=dfddProgNoStAddSup)
summary (reg1.a.2)


# Hypothesis 2.1 & 2.2 population density
log2.1 <- glm(addSup ~ popDensityHun, data=dfddProgNoSt, family=binomial)
summary (log2.1)
exp(coef(log2.1))
reg2.2.2 <- lm(addSupGRANTamountHH2 ~ popDensityHun, data=dfddProgNoStAddSup)
summary (reg2.2.2)

# Hypothesis 2.3 & 2.4 recipients
log2.3.1 <- glm(addSup ~ unemployment, data=dfddProgNoSt, family=binomial)
summary (log2.3.1)
exp(coef(log2.3.1))
reg2.4.2.1 <- lm(addSupGRANTamountHH2 ~ unemployment, data=dfddProgNoStAddSup)
summary (reg2.4.2.1)

log2.3.2 <- glm(addSup ~ hhHousingSup, data=dfddProgNoSt, family=binomial)
summary (log2.3.2)
exp(coef(log2.3.2))
reg2.4.2.2 <- lm(addSupGRANTamountHH2 ~ hhHousingSup, data=dfddProgNoStAddSup)
summary (reg2.4.2.2)

# Hypothesis 2.5 & 2.6 & 2.7 catholics
log2.5 <- glm(addSup ~ cathShare, data=dfddProgNoSt, family=binomial)
summary (log2.5)
exp(coef(log2.5))
reg2.6.2 <- lm(addSupGRANTamountHH2 ~ cathShare, data=dfddProgNoStAddSup)
summary (reg2.6.2)
log2.7 <- glm(inProgram ~ cathShare, data=dfdd, family=binomial)
summary (log2.7)
exp(coef(log2.7))

# Hypothesis 2.8 centralised funding
reg2.8.2 <- lm(addSupGRANTamountHH2 ~ progState, data=dfddProg)
summary (reg2.8.2)

# Hyp. 2 combined 
log2.a <- glm(addSup ~ popDensityHun + unemployment + cathShare, data=dfddProgNoSt, family=binomial)
summary (log2.a)
exp(coef(log2.a))
log2.b <- glm(addSup ~ popDensityHun + hhHousingSup + cathShare, data=dfddProgNoSt, family=binomial)
summary (log2.b)
exp(coef(log2.b))

reg2.a.2 <- lm(addSupGRANTamountHH2 ~ popDensityHun + unemployment + cathShare, data=dfddProgNoStAddSup)
summary (reg2.a.2)
reg2.b.2 <- lm(addSupGRANTamountHH2 ~ popDensityHun + hhHousingSup + cathShare, data=dfddProgNoStAddSup)
summary (reg2.b.2)

# Hypothesis 3.1 & 3.2 GDP
log3.1 <- glm(addSup ~ GDPpc, data=dfddProgNoSt, family=binomial)
summary (log3.1)
exp(coef(log3.1))
reg3.2.2 <- lm(addSupGRANTamountHH2 ~ GDPpc, data=dfddProgNoStAddSup)
summary (reg3.2.2)

# Hypothesis 3.3 & 3.4 
log3.3 <- glm(addSup ~ debtpcHun, data=dfddProgNoSt, family=binomial)
summary (log3.3)
exp(coef(log3.3))
reg3.4.2 <- lm(addSupGRANTamountHH2 ~ debtpcHun, data=dfddProgNoStAddSup)
summary (reg3.4.2)

# Hyp. 3 combined 
log3.a <- glm(addSup ~ GDPpc + debtpcHun, data=dfddProgNoSt, family=binomial)
summary (log3.a)
exp(coef(log3.a))
reg3.a.2 <- lm(addSupGRANTamountHH2 ~ GDPpc + debtpcHun, data=dfddProgNoStAddSup)
summary (reg3.a.2)

### All Combined pairwise
# logit model
log123p <- glm(addSup ~ councilLeft + councilFem + parlFem + councilChange + turnout +
               popDensityHun + hhHousingSup + cathShare +
                GDPpc + debtpcHun, data=dfddProgNoSt, family=binomial)
summary (log123p)
exp(coef(log123p))
log1p <- glm(addSup ~ councilLeft + councilFem + parlFem + councilChange + turnout
                , data=dfddProgNoSt, family=binomial)
summary (log1p)
exp(coef(log1p))
log3p <- glm(addSup ~ GDPpc + debtpcHun
               , data=dfddProgNoSt, family=binomial)
summary (log3p)
exp(coef(log3p))
log2p <- glm(addSup ~ popDensityHun + hhHousingSup + cathShare 
             , data=dfddProgNoSt, family=binomial)
summary (log2p)
exp(coef(log2p))

# regressions n = 83
reg123p.b <- lm(addSupGRANTamountHH2 ~ councilLeft + councilFem + parlFem + councilChange + turnout +
               popDensityHun + hhHousingSup + cathShare +
                 GDPpc + debtpcHun, data=dfddProgNoSt)
summary (reg123p.b)
reg1p.b <- lm(addSupGRANTamountHH2 ~ councilLeft + councilFem + parlFem + councilChange + turnout
              , data=dfddProgNoSt)
summary (reg1p.b)
reg3p.b <- lm(addSupGRANTamountHH2 ~ GDPpc + debtpcHun, data=dfddProgNoSt)
summary (reg3p.b)
reg2p.b <- lm(addSupGRANTamountHH2 ~ popDensityHun + hhHousingSup + cathShare, data=dfddProgNoSt)
summary (reg2p.b)


### tables etc. for latex
table(dfddProg$response)
table(dfddProg$addSup)
table(dfdd$addSup)
table(dfdd$addSup,dfdd$inProgram)
table(dfdd$progState,dfdd$inProgram)
table(dfdd$response,dfdd$inProgram)

table(dfdd$councilFem)
table(dfdd$councilLeft)
table(dfdd$councilChange)

# Variables
stargazer(dfddProg[c("addSupGRANTamountHH2")], 
          type = "latex", digits=1,flip = FALSE, omit.summary.stat = c("p25","p75"))
stargazer(dfdd[c("parlFem","turnout","cathShare","population",
                 "popDensity","unemployment","hhHousingSup","GDPpc","debtpc")], 
          type = "latex", digits=1,flip = FALSE, omit.summary.stat = c("p25","p75"))

### Regression output tables
# logit models
stargazer(log1p, log2p, log3p, log123p, type = "latex", 
          omit.stat=c("f", "ser"), align=TRUE,digits=1)
stargazer(log1.1, log1.3, log1.5, log1.7, log1.9, log1, type = "latex", 
          omit.stat=c("f", "ser"), align=TRUE,digits=1)
stargazer(log2.1, log2.3.1, log2.3.2, log2.5, log2.7, log2.a, log2.b, type = "latex", 
          omit.stat=c("f", "ser"), align=TRUE,digits=1)
stargazer(log3.1, log3.3, log3.a, type = "latex", 
          omit.stat=c("f", "ser"), align=TRUE,digits=1)
# set n = 83
stargazer(reg1p.b, reg2p.b, reg3p.b, reg123p.b, type = "latex", 
          omit.stat=c("f", "ser"), align=TRUE,digits=1)
# partial set n = 11
stargazer(reg1.a.2, reg2.b.2, reg3.a.2, type = "latex", 
          omit.stat=c("f", "ser"), align=TRUE,digits=1)
stargazer(reg1.2.2, reg1.4.2, reg1.6.2, reg1.8.2, reg1.10.2, reg1.a.2, type = "latex", 
          omit.stat=c("f", "ser"), align=TRUE,digits=1)
stargazer(reg2.2.2, reg2.4.2.1, reg2.4.2.2, reg2.6.2, reg2.8.2, reg2.a.2, reg2.b.2, type = "latex", 
          omit.stat=c("f", "ser"), align=TRUE,digits=1)
stargazer(reg3.2.2, reg3.4.2, reg3.a.2, type = "latex", 
          omit.stat=c("f", "ser"), align=TRUE,digits=1)

# exploratory maps
plot(dfds["inProgram"])
plot(dfds["addSup"])

plot(dfds["cathShare"])
plot(dfds["protShare"])
plot(dfds["hhHousingSup"])
plot(dfds["debtpc"])
plot(dfds["GDPpc"])

plot(dfds["councilFem"])
plot(dfds["parlFem"])
plot(dfds["councilChange"])
plot(dfds["councilLeft"])

plot(dfds["cathShare"], key.pos = 4, nbreaks = 10,border="white")

#ggplot() +
# geom_sf(data = dfds, aes(fill = inProgram)) +
#theme_bw() + 
#geom_polygon(aes(fill = Assault), color = "white")



ggplot(dfaggrCFRSum, aes(x=month, y=CFR, group=1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous()+ 
  scale_x_discrete(limits=dfaggrCFRSum$month)+
  labs(title="Average county-CFR per month",x="Month", y = "CFR")+
  theme_bw()
