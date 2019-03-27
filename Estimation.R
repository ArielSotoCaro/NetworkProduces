
GDP <- as.data.frame(gdp)

# .....................................
# TOMATO DATASET =====================
# .....................................

d_undir_T <- list()
for (k in 1:17){
    d <- as.data.frame(degree_Tomato_dir[[k]])
    colnames(d) <- "DC"
    d$States <- rownames(d)
    p <- data.frame(States=Tomato_edges_list[[k]]$Destino,
                    Price=Tomato_edges_list[[k]]$mPrice)
    
    auth_scores <- as.data.frame(auth_score_Tomato[[k]]$vector)
    colnames(auth_scores) <- "auth"
    auth_scores$States <- rownames(auth_scores)
    
    strengt <- as.data.frame(Strength_Tomato_dir[[k]])
    colnames(strengt) <- "strength"
    strengt$States <- rownames(strengt)
    # add mean price for each State of destination
    p <- p %>% group_by(States) %>% summarize(price = mean(Price))
    d <- inner_join(d,GDP[c(1,1+k)], by="States" )
    d <- inner_join(d,p, by="States" )
    d <- inner_join(d,auth_scores, by="States" )
    d <- inner_join(d,strengt, by="States" )
    colnames(d) <- c("DC","States","GDP","price","authority","strength")
    d$year <- rep(1997+k,nrow(d))
    d_undir_T[[k]] <- d
}

d_undir_T[[17]]

# Adding international Prices
for (i in 1:17){
  d_undir_T[[i]]$iPrice <- rep(Iprice_Tomato[i],nrow(d_undir_T[[i]]))
}

# Adding region
for (i in 1:17){
  d_undir_T[[i]] <- d_undir_T[[i]] %>% mutate(region = ifelse(States %in% NorOeste,"NW",
                                                ifelse(States %in% NorEste,"NE",
                                                       ifelse(States %in% Occidente,"W",
                                                              ifelse(States %in% Oriente,"E",
                                                                     ifelse(States %in% CentroNorte,"CN",
                                                                            ifelse(States %in% CentroSur,"CS",
                                                                                   ifelse(States %in% SurOeste,"SW",
                                                                                          ifelse(States %in% SurEste,"SE",0)))))))))
}


# Creating the complete dataset
dfT <- do.call(rbind.data.frame, d_undir_T)
dfT$produce <- 'tomato'
#################################################
##################################################



# .....................................
# AVOCADO DATASET =====================
# .....................................

d_undir_A <- list()
for (k in 1:17){
  d <- as.data.frame(degree_Avocado_dir[[k]])
  colnames(d) <- "DC"
  d$States <- rownames(d)
  p <- data.frame(States=Avocado_edges_list[[k]]$Destino,Price=Avocado_edges_list[[k]]$mPrice)
  
  auth_scores <- as.data.frame(auth_score_Avocado[[k]]$vector)
  colnames(auth_scores) <- "auth"
  auth_scores$States <- rownames(auth_scores)
  
  strengt <- as.data.frame(Strength_Avocado_dir[[k]])
  colnames(strengt) <- "strength"
  strengt$States <- rownames(strengt)
  
  # add mean price for each State of destination
  p <- p %>% group_by(States) %>% summarize(price = mean(Price))
  d <- inner_join(d,GDP[c(1,1+k)], by="States" )
  d <- inner_join(d,p, by="States" )
  d <- inner_join(d,auth_scores, by="States" )
  d <- inner_join(d,strengt, by="States" )
  colnames(d) <- c("DC","States","GDP","price","authority","strength")
  d$year <- rep(1997+k,nrow(d))
  d_undir_A[[k]] <- d
}

d_undir_A[[17]]

# Adding international Prices
for (i in 1:17){
  d_undir_A[[i]]$iPrice <- rep(Iprice_Avocado[i],nrow(d_undir_A[[i]]))
}

# Adding region
for (i in 1:17){
  d_undir_A[[i]] <- d_undir_A[[i]] %>% mutate(region = ifelse(States %in% NorOeste,"NW",
                                                              ifelse(States %in% NorEste,"NE",
                                                                     ifelse(States %in% Occidente,"W",
                                                                            ifelse(States %in% Oriente,"E",
                                                                                   ifelse(States %in% CentroNorte,"CN",
                                                                                          ifelse(States %in% CentroSur,"CS",
                                                                                                 ifelse(States %in% SurOeste,"SW",
                                                                                                        ifelse(States %in% SurEste,"SE",0)))))))))
}


# Creating the complete dataset
dfA <- do.call(rbind.data.frame, d_undir_A)
dfA$produce <- 'avocado'
###############################################
###############################################

# .....................................
# CORN DATASET =====================
# .....................................

d_undir_C <- list()
for (k in 1:17){
  d <- as.data.frame(degree_Corn_dir[[k]])
  colnames(d) <- "DC"
  d$States <- rownames(d)
  p <- data.frame(States=Corn_edges_list[[k]]$Destino,Price=Corn_edges_list[[k]]$mPrice)
  
  auth_scores <- as.data.frame(auth_score_Corn[[k]]$vector)
  colnames(auth_scores) <- "auth"
  auth_scores$States <- rownames(auth_scores)
  
  strengt <- as.data.frame(Strength_Corn_dir[[k]])
  colnames(strengt) <- "strength"
  strengt$States <- rownames(strengt)
  # add mean price for each State of destination
  p <- p %>% group_by(States) %>% summarize(price = mean(Price))
  d <- inner_join(d,GDP[c(1,1+k)], by="States" )
  d <- inner_join(d,p, by="States" )
  d <- inner_join(d,auth_scores, by="States" )
  d <- inner_join(d,strengt, by="States" )
  colnames(d) <- c("DC","States","GDP","price","authority","strength")
  d$year <- rep(1997+k,nrow(d))
  d_undir_C[[k]] <- d
}

d_undir_C[[17]]

# Adding international Prices
for (i in 1:17){
  d_undir_C[[i]]$iPrice <- rep(Iprice_Corn[i],nrow(d_undir_C[[i]]))
}

# Adding region
for (i in 1:17){
  d_undir_C[[i]] <- d_undir_C[[i]] %>% mutate(region = ifelse(States %in% NorOeste,"NW",
                                                              ifelse(States %in% NorEste,"NE",
                                                                     ifelse(States %in% Occidente,"W",
                                                                            ifelse(States %in% Oriente,"E",
                                                                                   ifelse(States %in% CentroNorte,"CN",
                                                                                          ifelse(States %in% CentroSur,"CS",
                                                                                                 ifelse(States %in% SurOeste,"SW",
                                                                                                        ifelse(States %in% SurEste,"SE",0)))))))))
}



# Creating the complete dataset
dfC <- do.call(rbind.data.frame, d_undir_C)
dfC$produce <- 'corn'
####################################
####################################

# big dataset
dfTotal <- rbind(dfC,dfA)
dfTotal <- rbind(dfTotal,dfT)

# Setting the factor level of REGION
dfTotal$region <- factor(dfTotal$region)
dfTotal$region <- relevel(dfTotal$region, ref ='NW')

# r1 <- DC ~ GDP + price
# r2 <- math~female + as.numeric(ses) + science

# https://www.princeton.edu/~otorres/Panel101R.pdf
library(foreign)
coplot(DC ~ year|States, type="l", data=df)        # Lines
coplot(authority ~ year|States, type="b", data=df)        # points and lines

library(gplots)
plotmeans(DC ~ States, main="Heterogeineity across States", data=df)
plotmeans(DC ~ year, main="Heterogeineity across years", data=df)

# Regular OLS regression does not consider 
# heterogeneity across groups or time

summary(ols<- lm(DC ~ GDP + price, data=df))
plot(df$GDP, df$DC, pch=19, xlab="x1", ylab="y")
abline(lm(df$DC~df$GDP),lwd=3, col="red")

summary(fixed.dum.a <-lm(authority ~ I(GDP/10000) + log(price) + log(iPrice) + region, data=dfTotal))
summary(fixed.dum.s <-lm(strength ~ I(GDP/10000) + log(price) + log(iPrice) + region, data=dfTotal))

library(texreg)
texreg(list(fixed.dum.a,fixed.dum.s))
stargazer(fixed.dum.a,fixed.dum.s, type='text')

library(plm)
# Tomato
dfT. <-  dfT[!duplicated(dfT[c("States","year")]),]
summary(t.fixed.a <-plm(authority ~ GDP + price + iPrice + region, data=dfT.,
                      index=c("States","year"), model="within"))
summary(t.fixed <-plm(DC ~ GDP + price + iPrice + region, data=dfT.,
                    index=c("States","year"), model="within"))
summary(t.fixed.s <-plm(strength ~ GDP + price + iPrice + region, data=dfT.,
                      index=c("States","year"), model="within"))

stargazer(t.fixed.a,t.fixed.s, type='text')

# Avocado
dfA. <-  dfA[!duplicated(dfA[c("States","year")]),]
summary(a.fixed.a <-plm(authority ~ GDP + price + iPrice + region, data=dfA.,
                      index=c("States","year"), model="within"))
summary(a.fixed <-plm(DC ~ GDP + price + iPrice + region, data=dfA.,
                    index=c("States","year"), model="within"))
summary(a.fixed.s <-plm(strength ~ GDP + price + iPrice + region, data=dfA.,
                      index=c("States","year"), model="within"))

stargazer(a.fixed.a,a.fixed.s, type='text')

# Corn
dfC. <-  dfC[!duplicated(dfC[c("States","year")]),]
summary(c.fixed.a <-plm(authority ~ GDP + price + iPrice + region, data=dfC.,
                        index=c("States","year"), model="within"))
summary(c.fixed <-plm(DC ~ GDP + price + iPrice + region, data=dfC.,
                      index=c("States","year"), model="within"))
summary(c.fixed.s <-plm(strength ~ GDP + price + iPrice + region, data=dfC.,
                        index=c("States","year"), model="within"))
stargazer(c.fixed.a,c.fixed.s, type='text')

fixef(t.fixed.s)    # Display the fixed effects (constants for each State)
pFtest(t.fixed.a, fixed.dum.a)    # Testing for fixed effects, null: OLS better than fixed
pFtest(t.fixed.s, fixed.dum.s)
pFtest(a.fixed.a, fixed.dum.a)
pFtest(a.fixed.s, fixed.dum.s)
pFtest(c.fixed.a, fixed.dum.a) # ??
pFtest(c.fixed.s, fixed.dum.s)
# if the p-value is < 0.05 then the fixed effects model is a better choice

# RANDOM EFFECTS
# TOMATO
# summary(random <-plm(DC ~ GDP + price + iPrice + region, data=dfT.,
# index=c("States", "year"), model="random"))
summary(t.random.a <-plm(authority ~ GDP + price + iPrice + region, data=dfT.,
                         index=c("States", "year"), model="random"))
summary(t.random.s <-plm(strength ~ GDP + price + iPrice + region, data=dfT.,
                         index=c("States", "year"), model="random"))
# AVOCADO
# summary(random <-plm(DC ~ GDP + price + iPrice + region, data=dfA.,
# index=c("States", "year"), model="random"))
summary(a.random.a <-plm(authority ~ GDP + price + iPrice + region, data=dfA.,
                         index=c("States", "year"), model="random"))
summary(a.random.s <-plm(strength ~ GDP + price + iPrice + region, data=dfA.,
                         index=c("States", "year"), model="random"))

# CORN
# summary(random <-plm(DC ~ GDP + price + iPrice + region, data=dfC.,
#                      index=c("States", "year"), model="random"))
summary(c.random.a <-plm(authority ~ GDP + price + iPrice + region, data=dfC.,
                     index=c("States", "year"), model="random"))
summary(c.random.s <-plm(strength ~ GDP + price + iPrice + region, data=dfC.,
                     index=c("States", "year"), model="random"))

# FIXED OR RANDOM?
phtest(t.fixed.a, t.random.a)
phtest(t.fixed.s, t.random.s)

phtest(a.fixed.a, a.random.a)
phtest(a.fixed.s, a.random.s)

phtest(c.fixed.a, c.random.a)
phtest(c.fixed.s, c.random.s)
# if this number is < 0.05 then use fixed effects

# Testing for time-fixed effects
summary(fixed.time <- plm(DC ~ GDP + price + factor(year), data=df, index=c("States", "year"), model="within"))
summary(fixed.time <- plm(authority ~ GDP + price + factor(year), data=dfT., index=c("States", "year"), model="within"))

# Testing time-fixed effects. The null is that no time-fixed effects needed
pFtest(fixed.time, fixed)
# If this number is < 0.05 then use time-fixed effects. 
# In this example, we need to use time-fixed effects

# Testing for random effects: Breusch-Pagan Lagrange multiplier (LM)
# Regular OLS (pooling model) using plm
summary(pool <-plm(DC ~ GDP + price, data=df, index=c("States","year"), model="pooling"))
summary(pool <-plm(authority ~ GDP + price, data=df, index=c("States","year"), model="pooling"))

# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better)
plmtest(pool, type=c("bp"))
# If p-value is >0.05 we failed to reject the null and conclude that random effects
# is not appropriate.  This is, no evidence of significant differences
# across countries, therefore you can run a simple OLS regression
# if p<0.05 is better use random effects model.


# Testing for cross-sectional dependence/contemporaneous correlation:
# using Breusch-Pagan LM test of independence and Pasaran CD test
pcdtest(fixed, test = c("lm"))
pcdtest(fixed, test = c("cd"))

pcdtest(random, test = c("lm"))
pcdtest(random, test = c("cd"))

pcdtest(fixed.time, test = c("lm"))
pcdtest(fixed.time, test = c("cd"))
# if p-value <0.05 there is cross-sectional dependence (bad)

# Testing for serial correlation
pbgtest(t.fixed.a)
pbgtest(t.fixed.s)
pbgtest(a.fixed.a)
pbgtest(a.fixed.s)
pbgtest(c.fixed.a)
pbgtest(c.fixed.s)

pbgtest(t.random.a)
pbgtest(t.random.s)
pbgtest(a.random.a)
pbgtest(a.random.s)
pbgtest(c.random.a)
pbgtest(c.random.s)

pbgtest(fixed.time)
# No serial correlation if p>0.05

# Testing for heteroskedasticity 
library(lmtest)
bptest(DC ~ GDP + price + factor(States), data = df, studentize=F)
bptest(authority ~ GDP + price + iPrice + region, data = dfT., studentize=F)
bptest(strength ~ GDP + price + iPrice + region, data = dfT., studentize=F)

bptest(authority ~ GDP + price + iPrice + region, data = dfA., studentize=F)
bptest(strength ~ GDP + price + iPrice + region, data = dfA., studentize=F)

bptest(authority ~ GDP + price + iPrice + region, data = dfC., studentize=F)
bptest(strength ~ GDP + price + iPrice + region, data = dfC., studentize=F)
# if p<0.05 presence of Heteroskedasticity

# Controlling for heteroskedasticity: Robust covariance matrix estimation (Sandwich estimator)

# Controlling for heteroskedasticity: Random effects
coeftest(random)       # Original coefficients
coeftest(random, vcov=vcovHC)       # Heteroskedasticity consistent coefficients
coeftest(random, vcovHC(random, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3

# Controlling for heteroskedasticity: Fixed effects
coeftest(fixed)      # Original coefficients
coeftest(fixed, vcovHC) # Heteroskedasticityconsistent coefficients
coeftest(fixed, vcovHC(fixed, method = "arellano")) # Heteroskedasticity consistent coefficients (Arellano)
coeftest(fixed, vcovHC(fixed, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3




# SYSTEM OF EQU
library(systemfit)
library(plyr)


aNode_tomato <-dfT$authority  ~ dfT$GDP + dfT$price + dfT$iPrice + as.factor(dfT$region)
aNode_avocado <-dfA$authority  ~ dfA$GDP + dfA$price + dfA$iPrice + as.factor(dfA$region)
aNode_corn <-dfC$authority  ~ dfC$GDP + dfC$price + dfC$iPrice + as.factor(dfC$region)

sNode_tomato <-dfT$strength  ~ dfT$GDP + dfT$price + dfT$iPrice + as.factor(dfT$region)
sNode_avocado <-dfA$strength  ~ dfA$GDP + dfA$price + dfA$iPrice + as.factor(dfA$region)
sNode_corn <-dfC$strength  ~ dfC$GDP + dfC$price + dfC$iPrice + as.factor(dfC$region)

# the data set are not equal wrt # of rows. Whit this I'm adding rows with zeros
temp <- as.data.frame(matrix(0,24,10))
names(temp) <- names(dfT)
temp$States <- as.character(temp$States)
temp$region <- as.character(temp$region)
temp$produce <- as.character(temp$produce)
dfT <- bind_rows(dfT, temp)

temp <- as.data.frame(matrix(0,28,10))
names(temp) <- names(dfT)
temp$States <- as.character(temp$States)
temp$region <- as.character(temp$region)
temp$produce <- as.character(temp$produce)
dfA <- bind_rows(dfA, temp)

nrow(dfT)
nrow(dfA)
nrow(dfC)


sys_a <- list(aNode_tomato,aNode_avocado,aNode_corn)
sys_s <- list(sNode_tomato,sNode_avocado,sNode_corn)

mex.sys.a <- systemfit(sys_a, method="SUR")
mex.sys.s <- systemfit(sys_s, method="SUR")

summary(mex.sys.a)
summary(mex.sys.s)
library(stargazer)
stargazer(coef(mex.sys.a),coef(mex.sys.s),align=TRUE,type = 'text',flip = TRUE)
library(texreg)
texreg(list(mex.sys.a,mex.sys.s))
