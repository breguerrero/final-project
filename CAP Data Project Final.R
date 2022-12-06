library(stats)
library(data.table)
library(magrittr)
library(tidyr)
library(tidyverse)
library(corrplot)
library(dplyr)
library(readxl)
library(ufs)
library(car)
library(GPArotation)
library(tibble)


#### LEGEND ####
#EXT = extroversion
#EST = 
#CSN = 
#ARG = argumentative
#OPN = openness

####Data Load####
library(readr)
data_final <- read_delim("Downloads/data-final.csv", 
                         delim = "\t", escape_double = FALSE, 
                         col_types = cols(`EXT1\tEXT2\tEXT3\tEXT4\tEXT5\tEXT6\tEXT7\tEXT8\tEXT9\tEXT10\tEST1\tEST2\tEST3\tEST4\tEST5\tEST6\tEST7\tEST8\tEST9\tEST10\tAGR1\tAGR2\tAGR3\tAGR4\tAGR5\tAGR6\tAGR7\tAGR8\tAGR9\tAGR10\tCSN1\tCSN2\tCSN3\tCSN4\tCSN5\tCSN6\tCSN7\tCSN8\tCSN9\tCSN10\tOPN1\tOPN2\tOPN3\tOPN4\tOPN5\tOPN6\tOPN7\tOPN8\tOPN9\tOPN10\tEXT1_E\tEXT2_E\tEXT3_E\tEXT4_E\tEXT5_E\tEXT6_E\tEXT7_E\tEXT8_E\tEXT9_E\tEXT10_E\tEST1_E\tEST2_E\tEST3_E\tEST4_E\tEST5_E\tEST6_E\tEST7_E\tEST8_E\tEST9_E\tEST10_E\tAGR1_E\tAGR2_E\tAGR3_E\tAGR4_E\tAGR5_E\tAGR6_E\tAGR7_E\tAGR8_E\tAGR9_E\tAGR10_E\tCSN1_E\tCSN2_E\tCSN3_E\tCSN4_E\tCSN5_E\tCSN6_E\tCSN7_E\tCSN8_E\tCSN9_E\tCSN10_E\tOPN1_E\tOPN2_E\tOPN3_E\tOPN4_E\tOPN5_E\tOPN6_E\tOPN7_E\tOPN8_E\tOPN9_E\tOPN10_E\tdateload\tscreenw\tscreenh\tintroelapse\ttestelapse\tendelapse\tIPC\tcountry\tlat_appx_lots_of_err\tlong_appx_lots_of_err` = col_number()), 
                         na = c("0"), trim_ws = TRUE)
View(data_final)

data_final <- data_final[,-c(101:103,109,110)]#remove screen and location data

data_final_full <- data_final[c(1:104)]
data_final_fullnum <- as.data.frame(sapply(data_final_full, as.numeric))
sapply(data_final_fullnum, class)

data_final_fullnum$country <- data_final[,c(105)]
data_final_fullnum$country <- as.character(data_final_fullnum$country)
data_final_full <- data_final_fullnum

##Impute
library(Amelia)
data_final_fullnum_I <- amelia(data_final_fullnum, m = 1, 
                     idvars = c(51:105),
                     ords = c(1:50))
data_final_fullnum_Impout <- data_final_fullnum_I$imputations[[1]]
data_final_I <- data_final_fullnum_Impout
sapply(data_final_I, class)
missmap(data_final_fullnum_Impout, vars = c(1:50))
table(data_final_I)

##Columns from Character to Numeric (subset first; df transformation after)
#For Survey Data

colsnum.data_ext <- data_final_I[c(1:10)] 
colsnum.data_est <- data_final_I[c(11:20)]
colsnum.data_arg <- data_final_I[c(21:30)]
colsnum.data_csn <- data_final_I[c(31:40)]
colsnum.data_opn <- data_final_I[c(41:50)]


colsnum.data_ext <- as.data.frame(sapply(colsnum.data_ext, as.numeric))
sapply(colsnum.data_ext, class)
colsnum.data_est <- as.data.frame(sapply(colsnum.data_est, as.numeric))
sapply(colsnum.data_est, class)
colsnum.data_arg <- as.data.frame(sapply(colsnum.data_arg, as.numeric))
sapply(colsnum.data_arg, class)
colsnum.data_csn <- as.data.frame(sapply(colsnum.data_csn, as.numeric))
sapply(colsnum.data_csn, class)
colsnum.data_opn <- as.data.frame(sapply(colsnum.data_opn, as.numeric))
sapply(colsnum.data_opn, class)

#For Time Taken on Each Item
colsnum.data_extRT <- data_final_I[c(51:60)]
colsnum.data_estRT <- data_final_I[c(61:70)]
colsnum.data_argRT <- data_final_I[c(71:80)]
colsnum.data_csnRT <- data_final_I[c(81:90)]
colsnum.data_opnRT <- data_final_I[c(91:100)]

colsnum.data_extRT <- as.data.frame(sapply(colsnum.data_extRT, as.numeric))
sapply(colsnum.data_extRT, class)
colsnum.data_estRT <- as.data.frame(sapply(colsnum.data_estRT, as.numeric))
sapply(colsnum.data_estRT, class)
colsnum.data_argRT <- as.data.frame(sapply(colsnum.data_argRT, as.numeric))
sapply(colsnum.data_argRT, class)
colsnum.data_csnRT <- as.data.frame(sapply(colsnum.data_csnRT, as.numeric))
sapply(colsnum.data_csnRT, class)
colsnum.data_opnRT <- as.data.frame(sapply(colsnum.data_opnRT, as.numeric))
sapply(colsnum.data_opnRT, class)

colMeans(as.data.frame(colsnum.data_arg))

####Data Clean####
library(psych)

describe(colsnum.data_arg) 
describe(colsnum.data_ext)
describe(colsnum.data_est)
describe(colsnum.data_opn)
describe(colsnum.data_csn)

arg_summary <- describe(colsnum.data_arg)
barplot(arg_summary$mean, names.arg = arg_summary$vars)

chisq.test(colsnum.data_arg$AGR1,colsnum.data_argRT$AGR1_E)
View(colsnum.data_arg)
EFAcolsnum.data_arg <- as.data.frame(as.character(colsnum.data_arg))
EFAcolsnum.data_arg <- as.data.frame(sapply(colsnum.data_arg, as.factor))

sapply(colsnum.data_arg, class)
View(EFAcolsnum.data_arg)
EFA_ARGitems <- factanal(colsnum.data_arg, factor = 1, rotation = "promax")
EFA_ARGitems

#demographic
country_table <- table(data_final_I$country)
country_table
table(data_final$dateload)
plot(country.data)

####Rename Columns####

levels(ScicommGrad_ColClean$Q12_eth)

ScicommGrad_ColClean$Q32_theoryscicomm [ScicommGrad_ColClean$Q32_theoryscicomm == "3"] <- "0"

####Labels####
ScicommGrad$Q8 <- factor(ScicommGrad$Q8, labels = c("Undergrad","Masters", "PhD", "Other")) 
ScicommGrad$Q9 <- factor(ScicommGrad$Q9, labels = c("Masters","PhD","Other","None"))

ScicommGrad$Q12 <- factor(ScicommGrad$Q12, labels = c("Hispanic", "White", 
                                                      "Black/AfrAmer", 
                                                      "Asian", 
                                                      "Multiracial", 
                                                      "Prefer NA"))

ScicommGrad$Q13 <- factor(ScicommGrad$Q13, labels = c("Woman","Man",
                                                      "Nonbinary", 
                                                      "Prefer NA"))

ScicommGrad$Q5.5 <- coalesce(ScicommGrad$Q5, ScicommGrad$Q6)
ScicommGrad <- ScicommGrad[,-c(4:5)]
ScicommGrad <- relocate(ScicommGrad, 'Q5.5', .after = 'Q4')

ScicommGrad <- ScicommGrad[,-c(24)] #remove Q30


####Check Structure####
#lapply(ScicommGrad.imp.out$Q7_school, unique)
#sapply(ScicommGrad.imp.out$Q7_school, levels)

dim(ScicommGrad.imp.out)
str(ScicommGrad.imp.out)
summary(ScicommGrad.imp.out)

#######Analysis#####

ScicommGrad.imp.out <- ScicommGrad.imp.out[ScicommGrad.imp.out$Q13_gen != 6,]
ScicommGrad.imp.out <- ScicommGrad.imp.out[ScicommGrad.imp.out$Q13_gen != 5,]
levels(ScicommGrad.imp.out$Q13_gen)

ScicommGrad.imp.out$Q13_gen <- factor(ScicommGrad.imp.out$Q13_gen, levels=c("2",'1'))

ScicommGrad.imp.out$Q33_blog <- factor(ScicommGrad.imp.out$Q33_blog, levels=c("0",'1'))
ScicommGrad.imp.out$Q33_scidemo <- factor(ScicommGrad.imp.out$Q33_scidemo, levels=c("0",'1'))
ScicommGrad.imp.out$Q33_socials <- factor(ScicommGrad.imp.out$Q33_socials, levels=c("0",'1'))
ScicommGrad.imp.out$Q33_voluninst <- factor(ScicommGrad.imp.out$Q33_voluninst, levels=c("0",'1'))
ScicommGrad.imp.out$Q33_k12 <- factor(ScicommGrad.imp.out$Q33_k12, levels=c("0",'1'))
ScicommGrad.imp.out$Q33_scitalk <- factor(ScicommGrad.imp.out$Q33_scitalk, levels=c("0",'1'))
ScicommGrad.imp.out$Q33_prodreview<- factor(ScicommGrad.imp.out$Q33_prodreview, levels=c("0",'1'))
ScicommGrad.imp.out$Q33_ta<- factor(ScicommGrad.imp.out$Q33_ta, levels=c("0",'1'))
ScicommGrad.imp.out$Q33_grade <- factor(ScicommGrad.imp.out$Q33_grade, levels=c("0",'1'))
ScicommGrad.imp.out$Q33_diss <- factor(ScicommGrad.imp.out$Q33_diss, levels=c("0",'1'))
ScicommGrad.imp.out$Q33_lobby <- factor(ScicommGrad.imp.out$Q33_lobby, levels=c("0",'1'))
ScicommGrad.imp.out$Q33_sciconf<- factor(ScicommGrad.imp.out$Q33_sciconf, levels=c("0",'1'))
ScicommGrad.imp.out$Q33_farmersmarket<- factor(ScicommGrad.imp.out$Q33_farmersmarket, levels=c("0",'1'))
ScicommGrad.imp.out$Q33_oped<- factor(ScicommGrad.imp.out$Q33_oped, levels=c("0",'1'))

ScicommGrad.imp.out <- ScicommGrad.imp.out[,-c(52)]

##Descriptive
#Demographic

summary(ScicommGrad.imp.out$Q2_yrsgrad)
barplot(prop.table(table(ScicommGrad.imp.out$Q2_yrsgrad)))

summary(ScicommGrad.imp.out$Q3_disc) 
barplot(prop.table(table(ScicommGrad.imp.out$Q3_disc)))

summary(ScicommGrad.imp.out$Q4_US) 
barplot(prop.table(table(ScicommGrad.imp.out$Q4_US)))

summary(ScicommGrad.imp.out$Q7_school)
barplot(prop.table(table(ScicommGrad.imp.out$Q7_school)))

summary(ScicommGrad.imp.out$Q9_edpur)
barplot(prop.table(table(ScicommGrad.imp.out$Q9_edpur)), 
        names.arg = c("Ph.D", "Masters", "Other"))

summary(ScicommGrad.imp.out$Q13_gen) #1 man, 2 woman, 5 nonbi, 6 prefer na
barplot(prop.table(table(ScicommGrad.imp.out$Q13_gen)), names.arg = c("Man","Woman"))

summary(ScicommGrad.imp.out$Q12_eth) 
barplot(prop.table(table(ScicommGrad.imp.out$Q12_eth)), 
        names.arg = c("Prefer NA","Hisp","White","AfriAmer","Asian","Multi"))

summary(ScicommGrad.imp.out$Q10_hisp) 
barplot(prop.table(table(ScicommGrad.imp.out$Q10_hisp)))

#Scicomm Experiences
summary(ScicommGrad.imp.out$Q15_ta) 
barplot(prop.table(table(ScicommGrad.imp.out$Q15_ta)))

summary(ScicommGrad.imp.out$Q19_traininst) 
barplot(prop.table(table(ScicommGrad.imp.out$Q19_traininst)))

summary(ScicommGrad.imp.out$Q20_donescicomm) 
barplot(prop.table(table(ScicommGrad.imp.out$Q20_donescicomm)))

table(ScicommGrad.imp.out$Q15_ta,ScicommGrad.imp.out$Q20_donescicomm)
barplot(prop.table(table(ScicommGrad.imp.out$Q15_ta,
                         ScicommGrad.imp.out$Q20_donescicomm)), 
        beside = TRUE, legend.text = TRUE) #legend(locator(1), title = "Done Scicomm")
chisq.test(ScicommGrad.imp.out$Q15_ta,ScicommGrad.imp.out$Q20_donescicomm)

table(ScicommGrad.imp.out$Q19_traininst,ScicommGrad.imp.out$Q20_donescicomm)
barplot(prop.table(table(ScicommGrad.imp.out$Q19_traininst,
                         ScicommGrad.imp.out$Q20_donescicomm)), beside = TRUE)
plot(table(ScicommGrad.imp.out$Q19_traininst, ScicommGrad.imp.out$Q20_donescicomm))
chisq.test(ScicommGrad.imp.out$Q19_traininst,ScicommGrad.imp.out$Q20_donescicomm)

table(ScicommGrad.imp.out$Q25_disscomm)
table(ScicommGrad.imp.out$Q20_donescicom,ScicommGrad.imp.out$Q25_disscomm)

table(ScicommGrad.imp.out$Q15_ta, ScicommGrad.imp.out$Q13_gen)
barplot(prop.table(table(ScicommGrad.imp.out$Q15_ta, 
                         ScicommGrad.imp.out$Q13_gen)), beside = TRUE)
chisq.test(ScicommGrad.imp.out$Q15_ta, ScicommGrad.imp.out$Q13_gen)
qchisq(p = 0.95, df = 1)


Q32_coreskills <- ScicommGrad.imp.out[,c(26:36)]
describeBy(Q32_coreskills, group = ScicommGrad.imp.out$Q15_ta)

Q33_whatisscicomm <- ScicommGrad.imp.out[,c(48:61)]
describeBy(Q33_whatisscicomm, group = ScicommGrad.imp.out$Q20_donescicomm)

# table(ScicommGrad$Q33_socials, ScicommGrad$Q32_theoryscicomm)
# chisq.test(scicommtable)
# 
# scicommtable1 <- table(ScicommGrad$Q33_lobby, ScicommGrad$Q33_sciconf)
# scicommtable1
# scicommtable1_chi <-chisq.test(scicommtable1)
# scicommtable1_chi$expected
# scicommtable1_chi$observed
# 
# scicommtable_train <- table(ScicommGrad$Q30, ScicommGrad$Q3)
# 
# coreskills <- ScicommGrad[c(17:27)]
# View(coreskills)
# describe(coreskills)
# describeBy(coreskills, group = ScicommGrad$Q13)
# 
# coreskills_lm <- lm(coreskills)
# summary(coreskills_lm)
# 
# scatterplot(coreskills_lm)

####Inferential####
#EFA Q18

Q18<- ScicommGrad.imp.out %>% select(13:20)
Q18

str(Q18)
Q18 <- as.data.frame(Q18)

describe(Q18)
alpha(Q18)

EFA_Q18_1 <- factanal(Q18, factor = 1, rotation = "promax")
EFA_Q18_1
EFA_Q18_2 <- factanal(Q18, factor = 2, rotation = "promax")
EFA_Q18_2
EFA_Q18_3<- factanal(Q18, factor = 3, rotation = "promax")
EFA_Q18_3

view(Q18)
Q18_Xothhelp <- Q18[,-c(5)]

EFA_Q18_Xothhelp1<- factanal(Q18_Xothhelp, factor = 2, rotation = "promax")
EFA_Q18_Xothhelp1

view(EFA_Q18_Xothhelp1)
Q18_X2col <- Q18_Xothhelp[,-c(4)]

EFA_Q18_X2col_1 <- factanal(Q18_X2col, factor = 2, rotation = "promax")
EFA_Q18_X2col_1

alpha(EFA_Q18_X2col_1)

####Grouping Factors of Q18####
Q18_X2col <- Q18_X2col[,-c(7)]
Q18_X2col$PerfComp_Means <- apply(Q18_X2col[1:3],1,mean)
Q18_X2col$Interest_Means <- apply(Q18_X2col[4:6],1,mean)

t.test(Q18_X2col$PerfComp_Means~ScicommGrad.imp.out$Q19_traininst)
t.test(Q18_X2col$Interest_Means~ScicommGrad.imp.out$Q19_traininst)

t.test(Q18_X2col$PerfComp_Means~ScicommGrad.imp.out$Q20_donescicomm)
t.test(Q18_X2col$Interest_Means~ScicommGrad.imp.out$Q20_donescicomm)

t.test(Q18_X2col$PerfComp_Means~ScicommGrad.imp.out$Q13_gen)
t.test(Q18_X2col$Interest_Means~ScicommGrad.imp.out$Q13_gen)

t.test(Q18_X2col$PerfComp_Means~ScicommGrad.imp.out$Q4_US)
t.test(Q18_X2col$Interest_Means~ScicommGrad.imp.out$Q4_US)

t.test(Q18_X2col$PerfComp_Means~ScicommGrad.imp.out$Q15_ta) #***
t.test(Q18_X2col$Interest_Means~ScicommGrad.imp.out$Q15_ta)

t.test(Q18_X2col$PerfComp_Means~ScicommGrad.imp.out$Q25_disscomm)
t.test(Q18_X2col$Interest_Means~ScicommGrad.imp.out$Q25_disscomm)

#Testing Reliability of Q18 Factors

####LCA of Q33####
library(poLCA)
str(ScicommGrad.imp.out)

####Reorder binary to 1/2#####
Q33_whatisscicomm$Q33_scidemo <- recode(Q33_whatisscicomm$Q33_scidemo, "0=1;1=2", as.factor = T)
Q33_whatisscicomm$Q33_blog <- recode(Q33_whatisscicomm$Q33_blog, "0=1;1=2", as.factor = T)
Q33_whatisscicomm$Q33_socials <- recode(Q33_whatisscicomm$Q33_socials, "0=1;1=2", as.factor = T)
Q33_whatisscicomm$Q33_voluninst <- recode(Q33_whatisscicomm$Q33_voluninst, "0=1;1=2", as.factor = T)
Q33_whatisscicomm$Q33_k12 <- recode(Q33_whatisscicomm$Q33_k12, "0=1;1=2", as.factor = T)
Q33_whatisscicomm$Q33_scitalk <- recode(Q33_whatisscicomm$Q33_scitalk, "0=1;1=2", as.factor = T)
Q33_whatisscicomm$Q33_prodreview <- recode(Q33_whatisscicomm$Q33_prodreview, "0=1;1=2", as.factor = T)
Q33_whatisscicomm$Q33_ta <- recode(Q33_whatisscicomm$Q33_ta, "0=1;1=2", as.factor = T)
Q33_whatisscicomm$Q33_grade <- recode(Q33_whatisscicomm$Q33_grade, "0=1;1=2", as.factor = T)
Q33_whatisscicomm$Q33_diss <- recode(Q33_whatisscicomm$Q33_diss, "0=1;1=2", as.factor = T)
Q33_whatisscicomm$Q33_lobby <- recode(Q33_whatisscicomm$Q33_lobby, "0=1;1=2", as.factor = T)
Q33_whatisscicomm$Q33_sciconf <- recode(Q33_whatisscicomm$Q33_sciconf, "0=1;1=2", as.factor = T)
Q33_whatisscicomm$Q33_farmersmarket <- recode(Q33_whatisscicomm$Q33_farmersmarket, "0=1;1=2", as.factor = T)
Q33_whatisscicomm$Q33_oped <- recode(Q33_whatisscicomm$Q33_oped, "0=1;1=2", as.factor = T)

####LCA Formula & Run ####
Q33_whatisscicomm_LCA <- as.formula(cbind(Q33_scidemo, Q33_blog, Q33_socials, 
                                                   Q33_voluninst, Q33_k12, Q33_scitalk, 
                                                   Q33_prodreview, Q33_ta, Q33_grade, 
                                                   Q33_diss, Q33_lobby, Q33_sciconf, 
                                                   Q33_farmersmarket, Q33_oped)~1)

Q33_LCA2class <-poLCA(Q33_whatisscicomm_LCA, Q33_whatisscicomm, nclass = 2, maxiter = 1000, nrep = 3)
Q33_LCA2class
plot(Q33_LCA2class)

poLCA(Q33_whatisscicomm_LCA, Q33_whatisscicomm, nclass = 3, maxiter = 1000, nrep = 3)
poLCA(Q33_whatisscicomm_LCA, Q33_whatisscicomm, nclass = 4, maxiter = 1000, nrep = 3)

poLCA(Q33_whatisscicomm_LCA, Q33_whatisscicomm, nclass = 3, graphs = TRUE)
poLCA(Q33_whatisscicomm_LCA, Q33_whatisscicomm, nclass = 4, graphs = TRUE)

####Comparative Analysis with LCA####
Q33_whatisscicomm$Q20_donescicomm <- ScicommGrad.imp.out$Q20_donescicomm
Q33_whatisscicomm$Q20_donescicomm <- recode(Q33_whatisscicomm$Q20_donescicomm, "0=1;1=2", as.factor = T)

Q33_whatisscicomm$Q15_ta <- ScicommGrad.imp.out$Q15_ta
Q33_whatisscicomm$Q15_ta <- recode(Q33_whatisscicomm$Q15_ta, "0=1;1=2", as.factor = T)

Q33_whatisscicomm_LCA_comp1 <- as.formula(cbind(Q33_scidemo, Q33_blog, Q33_socials, 
                                          Q33_voluninst, Q33_k12, Q33_scitalk, 
                                          Q33_prodreview, Q33_ta, Q33_grade, 
                                          Q33_diss, Q33_lobby, Q33_sciconf, 
                                          Q33_farmersmarket, Q33_oped)~Q20_donescicomm+Q15_ta)

Q33_LCA2class_comp1<-poLCA(Q33_whatisscicomm_LCA_comp1, Q33_whatisscicomm, nclass = 2, maxiter = 1000, nrep = 3) # if yes donecomm, more likely to be in class 2
#must convert donecomm coefficient to odds ratio (type in to google; interpreting outcomes of logistic reg)
odds<- exp(-1.50684) #1/5 ratio ---5x times more likely to ....
prob <- odds / (1 + odds)
prob #80% chance of being class 2
#Q33_LCA3class_comp1<-poLCA(Q33_whatisscicomm_LCA_comp1, Q33_whatisscicomm, nclass = 3, maxiter = 1000, nrep = 3)
Q33_LCA2class_comp1
plot(Q33_LCA2class_comp1)

poLCA(Q33_whatisscicomm_LCA, Q33_whatisscicomm, nclass = 3, maxiter = 1000, nrep = 3)


