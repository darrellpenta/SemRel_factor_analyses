rm(list=ls())
library(languageR) #calls languageR library
f1errout<-read.table("data/errordata.txt", header=T) #reads in all data from data file
d<-f1errout #renames data file
d$subj<-as.factor(d$subj) #designates "subject" as a factor
d$pct<-ifelse(d$errd==0&d$errcord==0,0,(d$errd/(d$errcord))*100) #Calculates the error rates (percent, including dys)
data.subj<-aggregate(d$pct, list(d$subj, d$semint, d$related, d$n2num ), mean) #aggregates d with dysfluencies by subject; DP, added n2number as a factor; from here throughout, changed "pref" to "related"

colnames(data.subj)<-c("subj","semint","related", "n2num", "error") #gives names to columns in data.subj

integ<-subset(data.subj,semint=="integ") #designates subset integ
unint<-subset(data.subj,semint=="unint") #designates subset unint
relat<-subset(data.subj,related=="rel") #designates subset rel
unrel<-subset(data.subj,related=="unrel") #designates subset unrel
sing<-subset(data.subj,n2num=="sing") #designates subset sing
plur<-subset(data.subj,n2num=="plur") #designates subset plur
relat.int.plur<-subset(data.subj, related=="rel" & semint=="integ" & n2num=="plur") #designates subset relat/int/plur
relat.int.sing<-subset(data.subj, related=="rel" & semint=="integ" & n2num=="sing") #designates subset relat/int/sing
relat.unint.plur<-subset(data.subj, related=="rel" & semint=="unint" & n2num=="plur") #designates subset relat/unint/plur
relat.unint.sing<-subset(data.subj, related=="rel" &  semint=="unint" & n2num=="sing") #designates subset relat/unint/sing
unrel.int.plur<-subset(data.subj, related=="unrel" & semint=="integ" & n2num=="plur") #designates subset unrel/int/plur
unrel.int.sing<-subset(data.subj, related=="unrel" & semint=="integ" & n2num=="sing") #designates subset unrel/int/sing
unrel.unint.plur<-subset(data.subj, related=="unrel" & semint=="unint" & n2num=="plur") #designates subset unrel/unint/plur
unrel.unint.sing<-subset(data.subj, related=="unrel" & semint=="unint" & n2num=="sing") #designates subset unrel/unint/sing
relat.plur<-subset(data.subj, related=="rel" & n2num=="plur") #designates subset relat/plur
relat.sing<-subset(data.subj, related=="rel" &  n2num=="sing") #designates subset relat/sing
unrel.plur<-subset(data.subj, related=="unrel" &  n2num=="plur") #designates subset unrelat/plur
unrel.sing<-subset(data.subj, related=="unrel" & n2num=="sing") #designates subset unrelat/sing
integ.plur<-subset(data.subj, semint=="integ" & n2num=="plur") #designates subset integ/plur
integ.sing<-subset(data.subj, semint=="integ" &  n2num=="sing") #designates subset integ/sing
unint.plur<-subset(data.subj, semint=="unint" &  n2num=="plur") #designates subset unint/plur
unint.sing<-subset(data.subj, semint=="unint" & n2num=="sing") #designates subset unint/sing
integ.relat<-subset(data.subj, semint=="integ" & related=="rel") #designates subset integ/rel
integ.unrel<-subset(data.subj, semint=="integ" & related=="unrel") #designates subset integ/unrel
unint.relat<-subset(data.subj, semint=="unint" & related=="rel") #designates subset unint/rel
unint.unrel<-subset(data.subj, semint=="unint" & related=="unrel") #designates subset unint/unrel

ds<-data.frame(data=c(
  "gmean",
  "integ",
  "unint",
  "relat",
  "unrel",
  "integrel",
  "integunrel",
  "unintrel",
  "unintunrel",
  "plur",
  "sing",
  "intplur",
  "intsing",
  "unintplur",
  "unintsing",
  "relplur",
  "relsing",
  "unrelplur",
  "unrelsing",
  "relintplur",
  "relintsing",
  "relunintplur",
  "relunintsing",
  "unrelintplur",
  "unrelintsing",
  "unrelunintplur",
  "unrelunintsing"),

  n=c(length(data.subj$error),
      length(integ$error),
      length(unint$error),
      length(relat$error),
      length(unrel$error),
      length(integ.relat$error),
      length(integ.unrel$error),
      length(unint.relat$error),
      length(unint.unrel$error),
      length(plur$error),
      length(sing$error),
      length(integ.plur$error),
      length(integ.sing$error),
      length(unint.plur$error),
      length(unint.sing$error),
      length(relat.plur$error),
      length(relat.sing$error),
      length(unrel.plur$error),
      length(unrel.sing$error),
      length(relat.int.plur$error),
      length(relat.int.sing$error),
      length(relat.unint.plur$error),
      length(relat.unint.sing$error),
      length(unrel.int.plur$error),
      length(unrel.int.sing$error),
      length(unrel.unint.plur$error),
      length(unrel.unint.sing$error)),

  N=c(length(data.subj$error),
      length(integ$error),
      length(unint$error),
      length(relat$error),
      length(unrel$error),
      length(integ.relat$error),
      length(integ.unrel$error),
      length(unint.relat$error),
      length(unint.unrel$error),
      length(plur$error),
      length(sing$error),
      length(integ.plur$error),
      length(integ.sing$error),
      length(unint.plur$error),
      length(unint.sing$error),
      length(relat.plur$error),
      length(relat.sing$error),
      length(unrel.plur$error),
      length(unrel.sing$error),
      length(relat.int.plur$error),
      length(relat.int.sing$error),
      length(relat.unint.plur$error),
      length(relat.unint.sing$error),
      length(unrel.int.plur$error),
      length(unrel.int.sing$error),
      length(unrel.unint.plur$error),
      length(unrel.unint.sing$error)),

  mean=c(mean(data.subj$error),
         mean(integ$error),
         mean(unint$error),
         mean(relat$error),
         mean(unrel$error),
         mean(integ.relat$error),
         mean(integ.unrel$error),
         mean(unint.relat$error),
         mean(unint.unrel$error),
         mean(plur$error),
         mean(sing$error),
         mean(integ.plur$error),
         mean(integ.sing$error),
         mean(unint.plur$error),
         mean(unint.sing$error),
         mean(relat.plur$error),
         mean(relat.sing$error),
         mean(unrel.plur$error),
         mean(unrel.sing$error),
         mean(relat.int.plur$error),
         mean(relat.int.sing$error),
         mean(relat.unint.plur$error),
         mean(relat.unint.sing$error),
         mean(unrel.int.plur$error),
         mean(unrel.int.sing$error),
         mean(unrel.unint.plur$error),
         mean(unrel.unint.sing$error)),

  sd=c(sd(data.subj$error),
       sd(integ$error),
       sd(unint$error),
       sd(relat$error),
       sd(unrel$error),
       sd(integ.relat$error),
       sd(integ.unrel$error),
       sd(unint.relat$error),
       sd(unint.unrel$error),
       sd(plur$error),
       sd(sing$error),
       sd(integ.plur$error),
       sd(integ.sing$error),
       sd(unint.plur$error),
       sd(unint.sing$error),
       sd(relat.plur$error),
       sd(relat.sing$error),
       sd(unrel.plur$error),
       sd(unrel.sing$error),
       sd(relat.int.plur$error),
       sd(relat.int.sing$error),
       sd(relat.unint.plur$error),
       sd(relat.unint.sing$error),
       sd(unrel.int.plur$error),
       sd(unrel.int.sing$error),
       sd(unrel.unint.plur$error),
       sd(unrel.unint.sing$error)),

  se=c(sd(data.subj$error)/sqrt(length(data.subj$error)),
       sd(integ$error)/sqrt(length(integ$error)),
       sd(unint$error)/sqrt(length(unint$error)),
       sd(relat$error)/sqrt(length(relat$error)),
       sd(unrel$error)/sqrt(length(unrel$error)),

       sd(integ.relat$error)/sqrt(length(integ.relat$error)),
       sd(integ.unrel$error)/sqrt(length(integ.unrel$error)),
       sd(unint.relat$error)/sqrt(length(unint.relat$error)),
       sd(unint.unrel$error)/sqrt(length(unint.unrel$error)),
       sd(plur$error)/sqrt(length(plur$error)),
       sd(sing$error)/sqrt(length(sing$error)),
       sd(integ.plur$error)/sqrt(length(integ.plur$error)),
       sd(integ.sing$error)/sqrt(length(integ.sing$error)),
       sd(unint.plur$error)/sqrt(length(unint.plur$error)),
       sd(unint.sing$error)/sqrt(length(unint.sing$error)),
       sd(relat.plur$error)/sqrt(length(relat.plur$error) ),
       sd(relat.sing$error)/sqrt(length(relat.sing$error)),
       sd(unrel.plur$error)/sqrt(length(unrel.plur$error)),
       sd(unrel.sing$error)/sqrt(length(unrel.sing$error)),
       sd(relat.int.plur$error)/sqrt(length(relat.int.plur$error)),
       sd(relat.int.sing$error)/sqrt(length(relat.int.sing$error)),
       sd(relat.unint.plur$error)/sqrt(length(relat.unint.plur$error)),
       sd(relat.unint.sing$error)/sqrt(length(relat.unint.sing$error)),
       sd(unrel.int.plur$error)/sqrt(length(unrel.int.plur$error)),
       sd(unrel.int.sing$error)/sqrt(length(unrel.int.sing$error)),
       sd(unrel.unint.plur$error)/sqrt(length(unrel.unint.plur$error)),
       sd(unrel.unint.sing$error)/sqrt(length(unrel.unint.sing$error))
  ))
sink("output/SemRel Factor Analyses.txt")
cat("__________________Descriptive Stats for 2X2X2 ANOVA", sep="", fill = 80)
print(ds) #prints descrip stats for 2x2x2 ANOVA
print(rep(c(" "),times=10), quote=F)
print(rep(c("-"),times=30), quote=F)
print(rep(c(" "),times=10), quote=F)
a.2x2x2<-aov(error~semint*related*n2num+Error(subj/(semint*related*n2num)),data=data.subj)# Performs 2x2x2 anova

cat("__________________2X2X2 ANOVA", sep="", fill=80)
print(summary(a.2x2x2)) #2x2x2 anova table
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c("="),times=40), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
###INTEGRATED ITEMS
ds.integ <- data.frame(data=c("integ","intrel","intunrel"),

                     n=c(length(integ$error),
                         length(integ.relat$error),
                         length(integ.unrel$error)),

                     N=c(length(integ$error),
                         length(integ.relat$error),
                         length(integ.unrel$error)),

                     mean=c(mean(integ$error),
                            mean(integ.relat$error),
                            mean(integ.unrel$error)),

                     sd=c(sd(integ$error),
                          sd(integ.relat$error),
                          sd(integ.unrel$error)),

                     se=c(sd(integ$error)/sqrt(length(integ$error)),
                          sd(integ.relat$error)/sqrt(length(integ.relat$error)),
                          sd(integ.unrel$error)/sqrt(length(integ.unrel$error)))) #sets up table of integ responses

cat("__________________Descriptive Stats for Integrated responses", sep="", fill=80)
print(ds.integ) #prints descrip stats
print(rep(c(" "),times=10), quote=F)
print(rep(c("-"),times=30), quote=F)
print(rep(c(" "),times=10), quote=F)
a.integ<-aov(error~related+Error(subj/related),data=integ) # comparison of integ responses
cat("__________________ANOVA: Integrated items", sep="", fill=80)
print(summary(a.integ)) #integ anova table
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c("="),times=40), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
ds.unint<-data.frame(data=c("unint","unintrel","unintunrel"),

                     n=c(length(unint$error),
                         length(unint.relat$error),
                         length(unint.unrel$error)),

                     N=c(length(unint$error),
                         length(unint.relat$error),
                         length(unint.unrel$error)),

                     mean=c(mean(unint$error),
                            mean(unint.relat$error),
                            mean(unint.unrel$error)),

                     sd=c(sd(unint$error),
                          sd(unint.relat$error),
                          sd(unint.unrel$error)),

                     se=c(sd(unint$error)/sqrt(length(unint$error)),
                          sd(unint.relat$error)/sqrt(length(unint.relat$error)),
                          sd(unint.unrel$error)/sqrt(length(unint.unrel$error)))) #sets up table of unint responses

cat("__________________Descriptive Stats for Unintegrated itesm ANOVA", sep="", fill=80)
print(ds.unint) #prints descrip stats

a.unint<-aov(error~related+Error(subj/related),data=unint) #paired comparison of unint responses
cat("__________________ANOVA: Unintegrated items", sep="", fill=80)
print(summary(a.unint)) #unint anova table
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c("="),times=40), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)

##########PAIRED COMPARISONS BELOW HERE
###Integration Paired Comparison
f1errout<-read.table("data/errint.txt", header=T) #reads in all data Integrated (ignoring Related) data file
d<-f1errout #as above
d$subj<-as.factor(d$subj) #designates "subject" as a factor
d$pct<-ifelse(d$errd==0&d$errcord==0,0,(d$errd/(d$errcord))*100) #as above
data.subj<-aggregate(d$pct, list(d$subj, d$semint, d$related, d$n2num ), mean) #as above
colnames(data.subj)<-c("subj","semint","related", "n2num", "error") #as above
integ<-subset(data.subj,semint=="integ") #designates subset integ
unint<-subset(data.subj,semint=="unint") #designates subset unint
sing<-subset(data.subj,n2num=="sing") #designates subset sing
plur<-subset(data.subj,n2num=="plur") #designates subset plur
integ.plur<-subset(data.subj, semint=="integ" & n2num=="plur") #designates subset integ/plur
integ.sing<-subset(data.subj, semint=="integ" &  n2num=="sing") #designates subset integ/sing
unint.plur<-subset(data.subj, semint=="unint" &  n2num=="plur") #designates subset unint/plur
unint.sing<-subset(data.subj, semint=="unint" & n2num=="sing") #designates subset unint/sing

ds<-data.frame(data=c(
  "gmean",
  "integ",
  "unint",
  "plur",
  "sing",
  "intplur",
  "intsing",
  "unintplur",
  "unintsing"
),

n=c(length(data.subj$error),
    length(integ$error),
    length(unint$error),
    length(plur$error),
    length(sing$error),
    length(integ.plur$error),
    length(integ.sing$error),
    length(unint.plur$error),
    length(unint.sing$error)
),

N=c(length(data.subj$error),
    length(integ$error),
    length(unint$error),
    length(plur$error),
    length(sing$error),
    length(integ.plur$error),
    length(integ.sing$error),
    length(unint.plur$error),
    length(unint.sing$error)
),

mean=c(mean(data.subj$error),
       mean(integ$error),
       mean(unint$error),
       mean(plur$error),
       mean(sing$error),
       mean(integ.plur$error),
       mean(integ.sing$error),
       mean(unint.plur$error),
       mean(unint.sing$error)
),

sd=c(sd(data.subj$error),
     sd(integ$error),
     sd(unint$error),
     sd(plur$error),
     sd(sing$error),
     sd(integ.plur$error),
     sd(integ.sing$error),
     sd(unint.plur$error),
     sd(unint.sing$error)
),

se=c(sd(data.subj$error)/sqrt(length(data.subj$error)),
     sd(integ$error)/sqrt(length(integ$error)),
     sd(unint$error)/sqrt(length(unint$error)),
     sd(plur$error)/sqrt(length(plur$error)),
     sd(sing$error)/sqrt(length(sing$error)),
     sd(integ.plur$error)/sqrt(length(integ.plur$error)),
     sd(integ.sing$error)/sqrt(length(integ.sing$error)),
     sd(unint.plur$error)/sqrt(length(unint.plur$error)),
     sd(unint.sing$error)/sqrt(length(unint.sing$error))

))
cat("__________________Descriptive Stats for ANOVA on Integrated items, ignoring Related", sep="",fill=80)
print(ds) #prints descrip stats
print(rep(c(" "),times=10), quote=F)
print(rep(c("-"),times=30), quote=F)
print(rep(c(" "),times=10), quote=F)
a.2x2<-aov(error~semint*n2num+Error(subj/(semint*n2num)),data=data.subj)#2x2x2 anova
cat("__________________ANOVA: Integrated items, ignoring Related", sep="",fill=80)
print(summary(a.2x2)) #2x2 anova table
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c("="),times=40), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
##Integrated Paired Comparision
ds.integ<-data.frame(data=c("n2num","plur","sing"),

                     n=c(length(integ$error),
                         length(integ.plur$error),
                         length(integ.sing$error)),

                     N=c(length(integ$error),
                         length(integ.plur$error),
                         length(integ.sing$error)),

                     mean=c(mean(integ$error),
                            mean(integ.plur$error),
                            mean(integ.sing$error)),

                     sd=c(sd(integ$error),
                          sd(integ.plur$error),
                          sd(integ.sing$error)),

                     se=c(sd(integ$error)/sqrt(length(integ$error)),
                          sd(integ.plur$error)/sqrt(length(integ.plur$error)),
                          sd(integ.sing$error)/sqrt(length(integ.sing$error)))) #sets up table of integ responses

cat("__________________Descriptive Stats for Integration Paired Comparison, ignoring Related", sep="",fill=80)
print(ds.integ) #prints descrip stats
print(rep(c(" "),times=10), quote=F)
print(rep(c("-"),times=30), quote=F)
print(rep(c(" "),times=10), quote=F)
a.integ<-aov(error~n2num+Error(subj/n2num),data=integ) #paired comparison of integ responses
cat("__________________ANOVA: Integrated Paired Comparison, ignoring Related", sep="",fill=80)
print(summary(a.integ)) #integ anova table
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c("="),times=40), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
##Unintegrated Paired Comparison
ds.uninteg<-data.frame(data=c("n2num","plur","sing"),

                       n=c(length(unint$error),
                           length(unint.plur$error),
                           length(unint.sing$error)),

                       N=c(length(unint$error),
                           length(unint.plur$error),
                           length(unint.sing$error)),

                       mean=c(mean(unint$error),
                              mean(unint.plur$error),
                              mean(unint.sing$error)),

                       sd=c(sd(unint$error),
                            sd(unint.plur$error),
                            sd(unint.sing$error)),

                       se=c(sd(unint$error)/sqrt(length(unint$error)),
                            sd(unint.plur$error)/sqrt(length(unint.plur$error)),
                            sd(unint.sing$error)/sqrt(length(unint.sing$error)))) #sets up table of uninteg responses

cat("__________________Descriptive Stats for Unintegrated Paired Comparison, ignoring Related", sep="",fill=80)
print(ds.uninteg) #prints descrip stats
print(rep(c(" "),times=10), quote=F)
print(rep(c("-"),times=30), quote=F)
print(rep(c(" "),times=10), quote=F)
a.uninteg<-aov(error~n2num+Error(subj/n2num),data=unint) #paired comparison of integ responses
cat("__________________ANOVA: Unintegrated Paired Comparison, ignoring Related", sep="",fill=80)
print(summary(a.uninteg)) #integ anova table

print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c("="),times=40), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
###############
### Relatedness paired comparison
f1errout<-read.table("data/errrel.txt", header=T) #reads in Related data, ignoring integration
d<-f1errout #as above
d$subj<-as.factor(d$subj) #as above
d$pct<-ifelse(d$errd==0&d$errcord==0,0,(d$errd/(d$errcord))*100) #as above
data.subj<-aggregate(d$pct, list(d$subj, d$semint, d$related, d$n2num ), mean) #as above
colnames(data.subj)<-c("subj","semint","related", "n2num", "error") #as above
relat<-subset(data.subj,related =="rel") #designates subset rel
unrel<-subset(data.subj,related =="unrel") #designates subset unrel
sing<-subset(data.subj,n2num=="sing") #designates subset sing
plur<-subset(data.subj,n2num=="plur") #designates subset plur
relat.plur<-subset(data.subj, related=="rel" & n2num=="plur") #designates subset relat/plur
relat.sing<-subset(data.subj, related=="rel" &  n2num=="sing") #designates subset relat/sing
unrel.plur<-subset(data.subj, related=="unrel" &  n2num=="plur") #designates subset unrelat/plur
unrel.sing<-subset(data.subj, related=="unrel" & n2num=="sing") #designates subset unrelat/sing
ds<-data.frame(data=c(
  "gmean",
  "relat",
  "unrel",
  "plur",
  "sing",
  "relatplur",
  "relatsing",
  "unrelplur",
  "unrelsing"
),

n=c(length(data.subj$error),
    length(relat$error),
    length(unrel$error),
    length(plur$error),
    length(sing$error),
    length(relat.plur$error),
    length(relat.sing$error),
    length(unrel.plur$error),
    length(unrel.sing$error)
),

N=c(length(data.subj$error),
    length(relat$error),
    length(unrel$error),
    length(plur$error),
    length(sing$error),
    length(relat.plur$error),
    length(relat.sing$error),
    length(unrel.plur$error),
    length(unrel.sing$error)
),

mean=c(mean(data.subj$error),
       mean(relat$error),
       mean(unrel$error),
       mean(plur$error),
       mean(sing$error),
       mean(relat.plur$error),
       mean(relat.sing$error),
       mean(unrel.plur$error),
       mean(unrel.sing$error)
),

sd=c(sd(data.subj$error),
     sd(relat$error),
     sd(unrel$error),
     sd(plur$error),
     sd(sing$error),
     sd(relat.plur$error),
     sd(relat.sing$error),
     sd(unrel.plur$error),
     sd(unrel.sing$error)
),

se=c(sd(data.subj$error)/sqrt(length(data.subj$error)),
     sd(relat$error)/sqrt(length(relat$error)),
     sd(unrel$error)/sqrt(length(unrel$error)),
     sd(plur$error)/sqrt(length(plur$error)),
     sd(sing$error)/sqrt(length(sing$error)),
     sd(relat.plur$error)/sqrt(length(relat.plur$error)),
     sd(relat.sing$error)/sqrt(length(relat.sing$error)),
     sd(unrel.plur$error)/sqrt(length(unrel.plur$error)),
     sd(unrel.sing$error)/sqrt(length(unrel.sing$error))

))

cat("__________________Descriptive Stats for ANOVA on Related items, ignoring Integration", sep="",fill=80)
print(ds) #prints descrip stats
print(rep(c(" "),times=10), quote=F)
print(rep(c("-"),times=30), quote=F)
print(rep(c(" "),times=10), quote=F)
a.2x2<-aov(error~related*n2num+Error(subj/(related*n2num)),data=data.subj)#2x2x2 anova
cat("__________________ANOVA: Related items, ignoring Integration", sep="",fill=80)
print(summary(a.2x2)) #2x2 anova table
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c("="),times=40), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
##Related Paired Comparison
ds.relat<-data.frame(data=c("n2num","plur","sing"),

                     n=c(length(relat$error),
                         length(relat.plur$error),
                         length(relat.sing$error)),

                     N=c(length(relat$error),
                         length(relat.plur$error),
                         length(relat.sing$error)),

                     mean=c(mean(relat$error),
                            mean(relat.plur$error),
                            mean(relat.sing$error)),

                     sd=c(sd(relat$error),
                          sd(relat.plur$error),
                          sd(relat.sing$error)),

                     se=c(sd(relat$error)/sqrt(length(relat$error)),
                          sd(relat.plur$error)/sqrt(length(relat.plur$error)),
                          sd(relat.sing$error)/sqrt(length(relat.sing$error)))) #sets up table of relat responses

cat("__________________Descriptive Stats for Paired Compairsion of Related items, ignoring Integration", sep="",fill=80)
print(ds.relat) #prints descrip stats
print(rep(c(" "),times=10), quote=F)
print(rep(c("-"),times=30), quote=F)
print(rep(c(" "),times=10), quote=F)
a.relat<-aov(error~n2num+Error(subj/n2num),data=relat) #paired comparison of relat responses
cat("__________________ANOVA: Paired Compairsion of Related items, ignoring Integration", sep="",fill=80)
print(summary(a.relat)) #relat anova table
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c("="),times=40), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
##Unrelated paired comparison
ds.unrel<-data.frame(data=c("n2num","plur","sing"),

                     n=c(length(unrel$error),
                         length(unrel.plur$error),
                         length(unrel.sing$error)),

                     N=c(length(unrel$error),
                         length(unrel.plur$error),
                         length(unrel.sing$error)),

                     mean=c(mean(unrel$error),
                            mean(unrel.plur$error),
                            mean(unrel.sing$error)),

                     sd=c(sd(unrel$error),
                          sd(unrel.plur$error),
                          sd(unrel.sing$error)),

                     se=c(sd(unrel$error)/sqrt(length(unrel$error)),
                          sd(unrel.plur$error)/sqrt(length(unrel.plur$error)),
                          sd(unrel.sing$error)/sqrt(length(unrel.sing$error)))) #sets up table of unrel responses

cat("__________________Descriptive Stats for Paired Compairsion of Unrelated items, ignoring Integration", sep="",rep=80)
print(ds.unrel) #prints descrip stats
print(rep(c(" "),times=10), quote=F)
print(rep(c("-"),times=30), quote=F)
print(rep(c(" "),times=10), quote=F)
a.unrel<-aov(error~n2num+Error(subj/n2num),data=unrel) #paired comparison of unrel responses
cat("__________________ANOVA: Paired Unelated items, ignoring Integration", sep="",fill=80)
print(summary(a.unrel)) #unrel anova table
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c("="),times=40), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
###Paired comparisions for each condition
f1errout<-read.table("data/errordata.txt", header=T) #as above
d<-f1errout #as above
d$subj<-as.factor(d$subj) #as above
d$pct<-ifelse(d$errd==0&d$errcord==0,0,(d$errd/(d$errcord))*100) #as above
data.subj<-aggregate(d$pct, list(d$subj, d$semint, d$related, d$n2num ), mean) #as above
colnames(data.subj)<-c("subj","semint","related", "n2num", "error") #as above

#Integrated Related paired#
integ.relat<-subset(data.subj, semint=="integ" & related=="rel") #designates subset integ/rel
relat.int.plur<-subset(data.subj, related=="rel" & semint=="integ" & n2num=="plur") #designates subset relat/int/plur
relat.int.sing<-subset(data.subj, related=="rel" & semint=="integ" & n2num=="sing") #designates subset relat/int/sing
ds.integrel<-data.frame(data=c("n2num","plur","sing"),

                        n=c(length(integ.relat$error),
                            length(relat.int.plur$error),
                            length(relat.int.sing$error)),

                        N=c(length(integ.relat$error),
                            length(relat.int.plur$error),
                            length(relat.int.sing$error)),

                        mean=c(mean(integ.relat$error),
                               mean(relat.int.plur$error),
                               mean(relat.int.sing$error)),

                        sd=c(sd(integ.relat$error),
                             sd(relat.int.plur$error),
                             sd(relat.int.sing$error)),

                        se=c(sd(integ.relat$error)/sqrt(length(integ.relat$error)),
                             sd(relat.int.plur$error)/sqrt(length(relat.int.plur$error)),
                             sd(relat.int.sing$error)/sqrt(length(relat.int.sing$error)))) #sets up table of integREl responses

cat("__________________Descriptive Stats for Paired Compairsion of Integrated Related items", sep="",fill=80)
print(ds.integrel) #prints descrip stats
print(rep(c(" "),times=10), quote=F)
print(rep(c("-"),times=30), quote=F)
print(rep(c(" "),times=10), quote=F)
a.integrel<-aov(error~n2num+Error(subj/n2num),data=integ.relat) #paired comparison of integRel responses
cat("ANOVA: Paired Compairsion of Integrated Related items", sep="", fill=80 )
print(summary(a.integrel)) #integRel anova table
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c("="),times=40), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
#Integrated Unrelated paired#
integ.unrel<-subset(data.subj, semint=="integ" & related=="unrel") #designates subset integ/unrel
unrel.int.plur<-subset(data.subj, related=="unrel" & semint=="integ" & n2num=="plur") #designates subset unrel/int/plur
unrel.int.sing<-subset(data.subj, related=="unrel" & semint=="integ" & n2num=="sing") #designates subset unrel/int/sing
ds.integunrel<-data.frame(data=c("n2num","plur","sing"),

                          n=c(length(integ.unrel$error),
                              length(unrel.int.plur$error),
                              length(unrel.int.sing$error)),

                          N=c(length(integ.unrel$error),
                              length(unrel.int.plur$error),
                              length(unrel.int.sing$error)),

                          mean=c(mean(integ.unrel$error),
                                 mean(unrel.int.plur$error),
                                 mean(unrel.int.sing$error)),

                          sd=c(sd(integ.unrel$error),
                               sd(unrel.int.plur$error),
                               sd(unrel.int.sing$error)),

                          se=c(sd(integ.unrel$error)/sqrt(length(integ.unrel$error)),
                               sd(unrel.int.plur$error)/sqrt(length(unrel.int.plur$error)),
                               sd(unrel.int.sing$error)/sqrt(length(unrel.int.sing$error)))) #sets up table of IntegUNrel responses

cat("__________________Descriptive Stats for Paired Compairsion of Integrated Unrelated items",sep="",fill=80)
print(ds.integunrel) #prints descrip stats Integ Unrel
print(rep(c(" "),times=10), quote=F)
print(rep(c("-"),times=30), quote=F)
print(rep(c(" "),times=10), quote=F)
a.integunrel<-aov(error~n2num+Error(subj/n2num),data=integ.unrel) #paired comparison of IntegUnrel responses
cat("__________________ANOVA:Paired Comparison of Integrated Unrelated items", sep="",fill=80)
print(summary(a.integunrel)) #integUnrel anova table
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c("="),times=40), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
#UnintRel#
unint.relat<-subset(data.subj, semint=="unint" & related=="rel") #designates subset unint/rel
relat.unint.plur<-subset(data.subj, related=="rel" & semint=="unint" & n2num=="plur") #designates subset relat/unint/plur
relat.unint.sing<-subset(data.subj, related=="rel" &  semint=="unint" & n2num=="sing") #designates subset relat/unint/sing

ds.unintrel<-data.frame(data=c("n2num","plur","sing"),

                        n=c(length(unint.relat$error),
                            length(relat.unint.plur$error),
                            length(relat.unint.sing$error)),

                        N=c(length(unint.relat$error),
                            length(relat.unint.plur$error),
                            length(relat.unint.sing$error)),

                        mean=c(mean(unint.relat$error),
                               mean(relat.unint.plur$error),
                               mean(relat.unint.sing$error)),

                        sd=c(sd(unint.relat$error),
                             sd(relat.unint.plur$error),
                             sd(relat.unint.sing$error)),

                        se=c(sd(unint.relat$error)/sqrt(length(unint.relat$error)),
                             sd(relat.unint.plur$error)/sqrt(length(relat.unint.plur$error)),
                             sd(relat.unint.sing$error)/sqrt(length(relat.unint.sing$error)))) #sets up table of unintrel responses

cat("__________________Descriptive Stats for Paired Compairsion of Unitegrated Related items", sep="",fill=80)
print(ds.unintrel) #prints descrip stats
print(rep(c(" "),times=10), quote=F)
print(rep(c("-"),times=30), quote=F)
print(rep(c(" "),times=10), quote=F)
a.unintrel<-aov(error~n2num+Error(subj/n2num),data=unint.relat) #paired comparison of unintrel responses
cat("__________________ANOVA: Paired Compairsion of Unitegrated Related items", sep="",fill=80)
print(summary(a.unintrel)) #unintunrel anova table
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c("="),times=40), quote=F)
print(rep(c(" "),times=10), quote=F)
print(rep(c(" "),times=10), quote=F)

#UnintUnrel#
unint.unrel<-subset(data.subj, semint=="unint" & related=="unrel") #designates subset unint/unrel
unrel.unint.plur<-subset(data.subj, related=="unrel" & semint=="unint" & n2num=="plur") #designates subset unrel/unint/plur
unrel.unint.sing<-subset(data.subj, related=="unrel" & semint=="unint" & n2num=="sing") #designates subset unrel/unint/sing
ds.unintunrel<-data.frame(data=c("n2num","plur","sing"),

                          n=c(length(unint.unrel$error),
                              length(unrel.unint.plur$error),
                              length(unrel.unint.sing$error)),

                          N=c(length(unint.unrel$error),
                              length(unrel.unint.plur$error),
                              length(unrel.unint.sing$error)),

                          mean=c(mean(unint.unrel$error),
                                 mean(unrel.unint.plur$error),
                                 mean(unrel.unint.sing$error)),

                          sd=c(sd(unint.unrel$error),
                               sd(unrel.unint.plur$error),
                               sd(unrel.unint.sing$error)),

                          se=c(sd(unint.unrel$error)/sqrt(length(unint.unrel$error)),
                               sd(unrel.unint.plur$error)/sqrt(length(unrel.unint.plur$error)),
                               sd(unrel.unint.sing$error)/sqrt(length(unrel.unint.sing$error)))) #sets up table of unintunrel responses

print("__________________Descriptive Stats for Paired Compairsion of Unitegrated Unrelated items", sep="",fill=80)
print(ds.unintunrel) #prints descrip stats
a.unintunrel<-aov(error~n2num+Error(subj/n2num),data=unint.unrel) #paired comparison of unintunrel responses
cat("__________________ANOVA: Paired Compairsion of Unitegrated Unrelated items", sep="",fill=80)
print(summary(a.unintunrel)) #unintunrel anova table
sink()
