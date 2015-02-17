library(languageR)

library(lme4)

#install.packages("lme4", type="both", repos=c("http://lme4.r-forge.r-project.org/repos",                        getOption("repos")[["CRAN"]]))

library(lmerTest)

drop.levels <- function(dat){
  dat[] <- lapply(dat, function(x) x[,drop=TRUE])
  return(dat)
}

se <- function(x) {
  sd(x)/sqrt(length(x))
}



d<-read.table("errordata.txt", header=T)

#  subj subexpt semint related n2num errnd errd errcornd errcord list

d$subj<-as.factor(d$subj)

d.dys<-d[,c("subj","semint","related","n2num","errd","errcord")]
d.dys$subj<-as.factor(d.dys$subj)
d.dys$corr<-d.dys$errcord-d.dys$errd
d.dys$tot<-d.dys$errcord
d.dys$elog<-log((d.dys$errd + .5)/(d.dys$corr+.5))
d.dys$rates<-ifelse(d.dys$errd==0&d.dys$corr==0,0,(d.dys$errd/d.dys$tot)*100)
d.dys$v<-(1/(d.dys$errd+.5)) + (1/(d.dys$corr + .5))
d.dys$asin<-asin(sqrt(d.dys$rates/100))

integ<-subset(d.dys, semint=="integ")
integ<-drop.levels(integ)

unint<-subset(d.dys, semint=="unint")
unint<-drop.levels(unint)

rel<-subset(d.dys, related=="rel")
rel<-drop.levels(rel)

unrel<-subset(d.dys, related=="unrel")
unrel<-drop.levels(unrel)

plur<-subset(d.dys, n2num=="plur")
plur<-drop.levels(plur)

sing<-subset(d.dys, n2num=="sing")
sing<-drop.levels(sing)

integ.rel<-subset(integ, related=="rel")
integ.rel<-drop.levels(integ.rel)

integ.unrel<-subset(integ, related=="unrel")
integ.unrel<-drop.levels(integ.unrel)

integ.plur<-subset(integ, n2num=="plur")
integ.plur<-drop.levels(integ.plur)

integ.sing<-subset(integ, n2num=="sing")
integ.sing<-drop.levels(integ.sing)

unint.rel<-subset(unint, related=="rel")
unint.rel<-drop.levels(unint.rel)

unint.unrel<-subset(unint, related=="unrel")
unint.unrel<-drop.levels(unint.unrel)

unint.plur<-subset(unint, n2num=="plur")
unint.plur<-drop.levels(unint.plur)

unint.sing<-subset(unint, n2num=="sing")
unint.sing<-drop.levels(unint.sing)

rel.plur<-subset(rel, n2num=="plur")
rel.plur<-drop.levels(rel.plur)

rel.sing<-subset(rel, n2num=="sing")
rel.sing<-drop.levels(rel.sing)

unrel.plur<-subset(unrel, n2num=="plur")
unrel.plur<-drop.levels(unrel.plur)

unrel.sing<-subset(unrel, n2num=="sing")
unrel.sing<-drop.levels(unrel.sing)

integ.rel.plur<-subset(d.dys, semint=="integ" & related=="rel" & n2num=="plur")
integ.rel.plur<-drop.levels(integ.rel.plur)

integ.rel.sing<-subset(d.dys, semint=="integ" & related=="rel" & n2num=="sing")
integ.rel.sing<-drop.levels(integ.rel.sing)

integ.unrel.plur<-subset(d.dys, semint=="integ" & related=="unrel" & n2num=="plur")
integ.unrel.plur<-drop.levels(integ.unrel.plur)

integ.unrel.sing<-subset(d.dys, semint=="integ" & related=="unrel" & n2num=="sing")
integ.unrel.sing<-drop.levels(integ.unrel.sing)

unint.rel.plur<-subset(d.dys, semint=="unint" & related=="rel" & n2num=="plur")
unint.rel.plur<-drop.levels(unint.rel.plur)

unint.rel.sing<-subset(d.dys, semint=="unint" & related=="rel" & n2num=="sing")
unint.rel.sing<-drop.levels(unint.rel.sing)

unint.unrel.plur<-subset(d.dys, semint=="unint" & related=="unrel" & n2num=="plur")
unint.unrel.plur<-drop.levels(unint.unrel.plur)

unint.unrel.sing<-subset(d.dys, semint=="unint" & related=="unrel" & n2num=="sing")
unint.unrel.sing<-drop.levels(unint.unrel.sing)


contrasts(d.dys$semint)<-cbind("integ"=c(1,-1))
contrasts(d.dys$rel)<-cbind("unrel"=c(-1,1))
contrasts(d.dys$n2num)<-cbind("plur"=c(-1,1))

elogr.subj.dys <- lmer(elog ~ semint*related*n2num + (1|subj), data = d.dys, weights = (1/v))

print(summary(elogr.subj.dys))
print(anova(elogr.subj.dys))
print(step(elogr.subj.dys))
#p.dys<-pvals.fnc(elogr.subj.dys, nsim=10000)
#print(p.dys)

d.nodys<-d[,c("subj","semint","related","n2num","errnd","errcornd")]
d.nodys$subj<-as.factor(d.nodys$subj)
d.nodys$corr<-d.nodys$errcornd-d.nodys$errnd
d.nodys$tot<-d.nodys$errcornd
d.nodys$elog<-log((d.nodys$errnd + .5)/(d.nodys$corr+.5))
d.nodys$rates<-ifelse(d.nodys$errnd==0&d.nodys$corr==0,0,(d.nodys$errnd/d.nodys$tot)*100)
d.nodys$v<-(1/(d.nodys$errnd+.5)) + (1/(d.nodys$corr + .5))
d.nodys$asin<-asin(sqrt(d.nodys$rates/100))


contrasts(d.nodys$semint)<-cbind("integ"=c(1,-1))
contrasts(d.nodys$related)<-cbind("unrel"=c(-1,1))
contrasts(d.nodys$n2num)<-cbind("plur"=c(-1,1))

elogr.subj.nodys <- lmer(elog ~ semint*related*n2num + (1|subj), data = d.nodys, weights = (1/v))

print(summary(elogr.subj.nodys))
print(anova(elogr.subj.nodys))
print(step(elogr.subj.nodys))
#p.nodys<-pvals.fnc(elogr.subj.nodys, nsim=10000)
#print(p.nodys)

elogr.subj.dys.SemRelIntxn <- lmer(elog ~ semint*related + (1|subj), data = d.dys, weights = (1/v))
print(summary(elogr.subj.dys.SemRelIntxn))
#print(anova(elogr.subj.dys.SemRelIntxn))
#print(step(elogr.subj.dys.SemRelIntxn))

elogr.subj.dys.RelCases<-lmer(elog ~ semint*n2num + (1|subj), data = rel, weights = (1/v))
print(summary(elogr.subj.dys.RelCases))
#print(anova(elogr.subj.dys.RelCases))
#print(step(elogr.subj.dys.RelCases))
#p.values.lmer(elogr.subj.dys.RelCases)

elogr.subj.dys.UnrelCases<-lmer(elog ~ semint*n2num + (1|subj), data = unrel, weights = (1/v))
print(summary(elogr.subj.dys.UnrelCases))
#print(anova(elogr.subj.dys.UnrelCases))
#print(step(elogr.subj.dys.UnrelCases))
#p.values.lmer(elogr.subj.dys.UnrelCases)


elogr.subj.dys.IntegCases<-lmer(elog ~ related*n2num + (1|subj), data = integ, weights = (1/v))
print(summary(elogr.subj.dys.IntegCases))
#print(anova(elogr.subj.dys.IntegCases))
#print(step(elogr.subj.dys.IntegCases))
#p.values.lmer(elogr.subj.dys.IntegCases)

elogr.subj.dys.UnintCases<-lmer(elog ~ related*n2num + (1|subj), data = unint, weights = (1/v))
print(summary(elogr.subj.dys.UnintCases))
#print(anova(elogr.subj.dys.UnintCases))
#print(step(elogr.subj.dys.UnintCases))
#p.values.lmer(elogr.subj.dys.UnintCases)



elogr.subj.dys.IntegRelCases<-lmer(elog ~ n2num + (1|subj), data = integ.rel, weights = (1/v))
print(summary(elogr.subj.dys.IntegRelCases))
#print(anova(elogr.subj.dys.IntegRelCases))
#print(step(elogr.subj.dys.IntegRelCases))
#p.values.lmer(elogr.subj.dys.IntegRelCases)

elogr.subj.dys.IntegUnrelCases<-lmer(elog ~ n2num + (1|subj), data = integ.unrel, weights = (1/v))
print(summary(elogr.subj.dys.IntegUnrelCases))
print(anova(elogr.subj.dys.IntegUnrelCases))
print(step(elogr.subj.dys.IntegUnrelCases))
#p.values.lmer(elogr.subj.dys.IntegUnrelCases)

elogr.subj.dys.UnintRelCases<-lmer(elog ~ n2num + (1|subj), data = unint.rel, weights = (1/v))
print(summary(elogr.subj.dys.UnintRelCases))
#print(anova(elogr.subj.dys.UnintRelCases))
#print(step(elogr.subj.dys.UnintRelCases))

#p.values.lmer(elogr.subj.dys.UnintRelCases)

elogr.subj.dys.UnintUnrelCases<-lmer(elog ~ n2num + (1|subj), data = unint.unrel, weights = (1/v))
print(summary(elogr.subj.dys.UnintUnrelCases))
#print(anova(elogr.subj.dys.UnintUnrelCases))
#print(step(elogr.subj.dys.UnintUnrelCases))
#p.values.lmer(elogr.subj.dys.UnintUnrelCases)


elogr.subj.dys.IntegSameCases<-lmer(elog ~ related + (1|subj), data = integ.plur, weights = (1/v))
print(summary(elogr.subj.dys.UnintUnrelCases))
#print(anova(elogr.subj.dys.UnintUnrelCases))
#print(step(elogr.subj.dys.UnintUnrelCases))

#p.values.lmer(elogr.subj.dys.IntegSameCases)

elogr.subj.dys.IntegDiffCases<-lmer(elog ~ related + (1|subj), data = integ.sing, weights = (1/v))
print(summary(elogr.subj.dys.IntegDiffCases))
#print(anova(elogr.subj.dys.IntegDiffCases))
#print(step(elogr.subj.dys.IntegDiffCases))
#p.values.lmer(elogr.subj.dys.IntegDiffCases)

elogr.subj.dys.UnintSameCases<-lmer(elog ~ related + (1|subj), data = unint.plur, weights = (1/v))
print(summary(elogr.subj.dys.UnintSameCases))
#print(anova(elogr.subj.dys.UnintSameCases))
#print(step(elogr.subj.dys.UnintSameCases))
#p.values.lmer(elogr.subj.dys.UnintSameCases)

elogr.subj.dys.UnintDiffCases<-lmer(elog ~ related + (1|subj), data = unint.sing, weights = (1/v))
print(summary(elogr.subj.dys.UnintSameCases))
#print(anova(elogr.subj.dys.UnintSameCases))
#print(step(elogr.subj.dys.UnintSameCases))
#p.values.lmer(elogr.subj.dys.UnintDiffCases)



tapply(d.dys$rates, d.dys$semint:d.dys$related:d.dys$n2num, mean)