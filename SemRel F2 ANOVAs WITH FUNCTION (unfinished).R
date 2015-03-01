rm(list = ls()) # clears environment
library(languageR) # calls languageR library
source(file = "StatRep.R")
#
# -----------------------------------PREPARE DATA FILE FOR ANALYSES---------------------------------
#
f2errout <- read.table("data/SR_F2_errordata.txt", header = T) # reads in all data from data file

d <- f2errout # renames data file

d$item <- as.factor(d$item) # designates "itemect" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)

#aggregates d with dysfluencies
data.item <- aggregate(d$pct, list(d$item, d$integ, d$related, d$n2num ), mean)

colnames(data.item) <- c("item", "semint", "related", "n2num", "error") # renames columns

# Below, designates various subsets of the original data file
data.list = list(
  gmean = data.item,
  integ = subset(data.item, semint   ==  "integ"), 
  unint = subset(data.item, semint   ==  "unint"),
  relat = subset(data.item, related  ==  "rel"), 
  unrel = subset(data.item, related  ==  "unrel"), 
  sing  = subset(data.item, n2num    ==  "sing"), 
  plur  = subset(data.item, n2num    ==  "plur"),
  relat.int.plur   = subset(data.item, related == "rel"   & semint  == "integ" & n2num == "plur"), 
  relat.int.sing   = subset(data.item, related == "rel"   & semint  == "integ" & n2num == "sing"),
  relat.unint.plur = subset(data.item, related == "rel"   & semint  == "unint" & n2num == "plur"), 
  relat.unint.sing = subset(data.item, related == "rel"   & semint  == "unint" & n2num == "sing"),
  unrel.int.plur   = subset(data.item, related == "unrel" & semint  == "integ" & n2num == "plur"),
  unrel.int.sing   = subset(data.item, related == "unrel" & semint  == "integ" & n2num == "sing"),
  unrel.unint.plur = subset(data.item, related == "unrel" & semint  == "unint" & n2num == "plur"),
  unrel.unint.sing = subset(data.item, related == "unrel" & semint  == "unint" & n2num == "sing"),
  relat.plur       = subset(data.item, related == "rel"   & n2num   == "plur"),
  relat.sing       = subset(data.item, related == "rel"   & n2num   == "sing"),
  unrel.plur       = subset(data.item, related == "unrel" & n2num   == "plur"),
  unrel.sing       = subset(data.item, related == "unrel" & n2num   == "sing"),
  integ.plur       = subset(data.item, semint  == "integ" & n2num   == "plur"),
  integ.sing       = subset(data.item, semint  == "integ" & n2num   == "sing"),
  unint.plur       = subset(data.item, semint  == "unint" & n2num   == "plur"),
  unint.sing       = subset(data.item, semint  == "unint" & n2num   == "sing"),
  integ.relat      = subset(data.item, semint  == "integ" & related == "rel"), 
  integ.unrel      = subset(data.item, semint  == "integ" & related == "unrel"),
  unint.relat      = subset(data.item, semint  == "unint" & related == "rel"), 
  unint.unrel      = subset(data.item, semint  == "unint" & related == "unrel") 
)

# ---------------------------------------------------------------------------------------------------
#
# --------------------------------2 X 2 X 2 ANOVA-----------------------------------------------------

sink("output/SemRel F2 Factorial Analyses_TO COMPARE.txt")

cat(" ", "\n")
cat("BY-ITEMS FACTORIAL ANALYSES RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")
cat(rep(c("-"), times=40, quote=F),"\n")
cat("2X2X2 ANOVA: SEMREL", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")

ds <- StatRep(c("gmean","integ","unint","relat","unrel","integ.relat","integ.unrel","unint.relat","unint.unrel", "sing","plur", "integ.plur","integ.sing","unint.plur","unint.sing","relat.plur","relat.sing","unrel.plur","unrel.sing","relat.int.plur","relat.int.sing","relat.unint.plur","relat.unint.sing","unrel.int.plur","unrel.int.sing","unrel.unint.plur","unrel.unint.sing"))


print(format(ds, trim = FALSE, nsmall = 4, scientific = FALSE)) # prints descrip stats for 2x2x2 ANOVA
cat(" ", "\n")

# Computes the anova
a.2x2x2 <- aov(error ~ semint * related * n2num + Error(item / (semint * related * n2num)), data = data.item)
print(summary(a.2x2x2))
cat(" ", "\n")
cat(" ", "\n")

#
#========================================================================================================
#
#------------------------------------RELATED - UNRELATED ITEMS PAIRED COMPARISONS--------------------------------------
#
rm(list = ls()) # clears environment
library(languageR) # calls languageR library
source(file = "StatRep.R")

f2errout <- read.table("data/SR_F2_errrel.txt", header = T) # reads in all data from data file
d <- f2errout # renames data file
d$item <- as.factor(d$item) # designates "item" as a factor
# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
#aggregates d with dysfluencies
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error") # renames columns

data.list  = list(
gmean      =  data.item,
relat      = subset(data.item, related == "rel"),
unrel      = subset(data.item, related == "unrel"),
sing       = subset(data.item, n2num   == "sing"),
plur       = subset(data.item, n2num   == "plur"),
relat.plur = subset(data.item, related == "rel"   & n2num == "plur"),
relat.sing = subset(data.item, related == "rel"   & n2num == "sing"),
unrel.plur = subset(data.item, related == "unrel" & n2num == "plur"),
unrel.sing = subset(data.item, related == "unrel" & n2num == "sing")
)

cat(rep(c("-"), times=40, quote=F),"\n")
cat("RELATED ITEMS ANALYSES", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
ds <- StatRep(c("gmean","relat","unrel","plur","sing","relat.plur","relat.sing","unrel.plur","unrel.sing"))
print(format(ds, trim = FALSE, nsmall = 4, scientific = FALSE))
cat(" ", "\n")

a.2x2 <- aov(error ~ related * n2num + Error(item / (related * n2num)), data = data.item)
print(summary(a.2x2))
cat(" ", "\n")
cat(" ", "\n")

# -------------------------RELATED ITEMS ANALYSES------------------------------

ds.relat <- StatRep(c("relat","relat.plur","relat.sing"))

cat(">>>  RELATED ITEMS PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.relat)
cat(" ", "\n")

a.relat <- aov(error ~ n2num + Error(item / n2num), data = data.list$relat)
print(summary(a.relat))
cat(" ", "\n")
cat(" ", "\n")
#------------------------------UNRELATED PAIRED COMPARISION----------------
ds.unrel <- StatRep(c("unrel", "unrel.plur", "unrel.sing"))

              
cat(">>>  UNRELATED ITEMS PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.unrel)
cat(" ", "\n")

a.unrel <- aov(error ~ n2num + Error(item / n2num), data = data.list$unrel)
print(summary(a.unrel))
cat(" ", "\n")
cat(" ", "\n")

sink()
#==========================================================================================================
#
# ----------------------------INTEGRATED - UNINTEGRATED PAIRED COMPARISONS --------------------------------
#
f2errout <- read.table("data/SR_F2_errint.txt", header = T) # reads in all data from data file
d <- f2errout # renames data file


d$item <- as.factor(d$item) # designates "item" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)

#aggregates d with dysfluencies
data.item <- aggregate(d$pct, list(d$item, d$semint, d$n2num ), mean)

colnames(data.item) <- c("item", "semint", "n2num", "error") # renames columns


integ      <- subset(data.item, semint == "integ")
unint      <- subset(data.item, semint == "unint")
sing       <- subset(data.item, n2num  == "sing")
plur       <- subset(data.item, n2num  == "plur")
integ.plur <- subset(data.item, semint == "integ" & n2num == "plur")
integ.sing <- subset(data.item, semint == "integ" & n2num == "sing")
unint.plur <- subset(data.item, semint == "unint" & n2num == "plur")
unint.sing <- subset(data.item, semint == "unint" & n2num == "sing")

ds <- data.frame(data = c(
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

n = c(length(data.item$error),
      length(integ$error),
      length(unint$error),
      length(plur$error),
      length(sing$error),
      length(integ.plur$error),
      length(integ.sing$error),
      length(unint.plur$error),
      length(unint.sing$error)
),

N = c(length(data.item$error),
      length(integ$error),
      length(unint$error),
      length(plur$error),
      length(sing$error),
      length(integ.plur$error),
      length(integ.sing$error),
      length(unint.plur$error),
      length(unint.sing$error)
),

mean = c(mean(data.item$error),
         mean(integ$error),
         mean(unint$error),
         mean(plur$error),
         mean(sing$error),
         mean(integ.plur$error),
         mean(integ.sing$error),
         mean(unint.plur$error),
         mean(unint.sing$error)
),

sd = c(sd(data.item$error),
       sd(integ$error),
       sd(unint$error),
       sd(plur$error),
       sd(sing$error),
       sd(integ.plur$error),
       sd(integ.sing$error),
       sd(unint.plur$error),
       sd(unint.sing$error)
),

se = c(sd(data.item$error) / sqrt(length(data.item$error)),
       sd(integ$error) / sqrt(length(integ$error)),
       sd(unint$error) / sqrt(length(unint$error)),
       sd(plur$error) / sqrt(length(plur$error)),
       sd(sing$error) / sqrt(length(sing$error)),
       sd(integ.plur$error) / sqrt(length(integ.plur$error)),
       sd(integ.sing$error) / sqrt(length(integ.sing$error)),
       sd(unint.plur$error) / sqrt(length(unint.plur$error)),
       sd(unint.sing$error) / sqrt(length(unint.sing$error))

))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("INTEGRATED ITEMS ANALYSES", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds)
cat(" ", "\n")

a.2x2 <- aov(error ~ semint * n2num + Error(item / (semint * n2num)), data = data.item)  # 2x2 anova
print(summary(a.2x2))
cat(" ", "\n")
cat(" ", "\n")

#
# -----------------------INTEGRATED PAIRED COMPARISONS------------------------------------------------------
#

ds.integ <- data.frame(data = c("n2num", "plur", "sing"),

                     n = c(length(integ$error),
                           length(integ.plur$error),
                           length(integ.sing$error)),

                     N = c(length(integ$error),
                           length(integ.plur$error),
                           length(integ.sing$error)),

                     mean = c(mean(integ$error),
                              mean(integ.plur$error),
                              mean(integ.sing$error)),

                     sd = c(sd(integ$error),
                            sd(integ.plur$error),
                            sd(integ.sing$error)),

                     se = c(sd(integ$error) / sqrt(length(integ$error)),
                            sd(integ.plur$error) / sqrt(length(integ.plur$error)),
                            sd(integ.sing$error) / sqrt(length(integ.sing$error))))

cat(">>>  INTEGRATED ITEMS PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.integ)
cat(" ", "\n")


a.integ <- aov(error ~ n2num + Error(item / n2num), data = integ)
print(summary(a.integ))
cat(" ", "\n")
cat(" ", "\n")
#
#----------------------UNINTEGRATED PAIRED COMPARISONS--------------------------------------------------
#

ds.uninteg <- data.frame(data = c("n2num", "plur", "sing"),

                       n = c(length(unint$error),
                             length(unint.plur$error),
                             length(unint.sing$error)),

                       N = c(length(unint$error),
                             length(unint.plur$error),
                             length(unint.sing$error)),

                     mean = c(mean(unint$error),
                              mean(unint.plur$error),
                              mean(unint.sing$error)),

                       sd = c(sd(unint$error),
                              sd(unint.plur$error),
                              sd(unint.sing$error)),

                       se = c(sd(unint$error) / sqrt(length(unint$error)),
                              sd(unint.plur$error) / sqrt(length(unint.plur$error)),
                              sd(unint.sing$error) / sqrt(length(unint.sing$error))))

cat(">>>  UNINTEGRATED ITEMS PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.uninteg)
cat(" ", "\n")

a.uninteg <- aov(error ~ n2num + Error(item / n2num), data = unint)
print(summary(a.uninteg))
cat(" ", "\n")
cat(" ", "\n")


# ---------------------------------------Paired comparisions for each condition------------------
#

f2errout <- read.table("data/SR_F2_errordata.txt", header = T) # reads in all data from data file
d <- f2errout # renames data file
d$item <- as.factor(d$item) # designates "itemect" as a factor

d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord))*100)
data.item <- aggregate(d$pct, list(d$item, d$integ, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "semint", "related", "n2num", "error") # renames columns
# ----------------Integrated Related paired----------------------
integ.relat    <- subset(data.item, semint  == "integ" & related == "rel")
relat.int.plur <- subset(data.item, related == "rel"   & semint  == "integ" & n2num == "plur")
relat.int.sing <- subset(data.item, related == "rel"   & semint  == "integ" & n2num == "sing")

ds.integrel <- data.frame(data = c("n2num","plur","sing"),

                          n = c(length(integ.relat$error),
                                length(relat.int.plur$error),
                                length(relat.int.sing$error)),

                          N = c(length(integ.relat$error),
                                length(relat.int.plur$error),
                                length(relat.int.sing$error)),

                          mean = c(mean(integ.relat$error),
                                   mean(relat.int.plur$error),
                                   mean(relat.int.sing$error)),

                          sd = c(sd(integ.relat$error),
                                 sd(relat.int.plur$error),
                                 sd(relat.int.sing$error)),

                          se = c(sd(integ.relat$error) / sqrt(length(integ.relat$error)),
                                 sd(relat.int.plur$error) / sqrt(length(relat.int.plur$error)),
                                 sd(relat.int.sing$error) / sqrt(length(relat.int.sing$error))))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("VARIOUS PAIRED COMBINATIONS", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
cat(" ", "\n")

cat(">>>  INTEGRATED -RELATED PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.integrel)
print(rep(c("-"), times = 50), quote = F)
cat(" ", "\n")

a.integrel <- aov(error ~ n2num + Error(item / n2num), data = integ.relat)
print(summary(a.integrel))

#-------------------Integrated Unrelated paired--------------------
integ.unrel    <- subset(data.item, semint  == "integ" & related == "unrel")
unrel.int.plur <- subset(data.item, related == "unrel" & semint  == "integ" & n2num == "plur")
unrel.int.sing <- subset(data.item, related == "unrel" & semint  == "integ" & n2num == "sing")

ds.integunrel <- data.frame(data = c("n2num", "plur", "sing"),

                            n = c(length(integ.unrel$error),
                                  length(unrel.int.plur$error),
                                  length(unrel.int.sing$error)),

                            N = c(length(integ.unrel$error),
                                  length(unrel.int.plur$error),
                                  length(unrel.int.sing$error)),

                            mean = c(mean(integ.unrel$error),
                                     mean(unrel.int.plur$error),
                                     mean(unrel.int.sing$error)),

                            sd = c(sd(integ.unrel$error),
                                   sd(unrel.int.plur$error),
                                   sd(unrel.int.sing$error)),

                            se = c(sd(integ.unrel$error) / sqrt(length(integ.unrel$error)),
                                   sd(unrel.int.plur$error) / sqrt(length(unrel.int.plur$error)),
                                   sd(unrel.int.sing$error) / sqrt(length(unrel.int.sing$error))))

cat(">>>  INTEGRATED -UNRELATED PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.integunrel)
cat(" ", "\n")

a.integunrel <- aov(error ~ n2num + Error(item / n2num), data = integ.unrel)
print(summary(a.integunrel))
cat(" ", "\n")
cat(" ", "\n")

# ----------------------Uninegrated Related paired-------------------------
unint.relat      <- subset(data.item, semint  == "unint" & related == "rel")
relat.unint.plur <- subset(data.item, related == "rel"   & semint  == "unint" & n2num == "plur")
relat.unint.sing <- subset(data.item, related == "rel"   & semint  == "unint" & n2num == "sing")

ds.unintrel <- data.frame(data = c("n2num", "plur", "sing"),

                          n = c(length(unint.relat$error),
                                length(relat.unint.plur$error),
                                length(relat.unint.sing$error)),

                          N = c(length(unint.relat$error),
                                length(relat.unint.plur$error),
                                length(relat.unint.sing$error)),

                          mean = c(mean(unint.relat$error),
                                   mean(relat.unint.plur$error),
                                   mean(relat.unint.sing$error)),

                          sd = c(sd(unint.relat$error),
                                 sd(relat.unint.plur$error),
                                 sd(relat.unint.sing$error)),

                          se = c(sd(unint.relat$error) / sqrt(length(unint.relat$error)),
                                 sd(relat.unint.plur$error) / sqrt(length(relat.unint.plur$error)),
                                 sd(relat.unint.sing$error) / sqrt(length(relat.unint.sing$error))))

cat(">>>  UNINTEGRATED - RELATED PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.unintrel)
cat(" ", "\n")
a.unintrel <- aov(error ~ n2num + Error(item / n2num), data = unint.relat)
print(summary(a.unintrel))
cat(" ", "\n")
cat(" ", "\n")

# -------------------------Unintegrated Unrelated comparisons-------------------
unint.unrel      <- subset(data.item, semint  == "unint" & related == "unrel")
unrel.unint.plur <- subset(data.item, related == "unrel" & semint  == "unint" & n2num == "plur")
unrel.unint.sing <- subset(data.item, related == "unrel" & semint  == "unint" & n2num == "sing")

ds.unintunrel <- data.frame(data=c("n2num","plur","sing"),

                            n = c(length(unint.unrel$error),
                                  length(unrel.unint.plur$error),
                                  length(unrel.unint.sing$error)),

                            N = c(length(unint.unrel$error),
                                  length(unrel.unint.plur$error),
                                  length(unrel.unint.sing$error)),

                            mean = c(mean(unint.unrel$error),
                                     mean(unrel.unint.plur$error),
                                     mean(unrel.unint.sing$error)),

                            sd = c(sd(unint.unrel$error),
                                   sd(unrel.unint.plur$error),
                                   sd(unrel.unint.sing$error)),

                            se = c(sd(unint.unrel$error) / sqrt(length(unint.unrel$error)),
                                   sd(unrel.unint.plur$error) / sqrt(length(unrel.unint.plur$error)),
                                   sd(unrel.unint.sing$error) / sqrt(length(unrel.unint.sing$error))))

cat(">>>  UNINTEGRATED - UNRELATED PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.unintunrel)
cat(" ", "\n")

a.unintunrel <- aov(error ~ n2num + Error(item / n2num), data = unint.unrel)
print(summary(a.unintunrel))
cat(" ", "\n")
cat(" ", "\n")




sink()