
rm(list = ls()) # clears environment
library(languageR)




# ====================================================================================================
# ------------------------------------CATEGORY COORDINATE ITEMS ANALYSES
# ====================================================================================================
#
# -----------------------------------PREPARE DATA FILE FOR ANALYSES---------------------------------
#
f1errout <- read.table("data/SR2_F1_errcat.txt", header = T) # reads in all data from data file

d <- f1errout # renames data file

d$subj <- as.factor(d$subj) # designates "subject" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)

# aggregates d with dysfluencies

data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)

colnames(data.subj) <- c("subj", "related", "n2num", "error") # renames columns

# Below, designates various subsets of the original data file
relat       <- subset(data.subj, related  ==  "rel")
unrel       <- subset(data.subj, related  ==  "unrel")
sing        <- subset(data.subj, n2num    ==  "sing")
plur        <- subset(data.subj, n2num    ==  "plur")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")



ds <- data.frame(data = c(
  "gmean",
  "relat",
  "unrel",
  "plur",
  "sing",
  "relplur",
  "relsing",
  "unrelplur",
  "unrelsing"),

  n = c(length(data.subj$error),
        length(relat$error),
        length(unrel$error),
        length(plur$error),
        length(sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error)),

  N = c(length(data.subj$error),
        length(relat$error),
        length(unrel$error),
        length(plur$error),
        length(sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error)),

  mean = c(mean(data.subj$error),
           mean(relat$error),
           mean(unrel$error),
           mean(plur$error),
           mean(sing$error),
           mean(relat.plur$error),
           mean(relat.sing$error),
           mean(unrel.plur$error),
           mean(unrel.sing$error)),

  sd = c(sd(data.subj$error),
         sd(relat$error),
         sd(unrel$error),
         sd(plur$error),
         sd(sing$error),
         sd(relat.plur$error),
         sd(relat.sing$error),
         sd(unrel.plur$error),
         sd(unrel.sing$error)),

  se = c(sd(data.subj$error) / sqrt(length(data.subj$error)),
         sd(relat$error) / sqrt(length(relat$error)),
         sd(unrel$error) / sqrt(length(unrel$error)),
         sd(plur$error) / sqrt(length(plur$error)),
         sd(sing$error) / sqrt(length(sing$error)),
         sd(relat.plur$error) / sqrt(length(relat.plur$error) ),
         sd(relat.sing$error) / sqrt(length(relat.sing$error)),
         sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
         sd(unrel.sing$error) / sqrt(length(unrel.sing$error))
  ))


#--------------------------------2 X 2 ANOVA------------------------------------------------------


sink("output/SemRel2 F1 Factorial Analyses.txt")
cat(" ", "\n")
cat("BY-SUBJECTS FACTORIAL ANALYSES RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")
cat(rep(c("-"), times=40, quote=F),"\n")
cat("2X2 ANOVA: CATEGORY COORDINATES", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds)
cat(" ", "\n")

a.2x2 <- aov(error ~ related * n2num + Error(subj / (related * n2num)), data = data.subj)
print(summary(a.2x2))

cat(" ", "\n")
cat(" ", "\n")

#  --------------------RELATED vs. UNRELATED ITEMS PAIRED COMPARISONS--------------------------------
#
#  ------------RELATED ITEMS------------------------------

ds.relat <- data.frame(data = c("n2num","plur","sing"),

                       n = c(length(relat$error),
                             length(relat.plur$error),
                             length(relat.sing$error)),

                       N = c(length(relat$error),
                             length(relat.plur$error),
                             length(relat.sing$error)),

                       mean = c(mean(relat$error),
                                mean(relat.plur$error),
                                mean(relat.sing$error)),

                       sd = c(sd(relat$error),
                              sd(relat.plur$error),
                              sd(relat.sing$error)),

                       se = c(sd(relat$error) / sqrt(length(relat$error)),
                              sd(relat.plur$error) / sqrt(length(relat.plur$error)),
                              sd(relat.sing$error) / sqrt(length(relat.sing$error))))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("RELATED VS. UNRELATED ITEMS PAIRED COMPARISONS", fill = 60)
cat(rep(c("-"), times=40, quote=F),"\n")
cat(" ", "\n")
cat(" ", "\n")
cat(">>> RELATED ITEMS PARIED COMPARISONS", fill=60 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.relat)
cat(" ", "\n")

a.relat <- aov(error ~ n2num + Error(subj / n2num), data = relat)
print(summary(a.relat))

cat(" ", "\n")
cat(" ", "\n")

# -----------------------UNRELATED ITEMS----------------
ds.unrel <- data.frame(data = c("n2num", "plur", "sing"),

                       n = c(length(unrel$error),
                             length(unrel.plur$error),
                             length(unrel.sing$error)),

                       N = c(length(unrel$error),
                             length(unrel.plur$error),
                             length(unrel.sing$error)),

                       mean = c(mean(unrel$error),
                                mean(unrel.plur$error),
                                mean(unrel.sing$error)),

                       sd = c(sd(unrel$error),
                              sd(unrel.plur$error),
                              sd(unrel.sing$error)),

                       se = c(sd(unrel$error) / sqrt(length(unrel$error)),
                              sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
                              sd(unrel.sing$error) / sqrt(length(unrel.sing$error))))

cat(">>> UNRELATED ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.unrel)
cat(" ", "\n")

a.unrel <- aov(error ~ n2num + Error(subj / n2num), data = unrel)
print(summary(a.unrel))


cat(" ", "\n")
cat(" ", "\n")


#
# -----------------------------Paired comparisions of Plural - Singular items
#
#------------Plural Items
#

f1errout <- read.table("data/SR2_F1_errcat.txt", header = T)

d <- f1errout
d <- subset(d, n2num !="sing")
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")
relat       <- subset(data.subj, related  ==  "rel")
unrel       <- subset(data.subj, related  ==  "unrel")
sing        <- subset(data.subj, n2num    ==  "sing")
plur        <- subset(data.subj, n2num    ==  "plur")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")

ds.plur <- data.frame(data = c("Related","Rel","unrel"),

                      n = c(length(plur$error),
                            length(relat.plur$error),
                            length(unrel.plur$error)),

                      N = c(length(plur$error),
                            length(relat.plur$error),
                            length(unrel.plur$error)),

                      mean = c(mean(plur$error),
                               mean(relat.plur$error),
                               mean(unrel.plur$error)),

                      sd = c(sd(plur$error),
                             sd(relat.plur$error),
                             sd(unrel.plur$error)),

                      se = c(sd(plur$error) / sqrt(length(plur$error)),
                             sd(relat.plur$error) / sqrt(length(relat.plur$error)),
                             sd(unrel.plur$error) / sqrt(length(unrel.plur$error))))


cat(rep(c("-"), times=40, quote=F),"\n")
cat("PLURAL VS. SINGULAR PAIRED COMPARISONS", fill = 50)
cat(rep(c("-"), times=40, quote=F),"\n")
cat(" ", "\n")
cat(" ", "\n")
cat(">>>  PLURAL ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.plur)
cat(" ", "\n")

a.plur <- aov(error ~ related + Error(subj / related), data = plur)
print(summary(a.plur))
cat(" ", "\n")
cat(" ", "\n")


# -------------------------------Singular Items----------------------
f1errout <- read.table("data/SR2_F1_errcat.txt", header = T)
d <- f1errout
d <- subset(d, n2num !="plur")
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")

relat       <- subset(data.subj, related  ==  "rel")
unrel       <- subset(data.subj, related  ==  "unrel")
sing        <- subset(data.subj, n2num    ==  "sing")
plur        <- subset(data.subj, n2num    ==  "plur")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")
ds.sing <- data.frame(data = c("n2num","plur","sing"),

                      n = c(length(sing$error),
                            length(relat.sing$error),
                            length(unrel.sing$error)),

                      N = c(length(sing$error),
                            length(relat.sing$error),
                            length(unrel.sing$error)),

                      mean = c(mean(sing$error),
                               mean(relat.sing$error),
                               mean(unrel.sing$error)),

                      sd = c(sd(sing$error),
                             sd(relat.sing$error),
                             sd(unrel.sing$error)),

                      se = c(sd(sing$error) / sqrt(length(sing$error)),
                             sd(relat.sing$error) / sqrt(length(relat.sing$error)),
                             sd(unrel.sing$error) / sqrt(length(unrel.sing$error))))


cat(">>>  SINGULAR ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.sing)
cat(" ", "\n")

a.sing <- aov(error ~ related + Error(subj / related), data = sing)
print(summary(a.sing))
cat(" ", "\n")
cat(" ", "\n")
cat("\n", rep(c("//\\\\"), times = 25, quote = F),"\n")  # ==================CATEGORY ABOVE & PROP BELOW ==========================

cat(" ", "\n")
cat(" ", "\n")

# # ====================================================================================================
# # --------------------------------------PROPERTY ITEMS ANALYSES---------------------
# # ====================================================================================================

f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")
relat       <- subset(data.subj, related == "rel")
assoc       <- subset(data.subj, related == "assoc")
unrel       <- subset(data.subj, related == "unrel")
sing        <- subset(data.subj, n2num   == "sing")
plur        <- subset(data.subj, n2num   == "plur")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
assoc.plur  <- subset(data.subj, related == "assoc" & n2num   == "plur")
assoc.sing   <- subset(data.subj, related == "assoc" & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")


ds <- data.frame(data = c(
  "gmean",
  "assoc",
  "relat",
  "unrel",
  "plur",
  "sing",
  "assocplur",
  "assocsing",
  "relplur",
  "relsing",
  "unrelplur",
  "unrelsing"),

  n = c(length(data.subj$error),
        length(assoc$error),
        length(relat$error),
        length(unrel$error),
        length(plur$error),
        length(sing$error),
        length(assoc.plur$error),
        length(assoc.sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error)),

  N = c(length(data.subj$error),
        length(assoc$error),
        length(relat$error),
        length(unrel$error),
        length(plur$error),
        length(sing$error),
        length(assoc.plur$error),
        length(assoc.sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error)),

  mean = c(mean(data.subj$error),
           mean(assoc$error),
           mean(relat$error),
           mean(unrel$error),
           mean(plur$error),
           mean(sing$error),
           mean(assoc.plur$error),
           mean(assoc.sing$error),
           mean(relat.plur$error),
           mean(relat.sing$error),
           mean(unrel.plur$error),
           mean(unrel.sing$error)),

  sd = c(sd(data.subj$error),
         sd(assoc$error),
         sd(relat$error),
         sd(unrel$error),
         sd(plur$error),
         sd(sing$error),
         sd(assoc.plur$error),
         sd(assoc.sing$error),
         sd(relat.plur$error),
         sd(relat.sing$error),
         sd(unrel.plur$error),
         sd(unrel.sing$error)),

  se = c(sd(data.subj$error) / sqrt(length(data.subj$error)),
         sd(assoc$error) / sqrt(length(assoc$error)),
         sd(relat$error) / sqrt(length(relat$error)),
         sd(unrel$error) / sqrt(length(unrel$error)),
         sd(plur$error) / sqrt(length(plur$error)),
         sd(sing$error) / sqrt(length(sing$error)),
         sd(assoc.plur$error) / sqrt(length(assoc.plur$error)),
         sd(assoc.sing$error) / sqrt(length(assoc.sing$error)),
         sd(relat.plur$error) / sqrt(length(relat.plur$error)),
         sd(relat.sing$error) / sqrt(length(relat.sing$error)),
         sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
         sd(unrel.sing$error) / sqrt(length(unrel.sing$error))
  ))

#
# --------------------------------3 X 2  ANOVA------------------------------------------------------
#
cat(rep(c("-"), times=40, quote=F),"\n")
cat("2X2 ANOVA: PROPERTY ITEMS", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds)
cat(" ", "\n")
a.3x2 <- aov(error ~ related * n2num + Error(subj / (related * n2num)), data = data.subj)
print(summary(a.3x2))
cat(" ", "\n")
cat(" ", "\n")

#
# ========================================================================================================
#
# --------------------RELATED - ASSOCIATED ITEMS --------------------------------------

f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d <- subset(d, related != "unrel")
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")
relat       <- subset(data.subj, related == "rel")
assoc       <- subset(data.subj, related == "assoc")
unrel       <- subset(data.subj, related == "unrel")
sing        <- subset(data.subj, n2num   == "sing")
plur        <- subset(data.subj, n2num   == "plur")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
assoc.plur  <- subset(data.subj, related == "assoc" & n2num   == "plur")
assoc.sing   <- subset(data.subj, related == "assoc" & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")

assrel <- data.frame(data = c(
  "gmean",
  "assoc",
  "relat",
  "plur",
  "sing",
  "assocplur",
  "assocsing",
  "relplur",
  "relsing"),

  n = c(length(data.subj$error),
        length(assoc$error),
        length(relat$error),
        length(plur$error),
        length(sing$error),
        length(assoc.plur$error),
        length(assoc.sing$error),
        length(relat.plur$error),
        length(relat.sing$error)),

  N = c(length(data.subj$error),
        length(assoc$error),
        length(relat$error),
        length(plur$error),
        length(sing$error),
        length(assoc.plur$error),
        length(assoc.sing$error),
        length(relat.plur$error),
        length(relat.sing$error)),

  mean = c(mean(data.subj$error),
           mean(assoc$error),
           mean(relat$error),
           mean(plur$error),
           mean(sing$error),
           mean(assoc.plur$error),
           mean(assoc.sing$error),
           mean(relat.plur$error),
           mean(relat.sing$error)),

  sd = c(sd(data.subj$error),
         sd(assoc$error),
         sd(relat$error),
         sd(plur$error),
         sd(sing$error),
         sd(assoc.plur$error),
         sd(assoc.sing$error),
         sd(relat.plur$error),
         sd(relat.sing$error)),

  se = c(sd(data.subj$error) / sqrt(length(data.subj$error)),
         sd(assoc$error) / sqrt(length(assoc$error)),
         sd(relat$error) / sqrt(length(relat$error)),
         sd(plur$error) / sqrt(length(plur$error)),
         sd(sing$error) / sqrt(length(sing$error)),
         sd(assoc.plur$error) / sqrt(length(assoc.plur$error)),
         sd(assoc.sing$error) / sqrt(length(assoc.sing$error)),
         sd(relat.plur$error) / sqrt(length(relat.plur$error)),
         sd(relat.sing$error) / sqrt(length(relat.sing$error))
  ))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("ASSOCIATED VS. RELATED VS. UNRELATED  PAIRED COMPARISONS", fill = 50)
cat(rep(c("-"), times=40, quote=F),"\n")
cat(" ", "\n")
cat(" ", "\n")
cat(">>>  ASSOCIATED VS. RELATED", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(assrel)
cat(" ", "\n")

a.assrel <- aov(error ~ related * n2num + Error(subj / (related * n2num)), data = data.subj)
print(summary(a.assrel))

cat(" ", "\n")
cat(" ", "\n")


# --------------------RELATED - UNRELATED ITEMS --------------------------------------

f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d <- subset(d, related != "assoc")
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")

relat       <- subset(data.subj, related == "rel")
assoc       <- subset(data.subj, related == "assoc")
unrel       <- subset(data.subj, related == "unrel")
sing        <- subset(data.subj, n2num   == "sing")
plur        <- subset(data.subj, n2num   == "plur")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
assoc.plur  <- subset(data.subj, related == "assoc" & n2num   == "plur")
assoc.sing   <- subset(data.subj, related == "assoc" & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")

relunr <- data.frame(data = c(
  "gmean",
  "relat",
  "unrel",
  "plur",
  "sing",
  "relplur",
  "relsing",
  "unrelplur",
  "unrelsing"),

  n = c(length(data.subj$error),
        length(relat$error),
        length(unrel$error),
        length(plur$error),
        length(sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error)),

  N = c(length(data.subj$error),
        length(relat$error),
        length(unrel$error),
        length(plur$error),
        length(sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error)),

  mean = c(mean(data.subj$error),
           mean(relat$error),
           mean(unrel$error),
           mean(plur$error),
           mean(sing$error),
           mean(relat.plur$error),
           mean(relat.sing$error),
           mean(unrel.plur$error),
           mean(unrel.sing$error)),

  sd = c(sd(data.subj$error),
         sd(relat$error),
         sd(unrel$error),
         sd(plur$error),
         sd(sing$error),
         sd(relat.plur$error),
         sd(relat.sing$error),
         sd(unrel.plur$error),
         sd(unrel.sing$error)),

  se = c(sd(data.subj$error) / sqrt(length(data.subj$error)),
         sd(relat$error) / sqrt(length(relat$error)),
         sd(unrel$error) / sqrt(length(unrel$error)),
         sd(plur$error) / sqrt(length(plur$error)),
         sd(sing$error) / sqrt(length(sing$error)),
         sd(relat.plur$error) / sqrt(length(relat.plur$error)),
         sd(relat.sing$error) / sqrt(length(relat.sing$error)),
         sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
         sd(unrel.sing$error) / sqrt(length(unrel.sing$error))
  ))


cat(">>>  RELATED VS. UNRELATED", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(relunr)
cat(" ", "\n")

a.relunr <- aov(error ~ related * n2num + Error(subj / (related * n2num)), data = data.subj)
print(summary(a.relunr))
cat(" ", "\n")
cat(" ", "\n")


# -------------------- ASSOCIATED - UNRELATED ITEMS --------------------------------------

f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d <- subset(d, related != "rel")
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")
relat       <- subset(data.subj, related == "rel")
assoc       <- subset(data.subj, related == "assoc")
unrel       <- subset(data.subj, related == "unrel")
sing        <- subset(data.subj, n2num   == "sing")
plur        <- subset(data.subj, n2num   == "plur")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
assoc.plur  <- subset(data.subj, related == "assoc" & n2num   == "plur")
assoc.sing   <- subset(data.subj, related == "assoc" & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")


assunr <- data.frame(data = c(
  "gmean",
  "assoc",
  "unrel",
  "plur",
  "sing",
  "assocplur",
  "assocsing",
  "relplur",
  "relsing"),

  n = c(length(data.subj$error),
        length(assoc$error),
        length(unrel$error),
        length(plur$error),
        length(sing$error),
        length(assoc.plur$error),
        length(assoc.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error)),

  N = c(length(data.subj$error),
        length(assoc$error),
        length(unrel$error),
        length(plur$error),
        length(sing$error),
        length(assoc.plur$error),
        length(assoc.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error)),

  mean = c(mean(data.subj$error),
           mean(assoc$error),
           mean(unrel$error),
           mean(plur$error),
           mean(sing$error),
           mean(assoc.plur$error),
           mean(assoc.sing$error),
           mean(unrel.plur$error),
           mean(unrel.sing$error)),

  sd = c(sd(data.subj$error),
         sd(assoc$error),
         sd(unrel$error),
         sd(plur$error),
         sd(sing$error),
         sd(assoc.plur$error),
         sd(assoc.sing$error),
         sd(unrel.plur$error),
         sd(unrel.sing$error)),

  se = c(sd(data.subj$error) / sqrt(length(data.subj$error)),
         sd(assoc$error) / sqrt(length(assoc$error)),
         sd(unrel$error) / sqrt(length(unrel$error)),
         sd(plur$error) / sqrt(length(plur$error)),
         sd(sing$error) / sqrt(length(sing$error)),
         sd(assoc.plur$error) / sqrt(length(assoc.plur$error)),
         sd(assoc.sing$error) / sqrt(length(assoc.sing$error)),
         sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
         sd(unrel.sing$error) / sqrt(length(unrel.sing$error))
  ))

cat(">>>  ASSOCIATED VS. UNRELATED", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(assunr)
cat(" ", "\n")

a.assunr <- aov(error ~ related * n2num + Error(subj / (related * n2num)), data = data.subj)
print(summary(a.assunr))

cat(" ", "\n")
cat(" ", "\n")  # ==========================================================================================


#
# --------------------------------- PAIRED COMPARISONS---------------------
#

f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")

relat       <- subset(data.subj, related == "rel")
assoc       <- subset(data.subj, related == "assoc")
unrel       <- subset(data.subj, related == "unrel")
sing        <- subset(data.subj, n2num   == "sing")
plur        <- subset(data.subj, n2num   == "plur")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
assoc.plur  <- subset(data.subj, related == "assoc" & n2num   == "plur")
assoc.sing   <- subset(data.subj, related == "assoc" & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")


# # ------------RELATED ITEMS------------------------------
#
ds.relat <- data.frame(data = c("n2num","plur","sing"),

                       n = c(length(relat$error),
                             length(relat.plur$error),
                             length(relat.sing$error)),

                       N = c(length(relat$error),
                             length(relat.plur$error),
                             length(relat.sing$error)),

                       mean = c(mean(relat$error),
                                mean(relat.plur$error),
                                mean(relat.sing$error)),

                       sd = c(sd(relat$error),
                              sd(relat.plur$error),
                              sd(relat.sing$error)),

                       se = c(sd(relat$error) / sqrt(length(relat$error)),
                              sd(relat.plur$error) / sqrt(length(relat.plur$error)),
                              sd(relat.sing$error) / sqrt(length(relat.sing$error))))


cat(rep(c("-"), times=40, quote=F),"\n")
cat("PAIRED COMPARISONS", fill = 50)
cat(rep(c("-"), times=40, quote=F),"\n")
cat(" ", "\n")
cat(" ", "\n")
cat(">>>  RELATED SING. VS. PLUR", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.relat)
cat(" ", "\n")

a.relat <- aov(error ~ n2num + Error(subj / n2num), data = relat)
print(summary(a.relat))
cat(" ", "\n")
cat(" ", "\n")


# # ------------ASSOC ITEMS------------------------------
#

ds.assoc <- data.frame(data = c("n2num","plur","sing"),

                       n = c(length(assoc$error),
                             length(assoc.plur$error),
                             length(assoc.sing$error)),

                       N = c(length(assoc$error),
                             length(assoc.plur$error),
                             length(assoc.sing$error)),

                       mean = c(mean(assoc$error),
                                mean(assoc.plur$error),
                                mean(assoc.sing$error)),

                       sd = c(sd(assoc$error),
                              sd(assoc.plur$error),
                              sd(assoc.sing$error)),

                       se = c(sd(assoc$error) / sqrt(length(assoc$error)),
                              sd(assoc.plur$error) / sqrt(length(assoc.plur$error)),
                              sd(assoc.sing$error) / sqrt(length(assoc.sing$error))))


cat(">>>  ASSOCIATED SING. VS. PLUR", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.assoc)
cat(" ", "\n")


a.assoc <- aov(error ~ n2num + Error(subj / n2num), data = relat)
print(summary(a.assoc))
cat(" ", "\n")
cat(" ", "\n")



# -----------------------UNRELATED ITEMS----------------
ds.unrel <- data.frame(data = c("n2num", "plur", "sing"),

                       n = c(length(unrel$error),
                             length(unrel.plur$error),
                             length(unrel.sing$error)),

                       N = c(length(unrel$error),
                             length(unrel.plur$error),
                             length(unrel.sing$error)),

                       mean = c(mean(unrel$error),
                                mean(unrel.plur$error),
                                mean(unrel.sing$error)),

                       sd = c(sd(unrel$error),
                              sd(unrel.plur$error),
                              sd(unrel.sing$error)),

                       se = c(sd(unrel$error) / sqrt(length(unrel$error)),
                              sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
                              sd(unrel.sing$error) / sqrt(length(unrel.sing$error))))

cat(">>>  UNRELATED SING. VS. PLUR", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.unrel)
cat(" ", "\n")

a.unrel <- aov(error ~ n2num + Error(subj / n2num), data = unrel)
print(summary(a.unrel))

cat(" ", "\n")
cat(" ", "\n")

# -----------------------------SINGULAR VS. PLURAL PAIRED COMPARISONS
#
#------------Plural Items
#
f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d <- subset(d, n2num !="sing")
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")


assoc       <- subset(data.subj, related  ==  "assoc")
relat       <- subset(data.subj, related  ==  "rel")
unrel       <- subset(data.subj, related  ==  "unrel")
sing        <- subset(data.subj, n2num    ==  "sing")
plur        <- subset(data.subj, n2num    ==  "plur")
assoc.plur  <- subset(data.subj, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.subj, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")


ds.plur <- data.frame(data = c("Related","Assoc","Rel","Unrel"),

                      n = c(length(plur$error),
                            length(assoc.plur$error),
                            length(relat.plur$error),
                            length(unrel.plur$error)),

                      N = c(length(plur$error),
                            length(assoc.plur$error),
                            length(relat.plur$error),
                            length(unrel.plur$error)),

                      mean = c(mean(plur$error),
                               mean(assoc.plur$error),
                               mean(relat.plur$error),
                               mean(unrel.plur$error)),

                      sd = c(sd(plur$error),
                             sd(assoc.plur$error),
                             sd(relat.plur$error),
                             sd(unrel.plur$error)),

                      se = c(sd(plur$error) / sqrt(length(plur$error)),
                             sd(assoc.plur$error) / sqrt(length(assoc.plur$error)),
                             sd(relat.plur$error) / sqrt(length(relat.plur$error)),
                             sd(unrel.plur$error) / sqrt(length(unrel.plur$error))))


cat(rep(c("-"), times=40, quote=F),"\n")
cat("PLURAL VS. SINGULAR PAIRED COMPARISONS", fill = 50)
cat(rep(c("-"), times=40, quote=F),"\n")
cat(" ", "\n")
cat(" ", "\n")
cat(">>>  PLURAL ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.plur)
cat(" ", "\n")

a.plur <- aov(error ~ related + Error(subj / related), data = plur)
print(summary(a.plur))

cat(" ", "\n")
cat(" ", "\n")
#
# ---------------------------------------Singular Items-------------------------
#

f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d <- subset(d, n2num !="plur")
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)

colnames(data.subj) <- c("subj", "related", "n2num", "error")

assoc       <- subset(data.subj, related  ==  "assoc")
relat       <- subset(data.subj, related  ==  "rel")
unrel       <- subset(data.subj, related  ==  "unrel")
sing        <- subset(data.subj, n2num    ==  "sing")
plur        <- subset(data.subj, n2num    ==  "plur")
assoc.plur  <- subset(data.subj, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.subj, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")

ds.sing <- data.frame(data = c("Related","Assoc","Rel","Unrel"),

                      n = c(length(sing$error),
                            length(assoc.sing$error),
                            length(relat.sing$error),
                            length(unrel.sing$error)),

                      N = c(length(sing$error),
                            length(assoc.sing$error),
                            length(relat.sing$error),
                            length(unrel.sing$error)),

                      mean = c(mean(sing$error),
                               mean(assoc.sing$error),
                               mean(relat.sing$error),
                               mean(unrel.sing$error)),

                      sd = c(sd(plur$error),
                             sd(assoc.sing$error),
                             sd(relat.sing$error),
                             sd(unrel.sing$error)),

                      se = c(sd(sing$error) / sqrt(length(sing$error)),
                             sd(assoc.sing$error) / sqrt(length(assoc.sing$error)),
                             sd(relat.sing$error) / sqrt(length(relat.sing$error)),
                             sd(unrel.sing$error) / sqrt(length(unrel.sing$error))))

cat(">>> SINGULAR ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.sing)
cat(" ", "\n")


a.sing <- aov(error ~ related + Error(subj / related), data = sing)
print(summary(a.sing))
cat(" ", "\n")
cat(" ", "\n")

# ------------------SUBSET PAIRED COMPARIONS---------------------
# -------- ASSOCIATED VS. RELATED PLURAL

f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d <- subset(d, n2num !="sing" & related != "unrel")
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")


assoc       <- subset(data.subj, related  ==  "assoc")
relat       <- subset(data.subj, related  ==  "rel")
unrel       <- subset(data.subj, related  ==  "unrel")
sing        <- subset(data.subj, n2num    ==  "sing")
plur        <- subset(data.subj, n2num    ==  "plur")
assoc.plur  <- subset(data.subj, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.subj, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")


ds.plur <- data.frame(data = c("Related","Assoc","Relat"),

                      n = c(length(plur$error),
                            length(assoc.plur$error),
                            length(relat.plur$error)),

                      N = c(length(plur$error),
                            length(assoc.plur$error),
                            length(relat.plur$error)),

                      mean = c(mean(plur$error),
                               mean(assoc.plur$error),
                               mean(relat.plur$error)),

                      sd = c(sd(plur$error),
                             sd(assoc.plur$error),
                             sd(relat.plur$error)),

                      se = c(sd(plur$error) / sqrt(length(plur$error)),
                             sd(assoc.plur$error) / sqrt(length(assoc.plur$error)),
                             sd(relat.plur$error) / sqrt(length(relat.plur$error))))

cat(">>>  ASSOCIATED VS. RELATED PLURAL ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.plur)
cat(" ", "\n")

a.plur <- aov(error ~ related + Error(subj / related), data = plur)
print(summary(a.plur))
cat(" ", "\n")
cat(" ", "\n")

# ----------------------RELATED VS. UNRELATED PLURAL
f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d <- subset(d, n2num !="sing" & related != "assoc")
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")


assoc       <- subset(data.subj, related  ==  "assoc")
relat       <- subset(data.subj, related  ==  "rel")
unrel       <- subset(data.subj, related  ==  "unrel")
sing        <- subset(data.subj, n2num    ==  "sing")
plur        <- subset(data.subj, n2num    ==  "plur")
assoc.plur  <- subset(data.subj, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.subj, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")


ds.plur <- data.frame(data = c("Related","Relat","Unrel"),

                      n = c(length(plur$error),
                            length(relat.plur$error),
                            length(unrel.plur$error)),

                      N = c(length(plur$error),
                            length(relat.plur$error),
                            length(unrel.plur$error)),

                      mean = c(mean(plur$error),
                               mean(relat.plur$error),
                               mean(unrel.plur$error)),

                      sd = c(sd(plur$error),
                             sd(relat.plur$error),
                             sd(unrel.plur$error)),

                      se = c(sd(plur$error) / sqrt(length(plur$error)),
                             sd(relat.plur$error) / sqrt(length(relat.plur$error)),
                             sd(unrel.plur$error) / sqrt(length(unrel.plur$error))))


cat(">>>  RELATED VS. UNRELATED PLURAL ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.plur)
cat(" ", "\n")

a.plur <- aov(error ~ related + Error(subj / related), data = plur)
print(summary(a.plur))
cat(" ", "\n")
cat(" ", "\n")


# ----------------------ASSOCIATED VS. UNRELATED PLURAL
f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d <- subset(d, n2num !="sing" & related != "rel")
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")


assoc       <- subset(data.subj, related  ==  "assoc")
relat       <- subset(data.subj, related  ==  "rel")
unrel       <- subset(data.subj, related  ==  "unrel")
sing        <- subset(data.subj, n2num    ==  "sing")
plur        <- subset(data.subj, n2num    ==  "plur")
assoc.plur  <- subset(data.subj, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.subj, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")


ds.plur <- data.frame(data = c("Related","Assoc","Unrel"),

                      n = c(length(plur$error),
                            length(assoc.plur$error),
                            length(unrel.plur$error)),

                      N = c(length(plur$error),
                            length(assoc.plur$error),
                            length(unrel.plur$error)),

                      mean = c(mean(plur$error),
                               mean(assoc.plur$error),
                               mean(unrel.plur$error)),

                      sd = c(sd(plur$error),
                             sd(assoc.plur$error),
                             sd(unrel.plur$error)),

                      se = c(sd(plur$error) / sqrt(length(plur$error)),
                             sd(assoc.plur$error) / sqrt(length(assoc.plur$error)),
                             sd(unrel.plur$error) / sqrt(length(unrel.plur$error))))


cat(">>>  ASSOCIATED VS. UNRELATED PLURAL ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.plur)
cat(" ", "\n")

a.plur <- aov(error ~ related + Error(subj / related), data = plur)
print(summary(a.plur))
cat(" ", "\n")
cat(" ", "\n")


# -------- ASSOCIATED VS. RELATED SINGULAR

f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d <- subset(d, n2num != "plur" & related != "unrel")
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")


assoc       <- subset(data.subj, related  ==  "assoc")
relat       <- subset(data.subj, related  ==  "rel")
unrel       <- subset(data.subj, related  ==  "unrel")
sing        <- subset(data.subj, n2num    ==  "sing")
plur        <- subset(data.subj, n2num    ==  "plur")
assoc.plur  <- subset(data.subj, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.subj, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")


ds.sing <- data.frame(data = c("Related","Assoc","Relat"),

                      n = c(length(sing$error),
                            length(assoc.sing$error),
                            length(relat.sing$error)),

                      N = c(length(sing$error),
                            length(assoc.sing$error),
                            length(relat.sing$error)),

                      mean = c(mean(sing$error),
                               mean(assoc.sing$error),
                               mean(relat.sing$error)),

                      sd = c(sd(sing$error),
                             sd(assoc.sing$error),
                             sd(relat.sing$error)),

                      se = c(sd(sing$error) / sqrt(length(sing$error)),
                             sd(assoc.sing$error) / sqrt(length(assoc.sing$error)),
                             sd(relat.sing$error) / sqrt(length(relat.sing$error))))

cat(">>>  ASSOCIATED VS. RELATED SINGULAR ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.sing)
cat(" ", "\n")

a.sing <- aov(error ~ related + Error(subj / related), data = sing)
print(summary(a.sing))
cat(" ", "\n")
cat(" ", "\n")

# ----------------------RELATED VS. UNRELATED SINGULAR
f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d <- subset(d, n2num != "plur" & related != "assoc")
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")


assoc       <- subset(data.subj, related  ==  "assoc")
relat       <- subset(data.subj, related  ==  "rel")
unrel       <- subset(data.subj, related  ==  "unrel")
sing        <- subset(data.subj, n2num    ==  "sing")
plur        <- subset(data.subj, n2num    ==  "plur")
assoc.plur  <- subset(data.subj, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.subj, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")


ds.sing <- data.frame(data = c("Related","Relat","Unrel"),

                      n = c(length(sing$error),
                            length(relat.sing$error),
                            length(unrel.sing$error)),

                      N = c(length(sing$error),
                            length(relat.sing$error),
                            length(unrel.sing$error)),

                      mean = c(mean(sing$error),
                               mean(relat.sing$error),
                               mean(unrel.sing$error)),

                      sd = c(sd(sing$error),
                             sd(relat.sing$error),
                             sd(unrel.sing$error)),

                      se = c(sd(sing$error) / sqrt(length(sing$error)),
                             sd(relat.sing$error) / sqrt(length(relat.sing$error)),
                             sd(unrel.sing$error) / sqrt(length(unrel.sing$error))))


cat(">>>  RELATED VS. UNRELATED SINGULAR ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.sing)
cat(" ", "\n")

a.sing <- aov(error ~ related + Error(subj / related), data = sing)
print(summary(a.sing))
cat(" ", "\n")
cat(" ", "\n")


# ----------------------ASSOCIATED VS. UNRELATED SINGULAR
f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d <- subset(d, n2num != "plur" & related != "rel")
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")


assoc       <- subset(data.subj, related  ==  "assoc")
relat       <- subset(data.subj, related  ==  "rel")
unrel       <- subset(data.subj, related  ==  "unrel")
sing        <- subset(data.subj, n2num    ==  "sing")
plur        <- subset(data.subj, n2num    ==  "plur")
assoc.plur  <- subset(data.subj, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.subj, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")


ds.sing <- data.frame(data = c("Related","Assoc","Unrel"),

                      n = c(length(sing$error),
                            length(assoc.sing$error),
                            length(unrel.sing$error)),

                      N = c(length(sing$error),
                            length(assoc.sing$error),
                            length(unrel.sing$error)),

                      mean = c(mean(sing$error),
                               mean(assoc.sing$error),
                               mean(unrel.sing$error)),

                      sd = c(sd(sing$error),
                             sd(assoc.sing$error),
                             sd(unrel.sing$error)),

                      se = c(sd(sing$error) / sqrt(length(sing$error)),
                             sd(assoc.sing$error) / sqrt(length(assoc.sing$error)),
                             sd(unrel.sing$error) / sqrt(length(unrel.sing$error))))


cat(">>> ASSOCIATED VS. UNRELATED SINGULAR ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.sing)
cat(" ", "\n")

a.sing <- aov(error ~ related + Error(subj / related), data = sing)
print(summary(a.sing))
cat(" ", "\n")
cat(" ", "\n")



sink()

