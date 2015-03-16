rm(list = ls()) 
library(languageR)

# ====================================================================================================
# CATEGORY COORDINATE ITEMS ANALYSES ------------------------------------------------
# ====================================================================================================
#
# PREPARE DATA FILE FOR ANALYSES -------------------------------------------------
#
f2errout <- read.table("data/SR2_F2_errcat.txt", header = T)
d <- f2errout
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error") 

relat       <- subset(data.item, related  == "rel")
unrel       <- subset(data.item, related  == "unrel")
sing        <- subset(data.item, n2num    == "sing")
plur        <- subset(data.item, n2num    == "plur")
relat.plur  <- subset(data.item, related  == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related  == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.item, related  == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related  == "unrel" & n2num   == "sing")



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

  n = c(length( data.item$error),
        length( relat$error),
        length( unrel$error),
        length( plur$error),
        length( sing$error),
        length( relat.plur$error),
        length( relat.sing$error),
        length( unrel.plur$error),
        length( unrel.sing$error)),

  N = c(length( data.item$error),
        length( relat$error),
        length( unrel$error),
        length( plur$error),
        length( sing$error),
        length( relat.plur$error),
        length( relat.sing$error),
        length( unrel.plur$error),
        length( unrel.sing$error)),

  mean = c(mean( data.item$error),
           mean( relat$error),
           mean( unrel$error),
           mean( plur$error),
           mean( sing$error),
           mean( relat.plur$error),
           mean( relat.sing$error),
           mean( unrel.plur$error),
           mean( unrel.sing$error)),

  sd = c(sd( data.item$error),
         sd( relat$error),
         sd( unrel$error),
         sd( plur$error),
         sd( sing$error),
         sd( relat.plur$error),
         sd( relat.sing$error),
         sd( unrel.plur$error),
         sd( unrel.sing$error)),

  se = c(sd( data.item$error)  / sqrt( length( data.item$error)),
         sd( relat$error)      / sqrt( length( relat$error)),
         sd( unrel$error)      / sqrt( length( unrel$error)),
         sd( plur$error)       / sqrt( length( plur$error)),
         sd( sing$error)       / sqrt( length( sing$error)),
         sd( relat.plur$error) / sqrt( length( relat.plur$error) ),
         sd( relat.sing$error) / sqrt( length( relat.sing$error)),
         sd( unrel.plur$error) / sqrt( length( unrel.plur$error)),
         sd( unrel.sing$error) / sqrt( length( unrel.sing$error))
  ))



#--------------------------------2 X 2 ANOVA------------------------------------------------------


sink("output/SemRel2 F2 Factorial Analyses.txt")
cat(" ", "\n")
cat("BY-SUBJECTS FACTORIAL ANALYSES RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")
cat(rep(c("-"), times=40, quote=F),"\n")
cat("2X2 ANOVA: CATEGORY COORDINATES", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds)
cat(" ", "\n")

a.2x2 <- aov(error ~ related * n2num + Error(item / (related * n2num)), data = data.item)
print(summary(a.2x2))

cat(" ", "\n")
cat(" ", "\n")

#  ------RELATED vs. UNRELATED ITEMS PAIRED COMPARISONS--------------------------------
#
#  ------RELATED ITEMS------------------------------

ds.relat <- data.frame(data = c("n2num","plur","sing"),

                       n = c(length( relat$error),
                             length( relat.plur$error),
                             length( relat.sing$error)),

                       N = c(length( relat$error),
                             length( relat.plur$error),
                             length( relat.sing$error)),

                       mean = c(mean( relat$error),
                                mean( relat.plur$error),
                                mean( relat.sing$error)),

                       sd = c(sd( relat$error),
                              sd( relat.plur$error),
                              sd( relat.sing$error)),

                       se = c(sd( relat$error)      / sqrt( length( relat$error)),
                              sd( relat.plur$error) / sqrt( length( relat.plur$error)),
                              sd( relat.sing$error) / sqrt( length( relat.sing$error))))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("RELATED VS. UNRELATED ITEMS PAIRED COMPARISONS", fill = 60)
cat(rep(c("-"), times=40, quote=F),"\n")
cat(" ", "\n")
cat(" ", "\n")
cat(">>> RELATED ITEMS PARIED COMPARISONS", fill=60 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.relat)
cat(" ", "\n")

a.relat <- aov(error ~ n2num + Error(item / n2num), data = relat)
print(summary(a.relat))

cat(" ", "\n")
cat(" ", "\n")

# -----------------------UNRELATED ITEMS----------------
ds.unrel <- data.frame(data = c("n2num", "plur", "sing"),

                       n = c(length( unrel$error),
                             length( unrel.plur$error),
                             length( unrel.sing$error)),

                       N = c(length( unrel$error),
                             length( unrel.plur$error),
                             length( unrel.sing$error)),

                       mean = c(mean( unrel$error),
                                mean( unrel.plur$error),
                                mean( unrel.sing$error)),

                       sd = c(sd( unrel$error),
                              sd( unrel.plur$error),
                              sd( unrel.sing$error)),

                       se = c(sd( unrel$error)      / sqrt( length( unrel$error)),
                              sd( unrel.plur$error) / sqrt( length( unrel.plur$error)),
                              sd( unrel.sing$error) / sqrt( length( unrel.sing$error))))

cat(">>> UNRELATED ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.unrel)
cat(" ", "\n")

a.unrel <- aov(error ~ n2num + Error(item / n2num), data = unrel)
print(summary(a.unrel))


cat(" ", "\n")
cat(" ", "\n")


#
# -----------------------------Paired comparisions of Plural - Singular items
#
#------------Plural Items
#

f2errout <- read.table("data/SR2_F2_errcat.txt", header = T)

d <- f2errout
d <- subset(d, n2num !="sing")
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")
relat       <- subset(data.item, related  == "rel")
unrel       <- subset(data.item, related  == "unrel")
sing        <- subset(data.item, n2num    == "sing")
plur        <- subset(data.item, n2num    == "plur")
relat.plur  <- subset(data.item, related  == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related  == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.item, related  == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related  == "unrel" & n2num   == "sing")

ds.plur <- data.frame(data = c("Related","Rel","unrel"),

                      n = c(length( plur$error),
                            length( relat.plur$error),
                            length( unrel.plur$error)),

                      N = c(length( plur$error),
                            length( relat.plur$error),
                            length( unrel.plur$error)),

                      mean = c(mean( plur$error),
                               mean( relat.plur$error),
                               mean( unrel.plur$error)),

                      sd = c(sd( plur$error),
                             sd( relat.plur$error),
                             sd( unrel.plur$error)),

                      se = c(sd( plur$error)       / sqrt( length( plur$error)),
                             sd( relat.plur$error) / sqrt( length( relat.plur$error)),
                             sd( unrel.plur$error) / sqrt( length( unrel.plur$error))))


cat(rep(c("-"), times=40, quote=F),"\n")
cat("PLURAL VS. SINGULAR PAIRED COMPARISONS", fill = 50)
cat(rep(c("-"), times=40, quote=F),"\n")
cat(" ", "\n")
cat(" ", "\n")
cat(">>>  PLURAL ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.plur)
cat(" ", "\n")

a.plur <- aov(error ~ related + Error(item / related), data = plur)
print(summary(a.plur))
cat(" ", "\n")
cat(" ", "\n")


# -------------------------------Singular Items----------------------
f2errout <- read.table("data/SR2_F2_errcat.txt", header = T)
d <- f2errout
d <- subset(d, n2num !="plur")
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")

relat       <- subset(data.item, related  == "rel")
unrel       <- subset(data.item, related  == "unrel")
sing        <- subset(data.item, n2num    == "sing")
plur        <- subset(data.item, n2num    == "plur")
relat.plur  <- subset(data.item, related  == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related  == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.item, related  == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related  == "unrel" & n2num   == "sing")
ds.sing <- data.frame(data = c("n2num","plur","sing"),
                      
                      n = c(length( sing$error),
                            length( relat.sing$error),
                            length( unrel.sing$error)),
                      
                      N = c(length( sing$error),
                            length( relat.sing$error),
                            length( unrel.sing$error)),
                      
                      mean = c(mean( sing$error),
                               mean( relat.sing$error),
                               mean( unrel.sing$error)),
                      
                      sd = c(sd( sing$error),
                             sd( relat.sing$error),
                             sd( unrel.sing$error)),
                      
                      se = c(sd( sing$error)       / sqrt( length( sing$error)),
                             sd( relat.sing$error) / sqrt( length( relat.sing$error)),
                             sd( unrel.sing$error) / sqrt( length( unrel.sing$error))))


cat(">>>  SINGULAR ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.sing)
cat(" ", "\n")

a.sing <- aov(error ~ related + Error(item / related), data = sing)
print(summary(a.sing))
cat(" ", "\n")
cat(" ", "\n")
cat("\n", rep(c("//\\\\"), times = 25, quote = F),"\n")  # ==================CATEGORY ABOVE & PROP BELOW ==========================

cat(" ", "\n")
cat(" ", "\n")

# ====================================================================================================
# PROPERTY ITEMS ANALYSES -----------------------------------------------------
# ====================================================================================================

f2errout <- read.table("data/SR2_F2_errprop.txt", header = T)
d <- f2errout
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")
relat       <- subset(data.item, related == "rel")
assoc       <- subset(data.item, related == "assoc")
unrel       <- subset(data.item, related == "unrel")
sing        <- subset(data.item, n2num   == "sing")
plur        <- subset(data.item, n2num   == "plur")
relat.plur  <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related == "rel"   & n2num   == "sing")
assoc.plur  <- subset(data.item, related == "assoc" & n2num   == "plur")
assoc.sing  <- subset(data.item, related == "assoc" & n2num   == "sing")
unrel.plur  <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related == "unrel" & n2num   == "sing")


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

  n = c(length( data.item$error),
        length( assoc$error),
        length( relat$error),
        length( unrel$error),
        length( plur$error),
        length( sing$error),
        length( assoc.plur$error),
        length( assoc.sing$error),
        length( relat.plur$error),
        length( relat.sing$error),
        length( unrel.plur$error),
        length( unrel.sing$error)),

  N = c(length( data.item$error),
        length( assoc$error),
        length( relat$error),
        length( unrel$error),
        length( plur$error),
        length( sing$error),
        length( assoc.plur$error),
        length( assoc.sing$error),
        length( relat.plur$error),
        length( relat.sing$error),
        length( unrel.plur$error),
        length( unrel.sing$error)),

  mean = c(mean( data.item$error),
           mean( assoc$error),
           mean( relat$error),
           mean( unrel$error),
           mean( plur$error),
           mean( sing$error),
           mean( assoc.plur$error),
           mean( assoc.sing$error),
           mean( relat.plur$error),
           mean( relat.sing$error),
           mean( unrel.plur$error),
           mean( unrel.sing$error)),

  sd = c(sd( data.item$error),
         sd( assoc$error),
         sd( relat$error),
         sd( unrel$error),
         sd( plur$error),
         sd( sing$error),
         sd( assoc.plur$error),
         sd( assoc.sing$error),
         sd( relat.plur$error),
         sd( relat.sing$error),
         sd( unrel.plur$error),
         sd( unrel.sing$error)),

  se = c(sd( data.item$error)  / sqrt( length( data.item$error)),
         sd( assoc$error)      / sqrt( length( assoc$error)),
         sd( relat$error)      / sqrt( length( relat$error)),
         sd( unrel$error)      / sqrt( length( unrel$error)),
         sd( plur$error)       / sqrt( length( plur$error)),
         sd( sing$error)       / sqrt( length( sing$error)),
         sd( assoc.plur$error) / sqrt( length( assoc.plur$error)),
         sd( assoc.sing$error) / sqrt( length( assoc.sing$error)),
         sd( relat.plur$error) / sqrt( length( relat.plur$error)),
         sd( relat.sing$error) / sqrt( length( relat.sing$error)),
         sd( unrel.plur$error) / sqrt( length( unrel.plur$error)),
         sd( unrel.sing$error) / sqrt( length( unrel.sing$error))
  ))

#
# --------------------------------3 X 2  ANOVA------------------------------------------------------
#
cat(rep(c("-"), times=40, quote=F),"\n")
cat("2X2 ANOVA: PROPERTY ITEMS", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds)
cat(" ", "\n")
a.3x2 <- aov(error ~ related * n2num + Error(item / (related * n2num)), data = data.item)
print(summary(a.3x2))
cat(" ", "\n")
cat(" ", "\n")

#
# ========================================================================================================
#
# --------------------RELATED - ASSOCIATED ITEMS --------------------------------------

f2errout <- read.table("data/SR2_F2_errprop.txt", header = T)
d <- f2errout
d <- subset(d, related != "unrel")
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")
relat       <- subset(data.item, related == "rel")
assoc       <- subset(data.item, related == "assoc")
unrel       <- subset(data.item, related == "unrel")
sing        <- subset(data.item, n2num   == "sing")
plur        <- subset(data.item, n2num   == "plur")
relat.plur  <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related == "rel"   & n2num   == "sing")
assoc.plur  <- subset(data.item, related == "assoc" & n2num   == "plur")
assoc.sing  <- subset(data.item, related == "assoc" & n2num   == "sing")
unrel.plur  <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related == "unrel" & n2num   == "sing")

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

  n = c(length( data.item$error),
        length( assoc$error),
        length( relat$error),
        length( plur$error),
        length( sing$error),
        length( assoc.plur$error),
        length( assoc.sing$error),
        length( relat.plur$error),
        length( relat.sing$error)), 

  N = c(length( data.item$error),
        length( assoc$error),
        length( relat$error),
        length( plur$error),
        length( sing$error),
        length( assoc.plur$error),
        length( assoc.sing$error),
        length( relat.plur$error),
        length( relat.sing$error)), 

  mean = c(mean( data.item$error),
           mean( assoc$error),
           mean( relat$error),
           mean( plur$error),
           mean( sing$error),
           mean( assoc.plur$error),
           mean( assoc.sing$error),
           mean( relat.plur$error),
           mean( relat.sing$error)), 

  sd = c(sd( data.item$error),
         sd( assoc$error),
         sd( relat$error),
         sd( plur$error),
         sd( sing$error),
         sd( assoc.plur$error),
         sd( assoc.sing$error),
         sd( relat.plur$error),
         sd( relat.sing$error)),

  se = c(sd( data.item$error)  / sqrt( length( data.item$error)),
         sd( assoc$error)      / sqrt( length( assoc$error)),
         sd( relat$error)      / sqrt( length( relat$error)),
         sd( plur$error)       / sqrt( length( plur$error)),
         sd( sing$error)       / sqrt( length( sing$error)),
         sd( assoc.plur$error) / sqrt( length( assoc.plur$error)),
         sd( assoc.sing$error) / sqrt( length( assoc.sing$error)),
         sd( relat.plur$error) / sqrt( length( relat.plur$error)),
         sd( relat.sing$error) / sqrt( length( relat.sing$error))
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

a.assrel <- aov(error ~ related * n2num + Error(item / (related * n2num)), data = data.item)
print(summary(a.assrel))

cat(" ", "\n")
cat(" ", "\n")


# --------------------RELATED - UNRELATED ITEMS --------------------------------------

f2errout <- read.table("data/SR2_F2_errprop.txt", header = T)
d <- f2errout
d <- subset(d, related != "assoc")
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")

relat       <- subset(data.item, related == "rel")
assoc       <- subset(data.item, related == "assoc")
unrel       <- subset(data.item, related == "unrel")
sing        <- subset(data.item, n2num   == "sing")
plur        <- subset(data.item, n2num   == "plur")
relat.plur  <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related == "rel"   & n2num   == "sing")
assoc.plur  <- subset(data.item, related == "assoc" & n2num   == "plur")
assoc.sing   <- subset(data.item, related == "assoc" & n2num   == "sing")
unrel.plur  <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related == "unrel" & n2num   == "sing")

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

  n = c( length( data.item$error),
        length( relat$error),
        length( unrel$error),
        length( plur$error),
        length( sing$error),
        length( relat.plur$error),
        length( relat.sing$error),
        length( unrel.plur$error),
        length( unrel.sing$error)),

  N = c( length( data.item$error),
        length( relat$error),
        length( unrel$error),
        length( plur$error),
        length( sing$error),
        length( relat.plur$error),
        length( relat.sing$error),
        length( unrel.plur$error),
        length( unrel.sing$error)),

  mean = c( mean( data.item$error),
           mean( relat$error),
           mean( unrel$error),
           mean( plur$error),
           mean( sing$error),
           mean( relat.plur$error),
           mean( relat.sing$error),
           mean( unrel.plur$error),
           mean( unrel.sing$error)),

  sd = c( sd( data.item$error),
         sd( relat$error),
         sd( unrel$error),
         sd( plur$error),
         sd( sing$error),
         sd( relat.plur$error),
         sd( relat.sing$error),
         sd( unrel.plur$error),
         sd( unrel.sing$error)),

  se = c( sd( data.item$error) / sqrt( length( data.item$error)),
         sd( relat$error)      / sqrt( length( relat$error)),
         sd( unrel$error)      / sqrt( length( unrel$error)),
         sd( plur$error)       / sqrt( length( plur$error)),
         sd( sing$error)       / sqrt( length( sing$error)),
         sd( relat.plur$error) / sqrt( length( relat.plur$error)),
         sd( relat.sing$error) / sqrt( length( relat.sing$error)),
         sd( unrel.plur$error) / sqrt( length( unrel.plur$error)),
         sd( unrel.sing$error) / sqrt( length( unrel.sing$error))
  ))


cat(">>>  RELATED VS. UNRELATED", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(relunr)
cat(" ", "\n")

a.relunr <- aov(error ~ related * n2num + Error(item / (related * n2num)), data = data.item)
print(summary(a.relunr))
cat(" ", "\n")
cat(" ", "\n")


# -------------------- ASSOCIATED - UNRELATED ITEMS --------------------------------------

f2errout <- read.table("data/SR2_F2_errprop.txt", header = T)
d <- f2errout
d <- subset(d, related != "rel")
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")
relat       <- subset(data.item, related == "rel")
assoc       <- subset(data.item, related == "assoc")
unrel       <- subset(data.item, related == "unrel")
sing        <- subset(data.item, n2num   == "sing")
plur        <- subset(data.item, n2num   == "plur")
relat.plur  <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related == "rel"   & n2num   == "sing")
assoc.plur  <- subset(data.item, related == "assoc" & n2num   == "plur")
assoc.sing   <- subset(data.item, related == "assoc" & n2num   == "sing")
unrel.plur  <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related == "unrel" & n2num   == "sing")


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

  n = c( length( data.item$error),
        length( assoc$error),
        length( unrel$error),
        length( plur$error),
        length( sing$error),
        length( assoc.plur$error),
        length( assoc.sing$error),
        length( unrel.plur$error),
        length( unrel.sing$error)),

  N = c( length( data.item$error),
        length( assoc$error),
        length( unrel$error),
        length( plur$error),
        length( sing$error),
        length( assoc.plur$error),
        length( assoc.sing$error),
        length( unrel.plur$error),
        length( unrel.sing$error)),

  mean = c( mean( data.item$error),
           mean( assoc$error),
           mean( unrel$error),
           mean( plur$error),
           mean( sing$error),
           mean( assoc.plur$error),
           mean( assoc.sing$error),
           mean( unrel.plur$error),
           mean( unrel.sing$error)),

  sd = c( sd( data.item$error),
         sd( assoc$error),
         sd( unrel$error),
         sd( plur$error),
         sd( sing$error),
         sd( assoc.plur$error),
         sd( assoc.sing$error),
         sd( unrel.plur$error),
         sd( unrel.sing$error)),

  se = c( sd( data.item$error) / sqrt( length( data.item$error)),
         sd( assoc$error)      / sqrt( length( assoc$error)),
         sd( unrel$error)      / sqrt( length( unrel$error)),
         sd( plur$error)       / sqrt( length( plur$error)),
         sd( sing$error)       / sqrt( length( sing$error)),
         sd( assoc.plur$error) / sqrt( length( assoc.plur$error)),
         sd( assoc.sing$error) / sqrt( length( assoc.sing$error)),
         sd( unrel.plur$error) / sqrt( length( unrel.plur$error)),
         sd( unrel.sing$error) / sqrt( length( unrel.sing$error))
  ))

cat(">>>  ASSOCIATED VS. UNRELATED", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(assunr)
cat(" ", "\n")

a.assunr <- aov(error ~ related * n2num + Error(item / (related * n2num)), data = data.item)
print(summary(a.assunr))

cat(" ", "\n")
cat(" ", "\n")  # ==========================================================================================


#
# --------------------------------- PAIRED COMPARISONS---------------------
#

f2errout <- read.table("data/SR2_F2_errprop.txt", header = T)
d <- f2errout
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")

relat       <- subset(data.item, related == "rel")
assoc       <- subset(data.item, related == "assoc")
unrel       <- subset(data.item, related == "unrel")
sing        <- subset(data.item, n2num   == "sing")
plur        <- subset(data.item, n2num   == "plur")
relat.plur  <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related == "rel"   & n2num   == "sing")
assoc.plur  <- subset(data.item, related == "assoc" & n2num   == "plur")
assoc.sing   <- subset(data.item, related == "assoc" & n2num   == "sing")
unrel.plur  <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related == "unrel" & n2num   == "sing")


# # ------------RELATED ITEMS------------------------------
#
ds.relat <- data.frame(data = c("n2num","plur","sing"),

                       n = c( length( relat$error),
                              length( relat.plur$error),
                              length( relat.sing$error)),

                       N = c( length( relat$error),
                              length( relat.plur$error),
                              length( relat.sing$error)),

                    mean = c( mean( relat$error),
                              mean( relat.plur$error),
                              mean( relat.sing$error)),

                       sd = c( sd( relat$error),
                               sd( relat.plur$error),
                               sd( relat.sing$error)),

                       se = c( sd( relat$error)      / sqrt( length( relat$error)),
                               sd( relat.plur$error) / sqrt( length( relat.plur$error)),
                               sd( relat.sing$error) / sqrt( length( relat.sing$error))))


cat(rep(c("-"), times=40, quote=F),"\n")
cat("PAIRED COMPARISONS", fill = 50)
cat(rep(c("-"), times=40, quote=F),"\n")
cat(" ", "\n")
cat(" ", "\n")
cat(">>>  RELATED SING. VS. PLUR", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.relat)
cat(" ", "\n")

a.relat <- aov(error ~ n2num + Error(item / n2num), data = relat)
print(summary(a.relat))
cat(" ", "\n")
cat(" ", "\n")


# # ------------ASSOC ITEMS------------------------------
#

ds.assoc <- data.frame(data = c("n2num","plur","sing"),

                       n = c( length( assoc$error),
                              length( assoc.plur$error),
                              length( assoc.sing$error)),

                       N = c( length( assoc$error),
                              length( assoc.plur$error),
                              length( assoc.sing$error)),

                     mean = c( mean( assoc$error),
                               mean( assoc.plur$error),
                               mean( assoc.sing$error)),

                       sd = c( sd( assoc$error),
                               sd( assoc.plur$error),
                               sd( assoc.sing$error)),

                       se = c( sd( assoc$error)     / sqrt( length( assoc$error)),
                               sd( assoc.plur$error) / sqrt( length( assoc.plur$error)),
                               sd( assoc.sing$error) / sqrt( length( assoc.sing$error))))


cat(">>>  ASSOCIATED SING. VS. PLUR", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.assoc)
cat(" ", "\n")


a.assoc <- aov(error ~ n2num + Error(item / n2num), data = relat)
print(summary(a.assoc))
cat(" ", "\n")
cat(" ", "\n")



# -----------------------UNRELATED ITEMS----------------
ds.unrel <- data.frame(data = c("n2num", "plur", "sing"),

                       n = c( length( unrel$error),
                              length( unrel.plur$error),
                              length( unrel.sing$error)),

                       N = c( length( unrel$error),
                              length( unrel.plur$error),
                              length( unrel.sing$error)),
                       
                    mean = c( mean( unrel$error),
                              mean( unrel.plur$error),
                              mean( unrel.sing$error)),

                       sd = c( sd( unrel$error),
                               sd( unrel.plur$error),
                               sd( unrel.sing$error)),
 
                       se = c( sd( unrel$error)     / sqrt( length( unrel$error)),
                               sd( unrel.plur$error) / sqrt( length( unrel.plur$error)),
                               sd( unrel.sing$error) / sqrt( length( unrel.sing$error))))

cat(">>>  UNRELATED SING. VS. PLUR", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.unrel)
cat(" ", "\n")

a.unrel <- aov(error ~ n2num + Error(item / n2num), data = unrel)
print(summary(a.unrel))

cat(" ", "\n")
cat(" ", "\n")

# -----------------------------SINGULAR VS. PLURAL PAIRED COMPARISONS
#
#------------Plural Items
#
f2errout <- read.table("data/SR2_F2_errprop.txt", header = T)
d <- f2errout
d <- subset(d, n2num !="sing")
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")


assoc       <- subset(data.item, related  ==  "assoc")
relat       <- subset(data.item, related  ==  "rel")
unrel       <- subset(data.item, related  ==  "unrel")
sing        <- subset(data.item, n2num    ==  "sing")
plur        <- subset(data.item, n2num    ==  "plur")
assoc.plur  <- subset(data.item, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.item, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related == "unrel" & n2num   == "sing")


ds.plur <- data.frame(data = c("Related","Assoc","Rel","Unrel"),

                      n = c( length( plur$error),
                             length( assoc.plur$error),
                             length( relat.plur$error),
                             length( unrel.plur$error)),

                      N = c( length( plur$error),
                             length( assoc.plur$error),
                             length( relat.plur$error),
                             length( unrel.plur$error)),
                      
                    mean = c( mean( plur$error),
                              mean( assoc.plur$error),
                              mean( relat.plur$error),
                              mean( unrel.plur$error)),

                      sd = c( sd( plur$error),
                              sd( assoc.plur$error),
                              sd( relat.plur$error),
                              sd( unrel.plur$error)),

                      se = c( sd( plur$error)      / sqrt( length( plur$error)),
                              sd( assoc.plur$error) / sqrt( length( assoc.plur$error)),
                              sd( relat.plur$error) / sqrt( length( relat.plur$error)),
                              sd( unrel.plur$error) / sqrt( length( unrel.plur$error))))


cat(rep(c("-"), times=40, quote=F),"\n")
cat("PLURAL VS. SINGULAR PAIRED COMPARISONS", fill = 50)
cat(rep(c("-"), times=40, quote=F),"\n")
cat(" ", "\n")
cat(" ", "\n")
cat(">>>  PLURAL ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.plur)
cat(" ", "\n")

a.plur <- aov(error ~ related + Error(item / related), data = plur)
print(summary(a.plur))

cat(" ", "\n")
cat(" ", "\n")
#
# ---------------------------------------Singular Items-------------------------
#

f2errout <- read.table("data/SR2_F2_errprop.txt", header = T)
d <- f2errout
d <- subset(d, n2num !="plur")
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)

colnames(data.item) <- c("item", "related", "n2num", "error")

assoc       <- subset(data.item, related  ==  "assoc")
relat       <- subset(data.item, related  ==  "rel")
unrel       <- subset(data.item, related  ==  "unrel")
sing        <- subset(data.item, n2num    ==  "sing")
plur        <- subset(data.item, n2num    ==  "plur")
assoc.plur  <- subset(data.item, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.item, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related == "unrel" & n2num   == "sing")

ds.sing <- data.frame(data = c("Related","Assoc","Rel","Unrel"),

                      n = c( length( sing$error),
                             length( assoc.sing$error),
                             length( relat.sing$error),
                             length( unrel.sing$error)),

                      N = c( length( sing$error),
                             length( assoc.sing$error),
                             length( relat.sing$error),
                             length( unrel.sing$error)),

                    mean = c( mean( sing$error),
                              mean( assoc.sing$error),
                              mean( relat.sing$error),
                              mean( unrel.sing$error)),

                      sd = c( sd( plur$error),
                              sd( assoc.sing$error),
                              sd( relat.sing$error),
                              sd( unrel.sing$error)),

                      se = c( sd( sing$error)       / sqrt( length( sing$error)),
                              sd( assoc.sing$error) / sqrt( length( assoc.sing$error)),
                              sd( relat.sing$error) / sqrt( length( relat.sing$error)),
                              sd( unrel.sing$error) / sqrt( length( unrel.sing$error))))

cat(">>> SINGULAR ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.sing)
cat(" ", "\n")


a.sing <- aov(error ~ related + Error(item / related), data = sing)
print(summary(a.sing))
cat(" ", "\n")
cat(" ", "\n")

# ------------------SUBSET PAIRED COMPARIONS---------------------
# -------- ASSOCIATED VS. RELATED PLURAL

f2errout <- read.table("data/SR2_F2_errprop.txt", header = T)
d <- f2errout
d <- subset(d, n2num !="sing" & related != "unrel")
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")


assoc       <- subset(data.item, related  ==  "assoc")
relat       <- subset(data.item, related  ==  "rel")
unrel       <- subset(data.item, related  ==  "unrel")
sing        <- subset(data.item, n2num    ==  "sing")
plur        <- subset(data.item, n2num    ==  "plur")
assoc.plur  <- subset(data.item, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.item, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related == "unrel" & n2num   == "sing")


ds.plur <- data.frame(data = c("Related","Assoc","Relat"),

                      n = c( length( plur$error),
                             length( assoc.plur$error),
                             length( relat.plur$error)),

                      N = c( length( plur$error),
                             length( assoc.plur$error),
                             length( relat.plur$error)),

                    mean = c( mean( plur$error),
                              mean( assoc.plur$error),
                              mean( relat.plur$error)),

                      sd = c( sd( plur$error),
                              sd( assoc.plur$error),
                              sd( relat.plur$error)),

                      se = c( sd( plur$error)       / sqrt( length( plur$error)),
                              sd( assoc.plur$error) / sqrt( length( assoc.plur$error)),
                              sd( relat.plur$error) / sqrt( length( relat.plur$error))))

cat(">>>  ASSOCIATED VS. RELATED PLURAL ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.plur)
cat(" ", "\n")

a.plur <- aov(error ~ related + Error(item / related), data = plur)
print(summary(a.plur))
cat(" ", "\n")
cat(" ", "\n")

# ----------------------RELATED VS. UNRELATED PLURAL
f2errout <- read.table("data/SR2_F2_errprop.txt", header = T)
d <- f2errout
d <- subset(d, n2num !="sing" & related != "assoc")
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")


assoc       <- subset(data.item, related  ==  "assoc")
relat       <- subset(data.item, related  ==  "rel")
unrel       <- subset(data.item, related  ==  "unrel")
sing        <- subset(data.item, n2num    ==  "sing")
plur        <- subset(data.item, n2num    ==  "plur")
assoc.plur  <- subset(data.item, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.item, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related == "unrel" & n2num   == "sing")


ds.plur <- data.frame(data = c("Related","Relat","Unrel"),

                      n = c( length( plur$error),
                             length( relat.plur$error),
                             length( unrel.plur$error)),

                      N = c( length( plur$error),
                             length( relat.plur$error),
                             length( unrel.plur$error)),

                    mean = c( mean( plur$error),
                              mean( relat.plur$error),
                              mean( unrel.plur$error)),

                      sd = c( sd( plur$error),
                              sd( relat.plur$error),
                              sd( unrel.plur$error)),

                      se = c( sd( plur$error) / sqrt( length( plur$error)),
                              sd( relat.plur$error) / sqrt( length( relat.plur$error)),
                              sd( unrel.plur$error) / sqrt( length( unrel.plur$error))))


cat(">>>  RELATED VS. UNRELATED PLURAL ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.plur)
cat(" ", "\n")

a.plur <- aov(error ~ related + Error(item / related), data = plur)
print(summary(a.plur))
cat(" ", "\n")
cat(" ", "\n")


# ----------------------ASSOCIATED VS. UNRELATED PLURAL
f2errout <- read.table("data/SR2_F2_errprop.txt", header = T)
d <- f2errout
d <- subset(d, n2num !="sing" & related != "rel")
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")


assoc       <- subset(data.item, related  ==  "assoc")
relat       <- subset(data.item, related  ==  "rel")
unrel       <- subset(data.item, related  ==  "unrel")
sing        <- subset(data.item, n2num    ==  "sing")
plur        <- subset(data.item, n2num    ==  "plur")
assoc.plur  <- subset(data.item, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.item, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related == "unrel" & n2num   == "sing")


ds.plur <- data.frame(data = c("Related","Assoc","Unrel"),

                      n = c( length( plur$error),
                             length( assoc.plur$error),
                             length( unrel.plur$error)),

                      N = c( length( plur$error),
                             length( assoc.plur$error),
                             length( unrel.plur$error)),
                      
                    mean = c( mean( plur$error),
                              mean( assoc.plur$error),
                              mean( unrel.plur$error)),

                      sd = c( sd( plur$error),
                              sd( assoc.plur$error),
                              sd( unrel.plur$error)),

                      se = c( sd( plur$error)       / sqrt( length( plur$error)),
                              sd( assoc.plur$error) / sqrt( length( assoc.plur$error)),
                              sd( unrel.plur$error) / sqrt( length( unrel.plur$error))))


cat(">>>  ASSOCIATED VS. UNRELATED PLURAL ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.plur)
cat(" ", "\n")

a.plur <- aov(error ~ related + Error(item / related), data = plur)
print(summary(a.plur))
cat(" ", "\n")
cat(" ", "\n")


# -------- ASSOCIATED VS. RELATED SINGULAR

f2errout <- read.table("data/SR2_F2_errprop.txt", header = T)
d <- f2errout
d <- subset(d, n2num != "plur" & related != "unrel")
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")


assoc       <- subset(data.item, related  ==  "assoc")
relat       <- subset(data.item, related  ==  "rel")
unrel       <- subset(data.item, related  ==  "unrel")
sing        <- subset(data.item, n2num    ==  "sing")
plur        <- subset(data.item, n2num    ==  "plur")
assoc.plur  <- subset(data.item, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.item, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related == "unrel" & n2num   == "sing")


ds.sing <- data.frame(data = c("Related","Assoc","Relat"),

                      n = c( length( sing$error),
                             length( assoc.sing$error),
                             length( relat.sing$error)),

                      N = c( length( sing$error),
                             length( assoc.sing$error),
                             length( relat.sing$error)),

                    mean = c( mean( sing$error),
                              mean( assoc.sing$error),
                              mean( relat.sing$error)),

                      sd = c( sd( sing$error),
                              sd( assoc.sing$error),
                              sd( relat.sing$error)),

                      se = c( sd( sing$error) / sqrt( length( sing$error)),
                              sd( assoc.sing$error) / sqrt( length( assoc.sing$error)),
                              sd( relat.sing$error) / sqrt( length( relat.sing$error))))

cat(">>>  ASSOCIATED VS. RELATED SINGULAR ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.sing)
cat(" ", "\n")

a.sing <- aov(error ~ related + Error(item / related), data = sing)
print(summary(a.sing))
cat(" ", "\n")
cat(" ", "\n")

# ----------------------RELATED VS. UNRELATED SINGULAR
f2errout <- read.table("data/SR2_F2_errprop.txt", header = T)
d <- f2errout
d <- subset(d, n2num != "plur" & related != "assoc")
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")


assoc       <- subset(data.item, related  ==  "assoc")
relat       <- subset(data.item, related  ==  "rel")
unrel       <- subset(data.item, related  ==  "unrel")
sing        <- subset(data.item, n2num    ==  "sing")
plur        <- subset(data.item, n2num    ==  "plur")
assoc.plur  <- subset(data.item, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.item, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related == "unrel" & n2num   == "sing")


ds.sing <- data.frame(data = c("Related","Relat","Unrel"),

                      n = c( length( sing$error),
                            length( relat.sing$error),
                            length( unrel.sing$error)),

                      N = c( length( sing$error),
                             length( relat.sing$error),
                             length( unrel.sing$error)),

                      mean = c( mean( sing$error),
                                mean( relat.sing$error),
                                mean( unrel.sing$error)),

                      sd = c( sd( sing$error),
                              sd( relat.sing$error),
                              sd( unrel.sing$error)),

                      se = c( sd( sing$error)       / sqrt( length( sing$error)),
                              sd( relat.sing$error) / sqrt( length( relat.sing$error)),
                              sd( unrel.sing$error) / sqrt( length( unrel.sing$error))))


cat(">>>  RELATED VS. UNRELATED SINGULAR ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.sing)
cat(" ", "\n")

a.sing <- aov(error ~ related + Error(item / related), data = sing)
print(summary(a.sing))
cat(" ", "\n")
cat(" ", "\n")


# ----------------------ASSOCIATED VS. UNRELATED SINGULAR
f2errout <- read.table("data/SR2_F2_errprop.txt", header = T)
d <- f2errout
d <- subset(d, n2num != "plur" & related != "rel")
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error")


assoc       <- subset(data.item, related  ==  "assoc")
relat       <- subset(data.item, related  ==  "rel")
unrel       <- subset(data.item, related  ==  "unrel")
sing        <- subset(data.item, n2num    ==  "sing")
plur        <- subset(data.item, n2num    ==  "plur")
assoc.plur  <- subset(data.item, related == "assoc"   & n2num   == "plur")
assoc.sing  <- subset(data.item, related == "assoc"   & n2num   == "sing")
relat.plur  <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related == "unrel" & n2num   == "sing")


ds.sing <- data.frame(data = c("Related","Assoc","Unrel"),

                      n = c( length( sing$error),
                             length( assoc.sing$error),
                             length( unrel.sing$error)),

                      N = c( length( sing$error),
                             length( assoc.sing$error),
                             length( unrel.sing$error)),

                    mean = c( mean( sing$error),
                              mean( assoc.sing$error),
                              mean( unrel.sing$error)),

                      sd = c( sd( sing$error),
                              sd( assoc.sing$error),
                              sd( unrel.sing$error)),

                      se = c( sd( sing$error)      / sqrt( length( sing$error)),
                              sd( assoc.sing$error) / sqrt( length( assoc.sing$error)),
                              sd( unrel.sing$error) / sqrt( length( unrel.sing$error))))


cat(">>> ASSOCIATED VS. UNRELATED SINGULAR ITEMS PARIED COMPARISONS", fill=50 )
cat(rep(c("-"), times=25, quote=F),"\n")
print(ds.sing)
cat(" ", "\n")

a.sing <- aov(error ~ related + Error(item / related), data = sing)
print(summary(a.sing))
cat(" ", "\n")
cat(" ", "\n")
sink()

# =============================================================================================
# FIGURES -----------------------------------------------------------------------------------


rm(list = ls()) 
library(languageR)
library(ggplot2)
library(grid)

# CATEGORY COORDINATES FIGURE
f2caterr  <- read.table("data/SR2_F2_errcat.txt", header = T) 
d.cat      <- f2caterr
d.cat$pct  <- ifelse(d.cat$errd == 0 & d.cat$errcord == 0, 0, (d.cat$errd / (d.cat$errcord)) * 100)
cat.pl     <- subset(d.cat, related == "rel"   & n2num == "plur")
cat.s      <- subset(d.cat, related == "rel"   & n2num == "sing")
non.cat.pl <- subset(d.cat, related == "unrel" & n2num == "plur")
non.cat.s  <- subset(d.cat, related == "unrel" & n2num == "sing")


data.item.cat <- aggregate(d.cat$pct, list(d.cat$item, d.cat$related, d.cat$n2num ), mean)
colnames(data.item.cat) <- c("item", "related", "n2num", "error") 
cat.rel.plur  <- subset(data.item.cat, related == "rel"   & n2num   == "plur")
cat.rel.sing  <- subset(data.item.cat, related == "rel"   & n2num   == "sing")
cat.unr.plur  <- subset(data.item.cat, related == "unrel" & n2num   == "plur")
cat.unr.sing  <- subset(data.item.cat, related == "unrel" & n2num   == "sing")


df.cat <- data.frame(
  Relatedness = c("Category Coordinate","Category Coordinate","Non-Coordinate","Non-Coordinate"),
  Number = c("Singular","Plural","Singular","Plural"),
  Error = c(sum( cat.s$errd),
            sum( cat.pl$errd),
            sum( non.cat.s$errd),
            sum( non.cat.pl$errd)),
  
  Correct = c(sum( cat.s$errcord)      - sum( cat.s$errd),
              sum( cat.pl$errcord)     - sum( cat.pl$errd),
              sum( non.cat.s$errcord)  - sum( non.cat.s$errd),
              sum( non.cat.pl$errcord) - sum( non.cat.pl$errd)),
  
  
  All = c(sum( cat.s$errcord),
          sum( cat.pl$errcord),
          sum( non.cat.s$errcord),
          sum( non.cat.pl$errcord)),
  
  Rate = c(sum( cat.s$errd)      / sum( cat.s$errcord),
           sum( cat.pl$errd)     / sum( cat.pl$errcord),
           sum( non.cat.s$errd)  / sum( non.cat.s$errcord),
           sum( non.cat.pl$errd) / sum( non.cat.pl$errcord)),
  
  ErrPer = c(  (sum( cat.s$errd)      / sum( cat.s$errcord))  * 100,
               (sum( cat.pl$errd)     / sum( cat.pl$errcord)) * 100,
               (sum( non.cat.s$errd)  / sum( non.cat.s$errcord))  * 100,
               (sum( non.cat.pl$errd) / sum( non.cat.pl$errcord)) * 100),
  
  SE = c(
         sd( cat.rel.plur$error) / sqrt( length( cat.rel.plur$error)),
         sd( cat.rel.sing$error) / sqrt( length( cat.rel.sing$error)),
         sd( cat.unr.plur$error) / sqrt( length( cat.unr.plur$error)),
         sd( cat.unr.sing$error) / sqrt( length( cat.unr.sing$error))
  ))

View(df.cat)

# CATEGORY MIS-MATCH EFFECTS TABLE & FIG ----------------------------
se.cat.dat <- read.table("data/SR2_cat_SEdata.txt", header=TRUE) 
se.cat.dat$error <- ifelse(se.cat.dat$errd == 0 & se.cat.dat$errcord == 0, 0, (se.cat.dat$errd / (se.cat.dat$errcord)) * 100)
se.cat.rel   <- subset(se.cat.dat, related == "rel"   & n2num == "plur")
se.cat.unr   <- subset(se.cat.dat, related == "unrel" & n2num == "plur")



cat.mis.eff <-data.frame(
  Relatedness = c( "Category Coordinate", "Non-Coordinate"),
  ErrRate = c( 
    ( df.cat[2, 7] - df.cat[1, 7]),
    ( df.cat[4, 7] - df.cat[3, 7])),
  
  SE = c(
    sd( se.cat.rel$error) / sqrt( length( se.cat.rel$error)),
    sd( se.cat.unr$error) / sqrt( length( se.cat.unr$error)))
)

View(cat.mis.eff)

# PROPERTY FIGURES ----------------
f2properr  <- read.table("data/SR2_F2_errprop.txt", header = T) 
d.prop     <- f2properr 
d.prop$pct <- ifelse(d.prop$errd == 0 & d.prop$errcord == 0, 0, (d.prop$errd / (d.prop$errcord)) * 100)
prop.pl     <- subset(d.prop, related == "rel"   & n2num == "plur")
prop.s      <- subset(d.prop, related == "rel"   & n2num == "sing")
asso.pl     <- subset(d.prop, related == "assoc" & n2num == "plur")
asso.s      <- subset(d.prop, related == "assoc" & n2num == "sing")
prop.unr.pl <- subset(d.prop, related == "unrel" & n2num == "plur")
prop.unr.s  <- subset(d.prop, related == "unrel" & n2num == "sing")

data.item.prop <- aggregate(d.prop$pct, list(d.prop$item, d.prop$related, d.prop$n2num ), mean)
colnames(data.item.prop) <- c("item", "related", "n2num", "error") 

prop.plur  <- subset(data.item.prop, related == "rel"   & n2num   == "plur")
prop.sing  <- subset(data.item.prop, related == "rel"   & n2num   == "sing")
asso.plur  <- subset(data.item.prop, related == "assoc" & n2num   == "plur")
asso.sing  <- subset(data.item.prop, related == "assoc" & n2num   == "sing")
unrel.plur <- subset(data.item.prop, related == "unrel" & n2num   == "plur")
unrel.sing <- subset(data.item.prop, related == "unrel" & n2num   == "sing")

df.prop <- data.frame(

  Relatedness = c("Property","Property","Associate", "Associate", "Unrelated","Unrelated"),
  Number = c("Singular","Plural", "Singular","Plural", "Singular","Plural"),
  
  Error = c(sum( prop.s$errd),
            sum( prop.pl$errd),
            sum( asso.s$errd),
            sum( asso.pl$errd),
            sum( prop.unr.s$errd),
            sum( prop.unr.pl$errd)),
  
  Correct = c(sum( prop.s$errcord)      - sum( prop.s$errd),
              sum( prop.pl$errcord)     - sum( prop.pl$errd),
              sum( asso.s$errcord)      - sum( asso.s$errd),
              sum( asso.pl$errcord)     - sum( asso.pl$errd),
              sum( prop.unr.s$errcord)  - sum( prop.unr.s$errd),
              sum( prop.unr.pl$errcord) - sum( prop.unr.pl$errd)),
  
  
  All = c(sum( prop.s$errcord),
          sum( prop.pl$errcord),
          sum( asso.s$errcord),
          sum( asso.pl$errcord),
          sum( prop.unr.s$errcord),
          sum( prop.unr.pl$errcord)),
  
  Rate = c(sum( prop.s$errd)      / sum( prop.s$errcord),
           sum( prop.pl$errd)     / sum( prop.pl$errcord),
           sum( asso.s$errd)      / sum( asso.s$errcord),
           sum( asso.pl$errd)     / sum( asso.pl$errcord),
           sum( prop.unr.s$errd)  / sum( prop.unr.s$errcord),
           sum( prop.unr.pl$errd) / sum( prop.unr.pl$errcord)),
  
  ErrPer = c(  ( sum( prop.s$errd)      / sum( prop.s$errcord))  * 100,
               ( sum( prop.pl$errd)     / sum( prop.pl$errcord)) * 100,
               ( sum( asso.s$errd)      / sum( asso.s$errcord))  * 100,
               ( sum( asso.pl$errd)     / sum( asso.pl$errcord)) * 100,
               ( sum( prop.unr.s$errd)  / sum( prop.unr.s$errcord))  * 100,
               ( sum( prop.unr.pl$errd) / sum( prop.unr.pl$errcord)) * 100),
  
  SE = c(
    sd( prop.plur$error)   / sqrt( length( prop.plur$error)),
    sd( prop.sing$error)   / sqrt( length( prop.sing$error)),
    sd( asso.plur$error)   / sqrt( length( asso.plur$error)),
    sd( asso.sing$error)   / sqrt( length( asso.sing$error)),
    sd( unrel.plur$error)  / sqrt( length(unrel.plur$error)),
    sd( unrel.sing$error)  / sqrt( length(unrel.sing$error))
  ))

View(df.prop)


# PROPERTY MIS-MATCH EFFECTS TABLE & FIG ----------------------------
se.prop.dat <- read.table("data/SR2_prop_SEdata.txt", header = TRUE)
se.prop.dat$error <- ifelse(se.prop.dat$errd == 0 & se.prop.dat$errcord == 0, 0, (se.prop.dat$errd / (se.prop.dat$errcord)) * 100)

se.prop.plur   <- subset( se.prop.dat, related == "rel"   & n2num   == "plur")
se.assoc.plur  <- subset( se.prop.dat, related == "assoc" & n2num   == "plur")
se.unrel.plur  <- subset( se.prop.dat, related == "unrel" & n2num   == "plur")



prop.mis.eff <-data.frame(
  Relatedness = c( "Property", "Associate", "Unrelated"),
  ErrRate = c( 
    ( df.prop[2, 7] - df.prop[1, 7]),
    ( df.prop[4, 7] - df.prop[3, 7]),
    ( df.prop[6, 7] - df.prop[5, 7])),
  SE = c(
    sd( se.prop.plur$error)  / sqrt( length( se.prop.plur$error)),
    sd( se.assoc.plur$error) / sqrt( length( se.assoc.plur$error)),
    sd( se.unrel.plur$error) / sqrt( length( se.unrel.plur$error)))
)

View(prop.mis.eff)

semrel2.results <-rbind(cat.mis.eff, prop.mis.eff)










