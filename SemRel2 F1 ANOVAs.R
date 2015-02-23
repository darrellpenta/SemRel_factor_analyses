
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






# --------------------------------2 X 2 ANOVA------------------------------------------------------


sink("output/SemRel2 F1 Factor Analyses.txt")

cat("***BY-SUBJECTS FACTOR ANALYSES RUN ON:", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 80)


cat("__________________Descriptive Stats for 2X2 ANOVA: CATEGORY COORDINATES______________", sep = "", fill = 80)
print(ds) # prints descrip stats for 2x2x2 ANOVA


# Computes the anova
a.2x2 <- aov(error ~ related * n2num + Error(subj / (related * n2num)), data = data.subj)
cat("__________________2X2 ANOVA__________________", sep = "", fill = 80)
print(summary(a.2x2))
print(rep(c("="), times = 50), quote = F)


#
# ========================================================================================================
#
# --------------------RELATED - UNRELATED ITEMS PAIRED COMPARISONS--------------------------------------
#
# ------------RELATED ITEMS------------------------------

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

cat("__________________Descriptive Stats for Paired Compairsion of Related items________", sep = "", fill = 80)
print(ds.relat)
print(rep(c("-"), times = 50), quote = F)


a.relat <- aov(error ~ n2num + Error(subj / n2num), data = relat)
cat("__________________ANOVA: Paired Compairsion of Related items___________", sep = "", fill = 80)
print(summary(a.relat))
print(rep(c("="),times = 50), quote = F)

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

cat("__________________Descriptive Stats for Paired Compairsion of Unrelated items_________", sep = "", fill = 80)
print(ds.unrel)
print(rep(c("-"),times = 50), quote = F)


a.unrel <- aov(error ~ n2num + Error(subj / n2num), data = unrel)
cat("__________________ANOVA: Paired Unelated items", sep = "",fill = 80)
print(summary(a.unrel))
print(rep(c("="),times = 50), quote = F)



# ====================================================================================================
# --------------------------------------PROPERTY ITEMS ANALYSES---------------------
# ====================================================================================================
rm(list = ls()) # clears environment

# -----------------------------------PREPARE DATA FILE FOR ANALYSES---------------------------------
#
f1errout <- read.table("data/SR2_F1_errprop.txt", header = T) # reads in all data from data file

d <- f1errout # renames data file
d$subj <- as.factor(d$subj) # designates "subject" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)

# aggregates d with dysfluencies
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)

colnames(data.subj) <- c("subj", "related", "n2num", "error") # renames columns

# Below, designates various subsets of the original data file
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


cat("__________________Descriptive Stats for 3X2 ANOVA: PROPERTY ITEMS", sep = "", fill = 80)
print(ds)


# Computes the anova
a.3x2 <- aov(error ~ related * n2num + Error(subj / (related * n2num)), data = data.subj)
cat("__________________3X2 ANOVA__________________", sep = "", fill = 80)
print(summary(a.3x2))
print(rep(c("="), times = 50), quote = F)

#====================================================================================================
  #
  # -----------------------------Paired comparisions of Plural - Singular items
  #
  #------------Plural Items
  
  ds.plur <- data.frame(data = c("n2num","plur","sing"),
                        
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

cat("__________________Descriptive Stats for Paired Compairsion of Pural Items________", sep = "", fill = 80)
print(ds.plur) 
print(rep(c("-"), times = 50), quote = F)


a.plur <- aov(error ~ related + Error(subj / related), data = plur) 
cat("__________________ANOVA: Paired Plural items", sep = "", fill = 80)
print(summary(a.plur))
print(rep(c("="),times = 50), quote = F)

#------------Singular Items

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

cat("__________________Descriptive Stats for Paired Compairsion of Sing Items________", sep = "", fill = 80)
print(ds.sing) 
print(rep(c("-"), times = 50), quote = F)


a.sing <- aov(error ~ related + Error(subj / related), data = sing) 
cat("__________________ANOVA: Paired Sing items", sep = "", fill = 80)
print(summary(a.sing))
print(rep(c("="),times = 50), quote = F)
#
# ========================================================================================================
#
# --------------------RELATED - ASSOCIATED ITEMS --------------------------------------

rm(list = ls())
f1errout <- read.table("data/SR2_F1_errprop.txt", header = T) # reads in all data from data file

d <- f1errout # renames data file
d <- subset(d, related != "unrel")
d$subj <- as.factor(d$subj) # designates "subject" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)

# aggregates d with dysfluencies
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)

colnames(data.subj) <- c("subj", "related", "n2num", "error") # renames columns

# Below, designates various subsets of the original data file
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


cat("__________________Descriptive Stats for Ass-Rel", sep = "", fill = 80)
print(assrel)


# Computes the anova
a.assrel <- aov(error ~ related * n2num + Error(subj / (related * n2num)), data = data.subj)
cat("__________________Ass-Rel Anova__________________", sep = "", fill = 80)
print(summary(a.assrel))
print(rep(c("="), times = 50), quote = F)


# --------------------RELATED - UNRELATED ITEMS --------------------------------------

rm(list = ls())
f1errout <- read.table("data/SR2_F1_errprop.txt", header = T) # reads in all data from data file

d <- f1errout # renames data file
d <- subset(d, related != "assoc")
d$subj <- as.factor(d$subj) # designates "subject" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)

# aggregates d with dysfluencies
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)

colnames(data.subj) <- c("subj", "related", "n2num", "error") # renames columns

# Below, designates various subsets of the original data file
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


cat("__________________Descriptive Stats for Rel-Unr", sep = "", fill = 80)
print(relunr)


# Computes the anova
a.relunr <- aov(error ~ related * n2num + Error(subj / (related * n2num)), data = data.subj)
cat("__________________Rel-Unrel Anova__________________", sep = "", fill = 80)
print(summary(a.relunr))
print(rep(c("="), times = 50), quote = F)


# -------------------- ASSOCIATED - UNRELATED ITEMS --------------------------------------

rm(list = ls())
f1errout <- read.table("data/SR2_F1_errprop.txt", header = T) # reads in all data from data file

d <- f1errout # renames data file
d <- subset(d, related != "rel")
d$subj <- as.factor(d$subj) # designates "subject" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)

# aggregates d with dysfluencies
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)

colnames(data.subj) <- c("subj", "related", "n2num", "error") # renames columns

# Below, designates various subsets of the original data file
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


cat("__________________Descriptive Stats for Ass-Unrel", sep = "", fill = 80)
print(assunr)


# Computes the anova
a.assunr <- aov(error ~ related * n2num + Error(subj / (related * n2num)), data = data.subj)
cat("__________________Ass-Unrel Anova__________________", sep = "", fill = 80)
print(summary(a.assunr))
print(rep(c("="), times = 50), quote = F)


# ###### PAIRED RELATED
# ######
f1errout <- read.table("data/SR2_F1_errprop.txt", header = T) # reads in all data from data file

d <- f1errout # renames data file
d$subj <- as.factor(d$subj) # designates "subject" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)

# aggregates d with dysfluencies
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)

colnames(data.subj) <- c("subj", "related", "n2num", "error") # renames columns

# Below, designates various subsets of the original data file
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

cat("__________________Descriptive Stats for Paired Compairsion of Related items________", sep = "", fill = 80)
print(ds.relat)
print(rep(c("-"), times = 50), quote = F)


a.relat <- aov(error ~ n2num + Error(subj / n2num), data = relat)
cat("__________________ANOVA: Paired Compairsion of Related items___________", sep = "", fill = 80)
print(summary(a.relat))
print(rep(c("="),times = 50), quote = F)


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

cat("__________________Descriptive Stats for Paired Compairsion of Associated items________", sep = "", fill = 80)
print(ds.assoc)
print(rep(c("-"), times = 50), quote = F)


a.assoc <- aov(error ~ n2num + Error(subj / n2num), data = relat)
cat("__________________ANOVA: Paired Compairsion of Associated items___________", sep = "", fill = 80)
print(summary(a.assoc))
print(rep(c("="),times = 50), quote = F)



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

cat("__________________Descriptive Stats for Paired Compairsion of Unrelated items_________", sep = "", fill = 80)
print(ds.unrel)
print(rep(c("-"),times = 50), quote = F)


a.unrel <- aov(error ~ n2num + Error(subj / n2num), data = unrel)
cat("__________________ANOVA: Paired Unelated items", sep = "",fill = 80)
print(summary(a.unrel))
print(rep(c("="),times = 50), quote = F)

#====================================================================================================
#
#-----------------------------Paired comparisions of Plural - Singular items

rm(list = ls())
f1errout <- read.table("data/SR2_F1_errprop.txt", header = T) # reads in all data from data file

d <- f1errout # renames data file
d <- subset(d, related != "rel")
d$subj <- as.factor(d$subj) # designates "subject" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)

# aggregates d with dysfluencies
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)

colnames(data.subj) <- c("subj", "related", "n2num", "error") # renames columns

# Below, designates various subsets of the original data file
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
#------------Plural Items

#
# -----------------------------Paired comparisions of Plural - Singular items
#
#------------Plural Items

ds.plur <- data.frame(data = c("n2num","plur","sing"),
                      
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

cat("__________________Descriptive Stats for Paired Compairsion of Pural Items________", sep = "", fill = 80)
print(ds.plur) 
print(rep(c("-"), times = 50), quote = F)


a.plur <- aov(error ~ related + Error(subj / related), data = plur) 
cat("__________________ANOVA: Paired Plural items", sep = "", fill = 80)
print(summary(a.plur))
print(rep(c("="),times = 50), quote = F)

#------------Singular Items

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

cat("__________________Descriptive Stats for Paired Compairsion of Sing Items________", sep = "", fill = 80)
print(ds.sing) 
print(rep(c("-"), times = 50), quote = F)


a.sing <- aov(error ~ related + Error(subj / related), data = sing) 
cat("__________________ANOVA: Paired Sing items", sep = "", fill = 80)
print(summary(a.sing))
print(rep(c("="),times = 50), quote = F)






sink()


