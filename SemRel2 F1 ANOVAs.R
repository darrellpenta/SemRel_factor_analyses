
rm(list = ls()) # clears environment
library(languageR) 
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





#
# --------------------------------2 X 2 X 2 ANOVA------------------------------------------------------
#

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


