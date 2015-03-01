rm(list = ls()) # clears environment
library(languageR) # calls languageR library

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


#
# --------------------------------2 X 2 X 2 ANOVA-----------------------------------------------------

sink("output/SemRel F2 Factor Analyses.txt")

cat(" ", "\n")
cat("BY-ITEMS FACTOR ANALYSES RUN ON:", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")
cat(rep(c("-"), times=40, quote=F),"\n")
cat("2X2X2 ANOVA: SEMREL", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds) # prints descrip stats for 2x2x2 ANOVA
cat(" ", "\n")

# Computes the anova
a.2x2x2 <- aov(error ~ semint * related * n2num + Error(item / (semint * related * n2num)), data = data.item)
print(summary(a.2x2x2)) 
cat(" ", "\n")
cat(" ", "\n")
sink()



StatRep <- function(subset.names) {
  subset.list = data.list[subset.names]
  
  ds      <- data.frame(data = c(subset.names))
 ds$n    <- lapply(subset.list, nrow)
  ds$N    <- lapply(subset.list, nrow)
  ds$mean <- lapply(subset.list, function(item) mean(item$error))
  ds$sd   <- lapply(subset.list, function(item) sd(item$error))
  ds$se   <- lapply(subset.list, function(item) sd(item$error)/sqrt(length(item$error)))
    return(ds)  
}



ds <- StatRep(c("gmean","relat","unrel","sing","plur","unrel.unint.sing"))
print(ds)# 