rm(list = ls()) # clears environment
library(languageR) # calls languageR library
library(ggplot2)
library(grid)
install.packages("extrafont")
library(extrafont)
#
# -----------------------------------PREPARE DATA FILE FOR ANALYSES---------------------------------
#
f2errout <- read.table("data/SR_F2_errordata.txt", header = T) # reads in all data from data file

d <- f2errout # renames data file


d$item <- as.factor(d$item) # designates "itemect" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)

#aggregates d with dysfluencies
data.item <- aggregate(d$pct, list(d$item, d$integ, d$related, d$n2num), mean)
colnames(data.item) <- c("item", "semint", "related", "n2num", "error") # renames columns

# Below, designates various subsets of the original data file
integ <- subset(data.item, semint   ==  "integ")
unint <- subset(data.item, semint   ==  "unint")
relat <- subset(data.item, related  ==  "rel")
unrel <- subset(data.item, related  ==  "unrel")
sing  <- subset(data.item, n2num    ==  "sing")
plur  <- subset(data.item, n2num    ==  "plur")

#Below, additional subsetted groups
relat.int.plur   <- subset(data.item, related == "rel"   & semint  == "integ" & n2num == "plur")
relat.int.sing   <- subset(data.item, related == "rel"   & semint  == "integ" & n2num == "sing")
relat.unint.plur <- subset(data.item, related == "rel"   & semint  == "unint" & n2num == "plur")
relat.unint.sing <- subset(data.item, related == "rel"   & semint  == "unint" & n2num == "sing")
unrel.int.plur   <- subset(data.item, related == "unrel" & semint  == "integ" & n2num == "plur")
unrel.int.sing   <- subset(data.item, related == "unrel" & semint  == "integ" & n2num == "sing")
unrel.unint.plur <- subset(data.item, related == "unrel" & semint  == "unint" & n2num == "plur")
unrel.unint.sing <- subset(data.item, related == "unrel" & semint  == "unint" & n2num == "sing")
relat.plur       <- subset(data.item, related == "rel"   & n2num   == "plur")
relat.sing       <- subset(data.item, related == "rel"   & n2num   == "sing")
unrel.plur       <- subset(data.item, related == "unrel" & n2num   == "plur")
unrel.sing       <- subset(data.item, related == "unrel" & n2num   == "sing")
integ.plur       <- subset(data.item, semint  == "integ" & n2num   == "plur")
integ.sing       <- subset(data.item, semint  == "integ" & n2num   == "sing")
unint.plur       <- subset(data.item, semint  == "unint" & n2num   == "plur")
unint.sing       <- subset(data.item, semint  == "unint" & n2num   == "sing")
integ.relat      <- subset(data.item, semint  == "integ" & related == "rel")
integ.unrel      <- subset(data.item, semint  == "integ" & related == "unrel")
unint.relat      <- subset(data.item, semint  == "unint" & related == "rel")
unint.unrel      <- subset(data.item, semint  == "unint" & related == "unrel")


ds <- data.frame(data = c(
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
  
  n = c(length(data.item$error),
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
  
  N = c(length(data.item$error),
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
  
  mean = c(mean(data.item$error),
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
  
  sd = c(sd(data.item$error),
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
  
  se = c(sd(data.item$error) / sqrt(length(data.item$error)),
         sd(integ$error) / sqrt(length(integ$error)),
         sd(unint$error) / sqrt(length(unint$error)),
         sd(relat$error) / sqrt(length(relat$error)),
         sd(unrel$error) / sqrt(length(unrel$error)),
         sd(integ.relat$error) / sqrt(length(integ.relat$error)),
         sd(integ.unrel$error) / sqrt(length(integ.unrel$error)),
         sd(unint.relat$error) / sqrt(length(unint.relat$error)),
         sd(unint.unrel$error) / sqrt(length(unint.unrel$error)),
         sd(plur$error) / sqrt(length(plur$error)),
         sd(sing$error) / sqrt(length(sing$error)),
         sd(integ.plur$error) / sqrt(length(integ.plur$error)),
         sd(integ.sing$error) / sqrt(length(integ.sing$error)),
         sd(unint.plur$error) / sqrt(length(unint.plur$error)),
         sd(unint.sing$error) / sqrt(length(unint.sing$error)),
         sd(relat.plur$error) / sqrt(length(relat.plur$error) ),
         sd(relat.sing$error) / sqrt(length(relat.sing$error)),
         sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
         sd(unrel.sing$error) / sqrt(length(unrel.sing$error)),
         sd(relat.int.plur$error) / sqrt(length(relat.int.plur$error)),
         sd(relat.int.sing$error) / sqrt(length(relat.int.sing$error)),
         sd(relat.unint.plur$error) / sqrt(length(relat.unint.plur$error)),
         sd(relat.unint.sing$error) / sqrt(length(relat.unint.sing$error)),
         sd(unrel.int.plur$error) / sqrt(length(unrel.int.plur$error)),
         sd(unrel.int.sing$error) / sqrt(length(unrel.int.sing$error)),
         sd(unrel.unint.plur$error) / sqrt(length(unrel.unint.plur$error)),
         sd(unrel.unint.sing$error) / sqrt(length(unrel.unint.sing$error))
  ))


# PREPARE RESULTS TABLES-------------------------


rel.int.pl   <- subset(d, related == "rel"   & integ  == "integ" & n2num == "plur")
rel.int.s    <- subset(d, related == "rel"   & integ  == "integ" & n2num == "sing")
rel.uni.pl   <- subset(d, related == "rel"   & integ  == "unint" & n2num == "plur")
rel.uni.s    <- subset(d, related == "rel"   & integ  == "unint" & n2num == "sing")
unr.int.pl   <- subset(d, related == "unrel" & integ  == "integ" & n2num == "plur")
unr.int.s    <- subset(d, related == "unrel" & integ  == "integ" & n2num == "sing")
unr.uni.pl   <- subset(d, related == "unrel" & integ  == "unint" & n2num == "plur")
unr.uni.s    <- subset(d, related == "unrel" & integ  == "unint" & n2num == "sing")



df <- data.frame(
  Relatedness = c("Related","Related","Related","Related","Unrelated","Unrelated","Unrelated","Unrelated"),
  Integration = c("Integrated","Integrated","Unintegrated", "Unintegrated", "Integrated","Integrated","Unintegrated","Unintegrated"),
  Number = c("Singular","Plural","Singular","Plural","Singular","Plural","Singular","Plural"),
  Error = c(sum(rel.int.s$errd),
            sum(rel.int.pl$errd),
            sum(rel.uni.s$errd),
            sum(rel.uni.pl$errd),
            sum(unr.int.s$errd),
            sum(unr.int.pl$errd),
            sum(unr.uni.s$errd),
            sum(unr.uni.pl$errd)),
  
  Correct = c(sum(rel.int.s$errcord)-sum(rel.int.s$errd),
          sum(rel.int.pl$errcord)  -sum(rel.int.pl$errd),
          sum(rel.uni.s$errcord)   -sum(rel.uni.s$errd),
          sum(rel.uni.pl$errcord)  -sum(rel.uni.pl$errd),
          sum(unr.int.s$errcord)   -sum(unr.int.s$errd),
          sum(unr.int.pl$errcord)  -sum(unr.int.pl$errd),
          sum(unr.uni.s$errcord)   -sum(unr.uni.s$errd),
          sum(unr.uni.pl$errcord)  -sum(unr.uni.pl$errd)),
  
  
  All = c(sum(rel.int.s$errcord),
            sum(rel.int.pl$errcord),
            sum(rel.uni.s$errcord),
            sum(rel.uni.pl$errcord),
            sum(unr.int.s$errcord),
            sum(unr.int.pl$errcord),
            sum(unr.uni.s$errcord),
            sum(unr.uni.pl$errcord)),
  
  Rate = c(sum(rel.int.s$errd)   / sum(rel.int.s$errcord),
            sum(rel.int.pl$errd) / sum(rel.int.pl$errcord),
            sum(rel.uni.s$errd)  / sum(rel.uni.s$errcord),
            sum(rel.uni.pl$errd) / sum(rel.uni.pl$errcord),
            sum(unr.int.s$errd)  / sum(unr.int.s$errcord),
            sum(unr.int.pl$errd) / sum(unr.int.pl$errcord),
            sum(unr.uni.s$errd)  / sum(unr.uni.s$errcord),
            sum(unr.uni.pl$errd) / sum(unr.uni.pl$errcord)),
  
 ErrPer = c(  (sum(rel.int.s$errd)/ sum(rel.int.s$errcord))  * 100,
              (sum(rel.int.pl$errd) / sum(rel.int.pl$errcord)) * 100,
              (sum(rel.uni.s$errd)  / sum(rel.uni.s$errcord))  * 100,
              (sum(rel.uni.pl$errd) / sum(rel.uni.pl$errcord)) * 100,
              (sum(unr.int.s$errd)  / sum(unr.int.s$errcord))  * 100,
              (sum(unr.int.pl$errd) / sum(unr.int.pl$errcord)) * 100,
              (sum(unr.uni.s$errd)  / sum(unr.uni.s$errcord))  * 100,
              (sum(unr.uni.pl$errd) / sum(unr.uni.pl$errcord)) * 100),
SE = c(
sd(relat.int.sing$error) / sqrt(length(relat.int.sing$error)),
sd(relat.int.plur$error) / sqrt(length(relat.int.plur$error)),
sd(relat.unint.sing$error) / sqrt(length(relat.unint.sing$error)),
sd(relat.unint.plur$error) / sqrt(length(relat.unint.plur$error)),
sd(unrel.int.sing$error) / sqrt(length(unrel.int.sing$error)),
sd(unrel.int.plur$error) / sqrt(length(unrel.int.plur$error)),
sd(unrel.unint.sing$error) / sqrt(length(unrel.unint.sing$error)),
sd(unrel.unint.plur$error) / sqrt(length(unrel.unint.plur$error))
))
View(df)

# MIS-MATCH EFFECTS TABLE & FIG ----------------------------
se.dat <- read.table("data/SR_SEdata.txt", header=TRUE) 
se.dat$error <- ifelse(se.dat$errd == 0 & se.dat$errcord == 0, 0, (se.dat$errd / (se.dat$errcord)) * 100)
se.rel.int   <- subset(se.dat, related == "rel"   & semint  == "integ" & n2num == "plur")
se.rel.uni   <- subset(se.dat, related == "rel"   & semint  == "unint" & n2num == "plur")
se.unr.int   <- subset(se.dat, related == "unrel" & semint  == "integ" & n2num == "plur")
se.unr.uni   <- subset(se.dat, related == "unrel" & semint  == "unint" & n2num == "plur")



mis.eff <-data.frame(
    Related = c("Related","Related","Unrelated","Unrelated"),
    Integrated = c("Integrated", "Unintegrated", "Integrated", "Unintegrated"),
    ErrRate = c((df[2,8]-df[1,8]),(df[4,8]-df[3,8]),(df[6,8]-df[5,8]),(df[8,8]-df[7,8])),
    SE = c(
      sd(se.rel.int$error) / sqrt(length(se.rel.int$error)),
      sd(se.rel.uni$error) / sqrt(length(se.rel.uni$error)),
      sd(se.unr.int$error) / sqrt(length(se.unr.int$error)),
      sd(se.unr.uni$error) / sqrt(length(se.unr.uni$error)))
    )


# PREPARE FIGURES-------------------

dodge  <- position_dodge(width = 0.9)
g1     <- ggplot(data = mis.eff, aes(x = interaction(Integrated, Related), y = ErrRate, fill = interaction(Integrated, Related))) +
layer(geom="bar", stat="identity", position = position_dodge()) +
scale_fill_manual(values=c("#990000", "#CC6666", "#000099", "#9999CC")) +
guides(fill=FALSE)+
geom_errorbar(aes(ymax = ErrRate + SE, ymin = ErrRate - SE), position = dodge, width = 0.2)+ 
coord_cartesian(ylim = c(0, 17))+
scale_y_continuous(breaks=seq(0, 14, 2))+
annotate("text", x = 1:4, y = -1, label = rep(c("Integrated", "Unintegrated"), 2), size=6) +
annotate("text", c(1.5, 3.5), y = -2, label = c("Related", "Unrelated"), size=6) +

theme_classic() +
theme(text = element_text(size=18.5)) +
ylab("Mismatch effect (%)") +
theme(axis.title.y=element_text(vjust=1.5)) +
theme(plot.margin = unit(c(1, 1, 4, 1), "lines"), axis.title.x = element_blank(), axis.text.x = element_blank())

#p-value text
p.text = grobTree(textGrob(expression(paste(italic("*p"),"<.05")), x=0.02,  y=0.90, hjust=0,
                                       gp=gpar(col="black", fontsize=12)))
g1  <- g1 + annotation_custom(p.text)

# significance grouping bars                   
g1  <- g1 + geom_path(aes(group=1), x=c(1.5,1.5,3.5,3.5), y=c(14.5,15.5,15.5,14.5)) +
            geom_path(aes(group=1), x=c(1,1,2,2), y=c(13.5,14.5,14.5,13.5)) +
            geom_path(aes(group=1), x=c(3,3,4,4), y=c(13.5,14.5,14.5,13.5)) +
  annotate("text", x=2.5,y=16,label="*", size=8)


# remove clipping of x axis labels
g1 <- ggplot_gtable(ggplot_build(g1))
g1$layout$clip[g1$layout$name == "panel"] <- "off"

pdf(file = "figures/SemRel Mismatch Effects.pdf", useDingbats = FALSE)
grid.draw(g1)
dev.off()
grid.draw(g1)



#
# --------------------------------2 X 2 X 2 ANOVA-----------------------------------------------------

sink("output/SemRel F2 Factorial Analyses.txt")

cat(" ", "\n")
cat("BY-ITEMS FACTORIAL ANALYSES RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
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



#
#========================================================================================================
#
#------------------------------------RELATED - UNRELATED ITEMS PAIRED COMPARISONS--------------------------------------
#
f2errout <- read.table("data/SR_F2_errrel.txt", header = T) # reads in all data from data file
d <- f2errout # renames data file


d$item <- as.factor(d$item) # designates "item" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)

#aggregates d with dysfluencies
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)

colnames(data.item) <- c("item", "related", "n2num", "error") # renames columns


relat      <- subset(data.item, related == "rel")
unrel      <- subset(data.item, related == "unrel")
sing       <- subset(data.item, n2num   == "sing")
plur       <- subset(data.item, n2num   == "plur")
relat.plur <- subset(data.item, related == "rel"   & n2num == "plur")
relat.sing <- subset(data.item, related == "rel"   & n2num == "sing")
unrel.plur <- subset(data.item, related == "unrel" & n2num == "plur")
unrel.sing <- subset(data.item, related == "unrel" & n2num == "sing")

ds <- data.frame(data = c(
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

n = c(length(data.item$error),
      length(relat$error),
      length(unrel$error),
      length(plur$error),
      length(sing$error),
      length(relat.plur$error),
      length(relat.sing$error),
      length(unrel.plur$error),
      length(unrel.sing$error)
),

N = c(length(data.item$error),
      length(relat$error),
      length(unrel$error),
      length(plur$error),
      length(sing$error),
      length(relat.plur$error),
      length(relat.sing$error),
      length(unrel.plur$error),
      length(unrel.sing$error)
),

mean = c(mean(data.item$error),
         mean(relat$error),
         mean(unrel$error),
         mean(plur$error),
         mean(sing$error),
         mean(relat.plur$error),
         mean(relat.sing$error),
         mean(unrel.plur$error),
         mean(unrel.sing$error)
),

sd = c(sd(data.item$error),
       sd(relat$error),
       sd(unrel$error),
       sd(plur$error),
       sd(sing$error),
       sd(relat.plur$error),
       sd(relat.sing$error),
       sd(unrel.plur$error),
       sd(unrel.sing$error)
),

se = c(sd(data.item$error) / sqrt(length(data.item$error)),
       sd(relat$error) / sqrt(length(relat$error)),
       sd(unrel$error) / sqrt(length(unrel$error)),
       sd(plur$error) / sqrt(length(plur$error)),
       sd(sing$error) / sqrt(length(sing$error)),
       sd(relat.plur$error) / sqrt(length(relat.plur$error)),
       sd(relat.sing$error) / sqrt(length(relat.sing$error)),
       sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
       sd(unrel.sing$error) / sqrt(length(unrel.sing$error))
       
))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("RELATED ITEMS ANALYSES", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds)
cat(" ", "\n")

a.2x2 <- aov(error ~ related * n2num + Error(item / (related * n2num)), data = data.item)
print(summary(a.2x2))
cat(" ", "\n")

cat(" ", "\n")

# -------------------------RELATED ITEMS ANALYSES------------------------------

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


cat(">>>  RELATED ITEMS PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.relat)
cat(" ", "\n")

a.relat <- aov(error ~ n2num + Error(item / n2num), data = relat)
print(summary(a.relat))
cat(" ", "\n")
cat(" ", "\n")
#------------------------------UNRELATED PAIRED COMPARISION----------------
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

cat(">>>  UNRELATED ITEMS PAIRED COMPARISONS", sep = "", fill = 60)
cat(rep(c("-"), times=25, quote=F), "\n")
print(ds.unrel)
cat(" ", "\n")

a.unrel <- aov(error ~ n2num + Error(item / n2num), data = unrel)
print(summary(a.unrel))
cat(" ", "\n")
cat(" ", "\n")


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
