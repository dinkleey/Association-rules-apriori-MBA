library(Rcpp)
library(readxl)
df_diagn<-read.csv("D:/diabetes.csv") ###df with diagnostic classes (concat.to client id)
df_fd<-read.csv("D:/df_fd.csv") ###df with df with diag and disease - linked to purchase id

length(unique(df_fd$cid)) 
summary(df_fd)
df_fd2<-df_fd[(df_fd$age<=99) & (df_fd$age>=5),] #remove outliers in age (brackets: 5-99)
df_fd3<-df_fd2[complete.cases(df_fd2),] ###remove NA's 
length(unique(df_fd3$cid))
length(unique(df_fd4$cid))



#Prep mba for association matrix

str(df_fd3)
summary(df_fd3)
df_fd3$disease<-as.character(df_fd3$disease)
colnames(df_fd3)
df_fd4<-df_fd3[c("cid", "age", "sex", "segment", "freq_gen",
                 "disease")]
df_fd4$disease<-as.factor(df_fd4$disease)
df_fd4$disease
df_fd4<-df_fd4[-which(df_fd4$disease=="no_disease"),]

library(plyr)
library(dplyr)

df_bsk<-ddply(df_fd4, c("cid"),function(df1)
  paste(c(df1$disease),collapse = ",")) ##concatenate diseases

cid<-df_bsk$cid ##separate cid vector for subs merging 
df_bsk$cid<-NULL

write.csv(df_bsk, "df_bsk.csv", quote = FALSE,
          row.names = TRUE)


library(ggplot2)
library(arules)
library(arulesViz)
library(plyr)
library(plyr)
library(knitr)


### generate transaction data

dba<-read.transactions("df_bsk.csv", 
                       format = "basket", sep = ",")


summary(dba)
dev.off()

itemFrequencyPlot(dba, topN=20, type = "absolute",
                  xlab = "Frequency of disease",
                  ylab = "No. of cid")

##Remove outliers (nr of diseases/year/cid)

cat("\5+1.5*(5-3) outliers in nr of distinct diseases within a year")
cat("\nobservations screened out...cids with more than 13 distinct disease codes per year\n: 2717/469915=0.58%")


###Create rules or apriori MBA
rules<-apriori(dba, parameter = list(supp=0.00584, conf = 0.5)) 
rules<-sort(rules, by = "lift", decreasing = TRUE) ##order by lift
summary(rules)
inspect(rules[1:15])
top100rules<-rules[1:50]
print(top100rules)

###Vizualization
dev.off()
plot(top100rules, method = "scatterplot")
plot(top100rules,method = "graph", shading="confidence",interactive = TRUE)


arules_disease_map_fdfbcim<-data.frame(rules@quality)
df_rules_disease<-as(rules, "data.frame")
out<-capture.output(inspect(rules))
df_rules_disease["rhs"]<-rules@rhs@itemInfo$labels[rules@rhs@data@i+1]
df_rules_disease["Lhs"]<-rules@lhs@itemInfo$labels[rules@lhs@data@i+1]
df_rules_disease<-gsub("[^{]+\\{([^}]*)\\}[^{]+\\{([^}]*)\\}.*", "\\2", out)[-1]
df_rules_disease["out"]<-out


df_bsk["cid"]<-cid

str(df_rules_disease)

###attach diagnosis to disease
colnames(df_fd3)
df_fd5<-df_fd3[c("cid","age","sex","segment" ,
                 "freq_gen","diagnosis", "disease","freq_dis")]


summary(df_fd5)
df_fd6<-df_fd5[-which(df_fd5$disease=="no_disease"),]
length(unique(df_fd6$cid))

df_fd7<-merge(df_bsk, df_diagn, by.x="cid", by.y = "cid")





