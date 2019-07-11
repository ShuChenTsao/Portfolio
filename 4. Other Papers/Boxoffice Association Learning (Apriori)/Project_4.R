require(arules)
require(arulesViz)
BoxofficeDF=read.csv("/Users/Eric/Documents/R study/Final Project/BoxofficeDF.csv")

BoxofficeDF$Total_Gross <- as.character(BoxofficeDF$Total_Gross)
BoxofficeDF$Total_Gross <- gsub("\\$","", BoxofficeDF$Total_Gross)
BoxofficeDF$Total_Gross <- gsub(",","", BoxofficeDF$Total_Gross)

BoxofficeDF$over100M <- c("Yes")
BoxofficeDF[as.numeric(BoxofficeDF$Total_Gross) < 100000000, "over100M"] <- "No"


BoxofficeDF$Number_of_Theaters<-gsub(",","", BoxofficeDF$Number_of_Theaters)
BoxofficeDF$N_Thousand_Theaters<-c(0)
BoxofficeDF[as.numeric(BoxofficeDF$Number_of_Theaters) >1000, "N_Thousand_Theaters"] <- 1
BoxofficeDF[as.numeric(BoxofficeDF$Number_of_Theaters) >2000, "N_Thousand_Theaters"] <- 2
BoxofficeDF[as.numeric(BoxofficeDF$Number_of_Theaters) >3000, "N_Thousand_Theaters"] <- 3
BoxofficeDF[as.numeric(BoxofficeDF$Number_of_Theaters) >4000, "N_Thousand_Theaters"] <- 4

BoxofficeDF$Number_of_Opening_Theaters<-gsub(",","", BoxofficeDF$Number_of_Opening_Theaters)
BoxofficeDF$N_Thousand_Opening_Theaters<-c(0)
BoxofficeDF[as.numeric(BoxofficeDF$Number_of_Opening_Theaters) >1000, "N_Thousand_Opening_Theaters"] <- 1
BoxofficeDF[as.numeric(BoxofficeDF$Number_of_Opening_Theaters) >2000, "N_Thousand_Opening_Theaters"] <- 2
BoxofficeDF[as.numeric(BoxofficeDF$Number_of_Opening_Theaters) >3000, "N_Thousand_Opening_Theaters"] <- 3
BoxofficeDF[as.numeric(BoxofficeDF$Number_of_Opening_Theaters) >4000, "N_Thousand_Opening_Theaters"] <- 4



BoxofficeDF[,1] <- factor(BoxofficeDF[,1])
BoxofficeDF[,2] <- factor(BoxofficeDF[,2])
BoxofficeDF[,5] <- factor(BoxofficeDF[,5])
BoxofficeDF[,11] <- factor(BoxofficeDF[,11])
BoxofficeDF[,12] <- factor(BoxofficeDF[,12])
BoxofficeDF[,13] <- factor(BoxofficeDF[,13])


BoxofficeDF_raw2 <- data.frame(BoxofficeDF$Year,BoxofficeDF$Studio, BoxofficeDF$over100M, BoxofficeDF$N_Thousand_Theaters, BoxofficeDF$N_Thousand_Opening_Theaters)
names(BoxofficeDF_raw2) <- c("Year","Studio","over100M", "n_K", "n_K_open")
str(BoxofficeDF_raw2)


#minlen=2,supp=0.05


rule <- apriori(BoxofficeDF_raw2, 
                parameter=list(minlen=2, supp=0.05, conf=0.7),  
                appearance = list(default="lhs",
                                  rhs=c("over100M=Yes", "over100M=No") 
                )
)  


inspect(rule)
sort.rule <- sort(rule, by="lift")
inspect(sort.rule)

subset.matrix <- as.matrix(is.subset(x=sort.rule, y=sort.rule))
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
sort.rule <- sort.rule[!redundant]
inspect(sort.rule)

plot(sort.rule)
plot(sort.rule, method="graph", control=list(type="items"))
plot(sort.rule, method="grouped")



#minlen=2,supp=0.1


rule <- apriori(BoxofficeDF_raw2, 
                parameter=list(minlen=2, supp=0.1, conf=0.7),  
                appearance = list(default="lhs",
                                  rhs=c("over100M=Yes", "over100M=No") 
                )
)  


inspect(rule)
sort.rule <- sort(rule, by="lift")
inspect(sort.rule)

subset.matrix <- as.matrix(is.subset(x=sort.rule, y=sort.rule))
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
sort.rule <- sort.rule[!redundant]
inspect(sort.rule)

plot(sort.rule)
plot(sort.rule, method="graph", control=list(type="items"))
plot(sort.rule, method="grouped")



#minlen=3,supp=0.05


rule <- apriori(BoxofficeDF_raw2, 
                parameter=list(minlen=3, supp=0.05, conf=0.7),  
                appearance = list(default="lhs",
                                  rhs=c("over100M=Yes", "over100M=No") 
                )
)  


inspect(rule)
sort.rule <- sort(rule, by="lift")
inspect(sort.rule)

subset.matrix <- as.matrix(is.subset(x=sort.rule, y=sort.rule))
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
sort.rule <- sort.rule[!redundant]
inspect(sort.rule)

plot(sort.rule)
plot(sort.rule, method="graph", control=list(type="items"))
plot(sort.rule, method="grouped")

