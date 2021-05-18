
#library(ggplot2)
#library(ggpubr)
#library(tidyr)
#library(TeachingDemos)
#library(datasets)


dirstudio <- dirname(rstudioapi::getSourceEditorContext()$path)
filename <- "breast-cancer.data"
file <- file.path(dirstudio, filename)
columns <- c("class", "age", "menopause", "tumor.size", "inv.nodes", 
             "node.caps","deg.malig", "breast", "breast.quad", "irradiat")
tabla <- read.csv(file, col.names = columns)
tabla2 <- tabla 
tabla2$class <- as.factor(tabla2$class)
tabla2$age <- as.factor(tabla2$age) 
tabla2$menopause <- as.factor(tabla2$menopause) 
tabla2$tumor.size <- as.factor(tabla2$tumor.size)
tabla2$inv.nodes <- as.factor(tabla2$inv.nodes) 
tabla2$node.caps <- as.factor(tabla2$node.caps) 
tabla2$breast <- as.factor(tabla2$breast) 
tabla2$breast.quad <- as.factor(tabla2$breast.quad) 
tabla2$irradiat <- as.factor(tabla2$irradiat)

#subtabla1 <- table(tabla$class, tabla$age)
#subtabla2 <- table(tabla$class, tabla$menopause)
#subtabla3 <- table(tabla$class, tabla$tumor.size)
#subtabla4 <- table(tabla$class, tabla$inv.nodes)
#subtabla5 <- table(tabla$class, tabla$node.caps)
#subtabla6 <- table(tabla$class, tabla$deg.malig)
#subtabla7 <- table(tabla$class, tabla$breast)
#subtabla8 <- table(tabla$class, tabla$breast.quad)
#subtabla9 <- table(tabla$class, tabla$irradiat)
#data.mean <- tapply(tabla$deg.malig, INDEX = tabla$class, FUN = mean)
#class.table <- table(tabla$class)
#dat <- prop.table(class.table)




summary(tabla2)

tabla2=tabla2[!(tabla2$node.caps == "?"),]
tabla2=tabla2[!(tabla2$breast.quad == "?"),]

summary(tabla2)




attach(tabla2)
age_class<- table(age, class)
age_class

library(vcd)
assocstats(age_class)
