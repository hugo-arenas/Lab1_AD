
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

attach(tabla2)
age_menopause<- table(age, menopause)
age_menopause

attach(tabla2)
age_tumor.size<- table(age, tumor.size)
age_tumor.size

attach(tabla2)
age_inv.nodes<- table(age, inv.nodes)
age_inv.nodes

attach(tabla2)
age_node.caps<- table(age, node.caps)
age_node.caps

attach(tabla2)
age_deg.malig<- table(age, deg.malig)
age_deg.malig
attach(tabla2)
age_breast<- table(age, breast )
age_breast
attach(tabla2)
age_breast.quad<- table(age, breast.quad)
age_breast.quad


library(vcd)
assocstats(age_class)
assocstats(age_menopause)
assocstats(age_tumor.size)
assocstats(age_inv.nodes)
assocstats(age_node.caps)
assocstats(age_deg.malig)
assocstats(age_breast)
assocstats(age_breast.quad)


