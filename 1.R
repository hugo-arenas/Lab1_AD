
#library(ggplot2)
#library(ggpubr)
#library(tidyr)
#library(TeachingDemos)
#library(datasets)
library(vcd)


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

bool.values <- tabla2$node.caps=='?'
tabla2 <- tabla2[!bool.values,]

bool.values <- tabla2$breast.quad =='?'
tabla2 <- tabla2[!bool.values,]

summary(tabla2)

attach(tabla2)
#age_class<- table(age, class)
#age_class

#age_menopause<- table(age, menopause)
#age_menopause

#age_tumor.size<- table(age, tumor.size)
#age_tumor.size

#age_inv.nodes<- table(age, inv.nodes)
#age_inv.nodes

#age_node.caps<- table(age, node.caps)
#age_node.caps
#age_node.caps[, -1]

#age_deg.malig<- table(age, deg.malig)
#age_deg.malig

#age_breast<- table(age, breast )
#age_breast

#age_breast.quad<- table(age, breast.quad)
#age_breast.quad
#age_breast.quad[, -1]

class_age <- table(class, age)
class_age

class_menopause <- table(class, menopause)
class_menopause

class_tumor.size <- table(class, tumor.size)
class_tumor.size

class_inv.nodes <- table(class, inv.nodes)
class_inv.nodes

class_node.caps <- table(class, node.caps)
class_node.caps

class_deg.malig <- table(class, deg.malig)
class_deg.malig

class_breast <- table(class, breast)
class_breast

class_breast.quad <- table(class, breast.quad)
class_breast.quad

class_irradiat <- table(class, irradiat)
class_irradiat

#assocstats(age_class)
#assocstats(age_menopause)
#assocstats(age_tumor.size)
#assocstats(age_inv.nodes)
#assocstats(age_node.caps)
#assocstats(age_deg.malig)
#assocstats(age_breast)
#assocstats(age_breast.quad)

cor.class_age <- assocstats(class_age)
cor.class_menopause <- assocstats(class_menopause)
cor.class_tumor.size <- assocstats(class_tumor.size)
cor.class_inv.nodes <- assocstats(class_inv.nodes)
cor.class_node.caps <- assocstats(class_node.caps)
cor.class_breast <- assocstats(class_breast)
cor.class_breast.quad <- assocstats(class_breast.quad)
cor.class_irradiat <- assocstats(class_irradiat)
