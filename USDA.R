getwd()

#Snapshot of dataset
str(USDA)
#Statistical Summary
summary(USDA)

#Row no for food which has max sodium levels
which.max(USDA$Sodium)
USDA[265,]
names(USDA)
#Food items which have Na > 10000
HighSodium <- subset(USDA, Sodium > 10000)

#Food items which have high sodium content
HighSodium$Description

USDA[USDA$Description == 'CAVIAR',]
match("CAVIAR", USDA$Description)

summary(USDA$Sodium)
sd(USDA$Sodium, na.rm = TRUE)

USDA$Sodium[USDA$Description == 'CAVIAR']

USDA$Description[265]
USDA[265,]
summary(USDA)



#Scatterplot b/w protein and fat
plot(USDA$Protein,USDA$TotalFat)
plot(USDA$Protein,USDA$TotalFat,xlab = 'Protein', ylab = 'Fat', main = 'Fat vs Protein', col = 'Red')

#Histogram for vitaminc
hist(USDA$VitaminC, xlab = 'VitaminC in mg', main = 'Histogram of VitaminC', xlim = c(0,100), breaks = 500)
summary(USDA$VitaminC)
sd(USDA$VitaminC, na.rm = TRUE)
nrow(USDA[USDA$VitaminC >10,])
USDA$Description[which.max(USDA$VitaminC)]
USDA$VitaminC[which.max(USDA$VitaminC)]

boxplot(USDA$VitaminC, ylab = 'VitaminC in mg', main = 'Boxplot of VitaminC')

boxplot(USDA$Sugar, ylab = "Sugar (g)", main = "Boxplot of Sugar")

#Creating a variable that takes value 1 if the food has higher sodium than average, 0 otherwise
USDA$HighSodium <- c(0)
str(USDA)
USDA$HighSodium[USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE)] <- 1
table(USDA$HighSodium)

#Similarly for HighProtein, HigCarbs, HighFat
USDA$HighProteins <- c(0)
USDA$HighProteins[USDA$Protein > mean(USDA$Protein, na.rm = TRUE)] <- 1
table(USDA$HighProteins)

USDA$HigCarbs <- c(0)
USDA$HigCarbs[USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE)] <- 1
table(USDA$HigCarbs)

USDA$HigFat <- c(0)
USDA$HigFat[USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE)] <- 1
table(USDA$HigFat)

#High Sodium and High Fat
table(USDA$HigFat, USDA$HighSodium)

# Average amount of iron sorted by high and low protein?

names(USDA)
tapply(USDA$Iron, USDA$HighProteins, mean, na.rm = TRUE)

# Maximum level of Vitamin C in hfoods with high and low carbs?

tapply(USDA$VitaminC, USDA$HigCarbs, max, na.rm = TRUE)

# Using summary function with tapply

tapply(USDA$VitaminC, USDA$HigCarbs, summary, na.rm=TRUE)
