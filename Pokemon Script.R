#---------- POKEMON CHALLENGE ----------------#

#----------CHAN LE, 19.02.20 -----------------#

install.packages("fastDummies")


library(tidyverse)
library(Amelia)
library(corrplot)
library(fastDummies)


# # Input data and reset name ----
data <- read.csv2("pokemon.csv", sep = ",")

colnames(data) <- c("index","name","type1","type2","hp","atk","def","spAtk","spDef","spe","gen","legendary")



statName <- c("hp","atk","def","spAtk","spDef","spe")
type <- unique(data$type1)

# # Set dummies for Types ----
data$type2 <- as.character(data$type2)

data <- data %>% mutate(type2 = replace(type2, type2 == "", "Mono")) 
# %>% mutate(legendary = ifelse(legendary == TRUE, as.integer(0), as.integer(1)))

data$type2 <- as.factor(data$type2)

dataDummy <- data %>% dummy_cols(select_columns = c("type1","type2"), 
                                 remove_first_dummy = T, # To avoid perfect multi-collinearity
                                 remove_selected_columns = T, 
                                 ignore_na = T)

# Rename Mono type dummy collumn
dataDummy <- dataDummy %>% rename(monoType = type2_Mono)

# Take out index column
dataDummy <-  dataDummy %>% select(-index, -name, -gen)


# # Train-test splitting ----
# Shuffle data
index <- createDataPartition(dataDummy$hp, p = 0.8,
                             list = F, times = 1)

trainData <- dataDummy[index,]
# trainDataLabel <- trainData$legendary
# trainData <- select(trainData, -legendary)

testData <- dataDummy[-index,]
testDataLabel <- testData$legendary
test <- select(testData, -legendary)

# # Fit candidate models ----
modelList <- list()

modelList[[1]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe, family = binomial(link = "logit"), data = trainData)

modelList[[2]] <- glm(legendary ~ ., family = binomial(link = "logit"), data = trainData)

modelList[[3]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + monoType, family = binomial(link = "logit"), data = trainData)

modelList[[4]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + monoType +
                        type1_Dark + type1_Dragon + type1_Electric + type1_Fairy +
                      type1_Fighting + type1_Fire + type1_Flying + type1_Ghost + type1_Grass + 
                        type1_Ground + type1_Ice + type1_Normal + type1_Poison + type1_Psychic + type1_Rock +   
                      type1_Steel + type1_Water, family = binomial(link = "logit"), data = trainData)

modelList[[5]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + atk*spAtk + def*spDef + monoType, family = binomial(link = "logit"), data = trainData)

modelList[[6]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + atk*spAtk + def*spDef + def*spDef*spe + monoType, family = binomial(link = "logit"), data = trainData)

modelList[[7]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + atk*def + spAtk*spDef + monoType, family = binomial(link = "logit"), data = trainData)


summary(modelList[[1]])
summary(modelList[[2]])
summary(modelList[[3]])
summary(modelList[[4]])
summary(modelList[[5]])
summary(modelList[[6]])
summary(modelList[[7]])











# Filter out mono type Pokemons, there are 386 of them
dataMono <- filter(data, !(type2 %in% type))

dataMonoStats <- dataMono %>% select(dots = statName)
colnames(dataMonoStats) <- statName

# Plot attack and defense
corrplot(cor(dataMonoStats), addCoef.col = "black", type = "upper")

# Plot HP vs Speed
ggplot(dataMono, aes(x = hp, y = spe)) + geom_point() + facet_grid(. ~ type1)

