#---------- POKEMON CHALLENGE ----------------#

#----------CHAN LE, 19.02.20 -----------------#

install.packages("fastDummies")
install.packages("AICcmodavg")

library(tidyverse)
library(Amelia)
library(corrplot)
library(fastDummies)
library(AICcmodavg)
library(caret)


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

dataDummy <- dataDummy %>% mutate_at(vars(matches("type")), as.factor)

# # Descriptive Plotting
# Plot distribution of legendary Pokemons across types
data %>% filter(legendary == "True") %>% 
  select(type1, type2) %>% 
  pivot_longer(everything(), names_to = "TypeNo", values_to = "Type") %>% 
  ggplot(aes(x = Type)) + geom_bar()




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

modelList[[2]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + monoType, family = binomial(link = "logit"), data = trainData)

modelList[[3]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + atk*spAtk + def*spDef + monoType, family = binomial(link = "logit"), data = trainData)

modelList[[4]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + atk*spAtk + def*spDef + def*spDef*spe + monoType, family = binomial(link = "logit"), data = trainData)

modelList[[5]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + atk*def + spAtk*spDef + monoType, family = binomial(link = "logit"), data = trainData)

modelList[[6]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + 
                        atk*spAtk + 
                        def*spDef + 
                        atk*def + 
                        spAtk*spDef + 
                        hp*def +
                        hp*spDef +
                        monoType, family = binomial(link = "logit"), data = trainData)

modelList[[7]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + 
                        atk*spAtk + 
                        def*spDef + 
                        atk*def + 
                        spAtk*spDef + 
                        monoType, family = binomial(link = "logit"), data = trainData)

modelList[[8]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + 
                        atk*spAtk + 
                        def*spDef*hp + 
                        atk*def + 
                        spAtk*spDef + 
                        monoType, family = binomial(link = "logit"), data = trainData)

modelList[[9]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + 
                        atk*spAtk + 
                        def*spDef*hp + 
                        atk*def + 
                        spAtk*spDef + 
                        monoType + type1_Dragon, family = binomial(link = "logit"), data = trainData)

modelList[[10]] <- glm(legendary ~ spe + 
                        atk*spAtk + 
                        def*spDef*hp + 
                        atk*def + 
                        spAtk*spDef + 
                        monoType, family = binomial(link = "logit"), data = trainData)

modelList[[11]] <- glm(legendary ~ spe + 
                         atk:spAtk + 
                         atk:def + 
                         spAtk:spDef + 
                         monoType, family = binomial(link = "logit"), data = trainData)


modelList[[12]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + atk*def + spAtk*spDef, family = binomial(link = "logit"), data = trainData)



summary(modelList[[1]])
summary(modelList[[2]])
summary(modelList[[3]])
summary(modelList[[4]])
summary(modelList[[5]])
summary(modelList[[6]])
summary(modelList[[7]])
summary(modelList[[8]])
summary(modelList[[9]])
summary(modelList[[10]])
summary(modelList[[11]])

names <- paste("mod",1:length(modelList),sep = "")

# Compare candidate models
aictab(cand.set = modelList, modnames = names, sort = TRUE)

modelListAvg <- list(modelList[[5]], modelList[[12]])

# Create average model
modavg(modelListAvg)

# Check accuracy in training data
predictLabel <- predict(modelList[[5]], type = "response")
predictLabel <- ifelse(predictLabel < 0.5, FALSE, TRUE)

table(predictLabel, trainData$legendary)







# Filter out mono type Pokemons, there are 386 of them
dataMono <- filter(data, !(type2 %in% type))

dataMonoStats <- dataMono %>% select(dots = statName)
colnames(dataMonoStats) <- statName

# Plot attack and defense
corrplot(cor(dataMonoStats), addCoef.col = "black", type = "upper")

# Plot HP vs Speed
ggplot(dataMono, aes(x = hp, y = spe)) + geom_point() + facet_grid(. ~ type1)

