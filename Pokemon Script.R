#---------- POKEMON CHALLENGE ----------------#

#----------CHAN LE, 19.02.20 -----------------#

install.packages("fastDummies")
install.packages("AICcmodavg")
install.packages("randomForest")

library(tidyverse)
library(Amelia)
library(corrplot)
library(fastDummies)
library(AICcmodavg)
library(caret)
library(randomForest)

# # Input data and reset name ----
data <- read.csv2("pokemon.csv", sep = ",")

colnames(data) <- c("index","name","type1","type2","hp","atk","def","spAtk","spDef","spe","gen","legendary")

statName <- c("hp","atk","def","spAtk","spDef","spe")
type <- unique(data$type1)

# # Set dummies for Types ----
data$type2 <- as.character(data$type2)

data <- data %>% mutate(type2 = replace(type2, type2 == "", "Mono")) 

data$type2 <- as.factor(data$type2)

# Dumify dual types
dataDummy <- data %>% 
  mutate(type = paste0(type1,"_", type2)) %>% # Combine 2 types into one cells
  select(-type1, -type2) %>% 
  dummy_cols(select_columns = "type", 
             split = "_") # Split them again to have multiple categories in the dummies

# Remove column used for dummification and 1 other column to avoid multicollinearity
dataDummy <- dataDummy %>% select(-type, -type_Normal)

# Take out index column
dataDummy <-  dataDummy %>% select(-index, -name, -gen)

# # Descriptive Plotting
# Plot distribution of legendary Pokemons across types
data %>% filter(legendary == "True") %>% 
  select(type1, type2) %>% 
  pivot_longer(everything(), names_to = "TypeNo", values_to = "Type") %>% 
  ggplot(aes(x = Type)) + geom_bar()

# Plot correlation plot
dataDummy %>% select(-legendary) %>% cor() %>% corrplot()

# # Train-test splitting ----
# Shuffle data
index <- createDataPartition(dataDummy$hp, p = 0.8,
                             list = F, times = 1)

trainData <- dataDummy[index,]

testData <- dataDummy[-index,]
testDataLabel <- testData$legendary
test <- select(testData, -legendary)

# # Fit candidate models ----
modelList <- list()

modelList[[1]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe, family = binomial(link = "logit"), data = trainData)

modelList[[2]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + type_Mono, family = binomial(link = "logit"), data = trainData)

modelList[[3]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + atk*spAtk + def*spDef + type_Mono, family = binomial(link = "logit"), data = trainData)

modelList[[4]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + atk*spAtk + def*spDef + def*spDef*spe + type_Mono, family = binomial(link = "logit"), data = trainData)

modelList[[5]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + atk*def + spAtk*spDef + type_Mono, family = binomial(link = "logit"), data = trainData)

modelList[[6]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + 
                        atk*spAtk + 
                        def*spDef + 
                        atk*def + 
                        spAtk*spDef + 
                        hp*def +
                        hp*spDef +
                        type_Mono, family = binomial(link = "logit"), data = trainData)

modelList[[7]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + 
                        atk*spAtk + 
                        def*spDef + 
                        atk*def + 
                        spAtk*spDef + 
                        type_Mono, family = binomial(link = "logit"), data = trainData)

modelList[[8]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + 
                        atk*spAtk + 
                        def*spDef*hp + 
                        atk*def + 
                        spAtk*spDef + 
                        type_Mono, family = binomial(link = "logit"), data = trainData)

modelList[[9]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + 
                        atk*spAtk + 
                        def*spDef*hp + 
                        atk*def + 
                        spAtk*spDef + 
                        type_Mono + type_Dragon, family = binomial(link = "logit"), data = trainData)

modelList[[10]] <- glm(legendary ~ spe + 
                        atk*spAtk + 
                        def*spDef*hp + 
                        atk*def + 
                        spAtk*spDef + 
                        type_Mono, family = binomial(link = "logit"), data = trainData)

modelList[[11]] <- glm(legendary ~ spe + 
                         atk:spAtk + 
                         atk:def + 
                         spAtk:spDef + 
                         type_Mono, family = binomial(link = "logit"), data = trainData)


modelList[[12]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + atk*def + spAtk*spDef, family = binomial(link = "logit"), data = trainData)

modelList[[13]] <- glm(legendary ~ hp + atk + def + spAtk + spDef + spe + 
                         atk*def + spAtk*spDef + type_Mono +
                         type_Psychic + type_Dragon + type_Flying, family = binomial(link = "logit"), data = trainData)



# # Summary and comparison ----
names <- paste("mod",1:length(modelList),sep = "")

# Compare candidate models
aictab(cand.set = modelList, modnames = names, sort = TRUE)

modelListAvg <- list(modelList[[5]], modelList[[12]])

# Create average model
modavg(modelListAvg, parm = "legendary")

# Check accuracy in training data
predictLabel <- predict(modelList[[5]], type = "response")
predictLabel <- ifelse(predictLabel < 0.5, FALSE, TRUE)

table(predictLabel, trainData$legendary)

# # Random forest model ----
modelListRF <- list()

# Train - Test split
trainDataRF <- data[index,] %>% select(-index, -name, -gen)
testDataRF <- data[-index,] %>% select(-index, -name, -gen, -legendary)
testDataRFLabel <- data[-index,]$legendary

modelListRF[[1]] <- randomForest(legendary ~ ., data = trainDataRF, 
                                 mtry = 3, ntree = 2500)

modelListRF[[2]] <- randomForest(legendary ~ ., data = trainData,
                                 mtry = 3, ntree = 2500)

modelListRF[[2]]$importance

# Create grid search result data frame
mtryGrid <- seq(2,ncol(trainData),1)
ntreeGrid <- seq(500, 3000, 500)

gridSearchRF <- expand.grid(mtryGrid, ntreeGrid)
gridSearchRF <- cbind(gridSearchRF, matrix(nrow = nrow(gridSearchRF), ncol = 3))
colnames(gridSearchRF) <- c("mtry","ntree","OBB","True","False")

# Grid search for best ntree and best mtry
temp <- 1

for (i in ntreeGrid){
  for (j in mtryGrid){
    modelRF <- randomForest(legendary ~ ., data = trainDataRF,
                                     mtry = j, ntree = i)   
    result <- modelRF$err.rate %>% as.data.frame() %>% 
      summarise_each(funs = mean) %>% as.matrix()
    gridSearchRF[temp,3:5] <- result
    
    temp <- temp + 1
  }
}

# Grid search result: mtry = 3, ntree = 2500
gridSearchRF %>% filter(OBB == min(OBB))

#----#
























# Filter out mono type Pokemons, there are 386 of them
dataMono <- filter(data, !(type2 %in% type))

dataMonoStats <- dataMono %>% select(dots = statName)
colnames(dataMonoStats) <- statName

# Plot attack and defense
corrplot(cor(dataMonoStats), addCoef.col = "black", type = "upper")

# Plot HP vs Speed
ggplot(dataMono, aes(x = hp, y = spe)) + geom_point() + facet_grid(. ~ type1)

