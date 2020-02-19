#---------- POKEMON CHALLENGE ----------------#

#----------CHAN LE, 19.02.20 -----------------#


library(tidyverse)
library(Amelia)
library(corrplot)

data <- read.csv2("pokemon.csv", sep = ",")

colnames(data) <- c("index","name","type1","type2","hp","atk","def","spAtk","spDef","spe","gen","legendary")

table(data$type2)

type <- unique(data$type1)

statName <- c("hp","atk","def","spAtk","spDef","spe")

# Filter out mono type Pokemons, there are 386 of them
dataMono <- filter(data, !(type2 %in% type))

dataMonoStats <- dataMono %>% select(dots = statName)
colnames(dataMonoStats) <- statName

# Plot attack and defense
corrplot(cor(dataMonoStats), addCoef.col = "black", type = "upper")

# Plot HP vs Speed
ggplot(dataMono, aes(x = hp, y = spe)) + geom_point() + facet_grid(. ~ type1)

