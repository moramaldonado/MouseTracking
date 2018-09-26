#set working directory
getwd()
#charging tha data.... 
#information file
Info.Plurals <- read.csv(file="Data/Information_Plurals.csv", header=TRUE, sep=",")
Info.Plurals$Subject<- c(1:length(Info.Plurals$Subject))

#data file
Data.Plurals <- read.csv(file="Data/Data_Plurals.csv", header=TRUE, sep=",")
Data.Plurals$RT.Log <- log(Data.Plurals$RT)

var = 0
for (i in levels(factor(Data.Plurals$Subject)))
{
  var = var + 1
  Data.Plurals$Subject[Data.Plurals$Subject == i] <- var}

Data.Plurals$Item.number<- mapvalues(Data.Plurals$Item.number, from = c(0:273), to = c(1:274))

Data.Plurals$Type[Data.Plurals$Sentence_Type=='starget' & Data.Plurals$Truth.value == 'True'] <- 'target'
Data.Plurals$Type[Data.Plurals$Sentence_Type=='starget' & Data.Plurals$Truth.value == 'False'] <- 'filler'
Data.Plurals$Type[Data.Plurals$Sentence_Type=='controlp' |  Data.Plurals$Sentence_Type=='controln'] <- 'control'

Data.Plurals$Sentence_Type <- as.character(Data.Plurals$Sentence_Type)
Data.Plurals$Sentence_Type[Data.Plurals$Condition == 'wc'] <- 'wc'
Data.Plurals$Sentence_Type <- as.factor(Data.Plurals$Sentence_Type)

levels(Data.Plurals$Condition) <- c('Cumulative', 'Distributive','At least C')
Data.Plurals$Condition <- factor(Data.Plurals$Condition, levels = rev(levels(Data.Plurals$Condition)))



Data.Plurals$Picture[Data.Plurals$Quantifier == 21 & Data.Plurals$Condition == 'Distributive'] <- "2,1"
Data.Plurals$Picture[Data.Plurals$Quantifier == 22 & Data.Plurals$Condition == 'Cumulative'] <- "2,1"
Data.Plurals$Picture[Data.Plurals$Quantifier == 22 & Data.Plurals$Condition == 'Distributive'] <- "2,2"
Data.Plurals$Picture[Data.Plurals$Quantifier == 24 & Data.Plurals$Condition == 'Cumulative'] <- "2,2"
Data.Plurals$Picture[Data.Plurals$Quantifier == 24 & Data.Plurals$Condition == 'Distributive'] <- "2,4"
Data.Plurals$Picture[Data.Plurals$Quantifier == 28 & Data.Plurals$Condition == 'Cumulative'] <- "2,4"

Data.Plurals$Quantifier <- as.character(Data.Plurals$Quantifier)
Data.Plurals$Quantifier <- as.factor(Data.Plurals$Quantifier)
levels(Data.Plurals$Quantifier) <- c('2,1','2,2','2,4','2,8')


Data_Plurals.positions <- read.csv(file="Data/Data_Plurals_positions.csv", header=TRUE, sep=",")

Data_Plurals.positions$Type[Data_Plurals.positions$Sentence_Type=='starget' & Data_Plurals.positions$Truth.value == 'True'] <- 'target'
Data_Plurals.positions$Type[Data_Plurals.positions$Sentence_Type=='starget' & Data_Plurals.positions$Truth.value == 'False'] <- 'filler'
Data_Plurals.positions$Type[Data_Plurals.positions$Sentence_Type=='controlp' |  Data_Plurals.positions$Sentence_Type=='controln'] <- 'control'


