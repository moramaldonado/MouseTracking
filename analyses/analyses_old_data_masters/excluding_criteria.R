#Exclude not native speakers
natives <- subset(Info.Plurals, grepl('en',Info.Plurals$Language, ignore.case=TRUE))
not_natives <- subset(Info.Plurals, !(Subject %in% natives$Subject))
Info.Plurals <- subset(Info.Plurals, (Subject %in% natives$Subject))
Data.Plurals <- subset(Data.Plurals, !(Subject %in% not_natives$Subject))
Data_Plurals.positions <- subset(Data_Plurals.positions, !(Subject %in% not_natives$Subject))


#Information subjects
mean(Info.Plurals$Age)
sd(Info.Plurals$Age)

#Criteria of exclusion: Having a touch screen
mouse_exclusion <- Info.Plurals$Subject[Info.Plurals$Clicker != 'mouse'] 

#Criteria of exclusion: Having a touch screen (NOT USING NOW)
lefthanded <- Info.Plurals$Subject[Info.Plurals$Handeness == 'left'] 

#Criteria of exclusion ACCURACY: <80% accuracy in controls
accuracy_per_subject_controls <- aggregate(Accuracy~Subject, data= subset(Data.Plurals, Sentence_Type=='controlp' | Sentence_Type == 'controln'), mean, na.rm=T)
accuracy_exclusion <- accuracy_per_subject_controls$Subject[accuracy_per_subject_controls$Accuracy < 0.75] 
print(length(accuracy_exclusion ))

#restrict analyses 
BADSUBJECTS <- c(accuracy_exclusion, mouse_exclusion)
print(BADSUBJECTS)
Data_Plurals.positions <- subset(Data_Plurals.positions, !(Subject %in% BADSUBJECTS))
Data.Plurals <- subset(Data.Plurals, !(Subject %in% BADSUBJECTS))
Info.Plurals <- subset(Info.Plurals, !(Subject %in% BADSUBJECTS))
Data_Plurals.positions$Subject <- factor(Data_Plurals.positions$Subject)
Data.Plurals$Subject <- factor(Data.Plurals$Subject)
Info.Plurals$Subject <- factor(Info.Plurals$Subject)

## exclusion based on time
exclusion_RT <- c(mean(Data.Plurals$RT.Log)-2*sd(Data.Plurals$RT.Log), mean(Data.Plurals$RT.Log)+2*sd(Data.Plurals$RT.Log) )
print(mean(Data.Plurals$RT.Log))
print(sd(Data.Plurals$RT.Log))
print(exclusion_RT)
excluded_trials <- subset(Data.Plurals, RT.Log < exclusion_RT[1] | RT.Log > exclusion_RT[2]) #Maybe not the right thing to do
print(nrow(excluded_trials)/nrow(Data.Plurals))

Data.Plurals <- subset(Data.Plurals, RT.Log > exclusion_RT[1] & RT.Log < exclusion_RT[2]) 
Data_Plurals.positions$RT.Log <- log(Data_Plurals.positions$RT)
Data_Plurals.positions <- subset(Data_Plurals.positions, RT.Log > exclusion_RT[1] & RT.Log < exclusion_RT[2]) 
Data.Plurals$grp <- paste0(Data.Plurals$Subject,Data.Plurals$Item.number,sep='_')
Data_Plurals.positions$grp <- paste0(Data_Plurals.positions$Subject,Data_Plurals.positions$Item.number,sep='_')
identical(levels(Data_Plurals.positions$grp), levels(Data.Plurals$grp))



## dividing into two data sets: accurate controls and experimental items 
controls <- subset(Data.Plurals, Type=='control' & Accuracy==1)
controls$id <- 1:nrow(controls)
controls$Condition <- if_else(controls$Sentence_Type=='controlp', 'Pos', 'Neg')
counts<-data.frame(table(controls$Subject))
try <- controls %>%
  dplyr::select(Subject, Item.number, id)
  
controls.positions <- subset(Data_Plurals.positions,  Sentence_Type=='controlp'|Sentence_Type=='controln')
controls.positions$Condition <- if_else(controls.positions$Sentence_Type=='controlp', 'Pos', 'Neg')
controls.positions <- subset(controls.positions,  Accuracy==1)
controls.positions <-  dplyr::full_join(controls.positions, try, by=c("Subject", "Item.number"))


experimental_items <- subset(Data.Plurals, Type=='target')
experimental_items.positions <- subset(Data_Plurals.positions, Type=='target')




