#Exclude not native speakers
natives <- subset(Info.Plurals, grepl('en',Info.Plurals$Language, ignore.case=TRUE))
not_natives <- subset(Info.Plurals, !(Subject %in% natives$Subject))
Info.Plurals <- subset(Info.Plurals, (Subject %in% natives$Subject))
Data.Plurals <- subset(Data.Plurals, !(Subject %in% not_natives$Subject))

#Information subjects
mean(Info.Plurals$Age)
sd(Info.Plurals$Age)

#Criteria of exclusion: Having a touch screen
mouse_exclusion <- Info.Plurals$Subject[Info.Plurals$Clicker != 'mouse' ] 

#Criteria of exclusion: Having a touch screen (NOT USING NOW)
lefthanded <- Info.Plurals$Subject[Info.Plurals$Handeness == 'left'] 

#Criteria of exclusion ACCURACY: <80% accuracy in controls
accuracy_per_subject_controls <- aggregate(Accuracy~Subject, data= subset(Data.Plurals, Sentence_Type=='controlp' | Sentence_Type == 'controln'), mean, na.rm=T)
accuracy_exclusion <- accuracy_per_subject_controls$Subject[accuracy_per_subject_controls$Accuracy < 0.80] 

#restrict analyses 
BADSUBJECTS <- c(accuracy_exclusion, mouse_exclusion)
print(BADSUBJECTS)
Data.Plurals <- subset(Data.Plurals, !(Subject %in% BADSUBJECTS))
Info.Plurals <- subset(Info.Plurals, !(Subject %in% BADSUBJECTS))
Data.Plurals$Subject <- factor(Data.Plurals$Subject)
Info.Plurals$Subject <- factor(Info.Plurals$Subject)

controls <- subset(Data.Plurals, Type=='control' & Accuracy==1)
experimental_items <- subset(Data.Plurals, Type=='target' & Accuracy==1)

