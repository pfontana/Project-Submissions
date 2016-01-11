tumblr2=read.csv("tumblrData2.csv")
hour2 <- tumblr2$hour
notes2 <- tumblr2$notes
plot(hour2, notes2, main="Notes by Hour", xlab="Hour", ylab="Total Notes per Post")
notes_index2 <- tumblr2$notes_index
plot(hour2, notes_index2, main="Notes Index by Hour", xlab="Hour", ylab="Notes Index")

str(tumblr2)

#linear regression with notes_index
tumblr3 = subset(tumblr2, post_frequency<6)
notes_indexReg=lm(notes_index ~ hour + post_frequency,data=tumblr3)
summary(notes_indexReg)
notes_indexReg$residuals
SSE = sum(notes_indexReg$residuals^2)
SSE

#linear regression with notes
tumblr3 = subset(tumblr2, post_frequency<6)
notesReg=lm(notes ~ hour + post_frequency,data=tumblr3)
summary(notesReg)
notesReg$residuals
SSE = sum(notesReg$residuals^2)
SSE

#logistic regression with notes_index
tumblr3 = subset(tumblr2, post_frequency<6)
summary(glm(notes_index ~ hour, data=tumblr3))
summary(glm(notes_index ~ post_frequency, data=tumblr3))
LogModel = glm(notes_index ~ hour + post_frequency, data=tumblr3)
summary(LogModel)
cor(tumblr3$notes_index, tumblr3$hour)
cor(tumblr3$notes_index, tumblr3$post_frequency)

#logistic regression with notes
tumblr3 = subset(tumblr2, post_frequency<6)
summary(glm(notes ~ hour, data=tumblr3))
summary(glm(notes ~ post_frequency, data=tumblr3))
LogModel = glm(notes ~ hour + post_frequency, data=tumblr3)
summary(LogModel)
cor(tumblr3$notes, tumblr3$hour)
cor(tumblr3$notes, tumblr3$post_frequency)

NotesIndexByHourTable2 <- aggregate(tumblr2$notes_index, list(tumblr2$hour), FUN=mean)
NotesIndexByHourTable2

NotesIndexByPostFrequencyTable2 <- aggregate(tumblr2$notes_index, list(tumblr2$post_frequency), FUN=mean)
NotesIndexByPostFrequencyTable2

plot(NotesIndexByPostFrequencyTable2, main="Notes Index by Post Frequency (in a day)",xlab="Number of Posts (in a day)",ylab="Notes Index")

AverageNotesByPostFrequencyTable2 <- aggregate(tumblr2$notes, list(tumblr2$post_frequency), FUN=mean)
AverageNotesByPostFrequencyTable2

plot(AverageNotesByPostFrequencyTable2, main="Average Notes per Post by Post Frequency (in a day)",xlab="Number of Posts (in a day)",ylab="Average Notes per Post")


tumblr_calvin = subset(tumblr2, blog=="calvinklein")
tumblr_disney = subset(tumblr2, blog=="disney")
tumblr_thesignal = subset(tumblr2, blog=="thesignal")
tumblr_gq = subset(tumblr2, blog=="gq")
tumblr_glamour = subset(tumblr2, blog=="glamour")
tumblr_adidas = subset(tumblr2, blog=="adidas")
tumblr_comedy = subset(tumblr2, blog=="comedycentral")
calvin_NotesIndexByPostFrequencyTable <- aggregate(tumblr_calvin$notes_index, list(tumblr_calvin$post_frequency), FUN=mean)
plot(calvin_NotesIndexByPostFrequencyTable, type="o",col="red",main="Calvin Klein Notes Index by Post Frequency (in a day)",xlab="Number of Posts (in a day)",ylab="Notes Index")
thesignal_NotesIndexByPostFrequencyTable <- aggregate(tumblr_thesignal$notes_index, list(tumblr_thesignal$post_frequency), FUN=mean)
plot(thesignal_NotesIndexByPostFrequencyTable, type="o",col="blue",main="The Signal Notes Index by Post Frequency (in a day)",xlab="Number of Posts (in a day)",ylab="Notes Index")
disney_NotesIndexByPostFrequencyTable <- aggregate(tumblr_disney$notes_index, list(tumblr_disney$post_frequency), FUN=mean)
plot(disney_NotesIndexByPostFrequencyTable, type="o",col="pink",main="Disney Notes Index by Post Frequency (in a day)",xlab="Number of Posts (in a day)",ylab="Notes Index")
glamour_NotesIndexByPostFrequencyTable <- aggregate(tumblr_glamour$notes_index, list(tumblr_glamour$post_frequency), FUN=mean)
plot(glamour_NotesIndexByPostFrequencyTable, type="o",col="green",main="Glamour Notes Index by Post Frequency (in a day)",xlab="Number of Posts (in a day)",ylab="Notes Index")
gq_NotesIndexByPostFrequencyTable <- aggregate(tumblr_gq$notes_index, list(tumblr_gq$post_frequency), FUN=mean)
plot(gq_NotesIndexByPostFrequencyTable, type="o",col="orange",main="GQ Notes Index by Post Frequency (in a day)",xlab="Number of Posts (in a day)",ylab="Notes Index")
comedy_NotesIndexByPostFrequencyTable <- aggregate(tumblr_comedy$notes_index, list(tumblr_comedy$post_frequency), FUN=mean)
plot(comedy_NotesIndexByPostFrequencyTable, type="o",col="purple",main="Comedy Central Notes Index by Post Frequency (in a day)",xlab="Number of Posts (in a day)",ylab="Notes Index")
adidas_NotesIndexByPostFrequencyTable <- aggregate(tumblr_adidas$notes_index, list(tumblr_adidas$post_frequency), FUN=mean)
plot(adidas_NotesIndexByPostFrequencyTable, type="o",col="yellow",main="Adidas Central Notes Index by Post Frequency (in a day)",xlab="Number of Posts (in a day)",ylab="Notes Index")




BRFB=read.csv("BRFB.csv")
str(BRFB)
FBhour <- BRFB$hour
FBengagement_rate <- BRFB$engagement_rate
FBdow <- BRFB$dow
plot(FBhour, FBengagement_rate, main="Engagement by Hour", xlab="Hour", ylab="Engagement Rate")
FBinteractions <- BRFB$interactions
plot(FBhour, FBinteractions, main="Interactions by Hour", xlab="Hour", ylab="Interactions")

#linear regression with engagement_rate
FBeng_rateReg=lm(engagement_rate ~ hour + post_frequency + dowNum,data=BRFB)
summary(FBeng_rateReg)
FBeng_rateReg$residuals
SSE = sum(FBeng_rateReg$residuals^2)
SSE

#linear regression with interactions
FBinteractionsReg=lm(interactions ~ hour + post_frequency + dowNum,data=BRFB)
summary(FBinteractionsReg)
FBinteractionsReg$residuals
SSE = sum(FBinteractionsReg$residuals^2)
SSE

#linear regression with interactions
FBinteractionsReg=lm(interactions ~ hour + post_frequency,data=BRFB)
summary(FBinteractionsReg)
FBinteractionsReg$residuals
SSE = sum(FBinteractionsReg$residuals^2)
SSE

#logistic regression with engagement_rate
summary(glm(engagement_rate ~ dowNum,data=BRFB))
summary(glm(engagement_rate ~ post_frequency,data=BRFB))
summary(glm(engagement_rate ~ hour,data=BRFB))
LogModel = glm(engagement_rate ~ hour + post_frequency + dowNum,data=BRFB)
summary(LogModel)
cor(BRFB$engagement_rate, BRFB$hour)
cor(BRFB$engagement_rate, BRFB$post_frequency)
cor(BRFB$engagement_rate, BRFB$dowNum)

#logistic regression with interactions
summary(glm(interactions ~ dowNum,data=BRFB))
summary(glm(interactions ~ post_frequency,data=BRFB))
summary(glm(interactions ~ hour,data=BRFB))
LogModel = glm(interactions ~ hour + post_frequency + dowNum,data=BRFB)
summary(LogModel)
cor(BRFB$interactions, BRFB$hour)
cor(BRFB$interactions, BRFB$post_frequency)
cor(BRFB$interactions, BRFB$dowNum)

FBEngByHourTable <- aggregate(BRFB$engagement_rate, list(BRFB$hour), FUN=mean)
FBEngByHourTable

FBEngByPostFrequencyTable <- aggregate(BRFB$engagement_rate, list(BRFB$post_frequency), FUN=mean)
FBEngByPostFrequencyTable

FBEngBydowNumTable <- aggregate(BRFB$engagement_rate, list(BRFB$dowNum), FUN=mean)
FBEngBydowNumTable

FBInteractionsByHourTable <- aggregate(BRFB$interactions, list(BRFB$hour), FUN=mean)
FBInteractionsByHourTable

FBInteractionsByPostFrequencyTable <- aggregate(BRFB$interactions, list(BRFB$post_frequency), FUN=mean)
FBInteractionsByPostFrequencyTable

FBInteractionsBydowNumTable <- aggregate(BRFB$interactions, list(BRFB$dowNum), FUN=mean)
FBInteractionsBydowNumTable

plot(FBInteractionsByPostFrequencyTable, main="Average Interactions per Post by Post Frequency (in a day)",xlab="Number of Posts (in a day)",ylab="Average Interactions per Post")

#linear regression with reach
FBreachReg=lm(total_reach ~ hour + post_frequency + dowNum,data=BRFB)
summary(FBreachReg)




#linear regression with interactions
FBinteractionsReg=lm(interactions ~ hour + post_frequency + dowNum + year + month + hour + total_reach + viral_reach + impressions+ Wkend + engaged_users + engagement_rate,data=BRFB)
summary(FBinteractionsReg)

#linear regression with reach
FBreachReg=lm(total_reach ~ hour + post_frequency + dowNum + year + month + hour + Wkend + interactions + viral_reach + impressions + engaged_users + engagement_rate,data=BRFB)
summary(FBreachReg)




#descision trees
install.packages("rpart")
library(caTools)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
FBTree=rpart(post_frequency ~ hour + dowNum + year + month + hour + Wkend,data=BRFB, method=class, minbucket=25)
PredictCART = predict(FBTree,newdata=FBTest,type="class")
table(FBTest$post_frequency,PredictCart)
#sum Rights / sum all
#gives accuracy percentage of the model

install.packages("randomForest")
library(randomForest)

FBTree_Interactions=rpart(post_frequency ~ interactions,data=BRFB,minbucket=2)
prp(FBTree_Interactions)


FBTree_Reach=rpart(post_frequency ~ total_reach,data=BRFB,minbucket=5)
prp(FBTree_Reach)

