#Presidential Speaker Classification with Native Bayes

install.packages("tm")
library(tm)
install.packages("plyr")
library(plyr)
##Pre-Processing.

#Get file addresses of Bush and Obama speeches
Bush.folder=("C:\\Users\\peach\\Desktop\\Speeches\\George Bush")
Obama.folder=("C:\\Users\\peach\\Desktop\\Speeches\\Barack Obama")

Bush.speeches.files = list.files(Bush.folder)
Obama.speeches.files = list.files(Obama.folder)

Bush.speeches.files.addresses=paste(Bush.folder, Bush.speeches.files, sep="\\")
Obama.speeches.files.addresses=paste(Obama.folder, Obama.speeches.files, sep="\\")

#create corpus of Bush and Obama speeches
Bush.corpus=lapply(Bush.speeches.files.addresses, FUN= readLines)
Obama.corpus=lapply(Obama.speeches.files.addresses, FUN=readLines)

#combine text of all txt files into one massive document for each President. Have space between each speech.
Bush.corpus.1=lapply(Bush.corpus, FUN=paste,collapse=" ")
Obama.corpus.1=lapply(Obama.corpus, FUN=paste,collapse=" ")

#remove all non-word characters from corpora. Replace with a space.
Bush.corpus.2=gsub(pattern="\\W", " ", Bush.corpus.1)
Obama.corpus.2=gsub(pattern="\\W", " ", Obama.corpus.1)

#remove all digit characters from corpora. Replace with a space.
Bush.corpus.3=gsub(pattern="\\d", " ", Bush.corpus.2)
Obama.corpus.3=gsub(pattern="\\d", " ", Obama.corpus.2)

#make corpora all lowercase.
Bush.corpus.4=tolower(Bush.corpus.3)
Obama.corpus.4=tolower(Obama.corpus.3)

#remove stopwords from corpora
Bush.corpus.5=removeWords(Bush.corpus.4, stopwords())
Obama.corpus.5=removeWords(Obama.corpus.4, stopwords())

#remove "will" and "shall" from the corpora
words=c("will","shall")
Bush.corpus.6=removeWords(Bush.corpus.5, words)
Obama.corpus.6=removeWords(Obama.corpus.5,words)

#replace all single-length words in the corpora with with a space.
Bush.corpus.7=gsub(pattern="\\b[A-z]\\b{1}", replace=" ", Bush.corpus.6)
Obama.corpus.7=gsub(pattern="\\b[A-z]\\b{1}", replace=" ", Obama.corpus.6)

#reduce white space to a single space in corpora.
Bush.corpus.8=stripWhitespace(Bush.corpus.7)
Obama.corpus.8=stripWhitespace(Obama.corpus.7)

#######

#preprocessing is completed. Turn corpora into objects of VectorSource.
Bush.corpus.9=VectorSource(Bush.corpus.8)
Obama.corpus.9=VectorSource(Obama.corpus.8)

#convert corpora into "tm" corpus.
Bush.corpus.10=Corpus(Bush.corpus.9)
Obama.corpus.10=Corpus(Obama.corpus.9)

#turn corpora into term document matrices.
Bush.tdm.1=TermDocumentMatrix(Bush.corpus.10)
Obama.tdm.1=TermDocumentMatrix(Obama.corpus.10)

#create tdm where terms must appear at least 70% of documents to be included.
Bush.tdm.2=removeSparseTerms(Bush.tdm.1, 0.70)
Obama.tdm.2=removeSparseTerms(Obama.tdm.1,0.70)

#transpose Bush and Obama tdm
Bush.tdm.3=t(Bush.tdm.2)
Obama.tdm.3=t(Obama.tdm.2)

#convert tdms into data frames. (Cannot do directly.)
Bush.tdm.4=as.data.frame(as.matrix(Bush.tdm.3))
Obama.tdm.4=as.data.frame(as.matrix(Obama.tdm.3))

#check first 5 rows and 7 columns of Obama.tdm.4 to see if any data frame labels appear
Obama.tdm.4[1:5,1:7]

#with cbind, add label "44" to the data frame you have just created. [5,7]
cbind(Obama.tdm.4[1:5,1:7], "44")

#Label Bush.tdm.4 and Obama.tdm.4 with "Bush" and "Obama" respectively. Save into new tdm.
Bush.tdm.5=cbind(Bush.tdm.4, "Bush")
Obama.tdm.5=cbind(Obama.tdm.4, "Obama")

#Name the Bush and Obama columns "LABEL"
colnames(Bush.tdm.5)[ncol(Bush.tdm.5)]="LABEL"
colnames(Obama.tdm.5)[ncol(Obama.tdm.5)]="LABEL"

#combine latest version of Bush.tdm.5/Obama.tdm.5 by row. Name new data frame 'stack.1'.
stack.1=rbind.fill(Bush.tdm.5,Obama.tdm.5)

#make stack2 by substituting all N/A values with 0. print first 5 columns to double-check.
stack.2=stack.1
stack.2[is.na(stack.2)]=0
stack.2[,1:5]

#create dataframe containing only the Label column from stack.2
stackLabel=stack.2[,"LABEL"]

#create stack.3, which is stack.2 but without the Label column
stack.3=stack.2
stack.3=stack.3[,colnames(stack.2)!="LABEL"]

#create training and testing data (80/20): one pair for Label and one pair for stack.3
ind=sample(2,nrow(stack.3),replace=TRUE,prob=c(0.8,0.2))
stack3train=stack.3[ind==1,]
stack3test=stack.3[ind==2,]
stackLabelTrain=stackLabel[ind==1]
stackLabelTest=stackLabel[ind==2]

#create function that returns 1 if positive number and 0 if not.
zero_one<-function(x){
  y=ifelse(x>0,1,0)
  y
}

#apply function to stack3train and stack3test, appending 0_1 suffix to name
trainingset0_1=zero_one(stack3train)
testingset0_1=zero_one(stack3test)

library(e1071)
##Prediction

#train naiveBayes with trainingset0_1 and stackLabelTrain
speaker_classifier=naiveBayes(trainingset0_1,stackLabelTrain)

#"save" output of speaker_classifer prediction with testingset0_1 to speaker_pred
speaker_pred=predict(speaker_classifier,testingset0_1)

##Accuracy

#calculate accuracy using table matrix
t=table(speaker_pred,stackLabelTest)
t
accuracy=sum(diag(t))/sum(t)*100
accuracy

#accuracy is 100%
