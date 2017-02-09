## Code to send emails
## Started 1/8/17

## For every sending email need to login and turn on access for less secure apps on sending email https://www.google.com/settings/security/lesssecureapps

## Program needs two .csv files one with sending email addresses called SendList.csv with columns called Email and Psswd, the other .csv is of receiving emails and has one column called Emails.

# install.packages("mailR") 
# 



#Load Library
library(mailR)


#Function to send emails to recipient and bcc email list using individual email address

SpamEmails<-function(SendList,RecipientList,bccList,subjectLine,bodyEmail,Password){
  
  sender <- SendList
  recipients <- RecipientList
  send.mail(from = sender,
            to = recipients,
            bcc = bccList,
            subject = subjectLine,
            body = bodyEmail,
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = SendList,            
                        passwd = Password, ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
}


##Function to create list of lists to feed into SpamEmails function

SpamInput<-function(SendDf,RecipientDf,SubjectText,BodyText,ListLen){
  
  ## Shuffle SendList
  
  df2 <- SendDf[sample(nrow(SendDf)),]   
  
  RecipLen<-floor(nrow(RecipientDf)/(length(SendDf$Email)*ListLen))*length(SendDf$Email)*ListLen
  
  ShortList<-RecipientDf$Emails[1:RecipLen]
  
  
  ExampleList2<-Map(list,
                    SendList = as.character(SendDf$Email),
                    RecipientList = as.character(RecipientDf[1,]),
                    bccList=split(ShortList, rep(1:length(SendDf$Email), each = ListLen)),
                    subjectLine=SubjectText,
                    bodyEmail=BodyText,
                    Password = as.character(SendDf$Psswd)
  )
  
  
  ExampleList2
  
}


## Function to run SpamEmails on input list of lists

TrumpFucker<-function(Emailsdf,receivedf,SubText,BodText,RecipLen){
  
  spamlist<-SpamInput(SendDf=Emailsdf,RecipientDf=receivedf,SubjectText=SubText,BodyText=BodText,ListLen=RecipLen)
  
  # lapply(testList,SpamEmails)
  lapply(spamlist,function(x){do.call(SpamEmails, args=x)})
}



# Reading in send and receive email lists

SendListdf <- read.csv(file="/Desktop/Email Spam Function/SendList.csv", header=TRUE, stringsAsFactors = FALSE)
RecipientListdf <- read.csv(file="/Desktop/Email Spam Function/RecipientList.csv", header=TRUE, stringsAsFactors = FALSE)


## TestRun

# text1<-"Tell Trump to Shove it!"
# 
# text2<-"I never thought I'd root for a group of lawyers to do anything but fall off a bridge, but God bless the ACLU!!  Please tell your representatives to tell Trump to shove his racist travel ban.  This is not American, this not what decent people stand for!  https://www.aclu.org/blog/speak-freely/president-trumps-first-week-aclu-hands-him-first-stinging-rebuke?redirect=blog/president-trumps-first-week-aclu-hands-him-first-stinging-rebuke"

text3<-"Don't Let Trump Kill Children"

text4<-"One of the most disturbing things I can remember from the 2016 campaign was Trump saying: they care about their families.  You have to go after their families.  I would take out their families.

Later when confronted during the republican debates with the fact that many in the armed services said they would refuse his unlawful order to kill the innocent relatives of terrorists, because it was a war crime, he said: They won't refuse.  They're not going to refuse me.  Believe me.  

I have family in the military, how will Trump get my family to revenge kill innocent un-armed men, women, and children?  STOP TRUMP!"

TrumpFucker(Emailsdf=SendListdf,receivedf=RecipientListdf,SubText=text3,BodText=text4,RecipLen=98)



