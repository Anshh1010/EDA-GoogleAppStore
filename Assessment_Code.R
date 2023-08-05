library(openxlsx)
library(readxl)
#Reading divided datasets
df1<-read_excel('/Users/anshhchaturvedi/Desktop/Assessment.xlsx')
df1
df2<-read_excel('/Users/anshhchaturvedi/Desktop/Assessment2.xlsx')
df2
df3<-read_excel('/Users/anshhchaturvedi/Desktop/Assessment3.xlsx')
df3
#Merging divided datasets
df4<-merge(df1,df2,by="ID")
df5<-merge(df4,df3,by="ID")
write.xlsx(df5,'/Users/anshhchaturvedi/Desktop/Assessment_Combined.xlsx')
df<-data.frame(df5)
#Since NA values are taken as string input in .xlsx format, I've changed the string values to actual NA values, so that we can use is.na functions
df[df=="NA"]<-NA
df[df=="NaN"]<-NA

mymode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

colnames(df)
df$Category[is.na(df$Category)]<-mymode(df$Category)
df$Category
df$Rating[is.na(df$Rating)]<-mean(!is.na(df$Rating))
df$Rating
df$Reviews[is.na(df$Reviews)]<-mean(!is.na(df$Reviews))
df$Reviews
#For sizes ending in kB
#Mode of Size variable is Varies with Size
input <- df$Size
output <- sapply(input, function(x) {
  ifelse(grepl("k$", x), paste0(0.001*as.numeric(sub("(\\d+(?:\\.\\d+)?)k", "\\1", x)), "M"), x)
})
output
df$Size<-output
df$Size[is.na(df$Size)]<-mymode(df$Size)
df$Size
#Changing data type of Installs from string to numeric
df$Installs<-as.integer(df$Installs)
class(df$Installs)
df$Installs[is.na(df$Installs)]<-mean(!is.na(df$Installs))
df$Installs

df$Type[is.na(df$Type)]<-mymode(df$Type)
df$Type
#Changing price values to integer from string
df$Price<-as.integer(df$Price)
class(df$Price)
df$Price[is.na(df$Price)]<-mymode(df$Price)
df$Price

df$Content.Rating[is.na(df$Content.Rating)]<-mymode(df$Content.Rating)
df$Content.Rating
df$Genres[is.na(df$Genres)]<-mymode(df$Genres)
df$Genres
df$Last.Updated[is.na(df$Last.Updated)]<-mymode(df$Last.Updated)
df$Last.Updated
#mode of Current.Ver is also Varies with device
df$Current.Ver[is.na(df$Current.Ver)]<-mymode(df$Current.Ver)
df$Current.Ver

df$Android.Ver[is.na(df$Android.Ver)]<-mymode(df$Android.Ver)
df$Android.Ver
write.xlsx(df,'/Users/anshhchaturvedi/Desktop/Assessment_Final.xlsx')

