library(dplyr)
library(readr)
library(ggplot2)
by_tag_year <- read.csv("C:/Users/Amaan Sajid/OneDrive/Desktop/R/QueryResults_selected.csv")
df<- data.frame(by_tag_year)
head(df)
names(df)

year<-unique(df$Year)

python<- subset(df, TagName=="python")
print(python)
if (any(df$TagName == "python")) {
  plot(df$Year[df$TagName == "python"], df$TagCount[df$TagName == "python"],
       xlab = "Years", ylab = "Python Tag Count",
       main = "Python Tag Count Over the Years",
       type = "l", col = "blue")
}
#---------------------------------------------------
r<- subset(df, TagName=="r")
print(r)
if (any(df$TagName == "r")) {
  plot(df$Year[df$TagName == "r"], df$TagCount[df$TagName == "r"],
       xlab = "Years", ylab = "R Tag Count",
       main = "R Tag Count Over the Years",
       type = "l", col = "blue")
}
#----------------------------------------------------
proglang<-unique(df$TagName)
print(proglang)

tottag<- unique(df$TotalTagsUsed)

#---------------------------------------------------
selected_tag<-c("c","python","r","javascript")
tags<-df[df$TagName %in% selected_tag,]

ggplot(tags,aes(x=Year,y=TagCount,color=TagName))+geom_line()+labs(x = "Year", y = "Tag Count", title = "Tag Count Over the Years") +
  theme_minimal()

#---------------------------------------------------

tag_sum<- aggregate(TagCount ~ TagName,data=df,sum)
print(tag_sum)

colors <- c("red", "blue", "green", "purple", "orange", "pink", "brown", "cyan", "magenta", "blue")
par(mar = c(5, 5, 4, 2)) 
barplot(tag_sum$TagCount, names.arg = tag_sum$TagName, col = colors,
       main = "Total Tag Count Over Years",las = 2)

#------------------------------------------------------------
tag_sum <- tag_sum[order(-tag_sum$TagCount), ]
print(tag_sum)
top_5_tags <- head(tag_sum, 5)
print(top_5_tags)

filtered_df <- df[df$TagName %in% top_5_tags$TagName, ]

ggplot(filtered_df,aes(x=Year,y=TagCount,color=TagName))+geom_line()+labs(x = "Top 5 programming Lang") +
  theme_minimal()