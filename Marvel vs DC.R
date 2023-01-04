library(readr)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(gridExtra)
library(wesanderson)
library(pander)
library(plotly)
#
a<-"https://raw.githubusercontent.com/cosmoduende/r-marvel-vs-dc/main/dataset_shdb/heroesInformation.csv"
data<-read.csv(url(a))
View(data)
#
q<-"https://raw.githubusercontent.com/cosmoduende/r-marvel-vs-dc/main/dataset_shdb/charactersStats.csv"
characterstat<-read.csv(url(q))
View(characterstat)
#
powers<-read.csv("super_hero_powers.csv")
View(powers)
#subset
colnames(data)[colnames(data)=="name"]<-"Name"
marveldc<-data[(data$Publisher=="Marvel Comics"|data$Publisher=="DC Comics"),]
View(marveldc)
#REMOVE NAME DUPLICATES AND SELECT COLUMNS 
marveldc<-marveldc[!duplicated(marveldc$Name),]
marveldc<-marveldc %>% select(Name,Gender, Race, Publisher)
#
marveldcinfoall<-join(marveldc,characterstat,by="Name",type="inner")
View(marveldcinfoall)
colnames(powers)[colnames(powers)=="hero_names"]<-"Name"
fullinfo<-join(marveldcinfoall,powers,by="Name",type="inner")
#main data
View(fullinfo)
#TRANSFORM INTO A SINGLE COLUMN SUPER POWER
marveldcc<-melt(fullinfo,id=c("Name", "Gender", "Race", "Publisher", "Alignment", "Intelligence", 
                              "Strength", "Speed", "Durability", "Power", "Combat", "Total"))
marveldcc
colnames(marveldcc)[colnames(marveldcc)=="variable"]<-"Superpower"
marveldcc<-marveldcc%>%filter(value=="TRUE")%>%select(-value)
summary(marveldcc)
View(marveldcc)
#CONVERT CATEGORICAL COLUMNS TO FACTORS
marveldcc$Name <- as.factor(marveldcc$Name)
marveldcc$Gender <- as.factor(marveldcc$Gender)
marveldcc$Race <- as.factor(marveldcc$Race)
marveldcc$Publisher <- as.factor(marveldcc$Publisher)
marveldcc$Alignment <- as.factor(marveldcc$Alignment)
marveldcc$Superpower <- as.factor(marveldcc$Superpower)
#
dcMarvelPalette <- c("#0476F2", "#EC1E24")
goodBadPalette <- c("#E58700", "#0EBF7D", "#C99902")
#
ggplot(marveldcc,aes(Publisher,fill=Publisher))+geom_bar(stat="count",aes(fill=Publisher))+scale_fill_manual(values = dcMarvelPalette)
#MALES AND FEMALES IN DC AND MARVEL
marvelgender<-marveldcc%>%filter(!is.na(Gender))%>%group_by(Gender)%>%count(Publisher)%>%select(Gender,Publisher,count=n)
marvelgender
ggplot(marvelgender,aes(x=Gender,y=count))+geom_bar(stat="identity",aes(fill=Publisher))+facet_wrap(~Publisher)+scale_fill_manual(values = dcMarvelPalette)
#RACE DIFFERENCE BETWEEN MARVEL AND DC
DCrace<-marveldcc%>%filter(!is.na(Race))%>%filter(Publisher=="DC Comics") %>%group_by(Race)%>%count(Race)%>%select(Race,Count=n) %>%arrange(-Count)     
DCrace
DCrace<-ggplot(DCrace,aes(x=reorder(Race,Count),y=Count))+geom_bar(stat="identity",aes(fill=Race))+coord_flip()
#2nd part
marvelRace <- marveldcc %>%
  filter(!is.na(Race)) %>%
  filter(Publisher == "Marvel Comics") %>%
  group_by(Race) %>%
  count(Race) %>%
  select(Race, Count = n) %>%
  arrange(-Count)

marvelRace<-ggplot(marvelRace, aes(x = reorder(Race, Count), y = Count)) + 
  geom_bar(stat = "identity", aes(fill = Race)) +
  geom_label(stat = "identity", aes(label = Count)) +
  labs(x = "Race", y = "No. of Characters", title = "Marvel Character Races", subtitle= "Top 15 races") 

grid.arrange(DCrace,marvelRace,ncol=1)
# HEROES AND VILLIANS IN MARVEL AND DC
marveldc_allignment<-marveldcc%>%filter(!is.na(Alignment))%>%group_by(Alignment)%>%count(Publisher)%>%select(Alignment,Publisher,Count=n)                  

ggplot(marveldc_allignment,aes(x=Alignment,y=Count))+geom_bar(stat="identity",aes(fill=Publisher))+geom_label(stat="identity",aes(label=Count))+facet_wrap(~Publisher)+scale_fill_manual(values = dcMarvelPalette) 
#Intelligence
boxIntel<-ggplot(marveldcc,aes(x=Publisher,y=Intelligence,fill=Publisher))+geom_boxplot()

# STRENGTH
#a<-marveldcc%>%group_by(Publisher)%>%count(Strength) %>%select(Publisher,Strength,Count=n)
boxStrength <- ggplot(marveldcc, aes(x = Publisher, y = Strength, fill = Publisher)) + 
  geom_boxplot() +
  labs(x = "", title = "DC and Marvel Characters", subtitle = "Comparison of Strength") +
  
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

# SPEED
boxSpeed <- ggplot(marveldcc, aes(x = Publisher, y = Speed, fill = Publisher)) + 
  geom_boxplot() +
  labs(x = "", title = "DC and Marvel Characters", subtitle = "Comparison of Speed") +
  scale_fill_manual(values=dcMarvelPalette)

# DURABILITY
boxDurability <- ggplot(marveldcc, aes(x = Publisher, y = Durability, fill = Publisher)) + 
  geom_boxplot() +
  labs(x = "", title = "DC and Marvel Characters", subtitle = "Comparison of Durability") +
  scale_fill_manual(values=dcMarvelPalette)

# POWER
boxPower <- ggplot(marveldcc, aes(x = Publisher, y = Power, fill = Publisher)) + 
  geom_boxplot() +
  labs(x = "", title = "DC and Marvel Characters", subtitle = "Comparison of Power") +
  scale_fill_manual(values=dcMarvelPalette)

# COMBAT
boxCombat <- ggplot(marveldcc, aes(x = Publisher, y = Combat, fill = Publisher)) + 
  geom_boxplot() +
  labs(x = "", title = "DC and Marvel Characters", subtitle = "Comparison of Combat") +
  scale_fill_manual(values=dcMarvelPalette)

grid.arrange(boxIntel, boxStrength, boxSpeed, boxDurability, boxPower, boxCombat,  ncol = 2)
#
ggplot(marveldcc, aes(x = Publisher, y = Total, fill = Publisher)) + 
  geom_boxplot() +
  labs(x = "Publisher", title = "DC and Marvel Characters", subtitle =  "Most Powerful Characters Comparison (Sum of abilities)") + 
  scale_fill_manual(values=dcMarvelPalette)
#TOP POWERFUL CHARACTERS MARVEL AND DC
DCtotal<-marveldcc%>%filter(Publisher=="DC Comics")%>%group_by(Name,Alignment)%>%distinct(Total) %>%select(Name,Total)%>%arrange(-Total)
dcTotal <- ggplot(DCtotal[1:20,], aes(x = reorder(Name, Total), y = Total)) + 
  geom_bar(stat = "identity", aes(fill =Alignment)) +
  labs(x = "Character", y = "Total", title = "Top 20 DC Characters", subtitle = "Most Powerful Heroes or Villains") + 
  scale_fill_manual(values=goodBadPalette, labels = c("villain", "hero", "neutral"))
#2nd part
marvelTotal <- marveldcc %>%
  filter(Publisher == "Marvel Comics") %>%
  group_by(Name, Alignment) %>%
  distinct(Total) %>%
  select(Name, Total) %>%
  arrange(-Total)

marvelTotal <- ggplot(marvelTotal[1:20, ], aes(x = reorder(Name, Total), y = Total)) + 
  geom_bar(stat = "identity", aes(fill = Alignment)) +
  labs(x = "Character", y = "Total", title = "Top 20 Marvel Characters", subtitle = "Most Powerful Heroes or Villains")  +
  scale_fill_manual(values=goodBadPalette, labels = c("villain", "hero", "neutral"))
grid.arrange(marvelTotal,dcTotal,ncol=2)
#TOP SUPERPOWERS MARVEL AND DC

dcSuperP <- marveldcc %>%
  filter(Publisher == "DC Comics") %>%
  group_by(Superpower) %>%
  dplyr::count(Superpower) %>%
  select(Superpower, Count = n) %>%
  arrange(-Count)
#

dcSuperP <- ggplot(dcSuperP[1:20, ], aes(x = reorder(Superpower, Count), y = Count)) + 
  geom_bar(stat = "identity", aes(fill = Superpower)) +
  geom_label(stat = "identity", aes(label = Count)) +
  labs(x = "Superpower", y = "No. of Characters", title = "DC Comics", subtitle = "Top 20 Superpowers") 
#2nd part
marvelSuperP <- marveldcc %>%
  filter(Publisher == "Marvel Comics") %>%
  group_by(Superpower) %>%
  dplyr::count(Superpower) %>%
  select(Superpower, Count = n) %>%
  arrange(-Count)

marvelSuperP <- ggplot(marvelSuperP[1:20, ], aes(x = reorder(Superpower, Count), y = Count)) + 
  geom_bar(stat = "identity", aes(fill = Superpower)) +
  geom_label(stat = "identity", aes(label = Count)) +
  labs(x = "Superpower", y = "No. of Characters", title = "Marvel Comics", subtitle = "Top 20 Superpowers") 
#TOP CHARACTERS HIGHEST No. OF SUPERPOWERS
marvelDcSuperP <- marveldcc %>%
  group_by(Name, Publisher) %>%
  dplyr::count(Name) %>%
  select(Name, Count = n) %>%
  arrange(-Count)
ggplot(marvelDcSuperP[1:25, ], aes(x = reorder(Name, Count), y = Count)) + 
  geom_bar(stat = "identity", aes(fill = Publisher)) +
  labs(x = "Character", y = "No. of superpowers", title = "Top 25 Marvel and DC Characters", subtitle = "With Highest No. of Superpowers") +
  scale_fill_manual(values=dcMarvelPalette)