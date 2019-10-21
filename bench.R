#Peter's Research Benchmarking DATA manipulation:
install.packages(c("tidyverse", "cowplot"))
library(tidyverse)##Load packages after installing them:
library(cowplot)
bench <- read.csv("~/00DeakinUni/R/BCL_R/BCL/bench2.csv")##Load Peter's Research Indices DATA:

#Compute Cumulative Sums of Peter's FundsAs_CoLeader:=======
a <- bench %>% 
  group_by(Year) %>%
  transform(CumSumFund=cumsum(N),
            CumSumPubs=cumsum(Publications.per.year),
            CumSumCite=cumsum(Citations.per.year)) %>%
  select(Year, CumSumPubs, CumSumCite, CumSumFund) %>%
  mutate(lead = "N")%>%
  as.data.frame

#Compute Cumulative Sums of Peter's FundsAs_ChiefLeader
b <- bench %>% 
  group_by(Year) %>%
  transform(CumSumFund=cumsum(Y),
            CumSumPubs=cumsum(Publications.per.year),
            CumSumCite=cumsum(Citations.per.year)) %>%
  select(Year, CumSumPubs, CumSumCite, CumSumFund) %>%
  mutate(lead = "Y")%>%
  as.data.frame

ab <- rbind (a,b)#Bind them
max(ab$CumSumCite)#2617
max(ab$CumSumPubs)#120

#PLOT========
MyBreaks <- c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

p <- ggplot() +
  geom_bar(width=0.5, data = ab, aes(x=Year, y=CumSumFund/1000000, fill = lead), position="stack",stat="identity")+
  scale_fill_manual("CI Leadership:", values = c("Y" ="black" , "N" = "grey"), labels = c("Chief (Lead) Investigator", "Co- Investigator"))+
  ylab("Research Income  \n (Millions AU$, cumulative)") +xlab("")+
  geom_line(data = ab, aes(x=Year, y=CumSumPubs/10), stat="identity")+
  geom_point(data = ab, aes(x=Year, y=CumSumPubs/10), stat="identity") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "No. of Research Publications \n (cumulative)"))+
  theme_minimal()+
  scale_x_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(2004,2019.4))+
  theme_classic() +
  theme(legend.position   = c(0.75,0.9),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.text.x=element_text(size=14, face = "bold", angle = 45, vjust = 0.5),
        axis.text.y=element_text(size=14, face = "bold"),
        axis.title.y=element_text(size=16))
p


#Create Inset plot made of citations:
inset.plot <- ggplot() + 
  geom_line(data = ab, aes(x=Year, y=CumSumCite),linetype =  "dotted", stat="identity")+
  geom_point(data = b, aes(x=Year, y=CumSumCite), stat="identity",size = 2) +
  ylab("No. of Citations")+
  scale_x_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(2004,2019.4))+
  theme_classic()+
    theme(legend.position = "none",
        axis.text.x=element_text(size=14, face = "bold", angle = 45,vjust = 0.5),
        axis.text.y=element_text(size=14, face = "bold"),
        axis.title.y=element_text(size=16) ,
        axis.title.x=element_blank())

inset.plot

#Merge instet.plot and p (main plot) together:
plot.with.inset <-
  ggdraw() +
  draw_plot(p) +
  draw_plot(inset.plot, x = 0.1, y = .5, width = .5, height = .5)

#Save the plot with ggsave() and check the output:
ggsave(filename = "plot.with.inset.png", 
       plot = plot.with.inset,
       width = 32, 
       height = 12,
       units = "cm",
       dpi = 600)



