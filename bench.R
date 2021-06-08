#Peter's Research Benchmarking DATA manipulation SECOND GO:====
library(tidyverse)##Load packages after installing them:
library(cowplot)
bench <- read.csv("~/00DeakinUni/R/BCL_R/BCL/bench4.csv")##Load Peter's Research Indices DATA:
str(bench)#Y for funds gained as YES-Chief Principal Investigator, N = NO (as Co-, Total AUD $$$)

#Compute Cumulative Sums of Peter's FundsAs_CoLeader (N):=======
a <- bench %>% 
  group_by(Year) %>%
  transform(CumSumFund=cumsum(N),#Cumulative Sums of Peter's FundsAs_CoLeader
            CumSumPubs=cumsum(Publications.per.year),
            CumSumCite=cumsum(Citations.per.year)) %>%
  select(Year, CumSumPubs, CumSumCite, CumSumFund) %>%
  mutate(lead = "N")%>%
  as.data.frame
a
#Compute Cumulative Sums of Peter's FundsAs_ChiefLeader (Y)
b <- bench %>% 
  group_by(Year) %>%
  transform(CumSumFund=cumsum(Y), #Cumulative Sums of Peter's FundsAs_ChiefLeader (Y for YES-Chief Principla Investigator)
            CumSumPubs=cumsum(Publications.per.year),
            CumSumCite=cumsum(Citations.per.year)) %>%
  select(Year, CumSumPubs, CumSumCite, CumSumFund) %>%
  mutate(lead = "Y")%>%
  as.data.frame

ab <- rbind (a,b)#Bind them
ab1 <- filter(ab, Year > 2006)
ab1$Pubs_Data <- "solid"
ab1$Cite_Data <- "dotted"
ab1
max(ab1$CumSumCite)#4247
max(ab1$CumSumPubs)#166

#PLOT========
#No. of pubs and citations:
MyBreaks <- c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)

 main_plot <-  ggplot() +
    geom_line(data = ab1, aes(x=Year, y=CumSumCite, lty = Cite_Data), stat="identity")+
    geom_point(data = ab1, aes(x=Year, y=CumSumCite), stat="identity",size = 2) +
    ylab("\n No. of Citations (cumulative)") +xlab("") +
  geom_line(data = ab1, aes(x=Year, y=CumSumPubs*15, lty = Pubs_Data), stat="identity")+
  geom_point(data = ab1, aes(x=Year, y=CumSumPubs*15), stat="identity") +
  scale_y_continuous(sec.axis = sec_axis(~./15, name = "No. of  Publications (cumulative)"))+
  scale_linetype_manual( "", values=c( "dotted","solid"),  labels=c("Citations","Publications"))+
   theme_minimal()+
  scale_x_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(2007,2021.4)) +
  theme_classic() +
  theme(legend.position   = c(0.9,0.15),
        #legend.title = element_blank(),
        legend.text = element_text(size=14, colour = "black"),
        axis.text.x=element_text(size=14,  angle = 45, vjust = 0.5, colour = "black"),
        axis.text.y=element_text(size=14,  colour = "black"),
        axis.title.y=element_text(size=14, colour = "black"))
main_plot


#Create Inset plot made of cumulative funds:
inset.plot2 <- ggplot() + 
  geom_bar(width=0.5, data = ab1, aes(x=Year, y=CumSumFund/1000000, fill = lead), position="stack" ,stat="identity")+
  scale_fill_manual("", values = c("Y" ="black" , "N" = "grey"), labels = c("Chief (Lead) Investigator", "Co-Investigator"))+
  ylab("Research Income  \n (Millions AU$, cumulative)") +xlab("")+
  scale_x_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(2006.5,2021.4))+
  theme_classic()+
  theme(legend.position = c(0.25,0.9),
        legend.title = element_text(size=14, colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        axis.text.x=element_text(size=14,  angle = 45,vjust = 0.5, colour = "black"),
        axis.text.y=element_text(size=14,  colour = "black"),
        axis.title.y=element_text(size=14, colour = "black") ,
        axis.title.x=element_blank())

inset.plot2

#Merge instet.plot and p (main plot) together:
plot.with.inset2 <-
  ggdraw() +
  draw_plot(main_plot) +
  draw_plot(inset.plot2, x = 0.1, y = 0.4, width = .5, height = .6)

#Save the plot with ggsave() and check the output:
ggsave(filename = "Peter_Macreadie_PubsBenchmark_plot2021.jpeg", 
       plot = plot.with.inset2,
       width = 32, 
       height = 12,
       units = "cm",
       dpi = 1600)
