#Install 2 R-packages we need:
if (!require(readxl)) library(readxl) # load that package if not done already
if (!require(tidyverse)) library(tidyverse) # load that package if not done already

#Load these packages into your working environment
library("readxl")
library("tidyverse")

#IMPORTANT:
#YOUR SAMPLE_ID (I named it CNcode) should be consistent across all your spreadsheets.

#Specify path to folder where you store youre CN spreadsheets and list them all:
files <- list.files(path = "C:/Users/BlueCarbon/Documents/0.Deakin Uni/R/BCL_R/CN_DATA_12jun19",
                    pattern = "*.xls", full.names = T)

#Create function to export data from sheet = 1 (Sample Table)
ReadCN_smp <- function(x) read_xlsx (path = x,
                                     sheet = 1,
                                     skip = 7,
                                     range = cell_cols("C:E"))

#Export "Sample Table" data from all files in your folder:
tbl1 <- sapply(files, 
             ReadCN_smp, simplify=FALSE) %>% 
             bind_rows(.id = "id1")


#Create function to export data from sheet = 2 (Element%)
ReadCN <- function(x) read_xlsx (path = x,sheet = 2,skip = 7, range = cell_cols("C:D"))

#Export "Element%" data from all files in your folder using sapply:
tbl2 <- sapply(files, 
               ReadCN, simplify=FALSE) %>% 
  bind_rows(.id = "id2")

#Bind sample (tbl1) and CN (tbl2) data together
CN_DATA <- cbind(tbl1,tbl2)#bind smp and CN data

#Double check if data alligns well:
all.equal(tbl1$id1,tbl2$id2) #should be TRUE!

#Clean up the file to keep data you need in a form you/R likes (no special signs):
CN_DATA_clean <- CN_DATA %>%
                 filter(Type == "Smp") %>%
  select("id1", "Sample Name","Weight", "(N) %", "(C) %" ) %>%
  rename(file = "id1", CNCode = "Sample Name", Weight.mg = "Weight",
         N.percent = "(N) %", C.percent = "(C) %")

#Check for duplicates:
anyDuplicated(CN_DATA_clean$CNCode)#Should be 0!!!
#If not 0, some samples are dupliacted,
#You have to decide what to do with duplicates (e.g., average them, remove them)

#Merge new CN data with your MASTER file:
MASTER_DATA <- read_csv("CN.csv")
NewDATA <- left_join(MASTER_DATA, CN_DATA_clean, by = "CNCode")

dim(MASTER_DATA)#2233 rows   36 cols
dim(NewDATA)#2233 rows  40 cols

#Plot======
NewDATA <- NewDATA [ !is.na(NewDATA$site),]
NewDATA$habitat <- factor(NewDATA$habitat,
                   levels = c("Saltmarsh","Mangrove","Seagrass"))
NewDATA <-filter(NewDATA, HCl_Bubbles =="no")
names(NewDATA)
ggplot(NewDATA, aes(x = CPb, y = C.percent,color=habitat)) +
  geom_boxplot() +
  facet_grid(.~habitat)+ geom_jitter(alpha = 0.4)+
  ylab("Organic Carbon (%)") + xlab("") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
                strip.background =  element_rect(fill = "white"))

ggplot(NewDATA, aes(x = habitat, y = C.percent,color=habitat)) +
  geom_boxplot() +
  facet_grid(.~site)+
  ylab("Organic Carbon (%)") + xlab("") +
  theme_bw() + geom_jitter(alpha = 0.4)+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        strip.background =  element_rect(fill = "white"))
