library(tidyverse)
library(stringr)
library(webr)

# Run the following to guess the encoding of the file if needed
#guess_encoding("qualitative result.csv")

# Run the following code to get the raw qualitative data
data = read.csv("qualitative result.csv", sep=";", dec=";", header=TRUE,
                fileEncoding = "windows-1252") %>% 
  mutate(ID_job=row_number(),
       Title = tolower(Title),
       Description = tolower(Description)) %>% 
  select(ID_job,Profession,Title,Title.value, Description, Description.value, X4.groups)
view(data)



# For Qualitative result

# For Qualitative result column
data_R2 <- data %>% 
  mutate(Qualitative.result = rowMeans(data[c('Title.value',
                                              'Description.value')]))

# For "bias" column
data_R2 <- data_R2 %>%
  mutate(quali.bias = case_when(Qualitative.result == 1 ~ "biaisé F",
                                Qualitative.result == 0.5 ~ "M biaisé F",
                                Qualitative.result == -1 ~ "biaisé H",
                                Qualitative.result == -0.5 ~ "M biaisé H",
                                          T ~ "paritaire"
                                ))

#For quantitative result

#create a list of epicene profession
Epicen.profession = c("secrétaire", "maire", "juge", "architecte", "artiste", "linguiste", "spécialiste", "cariste",
                      "journaliste", "responsable", "comptable", "gendarme", "diplomate", "maire", "ministre", 
                      "peintre", "poète")

check_epicene_metier <- function(metier){
  
  titre_split <- (str_split(metier, "[:space:]|[:punct:]"))[[1]]
  
  vector_metier <- NULL
  
  for(element in titre_split){
    for(epicen in Epicen.profession){
      if(element == epicen){
        vector_metier <- c(vector_metier,TRUE)
      }else{
        vector_metier <- c(vector_metier,FALSE)
      }
    }
  }
  
  TRUE %in% vector_metier
  
}


#count occurences in "Title" column

#for truncated forms
data_R2 <- data_R2 %>%
  group_by(ID_job) %>% 
  mutate(ve.count.tit = str_count(Title, "[:space:]?(?<=\\(|\\.)ve\\b(?=\\)|[:space:]|\\.|,|$)"),
                   ive.count.tit = str_count(Title, "[:space:]?(?<=\\(|\\.)ive\\b(?=\\)|[:space:]|\\.|,|$)"),
                   e.count.tit = str_count(Title, "[:space:]?(?<=\\(|\\.)e\\b(?=\\)|[:space:]|\\.|,|$)"),
                   trice.count.tit = str_count(Title, "[:space:]?(?<=\\(|\\.|\\·|\\-|\\/)t?rice\\b(?=\\)|[:space:]|\\.|,|$)"),
                   ère.count.tit = str_count(Title, "[:space:]?(?<=\\(|\\.)ère\\b(?=\\)|[:space:]|\\.|,|$)"),
                   euse.count.tit = str_count(Title, "[:space:]?(?<=\\(|\\.)euse\\b(?=\\)|[:space:]|\\.|,|$)"),
                   ne.count.tit = str_count(Title, "[:space:]?(?<=\\(|\\.)ne\\b(?=\\)|[:space:]|\\.|,|$)"),
                   sum_troncat_title = ve.count.tit + ive.count.tit + e.count.tit + trice.count.tit + 
                                       ère.count.tit + euse.count.tit + ne.count.tit) %>%
  ungroup()

#for H/F
data_R2 <- data_R2 %>%
  group_by(ID_job) %>% 
  mutate( hf.count.tit = str_count(Title, "h/f"),
          sum_hf_title = hf.count.tit) %>% 
  ungroup()

#for doublet
data_R2 <- data_R2 %>%
  group_by(ID_job) %>% 
  mutate( if.ive.tit = str_detect(Title,'(?<!\\b)ive\\b') == TRUE & str_detect(Title,'(?<!\\b)if\\b') == TRUE,
          ien.ienne.tit = str_detect(Title,'(?<!\\b)ienne\\b') == TRUE & str_detect(Title,'(?<!\\b)ien\\b') == TRUE,
          teur.trice.tit = str_detect(Title,'(?<!\\b)trice\\b') == TRUE & str_detect(Title,'(?<!\\b)teur\\b') == TRUE,
          ier.ère.tit = str_detect(Title,'(?<!\\b)ère\\b') == TRUE & str_detect(Title,'(?<!\\b)i?er\\b') == TRUE,
          eur.euse.tit = str_detect(Title,'(?<!\\b)euse\\b') == TRUE & str_detect(Title,'(?<!\\b)eur\\b') == TRUE,
          al.ale.tit = str_detect(Title,'(?<!\\b)ale\\b') == TRUE & str_detect(Title,'(?<!\\b)al\\b') == TRUE,
          é.ée.tit = str_detect(Title,'(?<!\\b)ée\\b') == TRUE & str_detect(Title,'(?<!\\b)é\\b') == TRUE,
          sum_doublet_title = ifelse(TRUE %in% c(if.ive.tit, teur.trice.tit, ier.ère.tit, eur.euse.tit, 
                                                 al.ale.tit, ien.ienne.tit, é.ée.tit),1,0)) %>% 
  ungroup()

#for masculine adjective
data_R2 <- data_R2 %>%
  group_by(ID_job) %>%
  mutate( if.tit = str_detect(Title,'(?<!\\b)ive\\b') == FALSE & str_detect(Title,'(?<!\\b)if\\b') == TRUE,
          ien.tit = str_detect(Title,'(?<!\\b)ienne\\b') == FALSE & str_detect(Title,'(?<!\\b)ien\\b') == TRUE,
          teur.tit = str_detect(Title,'(?<!\\b)trice\\b') == FALSE & str_detect(Title,'(?<!\\b)teur\\b') == TRUE,
          ier.tit = str_detect(Title,'(?<!\\b)ère\\b') == FALSE & str_detect(Title,'(?<!\\b)i?er\\b') == TRUE,
          eur.tit = str_detect(Title,'(?<!\\b)euse\\b') == FALSE & str_detect(Title,'(?<!\\b)eur\\b') == TRUE,
          al.tit = str_detect(Title,'(?<!\\b)ale\\b') == FALSE & str_detect(Title,'(?<!\\b)al\\b') == TRUE,
          é.tit = str_detect(Title,'(?<!\\b)ée\\b') == FALSE & str_detect(Title,'(?<!\\b)é\\b') == TRUE,
          pres_masc = TRUE %in% c(if.tit, teur.tit ,ier.tit ,eur.tit ,al.tit ,ien.tit ,é.tit)) %>% 
  ungroup()

#for feminine adjective
data_R2 <- data_R2 %>%
  group_by(ID_job) %>%
  mutate( ive.tit = str_detect(Title,'(?<!\\b)ive\\b') == TRUE & str_detect(Title,'(?<!\\b)if\\b') == FALSE,
          ienne.tit = str_detect(Title,'(?<!\\b)ienne\\b') == TRUE & str_detect(Title,'(?<!\\b)ien\\b') == FALSE,
          trice.tit = str_detect(Title,'(?<!\\b)trice\\b') == TRUE & str_detect(Title,'(?<!\\b)teur\\b') == FALSE,
          ère.tit = str_detect(Title,'(?<!\\b)ère\\b') == TRUE & str_detect(Title,'(?<!\\b)i?er\\b') == FALSE,
          euse.tit = str_detect(Title,'(?<!\\b)euse\\b') == TRUE & str_detect(Title,'(?<!\\b)eur\\b') == FALSE,
          ale.tit = str_detect(Title,'(?<!\\b)ale\\b') == TRUE & str_detect(Title,'(?<!\\b)al\\b') == FALSE,
          ée.tit = str_detect(Title,'(?<!\\b)ée\\b') == TRUE & str_detect(Title,'(?<!\\b)é\\b') == FALSE,
          pres_fem = TRUE %in% c(ive.tit, trice.tit ,ère.tit ,euse.tit ,ale.tit ,ienne.tit ,ée.tit)) %>% 
  ungroup()

#for epicene title profession
data_R2 <- data_R2 %>%
  group_by(ID_job) %>%
  mutate(sum_epicene_title = ifelse(check_epicene_metier(Title) == TRUE,1,0)) %>% 
  ungroup()





#count occurences in "Description" column

#for truncated forms
data_R2 <- data_R2 %>%
  group_by(ID_job) %>%
  mutate(ve.count.desc = str_count(Description, "[:space:]?(?<=\\(|\\.)ve\\b(?=\\)|[:space:]|\\.|,|$)"),
         ive.count.desc  = str_count(Description , "[:space:]?(?<=\\(|\\.)ive\\b(?=\\)|[:space:]|\\.|,|$)"),
         e.count.desc  = str_count(Description , "[:space:]?(?<=\\(|\\.)e\\b(?=\\)|[:space:]|\\.|,|$)"),
         trice.count.desc  = str_count(Description , "[:space:]?(?<=\\(|\\.)trice\\b(?=\\)|[:space:]|\\.|,|$)"),
         ère.count.desc = str_count(Description , "[:space:]?(?<=\\(|\\.)ère\\b(?=\\)|[:space:]|\\.|,|$)"),
         euse.count.desc  = str_count(Description , "[:space:]?(?<=\\(|\\.)euse\\b(?=\\)|[:space:]|\\.|,|$)"),
         ne.count.desc  = str_count(Description , "[:space:]?(?<=\\(|\\.)ne\\b(?=\\)|[:space:]|\\.|,|$)"),
         sum_troncat_description = ve.count.desc  + ive.count.desc + e.count.desc + trice.count.desc  +
           ère.count.desc  + euse.count.desc  + ne.count.desc) %>%
  ungroup()

#for H/F
data_R2 <- data_R2 %>%
  group_by(ID_job) %>%
  mutate(hf.count.desc  = str_count(Description, "h/f"),
         sum_hf_description = hf.count.desc) %>% 
  ungroup()

#for doublet
data_R2 <- data_R2 %>%
  group_by(ID_job) %>%
  mutate(eur.euse.desc = str_detect(Description,
                                    "((?<!\\b)eur(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+euse\\b)|
                                    (((?<!\\b)euse(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+eurs?\\b))"),
         if.ive.desc = str_detect(Description,
                                  "((?<!\\b)if(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+ive\\b)|
                                  (((?<!\\b)ive(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+if\\b))"),
         teur.trice.desc = str_detect(Description,
                                      "((?<!\\b)teur(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+trice\\b)|
                                      (((?<!\\b)trice(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+teur\\b))"),
         teur.trice.desc = str_detect(Description,
                                      "((?<!\\b)teur(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+trice\\b)|
                                      (((?<!\\b)trice(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+teur\\b))"),
         ien.ienne.desc = str_detect(Description,
                                     "((?<!\\b)ien(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+ienne\\b)|
                                     (((?<!\\b)ienne(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+ien\\b))"),
         al.ale.desc = str_detect(Description,
                                  "((?<!\\b)al(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+ale\\b)|
                                  (((?<!\\b)ale(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+al\\b))"),
         nt.nte.desc = str_detect(Description,
                                  "((?<!\\b)nt(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+nte\\b)|
                                  (((?<!\\b)nte(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))\\w+nt\\b))"),
         le.la.desc = str_detect(Description,
                                 "\\ble(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))la\\b|
                                 \\bla(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))le\\b"),
         un.une.desc = str_detect(Description,
                                  "\\bun(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))une\\b|
                                  bune(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))un\\b"),
         il.elle.desc = str_detect(Description,
                                   "\\bil(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))elle\\b|
                                   \\belle(([:blank:](\\w+[:blank:])?ou([:blank:]\\w+)?[:blank:])|([:blank:]?[:punct:][:blank:]?))il\\b"),

         sum_doublet_description = ifelse(TRUE %in% c(if.ive.desc, teur.trice.desc, eur.euse.desc ,al.ale.desc ,
                                                      ien.ienne.desc, nt.nte.desc, le.la.desc, un.une.desc, il.elle.desc),1,0)) %>% 
  ungroup()


#for 4 groups
data_R2 <- data_R2 %>%
  group_by(ID_job) %>%
  mutate(X4.groups.description = str_count(Description, "(programme|égalité).?d.?accès.?à.?l.?(égalité|emploi)")) %>% 
  ungroup()


#TEST 
# we need to do random checking to see if the functions capture what we want
str_detect(c("agent(e)",
             "agent.e ",
             "administratif(ve)",
             "conduc(trice)",
             "conduc.trice ",
             "administratif.ve ",
             "administrat.ive",
             "journali(ère)",
             "livreur(euse)",
             "mécanicien(ne)"),
           c("[\\(\\.]ve[\\) \\.\\,]",
           "[\\(\\.]ive[\\) \\.\\,]"))


# or look at the text of the 1st row and extract the count that we generated
data_R2 %>%
  # take a specific row number
  slice(8) %>%
  # show the description
  pull(Description)

# extract the counts of items and compare with the previous text
data_R2 %>%
  # take a specific row number
  slice(8) %>%
  # take relevant columns
  select(e.count.tit)

#test also 4 groups
data_R2 %>% 
  filter(X4.groups == X4.groups.description) %>% 
  select(Profession, X4.groups, X4.groups.description)

taux_exact <- nrow(data_R2 %>% 
                     filter((X4.groups == 0 & X4.groups.description != 0) |
                              (X4.groups == -1 & X4.groups.description == 0)))/nrow(data_R)*100
taux_exact

######################################################################################

#Analysis

#for Quantitative title 
data_R2 <- data_R2 %>% 
  mutate(Quanti.title = ifelse(sum_epicene_title > 0 & sum_hf_title + sum_doublet_title + sum_troncat_title > 0,
                        0,
                        ifelse(sum_epicene_title > 0 & pres_masc == TRUE,
                               -1,
                               ifelse(sum_epicene_title > 0 & pres_fem == TRUE,
                                      1,
                                      ifelse(sum_epicene_title == 0  & sum_troncat_title > 0,
                                             0,
                                             ifelse(sum_epicene_title == 0 & sum_hf_title > 0,
                                                    0,
                                                    ifelse(sum_epicene_title == 0 & sum_doublet_title > 0,
                                                           0,
                                                           ifelse(sum_epicene_title == 0 & pres_masc == TRUE,
                                                                  -1,
                                                                  ifelse(sum_epicene_title == 0 & pres_fem == TRUE,
                                                                         1,
                                                                         0)))))))
                        
  ))

#test 
data_R2 %>% 
select(Profession,Title, Title.value,Quanti.title) %>% 
  filter(Title.value != Quanti.title) %>% 
  select(Profession) %>% table()

taux_exact <- nrow(data_R2 %>% 
                     filter((Title.value == Quanti.title)))/nrow(data_R)*100
taux_exact

#for Quantitative description
data_R2 <- data_R2 %>% 
  mutate(Quanti.description = ifelse(sum_doublet_description > 0,
                                     0,
                                     ifelse(sum_troncat_description + sum_hf_description > 0,
                                            0,
                                            Quanti.title)
                                     
  ))


#test
data_R2 %>% 
  select(Profession, Description.value, Quanti.description) %>% 
  filter(Description.value != Quanti.description) %>% 
  select(Profession) %>% table()


taux_exact <- nrow(data_R2 %>% 
                     filter((Description.value == Quanti.description )))/nrow(data_R)*100
taux_exact

#for classes
data_R2 <- data_R2 %>%
  mutate(class_desc = str_extract(Description, "class(e)?[:blank:][:digit:]+"),
         class_title = str_extract(Title, "class(e)?[:blank:][:digit:]+"))
         

data_R2 <- data_R2 %>% 
  mutate(classe = case_when(stringr::str_extract(class_desc,"[:digit:]+") == stringr::str_extract(class_title,"[:digit:]+") ~ stringr::str_extract(class_desc,"[:digit:]+"),
                            stringr::str_extract(class_desc,"[:digit:]+") != stringr::str_extract(class_title,"[:digit:]+") ~ "ERROR",
                            is.na(class_title) ~ stringr::str_extract(class_desc,"[:digit:]+"),
                            is.na(class_desc) ~ stringr::str_extract(class_title,"[:digit:]+")
                            ))
  
data_R3 <- data_R2 %>% 
  filter(!is.na(classe) & classe != "ERROR") %>% 
  group_by(classe) %>% 
  mutate(nRow = n(),
         val_bias = 1,
         Qualitative.result = as.character(Qualitative.result)) %>% 
  group_by(classe,Qualitative.result,nRow,Profession) %>% 
  summarise(val_norm = sum(val_bias)/nRow) 


#for bias indicator
data_R2 <- data_R2 %>%
  mutate(Bias.indicator = rowMeans(data_R2[c('Quanti.description','Quanti.title')]),
         Bias.indicator = round(Bias.indicator, digits = 4))

#test
data_R2 %>% 
  select(Profession, Bias.indicator, Qualitative.result) %>% 
  filter(Bias.indicator != Qualitative.result) %>% 
  select(Profession) %>% table()


taux_exact <- nrow(data_R2 %>% 
                     filter((Bias.indicator == Qualitative.result)))/nrow(data_R)*100
taux_exact

#####################################################################################################################
#stats

#plot classes

data_R3 %>%
  dplyr::mutate(Qualitative.result = case_when(Qualitative.result == "-1" | Qualitative.result == "-0.5" ~ "Biaisé H",
                                               Qualitative.result == "1" | Qualitative.result == "0.5" ~ "Biaisé F",
                                               Qualitative.result == "0" ~ "Paritaire")) %>% 
  ggplot(aes(x = classe, y = val_norm, group=Qualitative.result,fill=Qualitative.result)) +
  geom_bar(stat="identity",position="dodge",width = .5) +
  facet_grid(~Profession)


# plot the quali result with Pie Donut
data_R2 %>% 
  select(Profession, quali.bias, Qualitative.result) %>% table()
data_R2 %>%
  webr::PieDonut(aes(Profession, quali.bias))


# plot the quanti result 

#plot the epicene writing rules use
data_R2 <- data_R2 %>%
  mutate(trancat = case_when(sum_troncat_description + sum_troncat_title > 0 ~ 1, T ~ 0),
         doublet = case_when(sum_doublet_description + sum_doublet_title > 0 ~ 1 ,T ~0),
         h.f = case_when(sum_hf_description + sum_hf_title > 0 ~ 1,T ~0))

data_R2 %>%
  select(Profession, trancat, doublet, h.f) %>% 
  gather(key='variable',value='valeur', trancat, doublet, h.f) %>% 
  ggplot(aes(x = Profession,  fill = Profession)) +
  geom_bar(position = "dodge") +
  ggplot2::facet_grid(valeur~variable) +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)

# Quanti.description
data_R2 %>%
  mutate(Quanti.description = factor(Quanti.description)) %>%
  ggplot(aes(x = Profession, fill = Quanti.description)) +
  geom_bar(position = "dodge")+
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)


#quanti.title
data_R2 %>%
  mutate(Quanti.title = factor(Quanti.title)) %>%
  ggplot(aes(x = Profession, fill = Quanti.title)) +
  geom_bar(position = "dodge")+
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)


#boxplot bias indicator with qualitative result 
data_R2 %>%
  mutate(Bias.indicator = factor(Bias.indicator)) %>%
  ggplot(aes(x = Profession, y = Qualitative.result,
             fill = Bias.indicator)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        axis.title.x=element_blank(), 
        strip.text.x = element_text(size=12,face="bold"), 
        legend.text=element_text(size=12)) +
  scale_fill_viridis_d() +
  geom_point(position = position_jitterdodge(jitter.width = 0.5),
             alpha = 0.3)


# regression quantitative and qualitative title
data_R2 %>%
  ggplot(aes(x = Title.value, y = Quanti.title, colour = Profession)) +
  geom_point() +
  geom_smooth(method = "lm") 


#regression qualitative and quantitative description
data_R2 %>%
  ggplot(aes(x = Description.value, y = Quanti.description, colour = Profession)) +
  geom_point() +
  geom_smooth(method = "lm") 

#regression qualitative result & Biais indicator
data_R2 %>%
  ggplot(aes(x = Qualitative.result, y = Bias.indicator, colour = Profession)) +
  geom_point() +
  geom_smooth(method = "lm")


#correlation
library(GGally)
data_R2 %>%
  select(Quanti.description,
         Quanti.title,
         Bias.indicator,
         Title.value,
         Description.value,
         Qualitative.result) %>%
  ggpairs(lower=list(continuous=wrap("smooth", colour="black")),
          upper = list(continuous = wrap("cor", size=4, colour = "black"))) +
  theme(strip.text = element_text(size = 8),
        axis.text = element_text(size = 9))


#for inference tree
#run PCA
library(ggfortify)
data_R2 %>%
  select(-c(ID_job, Profession, Title, Description)) %>%
  select(-contains("tit")) %>%
  select(-contains("desc")) %>%
  # run PCA
  prcomp() %>%
  autoplot(data_R2 = data_R2,
           # add colour to the points based on qualitative result
           colour = 'Qualitative.result',
           # add arrows showing the effect of each variable
           loadings = TRUE,
           loadings.colour = "blue",
           # add the names of the arrows
           loadings.label = TRUE,
           loadings.label.colour = "blue")

glimpse(data_R2)

library(party)
ctree(Qualitative.result ~. ,
      data = data_R2 %>% 
        # change predicted class to factors
        mutate(Profession = factor(Profession)) %>%
        mutate(Qualitative.result = factor(Qualitative.result)) %>%
        # remove not needed columns
        select(-c(Title, Description, ID_job)),
      controls = ctree_control(testtype = "MonteCarlo")) -> Bias_tree
plot(Bias_tree)

# we extract the responses from the tree 
pred_biais<-predict(Bias_tree) 
# and use them to predict the value of "To" in every row
accuracy_table<-table(pred_biais,data_R2$Qualitative.result)
# the columns are the right output, the rows are the predictions
accuracy_table
