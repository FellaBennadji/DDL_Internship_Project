library(tidyverse)
library(stringr)

# Run the following to guess the encoding of the file if needed
#guess_encoding("qualitative result.csv")

# Run the following code to get the raw qualitative data
data = read.csv("qualitative result.csv", sep=";", dec=";", header=TRUE,
                fileEncoding = "windows-1252")
view(data)

# For Qualitative result
# For Qualitative result column
data_R <- data %>% 
  mutate(Qualitative.result = rowMeans(data[c('Title.value',
                                              'Title.remark',
                                              'Description.value', 
                                              'Description.remark',
                                              'X4.groups')], na.rm=TRUE),
         Qualitative.result = round(Qualitative.result, digits = 4))

#for qualitative result value column
data_R <- data_R %>% 
  mutate(Qualitative.result.value = case_when(Qualitative.result >= 0.5 ~ 1,
                                              Qualitative.result <= -0.5 ~ -1,
                                              T ~ 0
  ))

#for qualitative title column
data_R <- data_R %>% 
  mutate(Qualitative.title = rowMeans(data[c('Title.value',
                                             'Title.remark')], na.rm=TRUE),
         Qualitative.title = round(Qualitative.title, digits = 4))

#for qualitative description column
data_R <- data_R %>% 
  mutate(Qualitative.description = rowMeans(data[c('Description.value', 
                                                   'Description.remark',
                                                   'X4.groups')], na.rm=TRUE),
         Qualitative.description = round(Qualitative.description, digits = 4))



#For quantitative result

#count occurances in "Title" column
data_R <- data_R %>%
  mutate(Title = tolower(Title),
         ve.count.title = str_count(Title, "[\\(\\.]ve[\\) \\.\\,]"),
         ive.count.title = str_count(Title, "[\\(\\.]ive"),
         e.count.title = str_count(Title, "[\\(\\.]e[\\) \\.\\,]"),
         trice.count.title = str_count(Title, "[\\(\\.]trice[\\) \\.\\,]"),
         ère.count.title = str_count(Title, "[\\(\\.]ère[\\) \\.\\,]"),
         euse.count.title = str_count(Title, "[\\(\\.]euse[\\) \\.\\,]"),
         ne.count.title = str_count(Title, "[\\(\\.]ne[\\) \\.\\,]"),
         hf.count.title = str_count(Title, "h/f"))


#count occurances in "Description" colomn
data_R <- data_R %>%
  # change all to lower case
  mutate(Description = tolower(Description),
         #delete control character : newline character '\n'
         Description = str_replace_all(Description, "[\n]", ""),
         #count occurances
         il.count.description = str_count(Description, "[ ']il[ ]"),
         elle.count.description = str_count(Description, "[ ']elle[ ]"),
         un.count.description = str_count(Description, "[ ']un[ ]"),
         une.count.description = str_count(Description, "[ ']une[ ]"),
         ve.count.description = str_count(Description, "[\\(\\.]ve[\\) \\.\\,]"),
         ive.count.description = str_count(Description, "[\\(\\.]ive"),
         e.count.description = str_count(Description, "[\\(\\.]e[\\) \\.\\,]"),
         trice.count.description = str_count(Description, "[\\(\\.]trice[\\) \\.\\,]"),
         ère.count.description = str_count(Description, "[\\(\\.]ère[\\) \\.\\,]"),
         euse.count.description = str_count(Description, "[\\(\\.]euse[\\) \\.\\,]"),
         ne.count.description = str_count(Description, "[\\(\\.]ne[\\) \\.\\,]"),
         hf.count.description = str_count(Description, "h/f"),
         X4.groups.description = str_count(Description, "(programme|égalité).?d.?accès.?à.?l.?(égalité|emploi)"))

#TEST 
# we need to do random checkings to see if the functions capture what we want
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
data_R %>%
  # take a specific row number
  slice(8) %>%
  # show the description
  pull(Description)

# extract the counts of items and compare with the previous text
data_R %>%
  # take a specific row number
  slice(10) %>%
  # take relevant columns
  select(euse.count.title)


#for Quanti.description
data_R = data_R %>%
  mutate(Quanti.description = case_when(
    #if secretary has 0 indicator means "biaisé femme" = 1
    Profession == "secretary" & (ve.count.description == 0 &
                                   ive.count.description == 0 &
                                   e.count.description == 0 &
                                   trice.count.description == 0 &
                                   ère.count.description == 0 &
                                   euse.count.description == 0 &
                                   ne.count.description == 0 &
                                   hf.count.description == 0 &
                                   X4.groups.description > 0 ) ~ 1,
    #if driver has 0 indicator means "biaisé homme" = -1
    Profession == "driver" & (ve.count.description == 0 &
                                ive.count.description == 0 &
                                e.count.description == 0 &
                                trice.count.description == 0 &
                                ère.count.description == 0 &
                                euse.count.description == 0 &
                                ne.count.description == 0 &
                                hf.count.description == 0 &
                                X4.groups.description == 0 ) ~ -1,
    #else means "paritaire" = 0
    TRUE ~ 0
  ))

#for Quanti.title  
data_R = data_R %>%
  mutate(Quanti.title = case_when(
    #if secretary has 0 indicator means "feminine bias" = 1
    Profession == "secretary" & (ve.count.title == 0 &
                                   ive.count.title == 0 &
                                   e.count.title == 0 &
                                   trice.count.title == 0 &
                                   ère.count.title == 0 &
                                   euse.count.title == 0 &
                                   ne.count.title == 0 &
                                   hf.count.title == 0) ~ 1,
    #if driver has 0 indicator means "masculine bias" = -1
    Profession == "driver" & (ve.count.title == 0 &
                                ive.count.title == 0 &
                                e.count.title == 0 &
                                trice.count.title == 0 &
                                ère.count.title == 0 &
                                euse.count.title == 0 &
                                ne.count.title == 0 &
                                hf.count.title == 0) ~ -1,
    #else means "equal" = 0
    TRUE ~ 0
  ))

#for first indicator
data_R <- data_R %>%
  mutate(First.indicator = rowMeans(data_R[c('Quanti.description','Quanti.title')]),
         First.indicator = round(First.indicator, digits = 4))


#for second indicator
data_R <- data_R %>% 
  mutate(Second.indicator = case_when((il.count.description + un.count.description) > (elle.count.description + une.count.description) ~ -1,
                                      (il.count.description + un.count.description) == (elle.count.description + une.count.description) ~ 0,
                                      T ~ 1                                     
  ))

#for Words count column
data_R <- data_R %>%
  mutate(Description = gsub("[^[:alnum:][:space:]']", "", Description),
         Description = gsub('[[:digit:]]+', '', Description),
         Words.count = str_count(Description, "\\w+"))

# for ratio
data_R <- data_R %>% 
  mutate(il.count.ratio = round(il.count.description / Words.count * 100,4),
         elle.count.ratio = round(elle.count.description / Words.count * 100,4),
         un.count.ratio = round(un.count.description / Words.count * 100,4),
         une.count.ratio = round(une.count.description / Words.count * 100,4))


#stats

# plot the quanti result with bar plot

# Quanti.description
data_R %>%
  # change the column to factor for plotting
  mutate(Quanti.description = factor(Quanti.description)) %>%
  # set x axis variables and color
  ggplot(aes(x = Profession, fill = Quanti.description)) +
  # set the bars to be on the x axis rather than stacked
  geom_bar(position = "dodge")+
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)

#quanti.title
data_R %>%
  # change the column to factor for plotting
  mutate(Quanti.title = factor(Quanti.title)) %>%
  # set x axis variables and color
  ggplot(aes(x = Profession, fill = Quanti.title)) +
  # set the bars to be on the x axis rather than stacked
  geom_bar(position = "dodge")+
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)


#boxplot Quanti.description with qualitative description
data_R %>%
  mutate(Quanti.description = factor(Quanti.description)) %>%
  ggplot(aes(x = Profession, y = Qualitative.description,
             fill = Quanti.description)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        axis.title.x=element_blank(), 
        strip.text.x = element_text(size=12,face="bold"), 
        legend.text=element_text(size=12)) +
  scale_fill_viridis_d() +
  geom_point(position = position_jitterdodge(jitter.width = 0.5),
             alpha = 0.3)


#boxplot quanti.title with qualitative title
data_R %>%
  mutate(Quanti.title = factor(Quanti.title)) %>%
  ggplot(aes(x = Profession, y = Qualitative.title,
             fill = Quanti.title)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        axis.title.x=element_blank(), 
        strip.text.x = element_text(size=12,face="bold"), 
        legend.text=element_text(size=12)) +
  scale_fill_viridis_d() +
  geom_point(position = position_jitterdodge(jitter.width = 0.5),
             alpha = 0.3)


#boxplot first indicator with qualitative result 
data_R %>%
  mutate(First.indicator = factor(First.indicator)) %>%
  ggplot(aes(x = Profession, y = Qualitative.result,
             fill = First.indicator)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        axis.title.x=element_blank(), 
        strip.text.x = element_text(size=12,face="bold"), 
        legend.text=element_text(size=12)) +
  scale_fill_viridis_d() +
  geom_point(position = position_jitterdodge(jitter.width = 0.5),
             alpha = 0.3)

# regression quatitative and qualitative title
data_R %>%
  ggplot(aes(x = Qualitative.title,y = Quanti.title, colour = Profession)) +
  geom_point() +
  # adding regression line
  geom_smooth(method = "lm") 

#regression qualitative and quantitative description
data_R %>%
  ggplot(aes(x = Qualitative.description,y = Quanti.description, colour = Profession)) +
  geom_point() +
  # adding regression line
  geom_smooth(method = "lm") 

#correlation
library(GGally)
data_R %>%
  select(Quanti.description,
         Quanti.title,
         First.indicator,
         Second.indicator,
         Qualitative.title,
         Qualitative.description,
         Qualitative.result) %>%
  # make a correlation plot for each possible pair of variables
  ggpairs(lower=list(continuous=wrap("smooth", colour="black")),
          upper = list(continuous = wrap("cor", size=4, colour = "black"))) +
  theme(strip.text = element_text(size = 8),
        axis.text = element_text(size = 9))


#for inference tree
#run PCA
library(ggfortify)
rownames(data) = data$Title
data_R %>%
  select(-c(Profession, Title, Company, Location, Description, Links,
            Title.remark, Description.remark, Words.count)) %>% 
  mutate(First.indicator = as.numeric(First.indicator)) %>%
  select(-contains("Description")) %>%
  # run PCA
  prcomp() %>%
  autoplot(data = data_R,
           # add colour to the points based on species
           colour = 'Profession',
           shape = FALSE,
           label = TRUE,
           # add arrows showing the effect of each variable
           loadings = TRUE,
           loadings.colour = "blue",
           # add the names of the arrows
           loadings.label = TRUE,
           loadings.label.colour = "blue")


library(party)
ctree(Profession ~. ,
      data = data_R %>% 
        # change predicted class to factors
        mutate(Profession = factor(Profession)) %>%
        mutate(First.indicator = as.numeric(First.indicator)) %>%
        # remove not needed columns
        select(-c(Title, Company, Location, Description, Links,
                  Title.remark, Description.remark)),
      controls = ctree_control(testtype = "MonteCarlo")) -> Profession_tree
plot(Profession_tree)


# we extract the responses from the tree 
pred_biais<-predict(Profession_tree) 
# and use them to predict the value of "To" in every row
accuracy_table<-table(pred_biais,data_R$Profession)
# the columns are the right output, the rows are the predictions
accuracy_table
