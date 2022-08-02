library("readxl")
library("ggplot2")
library("GGally")
library("tibble")
coded<-read_xlsx("D:\\Switchdrive\\Private\\Cours IHEID\\Thèse\\Coding\\Tweets\\R_Tweet\\Tweets_FR_merged.xlsx")
#merge old tweet dataset from before the coding
df_FR<-merge(x=tweets_FR, y=coded, by="text")

#remove duplicating tweets
df_Fr<-df_FR[!duplicated(df_FR$text), ]
#create dataframe with sums of columns
sum<-c(colSums(df_Fr [,84:111], na.rm = TRUE))
name<-c(colnames(df_Fr [,84:111]))
df<-data.frame(sum)

#heat map
dfh<-data.matrix(df_Fr[,84:111])
dfh[is.na(dfh)]<-0
heatmap<-heatmap(dfh, scale = "none")
heatmap
ggcorr(dfh, hjust=1, label = TRUE, label_size = 2, layout.exp = 7)

#barplot results
bp_df<-df
bp_df<-rownames_to_column(df, "name")

color<-c("ben_convi_use_exp"="#F4D03F","ben_eco_op"="#F4D03F","ben_protection"="#F4D03F","ben_num_digi"="#F4D03F",
         tech_design="#8DD18B",
         "risk_privacy"="grey","risk_price"="grey","risk_inter"="grey","risk_legal"="grey","risk_secu"="grey","risk_stake"="grey",
         "gov_private"="#F6DDCC","gov_state"="#F6DDCC","gov_sov"="#F6DDCC","gov_private_innov"="#F6DDCC","gov_neo"="#F6DDCC",
         "con_sur_eco"="#3498DB","con_dem_rig"="#3498DB","con_power"="#3498DB","con_sur"="#3498DB",
         "pass_yes"="#D2B4DE","pass_no"="#D2B4DE",
         "oth_prag"="#76D7C4","oth_priori"="#76D7C4","oth_eid_op"="#76D7C4","oth_contra"="#76D7C4","oth_trust"="#76D7C4", "oth_under"="#76D7C4")

bp<-ggplot(bp_df, aes(x=name, y=sum))+
  geom_bar (aes(fill=factor(name)),stat="identity")+
  scale_fill_manual(values = color)+
  theme(axis.text.x = element_text(angle=45, hjust = 1, size=12))+
  xlab("Subcategories")+ylab("Count")+labs(fill="Subcategories by Categories")
bp
#Same for dataset FR but without repetition
coded_rep<-read_xlsx("D:\\Switchdrive\\Private\\Cours IHEID\\Thèse\\Coding\\Tweets\\R_Tweet\\Tweets_FR_merged_no_rep_final.xlsx")
#merge old tweet dataset from before the coding
df_FR_rep<-merge(x=tweets_FR, y=coded_rep, by="text")

#remove duplicating tweets
df_Fr_rep<-df_FR_rep[!duplicated(df_FR$text), ]
#create dataframe with sums of columns
sum<-c(colSums(df_Fr_rep [,84:111], na.rm = TRUE))
name<-c(colnames(df_Fr_rep [,84:111]))
df_rep<-data.frame(sum)

