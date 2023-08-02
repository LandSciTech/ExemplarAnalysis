#################
#Code for calculating composite scores, doing cluster analysis, and making scatter plots.

library(tidyverse)
library(cluster)
library(ggdendro)
if(!require(ggrepel)) install.packages("dendextend")
library(ggrepel)
if(!require(dendextend)) install.packages("dendextend")
library("dendextend")

ignoreMissing = T #Set False to treat missing values as zeroes, True to ignore missing values.

#get composite scores
colsParticipation <-c("braidSetting","braidDesign","braidImplement","braidAnalysis","braidDissem")
# question numbers 13,15,17,19,21 - matches line 201 of R Code.Rmd
colsQuality <- c("qualAccess","qualSalience","qualCredit","ethicsLocal","ethicsConsent","ethicsCommConsent","ethicsIP","ethicsDataSov")
# question numbers 22,23,26,32,33,34,37,38 - matches line 352 of R Code.Rmd
colsCharacter <- c("braidOngoing","braidMembers","govPower","relevance","ro6")

#standardize all to 0-1 & calculate composite scores
#Remove NA variable from composite for that study.

ds=read.csv("./results/indicators.csv")

#special cases
# braidOngoing
ds$braidOngoing[ds$braidOngoing=="Yes"]=1
ds$braidOngoing[ds$braidOngoing=="Not reported"]=0
ds$braidOngoing = as.numeric(ds$braidOngoing)
#braidMembers
ds$braidMembers[ds$braidMembers=="O = Other (listed under \x91Notes\x92)"]=NA
ds$braidMembers[grepl("NR ",ds$braidMembers,fixed=T)]=NA
ds$braidMembers[grepl("A ",ds$braidMembers,fixed=T)]="0"
ds$braidMembers[grepl("M ",ds$braidMembers,fixed=T)]="0"
ds$braidMembers[grepl("C ",ds$braidMembers,fixed=T)]="1"
#Coded between 0 and 1 according to line 647 of R Code.Rmd

ds$braidMembers= as.numeric(ds$braidMembers)
#govPower
ds$govPower[grepl("Yes",ds$govPower)]=1
ds$govPower[grepl("mentioned",ds$govPower)]=0
unique(ds$govPower)
ds$govPower=as.numeric(ds$govPower)
#Note: it looks like non-binary variables are not standardized in R Code.Rmd.
#This effectively puts greater weight on the non-binary variables. Let's discuss.
colStd<-function(inCol,maxVal){
  if(max(inCol,na.rm=T)>maxVal){maxVal=max(inCol,na.rm=T)}
  codes = inCol/maxVal
  return(codes)
}
ds$compParticipation = 0
ds$compQuality = 0
ds$compCharacter = 0
ds$denomParticipation = 0
ds$denomQuality = 0
ds$denomCharacter = 0

for(c in colsParticipation){
  ds[[c]]<-colStd(ds[[c]],maxVal=4)
  compBit = ds[[c]]
  compBit[is.na(compBit)] = 0
  if(ignoreMissing){denomBit = !is.na(ds[[c]])}else{denomBit=1}
  ds$compParticipation = ds$compParticipation+compBit
  ds$denomParticipation = ds$denomParticipation+denomBit

}
for(c in colsQuality){
  ds[[c]]<-colStd(ds[[c]],maxVal=1)
  compBit = ds[[c]]
  compBit[is.na(compBit)] = 0
  if(ignoreMissing){denomBit = !is.na(ds[[c]])}else{denomBit=1}
  ds$compQuality = ds$compQuality+compBit
  ds$denomQuality = ds$denomQuality +denomBit
}
for(c in colsCharacter){
  ds[[c]]<-colStd(ds[[c]],maxVal=1)
  compBit = ds[[c]]
  compBit[is.na(compBit)] = 0
  if(ignoreMissing){denomBit = !is.na(ds[[c]])}else{denomBit=1}
  ds$compCharacter = ds$compCharacter+compBit
  ds$denomCharacter = ds$denomCharacter +denomBit
}

ds$compParticipation = ds$compParticipation/ds$denomParticipation
ds$compQuality = ds$compQuality/ds$denomQuality
ds$compCharacter = ds$compCharacter/ds$denomCharacter

write.csv(ds,file = paste0("./results/wCompositesv2_ignoreMissing",ignoreMissing,".csv"))

sum(is.na(ds$compParticipation)|is.na(ds$compQuality))
nrow(ds)

#plots
ds$studyIDSmall=gsub("BIAS-K_TER_","",ds$studyID,fixed=T)


theme_set(theme_bw())
pdf(paste0("./results/exemplarsv2_Cluster1Scatter_ignoreMissing",ignoreMissing,".pdf"),width=10,height=10)
base1<-ggplot(ds,aes(x=compParticipation,y=compQuality,col=compCharacter,label=studyIDSmall))+
  geom_point(position=position_jitter(),shape=3)+
  geom_text(hjust=0, vjust=0)+
  xlab("Composite Score for Level of Indigenous Participation")+
  ylab("Composite Score for Quality Indicators")
print(base1)
dev.off()

pdf(paste0("./results/exemplarsv2_Cluster2Scatter_ignoreMissing",ignoreMissing,".pdf"),width=10,height=10)
base1<-ggplot(ds,aes(x=compCharacter,y=compQuality,col=compParticipation,label=studyIDSmall))+
  geom_point(position=position_jitter(),shape=3)+
  geom_text(hjust=0, vjust=0)+
  xlab("Composite Score for Character")+
  ylab("Composite Score for Quality Indicators")
print(base1)
dev.off()

#cluster analysis
names(ds)
df = subset(ds,select=c(compParticipation,compQuality,compCharacter))
rownames(df)=df$studyID

## note scores are already standardized above, so this step is unneccesary.
df_std =mutate_all(df, scale)
row.names(df_std) <- df$StudyID

# Assessing which clustering structure to use based on strength of clusters
cat("The following are agglomerative coefficients for different clustering structures. The coefficient closest to 1 represents the strongest clustering method.")
clusteringmethod <- c( "average", "single", "complete", "ward")
names(clusteringmethod) <- c( "average", "single", "complete", "ward")
ac <- function(x) {
  agnes(df_std, method = x)$ac
}
map_dbl(clusteringmethod, ac)
cat("Based on the agglomerative coefficients, the Ward's method shows the strongest clustering structure.")

clusters <- hclust(dist(df_std,method = "euclidean"),method="ward.D")

plot(clusters)

dend = as.dendrogram(clusters)

leafNames <- subset(ds,select=studyIDSmall)

leafNames$id <- seq(1:nrow(leafNames))
leafNames$studyIDSmall <- leafNames$studyIDSmall[labels(dend)]
leafNames$id <- leafNames$id[labels(dend)]
leafNames$label <- labels(dend)

labels(dend) <- leafNames$studyIDSmall

# number of clusters to use
clustNum <- 3

#add cluster grouping info to scatter plot
# get clusters for each paper at level 3
cut3 <- cutree(dend, k = clustNum)
# prune to just get center cluster
dend_prune <- prune(dend, names(cut3)[cut3 != 1])

clustNum <- 5

col_pal <- palette.colors(clustNum+1) %>% as.vector() %>% .[-clustNum]

dend_prune <- color_branches(dend_prune, k = clustNum, col = col_pal) %>%
  color_labels(k = clustNum, col = col_pal)

cut5 <- cutree(dend_prune, k = clustNum)

clusts_to_col <- cut3
clusts_to_col[clusts_to_col != 1] <- 99
clusts_to_col[clusts_to_col == 1] <- cut5
clusts_to_col <- clusts_to_col[order.dendrogram(dend)]

col_pal2 <- c("grey65", col_pal) %>% setNames(c("99", as.character(1:5)))

col_pal2 <- col_pal2[as.character(clusts_to_col)]

dend_all_cols <- dend %>%
  color_labels(clusters = clusts_to_col, col = col_pal2)

paperGroups <-cutree(dend_prune, k = clustNum) %>% as.data.frame()
paperGroups <- paperGroups %>% mutate(studyIDSmall = rownames(paperGroups)) %>%
  setNames(c("clusterID", "studyIDSmall"))

#Identify clusters of interest on dendrogram - to correspond scatter plot below.
pdf(paste0("./results/dendrov2_v1_ignoreMissing",ignoreMissing,".pdf"),width=10,height=15)
plot(dend_all_cols, horiz = TRUE)
dev.off()

#see all together
hist(ds$compCharacter)
paperGroups$included = T
paperGroups$included[is.element(paperGroups$studyIDSmall,c(28,10,87,1,14,160,71,115,77,38))]=F

includedBefore=c(30,103,9,112,59,166,6,95,101,110,67,111)
setdiff(includedBefore,paperGroups$studyIDSmall) #everything in your first cut is in here still
setdiff(paperGroups$studyIDSmall,includedBefore) #these are the additions not in your first cut.
setdiff(paperGroups$studyIDSmall[paperGroups$included],includedBefore)


ds$clusterID=NULL;ds$included=NULL;ds=merge(ds,paperGroups,all.x=T)
ds$included[is.na(ds$included)]=F
ds$clusterID=LETTERS[ds$clusterID]

# too many labels, don't label NA clusters
ds <- mutate(ds, label = ifelse(is.na(clusterID), NA, studyIDSmall))
ds$compCharacterCategory = floor(ds$compCharacter*4)/4

unique(ds$compCharacterCategory)
ds$compCharacterCategory[ds$compCharacterCategory==1]=0.75
unique(ds$compCharacterCategory)

pdf(paste0("./results/exemplarsv2_Cluster3Scatter_ignoreMissing",ignoreMissing,".pdf"),width=7.5,height=5.5)
base1 <- ggplot(ds, aes(x = compParticipation, y = compQuality,
                        col = as.factor(clusterID), label = label,
                        shape = as.factor(clusterID))) +
  geom_point(position = position_jitter(width = 0.02, height = 0.02)) +
  geom_text_repel(force_pull = 0.5, position = position_jitter(width = 0.02, height = 0.02), show.legend = FALSE) +
  scale_shape_discrete(na.value = 24)+
  scale_color_manual(values = col_pal)+
  facet_wrap(~compCharacterCategory, labeller = labeller(compCharacterCategory = c("0" = "Characteristic Composite: 0 - 0.25",
                                                           "0.25" = "Characteristic Composite: 0.25 - 0.5",
                                                           "0.5" = "Characteristic Composite: 0.5 - 0.75",
                                                           "0.75" = "Characteristic Composite: 0.75 - 1"))) +
  xlab("Composite Score for Level of Indigenous Participation") +
  ylab("Composite Score for Quality Indicators")+
  labs(colour = "Cluster", shape = "Cluster")
print(base1)
dev.off()


pdf(paste0("./results/exemplarsv2_Cluster3ScatterCartoon_ignoreMissing",ignoreMissing,".pdf"),width=5,height=3)
base1 <- ggplot(ds, aes(x = compParticipation, y = compQuality,
                        col = "grey",
                        shape = as.factor(clusterID))) +
  geom_point(position = position_jitter(width = 0.02, height = 0.02)) +
  scale_shape_discrete(na.value = 24)+
  scale_color_manual(values = col_pal)+
  facet_wrap(~compCharacterCategory, labeller = labeller(compCharacterCategory = c("0" = "Characteristic Composite: 0 - 0.25",
                                                                                   "0.25" = "Characteristic Composite: 0.25 - 0.5",
                                                                                   "0.5" = "Characteristic Composite: 0.5 - 0.75",
                                                                                   "0.75" = "Characteristic Composite: 0.75 - 1"))) +
  xlab("Composite Score for Level of Indigenous Participation") +
  ylab("Composite Score for Quality Indicators")+
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                       panel.background = element_blank(),
        axis.title.x = element_text(colour = "#d95f02"),axis.title.y = element_text(colour = "#1b9e77"),strip.text = element_text(colour = '#7570b3'),strip.background =element_rect(fill="white"))+
  scale_x_continuous(breaks =c(0,1))+scale_y_continuous(breaks =c(0,0.9))
print(base1)
dev.off()
