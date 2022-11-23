############################## 
# Add functions
##############################
library(tidyverse)
library(reshape2)
library(psych)
library(factoextra)
library(ggfortify)
require(FactoMineR)
library(data.table)
library(sjmisc)
library(sjPlot)
require(MASS)
require(cowplot)
library(sjstats)
library(pwr)
library(patchwork)


############################## 
# Add necessary datasets
##############################

# Boxgrove Size/shape data
plan_filenames<-list.files("data/Boxgrove_Iovita/Measurements/plan", pattern="*.txt")
profile_filenames<-list.files("data/Boxgrove_Iovita/Measurements/profile", pattern="*.txt")

# Boxgrove Size/area data
metric_filenames<-list.files("data/Boxgrove_Iovita/Measurements/metric", pattern = "*.csv")

# Experiment nodule Size/shape data
nodule_plan_filenames<-list.files("data/Experiment/Nodule_Pretraining/Measurements/plan", pattern="*.txt")
nodule_profile_filenames<-list.files("data/Experiment/Nodule_Pretraining/Measurements/profile", pattern="*.txt")

# Experiment nodule Size/area data
nodule_metric_filenames<-list.files("data/Experiment/Nodule_Pretraining/Measurements/metric", pattern = "*.csv")


# Experimental handaxe data
experimental_data<-read.csv("data/Experiment/experimental_handaxes.csv")

experimental_data<-experimental_data %>%
  rename(individual = knapper) %>%
  mutate(group=ifelse(individual %in% c(1:21),"novice",
                      ifelse(individual %in% c(22:37),"control","expert")))

##############################
# Open Read function
# Calls txt and csv files into R
##############################

# open txt files function
open_read<-function(filename, foldername){
  open_file<-read.delim(paste(foldername,"/",filename,sep=""),header=F,sep='_')
  colnames(open_file)<-c("variable","measurement_point","measurement")
  open_file$individual<-gsub(".txt","",filename)
  return(open_file)
}

##############################
# Merge shape data function
# Merges size data for shape analysis
##############################
# Merge plans and profiles of silhouette with 162-cm-scale
merge_shape_data_function<-function(plan_names,profile_names,plan_pathway,profile_pathway) {
  
  # combine txt files for each recording set
  handaxe_plan_measurements<-do.call(rbind,lapply(plan_names,open_read,plan_pathway))
  handaxe_profile_measurements<-do.call(rbind,lapply(profile_names,open_read,profile_pathway))
  
  # clean and reshape plan data 
  handaxe_plan_measurements_reshape<-handaxe_plan_measurements %>%
    mutate(variable=recode(handaxe_plan_measurements$variable, Width = "width"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_width_mm=measurement*25.4) %>%
    dplyr::select(-c(variable,measurement)) %>%
    dcast(individual ~ measurement_point,value.var="shape_width_mm")
  
  # clean and reshape profile data 
  handaxe_profile_measurements_reshape<-handaxe_profile_measurements %>%
    mutate(variable=recode(handaxe_profile_measurements$variable, Width = "thickness"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_thickness_mm=measurement*25.4) %>%
    dplyr::select(-c(variable,measurement)) %>%
    dcast(individual ~ measurement_point,value.var="shape_thickness_mm")
  
  # create merged shape data
  merged_shape_data<-handaxe_plan_measurements_reshape %>% 
    full_join(handaxe_profile_measurements_reshape,by=c("individual")) %>%
    mutate_if(is.character,as.factor)
  
  return(merged_shape_data)
}


# Combined width/thickness data for Boxgrove
boxgrove_measurements<-merge_shape_data_function(plan_filenames,profile_filenames,"data/Boxgrove_Iovita/Measurements/plan","data/Boxgrove_Iovita/Measurements/profile")

# Save the merged data as a csv file
write.csv(boxgrove_measurements,"data/Boxgrove_Iovita/Measurements/merged.csv", row.names = FALSE)

##############################
# Open Read size/area function
# Calls size/area txt and csv files into R
##############################

open_read_area<-function(filename, foldername){
  open_file_area<-read.csv(paste(foldername,"/",filename,sep=""),header=T)
  colnames(open_file_area)<-c("individual","x","y","max_width","max_length")
  return(open_file_area)
}

##############################
# Size_area function
# Merges size and area data
##############################

size_area_function<-function(area_names,area_pathway) {
  
  # combine csv files,delete extraneous columns
  handaxe_area_measurements<-do.call(rbind,lapply(area_names,open_read_area,area_pathway))
  col_names_list=c("x","y","max_width")
  handaxe_area_measurements[col_names_list]<-NULL
  
  
  # restructure dataframe
  handaxe_area_measurements<- handaxe_area_measurements %>%
    dplyr::select(individual, max_length) %>%
    mutate(max_length=max_length*25.4,
           individual=as.factor(individual))
  
  return(handaxe_area_measurements)
}

handaxes<-size_area_function(metric_filenames,"data/Boxgrove_Iovita/Measurements/metric")

# merge boxgrove shape and max_length datasets

boxgrove_complete<- handaxes %>% 
  full_join(boxgrove_measurements,by=c("individual")) %>%
  mutate_if(is.character,as.factor) %>%
  mutate(assessment=rep("11",times=length(individual)),
         group=rep("boxgrove",times=length(individual))) %>%
  dplyr::select(individual,assessment,group,width_0.1,width_0.2,width_0.3,width_0.4,
                width_0.5,width_0.6,width_0.7,width_0.8,width_0.9,
                thickness_0.1,thickness_0.2,thickness_0.3,thickness_0.4,
                thickness_0.5,thickness_0.6,thickness_0.7,thickness_0.8,
                thickness_0.9,max_length) %>%
  na.omit()


# join boxgrove and experimental

boxgrove_experiment_data<-rbind(boxgrove_complete,experimental_data)

# calculate geomean

boxgrove_experiment_data_scaled<-boxgrove_experiment_data %>%
  mutate(geomean=apply(boxgrove_experiment_data[c(4:22)],1,geometric.mean))

boxgrove_experiment_data_scaled[c(4:22)]=boxgrove_experiment_data_scaled[c(4:22)]/boxgrove_experiment_data_scaled$geomean
boxgrove_experiment_data_scaled$geomean<-NULL

########### PCA

boxgrove_experiment_data_shapes<- boxgrove_experiment_data_scaled[ , which(names(boxgrove_experiment_data_scaled) %in% 
                                                                             c("individual","assessment","group","width_0.1","width_0.2","width_0.3","width_0.4",
                                                                               "width_0.5","width_0.6","width_0.7","width_0.8","width_0.9","thickness_0.1",
                                                                               "thickness_0.2","thickness_0.3","thickness_0.4","thickness_0.5","thickness_0.6",
                                                                               "thickness_0.7","thickness_0.8","thickness_0.9","max_length"))]

individual <- boxgrove_experiment_data_shapes[, 1]
assessment <- boxgrove_experiment_data_shapes[, 2]
group <- boxgrove_experiment_data_shapes[, 3]

# Calculate PCA

boxgrove_experiment_pca<-prcomp(boxgrove_experiment_data_shapes[,-c(1:3)], scale. = F)
boxgrove_experiment_data_shapes1 <- boxgrove_experiment_data_shapes %>% 
  mutate(group=recode(boxgrove_experiment_data_shapes$group, control = "novice"))

bepca <-subset(boxgrove_experiment_data_shapes, select=-c(assessment,group))
bepca <-prcomp(boxgrove_experiment_data_shapes[,-c(1:3)],  scale = TRUE)
bepca1 <-prcomp(boxgrove_experiment_data_shapes[,-c(1:3)],  scale = FALSE)

fviz_pca_ind(bepca1, habillage= boxgrove_experiment_data_shapes1$group, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "convex",
             legend.title = "Groups",
             label="none")
ggsave("General PCA1.png", path="figure", dpi = 600)

# Cumulative scree plot
fviz_eig(bepca1, addlabels=TRUE, hjust = -0.1)
ggsave("Scree plot.png", path="figure", dpi = 600)

# Compute standard deviation of each principal component

std_dev<- boxgrove_experiment_pca$sdev

# Compute variance

pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

# Graph of overall contributions

fviz_contrib(boxgrove_experiment_pca, choice = "var", axes = 1, top = 10)
fviz_contrib(boxgrove_experiment_pca, choice = "var", axes = 2, top = 10)

###Access PCA results###

# Eigenvalues

eig.val <- get_eigenvalue(boxgrove_experiment_pca)

# Results for Variables

res.var <- get_pca_var(boxgrove_experiment_pca)
variable_loadings<-data.frame(res.var$coord)         # Coordinates
variable_loadings_pc12 <- subset(variable_loadings, select=c("Dim.1", "Dim.2"))
write.csv(variable_loadings_pc12,"data\\variable_loadings_pc12.csv", row.names = TRUE)



# Results for individuals

res.ind <- get_pca_ind(boxgrove_experiment_pca)
Indiv_handaxe_scores<-data.frame(res.ind$coord)        # Coordinates

# add data back to dataset

boxgrove_experiment_data_shapes$PC1<-Indiv_handaxe_scores$Dim.1
boxgrove_experiment_data_shapes$PC2<-Indiv_handaxe_scores$Dim.2

# plotting the pc1 and pc2 by assessment stage
boxgrove_experiment_data_shapes2 <- boxgrove_experiment_data_shapes%>%
  filter(assessment != "10" & assessment != "11")

ggplot(data = boxgrove_experiment_data_shapes2) +
  aes(x = as.numeric(assessment), y = PC1) +
  geom_point()+
  geom_smooth()

ggplot(data = boxgrove_experiment_data_shapes2) +
  aes(x = as.numeric(assessment), y = PC2) +
  geom_point()+
  geom_smooth()

# plot pc 1,2 by group

boxgrove_experiment_data_scaled<-boxgrove_experiment_data_scaled %>%
  mutate(assessment_stage=ifelse(assessment %in% c("1"),"Pre-training",
                                 ifelse(assessment %in% c("2","3","4","5"),"Early training",
                                        ifelse(assessment %in% c("6","7","8","9"),"Late training",
                                               ifelse(assessment == "10","Expert","Boxgrove")))))

# Using PCA method and ggplot

pca1 <- PCA(boxgrove_experiment_data_shapes[,-c(1:3)], 
            quali.sup = c(8:10), graph = FALSE)

boxgrove_experiment_data_shapes$pc1 <- pca1$ind$coord[, 1] # indexing the first column
boxgrove_experiment_data_shapes$pc2 <- pca1$ind$coord[, 2]  # indexing the second column

boxgrove_experiment_data_shapes<-boxgrove_experiment_data_shapes %>%
  mutate(assessment_stage=ifelse(assessment %in% c("1"),"Pre-training",
                                 ifelse(assessment %in% c("2","3","4","5"),"Early training",
                                        ifelse(assessment %in% c("6","7","8","9"),"Late training",
                                               ifelse(assessment == "10","Expert","Boxgrove")))),
         group=recode(boxgrove_experiment_data_shapes$group, control = "novice"))

pca.vars <- pca1$var$coord %>% data.frame
pca.vars$vars <- rownames(pca.vars)
pca.vars.m <- melt(pca.vars, id.vars = "vars")

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

circ <- circleFun(c(0,0),2,npoints = 500)

# Plot PC results by learning stage

boxgrove_experiment_data_shapes$assessment_stage<-factor(boxgrove_experiment_data_shapes$assessment_stage,levels = c("Boxgrove","Expert","Late training","Early training","Pre-training"))

hist(boxgrove_experiment_data_shapes$PC1)

PC1_summary<-boxgrove_experiment_data_shapes  %>%
  group_by(assessment_stage) %>%
  summarise(mean=mean(PC1),sd=sd(PC1))

aov_PC1<-aov(PC1~assessment_stage,data=boxgrove_experiment_data_shapes)
summary(aov_PC1)
anova_stats(aov_PC1)

TukeyHSD(aov_PC1)

PC2_summary<-boxgrove_experiment_data_shapes  %>%
  group_by(assessment_stage) %>%
  summarise(mean=mean(PC2),sd=sd(PC2))

aov_PC2<-aov(PC2~assessment_stage,data=boxgrove_experiment_data_shapes)
summary(aov_PC2)
anova_stats(aov_PC2)

TukeyHSD(aov_PC2)



p1<-ggstatsplot::ggbetweenstats(
  data  = boxgrove_experiment_data_shapes,
  x     = assessment_stage,
  y     = PC1,
  ggsignif.args = list(textsize = 1.5, tip_length = 0.01),
  results.subtitle = FALSE,
  title = "A between-group comparison of PC1 values"
)
ggplot2::ggsave("PC1 comparison.png", path="figure", width = 20,
                height = 10, units = "cm", dpi = 600)
ggstatsplot::extract_stats(p1)

p2<-ggstatsplot::ggbetweenstats(
  data  = boxgrove_experiment_data_shapes,
  x     = assessment_stage,
  y     = PC2,
  ggsignif.args = list(textsize = 1.5, tip_length = 0.01),
  results.subtitle = FALSE,
  title = "A between-group comparison of PC2 values"
)
ggplot2::ggsave("PC2 comparison.png", path="figure", width = 20,
                height = 10, units = "cm", dpi = 600)
ggstatsplot::extract_stats(p2)


write.csv(boxgrove_experiment_data_shapes,"data/Experiment/boxgrove_experiment_data_shapes.csv", row.names = FALSE)

# correlation between PC1 and PC2
ggstatsplot::grouped_ggscatterstats(
  data  = dplyr::filter(boxgrove_experiment_data_shapes, group %in% c("boxgrove", "expert", "novice"))
  ,
  x     = PC1,
  y     = PC2,
  grouping.var     = group,
  xlab  = "PC1",
  ylab  = "PC2",
)
ggplot2::ggsave("PC correlation.png", width = 20, height = 10, path="figure", dpi = 600)



data1 <- read.csv("data/Experiment/handaxe_end_weights.csv")
data2 <- read.csv("data/Experiment/handaxe_start_weights.csv")
total <- inner_join(data1, data2, by="Core.Number")
total <- total %>%
  mutate(delta_Weight= Start_Weight-End_Weight)
total <- subset(total, select = -c(Experiment, experiment) )
data3 <- read.csv("data/Experiment/Experiment_Core_Weight_Control_Expert.csv")
data3 <- data3 %>%
  mutate(delta_Weight= Start_Weight-End_Weight)
total1 <- rbind(total, data3)
# Core P4:A1, P12:A1, P14:A3, P20:1, P20:2 are removed because they don't
# have shape data due to breakage. P means participant, A means assessment
total2 <- total1[-c(23, 69, 75, 114, 115), ]


total_grouped <-total2 %>%
  mutate(Assessment_stage=ifelse(Assessment %in% c("1"),"Pre-training",
                                 ifelse(Assessment %in% c("2","3","4","5"),"Early training",
                                        ifelse(Assessment %in% c("6","7","8","9"),"Late training", 
                                          ifelse(Assessment == "10","Expert","Boxgrove")))))
# write.csv(total_grouped,"data/Experiment/Weight data.csv", row.names = FALSE)

#reorder the dataframe based on training stages for plotting.
corder <- c("Expert", "Late training", "Early training", "Pre-training")
total_grouped1<- total_grouped %>%
  mutate(Assessment_stage =  factor(Assessment_stage, levels = corder)) %>%
  arrange(Assessment_stage) 



p <- ggstatsplot::ggbetweenstats(
  data  = total_grouped1,
  x     = Assessment_stage,
  y     = delta_Weight,
  ggsignif.args = list(textsize = 1.5, tip_length = 0.01),
  results.subtitle = FALSE,
  title = "A between-group comparison of delta weight")
ggplot2::ggsave("delta weight comparison3.png", path="figure", width = 20,
                height = 10, units = "cm", dpi = 600)

ggstatsplot::extract_stats(p)

