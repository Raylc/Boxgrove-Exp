############################## 
# Add functions
##############################

library(dplyr)
library(tidyr)
library(data.table)
library(psych)
library(factoextra)
library(ggfortify)
library(sjmisc)
library(sjPlot)
require(FactoMineR)
require(MASS)
require(reshape2)
require(cowplot)
library(sjstats)

############################## 
# Set working directory and source files
##############################

# Set to source of shape and size data txt files
getwd()

############################## 
# Add necessary datasets
##############################

# Size/shape data
plan_filenames_160<-list.files("Handaxes/boxgrove_measurements/plan_822_160", pattern="*.txt")
profile_filenames_160<-list.files("Handaxes/boxgrove_measurements/profile_822_160", pattern="*.txt")
plan_filenames_162<-list.files("Handaxes/boxgrove_measurements/plan_2", pattern="*.txt")
profile_filenames_162<-list.files("Handaxes/boxgrove_measurements/profile_2", pattern="*.txt")

# Size/area data
area_filenames_8_20<-list.files("boxgrove_handaxes/Area/area_8_20", pattern = "*.csv")
area_filenames_8_21<-list.files("boxgrove_handaxes/Area/area_8_21", pattern = "*.csv")
area_filenames_8_22<-list.files("boxgrove_handaxes/Area/area_8_22", pattern = "*.csv")

#experimental handaxe data
experimental_data<-read.csv("experimental_handaxes.csv")

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
plan_names=plan_filenames_160
profile_names=profile_filenames_160
plan_pathway="Handaxes/boxgrove_measurements/plan_822_160"
profile_pathway="Handaxes/boxgrove_measurements/profile_822_160"

merge_shape_data_function_160<-function(plan_names,profile_names,plan_pathway,profile_pathway) {
  
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
    mutate(variable=dplyr::recode(variable, Width = "thickness"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_thickness_mm=measurement*25.4) %>%
    select(-c(variable,measurement)) %>%
    dcast(individual ~ measurement_point,value.var="shape_thickness_mm")
  
  # create merged shape data
  merged_shape_data<-handaxe_plan_measurements_reshape %>% 
    full_join(handaxe_profile_measurements_reshape,by=c("individual")) %>%
    mutate_if(is.character,as.factor)
  
  return(merged_shape_data)
}

merge_shape_data_function_162<-function(plan_names,profile_names,plan_pathway,profile_pathway) {
  
  # combine txt files for each recording set
  handaxe_plan_measurements<-do.call(rbind,lapply(plan_names,open_read,plan_pathway))
  handaxe_profile_measurements<-do.call(rbind,lapply(profile_names,open_read,profile_pathway))
  
  # clean and reshape plan data 
  handaxe_plan_measurements_reshape<-handaxe_plan_measurements %>%
    mutate(variable=recode(handaxe_plan_measurements$variable, Width = "width"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_width_mm=measurement*25.4,
           shape_width_mm=shape_width_mm-(shape_width_mm*0.01477)) %>%
    select(-c(variable,measurement)) %>%
    dcast(individual ~ measurement_point,value.var="shape_width_mm")
  
  # clean and reshape profile data 
  handaxe_profile_measurements_reshape<-handaxe_profile_measurements %>%
    mutate(variable=dplyr::recode(variable, Width = "thickness"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_thickness_mm=measurement*25.4,
           shape_thickness_mm=shape_thickness_mm-(shape_thickness_mm*0.01477 # adjusts scale to correct for 1:162 issue
           )) %>%
    select(-c(variable,measurement)) %>%
    dcast(individual ~ measurement_point,value.var="shape_thickness_mm")
  
  # create merged shape data
  merged_shape_data<-handaxe_plan_measurements_reshape %>% 
    full_join(handaxe_profile_measurements_reshape,by=c("individual")) %>%
    mutate_if(is.character,as.factor)
  
  return(merged_shape_data)
}

#Create combined width/thickness data for two different scales (1:160 & 1:162)

boxgrove_measurements_160<-merge_shape_data_function_160(plan_filenames_160,profile_filenames_160,"Handaxes/boxgrove_measurements/plan_822_160","Handaxes/boxgrove_measurements/profile_822_160")
boxgrove_measurements_162<-merge_shape_data_function_162(plan_filenames_162,profile_filenames_162,"Handaxes/boxgrove_measurements/plan_2","Handaxes/boxgrove_measurements/profile_2")

# Merge 1:160 & 1:162 (scales have been corrected in merge_shape_data_function_160 & merge_shape_data_function_162 functions)

boxgrove_measurements<-
  full_join(boxgrove_measurements_160,boxgrove_measurements_162)

##############################
# Open Read size/area function
# Calls size/area txt and csv files into R
##############################

open_read_area<-function(filename, foldername){
  open_file_area<-read.csv(paste(foldername,"/",filename,sep=""),header=T)
  colnames(open_file_area)<-c("area","perimeter","x","y","max_width","max_length","individual")
  return(open_file_area)
}

##############################
# Size_area function
# Merges size and area data
##############################

size_area_function_8_20<-function(area_names,area_pathway) {
  
  # combine csv files,delete extraneous columns
  handaxe_area_measurements<-do.call(rbind,lapply(area_names,open_read_area,area_pathway))
  col_names_list=c("x","y","max_width")
  handaxe_area_measurements[col_names_list]<-NULL
  
  handaxe_area_measurements<-handaxe_area_measurements[-(1:60),] 
  
  # restructure dataframe
  handaxe_area_measurements<- handaxe_area_measurements %>%
    select(individual, perimeter,max_length) %>%
    mutate(perimeter=perimeter*25.4,   # convert inches squared to mm squared
           max_length=max_length*25.4,
           perimeter=perimeter-(perimeter*0.01477),
           max_length=max_length-(max_length*0.01477),
           individual=as.factor(individual))
  
  return(handaxe_area_measurements)
}

size_area_function_8_21<-function(area_names,area_pathway) {
  
  # combine csv files,delete extraneous columns
  handaxe_area_measurements<-do.call(rbind,lapply(area_names,open_read_area,area_pathway))
  col_names_list=c("x","y","max_width")
  handaxe_area_measurements[col_names_list]<-NULL
  
  handaxe_area_measurements<-handaxe_area_measurements[-(1:76),] 
  
  # restructure dataframe
  handaxe_area_measurements<- handaxe_area_measurements %>%
    select(individual, perimeter,max_length) %>%
    mutate(perimeter=perimeter*25.4,   # convert inches squared to mm squared
           max_length=max_length*25.4,
           perimeter=perimeter-(perimeter*0.01477),
           max_length=max_length-(max_length*0.01477),
           individual=as.factor(individual))
  
  return(handaxe_area_measurements)
}

size_area_function_8_22<-function(area_names,area_pathway) {
  
  # combine csv files,delete extraneous columns
  handaxe_area_measurements<-do.call(rbind,lapply(area_names,open_read_area,area_pathway))
  col_names_list=c("x","y","max_width")
  handaxe_area_measurements[col_names_list]<-NULL
  
  handaxe_area_measurements<-handaxe_area_measurements[-(1:53),] 
  
  # restructure dataframe
  handaxe_area_measurements<- handaxe_area_measurements %>%
    select(individual, perimeter,max_length) %>%
    mutate(perimeter=perimeter*25.4,   # convert inches squared to mm squared
           max_length=max_length*25.4,
           individual=as.factor(individual))
  
  return(handaxe_area_measurements)
}

handaxes_8_20<-size_area_function_8_20(area_filenames_8_20,"boxgrove_handaxes/Area/area_8_20")
handaxes_8_21<-size_area_function_8_21(area_filenames_8_21,"boxgrove_handaxes/Area/area_8_21")
handaxes_8_22<-size_area_function_8_22(area_filenames_8_22,"boxgrove_handaxes/Area/area_8_22")

boxgrove_combined<-full_join(handaxes_8_20,handaxes_8_21)
boxgrove_combined<-full_join(boxgrove_combined,handaxes_8_22)

# merge boxgrove shape and max_length datasets

boxgrove_complete<- boxgrove_combined %>% 
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

# Compute standard deviation of each principal component

std_dev<- boxgrove_experiment_pca$sdev

# Compute variance

pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

# Scree plot

pc_scree_plot<-plot(prop_varex, xlab = "Principal Component",
                    ylab = "Proportion of Variance Explained",
                    type = "b")

# Cumulative scree plot

pc_cumulative_plot<-plot(cumsum(prop_varex), xlab = "Principal Component",
                         ylab = "Cumulative Proportion of Variance Explained",
                         type = "b")

# Graph of variables

fviz_pca_var(boxgrove_experiment_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Graph of overall contributions

fviz_contrib(boxgrove_experiment_pca, choice = "var", axes = 1, top = 10)
fviz_contrib(boxgrove_experiment_pca, choice = "var", axes = 2, top = 10)

###Access PCA results###

# Eigenvalues

eig.val <- get_eigenvalue(boxgrove_experiment_pca)

# Results for Variables

res.var <- get_pca_var(boxgrove_experiment_pca)
variable_loadings<-data.frame(res.var$coord)         # Coordinates

# Results for individuals

res.ind <- get_pca_ind(boxgrove_experiment_pca)
Indiv_handaxe_scores<-data.frame(res.ind$coord)        # Coordinates

# add data back to dataset

boxgrove_experiment_data_shapes$PC1<-Indiv_handaxe_scores$Dim.1
boxgrove_experiment_data_shapes$PC2<-Indiv_handaxe_scores$Dim.2

# plot pc 1,2 by group

boxgrove_experiment_data_scaled<-boxgrove_experiment_data_scaled %>%
  mutate(assessment_stage=ifelse(assessment %in% c("1","2","3"),"Earlier",
                                 ifelse(assessment %in% c("4","5","6"),"Middle",
                                        ifelse(assessment %in% c("7","8","9"),"Later",
                                               ifelse(assessment == "10","Expert","Boxgrove")))))

# Using PCA method and ggplot

pca1 <- PCA(boxgrove_experiment_data_shapes[,-c(1:3)], 
            quali.sup = c(8:10), graph = FALSE)
plot.PCA(pca1)

boxgrove_experiment_data_shapes$pc1 <- pca1$ind$coord[, 1] # indexing the first column
boxgrove_experiment_data_shapes$pc2 <- pca1$ind$coord[, 2]  # indexing the second column

boxgrove_experiment_data_shapes<-boxgrove_experiment_data_shapes %>%
  mutate(assessment_stage=ifelse(assessment %in% c("1","2","3"),"Earlier",
                                 ifelse(assessment %in% c("4","5","6"),"Middle",
                                        ifelse(assessment %in% c("7","8","9"),"Later",
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

p <-ggplot(data = boxgrove_experiment_data_shapes, aes(x = pc1, y = pc2, color = group, shape = group)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  guides(color = guide_legend(title = "group"), shape = guide_legend(title = "group")) +
  scale_shape_manual(values = c(15, 16, 17, 16)) +
  geom_point(alpha = 0.8, size = 2) 

p + stat_ellipse(geom="polygon", aes(fill = group), 
                 alpha = 0.2, 
                 show.legend = FALSE, 
                 level = 0.80) +
  xlab("PC 1 (56.65%)") + 
  ylab("PC 2 (16.20%)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent"))

# Plot PC results by learning stage

boxgrove_experiment_data_shapes$assessment_stage<-factor(boxgrove_experiment_data_shapes$assessment_stage,levels = c("Boxgrove","Expert","Later","Middle","Earlier"))

hist(boxgrove_experiment_data_shapes$pc1)

pc1_summary<-boxgrove_experiment_data_shapes  %>%
  group_by(assessment_stage) %>%
  summarise(mean=mean(pc1),sd=sd(pc1))

aov_pc1<-aov(pc1~assessment_stage,data=boxgrove_experiment_data_shapes)
summary(aov_pc1)
anova_stats(aov_pc1)

TukeyHSD(aov_pc1)

pc2_summary<-boxgrove_experiment_data_shapes  %>%
  group_by(assessment_stage) %>%
  summarise(mean=mean(pc2),sd=sd(pc2))

aov_pc2<-aov(pc2~assessment_stage,data=boxgrove_experiment_data_shapes)
summary(aov_pc2)
anova_stats(aov_pc2)

TukeyHSD(aov_pc2)

ggplot(data = pc1_summary,aes(x=assessment_stage,y=mean,fill=assessment_stage))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)+
  scale_color_viridis_d()+
  theme(legend.position="none")+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Average PC 1 score")+
  theme(text = element_text(size=27))

ggplot(data = pc2_summary,aes(x=assessment_stage,y=mean,fill=assessment_stage))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)+
  scale_color_viridis_d()+
  theme(legend.position="none")+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Average PC 2 score")+
  theme(text = element_text(size=27))
