library(tidyverse);
library(sparsepca);
library(kernlab);
library(tsne);
library(umap);
library(fpc);
library(aricode);

load("derived_data/train_cleaned.Rdata"); # "train"

# Amenities Indicators
amenities <- train %>% mutate(i_alley=ifelse(Alley=="NA",0,1)) %>%
mutate(i_basement=ifelse(BsmtQual=="NA",0,1)) %>%
mutate(i_fireplace=ifelse(FireplaceQu=="NA",0,1)) %>%
mutate(i_garage=ifelse(GarageFinish=="NA",0,1)) %>%
mutate(i_pool=ifelse(PoolQC=="NA",0,1)) %>%
mutate(i_fence=ifelse(Fence=="NA",0,1)) %>%
select(c(i_alley,i_basement,i_fireplace,i_garage,i_pool,i_fence));

# Basement-Garage-Pool Status
basement_garage_pool <- amenities %>% mutate(bgp_status=ifelse((i_basement==1) & (i_garage==1) & (i_pool==1),"Basement, Garage, & Pool",
                                                        ifelse((i_basement==1) & (i_garage==1) & (i_pool==0),"Basement and Garage Only",
                                                        ifelse((i_basement==1) & (i_garage==0) & (i_pool==0),"Basement Only",
                                                        ifelse((i_basement==0) & (i_garage==1) & (i_pool==0),"Garage Only","None"))))) %>%
mutate(bgp_status_n=ifelse((i_basement==1) & (i_garage==1) & (i_pool==1),5,
                           ifelse((i_basement==1) & (i_garage==1) & (i_pool==0),4,
                                  ifelse((i_basement==1) & (i_garage==0) & (i_pool==0),3,
                                         ifelse((i_basement==0) & (i_garage==1) & (i_pool==0),2,1))))) %>%
select(c(bgp_status, bgp_status_n));

# Format Matrix
X <- model.matrix(~ MSSubClass   + MSZoning    +  LotFrontage  + LotArea      + Street      + Alley         + LotShape      + LandContour   + 
                    Utilities    + LotConfig   +  LandSlope    + Neighborhood + Condition1  + Condition2    + BldgType      + HouseStyle    + OverallQual  +
                    OverallCond  + YearBuilt   + RoofStyle    + RoofMatl    + Exterior1st   + Exterior2nd   + MasVnrType    + MasVnrArea   +
                    ExterQual    + ExterCond   +  Foundation   + BsmtQual     + BsmtCond    + BsmtExposure  + BsmtFinType1  + BsmtFinSF1    + BsmtFinType2 +
                    BsmtFinSF2   + BsmtUnfSF   +  TotalBsmtSF  + Heating      + HeatingQC   + CentralAir    + Electrical    + FirstFlrSF    + SecondFlrSF  +
                    LowQualFinSF + GrLivArea   +  BsmtFullBath + BsmtHalfBath + FullBath    + HalfBath      + BedroomAbvGr  + KitchenAbvGr  + KitchenQual  +
                    TotRmsAbvGrd + Functional  +  Fireplaces   + FireplaceQu  + GarageType   + GarageFinish  + GarageCars    + GarageArea   +
                    GarageQual   + GarageCond  +  PavedDrive   + WoodDeckSF   + OpenPorchSF + EnclosedPorch + ThreeSsnPorch + ScreenPorch   + PoolArea     +
                    PoolQC       + Fence       +  MiscFeature  + MiscVal      + MoSold      + YrSold        + SaleType      + SaleCondition, data=train);
Xs <- X[,-1];

########## Dimension Reduction ##########
# UMAP Seems to do the best

# PCA
pca <- prcomp(Xs, center=FALSE, scale=FALSE);
ggplot(, aes(x = pca$x[,1], y = pca$x[,2])) + 
  geom_point(shape=21, fill="skyblue", color="black") +
  labs(title = "Identifying Clusters from PCA",
       x = "via PC1",
       y= "via PC2");
ggplot(, aes(x = pca$x[,1], y = pca$x[,2], color=factor(basement_garage_pool$bgp_status))) +
  geom_point();



# Kernel PCA
kpca=kpca(Xs);
ggplot(, aes(x = kpca@pcv[,1], y = kpca@pcv[,2])) + 
  geom_point(shape=21, fill="skyblue", color="black") +
  labs(title = "Identifying Clusters from Kernel PCA",
       x = "via PC1",
       y= "via PC2");
ggplot(, aes(x = kpca@pcv[,1], y = kpca@pcv[,2], color=factor(basement_garage_pool$bgp_status))) +
  geom_point();



# tSNE
tsne_data=tsne(Xs);
ggplot(, aes(x = tsne_data[,1], y = tsne_data[,2])) + 
  geom_point(shape=21, fill="skyblue", color="black") +
  labs(title = "Identifying Clusters from tSNE",
       x = "tSNE1",
       y= "tSNE2");
ggplot(, aes(x = tsne_data[,1], y = tsne_data[,2], color=factor(basement_garage_pool$bgp_status))) +
  geom_point() +
  scale_color_manual(values = c("Basement and Garage Only" = "skyblue",
                                "Basement Only" = "#FF5733",
                                "Basement, Garage, & Pool"= "#FFC300",
                                "Garage Only" = "#9467BD",
                                "None" = "#2CA02C")) +
  labs(x = "tSNE1",
       y = "tSNE2",
       title = "True Clusters in tSNE Space",
       color = "Cluster");



# UMAP
umap_results = umap(Xs);
ggplot(, aes(x = umap_results$layout[,1], y = umap_results$layout[,2])) + 
  geom_point(shape=21, fill="skyblue", color="black") +
  labs(title = "Identifying Clusters from UMAP",
       x = "UMAP1",
       y= "UMAP2");
ggplot(, aes(x = umap_results$layout[,1], y = umap_results$layout[,2], color=factor(basement_garage_pool$bgp_status))) +
  geom_point();


# tSNE and UMAP seem to line up groups best for clustering verification


########## Clustering ##########

# K-means
kmeans_pca    <- kmeans(pca$x,               centers=5, nstart=20);
ggplot(, aes(x = pca$x[,1], y = pca$x[,2], color=as.character(kmeans_pca$cluster))) + 
  geom_point();
(kmeans_pca_nmi <- NMI(basement_garage_pool$bgp_status_n,kmeans_pca$cluster));

kmeans_kernel <- kmeans(kpca@pcv,            centers=5, nstart=20);
ggplot(, aes(x = kpca@pcv[,1], y = kpca@pcv[,2], color=as.character(kmeans_kernel$cluster))) + 
  geom_point();
(kmeans_kernel_nmi <- NMI(basement_garage_pool$bgp_status_n,kmeans_kernel$cluster));

kmeans_tSNE   <- kmeans(tsne_data,           centers=5, nstart=20);
ggplot(, aes(x = tsne_data[,1], y = tsne_data[,2], color=as.character(kmeans_tSNE$cluster))) + 
  geom_point();
(kmeans_tSNE_nmi <- NMI(basement_garage_pool$bgp_status_n,kmeans_tSNE$cluster));

kmeans_UMAP   <- kmeans(umap_results$layout, centers=5, nstart=20);
ggplot(, aes(x = umap_results$layout[,1], y = umap_results$layout[,2], color=as.character(kmeans_UMAP$cluster))) + 
  geom_point();
(kmeans_UMAP_nmi <- NMI(basement_garage_pool$bgp_status_n,kmeans_UMAP$cluster));



# DBSCAN
set.seed(730307019);

dbscan_pca <-  dbscan(pca$x, eps = 4.7);
ggplot(, aes(x = pca$x[,1], y = pca$x[,2], color=as.character(dbscan_pca$cluster))) + 
  geom_point();
(dbsan_pca_nmi <- NMI(basement_garage_pool$bgp_status_n,dbscan_pca$cluster));

dbscan_tSNE <- dbscan(tsne_data, eps = 12);
ggplot(, aes(x = tsne_data[,1], y = tsne_data[,2], color=as.character(dbscan_tSNE$cluster))) + 
  geom_point();
(dbscan_tSNE_nmi <- NMI(basement_garage_pool$bgp_status_n,dbscan_tSNE$cluster));

dbscan_UMAP <- dbscan(umap_results$layout, eps = 1);
ggplot(, aes(x = umap_results$layout[,1], y = umap_results$layout[,2], color=as.character(dbscan_UMAP$cluster))) + 
  geom_point();
(dbscan_UMAP_nmi <- NMI(basement_garage_pool$bgp_status_n,dbscan_UMAP$cluster));



# Spectral Clustering
specc_pca    <- specc(pca$x,               centers=5);
ggplot(, aes(x = pca$x[,1], y = pca$x[,2], color=as.character(specc_pca))) + 
  geom_point();
(specc_pca_nmi <- NMI(basement_garage_pool$bgp_status_n,as.numeric(specc_pca)));

specc_kernel <- specc(kpca@pcv,            centers=5);
ggplot(, aes(x = kpca@pcv[,1], y = kpca@pcv[,2], color=as.character(specc_kernel))) + 
  geom_point();
(specc_kernel_nmi <- NMI(basement_garage_pool$bgp_status_n,as.numeric(specc_kernel)));

specc_tSNE   <- specc(tsne_data,           centers=5);
ggplot(, aes(x = tsne_data[,1], y = tsne_data[,2], color=as.character(specc_tSNE))) + 
  geom_point() +
  scale_color_manual(values = c("1" = "skyblue",
                                "4" = "#FF5733",
                                "5" = "#FFC300",
                                "2" = "#9467BD",
                                "3" = "#2CA02C")) +
  labs(x = "tSNE1",
       y = "tSNE2",
       title = "Spectral Clustering Results in tSNE Space",
       color = "Cluster");
(specc_tSNE_nmi <- NMI(basement_garage_pool$bgp_status_n,as.numeric(specc_tSNE)));

specc_UMAP   <- specc(umap_results$layout, centers=5);
ggplot(, aes(x = umap_results$layout[,1], y = umap_results$layout[,2], color=as.character(specc_UMAP))) + 
  geom_point();
(specc_UMAP_nmi <- NMI(basement_garage_pool$bgp_status_n,as.numeric(specc_UMAP)));


