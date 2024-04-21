library(tidyverse);
library(car);
library(mice);

# Numeric Checks
a <- function(var) {
  print(class(main0[[var]]));
  hist(main0[[var]]);
  print(sum(is.na(main0[[var]])));
}

# Categorical Checks
b <- function(var) {
  print(class(main0[[var]]));
  print(table(main0[[var]]));
  print(sum(is.na(main0[[var]])));
}

# Main Function
clean_and_impute <- function(main0) {
  
  ########## Formats ##########
  
    # Fix Problematic Names
  main <- main0 %>%
    rename(FirstFlrSF=`1stFlrSF`) %>%
    rename(SecondFlrSF=`2ndFlrSF`) %>%
    rename(ThreeSsnPorch=`3SsnPorch`);
  
  # Cases where "NA" is not missing data
  main <- main %>%
    mutate(Alley=ifelse(is.na(Alley),"NA",Alley)) %>%
    mutate(BsmtQual=ifelse(is.na(BsmtQual),"NA",BsmtQual)) %>%
    mutate(BsmtCond=ifelse(is.na(BsmtCond),"NA",BsmtCond)) %>%
    mutate(BsmtExposure=ifelse(is.na(BsmtExposure),"NA",BsmtExposure)) %>%
    mutate(BsmtFinType1=ifelse(is.na(BsmtFinType1),"NA",BsmtFinType1)) %>%
    mutate(BsmtFinType2=ifelse(is.na(BsmtFinType1),"NA",BsmtFinType2)) %>%
    mutate(FireplaceQu=ifelse(is.na(FireplaceQu),"NA",FireplaceQu)) %>%
    mutate(GarageType=ifelse(is.na(GarageType),"NA",GarageType)) %>%
    mutate(GarageFinish=ifelse(is.na(GarageFinish),"NA",GarageFinish)) %>%
    mutate(GarageQual=ifelse(is.na(GarageQual),"NA",GarageQual)) %>%
    mutate(GarageCond=ifelse(is.na(GarageCond),"NA",GarageCond)) %>%
    mutate(PoolQC=ifelse(is.na(PoolQC),"NA",PoolQC)) %>%
    mutate(PavedDrive=ifelse(is.na(PavedDrive),"NA",PavedDrive)) %>%
    mutate(Fence=ifelse(is.na(Fence),"NA",Fence)) %>%
    mutate(MiscFeature=ifelse(is.na(MiscFeature),"NA",MiscFeature));
  
  # Categorical Variables
  main <- main %>%
    mutate(MSSubClass   =factor(MSSubClass,    ordered=FALSE, levels=c(20,30,40,45,50,60,70,75,80,85,90,120,150,160,180,190))) %>%
    mutate(MSZoning     =factor(MSZoning,      ordered=FALSE, levels=c("A","C","FV","I","RH","RL","RP","RM"))) %>%
    mutate(Street       =factor(Street,        ordered=FALSE, levels=c("Grvl","Pave"))) %>%
    mutate(Alley        =factor(Alley,         ordered=FALSE, levels=c("Grvl","Pave","NA"))) %>%
    mutate(LandContour  =factor(LandContour,   ordered=FALSE, levels=c("Lvl","Bnk","HLS","Low"))) %>%
    mutate(LotConfig    =factor(LotConfig,     ordered=FALSE, levels=c("Inside","Cornder","CulDSac","FR2","FR3"))) %>%
    mutate(Neighborhood =factor(Neighborhood,  ordered=FALSE, levels=c("Blmngtn", "Blueste", "BrDale", "BrkSide", "ClearCr", "CollgCr", "Crawfor", "Edwards", "Gilbert", "IDOTRR", "MeadowV", "Mitchel", "Names", "NoRidge", "NPkVill", "NridgHt", "NWAmes", "OldTown", "SWISU", "Sawyer", "SawyerW", "Somerst", "StoneBr", "Timber", "Veenker"))) %>%
    mutate(Condition1   =factor(Condition1,    ordered=FALSE, levels=c("Artery", "Feedr", "Norm", "RRNn", "RRAn", "PosN", "PosA", "RRNe", "RRAe"))) %>%
    mutate(Condition2   =factor(Condition2,    ordered=FALSE, levels=c("Artery", "Feedr", "Norm", "RRNn", "RRAn", "PosN", "PosA", "RRNe", "RRAe"))) %>%
    mutate(BldgType     =factor(BldgType,      ordered=FALSE, levels=c("1Fam","2FmCon","Duplx","TwnhsE","TwnhsI"))) %>%
    mutate(HouseStyle   =factor(HouseStyle,    ordered=FALSE, levels=c("1Story", "1.5Fin", "1.5Unf", "2Story", "2.5Fin", "2.5Unf", "SFoyer", "SLvl"))) %>%
    mutate(RoofStyle    =factor(RoofStyle,     ordered=FALSE, levels=c("Flat", "Gable", "Gambrel", "Hip", "Mansard", "Shed"))) %>%
    mutate(RoofMatl     =factor(RoofMatl,      ordered=FALSE, levels=c("ClyTile", "CompShg", "Membran", "Metal", "Roll", "Tar&Grv", "WdShake", "WdShngl"))) %>%
    mutate(Exterior1st  =factor(Exterior1st,   ordered=FALSE, levels=c("AsbShng", "AsphShn", "BrkComm", "BrkFace", "CBlock", "CemntBd", "HdBoard", "ImStucc", "MetalSd", "Other", "Plywood", "PreCast", "Stone", "Stucco", "VinylSd", "Wd Sdng", "WdShing"))) %>%
    mutate(Exterior2nd  =factor(Exterior2nd,   ordered=FALSE, levels=c("AsbShng", "AsphShn", "BrkComm", "BrkFace", "CBlock", "CemntBd", "HdBoard", "ImStucc", "MetalSd", "Other", "Plywood", "PreCast", "Stone", "Stucco", "VinylSd", "Wd Sdng", "WdShing"))) %>%
    mutate(MasVnrType   =factor(MasVnrType,    ordered=FALSE, levels=c("BrkCmn","BrkFace","CBlock","None","Stone"))) %>%
    mutate(Foundation   =factor(Foundation,    ordered=FALSE, levels=c("BrkTil","CBlock","PConc","Slab","Stone","Wood"))) %>%
    mutate(Heating      =factor(Heating,       ordered=FALSE, levels=c("Floor","GasA","GasW","Grav","OthW","Wall"))) %>%
    mutate(CentralAir   =factor(CentralAir,    ordered=FALSE, levels=c("N","Y"))) %>%
    mutate(Electrical   =factor(Electrical,    ordered=FALSE, levels=c("SBrkr","FuseA","FuseF","FuseP","Mix"))) %>%
    mutate(GarageType   =factor(GarageType,    ordered=FALSE, levels=c("2Types","Attchd","Basment","BuiltIn","CarPort","Detchd","NA"))) %>%
    mutate(Fence        =factor(Fence,         ordered=FALSE, levels=c("GdPrv","MnPrv","GdWo","MnWw","NA"))) %>%
    mutate(MiscFeature  =factor(MiscFeature,   ordered=FALSE, levels=c("Elev","Gar2","Othr","Shed","TenC","NA"))) %>%
    mutate(SaleType     =factor(SaleType,      ordered=FALSE, levels=c("WD", "CWD", "VWD", "New", "COD", "Con", "ConLw", "ConLI", "ConLD", "Oth"))) %>%
    mutate(SaleCondition=factor(SaleCondition, ordered=FALSE, levels=c("Normal","Abnorml","AdjLand","Alloca","Family","Partial")));
  
  # Ordinal Variables
  main <- main %>%
    mutate(LotShape=factor(LotShape,         ordered=TRUE, levels=c("Reg", "IR1", "IR2", "IR3"))) %>%
    mutate(Utilities=factor(Utilities,       ordered=TRUE, levels=c("AllPub","NoSewr","NoSeWa","ELO"))) %>%
    mutate(LandSlope=factor(LandSlope,       ordered=TRUE, levels=c("Gtl","Mod","Sev"))) %>%
    mutate(ExterQual=factor(ExterQual,       ordered=TRUE, levels=c("Ex","Gd","TA","Fa","Po"))) %>%
    mutate(ExterCond=factor(ExterCond,       ordered=TRUE, levels=c("Ex","Gd","TA","Fa","Po"))) %>%
    mutate(BsmtQual=factor(BsmtQual,         ordered=TRUE, levels=c("Ex","Gd","TA","Fa","Po","NA"))) %>%
    mutate(BsmtCond=factor(BsmtCond,         ordered=TRUE, levels=c("Ex","Gd","TA","Fa","Po","NA"))) %>%
    mutate(BsmtExposure=factor(BsmtExposure, ordered=TRUE, levels=c("Gd","Av","Mn","No","NA"))) %>%
    mutate(BsmtFinType1=factor(BsmtFinType1, ordered=TRUE, levels=c("GLQ","ALQ","BLQ","Rec","LwQ","Unf","NA"))) %>%
    mutate(BsmtFinType2=factor(BsmtFinType2, ordered=TRUE, levels=c("GLQ","ALQ","BLQ","Rec","LwQ","Unf","NA"))) %>%
    mutate(FireplaceQu=factor(FireplaceQu,   ordered=TRUE, levels=c("Ex","Gd","TA","Fa","Po","NA"))) %>%
    mutate(GarageFinish=factor(GarageFinish, ordered=TRUE, levels=c("Fin","RFn","Unf","NA"))) %>%
    mutate(GarageQual=factor(GarageQual,     ordered=TRUE, levels=c("Ex","Gd","TA","Fa","Po","NA"))) %>%
    mutate(GarageCond=factor(GarageCond,     ordered=TRUE, levels=c("Ex","Gd","TA","Fa","Po","NA"))) %>%
    mutate(PoolQC=factor(PoolQC,             ordered=TRUE, levels=c("Ex","Gd","TA","Fa","NA"))) %>%
    mutate(HeatingQC=factor(HeatingQC,       ordered=TRUE, levels=c("Ex","Gd","TA","Fa","Po"))) %>%
    mutate(KitchenQual=factor(KitchenQual,   ordered=TRUE, levels=c("Ex","Gd","TA","Fa","Po"))) %>%
    mutate(Functional=factor(Functional,     ordered=TRUE, levels=c("Typ","Min1","Min2","Mod","Maj1","Maj2","Sev","Sal"))) %>%
    mutate(PavedDrive=factor(PavedDrive,     ordered=TRUE, levels=c("Y","P","N")));
  
  ########## Location and Scale Disparities ##########
  
  # Reduce Location and Scale Disparities (of Numeric Variables)
  main <- main %>%
    mutate(LotFrontage  =(LotFrontage  -mean(LotFrontage  , na.rm=TRUE))/sd(LotFrontage  , na.rm=TRUE)) %>%
    mutate(LotArea      =(LotArea      -mean(LotArea      , na.rm=TRUE))/sd(LotArea      , na.rm=TRUE)) %>%
    mutate(MasVnrArea   =(MasVnrArea   -mean(MasVnrArea   , na.rm=TRUE))/sd(MasVnrArea   , na.rm=TRUE)) %>%
    mutate(BsmtFinSF1   =(BsmtFinSF1   -mean(BsmtFinSF1   , na.rm=TRUE))/sd(BsmtFinSF1   , na.rm=TRUE)) %>%
    mutate(BsmtFinSF2   =(BsmtFinSF2   -mean(BsmtFinSF2   , na.rm=TRUE))/sd(BsmtFinSF2   , na.rm=TRUE)) %>%
    mutate(BsmtUnfSF    =(BsmtUnfSF    -mean(BsmtUnfSF    , na.rm=TRUE))/sd(BsmtUnfSF    , na.rm=TRUE)) %>%
    mutate(TotalBsmtSF  =(TotalBsmtSF  -mean(TotalBsmtSF  , na.rm=TRUE))/sd(TotalBsmtSF  , na.rm=TRUE)) %>%
    mutate(FirstFlrSF   =(FirstFlrSF   -mean(FirstFlrSF   , na.rm=TRUE))/sd(FirstFlrSF   , na.rm=TRUE)) %>%
    mutate(SecondFlrSF  =(SecondFlrSF  -mean(SecondFlrSF  , na.rm=TRUE))/sd(SecondFlrSF  , na.rm=TRUE)) %>%
    mutate(LowQualFinSF =(LowQualFinSF -mean(LowQualFinSF , na.rm=TRUE))/sd(LowQualFinSF , na.rm=TRUE)) %>%
    mutate(GrLivArea    =(GrLivArea    -mean(GrLivArea    , na.rm=TRUE))/sd(GrLivArea    , na.rm=TRUE)) %>%
    mutate(GarageArea   =(GarageArea   -mean(GarageArea   , na.rm=TRUE))/sd(GarageArea   , na.rm=TRUE)) %>%
    mutate(WoodDeckSF   =(WoodDeckSF   -mean(WoodDeckSF   , na.rm=TRUE))/sd(WoodDeckSF   , na.rm=TRUE)) %>%
    mutate(OpenPorchSF  =(OpenPorchSF  -mean(OpenPorchSF  , na.rm=TRUE))/sd(OpenPorchSF  , na.rm=TRUE)) %>%
    mutate(EnclosedPorch=(EnclosedPorch-mean(EnclosedPorch, na.rm=TRUE))/sd(EnclosedPorch, na.rm=TRUE)) %>%
    mutate(ThreeSsnPorch=(ThreeSsnPorch-mean(ThreeSsnPorch, na.rm=TRUE))/sd(ThreeSsnPorch, na.rm=TRUE)) %>%
    mutate(ScreenPorch  =(ScreenPorch  -mean(ScreenPorch  , na.rm=TRUE))/sd(ScreenPorch  , na.rm=TRUE)) %>%
    mutate(PoolArea     =(PoolArea     -mean(PoolArea     , na.rm=TRUE))/sd(PoolArea     , na.rm=TRUE)) %>%
    mutate(MiscVal      =(MiscVal      -mean(MiscVal      , na.rm=TRUE))/sd(MiscVal      , na.rm=TRUE)) %>%
    mutate(YearBuilt	  =(YearBuilt    -mean(YearBuilt    , na.rm=TRUE))/sd(YearBuilt    , na.rm=TRUE)) %>%
    mutate(YearRemodAdd =(YearRemodAdd -mean(YearRemodAdd , na.rm=TRUE))/sd(YearRemodAdd , na.rm=TRUE)) %>%
    mutate(GarageYrBlt  =(GarageYrBlt  -mean(GarageYrBlt  , na.rm=TRUE))/sd(GarageYrBlt  , na.rm=TRUE)) %>%
    mutate(MoSold       =(MoSold       -mean(MoSold       , na.rm=TRUE))/sd(MoSold       , na.rm=TRUE)) %>%
    mutate(YrSold       =(YrSold       -mean(YrSold       , na.rm=TRUE))/sd(YrSold       , na.rm=TRUE));
  
  ########## Imputation ##########
  
  # Imputation
  main <- tibble(complete(mice(main, method="rf")));
  
  main <- mutate(main,Utilities=ifelse(is.na(Utilities),"AllPub",Utilities));
  
  ########## Eigenanalysis ##########
  
  # Eigenanalysis
  # X <- model.matrix(~ MSSubClass   + MSZoning    +  LotFrontage  + LotArea      + Street      + Alley         + LotShape      + LandContour   + 
  #                     Utilities    + LotConfig   +  LandSlope    + Neighborhood + Condition1  + Condition2    + BldgType      + HouseStyle    + OverallQual  +
  #                     OverallCond  + YearBuilt   +  YearRemodAdd + RoofStyle    + RoofMatl    + Exterior1st   + Exterior2nd   + MasVnrType    + MasVnrArea   +
  #                     ExterQual    + ExterCond   +  Foundation   + BsmtQual     + BsmtCond    + BsmtExposure  + BsmtFinType1  + BsmtFinSF1    + BsmtFinType2 +
  #                     BsmtFinSF2   + BsmtUnfSF   +  TotalBsmtSF  + Heating      + HeatingQC   + CentralAir    + Electrical    + FirstFlrSF    + SecondFlrSF  +
  #                     LowQualFinSF + GrLivArea   +  BsmtFullBath + BsmtHalfBath + FullBath    + HalfBath      + BedroomAbvGr  + KitchenAbvGr  + KitchenQual  +
  #                     TotRmsAbvGrd + Functional  +  Fireplaces   + FireplaceQu  + GarageType  + GarageYrBlt   + GarageFinish  + GarageCars    + GarageArea   +
  #                     GarageQual   + GarageCond  +  PavedDrive   + WoodDeckSF   + OpenPorchSF + EnclosedPorch + ThreeSsnPorch + ScreenPorch   + PoolArea     +
  #                     PoolQC       + Fence       +  MiscFeature  + MiscVal      + MoSold      + YrSold        + SaleType      + SaleCondition, data=main);
  X <- model.matrix(~ MSSubClass   + MSZoning    +  LotFrontage  + LotArea      + Street      + Alley         + LotShape      + LandContour   + 
                      Utilities    + LotConfig   +  LandSlope    + Neighborhood + Condition1  + Condition2    + BldgType      + HouseStyle    + OverallQual  +
                      OverallCond  + YearBuilt   + RoofStyle    + RoofMatl    + Exterior1st   + Exterior2nd   + MasVnrType    + MasVnrArea   +
                      ExterQual    + ExterCond   +  Foundation   + BsmtQual     + BsmtCond    + BsmtExposure  + BsmtFinType1  + BsmtFinSF1    + BsmtFinType2 +
                      BsmtFinSF2   + BsmtUnfSF   +  TotalBsmtSF  + Heating      + HeatingQC   + CentralAir    + Electrical    + FirstFlrSF    + SecondFlrSF  +
                      LowQualFinSF + GrLivArea   +  BsmtFullBath + BsmtHalfBath + FullBath    + HalfBath      + BedroomAbvGr  + KitchenAbvGr  + KitchenQual  +
                      TotRmsAbvGrd + Functional  +  Fireplaces   + FireplaceQu  + GarageType   + GarageFinish  + GarageCars    + GarageArea   +
                      GarageQual   + GarageCond  +  PavedDrive   + WoodDeckSF   + OpenPorchSF + EnclosedPorch + ThreeSsnPorch + ScreenPorch   + PoolArea     +
                      PoolQC       + Fence       +  MiscFeature  + MiscVal      + MoSold      + YrSold        + SaleType      + SaleCondition, data=main);
  Xs <- X[,-1];
  pc <- prcomp(Xs, center=F, scale=F);
  pc_var <- format(summary(pc)$importance[2,], scientific=FALSE);
  pc_cum_var <- format(summary(pc)$importance[3,], scientific=FALSE);
  first_pcs <- format(pc$rotation[,1:4], scientific=FALSE);
  
  ggplot(main0, aes(x = GarageYrBlt, y = YearBuilt)) + 
    geom_point(shape=21, fill="skyblue", color="black") +
    labs(title = "Collinearity Between YearBuilt and GarageYrBlt (Raw Data)", 
         x = "Year Garage Was Built (GarageYrBlt)", 
         y = "Original Construction Date (YearBuilt)") + 
    scale_x_continuous(breaks = seq(1870, 2010, by = 10)) +
    scale_y_continuous(breaks = seq(1870, 2010, by = 10));
  
  ggplot(main0, aes(x = YearRemodAdd, y = YearBuilt)) + 
    geom_point(shape=21, fill="orange", color="black") +
    labs(title = "Collinearity Between YearBuilt and YearRemodAdd (Raw Data)", 
         x = "Remodel Date (YearRemodAdd)", 
         y = "Original Construction Date (YearBuilt)") + 
    scale_x_continuous(breaks = seq(1870, 2010, by = 10)) +
    scale_y_continuous(breaks = seq(1870, 2010, by = 10));
  
  main <- select(main, -c(GarageYrBlt,YearRemodAdd));
  return(main);
}

#############################################################################
############################# Training Data #################################
#############################################################################

train0 <- read_csv("source_data/train.csv",show_col_types = FALSE);

train <- clean_and_impute(train0);

write_csv(train,"derived_data/train_cleaned.csv");
save(train, file="derived_data/train_cleaned.Rdata");

#############################################################################
############################# Testing Data ##################################
#############################################################################

test0 <- read_csv("source_data/test.csv",show_col_types = FALSE);

test <- clean_and_impute(test0);

write_csv(test,"derived_data/test_cleaned.csv");
save(test, file="derived_data/test_cleaned.Rdata");

