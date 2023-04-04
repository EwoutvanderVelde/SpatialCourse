
easypackages::packages ("rlang", "caret", "sf", "sp", "tmap", "mapview", "car", "RColorBrewer", "tidyverse", "osmdata", "nngeo", "FNN", "rpart", "rpart.plot", "sessioninfo", "rattle", "ipred", "tidymodels", "ranger", "modelStudio", "DALEX", "DALEXtra", "vip", "pdp", "rgeos", "spatialRF")


#Load in the data

housesdata <- st_read("data/nb_df.gpkg")


#Print the columns
names(housesdata)

#Make it into a spatial dataFrame and get the distance Matrix

housesdata$id <- 1:nrow(housesdata) #this is to give an unique ID to each row
housesdata$x <- st_coordinates(housesdata)[, 1] #get the X coordinate of the point
housesdata$y <- st_coordinates(housesdata)[, 2] #get the Y coordinate of the point

housesdata_sp <- as_Spatial(housesdata)
distance_matrix <- gDistance(housesdata_sp, byid=TRUE)
distance.thresholds <- c(0, 100, 300, 500)
housesdata_spdf <- housesdata %>% st_drop_geometry()
xy <- housesdata_spdf[, c("x", "y")]


#Columns to predict and the dependent variable

predictors = c("bedrooms", "bathrooms", "half_bathrooms", "lot_size", "construction_size", 
               "property_type_id_id", "real_age", "parks", "schools", "university", "hospital", 
               "sport_facility", "supermarket", "mall", "historic", "museum", "industry", 
               "subway", "bus", "stadium", "airport")
dependent="price"

randon.seed = set.seed(123)

spatialHousePriceRF <- spatialRF::rf_spatial(
  data = housesdata_spdf,
  dependent.variable.name = dependent,
  predictor.variable.names = predictors,
  distance.matrix = distance_matrix,
  distance.thresholds = 0,  # here we selected zero as in non-spatial model we found at multiple thresholds they all show auto-correlation
  method = "mem.moran.sequential", #default method, you can select other methods too such as mem.effect.recursive, check help for details
  ranger.arguments = list( #this part we keep same as non-spatial model
    mtry = 3,
    min.node.size = 5,
    num.trees = 500
  ),
  verbose = TRUE,
  seed = randon.seed
)

saveRDS(spatialHousePriceRF, file = 'data/Spatialfitv2.rds')

#print the model result
spatialHousePriceRF

# spatialHousePriceRF <- readRDS('data/Spatialfitv2.rds')

spatialHousePriceRF2 <- rf_tuning(
  model = spatialHousePriceRF,
  xy = xy, #location indicating coordinates of each neighborhood 
  repetitions = 5, #times the tuning process will run, such as K fold CV, here 5 means we used 5 folds
  num.trees = c(100, 1000), #the range within which the number of trees the model can select
  mtry = seq(
    2,
    length(spatialHousePriceRF$ranger.arguments$predictor.variable.names), #number of predictors
    by = 9),
  min.node.size = c(5, 15), #minimum rows the model can pick between 5 to 15
  seed = randon.seed,
  n.cores = 6, #used for faster calculation, check how many cores your laptop has before selecting the core numbers
  verbose = TRUE
)
spatialHousePriceRF2

saveRDS(spatialHousePriceRF2, file = 'data/Spatialtunev2.rds')
#spatialHousePriceRF2 <- readRDS('data/Spatialtunev2.rds')

#Repeat the spatial RF process 
spatialHousePriceRF2.repeat <- spatialRF::rf_repeat(
  model = spatialHousePriceRF2, 
  repetitions = 5,
  seed = randon.seed,
  verbose = TRUE
)

spatialHousePriceRF2.repeat

saveRDS(spatialHousePriceRF2.repeat, file = 'data/Spatialtunerepeatv2.rds')
#spatialHousePriceRF2.repeat <- readRDS('data/Spatialtunerepeatv2.rds')

VIPrep <- spatialRF::plot_importance(
  spatialHousePriceRF2.repeat, 
  verbose = TRUE
)
VIPrep

PDP1 <- spatialRF::plot_response_curves(
  spatialHousePriceRF2.repeat, 
  variables = predictors[0:6],
  quantiles = 0.5,
  ncol = 3,
  verbose = TRUE
) + 
  ggplot2::ggtitle("Spatial RF") 

PDP2 <- spatialRF::plot_response_curves(
  spatialHousePriceRF2.repeat, 
  variables = predictors[7:12],
  quantiles = 0.5,
  ncol = 3,
  verbose = TRUE
) + 
  ggplot2::ggtitle("Spatial RF")

PDP3 <- spatialRF::plot_response_curves(
  spatialHousePriceRF2.repeat, 
  variables = predictors[13:18],
  quantiles = 0.5,
  ncol = 3,
  verbose = TRUE
) + 
  ggplot2::ggtitle("Spatial RF")

PDP4 <- spatialRF::plot_response_curves(
  spatialHousePriceRF2.repeat, 
  variables = predictors[19:21],
  quantiles = 0.5,
  ncol = 3,
  verbose = TRUE
) + 
  ggplot2::ggtitle("Spatial RF")

PDP1
PDP2
PDP3
PDP4



