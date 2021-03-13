## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load, warning=FALSE, message=FALSE---------------------------------------
library(NHSRdatasets)
library(dplyr)
library(ggplot2)
library(caret)
library(rsample)
library(varhandle)

data("stranded_data")
glimpse(stranded_data)
prop.table(table(stranded_data$stranded.label))

## ----feature_engineering------------------------------------------------------

stranded_data <- stranded_data %>% 
  dplyr::mutate(stranded.label=factor(stranded.label)) %>% 
  dplyr::select(everything(), -c(admit_date))



## ----dummy_encode-------------------------------------------------------------
cats <- select_if(stranded_data, is.character)
cat_dummy <- varhandle::to.dummy(cats$frailty_index, "frail_ind") 
#Converts the frailty index column to dummy encoding and sets a column called "frail_ind" prefix
cat_dummy <- cat_dummy %>% 
  as.data.frame() %>% 
  dplyr::select(-frail_ind.No_index_item) #Drop the field of interest
# Drop the frailty index from the stranded data frame and bind on our new encoding categorical variables
stranded_data <- stranded_data %>% 
  dplyr::select(-frailty_index) %>% 
  bind_cols(cat_dummy) %>% na.omit(.)


## ----train_test_split---------------------------------------------------------
split <- rsample::initial_split(stranded_data, prop = 3/4)
train <- rsample::training(split)
test <- rsample::testing(split)


## ----class_model--------------------------------------------------------------
set.seed(123)
glm_class_mod <- caret::train(factor(stranded.label) ~ ., data = train, 
                 method = "glm")
print(glm_class_mod)


## ----predicting---------------------------------------------------------------
preds <- predict(glm_class_mod, newdata = test) # Predict class
pred_prob <- predict(glm_class_mod, newdata = test, type="prob") #Predict probs

# Join prediction on to actual test data frame and evaluate in confusion matrix

predicted <- data.frame(preds, pred_prob)
test <- test %>% 
  bind_cols(predicted) %>% 
  dplyr::rename(pred_class=preds)

glimpse(test)

## ----evaluation---------------------------------------------------------------

caret::confusionMatrix(test$stranded.label, test$pred_class, positive="Stranded")


