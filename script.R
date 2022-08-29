
# read data
setwd(q3_directory)
q3_spdf<- readOGR(dsn="wireless-hotspots-geojson.geojson",layer="wireless-hotspots-geojson")
setwd(working_directory)

#################################################################################################################
# task1
# From the table, what are some of the information you can deduce for each hotspot? 
# copy out data to dataframe
q3_data<-data.frame(q3_spdf@data,stringsAsFactors = TRUE)
#inspect map
mapview(q3_spdf)

# regex cleaning
q3_data$Y<-substring(q3_data$Description, regexpr("<th>Y</th> <td>", q3_data$Description)+15,regexpr("<th>X</th>", q3_data$Description)-28)
q3_data$X<-substring(q3_data$Description, regexpr("<th>X</th> <td>", q3_data$Description)+15,regexpr("<th>LOCATION_NAME</th>", q3_data$Description)-35)
q3_data$LOCATION_NAME<-substring(q3_data$Description, regexpr("<th>LOCATION_NAME</th> <td>", q3_data$Description)+27,regexpr("<th>LOCATION_TYPE</th>", q3_data$Description)-28)
q3_data$LOCATION_TYPE<-substring(q3_data$Description, regexpr("<th>LOCATION_TYPE</th> <td>", q3_data$Description)+27,regexpr("<th>POSTAL_CODE</th>", q3_data$Description)-35)
q3_data$POSTAL_CODE<-substring(q3_data$Description, regexpr("<th>POSTAL_CODE</th> <td>", q3_data$Description)+25,regexpr("<th>STREET_ADDRESS</th>", q3_data$Description)-28)
q3_data$STREET_ADDRESS<-substring(q3_data$Description, regexpr("<th>STREET_ADDRESS</th> <td>", q3_data$Description)+28,regexpr("<th>OPERATOR_NAME</th>", q3_data$Description)-35)
q3_data$OPERATOR_NAME<-substring(q3_data$Description, regexpr("<th>OPERATOR_NAME</th> <td>", q3_data$Description)+27,regexpr("<th>INC_CRC</th>", q3_data$Description)-28)

print(head(q3_data))
#################################################################################################################
# task2
# Using all earlier rows as well as all other columns in the dataset, build a classification model to predict the location type for these hotspots. 
# create garbled data, last 200 rows NA location data

q3_data$LOCATION_TYPE[q3_data$LOCATION_TYPE%in%c("Home/Centre Cares","Public Worship","Residential")]<-"Others"
q3_data_garbled<-q3_data
q3_data_garbled$LOCATION_TYPE[(length(q3_data_garbled$LOCATION_TYPE)-199):length(q3_data_garbled$LOCATION_TYPE)]<-list(NULL)
q3_data_garbled$LOCATION_TYPE<-factor(as.character(q3_data_garbled$LOCATION_TYPE))

# it is observed that location type is most related with location name
# train set
text_df <- tibble(line = 1:(nrow(q3_data_garbled)), text = q3_data_garbled$LOCATION_NAME)
text_df<-text_df %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  filter(!str_detect(word, "^[0-9]*$"))

###
dtm_df<-text_df%>%
  dplyr::count(line,word)%>%
  cast_dtm(document = line,term=word,value=n,weighting = tm::weightTfIdf)


# train test split
train.vector <- seq(1,1404)

text_train.dtm <- dtm_df[dtm_df$dimnames$Docs %in% train.vector,]
text_test.dtm <- dtm_df[!(dtm_df$dimnames$Docs %in% train.vector),]

# model randomforest
model_rf <- train(x = as.matrix(text_train.dtm ),
                     y = factor(q3_data_garbled$LOCATION_TYPE[1:(length(q3_data_garbled$LOCATION_TYPE)-200)]),
                     method = "ranger",
                     num.trees = 200,
                     importance="impurity",
                     trControl = trainControl(method = "oob"))

model_rf$finalModel%>%
  ranger::importance()%>%
  tibble::enframe(name = "variable", value = "varimp") %>%
  top_n(n = 20, wt = varimp) %>%
  # plot the metrics
  ggplot(aes(x = forcats::fct_reorder(variable, varimp), y = varimp)) +
  geom_col() +
  coord_flip() +
  labs(x = "Token",
       y = "Variable importance (higher is more important)")
#################################################################################################################
# task3
# Compared to the true location types, how good was your model?
a<-predict(model_rf,as.matrix(text_test.dtm))
confusionMatrix(factor(a),factor(q3_data$LOCATION_TYPE[(length(q3_data_garbled$LOCATION_TYPE)-199):length(q3_data_garbled$LOCATION_TYPE)]))
