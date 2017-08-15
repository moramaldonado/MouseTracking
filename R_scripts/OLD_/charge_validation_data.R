data_validation <- read.csv(file="data_R/Data_validation.csv", header=TRUE, sep=",")
calibration_data_validation <- data_validation %>% 
  filter(Sentence_Type=='calibration')%>%
  mutate(Subject = as.factor(Subject)) 

### ORDERING DATA
x <- paste0('x', sprintf("%03d", c(1:101)))
y <- paste0('y', sprintf("%03d", c(1:101)))

# Each x and y coordenate into two columns (101 coordenates per trial) 
normalized_positions_validation = calibration_data_validation %>%
  dplyr::select(Subject, Item.number, Polarity, Expected_response, Normalized.positions.X,Normalized.positions.Y) %>%
  separate(Normalized.positions.Y, into= y, sep = ",") %>%
  separate(Normalized.positions.X, into= x, sep = ",")
normalized_positions_validation[y] <- sapply(normalized_positions_validation[y],as.numeric)
normalized_positions_validation[x] <- sapply(normalized_positions_validation[x],as.numeric)

# Taking the negative of false items, to have everything in the same scale
normalized_positions_false = normalized_positions_validation%>%
  filter(Expected_response=='false')%>%
  dplyr::mutate_at(vars(starts_with('x')), funs('-'))
normalized_positions_true = filter(normalized_positions_validation, Expected_response=='true')
normalized_positions_validation = bind_rows(normalized_positions_false,normalized_positions_true)
rm(normalized_positions_true, normalized_positions_false)

#last arrangements
normalized_positions_validation$Subject <- factor(normalized_positions_validation$Subject)
normalized_positions_validation$Polarity <- factor(normalized_positions_validation$Polarity)
normalized_positions_validation$Expected_response <- factor(normalized_positions_validation$Expected_response)
normalized_positions_validation$Deviation <- ifelse(normalized_positions_validation$Polarity == "deviated",
                                         "NonCentral", "Central")



# Deltas
for(i in 2:101)
{ 
  name_x <- x[i]
  name_y <- y[i]
  name_x_last <- x[i-1]
  name_y_last <- y[i-1]
  name_dx <- paste0(name_x,'_delta')
  name_dy <- paste0(name_y,'_delta')
  name_ddx <- paste0(name_x,'_ddelta')
  name_ddy <- paste0(name_y,'_ddelta')
  normalized_positions_validation[[name_dx]] <-  normalized_positions_validation[[name_x]] -
    normalized_positions_validation[[name_x_last]]
  normalized_positions_validation[[name_dy]] <-  normalized_positions_validation[[name_y]] -
    normalized_positions_validation[[name_y_last]]
  if (i > 2) {
    name_dx_last <- paste0(name_x_last, '_delta')
    name_dy_last <- paste0(name_y_last, '_delta')
    normalized_positions_validation[[name_ddx]] <-  normalized_positions_validation[[name_dx]] -
      normalized_positions_validation[[name_dx_last]]
    normalized_positions_validation[[name_ddy]] <-  normalized_positions_validation[[name_dy]] -
      normalized_positions_validation[[name_dy_last]]
  }
}

