normalized_positions.plot.Y = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.Y, Item.number) %>%
  separate(Normalized.positions.Y, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Y.Position, 4:104) %>%
  mutate(Y.Position = as.numeric(Y.Position))%>%
  mutate(Item.number = as.factor(Item.number))

normalized_positions.plot.X = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.X, Item.number) %>%
  separate(Normalized.positions.X, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, X.Position, 4:104) %>%
  mutate(X.Position = as.numeric(X.Position))%>%
  mutate(Item.number = as.factor(Item.number))

normalized_positions.plot_raw <- merge(normalized_positions.plot.X,normalized_positions.plot.Y)
normalized_positions.plot_false_raw <- normalized_positions.plot_raw%>%
  filter(Expected_response=='false')

summary(normalized_positions.plot_raw)

# Taking the negative of false items, to have everything in the same scale
normalized_positions.plot_false <- normalized_positions.plot_raw%>%
  filter(Expected_response=='false')%>%
  dplyr::mutate_at('X.Position', funs('-'))

normalized_positions.plot_true = filter(normalized_positions.plot_raw, Expected_response=='true')
normalized_positions.plot = bind_rows(normalized_positions.plot_false,normalized_positions.plot_true)

summary(normalized_positions.plot)
