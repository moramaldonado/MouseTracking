### Testing LDA on negation data
##### FOR CONTROLS
normalized_positions = controls %>%
  dplyr::select(Subject, Item.number, Condition, Response, X.Position,Y.Position, Velocity, Acceleration, RawTime) %>%
  separate(Y.Position, into= y, sep = ",") %>%
  separate(X.Position, into= x, sep = ",") %>%
  separate(Velocity, into= v, sep = ",") %>%
  separate(RawTime, into= t, sep = ",") %>%
  separate(Acceleration, into= a, sep = ",")

normalized_positions[y] <- sapply(normalized_positions[y],as.numeric)
normalized_positions[x] <- sapply(normalized_positions[x],as.numeric)
normalized_positions[v] <- sapply(normalized_positions[v],as.numeric)
normalized_positions[a] <- sapply(normalized_positions[a],as.numeric)
normalized_positions[t] <- sapply(normalized_positions[t],as.numeric)

# Taking the negative of false items, to have everything in the same scale
normalized_positions_false = normalized_positions%>%
  filter(Response=='false')%>%
  dplyr::mutate_at(vars(starts_with('x')), funs('-'))
normalized_positions_true = filter(normalized_positions, Response=='true')
normalized_positions = bind_rows(normalized_positions_false,normalized_positions_true)
rm(normalized_positions_true, normalized_positions_false)

#More about classes
normalized_positions$Subject <- factor(normalized_positions$Subject)
normalized_positions$Condition <- factor(normalized_positions$Condition)
normalized_positions$Response <- factor(normalized_positions$Response)

normalized_positions_control <- normalized_positions

