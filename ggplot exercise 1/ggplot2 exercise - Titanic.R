# Titanic data is not available, sorry!

# Plot the distribution of the sexes within the classes of the ship.
ggplot(titanic, aes(x = Pclass, fill = Sex)) + 
  geom_bar(position = "dodge")

# Add facet_grid() layer to estimate chances of survival.
ggplot(titanic, aes(x = Pclass, fill = Sex)) + 
  geom_bar(position = "dodge") +
  facet_grid(. ~ Survived)

# Define an object for position jitterdodge.
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# Add age variable; change plot type accordingly.
ggplot(titanic, aes(x = Pclass, y=Age, col = Sex)) + 
  geom_point(size=3, alpha=0.5, position=posn.jd) +
  facet_grid(. ~ Survived)