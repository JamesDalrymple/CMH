graph <- list()

graph$benzo <- ggplot(data = agg$benzo$provider,
  aes(x = provider, y = num_Benzo_prescribed, ymax = 1.1*num_Benzo_prescribed))+
  geom_bar(stat = "identity", fill = "lightblue", color = "black", width = 1)+
  # my_theme+theme(axis.title.x = element_text(colour = "grey30"),
  #                axis.title.y = element_text(colour = "grey30") )+
  coord_flip()+
  labs(title="Number of Benzos Currently Prescribed",
       x="staff", y="number of prescriptions")+
  geom_text(data = agg$benzo$provider, aes(x = provider,
    y = num_Benzo_prescribed, label = num_Benzo_prescribed),
    hjust = -.1, size = 3)
# ggsave(filename="benzo barplot.png", plot=p_benzo, path=file.path(baseWD, resultsWD), width=5, height=5, units="in")

graph$stim <- ggplot(data = agg$stim$provider, aes(x = provider,
  y = num_Stimulants_prescribed, ymax = 1.1*num_Stimulants_prescribed)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black", width = 1) +
  # my_theme+theme(axis.title.x = element_text(colour = "grey30"),
  #                axis.title.y = element_text(colour = "grey30") )+
  coord_flip()+
  labs(title = "Number of Stimulants Currently Prescribed", x = "staff",
    y = "number of prescriptions") +
  geom_text(data = agg$stim$provider, aes(x = provider,
    y = num_Stimulants_prescribed, label = num_Stimulants_prescribed),
    hjust = -.1, size = 3)
# ggsave(filename="stimulants barplot.png", plot=p_stim, path=file.path(baseWD, resultsWD), width=5, height=4, units="in")

graph$benzo_stim <- ggplot(data = agg$benzo_stim$provider, aes(x = provider,
  y = num_benzo_stim_prescribed, ymax = 1.1*num_benzo_stim_prescribed))+
  geom_bar(stat = "identity", fill = "lightblue", color = "black", width = 1)+
  # my_theme+theme(axis.title.x = element_text(colour = "grey30"),
  #                axis.title.y = element_text(colour = "grey30") )+
  coord_flip()+
  labs(title = "Number of Benzos and Stimulants Currently Prescribed",
       x = "staff", y = "number of prescriptions")+
  geom_text(data = agg$benzo_stim$provider, aes(x=provider, y=num_benzo_stim_prescribed,
    label=num_benzo_stim_prescribed), hjust = -.1, size = 3)
# ggsave(filename="benzo_stim barplot.png", plot=p_benzo_stim, path=file.path(baseWD, resultsWD), width=5, height=3, units="in")
