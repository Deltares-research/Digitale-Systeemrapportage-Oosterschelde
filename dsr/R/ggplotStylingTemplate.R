ggplot(dataset, aes(x=, y=)) +                                        # Call Plot
  geom_point(colour="gray") +                                                    # Set as a point plot
  geom_point(data=dataset[1:5,], pch=16, colour="blueviolet") +                   # Color Specific Points
  geom_point(data=dataset[9:13,], pch=16, colour="darksalmon") +
  geom_smooth(method = loess,                                                    # Apply Smoothing Line
              color = "deepskyblue4",                                            #   Line Color
              fill = "lightblue1",                                               #   Standard Error Fill
              linetype = "dashed",                                               #   Line Type
              se = TRUE) +                                                       #   Standard Error Add
  ggtitle(expression(atop(bold("Scatter Plot: Cereal"),                          # Title w/ Sub-Title
                          atop(italic("Sugar & Rating Correlation"),"")))) +
  xlab("Rating") +                                                               # X Axis Label
  ylab("Sugars") +                                                               # Y Axis Label
  theme_bw() +                                                                   # Black and White Theme
  theme(plot.title = element_text(hjust=0.5, vjust=-0.4, size=10),               # Centers Plot Title
        panel.grid.major = element_blank(),                                      # Remove Grid Lines
        panel.grid.minor = element_blank(),                                      # Remove Grid Lines
        panel.border = element_rect(colour="lightgoldenrod"),                    # Change Panel Border
        axis.title.x = element_text(colour="firebrick", size=8, face="bold"),    # X Axis Title
        axis.title.y = element_text(colour="firebrick", size=8, face="bold"),    # Y Axis Title
        axis.ticks.x = element_line(colour="black", size=1),                     # X Axis Tick Marks
        axis.ticks.y = element_line(colour="black", size=1),                     # Y Axis Tick Marks
        axis.text.x = element_text(colour="darkorange", size=8, face="bold"),    # X Axis Labels
        axis.text.y = element_text(colour="darkorange", size=8, face="bold")) # Y Axis Labels