#bostonMap <- Map of Boston imported from Google Maps 
bostonMap <- qmap("boston", zoom = 15)
#crime_MV_acc is a collection of the M/V accidents reported by the BPD filtered with the help of corresponding crime codes using rms_offense_code 
crime_MV_acc <-
crime %>% filter(
OFFENSE_CODE %in% c(
3810,
3820,
3801,
3807,
3803,
3805,
3802,
3205,
3811,
3820,
3821,
3830,
3831,
3706,
3702,
3704,
3701,
3205,
3206
)
)


```

```{r code3, echo=TRUE}
boston <-
  get_map(location = "boston", zoom = 12) ##Get the boston map
  bostonMap <- ggmap(boston, extent = "device")       ##Prepare Map
  
  bostonMap +
  stat_density2d(
  aes(
  x = crime_MV_acc$Long,
  y = crime_MV_acc$Lat,
  fill = ..level..,
  alpha = ..level..
  ),
  bins = 10,
  geom = "polygon",
  data = crime_MV_acc
  )  +
  scale_fill_gradient(low = "grey", high = "red") +
  ggtitle("Density of M/V accidents in Boston") +
  theme(
  panel.background = element_rect(fill = 'grey75'),
  plot.background = element_rect(fill = 'grey')
  )
  
```



```{r code5,echo=T}
#group M/v accidents by the day of the week and store in summmary_week
summary_week <-
  crime_MV_acc %>% group_by(DAY_OF_WEEK) %>% summarise(total_incidents = n()) %>%
  arrange(-total_incidents)

  ggplot(data = summary_week) + geom_bar(
  aes(
  x = summary_week$DAY_OF_WEEK,
  y = summary_week$total_incidents,
  fill = summary_week$DAY_OF_WEEK
  ),
  stat = "identity"
  ) +
  ggtitle("Variation of Motor vehicle accidents by Days") +
  labs(x = "Day of the week", y = "Number of Accidents reported") +
  guides(fill = guide_legend("Day")) +
  theme(
  panel.background = element_rect(fill = 'grey75'),
  plot.background = element_rect(fill = 'grey')
  )
  
```





```{r code6,echo=TRUE}
#to plot variation of number of accidents by the time of the day
crime_MV_acc %>%
group_by(HOUR) %>%
summarise(total_incidents = n()) %>%
ggplot(aes(x = HOUR, y = total_incidents)) +
geom_line(color = "red", size = 2) +
ggtitle("Variation of M/V accidents by the hours of the day") +
labs(x = "Hours of the Day", y = "Number of reported M/V accidents") +
theme(
panel.background = element_rect(fill = 'grey75'),
plot.background = element_rect(fill = 'grey')
)
```


```{r code7,echo=TRUE}
#group by offense_description and roll up the data
offense_summary <- crime_MV_acc %>%
  group_by(OFFENSE_DESCRIPTION) %>%
  summarise(total_incidents = n())

  offense_summary %>%
  ggplot() +
  geom_bar(
  aes(
  x = offense_summary$OFFENSE_DESCRIPTION,
  y = offense_summary$total_incidents,
  fill = offense_summary$OFFENSE_DESCRIPTION
  ),
  stat = "identity"
  ) +
  theme(axis.text.x = element_blank()) +
  ggtitle("Accidents involving M/V in Boston") +
  labs(x = "Offense description", y = "Count of reported cases") +
  theme(
  panel.background = element_rect(fill = 'grey75'),
  plot.background = element_rect(fill = 'grey'),
  legend.background = element_rect(color = 'grey')
  ) +
  guides(fill = guide_legend(title = 'Offense Description'))
  
 offense_summary <- offense_summary%>%mutate(percent=total_incidents/sum(total_incidents)*100)
                                            
```
