---
title: "Population Pyramid Animation"
output: 
  html_document: 
    keep_md: yes
---



## Objectives 

* Procure, clean, analyze the publically available dataset
* Create a visualization to display the data graphically
* Animate the plot to show changes over time


##### Install packages/load libraries

```r
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, ggpol, gganimate, gifski, kableExtra)
```

* **pacman**: Load all the packages at once
* **tidyverse:** Prepare the dataframe (tidyr), manipulate the dataframe with ease (dplyr, stringr), produce plots (ggplot2)
* **ggpol:** Additional features for ggplot2
* **gganimate:** Animation of graphics
* **gifski:** To render files as a GIF
* **kableExtra:** Create visually appealing tables
   
   

##### Import .csv file

```r
CAPop <- read.csv('CAPop.csv')
```

##### Manipulate the dataframe

```r
CAPop <- CAPop %>% 
  # Remove NA values at the bottom of the dataframe
  na.omit %>% 
  # Create column for age groups
  mutate(Age = substr(Series.Name, 17, 21),
         # Change the value for last Age group
         Age = as.factor(case_when(Age == '80 an' ~ '80+', TRUE ~ Age)),
         # Greate a column for Gender
         Gender = as.factor(ifelse(str_detect(Series.Name, 'female$') == TRUE, 'Female', 'Male')),
         # Reorder the factor levels (important for the order data is displayed)
         Gender = factor(Gender, levels = c('Male', 'Female'))) %>% 
  # Remove unused columns
  select(-Country.Name, - Series.Name) %>% 
  # Change from wide to long format
  gather(Year, Pop, X1960:X2018) %>% 
  # Remove prefix from the year
  mutate(Year = as.integer(substr(Year, 2, 5)),
         # Change all Male Pop values to negative for the population pyramid
         Pop = ifelse(Gender == 'Male', as.integer(Pop * -1), as.integer(Pop)))
```


```r
dim(CAPop)
```

```
## [1] 2006    4
```

There are 2006 records and 5 attributes. Lets take a look at the first 6 records to get a sense of what is in the data after manipulation.





```r
kable(head(CAPop)) %>%
  kable_styling(bootstrap_options = 'striped', font_size = 12, full_width = FALSE, position = 'center')
```

<table class="table table-striped" style="font-size: 12px; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Age </th>
   <th style="text-align:left;"> Gender </th>
   <th style="text-align:right;"> Year </th>
   <th style="text-align:right;"> Pop </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 00-04 </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 1960 </td>
   <td style="text-align:right;"> 1086588 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 00-04 </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:right;"> 1960 </td>
   <td style="text-align:right;"> -1137336 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05-09 </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 1960 </td>
   <td style="text-align:right;"> 996613 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05-09 </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:right;"> 1960 </td>
   <td style="text-align:right;"> -1041083 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10-14 </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 1960 </td>
   <td style="text-align:right;"> 869802 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10-14 </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:right;"> 1960 </td>
   <td style="text-align:right;"> -908790 </td>
  </tr>
</tbody>
</table>

We can create the **animation** in **3** distinct steps:
1. Create the static plot
2. Apply animation parameters
3. Render to file


  
  

#### Static Plot

```r
PopPyramid <- CAPop %>%
  ggplot(aes(
             x = Age,
             y = Pop/1000,
             fill = Gender)
        ) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('#4682b4', '#ee7989')) + 
  facet_share(
              ~ Gender,
              dir = 'h',
              scales = 'free',
              reverse_num = TRUE
              ) +
  labs(
       title = 'Canada Population Estimate 1960 - 2018\n\n{closest_state}',
       subtitle = '\n\nAge Group',
       y = '\n\nPopulation (in thousands)',
       caption  = '\n\nData Source: https://databank.worldbank.org'
       ) + 
  coord_flip() +
  theme(
    plot.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),                                  
    plot.tag = element_text(),
    axis.text = element_text(size = 14),
    legend.key.size = unit(0.75, 'cm'),
    legend.text = element_text(
                               size = 15,
                               face = 'bold'
                               ),
    plot.title = element_text(
                              size = 22,
                              hjust = 0.5,
                              face = 'bold'
                              ),
    plot.subtitle = element_text(
                                 size = 14,
                                 hjust = 0.5,
                                 face = 'bold'
                                 ),
    axis.title.x = element_text(
                                size = 16,
                                face = 'bold'
                                ),
    plot.caption = element_text(
                                size = 12,
                                hjust = 0.5,
                                face = 'italic',
                                color = 'gray'
                                )
  )
```

##### Animation Parameters

```r
PopPyramid + 
  transition_states(
                    Year,
                    transition_length = 1,
                    state_length = 2
                    ) + 
  enter_fade() +
  exit_fade() + 
  ease_aes('cubic-in-out')
```

##### Render to GIF

```r
animate(
        PopPyramid,
        fps = 24,
        duration = 45,
        width = 500,
        height = 500,
        renderer = gifski_renderer('PopPyramid.gif')
        )
```
  
  
<p align = 'center'>
<img src = 'https://github.com/SiphuLangeni/Population-Pyramid-Animation/blob/master/Figs/PopPyramid.gif' />
</p>
