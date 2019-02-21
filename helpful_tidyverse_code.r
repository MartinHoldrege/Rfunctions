#Martin Holdrege
#helpful tidyverse code
#helpful code from the R for data science books
library(tidyverse)
#dplyr
select(flights,-c(year,day))#remove columns by name
select(flights,month:hour)#selecting range of columns
512%/%10; 512%%10 #modular arithmetic
flights
transmute(flights,
          hr=dep_time%/%100,
          min=dep_time%%100)
#group_by(); followed by summarize()
count()
n()
flights %>% 
    group_by(carrier) %>% 
    summarise(mean_del=mean(arr_delay,na.rm=T)) %>% 
    arrange(desc(mean_del))
diamonds %>% 
    count(cut_width(carat,0.5))

#tibbles
print(flights,n=10,width=Inf)

#
mtcars %>% split(.$cyl) # make list based on a variable

#
seq_range() # vector of n evenly spaced values from min to max of vector

by_country <- gapminder %>%
    group_by(country, continent) %>% 
    nest() # data frame of lists
#~~~
label <- tibble(
    displ = Inf,
    hwy = Inf,
    cyl = 5,
    label = "label for just one facet"
)

ggplot(mpg, aes(displ, hwy))+
    geom_point()+
    facet_wrap(~cyl)+
    geom_text(
        aes(label = label),
        data = label,
        vjust = "top",
        hjust = "right"
    )
#~~~~

ggplot(aes(-depth, vwc_sub_inc,  shape = trmt)) +
  geom_point()+
  geom_line()+
  labs(y = "water flux (% vwc/month)",
       x = "Soil depth (cm)") +
  scale_y_continuous(position = "right") +
  coord_flip(xlim = c(-100, 0)) +
  guides(shape = guide_legend(title = "",
                              override.aes = list(alpha = 1, size = 1))) +
  scale_x_continuous(breaks = seq(from = -100, to = 0, by = 25),
                     labels = seq(100, 0, by = -25)) +
  scale_shape_manual(values = c(1, 2),
                     labels = expression(control,
                                         4~mm~treatment~(3~degree*C))) +                
  theme(legend.text.align = 0, 
        legend.position = c(.62,.3), 
        text = element_text(size = 11),
        rect = element_rect(fill = "white", size = 0.7, color = "black"),
        axis.text = element_text(size = 9),
        axis.line = element_blank(), 
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(vjust = 0))
