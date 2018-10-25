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
