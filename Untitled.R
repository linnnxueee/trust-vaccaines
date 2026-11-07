install.packages("knitr")
library(knitr)

library(dplyr)
library(readr)
library(ggplot2)
ed_levels <- c(
  "Primary school",
  "Secondary school (age under 15 years old)",
  "GNVQ / GSVQ / GCSE/ SCE standard.",
  "NVQ1, NVQ2",
  "NVQ3/ SCE Higher Grade/ Advanced GNVQ/ GCE A/AS or similar.",
  "NVQ4 / HNC / HND / Bachelor's degree or similar.",
  "NVQ5 or post-graduate diploma."
)
like_levels <- c("Strongly disagree",
           "Tend to disagree",
           "Neither agree nor disagree",
           "Tend to agree",
           "Strongly agree",
           "Don\u2019t know")
d_trustvac <- read_csv(
  "../trust-vaccines.csv",
  col_types = cols(
    col_number(),
    col_factor(),
    col_factor(ordered = TRUE, levels = ed_levels),
    col_factor(ordered = TRUE, levels = like_levels),
    col_factor(ordered = TRUE, levels = like_levels),
    col_factor(ordered = TRUE, levels = like_levels),
    col_factor(ordered = TRUE, levels = like_levels),
    col_factor(ordered = TRUE, levels = like_levels),
    col_factor(ordered = TRUE, levels = like_levels),
    col_factor(ordered = TRUE, levels = like_levels),
    col_factor(ordered = TRUE, levels = like_levels),
    col_factor(ordered = TRUE, levels = like_levels)))

d_trustvac <- d_trustvac%>%
  mutate(
    govhonest_coded = govhonest %>% as.numeric)
)

d_trustvac%>%
  count(gender_nb)
##gender
d_trustvac %>%
  ggplot(aes(x = gender_nb)) +
  geom_bar()

##age
d_trustvac %>%
  ggplot(aes(x=age))+geom_histogram(binwidth = 5)

d_trustvac %>%
  summarise(n=n(),M= mean(age)%>%round(digits=2),
            
            SD=sd(age)%>%round(digits=2))

#high education level
d_trustvac %>%
  ggplot(aes(x=hhedqual))+
  geom_bar()+
  coord_flip()

#descriptive statistics
````{r}
as.numeric(d_trustvac$govhonest)
tibble(d_trustvac$govhonest, as.numeric(d_trustvac$govhonest))%>%
  kable()
```
```{r}
d_trustvac$govhonest %>%as.numeric %>% median(na.rm = TRUE)
  
```