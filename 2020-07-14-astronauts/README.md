Astronaut database
================

## TidyTuesday \[2020-07-14\]

``` r
library(tidytuesdayR)
library(tidyverse)
```

A quick look at the data set

``` r
d <- tidytuesdayR::tt_load("2020-07-14")[[1]]
#> --- Compiling #TidyTuesday Information for 2020-07-14 ----
#> --- There is 1 file available ---
#> --- Starting Download ---
#> 
#>  Downloading file 1 of 1: `astronauts.csv`
#> --- Download complete ---
```

``` r
glimpse(d)
#> Rows: 1,277
#> Columns: 24
#> $ id                       <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,...
#> $ number                   <dbl> 1, 2, 3, 3, 4, 5, 5, 6, 6, 7, 7, 7, 8, 8, ...
#> $ nationwide_number        <dbl> 1, 2, 1, 1, 2, 2, 2, 4, 4, 3, 3, 3, 4, 4, ...
#> $ name                     <chr> "Gagarin, Yuri", "Titov, Gherman", "Glenn,...
#> $ original_name            <chr> "<U+0413><U+0410><U+0413><U+0410><U+0420><U+0418><U+041D> <U+042E><U+0440><U+0438><U+0439> <U+0410><U+043B><U+0435><U+043A><U+0441><U+0435><U+0435><U+0432><U+0438><U+0447>", "<U+0422><U+0418><U+0422><U+041E><U+0412> <U+0413><U+0435><U+0440><U+043C><U+0430><U+043D> <U+0421>...
#> $ sex                      <chr> "male", "male", "male", "male", "male", "m...
#> $ year_of_birth            <dbl> 1934, 1935, 1921, 1921, 1925, 1929, 1929, ...
#> $ nationality              <chr> "U.S.S.R/Russia", "U.S.S.R/Russia", "U.S."...
#> $ military_civilian        <chr> "military", "military", "military", "milit...
#> $ selection                <chr> "TsPK-1", "TsPK-1", "NASA Astronaut Group ...
#> $ year_of_selection        <dbl> 1960, 1960, 1959, 1959, 1959, 1960, 1960, ...
#> $ mission_number           <dbl> 1, 1, 1, 2, 1, 1, 2, 1, 2, 1, 2, 3, 1, 2, ...
#> $ total_number_of_missions <dbl> 1, 1, 2, 2, 1, 2, 2, 2, 2, 3, 3, 3, 2, 2, ...
#> $ occupation               <chr> "pilot", "pilot", "pilot", "PSP", "Pilot",...
#> $ year_of_mission          <dbl> 1961, 1961, 1962, 1998, 1962, 1962, 1970, ...
#> $ mission_title            <chr> "Vostok 1", "Vostok 2", "MA-6", "STS-95", ...
#> $ ascend_shuttle           <chr> "Vostok 1", "Vostok 2", "MA-6", "STS-95", ...
#> $ in_orbit                 <chr> "Vostok 2", "Vostok 2", "MA-6", "STS-95", ...
#> $ descend_shuttle          <chr> "Vostok 3", "Vostok 2", "MA-6", "STS-95", ...
#> $ hours_mission            <dbl> 1.77, 25.00, 5.00, 213.00, 5.00, 94.00, 42...
#> $ total_hrs_sum            <dbl> 1.77, 25.30, 218.00, 218.00, 5.00, 519.33,...
#> $ field21                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
#> $ eva_hrs_mission          <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, ...
#> $ total_eva_hrs            <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, ...
```

First thought, what is the difference in time between when an astronaut
is first selected and when they go on their first mission? How does it
vary by nationality?

``` r
d %>% 
  filter(mission_number == 1) %>% # first mission
  mutate(age_at_selection = year_of_selection - year_of_birth,
         age_at_first_mission = year_of_mission - year_of_birth,
         diff = age_at_first_mission - age_at_selection) %>% 
  group_by(nationality) %>% 
  filter(n() > 20) %>% # Select high count nationalities
  gather(k, v, age_at_selection, age_at_first_mission, diff) %>% 
  mutate(k_pretty = case_when(k == "age_at_selection" ~ "Age at selection year",
                              k == "age_at_first_mission" ~ "Age at first mission",
                              k == "diff" ~ "Difference",
                              TRUE ~ "Error")) %>% 
  ggplot()+
  geom_histogram(aes(v, ..density.., fill=k_pretty), 
                 position = "identity", 
                 alpha=1/2, binwidth = 1, center=0)+
  facet_wrap(~nationality, scales = "free_y", ncol=1)+
  scale_fill_viridis_d("")+
  scale_x_continuous(breaks=scales::pretty_breaks(10))+
  theme(legend.position = "bottom")+
  labs(title = "First mission - distribution of age at selection-year and age at mission-year",
       subtitle = "Facetted by the two biggest astronaut nationalities",
       x = "Age [years]",
       y = "Density estimate")
```

<img src="README_files/figure-gfm/unnamed-chunk-5-1.png" width="100%" />
