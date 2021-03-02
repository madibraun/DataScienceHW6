---
title: 'Weekly Exercises #6'
author: "Madeline Braun"
output: 
  html_document:
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
    df_print: paged
    code_download: true
---





```r
library(tidyverse)     # for data cleaning and plotting
library(gardenR)       # for Lisa's garden data
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(palmerpenguins)# for Palmer penguin data
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(leaflet)       # for highly customizable mapping
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(gganimate)     # for adding animation layers to ggplots
library(gifski)        # for creating the gif (don't need to load this library every time,but need it installed)
library(transformr)    # for "tweening" (gganimate)
library(shiny)         # for creating interactive apps
library(patchwork)     # for nicely combining ggplot2 graphs  
library(gt)            # for creating nice tables
library(rvest)         # for scraping data
library(robotstxt)     # for checking if you can scrape data
library(janitor)
theme_set(theme_minimal())
```


```r
# Lisa's garden data
data("garden_harvest")

#COVID-19 data from the New York Times
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
```

## Put your homework on GitHub!

Go [here](https://github.com/llendway/github_for_collaboration/blob/master/github_for_collaboration.md) or to previous homework to remind yourself how to get set up. 

Once your repository is created, you should always open your **project** rather than just opening an .Rmd file. You can do that by either clicking on the .Rproj file in your repository folder on your computer. Or, by going to the upper right hand corner in R Studio and clicking the arrow next to where it says Project: (None). You should see your project come up in that list if you've used it recently. You could also go to File --> Open Project and navigate to your .Rproj file. 

## Instructions

* Put your name at the top of the document. 

* **For ALL graphs, you should include appropriate labels.** 

* Feel free to change the default theme, which I currently have set to `theme_minimal()`. 

* Use good coding practice. Read the short sections on good code with [pipes](https://style.tidyverse.org/pipes.html) and [ggplot2](https://style.tidyverse.org/ggplot2.html). **This is part of your grade!**

* **NEW!!** With animated graphs, add `eval=FALSE` to the code chunk that creates the animation and saves it using `anim_save()`. Add another code chunk to reread the gif back into the file. See the [tutorial](https://animation-and-interactivity-in-r.netlify.app/) for help. 

* When you are finished with ALL the exercises, uncomment the options at the top so your document looks nicer. Don't do it before then, or else you might miss some important warnings and messages.

## Your first `shiny` app 

  1. This app will also use the COVID data. Make sure you load that data and all the libraries you need in the `app.R` file you create. Below, you will post a link to the app that you publish on shinyapps.io. You will create an app to compare states' cumulative number of COVID cases over time. The x-axis will be number of days since 20+ cases and the y-axis will be cumulative cases on the log scale (`scale_y_log10()`). We use number of days since 20+ cases on the x-axis so we can make better comparisons of the curve trajectories. You will have an input box where the user can choose which states to compare (`selectInput()`) and have a submit button to click once the user has chosen all states they're interested in comparing. The graph should display a different line for each state, with labels either on the graph or in a legend. Color can be used if needed. 
  
Link: [Click Here!](https://m-braun.shinyapps.io/hw6_app/)
  
## Warm-up exercises from tutorial

  2. Read in the fake garden harvest data. Find the data [here](https://github.com/llendway/scraping_etc/blob/main/2020_harvest.csv) and click on the `Raw` button to get a direct link to the data. 
  

```r
garden_fake <- read_csv("https://raw.githubusercontent.com/llendway/scraping_etc/main/2020_harvest.csv")
```
  
  
  3. Read in this [data](https://www.kaggle.com/heeraldedhia/groceries-dataset) from the kaggle website. You will need to download the data first. Save it to your project/repo folder. Do some quick checks of the data to assure it has been read in appropriately.


```r
groceries <- read_csv("Groceries_dataset.csv")
```


```r
head(groceries, 3)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Member_number"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Date"],"name":[2],"type":["chr"],"align":["left"]},{"label":["itemDescription"],"name":[3],"type":["chr"],"align":["left"]}],"data":[{"1":"1808","2":"21-07-2015","3":"tropical fruit"},{"1":"2552","2":"05-01-2015","3":"whole milk"},{"1":"2300","2":"19-09-2015","3":"pip fruit"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


  4. CHALLENGE(not graded): Write code to replicate the table shown below (open the .html file to see it) created from the `garden_harvest` data as best as you can. When you get to coloring the cells, I used the following line of code for the `colors` argument:
  

```r
colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::YlGn"
      ) %>% as.character()
```



```r
garden_clean <- garden_fake %>% 
  select(-`This is my awesome data!`) %>% 
  row_to_names(row_number = 2) %>% 
  filter(vegetable %in% c("beans", "carrots", "tomatoes")) %>% 
  mutate(date = as.Date(date, "%m/%d/%y"), 
         month = month(date, label = TRUE),
         weight = ifelse(weight == "-", 0, weight)) %>% 
  group_by(vegetable, variety, month) %>% 
  summarise(weight = sum(as.numeric(weight)*0.00220462)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = month, values_from = weight) %>% 
  replace_na(list(Jul = 0, Aug = 0, Sep = 0, Oct = 0))
  
  
garden_clean %>% 
  gt(rowname_col = "variety",
     groupname_col = "vegetable") %>% 
  data_color(
    columns = vars(Jul, Aug, Sep, Oct),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::YlGn") %>% 
        as.character(),
      domain = NULL
  ))
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#cdtxuaukky .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#cdtxuaukky .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cdtxuaukky .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#cdtxuaukky .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#cdtxuaukky .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cdtxuaukky .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cdtxuaukky .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#cdtxuaukky .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#cdtxuaukky .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#cdtxuaukky .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#cdtxuaukky .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#cdtxuaukky .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#cdtxuaukky .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#cdtxuaukky .gt_from_md > :first-child {
  margin-top: 0;
}

#cdtxuaukky .gt_from_md > :last-child {
  margin-bottom: 0;
}

#cdtxuaukky .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#cdtxuaukky .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#cdtxuaukky .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cdtxuaukky .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#cdtxuaukky .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cdtxuaukky .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#cdtxuaukky .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#cdtxuaukky .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cdtxuaukky .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cdtxuaukky .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#cdtxuaukky .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cdtxuaukky .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#cdtxuaukky .gt_left {
  text-align: left;
}

#cdtxuaukky .gt_center {
  text-align: center;
}

#cdtxuaukky .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#cdtxuaukky .gt_font_normal {
  font-weight: normal;
}

#cdtxuaukky .gt_font_bold {
  font-weight: bold;
}

#cdtxuaukky .gt_font_italic {
  font-style: italic;
}

#cdtxuaukky .gt_super {
  font-size: 65%;
}

#cdtxuaukky .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="cdtxuaukky" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Jul</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Aug</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Sep</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Oct</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">beans</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Bush Bush Slender</td>
      <td class="gt_row gt_right" style="background-color: #004529; color: #FFFFFF;">12.3679182</td>
      <td class="gt_row gt_right" style="background-color: #CBEA9C; color: #000000;">9.0367374</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE2; color: #000000;">0.2072343</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Chinese Red Noodle</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
      <td class="gt_row gt_right" style="background-color: #FEFEDC; color: #000000;">0.7848447</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Classic Slenderette</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
      <td class="gt_row gt_right" style="background-color: #F9FDC2; color: #000000;">3.0732403</td>
      <td class="gt_row gt_right" style="background-color: #FEFFDE; color: #000000;">0.5313134</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">carrots</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Bolero</td>
      <td class="gt_row gt_right" style="background-color: #FEFFDE; color: #000000;">0.2557359</td>
      <td class="gt_row gt_right" style="background-color: #F8FCBF; color: #000000;">3.3686594</td>
      <td class="gt_row gt_right" style="background-color: #FEFFE0; color: #000000;">0.3703762</td>
      <td class="gt_row gt_right" style="background-color: #75C477; color: #000000;">0.9898744</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Dragon</td>
      <td class="gt_row gt_right" style="background-color: #FEFFE0; color: #000000;">0.1763696</td>
      <td class="gt_row gt_right" style="background-color: #FBFDCF; color: #000000;">1.9819534</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
      <td class="gt_row gt_right" style="background-color: #004529; color: #FFFFFF;">1.9466795</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">greens</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
      <td class="gt_row gt_right" style="background-color: #FEFFE1; color: #000000;">0.3725808</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">King Midas</td>
      <td class="gt_row gt_right" style="background-color: #F7FCBB; color: #000000;">1.4881185</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
      <td class="gt_row gt_right" style="background-color: #FEFFE1; color: #000000;">0.3527392</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">tomatoes</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Amish Paste</td>
      <td class="gt_row gt_right" style="background-color: #F0F9B4; color: #000000;">1.9158148</td>
      <td class="gt_row gt_right" style="background-color: #004529; color: #FFFFFF;">31.2637162</td>
      <td class="gt_row gt_right" style="background-color: #31964F; color: #FFFFFF;">19.1184646</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Better Boy</td>
      <td class="gt_row gt_right" style="background-color: #DFF2A7; color: #000000;">2.7866397</td>
      <td class="gt_row gt_right" style="background-color: #90D082; color: #000000;">13.8802875</td>
      <td class="gt_row gt_right" style="background-color: #7AC77A; color: #000000;">13.6686440</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Big Beef</td>
      <td class="gt_row gt_right" style="background-color: #FBFED0; color: #000000;">0.7495708</td>
      <td class="gt_row gt_right" style="background-color: #A1D889; color: #000000;">12.6567234</td>
      <td class="gt_row gt_right" style="background-color: #C4E799; color: #000000;">8.5473117</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Black Krim</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
      <td class="gt_row gt_right" style="background-color: #88CD7F; color: #000000;">14.4601026</td>
      <td class="gt_row gt_right" style="background-color: #FEFFDE; color: #000000;">0.5202903</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Bonny Best</td>
      <td class="gt_row gt_right" style="background-color: #F4FBB7; color: #000000;">1.7196036</td>
      <td class="gt_row gt_right" style="background-color: #75C577; color: #000000;">15.8423993</td>
      <td class="gt_row gt_right" style="background-color: #DCF1A6; color: #000000;">6.5102429</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Brandywine</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
      <td class="gt_row gt_right" style="background-color: #ACDC8E; color: #000000;">11.8101493</td>
      <td class="gt_row gt_right" style="background-color: #F8FCC0; color: #000000;">2.9145076</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Cherokee Purple</td>
      <td class="gt_row gt_right" style="background-color: #F3FAB6; color: #000000;">1.7504683</td>
      <td class="gt_row gt_right" style="background-color: #9CD587; color: #000000;">13.0359181</td>
      <td class="gt_row gt_right" style="background-color: #FEFFDF; color: #000000;">0.4431286</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">grape</td>
      <td class="gt_row gt_right" style="background-color: #F8FCBF; color: #000000;">1.3426136</td>
      <td class="gt_row gt_right" style="background-color: #75C477; color: #000000;">15.8688548</td>
      <td class="gt_row gt_right" style="background-color: #A9DB8C; color: #000000;">10.6527238</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Jet Star</td>
      <td class="gt_row gt_right" style="background-color: #FCFED1; color: #000000;">0.6944553</td>
      <td class="gt_row gt_right" style="background-color: #DBF1A4; color: #000000;">7.6037344</td>
      <td class="gt_row gt_right" style="background-color: #E1F3A9; color: #000000;">5.9634971</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Mortgage Lifter</td>
      <td class="gt_row gt_right" style="background-color: #F3FAB6; color: #000000;">1.7659006</td>
      <td class="gt_row gt_right" style="background-color: #A7DB8C; color: #000000;">12.1430470</td>
      <td class="gt_row gt_right" style="background-color: #C7E89A; color: #000000;">8.3312590</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Old German</td>
      <td class="gt_row gt_right" style="background-color: #E0F3A8; color: #000000;">2.7425473</td>
      <td class="gt_row gt_right" style="background-color: #C9E99B; color: #000000;">9.2527901</td>
      <td class="gt_row gt_right" style="background-color: #A5DA8B; color: #000000;">10.8753905</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">volunteers</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
      <td class="gt_row gt_right" style="background-color: #A2D889; color: #000000;">12.5685386</td>
      <td class="gt_row gt_right" style="background-color: #004529; color: #FFFFFF;">27.6260932</td>
      <td class="gt_row gt_right" style="background-color: #FFFFE5; color: #000000;">0.0000000</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->



  5. Create a table using `gt` with data from your project or from the `garden_harvest` data if your project data aren't ready.
  

```r
soil <- read_csv("soil_magnetism_data.csv")
```


```r
soil %>% 
  gt(rowname_col = "sample_ID",
      groupname_col = "road") %>% 
  data_color(
    columns = vars(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11),
    colors = scales::col_bin(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::Reds"
      ) %>% as.character(),
      domain = NULL
    ))
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ufhosedinq .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ufhosedinq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ufhosedinq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ufhosedinq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ufhosedinq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ufhosedinq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ufhosedinq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ufhosedinq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ufhosedinq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ufhosedinq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ufhosedinq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ufhosedinq .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ufhosedinq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ufhosedinq .gt_from_md > :first-child {
  margin-top: 0;
}

#ufhosedinq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ufhosedinq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ufhosedinq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ufhosedinq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ufhosedinq .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ufhosedinq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ufhosedinq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ufhosedinq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ufhosedinq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ufhosedinq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ufhosedinq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ufhosedinq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ufhosedinq .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ufhosedinq .gt_left {
  text-align: left;
}

#ufhosedinq .gt_center {
  text-align: center;
}

#ufhosedinq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ufhosedinq .gt_font_normal {
  font-weight: normal;
}

#ufhosedinq .gt_font_bold {
  font-weight: bold;
}

#ufhosedinq .gt_font_italic {
  font-style: italic;
}

#ufhosedinq .gt_super {
  font-size: 65%;
}

#ufhosedinq .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="ufhosedinq" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">state</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">distance_cm</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">avg_outlier</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">M1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">M2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">M3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">M4</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">M5</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">M6</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">M7</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">M8</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">M9</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">M10</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">M11</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sus_avg</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sus_stdv</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">traffic_vol</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">traffic_class</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">Shepard Road</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">S10</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #CB181D; color: #FFFFFF;">0.003130</td>
      <td class="gt_row gt_right" style="background-color: #E33127; color: #FFFFFF;">0.002420</td>
      <td class="gt_row gt_right" style="background-color: #E33127; color: #FFFFFF;">0.002290</td>
      <td class="gt_row gt_right" style="background-color: #CB181D; color: #FFFFFF;">0.003150</td>
      <td class="gt_row gt_right" style="background-color: #CB181D; color: #FFFFFF;">0.003400</td>
      <td class="gt_row gt_right" style="background-color: #CB181D; color: #FFFFFF;">0.003560</td>
      <td class="gt_row gt_right" style="background-color: #67000D; color: #FFFFFF;">0.003530000</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.003068571</td>
      <td class="gt_row gt_right">5.166370e-04</td>
      <td class="gt_row gt_right">18000</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">S50</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FC9272; color: #000000;">0.001540</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001430</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001350</td>
      <td class="gt_row gt_right" style="background-color: #FB6A4A; color: #000000;">0.002030</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001960</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001880</td>
      <td class="gt_row gt_right" style="background-color: #CB181D; color: #FFFFFF;">0.001910000</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.001728571</td>
      <td class="gt_row gt_right">2.793700e-04</td>
      <td class="gt_row gt_right">18000</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">S150</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000835</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000921</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000837</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000828</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000799</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000771</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000831833</td>
      <td class="gt_row gt_right">5.055850e-05</td>
      <td class="gt_row gt_right">18000</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">S250</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">250</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001290</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001030</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001080</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001220</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000708</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000770</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.000820000</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000988286</td>
      <td class="gt_row gt_right">2.270840e-04</td>
      <td class="gt_row gt_right">18000</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">S100</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000802</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000957</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000801</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000977</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000954</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000941</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000905333</td>
      <td class="gt_row gt_right">8.125190e-05</td>
      <td class="gt_row gt_right">18000</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">S700</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">700</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000714</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000662</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000668</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000525</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000505</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000492</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000594333</td>
      <td class="gt_row gt_right">9.755550e-05</td>
      <td class="gt_row gt_right">18000</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">S760</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">760</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000949</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000992</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001000</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001140</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000973</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001120</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.000973000</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.001021000</td>
      <td class="gt_row gt_right">7.641550e-05</td>
      <td class="gt_row gt_right">18000</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">Mississippi River Boulevard</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M10</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000868</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000877</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000882</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001070</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001030</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000969</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000949333</td>
      <td class="gt_row gt_right">8.698890e-05</td>
      <td class="gt_row gt_right">3800</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M50</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000670</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000721</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000722</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001010</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000991</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000999</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000852167</td>
      <td class="gt_row gt_right">1.631430e-04</td>
      <td class="gt_row gt_right">3800</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M150</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000784</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000680</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000624</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000842</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000803</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000787</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000753333</td>
      <td class="gt_row gt_right">8.307510e-05</td>
      <td class="gt_row gt_right">3800</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M100</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000710</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000598</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000663</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000772</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000743</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000714</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.000725000</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000703571</td>
      <td class="gt_row gt_right">5.720970e-05</td>
      <td class="gt_row gt_right">3800</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M600</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">outlier</td>
      <td class="gt_row gt_right" style="background-color: #A50F15; color: #FFFFFF;">0.003740</td>
      <td class="gt_row gt_right" style="background-color: #B21218; color: #FFFFFF;">0.003710</td>
      <td class="gt_row gt_right" style="background-color: #B21218; color: #FFFFFF;">0.003410</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.003620000</td>
      <td class="gt_row gt_right">1.824830e-04</td>
      <td class="gt_row gt_right">3800</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M600</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000486</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000463</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000452</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000426</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000424</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000384</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000439167</td>
      <td class="gt_row gt_right">3.570110e-05</td>
      <td class="gt_row gt_right">3800</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M880</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">880</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000972</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000876</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000872</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000811</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000727</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000885</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000857167</td>
      <td class="gt_row gt_right">8.198880e-05</td>
      <td class="gt_row gt_right">3800</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">Brimhall Street</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">B10</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FC9272; color: #000000;">0.001540</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001360</td>
      <td class="gt_row gt_right" style="background-color: #FB6A4A; color: #000000;">0.001830</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000611</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000473</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000566</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.000552000</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000990286</td>
      <td class="gt_row gt_right">5.667960e-04</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">B50</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000655</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000665</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000638</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000479</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000526</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000508</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000578500</td>
      <td class="gt_row gt_right">8.306800e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">B100</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000324</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000292</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000287</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000665</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000530</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000547</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000440833</td>
      <td class="gt_row gt_right">1.605860e-04</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">B100</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">outlier</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001280</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001270</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001470</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.001340000</td>
      <td class="gt_row gt_right">1.126940e-04</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">B150</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000574</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000593</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000601</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000624</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000616</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000613</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000603500</td>
      <td class="gt_row gt_right">1.818520e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">B250</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">250</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000656</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000701</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000701</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000685</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000648</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000558</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000492000</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.000457</td>
      <td class="gt_row gt_right" style="background-color: #FC8A6A; color: #000000;">0.000403</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000589000</td>
      <td class="gt_row gt_right">1.143810e-04</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">B550</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">550</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000494</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000567</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000538</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000616</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000553</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000652</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000570000</td>
      <td class="gt_row gt_right">5.645880e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">B600</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000615</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000622</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000635</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000423</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000442</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000423</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000526667</td>
      <td class="gt_row gt_right">1.070410e-04</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">George Rice Drive</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">C10</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000669</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000644</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000862</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000856</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000870</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000279</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000309000</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000283</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000282</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000361</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000303000</td>
      <td class="gt_row gt_right">0.000519818</td>
      <td class="gt_row gt_right">2.603310e-04</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">C50</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000771</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000750</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000692</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000334</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000313</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000316</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000448000</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.000428</td>
      <td class="gt_row gt_right" style="background-color: #FC8A6A; color: #000000;">0.000416</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000496444</td>
      <td class="gt_row gt_right">1.884200e-04</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">C100</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000407</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000408</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000380</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000385</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000356</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000321</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.000584000</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.000438</td>
      <td class="gt_row gt_right" style="background-color: #F24632; color: #FFFFFF;">0.000571</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000427778</td>
      <td class="gt_row gt_right">9.118080e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">C150</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">outlier</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000962</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000942</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000992</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000965333</td>
      <td class="gt_row gt_right">2.516610e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">C150</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000376</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000354</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000363</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000284</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000260</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000241</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000313000</td>
      <td class="gt_row gt_right">5.828210e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">C300</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">300</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000351</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000321</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000323</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000350</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000323</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000324</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000394000</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000363</td>
      <td class="gt_row gt_right" style="background-color: #FDCAB4; color: #000000;">0.000380</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000347667</td>
      <td class="gt_row gt_right">2.723050e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">C600</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000369</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000406</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000433</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000349</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000350</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000366</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000326000</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000349</td>
      <td class="gt_row gt_right" style="background-color: #FDCAB4; color: #000000;">0.000339</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000365222</td>
      <td class="gt_row gt_right">3.401390e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">I-75 Walnut Avenue Offramp</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">I50</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000694</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000734</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000720</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000660</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000710</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000763</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.000791000</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000724571</td>
      <td class="gt_row gt_right">4.342760e-05</td>
      <td class="gt_row gt_right">5890</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">I100</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000803</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000864</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000774</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000586</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000507</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000547</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000680167</td>
      <td class="gt_row gt_right">1.511790e-04</td>
      <td class="gt_row gt_right">5890</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">I150</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000323</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000282</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000458</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000476</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000427</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000417</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000397167</td>
      <td class="gt_row gt_right">7.741170e-05</td>
      <td class="gt_row gt_right">5890</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">I200</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">200</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000346</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000327</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000331</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000689</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000559</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000665</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000486167</td>
      <td class="gt_row gt_right">1.717470e-04</td>
      <td class="gt_row gt_right">5890</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">I300</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">300</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000285</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000348</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000279</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000271</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000254</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000240</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000279500</td>
      <td class="gt_row gt_right">3.743130e-05</td>
      <td class="gt_row gt_right">5890</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">I400</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">400</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000401</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000357</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000379</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000229</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000217</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000139</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000287000</td>
      <td class="gt_row gt_right">1.063280e-04</td>
      <td class="gt_row gt_right">5890</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">I500</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">500</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000882</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000113</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000128</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000374333</td>
      <td class="gt_row gt_right">4.397160e-04</td>
      <td class="gt_row gt_right">5890</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">I500+</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">500</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000290</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000261</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000261</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000299</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000276</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000301</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000281333</td>
      <td class="gt_row gt_right">1.805180e-05</td>
      <td class="gt_row gt_right">5890</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">I600</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000019</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000083</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000079</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000271</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000153</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000133</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000123000</td>
      <td class="gt_row gt_right">8.632030e-05</td>
      <td class="gt_row gt_right">5890</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">Walnut Avenue</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">W10</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000832</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000750</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000701</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000920</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000926</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000985</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000852333</td>
      <td class="gt_row gt_right">1.108090e-04</td>
      <td class="gt_row gt_right">25500</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">W50</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000559</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000675</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000662</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000754</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000759</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000741</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.000750000</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000700000</td>
      <td class="gt_row gt_right">7.362970e-05</td>
      <td class="gt_row gt_right">25500</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">W100</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000593</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000562</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000540</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000554</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000526</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000626</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000566833</td>
      <td class="gt_row gt_right">3.677180e-05</td>
      <td class="gt_row gt_right">25500</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">W250</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">250</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000391</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000373</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000363</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000619</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000602</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000556</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000484000</td>
      <td class="gt_row gt_right">1.207840e-04</td>
      <td class="gt_row gt_right">25500</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">W350</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">350</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000334</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000316</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000303</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000385</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000432</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000369</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000356500</td>
      <td class="gt_row gt_right">4.832700e-05</td>
      <td class="gt_row gt_right">25500</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">W450</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">450</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000335</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000309</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000312</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000279</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000232</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000238</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000284167</td>
      <td class="gt_row gt_right">4.208290e-05</td>
      <td class="gt_row gt_right">25500</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">W600</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000229</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000222</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000224</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000222</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000215</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000237</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000224833</td>
      <td class="gt_row gt_right">7.467710e-06</td>
      <td class="gt_row gt_right">25500</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">Babb Road</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">BA10</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000219</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000228</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000210</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000148</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000162</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000137</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000184000</td>
      <td class="gt_row gt_right">3.956260e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">BA50</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000143</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000146</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000155</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000137</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000144</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000150</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000145833</td>
      <td class="gt_row gt_right">6.177920e-06</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">BA100</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000203</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000228</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000230</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000185</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000207</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000172</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000204167</td>
      <td class="gt_row gt_right">2.300800e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">BA150</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000235</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000258</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000250</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000269</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000267</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000267</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000257667</td>
      <td class="gt_row gt_right">1.323130e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">BA200</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">200</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000284</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000290</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000292</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000347</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000325</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000345</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000313833</td>
      <td class="gt_row gt_right">2.874310e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">BA300</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">300</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000207</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000209</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000214</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000175</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000189</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000197</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000198500</td>
      <td class="gt_row gt_right">1.461160e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">BA400</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">400</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000400</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000379</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000383</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000387</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000419</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000468</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000406000</td>
      <td class="gt_row gt_right">3.366900e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">BA500</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">500</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000161</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000161</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000160</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000428</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000376</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000386</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000278667</td>
      <td class="gt_row gt_right">1.304360e-04</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">BA600</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000165</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000176</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000169</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000164</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000148</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000148</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000161667</td>
      <td class="gt_row gt_right">1.139590e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">Mill Creek Road</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M10</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000209</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000226</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000287</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000408</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000435</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000337</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000317000</td>
      <td class="gt_row gt_right">9.322020e-05</td>
      <td class="gt_row gt_right">1390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M50</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000519</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000475</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000441</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000481</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000513</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000487</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000486000</td>
      <td class="gt_row gt_right">2.824890e-05</td>
      <td class="gt_row gt_right">1390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M100</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000509</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000547</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000573</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000358</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000300</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000256</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000423833</td>
      <td class="gt_row gt_right">1.360230e-04</td>
      <td class="gt_row gt_right">1390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M150</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000201</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000220</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000222</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000395</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000352</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000381</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000295167</td>
      <td class="gt_row gt_right">8.992760e-05</td>
      <td class="gt_row gt_right">1390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M200</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">200</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000202</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000225</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000227</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000265</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000277</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000283</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000246500</td>
      <td class="gt_row gt_right">3.294690e-05</td>
      <td class="gt_row gt_right">1390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M300</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">300</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000159</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000190</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000224</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000224</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000250</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000270</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000219500</td>
      <td class="gt_row gt_right">4.012850e-05</td>
      <td class="gt_row gt_right">1390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M400</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">400</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000305</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000299</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000287</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000237</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000278</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000274</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000280000</td>
      <td class="gt_row gt_right">2.418260e-05</td>
      <td class="gt_row gt_right">1390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M500</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">500</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000243</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000249</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000223</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000102</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000125</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000096</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000147000</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000169286</td>
      <td class="gt_row gt_right">6.711860e-05</td>
      <td class="gt_row gt_right">1390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">M600</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000139</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000120</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000104</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000100</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000070</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000073</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000101000</td>
      <td class="gt_row gt_right">2.668330e-05</td>
      <td class="gt_row gt_right">1390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">Old Babb Road</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">OB10</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000699</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000800</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000744</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000724</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000372</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000389</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000338000</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000580857</td>
      <td class="gt_row gt_right">2.035100e-04</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">OB50</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000348</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000282</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000327</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000380</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000384</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000275</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000332667</td>
      <td class="gt_row gt_right">4.697940e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">OB100</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000441</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000392</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000419</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000430</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000422</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000404</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000418000</td>
      <td class="gt_row gt_right">1.767480e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">OB150</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000213</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000205</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000239</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000664</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000681</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000650</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000442000</td>
      <td class="gt_row gt_right">2.447400e-04</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">OB200</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">200</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000141</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000133</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000135</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000211</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000209</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000231</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000176667</td>
      <td class="gt_row gt_right">4.492510e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">OB300</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">300</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000365</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000346</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000308</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000054</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000036</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000067</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000196000</td>
      <td class="gt_row gt_right">1.587510e-04</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">OB400</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">400</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000146</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000129</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000126</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000212</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000212</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000238</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000177167</td>
      <td class="gt_row gt_right">4.906490e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">OB500</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">500</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000271</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000251</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000251</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000279</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000268</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000285</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000267500</td>
      <td class="gt_row gt_right">1.411030e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">OB500</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">500</td>
      <td class="gt_row gt_left">outlier</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001020</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001020</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001110</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.001050000</td>
      <td class="gt_row gt_right">5.196150e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">OB600</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000256</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000319</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000283</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000413</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000373</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000396</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000340000</td>
      <td class="gt_row gt_right">6.371810e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">Utility Road</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">U10</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000225</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000204</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000192</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000188</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000165</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000184</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000325000</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000320</td>
      <td class="gt_row gt_right" style="background-color: #FDCAB4; color: #000000;">0.000304</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000234111</td>
      <td class="gt_row gt_right">6.393250e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">U50</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000330</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000327</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000335</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000406</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000417</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000425</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000392000</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000370</td>
      <td class="gt_row gt_right" style="background-color: #FDCAB4; color: #000000;">0.000326</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000369778</td>
      <td class="gt_row gt_right">4.126670e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">U100</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000303</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000303</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000312</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000282</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000282</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000298</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000362000</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000372</td>
      <td class="gt_row gt_right" style="background-color: #FDCAB4; color: #000000;">0.000320</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000314889</td>
      <td class="gt_row gt_right">3.210700e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">U150</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000334</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000327</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000301</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000296</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000290</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000309</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000386000</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.000433</td>
      <td class="gt_row gt_right" style="background-color: #FDCAB4; color: #000000;">0.000373</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000338778</td>
      <td class="gt_row gt_right">4.868720e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">U200</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">200</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000496</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000476</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000485</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000474</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000462</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000484</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000368000</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.000409</td>
      <td class="gt_row gt_right" style="background-color: #FDCAB4; color: #000000;">0.000369</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000447000</td>
      <td class="gt_row gt_right">5.096320e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">U300</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">300</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000361</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000351</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000345</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000566</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000593</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000565</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.000975000</td>
      <td class="gt_row gt_right" style="background-color: #67000D; color: #FFFFFF;">0.000940</td>
      <td class="gt_row gt_right" style="background-color: #67000D; color: #FFFFFF;">0.000931</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000625222</td>
      <td class="gt_row gt_right">2.613990e-04</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">U400</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">400</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000674</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000662</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000640</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000557</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000536</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000482</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000591833</td>
      <td class="gt_row gt_right">7.796000e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">U500</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">500</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000648</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000600</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000615</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000643</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000623</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000644</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000628833</td>
      <td class="gt_row gt_right">1.926050e-05</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">U600</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000719</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000698</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000706</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000881</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000907</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000912</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000803833</td>
      <td class="gt_row gt_right">1.060820e-04</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">Dalton Bypass Corporate Drive</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">D10</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #67000D; color: #FFFFFF;">0.004440</td>
      <td class="gt_row gt_right" style="background-color: #67000D; color: #FFFFFF;">0.004190</td>
      <td class="gt_row gt_right" style="background-color: #67000D; color: #FFFFFF;">0.004630</td>
      <td class="gt_row gt_right" style="background-color: #67000D; color: #FFFFFF;">0.007770</td>
      <td class="gt_row gt_right" style="background-color: #67000D; color: #FFFFFF;">0.006630</td>
      <td class="gt_row gt_right" style="background-color: #67000D; color: #FFFFFF;">0.006290</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.005658333</td>
      <td class="gt_row gt_right">1.449143e-03</td>
      <td class="gt_row gt_right">16600</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">D50</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FB6A4A; color: #000000;">0.002140</td>
      <td class="gt_row gt_right" style="background-color: #E33127; color: #FFFFFF;">0.002200</td>
      <td class="gt_row gt_right" style="background-color: #E33127; color: #FFFFFF;">0.002260</td>
      <td class="gt_row gt_right" style="background-color: #FB6A4A; color: #000000;">0.002870</td>
      <td class="gt_row gt_right" style="background-color: #FB6A4A; color: #000000;">0.002750</td>
      <td class="gt_row gt_right" style="background-color: #FB6A4A; color: #000000;">0.002570</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.002465000</td>
      <td class="gt_row gt_right">3.079450e-04</td>
      <td class="gt_row gt_right">16600</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">D100</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001340</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001230</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001150</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001100</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001070</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001060</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.001158333</td>
      <td class="gt_row gt_right">1.087040e-04</td>
      <td class="gt_row gt_right">16600</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">D150</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000957</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001000</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000993</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001540</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001540</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001500</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.001255000</td>
      <td class="gt_row gt_right">2.983110e-04</td>
      <td class="gt_row gt_right">16600</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">D200</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">200</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001100</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000983</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000933</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000870</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000847</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000868</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000933500</td>
      <td class="gt_row gt_right">9.589320e-05</td>
      <td class="gt_row gt_right">16600</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">D300</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">300</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001000</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000795</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000853</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000732</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000902</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000906</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.000945000</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000876143</td>
      <td class="gt_row gt_right">9.086520e-05</td>
      <td class="gt_row gt_right">16600</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">D300</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">300</td>
      <td class="gt_row gt_left">outlier</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000328</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000369</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000364</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000353667</td>
      <td class="gt_row gt_right">2.236810e-05</td>
      <td class="gt_row gt_right">16600</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">D400</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">400</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000515</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000468</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000447</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000329</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000283</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000303</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000390833</td>
      <td class="gt_row gt_right">9.766560e-05</td>
      <td class="gt_row gt_right">16600</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">D500</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">500</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000306</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000370</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000324</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000399</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000403</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000399</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000366833</td>
      <td class="gt_row gt_right">4.224410e-05</td>
      <td class="gt_row gt_right">16600</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">D550</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">550</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000195</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000163</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000175</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000224</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000244</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000277</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000213000</td>
      <td class="gt_row gt_right">4.346490e-05</td>
      <td class="gt_row gt_right">16600</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">D600</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">outlier</td>
      <td class="gt_row gt_right" style="background-color: #EF3B2C; color: #FFFFFF;">0.002710</td>
      <td class="gt_row gt_right" style="background-color: #E33127; color: #FFFFFF;">0.002270</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000969</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001660</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.001902250</td>
      <td class="gt_row gt_right">7.566020e-04</td>
      <td class="gt_row gt_right">16600</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">D600</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000384</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000315</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000251</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000288</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000260</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000226</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000236000</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000280000</td>
      <td class="gt_row gt_right">5.507270e-05</td>
      <td class="gt_row gt_right">16600</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">Dalton Bypass Caunasauga Drive</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">DC10</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001480</td>
      <td class="gt_row gt_right" style="background-color: #FB6A4A; color: #000000;">0.001590</td>
      <td class="gt_row gt_right" style="background-color: #FB6A4A; color: #000000;">0.001610</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001200</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001230</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001220</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.001388333</td>
      <td class="gt_row gt_right">1.934340e-04</td>
      <td class="gt_row gt_right">12300</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">DC50</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000954</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000952</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000903</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000854</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000802</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000752</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000869500</td>
      <td class="gt_row gt_right">8.206280e-05</td>
      <td class="gt_row gt_right">12300</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">DC100</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000779</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000612</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000585</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000861</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000749</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000725</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000718500</td>
      <td class="gt_row gt_right">1.040230e-04</td>
      <td class="gt_row gt_right">12300</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">DC150</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000831</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000765</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000751</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001160</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001050</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001130</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000947833</td>
      <td class="gt_row gt_right">1.867930e-04</td>
      <td class="gt_row gt_right">12300</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">DC200</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">200</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000678</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000556</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000558</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000534</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000524</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000450</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000550000</td>
      <td class="gt_row gt_right">7.404860e-05</td>
      <td class="gt_row gt_right">12300</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">DC200</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">200</td>
      <td class="gt_row gt_left">outlier</td>
      <td class="gt_row gt_right" style="background-color: #FC9272; color: #000000;">0.001550</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001350</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001290</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.001396667</td>
      <td class="gt_row gt_right">1.361370e-04</td>
      <td class="gt_row gt_right">12300</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">DC300</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">300</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000846</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000812</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000736</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000683</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000685</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000771</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000755500</td>
      <td class="gt_row gt_right">6.667760e-05</td>
      <td class="gt_row gt_right">12300</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">DC400</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">400</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000689</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000713</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000681</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000929</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000863</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001040</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.000644000</td>
      <td class="gt_row gt_right" style="background-color: #E33127; color: #FFFFFF;">0.000664</td>
      <td class="gt_row gt_right" style="background-color: #BC141A; color: #FFFFFF;">0.000680</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000767000</td>
      <td class="gt_row gt_right">1.412850e-04</td>
      <td class="gt_row gt_right">12300</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">DC500</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">500</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000332</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000318</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000335</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000520</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000456</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000443</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.000807000</td>
      <td class="gt_row gt_right" style="background-color: #B21218; color: #FFFFFF;">0.000787</td>
      <td class="gt_row gt_right" style="background-color: #BC141A; color: #FFFFFF;">0.000696</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000521556</td>
      <td class="gt_row gt_right">1.952600e-04</td>
      <td class="gt_row gt_right">12300</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">DC600</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000534</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000573</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000591</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000484</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000460</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000455</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000516167</td>
      <td class="gt_row gt_right">5.844460e-05</td>
      <td class="gt_row gt_right">12300</td>
      <td class="gt_row gt_left">high</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">College Drive</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">CD10</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000524</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000465</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000509</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000340</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000328</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000320</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000414333</td>
      <td class="gt_row gt_right">9.532400e-05</td>
      <td class="gt_row gt_right">5210</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">CD50</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000286</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000288</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000283</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000316</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000310</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000295</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000296333</td>
      <td class="gt_row gt_right">1.363330e-05</td>
      <td class="gt_row gt_right">5210</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">CD100</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000337</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000360</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000336</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000524</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000469</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000428</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000409000</td>
      <td class="gt_row gt_right">7.758870e-05</td>
      <td class="gt_row gt_right">5210</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">CD150</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000253</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000284</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000288</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000294</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000284</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000285</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000281333</td>
      <td class="gt_row gt_right">1.438980e-05</td>
      <td class="gt_row gt_right">5210</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">CD200</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">200</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000242</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000236</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000237</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000265</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000251</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000241</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000245333</td>
      <td class="gt_row gt_right">1.100300e-05</td>
      <td class="gt_row gt_right">5210</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">CD300</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">300</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000474</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000489</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000490</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000349</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000336</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000331</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000411500</td>
      <td class="gt_row gt_right">8.020160e-05</td>
      <td class="gt_row gt_right">5210</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">CD400</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">400</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000279</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000269</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000281</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000488</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000452</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000435</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000367333</td>
      <td class="gt_row gt_right">1.012260e-04</td>
      <td class="gt_row gt_right">5210</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">CD500</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">500</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000303</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000295</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000308</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000362</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000369</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000359</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.000804000</td>
      <td class="gt_row gt_right" style="background-color: #B21218; color: #FFFFFF;">0.000754</td>
      <td class="gt_row gt_right" style="background-color: #BC141A; color: #FFFFFF;">0.000693</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000471889</td>
      <td class="gt_row gt_right">2.123830e-04</td>
      <td class="gt_row gt_right">5210</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">CD600</td>
      <td class="gt_row gt_left">Georgia</td>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000272</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000271</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000263</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000391</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000377</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000363</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000322833</td>
      <td class="gt_row gt_right">6.007470e-05</td>
      <td class="gt_row gt_right">5210</td>
      <td class="gt_row gt_left">medium</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="19" class="gt_group_heading">Inver Grove Trail</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">R10</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000785</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000900</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001000</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001120</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000573</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000612</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.000603000</td>
      <td class="gt_row gt_right" style="background-color: #B21218; color: #FFFFFF;">0.000727</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000790000</td>
      <td class="gt_row gt_right">2.010100e-04</td>
      <td class="gt_row gt_right">390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">R50</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000807</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000963</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000929</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000899667</td>
      <td class="gt_row gt_right">8.203250e-05</td>
      <td class="gt_row gt_right">390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">R150</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000983</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000934</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000891</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000603</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000448</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000463</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000720333</td>
      <td class="gt_row gt_right">2.441010e-04</td>
      <td class="gt_row gt_right">390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">R280</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">280</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001060</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000741</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000804</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001040</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001640</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001730</td>
      <td class="gt_row gt_right" style="background-color: #CB181D; color: #FFFFFF;">0.001760000</td>
      <td class="gt_row gt_right" style="background-color: #FB6A4A; color: #000000;">0.000554</td>
      <td class="gt_row gt_right" style="background-color: #FC8A6A; color: #000000;">0.000484</td>
      <td class="gt_row gt_right" style="background-color: #67000D; color: #FFFFFF;">0.000500</td>
      <td class="gt_row gt_right" style="background-color: #67000D; color: #FFFFFF;">0.000512667</td>
      <td class="gt_row gt_right">0.000984152</td>
      <td class="gt_row gt_right">5.080060e-04</td>
      <td class="gt_row gt_right">390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">R330</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">330</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001030</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000991</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001050</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.001023667</td>
      <td class="gt_row gt_right">3.000560e-05</td>
      <td class="gt_row gt_right">390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">R410</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">410</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FEE0D2; color: #000000;">0.000970</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001090</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000917</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000520</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000437</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000372</td>
      <td class="gt_row gt_right" style="background-color: #FFF5F0; color: #000000;">0.000443000</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.000678429</td>
      <td class="gt_row gt_right">3.011180e-04</td>
      <td class="gt_row gt_right">390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">R0520</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">520</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001310</td>
      <td class="gt_row gt_right" style="background-color: #FB6A4A; color: #000000;">0.001620</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001280</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001080</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001110</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.001280000</td>
      <td class="gt_row gt_right">2.152910e-04</td>
      <td class="gt_row gt_right">390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">R760</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">760</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001240</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001230</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000813</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.001094333</td>
      <td class="gt_row gt_right">2.436930e-04</td>
      <td class="gt_row gt_right">390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">R900</td>
      <td class="gt_row gt_left">Minnesota</td>
      <td class="gt_row gt_right">900</td>
      <td class="gt_row gt_left">average</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001150</td>
      <td class="gt_row gt_right" style="background-color: #FED4C1; color: #000000;">0.000865</td>
      <td class="gt_row gt_right" style="background-color: #FDA081; color: #000000;">0.001070</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001580</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001610</td>
      <td class="gt_row gt_right" style="background-color: #FCBBA1; color: #000000;">0.001110</td>
      <td class="gt_row gt_right" style="background-color: #FB6A4A; color: #000000;">0.001433333</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right" style="background-color: #808080; color: #000000;">NA</td>
      <td class="gt_row gt_right">0.001259762</td>
      <td class="gt_row gt_right">2.833340e-04</td>
      <td class="gt_row gt_right">390</td>
      <td class="gt_row gt_left">low</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

  
  6. Use `patchwork` operators and functions to combine at least two graphs using your project data or `garden_harvest` data if your project data aren't read.
  
  

```r
plot1 <- soil %>% 
  filter(state == "Minnesota") %>% 
  mutate(traffic_class = fct_relevel(traffic_class, "low", "medium", "high")) %>% 
  ggplot(aes(x = distance_cm, y = sus_avg, 
             ymin = sus_avg-sus_stdv, ymax = sus_avg+sus_stdv,
             color = traffic_class)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Distance from Road (cm)",
       y = "Average Magnetic Susceptibility",
       color = "Traffic Volume Classification",
       title = "Magnetic Susceptibility of Roadside Soils in Minnesota") +
  facet_wrap(~road) 

plot2 <- soil %>% 
  filter(state == "Minnesota") %>% 
  mutate(traffic_class = fct_relevel(traffic_class, "low", "medium", "high")) %>%
  ggplot(aes(x = traffic_class, y = sus_avg, group = traffic_class)) +
  geom_boxplot(aes(fill = traffic_class), alpha = 0.5) +
  labs(x = "Traffic Volume Classification",
       y = "") +
  theme(legend.position = "none")

plot1 + plot2 + plot_layout(ncol = 1, nrow = 2)
```

![](06_exercises_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
  
  

**DID YOU REMEMBER TO UNCOMMENT THE OPTIONS AT THE TOP?**
