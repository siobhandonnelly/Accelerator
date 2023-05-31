#Installing the packages used to load in and clean the dataset
# install.packages("tidyverse", dependencies = TRUE, type = "win.binary")
# install.packages("readxl", dependencies = TRUE, type = "win.binary")
# install.packages("janitor", dependencies = TRUE, type = "win.binary")
# install.packages("plotly", dependencies = TRUE, type = "win.binary")

#Loading in the Tidyverse
library(tidyverse)
library(usethis)
library(ggplot2)
library(dplyr)
library(plotly)

# use_github(protocol = 'https', auth_token = Sys.getenv("GITHUB_PAT"))



#Trying to load in data with nulls dealt with
nature_of_work <- read_csv("UKHSA dataset.csv", na = c("U", "Z0 - Missing data" ,"NA", "Not Known" ,"Not applicable", "-1","*",".", "", "NULL"))
#Taking a look at the dataset 
View(nature_of_work)

nature_of_work$f_zcohort <- as.factor(nature_of_work$f_zcohort)

#Attempting to order the SOCcode variable
typeof("f_xwrk2020soc1")
nature_of_work$f_xwrk2020soc1 <- factor(nature_of_work$f_xwrk2020soc1, levels = c("Managers, directors and senior officials", "Professional occupations", "Associate professional occupations", "Administrative and secretarial occupations", "Skilled trades occupations", "Caring, leisure and other service occupations", "Sales and customer service occupations", "Process, plant and machine operatives", "Elementary occupations"))

nature_of_work$f_xwrk2007sic1 <- factor(nature_of_work$f_xwrk2007sic1, levels = c("Agriculture, forestry and fishing", "Mining and quarrying", "Manufacturing", "Electricity, gas, steam and air conditioning supply" , "Water supply; sewerage, waste management and remediation activities", "Construction" , "Wholesale and retail trade; repair of motor vehicles and motorcycles" , "Transportation and storage", "Accommodation and food service activities" , "Information and communication" , "Financial and insurance activities", "Real estate activities", "Professional, scientific and technical activities", "Administrative and support service activities", "Public administration and defence; compulsory social security", "Education", "Human health and social work activities", "Arts, entertainment and recreation", "Other service activities", "Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use", "Activities of extraterritorial organisations and bodies"))


nature_of_work$f_xempbasis <- factor(nature_of_work$f_xempbasis, levels = c("On a permanent/open ended contract", "On a fixed-term contract lasting 12 months or longer","On a fixed-term contract lasting less than 12 months","Temping (including supply teaching)","On a zero hours contract","Volunteering","On an internship","Other","Not known"))
#nature_of_work$f_providername <- factor(nature_of_work$f_providername, levels = c("AA School of Architecture","ACM Guildford Limited","AECC University College","Abertay University","Aberystwyth University","Academy of Live and Recorded Arts","All Nations Christian College","Anglia Ruskin University","Apex College London","Arden University","Arts Educational Schools","Aston University","BIMM Limited","Bangor University","Bath Spa University","Birkbeck College","Birmingham City University","Bishop Grosseteste University","Bloomsbury Institute","Bournemouth University","Bristol Baptist College","Brunel University London","Buckinghamshire New University","Cambridge Arts and Sciences Limited","Canterbury Christ Church University","Cardiff Metropolitan University","Cardiff University","Chicken Shed Theatre Company","City and Guilds of London Art School","City, University of London","Cliff College","Conservatoire for Dance and Drama","Courtauld Institute of Art","Coventry University","Cranfield University","De Montfort University","Edge Hill University","Edinburgh Napier University","Empire College London Limited","Falmouth University","ForMission Ltd","Futureworks","Glasgow Caledonian University","Glasgow School of Art","Glyndŵr University","Goldsmiths College","Gower College Swansea","Grŵµp Llandrillo Menai","Grŵµp NPTC Group","Guildhall School of Music and Drama","Harper Adams University","Hartpury University","Heriot-Watt University","ICOM","ICON College of Technology and Management","Imperial College of Science, Technology and Medicine","Institute of Contemporary Music Performance","Istituto Marangoni Limited","Keele University","Kensington College of Business","King's College London","Kingston University","LCCM AU UK Limited","Leeds Arts University","Leeds Beckett University","Leeds Conservatoire","Leeds Trinity University","Liverpool Hope University","Liverpool John Moores University","Liverpool School of Tropical Medicine","London Business School","London Churchill College Ltd","London Metropolitan University","London School of Academics Ltd","London School of Commerce & IT Limited","London School of Economics and Political Science","London School of Hygiene and Tropical Medicine","London School of Management Education","London South Bank University","Loughborough University","Luther King House Educational Trust","Matrix College of Counselling and Psychotherapy Ltd","Mattersey Hall","Met Film School Limited","Middlesex University","Millennium Performing Arts Ltd","Mont Rose College of Management and Sciences","Moorlands College","Mountview Academy of Theatre Arts","Nazarene Theological College","Nelson College London Ltd","New College of the Humanities","Newcastle University","Newman University","Norland College","Northern College of Acupuncture","Norwich University of the Arts","Oxford Brookes University","Oxford Business College","Pearson College","Plymouth College of Art","Point Blank Music School","Queen Margaret University, Edinburgh","Queen Mary University of London","Queen's University Belfast","Ravensbourne University London","Regent College","Regent's University London","Regents Theological College","Richmond, The American International University in London","Robert Gordon University","Roehampton University","Rose Bruford College of Theatre and Performance","Royal Academy of Dance","Royal Academy of Music","Royal Agricultural University","Royal College of Art","Royal College of Music","Royal Conservatoire of Scotland","Royal Holloway and Bedford New College","Royal Northern College of Music","SAE Education Limited","SOAS University of London","SRUC","Sheffield Hallam University","Slough Borough Council","Solent University","St George's, University of London","St Mary's University College","St Mary's University, Twickenham","St Mellitus College","St Nicholas Montessori Training Limited","Staffordshire University","Stranmillis University College","Swansea University","Teesside University","The Arts University Bournemouth","The City College","The College of Integrated Chinese Medicine","The Institute of Cancer Research","The Islamic College","The Liverpool Institute for Performing Arts","The London Institute of Banking & Finance","The London School of Architecture","The Manchester Metropolitan University","The Markfield Institute of Higher Education","The Metanoia Institute","The National Film and Television School","The Nottingham Trent University","The Open University","The Royal Central School of Speech and Drama","The Royal Veterinary College","The Salvation Army","The Sherwood Psychotherapy Training Institute Limited","The University College of Osteopathy","The University of Aberdeen","The University of Bath","The University of Birmingham","The University of Bolton","The University of Bradford","The University of Brighton","The University of Bristol","The University of Buckingham","The University of Cambridge")

nature_of_work$f_providername <- factor(nature_of_work$f_providername)
nature_of_work <- nature_of_work %>%
  mutate(study_mode = case_when(f_xqmode01 %in% ("Part-time") ~ 2 ,
                                   f_xqmode01 %in% ("Full-time") ~ 1 
  ))

nature_of_work <- nature_of_work %>%
  mutate(work_mean_num = case_when(f_wrkmean %in% ("Strongly agree") ~ 5,
                                   f_wrkmean %in% ("Agree")  ~ 4 ,
                                   f_wrkmean %in% ("Neither agree nor disagree") ~ 3 ,
                                   f_wrkmean %in% ("Disagree") ~ 2 ,
                                   f_wrkmean %in% ("Strongly disagree") ~ 1 
                                   ))

nature_of_work <- nature_of_work %>%
  mutate(work_skills_num = case_when(f_wrkskills %in% ("Strongly agree") ~ 5,
                                   f_wrkskills %in% ("Agree")  ~ 4 ,
                                   f_wrkskills %in% ("Neither agree nor disagree") ~ 3 ,
                                   f_wrkskills %in% ("Disagree") ~ 2 ,
                                   f_wrkskills %in% ("Strongly disagree") ~ 1 
                                   ))

nature_of_work <- nature_of_work %>%
  mutate(work_ontrack_num = case_when(f_wrkontrack %in% ("Strongly agree") ~ 5,
                                   f_wrkontrack %in% ("Agree")  ~ 4 ,
                                   f_wrkontrack %in% ("Neither agree nor disagree") ~ 3 ,
                                   f_wrkontrack %in% ("Disagree") ~ 2 ,
                                   f_wrkontrack %in% ("Strongly disagree") ~ 1 
                                    ))

nature_of_work <- nature_of_work %>%
  mutate(danow = ((work_ontrack_num + work_skills_num + work_mean_num) / 3))
          

#creating a data frame that containsonly distinct individuals
now_update2 <- distinct(nature_of_work, f_zanonymous, .keep_all = TRUE)

#restricting the data fram to those who studied full time and responded to all three GV questions
now_update2 <- subset(nature_of_work, study_mode == 1 | work_ontrack_num !=NA | work_skills_num!=NA | work_mean_num!=NA)



plot_creation <- function(df, colName) {
 ndf <- df %>%
    filter(!is.na(df[[colName]])) 
 ndf %>% 
    group_by(f_zcohort, ndf[[colName]]) %>%
    mutate(mean_danow = mean(danow, na.rm = TRUE)) %>% 
    ggplot() +
    geom_col(
      mapping = aes(x = f_zcohort,
                    y = mean_danow,
                    fill = ndf[[colName]]),
      position = position_dodge(),
    )  +
      coord_cartesian(ylim=c(1,5)) +
        scale_fill_manual(values = c("#1F4388",
                                 "#83C7BC",
                                 "#1E355E",
                                 "#6A86B8",
                                 "#A93439",
                                 "#CE3162",
                                 "#E57D3A",
                                 "#4EA585",
                                "#BBB332",
                                 "#E8D77E")
   ) # adding HESA colours
}

# Chart 1: Showing the design and nature of work score by the graduates Standard occupational code
graph1 <- plot_creation(now_update2,"f_xwrk2020soc1") + 

  labs(x = "Graduation Year", # Labels
       y = "Mean design and nature of work score",
       fill  = "Standard Occupational Code", # Legend Label
       title = "Graduates Design and nature of work score by SOC group",
       subtitle = "",
       caption = "")

graph1


# Chart 2: Showing the design and nature of work score by the graduates employment basis

graph2 <- plot_creation(now_update2,"f_xempbasis") +
  
  labs(x = "Graduation Year", # Labels
       y = "Mean design and nature of work score",
       fill  = "Employment Basis", # Legend Label
       title = "Graduates Design and nature of work score by Employment Basis",
       subtitle = "",
       caption = "")
graph2


# Chart 3: Showing the design and nature of work score by the graduates level of study

graph3 <- plot_creation(now_update2,"f_xglev501") +
  
  labs(x = "Graduation Year", # Labels
       y = "Mean design and nature of work score",
       fill  = "Level of Study", # Legend Label
       title = "Graduates Design and nature of work score by Level of Study",
       subtitle = "",
       caption = "")
graph3


# Chart 4: Showing the design and nature of work score by the graduates class of degree attained

graph4 <- plot_creation(now_update2,"f_xclass01") +
  
  labs(x = "Graduation Year", # Labels
       y = "Mean design and nature of work score",
       fill  = "Class of Degree", # Legend Label
       title = "Graduates Design and nature of work score by Class of Degree",
       subtitle = "",
       caption = "")
graph4


# Chart 5: Showing the design and nature of work score by the graduates domicile

graph5 <- plot_creation(now_update2,"f_xdomgr01") +
  
  labs(x = "Graduation Year", # Labels
       y = "Mean design and nature of work score",
       fill  = "Employment Basis", # Legend Label
       title = "Graduates Design and nature of work score by Employment Basis",
       subtitle = "",
       caption = "")
graph5

graph6 <- plot_creation(now_update2,"f_xwrk2007sic1") +
  
  labs(x = "Graduation Year", # Labels
       y = "Mean design and nature of work score",
       fill  = "Standard Industrial Classification (SIC)", # Legend Label
       title = "Graduates Design and nature of work score by their SIC code",
       subtitle = "",
       caption = "")
graph6

#creating an interactive plot for graph1
ggplotly(graph1)

plot_creation_without_acyear <- function(df, colName) {
  ndf <- df %>%
    filter(!is.na(df[[colName]]))
  
  ndf %>% 
    group_by(ndf[[colName]]) %>%
    mutate(mean_danow = mean(danow, na.rm = TRUE)) %>% 
    ggplot() +
    geom_col(
      mapping = aes(x = 1,  # Specify a constant value for x
                    y = mean_danow,
                    fill = ndf[[colName]]),
      position = position_dodge(),
    ) +
    coord_cartesian(ylim = c(1, 5)) +
    scale_fill_manual(values = c("#1F4388",
                                 "#83C7BC",
                                 "#1E355E",
                                 "#6A86B8",
                                 "#A93439",
                                 "#CE3162",
                                 "#E57D3A",
                                 "#4EA585",
                                 "#BBB332",
                                 "#E8D77E")
    )  # adding HESA colours
}
Option_1 <- plot_creation_without_acyear(now_update2,"f_xwrk2020soc1")


#working to try and replicate the above function but with creating a plot_ly chart. The code is not working yet. This is to see if there is a difference in speed between the two options

plotly_creation <- function(df, colName) {
  ndf <- df %>%
    filter(!is.na(df[[colName]])) 
  ndf %>% 
    group_by(f_zcohort, ndf[[colName]]) %>%
    summarize(mean_danow = mean(danow, na.rm = TRUE)) %>% 
    plot_ly(
      data= {summary},
      x = ~f_zcohort,
      y = ~mean_danow,
      color = ~ndf[[colName]],
      type = "bar",
      showlegend = FALSE) %>%
    hide_colorbar() %>%
    suppressWarnings()
  
}
Graph1 <- plotly_creation(now_update2,"f_xwrk2020soc1") 

 color_palette <- c("#1F4388",
                    "#83C7BC",
                    "#1E355E",
                    "#6A86B8",
                    "#A93439",
                    "#CE3162",
                    "#E57D3A",
                    "#4EA585",
                    "#BBB332",
                    "#E8D77E")
#creating the chart by itself is working but not in the function, but we are missing a group in the chart?

 ndf_soc <- now_update2 %>% 
   group_by( f_xwrk2020soc1) %>%
      summarise(mean_danow = median(danow, na.rm = TRUE))

 Chart1 <- plot_ly(
   data = ndf_soc,
   x = ~f_xwrk2020soc1,
   y = ~mean_danow,
   color = ~f_xwrk2020soc1,
   type = "bar",
   showlegend = FALSE,
   marker = list(color = color_palette)
 ) %>%
   layout(
     yaxis = list(title = 'Mean Fairwork score'),
     xaxis = list(title = 'SOC group', tickvals = list("Managers, directors and senior officials", "Professional occupations", "Associate professional occupations", "Administrative and secretarial occupations", "Skilled trades occupations", "Caring, leisure and other service occupations", "Sales and customer service occupations", "Process, plant and machine operatives", "Elementary occupations")),
     title = "Mean Fairwork score by graduates SOC major group"
   ) %>%
   hide_colorbar() %>%
   suppressWarnings()
 
 
 #creatin the chart for SIC
 ndf_sic <- now_update2 %>% 
   group_by(f_xwrk2007sic1) %>%
   summarise(mean_danow = median(danow, na.rm = TRUE))
 
 Chart2 <- plot_ly(
   data= {ndf_sic},
   x = ~f_xwrk2007sic1,
   y = ~mean_danow,
   color = ~f_xwrk2007sic1,
   type = "bar",
   showlegend = FALSE,
   marker = list(color = color_palette)) %>%
   layout(
     yaxis = list(title = 'Mean Fairwork score'),
     xaxis = list(title = 'SIC Group',tickvals = list("Agriculture, forestry and fishing", "Mining and quarrying", "Manufacturing", "Electricity, gas, steam and air conditioning supply" , "Water supply; sewerage, waste management and remediation activities", "Construction" , "Wholesale and retail trade; repair of motor vehicles and motorcycles" , "Transportation and storage", "Accommodation and food service activities" , "Information and communication" , "Financial and insurance activities", "Real estate activities", "Professional, scientific and technical activities", "Administrative and support service activities", "Public administration and defence; compulsory social security", "Education", "Human health and social work activities", "Arts, entertainment and recreation", "Other service activities", "Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use", "Activities of extraterritorial organisations and bodies")),
     title = "Mean Fairwork score by graduates SIC major group") %>%
   hide_colorbar() %>%
   suppressWarnings()
 
 
 #creatin the chart for empbasis
 ndf_f_xempbasis <- now_update2 %>% 
   group_by( f_xempbasis) %>%
   summarise(mean_danow = median(danow, na.rm = TRUE))
 
 Chart3 <- plot_ly(
   data= {ndf_f_xempbasis},
   x = ~f_xempbasis,
   y = ~mean_danow,
   color = ~f_xempbasis,
   type = "bar",
   showlegend = FALSE,
   marker = list(color = color_palette)) %>%
   layout(
     yaxis = list(title = 'Mean Fairwork score'),
     xaxis = list(title = 'Employment Basis',tickvals = list("On a permanent/open ended contract", "On a fixed-term contract lasting 12 months or longer","On a fixed-term contract lasting less than 12 months","Temping (including supply teaching)","On a zero hours contract","Volunteering","On an internship","Other","Not known")),
     title = "Mean Fairwork score by graduates employment basis") %>%
   hide_colorbar() %>%
   suppressWarnings()
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 