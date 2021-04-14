### ---
### Load necessary packages
### ---

library(dplyr)
library(multicon)
library(plotrix)
library(reshape2)
library(magrittr)

### ---
### Let's look at employee self-report
### about development opportunities in the
### Tempe, Arizona government (open data)
### ---

### ---
### First, read in the data
### ---

df1 <- read.csv("city-of-tempe-2016-employee-survey-data-4.csv")

glimpse(df1)

### ---
### Consult data dictionary to find questions
### pertaining to development:
### Looks like 2 - 8, with 1 being ID
### 73 being the department variable
### ---

df2 <- df1 %>%
  select(1:8, 73)

### ---
### Exclude employees who answered "I don't know" to development questions
### ---

df2 <- df2[df2$Q1..I.receive.training.to.do.my.job.effec != 9 & 
      df2$Q2..There.is.someone.at.work.who.encourag != 9 &  
      df2$Q3..I.have.been.mentored.at.work != 9 &  
      df2$Q4..I.have.received.fair.consideration.fo != 9 &  
      df2$Q5..I.am.aware.of.the.City.s.educational != 9 &  
      df2$Q6..The.City.s.programs.related.to.profes != 9 & 
      df2$Q7..Overall..I.am.satisfied.with.the.prof != 9,]

### ---
### Select training variables only and make composite
### ---

df3 <- df2 %>%
  select(2:8)

develop_comp = composite(df3)

### ---
### Create dataframe with department and
### composite development variable and
### rename department variable for ease of use
### ---

df4 <- data.frame(develop_comp, df2$department)

df4 <- df4 %>%
  rename(department = df2.department)

### ---
### Use sample sizes in each department 
### to find corresponding department names
### from data dictionary
### ---

df4 <- df3 %>%
  group_by(department) %>%
  filter(n() > 50) %>%
  count()

### ---
### Rename department levels using
### data dictionary
### ---

df4$department <- recode(df4$department,
                         "6" = "Community Development",
                         "9" = "Community Services",
                         "12" = "Internal Services",
                         "13" = "Police",
                         "14" = "Public Works",
                         "99" = "Other")

### ---
### Filter out NA variables, 
### group by department, and
### get average development satisfaction for
### each department
### ---

df5 <- df4 %>%
  filter(!is.na(department)) %>%
  group_by(department) %>%
  summarise(dept_mean_composite = (mean(develop_comp)))

### ---
### Make a barplot with error bars (SE)
### from summarized data 
### ---

ggplot(df5, aes(department, dept_mean_composite, 
                ymin = dept_mean_composite-std.error(dept_mean_composite), 
                ymax = dept_mean_composite+std.error(dept_mean_composite))) +
  geom_col(fill = "blue") +
  geom_errorbar() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 5)) +
  xlab("Department") +
  ylab("Composite Satisfaction with Development Opportunities")


### ---
### Create plots with facet_grid,
### to capture variables separately
### rather than as composite...
### ---

df2.2 <- df2 %>%
  select(1:9)

df2.2 <- df2.2 %>%
  rename(department = Q74..In.which.department.do.you.currently) %>%
  select(2:9)

df2.2 <- df2.2 %>%
  group_by(department) %>%
  filter(n() > 50)

df2.2 <- df2.2 %>%
  filter(!is.na(department)) %>%
  filter(department != "7") %>%
  filter(department != "99") %>%
  group_by(department) %>%
  summarise(across(everything(), list(mean)))

df2.2$department <- recode(df2.2$department,
                         "6" = "Community Development",
                         "9" = "Community Services",
                         "12" = "Internal Services",
                         "13" = "Police",
                         "14" = "Public Works",
                         "99" = "Other")


df_melted <- melt(df2.2, value.name = "value", id.vars = "department")

ggplot(data = df_melted, aes(x = department, y = value)) + 
  geom_col() + 
  facet_wrap(~variable) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 5)) +
  xlab("Department") +
  ylab("Composite Satisfaction with Development Opportunities") +
  theme(axis.text.x = element_text(angle = 90))



