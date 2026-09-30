source("C:/Users/PaddyK/Desktop/Random_r_Code/src/setup.R")

###Using the morphine example dataset
mental <- rio::import("C:/Users/PaddyK/Desktop/MSc Medical Statistics/Stat_Models_Discrete/Practicals/Session1/mental.dta")
summary(mental)
statar::sum_up(mental)
View(mental)

##simple linear
mentactmod1 <- lm(mentact~factor(treat), data=subset(mental, treat<3)) ##Treat has values 1, 2 and 3. We want to look at placebo vs morph only
summary(mentactmod1)
##interact
mentactInter <- lm(mentact~factor(treat)*prement, data=subset(mental, treat<3))
summary(mentactInter)
anova(mentactInter) ##this will hep us see the ANOVA table of the model like in STAT


##GLMS
#Gaussian: we can use the model from above to be fit as a GLM
mentactLinGLM <- glm(mentact~factor(treat)*prement,family="gaussian" ,data=subset(mental, treat<3))
summary(mentactLinGLM)

##PRACTICAL
#Question 1
statar::sum_up(mental)
mental %>% tabyl(treat)
mental$treat <- factor(mental$treat)

##Distributions
mental %>% ggplot(aes(x=mentact))
ggplot(mental, aes(x=mentact)) + geom_histogram(aes(y=..density..),bins = 8) + facet_wrap(~factor(treat))
ggplot(mental, aes(x=prement)) + geom_histogram(bins = 8) + facet_wrap(~factor(treat))
ggplot(data = mental, 
       aes(x = prement, fill = treat)) + 
  geom_density(alpha = 0.3)

ggplot(data = mental, 
       aes(x = mentact)) + 
  geom_density(alpha = 0.3) 
ggplot(data = mental, 
       aes(y = mentact, x=treat ,fill = treat)) + 
  geom_boxplot(alpha = 0.3)

ggplot(data = mental, 
       aes(y = mentact, x=prement ,color = treat)) + 
  geom_point()


mental_sum <- mental %>% 
  group_by(factor(treat)) %>% 
  summarise(
    across(c(mentact,prement),
    list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    q25 = ~quantile(.x, 0.25, na.rm = TRUE),
    q75 = ~quantile(.x, 0.75, na.rm = TRUE),
    count = ~sum(!is.na(.x))
    )
    )
  )

mental %>% 
    summarise(
    across(c(mentact,prement),
           list(
             mean = ~mean(.x, na.rm = TRUE),
             sd = ~sd(.x, na.rm = TRUE),
             min = ~min(.x, na.rm = TRUE),
             max = ~max(.x, na.rm = TRUE),
             median = ~median(.x, na.rm = TRUE),
             q25 = ~quantile(.x, 0.25, na.rm = TRUE),
             q75 = ~quantile(.x, 0.75, na.rm = TRUE),
             count = ~sum(!is.na(.x))
           )
    )
  )
mental %>% skimr::skim(prement, mentact)

###Linear regression may not be very necessary due to the non-normal nature of the mental activity variable (outcome)
## The mentact variable is highly skewed within groups

##Question 2
##Algebraic form of the models
















