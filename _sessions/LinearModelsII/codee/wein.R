




lm(formula = Qualität ~ Farbe_effekt, 
   data = wein)

wein = read_csv('1_Data/wein.csv')

skaliere = function(x) (x - mean(x))/sd(x)

a = wein %>% select(Qualität, Alkohol, Farbe) %>% 
  mutate(Farbe = as.numeric(Farbe == 'weiss'), 
         interakt = Farbe * Alkohol)

b = wein %>% select(Qualität, Alkohol, Farbe) %>% 
  mutate(Farbe = as.numeric(Farbe == 'weiss'), 
         Farbe = Farbe,
         Alkohol = skaliere(Alkohol),
         interakt = Farbe * Alkohol)

summary(lm(Qualität~Alkohol+Farbe+interakt, data = a))
summary(lm(Qualität~Alkohol+Farbe+interakt, data = b))

summary(lm(Alkohol~Farbe+interakt, data = a))
summary(lm(Alkohol~Farbe+interakt, data = b))

summary(lm(Farbe~Alkohol+interakt, data = a))
summary(lm(Farbe~Alkohol+interakt, data = b))


plot(a$Alkohol, a$interakt)


cor(b)

plot(a$Farbe, a$interakt)
plot(a$Alkohol, a$interakt)

plot(b$Alkohol, b$interakt)

summary(glm(formula = Qualität ~ Farbe * Dichte, data = wein))
summary(glm(formula = Qualität ~ Farbe * Alkohol, data = wein))
summary(glm(formula = Qualität ~ Farbe * Alkohol, data = wein))


summary(lm(formula = Qualität ~ Farbe * Alkohol, data = wein %>% mutate_if(is.numeric, scale)))


wein$Dichte


boxplot(wein)
