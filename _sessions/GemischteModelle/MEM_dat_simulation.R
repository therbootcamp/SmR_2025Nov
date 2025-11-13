# code from here: http://rstudio-pubs-static.s3.amazonaws.com/133409_8567e08df08d42a8977853e4f94ca005.html

require(lme4)
require(ggplot2)

form<-as.formula(c("~State+(State|ID) + (State|Movie)"))

set.seed(113)
# number of individuals
N.ind<-200
N.obs<-30
Movies <- 15
simdat <-data.frame(ID = factor(rep(1:N.ind,each = N.obs)),
                    Movie = factor(rep(paste0("M", 1:Movies), N.ind*2)),
                    State = factor(rep(c("Sober", "Drunk"), each=Movies,
                                       times = N.ind), levels = c("Sober", "Drunk")))

beta<-c(30, 26)
names(beta)<-c("(Intercept)", "StateDrunk")


# Subject random variance
V.ind0 <- 22
V.inde <- 45

# Movie random variance
V.m.0 <- 25
V.m.e <- 32

# Error variance  
V.err0 <- 52

# Covariances
COVind0.ind1e <- 0.023 * sqrt(V.ind0*V.inde)
COVm0.m1e <- 0.058 * sqrt(V.m.0 * V.m.e)

vcov1<-matrix(c(V.ind0,COVind0.ind1e,
                COVind0.ind1e,V.inde), 2, 2)

vcov2<-matrix(c(V.m.0, COVm0.m1e,
                COVm0.m1e, V.m.e), 2, 2)

theta<-c((chol(vcov1)/sqrt(V.err0))[1,1],
         (chol(vcov1)/sqrt(V.err0))[1,2],
         (chol(vcov1)/sqrt(V.err0))[2,2],
         (chol(vcov2)/sqrt(V.err0))[1,1],
         (chol(vcov2)/sqrt(V.err0))[1,2],
         (chol(vcov2)/sqrt(V.err0))[2,2])
names(theta)<-c("ID.(Intercept)","ID.StateDrunk.(Intercept)","ID.StateDrunk",
                "Movie.(Intercept)","Movie.StateDrunk.(Intercept)","Movie.StateDrunk")




set.seed(25)
response <- simulate(form, newdata = simdat, family = gaussian,
                     newparams = list(theta = theta, beta = beta,
                                      sigma = sqrt(V.err0)))
simdat$Tomatometer <- as.vector(response[,1])

# write_csv(simdat, "_sessions/MixedModels/Tomatometer_dat.csv")
# saveRDS(simdat, "_sessions/MixedModels/Tomatometer_dat.RDS")
simdat <- read_csv("_sessions/GemischteModelle/1_Data/tomatometer.csv") %>%
  mutate(zustand = factor(zustand, levels = c("nuechtern", "betrunken"), labels = c("Nüchtern", "Betrunken")))
model1<-lmer(tomatometer ~ zustand + (zustand|id) + (zustand|film), data=simdat)
summary(model1)

m_line <- fixef(model1)

# visualize model fit
fittednorms<-ranef(model1)$id[,1:2]
fittednorms[,1]<-fittednorms[,1]+getME(model1,"beta")[1]
fittednorms[,2]<-fittednorms[,2]+getME(model1,"beta")[2]
colnames(fittednorms)<-c("intercept","slope")

set.seed(25)
rand10<-sample(1:N.ind, 15, replace = FALSE)

plot1<-ggplot(simdat,aes(zustand, tomatometer))+
  geom_point(colour= "#606061", alpha = .15, size = 2.5)+
  # geom_segment(aes(x = 1, y = intercept,xend=2, yend=intercept+slope ),
  #              data=fittednorms,colour = "gray85", size = 1)+
  geom_segment(aes(x = 1, y = intercept,xend=2, yend=intercept+slope ),
               data=fittednorms[rand10,],colour = "#EA4B68", size = 1.5,
               alpha = .8)+
  geom_segment(aes(x = 1, y = m_line[1], xend = 2, yend = sum(m_line)),
               data=fittednorms[rand10,],colour = "black", size = 2,
               alpha = 1)+
  labs(x = "Zustand", y = "Tomatometer") +
  theme(axis.title.x=element_text(vjust=-1),axis.title.y=element_text(vjust=1)) +
  theme_bw() +
  ylim(0, 100) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18,face = "bold")
  )

plot1

ggsave("_sessions/GemischteModelle/image/MEM_example.png", width = 8,
       height = 5, device = "png", dpi = 600)


model1<-lmer(tomatometer ~ zustand + (1|id) + (1|film), data=simdat)
summary(model1)

m_line <- fixef(model1)

# visualize model fit
fittednorms<-ranef(model1)$id
fittednorms<-fittednorms+getME(model1,"beta")[1]
fittednorms$slope<-getME(model1,"beta")[2]
colnames(fittednorms)<-c("intercept","slope")

N.ind<-200
set.seed(25)
rand10<-sample(1:N.ind, 25, replace = FALSE)

plot1<-ggplot(simdat,aes(zustand, tomatometer))+
  geom_point(colour= "#606061", alpha = .15, size = 2.5)+
  # geom_segment(aes(x = 1, y = intercept,xend=2, yend=intercept+slope ),
  #              data=fittednorms,colour = "gray85", size = 1)+
  geom_segment(aes(x = 1, y = intercept,xend=2, yend=intercept+slope ),
               data=fittednorms[rand10,],colour = "#EA4B68", size = 1.5,
               alpha = .8)+
  geom_segment(aes(x = 1, y = m_line[1], xend = 2, yend = sum(m_line)),
               data=fittednorms[rand10,],colour = "black", size = 2,
               alpha = 1)+
  labs(x = "Zustand", y = "Tomatometer") +
  theme(axis.title.x=element_text(vjust=-1),axis.title.y=element_text(vjust=1)) +
  theme_bw() +
  ylim(0, 100) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18,face = "bold")
  )

plot1

ggsave("_sessions/GemischteModelle/image/RI_example.png", width = 8,
       height = 5, device = "png", dpi = 600)




plot2<-ggplot(simdat %>% mutate(State = case_when(State == "Sober" ~ "Condition 1",
                                                  TRUE ~ "Condition 2")),aes(State, Tomatometer))+
  geom_point(colour= "#606061", alpha = .15, size = 2.5)+
  # geom_segment(aes(x = 1, y = intercept,xend=2, yend=intercept+slope ),
  #              data=fittednorms,colour = "gray85", size = 1)+
  geom_segment(aes(x = 1, y = intercept,xend=2, yend=intercept+slope ),
               data=fittednorms[rand10,],colour = "#EA4B68", size = 1.5,
               alpha = .8)+
  geom_segment(aes(x = 1, y = m_line[1], xend = 2, yend = sum(m_line)),
               data=fittednorms[rand10,],colour = "black", size = 2,
               alpha = 1)+
  theme(axis.title.x=element_text(vjust=-1),axis.title.y=element_text(vjust=1)) +
  theme_classic() +
  ylim(0, 100) +
  labs(
    x = "",
    y = ""
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 18,face = "bold"),
    axis.line = element_line(size = 1.2)
  )

plot2

ggsave("_sessions/MixedModels/image/MEM_example2.png", width = 8,
       height = 5, device = "png", dpi = 600)
