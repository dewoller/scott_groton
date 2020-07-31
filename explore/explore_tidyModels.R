
source('_drake.R')
loadd( df_in_long )

#t1_names %>% clipr::write_clip()

df_in_long %>%
  distinct(type)

df_in_long %>%
  filter( type == 'dur') %>%
  { . } -> df_dur

library(lme4)


model1 <- lmer(value ~ cohort_age + (1 | cohort), data=df_dur)
summary(model1)
anova(model1)

model2 <- lmer(value ~ cohort_age + (1 | id), data=df_dur)
summary(model2)
anova(model1, model2)

model3 <- lmer(value ~ cohort_age + ( cohort_age | id), data=df_dur)
summary(model3)
anova(model1, model2, model3)

model4 <- lmer(value ~ cohort_age + ( 0+cohort_age | id), data=df_dur)
summary(model4)
anova(model1, model2, model3, model4)

model5 <- lmer(value ~ cohort_age + (1|id) + ( 0+cohort_age | id), data=df_dur)
summary(model5)
anova(model1, model2, model3, model4, model5)

summary(model)
anova(model)


library(nlme)



mahal = mahalanobis(
            df_dur[,c(5,7)],
            colMeans(df_dur[,c(5,7)], na.rm=TRUE),
            cov( df_dur[,c(5,7)], use='pairwise.complete.obs')
            )

cutoff = qchisq( 1-.001, ncol( df_dur[,c(5,7)] ))

cutoff
summary( mahal <cutoff)


df_no_out = df_dur[mahal <cutoff, ]


df_no_out


coorelation = cor( df_no_out[,c(5,7)], use='pairwise.complete.obs')

symnum( coorelation)

coorelation


# models

model1 = gls( value ~ 1,
    data = df_no_out,
method = 'ML',
na.action='na.omit')
summary(model1)



model1 = gls( value ~ 1,
             data = df_no_out,
             method = 'ML',
             na.action='na.omit')


model2 = lme( value ~ 1,
             data = df_no_out,
             method = 'ML',
             na.action='na.omit',
             random=~1|id)

anova(model1, model2)

model2.1 = lme( value ~ 1,
               data = df_no_out,
               method = 'ML',
               na.action='na.omit',
               random=list( ~1|id, ~1|cohort_age))

anova(model1, model2, model2.1)

model2.2 = lme( value ~ 1 + period,
             data = df_no_out,
             method = 'ML',
             na.action='na.omit',
             random=list( ~1|id, ~1|cohort_age))

anova(model1, model2, model2.1, model2.2)


models = tribble( ~model_name, ~formula,
                 "M1", as.formula(value ~ 1 + period + ( 1|id) ),
                 "M2", as.formula(value ~ 1 + period + ( 1|id) + (1|cohort_age) ),
                 "M3", as.formula(value ~ 1 + cohort*cohort_age + ( 1|id) + (1|cohort_age)) ,
                 "Mpaper.1", as.formula(value ~ 1 + cohort*cohort_age + ( 1+cohort_age|id)  ),
                 "Mpaper.2", as.formula(value ~ 1 + cohort*cohort_age + poly(cohort_age, degree=2, raw=TRUE) + ( 1+cohort_age|id)) ,
                 "Mpaper.3", as.formula(value ~ 1 + cohort*cohort_age + poly(cohort_age, degree=3, raw=TRUE) + ( 1+cohort_age|id)) ,
)


models %>%
  mutate( rv = map2(models$model_name, models$formula, function(model_name,formula) {
      lmer( formula , data = df_no_out, REML=FALSE ) })) %>%
with(.s
     do.call(anova, 
             lapply(names(rv), as.name)))

map(models, function(name) {
      browser()
      paste(name, collapse='-') })

ranova( M23)
anova( M21, M22, M23, M24_paper, M24_paper.1, M24_paper.1.p2, M24_paper.1.p3)

tidy(M23)
tidy(M23, effects = "fixed")
tidy(M23, conf.int = TRUE)
tidy(M23, effects = "ran_pars")
tidy(M23, effects = "ran_vals")
tidy(M23, effects = "ran_coefs")
tibble(augment(M23, df_no_out))
glance(M23)

library(lmerTest)
conflict_prefer("lmer", "lme4")



library('broom.mixed')




data("sleepstudy", package="lme4")
lmm1 <- lme(Reaction ~ Days, random=~ Days|Subject, sleepstudy)
tidy(lmm1)
tidy(lmm1, effects = "fixed")
tidy(lmm1, conf.int = TRUE)
tidy(lmm1, effects = "ran_pars")
tidy(lmm1, effects = "ran_vals")
tidy(lmm1, effects = "ran_coefs")
head(augment(lmm1, sleepstudy))
glance(lmm1)


prefix='cog_gml'

df_in %>%
  select(-starts_with(prefix)) %>%
  arrange(cohort)

#Total errors = CogGMLTER
#Rule break errors = CogGMLRER
#Duration? = CogGMLDUR

df_in %>%
  select(cohort, id, starts_with( prefix )) %>%
  pivot_longer( cols=starts_with( 'cog_gml' ),
                names_to=c('type','period') ,
                names_pattern="cog_gml(...)_t(.)",
                values_to='value',
                values_drop_na=TRUE ) %>%
  mutate( id = str_trim(id)) %>%
  mutate( period = as.numeric(period)) %>%
  { . } -> df_in_long

df_in_long %>%
  distinct( type ) %>%
  map( type, graph1 )


graph_type='dur'

graph1 = function( graph_type ) {

  df_in_long %>%
    filter( type == graph_type ) %>%
    group_by( cohort, period) %>%
    summarise( value = mean( value), n = n()) %>%
    ungroup() %>%
    mutate( cohort_period = cohort + period - 1 ) %>%
    mutate( cohort = as.factor(cohort)) %>%
    ggplot( aes( cohort_period, value, color=cohort   )) +
    geom_line() +
    geom_point(aes(size=n)) +
    ggtitle( paste('Average', type, 'by cohort ')) %>%
    { . } -> p
  print(p)

  df_in_long %>%
    filter( type == graph_type ) %>%
    mutate( cohort_period = cohort + period - 1 ) %>%
    group_by( cohort_period) %>%
    summarise( value = mean( value), n = n()) %>%
    ungroup() %>%
    ggplot( aes( cohort_period, value)) +
    geom_line() +
    geom_point(aes(size=n)) +
    ggtitle( paste('Average', prefix, '')) %>%
    { . } -> p
  print(p)

  df_in_long %>%
    filter( type == graph_type ) %>%
    mutate( cohort_period = as.factor(cohort + period - 1 )) %>%
    ggplot( aes( cohort_period, value)) +
    geom_boxplot() +
    ggtitle( paste('Average', type, '')) %>%
    { . } -> p
  print(p)

}







