library(ggplot2)
library(dplyr)
library(readr)
library(e1071)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

csv <- read.csv("surveyResult.csv")

# Curso é tendencioso para alguma área:
mean_curso_tendencioso = mean(csv$curso_tendencioso)
median_curso_tendencioso = median(csv$curso_tendencioso)
mode_curso_tendencioso = getmode(csv$curso_tendencioso)

# viés negativo, portando é assimétrica a esquerda:
# Moda > Mediana > Média
skewness(csv$curso_tendencioso)

# A base ensinada no curso é suficiente para iniciar em qualquer área:
mean_base_suficiente = mean(csv$base_suficiente)
median_base_suficiente = median(csv$base_suficiente)
mode_base_suficiente = getmode(csv$base_suficiente)

# viés negativo, portando é assimétrica a esquerda:
# Moda > Mediana > Média
skewness(csv$base_suficiente)

df = subset(csv, select = -c(data, ps_conheci_req_nao_ensinado, ps_conhe_req, proj_est_conhe_req_nao_ensinado, proj_est_conhe_req, area_mais_oportunidades, area_selec_eh_abordada_no_curso) )

# Curso é tendencioso para alguma área:
p = ggplot(df, aes(x=as.factor(curso_tendencioso)) ) +
  geom_bar() +
  theme(legend.position = "none") +
  labs(x = "Nível de tendência", y = "N° pessoas") +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5")) +
  
  geom_vline(xintercept = mean_curso_tendencioso, color = "red", size=0.5) +
  geom_text(aes(x=mean_curso_tendencioso, label="Média", y=15), colour="red", angle=90, vjust = 1.2) +
  
  geom_vline(xintercept = median_curso_tendencioso, color = "blue", size=1.5) +
  geom_text(aes(x=median_curso_tendencioso, label="Mediana", y=16), colour="blue", angle=90, vjust = 1.2) +
  
  geom_vline(xintercept = mode_curso_tendencioso, color = "green", size=0.5) +
  geom_text(aes(x=mode_curso_tendencioso, label="Moda", y=10), colour="green", angle=90, vjust = 1.2)

ggsave("bar_plot_curso_tendencioso.png", p, width = 6, height = 3)

# A base ensinada no curso é suficiente para iniciar em qualquer área:
p = ggplot(df, aes(x=as.factor(base_suficiente)) ) +
  geom_bar() +
  theme(legend.position = "none") +
  labs(x = "Nível de suficiência", y = "N° pessoas") +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5")) +

  geom_vline(xintercept = mean_base_suficiente, color = "red", size=0.5) +
  geom_text(aes(x=mean_base_suficiente, label="Média", y=15), colour="red", angle=90, vjust = 1.2) +

  geom_vline(xintercept = median_base_suficiente, color = "blue", size=1.5) +
  geom_text(aes(x=median_base_suficiente, label = "Mediana", y = 16), colour = "blue", angle = 90, vjust = 1.2) +
  
  geom_vline(xintercept = mode_base_suficiente, color = "green", size=0.5) +
  geom_text(aes(x = mode_base_suficiente, label = "Moda", y = 10), colour = "green", angle = 90, vjust = 1.2)

ggsave("bar_plot_base_suficiente.png", p, width = 6, height = 3)



p = ggplot(df, aes(x = periodo, y = curso_tendencioso, group = periodo) ) +
  geom_boxplot() +
  labs(x = "Periodo", y = "Nível de tendência do curso")

ggsave("tendencia_por_periodo.png", p, width = 6, height = 3)


p = ggplot(df, aes(x = periodo, y = base_suficiente, group = periodo) ) +
  geom_boxplot() +
  labs(x = "Periodo", y = "Nível de suficiência do curso")

ggsave("suficiencia_por_periodo.png", p, width = 6, height = 3)


# Desenvolvimento web é bem abordada no curso ?
df = subset(csv, select = -c(data, periodo, ps_conheci_req_nao_ensinado, ps_conhe_req, proj_est_conhe_req_nao_ensinado, proj_est_conhe_req, curso_tendencioso, base_suficiente))

des_web = subset(df, area_mais_oportunidades=="Desenvolvimento Web" & (area_selec_eh_abordada_no_curso=="Sim" | area_selec_eh_abordada_no_curso=="Não"))

p = ggplot(data = des_web, aes(x = area_selec_eh_abordada_no_curso)) + 
  geom_bar() +
  labs(x = "Desenvolvimento Web é bem abordada no curso ?", y = "N° pessoas")

ggsave("des_web.png", p, width = 6, height = 3)

# Conhecimento não abordado no curso por periodo(pessoas já formadas) ?
df = subset(csv, select = -c(data, ps_conhe_req, proj_est_conhe_req_nao_ensinado, proj_est_conhe_req, curso_tendencioso, base_suficiente, area_mais_oportunidades, area_selec_eh_abordada_no_curso))

per_formado = subset(df, periodo=="Já me formei")

p = ggplot(data = per_formado, aes(x = ps_conheci_req_nao_ensinado)) + 
  geom_bar() +
  labs(x = "Pessoas formadas já precisaram de algo que não foi abordado no curso ?", y = "N° pessoas")

ggsave("per_formadas.png", p, width = 6, height = 3)


# Conhecimento não abordado no curso por periodo(pessoas 7º período ou posterior) ?
df = subset(csv, select = -c(data, ps_conhe_req, proj_est_conhe_req_nao_ensinado, proj_est_conhe_req, curso_tendencioso, base_suficiente, area_mais_oportunidades, area_selec_eh_abordada_no_curso))

per_7_posterior = subset(df, periodo=="7º período ou posterior")

p = ggplot(data = per_7_posterior, aes(x = ps_conheci_req_nao_ensinado)) + 
  geom_bar() +
  labs(x = "Pessoas no 7º periodo ou posterior já precisaram de algo que não foi abordado no curso ?", y = "N° pessoas")

ggsave("per_7_per_posterior.png", p, width = 6, height = 3)


# Conhecimento não abordado no curso por periodo(pessoas 4º período ou anterior) ?
df = subset(csv, select = -c(data, ps_conhe_req, proj_est_conhe_req_nao_ensinado, proj_est_conhe_req, curso_tendencioso, base_suficiente, area_mais_oportunidades, area_selec_eh_abordada_no_curso))

per_4_anterior = subset(df, periodo=="4º período ou anterior")

p = ggplot(data = per_4_anterior, aes(x = ps_conheci_req_nao_ensinado)) + 
  geom_bar() +
  labs(x = "Pessoas no 4º periodo ou anterior já precisaram de algo que não foi abordado no curso ?", y = "N° pessoas")

ggsave("per_4_anterior.png", p, width = 6, height = 3)


# Conhecimento não abordado no curso por periodo(pessoas 6º período) ?
df = subset(csv, select = -c(data, ps_conhe_req, proj_est_conhe_req_nao_ensinado, proj_est_conhe_req, curso_tendencioso, base_suficiente, area_mais_oportunidades, area_selec_eh_abordada_no_curso))

per_6 = subset(df, periodo=="6º período")

p = ggplot(data = per_6, aes(x = ps_conheci_req_nao_ensinado)) + 
  geom_bar() +
  labs(x = "Pessoas no 6º período já precisaram de algo que não foi abordado no curso ?", y = "N° pessoas")

ggsave("per_6.png", p, width = 6, height = 3)


# Conhecimento não abordado no curso por periodo(pessoas 5º período) ?
df = subset(csv, select = -c(data, ps_conhe_req, proj_est_conhe_req_nao_ensinado, proj_est_conhe_req, curso_tendencioso, base_suficiente, area_mais_oportunidades, area_selec_eh_abordada_no_curso))

per_5 = subset(df, periodo=="5º período")

p = ggplot(data = per_5, aes(x = ps_conheci_req_nao_ensinado)) + 
  geom_bar() +
  labs(x = "Pessoas no 5º período já precisaram de algo que não foi abordado no curso ?", y = "N° pessoas")

ggsave("per_5.png", p, width = 6, height = 3)
