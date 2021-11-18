setwd("~/Doutorado/PED - CE342/Aulas/Macro/Slides_Aula_Macro")

source('Pacotes.R', verbose = F)

#######################################################################
#                                                                     #
#                     MANIPULAÇÃO E LIMPEZA DOS DADOS                 #
#                                                                     #
#######################################################################

# Começar os dados em 1999/08 e terminar em 2021/10, se possível

# IPCA - Cheio ####
ipca_cheio <- read.csv("Dados/IPCA_cheio.csv", sep = ';', dec = ',', header = F)
ipca_cheio <- as.xts(ts((ipca_cheio[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))

# Metas de inflação - Anual ####
metas <- read.csv("Dados/Metasdadosanual.csv", dec = ',')
metas[2:5] <- metas[2:5] * 100
metas

# Metas de inflação - Mensal ####
metas_mensal <- read.csv("Dados/Metasdadosmensal.csv", sep = ';', dec = ',')
metas_mensal[2:4] <- metas_mensal[2:4] * 100
metas_mensal

# IPCA e tradables/nontradables/monitorados ####

IPCA_decomp <- read.csv("Dados/IPCA_decomp.csv", sep = ',', dec = ',', header = T)

IPCA_decomp <- tibble(IPCA_decomp) %>%
  mutate(across(!Data, ~(as.numeric(.)/100)+1)) %>%
  mutate(across(!Data, ~ round((RcppRoll::roll_prodr(., n = 12, na.rm = F, fill = NA)-1)*100,2)))
glimpse(IPCA_decomp)

IPCA_ndur <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),2]), start = c(1999,8), end = c(2021,10), frequency = 12))
IPCA_semidur <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),3]), start = c(1999,8), end = c(2021,10), frequency = 12))
IPCA_dur <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),4]), start = c(1999,8), end = c(2021,10), frequency = 12))
IPCA_servicos <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),5]), start = c(1999,8), end = c(2021,10), frequency = 12))
IPCA_trad <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),6]), start = c(1999,8), end = c(2021,10), frequency = 12))
IPCA_monit <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),7]), start = c(1999,8), end = c(2021,10), frequency = 12))
IPCA_nontrad <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),8]), start = c(1999,8), end = c(2021,10), frequency = 12))

# Cambio ####
cambio <- read.csv("Dados/cambio.csv", sep = ';', dec = ',', header = T)
cambio <- as.xts(ts((cambio[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))

# Commodities ####
ibc <- read.csv("Dados/commodities.csv", sep = ';', dec = ',', header = T)
ibc_total <- as.xts(ts((ibc[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))
ibc_agro <- as.xts(ts((ibc[,3]), start = c(1999,8), end = c(2021,10), frequency = 12))
ibc_metal <- as.xts(ts((ibc[,4]), start = c(1999,8), end = c(2021,10), frequency = 12))
ibc_energia <- as.xts(ts((ibc[,5]), start = c(1999,8), end = c(2021,10), frequency = 12))

ibc_df <- reshape2::melt(ibc, id.vars = "Data")
ibc_df$Data <- seq(as.Date("1999-08-1"), as.Date("2021-10-1"), by = "month")


#ggplot(ibc_df) + geom_line(aes(x=Data,y=value,color=variable,linetype=variable), size = 1)

# Selic - Meta ####

selic <- read.csv("Dados/selic.csv")
selic <- as.xts(ts((selic[38:nrow(selic),2]), start = c(1999,8), end = c(2021,10), frequency = 12))
plot(selic)

# EMBI+ ####

embi <- read.csv("Dados/EMBI.csv")
embi
embi <- as.xts(ts((embi[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))
plot(embi)

# FED rates ####

fed_rate <- read.csv("Dados/FED_rates.csv")
fed_rate
fed_rate <- as.xts(ts((fed_rate[152:nrow(fed_rate),2]), start = c(1999,8), end = c(2021,10), frequency = 12))
plot(fed_rate)

# PIB ####

pib <- read.csv("Dados/PIB.csv")
pib
pib <- as.xts(ts((pib[11:nrow(pib),2]), start = c(1999,3), end = c(2021,2), frequency = 4))
plot(pib)

# hiato_pib ####

hiato_pib <- read_excel("Dados/hiato_PIB.xlsx")
hiato_pib <- as.xts(ts((hiato_pib[23:nrow(hiato_pib),4]), start = c(1999,3), end = c(2021,2), frequency = 4))
plot(hiato_pib)

# VIX  ####
vix <- read.csv("Dados/VIX.csv",dec = ',')
vix
# vix <- as.xts(ts((vix[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))
# plot(vix)

# MSCI ####
MSCI <- read.csv("Dados/MSCI.csv", dec = ',')
MSCI
# MSCI <- as.xts(ts((MSCI[,2]), start = c(2004,8), end = c(2021,10), frequency = 12))
# plot(MSCI)

#######################################################################
#                                                                     #
#                     GRÁFICOS                                        #
#                                                                     #
#######################################################################

# Criando diretórios ####
# periodos_aula <- c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)','Agregado')
# 
# for (i in periodos_aula) {
#   ifelse(!dir.exists(file.path(i)),
#          dir.create(file.path(i)),
#          FALSE)
# }

# Gráficos de RMI ####

date <- seq(as.Date("1999-08-1"), as.Date("2021-10-1"), by = "month")

df_metas <- tibble(date,metas_mensal[,2],metas_mensal[,3],metas_mensal[,4],metas_mensal[,5], 
             .name_repair = ~ c('date','Meta','Piso','Teto','IPCA'))

# rects_mensal <- data.frame(xstart = c(1999,2004,2011,2015,2017,2019),
#                     xend = c(2004,2011,2015,2017,2019,2021),
#                     col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)'))


rects_mensal <- data.frame(xstart = c(as.Date("1999-08-1"),as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1")),
                           xend = c(as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1"),as.Date( "2021-10-1")),
                           col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)'))

metas_mensalplot <- ggplot(df_metas)  + 
  geom_ribbon(aes(x=date, ymin=Meta, ymax =Teto), fill = "GREY80") +
  geom_ribbon(aes(x=date, ymin=Piso, ymax =Meta), fill = "GREY80") +
  geom_rect(data = rects_mensal, aes(xmin = xstart, xmax = xend, ymin = min(df_metas$Piso)-1, ymax = max(df_metas$IPCA)+1, fill = col), alpha = 0.4) +
  geom_line(aes(x=date, y=Piso), color = "GREY70") +
  geom_line(aes(x=date, y=Teto), color = "GREY70") +
  geom_line(aes(x=date, y=Meta), color = "GREY50") +
  geom_line(aes(x=date,y=IPCA), size = 1, color = 'darkred') +
  scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6),expand = c(0,0)) +
  scale_x_date(date_breaks = '1 year', date_minor_breaks = '6 months', date_labels = "%y",expand = c(0, 0.02)) +
  # scale_x_continuous(breaks=seq(1999,2021,1),                       
  #                     labels=paste(c(""),rep(1999:2021,each=1)),expand = c(0.012, 0.02)) +
  labs(title = 'Taxa de Inflação acumulada em 12 meses e bandas do Regime de Metas da Inflação.') +
  scale_fill_brewer(palette="Blues", name = 'Períodos') +
  guides(fill = "none") +
  ylab('') +
  xlab('') +
  theme_classic() +
  theme(  panel.grid = element_blank(), 
          panel.border = element_blank(),
          legend.position="right",
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          legend.key = element_rect(colour = "black"),
          legend.box.background = element_rect(colour = "black", size = 1),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
          axis.text.y = element_text(size=10)) 

metas_mensalplot
p <- ggplotly(metas_mensalplot) %>%
  layout(legend = list(orientation = "h", x = 0, y =-0.1))
p
# For loop para remover legendas
for (i in 1:8) {
  p$x$data[[i]]$showlegend <- F
  
}
p_metas <- p


# Tradables, Nontradables e Monitorados ####

date <- seq(as.Date("1999-08-1"), as.Date("2021-10-1"), by = "month")

df <- tibble(date,IPCA_trad, IPCA_nontrad,IPCA_monit,df_metas[,5])

#,             .name_repair = ~ c('date','Tradables','Nontradables','Monitorados','IPCA'))
# df %>%
#   mutate(across(!date,as.numeric))
colnames(df) <- c('date','Tradables','Nontradables','Monitorados','IPCA')
df

rects_mensal <- data.frame(xstart = c(as.Date("1999-08-1"),as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1")),
                           xend = c(as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1"),as.Date( "2021-10-1")),
                           col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)'))

df %>%
  mutate(across(!date,as.numeric))
df <- reshape2::melt(df, id.vars= 'date')
df

IPCA_BCB_plot <- ggplot(df)  + 
  geom_ribbon(data = df_metas, aes(x=date, ymin=Piso, ymax =Teto), fill = "GREY80") +
  #geom_ribbon(data= df_metas, aes(x=date, ymin=Piso, ymax =Meta), fill = "GREY80") +
  geom_rect(data = rects_mensal, aes(xmin = xstart, xmax = xend, ymin = min(df$value)-1, ymax = max(df$value)+1, fill = col), alpha = 0.4) +
  geom_line(aes(x=date, y=value, color=variable, linetype = variable), size = 1) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6), expand = c(0,0)) +
  scale_x_date(date_breaks = '1 year', date_minor_breaks = '6 months', date_labels = "%y",expand = c(0, 0.02)) +
  # scale_x_continuous(breaks=seq(1999,2021,1),                       
  #                     labels=paste(c(""),rep(1999:2021,each=1)),expand = c(0.012, 0.02)) +
  labs(title = 'Decomposição do IPCA') +
  scale_fill_brewer(palette="Blues", name = 'Períodos') +
  scale_color_brewer(palette="Set1", name = 'Períodos') +
  guides(linetype = "none", fill = 'none') +
  ylab('') +
  xlab('') +
  theme_classic() +
  theme(  panel.grid = element_blank(), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          legend.key = element_rect(colour = "black"),
          legend.box.background = element_rect(colour = "black", size = 1),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
          axis.text.y = element_text(size=10)) 

IPCA_BCB_plot

p <- ggplotly(IPCA_BCB_plot) %>%
  layout(legend = list(orientation = "h", x = 0, y =-0.1))

# For loop para remover legendas
for (i in 2:7) {
  p$x$data[[i]]$showlegend <- F
  
}
p_IPCA_BCB <- p

# IPCA Industrializados e Serviços ####


date <- seq(as.Date("1999-08-1"), as.Date("2021-10-1"), by = "month")

df <- tibble(date,IPCA_dur, IPCA_semidur,IPCA_ndur,IPCA_servicos,df_metas[,5],
             .name_repair = ~ c('date','Duráveis','Semiduráveis','Não duráveis','Serviços','IPCA'))

rects_mensal <- data.frame(xstart = c(as.Date("1999-08-1"),as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1")),
                           xend = c(as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1"),as.Date( "2021-10-1")),
                           col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)'))

df <- reshape2::melt(df, id.vars= 'date')
df

IPCA_Ind_plot <- ggplot(df)  + 
  geom_ribbon(data = df_metas, aes(x=date, ymin=Piso, ymax =Teto), fill = "GREY80") +
  geom_rect(data = rects_mensal, aes(xmin = xstart, xmax = xend, ymin = min(df$value)-1, ymax = max(df$value)+1, fill = col), alpha = 0.4) +
  geom_line(aes(x=date, y=value, color=variable, linetype = variable), size = 1) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6), expand = c(0,0)) +
  scale_x_date(date_breaks = '1 year', date_minor_breaks = '6 months', date_labels = "%y",expand = c(0, 0.02)) +
  labs(title = 'Decomposição do IPCA - Industrializados e Serviços') +
  scale_fill_brewer(palette="Blues", name = 'Períodos') +
  scale_color_brewer(palette="Set1", name = 'Períodos') +
  guides(linetype = "none", fill = 'none') +
  ylab('') +
  xlab('') +
  theme_classic() +
  theme(  panel.grid = element_blank(), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          legend.key = element_rect(colour = "black"),
          legend.box.background = element_rect(colour = "black", size = 1),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
          axis.text.y = element_text(size=10)) 

IPCA_Ind_plot

p <- ggplotly(IPCA_Ind_plot) %>%
  layout(legend = list(orientation = "h", x = 0, y =-0.1))

# For loop para remover legendas
for (i in 2:7) {
  p$x$data[[i]]$showlegend <- F
  
}
p_IPCA_Ind <- p


# Commodities ####


date <- seq(as.Date("1999-08-1"), as.Date("2021-10-1"), by = "month")
ibc_total
colnames(ibc_total) <- 'IBC'

df <- tibble(date, ibc_total, ibc_agro, ibc_metal, ibc_energia,
             .name_repair = ~ c('date','IBC - Total','IBC - Agro','IBC - Metal','IBC - Energia'))
df

rects_mensal <- data.frame(xstart = c(as.Date("1999-08-1"),as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1")),
                           xend = c(as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1"),as.Date( "2021-10-1")),
                           col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)'))

df %>%
  mutate(across(!date,as.numeric))
df <- reshape2::melt(df, id.vars= 'date')
df

IBC_plot <- ggplot(df)  + 
  #geom_ribbon(data = df_metas, aes(x=date, ymin=Piso, ymax =Teto), fill = "GREY80") +
  geom_rect(data = rects_mensal, aes(xmin = xstart, xmax = xend, ymin = min(df$value)-1, ymax = max(df$value)+1, fill = col), alpha = 0.4) +
  geom_line(aes(x=date, y=value, color=variable, linetype = variable), size = 1) +
  scale_y_continuous(labels = function(x) paste0(x, ""),breaks = scales::pretty_breaks(n = 6), expand = c(0,0)) +
  scale_x_date(date_breaks = '1 year', date_minor_breaks = '6 months', date_labels = "%y",expand = c(0, 0.02)) +
  labs(title = 'Índice de preços de commodities IBC') +
  scale_fill_brewer(palette="Blues", name = 'Períodos') +
  scale_color_brewer(palette="Set1", name = 'Períodos') +
  guides(linetype = "none", fill = 'none') +
  ylab('') +
  xlab('') +
  theme_classic() +
  theme(  panel.grid = element_blank(), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          legend.key = element_rect(colour = "black"),
          legend.box.background = element_rect(colour = "black", size = 1),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
          axis.text.y = element_text(size=10)) 

IBC_plot

p <- ggplotly(IBC_plot) %>%
  layout(legend = list(orientation = "h", x = 0, y =-0.1))
# For loop para remover legendas
for (i in 1:6) {
  p$x$data[[i]]$showlegend <- F
  
}
p_IBC <- p



# Cambio, Fed, EMBI ####

date <- seq(as.Date("1999-08-1"), as.Date("2021-10-1"), by = "month")
embi_2 <- embi/100
embi_2
embi
fed_embi <- embi_2 + fed_rate

plot(fed_embi)
plot(embi_2)
fed_rate

df <- tibble(date, selic, embi_2,fed_embi, cambio,
             .name_repair = ~ c('date','Selic','EMBI-Br','FED Rates + EMBI-Br','Taxa de câmbio nominal'))
df <- df %>%
  fill(Selic)
df$`Taxa de câmbio nominal` <- df$`Taxa de câmbio nominal` *5
df
rects_mensal <- data.frame(xstart = c(as.Date("1999-08-1"),as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1")),
                           xend = c(as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1"),as.Date( "2021-10-1")),
                           col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)'))

df %>%
  mutate(across(!date,as.numeric))
df <- reshape2::melt(df, id.vars= 'date')

df
Juros_cambio_plot <- ggplot(df)  + 
  #geom_ribbon(data = df_metas, aes(x=date, ymin=Piso, ymax =Teto), fill = "GREY80") +
  geom_rect(data = rects_mensal, aes(xmin = xstart, xmax = xend, ymin = min(df$value)-1, ymax = max(df$value)+1, fill = col), alpha = 0.4) +
  geom_line(aes(x=date, y=value, color=variable, linetype = variable),stat="identity", size = 1) +
  scale_y_continuous(labels = function(x) paste0(x, ""),breaks = scales::pretty_breaks(n = 6), expand = c(0,0),
                     sec.axis = sec_axis(~(./max(df[which(df['variable'] == 'Selic'),'value'])*5)), name="") +
  scale_x_date(date_breaks = '1 year', date_minor_breaks = '6 months', date_labels = "%y",expand = c(0, 0.02)) +
  labs(title = 'Taxa Selic, FED Rates + EMBI-Br e Taxa de câmbio nominal') +
  scale_fill_brewer(palette="Blues", name = 'Períodos') +
  scale_color_brewer(palette="Set1", name = 'Períodos') +
  guides(linetype = "none", fill = 'none') +
  ylab('') +
  xlab('') +
  theme_classic() +
  theme(  panel.grid = element_blank(), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          legend.key = element_rect(colour = "black"),
          legend.box.background = element_rect(colour = "black", size = 1),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
          axis.text.y = element_text(size=10)) 

Juros_cambio_plot

max(df$value)/max(df[which(df['variable'] == 'Selic'),'value'])*5

ay <- list(
  tickfont = list(size=12),
  titlefont=list(size=15),
  range = c(min(df$value),max(df$value)/max(df[which(df['variable'] == 'Selic'),'value'])*5),
  overlaying = "y",
  nticks = 6,
  side = "right",
  title = ""
)

p <- ggplotly(Juros_cambio_plot) %>%
  add_lines(x=~date, y=~value, colors=NULL, yaxis="y2", 
            data=df, showlegend=FALSE, inherit=FALSE) %>%
  layout(yaxis2 = ay, legend = list(orientation = "h", x = 0, y =-0.05)
         )



# For loop para remover legendas
for (i in 1:6) {
  p$x$data[[i]]$showlegend <- F
  
}
p_juros_cambio <- p

# PIB e Hiato ####

date <- seq(as.Date("1999-03-1"), as.Date("2021-2-1"), by = "quarter")
pib
hiato_pib_2 <- hiato_pib * 100
df <- tibble(date, pib, hiato_pib_2,
             .name_repair = ~ c('date','PIB','Hiato do PIB'))
df

rects_mensal <- data.frame(xstart = c(as.Date("1999-08-1"),as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1")),
                           xend = c(as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1"),as.Date( "2021-10-1")),
                           col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)'))

df %>%
  mutate(across(!date,as.numeric))
df <- reshape2::melt(df, id.vars= 'date')
df

PIB_Hiato_plot <- ggplot(df)  + 
  #geom_ribbon(data = df_metas, aes(x=date, ymin=Piso, ymax =Teto), fill = "GREY80") +
  geom_rect(data = rects_mensal, aes(xmin = xstart, xmax = xend, ymin = min(df$value)-1, ymax = max(df$value)+1, fill = col), alpha = 0.4) +
  geom_hline(yintercept = 0, colour= 'darkgrey') +
  geom_line(aes(x=date, y=value, color=variable, linetype = variable), size = 1) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6), expand = c(0,0)) +
  scale_x_date(date_breaks = '1 year', date_minor_breaks = '6 months', date_labels = "%y",expand = c(0, 0.02)) +
  labs(title = 'Variação real do PIB e Hiato do Produto') +
  scale_fill_brewer(palette="Blues", name = 'Períodos') +
  scale_color_brewer(palette="Set1", name = 'Períodos') +
  guides(linetype = "none", fill = 'none') +
  ylab('') +
  xlab('') +
  theme_classic() +
  theme(  panel.grid = element_blank(), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          legend.key = element_rect(colour = "black"),
          legend.box.background = element_rect(colour = "black", size = 1),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
          axis.text.y = element_text(size=10)) 

PIB_Hiato_plot

p <- ggplotly(PIB_Hiato_plot) %>%
  layout(legend = list(orientation = "h", x = 0, y =-0.1))
p
# For loop para remover legendas
for (i in 1:6) {
  p$x$data[[i]]$showlegend <- F
  
}
p_PIB_Hiato <- p


# VIX e MSCI ####

date <- seq(as.Date("1999-08-1"), as.Date("2021-10-1"), by = "month")
max_length <- max(c(length(vix[,2]), length(MSCI[,2])))    # Find out maximum length
max_length
length(MSCI[,2])
length(c(rep(NA, max_length - length(MSCI[,2])),MSCI[,2]))
length(vix[,2])
scale <- max(MSCI[,2])/max(vix[,2])-1

df_2 <- data.frame(date = date,
                 VIX = vix[,2]*scale,
                 MSCI = c(rep(NA, max_length - length(MSCI[,2])),MSCI[,2])
                 )
df_2

rects_mensal <- data.frame(xstart = c(as.Date("1999-08-1"),as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1")),
                           xend = c(as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1"),as.Date( "2021-10-1")),
                           col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)'))

df <- tibble(df_2) %>%
  mutate(across(!date,as.numeric))
df <- reshape2::melt(df, id.vars= 'date')
df_2

VIX_MSCI_plot <- ggplot(df)  + 
  #geom_ribbon(data = df_metas, aes(x=date, ymin=Piso, ymax =Teto), fill = "GREY80") +
  geom_rect(data = rects_mensal, aes(xmin = xstart, xmax = xend, ymin = min(df_2[61:267,2:3])-1, ymax = max(df_2[61:267,2:3])+1, fill = col), alpha = 0.4) +
  geom_line(aes(x=date, y=value, color=variable, linetype = variable), size = 1) +
  scale_y_continuous(labels = function(x) paste0(x, ""),breaks = scales::pretty_breaks(n = 6), expand = c(0,0),
                     sec.axis = sec_axis(~(./scale), name="")) +
  scale_x_date(date_breaks = '1 year', date_minor_breaks = '6 months', date_labels = "%y",expand = c(0, 0.02)) +
  labs(title = 'Índice MSCI e Índice MSCI') +
  scale_fill_brewer(palette="Blues", name = 'Períodos') +
  scale_color_brewer(palette="Set1", name = 'Períodos') +
  guides(linetype = "none", fill = 'none') +
  ylab('') +
  xlab('') +
  theme_classic() +
  theme(  panel.grid = element_blank(), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          legend.key = element_rect(colour = "black"),
          legend.box.background = element_rect(colour = "black", size = 1),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
          axis.text.y = element_text(size=10)) 

VIX_MSCI_plot

ay <- list(
  tickfont = list(size=12),
  titlefont=list(size=15),
  range = c(min(df_2[61:267,2:3])/scale,max(df_2[61:267,2:3])/scale),
  overlaying = "y",
  nticks = 6,
  side = "right",
  title = ""
)
ay
p <- ggplotly(VIX_MSCI_plot) %>%
  add_lines(x=~date, y=~value, colors=NULL, yaxis="y2", 
            data=df, showlegend=FALSE, inherit=FALSE) %>%
  layout(yaxis2 = ay,
         legend = list(orientation = "h", x = 0, y =-0.05),
         margin = list(r = 25)
  )
p

# For loop para remover legendas
for (i in 1:6) {
  p$x$data[[i]]$showlegend <- F
  
}
p_VIX_MSCI <- p
