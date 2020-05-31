
library(dplyr)
library(fabricatr)
library(xlsx)

num_alumnos=120


#algoritmo BM

indice=function(vector, valor){
  indice=rep(0, times=length(vector))
  for (i in 1:length(vector)){
    if (vector[i]==valor & !is.na(vector[i])) indice[i]=i
  }
  return(indice[indice>0])
}

BM=function(df, q){
  asignacion=rep(NA, times=length(zona_cat))
  Pos1_rev=df$Pos1_rev
  Pos2_rev=df$Pos2_rev
  Pos3_rev=df$Pos3_rev
  puntos=df$puntos_tot
  q_0=q
  for (r in 1:3) #3 rondas
  {
    pref_reveladas_coles=eval(parse(text=paste("Pos",r,"_rev", sep="")))
    for (i in 1:length(q))
    { #para cada colegio
      puntos_peticiones=puntos[pref_reveladas_coles==i & !is.na(pref_reveladas_coles) & is.na(asignacion)]
      alumnos_peticiones=indice(pref_reveladas_coles, i)
      mask=is.na(asignacion)[alumnos_peticiones]
      alumnos_peticiones=alumnos_peticiones[mask]
      if (q[i]>0) 
      {
        for (j in 1:q[i])#para cada plaza
        { 
          puntos_peticiones_sinNA=puntos_peticiones[!(is.na(puntos_peticiones))] #los puntos de los que piden el colegio
          if (length(puntos_peticiones_sinNA)>0)
          {
            index_max_pet= indice(puntos_peticiones,(max(puntos_peticiones_sinNA)))   
            #empates
            if (length(index_max_pet)>1) 
            {random=runif(length(index_max_pet))  
            ind=indice(random, max(random))
            index_max_pet=index_max_pet[ind]}  
            puntos_peticiones[index_max_pet]=NA
            index_max=alumnos_peticiones[index_max_pet]
            asignacion[index_max]=i
          }
        }
      }
      n_asign=sum(asignacion==i & !is.na(asignacion))
      q[i]=q_0[i]-n_asign
      #print(c(i,q[i],q_0[i],n_asign))
    }
  }
  return(asignacion)
}

mat_vacia=matrix(data=NA, nrow=num_alumnos, ncol=22)
df_vacio=data.frame(mat_vacia)

names(df_vacio)=c("sim","zona_cat","hermanos_cat","prudente","AA","OP","Pos1","Pos2","Pos3","Pos1_rev","Pos2_rev","Pos3_rev","pun_zona","pun_hermanos","pun_AA","puntos_tot","asignacion_antes","Pos1_rev_desp","Pos2_rev_desp","Pos3_rev_desp","puntos_tot_desp","asignacion_despues")


for( i in 1:1000){
  zona_cat=draw_categorical(prob=c(1/3,1/3,1/3), category_labels = c(1,2,3),N=num_alumnos)
  hermanos_cat=draw_categorical( prob=c(0.3534,0.4335,0.0974,0.016), N=num_alumnos)
  sim=rep(i, ntimes=num_alumnos)
  df=data.frame(sim,zona_cat,hermanos_cat)
  df_L=df %>% filter(zona_cat==1) 
  df_L=df_L %>% mutate(AA=draw_categorical(prob = c(0.2,0.2,0.6), N=dim(df_L)[1], category_labels = c(3,4,0))) %>%
    mutate(OP=draw_categorical(prob=c(0.15,0.85), N=dim(df_L)[1], category_labels = c(1,0))) %>%
    mutate(prudente=NA)
  df_C=df %>% filter(zona_cat==2) 
  df_C=df_C %>% mutate(AA=draw_categorical(prob = c(0.4,0.3,0.3), N=dim(df_C)[1], category_labels = c(3,4,0))) %>%
    mutate(OP=draw_categorical(prob=c(0.2,0.8), N=dim(df_C)[1], category_labels = c(1,0))) %>%
    mutate(prudente=draw_categorical(prob = c(0.3,0.7), N=dim(df_C)[1], category_labels = c(1,0)))
  df_R=df %>% filter(zona_cat==3) 
  df_R=df_R %>% mutate(AA=draw_categorical(prob = c(0.2, 0.2,0.6), N=dim(df_R)[1], category_labels = c(3,4,0))) %>%
    mutate(OP=draw_categorical(prob=c(0.15,0.85), N=dim(df_R)[1], category_labels = c(1,0)))  %>%
    mutate(prudente=NA)
  ###################################################
  df_L=df_L %>% mutate(Pos1=draw_categorical(N=dim(df_L)[1], p=c(0.5,0.3,0.2), category_labels = c(1,4,3))) %>%
    mutate(Pos2=ifelse(Pos1==1,2,ifelse(Pos1==4,1,ifelse(Pos1==3,1,NA)))) %>%
    mutate(Pos3=ifelse(Pos2==1,2,NA))%>% mutate(Pos2=ifelse(OP==1,NA,Pos2)) %>%
    mutate(Pos3=ifelse(OP==1,NA, Pos3))
  df_C=df_C %>% mutate(Pos1=draw_categorical(prob=c(0.6,0.4), N=dim(df_C)[1], category_labels = c(3,4))) %>%
    mutate(Pos2=ifelse(Pos1==3,4,ifelse(Pos1==4,3,NA))) %>%
    mutate(Pos3=draw_categorical(prob=c(0.5,0.5), N=dim(df_C)[1], category_labels = c(1,5))) %>%
    mutate(Pos3=ifelse(OP==1, NA, Pos3))
  df_R=df_R %>% mutate(Pos1=draw_categorical(N=dim(df_R)[1], p=c(0.5,0.3,0.2), category_labels = c(5,3,4))) %>%
    mutate(Pos2=ifelse(Pos1==5,6,ifelse(Pos1==4,5,ifelse(Pos1==3,5,NA)))) %>%
    mutate(Pos3=ifelse(Pos2==5,6,NA))%>% mutate(Pos2=ifelse(OP==1,NA,Pos2)) %>%
    mutate(Pos3=ifelse(OP==1,NA, Pos3))
  
  #PRE REFORMA
  df_L_PRE=df_L %>% mutate(Pos1_rev=ifelse(OP==1, Pos1, 1)) %>% mutate(Pos2_rev=ifelse(OP==1, Pos2, 2)) %>%
    mutate(Pos3_rev=ifelse(OP==1, Pos3, NA))
  df_C_PRE= df_C %>% mutate(Pos1_rev=ifelse(OP==1 | Pos1!=3 |prudente==0, Pos1, Pos2)) %>% 
    mutate(Pos2_rev=ifelse(OP==1 | Pos1!=3, Pos2, Pos3))  %>% mutate(Pos3_rev=ifelse(OP==1 | Pos1!=3, Pos3, NA))
  df_R_PRE=df_R %>% mutate(Pos1_rev=ifelse(OP==1, Pos1, 5)) %>% mutate(Pos2_rev=ifelse(OP==1, Pos2, 6)) %>%
    mutate(Pos3_rev=ifelse(OP==1, Pos3, NA))
  
  #post
  df_L_POST=df_L %>% mutate(Pos1_rev=ifelse(OP==1 | Pos1==1, Pos1, ifelse(AA!=2,AA,Pos1) )) %>% mutate(Pos2_rev=ifelse(Pos1==Pos1_rev | Pos1_rev==AA, Pos2, Pos3)) %>%
    mutate(Pos3_rev=ifelse(Pos1==Pos1_rev | Pos1_rev==AA, Pos3, NA))
  df_C_POST= df_C %>% mutate(Pos1_rev=ifelse(OP==1 | Pos1==4, Pos1,  ifelse(AA %in% c(3,4), AA, 4 ))) %>% 
    mutate(Pos2_rev=ifelse(Pos1_rev==Pos1, Pos2, Pos3)) %>% mutate(Pos3_rev=ifelse(Pos1==Pos1_rev, Pos3, NA))
  df_R_POST=df_R %>% mutate(Pos1_rev=ifelse(OP==1 | Pos1==5, Pos1, ifelse(AA!=6,AA,Pos1) )) %>% mutate(Pos2_rev=ifelse(Pos1==Pos1_rev | Pos1_rev==AA, Pos2, Pos3)) %>%
    mutate(Pos3_rev=ifelse(Pos1==Pos1_rev | Pos1_rev==AA, Pos3, NA))
  
  #puntos
  puntos_hermanos=c(0,10,20,30)
  
  #puntos antes
  puntos_zona_antes=c(4,4,1) 
  df_L_PRE=df_L_PRE %>% mutate(pun_zona=ifelse(Pos1_rev %in% c(1,2),puntos_zona_antes[1],puntos_zona_antes[3])) %>%
    mutate(pun_hermanos=puntos_hermanos[hermanos_cat+1]) %>%
    mutate(pun_AA=ifelse(Pos1_rev==AA,2.5,0)) 
  df_C_PRE=df_C_PRE %>% mutate(pun_zona=ifelse(Pos1_rev %in% c(3,4),puntos_zona_antes[2],puntos_zona_antes[3])) %>%
    mutate(pun_hermanos=puntos_hermanos[hermanos_cat+1]) %>%
    mutate(pun_AA=ifelse(Pos1_rev==AA,2.5,0)) 
  df_R_PRE=df_R_PRE %>% mutate(pun_zona=ifelse(Pos1_rev %in% c(5,6),puntos_zona_antes[1],puntos_zona_antes[3])) %>%
    mutate(pun_hermanos=puntos_hermanos[hermanos_cat+1]) %>%
    mutate(pun_AA=ifelse(Pos1_rev==AA,2.5,0)) 
  
  df_L_PRE=df_L_PRE %>% mutate(puntos_tot=pun_zona+pun_hermanos+pun_AA)
  df_C_PRE=df_C_PRE %>% mutate(puntos_tot=pun_zona+pun_hermanos+pun_AA)
  df_R_PRE=df_R_PRE %>% mutate(puntos_tot=pun_zona+pun_hermanos+pun_AA)
  df_antes=rbind(df_L_PRE,df_C_PRE,df_R_PRE)
  #puntos despues
  
  puntos_zona_despues=c(4,4.5,2) 
  df_L_POST=df_L_POST %>% mutate(pun_zona=ifelse(Pos1_rev %in% c(1,2),puntos_zona_despues[1],puntos_zona_despues[3])) %>%
    mutate(pun_hermanos=puntos_hermanos[hermanos_cat+1]) %>%
    mutate(pun_AA=ifelse(Pos1_rev==AA,2.5,0)) 
  df_C_POST=df_C_POST %>% mutate(pun_zona=ifelse(Pos1_rev %in% c(3,4),puntos_zona_despues[2],puntos_zona_despues[3])) %>%
    mutate(pun_hermanos=puntos_hermanos[hermanos_cat+1]) %>%
    mutate(pun_AA=ifelse(Pos1_rev==AA,2.5,0)) 
  df_R_POST=df_R_POST %>% mutate(pun_zona=ifelse(Pos1_rev %in% c(5,6),puntos_zona_despues[1],puntos_zona_despues[3])) %>%
    mutate(pun_hermanos=puntos_hermanos[hermanos_cat+1]) %>%
    mutate(pun_AA=ifelse(Pos1_rev==AA,2.5,0)) 
  
  df_L_POST=df_L_POST %>% mutate(puntos_tot=pun_zona+pun_hermanos+pun_AA)
  df_C_POST=df_C_POST %>% mutate(puntos_tot=pun_zona+pun_hermanos+pun_AA)
  df_R_POST=df_R_POST %>% mutate(puntos_tot=pun_zona+pun_hermanos+pun_AA)
  df_despues=rbind(df_L_POST, df_C_POST, df_R_POST)
  
  #capacidad 
  q=c(24,120,20,20,24,120)
  
  #asignacion
  asignacion_antes=BM(df_antes,q)
  df_asign_antes=cbind(df_antes, asignacion_antes)
  df_asign_antes=df_asign_antes %>% mutate(asignacion_antes=ifelse(is.na(asignacion_antes) & OP==0 & zona_cat==1,2,ifelse(is.na(asignacion_antes) & OP==0 & zona_cat==3,6,asignacion_antes))) 

  num_nas_2=df_asign_antes%>% filter(is.na(asignacion_antes) & OP==0 & zona_cat==2) %>% group_by(asignacion_antes) %>%summarise(total=n())
  
  df_asign_antes=df_asign_antes %>% 
    mutate(asignacion_antes=ifelse(is.na(asignacion_antes) & OP==0 & zona_cat==2, draw_categorical(prob=c(0.5,0.5), N=as.integer(num_nas_2[2]),category_labels=c(2,6)),asignacion_antes))
  
           
  asignacion_despues=BM(df_despues,q)
  df_asign_despues=cbind(df_despues, asignacion_despues)
  df_asign_despues=df_asign_despues %>% mutate(asignacion_despues=ifelse(is.na(asignacion_despues) & OP==0 & zona_cat==1,2,ifelse(is.na(asignacion_despues) & OP==0 & zona_cat==3,6,asignacion_despues)))
  
  num_nas_2_des=df_asign_despues%>% filter(is.na(asignacion_despues) & OP==0 & zona_cat==2) %>% group_by(asignacion_despues) %>%summarise(total=n())
  df_asign_despues=df_asign_despues %>% 
    mutate(asignacion_despues=ifelse(is.na(asignacion_despues) & OP==0 & zona_cat==2, draw_categorical(prob=c(0.5,0.5), N= as.integer(num_nas_2_des[2]),category_labels=c(2,6)),asignacion_despues))
  
  
  df_asign_despues=df_asign_despues %>% rename(Pos1_rev_desp=Pos1_rev, Pos2_rev_desp= Pos2_rev, Pos3_rev_desp=Pos3_rev, puntos_tot_desp=puntos_tot)
  df_asign_despues_res = df_asign_despues %>% 
    select(Pos1_rev_desp,Pos2_rev_desp,Pos3_rev_desp,puntos_tot_desp, asignacion_despues)
  df_antes_despues=cbind(df_asign_antes, df_asign_despues_res)
  
  df_vacio=rbind(df_vacio,df_antes_despues)

}

df_antes_despues=df_vacio[(num_alumnos+1):length(df_vacio[,1]),]
df_antes_despues %>% group_by(sim) %>%summarise(total=n())


# los que van a OP antes y despues--> los NA con OP=1
OP_antes=df_antes_despues %>%filter(OP==1, is.na(asignacion_antes) | asignacion_antes!=Pos1) %>% group_by(sim,zona_cat)  %>% 
  summarise(total= n()) %>% group_by(zona_cat) %>% summarise(avg_sim=mean(total), desv_st=sd(total))
OP_despues=df_antes_despues %>%filter(OP==1, is.na(asignacion_despues) | asignacion_despues!=Pos1) %>% group_by(sim,zona_cat)  %>% 
  summarise(total= n()) %>% group_by(zona_cat) %>% summarise(avg_sim=mean(total), desv_st=sd(total))

#primera opcion revelada antes y despues
primera_revelada_antes= df_antes_despues %>% filter(Pos1_rev==asignacion_antes) %>% group_by(sim,zona_cat)  %>% 
  summarise(total= n()) %>% group_by(zona_cat) %>% summarise(avg_sim=mean(total), desv_st=sd(total))
primera_revelada_despues=df_antes_despues %>%filter(Pos1_rev==asignacion_despues) %>% group_by(sim,zona_cat) %>% 
  summarise(total= n()) %>% group_by(zona_cat) %>% summarise(avg_sim=mean(total), desv_st=sd(total))

#primera opcion real antes y despues
primera_real_antes= df_antes_despues %>% filter(Pos1==asignacion_antes) %>% group_by(sim, zona_cat)%>% 
  summarise(total= n()) %>% group_by(zona_cat) %>% summarise(avg_sim=mean(total), desv_st=sd(total))
primera_real_despues=df_antes_despues %>%filter(Pos1==asignacion_despues) %>% group_by(sim, zona_cat) %>% 
  summarise(total= n()) %>% group_by(zona_cat) %>% summarise(avg_sim=mean(total), desv_st=sd(total))

#cuantos pasan al default por zona
def_antes=df_antes_despues %>% filter(asignacion_antes %in% c(2,6))  %>% group_by(sim, zona_cat)%>% 
  summarise(total= n()) %>% group_by(zona_cat) %>% summarise(avg_sim=mean(total), desv_st=sd(total))

def_despues=df_antes_despues %>% filter(asignacion_despues %in% c(2,6))  %>% group_by(sim, zona_cat)%>% 
  summarise(total= n()) %>% group_by(zona_cat) %>% summarise(avg_sim=mean(total), desv_st=sd(total))


#colegios
colegios_antes=df_antes_despues %>% group_by(sim, asignacion_antes)%>% 
 summarise(total= n()) %>% group_by(asignacion_antes) %>% summarise(avg_sim=mean(total), desv_st=sd(total))
colegios_despues=df_antes_despues %>% group_by(sim, asignacion_despues)%>% 
 summarise(total= n()) %>% group_by(asignacion_despues) %>% summarise(avg_sim=mean(total), desv_st=sd(total))

 
cond_iguales=(asignacion_antes==asignacion_despues & !is.na(asignacion_antes) & !is.na(asignacion_despues)| (is.na(asignacion_antes) & is.na(asignacion_despues)))
 
mejoras=df_antes_despues  %>% 
 mutate(mejora=ifelse(asignacion_despues==Pos1 & !is.na(asignacion_despues) & (asignacion_antes!=Pos1 | is.na(asignacion_antes)),1,ifelse(asignacion_despues==Pos2 & !is.na(asignacion_despues) & ((asignacion_antes!=Pos1 & asignacion_antes!=Pos2)|is.na(asignacion_antes)),1,ifelse(cond_iguales,99,0)))) 

mejoras_res=mejoras %>%  group_by(sim, mejora, zona_cat, AA,OP)%>% 
 summarise(total= n()) %>% group_by(mejora, zona_cat,AA,OP) %>% summarise(avg_sim=mean(total), desv_st=sd(total))

mejoras_zona=mejoras %>%  group_by(sim, mejora, zona_cat)%>% 
  summarise(total= n()) %>% group_by(mejora, zona_cat) %>% summarise(avg_sim=mean(total), desv_st=sd(total))

mejoras_OP=mejoras %>%  group_by(sim, mejora, OP)%>% 
  summarise(total= n()) %>% group_by(mejora,OP) %>% summarise(avg_sim=mean(total), desv_st=sd(total))

mejoras_AA=mejoras %>%  group_by(sim, mejora, AA)%>% 
  summarise(total= n()) %>% group_by(mejora, AA) %>% summarise(avg_sim=mean(total), desv_st=sd(total))

