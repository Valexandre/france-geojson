library(tidyverse)
library(sf)
library(data.table)

DepsCarte<-st_read("departements.shp")
listedeps<-unique(DepsCarte$INSEE_DEP)


DonneesCarreau<-st_read("200m_carreaux_metropole.shp")
DonneesCarreau<-DonneesCarreau%>%dplyr::select(idINSPIRE,ind_c,nbcar)

Mart<-st_read("200m_carreaux_martinique.shp")
Reu<-st_read("200m_carreaux_reunion.shp")
DonneesCarreau<-rbind(DonneesCarreau%>%dplyr::select(idINSPIRE,ind_c,nbcar),
                      Mart%>%dplyr::select(idINSPIRE,ind_c,nbcar),
                      Reu%>%dplyr::select(idINSPIRE,ind_c,nbcar))

UnCPParCarreau<-function(x){
  DepX<-DepsCarte%>%filter(INSEE_DEP==x)%>%st_transform(crs=2154)%>%
    st_buffer(dist = 1000)%>%st_transform(crs=4326)
    CarreauxDep<-st_join(x = DonneesCarreau, y =st_sf(DepX), join=st_intersects)%>%
      filter(INSEE_DEP==x)
  colnames(CarreauxDep)
  head(CarreauxDep)
  CarreauxDep<-CarreauxDep[,c(1,2,5,7)]
  
  urlgz<-paste0("https://adresse.data.gouv.fr/data/ban/adresses/latest/csv/adresses-",x,".csv.gz")
  dt = fread(urlgz)
  justenough<-dt%>%select(code_postal,lon,lat)%>%
    mutate(code_postal=str_pad(code_postal,width = 5,pad="0"))%>%
    st_as_sf(coords=c("lon","lat"),crs=4326)%>%
    group_by(code_postal)%>%
    summarise()

JointurePoints<-st_join(CarreauxDep,justenough,join=st_intersects)
JointurePoints<-JointurePoints%>%filter(!is.na(code_postal))
JointurePointsRegroup<-JointurePoints%>%group_by(code_postal)%>%summarise()
st_write(JointurePointsRegroup,paste0("CARTECP_",x,".geojson"),layer = "CP")
}
walk(.x = sort(listedeps),.f =UnCPParCarreau)
