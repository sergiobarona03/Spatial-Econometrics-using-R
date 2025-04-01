
######################################
## Recodificar datos de la encuesta ##
######################################
encuesta <- readxl::read_excel("Datos.xlsx")

#######################################
## Carga y conteo en la encuesta     ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################################
encuesta$wetland[encuesta$wetland ==  "Acuaparque de la Ca¤a"] = "Humedal Parque de la caña"
  encuesta$wetland[encuesta$wetland ==  "Batallón del Municipio"] = "Batallón"
  encuesta$wetland[encuesta$wetland ==  "Charco Azul"] = "Charco Azul"
  encuesta$wetland[encuesta$wetland ==  "Club Campestre"] = "Club Campestre"
  encuesta$wetland[encuesta$wetland ==  "Club del Municipio"] = "Club del Municipio"
  encuesta$wetland[encuesta$wetland ==  "Club Shalom"] = "Shalom"
  encuesta$wetland[encuesta$wetland ==  "El Antojo"] = "El Antojo"
  encuesta$wetland[encuesta$wetland ==  "El Embudo"] =  "El Embudo"
  encuesta$wetland[encuesta$wetland ==  "El Pondaje"] = "El Pondaje"
  encuesta$wetland[encuesta$wetland ==  "El Retiro"] = "El Retiro"
  encuesta$wetland[encuesta$wetland ==  "Ibis"] = "Humedal Ibis"
  encuesta$wetland[encuesta$wetland ==  "Isaías Duarte Cancino"] = "Isaias Duarte Cancino" 
  encuesta$wetland[encuesta$wetland ==  "La Babilla Zanjón del Burro"] = "La Babilla Zanjón del Burro"
  encuesta$wetland[encuesta$wetland ==  "La María"] = "La Maria"  
  encuesta$wetland[encuesta$wetland ==  "Lago Verde"] = "Lago Verde" 
  encuesta$wetland[encuesta$wetland ==  "Laguna El Sombrerito"] = "Laguna El Sombrerito"
  encuesta$wetland[encuesta$wetland ==  "Limonar"] = "El Limonar"
  encuesta$wetland[encuesta$wetland ==  "Madrevieja La Pailita"] = "Madrevieja La Pailita" 
  encuesta$wetland[encuesta$wetland ==  "Pacheco"] = "Humedal Pacheco"    
  encuesta$wetland[encuesta$wetland ==  "Palmeto"] = "Palmeto" 
  encuesta$wetland[encuesta$wetland ==  "Panamericano"] = "Panamericano"
  encuesta$wetland[encuesta$wetland ==  "Parque de las Garzas"] = "Las Garzas" 
  encuesta$wetland[encuesta$wetland ==  "Reservorio Agrícola"] = "Reservorio Agricola"  
  encuesta$wetland[encuesta$wetland ==  "Reservorio Puerto Mallarino"] = "Puerto Mallarino" 
  encuesta$wetland[encuesta$wetland ==   "Santa Helena"] = "Humedal Santa Elena"  
  encuesta$wetland[encuesta$wetland ==  "Universidad del Valle"] = "Univalle" 
  encuesta$wetland[encuesta$wetland ==  "Universidad Icesi"] = "Humedal ICESI" 
  encuesta$wetland[encuesta$wetland ==  "Universidad Javeriana"] = "Javeriana" 
  encuesta$wetland[encuesta$wetland ==  "Universidad San Buenaventura"] = "San Buenaventura" 


  writexl::write_xlsx(encuesta, "Datos_recod.xlsx")  
  
  
  