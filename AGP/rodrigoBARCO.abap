select inob~CUOBJ,
       inob~KLART,
       inob~OBJEK,
       AUSP~ATINN,
       AUSP~ATWRT 
from inob 
inner join AUSP 
on  inob~CUOBJ = AUSP~OBJEK 
inner join ztconstantes as zconsklart
on inob~klart = zconsklart~valor1 and
   zconsklart~modulo = 'PP' and
   zconsklart~aplicacion = 'VALIDACION' and
   zconsklart~programa = 'ZPPP0003' and
   zconsklart~campo = 'KLART' 
where inob~OBJEK like '%100004256'

'000000000000271353'

inner join ztconstantes as zconsatinn
on ausp~atinn = zconsatinn~valor1 and
   zconsatinn~modulo = 'PP' and
   zconsatinn~aplicacion = 'VALIDACION' and
   zconsatinn~programa = 'ZPPP0003' and
   zconsatinn~campo = 'ATINN' 
   
   
  select * from  inob 
  select * from  AUSP where atinn = 'Z_PIECE_TYPE' 
  
  000000000000178694
  000000000000565348
  
  
  select * from ztconstantes
where modulo = 'PP' and
    aplicacion = 'VALIDACION' and
    programa = 'ZPPP0003' and
    ( campo = 'ATINN' or campo = 'KLART' )
    
select AUSP~OBJEK
from inob 
inner join AUSP 
on  inob~CUOBJ = AUSP~OBJEK 
where AUSP~OBJEK ='000000000000271353'
    
    
    
    
select * from  inob     
where cuobj = '271353'

select * from ausp 
where OBJEK  = '000000000000271353'
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    