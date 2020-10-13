select * 
from z_d_ordproreal_nv1_8 
where orden = '002100002087'

select * 
from z_d_ordproreal_nv2_11 
where orden = '002100002087'

select * 
from z_d_ordproreal_nv2_10 
where orden = '002100002087'

select * 
from z_d_ordproreal_nv3_11 
where orden = '002100002087' 

select * 
from z_d_ordproreal_nv3_10 
where orden = '002100002087'



select * from 
Z_D_OrdProReal_Nv1_5
where orden = '002100002087'



select * from Z_D_OrdProReal_Nv1_7
where orden = '002100002087'

select * from 
Z_D_OrdProReal_Nv1_6
where orden = '002100002087'

select * from 
Z_D_OrdProReal_Nv1_5
where orden = '002100002087'

select * from 
Z_D_ORDPROREAL_NV1_4_1
where orden = '002100002087'







select distinct 
    a~Orden, 
    a~Componente, 
    a~Lote,        
    a~Centro_Consumo, 
    a~Anio_Periodo, 
    sum( a~Importe ) as Importe, 
    sum( a~Ctd_Total_Consu ) as Ctd_Total_Consu, 
    a~Um_Total_Consu, 
    sum( a~Ctd_Base ) as Ctd_Base, 
    a~Und_Base, 
    sum( a~Costo_Total_Real ) as Costo_Total_Real, 
    a~Orden_Creacion, 
    a~atwrt 

from Z_D_OrdProReal_Nv1_5 as a
WHERE  a~orden = '002100002087'
group by a~Orden, 
          a~Componente, 
          a~Lote,          
          a~Centro_Consumo, 
          a~Anio_Periodo,
          a~Um_Total_Consu,
          a~Und_Base,
          a~Orden_Creacion, 
          a~atwrt
 having sum( a~Importe ) <> 0 and 
 sum( a~Ctd_Total_Consu ) <> 0 and 
 sum( a~Ctd_Base ) <> 0 and 
 sum( a~Costo_Total_Real ) <> 0 
 
 
 
 
 
 select 
         a~Orden, 
        a~Componente, 
        a~Lote, 
        a~Centro_Consumo,
        a~Anio_Periodo,  
        
        
        case
            when b~bwart = '102' then b~ERFMG * -1
                else b~ERFMG end as Ctd_Total_Consu,
 
        b~ERFME as Um_Total_Consu, 
        
        b~aufnr as Orden_Creacion,
        e~atwrt
 
 from Z_D_OrdProReal_Nv1_4 as a
inner join aufm as b 
on b~matnr = a~Componente and 
b~charg = a~Lote and 
b~werks = a~Centro_Consumo
left outer join mch1 as c 
on c~matnr = a~Componente and 
c~charg = a~Lote 
left outer join cabn as d 
on d~atnam = 'CALIFICACIO_1000'
left outer join ausp as e 
on e~objek = c~cuobj_bm and 
e~atinn = d~atinn and 
e~klart = '023'
inner join afko as f 
on f~plnbez = a~Componente and 
f~aufnr = b~aufnr
where ( b~bwart = '101' or b~bwart = '102' )
  and  a~orden = '002100002087'
  
  
  
select * from   
Z_TF_ORDPROREAL_NV2_5
where Orden_Creacion = '002100002087'

select * from   
Z_D_ORDPROREAL_NV2_4
where Orden_principal = '002100002087'

select * from   
Z_D_OrdProReal_Nv2_3
where Orden_principal = '002100002087'

select * from   
Z_D_OrdProReal_Nv2_2
where Orden_principal = '002100002087'

select * from   
Z_D_OrdProReal_Nv2
where Orden_principal = '002100002087'

select * from   
Z_D_OrdProReal_Nv1_8
where Orden = '002100002087'

select * from   
Z_D_Ver_UmTotalCon_NV2
where Orden_Creacion = '002100001640'

select * from   
Z_D_OrdProReal_Nv2_4
where Orden_principal = '002100002087'

select * from   
Z_D_OrdProReal_Nv2_3
where Orden_principal = '002100002087'

select * from   
Z_TF_ORDPROREAL_NV2_5
where Orden_principal = '002100002087'



