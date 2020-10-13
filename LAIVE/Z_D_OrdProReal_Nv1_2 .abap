@AbapCatalog.sqlViewName: 'ZDORDPROREALNV12'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ordenes de proceso real nivel 1_2'

define view Z_D_OrdProReal_Nv1_2 as select from Z_D_OrdProReal_Nv1 as a
left outer join mbew b on b.matnr = a.Componente and b.bwkey = a.Centro_Consumo 

{

    a.Orden, 
    a.Componente, 
    a.Fecha_Contable,
    
    substring(a.Fecha_Contable,1,4) as Anio,
    substring(a.Fecha_Contable,5,2) as Mes,
    substring(a.Fecha_Contable,1,6) as Anio_Periodo,
    
    case substring(a.Fecha_Contable,5,2)
        when '01' then '001'
        when '02' then '002'
        when '03' then '003'
        when '04' then '004'
        when '05' then '005'
        when '06' then '006'
        when '07' then '007'
        when '08' then '008'
        when '09' then '009'
        when '10' then '010'
        when '11' then '011'
        else '012' end as Periodo,
         
    Cls_Mvto, 
    a.Lote, 
    a.Centro_Consumo, 
    a.Importe, 
    a.Moneda,
    b.vprsv,
    b.kaln1, 
    
    case
        when a.Ctd_Total_Consu_1 <> 0 and a.Ctd_Total_Consu_CWM <> 0 then a.Ctd_Total_Consu_CWM
        when a.Ctd_Total_Consu_1 <> 0 and a.Ctd_Total_Consu_CWM = 0 then a.Ctd_Total_Consu_1
        when a.Ctd_Total_Consu_1 = 0 and a.Ctd_Total_Consu_CWM <> 0 then a.Ctd_Total_Consu_CWM
            else 0 end as Ctd_Total_Consu,
     
    
    case
        when a.Ctd_Total_Consu_1 <> 0 and a.Ctd_Total_Consu_CWM <> 0 then a.Und_Consu_CWM
        when a.Ctd_Total_Consu_1 <> 0 and a.Ctd_Total_Consu_CWM = 0 then a.Und_Consu_1
        when a.Ctd_Total_Consu_1 = 0 and a.Ctd_Total_Consu_CWM <> 0 then a.Und_Consu_CWM
            else a.Und_Consu_1 end as Um_Total_Consu,
            
    a.Ctd_Base,
    a.Und_Base
    
} 
