@AbapCatalog.sqlViewName: 'ZDORDPROREALNV38'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ordenes de proceso real nivel 3_8'

define view Z_D_OrdProReal_Nv3_8 as select distinct from Z_D_OrdProReal_Nv3_7 as a

{
    a.Orden_Principal,
    a.Orden_Creacion,
    a.Orden_Creacion_N, 
    a.Componente, 
    a.Lote, 
    a.Centro_Consumo, 
    a.Anio_Periodo, 
    sum(a.Importe) as Importe, 
    --sum(a.Costo_Unitario) as Costo_Unitario, 
    --a.Moneda_Costo_Unitario, 
    sum(a.Ctd_Total_Consu) as Ctd_Total_Consu, 
    a.Um_Total_Consu, 
    sum(a.Ctd_Base) as Ctd_Base, 
    a.Und_Base, 
    sum(a.Costo_Total_Real) as Costo_Total_Real, 
    a.atwrt 
    --a.plnbez, 
    --a.aufnr
    
}group by a.Orden_Principal,
          a.Orden_Creacion,
          a.Orden_Creacion_N,
          a.Componente, 
          a.Lote, 
          a.Centro_Consumo, 
          a.Anio_Periodo,
          --a.Moneda_Costo_Unitario,
          a.Um_Total_Consu,
          a.Und_Base,
          //a.Orden_Creacion, 
          a.atwrt
          --a.plnbez, 
          --a.aufnr
 having sum(a.Importe) != 0 and /*sum(a.Costo_Unitario) !=0  and*/ sum(a.Ctd_Total_Consu) !=0 and sum(a.Ctd_Base) !=0 and sum(a.Costo_Total_Real) !=0





/*as select from Z_D_OrdProReal_Nv3_6 as a
left outer join Z_D_OrdProReal_Nv3_7 b on a.Orden_Principal = b.Orden_Principal and a.Orden_Creacion = b.Orden_Creacion and a.Componente = b.Componente and a.Lote = b.Lote and a.Centro_Consumo = b.Centro_Consumo

{
    a.Orden_Principal,
    a.Orden_Creacion,
    --a.Orden_Creacion_N2,
    --b.Orden_Creacion_2, 
    a.Componente, 
    a.Anio_Periodo, 
    a.Lote, 
    a.Centro_Consumo, 
    a.Importe, 
    --a.Costo_Unitario, 
    --a.Moneda_Costo_Unitario, 
    a.Ctd_Total_Consu, 
    a.Um_Total_Consu, 
    a.Ctd_Base, 
    a.Und_Base, 
    a.Costo_Total_Real,
    a.Porcen_Consumo,
 
    case 
        when b.atwrt is null then 'NA'
            else b.atwrt end as Valor,
            
    b.plnbez,
    b.aufnr
    
}*/
