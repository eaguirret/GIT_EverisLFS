@AbapCatalog.sqlViewName: 'ZDORDPROREALNV16'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ordenes de proceso real nivel 1_6'

define view Z_D_OrdProReal_Nv1_6 as select distinct from Z_D_OrdProReal_Nv1_5 as a
{
    a.Orden, 
    a.Componente, 
    a.Lote,        //VARROYO 18062020
    a.Centro_Consumo, 
    a.Anio_Periodo, 
    sum(a.Importe) as Importe, 
//    sum(a.Costo_Unitario) as Costo_Unitario, 
//    a.Moneda_Costo_Unitario, 
    sum(a.Ctd_Total_Consu) as Ctd_Total_Consu, 
    a.Um_Total_Consu, 
    sum(a.Ctd_Base) as Ctd_Base, 
    a.Und_Base, 
    sum(a.Costo_Total_Real) as Costo_Total_Real, 
    a.Orden_Creacion, 
    a.atwrt 
    --a.plnbez, 
    --a.aufnr
    
}group by a.Orden, 
          a.Componente, 
          a.Lote,         //VARROYO 18062020 
          a.Centro_Consumo, 
          a.Anio_Periodo,
          --a.Moneda_Costo_Unitario,
          a.Um_Total_Consu,
          a.Und_Base,
          a.Orden_Creacion, 
          a.atwrt
          --a.plnbez, 
          --a.aufnr
 having sum(a.Importe) != 0 and /*sum(a.Costo_Unitario) !=0  and*/ sum(a.Ctd_Total_Consu) !=0 and sum(a.Ctd_Base) !=0 and sum(a.Costo_Total_Real) !=0        
          
