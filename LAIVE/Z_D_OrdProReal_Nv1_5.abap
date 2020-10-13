@AbapCatalog.sqlViewName: 'ZDORDPROREALNV15'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ordenes de proceso real nivel 1_5'

define view Z_D_OrdProReal_Nv1_5 as select distinct from Z_D_OrdProReal_Nv1_4 as a
inner join aufm b on b.matnr = a.Componente and b.charg = a.Lote and b.werks = a.Centro_Consumo
left outer join mch1 c on c.matnr = a.Componente and c.charg = a.Lote 
left outer join cabn d on d.atnam = 'CALIFICACIO_1000'
left outer join ausp e on e.objek = c.cuobj_bm and e.atinn = d.atinn and e.klart = '023'
inner join afko f on f.plnbez = a.Componente and f.aufnr = b.aufnr

{
        a.Orden, 
        a.Componente, 
        a.Lote, 
        a.Centro_Consumo,
        a.Anio_Periodo,  
        
        case 
            when b.bwart = '102' then a.Importe * -1
                else a.Importe end as Importe, 
        
//        case 
//            when b.bwart = '102' then a.Costo_Unitario * -1 
//                else a.Costo_Unitario end as Costo_Unitario,
//         
//        a.Moneda_Costo_Unitario, 
        
        case
            when b.bwart = '102' then a.Ctd_Total_Consu * -1
                else a.Ctd_Total_Consu end as Ctd_Total_Consu,
 
        a.Um_Total_Consu, 
        
        case
            when b.bwart = '102' then a.Ctd_Base * -1
                else a.Ctd_Base end as Ctd_Base,
 
        a.Und_Base, 
        
        case
            when b.bwart = '102' then a.Costo_Total_Real * -1
                else a.Costo_Total_Real end as Costo_Total_Real,
        
        --b.bwart,
        b.aufnr as Orden_Creacion,
        e.atwrt
        --f.plnbez,
        --f.aufnr
        
}where b.bwart = '101' or b.bwart = '102'
