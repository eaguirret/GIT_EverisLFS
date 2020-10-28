@AbapCatalog.sqlViewName: 'ZVNMM_DOCSP_SPED'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
//@AccessControl.authorizationCheck: #CHECK
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Documentos de SolPed'
@VDM.viewType : #BASIC
@AbapCatalog.Buffering.status: #SWITCHED_OFF
define view ZDDLMM_DOCSP_SPED as select from eban as a
      left outer join esll as b
        on a.packno = b.packno
      left outer join t023t as c
        on  c.matkl = a.matkl
       and  c.spras = 'S'
       left outer  join ekpo as d
       on  d.ebeln = a.ebeln
       and d.ebelp = a.ebelp
       and d.loekz = 'L'
       left outer join cdhdr as e
       on  e.objectid   = a.banfn
       and e.objectclas = 'BANF'
       left outer join zvnmm_trama_sped as f  //Validamos is existe en la tabla zttrama_interf
         on f.nrosolped  = a.banfn   //BANFN Número de la solicitud de pedido
        and f.possolped  = a.bnfpo   //BNFPO Número de posición de la solicitud de pedido
       {
key      a.bsart,
         a.bnfpo,
         a.matnr,
         a.knttp,
         a.pstyp,
         a.ematn,
         a.meins,
         a.lpein,
         a.lfdat,
         a.matkl,
         a.ekgrp,
         a.afnam,
         a.bednr,
         a.lifnr,
         a.txz01,
         a.preis,
         a.menge,
         a.ekorg,
key      a.banfn,
         a.peinh,
         b.sub_packno,
         c.wgbez,
         a.ernam,
         a.loekz,
         a.lgort,
//      add
key      e.objectid,   //@s_n_soli
key      e.udate,      //@s_fe_m_p
key      e.utime,        //@p_h_md_p
         d.ebeln,
         a.ebeln as Pedido,
         a.badat,
         case
////         when e.udate = '00000000' and e.utime = '000000'
         when e.objectid <> '' //and e.utime = '00:00:00'
            then 'M' //Consideramos como ModificaciÃ³n de SolPed
         else 'N'    //Consideramos como CreaciÃ³n de SolPed
         end as flag,
          f.maxcorre,

        @EndUserText.quickInfo: 'Fecha10'

         CONCAT(e.udate,e.utime) as fec_hora

}
where a.konnr = ''
  and a.bsart = 'ZM'
  and ( a.loekz = ''
//EMAT-Enrique Aguirre - LFSENAG-578 -27.10.2020
  or exists
(
    select trama.campo21
    from zttrama_interf as trama
    where trama.interfaz = 'Solicitud pedido'
          and trama.campo21 = a.banfn
          and trama.error = '' and 
    trama.contador in
(
    select max( contador )
    from zttrama_interf as b
    where b.interfaz = 'Solicitud pedido'
    and b.campo21 = trama.campo21 and
    b.error = ''
)

) )
//EMAT-Enrique Aguirre - LFSENAG-578 -27.10.2020

//  and (  d.loekz = 'L' or a.ebeln = '')
  and (a.ebeln = '' or d.ebeln <> '')
