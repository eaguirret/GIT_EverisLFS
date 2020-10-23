select
      Distinct
      item~db_key as item_db_key,
      node~db_key,
      node~tspid,
      intr~name_org1,
      node~datetime_chscs,
      node~tor_id,
      item~consignee_id,
      loct~descr40,
      srot~sfir_id,
      sdoc001~btd_id  as nro_oc,
      sdoc56~btd_id  as nro_hes,
      srott~ddtext   as lifecycle_t,
      chrrt~ddtext   as invoicing_t,
      ekbe~belnr,
      substring( item~base_btd_id, 26, 10 ) as vbeln,
      dcco~kostl
from    /scmtms/d_torrot as node
    inner join      /scmtms/d_torite as item      on node~db_key = item~parent_key
    inner join      /scmtms/d_torstp as stop      on node~db_key = stop~parent_key
    inner join      /scmtms/d_tchrgr as chrr      on node~db_key = chrr~host_key

    inner join      /scmtms/d_sf_itm as sitm      on node~db_key = sitm~tor_root_key
    inner join      /scmtms/d_sf_rot as srot      on sitm~parent_key = srot~db_key
    left outer join /scmtms/d_sf_doc as sdoc001   on sitm~parent_key = sdoc001~parent_key  and sdoc001~fsd_btd_status = '01'
    left outer join /scmtms/d_sf_doc as sdoc56    on sitm~parent_key = sdoc56~parent_key   and sdoc56~fsd_btd_status = '01'
    inner join      but000           as intr      on node~tspid = intr~partner
    inner join      /sapapo/loc      as loca      on item~consignee_id = loca~locno
    inner join      /sapapo/loct     as loct      on  loca~locid = loct~locid
                                                  and loct~spras = 'S'
    inner join      dd07t            as srott     on srot~lifecycle = srott~domvalue_l
    inner join      dd07t            as chrrt     on chrr~invoicing = chrrt~domvalue_l
    left outer join ekbe                          on sdoc001~btd_id = ekbe~ebeln
    left outer join zsdt_destceco    as dcco      on node~consigneeid = dcco~kunnr
where
      item~main_cargo_item =  'X'
      and item~item_cat    = 'PRD'
  and stop~stop_cat        =  'I'
  and stop~stop_seq_pos    <> 'F'
  and srott~domname        =  '/SCMTMS/SFIR_LC_STATUS'
  and srott~ddlanguage     =  'S'
  and srott~as4local       =  'A'
  and chrrt~domname        =  '/SCMTMS/D_INVOICING_STAT_CODE'
  and chrrt~ddlanguage     =  'S'
  and chrrt~as4local       =  'A'
  and sdoc001~btd_tco      =  '001'
  and sdoc56~btd_tco       =  '56'
  and ekbe~vgabe           =  '1'
  and node~tor_id          = '00000000006100005246'
