SELECT
        tori~base_btd_id,
        tori~base_btditem_id,
        tori~product_id,
        tori~orig_btd_id,
        tori~parent_key,
        tori~item_cat,
        tori~qua_pcs_val,
        tori~del_ear_reqtrq,
        tori~platenumber
FROM /scmtms/d_torrot as torr
inner join /scmtms/d_torite as tori
    on tori~parent_key = torr~db_key
       and tori~base_btd_id IS NOT NULL
WHERE torr~tor_id = '00000000006100018000'

**********************************************************************

select vbak~vbeln,
       vbak~bstnk,
       zsdalmanac~tipo_almac,
       zsdalmanac~cod_almacen
from vbak
inner join zsd_tipo_almac as zsdalmanac
    on zsdalmanac~bstnk = vbak~bstnk
where vbak~vbeln in
( SELECT  Distinct right( tori~orig_btd_id, 10 )  as dato
              FROM /scmtms/d_torrot as torr
              inner join /scmtms/d_torite as tori
                    on tori~parent_key = torr~db_key
                     and tori~base_btd_id IS NOT NULL
              WHERE torr~tor_id = '00000000006100018000' )

**********************************************************************

select Distinct
       lips~vbeln,
       mch1~charg,
       mch1~vfdat
from lips
inner join mch1
    on mch1~matnr  = lips~matnr
    and mch1~charg = lips~charg
left outer join likp
on lips~vbeln = likp~vbeln and
   likp~wadat_ist IS NULL
where lips~vbeln in
    ( SELECT  right( tori~base_btd_id, 10 )  as dato
    FROM /scmtms/d_torrot as torr
    inner join /scmtms/d_torite as tori
        on tori~parent_key = torr~db_key
           and tori~base_btd_id IS NOT NULL
    WHERE torr~tor_id = '00000000006100018000' )
and likp~vbeln is null


**********************************************************************


select mara~matnr,
       makt~maktx,
       mean~ean11,
       mean~eantp
from mara
inner join makt
    on makt~matnr = mara~matnr
inner join mean
    on mean~matnr = mara~matnr
inner join zsd_mat_calculo as zsdcalc
    on zsdcalc~matnr = mara~matnr
where mara~matnr in
( SELECT  Distinct tori~product_id as material
              FROM /scmtms/d_torrot as torr
              inner join /scmtms/d_torite as tori
                    on tori~parent_key = torr~db_key
                     and tori~base_btd_id IS NOT NULL
              WHERE torr~tor_id = '00000000006100018000' )



 
