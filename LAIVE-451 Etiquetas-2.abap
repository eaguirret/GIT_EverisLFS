SELECT

        tori~base_btd_id,
        tori~base_btditem_id,
        tori~parent_key,
        tori~item_cat,
        tori~qua_pcs_val,
        tori~product_id,
        tori~del_ear_reqtrq,
        tori~orig_btd_id,
        tori~platenumber
*        mara~matnr,
*        makt~maktx,
*        mean~ean11,
*        mean~eantp
*        mch1~vfdat,
*        vbak~bstnk
*        likp~vbeln,
*        likp~wadat_ist
*        lips~posnr,
*        lips~charg,
*        lips~lfimg,
FROM /scmtms/d_torrot as torr
inner join /scmtms/d_torite as tori
    on tori~parent_key = torr~db_key
       and tori~base_btd_id IS NOT NULL
WHERE torr~tor_id = '00000000006100018000'




SELECT * from vbak where vbeln = '0000119877'
        mara~matnr,
        makt~maktx,
        mean~ean11,
        mean~eantp,
        mch1~vfdat,
        vbak~bstnk,
        likp~vbeln,
        likp~wadat_ist,
        lips~posnr,
        lips~charg,
        lips~lfimg
FROM likp
inner join mara
    on mara~matnr = tori~product_id
inner join makt
    on makt~matnr = mara~matnr
inner join mean
    on mean~matnr = mara~matnr
inner join zsd_mat_calculo as zsdcalc
    on zsdcalc~matnr = mara~matnr
inner join lips
    on lips~vbeln = likp~vbeln
inner join mch1
    on mch1~matnr = lips~matnr
   and mch1~charg = lips~charg
inner join vbak
    on  vbak~vbeln = likp~vbeln
inner join zsd_tipo_almac as zsdalmanac
    on zsdalmanac~bstnk = vbak~bstnk
WHERE likp~wadat_ist IS NULL




SELECT right( tori~base_btd_id, 10 ) as  base_btd_id
FROM /scmtms/d_torrot as torr
inner join /scmtms/d_torite as tori
    on tori~parent_key = torr~db_key
       and tori~base_btd_id IS NOT NULL
WHERE torr~tor_id = '00000000006100018000'
