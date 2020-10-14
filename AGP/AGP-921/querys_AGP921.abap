SELECT afpo~kdauf, 
       afpo~posnr, 
       afpo~pwerk,
       aufk~zzcolor, 
       aufk~zzst_toc, 
       aufk~auart,
       afko~dispo, 
       afko~gstrp, 
       afko~gltrp, 
       afko~cy_seqnr,
       caufv~ftrmi
FROM afpo
inner join aufk
on afpo~aufnr = aufk~aufnr
inner join afko
on afpo~aufnr = afko~aufnr
inner join caufv
on afpo~aufnr = caufv~aufnr
WHERE afpo~aufnr = '000010000074'

**********************************************************************

select Distinct
       vbpa~parvw,
       vbak~vkbur,
       vbak~submi,
       vbrp~vbeln,
       ztvbrk~fsalida, 
       ztvbrk~fllegada, 
       ztvbrk~awb,
       vbrk~fkdat,
       vbkd~bstdk,
       vbkd~inco1
from vbpa
inner join vbak
on vbpa~vbeln = vbak~vbeln 
   AND vbpa~parvw EQ 'AG'
inner join vbrp
on vbrp~aubel = vbpa~vbeln
inner join ztvbrk
on ztvbrk~factamerica = vbrp~vbeln   
inner join vbrk   
on vbrk~vbeln = vbrp~vbeln
inner join vbkd
on vbkd~vbeln = vbpa~vbeln

**********************************************************************

select makt~maktx,
       marm~umren  
from makt
inner join marm
on    makt~matnr = marm~matnr 
  and makt~spras = 'S'
  and marm~meinh = 'M2'
  
**********************************************************************

select 
      inob~cuobj,
      inob~klart,
      ausp~atinn,
      ausp~atwrt,
      cawn~atzhl,
      cawnt~atwtb
from inob
inner join ausp
on  inob~cuobj = ausp~objek 
inner join cawn
on  ausp~atwrt = cawn~atwrt 
    and ausp~atinn = cawn~atinn
inner join cawnt
on  cawnt~atzhl = cawn~atzhl
    and cawnt~spras = 'S'
    and cawnt~atinn = cawn~atinn
where 
    inob~klart in ('001','019')   
 
 
 
 
"EMAT - Enrique Aguirre - AGP-921 - 13.10.2020 - I
DATA: lr_aufnr TYPE RANGE OF aufnr.

lr_aufnr = VALUE #( FOR ls_aufnr IN lt_ioconf_tab
                  ( sign = 'I'
                    option = 'EQ'
                    low = ls_aufnr-aufnr )
                  ).

    DELETE ADJACENT DUPLICATES FROM lr_aufnr.

    SELECT afpo~kdauf,
           afpo~posnr,
           afpo~pwerk,
           aufk~zzcolor,
           aufk~zzst_toc,
           aufk~auart,
           afko~dispo,
           afko~gstrp,
           afko~gltrp,
           afko~cy_seqnr,
           caufv~ftrmi
    FROM afpo
    inner join aufk
    on afpo~aufnr = aufk~aufnr
    inner join afko
    on afpo~aufnr = afko~aufnr
    inner join caufv
    on afpo~aufnr = caufv~aufnr
    WHERE afpo~aufnr in @lr_aufnr
    INTO TABLE @data(it_aufnr).



DATA: lr_zped_clie TYPE RANGE OF vbeln.

lr_zped_clie = VALUE #( FOR wa_zped_clie IN lt_ioconf_tab
                  ( sign = 'I'
                    option = 'EQ'
                    low = wa_zped_clie-zped_clie )
                      ).

    DELETE ADJACENT DUPLICATES FROM lr_zped_clie.

IF lr_zped_clie[] IS NOT INITIAL.
    select Distinct
           vbpa~parvw,
           vbak~vkbur,
           vbak~submi,
           vbrp~vbeln,
           ztvbrk~fsalida,
           ztvbrk~fllegada,
           ztvbrk~awb,
           vbrk~fkdat,
           vbkd~bstdk,
           vbkd~inco1
    from vbpa
    inner join vbak
    on vbpa~vbeln = vbak~vbeln
       AND vbpa~parvw EQ 'AG'
    inner join vbrp
    on vbrp~aubel = vbpa~vbeln
    inner join ztvbrk
    on ztvbrk~factamerica = vbrp~vbeln
    inner join vbrk
    on vbrk~vbeln = vbrp~vbeln
    inner join vbkd
    on vbkd~vbeln = vbpa~vbeln
    where vbpa~vbeln in @lr_zped_clie
    into TABLE @data(it_vbeln).
ENDIF.

DATA: lr_matnr TYPE RANGE OF matnr.

lr_matnr = VALUE #( FOR wa_matnr IN lt_ioconf_tab
                  ( sign = 'I'
                    option = 'EQ'
                    low = wa_matnr-matnr )
                  ).

DELETE ADJACENT DUPLICATES FROM lr_matnr.

select makt~maktx,
       marm~umren
from makt
inner join marm
on    makt~matnr = marm~matnr
  and makt~spras = 'S'
  and marm~meinh = 'M2'
inner join inob
on  inob~objek = marm~matnr
inner join ausp
on  inob~cuobj = ausp~objek
    and inob~klart in ('001','019')
inner join cawn
on  ausp~atwrt = cawn~atwrt
    and ausp~atinn = cawn~atinn
inner join cawnt
on  cawnt~atzhl = cawn~atzhl
    and cawnt~spras = 'S'
    and cawnt~atinn = cawn~atinn
where makt~matnr in @lr_matnr
into table @data(it_maktinob).


"EMAT - Enrique Aguirre - AGP-921 - 13.10.2020 - I

   
   