
*
*SELECT  bsart,
*        bnfpo,
*        matnr,
*        knttp,
*        pstyp,
*        ematn,
*        meins,
*        lpein,
*        lfdat,
*        matkl,
*        ekgrp,
*        afnam,
*        bednr,
*        lifnr,
*        txz01,
*        preis,
*        menge,
*        ekorg,
*        banfn,
*        peinh,
*        sub_packno,
*        wgbez,
*        ernam,
*        loekz,
*        lgort,
*        maxcorre,
*        flag
*FROM zvnmm_docsp_sped
*WHERE ( flag  IN ('M','N')
*AND ( ( flag = 'M'
*AND ( fec_hora >= '20201023133000' AND
*      fec_hora <= '20201023134241' ) )
*OR ( flag = 'N' AND badat IN ( '20201023' ) ) ) )~


*select * from zvnmm_docsp_sped
**********************************************************************


select
         a~bsart,
         a~banfn,
         a~bnfpo,
         a~matnr,
         a~knttp,
         a~pstyp,
         a~ematn,
         a~meins,
         a~lpein,
         a~lfdat,
         a~matkl,
         a~ekgrp,
         a~afnam,
         a~bednr,
         a~lifnr,
         a~txz01,
         a~preis,
         a~menge,
         a~ekorg,
         a~peinh,
         b~sub_packno,
         c~wgbez,
         a~ernam,
         a~loekz,
         a~lgort,
         e~objectid,
         e~udate,
         e~utime,
         d~ebeln,
         a~ebeln as Pedido,
         a~badat
from eban as a
      left outer join esll as b
        on a~packno = b~packno
      left outer join t023t as c
        on  c~matkl = a~matkl
       and  c~spras = 'S'
       left outer  join ekpo as d
       on  d~ebeln = a~ebeln
       and d~ebelp = a~ebelp
       and d~loekz = 'L'
       left outer join cdhdr as e
       on  e~objectid   = a~banfn
       and e~objectclas = 'BANF'
       left outer join zvnmm_trama_sped as f
         on f~nrosolped  = a~banfn
        and f~possolped  = a~bnfpo
where a~konnr = ''
  and a~bsart = 'ZM'
  and ( a~loekz = ''

  or exists

(
select trama~campo21
from zttrama_interf  as trama
where trama~interfaz = 'Solicitud pedido'
and trama~campo21 = a~banfn and
trama~error = '' and
trama~contador in
(
    select max( contador )
    from zttrama_interf as b
    where b~interfaz = 'Solicitud pedido'
    and b~campo21 = trama~campo21 and
    b~error = ''
)

) )

  and ( a~ebeln = ' ' or
        d~ebeln <> ' ' )

*
*SELECT * FROM ZCDS_SLPTRAMAX
*
*
*select a~campo21
*from zttrama_interf  as a
*where a~interfaz = 'Solicitud pedido'
*and a~campo21 = '1910000760' and
*a~error = '' and
*a~contador in
*(
*    select max( contador )
*    from zttrama_interf as b
*    where b~interfaz = 'Solicitud pedido'
*    and b~campo21 = a~campo21 and
*    b~error = ''
*)


*
*1910000017
*1910000760


zvnmm_docre_sped
