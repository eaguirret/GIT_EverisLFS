select Distinct
       NV1_3~orden,
       NV1_3~componente,
       MBEW~VPRSV,
       MBEW~KALN1,
       MBEWH~LFGJA,
       MBEWH~LFMON,
       MBEWH~LBKUM,
       substring( NV1_3~anio_periodo,1,4 ) as anno,
       substring( NV1_3~anio_periodo,5,2 ) as mes
from MBEW
inner join MBEWH
on MBEW~MATNR = MBEWH~MATNR and
   MBEW~BWKEY = MBEWH~BWKEY and
   MBEW~VPRSV = 'S'
inner join Z_TF_ORDPROREAL_NV1_3 as NV1_3
on MBEW~MATNR = NV1_3~componente
   and MBEW~BWKEY = NV1_3~centro_consumo
inner join MLDOC
on MBEW~KALN1 = MLDOC~KALNR AND
   concat( MLDOC~JAHRPER,
where orden = '001300000001' and
      componente = '000000000060000045'




select Distinct
       NV1_3~orden,
       NV1_3~componente,
       MBEW~VPRSV,
       MBEW~KALN1,
       MBEWH~LFGJA,
       MBEWH~LFMON,
       MBEWH~LBKUM,
       MBEWH~SALK3,
       MLDOC~QUANT,
       MLDOC~STVAL,
       MLDOC~PRD,
       MLDOC~KDM,
       MLDOC~jahrper,
       NV1_3~anio_periodo,
       substring( NV1_3~anio_periodo,1,4 ) as anno,
       substring( NV1_3~anio_periodo,5,2 ) as mes
from MBEW
inner join MBEWH
on MBEW~MATNR = MBEWH~MATNR and
   MBEW~BWKEY = MBEWH~BWKEY and
   MBEW~VPRSV = 'S'
inner join Z_TF_ORDPROREAL_NV1_3 as NV1_3
on MBEW~MATNR = NV1_3~componente
   and MBEW~BWKEY = NV1_3~centro_consumo
inner join MLDOC
on MLDOC~KALNR = MBEW~KALN1 and
   MLDOC~CATEG in ( 'ZU', 'ND', 'VP' ) and
   MLDOC~ptyp in ( 'BB', 'BF', 'BU', '', 'BC' ) and
   MLDOC~waers = 'PEN'
   "and MLDOC~jahrper = NV1_3~anio_periodo
where orden = '001300000001' and
      componente = '000000000060000045'




select * from Z_TF_ORDPROREAL_NV1_3
select * from Z_TF_ORDPROREAL_NV2_5
select * from Z_TF_ORDPROREAL_NV3_5
