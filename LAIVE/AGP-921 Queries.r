

select Distinct
       afru~aufnr
from afru
inner join afpo
  on AFRU~aufnr = afpo~aufnr
inner join inob
  on afpo~matnr = INOB~OBJEK
inner join AUSP
    on AUSP~OBJEK = INOB~CUOBJ
       and INOB~KLART = '001'
 where AUSP~ATINN  IN @rng_atinn
       and AUSP~ATWRT IN @lr_atwrt
       and afru~aufnr IN @s_aufnr
       and afru~werks in @S_WERKS
 INTO TABLE @DATA(it_aufnr_).



 
select count( distinct 
afru~aufnr ) as contador 
from afru
inner join afpo
on afru~aufnr = afpo~aufnr
inner join inob
on afpo~matnr = inob~objek
and inob~klart = '001'




select  Distinct
afru~aufnr as afru_aufnr,
afru~werks as afru_werks,
afpo~aufnr as afpo_aufnr,
afpo~matnr as afpo_matnr, 
inob~objek as inob_objek,
inob~cuobj as inob_cuobj,
inob~klart as inob_klart,
ausp~objek as ausp_objek,
ausp~atinn as ausp_atinn,
ausp~atwrt as ausp_atwrt
from ausp     
INNER JOIN inob
ON   ausp~objek = inob~cuobj    and
inob~klart = '001'         and
ausp~atinn = '0000000911'  and
ausp~atwrt = '03'
inner join afpo
on afpo~matnr = inob~objek 
inner join afru
on   afru~aufnr = afpo~aufnr and
afru~werks = 'CO01'

SELECT AFRU~werks,
       AFRU~AUFNR,
       AFRU~RUECK,
       AFRU~RMZHL,
       CAWNT~ATWTB
FROM AFRU
INNER JOIN CRHD
ON CRHD~OBJID = AFRU~ARBID

INNER JOIN INOB
ON CONCAT( CRHD~WERKS, CRHD~ARBPL ) =
INOB~OBJEK 
AND INOB~KLART = '019'

INNER JOIN AUSP
ON AUSP~OBJEK = INOB~CUOBJ AND
   AUSP~ATINN = '0000001457'

INNER JOIN CAWN
ON CAWN~ATINN = AUSP~ATINN AND
CAWN~ATWRT = AUSP~ATWRT

INNER JOIN CAWNT
ON CAWNT~ATINN = CAWN~ATINN AND
   CAWNT~ATZHL = CAWN~ATZHL AND
   CAWNT~ADZHL = CAWN~ADZHL AND
   CAWNT~SPRAS = 'S'
where AFRU~AUFNR = '000010259646'
      
*000010259646      
*000006397701
*WHERE concat( AFRU~RUECK, AFRU~RMZHL ) in
*( '000639770100000001', '000639770100000002' )

*where AFRU~RUECK IN ( '0006397703', '0006397701' ) AND
*      AFRU~RMZHL IN ( '00000001', '00000002' )   
   
