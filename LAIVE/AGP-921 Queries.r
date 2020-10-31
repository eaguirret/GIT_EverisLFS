
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

