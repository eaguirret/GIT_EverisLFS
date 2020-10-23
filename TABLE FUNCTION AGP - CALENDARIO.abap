class ZCLASS_CALENDARIO_CO definition
  public
  final
  create public .

public section.
INTERFACES
if_amdp_marker_hdb.
CLASS-METHODS get_update
for table function ZTF_CALENDARIO_CO.

ENDCLASS.



CLASS ZCLASS_CALENDARIO_CO IMPLEMENTATION.


method get_update BY DATABASE FUNCTION FOR HDB LANGUAGE
                     SQLSCRIPT OPTIONS READ-ONLY
                     USING VBAP VBAK VBEP INOB KNA1 AUSP CAUFV JEST AFRU VBKD MARM AFPO VBPA CAWN CAWNT MAST STPO AFVC CRHD VBAKUK T052U TVKBT MATDOC MARA ZTPESOBALANZA WB2_V_VBRK_VBRP2 KNVV MBVMBEW.
Return

Select distinct
--
    PedidoItem.MANDT as Mandante,
    1 as num,
    PedidoItem.Werks as Planta,
    PedidoItem.KWMENG as QtyPecasPorPos,
    --( Select count( 0 ) from VBAP subitem where subitem.VBELN = PedidoItem.vbeln and subitem.POSNR = PedidoItem.PosNr and subitem.mandt = PedidoItem.Mandt) as QtyPos,
    (select sum(KWMENG) from VBAP subitem where subitem.VBELN = Pedido.VBELN and subitem.MANDT = PedidoItem.MANDT) as QtyTotalPecas,
    (select min(concat(ERSDA, ERZET)) from AFRU where MANDT = Pedido.Mandt and AUFNR = Ordem.AUFNR/* and ZZKTSCH IN ('01VEXTU','01VEXT','VP01')*/) as LiberationDate, -- parametrizar campos das outras plantas!
    -- InternalDate -> Sera Feito pelo Miguel
   PedidoItemData.EDATU as OrderDate,
   PedidoItemOrdem.GLTRP as EndDate,
   PedidoItemOrdem.GSTRP as StartDate,
   (select max(concat(ERSDA, ERZET)) from AFRU where AUFNR = Ordem.AUFNR and ZZTIPO_NOTIF IN ('RECHAZO')) as LastRecDate,
   -- CreationDate -> Puxar Direto do Genesis
   -- PlantDate -> Genesis
   -- _TSUpdateUTC -> Sera feito no SQL
   Pedido.VBELN as SapOrder,
    PedidoItem.POSNR as Pos,
   -- GenesisOrder -> sera buscado do Genesis!
    Cliente.KUNNR as ClientCode,
    Cliente.NAME1 as ClientName,
    RecMercadoria.KUNNR as RecMercadoriaCode,
    RecMercadoria.NAME1 as RecMercadoriaClientName,
    replace(concat(VehMarca.ATWRT,concat('- -',concat(VehModel.ATWRT,concat('- -',(concat(VehConfig.ATWRT,concat('- -',VehYear.ATFLV))))))),'-','') as VehName, -- verificar porque esta vindo sem ano - OK
   VehCod.ATWRT as VehCode,
   AGPVersion.ATWRT as ManVersion, -- -> caracteristica versao agp - ok
   TipoPeca.ATWRT as PartCode,
   -- PartShort -> Sera feito no SQL, nao existe no SAP
   TipoPeca_Cawnt.ATWTB as NomeLongo_Peca,
--   SubProduto_CAWNT.ATWTB as ProductProject,
   (
    Select String_AGG( SubProduto_CAWNT.ATWTB,',')
    from AUSP SubProduto
        Left Join CAWN SubProduto_Cawn on SubProduto_Cawn.ATINN = SubProduto.ATINN and SubProduto_Cawn.ATWRT = SubProduto.ATWRT and SubProduto_Cawn.Mandt = PedidoItem.MANDT
    Left Join CAWNT SubProduto_Cawnt on SubProduto_Cawnt.Mandt = PedidoItem.Mandt and SubProduto_Cawnt.ATINN = SubProduto.ATINN and SubProduto_Cawnt.ATZHL = SubProduto_Cawn.ATZHL
        and SubProduto_cawnt.spras = ( CASE PedidoItem.WERKS WHEN 'BR01' THEN 'P' ELSE 'S' END)
    where SubProduto.OBJEK = Caracteristicas001.CUOBJ and SubProduto.Mandt = PedidoItem.MANDT and SubProduto.ATINN = '0000000868'
    ) as ProductProject,
   (select MAX(ZZKTSCH) from AFRU where AFRU.Mandt = PedidoItem.Mandt and AFRU.AUFNR = Ordem.AUFNR AND concat(AFRU.ERSDA, AFRU.ERZET ) =
   (select max (concat(ssafru.ERSDA, ssafru.ERZET )) from AFRU ssafru where ssafru.Mandt = PedidoItem.Mandt and ssafru.AUFNR = Ordem.AUFNR and ssafru.ZZTIPO_NOTIF <> '')
        --concat(AFRU.RUECK, AFRU.RMZHL) =
        --( Select max(concat(RUECK,  RMZHL)) from AFRU ssafru where ssafru.mandt = Pedido.MAndt and ssafru.AUFNR = Ordem.AUFNR and ssafru.ZZTIPO_NOTIF <> '')
    ) as CurrentModelKey,

    (select max (concat(ssafru2.ERSDA, ssafru2.ERZET )) from AFRU ssafru2 where ssafru2.Mandt = PedidoItem.Mandt and ssafru2.AUFNR = Ordem.AUFNR and ssafru2.ZZTIPO_NOTIF <> ''
    ) as fechaCurrentModel,

   CASE (Select MIN(STAT) from JEST Where Mandt = PedidoItem.Mandt and INACT = '' and OBJNR = PedidoItemOrdem.OBJNR
    and Stat IN ( 'I0001', 'I0002', 'I0045', 'I0046') )
        WHEN 'I0001' THEN 'ABERTO'
        WHEN 'I0002' THEN 'LIBERADO'
        WHEN 'I0045' THEN 'ENC.TEC'
        WHEN 'I0046' THEN 'ENC.COS'
        --WHEN 'I0012' THEN 'FORNECIDO'
      ELSE ''
      end as OrderStatus, --> ALGUMAS ESTAO FORNECIDO E ENC. TECNICO 10035178 , CONFIRMAR O QUE E FORNECIDO


    (
    Select MAX(xx.ARBPL) from
    AFRU x
        Inner join CRHD xx on xx.OBJID = x.ARBID and x.mandt = PedidoItem.MANDT
    where x.AUFNR = Ordem.AUFNR AND
                                  x.MANDT = Ordem.MANDT and
                                  x.ZZTIPO_NOTIF <> '' and
                                  x.STZHL = '00000000' and
                                  x.STOKZ = '' AND
                                  concat(x.RUECK, x.RMZHL) =
                                    (select max (concat(TEST.RUECK, TEST.RMZHL))
                                    from AFRU TEST where TEST.AUFNR = Ordem.AUFNR and
                                    concat(TEST.ERSDA, TEST.ERZET) =
                                    (select max (concat(z.ERSDA, z.ERZET )) as Data_Hora
                                    from afru z where z.aufnr = Ordem.aufnr and z.STOKZ = ''
                                    and z.STZHL = '00000000')) ) as FactorySector,
   PedidoItem.MATNR as ZFERcode,
   Ordem.CHARG as LoteLogistico,
   Ordem.AUFNR as ProdOrder,
   Ordem.PLNUM as OrdenProvision,
   CASE WHEN Ordem.AUFNR <> '' THEN 'OF'
   ELSE '' END as TipoOrdem,

   CASE WHEN PedidoItem.ABGRU <> '' then PedidoItem.ABGRU
   WHEN (Select count(0) from AFRU where AUFNR = Ordem.AUFNR and Mandt = PedidoItem.Mandt) =
     0 then '01CORTE'
   WHEN Ordem.AUFNR = '' THEN ''
    ELSE
   (select MIN(concat(concat(w.KTSCH,'-'), x.ARBPL)) from AFVC w
        INNER JOIN CRHD x on x.OBJID = w.ARBID
        where w.AUFPL =
            (Select MAX(AUFPL) from AFRU where AUFNR = Ordem.AUFNR and Mandt = PedidoItem.Mandt)

        and w.VORNR = (select min (MINIMO.VORNR)
                                                            from AFVC MINIMO where MINIMO.AUFPL = w.AUFPL and MINIMO.VORNR >
                                                            (select max(VORNR) from AFRU where AUFNR = Ordem.AUFNR and ZZTIPO_NOTIF IN ('APROVADO','APROBADO'))
                                                            ))
    END as NextModelKey,

   (select count(0) from JEST ssjest
    inner join CAUFV ssCAUFV on ssCAUFV.OBJNR = ssjest.OBJNR
    Where
        ssCAUFV.kdauf = PedidoItem.vbeln and ssCAUFV.kdpos = PedidoItem.PosNr and
    /*ssjest.OBJNR = PedidoItemOrdem.OBJNR and */ssjest.MANDT = PedidoItem.MANDT and ssjest.INACT = '' and ssjest.STAT IN ('I0001','I0002') ) as QtyOpen, -- REVISAR CONTAGEM DE ABERTOS -- 1100002160
   --(select count(0) from JEST Where OBJNR = PedidoItemOrdem.OBJNR and MANDT = PedidoItem.MANDT and INACT = '' and STAT IN ('I0045','I0046')) as QtyDone, --> Sera feito no SQL
   -- OrderType -> sera feito no Genesis
   -- Sets Equivalentes sera calculado no SQL, aqui puxa os 2 campos >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   MedidasMaterial.BREIT as AlturaPeca,
   MedidasMaterial.LAENG as LarguraPeca,
   --<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   PedidoPlanta.LIFNR as ManufactPlant,
   DescMercado.BEZEI as Market,
   Cliente.LAND1 as ClientCountry,
   ReservasMaterial.ROMEN AS ItemArea,--> Verificar se nao e largura
--   BallisticStandard_Cawnt.ATWTB as BallisticStandard, -- removido
--   CertThickness.ATWRT as BallisticThickness, -- removido
   BallisticLevel_Cawnt.ATWTB as LevelAGP,-- duplicado para a colombia
   -- FinalPrice -> Vira do genesis
   PlanCurvo_Cawnt.ATWTB as PlanCurve,
   (CASE PedidoPlantaOfCom.LIFNR WHEN '' THEN Pedido.VKBUR ELSE PedidoPlantaOfCom.LIFNR END) as CommercialUnit,
   PedidoDadosCom.INCO1 as Incoterm,
   PedidoDadosCom.BSTKD as ClientOScode,
   -- Transporte -- Segundo manu nao existe no sap
   FormulaCode.ATWRT as Formula,
   TermPagoDesc.TEXT1 as PaymentTerms,
   ( CASE PedidoItemOrdem.WERKS WHEN 'BR01' THEN MedidasMaterial.BRGEW ELSE
   coalesce(
   ( Select MAX(BRGEW) from ZTPESOBALANZA where Mandt = PedidoItem.Mandt and AUFNR = Ordem.AUFNR and CHARG = Ordem.CHARG),
   Material.ntgew
   )
   END ) Weight,
   -- GlobalStatus -- no SQL
   -- GlassJet -- Nao e possivel pegar do sap
   TempladoQuimico.ATWRT as TempladoQuimico,
   -- FechaRepeccion -> Vira do Genesis
   (
    Select MAX(concat(CPUDT,CPUTM))  FROM MATDOC where mandt = PedidoItem.Mandt and STOCK_QTY > 0 and HEADER_COUNTER = 0 and CHARG_SID = Ordem.CHARG
    and BWART IN('101','311')
    and LGORT = ( CASE PedidoPlanta.LIFNR when 'PBR01' THEN 'PP22' WHEN 'PCO01' THEN 'IM03' ELSE 'P3' END )
     --+ CPUTM = HORA
   ) as StockDate,
   -- PlantPrice -> Vira do Genesis
   (
   CASE WHEN ( Cliente.AUFSD <> '' OR Cliente.FAKSD <> '' OR Cliente.LIFSD <> '' OR ClienteBloqueo.AUFSD <> '') THEN '01' ELSE '' END
   ) as BloqueoCliente,
   Pedido.LIFSK as BloqueoSAC,
   -- Fijado -> consulta provisional
   (select max(ERSDA) from AFRU ssafru4 where ssafru4.Mandt = PedidoItem.Mandt and ssafru4.AUFNR = Ordem.AUFNR and ssafru4.ZZTIPO_NOTIF IN ('APROVADO','APROBADO') and
   ssafru4.ZZKTSCH = (CASE WERKS WHEN 'BR01' THEN 'INSPFIN' ELSE 'PREEMPQ' END )) as FechaNotifFinal, --> verificar porque esta 0 e agregar a hora
--   (Select String_AGG( ATWRT,',') from AUSP where AUSP.OBJEK = Caracteristicas001.CUOBJ and AUSP.mandt = mara.Mandt and AUSP.ATINN = '0000000875') as GeomDif,
--   (Select String_AGG( ATWRT,',') from AUSP where AUSP.OBJEK = Caracteristicas001.CUOBJ and AUSP.mandt = mara.Mandt and AUSP.ATINN = '0000000870') as BehDif,
    (Select Count( 0 ) from AFRU as ssafru5 where ssafru5.AUFNR = Ordem.AUFNR and ssafru5.Mandt = PedidoItem.Mandt and ssafru5.ZZTIPO_NOTIF IN ('REFUGO','RECHAZO' ) ) as QtdRefugo,
--    Case WHEN Mov.Charg <> '' THEN 'MSKA' ELSE 'AFPO' END as Origem
    '' as Origem,
    MaterialPreco.STPRS as CostoLivreUtilizacao
From VBAP PedidoItem --pedido item
    Inner Join VBAK Pedido on Pedido.VBELN = PedidoItem.VBELN and Pedido.MANDT = PedidoItem.MANDT --pedido
    Inner Join VBEP PedidoItemData on PedidoItemData.VBelN = Pedido.VBELN and PedidoItemData.Posnr = PedidoItem.PosNr and PedidoItemData.WMENG >0 and PedidoItemData.Mandt =PedidoItem.MANDT
    Inner Join VBPA PedidoPlanta on PedidoPlanta.MANDT = PedidoItem.MANDT and PedidoPlanta.VBELN = PedidoItem.VBELN and PedidoPlanta.PARVW = 'Z0'
    Left Join VBPA PedidoPlantaOfCom on PedidoPlantaOfCom.MANDT = PedidoItem.MANDT and PedidoPlantaOfCom.VBELN = PedidoItem.VBELN and PedidoPlantaOfCom.PARVW = 'Z1'
    Left Join VBPA PedidoPlantaRecMercadoria on PedidoPlantaRecMercadoria.MANDT = PedidoItem.MANDT and PedidoPlantaRecMercadoria.VBELN = PedidoItem.VBELN and PedidoPlantaRecMercadoria.PARVW = 'WE'
    Left join KNA1 RecMercadoria on RecMercadoria.KUNNR = PedidoPlantaRecMercadoria.KUNNR and RecMercadoria.MANDT = PedidoItem.MANDT
    Inner join KNA1 Cliente on Cliente.KUNNR = Pedido.KUNNR and Cliente.MANDT = PedidoItem.MANDT
    Left Join KNVV ClienteBloqueo on ClienteBloqueo.Mandt = PedidoItem.Mandt and ClienteBloqueo.KUNNR = Pedido.KUNNR and ClienteBloqueo.VKORG = PedidoItem.Werks
    Inner Join VBKD PedidoDadosCom on PedidoDadosCom.MANDT = PedidoItem.Mandt and PedidoDadosCom.VBELN = Pedido.VBELN and PedidoDadosCom.POSNR = PedidoItem.POSNR
    Inner Join VBAKUK PedidoStatus on PedidoStatus.MANDT = PedidoItem.Mandt and PedidoStatus.VBELN = Pedido.VBELN
    Left Join T052U TermPagoDesc on TermPagoDesc.Mandt = PedidoItem.Mandt and TermPagoDesc.ZTERM = PedidoDadosCom.ZTERM
        and TermPagoDesc.SPRAS = ( CASE PedidoItem.WERKS WHEN 'BR01' THEN 'P' ELSE 'S' END )
    Left Join TVKBT DescMercado on DescMercado.Mandt = PedidoItem.Mandt and DescMercado.VKBUR = Pedido.VKBUR
        and DescMercado.SPRAS  = ( CASE PedidoItem.WERKS WHEN 'BR01' THEN 'P' ELSE 'S' END )


--    left join ZCDSMSKAASSOC e on e.Pedido = Pedido.VBELN and PedidoItem.POSNR = e.Pos

/*    left join AFPO TOrdem on TOrdem.KDAUF = Pedido.VBELN and TOrdem.KDPOS = PedidoItem.POSNR and TOrdem.MANDT = Pedido.MANDT and TOrdem.WEMNG = 0
    --Filtro de Ordens Encerradas--
    and (select count(*)
        from CAUFV r
            inner join JEST q on q.OBJNR = r.OBJNR
            inner join AFPO a on a.AUFNR = R.aufnr and a.Mandt = r.Mandt
                where r.AUFNR = TOrdem.AUFNR and q.INACT = ''
            and
            (
                ( q.STAT in('I0001' ,'I0002', 'I0012') )
            OR
                (
                q.STAT in('I0045','I0046') and a.elikz = 'X'
                )
            )

            ) >0
            */

/*    left join WB2_V_VBRK_VBRP2 EXPED on EXPED.AUBEL_I = Pedido.VBELN and  EXPED.AUPOS_I = PedidoItem.POSNR and EXPED.MANDT = Pedido.MANDT
    and EXPED.FKSTO = '' and EXPED.VBTYP in ('M', '5', 'O', 'P')*/

   /*

    left join (select INTER.VBELN, INTER.VBELP, X.CHARG_I from EKKN INTER
    inner join WB2_V_VBRK_VBRP2 X on INTER.EBELN = X.AUBEL_I AND substring(X.AUPOS_I,2) = INTER.EBELP
    where X.FKSTO <> 'X' and X.VBTYP in ('M', '5', 'O', 'P') and INTER.MANDT = '300' and X.MANDT = '300')
    as NF_CD ON NF_CD.VBELN = Pedido.VBELN and NF_CD.VBELP = PedidoItem.POSNR
*/
    /*left join AFPO Ordem on Ordem.CHARG = ( case when e.Lote is not null then e.Lote else
    case when TOrdem.CHARG is not null then TOrdem.CHARG else
    case when EXPED.CHARG_I is not null then EXPED.CHARG_I else NF_CD.CHARG_I end end end )
    and Ordem.MANDT = PedidoItem.MANDT
    */

    /*
    Left Join NSDM_V_MSEG as Mov on Mov.Mandt = Pedido.Mandt and Mov.KDAUF = Pedido.VBELN and Mov.KDPOS = PedidoItem.POSNR and
        Mov.BWART = '413' and Mov.XAUTO = '' and
        ( Select count( 0 ) from NSDM_V_MSEG ssmov where ssmov.Mandt = Pedido.Mandt and ssmov.CHARG = Mov.Charg and ssmov.KDAUF <> Mov.KDAUF and
          concat( ssmov.CPUDT_MKPF, ssmov.CPUTM_MKPF ) > concat( Mov.CPUDT_MKPF, Mov.CPUTM_MKPF ) and BWART = '413' and XAUTO = '' ) <= 0
    */
--    Left Join ZQUERYMSEGCO Mov on Mov.Mandt = Pedido.Mandt and Mov.Pedido = Pedido.VBELN and Mov.Pos = PedidoItem.Posnr

    /*Left Join NSDM_V_MSKA Mov on Mov.mandt = Pedido.Mandt and
         Mov.Charg = ( Select Max( Charg ) from NSDM_V_MSKA ss where ss.Mandt = Pedido.Mandt and
                        ss.VBELN = Pedido.VBELN and ss.Posnr = PedidoItem.Posnr
                         and ERSDA = ( Select max(ERSDA) from NSDM_V_MSKA ss3
                            where ss3.Mandt = Pedido.Mandt and ss3.Vbeln=ss.vbeln and ss3.posnr = ss.posnr) )
*/
    Left Join AFPO Ordem on Ordem.Mandt = Pedido.Mandt and
--        Ordem.CHARG = ( Case WHEN Mov.Lote <> '' THEN Mov.Lote ELSE Ordem.CHARG END ) and
--        Ordem.KDAUF = ( Case WHEN Mov.Lote <> '' THEN Ordem.KDAUF ELSE Pedido.VBELN END ) and
--        Ordem.KDPOS = ( Case WHEN Mov.Lote <> '' THEN Ordem.KDPOS ELSE PedidoItem.POSNR END )

-- antigo, ate 03/08
      --Ordem.KDAUF = Pedido.VBELN and Ordem.KDPOS = PedidoItem.POSNR

      /* and Ordem.WEMNG = 0*/
--    Left Join AFKO MestreOrdemPCP on MestreOrdemPCP.AUFNR = Ordem.AUFNR and MestreOrdemPCP.Mandt = PedidoItem.MANDT
--    Left Join AUFK MestreOrdem on MestreOrdem.AUFNR = Ordem.Aufnr and MestreOrdem.Mandt = PedidoItem.MANDT
-- novo a partir de 03/08
     Pedido.VBELN = ( Case when Ordem.ZZKDAUF = '' THEN Ordem.KDAUF ELSE Ordem.ZZKDAUF END) and
     PedidoItem.POSNR = ( Case when (Ordem.ZZKDPOS = '' OR Ordem.ZZKDPOS = '000000') THEN Ordem.KDPOS ELSE Ordem.ZZKDPOS END )

    left join CAUFV PedidoItemOrdem on PedidoItemOrdem.AUFNR = Ordem.AUFNR and PedidoItemOrdem.MANDT = PedidoItem.MANDT

    Inner Join MARA Material on Material.Mandt = PedidoItem.Mandt and Material.MATNR = PedidoItem.MATNR
    Left join MARM MedidasMaterial on MedidasMaterial.MATNR = PedidoItem.MATNR and MedidasMaterial.MEINH = 'ST' and MedidasMaterial.MANDT = PedidoItem.MANDT
    Left Join MBVMBEW MaterialPreco on MaterialPreco.Mandt = PedidoItem.Mandt and MaterialPreco.matnr = MAterial.Matnr and MaterialPreco.VPRSV = 'S' and MaterialPreco.BWKEY = 'CO01' and MaterialPreco.STPRS <> 0
    /*Left Join MAST ListaMaterial ON
        ListaMaterial.MATNR = PedidoItem.MATNR AND
        ListaMaterial.WERKS = PedidoItem.Werks and
        ListaMaterial.STLAN = '1' and
        ListaMaterial.STLAL = Ordem.VerID*/
    Left Join STPO ReservasMaterial ON
        ReservasMaterial.Mandt = PedidoItem.Mandt and
        ReservasMaterial.STLNR =
            ( Select MAX( STLNR ) from MAST where mandt = PedidoItem.mandt and MATNR = PedidoItem.MATNR and STLAN = '1') and
--        ListaMaterial.STLNR AND
        ReservasMaterial.STLTY = 'M' and
        ReservasMaterial.POSNR = '0100'
/*( Select Min(POSNR) from STPO ss where ss.Mandt = PedidoItem.Mandt and ss.STLNR = ListaMaterial.STLNR AND ss.STLTY = 'M' ) */-- Verificar campo eliminado!

    Left Join INOB Caracteristicas001 on Caracteristicas001.OBJEK = PedidoItem.MATNR and Caracteristicas001.Mandt = PedidoItem.MANDT and Caracteristicas001.Klart = '001'
    --Left Join INOB Caracteristicas023 on Caracteristicas023.OBJEK = PedidoItem.MATNR and Caracteristicas023.KLART ='023' and Caracteristicas023.MANDT = Ordem.MANDT

    Left Join AUSP TipoPeca on TipoPeca.OBJEK = Caracteristicas001.CUOBJ and TipoPeca.Mandt = PedidoItem.MANDT and TipoPeca.ATINN = '0000000894'
    Left Join CAWN TipoPeca_Cawn on TipoPeca_Cawn.ATINN = TipoPeca.ATINN and TipoPeca_Cawn.ATWRT = TipoPeca.ATWRT and TipoPeca_Cawn.Mandt = PedidoItem.MANDT
    Left Join CAWNT TipoPeca_Cawnt on TipoPeca.Mandt = PedidoItem.Mandt and TipoPeca_Cawnt.ATINN = TipoPeca.ATINN and TipoPeca_Cawnt.ATZHL = TipoPeca_Cawn.ATZHL and TipoPeca_Cawnt.spras = ( CASE PedidoItem.WERKS WHEN 'BR01' THEN 'P' ELSE 'S' END )

/*    Left Join AUSP SubProduto on SubProduto.OBJEK = Caracteristicas001.CUOBJ and SubProduto.Mandt = PedidoItem.MANDT and SubProduto.ATINN = '0000000868'
    Left Join CAWN SubProduto_Cawn on SubProduto_Cawn.ATINN = SubProduto.ATINN and SubProduto_Cawn.ATWRT = SubProduto.ATWRT and SubProduto_Cawn.Mandt = PedidoItem.MANDT
    Left Join CAWNT SubProduto_Cawnt on SubProduto_Cawnt.Mandt = PedidoItem.Mandt and SubProduto_Cawnt.ATINN = SubProduto.ATINN and SubProduto_Cawnt.ATZHL = SubProduto_Cawn.ATZHL
        and SubProduto_cawnt.spras = ( CASE PedidoItem.WERKS WHEN 'BR01' THEN 'P' ELSE 'S' END )
*/
    Left Join AUSP VehCod on VehCod.OBJEK = Caracteristicas001.CUOBJ and VehCod.Mandt = PedidoItem.MANDT and VehCod.ATINN = '0000000838'
    Left Join AUSP VehMarca on VehMarca.OBJEK = Caracteristicas001.CUOBJ and VehMarca.Mandt = PedidoItem.Mandt and VehMarca.ATINN = '0000000871'
    Left Join AUSP VehModel on VehModel.OBJEK = Caracteristicas001.CUOBJ and VehModel.Mandt = PedidoItem.Mandt and VehModel.ATINN = '0000000866'
    Left Join AUSP VehConfig on VehConfig.OBJEK = Caracteristicas001.CUOBJ and VehConfig.Mandt = PedidoItem.Mandt and VehConfig.ATINN = '0000000906'
    Left Join AUSP VehYear on VehYear.OBJEK = Caracteristicas001.CUOBJ and VehYear.Mandt = PedidoItem.Mandt and VehYear.ATINN = '0000000893'

    Left Join AUSP BallisticStandard on BallisticStandard.OBJEK = Caracteristicas001.CUOBJ and BallisticStandard.Mandt = PedidoItem.MANDT and BallisticStandard.ATINN = '0000000835'
    Left Join CAWN BallisticStandard_Cawn on BallisticStandard_Cawn.ATINN = BallisticStandard.ATINN and BallisticStandard_Cawn.ATWRT = BallisticStandard.ATWRT and BallisticStandard_Cawn.Mandt = PedidoItem.MANDT
    Left Join CAWNT BallisticStandard_Cawnt on BallisticStandard_Cawnt.Mandt = PedidoItem.Mandt and BallisticStandard_Cawnt.ATINN = BallisticStandard.ATINN and BallisticStandard_Cawnt.ATZHL = BallisticStandard_Cawn.ATZHL
        and BallisticStandard_cawnt.spras = ( CASE PedidoItem.WERKS WHEN 'BR01' THEN 'P' ELSE 'S' END )

    Left Join AUSP BallisticLevel on BallisticLevel.OBJEK = Caracteristicas001.CUOBJ and BallisticLevel.Mandt = PedidoItem.MANDT and BallisticLevel.ATINN = '0000000911'
    Left Join CAWN BallisticLevel_Cawn on BallisticLevel_Cawn.ATINN = BallisticLevel.ATINN and BallisticLevel_Cawn.ATWRT = BallisticLevel.ATWRT and BallisticLevel_Cawn.Mandt = PedidoItem.MANDT
    Left Join CAWNT BallisticLevel_Cawnt on BallisticLevel_Cawnt.Mandt = PedidoItem.Mandt and BallisticLevel_Cawnt.ATINN = BallisticLevel.ATINN and BallisticLevel_Cawnt.ATZHL = BallisticLevel_Cawn.ATZHL
        and BallisticLevel_cawnt.spras = ( CASE PedidoItem.WERKS WHEN 'BR01' THEN 'P' ELSE 'S' END )

    Left Join AUSP FormulaCode on FormulaCode.Mandt = PedidoItem.Mandt and FormulaCode.OBJEK = Caracteristicas001.CUOBJ and FormulaCode.Mandt = PedidoItem.MANDT and FormulaCode.ATINN = '0000000833'
    Left Join AUSP TempladoQuimico on TempladoQuimico.Mandt = PedidoItem.Mandt and TempladoQuimico.OBJEK = Caracteristicas001.CUOBJ and TempladoQuimico.Mandt = PedidoItem.MANDT and TempladoQuimico.ATINN = '0000000865'
    Left Join AUSP CertThickness on CertThickness.Mandt = PedidoItem.Mandt and CertThickness.OBJEK = Caracteristicas001.CUOBJ and CertThickness.Mandt = PedidoItem.MANDT and CertThickness.ATINN = '0000000877'

    Left Join AUSP PlanCurvo on PlanCurvo.OBJEK = Caracteristicas001.CUOBJ and PlanCurvo.Mandt = PedidoItem.MANDT and PlanCurvo.ATINN = '0000000836'
    Left Join CAWN PlanCurvo_Cawn on PlanCurvo_Cawn.ATINN = PlanCurvo.ATINN and PlanCurvo_Cawn.ATWRT = PlanCurvo.ATWRT and PlanCurvo_Cawn.Mandt = PedidoItem.MANDT
    Left Join CAWNT PlanCurvo_Cawnt on PlanCurvo_Cawnt.Mandt = PedidoItem.Mandt and PlanCurvo_Cawnt.ATINN = PlanCurvo.ATINN and PlanCurvo_Cawnt.ATZHL = PlanCurvo_Cawn.ATZHL
        and PlanCurvo_cawnt.spras = ( CASE PedidoItem.WERKS WHEN 'BR01' THEN 'P' ELSE 'S' END )

    Left Join AUSP AGPVersion on AGPVersion.Mandt = PedidoItem.Mandt and AGPVersion.OBJEK = Caracteristicas001.CUOBJ and AGPVersion.mandt = PedidoItem.Mandt and AGPVersion.ATINN = '0000000876'
/*
    Left Join
        ( Select OBJNR,STAT,MANDT from JEST Where INACT = '' and STAT IN
            ( 'I0001', 'I0002', 'I0045', 'I0046' ) and Mandt = '300' ) OrdemStatus ON OrdemStatus.OBJNR = PedidoItemOrdem.OBJNR
*/

Where
    PedidoItem.MANDT = '300' AND
    PedidoPlanta.LIFNR = 'PCO01' AND
    PedidoItem.SPART IN ( 'S0','S1','S3' ) AND
    Material.SPART IN ( 'S0','S1','S3' ) AND
--    PedidoStatus.LFSTK <> 'C' AND PedidoStatus.ABSTK <> 'C' AND
--    PedidoItem.ABGRU = '' AND -- Motivo de recusa em branco
    ( select count( VBELN ) from WB2_V_VBRK_VBRP2 FacturaLote where FacturaLote.Mandt = PedidoItem.Mandt and FacturaLote.CHARG_I = Ordem.CHARG and
    FacturaLote.fksto = '' and FacturaLote.vbtyp IN ( 'M', 'O', '5' )) = 0

    -- tirar EGLASS
    --Left Join WB2_V_VBRK_VBRP2 FacturaLote on

    --FacturaLote.VBELN = ''
    /*
    AND 1 =
    (
    CASE WHEN PedidoPlanta.LIFNR = 'PBR01' AND QtdRefugo > 0 then 0
    ELSE 1 END
    )*/

-- verificar com edemar sobre os itens cancelados,como remover da consulta.

    --PedidoItem.ABGRU = ''-- and
/*    ((fp.LIFNR in ('PBR01','PBR21')) or (fp.KUNNR in ('PBR21','PBR01')) or
    PedidoItem.WERKS in ('BR01','BR21'))*/

/*group by PedidoItem.MANDT, PedidoItem.VBELN, Pedido.BSTNK, h.NAME1, Pedido.VDATU, PedidoItem.POSNR, c.OBJNR, k.KTEXT, PedidoItem.MATNR, g.CUOBJ, b.BREIT, b.LAENG, Pedido.AUART, TOrdem.AUFNR,
i.ZZTIPO_NOTIF, i.ZZKTSCH, estoque.LGORT, estoque.KALAB, Ordem.AUFNR, c.GLTRI, estoque.CHARG, Ordem.CHARG, EXPED.POSNR_I, EXPED.CHARG_I, NF_CD.CHARG_I,
NF_CD.VBELN, PLACE.LGTYP, PLACE.LGPLA, Pedido.ERDAT, Pedido.LIFSK, Pedido.VDATU, dispo.DISPO, ST.ARBPL, i.ZZDESTINO, CAMINHO.AUFPL, CAMINHO.VORNR, c.AUFPL,
c.DISPO*/
;

ENDMETHOD.
ENDCLASS.
