function zpp_confirmacion_pickin_ov_wms.
*"----------------------------------------------------------------------
*"*"Interfase local EMAT
*"  IMPORTING
*"     VALUE(INPUT) TYPE  ZSYNC_SAPSHIPMENT
*"----------------------------------------------------------------------
***********************************************************************************
*      H I S T O R I A L    D E    M O D I F I C A C I O N E S                    *
***********************************************************************************
*^ Fecha: 25.06.2019    Autor: Andrés Vivas - Everis Perú                         *
*^ Marca: @0001                                                                   *
*^ Descripción del cambio: Ajustes generales                                      *
*^ Código Requerimiento  : <Código de Requerimiento>                              *
***********************************************************************************
*^ Fecha: 15.03.2020    Autor: Christian Astete                                    *
*^ Marca: @0001                                                                    *
*^ Descripción del cambio: Proceso de Expedición por Constante solo funcionaba PRD *
*^ Código Requerimiento  : LAIVELFS-262                                            *
***********************************************************************************

  data(lwa_data) = input-data_area.
  data(lt_confir_exp) = lwa_data-sapshipment[].

  types: begin of ty_consolidada.
****Datos del Detalle
  types:
    correlativo_d      type zlt_tramadetalle-correlativo      , "Correlativo
    almacen_sap_d	     type zlt_tramadetalle-almacen_sap      , "Almacén
    orden_externa_d	   type zlt_tramadetalle-orden_externa    , "Documento de operación comercial base
    numero_trans_d     type zlt_tramadetalle-numero_trans     , "Numero de Transporte
    tipo_de_doc_d      type zlt_tramadetalle-tipo_de_doc      , "Tipo de Documento
    linea_externa_d    type zlt_tramadetalle-linea_externa    , "Posición del documento base para operación comercial
    lpn_atributo_7_d   type zlt_tramadetalle-lpn_atributo_7   , "Este campo va Vacio
    fec_ven_atri_2_d   type zlt_tramadetalle-fec_ven_atri_2   , "Este dato es la fecha de vencimiento
    consist_atri_3_d   type zlt_tramadetalle-consist_atri_3   , "consistencia
    lote_grup_atri9_d  type zlt_tramadetalle-lote_grup_atri9  , "Va vacio
    direcci_atri_8_d   type zlt_tramadetalle-direcci_atri_8   , "Direccionamiento
    direcci_atri_2_d   type zlt_tramadetalle-direcci_atri_2   , "Va vacio
    turno_canal_d      type zlt_tramadetalle-turno_canal      , "z_consolidado
    receptor_d         type zlt_tramadetalle-receptor         , "clientes WMS
    cliente_d	         type zlt_tramadetalle-cliente          , "Destinatario
    cantidad_orden_d   type zlt_tramadetalle-cantidad_orden   , "Cantidad_orden
    cantidad_orden_d1  type zlt_tramadetalle-cantidad_orden   , "Cantidad_orden
****Datos de la Trama
    orden_externa_t    type zlt_trama-orden_externa       , "Documento de operación comercial base
    z_agrupador_t      type zlt_trama-z_agrupador         , "Agrupador
    linea_externa_t    type zlt_trama-linea_externa       , "Posición del documento base para operación comercial
    receptor_t         type zlt_trama-receptor            , "clientes WMS
    numero_trans_t     type zlt_trama-numero_trans        , "Numero de Transporte
    tipo_de_doc_t      type zlt_trama-tipo_de_doc         , "Tipo de Documento
    almacen_sap_t      type zlt_trama-almacen_sap         , "Almacén
    lpn_atributo_7_t   type zlt_trama-lpn_atributo_7      , "Este campo va Vacio
    fec_ven_atri_2_t   type zlt_trama-fec_ven_atri_2      , "Este dato es la fecha de vencimiento
    consist_atri_3_t   type zlt_trama-consist_atri_3      , "consistencia
    lote_grup_atri9_t  type zlt_trama-lote_grup_atri9     , "Va vacio
    direcci_atri_8_t   type zlt_trama-direcci_atri_8      , "Direccionamiento
    direcci_atri_2_t   type zlt_trama-direcci_atri_2      , "Va vacio
    turno_canal_t      type zlt_trama-turno_canal         , "z_consolidado
    cliente_t          type zlt_trama-cliente             , "Destinatario
    item_t             type zlt_trama-item                , "Prod.
    propietario_t      type zlt_trama-propietario         , "Dato por default: “LAIVE”.
    unidad_de_medida_t type zlt_trama-unidad_de_medida    , "Unidad de medida de cantidad
    vida_util_tvu_t    type zlt_trama-vida_util_tvu       , "TVU
    cantidad_orden_t   type zlt_trama-cantidad_orden      , "Cantidad prevista en unidad de medida de venta
    cantidad_moldes_t  type zlt_trama-cantidad_moldes     , "Cantidad prevista en unidad de medida de venta
    factor_sap_t       type zlt_trama-factor_sap          , "Cantidad prevista en unidad de medida de venta
    udm_sap_t          type zlt_trama-udm_sap             , "Unidad de medida base
    z_etenr_t          type zlt_trama-z_etenr             , "Nº de reparto
    z_edatu_t          type zlt_trama-z_edatu             . "Fecha de reparto
  "Datos trama retorno
      include structure zlt_trama_retor.
  types: salvar        type c length 1.
  types: eliminar      type c length 1.
  types: end of ty_consolidada.

  types: begin of ty_calculos,
           orden_externa_d   type zlt_tramadetalle-orden_externa    , "Documento de operación comercial base
           linea_externa_d   type zlt_tramadetalle-linea_externa    , "Posición del documento base para operación comercial
           cantidad_orden_d1 type zlt_tramadetalle-cantidad_orden   , "Cantidad_orden
           cod_picking       type zlt_trama_retor-cod_picking       , "Codigo de picking
           cantidad_picking  type zlt_trama_retor-cantidad_picking  , "Cantidad de picking
         end of ty_calculos.


  types: begin of y_entre,
           entrega type vbeln_vl,
           posnr   type posnr,
         end of y_entre,

         begin of y_consoli,
           vbeln_i type lips-vbeln,
           posnr_i type lips-posnr,
           vbeln_r type vbap-vbeln,
           posnr_r type vbap-posnr,
           kwmeng  type vbap-kwmeng,
           porcen  type p decimals 10,
         end of y_consoli,

         begin of y_temp,
           vbeln_i type lips-vbeln,
           posnr_i type lips-posnr,
           posnr_r type lips-posnr,
           porcen  type p decimals 10,
         end of y_temp,

         begin of y_entpos,
           vgbel type lips-vgbel, "Entrega
           vgpos type lips-vgpos,
         end of y_entpos.

  types: begin of ty_turbox,
           tor_id  type zlt_estatus-z_orden,
           almacen type zlt_trama_retor-almacen,
         end of ty_turbox.

  types: begin of y_lips_aux01,
           vbeln type lips-vbeln,
           posnr type lips-posnr,
           uepos type lips-uepos,
           uepvw type lips-uepvw,
         end of y_lips_aux01.

  types: begin of ty_lips_au,
           vbeln type lips-vbeln,
           posnr type lips-posnr,
           uepos type lips-uepos,
           uepvw type lips-uepvw,
         end of ty_lips_au.


  data:
    lt_calculos           type standard table of ty_calculos,
    lt_tablecon           type standard table of ty_consolidada,
    lt_tablecon_nc        type standard table of ty_consolidada,
    lt_tablecon_cs        type standard table of ty_consolidada,
    lt_tablecon_cd        type standard table of ty_consolidada,
    lt_tablecon_cd_lo     type standard table of ty_consolidada,
    lt_tablecon_nc_a      type standard table of ty_consolidada,
    lt_tablecon_cs_a      type standard table of ty_consolidada,
    lt_tablecon_cd_a      type standard table of ty_consolidada,
    lt_tablecon_ord       type standard table of ty_consolidada,
    lt_tablecon_ordf      type standard table of ty_consolidada,
    lt_tablecon_f         type standard table of ty_consolidada,
    lt_tablecon_aux       type standard table of ty_consolidada,
    lt_zlt_trama_retor    type standard table of zlt_trama_retor,
    lt_zlt_trama          type standard table of zlt_trama,
    lt_zlt_tramadetalle   type standard table of zlt_tramadetalle,
    lt_zlt_tramadetalle_f type standard table of zlt_tramadetalle,
    lt_zlt_tramadetalle2  type standard table of  zlt_tramadetalle,
    lt_lips_c             type standard table of lips,
    lt_lips_c2            type standard table of lips,
    lt_vbap               type standard table of vbap,
    t_entpos              type standard table of y_entre,
    lt_consoli            type standard table of y_consoli,
    lt_temp               type standard table of y_temp,
    lt_entpos             type standard table of y_entpos,
    lt_lips               type standard table of lips,
    lt_mch1_ul            type standard table of mch1,
    lt_likp_ul            type standard table of likp,
    lt_tablecon_eli       type standard table of ty_consolidada,
    lt_item_data_eli      type standard table of bapiobdlvitemchg,
    lt_item_control_eli   type standard table of bapiobdlvitemctrlchg,
    lt_lips_eli           type standard table of lips,
    lt_ausp_fi            type standard table of ausp,
    lt_turbox             type standard table of ty_turbox,
    lt_lips_aux01         type standard table of y_lips_aux01,
    lt_zlt_estatus2       type standard table of zlt_estatus,
    lt_lips_au            type standard table of ty_lips_au.

  data:
    t_item_data     type table of bapiobdlvitemchg,
    t_item_control  type table of bapiobdlvitemctrlchg,
    t_item_data_spl type table of /spe/bapiobdlvitemchg,
    t_itemdata      type table of bapishipmentitem,
    t_return        type table of bapiret2,
    t_cwm_item_data type table of /cwm/bapiobdlvitem.

  "Word Area
  data:
    wa_tramadetalle       like line of lt_zlt_tramadetalle_f.

  "Rangos
  data:
    rg_fecven2    type range of zlt_trama-fec_ven_atri_2,
    rg_conatr2    type range of zlt_trama-consist_atri_3,
    rg_direcc2    type range of zlt_trama-direcci_atri_8,
    rg_tvu        type range of zee_tvu,
    rg_fecven     type range of zlt_tramadetalle-fec_ven_atri_2,
    rg_conatr     type range of zlt_tramadetalle-consist_atri_3,
    rg_direcc     type range of zlt_tramadetalle-direcci_atri_8,
    rg_posnr      type range of lips-posnr,
    rg_entrega    type range of vbeln_vl,
    lr_posnr      type range of posnr,
    rg_objek      type range of ausp-objek,
    rg_desuere    type range of posnr,
    rg_desuere2   type range of posnr,
    rg_ordenexp4  type range of zlt_estatus-z_ordenexp,

    rwa_fecven2   like line of rg_fecven2,
    rwa_conatr2   like line of rg_conatr2,
    rwa_direcc2   like line of rg_direcc2,
    rwa_tvu       like line of rg_tvu,
    rwa_fecven    like line of rg_fecven,
    rwa_conatr    like line of rg_conatr,
    rwa_direcc    like line of rg_direcc,
    rwa_posnr     like line of rg_posnr,
    rwa_entrega   like line of rg_entrega,
    rwa_pos       like line of lr_posnr,
    rwa_objek     like line of rg_objek,
    rwa_desuere   like line of rg_desuere,
    rwa_desuere2  like line of rg_desuere2,
    rwa_ordenexp4 like line of rg_ordenexp4.


*¨Variables
  data:
    vl_sms_updentrega   type string,
    vl_sms_conentrega   type string,
    vl_flag_updentrega  type string,
    vl_flag_conentrega  type string,
    vl_fec2             type c length 8,
    vl_tvu2             type zlt_trama-vida_util_tvu,
    vl_tvu              type zlt_tramadetalle-vida_util_tvu,
    vl_material         type /scmtms/product_id,
    vl_fec              type c length 8,
    vl_nrotra           type c length 10,
    vl_orden_externa_f  type zlt_tramadetalle-orden_externa,
    w_header_data       type bapiobdlvhdrchg,
    w_header_control    type bapiobdlvhdrctrlchg,
    w_dlvcontrol        type bapidlvcontrol,
    v_deliv_numb        type vbeln_vl,
    v_posnr_vl          type posnr_vl,
    v_corre             type i,
    vl_atwrt            type ausp-atwrt,
    vl_atinn            type ausp-atinn,
    vl_atinn2	          type ausp-atinn,
    vl_canti_acu_pick   type zlt_trama_retor-cantidad_picking,
    vl_peso_acu_pick    type zlt_trama_retor-peso_picking_estandar,
    vl_kwmeng           type vbap-kwmeng,
    vl_sms_e3           type string,
    vl_msgno_e3         type msgno,  "   Número del mensaje de sistema
    vl_msgty_e3         type msgty,  "   Clase de mensaje
    vl_msgid_e3         type msgid,  "   Identificación de los mensajes
    vl_sms_c3           type string,
    vl_msgno_c3         type msgno,  "   Número del mensaje de sistema
    vl_msgty_c3         type msgty,  "   Clase de mensaje
    vl_msgid_c3         type msgid,  "   Identificación de los mensajes
    w_vbkok_wa          type vbkok,
    l_commit            type rvsel-xfeld,
    l_delivery          type likp-vbeln,
    l_update_picking    type rvsel-xfeld,
    t_vbpok_tab         type table of vbpok,
    t_vbpok_tab2        type table of vbpok,
    ltd_return          type table of bapiret2,
    w_vbpok_tab         like line of t_vbpok_tab,
    t_prot              like prott occurs 0 with header line,
    ls_error            type xfeld,
    lwa_lips            like line of lt_lips,
    lv_batch            type lips-charg,
    lv_lgmng            type lips-lgmng,
    lv_lfimg            type lips-lfimg,
    vl_sms_c            type string,
    vl_msgno_c          type msgno,  "   Número del mensaje de sistema
    vl_msgty_c          type msgty,  "   Clase de mensaje
    vl_msgid_c          type msgid,  "   Identificación de los mensajes
    vl_entrega          type vbeln,
    vl_kunag            type likp-kunag,
    wa_ztsd_ultlote     type ztsd_ultlote,
    vl_z_ordenexp       type zlt_estatus-z_ordenexp,
    vl_tor_id           type /scmtms/d_torrot-tor_id,
    lt_zlt_estatus      type standard table of zlt_estatus,
    wa_zlt_estatus      type zlt_estatus,
    ltd_return_eli      type table of bapiret2,
    lwa_h_data_eli      type bapiobdlvhdrchg,
    lwa_h_ctrl_eli      type bapiobdlvhdrctrlchg,
    wa_item_data_eli    like line of lt_item_data_eli,
    wa_item_control_eli like line of lt_item_control_eli,
    vl_orden_ext        type zlt_tramadetalle-orden_externa,
    vl_line_ext         type zlt_tramadetalle-linea_externa,
    vl_flag             type c length 1,
    vl_vbeln_f          type lips-vbeln,
    vl_uepos_f          type lips-uepos,
    vl_matnr            type matnr18,
    vl_matnr_eli        type matnr18.

  field-symbols:
    <fs_t_entpos>            like line of t_entpos,
    <fs_t_item_data>         like line of t_item_data,
    <fs_t_item_control>      like line of t_item_control,
    <fs_t_item_data_spl>     like line of t_item_data_spl,
    <fs_t_itemdata>          like line of t_itemdata,
    <fs_t_cwm_item_data>     like line of t_cwm_item_data,
    <fs_lt_consoli>          like line of lt_consoli,
    <fs_lt_lips_c2>          like line of lt_lips_c2,
    <fs_lt_temp>             like line of lt_temp,
    <fs_lt_mch1_ul>          like line of lt_mch1_ul,
    <fs_lt_likp_ul>          like line of lt_likp_ul,
    <fs_lt_tablecon>         like line of lt_tablecon,
    <fs_lt_zlt_trama>        like line of lt_zlt_trama,
    <fs_lt_zlt_tramadetalle> like line of lt_zlt_tramadetalle,
    <fs_lt_lips_c>           like line of lt_lips_c,
    <fs_lt_tablecon_eli>     like line of lt_tablecon_eli,
    <fs_lt_tablecon_x>       like line of lt_tablecon,
    <fs_lt_turbox>           like line of lt_turbox,
    <fs_lt_lips_aux01>       like line of lt_lips_aux01,
    <fs_lt_zlt_estatus2>     like line of lt_zlt_estatus2,
    <fs_lt_tablecon_nc>      like line of lt_tablecon_nc,
    <fs_lt_tablecon_cs>      like line of lt_tablecon_cs,
    <fs_lt_tablecon_cd>      like line of lt_tablecon_cd.




  free: lt_turbox.
**LOG-POINT ID ZLT_TRAMA_RETOR
  free: lt_zlt_trama_retor.
  loop at lt_confir_exp assigning field-symbol(<fs_01>).
    append initial line to lt_zlt_trama_retor assigning field-symbol(<fs_02>).
    move-corresponding: <fs_01> to <fs_02>.

    append initial line to lt_turbox assigning <fs_lt_turbox>.
    <fs_lt_turbox>-tor_id  = <fs_02>-nro_transporte.
    <fs_lt_turbox>-almacen = <fs_02>-almacen.

  endloop.

  if lines( lt_zlt_trama_retor ) <> 0.
    modify zlt_trama_retor from table lt_zlt_trama_retor.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = abap_true.
  endif.

  if lt_turbox is not initial.
    sort: lt_turbox by tor_id almacen.
    delete adjacent duplicates from lt_turbox comparing all fields.
  endif.

  free: rwa_ordenexp4 , rg_ordenexp4.
  loop at lt_turbox assigning <fs_lt_turbox>.

    free: rwa_ordenexp4 , rg_ordenexp4.
    data(lv_almacen4) = <fs_lt_turbox>-almacen.
    case lv_almacen4.
      when 'FRIO'.
        rwa_ordenexp4-sign   = 'I'.
        rwa_ordenexp4-option = 'EQ'.
        rwa_ordenexp4-low    = 'SP_1001'.
        append rwa_ordenexp4 to rg_ordenexp4.

        rwa_ordenexp4-sign   = 'I'.
        rwa_ordenexp4-option = 'EQ'.
        rwa_ordenexp4-low    = 'SP_1001_01'.
        append rwa_ordenexp4 to rg_ordenexp4.

      when 'SECO'.
        rwa_ordenexp4-sign   = 'I'.
        rwa_ordenexp4-option = 'EQ'.
        rwa_ordenexp4-low    = 'SP_1000'.
        append rwa_ordenexp4 to rg_ordenexp4.

        rwa_ordenexp4-sign   = 'I'.
        rwa_ordenexp4-option = 'EQ'.
        rwa_ordenexp4-low    = 'SP_1000_01'.
        append rwa_ordenexp4 to rg_ordenexp4.
    endcase.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = <fs_lt_turbox>-tor_id
      importing
        output = <fs_lt_turbox>-tor_id.

    free: lt_zlt_estatus2.
    if <fs_lt_turbox>-tor_id is not initial and
       rg_ordenexp4 is not initial.

      select * into table lt_zlt_estatus2
        from zlt_estatus
        where z_orden    eq <fs_lt_turbox>-tor_id
          and z_ordenexp in rg_ordenexp4.
      if sy-subrc = 0.
        loop at lt_zlt_estatus2 into data(wa_lt_zlt_estatus) .
          wa_lt_zlt_estatus-hora_ret = sy-uzeit.
          modify zlt_estatus from wa_lt_zlt_estatus.
          commit work and wait.
        endloop.
      endif.
    endif.

  endloop.

  free: vl_flag.
*{/@0001

  select * into table @data(lt_mandts)
    from zconstantes
    where zmodulo       = 'SD'
      and programm      = 'ZPP_CONFIRMACION_PICKIN_OV_WMS'
      and fieldname     = 'MANDANTES'
      and zsigno        = 'I'
      and zopcion       = 'EQ'.
  if sy-subrc = 0.

    types: y_mandt  type range of sy-mandt.
    data(rg_mandt) = value y_mandt(
        for ls_custx in lt_mandts
        let s = 'I'
            o = 'EQ'
        in sign = s
         option = o
    ( low = ls_custx-valor_low ) ).
  endif.

*  IF sy-mandt = '300' .
  if sy-mandt in rg_mandt .
*}/@0001

    select * into table @data(lt_usuarios)
      from zconstantes
      where zmodulo       = 'SD'
        and programm      = 'ZPP_CONFIRMACION_PICKIN_OV_WMS'
        and fieldname     = 'USUARIOS'
        and zsigno        = 'I'
        and zopcion       = 'EQ'.
    if sy-subrc = 0.

      types: y_uname  type range of sy-uname.
      data(rg_uname) = value y_uname(
          for ls_custx in lt_usuarios
          let s = 'I'
              o = 'EQ'
          in sign = s
           option = o
      ( low = ls_custx-valor_low ) ).

      if sy-uname in rg_uname.
        break everis_func.
        break everis_tech.

        select valor_low into TABLE @data(vl_valor_low)
          from zconstantes
          where zmodulo       = 'SD'
            and programm      = 'ZPP_CONFIRMACION_PICKIN_OV_WMS'
            and fieldname     = 'SAP_ORDEN_SALIDA'
            and zsigno        = 'I'
            and zopcion       = 'EQ'
            and zsecuencia    = '1'.
        if sy-subrc = 0.

          free: lt_zlt_trama_retor.
          select * into table lt_zlt_trama_retor
            from zlt_trama_retor
           where sap_orden_salida IN ( vl_valor_low ) .
          if sy-subrc = 0.

          endif.
        endif.
      endif.
    endif.
  endif.

  free: lt_tablecon,lt_zlt_tramadetalle2.
  loop at lt_zlt_trama_retor assigning field-symbol(<fs_lt_zlt_trama_retor>).
    "Logica para realizar la contabilización en SAP
    free: vl_tvu2.
    if <fs_lt_zlt_trama_retor>-orderbreak <> '&#x20;'.
      vl_tvu2 = <fs_lt_zlt_trama_retor>-orderbreak.
    else.
      vl_tvu2 = 0.
    endif.

    free: rg_fecven2,rwa_fecven2,vl_fec2.
    rwa_fecven2-sign   = 'I'.
    rwa_fecven2-option = 'EQ'.
    if <fs_lt_zlt_trama_retor>-fecha_transaccion is not initial.
      concatenate: '20' <fs_lt_zlt_trama_retor>-fecha_transaccion into vl_fec2.
    else.
      vl_fec2 = <fs_lt_zlt_trama_retor>-fecha_transaccion.
    endif.
    rwa_fecven2-low    = vl_fec2.
    append rwa_fecven2 to rg_fecven2.

    free: rg_conatr2,rwa_conatr2.
    rwa_conatr2-sign   = 'I'.
    rwa_conatr2-option = 'EQ'.
    if <fs_lt_zlt_trama_retor>-atributo03 <> '&#x20;'.
      rwa_conatr2-low    = <fs_lt_zlt_trama_retor>-atributo03.
    else.
      rwa_conatr2-low    = ' '.
    endif.
    append rwa_conatr2 to rg_conatr2.

    free: rg_direcc2,rwa_direcc2.
    rwa_direcc2-sign   = 'I'.
    rwa_direcc2-option = 'EQ'.
    if <fs_lt_zlt_trama_retor>-atributo08 <> '&#x20;'.
      rwa_direcc2-low    = <fs_lt_zlt_trama_retor>-atributo08.
    else.
      rwa_direcc2-low    = ' '.
    endif.
    append rwa_direcc2 to rg_direcc2.

    free: rg_tvu , rwa_tvu.
    rwa_tvu-sign   = 'I'.
    rwa_tvu-option = 'EQ'.
    rwa_tvu-low    = <fs_lt_zlt_trama_retor>-orderbreak.
    append rwa_tvu to rg_tvu.

    if vl_tvu2 = 0.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_tvu2
        importing
          output = vl_tvu2.
    else.
      if vl_tvu2 <> 0.
        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = vl_tvu2
          importing
            output = vl_tvu2.
      endif.
    endif.

    data: vl_sap_linea_orden_e type zlt_trama-linea_externa.
    free: vl_sap_linea_orden_e.

    vl_sap_linea_orden_e = <fs_lt_zlt_trama_retor>-sap_linea_orden.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = vl_sap_linea_orden_e
      importing
        output = vl_sap_linea_orden_e.

    free: lt_zlt_trama.
    select * into table lt_zlt_trama
      from zlt_trama
      where orden_externa  = <fs_lt_zlt_trama_retor>-sap_orden_salida
        and linea_externa  = vl_sap_linea_orden_e
        and fec_ven_atri_2 in rg_fecven2
        and consist_atri_3 in rg_conatr2
        and direcci_atri_8 in rg_direcc2
        and vida_util_tvu  = vl_tvu2. "IN rg_tvu. "= vl_tvu2.
    if sy-subrc = 0.

      free: lt_zlt_tramadetalle.
      data(vl_orden_externa) = conv /scmtms/base_btd_id( <fs_lt_zlt_trama_retor>-sap_orden_salida ).

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_orden_externa
        importing
          output = vl_orden_externa.

      data(vl_receptor) = conv kunnr( <fs_lt_zlt_trama_retor>-receptor ).
      vl_receptor = |{ vl_receptor alpha = in }|.

      free: vl_material.
      vl_material = <fs_lt_zlt_trama_retor>-cod_articulo.

      call function 'CONVERSION_EXIT_MATN1_INPUT'
        exporting
          input        = vl_material
        importing
          output       = vl_material
        exceptions
          length_error = 1
          others       = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.

      read table lt_zlt_trama index 1 assigning field-symbol(<fs_lt_zlt_trama_w>).
      if sy-subrc = 0.

        free: vl_tvu.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = <fs_lt_zlt_trama_w>-vida_util_tvu
          importing
            output = <fs_lt_zlt_trama_w>-vida_util_tvu.

        vl_tvu = <fs_lt_zlt_trama_w>-vida_util_tvu.

        free: rg_fecven,rwa_fecven,vl_fec.
        rwa_fecven-sign   = 'I'.
        rwa_fecven-option = 'EQ'.
        vl_fec = <fs_lt_zlt_trama_w>-fec_ven_atri_2.
        rwa_fecven-low    = vl_fec.
        append rwa_fecven to rg_fecven.

        free: rg_conatr,rwa_conatr.
        rwa_conatr-sign   = 'I'.
        rwa_conatr-option = 'EQ'.
        rwa_conatr-low    = <fs_lt_zlt_trama_w>-consist_atri_3.
        append rwa_conatr to rg_conatr.

        free: rg_direcc,rwa_direcc.
        rwa_direcc-sign   = 'I'.
        rwa_direcc-option = 'EQ'.
        rwa_direcc-low    = <fs_lt_zlt_trama_w>-direcci_atri_8.
        append rwa_direcc to rg_direcc.

        free: vl_nrotra.
        vl_nrotra = conv char10( <fs_lt_zlt_trama_retor>-nro_transporte ).
        "{+@lore04072019
        if vl_tvu = 0.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = vl_tvu
            importing
              output = vl_tvu.
        else.
          if vl_tvu <> 0.
*            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*              EXPORTING
*                input  = vl_tvu
*              IMPORTING
*                output = vl_tvu.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = vl_tvu
              importing
                output = vl_tvu.
          endif.
        endif.
        "}+@lore04072019

        free: lt_zlt_tramadetalle2.
        select * into table lt_zlt_tramadetalle2
         from zlt_tramadetalle
         where numero_trans   = vl_nrotra
           and receptor       = vl_receptor
           and fec_ven_atri_2 in rg_fecven
           and consist_atri_3 in rg_conatr
           and direcci_atri_8 in rg_direcc
           and item           = vl_material
           and vida_util_tvu  = vl_tvu.
        if sy-subrc = 0.

          loop at lt_zlt_tramadetalle2 assigning field-symbol(<fs_lt_zlt_tramadetalle2>).

            append initial line to lt_tablecon assigning <fs_lt_tablecon>.

            <fs_lt_tablecon>-correlativo_d      = <fs_lt_zlt_tramadetalle2>-correlativo      . "Correlativo
            <fs_lt_tablecon>-almacen_sap_d      = <fs_lt_zlt_tramadetalle2>-almacen_sap      . "Almacén
            <fs_lt_tablecon>-orden_externa_d    = <fs_lt_zlt_tramadetalle2>-orden_externa    . "Documento de operación comercial base
            <fs_lt_tablecon>-numero_trans_d     = <fs_lt_zlt_tramadetalle2>-numero_trans     . "Numero de Transporte
            <fs_lt_tablecon>-tipo_de_doc_d      = <fs_lt_zlt_tramadetalle2>-tipo_de_doc      . "Tipo de Documento
            <fs_lt_tablecon>-linea_externa_d    = <fs_lt_zlt_tramadetalle2>-linea_externa    . "Posición del documento base para operación comercial
            <fs_lt_tablecon>-lpn_atributo_7_d   = <fs_lt_zlt_tramadetalle2>-lpn_atributo_7   . "Este campo va Vacio
            <fs_lt_tablecon>-fec_ven_atri_2_d   = <fs_lt_zlt_tramadetalle2>-fec_ven_atri_2   . "Este dato es la fecha de vencimiento
            <fs_lt_tablecon>-consist_atri_3_d   = <fs_lt_zlt_tramadetalle2>-consist_atri_3   . "consistencia
            <fs_lt_tablecon>-lote_grup_atri9_d  = <fs_lt_zlt_tramadetalle2>-lote_grup_atri9  . "Va vacio
            <fs_lt_tablecon>-direcci_atri_8_d   = <fs_lt_zlt_tramadetalle2>-direcci_atri_8   . "Direccionamiento
            <fs_lt_tablecon>-direcci_atri_2_d   = <fs_lt_zlt_tramadetalle2>-direcci_atri_2   . "Va vacio
            <fs_lt_tablecon>-turno_canal_d      = <fs_lt_zlt_tramadetalle2>-turno_canal      . "z_consolidado
            <fs_lt_tablecon>-receptor_d         = <fs_lt_zlt_tramadetalle2>-receptor         . "clientes WMS
            <fs_lt_tablecon>-cliente_d          = <fs_lt_zlt_tramadetalle2>-cliente          . "Destinatario
            <fs_lt_tablecon>-cantidad_orden_d   = <fs_lt_zlt_tramadetalle2>-cantidad_orden   . "Cantidad Orden

            move-corresponding: <fs_lt_zlt_trama_retor> to <fs_lt_tablecon>.

            read table lt_zlt_trama index 1 assigning <fs_lt_zlt_trama>.
            if sy-subrc = 0.
              <fs_lt_tablecon>-orden_externa_t    = <fs_lt_zlt_trama>-orden_externa       . "Documento de operación comercial base
              <fs_lt_tablecon>-z_agrupador_t      = <fs_lt_zlt_trama>-z_agrupador         . "Agrupador
              <fs_lt_tablecon>-linea_externa_t    = <fs_lt_zlt_trama>-linea_externa       . "Posición del documento base para operación comercial
              <fs_lt_tablecon>-receptor_t         = <fs_lt_zlt_trama>-receptor            . "clientes WMS
              <fs_lt_tablecon>-numero_trans_t     = <fs_lt_zlt_trama>-numero_trans        . "Numero de Transporte
              <fs_lt_tablecon>-tipo_de_doc_t      = <fs_lt_zlt_trama>-tipo_de_doc         . "Tipo de Documento
              <fs_lt_tablecon>-almacen_sap_t      = <fs_lt_zlt_trama>-almacen_sap         . "Almacén
              <fs_lt_tablecon>-lpn_atributo_7_t   = <fs_lt_zlt_trama>-lpn_atributo_7      . "Este campo va Vacio
              <fs_lt_tablecon>-fec_ven_atri_2_t   = <fs_lt_zlt_trama>-fec_ven_atri_2      . "Este dato es la fecha de vencimiento
              <fs_lt_tablecon>-consist_atri_3_t   = <fs_lt_zlt_trama>-consist_atri_3      . "consistencia
              <fs_lt_tablecon>-lote_grup_atri9_t  = <fs_lt_zlt_trama>-lote_grup_atri9     . "Va vacio
              <fs_lt_tablecon>-direcci_atri_8_t   = <fs_lt_zlt_trama>-direcci_atri_8      . "Direccionamiento
              <fs_lt_tablecon>-direcci_atri_2_t   = <fs_lt_zlt_trama>-direcci_atri_2      . "Va vacio
              <fs_lt_tablecon>-turno_canal_t      = <fs_lt_zlt_trama>-turno_canal         . "z_consolidado
              <fs_lt_tablecon>-cliente_t          = <fs_lt_zlt_trama>-cliente             . "Destinatario
              <fs_lt_tablecon>-item_t             = <fs_lt_zlt_trama>-item                . "Prod.
              <fs_lt_tablecon>-propietario_t      = <fs_lt_zlt_trama>-propietario         . "Dato por default: “LAIVE”.
              <fs_lt_tablecon>-unidad_de_medida_t = <fs_lt_zlt_trama>-unidad_de_medida    . "Unidad de medida de cantidad
              <fs_lt_tablecon>-vida_util_tvu_t    = <fs_lt_zlt_trama>-vida_util_tvu       . "TVU
              <fs_lt_tablecon>-cantidad_orden_t   = <fs_lt_zlt_trama>-cantidad_orden      . "Cantidad prevista en unidad de medida de venta
              <fs_lt_tablecon>-cantidad_moldes_t  = <fs_lt_zlt_trama>-cantidad_moldes     . "Cantidad prevista en unidad de medida de venta
              <fs_lt_tablecon>-factor_sap_t       = <fs_lt_zlt_trama>-factor_sap          . "Cantidad prevista en unidad de medida de venta
              <fs_lt_tablecon>-udm_sap_t          = <fs_lt_zlt_trama>-udm_sap             . "Unidad de medida base
              <fs_lt_tablecon>-z_etenr_t          = <fs_lt_zlt_trama>-z_etenr             . "Nº de reparto
              <fs_lt_tablecon>-z_edatu_t          = <fs_lt_zlt_trama>-z_edatu             . "Fecha de reparto
            endif.
          endloop.
        endif.
      endif.
    endif.
  endloop.

  free: lt_tablecon_nc  ,  "No catch weight
        lt_tablecon_cs  ,  "catch weight sin desuere
        lt_tablecon_cd  .  "catch weight con desuere

  loop at lt_tablecon assigning <fs_lt_tablecon>.

    data(vl_matnr_f) = conv matnr18( <fs_lt_tablecon>-item_t ).
    vl_matnr_f = |{ vl_matnr_f alpha = in }|.

    free: vl_vbeln_f,
          vl_uepos_f.

    data(vl_aux1) = <fs_lt_tablecon>-orden_externa_d.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = vl_aux1
      importing
        output = vl_aux1.
    data(vl_aux2) = <fs_lt_tablecon>-linea_externa_d.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = vl_aux2
      importing
        output = vl_aux2.

    vl_vbeln_f = vl_aux1.
    vl_vbeln_f = |{ vl_vbeln_f alpha = in }|.
    vl_uepos_f = vl_aux2.
    vl_uepos_f = |{ vl_uepos_f alpha = in }|.


    select single cwqrel
        from mara
        into @data(vl_cwqrel_con_aux)
        where matnr = @vl_matnr_f.
    if sy-subrc = 0.
      case vl_cwqrel_con_aux.
        when 'X'. "CW

          free: lt_lips_au.
          select vbeln
                 posnr
                 uepos
                 uepvw
            into table lt_lips_au
            from lips
            where ( ( vbeln = vl_vbeln_f and
                      uepos = vl_uepos_f )
                  or
                    ( vbeln = vl_vbeln_f and
                      posnr = vl_uepos_f and
                      uepvw = 'C'  ) ).
          if sy-subrc = 0.
            unassign: <fs_lt_tablecon_cd>.
            append initial line to lt_tablecon_cd assigning <fs_lt_tablecon_cd>.
            move-corresponding: <fs_lt_tablecon> to <fs_lt_tablecon_cd>.
          else.
            unassign: <fs_lt_tablecon_cs>.
            append initial line to lt_tablecon_cs assigning <fs_lt_tablecon_cs>.
            move-corresponding: <fs_lt_tablecon> to <fs_lt_tablecon_cs>.
          endif.

        when ''.
          unassign: <fs_lt_tablecon_nc>.
          append initial line to lt_tablecon_nc assigning <fs_lt_tablecon_nc>.
          move-corresponding: <fs_lt_tablecon> to <fs_lt_tablecon_nc>.
      endcase.
    endif.

  endloop.


*{+Inicio de la logica catch weight con desuere

  free: rg_entrega,rwa_entrega,t_entpos.
  unassign: <fs_lt_tablecon_cd>.

  loop at lt_tablecon_cd assigning <fs_lt_tablecon_cd>.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = <fs_lt_tablecon_cd>-orden_externa_d
      importing
        output = <fs_lt_tablecon_cd>-orden_externa_d.

    rwa_entrega-sign   = 'I'.
    rwa_entrega-option = 'EQ'.
    data(vl_ent_cd) = conv vbeln( <fs_lt_tablecon_cd>-orden_externa_d ).
    vl_ent_cd = |{ vl_ent_cd alpha = in }|.
    rwa_entrega-low    = vl_ent_cd.

    append rwa_entrega to rg_entrega.

    append initial line to t_entpos assigning <fs_t_entpos>.
    <fs_t_entpos>-entrega  = <fs_lt_tablecon_cd>-orden_externa_d.
    <fs_t_entpos>-posnr    = <fs_lt_tablecon_cd>-linea_externa_d.
  endloop.

  sort: t_entpos by entrega posnr.
  sort: rg_entrega by low.
  if rg_entrega is not initial.
    delete adjacent duplicates from rg_entrega comparing low.
  endif.

  if rg_entrega is not initial.

    loop at rg_entrega assigning field-symbol(<fs_entrega_f>).

      data(vl_vbeln_cd) = conv vbeln( <fs_entrega_f>-low ).
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_vbeln_cd
        importing
          output = vl_vbeln_cd.

      free: lt_lips_aux01.
      select vbeln posnr uepos uepvw into table lt_lips_aux01
        from lips
        where vbeln = vl_vbeln_cd.
      if sy-subrc = 0.

        delete lt_lips_aux01 where uepvw <> 'C'.

        free: rwa_posnr , rg_posnr.
        loop at lt_lips_aux01 assigning <fs_lt_lips_aux01>.

          free: rwa_posnr .
          rwa_posnr-sign   = 'I'.
          rwa_posnr-option = 'EQ'.
          rwa_posnr-low    = <fs_lt_lips_aux01>-posnr.
          rwa_posnr-high   = <fs_lt_lips_aux01>-uepos.
          append rwa_posnr to rg_posnr.

        endloop.
      endif.

      free: lt_tablecon_cd_a.
      lt_tablecon_cd_a[] = lt_tablecon_cd[].

      if lt_tablecon_cd_a is not initial.
        sort: lt_tablecon_cd_a by orden_externa_d linea_externa_d correlativo_d.
        delete adjacent duplicates from lt_tablecon_cd_a comparing orden_externa_d linea_externa_d correlativo_d.
*        DELETE ADJACENT DUPLICATES FROM lt_tablecon_cd_a COMPARING orden_externa_d linea_externa_d cantidad_orden_d.
      endif.

      field-symbols: <fs_lt_tablecon_cd_a> like line of lt_tablecon_cd_a.
      data: vl_cantotal type zlt_tramadetalle-cantidad_orden  , "Cantidad_orden
            vl_rescan   type p decimals 7.

      unassign: <fs_lt_tablecon_cd>.
      free: vl_rescan.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = <fs_entrega_f>-low
        importing
          output = <fs_entrega_f>-low.

      loop at lt_tablecon_cd assigning <fs_lt_tablecon_cd>
                             where orden_externa_d = <fs_entrega_f>-low.

        loop at rg_posnr assigning field-symbol(<fs_rg_posnr10>)
                            where ( low = <fs_lt_tablecon_cd>-linea_externa_d
                               or   high = <fs_lt_tablecon_cd>-linea_externa_d ).

          free: vl_cantotal.
          unassign: <fs_lt_tablecon_cd_a>.

          data(vl_lin_ext_low)  = conv char10( <fs_rg_posnr10>-low ).
          data(vl_lin_ext_high) = conv char10( <fs_rg_posnr10>-high ).
          vl_lin_ext_low  = |{ vl_lin_ext_low alpha = in }|.
          vl_lin_ext_high = |{ vl_lin_ext_high alpha = in }|.

          unassign: <fs_lt_tablecon_cd_a>.
          loop at lt_tablecon_cd_a assigning <fs_lt_tablecon_cd_a>
                                       where orden_externa_d = <fs_entrega_f>-low
                                         and linea_externa_d = vl_lin_ext_low.
            vl_cantotal = vl_cantotal + <fs_lt_tablecon_cd_a>-cantidad_orden_d.
          endloop.

          unassign: <fs_lt_tablecon_cd_a>.
          loop at lt_tablecon_cd_a assigning <fs_lt_tablecon_cd_a>
                                       where orden_externa_d = <fs_entrega_f>-low
                                         and linea_externa_d = vl_lin_ext_high.
            vl_cantotal = vl_cantotal + <fs_lt_tablecon_cd_a>-cantidad_orden_d.
          endloop.

        endloop.

        <fs_lt_tablecon_cd>-cantidad_orden_d1 = vl_cantotal.
      endloop.
    endloop.

    sort: lt_tablecon_cd by orden_externa_d linea_externa_d cod_picking.

    free: lt_calculos.
    free: lt_tablecon_cd_lo.
    loop at rg_entrega assigning field-symbol(<fs_entrega_f2>).

      data(vl_vbeln_cd2) = conv vbeln( <fs_entrega_f2>-low ).
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_vbeln_cd2
        importing
          output = vl_vbeln_cd2.

      free: lt_lips_c.
      select * into table lt_lips_c
        from lips
        where vbeln = vl_vbeln_cd2.
      if sy-subrc = 0.

        delete lt_lips_c where uepvw <> 'C'.

        free: rwa_posnr , rg_posnr.
        loop at lt_lips_c assigning <fs_lt_lips_c>.

          free: rwa_posnr .
          rwa_posnr-sign   = 'I'.
          rwa_posnr-option = 'EQ'.
          rwa_posnr-low    = <fs_lt_lips_c>-uepos.
          rwa_posnr-high   = <fs_lt_lips_c>-posnr.
          append rwa_posnr to rg_posnr.

        endloop.


        loop at rg_posnr assigning field-symbol(<fs_rg_posnr_f>).

          sort: lt_tablecon_cd by orden_externa_d linea_externa_d cod_picking.
          loop at lt_tablecon_cd assigning <fs_lt_tablecon_cd>
                                 where orden_externa_d = <fs_entrega_f2>-low
                                   and ( linea_externa_d = <fs_rg_posnr_f>-low ).

            <fs_lt_tablecon_cd>-cantidad_orden_d1 = <fs_lt_tablecon_cd>-cantidad_orden_d1 / <fs_lt_tablecon_cd>-peso_sku_x_unid.
            call function 'ROUND'
              exporting
                decimals = 0
                input    = <fs_lt_tablecon_cd>-cantidad_orden_d1
                sign     = 'X'
              importing
                output   = <fs_lt_tablecon_cd>-cantidad_orden_d1.
            if lt_calculos is not initial.
              data(vl_ext_10) = conv char10( <fs_rg_posnr_f>-low ).
              vl_ext_10 = |{ vl_ext_10 alpha = in }|.

              data: vl_can_pi_acum type p decimals 7  . "Cantidad de picking
              free: vl_can_pi_acum.

              loop at lt_calculos assigning field-symbol(<fs_lt_calculos_2>)
                                  where orden_externa_d = <fs_entrega_f2>-low
                                    and linea_externa_d = vl_ext_10.

                vl_can_pi_acum = vl_can_pi_acum + <fs_lt_calculos_2>-cantidad_picking.
              endloop.

              <fs_lt_tablecon_cd>-cantidad_orden_d1 = <fs_lt_tablecon_cd>-cantidad_orden_d1 - vl_can_pi_acum.

              data: vl_can_pic_cd  type p decimals 7,
                    vl_can_pic_cd2 type p decimals 7.
              free: vl_can_pic_cd,
                    vl_can_pic_cd2.
              loop at lt_calculos assigning field-symbol(<fs_lt_calculos_3>)
                                          where cod_picking = <fs_lt_tablecon_cd>-cod_picking.
                vl_can_pic_cd2 = <fs_lt_calculos_3>-cantidad_picking.
                vl_can_pic_cd = vl_can_pic_cd + vl_can_pic_cd2.
              endloop.

              data: vl_can_01 type p decimals 7,
                    vl_can_02 type p decimals 7.

              free: vl_can_01,
                    vl_can_02.

              vl_can_01 = <fs_lt_tablecon_cd>-cantidad_picking.
              vl_can_02 = vl_can_pic_cd.

              <fs_lt_tablecon_cd>-cantidad_picking = vl_can_01 - vl_can_02.
*              <fs_lt_tablecon_cd>-cantidad_picking = <fs_lt_tablecon_cd>-cantidad_picking - vl_can_pic_cd.

              if <fs_lt_tablecon_cd>-cantidad_orden_d1 >= <fs_lt_tablecon_cd>-cantidad_picking.

              else.
                <fs_lt_tablecon_cd>-cantidad_picking = <fs_lt_tablecon_cd>-cantidad_orden_d1.
                condense:<fs_lt_tablecon_cd>-cantidad_picking no-gaps.
              endif.


            else.
              if <fs_lt_tablecon_cd>-cantidad_orden_d1 >= <fs_lt_tablecon_cd>-cantidad_picking.

              else.
                <fs_lt_tablecon_cd>-cantidad_picking = <fs_lt_tablecon_cd>-cantidad_orden_d1.
                condense:<fs_lt_tablecon_cd>-cantidad_picking no-gaps.
              endif.
            endif.

            append initial line to lt_calculos assigning field-symbol(<fs_lt_calculos>).
            move-corresponding: <fs_lt_tablecon_cd> to <fs_lt_calculos>.
*           orden_externa_d   TYPE zlt_tramadetalle-orden_externa    , "Documento de operación comercial base
*           linea_externa_d   TYPE zlt_tramadetalle-linea_externa    , "Posición del documento base para operación comercial
*           cantidad_orden_d1 TYPE zlt_tramadetalle-cantidad_orden   , "Cantidad_orden
*           cod_picking       TYPE zlt_trama_retor-cod_picking       , "Codigo de picking
*           cantidad_picking  TYPE zlt_trama_retor-cantidad_picking  , "Cantidad de picking
          endloop.

          unassign: <fs_lt_tablecon_cd>.
          loop at lt_tablecon_cd assigning <fs_lt_tablecon_cd>
                                 where linea_externa_d = <fs_rg_posnr_f>-high
                                   and orden_externa_d = <fs_entrega_f2>-low.

            data(vl_line_ext_ff) = conv char10( <fs_rg_posnr_f>-low ).
            vl_line_ext_ff = |{ vl_line_ext_ff alpha = in }|.

            read table lt_tablecon_cd with key cod_picking     = <fs_lt_tablecon_cd>-cod_picking
                                               linea_externa_d = vl_line_ext_ff
                                               orden_externa_d = <fs_entrega_f2>-low
                                      assigning field-symbol(<fs_lt_tablecon_cd_2>).
            if sy-subrc = 0.
*              <fs_lt_tablecon_cd>-cantidad_picking = <fs_lt_tablecon_cd_2>-cantidad_picking.
              <fs_lt_tablecon_cd>-eliminar = abap_true.
            endif.
          endloop.

          "nuevo
          unassign: <fs_lt_tablecon_cd>.
          loop at lt_tablecon_cd assigning <fs_lt_tablecon_cd>
                                 where linea_externa_d = <fs_rg_posnr_f>-low
                                   and orden_externa_d = <fs_entrega_f2>-low.

            append initial line to lt_tablecon_cd_lo assigning field-symbol(<fs_lt_tablecon_cd_lo>).
            move-corresponding:<fs_lt_tablecon_cd> to <fs_lt_tablecon_cd_lo>.

            <fs_lt_tablecon_cd_lo>-linea_externa_d = <fs_rg_posnr_f>-high.

            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = <fs_lt_tablecon_cd_lo>-linea_externa_d
              importing
                output = <fs_lt_tablecon_cd_lo>-linea_externa_d.

*            DATA(vl_line_ext_ff) = CONV char10( <fs_rg_posnr_f>-low ).
*            vl_line_ext_ff = |{ vl_line_ext_ff ALPHA = IN }|.
*
*            READ TABLE lt_tablecon_cd WITH KEY cod_picking     = <fs_lt_tablecon_cd>-cod_picking
*                                               linea_externa_d = vl_line_ext_ff
*                                               orden_externa_d = <fs_entrega_f2>-low
*                                      ASSIGNING FIELD-SYMBOL(<fs_lt_tablecon_cd_2>).
*            IF sy-subrc = 0.
**              <fs_lt_tablecon_cd>-cantidad_picking = <fs_lt_tablecon_cd_2>-cantidad_picking.
*              <fs_lt_tablecon_cd>-eliminar = abap_true.
*            ENDIF.
          endloop.

        endloop.
      endif.
    endloop.

    if lt_tablecon_cd_lo is not initial.
      append lines of lt_tablecon_cd_lo[] to lt_tablecon_cd.
    endif.

    unassign: <fs_lt_tablecon_cd>.
    loop at lt_tablecon_cd assigning <fs_lt_tablecon_cd>.
      condense:<fs_lt_tablecon_cd>-cantidad_picking no-gaps.
    endloop.

  endif.

  loop at lt_tablecon_cd assigning <fs_lt_tablecon_cd>.
    if <fs_lt_tablecon_cd>-cantidad_picking <> 0.
      data:
        vl_fac_01 type p decimals 7,
        vl_fac_02 type p decimals 7,
        vl_fac_03 type p decimals 7.

      free: vl_fac_01 ,
            vl_fac_02 ,
            vl_fac_03 .

      vl_fac_01 = <fs_lt_tablecon_cd>-peso_doble_uni.       "+@0001
*      vl_fac_01 = <fs_lt_tablecon_cd>-peso_picking_estandar.        "-@0001
      vl_fac_02 = <fs_lt_tablecon_cd>-cantidad_picking.
      vl_fac_03 = <fs_lt_tablecon_cd>-cantidad_trans_expedida.

      vl_fac_01 = vl_fac_02 * ( vl_fac_01 / vl_fac_03 ).
*{+@0001
      <fs_lt_tablecon_cd>-peso_doble_uni = abs( vl_fac_01 ).
      condense: <fs_lt_tablecon_cd>-peso_doble_uni no-gaps.
*}+@0001
*{-@0001
**      <fs_lt_tablecon_cd>-peso_picking_estandar = <fs_lt_tablecon_cd>-cantidad_picking * ( <fs_lt_tablecon_cd>-peso_picking_estandar / <fs_lt_tablecon_cd>-cantidad_trans_expedida ).
*      <fs_lt_tablecon_cd>-peso_picking_estandar = abs( vl_fac_01 ).
*      CONDENSE: <fs_lt_tablecon_cd>-peso_picking_estandar NO-GAPS.
*}-@0001
    else.
      data: vl_can_pi_r2 type p decimals 7.
      data: vl_can_pi_r3 type p decimals 7.
      free: vl_can_pi_r2.
      free: vl_can_pi_r3.

      loop at lt_tablecon_cd assigning field-symbol(<fs_lt_tablecon_cd_r2>)
                                   where orden_externa_d = <fs_lt_tablecon_cd>-orden_externa_d
                                     and linea_externa_d = <fs_lt_tablecon_cd>-linea_externa_d.

        vl_can_pi_r2 = <fs_lt_tablecon_cd_r2>-cantidad_picking.

        vl_can_pi_r3 = vl_can_pi_r3 + vl_can_pi_r2 .

      endloop.

      if vl_can_pi_r3 > 0.
        <fs_lt_tablecon_cd>-eliminar = abap_true.
      else.
        <fs_lt_tablecon_cd>-cod_picking = '&#x20;'.
      endif.

    endif.

  endloop.

  if lt_tablecon_cd is not initial.
    delete lt_tablecon_cd where eliminar = abap_true.
  endif.
*}+fin de la logica catch weight con desuere

*{+Inicio de la logica catch weight sin desuere
  free: rg_entrega,rwa_entrega,t_entpos.
  unassign: <fs_lt_tablecon_cs>.

  loop at lt_tablecon_cs assigning <fs_lt_tablecon_cs>.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = <fs_lt_tablecon_cs>-orden_externa_d
      importing
        output = <fs_lt_tablecon_cs>-orden_externa_d.

    rwa_entrega-sign   = 'I'.
    rwa_entrega-option = 'EQ'.
    data(vl_ent_cs) = conv vbeln( <fs_lt_tablecon_cs>-orden_externa_d ).
    vl_ent_cs = |{ vl_ent_cs alpha = in }|.
    rwa_entrega-low    = vl_ent_cs.

    append rwa_entrega to rg_entrega.

    append initial line to t_entpos assigning <fs_t_entpos>.
    <fs_t_entpos>-entrega  = <fs_lt_tablecon_cs>-orden_externa_d.
    <fs_t_entpos>-posnr    = <fs_lt_tablecon_cs>-linea_externa_d.
  endloop.

  sort: t_entpos by entrega posnr.
  sort: rg_entrega by low.
  if rg_entrega is not initial.
    delete adjacent duplicates from rg_entrega comparing low.
  endif.

  if rg_entrega is not initial.

    loop at rg_entrega assigning field-symbol(<fs_entrega_f_cs>).

      data(vl_vbeln_cs) = conv vbeln( <fs_entrega_f_cs>-low ).
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_vbeln_cs
        importing
          output = vl_vbeln_cs.

      free: lt_lips_c.
      select * into table lt_lips_c
        from lips
        where vbeln = vl_vbeln_cs.
      if sy-subrc = 0.

        free: rwa_posnr , rg_posnr.
        unassign: <fs_lt_lips_c>.
        loop at lt_lips_c assigning <fs_lt_lips_c>.

          free: rwa_posnr .
          rwa_posnr-sign   = 'I'.
          rwa_posnr-option = 'EQ'.
          rwa_posnr-low    = <fs_lt_lips_c>-posnr.
          rwa_posnr-high   = <fs_lt_lips_c>-uepos.
          append rwa_posnr to rg_posnr.

        endloop.
      endif.

      free: lt_tablecon_cs_a.
      lt_tablecon_cs_a[] = lt_tablecon_cs[].

      if lt_tablecon_cs_a is not initial.
        sort: lt_tablecon_cs_a by orden_externa_d linea_externa_d correlativo_d.
        delete adjacent duplicates from lt_tablecon_cs_a comparing orden_externa_d linea_externa_d correlativo_d.
*        DELETE ADJACENT DUPLICATES FROM lt_tablecon_cs_a COMPARING orden_externa_d linea_externa_d cantidad_orden_d.
      endif.

      field-symbols: <fs_lt_tablecon_cs_a> like line of lt_tablecon_cs_a.
      data: vl_cantotal_cs type zlt_tramadetalle-cantidad_orden  , "Cantidad_orden
            vl_rescan_cs   type p decimals 7.

      unassign: <fs_lt_tablecon_cs>.
      free: vl_rescan_cs.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = <fs_entrega_f_cs>-low
        importing
          output = <fs_entrega_f_cs>-low.

      unassign: <fs_lt_tablecon_cs>.
      loop at lt_tablecon_cs assigning <fs_lt_tablecon_cs>
                             where orden_externa_d = <fs_entrega_f_cs>-low.

        loop at rg_posnr assigning field-symbol(<fs_rg_posnr10_cs>)
                            where ( low = <fs_lt_tablecon_cs>-linea_externa_d
                               or   high = <fs_lt_tablecon_cs>-linea_externa_d ).

          free: vl_cantotal_cs.
          unassign: <fs_lt_tablecon_cs_a>.

          data(vl_lin_ext_low_cs)  = conv char10( <fs_rg_posnr10_cs>-low ).
          data(vl_lin_ext_high_cs) = conv char10( <fs_rg_posnr10_cs>-high ).
          vl_lin_ext_low_cs  = |{ vl_lin_ext_low_cs alpha = in }|.
          vl_lin_ext_high_cs = |{ vl_lin_ext_high_cs alpha = in }|.

          unassign: <fs_lt_tablecon_cs_a>.
          loop at lt_tablecon_cs_a assigning <fs_lt_tablecon_cs_a>
                                       where orden_externa_d = <fs_entrega_f_cs>-low
                                         and linea_externa_d = vl_lin_ext_low_cs.
            vl_cantotal_cs = vl_cantotal_cs + <fs_lt_tablecon_cs_a>-cantidad_orden_d.
          endloop.

          unassign: <fs_lt_tablecon_cs_a>.
          loop at lt_tablecon_cs_a assigning <fs_lt_tablecon_cs_a>
                                       where orden_externa_d = <fs_entrega_f_cs>-low
                                         and linea_externa_d = vl_lin_ext_high_cs.
            vl_cantotal_cs = vl_cantotal_cs + <fs_lt_tablecon_cs_a>-cantidad_orden_d.
          endloop.

        endloop.

        <fs_lt_tablecon_cs>-cantidad_orden_d1 = vl_cantotal_cs.
      endloop.
    endloop.

    sort: lt_tablecon_cs by orden_externa_d linea_externa_d cod_picking.

    free: lt_calculos.
    loop at rg_entrega assigning field-symbol(<fs_entrega_f2_cs>).

      data(vl_vbeln_cd2_cs) = conv vbeln( <fs_entrega_f2_cs>-low ).
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_vbeln_cd2_cs
        importing
          output = vl_vbeln_cd2_cs.

      free: lt_lips_c.
      select * into table lt_lips_c
        from lips
        where vbeln = vl_vbeln_cd2_cs.
      if sy-subrc = 0.

        free: rwa_posnr , rg_posnr.
        unassign:<fs_lt_lips_c>.
        loop at lt_lips_c assigning <fs_lt_lips_c>.

          free: rwa_posnr .
          rwa_posnr-sign   = 'I'.
          rwa_posnr-option = 'EQ'.
          rwa_posnr-low    = <fs_lt_lips_c>-uepos.
          rwa_posnr-high   = <fs_lt_lips_c>-posnr.
          append rwa_posnr to rg_posnr.

        endloop.


        loop at rg_posnr assigning field-symbol(<fs_rg_posnr_f_cs>).

          sort: lt_tablecon_cs by orden_externa_d linea_externa_d cod_picking.
          loop at lt_tablecon_cs assigning <fs_lt_tablecon_cs>
                                 where orden_externa_d = <fs_entrega_f2_cs>-low
                                   and ( linea_externa_d = <fs_rg_posnr_f_cs>-high ).

            <fs_lt_tablecon_cs>-cantidad_orden_d1 = <fs_lt_tablecon_cs>-cantidad_orden_d1 / <fs_lt_tablecon_cs>-peso_sku_x_unid.
            call function 'ROUND'
              exporting
                decimals = 0
                input    = <fs_lt_tablecon_cs>-cantidad_orden_d1
                sign     = 'X'
              importing
                output   = <fs_lt_tablecon_cs>-cantidad_orden_d1.
            if lt_calculos is not initial.
              data(vl_ext_10_cs) = conv char10( <fs_rg_posnr_f_cs>-high ).
              vl_ext_10_cs = |{ vl_ext_10_cs alpha = in }|.

              data: vl_can_pi_acum_cs type p decimals 7  . "Cantidad de picking
              free: vl_can_pi_acum_cs.

              loop at lt_calculos assigning field-symbol(<fs_lt_calculos_2_cs>)
                                  where orden_externa_d = <fs_entrega_f2_cs>-low
                                    and linea_externa_d = vl_ext_10_cs.

                vl_can_pi_acum_cs = vl_can_pi_acum_cs + <fs_lt_calculos_2_cs>-cantidad_picking.
              endloop.

              <fs_lt_tablecon_cs>-cantidad_orden_d1 = <fs_lt_tablecon_cs>-cantidad_orden_d1 - vl_can_pi_acum_cs.

              data: vl_can_pic_cs  type p decimals 7,
                    vl_can_pic_cs2 type p decimals 7.
              free: vl_can_pic_cs,
                    vl_can_pic_cs2.
              loop at lt_calculos assigning field-symbol(<fs_lt_calculos_3_cs>)
                                          where cod_picking = <fs_lt_tablecon_cs>-cod_picking.
                vl_can_pic_cs2 = <fs_lt_calculos_3_cs>-cantidad_picking.
                vl_can_pic_cs = vl_can_pic_cs + vl_can_pic_cs2.
              endloop.

              data: vl_can_01_cs type p decimals 7,
                    vl_can_02_cs type p decimals 7.

              free: vl_can_01_cs,
                    vl_can_02_cs.

              vl_can_01_cs = <fs_lt_tablecon_cs>-cantidad_picking.
              vl_can_02_cs = vl_can_pic_cs.

              <fs_lt_tablecon_cs>-cantidad_picking = vl_can_01_cs - vl_can_02_cs.

              if <fs_lt_tablecon_cs>-cantidad_orden_d1 >= <fs_lt_tablecon_cs>-cantidad_picking.

              else.
                <fs_lt_tablecon_cs>-cantidad_picking = <fs_lt_tablecon_cs>-cantidad_orden_d1.
                condense:<fs_lt_tablecon_cs>-cantidad_picking no-gaps.
              endif.


            else.
              if <fs_lt_tablecon_cs>-cantidad_orden_d1 >= <fs_lt_tablecon_cs>-cantidad_picking.

              else.
                <fs_lt_tablecon_cs>-cantidad_picking = <fs_lt_tablecon_cs>-cantidad_orden_d1.
                condense:<fs_lt_tablecon_cs>-cantidad_picking no-gaps.
              endif.
            endif.

            append initial line to lt_calculos assigning field-symbol(<fs_lt_calculos_cs>).
            move-corresponding: <fs_lt_tablecon_cs> to <fs_lt_calculos_cs>.
*           orden_externa_d   TYPE zlt_tramadetalle-orden_externa    , "Documento de operación comercial base
*           linea_externa_d   TYPE zlt_tramadetalle-linea_externa    , "Posición del documento base para operación comercial
*           cantidad_orden_d1 TYPE zlt_tramadetalle-cantidad_orden   , "Cantidad_orden
*           cod_picking       TYPE zlt_trama_retor-cod_picking       , "Codigo de picking
*           cantidad_picking  TYPE zlt_trama_retor-cantidad_picking  , "Cantidad de picking
          endloop.

          unassign: <fs_lt_tablecon_cs>.
          loop at lt_tablecon_cs assigning <fs_lt_tablecon_cs>
                                 where linea_externa_d = <fs_rg_posnr_f_cs>-high
                                   and orden_externa_d = <fs_entrega_f2_cs>-low.

            data(vl_line_ext_ff_cs) = conv char10( <fs_rg_posnr_f_cs>-low ).
            vl_line_ext_ff_cs = |{ vl_line_ext_ff_cs alpha = in }|.

            read table lt_tablecon_cs with key cod_picking     = <fs_lt_tablecon_cs>-cod_picking
                                               linea_externa_d = vl_line_ext_ff_cs
                                               orden_externa_d = <fs_entrega_f2_cs>-low
                                      assigning field-symbol(<fs_lt_tablecon_cd_2_cs>).
            if sy-subrc = 0.
              <fs_lt_tablecon_cs>-cantidad_picking = <fs_lt_tablecon_cd_2_cs>-cantidad_picking.
            endif.
          endloop.

        endloop.
      endif.
    endloop.

    unassign: <fs_lt_tablecon_cs>.
    loop at lt_tablecon_cs assigning <fs_lt_tablecon_cs>.
      condense:<fs_lt_tablecon_cs>-cantidad_picking no-gaps.
    endloop.

  endif.

  loop at lt_tablecon_cs assigning <fs_lt_tablecon_cs>.
    if <fs_lt_tablecon_cs>-cantidad_picking <> 0.
      data:
        vl_fac_01_cs type p decimals 7,
        vl_fac_02_cs type p decimals 7,
        vl_fac_03_cs type p decimals 7.

      free: vl_fac_01_cs ,
            vl_fac_02_cs ,
            vl_fac_03_cs .

*      vl_fac_01_cs = <fs_lt_tablecon_cs>-peso_picking_estandar.             "-@0001
      vl_fac_01_cs = <fs_lt_tablecon_cs>-peso_doble_uni.    "+@0001
      vl_fac_02_cs = <fs_lt_tablecon_cs>-cantidad_picking.
      vl_fac_03_cs = <fs_lt_tablecon_cs>-cantidad_trans_expedida.

      vl_fac_01_cs = vl_fac_02_cs * ( vl_fac_01_cs / vl_fac_03_cs ).
*{+@0001
      <fs_lt_tablecon_cs>-peso_doble_uni = abs( vl_fac_01_cs ).
      condense: <fs_lt_tablecon_cs>-peso_doble_uni no-gaps.
*}+@0001
*{-@0001
*      <fs_lt_tablecon_cs>-peso_picking_estandar = abs( vl_fac_01_cs ).
*      CONDENSE: <fs_lt_tablecon_cs>-peso_picking_estandar NO-GAPS.
*}-@0001
    else.
      data: vl_can_pi_r2_cs type p decimals 7.
      data: vl_can_pi_r3_cs type p decimals 7.
      free: vl_can_pi_r2_cs.
      free: vl_can_pi_r3_cs.

      loop at lt_tablecon_cs assigning field-symbol(<fs_lt_tablecon_cs_r2>)
                                   where orden_externa_d = <fs_lt_tablecon_cs>-orden_externa_d
                                     and linea_externa_d = <fs_lt_tablecon_cs>-linea_externa_d.

        vl_can_pi_r2_cs = <fs_lt_tablecon_cs_r2>-cantidad_picking.

        vl_can_pi_r3_cs = vl_can_pi_r3_cs + vl_can_pi_r2_cs .

      endloop.

      if vl_can_pi_r3_cs > 0.
        <fs_lt_tablecon_cs>-eliminar    = abap_true.
      else.
        <fs_lt_tablecon_cs>-cod_picking = '&#x20;'.
      endif.

    endif.

  endloop.

  if lt_tablecon_cs is not initial.
    delete lt_tablecon_cs where eliminar = abap_true.
  endif.

*}+fin de la logica catch weight sin desuere

*{+Inicio de la logica No catch weight

  free: rg_entrega,rwa_entrega,t_entpos.
  unassign: <fs_lt_tablecon_nc>.


  loop at lt_tablecon_nc assigning <fs_lt_tablecon_nc>.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = <fs_lt_tablecon_nc>-orden_externa_d
      importing
        output = <fs_lt_tablecon_nc>-orden_externa_d.

    rwa_entrega-sign   = 'I'.
    rwa_entrega-option = 'EQ'.
    data(vl_ent_nc) = conv vbeln( <fs_lt_tablecon_nc>-orden_externa_d ).
    vl_ent_nc = |{ vl_ent_nc alpha = in }|.
    rwa_entrega-low    = vl_ent_nc.

    append rwa_entrega to rg_entrega.

    append initial line to t_entpos assigning <fs_t_entpos>.
    <fs_t_entpos>-entrega  = <fs_lt_tablecon_nc>-orden_externa_d.
    <fs_t_entpos>-posnr    = <fs_lt_tablecon_nc>-linea_externa_d.
  endloop.

  sort: t_entpos by entrega posnr.
  sort: rg_entrega by low.
  if rg_entrega is not initial.
    delete adjacent duplicates from rg_entrega comparing low.
  endif.

  if rg_entrega is not initial.

    loop at rg_entrega assigning field-symbol(<fs_entrega_f_nc>).

      data(vl_vbeln_nc) = conv vbeln( <fs_entrega_f_nc>-low ).
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_vbeln_nc
        importing
          output = vl_vbeln_nc.

      free: lt_lips_c.
      select * into table lt_lips_c
        from lips
        where vbeln = vl_vbeln_nc.
      if sy-subrc = 0.

        free: rwa_posnr , rg_posnr.
        unassign: <fs_lt_lips_c>.
        loop at lt_lips_c assigning <fs_lt_lips_c>.

          free: rwa_posnr .
          rwa_posnr-sign   = 'I'.
          rwa_posnr-option = 'EQ'.
          rwa_posnr-low    = <fs_lt_lips_c>-posnr.
          rwa_posnr-high   = <fs_lt_lips_c>-uepos.
          append rwa_posnr to rg_posnr.

        endloop.
      endif.

      free: lt_tablecon_nc_a.
      lt_tablecon_nc_a[] = lt_tablecon_nc[].

      if lt_tablecon_nc_a is not initial.
        sort: lt_tablecon_nc_a by orden_externa_d linea_externa_d correlativo_d.
        delete adjacent duplicates from lt_tablecon_nc_a comparing orden_externa_d linea_externa_d correlativo_d.
*        DELETE ADJACENT DUPLICATES FROM lt_tablecon_nc_a COMPARING orden_externa_d linea_externa_d cantidad_orden_d.
      endif.

      field-symbols: <fs_lt_tablecon_nc_a> like line of lt_tablecon_nc_a.
      data: vl_cantotal_nc type zlt_tramadetalle-cantidad_orden  , "Cantidad_orden
            vl_rescan_nc   type p decimals 7.

      unassign: <fs_lt_tablecon_nc>.
      free: vl_rescan_nc.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = <fs_entrega_f_nc>-low
        importing
          output = <fs_entrega_f_nc>-low.

      unassign: <fs_lt_tablecon_nc>.
      loop at lt_tablecon_nc assigning <fs_lt_tablecon_nc>
                             where orden_externa_d = <fs_entrega_f_nc>-low.

        loop at rg_posnr assigning field-symbol(<fs_rg_posnr10_nc>)
                            where ( low = <fs_lt_tablecon_nc>-linea_externa_d
                               or   high = <fs_lt_tablecon_nc>-linea_externa_d ).

          free: vl_cantotal_nc.

          data(vl_lin_ext_low_nc)  = conv char10( <fs_rg_posnr10_nc>-low ).
          data(vl_lin_ext_high_nc) = conv char10( <fs_rg_posnr10_nc>-high ).
          vl_lin_ext_low_nc  = |{ vl_lin_ext_low_nc alpha = in }|.
          vl_lin_ext_high_nc = |{ vl_lin_ext_high_nc alpha = in }|.

          unassign: <fs_lt_tablecon_nc_a>.
          loop at lt_tablecon_nc_a assigning <fs_lt_tablecon_nc_a>
                                       where orden_externa_d = <fs_entrega_f_nc>-low
                                         and linea_externa_d = vl_lin_ext_low_nc.
            vl_cantotal_nc = vl_cantotal_nc + <fs_lt_tablecon_nc_a>-cantidad_orden_d.
          endloop.

          unassign: <fs_lt_tablecon_nc_a>.
          loop at lt_tablecon_nc_a assigning <fs_lt_tablecon_nc_a>
                                       where orden_externa_d = <fs_entrega_f_nc>-low
                                         and linea_externa_d = vl_lin_ext_high_nc.
            vl_cantotal_nc = vl_cantotal_nc + <fs_lt_tablecon_nc_a>-cantidad_orden_d.
          endloop.


        endloop.

        <fs_lt_tablecon_nc>-cantidad_orden_d1 = vl_cantotal_nc.
      endloop.
    endloop.

    sort: lt_tablecon_nc by orden_externa_d linea_externa_d cod_picking.

    free: lt_calculos.
    loop at rg_entrega assigning field-symbol(<fs_entrega_f2_nc>).

      data(vl_vbeln_cd2_nc) = conv vbeln( <fs_entrega_f2_nc>-low ).
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_vbeln_cd2_nc
        importing
          output = vl_vbeln_cd2_nc.

      free: lt_lips_c.
      select * into table lt_lips_c
        from lips
        where vbeln = vl_vbeln_cd2_nc.
      if sy-subrc = 0.


        free: rwa_posnr , rg_posnr.
        unassign:<fs_lt_lips_c>.
        loop at lt_lips_c assigning <fs_lt_lips_c>.

          free: rwa_posnr .
          rwa_posnr-sign   = 'I'.
          rwa_posnr-option = 'EQ'.
          rwa_posnr-low    = <fs_lt_lips_c>-uepos.
          rwa_posnr-high   = <fs_lt_lips_c>-posnr.
          append rwa_posnr to rg_posnr.

        endloop.


        loop at rg_posnr assigning field-symbol(<fs_rg_posnr_f_nc>).

          sort: lt_tablecon_nc by orden_externa_d linea_externa_d cod_picking.
          loop at lt_tablecon_nc assigning <fs_lt_tablecon_nc>
                                 where orden_externa_d = <fs_entrega_f2_nc>-low
                                   and ( linea_externa_d = <fs_rg_posnr_f_nc>-high ).

            <fs_lt_tablecon_nc>-cantidad_orden_d1 = <fs_lt_tablecon_nc>-cantidad_orden_d1 / 1.
            call function 'ROUND'
              exporting
                decimals = 0
                input    = <fs_lt_tablecon_nc>-cantidad_orden_d1
                sign     = 'X'
              importing
                output   = <fs_lt_tablecon_nc>-cantidad_orden_d1.
            if lt_calculos is not initial.
              data(vl_ext_10_nc) = conv char10( <fs_rg_posnr_f_nc>-high ).
              vl_ext_10_nc = |{ vl_ext_10_nc alpha = in }|.

              data: vl_can_pi_acum_nc type p decimals 7  . "Cantidad de picking
              free: vl_can_pi_acum_nc.

              loop at lt_calculos assigning field-symbol(<fs_lt_calculos_2_nc>)
                                  where orden_externa_d = <fs_entrega_f2_nc>-low
                                    and linea_externa_d = vl_ext_10_nc.

                vl_can_pi_acum_nc = vl_can_pi_acum_nc + <fs_lt_calculos_2_nc>-cantidad_picking.
              endloop.

              <fs_lt_tablecon_nc>-cantidad_orden_d1 = <fs_lt_tablecon_nc>-cantidad_orden_d1 - vl_can_pi_acum_nc.

              data: vl_can_pic_nc  type p decimals 7,
                    vl_can_pic_nc2 type p decimals 7.
              free: vl_can_pic_nc,
                    vl_can_pic_nc2.

              loop at lt_calculos assigning field-symbol(<fs_lt_calculos_3_nc>)
                                          where cod_picking = <fs_lt_tablecon_nc>-cod_picking.
                vl_can_pic_nc2 = <fs_lt_calculos_3_nc>-cantidad_picking.
                vl_can_pic_nc = vl_can_pic_nc + vl_can_pic_nc2.
              endloop.

              data: vl_can_01_nc type p decimals 7,
                    vl_can_02_nc type p decimals 7.

              free: vl_can_01_nc,
                    vl_can_02_nc.

              vl_can_01_nc = <fs_lt_tablecon_nc>-cantidad_picking.
              vl_can_02_nc = vl_can_pic_nc.

              <fs_lt_tablecon_nc>-cantidad_picking = vl_can_01_nc - vl_can_02_nc.

              if <fs_lt_tablecon_nc>-cantidad_orden_d1 >= <fs_lt_tablecon_nc>-cantidad_picking.

              else.
                <fs_lt_tablecon_nc>-cantidad_picking = <fs_lt_tablecon_nc>-cantidad_orden_d1.
                condense:<fs_lt_tablecon_nc>-cantidad_picking no-gaps.
              endif.


            else.
              if <fs_lt_tablecon_nc>-cantidad_orden_d1 >= <fs_lt_tablecon_nc>-cantidad_picking.

              else.
                <fs_lt_tablecon_nc>-cantidad_picking = <fs_lt_tablecon_nc>-cantidad_orden_d1.
                condense:<fs_lt_tablecon_nc>-cantidad_picking no-gaps.
              endif.
            endif.

            append initial line to lt_calculos assigning field-symbol(<fs_lt_calculos_nc>).
            move-corresponding: <fs_lt_tablecon_nc> to <fs_lt_calculos_nc>.
*           orden_externa_d   TYPE zlt_tramadetalle-orden_externa    , "Documento de operación comercial base
*           linea_externa_d   TYPE zlt_tramadetalle-linea_externa    , "Posición del documento base para operación comercial
*           cantidad_orden_d1 TYPE zlt_tramadetalle-cantidad_orden   , "Cantidad_orden
*           cod_picking       TYPE zlt_trama_retor-cod_picking       , "Codigo de picking
*           cantidad_picking  TYPE zlt_trama_retor-cantidad_picking  , "Cantidad de picking
          endloop.

          unassign: <fs_lt_tablecon_nc>.
          loop at lt_tablecon_nc assigning <fs_lt_tablecon_nc>
                                 where linea_externa_d = <fs_rg_posnr_f_nc>-high
                                   and orden_externa_d = <fs_entrega_f2_nc>-low.

            data(vl_line_ext_ff_nc) = conv char10( <fs_rg_posnr_f_nc>-low ).
            vl_line_ext_ff_nc = |{ vl_line_ext_ff_nc alpha = in }|.

            read table lt_tablecon_nc with key cod_picking     = <fs_lt_tablecon_nc>-cod_picking
                                               linea_externa_d = vl_line_ext_ff_nc
                                               orden_externa_d = <fs_entrega_f2_nc>-low
                                      assigning field-symbol(<fs_lt_tablecon_cd_2_nc>).
            if sy-subrc = 0.
              <fs_lt_tablecon_nc>-cantidad_picking = <fs_lt_tablecon_cd_2_nc>-cantidad_picking.
            endif.
          endloop.

        endloop.
      endif.
    endloop.

    unassign: <fs_lt_tablecon_nc>.
    loop at lt_tablecon_nc assigning <fs_lt_tablecon_nc>.
      condense:<fs_lt_tablecon_nc>-cantidad_picking no-gaps.
    endloop.

  endif.

  loop at lt_tablecon_nc assigning <fs_lt_tablecon_nc>.
    if <fs_lt_tablecon_nc>-cantidad_picking <> 0.
      data:
        vl_fac_01_nc type p decimals 7,
        vl_fac_02_nc type p decimals 7,
        vl_fac_03_nc type p decimals 7.

      free: vl_fac_01_nc ,
            vl_fac_02_nc ,
            vl_fac_03_nc .

      vl_fac_01_nc = <fs_lt_tablecon_nc>-peso_picking_estandar.
      vl_fac_02_nc = <fs_lt_tablecon_nc>-cantidad_picking.
      vl_fac_03_nc = <fs_lt_tablecon_nc>-cantidad_trans_expedida.

      vl_fac_01_nc = vl_fac_02_nc * ( vl_fac_01_nc / vl_fac_03_nc ).
*      <fs_lt_tablecon_cd>-peso_picking_estandar = <fs_lt_tablecon_cd>-cantidad_picking * ( <fs_lt_tablecon_cd>-peso_picking_estandar / <fs_lt_tablecon_cd>-cantidad_trans_expedida ).
      <fs_lt_tablecon_nc>-peso_picking_estandar = abs( vl_fac_01_nc ).
      condense: <fs_lt_tablecon_nc>-peso_picking_estandar no-gaps.

    else.
      data: vl_can_pi_r2_nc type p decimals 7.
      data: vl_can_pi_r3_nc type p decimals 7.
      free: vl_can_pi_r2_nc.
      free: vl_can_pi_r3_nc.

      loop at lt_tablecon_nc assigning field-symbol(<fs_lt_tablecon_nc_r2>)
                                   where orden_externa_d = <fs_lt_tablecon_nc>-orden_externa_d
                                     and linea_externa_d = <fs_lt_tablecon_nc>-linea_externa_d.

        vl_can_pi_r2_nc = <fs_lt_tablecon_nc_r2>-cantidad_picking.

        vl_can_pi_r3_nc = vl_can_pi_r3_nc + vl_can_pi_r2_nc .

      endloop.

      if vl_can_pi_r3_nc > 0.
        <fs_lt_tablecon_nc>-eliminar    = abap_true.
      else.
        <fs_lt_tablecon_nc>-cod_picking = '&#x20;'.
      endif.

    endif.

  endloop.

  if lt_tablecon_nc is not initial.
    delete lt_tablecon_nc where eliminar = abap_true.
  endif.

*}+fin de la logica No catch weight

  free: lt_tablecon.
  if lt_tablecon_cd is not initial.
*  lt_tablecon[] = lt_tablecon_cd[].
    append lines of lt_tablecon_cd[] to lt_tablecon[].
  endif.

  if lt_tablecon_cs is not initial.
*  lt_tablecon[] = lt_tablecon_cs[].
    append lines of lt_tablecon_cs[] to lt_tablecon[].
  endif.

  if lt_tablecon_nc is not initial.
*  lt_tablecon[] = lt_tablecon_nc[].
    append lines of lt_tablecon_nc[] to lt_tablecon[].
  endif.


  types: begin of ty_tabaux,
           orden_externa type zlt_tramadetalle-orden_externa    , "Documento de operación comercial base
           cod_picking   type zlt_trama_retor-cod_picking       , "Codigo Picking
           item          type zlt_trama-item                    , "Prod.
           can_tot       type vbap-kwmeng                       , "Cantidad Total general
           can_ent       type vbap-kwmeng                       , "Cantidad Total Entrega
           porcen        type p decimals 10                     , "Porcentaje
         end of ty_tabaux.

  data: lt_tabaux type standard table of ty_tabaux.
  field-symbols: <fs_lt_tabaux> like line of lt_tabaux.

  free: lt_tabaux.
  if lt_tablecon is not initial.

    unassign: <fs_lt_tablecon>.
    loop at lt_tablecon assigning <fs_lt_tablecon> .
      append initial line to lt_tabaux assigning <fs_lt_tabaux>.
      <fs_lt_tabaux>-orden_externa = <fs_lt_tablecon>-orden_externa_d . "Entrega
      <fs_lt_tabaux>-cod_picking   = <fs_lt_tablecon>-cod_picking     . "Cod_picking
      <fs_lt_tabaux>-item          = <fs_lt_tablecon>-item_t          . "Material
    endloop.

    if lt_tabaux is not initial.
      sort: lt_tabaux by orden_externa cod_picking item .

      loop at lt_tabaux assigning <fs_lt_tabaux> .
        loop at lt_tablecon assigning <fs_lt_tablecon>
                                where orden_externa_d = <fs_lt_tabaux>-orden_externa
                                  and cod_picking     = <fs_lt_tabaux>-cod_picking
                                  and item_t          = <fs_lt_tabaux>-item.
          <fs_lt_tabaux>-can_tot  = <fs_lt_tabaux>-can_tot + <fs_lt_tablecon>-cantidad_orden_d.
        endloop.
      endloop.
    endif.
  endif.


  free: rg_entrega,rwa_entrega,t_entpos.
  unassign: <fs_lt_tablecon>.

  loop at lt_tablecon assigning <fs_lt_tablecon>.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = <fs_lt_tablecon>-orden_externa_d
      importing
        output = <fs_lt_tablecon>-orden_externa_d.

    rwa_entrega-sign   = 'I'.
    rwa_entrega-option = 'EQ'.
    rwa_entrega-low    = <fs_lt_tablecon>-orden_externa_d.
    append rwa_entrega to rg_entrega.

    append initial line to t_entpos assigning <fs_t_entpos>.
    <fs_t_entpos>-entrega  = <fs_lt_tablecon>-orden_externa_d.
    <fs_t_entpos>-posnr    = <fs_lt_tablecon>-linea_externa_d.
  endloop.

  sort: t_entpos by entrega posnr.
  sort: rg_entrega by low.
  if rg_entrega is not initial.
    delete adjacent duplicates from rg_entrega comparing low.
  endif.


  free: vl_flag_updentrega ,
        vl_flag_conentrega .

  loop at rg_entrega assigning field-symbol(<fs_rg_entrega>) .

    free: vl_flag_updentrega ,
          vl_flag_conentrega .


    if lt_tablecon_f is not initial.
      free: lt_tablecon.
      lt_tablecon = lt_tablecon_f.
    else.
      lt_tablecon_f[] = lt_tablecon[] .
    endif.

    if lt_tablecon is not initial.
      delete lt_tablecon where orden_externa_d <> <fs_rg_entrega>-low.
    endif.


    read table lt_tablecon with key orden_externa_d = <fs_rg_entrega>-low assigning field-symbol(<fs_lt_tablecon2>).
    if sy-subrc = 0.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = <fs_lt_tablecon2>-orden_externa_d
        importing
          output = <fs_lt_tablecon2>-orden_externa_d.

      data(vl_vbeln_f2) = conv vbeln( <fs_lt_tablecon2>-orden_externa_d ).
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_vbeln_f2
        importing
          output = vl_vbeln_f2.

      free: lt_lips_c,
            rwa_posnr , rg_posnr.

      select * into table lt_lips_c
        from lips
        where vbeln = vl_vbeln_f2.
      if sy-subrc = 0.

        delete lt_lips_c where uepvw <> 'C'.

        free: rwa_posnr , rg_posnr.
        loop at lt_lips_c assigning <fs_lt_lips_c>.

          free: rwa_posnr .
          rwa_posnr-sign   = 'I'.
          rwa_posnr-option = 'EQ'.
          rwa_posnr-low    = <fs_lt_lips_c>-posnr.
          append rwa_posnr to rg_posnr.

          free: rwa_posnr .
          rwa_posnr-sign   = 'I'.
          rwa_posnr-option = 'EQ'.
          rwa_posnr-low    = <fs_lt_lips_c>-uepos.
          append rwa_posnr to rg_posnr.

        endloop.
      endif.

    endif.

    if rg_posnr is not initial.
      sort: lt_tablecon by linea_externa_d.
      if lt_tablecon is not initial.
        loop at lt_tablecon assigning field-symbol(<fs_tab>)
                                where linea_externa_d in rg_posnr.
          <fs_tab>-salvar = abap_true.
        endloop.

      endif.
    else.
      sort: lt_tablecon by cod_picking .
      if lt_tablecon is not initial.

      endif.
    endif.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = <fs_rg_entrega>-low
      importing
        output = <fs_rg_entrega>-low.

    free:lt_tablecon_eli.
    data(vl_entre_sc) = <fs_rg_entrega>-low.

    loop at lt_tablecon assigning <fs_lt_tablecon_x>.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = vl_entre_sc
        importing
          output = vl_entre_sc.
      if <fs_lt_tablecon_x>-orden_externa_d = vl_entre_sc
                      and <fs_lt_tablecon_x>-cod_picking = '&#x20;'.
        append initial line to lt_tablecon_eli assigning <fs_lt_tablecon_eli>.
        move-corresponding: <fs_lt_tablecon_x> to <fs_lt_tablecon_eli>.
      endif.
    endloop.

    if lt_tablecon is not initial.
      "Eliminación de las lineas en de picado en cero
      delete lt_tablecon where cod_picking = '&#x20;'.

      "Rango que indica si hay posiciones de desuere
      if rg_posnr is not initial.
*        salvar = abap_true.
        free: lt_tablecon_aux.
        move-corresponding: lt_tablecon[] to lt_tablecon_aux[].
        if lt_tablecon_aux is not initial.
          delete lt_tablecon_aux where salvar = abap_false.
        endif.
        if lt_tablecon is not initial.
          delete lt_tablecon where salvar = abap_true.
          sort: lt_tablecon by cod_picking.
*          DELETE ADJACENT DUPLICATES FROM lt_tablecon COMPARING cod_picking .
          append lines of lt_tablecon_aux to lt_tablecon.
        endif.

*       Eliminando los duplicados
        types: y_exter type range of zlt_tramadetalle-linea_externa.
        data(r_linext) = value y_exter(
            for ls_cust6 in lt_tablecon
            let s = 'I'
                o = 'EQ'
            in sign = s
             option = o
        ( low = ls_cust6-linea_externa_d ) ).

        if r_linext is not initial.
          sort: r_linext by low.
          delete adjacent duplicates from r_linext comparing low.
        endif.

        free: lt_tablecon_ordf.
        loop at r_linext assigning field-symbol(<fs_r_linext>).

          free: lt_tablecon_ord.
          loop at lt_tablecon assigning field-symbol(<fs_lt_tablecon_5>)
                              where linea_externa_d = <fs_r_linext>-low.
            append initial line to lt_tablecon_ord assigning field-symbol(<fs_lt_tablecon_ord>).
            move-corresponding: <fs_lt_tablecon_5> to <fs_lt_tablecon_ord>.
          endloop.

          if lt_tablecon_ord is not initial.
            sort: lt_tablecon_ord by cod_picking.
          endif.

          append lines of lt_tablecon_ord to lt_tablecon_ordf.

        endloop.

        free: lt_tablecon.
        lt_tablecon[] = lt_tablecon_ordf[].


      else.
        sort: lt_tablecon by cod_picking.
      endif.


    endif.


    if lt_tablecon_eli is not initial.
      "logica para eliminar lineas en la entrega de picking cero
      free: ltd_return_eli  ,
            lwa_h_data_eli  ,
            lwa_h_ctrl_eli  ,
            lt_item_data_eli,
            lt_item_control_eli,
            wa_item_data_eli ,
            wa_item_control_eli.

      lwa_h_data_eli-deliv_numb = <fs_rg_entrega>-low.
      lwa_h_ctrl_eli-deliv_numb = <fs_rg_entrega>-low.
*      lwa_h_ctrl_eli-dlv_del    = abap_true.
      sort: lt_tablecon_eli by linea_externa_d.

      if lt_tablecon_eli is not initial.
        delete adjacent duplicates from lt_tablecon_eli comparing linea_externa_d.
      endif.

      loop at lt_tablecon_eli assigning <fs_lt_tablecon_eli>
                                  where orden_externa_d = vl_entre_sc.

        free: wa_item_data_eli ,
              wa_item_control_eli.

        free: lt_lips_eli.
        select * into table lt_lips_eli
         from lips
         where vbeln = <fs_rg_entrega>-low
           and posnr = <fs_lt_tablecon_eli>-linea_externa_d.
        if sy-subrc = 0.

          free: vl_matnr_eli.
          vl_matnr_eli = <fs_lt_tablecon_eli>-item_t .
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = vl_matnr_eli
            importing
              output = vl_matnr_eli.


          wa_item_data_eli-deliv_numb      = <fs_rg_entrega>-low.                  "al vbeln trabajado.
          wa_item_data_eli-deliv_item      = <fs_lt_tablecon_eli>-linea_externa_d. "al posnr trabajado.
          wa_item_data_eli-material        = vl_matnr_eli.                         "matnr trabajado.
          read table lt_lips_eli index 1 assigning field-symbol(<fs_lt_lips_eli>).
          if sy-subrc = 0.
            wa_item_data_eli-dlv_qty         = <fs_lt_lips_eli>-lfimg.
            wa_item_data_eli-fact_unit_nom   = <fs_lt_lips_eli>-umvkz.
            wa_item_data_eli-fact_unit_denom = <fs_lt_lips_eli>-umvkn.
          endif.
          append wa_item_data_eli to lt_item_data_eli.
        endif.

        wa_item_control_eli-deliv_numb = <fs_rg_entrega>-low.                      "al vbeln trabajado.
        wa_item_control_eli-deliv_item = <fs_lt_tablecon_eli>-linea_externa_d.     "al posnr trabajado.
        wa_item_control_eli-del_item   = 'X'.
        append wa_item_control_eli to lt_item_control_eli.

      endloop.


      call function 'BAPI_OUTB_DELIVERY_CHANGE'
        exporting
          header_data    = lwa_h_data_eli
          header_control = lwa_h_ctrl_eli
          delivery       = <fs_rg_entrega>-low
        tables
          item_data      = lt_item_data_eli
          item_control   = lt_item_control_eli
          return         = ltd_return_eli.

      if line_exists( ltd_return_eli[ type = 'E' ] ).
        call function 'BAPI_TRANSACTION_ROLLBACK'.

        loop at ltd_return_eli assigning field-symbol(<fs_ret>)
                           where type = 'E'.
        endloop.
      else.
        commit work and wait .

        wait up to 1 seconds.

        "Logica para detalle
        types: lyd_deliv_item  type range of posnr.
        data(lr_deliv_item) = value lyd_deliv_item(
           for ls_cust2 in lt_item_control_eli
           let s = 'I'
               o = 'EQ'
           in sign = s
            option = o
        ( low = ls_cust2-deliv_item ) ).

      endif.
    endif.
    "Limpiando la tabla lt_tablecon_eli
    free: lt_tablecon_eli.


    "Logica para header_data
    w_header_data-deliv_numb       = <fs_rg_entrega>-low.

    "Logica para header_control
    w_header_control-deliv_numb    = <fs_rg_entrega>-low.
    v_deliv_numb = <fs_rg_entrega>-low.

    free:lr_posnr.

    if lr_deliv_item is not initial.
      "Logica para detalle
      types: lyd_posnr1  type range of posnr.
      data(lr_posnr1) = value lyd_posnr1(
         for ls_cust1 in t_entpos where ( entrega = <fs_rg_entrega>-low+2(8)
                                    and   posnr   not in lr_deliv_item  )
         let s = 'I'
             o = 'EQ'
         in sign = s
          option = o
      ( low = ls_cust1-posnr ) ).
    else.
      "Logica para detalle
      types: lyd_posnr3  type range of posnr.
      data(lr_posnr3) = value lyd_posnr3(
         for ls_cust3 in t_entpos where ( entrega = <fs_rg_entrega>-low+2(8) )
         let s = 'I'
             o = 'EQ'
         in sign = s
          option = o
      ( low = ls_cust3-posnr ) ).
    endif.

    if lr_posnr1 is not initial.
      move-corresponding: lr_posnr1 to lr_posnr.
    endif.

    if lr_posnr3 is not initial.
      move-corresponding: lr_posnr3 to lr_posnr.
    endif.


    if lr_posnr is not initial.
      sort: lr_posnr by low.
      delete adjacent duplicates from lr_posnr comparing low.
    endif.


    free:
          vl_canti_acu_pick,
          vl_peso_acu_pick,
          v_posnr_vl.

    v_posnr_vl = '900000'.

    loop at lr_posnr assigning field-symbol(<fs_lr_posnr>).

      v_corre    = 0.

      vl_orden_ext = <fs_rg_entrega>-low.
      vl_line_ext  = <fs_lr_posnr>-low.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_line_ext
        importing
          output = vl_line_ext.

      free: vl_canti_acu_pick,
            vl_peso_acu_pick.
      loop at lt_tablecon assigning <fs_lt_tablecon>
                             where orden_externa_d = vl_orden_ext+2(8)
                               and linea_externa_d = vl_line_ext.


        vl_canti_acu_pick = vl_canti_acu_pick + <fs_lt_tablecon>-cantidad_picking.
        condense: vl_canti_acu_pick no-gaps .
        vl_peso_acu_pick  = vl_peso_acu_pick  + <fs_lt_tablecon>-peso_picking_estandar.
        condense: vl_peso_acu_pick no-gaps.
        v_posnr_vl = v_posnr_vl + 1.
        v_corre    = v_corre + 1.
        "Agregando una nueva linea en el ITEM
        append initial line to t_item_data assigning <fs_t_item_data>.
*i.   Deliv_numb: z_entrega
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = <fs_rg_entrega>-low
          importing
            output = <fs_rg_entrega>-low.
        <fs_t_item_data>-deliv_numb = <fs_rg_entrega>-low. "Entrega

*ii.  Deliv_item: agregar las posiciones originales, y para el resto buscar en la tabla cuantos
*registros son por combinación de z_entrega y z_posición. Por cada uno poner el 90000X, empezando de 900001 en adelante.
        <fs_t_item_data>-deliv_item = v_posnr_vl.

*iii. Material: z_material de la combinación z_entrega y z_posición.

        free: vl_matnr.
        vl_matnr = <fs_lt_tablecon>-item_t .
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = vl_matnr
          importing
            output = vl_matnr.
        <fs_t_item_data>-material   = vl_matnr.
*iv.  Batch: buscar en la tabla zlt_tramaconfirmada por combinación de z_entrega y z_posición. Poner el z_lote por cada registro.
*Lógica para convertir LPN a LOTE SAP:
*ir a la ausp igualando atwrt con el LPN enviado, attin con
*"LPN" y obtener objek. Con este dato ir a la mch1 e igualar a CUOBJ_BM con el objek encontrado y matnr con el trabajado. Obtener charg.
*Lógica para bonificación:
*Si en la lips la posición tiene uepvw igual a X entonces buscar el uepos y prorratear la cantidad entre las dos líneas.

        free: vl_atwrt,
              vl_atinn,
              vl_atinn2.

        vl_atwrt = <fs_lt_tablecon>-lpn_origen_task.

        call function 'CONVERSION_EXIT_ATINN_INPUT'
          exporting
            input  = 'LPN'
          importing
            output = vl_atinn2.

        free: lt_ausp_fi,
              rwa_objek , rg_objek.

        select * into table lt_ausp_fi
          from ausp
          where atwrt = vl_atwrt
            and atinn = vl_atinn2.
        if sy-subrc = 0.

          free: rwa_objek , rg_objek.
          loop at lt_ausp_fi assigning field-symbol(<fs_lt_ausp_fi>).
            rwa_objek-sign   = 'I'.
            rwa_objek-option = 'EQ'.
            rwa_objek-low    = <fs_lt_ausp_fi>-objek.
            append rwa_objek to rg_objek.
          endloop.

          select single charg into @data(vl_charg)
            from mch1
            where cuobj_bm in @rg_objek
              and matnr    = @vl_matnr.
          if sy-subrc = 0.
*iv.  Batch:
            <fs_t_item_data>-batch = vl_charg.
          endif.
        endif.
*v.   Hieraritem: posición origanl cuando coincida el material.
        <fs_t_item_data>-hieraritem = <fs_lt_tablecon>-linea_externa_d.
*vi.  Usehieritm: "1" cuando la posicion sea 9xxxxx.
        <fs_t_item_data>-usehieritm = '1'.
*vii. Dlv_qty: buscar en la tabla zlt_tramaconfirmada por combinación de z_entrega y z_posición.
*     Poner el z_qpedida por cada registro. Cantidad_trans_expedida de la trama
        <fs_t_item_data>-dlv_qty = <fs_lt_tablecon>-cantidad_picking.
*viii.Fact_unit_nom: ir a la lips igualnado vbeln a z_entrega y posnr a z_posición. Obtener umvkz.
        select single umvkz into @data(vl_umvkz)
        from lips
        where vbeln = @<fs_rg_entrega>-low
          and posnr = @<fs_lt_tablecon>-linea_externa_d.
        if sy-subrc = 0.
          <fs_t_item_data>-fact_unit_nom = vl_umvkz.
        endif.
*ix.  Fact_unit_denom: ir a la lips igualnado vbeln a z_entrega y posnr a z_posición. Obtener umvkn.
        select single umvkn into @data(vl_umvkn)
          from lips
          where vbeln = @<fs_rg_entrega>-low
            and posnr = @<fs_lt_tablecon>-linea_externa_d.
        if sy-subrc = 0.
          <fs_t_item_data>-fact_unit_denom = vl_umvkn.
        endif.

        "Logica para llenar item control
        append initial line to t_item_control  assigning <fs_t_item_control>.
        <fs_t_item_control>-deliv_numb = <fs_rg_entrega>-low. "Entrega
        <fs_t_item_control>-deliv_item = v_posnr_vl.
        <fs_t_item_control>-chg_delqty = 'X'.

*        Verificando si el material es CW
        select single cwqrel
          from mara
          into @data(vl_cwqrel_con_12)
          where matnr = @vl_matnr.
        if sy-subrc = 0.
          case vl_cwqrel_con_12.
            when 'X'. "CW

              append initial line to t_cwm_item_data assigning <fs_t_cwm_item_data>.
              <fs_t_cwm_item_data>-deliv_numb = <fs_rg_entrega>-low. "Entrega
              <fs_t_cwm_item_data>-itm_number = v_posnr_vl.   "Posición
*              <fs_t_cwm_item_data>-dlv_qty    = <fs_lt_tablecon>-peso_picking_estandar.   "-@0001
              <fs_t_cwm_item_data>-dlv_qty    = <fs_lt_tablecon>-peso_doble_uni. "+@0001
              <fs_t_cwm_item_data>-sales_unit = 'KG'.

            when ' '. "Material normal

          endcase.
        endif.


      endloop.

      "LINEA DE CABECERA DE POSICIÓN
      unassign: <fs_t_item_data>.
      append initial line to t_item_data assigning <fs_t_item_data>.
      <fs_t_item_data>-deliv_numb      = <fs_rg_entrega>-low. "Entrega
      <fs_t_item_data>-deliv_item      = <fs_lr_posnr>-low.
      <fs_t_item_data>-material        = vl_matnr.
      <fs_t_item_data>-batch           = ' '.
      <fs_t_item_data>-hieraritem      = ' '.
      <fs_t_item_data>-usehieritm      = ' '.
      <fs_t_item_data>-dlv_qty         = 0. "vl_canti_acu_pick.
      <fs_t_item_data>-fact_unit_nom   = vl_umvkz.
      <fs_t_item_data>-fact_unit_denom = vl_umvkn.

*      Logica para llenar item control
      append initial line to t_item_control  assigning <fs_t_item_control>.
      <fs_t_item_control>-deliv_numb = <fs_rg_entrega>-low. "Entrega
      <fs_t_item_control>-deliv_item = <fs_lr_posnr>-low.   "posicion
      <fs_t_item_control>-chg_delqty = 'X'.

    endloop.



    free:
          vl_sms_updentrega,
          vl_sms_conentrega.

    move 'U' to w_dlvcontrol-upd_ind.

    free: rwa_desuere , rg_desuere.
    free: rwa_desuere2 , rg_desuere2.
    free: rwa_posnr , rg_posnr.
    loop at lt_lips_c assigning <fs_lt_lips_c>.

      free: rwa_posnr .
      rwa_posnr-sign   = 'I'.
      rwa_posnr-option = 'EQ'.
      rwa_posnr-low    = <fs_lt_lips_c>-posnr.
      append rwa_posnr to rg_posnr.

      free: rwa_posnr .
      rwa_posnr-sign   = 'I'.
      rwa_posnr-option = 'EQ'.
      rwa_posnr-low    = <fs_lt_lips_c>-uepos.
      append rwa_posnr to rg_posnr.

      free: rwa_desuere .
      rwa_desuere-sign     = 'I'.
      rwa_desuere-option   = 'BT'.
      rwa_desuere-low      = <fs_lt_lips_c>-uepos.
      rwa_desuere-high     = <fs_lt_lips_c>-posnr.
      append rwa_desuere to rg_desuere.

    endloop.

    if rg_posnr is not initial.
      free: lt_lips_c2.
      select * into table lt_lips_c2
        from lips
        where vbeln = v_deliv_numb
          and posnr in rg_posnr.
      if sy-subrc = 0.

        free: lt_vbap.
        select * into table lt_vbap
          from vbap
          for all entries in lt_lips_c2
          where vbeln eq lt_lips_c2-vgbel
            and posnr eq lt_lips_c2-vgpos.
        if sy-subrc = 0.


          data: vl_matnr_de type mara-matnr.
          free: lt_consoli,
                vl_matnr_de.

          free: rwa_desuere2 , rg_desuere2.
          unassign: <fs_lt_lips_c2>.
          loop at lt_lips_c2 assigning <fs_lt_lips_c2>.

            free: vl_matnr_de.
            vl_matnr_de = <fs_lt_lips_c2>-matnr.

            types: y_posnr_de  type range of lips-posnr.
            data(lr_posnr_de) = value y_posnr_de(
            for ls_cust7 in lt_lips_c2 where ( matnr = <fs_lt_lips_c2>-matnr )
            let s = 'I'
                o = 'EQ'
            in sign = s
             option = o
            ( low = ls_cust7-posnr ) ).
            if lr_posnr_de is not initial.
              sort: lr_posnr_de by low.
              delete adjacent duplicates from lr_posnr_de comparing low.
            endif.

*            FREE: rwa_desuere2 , rg_desuere2.
            free: rwa_desuere2 .
            loop at rg_desuere assigning field-symbol(<fs_rg_desuere>)
                                            where ( low  in lr_posnr_de
                                               or   high in lr_posnr_de ).
*                                          WHERE ( low  = <fs_lt_lips_c2>-posnr
*                                             OR   high = <fs_lt_lips_c2>-posnr ).
              free: rwa_desuere2 .
              rwa_desuere2-sign     = 'I'.
              rwa_desuere2-option   = 'BT'.

              read table lt_lips_c2 with key posnr = <fs_rg_desuere>-low assigning field-symbol(<fs_lt_lips_c22>).
              if sy-subrc = 0.
                rwa_desuere2-low    = <fs_lt_lips_c22>-vgpos.
              endif.

              read table lt_lips_c2 with key posnr = <fs_rg_desuere>-high assigning field-symbol(<fs_lt_lips_c23>).
              if sy-subrc = 0.
                rwa_desuere2-high     = <fs_lt_lips_c23>-vgpos.
              endif.

              append rwa_desuere2 to rg_desuere2.

            endloop.

            free: vl_kwmeng.
            loop at lt_vbap assigning field-symbol(<fs_lt_vbap_3>)
                                      where vbeln = <fs_lt_lips_c2>-vgbel
                                        and posnr = <fs_lt_lips_c2>-vgpos.
              vl_kwmeng = vl_kwmeng + <fs_lt_vbap_3>-kwmeng.
            endloop.



            append initial line to lt_consoli assigning <fs_lt_consoli>.
            <fs_lt_consoli>-vbeln_i = <fs_lt_lips_c2>-vbeln.
            <fs_lt_consoli>-posnr_i = <fs_lt_lips_c2>-posnr.
            read table lt_vbap with key vbeln = <fs_lt_lips_c2>-vgbel
                                        posnr = <fs_lt_lips_c2>-vgpos
                               assigning field-symbol(<fs_lt_vbap>).
            if sy-subrc = 0.
              <fs_lt_consoli>-vbeln_r = <fs_lt_vbap>-vbeln.
              <fs_lt_consoli>-posnr_r = <fs_lt_vbap>-posnr.
              <fs_lt_consoli>-kwmeng  = <fs_lt_vbap>-kwmeng.
              if vl_kwmeng is not initial.
                <fs_lt_consoli>-porcen  = ( ( <fs_lt_vbap>-kwmeng * 100 ) / vl_kwmeng ) / 100.
              endif.
            endif.

          endloop.

          if rg_desuere2 is not initial.
            sort: rg_desuere2 by low high.
            delete adjacent duplicates from rg_desuere2 comparing all fields.
          endif.

          loop at rg_desuere2 assigning field-symbol(<fs_rg_desuere_lo>).

            free: vl_kwmeng.
            unassign: <fs_lt_consoli>.
            read table lt_consoli with key posnr_r = <fs_rg_desuere_lo>-low assigning <fs_lt_consoli>.
            if sy-subrc = 0.
              vl_kwmeng = vl_kwmeng + <fs_lt_consoli>-kwmeng.
            endif.

            unassign: <fs_lt_consoli>.
            read table lt_consoli with key posnr_r = <fs_rg_desuere_lo>-high assigning <fs_lt_consoli>.
            if sy-subrc = 0.
              vl_kwmeng = vl_kwmeng + <fs_lt_consoli>-kwmeng.
            endif.

            loop at lt_consoli assigning field-symbol(<fs_lt_consoli_33>)
                                             where ( posnr_r = <fs_rg_desuere_lo>-low or
                                                     posnr_r = <fs_rg_desuere_lo>-high ).

              if vl_kwmeng is not initial.
                <fs_lt_consoli_33>-porcen  = ( ( <fs_lt_consoli_33>-kwmeng * 100 ) / vl_kwmeng ) / 100.
              endif.
            endloop.

          endloop.


          if lt_consoli is not initial.
            loop at lt_consoli assigning field-symbol(<fs_lt_consoli_3>).
              loop at t_item_data assigning field-symbol(<fs_t_item_data_3>)
                                   where deliv_numb = <fs_lt_consoli_3>-vbeln_i
                                     and hieraritem = <fs_lt_consoli_3>-posnr_i.

                <fs_t_item_data_3>-dlv_qty = <fs_t_item_data_3>-dlv_qty * <fs_lt_consoli_3>-porcen.

              endloop.

            endloop.
          endif.

          "esta tabla esta llena cuando hay un DESUERE
          if lt_consoli is not initial.

            free: lt_temp.
            loop at lt_consoli assigning field-symbol(<fs_lt_consoli_as>).
*              APPEND INITIAL LINE TO lt_temp ASSIGNING <fs_lt_temp>.
*              <fs_lt_temp>-vbeln_i = <fs_lt_consoli_as>-vbeln_i.
*              <fs_lt_temp>-posnr_i = <fs_lt_consoli_as>-posnr_i.
*              <fs_lt_temp>-porcen  = <fs_lt_consoli_as>-porcen.

              loop at t_item_data assigning field-symbol(<fs_t_item_data_as>)
                                            where deliv_numb = <fs_lt_consoli_as>-vbeln_i
                                             and  hieraritem = <fs_lt_consoli_as>-posnr_i.
                append initial line to lt_temp assigning <fs_lt_temp>.
                <fs_lt_temp>-vbeln_i = <fs_lt_consoli_as>-vbeln_i.
                <fs_lt_temp>-posnr_i = <fs_lt_consoli_as>-posnr_i.
                <fs_lt_temp>-porcen  = <fs_lt_consoli_as>-porcen.
                <fs_lt_temp>-posnr_r = <fs_t_item_data_as>-deliv_item.
              endloop.
*              READ TABLE t_item_data WITH KEY deliv_numb = <fs_lt_consoli_as>-vbeln_i
*                                              hieraritem = <fs_lt_consoli_as>-posnr_i
*                                     ASSIGNING FIELD-SYMBOL(<fs_t_item_data_as>).
*              IF sy-subrc = 0.
*                <fs_lt_temp>-posnr_r = <fs_t_item_data_as>-deliv_item.
*              ENDIF.
            endloop.



            if lt_temp is not initial.
              loop at t_cwm_item_data assigning field-symbol(<fs_t_cwm_item_data_as>).
                read table lt_temp with key vbeln_i = <fs_t_cwm_item_data_as>-deliv_numb
                                            posnr_r = <fs_t_cwm_item_data_as>-itm_number
                                      assigning <fs_lt_temp>.
                if sy-subrc = 0.
                  <fs_t_cwm_item_data_as>-dlv_qty = <fs_t_cwm_item_data_as>-dlv_qty  * <fs_lt_temp>-porcen.
                endif.
              endloop.
            endif.
          endif.
        endif.
      endif.
    endif.

    "Ejecucion de la Bapi
    call function 'DIALOG_SET_NO_DIALOG'.

    call function 'BAPI_OUTB_DELIVERY_CHANGE'
      exporting
        header_data    = w_header_data
        header_control = w_header_control
        delivery       = v_deliv_numb
        techn_control  = w_dlvcontrol
      tables
        item_data      = t_item_data
        item_control   = t_item_control
        return         = t_return
        item_data_spl  = t_item_data_spl
        cwm_item_data  = t_cwm_item_data.

    if line_exists( t_return[ type = 'E' ] ).

      call function 'BAPI_TRANSACTION_ROLLBACK'.
      break lore.
      "encontró error
      loop at t_return assigning field-symbol(<t_return>)
                          where type = 'E'.

        if <t_return>-message is initial.
          call function 'FORMAT_MESSAGE'
            exporting
              id        = <t_return>-id
              lang      = sy-langu
              no        = <t_return>-number
              v1        = <t_return>-message_v1
              v2        = <t_return>-message_v2
              v3        = <t_return>-message_v3
              v4        = <t_return>-message_v4
            importing
              msg       = <t_return>-message
            exceptions
              not_found = 1
              others    = 2.
          if sy-subrc = 0.
            vl_sms_updentrega = <t_return>-message.
          endif.
        endif.

        "@Inicio Aqui logica para actualizar el estatus en el monitor de retorno de WMS a SAP
        free: vl_sms_e3,
              vl_msgno_e3,
              vl_msgty_e3,
              vl_msgid_e3.

        vl_msgno_e3 = <t_return>-number.   "   Número del mensaje de sistema
        vl_msgty_e3 = <t_return>-type.     "   Clase de mensaje
        vl_msgid_e3 = <t_return>-id.       "   Identificación de los mensajes
        concatenate: vl_sms_updentrega <t_return>-message_v4 into vl_sms_updentrega separated by space.
        vl_sms_e3   = vl_sms_updentrega.   "   Mensaje Update Entrega

        exit.
        "@Fin

      endloop.

    else.

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.
      "modificación ok.
      vl_sms_updentrega = 'X'.

      do 5 times.

      enddo.

      free:
            vl_msgno_e3,
            vl_msgty_e3,
            vl_msgid_e3,
            vl_sms_e3.

      vl_msgno_e3 = ' '.    "   Número del mensaje de sistema
      vl_msgty_e3 = 'S'.    "   Clase de mensaje
      vl_msgid_e3 = ' '.    "   Identificación de los mensajes
      concatenate: 'Se Actualizo la Entrega:' v_deliv_numb into vl_sms_e3 separated by space .  " Mensaje Update Entrega

      vl_flag_updentrega = abap_true.

    endif.

    free: lt_zlt_tramadetalle_f,
          vl_orden_externa_f,
          wa_tramadetalle.

    if v_deliv_numb is not initial.

      vl_orden_externa_f = v_deliv_numb.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_orden_externa_f
        importing
          output = vl_orden_externa_f.

      select * into table lt_zlt_tramadetalle_f
        from zlt_tramadetalle
        where orden_externa = vl_orden_externa_f.
      if sy-subrc = 0.

        read table lt_zlt_trama_retor index 1 assigning field-symbol(<fs_tra_ret>).
        if sy-subrc = 0.
          data(vl_ov) = <fs_tra_ret>-sap_orden_salida.
        endif.

        loop at lt_zlt_tramadetalle_f into wa_tramadetalle.
          wa_tramadetalle-number_e          = vl_msgno_e3.    "   Número del mensaje de sistema
          wa_tramadetalle-type_e            = vl_msgty_e3.    "   Clase de mensaje
          wa_tramadetalle-id_e              = vl_msgid_e3.    "   Identificación de los mensajes
          wa_tramadetalle-message_e         = vl_sms_e3.      "   Mensaje Update Entrega
          wa_tramadetalle-user_c            = sy-uname.
          wa_tramadetalle-fecha_c           = sy-datum.
          wa_tramadetalle-hora_c            = sy-uzeit.
          wa_tramadetalle-sap_orden_salida  = vl_ov.
          modify zlt_tramadetalle from wa_tramadetalle.
          commit work and wait.
        endloop.
      endif.

    endif.

    "con esta linea no debe de continuar el proceso hacia abajo y debe de pasar a la siguiente entrega
    if vl_flag_updentrega = abap_false.

      "Limpiando los datos procesados.
      free:
          w_header_data,
          w_header_control,
          v_deliv_numb,
          w_dlvcontrol,
          t_item_data,
          t_item_control,
          t_return,
          t_item_data_spl,
          t_cwm_item_data,
          w_vbkok_wa,
          l_delivery,
          ls_error,
          t_prot,
*          t_vbpok_tab,
          vl_vbeln_f,
          lr_posnr,
          vl_matnr,
          lr_posnr1,
          lr_posnr3,

          vl_vbeln_f2,
          vl_entre_sc,
          r_linext,
          ltd_return_eli  ,
          lwa_h_data_eli  ,
          lwa_h_ctrl_eli  ,
          lt_item_data_eli,
          lt_item_control_eli,
          wa_item_data_eli ,
          wa_item_control_eli,
          vl_matnr_eli,
          lr_deliv_item,
          lr_posnr1,
          lr_posnr3,
          lr_posnr,
          lr_posnr_de.

      continue.
    endif.

    w_vbkok_wa-vbeln_vl = v_deliv_numb."Entrega
    w_vbkok_wa-wabuc    = 'X'.

    free: t_vbpok_tab,
          lt_entpos.
    unassign: <fs_t_item_data>.
    loop at t_item_data assigning <fs_t_item_data>.
      free: lwa_lips,
            lv_batch,
            lv_lgmng.
*i.     Vbeln_vl: z_entrega (de la 2)
      lwa_lips-vbeln = v_deliv_numb.
      w_vbpok_tab-vbeln_vl = lwa_lips-vbeln."Entrega
*ii.    Posnr_vl: ítem_data-delivery_numb (de la 2) sola las 9xxxxx.
      lwa_lips-posnr = <fs_t_item_data>-deliv_item.
      w_vbpok_tab-posnr_vl = lwa_lips-posnr.
*iii.   Vbeln: obtener la entrega principal a la que hace referencia.
      lwa_lips-vgbel = v_deliv_numb.
      w_vbpok_tab-vbeln = lwa_lips-vgbel."Entrega
*iv.    Posnn: obtener la posición principal a la que hace referencia
      lwa_lips-vgpos = <fs_t_item_data>-hieraritem.
      w_vbpok_tab-posnn = lwa_lips-vgpos.

      append initial line to lt_entpos assigning field-symbol(<fs_lt_entpos>).
      <fs_lt_entpos>-vgbel = lwa_lips-vgbel."Entrega
      <fs_lt_entpos>-vgpos = lwa_lips-vgpos.

*v.     Matnr: ítem_data-material  (de la 2).
      lwa_lips-matnr =  <fs_t_item_data>-material.
      w_vbpok_tab-matnr = lwa_lips-matnr.
*vi.    Charg: ítem_data-batch (de la 2).
      lv_batch = <fs_t_item_data>-batch.
      w_vbpok_tab-charg = lv_batch.
*vii.   Lfimg: ítem_data-dlv_qty (de la 2).
      lv_lgmng = <fs_t_item_data>-dlv_qty.

      w_vbpok_tab-pikmg = lv_lgmng.
      w_vbpok_tab-lfimg = lv_lgmng.

*viii.  Umvkz: ítem_data-fact_unit_nom (de la 2).
      lwa_lips-umvkz    = <fs_t_item_data>-fact_unit_nom.
      w_vbpok_tab-umvkz = lwa_lips-umvkz.
*ix.    Umvkn: ítem_data-fact_unit_denom (de la 2).
      lwa_lips-umvkn = <fs_t_item_data>-fact_unit_denom.
      w_vbpok_tab-umvkn = lwa_lips-umvkn.
*x.     ormng: ítem_data-dlv_qty (de la 2)
*    w_vbpok_tab-ormng = <fs_t_item_data>-dlv_qty.
      append w_vbpok_tab to t_vbpok_tab.

    endloop.

    if t_vbpok_tab is not initial.
      delete t_vbpok_tab where posnn = '0000'.
    endif.

    loop at t_vbpok_tab assigning field-symbol(<fs_t_vbpok_tab>).
      unassign: <fs_t_cwm_item_data>.
      read table t_cwm_item_data with key deliv_numb = <fs_t_vbpok_tab>-vbeln_vl
                                          itm_number = <fs_t_vbpok_tab>-posnr_vl
                                 assigning <fs_t_cwm_item_data>.
      if sy-subrc = 0.

        free: lv_lfimg.

        lv_lfimg = <fs_t_cwm_item_data>-dlv_qty.
        <fs_t_vbpok_tab>-/cwm/lfimg = lv_lfimg. " Peso_pickin_estandar
        <fs_t_vbpok_tab>-/cwm/pikmg = lv_lfimg. " Peso_pickin_estandar
        <fs_t_vbpok_tab>-/cwm/lfime = 'KG'.     " KG por defecto
        <fs_t_vbpok_tab>-/cwm/pikme = 'KG'.     " KG por defecto

      endif.

    endloop.

    do 5 times.

    enddo.

    free: t_prot,
          ls_error.

    data: vl_if_database_update_1 type  leshp_database_update.
    free: vl_if_database_update_1.
    vl_if_database_update_1 = 1.

    call function 'WS_DELIVERY_UPDATE_2'
      exporting
        vbkok_wa             = w_vbkok_wa
        commit               = 'X'
        synchron             = 'X'
        delivery             = l_delivery
        update_picking       = 'X'
        if_database_update_1 = vl_if_database_update_1
      importing
        ef_error_any         = ls_error
      tables
        prot                 = t_prot
        vbpok_tab            = t_vbpok_tab
      exceptions
        error_message        = 99.

    if ls_error = space.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.
    else.
      call function 'BAPI_TRANSACTION_ROLLBACK'.
    endif.

    if t_prot is not initial.

      free: vl_sms_c.
      loop at t_prot assigning field-symbol(<fs_t_prot>)
                              where msgty = 'E'.

        data(vl_lfimg) = conv char20( <fs_t_prot>-lfimg ).

        call function 'FORMAT_MESSAGE'
          exporting
            id        = <fs_t_prot>-msgid
            lang      = sy-langu
            no        = <fs_t_prot>-msgno
            v1        = <fs_t_prot>-msgv1
            v2        = <fs_t_prot>-msgv2
            v3        = <fs_t_prot>-msgv3
            v4        = <fs_t_prot>-msgv4
          importing
            msg       = vl_sms_c
          exceptions
            not_found = 1
            others    = 2.


        vl_msgno_c = <fs_t_prot>-msgno.   "   Número del mensaje de sistema
        vl_msgty_c = <fs_t_prot>-msgty.	  "   Clase de mensaje
        vl_msgid_c = <fs_t_prot>-msgid.	  "	  Identificación de los mensajes
        exit.
      endloop.

      vl_sms_conentrega = vl_sms_c.

*      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    else.

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.

      vl_sms_conentrega = 'X'.

      vl_flag_conentrega = abap_true.

      vl_msgty_c       = 'S'.  "Tipo mensaje: Para Update De Entrega
      vl_msgid_c       = ' '.  "Clase de mensaje de la entrega
      vl_msgno_c       = ' '.  "Número de mensaje de entrega
      concatenate: 'Se contabilizo la entrega' v_deliv_numb into vl_sms_c separated by space. "Texto de mensaje de entrega

    endif.

    free: lt_zlt_tramadetalle_f,
          vl_orden_externa_f,
          wa_tramadetalle.

    if v_deliv_numb is not initial.
      vl_orden_externa_f = v_deliv_numb.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_orden_externa_f
        importing
          output = vl_orden_externa_f.

      select * into table lt_zlt_tramadetalle_f
        from zlt_tramadetalle
        where orden_externa = vl_orden_externa_f.
      if sy-subrc = 0.

        read table lt_zlt_trama_retor index 1 assigning field-symbol(<fs_tra_ret2>).
        if sy-subrc = 0.
          data(vl_ov2) = <fs_tra_ret2>-sap_orden_salida.
        endif.

        loop at lt_zlt_tramadetalle_f into wa_tramadetalle.
          wa_tramadetalle-type_c            = vl_msgty_c.
          wa_tramadetalle-id_c              = vl_msgid_c.
          wa_tramadetalle-number_c          = vl_msgno_c.
          wa_tramadetalle-message_c         = vl_sms_c.
          wa_tramadetalle-user_c            = sy-uname.
          wa_tramadetalle-fecha_c           = sy-datum.
          wa_tramadetalle-hora_c            = sy-uzeit.
          wa_tramadetalle-sap_orden_salida  = vl_ov2.
          modify zlt_tramadetalle from wa_tramadetalle.
          commit work and wait.
        endloop.
      endif.
    endif.

    "Si no contabilizo no debe de seguir realizando las lineas hacia abajo u debe de pasar a la siguiente entrega.
    if vl_flag_conentrega eq abap_false.

      "Limpiando los datos procesados.
      free:
          w_header_data,
          w_header_control,
          v_deliv_numb,
          w_dlvcontrol,
          t_item_data,
          t_item_control,
          t_return,
          t_item_data_spl,
          t_cwm_item_data,
          w_vbkok_wa,
          l_delivery,
          ls_error,
          t_prot,
*          t_vbpok_tab,
          vl_vbeln_f,
          lr_posnr,
          vl_matnr,
          lr_posnr1,
          lr_posnr3,
          vl_vbeln_f2,
          vl_entre_sc,
          r_linext,
          ltd_return_eli  ,
          lwa_h_data_eli  ,
          lwa_h_ctrl_eli  ,
          lt_item_data_eli,
          lt_item_control_eli,
          wa_item_data_eli ,
          wa_item_control_eli,
          vl_matnr_eli,
          lr_deliv_item,
          lr_posnr1,
          lr_posnr3,
          lr_posnr,
          lr_posnr_de.

      continue.
    endif.

    "Logica ultimo lote.
    if t_vbpok_tab is not initial.

      types: lyd_range_m  type range of acdoca-matnr.
      data(lr_matnr_m) = value lyd_range_m(
           for ls_custm in t_vbpok_tab
           let s = 'I'
               o = 'EQ'
           in sign = s
            option = o
       ( low = ls_custm-matnr ) ).

      if lr_matnr_m is not initial.
        sort: lr_matnr_m by low.
        delete adjacent duplicates from lr_matnr_m comparing low.

        free:  vl_entrega,
               t_vbpok_tab2.
        loop at lr_matnr_m assigning field-symbol(<fs_lr_matnr_m>).

          free: t_vbpok_tab2.
          loop at t_vbpok_tab assigning field-symbol(<fs_t_vbpok_tab_m1>)
                                              where matnr = <fs_lr_matnr_m>-low.
            append initial line to t_vbpok_tab2 assigning field-symbol(<fs_t_vbpok_tab2>).
            move-corresponding: <fs_t_vbpok_tab_m1> to <fs_t_vbpok_tab2>.

            vl_entrega = <fs_t_vbpok_tab_m1>-vbeln_vl.
          endloop.

          free: lt_mch1_ul.
          select * into table lt_mch1_ul
            from mch1
            for all entries in t_vbpok_tab2
            where matnr = t_vbpok_tab2-matnr
              and charg = t_vbpok_tab2-charg.
          if sy-subrc = 0.

            sort: lt_mch1_ul by vfdat descending.

            if vl_entrega is not initial.
              free: lt_likp_ul,
                    vl_kunag.
              select * into table lt_likp_ul
                from likp
                where vbeln = vl_entrega.
              if sy-subrc = 0.
                read table lt_likp_ul index 1 assigning <fs_lt_likp_ul>.
                if sy-subrc = 0.
                  vl_kunag = <fs_lt_likp_ul>-kunag.
                endif.
              endif.
            endif.

            read table lt_mch1_ul index 1
                                  assigning field-symbol(<fs_lt_mch1_ul2>).
            if sy-subrc = 0.

              free: wa_ztsd_ultlote.
              wa_ztsd_ultlote-mandt      = sy-mandt.
              wa_ztsd_ultlote-z_cliente  = vl_kunag.
              wa_ztsd_ultlote-z_material = <fs_lt_mch1_ul2>-matnr.
              wa_ztsd_ultlote-z_venci    = <fs_lt_mch1_ul2>-vfdat.

              modify ztsd_ultlote from wa_ztsd_ultlote .
              commit work and wait.

            endif.
          endif.
        endloop.
      endif.
    endif.

    free: lt_zlt_tramadetalle_f,
          vl_orden_externa_f,
          wa_tramadetalle.

    if v_deliv_numb is not initial.
      vl_orden_externa_f = v_deliv_numb.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_orden_externa_f
        importing
          output = vl_orden_externa_f.

      select * into table lt_zlt_tramadetalle_f
        from zlt_tramadetalle
        where orden_externa = vl_orden_externa_f.
      if sy-subrc = 0.

        read table lt_zlt_trama_retor index 1 assigning field-symbol(<fs_tra_ret22>).
        if sy-subrc = 0.
          data(vl_ov22) = <fs_tra_ret22>-sap_orden_salida.
        endif.

        loop at lt_zlt_tramadetalle_f into wa_tramadetalle.
          wa_tramadetalle-type_c            = vl_msgty_c.
          wa_tramadetalle-id_c              = vl_msgid_c.
          wa_tramadetalle-number_c          = vl_msgno_c.
          wa_tramadetalle-message_c         = vl_sms_c.
          wa_tramadetalle-user_c            = sy-uname.
          wa_tramadetalle-fecha_c           = sy-datum.
          wa_tramadetalle-hora_c            = sy-uzeit.
          wa_tramadetalle-sap_orden_salida  = vl_ov22.
          modify zlt_tramadetalle from wa_tramadetalle.
          commit work and wait.
        endloop.
      endif.
    endif.

    select vbeln, posnr, vgbel, vgpos into table @data(lt_lips_2)
      from lips
      for all entries in @lt_entpos
      where vbeln = @lt_entpos-vgbel  "Entrega
        and posnr = @lt_entpos-vgpos. "Posición
    if sy-subrc = 0.

      loop at lt_lips_2 assigning field-symbol(<fs_lt_lips_2_fi>).
        "Logica para actualizar la tabla
        select * into table @data(gtd_ztsd_stock)
          from ztsd_stock
          where z_pedido   = @<fs_lt_lips_2_fi>-vgbel  "Pedido
            and z_posicion = @<fs_lt_lips_2_fi>-vgpos. "Posición
        if sy-subrc = 0.

          loop at gtd_ztsd_stock into data(wa_ztsd_stock).
            wa_ztsd_stock-z_eliminar = 'X'.
            modify ztsd_stock from wa_ztsd_stock .
            commit work and wait.
          endloop.

          "logica para actualizar el stock
          if vl_sms_conentrega = 'X'.

            data: rg_ordenexp type range of zlt_estatus-z_ordenexp.
            data: rwa_ordenexp like line of rg_ordenexp.

            free: rwa_ordenexp , rg_ordenexp.

            free: vl_z_ordenexp.
            data(ls_zlt_trama_retor) = value #( lt_zlt_trama_retor[ 1 ] default space ).
            if ls_zlt_trama_retor <> space..
              data(lv_almacen) = ls_zlt_trama_retor-almacen.
              case lv_almacen.
                when 'FRIO'.
                  rwa_ordenexp-sign   = 'I'.
                  rwa_ordenexp-option = 'EQ'.
                  rwa_ordenexp-low    = 'SP_1001'.
                  append rwa_ordenexp to rg_ordenexp.

                  rwa_ordenexp-sign   = 'I'.
                  rwa_ordenexp-option = 'EQ'.
                  rwa_ordenexp-low    = 'SP_1001_01'.
                  append rwa_ordenexp to rg_ordenexp.

                when 'SECO'.
                  rwa_ordenexp-sign   = 'I'.
                  rwa_ordenexp-option = 'EQ'.
                  rwa_ordenexp-low    = 'SP_1000'.
                  append rwa_ordenexp to rg_ordenexp.

                  rwa_ordenexp-sign   = 'I'.
                  rwa_ordenexp-option = 'EQ'.
                  rwa_ordenexp-low    = 'SP_1000_01'.
                  append rwa_ordenexp to rg_ordenexp.
              endcase.

              free: vl_tor_id.
              data(lv_ref_2) = ls_zlt_trama_retor-nro_transporte.
              vl_tor_id = lv_ref_2.
              condense: vl_tor_id.

              "update ZLT_ESTATUS
              free: lt_zlt_estatus,
                    wa_zlt_estatus.

              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = vl_tor_id
                importing
                  output = vl_tor_id.

              free: lt_zlt_estatus.
              select * into table lt_zlt_estatus
                from zlt_estatus
                where z_orden    = vl_tor_id
                  and z_ordenexp in rg_ordenexp.
              if sy-subrc = 0.
                if ls_error = 'X'.
                  loop at lt_zlt_estatus into wa_zlt_estatus .
                    wa_zlt_estatus-mandt         = sy-mandt.  " Mandante
                    wa_zlt_estatus-z_estatus     = '50'.      " 10 Fue contabilizado
                    wa_zlt_estatus-username	     = sy-uname.  "	Nombre de la persona que ha modif.documento de modificación
                    wa_zlt_estatus-udate         = sy-datum.  " Fecha de creación del documento de modificación
                    wa_zlt_estatus-utime         = sy-uzeit.  " Hora de modificación
                    wa_zlt_estatus-tcode         = sy-tcode.  " Transación en la que se efectuó una modificación
                    modify zlt_estatus from wa_zlt_estatus .

                    call function 'BAPI_TRANSACTION_COMMIT'
                      exporting
                        wait = 'X'.
                  endloop.
                else.
                  loop at lt_zlt_estatus into wa_zlt_estatus .
                    wa_zlt_estatus-mandt         = sy-mandt.  " Mandante
                    wa_zlt_estatus-z_estatus     = '10'.      " 10 Fue contabilizado
                    wa_zlt_estatus-username	     = sy-uname.  "	Nombre de la persona que ha modif.documento de modificación
                    wa_zlt_estatus-udate         = sy-datum.  " Fecha de creación del documento de modificación
                    wa_zlt_estatus-utime         = sy-uzeit.  " Hora de modificación
                    wa_zlt_estatus-tcode         = sy-tcode.  " Transación en la que se efectuó una modificación
                    modify zlt_estatus from wa_zlt_estatus .

                    call function 'BAPI_TRANSACTION_COMMIT'
                      exporting
                        wait = 'X'.
                  endloop.
                endif.

              endif.
            endif.
          endif.
        endif.
*      ENDIF.

        free: gtd_ztsd_stock ,
              wa_ztsd_stock,
              ls_zlt_trama_retor,
              lv_almacen,
              lt_zlt_estatus.

      endloop.
    endif.

    "Limpiando los datos procesados.
    free:
        w_header_data,
        w_header_control,
        v_deliv_numb,
        w_dlvcontrol,
        t_item_data,
        t_item_control,
        t_return,
        t_item_data_spl,
        t_cwm_item_data,
        w_vbkok_wa,
        l_delivery,
        ls_error,
        t_prot,
        t_vbpok_tab,
        vl_vbeln_f,
        lr_posnr,
        vl_matnr,

        vl_charg,
        vl_umvkz,
        vl_umvkn,
        vl_cwqrel_con_12,
        lr_matnr_m,
        lt_lips_2,
        gtd_ztsd_stock,
        ls_zlt_trama_retor,
        lv_almacen,
        lv_ref_2,
        lr_posnr1,
        lr_posnr3,
        vl_vbeln_f2,
        vl_entre_sc,
        r_linext,
        ltd_return_eli  ,
        lwa_h_data_eli  ,
        lwa_h_ctrl_eli  ,
        lt_item_data_eli,
        lt_item_control_eli,
        wa_item_data_eli ,
        wa_item_control_eli,
        vl_matnr_eli,
        lr_deliv_item,
        lr_posnr1,
        lr_posnr3,
        lr_posnr,
        lr_posnr_de,
        lr_matnr_m,
        lt_lips_2.



  endloop.



endfunction.
