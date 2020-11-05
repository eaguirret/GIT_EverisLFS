METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.
      ************************************************************************
      *      H I S T O R I A L    D E    M O D I F I C A C I O N E S         *
      ************************************************************************
      *^ Fecha: 03.08.2020    Autor: Cristian Castro                         *
      *^ Marca: @0CCC                                                        *
      *^ Descripción del cambio: Validaciones para fechas                    *
      *^ Código Requerimiento  : CASAPALC-189                                *
      ************************************************************************
      
      **TRY.
      *CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
      *  EXPORTING
      **    iv_entity_name          =
      **    iv_entity_set_name      =
      **    iv_source_name          =
      *    IO_DATA_PROVIDER        =
      **    it_key_tab              =
      **    it_navigation_path      =
      *    IO_EXPAND               =
      **    io_tech_request_context =
      **  IMPORTING
      **    er_deep_entity          =
      *    .
      ** CATCH /iwbep/cx_mgw_busi_exception .
      ** CATCH /iwbep/cx_mgw_tech_exception .
      **ENDTRY.
      
          TYPES: medicamentos TYPE STANDARD TABLE OF zcl_zscp_mm_posta_medi_mpc=>ts_diagnosticomedicamentos WITH DEFAULT KEY,
                 BEGIN OF lty_regdiagnostico.
              INCLUDE TYPE zcl_zscp_mm_posta_medi_mpc=>ts_diagnosticomedico.
          TYPES: medicamentos TYPE medicamentos,
                 END OF lty_regdiagnostico.
      
          DATA: lwa_regmedico    TYPE lty_regdiagnostico,
                lwa_diagnostico  TYPE ztpostamed_cab,
                ldt_medicamentos TYPE STANDARD TABLE OF ztpostamed_pos.
      
          DATA: lwa_goodsmvt_header  TYPE bapi2017_gm_head_01,
                ldt_goodsmvt_item    TYPE isu_bapi2017_gm_item_create_t,
                lwa_goodsmvt_headret TYPE bapi2017_gm_head_ret,
                ldt_return           TYPE bapiret2_t,
                lwa_return           TYPE bapireturn1.
      
          DATA:lwa_po_header          TYPE  bapiekkoc,
               ldt_po_items           TYPE  bapiekpoc_tp,
               ldt_po_items_schedules TYPE  bapieket_tp,
               ldt_bapireturn         TYPE fmfg_t_bapireturn,
               lb_flag                TYPE abap_bool.
      
          DATA: ldt_order_items_in      TYPE cmp_t_sditm,
                ldt_order_partners      TYPE cmp_t_parnr,
                ldt_order_schedules_in  TYPE oij_bapischdl_t,
                ls_bu                   TYPE kunnr,
                ls_dummy                TYPE string,
                ldt_material            TYPE zcl_zscp_mm_posta_medi_mpc=>tt_material,
                ldt_order_schedules_inx TYPE oij_bapischdlx_t,
                ldt_bapiret1            TYPE bapiret1_t,
                ldt_billing_data_in     TYPE bapivbrk_t,
                ldt_tab_bapiret         TYPE bapiret1_t,
                ldt_bapiekkn            TYPE bapiekkn_tp.
      
          DATA: lwa_poheader          TYPE bapimepoheader,
                lwa_poheaderx         TYPE bapimepoheaderx,
                ldt_bapimepoitem      TYPE bapimepoitem_tp,
                ldt_bapimepoitemx     TYPE bapimepoitemx_tp,
                ldt_bapimeposchedule  TYPE bapimeposchedule_tp,
                ldt_bapimeposchedulex TYPE bapimeposchedulx_tp,
                ldt_bapimepoaccount   TYPE bapimepoaccount_tp,
                ldt_bapimepoaccountx  TYPE bapimepoaccountx_tp.
      
          DATA: lo_message_container TYPE REF TO /iwbep/if_message_container.
      
          lo_message_container = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
      
          DATA: lwa_paciente   TYPE zcl_zscp_mm_posta_medi_mpc=>ts_paciente.
      
          DATA(lv_entity_set_name) = io_tech_request_context->get_entity_set_name( ).
      
      *   {+@0CCC
          DATA: lv_month    TYPE string,
                lv_year     TYPE string,
                lv_fecha    TYPE string,
                lv_xruem(1) TYPE c,
                lv_xruev(1) TYPE c,
                lv_date     TYPE dats,
                lv_ini      TYPE dats,
                lv_fin      TYPE dats,
                lv_datbp    TYPE dats,
                lv_datbp2   TYPE dats.
          CLEAR: lv_month, lv_year, lv_fecha, lv_xruem, lv_xruev,
                 lv_date, lv_ini, lv_fin.
      *   }+@0CCC
      
          CASE lv_entity_set_name.
            WHEN 'DiagnosticoMedicoSet'.
              io_data_provider->read_entry_data( IMPORTING es_data = lwa_regmedico ).
              lwa_regmedico-fecha_diagnostico = sy-datum.
              MOVE-CORRESPONDING lwa_regmedico TO lwa_diagnostico.
              LOOP AT lwa_regmedico-medicamentos ASSIGNING FIELD-SYMBOL(<fs_aux>).
                <fs_aux>-nposdiagnostico = sy-tabix * 10.
              ENDLOOP.
              MOVE-CORRESPONDING lwa_regmedico-medicamentos[] TO ldt_medicamentos.
      
              zcl_mm_posta_medica=>obtener_paciente( EXPORTING im_dni = CONV #( lwa_regmedico-dni )
                                                      CHANGING c_paciente = lwa_paciente  ).
      *   {+@0CCC
              SELECT SINGLE lfgja lfmon xruem xruev
                INTO (lv_year, lv_month, lv_xruem, lv_xruev)
                FROM marv WHERE bukrs EQ lwa_paciente-sociedad.
      
              IF sy-subrc EQ 0.
                CONCATENATE lv_year lv_month '01' INTO lv_fecha.
                MOVE lv_fecha TO lv_date.
                CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
                  EXPORTING
                    iv_date             = lv_date
                  IMPORTING
                    ev_month_begin_date = lv_ini
                    ev_month_end_date   = lv_fin.
      
                IF sy-datum EQ lv_ini.
                  lv_datbp  = sy-datum.
                  lv_datbp2 = sy-datum.
                ELSEIF sy-datum EQ lv_fin.
                  lv_datbp  = sy-datum.
                  lv_datbp2 = sy-datum.
                ELSEIF sy-datum GT lv_ini AND sy-datum LT lv_fin.
                  lv_datbp  = sy-datum.
                  lv_datbp2 = sy-datum.
                ELSEIF sy-datum LT lv_ini. "Periodo actual >  Fecha de sistema
                  IF lv_xruem EQ 'X'.
                    lv_datbp  = sy-datum.
                    lv_datbp2 = sy-datum.
                  ELSEIF lv_xruev EQ 'X'.
                    lv_datbp  = lv_ini.
                    lv_datbp2 = sy-datum.
                  ELSEIF lv_xruem IS INITIAL AND lv_xruev IS INITIAL.
                    lv_datbp  = lv_ini.
                    lv_datbp2 = sy-datum.
                  ENDIF.
                ELSEIF sy-datum GT lv_fin. "Periodo actual < Fecha de sistema
                  lv_datbp  = sy-datum.
                  lv_datbp2 = sy-datum.
                ENDIF.
              ENDIF.
      *   }+@0CCC
      *       CONSTANTES
              SELECT campo,valor1
              INTO TABLE @DATA(ldt_constantes)
              FROM ztconstantes
              WHERE modulo = 'MM'
                AND aplicacion = 'POSTA_MEDICA'
                AND programa = 'SCP'
                AND bukrs = @lwa_paciente-sociedad.
      
              CASE lwa_paciente-sociedad.
                WHEN '1000'.
      
                  LOOP AT ldt_constantes ASSIGNING FIELD-SYMBOL(<fs_constantes>).
                    CASE <fs_constantes>-campo.
                      WHEN 'BWART'.
                        DATA(ls_bwart) = <fs_constantes>-valor1.
                      WHEN 'GM_CODE'.
                        DATA(ls_gm_code) = <fs_constantes>-valor1.
                      WHEN 'KOSTL'.
                        DATA(ls_kostl) = <fs_constantes>-valor1.
                      WHEN 'WEVER'.
                        DATA(ls_wever) = <fs_constantes>-valor1.
                      WHEN OTHERS.
                        CONTINUE.
                    ENDCASE.
                  ENDLOOP.
      
                  SELECT matnr,charg
                  INTO TABLE @DATA(ldt_mchb)
                  FROM mchb FOR ALL ENTRIES IN @ldt_medicamentos
                  WHERE matnr = @ldt_medicamentos-material
                    AND werks = @ldt_medicamentos-centro
                    AND lgort = @ldt_medicamentos-almacen.
      
      *            lwa_goodsmvt_header-pstng_date = sy-datum. "-@0CCC
      *            lwa_goodsmvt_header-doc_date = sy-datum.   "-@0CCC
                  lwa_goodsmvt_header-pstng_date = lv_datbp. "+@0CCC
                  lwa_goodsmvt_header-doc_date = lv_datbp2.   "+@0CCC
                  lwa_goodsmvt_header-ver_gr_gi_slip = CONV #( ls_wever ).
      *           Documento salida de mercancias
                  LOOP AT ldt_medicamentos ASSIGNING FIELD-SYMBOL(<fs_medi>).
                    READ TABLE ldt_mchb ASSIGNING FIELD-SYMBOL(<fs_mchb>)
                    WITH KEY matnr =   <fs_medi>-material.
                    IF sy-subrc EQ 0.
                      DATA(ls_charg) = <fs_mchb>-charg.
                    ELSE.
                      CLEAR ls_charg.
                    ENDIF.
                    APPEND VALUE #( material = <fs_medi>-material
                                    plant    = <fs_medi>-centro
                                    stge_loc = <fs_medi>-almacen
                                    batch = ls_charg
                                    move_type = CONV #( ls_bwart )
                                    entry_qnt = <fs_medi>-cantidad
                                    entry_uom = <fs_medi>-um
                                    costcenter = CONV #( ls_kostl ) ) TO ldt_goodsmvt_item.
                  ENDLOOP.
      
                  zcl_mm_posta_medica=>bapi_goodsmvt_create( EXPORTING i_goodsmvt_header = lwa_goodsmvt_header
                                                              i_goodsmvt_code = CONV #( ls_gm_code )
                                                    IMPORTING e_goodsmvt_headret = lwa_goodsmvt_headret
                                                    CHANGING  c_goodsmvt_item = ldt_goodsmvt_item
                                                              c_return = ldt_return ).
      
                  IF lwa_goodsmvt_headret-mat_doc IS NOT INITIAL.
                    lwa_diagnostico-ndocgenerado = lwa_goodsmvt_headret-mat_doc.
                  ELSE.
                    LOOP AT ldt_return ASSIGNING FIELD-SYMBOL(<fs_return>).
                      CHECK <fs_return>-type = 'E'.
                      lo_message_container->add_message( EXPORTING iv_msg_type = /iwbep/cl_cos_logger=>error
                                                                     iv_msg_id  = <fs_return>-id
                                                                     iv_msg_number = <fs_return>-number
                                                                     iv_msg_text = <fs_return>-message
                                                                     iv_msg_v1 = <fs_return>-message_v1
                                                                     iv_msg_v2 = <fs_return>-message_v2
                                                                     iv_msg_v3 = <fs_return>-message_v3
                                                                     iv_msg_v4 = <fs_return>-message_v4
                                                                     iv_add_to_response_header = abap_true ).
                    ENDLOOP.
                  ENDIF.
                WHEN '2000'.
      
                  LOOP AT ldt_constantes ASSIGNING <fs_constantes>.
                    CASE <fs_constantes>-campo.
                      WHEN 'ESART'.
                        DATA(ls_esart) = <fs_constantes>-valor1.
                      WHEN 'EBSTYP'.
                        DATA(ls_ebstyp) = <fs_constantes>-valor1.
                      WHEN 'BUKRS'.
                        DATA(ls_bukrs) = <fs_constantes>-valor1.
                      WHEN 'EKORG'.
                        DATA(ls_ekorg) = <fs_constantes>-valor1.
                      WHEN 'BKGRP'.
                        DATA(ls_bkgrp) = <fs_constantes>-valor1.
                      WHEN 'ELIFN'.
                        DATA(ls_elifn) = <fs_constantes>-valor1.
                      WHEN 'RESWK'.
                        DATA(ls_reswk) = <fs_constantes>-valor1.
                      WHEN 'PLANT'.
                        DATA(ls_plant) = <fs_constantes>-valor1.
                      WHEN 'STGE_LOC'.
                        DATA(ls_stge_loc) = <fs_constantes>-valor1.
                      WHEN 'BWART1'.
                        DATA(ls_bwart1) = <fs_constantes>-valor1.
                      WHEN 'WEVER'.
                        ls_wever = <fs_constantes>-valor1.
                      WHEN 'KZBEW'.
                        DATA(ls_kzbew) = <fs_constantes>-valor1.
                      WHEN 'GM_CODE1'.
                        DATA(ls_gm_code1) = <fs_constantes>-valor1.
                      WHEN 'BWART2'.
                        DATA(ls_bwart2) = <fs_constantes>-valor1.
                      WHEN 'KOSTL'.
                        ls_kostl = <fs_constantes>-valor1.
                      WHEN 'GM_CODE2'.
                        DATA(ls_gm_code2) = <fs_constantes>-valor1.
                      WHEN 'FKARA'.
                        DATA(ls_fkara) = <fs_constantes>-valor1.
                      WHEN 'SPART'.
                        DATA(ls_spart) = <fs_constantes>-valor1.
                      WHEN 'VKORG'.
                        DATA(ls_vkorg) = <fs_constantes>-valor1.
                      WHEN 'VTWEG'.
                        DATA(ls_vtweg) = <fs_constantes>-valor1.
                      WHEN 'VBTYP_V'.
                        DATA(ls_vbtyp_v) = <fs_constantes>-valor1.
                      WHEN 'KNTTP'.
                        DATA(ls_knttp) = <fs_constantes>-valor1.
                      WHEN 'REF_DOC'.
                        DATA(ls_ref_doc) = <fs_constantes>-valor1.
                      WHEN OTHERS.
                        CONTINUE.
                    ENDCASE.
                  ENDLOOP.
      
                  DATA(ls_vendedora) = CONV bukrs( | { ls_elifn ALPHA = OUT } | ).
      
                  SELECT valor1 UP TO 1 ROWS
                  INTO @DATA(ls_kostl_vendedora)
                  FROM ztconstantes
                  WHERE modulo = 'MM'
                    AND aplicacion = 'POSTA_MEDICA'
                    AND programa = 'SCP'
                    AND bukrs = @ls_vendedora.
                  ENDSELECT.
      
                  lb_flag = abap_true.
      *            lwa_po_header-doc_type = ls_esart.
      *            lwa_po_header-doc_cat = ls_ebstyp.
      *            lwa_po_header-co_code = ls_bukrs.
      *            lwa_po_header-purch_org = ls_ekorg.
      *            lwa_po_header-pur_group = ls_bkgrp.
      *            lwa_po_header-vendor = ls_elifn.
      *            lwa_po_header-suppl_plnt = ls_reswk.
      *            lwa_po_header-created_by = sy-uname.
      *            lwa_po_header-langu = sy-langu.
      
                  lwa_poheader-comp_code = ls_bukrs.
                  lwa_poheader-doc_type = ls_esart.
                  lwa_poheader-vendor = ls_elifn.
                  lwa_poheader-purch_org = ls_ekorg.
                  lwa_poheader-pur_group = ls_bkgrp.
                  lwa_poheader-ref_1 = ls_plant.
      
                  lwa_poheaderx-comp_code = 'X'.
                  lwa_poheaderx-doc_type = 'X'.
                  lwa_poheaderx-vendor = 'X'.
                  lwa_poheaderx-purch_org = 'X'.
                  lwa_poheaderx-pur_group = 'X'.
                  lwa_poheaderx-ref_1 = 'X'.
      
                  SELECT matnr,bwtar
                  INTO TABLE @DATA(ldt_mbew)
                  FROM mbew FOR ALL ENTRIES IN @ldt_medicamentos
                  WHERE matnr = @ldt_medicamentos-material
                    AND bwkey = @ls_plant
                    AND bwtar NE ''.
      
                  SELECT matnr,charg
                  INTO TABLE @ldt_mchb
                  FROM mchb FOR ALL ENTRIES IN @ldt_medicamentos
                  WHERE matnr = @ldt_medicamentos-material
                    AND werks = @ldt_medicamentos-centro
                    AND lgort = @ldt_medicamentos-almacen.
      
                  LOOP AT lwa_regmedico-medicamentos ASSIGNING FIELD-SYMBOL(<fs_medicamento>).
                    DATA(li_tabix) = sy-tabix.
                    DATA(ls_lgort) = CONV lgort_d( <fs_medicamento>-almacen ).
                    READ TABLE ldt_mbew ASSIGNING FIELD-SYMBOL(<fs_mbew>)
                    WITH KEY matnr = <fs_medicamento>-material.
                    IF sy-subrc EQ 0.
                      DATA(ls_bwtar) = <fs_mbew>-bwtar.
                    ELSE.
                      CLEAR ls_bwtar.
                    ENDIF.
                    READ TABLE ldt_mchb ASSIGNING <fs_mchb>
                    WITH KEY matnr = <fs_medicamento>-material.
                    IF sy-subrc EQ 0.
                      ls_charg = <fs_mchb>-charg.
                    ELSE.
                      CLEAR ls_charg.
                    ENDIF.
      
      
                    APPEND VALUE #( po_item = <fs_medicamento>-nposdiagnostico
                                    material = <fs_medicamento>-material
                                    plant = ls_plant
                                    stge_loc = ls_stge_loc
                                    quantity = <fs_medicamento>-cantidad
                                    po_unit = <fs_medicamento>-um
                                    orderpr_un = <fs_medicamento>-um
      *                              acctasscat = ls_knttp  "JDD1709
                                    ) TO ldt_bapimepoitem.
      
                    APPEND VALUE #( po_item = <fs_medicamento>-nposdiagnostico
                                    material = 'X'
                                    plant = 'X'
                                    stge_loc = 'X'
                                    quantity = 'X'
                                    po_unit = 'X'
                                    orderpr_un = 'X'
      *                              acctasscat = 'X' "JDD1709
                                    ) TO ldt_bapimepoitemx.
      
                    APPEND VALUE #( po_item = <fs_medicamento>-nposdiagnostico
                                    sched_line = li_tabix
                                    quantity = <fs_medicamento>-cantidad ) TO ldt_bapimeposchedule.
      
                    APPEND VALUE #( po_item = <fs_medicamento>-nposdiagnostico
                                    sched_line = li_tabix
                                    po_itemx = 'X'
                                    quantity = 'X' ) TO ldt_bapimeposchedulex.
      * JDD1709 - Se elimina por no ser un pedido imputado - Inicio
      *              APPEND VALUE #( po_item = <fs_medicamento>-nposdiagnostico
      *                              serial_no = '01'
      *                              costcenter = ls_kostl ) TO ldt_bapimepoaccount.
      *
      *              APPEND VALUE #( po_item = <fs_medicamento>-nposdiagnostico
      *                              serial_no = '01'
      *                              po_itemx = 'X'
      *                              serial_nox = 'X'
      *                              costcenter = 'X'
      *
      *                               ) TO ldt_bapimepoaccountx.
      * JDD1709 - Se elimina por no ser un pedido imputado - Fin
      ***              APPEND VALUE #( po_item = <fs_medicamento>-nposdiagnostico
      ***                              material = <fs_medicamento>-material
      ***                              pur_mat = <fs_medicamento>-material
      ***                              plant = ls_plant
      ***                              val_type = ls_bwtar
      ***                              price_unit = 1
      ***                              conv_num1 = 1
      ***                              acctasscat = ls_knttp ) TO ldt_po_items.
      ***
      ***              APPEND VALUE #( po_item = <fs_medicamento>-nposdiagnostico
      ***                              serial_no = li_tabix
      ***                              quantity = <fs_medicamento>-cantidad
      ***                              cost_ctr = ls_kostl ) TO ldt_bapiekkn.
      ***
      ***              APPEND VALUE #( po_item = <fs_medicamento>-nposdiagnostico
      ***                              serial_no = li_tabix
      ***                              deliv_date = sy-datum
      ***                              batch = ls_charg
      ***                              quantity = <fs_medicamento>-cantidad ) TO ldt_po_items_schedules.
                  ENDLOOP.
      *           Documento pedido de compra
                  zcl_mm_posta_medica=>bapi_po_create1( EXPORTING i_poheader = lwa_poheader
                                                                  i_poheaderx = lwa_poheaderx
                                                         IMPORTING e_exppurchaseorder = DATA(ls_purchaseorder)
                                                         CHANGING c_poitem = ldt_bapimepoitem
                                                                  c_poitemx = ldt_bapimepoitemx
                                                                  c_poschedule = ldt_bapimeposchedule
                                                                  c_poschedulex = ldt_bapimeposchedulex
                                                                  c_poaccount = ldt_bapimepoaccount "JDD1709
                                                                  c_poaccountx = ldt_bapimepoaccountx "JDD1709
                                                                  c_return = ldt_return ).
      *            zcl_mm_posta_medica=>intercompany( EXPORTING i_po_header = lwa_po_header
      *                                               IMPORTING e_purchaseorder = DATA(ls_purchaseorder)
      *                                               CHANGING c_po_items = ldt_po_items
      *                                                        c_po_item_account_assignment = ldt_bapiekkn
      *                                                        c_po_items_schedules = ldt_po_items_schedules
      *                                                        c_return = ldt_bapireturn ).
      
                  WAIT UP TO 1 SECONDS.
                  IF ls_purchaseorder IS NOT INITIAL.
      
                    CLEAR ldt_return.
      
                    lwa_diagnostico-ndocgenerado = ls_purchaseorder.
      *             Documento entrada de mercancias Sociedad compradora
      *              lwa_goodsmvt_header-pstng_date = sy-datum. "-@0CCC
      *              lwa_goodsmvt_header-doc_date = sy-datum.   "-@0CCC
                    lwa_goodsmvt_header-pstng_date = lv_datbp. "+@0CCC
                    lwa_goodsmvt_header-doc_date = lv_datbp2.   "+@0CCC
                    lwa_goodsmvt_header-ver_gr_gi_slip = ls_wever.
      
                    SELECT ebeln,bedat UP TO 1 ROWS
                    INTO @DATA(ls_ekko)
                    FROM ekko
                    WHERE ebeln = @ls_purchaseorder.
                    ENDSELECT.
                    IF sy-subrc EQ 0.
                      lwa_goodsmvt_header-ref_doc_no = |{ ls_ref_doc && ls_ekko-bedat+4(2) && '.' && ls_ekko-bedat(4) }|.
                    ENDIF.
      
                    LOOP AT lwa_regmedico-medicamentos ASSIGNING <fs_medicamento>.
                      READ TABLE ldt_mbew ASSIGNING <fs_mbew>
                      WITH KEY matnr = <fs_medicamento>-material.
                      IF sy-subrc EQ 0.
                        ls_bwtar = <fs_mbew>-bwtar.
                      ELSE.
                        CLEAR ls_bwtar.
                      ENDIF.
                      READ TABLE ldt_mchb ASSIGNING <fs_mchb>
                      WITH KEY matnr = <fs_medicamento>-material.
                      IF sy-subrc EQ 0.
                        ls_charg = <fs_mchb>-charg.
                      ELSE.
                        CLEAR ls_charg.
                      ENDIF.
                      APPEND INITIAL LINE TO ldt_goodsmvt_item ASSIGNING FIELD-SYMBOL(<fs_goodsmvt>).
                      <fs_goodsmvt>-material = <fs_medicamento>-material.
                      <fs_goodsmvt>-plant    = ls_plant.
                      <fs_goodsmvt>-batch = ls_charg.
                      <fs_goodsmvt>-stge_loc = ls_stge_loc.
                      <fs_goodsmvt>-move_type = ls_bwart1.
                      <fs_goodsmvt>-vendor = ls_elifn.
                      <fs_goodsmvt>-val_type = ls_bwtar.
                      <fs_goodsmvt>-entry_qnt = <fs_medicamento>-cantidad.
                      <fs_goodsmvt>-entry_uom = <fs_medicamento>-um.
      *                <fs_goodsmvt>-po_pr_qnt = <fs_medicamento>-cantidad.
      *                <fs_goodsmvt>-orderpr_un = <fs_medicamento>-um.
                      <fs_goodsmvt>-po_number = ls_purchaseorder.
                      <fs_goodsmvt>-po_item = <fs_medicamento>-nposdiagnostico.
                      <fs_goodsmvt>-mvt_ind = ls_kzbew.
                    ENDLOOP.
      
                    zcl_mm_posta_medica=>bapi_goodsmvt_create( EXPORTING i_goodsmvt_header = lwa_goodsmvt_header
                                                             i_goodsmvt_code = CONV #( ls_gm_code1 )
                                                   IMPORTING e_goodsmvt_headret = lwa_goodsmvt_headret
                                                   CHANGING  c_goodsmvt_item = ldt_goodsmvt_item
                                                             c_return = ldt_return ).
                    IF lwa_goodsmvt_headret IS NOT INITIAL.
      *@cyt{
                      CLEAR: lwa_goodsmvt_headret.
                      REFRESH ldt_return.
      
                      lwa_goodsmvt_header-header_txt = ls_ekko-ebeln.
                      LOOP AT ldt_goodsmvt_item ASSIGNING <fs_goodsmvt>.
                        <fs_goodsmvt>-move_type = 'X01'.        "movimiento bwtar
                        <fs_goodsmvt>-no_more_gr = abap_true.   "flag consumo final
                        <fs_goodsmvt>-costcenter = ls_kostl.    "centro de costo
                        <fs_goodsmvt>-mvt_ind = ''.
                        <fs_goodsmvt>-po_pr_qnt = ''.
                        <fs_goodsmvt>-orderpr_un = ''.
                      ENDLOOP.
      
                      zcl_mm_posta_medica=>bapi_goodsmvt_create( EXPORTING i_goodsmvt_header = lwa_goodsmvt_header
                                                                            i_goodsmvt_code = CONV #( ls_gm_code2 )
                                                                  IMPORTING e_goodsmvt_headret = lwa_goodsmvt_headret
                                                                  CHANGING  c_goodsmvt_item = ldt_goodsmvt_item
                                                                            c_return = ldt_return ).
      
                      IF lwa_goodsmvt_headret IS NOT INITIAL.
      *@cyt}
                        DATA(lwa_goodsmvt_headret_101) = lwa_goodsmvt_headret. "no se usa luego
      *                 documento entrega
                        zcl_mm_posta_medica=>bapi_outb_delivery_create_sto( EXPORTING i_purchaseorder = ls_purchaseorder
                                                                            IMPORTING e_delivery = DATA(ls_delivery)
                                                                            CHANGING c_return = ldt_return ).
                        IF ls_delivery IS NOT INITIAL.
      *                 Contabilización de entrega
                          DATA(lb_error) = zcl_mm_posta_medica=>ws_delivery_update_2( EXPORTING i_delivery = ls_delivery
                                                                                                i_lgort = ls_lgort ).
                          IF lb_error IS INITIAL.
      ***                    CLEAR ldt_return.
      ***
      ***                    ldt_billing_data_in = VALUE #( ( salesorg = ls_vkorg
      ***                                                     distr_chan = ls_vtweg
      ***                                                     division = ls_spart
      ***                                                     ordbilltyp = ls_fkara
      ***                                                     sold_to = ls_elifn
      ***                                                     ref_doc = ls_delivery
      ***                                                     ref_doc_ca = ls_vbtyp_v ) ).
      ***
      ***                    zcl_mm_posta_medica=>bapi_billingdoc_createfromdata(  IMPORTING e_invoice = DATA(ls_invoice_sd)
      ***                                                                          CHANGING c_billing_data_in = ldt_billing_data_in
      ***                                                                                  c_return = ldt_tab_bapiret ).
      ***                    IF ls_invoice_sd IS NOT INITIAL.
      ****                 Documento Factura MM
      ***                      zcl_mm_posta_medica=>bapi_incominginvoice_create1( EXPORTING i_purchaseorder = ls_purchaseorder
      ***                                                                                   i_invoice = ls_invoice_sd
      ***                                                                                   i_goodsmvt = lwa_goodsmvt_headret_101
      ***                                                                         IMPORTING e_invoicedocnumber = DATA(ls_mminvoice)
      ***                                                                         CHANGING c_return = ldt_return ).
      ***                      IF ls_mminvoice IS INITIAL.
      ***                        LOOP AT ldt_return ASSIGNING <fs_return>.
      ***                          CHECK <fs_return>-type = 'E'.
      ***                          lo_message_container->add_message( EXPORTING iv_msg_type = /iwbep/cl_cos_logger=>error
      ***                                                                       iv_msg_id  = <fs_return>-id
      ***                                                                       iv_msg_number = <fs_return>-number
      ***                                                                       iv_msg_text = <fs_return>-message
      ***                                                                       iv_msg_v1 = <fs_return>-message_v1
      ***                                                                       iv_msg_v2 = <fs_return>-message_v2
      ***                                                                       iv_msg_v3 = <fs_return>-message_v3
      ***                                                                       iv_msg_v4 = <fs_return>-message_v4
      ***                                                                       iv_add_to_response_header = abap_true ).
      ***                        ENDLOOP.
      ***                      ENDIF.
      ***                    ELSE.
      ***                      LOOP AT ldt_tab_bapiret ASSIGNING FIELD-SYMBOL(<fs_bapiret>).
      ***                        CHECK <fs_bapiret>-type = 'E'.
      ***                        lo_message_container->add_message( EXPORTING iv_msg_type = /iwbep/cl_cos_logger=>error
      ***                                                                     iv_msg_id  = <fs_bapiret>-id
      ***                                                                     iv_msg_number = <fs_bapiret>-number
      ***                                                                     iv_msg_text = <fs_bapiret>-message
      ***                                                                     iv_msg_v1 = <fs_bapiret>-message_v1
      ***                                                                     iv_msg_v2 = <fs_bapiret>-message_v2
      ***                                                                     iv_msg_v3 = <fs_bapiret>-message_v3
      ***                                                                     iv_msg_v4 = <fs_bapiret>-message_v4
      ***                                                                     iv_add_to_response_header = abap_true ).
      ***                      ENDLOOP.
      ***                    ENDIF.
      
                          ELSE.
                            lo_message_container->add_message( EXPORTING iv_msg_type = /iwbep/cl_cos_logger=>error
                                                                         iv_msg_id  = sy-msgid
                                                                         iv_msg_number = sy-msgno
                                                                         iv_msg_v1 = sy-msgv1
                                                                         iv_msg_v2 = sy-msgv2
                                                                         iv_msg_v3 = sy-msgv3
                                                                         iv_msg_v4 = sy-msgv4
                                                                         iv_add_to_response_header = abap_true ).
                          ENDIF.
                        ELSE.
                          LOOP AT ldt_return ASSIGNING <fs_return>.
                            CHECK <fs_return>-type = 'E'.
                            lo_message_container->add_message( EXPORTING iv_msg_type = /iwbep/cl_cos_logger=>error
                                                                         iv_msg_id  = <fs_return>-id
                                                                         iv_msg_number = <fs_return>-number
                                                                         iv_msg_text = <fs_return>-message
                                                                         iv_msg_v1 = <fs_return>-message_v1
                                                                         iv_msg_v2 = <fs_return>-message_v2
                                                                         iv_msg_v3 = <fs_return>-message_v3
                                                                         iv_msg_v4 = <fs_return>-message_v4
                                                                         iv_add_to_response_header = abap_true ).
                          ENDLOOP.
                        ENDIF.
      *@cyt{
                      ELSE.
                        LOOP AT ldt_return ASSIGNING <fs_return>.
                          CHECK <fs_return>-type = 'E'.
                          lo_message_container->add_message( EXPORTING iv_msg_type = /iwbep/cl_cos_logger=>error
                                                                       iv_msg_id  = <fs_return>-id
                                                                       iv_msg_number = <fs_return>-number
                                                                       iv_msg_text = <fs_return>-message
                                                                       iv_msg_v1 = <fs_return>-message_v1
                                                                       iv_msg_v2 = <fs_return>-message_v2
                                                                       iv_msg_v3 = <fs_return>-message_v3
                                                                       iv_msg_v4 = <fs_return>-message_v4
                                                                       iv_add_to_response_header = abap_true ).
                        ENDLOOP.
                      ENDIF.
      *@cyt}
                    ELSE.
                      LOOP AT ldt_return ASSIGNING <fs_return>.
                        CHECK <fs_return>-type = 'E'.
                        lo_message_container->add_message( EXPORTING iv_msg_type = /iwbep/cl_cos_logger=>error
                                                                     iv_msg_id  = <fs_return>-id
                                                                     iv_msg_number = <fs_return>-number
                                                                     iv_msg_text = <fs_return>-message
                                                                     iv_msg_v1 = <fs_return>-message_v1
                                                                     iv_msg_v2 = <fs_return>-message_v2
                                                                     iv_msg_v3 = <fs_return>-message_v3
                                                                     iv_msg_v4 = <fs_return>-message_v4
                                                                     iv_add_to_response_header = abap_true ).
                      ENDLOOP.
                    ENDIF.
                  ELSE.
                    LOOP AT ldt_return ASSIGNING <fs_return>.
                      CHECK <fs_return>-type = 'E'.
                      lo_message_container->add_message( EXPORTING iv_msg_type = /iwbep/cl_cos_logger=>error
                                                                   iv_msg_id  = <fs_return>-id
                                                                   iv_msg_number = <fs_return>-number
                                                                   iv_msg_text = <fs_return>-message
                                                                   iv_msg_v1 = <fs_return>-message_v1
                                                                   iv_msg_v2 = <fs_return>-message_v2
                                                                   iv_msg_v3 = <fs_return>-message_v3
                                                                   iv_msg_v4 = <fs_return>-message_v4
                                                                   iv_add_to_response_header = abap_true ).
                    ENDLOOP.
      
      
      *              LOOP AT ldt_bapireturn ASSIGNING FIELD-SYMBOL(<fs_bapireturn>).
      *                CHECK <fs_bapireturn>-type = 'E'.
      *                lo_message_container->add_message( EXPORTING iv_msg_type = /iwbep/cl_cos_logger=>error
      *                                                               iv_msg_id  = CONV  #( <fs_bapireturn>-code(2) )
      *                                                               iv_msg_number = CONV  #( <fs_bapireturn>-code+2(3) )
      *                                                               iv_msg_text = <fs_bapireturn>-message
      *                                                               iv_msg_v1 = <fs_bapireturn>-message_v1
      *                                                               iv_msg_v2 = <fs_bapireturn>-message_v2
      *                                                               iv_msg_v3 = <fs_bapireturn>-message_v3
      *                                                               iv_msg_v4 = <fs_bapireturn>-message_v4
      *                                                               iv_add_to_response_header = abap_true ).
      *              ENDLOOP.
                  ENDIF.
      
                WHEN OTHERS.
      
                  LOOP AT ldt_constantes ASSIGNING <fs_constantes>.
                    CASE <fs_constantes>-campo.
                      WHEN 'AUART'.
                        DATA(ls_auart) = <fs_constantes>-valor1.
                      WHEN 'VKORG'.
                        ls_vkorg = <fs_constantes>-valor1.
                      WHEN 'VTWEG'.
                        ls_vtweg = <fs_constantes>-valor1.
                      WHEN 'SPART'.
                        ls_spart = <fs_constantes>-valor1.
                      WHEN 'VKBUR'.
                        DATA(ls_vkbur) = <fs_constantes>-valor1.
                      WHEN 'DZTERM'.
                        DATA(ls_dzterm) = <fs_constantes>-valor1.
                      WHEN 'DZLSCH'.
                        DATA(ls_dzlsch) = <fs_constantes>-valor1.
                      WHEN 'KTGRD'.
                        DATA(ls_ktgrd) = <fs_constantes>-valor1.
                      WHEN 'PARVW'.
                        DATA(ls_parvw) = <fs_constantes>-valor1.
                      WHEN 'PSTYV'.
                        DATA(ls_pstyv) = <fs_constantes>-valor1.
                      WHEN OTHERS.
                        CONTINUE.
                    ENDCASE.
                  ENDLOOP.
      
      
                  DATA(lwa_order_header_in) = VALUE bapisdhd1( refobjtype = sy-datum
                                                               doc_type = ls_auart
                                                               sales_org = ls_vkorg
                                                               distr_chan = ls_vtweg
                                                               division = ls_spart
                                                               sales_off = ls_vkbur
                                                               pmnttrms = ls_dzterm
                                                               purch_no_c = lwa_paciente-dni
                                                               pymt_meth = ls_dzlsch
                                                               accnt_asgn = ls_ktgrd ).
                  LOOP AT ldt_medicamentos ASSIGNING FIELD-SYMBOL(<fs_medicamentos>).
                    li_tabix = sy-tabix.
                    IF li_tabix = 1.
                      SPLIT lwa_paciente-nombre_empresa AT space INTO ls_bu ls_dummy.
                      APPEND VALUE #( partn_role = ls_parvw
                                      partn_numb = ls_bu ) TO ldt_order_partners.
      
                    ENDIF.
                    ls_lgort = CONV lgort_d( <fs_medicamentos>-almacen ).
                    CLEAR ldt_material.
                    zcl_mm_posta_medica=>obtener_material( EXPORTING i_matnr = <fs_medicamentos>-material
                                                           CHANGING c_materiales = ldt_material ).
                    READ TABLE ldt_material INTO DATA(lwa_material) INDEX 1.
      
                    APPEND VALUE #( itm_number = <fs_medicamentos>-nposdiagnostico
                                    plant =  lwa_material-centro
                                    store_loc = lwa_material-almacen
                                    material = <fs_medicamentos>-material
                                    target_qty = <fs_medicamentos>-cantidad
                                    item_categ = ls_pstyv
                                    pymt_meth = ls_dzlsch ) TO ldt_order_items_in.
      
                    APPEND VALUE #( itm_number = <fs_medicamentos>-nposdiagnostico
                                    sched_line = li_tabix
                                    req_date = sy-datum
                                    req_qty = <fs_medicamentos>-cantidad
                                    dlv_date = sy-datum ) TO ldt_order_schedules_in.
                    APPEND VALUE #( itm_number = <fs_medicamentos>-nposdiagnostico
                                    sched_line = 'X'
                                    updateflag = 'I'
                                    req_date = 'X'
                                    req_qty = 'X'
                                    dlv_date = 'X' ) TO ldt_order_schedules_inx.
                    ls_lgort = lwa_material-almacen.
                  ENDLOOP.
      *           Docuemto Pedido de ventas
                  zcl_mm_posta_medica=>bapi_salesorder_createfromdat2( EXPORTING i_order_header_in = lwa_order_header_in
                                                                       IMPORTING e_salesdocument = DATA(ls_salesdocument)
                                                                       CHANGING c_order_items_in = ldt_order_items_in
                                                                                c_order_partners = ldt_order_partners
                                                                                c_order_schedules_in = ldt_order_schedules_in
                                                                                c_order_schedules_inx = ldt_order_schedules_inx
                                                                                c_return = ldt_return ).
      
                  IF ls_salesdocument IS NOT INITIAL.
                    lwa_diagnostico-ndocgenerado = ls_salesdocument.
                    SELECT vbelv,vbeln,vbtyp_n UP TO 1 ROWS
                    INTO @DATA(lwa_entega)
                    FROM vbfa
                    WHERE vbelv = @ls_salesdocument
                      AND vbtyp_n = 'J'.
                    ENDSELECT.
      *             contabilizacion de entrega
                    lb_error = zcl_mm_posta_medica=>ws_delivery_update_2( EXPORTING i_lgort = ls_lgort i_delivery = lwa_entega-vbeln ).
                    IF lb_error IS NOT INITIAL.
      
                      lo_message_container->add_message( EXPORTING iv_msg_type = /iwbep/cl_cos_logger=>error
                                                                        iv_msg_id  = sy-msgid
                                                                        iv_msg_number = sy-msgno
                                                                        iv_msg_v1 = sy-msgv1
                                                                        iv_msg_v2 = sy-msgv2
                                                                        iv_msg_v3 = sy-msgv3
                                                                        iv_msg_v4 = sy-msgv4
                                                                        iv_add_to_response_header = abap_true ).
                    ENDIF.
      
      *****             Documento Factura SD
      ****              zcl_mm_posta_medica=>bapi_billingdoc_createmultiple( EXPORTING i_salesdocument = ls_salesdocument
      ****                                                                             i_lgort = ls_lgort
      ****                                                                   IMPORTING e_invoice = DATA(ls_invoice)
      ****                                                                   CHANGING c_return = ldt_bapiret1 ).
      ****              IF ls_invoice IS INITIAL.
      ****                LOOP AT ldt_bapiret1 ASSIGNING FIELD-SYMBOL(<fs_bapuret1>).
      ****                  CHECK <fs_bapuret1>-type = 'E'.
      ****                  lo_message_container->add_message( EXPORTING iv_msg_type = /iwbep/cl_cos_logger=>error
      ****                                                               iv_msg_id  = <fs_bapuret1>-id
      ****                                                               iv_msg_number = <fs_bapuret1>-number
      ****                                                               iv_msg_text = <fs_bapuret1>-message
      ****                                                               iv_msg_v1 = <fs_bapuret1>-message_v1
      ****                                                               iv_msg_v2 = <fs_bapuret1>-message_v2
      ****                                                               iv_msg_v3 = <fs_bapuret1>-message_v3
      ****                                                               iv_msg_v4 = <fs_bapuret1>-message_v4
      ****                                                               iv_add_to_response_header = abap_true ).
      ****                ENDLOOP.
      ****              ENDIF.
      
                  ELSE.
                    LOOP AT ldt_return ASSIGNING <fs_return>.
                      CHECK <fs_return>-type = 'E'.
                      lo_message_container->add_message( EXPORTING iv_msg_type = /iwbep/cl_cos_logger=>error
                                                                   iv_msg_id  = <fs_return>-id
                                                                   iv_msg_number = <fs_return>-number
                                                                   iv_msg_text = <fs_return>-message
                                                                   iv_msg_v1 = <fs_return>-message_v1
                                                                   iv_msg_v2 = <fs_return>-message_v2
                                                                   iv_msg_v3 = <fs_return>-message_v3
                                                                   iv_msg_v4 = <fs_return>-message_v4
                                                                   iv_add_to_response_header = abap_true ).
                    ENDLOOP.
                  ENDIF.
      
              ENDCASE.
      
              DATA(ldt_error) = lo_message_container->get_messages( ).
      
              IF ldt_error IS INITIAL AND lwa_paciente-tipo = 'E'.
                lwa_regmedico-ndocgenerado = lwa_diagnostico-ndocgenerado.
      *         Infotipo 9904
                lwa_return = zcl_mm_posta_medica=>hr_infotype_operation( EXPORTING i_paciente = lwa_paciente
                                                                                   i_regmedico = lwa_regmedico
                                                                                   i_intercompany = lb_flag ).
                IF lwa_return IS NOT INITIAL AND lwa_return-type = 'E'.
                  lo_message_container->add_message( EXPORTING iv_msg_type = /iwbep/cl_cos_logger=>error
                                                               iv_msg_id  = lwa_return-id
                                                               iv_msg_number = lwa_return-number
                                                               iv_msg_text = lwa_return-message
                                                               iv_msg_v1 = lwa_return-message_v1
                                                               iv_msg_v2 = lwa_return-message_v2
                                                               iv_msg_v3 = lwa_return-message_v3
                                                               iv_msg_v4 = lwa_return-message_v4
                                                               iv_add_to_response_header = abap_true ).
                ENDIF.
              ENDIF.
              ldt_error = lo_message_container->get_messages( ).
              IF ldt_error IS NOT INITIAL.
                copy_data_to_ref( EXPORTING is_data = lwa_regmedico
                              CHANGING  cr_data = er_deep_entity ).
              ELSEIF lwa_diagnostico-ndocgenerado IS NOT INITIAL.
      
                lwa_diagnostico-ndocdiagnostico = zcl_mm_posta_medica=>number_get_next( EXPORTING i_nr_range_nr = '01'
                                                                                                      i_object = 'ZREGMEDICO' ).
      
                LOOP AT ldt_medicamentos ASSIGNING <fs_medicamentos>.
                  <fs_medicamentos>-ndocdiagnostico = lwa_diagnostico-ndocdiagnostico.
                ENDLOOP.
                MOVE-CORRESPONDING ldt_medicamentos TO lwa_regmedico-medicamentos[].
                MOVE-CORRESPONDING lwa_diagnostico TO lwa_regmedico.
                MODIFY ztpostamed_cab FROM lwa_diagnostico.
                IF sy-subrc EQ 0.
                  MODIFY ztpostamed_pos FROM TABLE ldt_medicamentos.
                  IF sy-subrc EQ 0.
                    copy_data_to_ref( EXPORTING is_data = lwa_regmedico
                              CHANGING  cr_data = er_deep_entity ).
                  ENDIF.
                ENDIF.
              ENDIF.
            WHEN OTHERS.
              RETURN.
          ENDCASE.
      
        ENDMETHOD.