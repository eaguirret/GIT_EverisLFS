*&---------------------------------------------------------------------*
*&  Include           ZMM_CREATE_PEDIDO_F01.
*&---------------------------------------------------------------------*
*& MODIFICATION HISTORY                                                *
*&  NUM.  ID                 AUTHOR             DATE                   *
*& -----  ---------------  -----------------   --------------          *
*&  0001  T10000183972      EXT90860 (LLCB)     28/04/2014             *
*& DESCRIPTION:  Se modifica la subrutina FILL_POITEM para incluir el  *
*& campo PRICE_UNIT de la estructura ls_poitem con el valor del campo  *
*& PEINH de la solicitud de pedido.                                    *
*&---------------------------------------------------------------------*
*& NUM.  ID                 AUTHOR             DATE                    *
*& -----  ---------------  -----------------   --------------          *
*&  0002  T10000198576      EXT90860 (LLCB)     14/07/2014             *
*& DESCRIPTION:  Se crea la subrutina DELTE_POITEM para evitar que el  *
*& programa procese las posiciones de las solicitudes de pedido con    *
*& destinatario de mercancias SRM (EBKN-WEMPF).                        *
*&---------------------------------------------------------------------*
*& NUM.  ID                 AUTHOR             DATE                    *
*& -----  ---------------  -----------------   --------------          *
*&  0003  T10000203015      EXT90860 (LLCB)     01/09/2014             *
*& DESCRIPTION:  Discriminar solicitudes que tengan Ind. conclusión    *
*& informado (EBAN-EBAKZ) o bien que la cantidad pedida es mayor o     *
*& igual que la cantidad de la solicitud (EBAN-BSMNG => EBAN-MENGE).   *
*& La cantidad de la posición del pedido a crear, debe de ser la       *
*& diferencia entre EBAN-MENGE y EBAN-BSMNG.                           *
*&---------------------------------------------------------------------*
*& NUM.  ID                 AUTHOR             DATE                    *
*& -----  ---------------  -----------------   --------------          *
*& 0004   T10000220054      EXT90884 (MAU)     07/11/2014              *
*& DESCRIPTION: Generar pedido con grupo de compras correcto.          *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& NUM.  ID                 AUTHOR             DATE                    *
*& -----  ---------------  -----------------   --------------          *
*& 0005                     EXTPRY04 (EIB)     13/03/2019              *
*& DESCRIPTION: Coreccion de HES con importe y moneda inconsistentes   *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CARGA_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  CONSTANTS: lc_zrepid TYPE zrepid VALUE 'ZME_PROCESS_REQ_CUST'.
************************************************************************************
* Ajustamos filtrado Destinatario SRM en las Sol. Pedido para que no genere pedido *
************************************************************************************
* Versión antigua                                                                  *
************************************************************************************
*  "Selecciona SOLPED no tratadas
*  SELECT * INTO TABLE gt_items
*    FROM eban
*    WHERE banfn IN s_banfn  "Nro de Solicitud
*      AND bnfpo IN s_bnfpo  "Pos Nro de Solicitud
*      AND bsart EQ p_bsart  "Clase de Documento
*      AND loekz EQ space    "Pedidos Borrados
*      AND statu EQ 'N'      "Pedidos no tratados
*      AND werks IN s_werks  "Centro
*      AND lfdat IN s_lfdat  "Fecha de Entrega
*      AND lifnr IN s_lifnr  "Proveedor
*      AND konnr IN s_konnr  "Contrato Marco
*      AND ktpnr IN s_ktpnr  "Pos Contrato Marco
*      AND ( banpr EQ '02' OR banpr EQ '05' ) "Status tratamiento solicitud pedido
*      AND blckd EQ space    "Solicitud de pedido bloqueada
*    ORDER BY banfn ASCENDING bnfpo ASCENDING.
************************************************************************************
*  Nueva versión select de filtrado                                                *                                                                                 *
************************************************************************************
  "Selecciona SOLPED no tratadas
  SELECT * INTO TABLE gt_items
    FROM eban
    WHERE banfn IN s_banfn  "Nro de Solicitud
      AND bnfpo IN s_bnfpo  "Pos Nro de Solicitud
      AND bsart EQ p_bsart  "Clase de Documento
      AND loekz EQ space    "Pedidos Borrados
* Inicio modificacion T10000198576 LLCB 01/09/2014
* Código antiguo:
*      AND statu EQ 'N'      "Pedidos no tratados
* Código nuevo:
      AND ( statu EQ 'N' OR statu EQ 'B' ) "Pedidos no tratados o Pedidos creados
* Fin modificacion T10000198576 LLCB 01/09/2014
      AND werks IN s_werks  "Centro
      AND lfdat IN s_lfdat  "Fecha de Entrega
      AND lifnr IN s_lifnr  "Proveedor
      AND konnr IN s_konnr  "Contrato Marco
      AND ktpnr IN s_ktpnr  "Pos Contrato Marco
      AND ( banpr EQ '02' OR banpr EQ '05' ) "Status tratamiento solicitud pedido
      AND blckd EQ space    "Solicitud de pedido bloqueada
      ORDER BY banfn ASCENDING bnfpo ASCENDING.
************************************************************************************
*                                                                                  *
************************************************************************************
  DELETE gt_items WHERE konnr = space.  "+@RGN_06.09.19
  IF gt_items[] IS INITIAL.
    MESSAGE s057(zsgm).
  ELSE.

    "Selecciona el importe maximo establecido
    "para crear pedido de compras
    SELECT SINGLE zcod INTO gv_zcod
     FROM zu001
     WHERE zrepid EQ lc_zrepid
      AND  zdat   EQ 'PR'
      AND  zlevel EQ '01'
      AND  zsec   EQ '001'.

    IF gv_zcod IS INITIAL.
      MESSAGE e068(zsgm) WITH lc_zrepid.
    ENDIF.

  ENDIF.

ENDFORM.                    " CARGA_DATOS

*&---------------------------------------------------------------------*
*&      Form  CHECK_ITEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_items.

  DATA: lv_tabix     TYPE sy-tabix,
        lv_maxline   TYPE sy-dbcnt,
        ls_poheader  TYPE bapimepoheader,
        ls_poheaderx TYPE bapimepoheaderx,
        lt_poitem    TYPE TABLE OF bapimepoitem,
        lt_poitemx   TYPE TABLE OF bapimepoitemx,
        lt_return    TYPE TABLE OF bapiret2.

* INI - SGGMM - 27.04.2018
  DATA:   lt_account           TYPE TABLE OF bapimepoaccount,
          lt_accountx          TYPE TABLE OF bapimepoaccountx,
          lt_poservices        TYPE TABLE OF bapiesllc,
          lt_posrvaccessvalues TYPE TABLE OF bapiesklc,
          lt_polimit           TYPE TABLE OF bapiesuhc,
          lt_pocontractlimits  TYPE TABLE OF bapiesucc.
* FIN - SGGMM - 27.04.2018

DATA: p_flag_error_soamanager type c.
DATA: p_mensajeerror TYPE String.


  FIELD-SYMBOLS: <ls_items> TYPE eban.

  CONSTANTS: lc_poitem TYPE bapiekpoc-po_item VALUE 10.

  "Chequea que esta marcada la opcion de
  "excluir las posiciones con error.
  CHECK c_skerr  IS NOT INITIAL.

  "Valida los items antes de crear PO.
  LOOP AT gt_items ASSIGNING <ls_items>.

    CLEAR:ls_poheader,ls_poheaderx.
    REFRESH:lt_poitem,lt_poitemx,lt_return.
* INI - SGGMM - 27.04.2018
    REFRESH: lt_account,
             lt_accountx,
             lt_poservices,
             lt_posrvaccessvalues,
             lt_polimit,
             lt_pocontractlimits.
* FIN - SGGMM - 27.04.2018

    DESCRIBE TABLE gt_items LINES lv_maxline.
    PERFORM sap_indicador USING text-002 lv_maxline.

    lv_tabix = sy-tabix.
    "Carga datos Cabecera para pedido compras
    PERFORM fill_poheader USING p_bsart1 <ls_items>
                          CHANGING ls_poheader ls_poheaderx.

    "Carga datos posicion para pedido compras.
    PERFORM fill_poitem TABLES lt_poitem lt_poitemx
* INI - SGGMM - 27.04.2018
                               lt_account    lt_accountx
                               lt_poservices lt_posrvaccessvalues
                               lt_polimit    lt_pocontractlimits
* FIN - SGGMM - 27.04.2018
                        USING  <ls_items> lc_poitem.

    "Ejecuta Bapi en modo test.
    PERFORM bapi_po_create1 TABLES lt_poitem lt_poitemx lt_return
* INI - SGGMM - 27.04.2018
                                   lt_account    lt_accountx
                                   lt_poservices lt_posrvaccessvalues
                                   lt_polimit    lt_pocontractlimits
* FIN - SGGMM - 27.04.2018
                            USING  ls_poheader ls_poheaderx 'X' c_memory
                    CHANGING
                            P_FLAG_ERROR_SOAMANAGER
                            p_mensajeerror.


    READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.

    IF sy-subrc IS INITIAL.
      PERFORM get_log TABLES lt_return
                      USING  <ls_items> 'X'.
      "Excluye los items con error
      DELETE gt_items INDEX lv_tabix.
    ELSE.
      PERFORM get_log TABLES lt_return
                      USING  <ls_items> 'X'.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "CHECK_ITEMS
*&---------------------------------------------------------------------*
*&      Form  BAPI_PO_CREATE1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_po_create1 TABLES p_lt_poitem     STRUCTURE bapimepoitem
                            p_lt_poitemx    STRUCTURE bapimepoitemx
                            p_lt_return     STRUCTURE bapiret2
* INI - SGGMM - 27.04.2018
                            p_lt_account           STRUCTURE bapimepoaccount
                            p_lt_accountx          STRUCTURE bapimepoaccountx
                            p_lt_poservices        STRUCTURE bapiesllc
                            p_lt_posrvaccessvalues STRUCTURE bapiesklc
                            p_lt_poservicelimit    STRUCTURE bapiesuhc
                            p_lt_pocontractlimits  STRUCTURE bapiesucc
* FIN - SGGMM - 27.04.2018
                     USING  p_ls_poheader   TYPE bapimepoheader
                            p_ls_poheaderx  TYPE bapimepoheaderx
                            p_lv_testrun    TYPE bapiflag-bapiflag
                            p_lv_uncomplete TYPE bapiflag-bapiflag
                    CHANGING
                            P_FLAG_ERROR_SOAMANAGER TYPE C
                            p_mensajeerror TYPE String.


  DATA: ls_poheader      TYPE bapimepoheader,
        ls_poheaderx     TYPE bapimepoheaderx,
        lv_purchaseorder TYPE bapimepoheader-po_number,
        lv_testrun       TYPE bapiflag-bapiflag,
        lv_uncomplete    TYPE bapiflag-bapiflag,
        lt_poitem        TYPE TABLE OF bapimepoitem,
        lt_poitemx       TYPE TABLE OF bapimepoitemx,
        lt_return        TYPE TABLE OF bapiret2.

* INI - SGGMM - 27.04.2018
  DATA:
    lt_account           TYPE TABLE OF bapimepoaccount,
    lt_accountx          TYPE TABLE OF bapimepoaccountx,
    lt_poservices        TYPE TABLE OF bapiesllc,
    lt_posrvaccessvalues TYPE TABLE OF bapiesklc,
    lt_polimit           TYPE TABLE OF bapiesuhc,
    lt_pocontractlimits  TYPE TABLE OF bapiesucc.
* FIN - SGGMM - 27.04.2018

*Inicio modificación T10000239158 EXT90884 23.02.2015
  DATA: lv_exc TYPE c.
  FIELD-SYMBOLS: <lfs_return> TYPE bapiret2.
*Fin modificación T10000239158 EXT90884 23.02.2015

  "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert{
  DATA: lwa_input TYPE zclmt_consulta_presupuesto_res,
        lo_ai_sys TYPE REF TO cx_ai_system_fault.
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Comentar
  DATA(lo_consulta_presupuesto) = NEW zclco_os_consulta_presupuesto( ).
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Comentar

  DATA: lv_validacion TYPE c LENGTH 1.

  DATA: lv_mensajeerror    TYPE string,
        lv_ai_system_fault TYPE REF TO cx_ai_system_fault.

"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Comentar
  DATA: ls_puerto_logico_wssoamanager TYPE PRX_LOGICAL_PORT_NAME VALUE 'ZCONSULTAPRESUPUESTO'.

  TRY.
      CREATE OBJECT lo_consulta_presupuesto
        EXPORTING
          logical_port_name = ls_puerto_logico_wssoamanager.
    CATCH cx_ai_system_fault INTO lv_ai_system_fault.
      CALL METHOD lv_ai_system_fault->if_message~get_text
        RECEIVING
          result = lv_mensajeerror.
      APPEND INITIAL LINE TO lwa_input-mt_consulta_presupuesto_res-mensajes ASSIGNING FIELD-SYMBOL(<fs_mensaje>).
      <fs_mensaje>-descripcion_mensaje = lv_mensajeerror.
       P_FLAG_ERROR_SOAMANAGER = 'X'.
       p_mensajeerror = lv_mensajeerror.
  ENDTRY.

  "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert}
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Comentar


  MOVE: p_lt_poitem[]  TO lt_poitem[],
        p_lt_poitemx[] TO lt_poitemx[],
        p_ls_poheader  TO ls_poheader,
        p_ls_poheaderx TO ls_poheaderx,
        p_lv_testrun   TO lv_testrun,     "Modo Test
        p_lv_uncomplete TO lv_uncomplete. "Memory Uncomplete

* INI - SGGMM - 27.04.2018
  MOVE: p_lt_account[]            TO lt_account[],
        p_lt_accountx[]           TO lt_accountx[],
        p_lt_poservices[]         TO lt_poservices[],
        p_lt_posrvaccessvalues[]  TO lt_posrvaccessvalues[],
        p_lt_poservicelimit[]     TO lt_polimit[],
        p_lt_pocontractlimits[]   TO lt_pocontractlimits.
* FIN - SGGMM - 27.04.2018
* Inicio modificación T10000252571 EXTSMA01 16.09.2015
  PERFORM f_tratar_cond_pago TABLES   lt_poitem
                             CHANGING ls_poheader
                                      ls_poheaderx.
* Fin modificación T10000252571 EXTSMA01 16.09.2015

  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader          = ls_poheader
      poheaderx         = ls_poheaderx
      testrun           = lv_testrun
      memory_uncomplete = lv_uncomplete
      no_price_from_po  = 'X'
    IMPORTING
      exppurchaseorder  = lv_purchaseorder
    TABLES
      return            = lt_return
      poitem            = lt_poitem
      poitemx           = lt_poitemx
* INI - SGGMM - 27.04.2018
      poaccount         = lt_account
      poaccountx        = lt_accountx
      poservices        = lt_poservices
      posrvaccessvalues = lt_posrvaccessvalues
      polimits          = lt_polimit
      pocontractlimits  = lt_pocontractlimits.
* FIN - SGGMM - 27.04.2018

*Inicio modificación T10000239158 EXT90884 23.02.2015
  CLEAR: lv_exc.
* Se localizan si hay mensajes que indiquen que se ha
* excedido el limite del contrato
  READ TABLE lt_return TRANSPORTING NO FIELDS
                       WITH KEY id = '06'
                                number = '042'.
  IF sy-subrc EQ 0.
    lv_exc = 'X'.
  ENDIF.
*Fin modificación T10000239158 EXT90884 23.02.2015

*Inicio modificación T10000239158 EXT90884 23.02.2015
*  IF NOT lv_purchaseorder IS INITIAL.
  IF   lv_purchaseorder IS NOT INITIAL
   AND lv_exc IS INITIAL.

    "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert{
    CLEAR gv_purchaseorder.
    gv_purchaseorder = lv_purchaseorder.
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Nuevo
    p_ls_poheader-po_number = lv_purchaseorder.
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Nuevo

    "Invocamos al Service Consumer con el número generado por la bapi

    LOOP AT gw_output-mt_consulta_presupuesto-datos ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-pedido_intermedio = lv_purchaseorder.
    ENDLOOP.

    CLEAR gw_input.

"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Comentar
IF P_FLAG_ERROR_SOAMANAGER IS INITIAL.
    TRY .
        lo_consulta_presupuesto->os_consulta_presupuesto(
          EXPORTING
            input              =  gw_output
          IMPORTING
            output             =  gw_input ).
*      CATCH cx_ai_system_fault.    "
      CATCH cx_ai_system_fault INTO lo_ai_sys.
        CALL METHOD lo_ai_sys->if_message~get_text
          RECEIVING
            result = lv_mensajeerror.

        APPEND INITIAL LINE TO gw_input-mt_consulta_presupuesto_res-mensajes ASSIGNING <fs_mensaje>.
        <fs_mensaje>-descripcion_mensaje = lv_mensajeerror.
       P_FLAG_ERROR_SOAMANAGER = 'X'.
       p_mensajeerror = lv_mensajeerror.
    ENDTRY.
ELSE.
  MOVE-CORRESPONDING gw_output-mt_consulta_presupuesto-datos
  TO gw_input-mt_consulta_presupuesto_res-datos.
ENDIF.
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Comentar


    IF NOT gv_codigoejec IS INITIAL.
      CLEAR: gt_logpresupuesto[],
             gt_menpresupuesto[].

      DATA: lv_posicion TYPE i.

      lv_posicion = 1.
      LOOP AT gw_output-mt_consulta_presupuesto-datos ASSIGNING FIELD-SYMBOL(<fs_datos_output>).
        CLEAR gw_logpresupuesto.
        gw_logpresupuesto-codejecucion = gv_codigoejec.
        gw_logpresupuesto-pos_ejecucion = lv_posicion.
        gw_logpresupuesto-sociedad = <fs_datos_output>-sociedad.
        gw_logpresupuesto-ejercicio = <fs_datos_output>-ejercicio.
        gw_logpresupuesto-centro_costos = <fs_datos_output>-centro_costos.
        gw_logpresupuesto-elemento_pep = <fs_datos_output>-elemento_pep.
        gw_logpresupuesto-moneda = <fs_datos_output>-moneda.
        gw_logpresupuesto-numero_orden = <fs_datos_output>-numero_orden.
        gw_logpresupuesto-cuenta_mayor = <fs_datos_output>-cuenta_mayor.
        gw_logpresupuesto-numero_contrato = <fs_datos_output>-numero_contrato.
        gw_logpresupuesto-pedido_intermedio = <fs_datos_output>-pedido_intermedio.
        gw_logpresupuesto-posicion_documento_compras = <fs_datos_output>-posicion_documento_compras.
        gw_logpresupuesto-posicion_documento_presupuesto = <fs_datos_output>-posicion_documento_presupuesto.
        gw_logpresupuesto-texto_posicion = <fs_datos_output>-texto_posicion.
        gw_logpresupuesto-fondo = <fs_datos_output>-fondo.
        gw_logpresupuesto-centro_gestor = <fs_datos_output>-centro_gestor.
        gw_logpresupuesto-posicion_presupuestaria = <fs_datos_output>-posicion_presupuestaria.
        gw_logpresupuesto-importe_consulta = <fs_datos_output>-importe_consulta.
        gw_logpresupuesto-importe_disponible = <fs_datos_output>-importe_disponible.
        gw_logpresupuesto-ok = <fs_datos_output>-ok.
        gw_logpresupuesto-direccion = '1'.
        gw_logpresupuesto-fecha = sy-datum.
        gw_logpresupuesto-hora = sy-timlo.
        gw_logpresupuesto-transaccion = 'ZMM002'.
        gw_logpresupuesto-numenvio = '2'.

        APPEND gw_logpresupuesto TO gt_logpresupuesto.
        add 1 to lv_posicion.
      ENDLOOP.

      lv_posicion = 1.
      LOOP AT gw_input-mt_consulta_presupuesto_res-datos ASSIGNING FIELD-SYMBOL(<fs_datos_input>).
        CLEAR gw_logpresupuesto.
        gw_logpresupuesto-codejecucion = gv_codigoejec.
        gw_logpresupuesto-pos_ejecucion = lv_posicion.
        gw_logpresupuesto-sociedad = <fs_datos_input>-sociedad.
        gw_logpresupuesto-ejercicio = <fs_datos_input>-ejercicio.
        gw_logpresupuesto-centro_costos = <fs_datos_input>-centro_costos.
        gw_logpresupuesto-elemento_pep = <fs_datos_input>-elemento_pep.
        gw_logpresupuesto-moneda = <fs_datos_input>-moneda.
        gw_logpresupuesto-numero_orden = <fs_datos_input>-numero_orden.
        gw_logpresupuesto-cuenta_mayor = <fs_datos_input>-cuenta_mayor.
        gw_logpresupuesto-numero_contrato = <fs_datos_input>-numero_contrato.
        gw_logpresupuesto-pedido_intermedio = <fs_datos_input>-pedido_intermedio.
        gw_logpresupuesto-posicion_documento_compras = <fs_datos_input>-posicion_documento_comercial.
        gw_logpresupuesto-posicion_documento_presupuesto = <fs_datos_input>-posicion_documento_presupuesto.
        gw_logpresupuesto-texto_posicion = <fs_datos_input>-texto_posicion.
        gw_logpresupuesto-fondo = <fs_datos_input>-fondo.
        gw_logpresupuesto-centro_gestor = <fs_datos_input>-centro_gestor.
        gw_logpresupuesto-posicion_presupuestaria = <fs_datos_input>-posicion_presupuestaria.
        gw_logpresupuesto-importe_consulta = <fs_datos_input>-importe_consulta.
        gw_logpresupuesto-importe_disponible = <fs_datos_input>-importe_disponible.
        gw_logpresupuesto-ok = <fs_datos_input>-ok.
        gw_logpresupuesto-direccion = '2'.
        gw_logpresupuesto-fecha = sy-datum.
        gw_logpresupuesto-hora = sy-timlo.
        gw_logpresupuesto-transaccion = 'ZMM002'.
        gw_logpresupuesto-numenvio = '2'.

        APPEND gw_logpresupuesto TO gt_logpresupuesto.
        add 1 to lv_posicion.
      ENDLOOP.

      lv_posicion = 1.
      LOOP AT gw_input-mt_consulta_presupuesto_res-mensajes ASSIGNING FIELD-SYMBOL(<fs_mensaje_input>).
        CLEAR gw_menpresupuesto.
        gw_menpresupuesto-codejecucion = gv_codigoejec.
        gw_menpresupuesto-pos_ejecucion = lv_posicion.
        gw_menpresupuesto-numenvio = '2'.
        gw_menpresupuesto-sociedad = <fs_mensaje_input>-sociedad.
        gw_menpresupuesto-ejercicio = <fs_mensaje_input>-ejercicio.
        gw_menpresupuesto-documento_compras = <fs_mensaje_input>-documento_compras.
        gw_menpresupuesto-posicion_documento_compras = <fs_mensaje_input>-posicion_documento_compras.
        gw_menpresupuesto-descripcion_mensaje = <fs_mensaje_input>-descripcion_mensaje.

        APPEND gw_menpresupuesto TO gt_menpresupuesto.
        add 1 to lv_posicion.
      ENDLOOP.
      IF NOT gt_logpresupuesto[] IS INITIAL.
        MODIFY  ztlogdatospresup FROM TABLE gt_logpresupuesto.
      ENDIF.
      IF NOT gt_menpresupuesto[] IS INITIAL.
        MODIFY ztlogmensapresup FROM TABLE gt_menpresupuesto.
      ENDIF.

    ENDIF.

    lv_validacion = ''.


    LOOP AT gw_input-mt_consulta_presupuesto_res-datos ASSIGNING <fs>.
      IF <fs>-ok EQ 'X'.
        lv_validacion = 'X'.
      ELSE.
        lv_validacion = ''.
        EXIT.
      ENDIF.
    ENDLOOP.

"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Comentar
*    IF gw_input-mt_consulta_presupuesto_res-datos[] IS INITIAL
*  AND gw_input-mt_consulta_presupuesto_res-mensajes[] IS INITIAL.
*
*      APPEND INITIAL LINE TO gw_input-mt_consulta_presupuesto_res-mensajes ASSIGNING FIELD-SYMBOL(<fs_mensaje3>).
*      <fs_mensaje3>-descripcion_mensaje = 'Error al obtener información de COGA'.
*      PERFORM enviar_correo_errores
*      USING  gw_output <fs_mensaje3>-descripcion_mensaje lv_mensajeerror.
*    ENDIF.
    "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert}
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Comentar

*Fin modificación T10000239158 EXT90884 23.02.2015
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Comentar
*    IF lv_validacion EQ 'X'."JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Comentar
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      DELETE lt_return WHERE type NE 'S'.
*    ELSE.
*      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'."JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert
*      DATA: lv_asunto TYPE string.
*      CONCATENATE 'Log de error por falta de presupuesto:  ' lv_purchaseorder INTO lv_asunto.
*      PERFORM enviar_correo_errores USING  gw_output lv_asunto lv_mensajeerror.
*    ENDIF.
  ELSE.
*Inicio modificación T10000239158 EXT90884 23.02.2015
    IF lv_exc IS NOT INITIAL.
* Los mensajes que indican que se ha excedido el valor de contrato, se
* cambian de tipo Warning a Error.
      LOOP AT lt_return ASSIGNING <lfs_return> WHERE type EQ 'W'
                                                 AND id EQ '06'
                                                 AND number EQ '042'.
        <lfs_return>-type = 'E'.
      ENDLOOP.
    ENDIF.
*Fin modificación T10000239158 EXT90884 23.02.2015
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    DELETE lt_return WHERE type EQ 'W'.
  ENDIF.

  MOVE lt_return[] TO p_lt_return[].

ENDFORM.                    " BAPI_PO_CREATE1
*&---------------------------------------------------------------------*
*&      Form  INICIALIZACION_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization_data.

  REFRESH: gt_items,gt_header,gt_log,gt_fieldcat.
  CLEAR:   gs_layout,gv_zcod.

ENDFORM.                    " INICIALIZACION_DATOS
*&---------------------------------------------------------------------*
*&      Form  FILL_POITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_poitem TABLES p_lt_poitem  STRUCTURE bapimepoitem
                        p_lt_poitemx STRUCTURE bapimepoitemx
* INI - SGGMM - 27.04.2018
                        p_lt_account           STRUCTURE bapimepoaccount
                        p_lt_accountx          STRUCTURE bapimepoaccountx
                        p_lt_poservices        STRUCTURE bapiesllc
                        p_lt_posrvaccessvalues STRUCTURE bapiesklc
                        p_lt_polimit           STRUCTURE bapiesuhc
                        p_lt_pocontractlimits  STRUCTURE bapiesucc
* FIN - SGGMM - 27.04.2018
                 USING  p_ls_eban    TYPE eban
                        p_lv_poitem  TYPE bapiekpoc-po_item.

* INI - SGGMM - 27.04.2018
  CONSTANTS: lco_item_cat   TYPE bapimepoitem-item_cat VALUE '9',
             lco_acctasscat TYPE bapimepoitem-acctasscat VALUE 'K'.
* FIN - SGGMM - 27.04.2018


  DATA: ls_poitem  TYPE bapimepoitem,
        ls_poitemx TYPE bapimepoitemx.

* INI - SGGMM - 27.04.2018
  DATA: lv_pckg_no          TYPE bapiesllc-pckg_no,
        lv_pritem           TYPE bapiesllc-pckg_no,
        lv_line_no          TYPE bapiesllc-line_no,
        lv_praccount        TYPE bapimereqaccount-preq_item,
        lv_serviceaccount   TYPE bapi_srv_acc_data-doc_item,
        lv_servicelines     TYPE bapi_srv_service_line-doc_item,
        lv_prcomponents     TYPE bapimereqcomponent-preq_item,
        lv_servicelimit     TYPE bapi_srv_limit_data-doc_item,
        lv_bnfpo            TYPE bapiesllc-ext_line,
        lv_pos_contractlimt TYPE introw.

  DATA: lt_esll2          TYPE TABLE OF esll,
        lt_esll_tmp       TYPE TABLE OF esll,                     "+0005
        lt_praccount      TYPE TABLE OF bapimereqaccount,
        lt_prcomponents   TYPE TABLE OF bapimereqcomponent,
        lt_serviceaccount TYPE TABLE OF bapi_srv_acc_data,
        lt_servicelines   TYPE TABLE OF bapi_srv_service_line,
        lt_servicelimit   TYPE TABLE OF bapi_srv_limit_data,
        lt_prservicelimit TYPE TABLE OF bapi_srv_limit_data,
        lt_pritem         TYPE TABLE OF bapimereqitem,
        lt_sercontractlim TYPE TABLE OF bapi_srv_contract_limits.

  DATA: ls_account           TYPE bapimepoaccount,
        ls_accountx          TYPE bapimepoaccountx,
        ls_poservices        TYPE bapiesllc,
        ls_posrvaccessvalues TYPE bapiesklc,
        ls_esll              TYPE esll,
        ls_esll2             TYPE esll,
        ls_packno            TYPE packno,                          "+0005
        ls_praccount         TYPE bapimereqaccount,
        ls_prcomponents      TYPE bapimereqcomponent,
        ls_serviceaccount    TYPE bapi_srv_acc_data,
        ls_servicelines      TYPE bapi_srv_service_line,
        ls_pritem            TYPE bapimereqitem,
        ls_prservicelimit    TYPE bapi_srv_limit_data,
        ls_polimit           TYPE bapiesuhc,
        ls_pocontractlimits  TYPE bapiesucc,
        ls_esuc              TYPE esuc,
        ls_sercontractlim    TYPE bapi_srv_contract_limits.


  CALL FUNCTION 'BAPI_PR_GETDETAIL'
    EXPORTING
      number                = p_ls_eban-banfn
      account_assignment    = 'X'
      item_text             = 'X'
      header_text           = 'X'
      delivery_address      = 'X'
      version               = 'X'
      sc_components         = 'X'
      serial_numbers        = 'X'
      services              = 'X'
    TABLES
      pritem                = lt_pritem
      praccount             = lt_praccount
      prcomponents          = lt_prcomponents
      serviceaccount        = lt_serviceaccount
      servicelines          = lt_servicelines
      servicelimit          = lt_prservicelimit
      servicecontractlimits = lt_sercontractlim. " 24.05.2018

  IF sy-subrc EQ 0.
    MOVE: p_ls_eban-bnfpo TO lv_praccount,
          p_ls_eban-bnfpo TO lv_serviceaccount,
          p_ls_eban-bnfpo TO lv_servicelines,
          p_ls_eban-bnfpo TO lv_prcomponents,
          p_ls_eban-bnfpo TO lv_servicelimit,
          p_ls_eban-bnfpo TO lv_pritem.

    READ TABLE lt_prservicelimit   INTO ls_prservicelimit
    WITH KEY doc_item = lv_servicelimit. "p_ls_eban-bnfpo.
    READ TABLE lt_pritem   INTO ls_pritem
    WITH KEY preq_item = lv_pritem. "p_ls_eban-bnfpo.
    READ TABLE lt_prcomponents   INTO ls_prcomponents
    WITH KEY preq_item = lv_prcomponents. "p_ls_eban-bnfpo.
  ENDIF.

  IF gv_first EQ ' '.
    gv_packno = 1.
    gv_first = 'X'.
  ELSE.
    gv_packno = gv_packno + 2.
  ENDIF.

*  lv_pckg_no = p_ls_eban-bnfpo / 10.
*
*  if lv_pckg_no ne 1.
*    lv_pckg_no = ( lv_pckg_no * 2 ) - 1.
*  endif.

  lv_line_no = 0000000001.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gv_packno
    IMPORTING
      output = gv_packno.

  gv_subpackno = gv_packno + 1.

* FIN - SGGMM - 27.04.2018


  "Carga datos de posicion de pedido de compras
  ls_poitem-po_item    = p_lv_poitem.     "Nro posición documento de compras
  ls_poitem-short_text = p_ls_eban-txz01. "Texto breve material
  ls_poitem-material   = p_ls_eban-matnr. "Número de material
* INI - SGGMM - 27.04.2018

  ls_poitem-matl_group = p_ls_eban-matkl.
  ls_poitem-item_cat   = ls_pritem-item_cat.
  ls_poitem-acctasscat = ls_pritem-acctasscat.

  ls_poitem-preq_item  = p_ls_eban-bnfpo. "Posición de la solicitud de pedido
*  ls_poitem-pckg_no    = ls_esll-packno.  "número de paquete
  ls_poitem-pckg_no    = gv_packno.  "número de paquete
  ls_poitem-preq_no    = p_ls_eban-banfn. "numero solicitud de pedido

* FIN - SGGMM - 27.04.2018
  ls_poitem-plant      = p_ls_eban-werks. "Centro
  ls_poitem-po_unit    = p_ls_eban-meins. "Unidad de medida de pedido
  ls_poitem-agreement  = p_ls_eban-konnr. "Número del contrato superior
  ls_poitem-agmt_item  = p_ls_eban-ktpnr. "Número de posición del contrato superior
  ls_poitem-preq_no    = p_ls_eban-banfn. "Número de la solicitud de pedido
  ls_poitem-preq_item  = p_ls_eban-bnfpo. "Número de posición de la solicitud de pedido
  ls_poitem-quantity   = p_ls_eban-menge. "Cantidad solicitud de pedido
  ls_poitem-net_price  = p_ls_eban-preis. "Precio en la solicitud de pedido
* Inicio modificación 0001-T10000183972 LLCB 28.04.2014
  ls_poitem-price_unit = p_ls_eban-peinh. "cantidad base
* Fin modificación 0001-T10000183972 LLCB 28.04.2014
  APPEND ls_poitem TO p_lt_poitem.

  "Flags de Posiciones de pedido de compras
  ls_poitemx-po_item = p_lv_poitem.
  ls_poitemx-short_text = 'X'.
  ls_poitemx-material   = 'X'.
* INI - SGGMM - 27.04.2018
  IF ls_poitem-matl_group IS NOT INITIAL.
    ls_poitemx-matl_group = 'X'.
  ENDIF.

  ls_poitemx-item_cat   = 'X'.
  ls_poitemx-acctasscat = 'X'.

  IF ls_poitem-preq_item IS NOT INITIAL.
    ls_poitemx-preq_item  = 'X'.
  ENDIF.

  IF ls_poitem-pckg_no IS NOT INITIAL.
    ls_poitemx-pckg_no    = 'X'.
  ENDIF.

  IF ls_poitem-preq_no IS NOT INITIAL.
    ls_poitemx-preq_no    = 'X'.
  ENDIF.
* FIN - SGGMM - 27.04.2018

  ls_poitemx-plant      = 'X'.
  ls_poitemx-po_unit    = 'X'.
  ls_poitemx-agreement  = 'X'.
  ls_poitemx-agmt_item  = 'X'.
  ls_poitemx-preq_no    = 'X'.
  ls_poitemx-preq_item  = 'X'.
  ls_poitemx-quantity   = 'X'.
  ls_poitemx-net_price  = 'X'.
* Inicio modificación 0001-T10000183972 LLCB 28.04.2014
  ls_poitemx-price_unit = 'X'. "cantidad base
* Fin modificación 0001-T10000183972 LLCB 28.04.2014
  APPEND ls_poitemx TO p_lt_poitemx.

* INI - SGGMM - 27.04.2018
  LOOP AT lt_praccount INTO ls_praccount WHERE preq_item = lv_praccount.
** POACCOUNT
    ls_account-po_item    = p_lv_poitem.
    ls_account-serial_no  = ls_praccount-serial_no.
    ls_account-gl_account = ls_praccount-gl_account.
    ls_account-costcenter = ls_praccount-costcenter.
****
    ls_account-quantity = ls_praccount-quantity.
    ls_account-distr_perc = ls_praccount-distr_perc.
    ls_account-net_value = ls_praccount-net_value.
    ls_account-bus_area = ls_praccount-bus_area.
    ls_account-itm_number = ls_praccount-itm_number.
    ls_account-sched_line = ls_praccount-sched_line.
    ls_account-orderid = ls_praccount-orderid.
    ls_account-co_area = ls_praccount-co_area.
    ls_account-profit_ctr = ls_praccount-profit_ctr.
****

    APPEND ls_account TO p_lt_account.

** POACCOUNTX
    ls_accountx-po_item    = p_lv_poitem.
    ls_accountx-serial_no  = ls_praccount-serial_no.

    IF ls_account-po_item IS NOT INITIAL.
      ls_accountx-po_itemx   = 'X'.
    ENDIF.

    IF ls_account-serial_no IS NOT INITIAL.
      ls_accountx-serial_nox = 'X'.
    ENDIF.

    IF ls_account-gl_account IS NOT INITIAL .
      ls_accountx-gl_account = 'X'.
    ENDIF.

    IF ls_account-costcenter IS NOT INITIAL.
      ls_accountx-costcenter = 'X'.
    ENDIF.

***
    IF ls_account-quantity IS NOT INITIAL.
      ls_accountx-quantity  = 'X'.
    ENDIF.

    IF ls_account-distr_perc IS NOT INITIAL.
      ls_accountx-distr_perc   = 'X'.
    ENDIF.

    IF ls_account-net_value IS NOT INITIAL.
      ls_accountx-net_value   = 'X'.
    ENDIF.

    IF ls_account-bus_area IS NOT INITIAL.
      ls_accountx-bus_area   = 'X'.
    ENDIF.

    IF ls_account-itm_number IS NOT INITIAL.
      ls_accountx-itm_number   = 'X'.
    ENDIF.

    IF ls_account-sched_line IS NOT INITIAL.
      ls_accountx-sched_line   = 'X'.
    ENDIF.

    IF ls_account-orderid IS NOT INITIAL.
      ls_accountx-orderid   = 'X'.
    ENDIF.

    IF ls_account-co_area IS NOT INITIAL.
      ls_accountx-co_area   = 'X'.
    ENDIF.

    IF ls_account-profit_ctr IS NOT INITIAL.
      ls_accountx-profit_ctr   = 'X'.
    ENDIF.

***
    APPEND ls_accountx TO p_lt_accountx.

    CLEAR: ls_account,
           ls_accountx.
  ENDLOOP.

** POSERVICELIMIT

*  ls_polimit-pckg_no = p_ls_eban-packno.
  ls_polimit-pckg_no = gv_packno.
  ls_polimit-no_limit = ls_prservicelimit-no_limit.
  ls_polimit-exp_value = ls_prservicelimit-exp_value.
  ls_polimit-price_chg = ls_prservicelimit-con_price_chg.
  ls_polimit-serv_type = ls_prservicelimit-ssc_srv_type.
  ls_polimit-edition   = ls_prservicelimit-ssc_edition.
  ls_polimit-ssc_limit = ls_prservicelimit-ssc_limit.
  ls_polimit-ssc_nolim = ls_prservicelimit-ssc_no_limit.
  ls_polimit-ssc_prschg = ls_prservicelimit-mss_price_chg.
  APPEND ls_polimit TO p_lt_polimit.

* INI - SGGMM - 24.05.2018
  CLEAR: ls_pocontractlimits,
         ls_sercontractlim,
         lv_pos_contractlimt.

  lv_pos_contractlimt = lv_line_no.
  LOOP AT lt_sercontractlim INTO ls_sercontractlim.

    ls_pocontractlimits-pckg_no    = gv_packno.
    ls_pocontractlimits-line_no    = lv_pos_contractlimt.
    ls_pocontractlimits-con_number = ls_sercontractlim-contract.
    ls_pocontractlimits-con_item   = ls_sercontractlim-contract_item.
    ls_pocontractlimits-limit      = ls_sercontractlim-limit.
    ls_pocontractlimits-no_limit   = ls_sercontractlim-no_limit.


    APPEND ls_pocontractlimits TO p_lt_pocontractlimits.
    CLEAR: ls_pocontractlimits,
           ls_sercontractlim.

    lv_pos_contractlimt = lv_pos_contractlimt + 1.
  ENDLOOP.
* FIN - SGGMM - 24.05.2018

*  if ls_esll is not initial.
  IF lt_servicelines[] IS NOT INITIAL AND
     lt_serviceaccount[] IS NOT INITIAL.
* {Ini 0005
    IF p_ls_eban-konnr NE space.
      SELECT SINGLE *
        INTO ls_esll
        FROM esll
        WHERE packno = p_ls_eban-packno.
      IF sy-subrc EQ 0 AND ls_esll-sub_packno IS NOT INITIAL.
* Se obtiene Nro de Servicios
        SELECT *
        INTO TABLE lt_esll_tmp
        FROM esll
        WHERE packno = ls_esll-sub_packno.
      ENDIF.

* Se Obtiene el paquete del contrato
      CLEAR: ls_packno.
      SELECT SINGLE packno
        INTO ls_packno
        FROM ekpo
        WHERE ebeln = p_ls_eban-konnr
          AND ebelp = p_ls_eban-ktpnr.
*
      SELECT SINGLE *
        INTO ls_esll
        FROM esll
        WHERE packno = ls_packno.
      IF sy-subrc EQ 0 AND ls_esll-sub_packno IS NOT INITIAL.
* Se obtiene paquete de HES con el subpaquete
        SELECT *
        INTO TABLE lt_esll2
        FROM esll
        FOR ALL ENTRIES IN lt_esll_tmp
        WHERE packno = ls_esll-sub_packno
          AND srvpos = lt_esll_tmp-srvpos.

        LOOP AT lt_esll_tmp ASSIGNING FIELD-SYMBOL(<fs_esll>).
          READ TABLE lt_esll2 ASSIGNING FIELD-SYMBOL(<fs_esll2>) WITH KEY srvpos = <fs_esll>-srvpos.
          IF sy-subrc EQ 0.
            <fs_esll2>-extrow = <fs_esll>-extrow.
          ENDIF.
        ENDLOOP.
      ENDIF.
      CLEAR: lt_esll_tmp[].
    ELSE.
* }Fin 0005
      SELECT SINGLE *
        INTO ls_esll
        FROM esll
        WHERE packno = p_ls_eban-packno.
      IF sy-subrc EQ 0 AND ls_esll-sub_packno IS NOT INITIAL.
        SELECT *
        INTO TABLE lt_esll2
        FROM esll
        WHERE packno = ls_esll-sub_packno.
      ENDIF.
    ENDIF.

    IF ls_esll IS NOT INITIAL.
** POSERVICES

      ls_poservices-pckg_no = gv_packno.
      ls_poservices-line_no = 0000000001.
      ls_poservices-outl_level = 0.
      ls_poservices-outl_ind = 'X'.
      ls_poservices-subpckg_no = gv_subpackno.
      APPEND ls_poservices TO p_lt_poservices.

**** SUBPACKAGE INFO

      IF ls_esll-sub_packno IS NOT INITIAL.

*      lv_bnfpo = 0000000010.
*        lv_pckg_no = ls_poservices-subpckg_no.

        LOOP AT lt_esll2 INTO ls_esll2 ."where .
          CLEAR: ls_poservices,
                 ls_servicelines,
                 ls_serviceaccount.

          ls_poservices-OUTL_NO = p_lv_poitem. "EMAT - Enrique Aguirre - LFSENAG-473 - Nuevo

          READ TABLE lt_servicelines INTO ls_servicelines
          WITH KEY srv_line = ls_esll2-extrow
                   doc_item = lv_servicelines.

          READ TABLE lt_serviceaccount INTO ls_serviceaccount
          WITH KEY srv_line = ls_esll2-extrow
                   doc_item = lv_serviceaccount.

          lv_line_no = lv_line_no + 1.

          ls_poservices-pckg_no = gv_subpackno.
          ls_poservices-line_no = lv_line_no.
          ls_poservices-ext_line = ls_esll2-extrow.
          ls_poservices-outl_level = 0.
          ls_poservices-edition = ls_servicelines-ssc_edition.
          ls_poservices-service = ls_servicelines-service.
          ls_poservices-quantity = ls_servicelines-quantity.
          ls_poservices-ovf_unlim = ls_servicelines-ovf_unlim. "@EMAT -  26.05.2019 - LFSENAG-265

*        ls_poservice-ovf_tol = ls_servicelines-ovf_tol.
          ls_poservices-base_uom = ls_servicelines-uom.
          IF p_ls_eban-konnr NE space.
            ls_poservices-gr_price = ls_esll2-tbtwr.
          ELSE.
            ls_poservices-gr_price = ls_servicelines-gross_price.
          ENDIF.
          ls_poservices-price_unit = ls_servicelines-price_unit.
          ls_poservices-short_text = ls_servicelines-short_text.
          ls_poservices-pln_pckg = ls_esll2-pln_packno.
          ls_poservices-pln_line = ls_esll2-pln_introw.

          APPEND ls_poservices TO p_lt_poservices.

*        lv_bnfpo = lv_bnfpo + 10.

** POSRVACCESSVALUE
          ls_posrvaccessvalues-pckg_no = gv_subpackno.
          ls_posrvaccessvalues-line_no = ls_poservices-line_no.
          ls_posrvaccessvalues-serno_line = ls_serviceaccount-serial_no.
          ls_posrvaccessvalues-percentage = ls_serviceaccount-percent.
          "@EMAT-001 - 10.12.2019 - LFSENAG-396 - Delete - José Odar.
          "ls_posrvaccessvalues-serial_no = ls_serviceaccount-serial_no.
          "@EMAT-001 - 10.12.2019 - LFSENAG-396 - Delete - José Odar.

          "@EMAT-001 - 10.12.2019 - LFSENAG-396 - Insert - José Odar.
          ls_posrvaccessvalues-serial_no = ls_serviceaccount-serial_no_item.
          "@EMAT-001 - 10.12.2019 - LFSENAG-396 - Insert - José Odar.
          ls_posrvaccessvalues-quantity = ls_serviceaccount-quantity.
          ls_posrvaccessvalues-net_value = ls_serviceaccount-net_value.

          APPEND ls_posrvaccessvalues TO p_lt_posrvaccessvalues.

        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
* FIN - SGGMM - 27.04.2018


*  Begin @RGN_05.08.19
* Considerar el monto total en el tipo de moneda correspondiente.
*  Lineas insertadas
  FIELD-SYMBOLS : <fs_poitem> LIKE bapimepoitem.
  FIELD-SYMBOLS : <fs_poservices> LIKE bapiesllc.
  DATA: lv_net_price TYPE bapicurext.
  LOOP AT p_lt_poitem ASSIGNING <fs_poitem>.
    READ TABLE p_lt_poservices ASSIGNING <fs_poservices> WITH KEY pckg_no = <fs_poitem>-pckg_no.
    "@JCL_20.08.19+{
    IF sy-subrc EQ 0.
      LOOP AT p_lt_poservices INTO ls_poservices WHERE pckg_no = <fs_poservices>-subpckg_no .
        lv_net_price  = lv_net_price + ( ls_poservices-gr_price * ls_poservices-quantity ).
      ENDLOOP.
      <fs_poitem>-net_price = lv_net_price.
      p_ls_eban-preis = lv_net_price.
      CLEAR lv_net_price.
    ENDIF.
    "@JCL_20.08.19+}

    "@JCL_20.08.19-{
*    LOOP AT p_lt_poservices INTO ls_poservices WHERE pckg_no = <fs_poservices>-subpckg_no .
*      lv_net_price  = lv_net_price + ( ls_poservices-gr_price * ls_poservices-quantity ).
*    ENDLOOP.
*    <fs_poitem>-net_price = lv_net_price.
*    p_ls_eban-preis = lv_net_price.
*    CLEAR lv_net_price.
    "@JCL_20.08.19-}
  ENDLOOP.
*  End @RGN_05.08.19
ENDFORM.                    " FILL_POITEM
* FIN - SGGMM - 27.04.2018
*&---------------------------------------------------------------------*
*&      Form  FILL_POHEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BSART1  text
*----------------------------------------------------------------------*
FORM fill_poheader USING    p_p_bsart      TYPE bsart
                            p_ls_header    TYPE eban
                   CHANGING p_ls_poheader  TYPE bapimepoheader
                            p_ls_poheaderx TYPE bapimepoheaderx.

* Inicio modificación T10000356517 EXTSMA01 13.07.2017
* Se declara la tabla interna 'l_zu001_tab'.
  DATA: l_zu001_tab TYPE STANDARD TABLE OF zu001,
        l_zu001_wa  TYPE zu001.

  CONSTANTS:  lco_zrepid TYPE zrepid VALUE 'ZMM_CREATE_PEDIDO',
              lco_zdat   TYPE zdat   VALUE 'GC'.

* INI - SGGMM - 27.04.2018
*  constants:  lco_doc_type type bapimepoheader-doc_type value 'NB'. " MOD - FLER - 04.05.2018
  CONSTANTS:  lco_doc_type TYPE bapimepoheader-doc_type VALUE 'ZPCM'.
* FIN - SGGMM - 27.04.2018
* Fin modificación T10000356517 EXTSMA01 13.07.2017

  CLEAR: p_ls_poheader,p_ls_poheaderx.

* Inicio modificación T10000356517 EXTSMA01 13.07.2017
  SELECT zcod zcod2
    INTO CORRESPONDING FIELDS OF TABLE l_zu001_tab
    FROM zu001
    WHERE zrepid EQ lco_zrepid
    AND zdat EQ lco_zdat.

  CLEAR: l_zu001_wa.
  READ TABLE l_zu001_tab INTO l_zu001_wa WITH KEY zcod = p_ls_header-werks.
  IF sy-subrc EQ 0.
    IF p_ls_header-ekgrp NE l_zu001_wa-zcod2.
      p_ls_poheader-pur_group = l_zu001_wa-zcod2.
    ELSE.
      p_ls_poheader-pur_group  = p_ls_header-ekgrp.
    ENDIF.
  ELSE.
    p_ls_poheader-pur_group  = p_ls_header-ekgrp.
  ENDIF.
* Fin modificación T10000356517 EXTSMA01 13.07.2017

  "Carga datos de Cabecera del pedido de Compras
  p_ls_poheader-comp_code  = p_bukrs.           "Sociedad
* INI - SGGMM - 27.04.2018
  p_ls_poheader-doc_type   = p_p_bsart.         "Clase de documento de compras
*  p_ls_poheader-doc_type   = lco_doc_type.      "NB
* FIN - SGGMM - 27.04.2018
  p_ls_poheader-creat_date = sy-datum.          "Fecha del documento de compras
  p_ls_poheader-created_by = sy-uname.          "Usuario
  p_ls_poheader-vendor     = p_ls_header-lifnr. "Número de cuenta del proveedor
  p_ls_poheader-purch_org  = p_ls_header-ekorg. "Organización de compras
* Inicio modificación T10000356517 EXTSMA01 20.07.2017
* p_ls_poheader-pur_group  = p_ls_header-ekgrp. "Grupo de compras
* Fin modificación T10000356517 EXTSMA01 20.07.2017

*Inicio modificacion EXTSMA01 T10000280727  09.10.2015
*Codigo Antiguo:
*  p_ls_poheader-currency   = p_ls_header-waers. "Clave de moneda
*Codigo Nuevo:
*Se recupera la moneda del contrato.
  IF p_ls_header-konnr IS NOT INITIAL.
    SELECT SINGLE waers INTO  p_ls_poheader-currency
      FROM ekko
      WHERE ebeln = p_ls_header-konnr.
  ELSE.
    p_ls_poheader-currency   = p_ls_header-waers.
  ENDIF.
*Fin Inicio modificacion EXTSMA01 T10000280727  09.10.2015

  "Marca los campos para que se tomen.
  p_ls_poheaderx-comp_code  = 'X'.
  p_ls_poheaderx-doc_type   = 'X'.
  p_ls_poheaderx-creat_date = 'X'.
  p_ls_poheaderx-created_by = 'X'.
  p_ls_poheaderx-vendor     = 'X'.
  p_ls_poheaderx-purch_org  = 'X'.
  p_ls_poheaderx-pur_group  = 'X'.
  p_ls_poheaderx-currency   = 'X'.

ENDFORM.                    " FILL_POHEADER

*&---------------------------------------------------------------------*
*&      Form  CREATE_PEDIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_purchase_order.

* Types
  TYPES: BEGIN OF ts_ekko,
           ebeln TYPE ekko-ebeln,
           kdatb TYPE ekko-kdatb,
           kdate TYPE ekko-kdate,
           konnr TYPE ekko-konnr,
           ktwrt TYPE ekko-ktwrt,
           waers TYPE ekko-WAERS, "EMAT-Enrique Aguirre-24.09.2020 - LFSENAG-558
           wkurs TYPE ekko-wkurs. "EMAT-Enrique Aguirre-24.09.2020 - LFSENAG-558
  TYPES: END OF ts_ekko.

  TYPES: BEGIN OF ts_ekab,
           konnr TYPE ekab-konnr,
           netwr TYPE ekab-netwr.
  TYPES: END OF ts_ekab.

* Tablas
  DATA: lt_ekko              TYPE TABLE OF ts_ekko,
        lt_ekab              TYPE TABLE OF ekab,
        lt_ekabt             TYPE TABLE OF ts_ekab,
        lt_eban              TYPE TABLE OF eban,
        lt_poitem            TYPE TABLE OF bapimepoitem,
        lt_poitemx           TYPE TABLE OF bapimepoitemx,
* INI - SGGMM - 27.04.2018
        lt_account           TYPE TABLE OF bapimepoaccount,
        lt_accountx          TYPE TABLE OF bapimepoaccountx,
        lt_poservices        TYPE TABLE OF bapiesllc,
        lt_posrvaccessvalues TYPE TABLE OF bapiesklc,
        lt_polimit           TYPE TABLE OF bapiesuhc,
        lt_pocontractlimits  TYPE TABLE OF bapiesucc,
* FIN - SGGMM - 27.04.2018
        lt_return            TYPE TABLE OF bapiret2.


"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Nuevo
DATA: p_flag_error_soamanager type c.
DATA: p_mensajeerror TYPE String.
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Nuevo

* Estructuras
  DATA: ls_ekab      TYPE ts_ekab,
        ls_ekab2     TYPE ts_ekab,
        ls_ekko      TYPE ts_ekko,
        ls_poheader  TYPE bapimepoheader,
        ls_poheaderx TYPE bapimepoheaderx,
        ls_return    TYPE bapiret2.

* Variables
  DATA: lv_preis   TYPE eban-preis,
        lv_konnr   TYPE eban-konnr,
        lv_poitem  TYPE bapimepoitem-po_item,
        lv_ktwrt   TYPE ekko-ktwrt,
        lv_difer   TYPE ekko-ktwrt,
        lv_preis1  TYPE bseg-dmbtr,
        lv_preis2  TYPE bseg-dmbtr,
        lv_menge   TYPE eban-menge,
        lv_tfill   TYPE sy-tfill,
        lv_maxline TYPE sy-dbcnt,
        lv_error   TYPE bapiflag-bapiflag,
        lv_testrun TYPE bapiflag-bapiflag.

  FIELD-SYMBOLS: <ls_items>  TYPE eban,
                 <ls_header> TYPE eban,
                 <ls_ekab>   TYPE ekab.

  "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert{
  DATA: lv_validar_presupuesto TYPE c LENGTH 1.
  "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert}

  CLEAR:ls_poheader,ls_poheaderx,ls_return,lv_testrun.
  REFRESH:lt_poitem,lt_poitemx,lt_return.

* INI - SGGMM - 27.04.2018
  CLEAR: gv_packno,
         gv_subpackno,
         gv_first.

  REFRESH: lt_account,
           lt_accountx,
           lt_poservices,
           lt_posrvaccessvalues,
           lt_polimit,
           lt_pocontractlimits.
* FIN - SGGMM - 27.04.2018

  CHECK gt_items[] IS NOT INITIAL.
  MOVE  gt_items[] TO gt_header[].

* Inicio modificacion T10000260645 EXT90884 12.06.2015
*  SORT gt_header BY lifnr lfdat werks konnr DESCENDING.
  SORT gt_header BY lifnr lfdat werks ekgrp ekorg konnr DESCENDING.
* Fin modificacion T10000260645 EXT90884 12.06.2015

*Inicio modificacion EXT90884 T10000220054 07.11.2014
*Codigo Antiguo:
*  DELETE ADJACENT DUPLICATES FROM gt_header
*    COMPARING lifnr lfdat werks.
  DELETE ADJACENT DUPLICATES FROM gt_header
    COMPARING lifnr lfdat werks ekgrp ekorg.
*Fin modificacion EXT90884 T10000220054 07.11.2014

  MOVE gt_items[] TO lt_eban[].
  SORT lt_eban BY konnr.

  DELETE ADJACENT DUPLICATES FROM lt_eban
  COMPARING konnr.

  SELECT ebeln kdatb kdate konnr ktwrt
         waers "EMAT- Enrique Aguirre - LFSENAG-558-24.09.2020
         wkurs "EMAT- Enrique Aguirre - LFSENAG-558-24.09.2020
    INTO TABLE lt_ekko
    FROM ekko
    FOR ALL ENTRIES IN lt_eban
    WHERE ebeln EQ lt_eban-konnr.
  SORT lt_ekko BY ebeln.

  IF lt_ekko[] IS NOT INITIAL.

    SELECT * INTO TABLE lt_ekab
     FROM ekab
     FOR ALL ENTRIES IN lt_ekko
     WHERE konnr EQ lt_ekko-ebeln.

    IF sy-subrc IS INITIAL.
      SORT lt_ekab.
      LOOP AT lt_ekab ASSIGNING <ls_ekab>.
        ls_ekab2-konnr = <ls_ekab>-konnr.
        ls_ekab2-netwr = <ls_ekab>-netwr.
        COLLECT ls_ekab2 INTO lt_ekabt.
      ENDLOOP.
      SORT lt_ekabt BY konnr.
    ENDIF.
  ENDIF.

  DESCRIBE TABLE gt_header LINES lv_maxline.
  LOOP AT gt_header ASSIGNING <ls_header>.

    CLEAR:ls_poheader,ls_poheaderx,lv_preis1,lv_poitem,lv_konnr,ls_return.
    REFRESH:lt_poitem,lt_poitemx,lt_return.
* INI - SGGMM - 27.04.2018
    REFRESH: lt_account,
             lt_accountx,
             lt_poservices,
             lt_posrvaccessvalues,
             lt_polimit,
             lt_pocontractlimits.
* FIN - SGGMM - 27.04.2018

    PERFORM sap_indicador USING text-001 lv_maxline.

    "Carga datos Cabecera para pedido compras
    PERFORM fill_poheader USING p_bsart2 <ls_header>
                          CHANGING ls_poheader ls_poheaderx.

    "Agrupa pedidos por centro, proveedor fecha entrega, grupo de compras
    "y organización de compras
    LOOP AT gt_items ASSIGNING <ls_items>
      WHERE werks EQ <ls_header>-werks
       AND  lfdat EQ <ls_header>-lfdat
       AND  lifnr EQ <ls_header>-lifnr
* Inicio modificacion T10000260645 EXT90884 12.06.2015
       AND  ekgrp EQ <ls_header>-ekgrp
       AND  ekorg EQ <ls_header>-ekorg
* Fin modificacion T10000260645 EXT90884 12.06.2015
       AND  konnr IS NOT INITIAL.

      READ TABLE lt_ekko  WITH KEY ebeln = <ls_items>-konnr
        INTO ls_ekko BINARY SEARCH.

      IF <ls_items>-lfdat GE ls_ekko-kdatb AND
         <ls_items>-lfdat LE ls_ekko-kdate.

        IF lv_konnr NE <ls_items>-konnr.
          CLEAR: lv_preis, lv_ktwrt.
        ENDIF.

        READ TABLE lt_ekabt WITH KEY konnr = <ls_items>-konnr
          INTO ls_ekab BINARY SEARCH.
* Inicio modificacion T10000183972 ILG 29/04/2014
* debemos dividir por la cantidad base.
*        lv_preis = ( <ls_items>-menge * <ls_items>-preis ) + lv_preis.
        "EMAT - Enrique Aguirre - LFSENAG-573 - 19.10.2020 Comentar
            "lv_preis = ( ( <ls_items>-menge * <ls_items>-preis ) / <ls_items>-peinh ) + lv_preis.
        "EMAT - Enrique Aguirre - LFSENAG-573 - 19.10.2020 Comentar
* Fin modificacion T10000183972 ILG 29/04/2014
        "EMAT - Enrique Aguirre - LFSENAG-558 - 24.09.2020
         DATA: ls_preis_USD TYPE eban-preis.
         DATA: itt_return TYPE BAPIRET1.
        IF <ls_items>-waers NE ls_ekko-waers
            and <ls_items>-preis GT 0.
         DATA: exch_rate type BAPI1093_0.
         CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
           EXPORTING
             rate_type        = 'M'
             from_curr        = ls_ekko-waers
             to_currncy       = <ls_items>-waers
             DATE             = sy-datum
          IMPORTING
            EXCH_RATE        = exch_rate
            RETURN           = itt_return.
         IF sy-subrc = 0.
            ls_preis_USD = <ls_items>-preis / exch_rate-exch_rate. "ls_ekko-wkurs.
         else.
           MESSAGE id '00' TYPE 'S' NUMBER '398' WITH 'No existe tipo de cambio USD para' <ls_items>-waers DISPLAY LIKE 'E'.
         ENDIF.
          "ls_ekko-wkurs.
        ELSE.
          ls_preis_USD = <ls_items>-preis.
        ENDIF.
        "EMAT - Enrique Aguirre - LFSENAG-558 - 24.09.2020

         "EMAT - Enrique Aguirre - LFSENAG-573 - 19.10.2020
          lv_preis = ( ( <ls_items>-menge * ls_preis_USD ) / <ls_items>-peinh ) + lv_preis.
         "EMAT - Enrique Aguirre - LFSENAG-573 - 19.10.2020


        "EMAT - Enrique Aguirre - LFSENAG-558 - 24.09.2020 - Replace
          "lv_ktwrt = lv_preis + ls_ekab-netwr.
          lv_ktwrt = lv_preis + ls_ekab-netwr.
        "EMAT - Enrique Aguirre - LFSENAG-558 - 24.09.2020 - Replace

        lv_konnr = <ls_items>-konnr.

        IF lv_ktwrt LE ls_ekko-ktwrt.

          lv_poitem = lv_poitem + 10.
          "Carga datos posicion para pedido compras.
          PERFORM fill_poitem TABLES lt_poitem lt_poitemx
* INI - SGGMM - 27.04.2018
                                     lt_account    lt_accountx
                                     lt_poservices lt_posrvaccessvalues
                                     lt_polimit    lt_pocontractlimits
* FIN - SGGMM - 27.04.2018
                              USING  <ls_items>  lv_poitem.
        ELSE.
          ls_return-type = 'E'.
          ls_return-id = '06'.
          ls_return-number = '042'.
          lv_difer = lv_ktwrt - ls_ekko-ktwrt.
          MESSAGE e042(06) INTO ls_return-message
            WITH <ls_items>-konnr lv_difer.
          "El valor previsto del pedido abierto se ha excedido en
          APPEND ls_return TO lt_return.

  "EMAT - Enrique Aguirre - LFSENAG-573 -19.10.2020 - I
          DATA: aux_return TYPE TABLE OF bapiret2.
          aux_return = lt_return.
  "EMAT - Enrique Aguirre - LFSENAG-573 -19.10.2020 - I

          PERFORM get_log TABLES lt_return
                        USING  <ls_items> 'X'.
        ENDIF.

      ELSE.
        ls_return-type = 'E'.
        ls_return-id = 'ZSGM'.
        ls_return-number = '034'.
        MESSAGE e034(zsgm) INTO ls_return-message
          WITH <ls_items>-konnr ls_ekko-kdate.
        "El periodo de validez del pedido abierto finaliza el
        APPEND ls_return TO lt_return.

        PERFORM get_log TABLES lt_return
                        USING  <ls_items> 'X'.
      ENDIF.
    ENDLOOP.

  "EMAT - Enrique Aguirre - LFSENAG-573 -19.10.2020 - I
    READ TABLE aux_return WITH KEY number = '042' TYPE = 'E' TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0.
     CLEAR aux_return.
     CONTINUE.
    ENDIF.
  "EMAT - Enrique Aguirre - LFSENAG-573 - 19.10.2020 - I


    IF lt_poitem[] IS NOT INITIAL.

      "Clase de documento de compras ZPCM
      ls_poheader-doc_type = p_bsart1.
      "Crea pedido de compras.
      "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert{
      PERFORM validar_presupuesto TABLES lt_poitem
                                         lt_poservices
                                         lt_account
                                  USING  ls_poheader
                                  CHANGING lv_validar_presupuesto
                                           gw_output
                                           gw_input
                                           p_flag_error_soamanager
                                           p_mensajeerror.
      "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert}
      IF lv_validar_presupuesto EQ 'X' "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert
"Si hay error de conexion con el webservice igual se ejecuta la BAPI
    OR p_flag_error_soamanager EQ 'X'. "EMAT - Enrique Aguirre - LFSENAG-473 - 01.06.2020
"Si hay error de conexion con el webservice igual se ejecuta la BAPI
        PERFORM bapi_po_create1 TABLES lt_poitem lt_poitemx lt_return
* INI - SGGMM - 27.04.2018
                                       lt_account    lt_accountx
                                       lt_poservices lt_posrvaccessvalues
                                       lt_polimit    lt_pocontractlimits
* FIN - SGGMM - 27.04.2018
                                USING  ls_poheader ls_poheaderx space c_memory
                                CHANGING
                                       p_flag_error_soamanager
                                       p_mensajeerror.


      ENDIF.

      PERFORM get_log TABLES lt_return
                      USING  <ls_items> space.
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Nuevo
IF p_flag_error_soamanager EQ 'X'.
   CONCATENATE 'Adv: Pedido ' ls_poheader-po_number ' Error Conexión:'
                space sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4)
                INTO Data(_lv_asunto) RESPECTING BLANKS.
   PERFORM enviar_correo_errores USING gw_output _lv_asunto p_mensajeerror.
ENDIF.
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Nuevo

      REFRESH:lt_poitem,lt_poitemx,lt_return.

* INI - SGGMM - 27.04.2018
      REFRESH: lt_account,
               lt_accountx,
               lt_poservices,
               lt_posrvaccessvalues,
               lt_polimit,
               lt_pocontractlimits.
* FIN - SGGMM - 27.04.2018

    ENDIF.

    LOOP AT gt_items ASSIGNING <ls_items>
      WHERE  werks EQ <ls_header>-werks
        AND  lfdat EQ <ls_header>-lfdat
        AND  lifnr EQ <ls_header>-lifnr
* Inicio modificacion T10000260645 EXT90884 12.06.2015
        AND  ekgrp EQ <ls_header>-ekgrp
        AND  ekorg EQ <ls_header>-ekorg
* Fin modificacion T10000260645 EXT90884 12.06.2015
        AND  konnr EQ space.

      IF <ls_items>-infnr IS NOT INITIAL.

        SELECT SINGLE netpr FROM eine
          INTO lv_menge
          WHERE infnr EQ <ls_items>-infnr
          AND   ekorg EQ <ls_items>-ekorg
          AND   werks EQ <ls_items>-werks
          AND   netpr NE 0.

        IF sy-subrc IS NOT INITIAL.
          lv_menge = <ls_items>-menge.
        ENDIF.

      ELSE.
        lv_menge = <ls_items>-menge.
      ENDIF.
* Inicio modificacion T10000183972 ILG 29/04/2014
* debemos dividir por la cantidad base.
*      lv_preis2 = ( lv_menge * <ls_items>-preis ).
*      lv_preis1 = ( lv_menge * <ls_items>-preis ) + lv_preis1.
      lv_preis2 = ( lv_menge * <ls_items>-preis ) / <ls_items>-peinh.
      lv_preis1 = ( ( lv_menge * <ls_items>-preis ) / <ls_items>-peinh ) + lv_preis1.
* Fin modificacion T10000183972 29/04/2014
      IF lv_preis2 LT gv_zcod.
        IF lv_preis1 LT gv_zcod.

          lv_poitem = lv_poitem + 10.
          "Carga datos posicion para pedido compras
          PERFORM fill_poitem TABLES lt_poitem lt_poitemx
* INI - SGGMM - 27.04.2018
                                     lt_account    lt_accountx
                                     lt_poservices lt_posrvaccessvalues
                                     lt_polimit    lt_pocontractlimits
* FIN - SGGMM - 27.04.2018
                              USING  <ls_items>  lv_poitem.

        ELSE.

          DESCRIBE TABLE lt_poitem LINES lv_tfill.
          IF lv_tfill EQ 1.
            lv_error = 'X'.
          ELSE.
            lv_error = space.
          ENDIF.

          "Clase de documento de compras ZPM
          ls_poheader-doc_type = p_bsart2.

          "Crea pedido de compras.
          "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert{
          PERFORM validar_presupuesto TABLES lt_poitem
                                             lt_poservices
                                             lt_account
                                       USING  ls_poheader
                                      CHANGING lv_validar_presupuesto
                                               gw_output
                                               gw_input
                                               p_flag_error_soamanager
                                               p_mensajeerror.

          "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert}
          IF lv_validar_presupuesto EQ 'X' "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert
"Si hay error de conexion con el webservice igual se ejecuta la BAPI
    OR p_flag_error_soamanager EQ 'X'. "EMAT - Enrique Aguirre - LFSENAG-473 - 01.06.2020
"Si hay error de conexion con el webservice igual se ejecuta la BAPI


            PERFORM bapi_po_create1 TABLES lt_poitem lt_poitemx lt_return
* INI - SGGMM - 27.04.2018
                                           lt_account    lt_accountx
                                           lt_poservices lt_posrvaccessvalues
                                           lt_polimit    lt_pocontractlimits
* FIN - SGGMM - 27.04.2018
                                    USING  ls_poheader ls_poheaderx space c_memory
                                    CHANGING
                                             P_FLAG_ERROR_SOAMANAGER
                                             p_mensajeerror.


          ENDIF.
          PERFORM get_log TABLES lt_return
                          USING  <ls_items> lv_error.

"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Nuevo
IF p_flag_error_soamanager EQ 'X'.
   CONCATENATE 'Adv: Pedido ' ls_poheader-po_number ' Error Conexión:'
                space sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4)
                INTO _lv_asunto RESPECTING BLANKS.
   PERFORM enviar_correo_errores USING gw_output _lv_asunto p_mensajeerror.
ENDIF.
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Nuevo


          CLEAR: lv_preis1,lv_poitem.
          REFRESH: lt_poitem,lt_poitemx.
* INI - SGGMM - 27.04.2018
          REFRESH: lt_account,
                   lt_accountx,
                   lt_poservices,
                   lt_posrvaccessvalues,
                   lt_polimit,
                   lt_pocontractlimits.
* FIN - SGGMM - 27.04.2018
          ADD 10 TO lv_poitem.
* Inicio modificacion T10000183972 ILG 29/04/2014
* debemos dividir por la cantidad base.
*          lv_preis1 = ( lv_menge * <ls_items>-preis ) + lv_preis1.
          lv_preis1 = ( ( lv_menge * <ls_items>-preis ) / <ls_items>-peinh ) + lv_preis1.
* fin modificacion T10000183972 ILG 29/04/2014

          "Carga datos posicion para pedido compras.
          PERFORM fill_poitem TABLES lt_poitem lt_poitemx
* INI - SGGMM - 27.04.2018
                                     lt_account    lt_accountx
                                     lt_poservices lt_posrvaccessvalues
                                     lt_polimit    lt_pocontractlimits
* FIN - SGGMM - 27.04.2018
                              USING  <ls_items>  lv_poitem.

        ENDIF.
      ELSE.
        "Se ha superado la compra en plaza
        ls_return-type = 'E'.
        ls_return-id = 'ZSGM'.
        ls_return-number = '071'.
        lv_difer = lv_ktwrt - ls_ekko-ktwrt.
        MESSAGE e071(zsgm) INTO ls_return-message.
        APPEND ls_return TO lt_return.

        PERFORM get_log TABLES lt_return
                        USING  <ls_items> 'X'.
      ENDIF.

    ENDLOOP.

    IF lt_poitem[] IS NOT INITIAL.

      "Clase de documento de compras ZPM
      ls_poheader-doc_type = p_bsart2.

      "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert{
      PERFORM validar_presupuesto TABLES lt_poitem
                                         lt_poservices
                                         lt_account
                                  USING  ls_poheader
                                  CHANGING lv_validar_presupuesto
                                           gw_output
                                           gw_input
                                           p_flag_error_soamanager
                                           p_mensajeerror.

      "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert}
      IF lv_validar_presupuesto EQ 'X' "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert
"Si hay error de conexion con el webservice igual se ejecuta la BAPI
    OR p_flag_error_soamanager EQ 'X'. "EMAT - Enrique Aguirre - LFSENAG-473 - 01.06.2020
"Si hay error de conexion con el webservice igual se ejecuta la BAPI


        "Crea pedido de compras.
        PERFORM bapi_po_create1 TABLES lt_poitem lt_poitemx lt_return
* INI - SGGMM - 27.04.2018
                                       lt_account    lt_accountx
                                       lt_poservices lt_posrvaccessvalues
                                       lt_polimit    lt_pocontractlimits
* FIN - SGGMM - 27.04.2018
                                USING  ls_poheader ls_poheaderx space c_memory
                                CHANGING
                                        P_FLAG_ERROR_SOAMANAGER
                                        p_mensajeerror.


      ENDIF.
      PERFORM get_log TABLES lt_return
                      USING  <ls_items> space.

"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Nuevo
IF p_flag_error_soamanager EQ 'X'.
   CONCATENATE 'Adv: Pedido ' ls_poheader-po_number ' Error Conexión:'
                space sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4)
                INTO _lv_asunto RESPECTING BLANKS.
   PERFORM enviar_correo_errores USING gw_output _lv_asunto p_mensajeerror.
ENDIF.
"EMAT - Enrique Aguirre - LFSENAG-473 - 27.05.2020 - Nuevo

    ENDIF.

  ENDLOOP.
ENDFORM.                    " CREATE_PEDIDO1

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_RETURN  text
*----------------------------------------------------------------------*
FORM get_log  TABLES p_lt_return STRUCTURE bapiret2
              USING  p_ls_header TYPE eban
                     p_lv_error  TYPE bapiflag-bapiflag.

  DATA: ls_log    TYPE zbapiret2,
        ls_return TYPE bapiret2,
        lt_return TYPE TABLE OF bapiret2.

  CONSTANTS: lc_green  TYPE icon-name VALUE '@08@', "S Success
             lc_yellow TYPE icon-name VALUE '@09@', "W Warning
             lc_red    TYPE icon-name VALUE '@0A@'. "E Error

  IF p_lt_return[] IS NOT INITIAL.

    MOVE p_lt_return[] TO lt_return[].

    LOOP AT lt_return INTO ls_return
      WHERE type EQ 'E' OR
            type EQ 'W' OR
            type EQ 'S'.

      IF ls_return-type = 'E'.
        ls_log-icon = lc_red.
      ELSEIF ls_return-type = 'W'.
        ls_log-icon = lc_yellow.
      ELSEIF ls_return-type = 'S'.
        ls_log-icon = lc_green.
      ENDIF.

      MOVE-CORRESPONDING ls_return TO ls_log.
      ls_log-werks = p_ls_header-werks.
      ls_log-lfdat = p_ls_header-lfdat.
      ls_log-lifnr = p_ls_header-lifnr.

      IF p_lv_error IS NOT INITIAL.
        ls_log-banfn = p_ls_header-banfn.
        ls_log-bnfpo = p_ls_header-bnfpo.
      ENDIF.

      APPEND ls_log TO gt_log.

    ENDLOOP.
  ENDIF.

  CLEAR: p_lt_return[].

*  "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert{.
  IF line_exists( gw_input-mt_consulta_presupuesto_res-datos[ OK = abap_false ] ).
*  IF NOT gw_input-mt_consulta_presupuesto_res-datos IS INITIAL.

    LOOP AT gw_input-mt_consulta_presupuesto_res-mensajes ASSIGNING FIELD-SYMBOL(<mensajes>).
      ls_log-icon = lc_red.
      ls_log-werks = p_ls_header-werks.
      ls_log-lfdat = p_ls_header-lfdat.
      ls_log-lifnr = p_ls_header-lifnr.
      ls_log-message = <mensajes>-descripcion_mensaje.
      APPEND ls_log TO gt_log.
    ENDLOOP.

    DATA: lv_asunto TYPE string.
    READ TABLE gw_input-mt_consulta_presupuesto_res-mensajes ASSIGNING FIELD-SYMBOL(<mensajes2>) INDEX 1.
    IF sy-subrc EQ 0.
      IF NOT gv_purchaseorder IS INITIAL.
        CONCATENATE 'Log de error por falta de presupuesto' space gv_purchaseorder sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4)  INTO lv_asunto RESPECTING BLANKS.
      ELSE.
        CONCATENATE 'Log de error por falta de presupuesto' space  sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4)  INTO lv_asunto RESPECTING BLANKS.
      ENDIF.

      PERFORM enviar_correo_errores USING gw_output lv_asunto ''.

    ENDIF.
  ENDIF.
*  "JRCL – Johan Cochachi – LFSENAG-473 – 27.04.2020 – insert}
*
ENDFORM.                    " DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Form  CATALOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM catalog.

  DATA: lt_fieldcat TYPE lvc_t_fcat.

  FIELD-SYMBOLS: <ls_fieldcat> TYPE lvc_s_fcat.

  "Crea Catalogo para ALV
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = 'ZBAPIRET2'
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
*     I_INTERNAL_TABNAME     =
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_fieldcat ASSIGNING <ls_fieldcat>.

    CASE <ls_fieldcat>-fieldname.

      WHEN 'ICON'.
        <ls_fieldcat>-icon = 'X'.
        <ls_fieldcat>-fix_column = 'X'.

      WHEN 'TYPE'.
        <ls_fieldcat>-fix_column = 'X'.
        <ls_fieldcat>-no_out = 'X'.

      WHEN 'ID'.
        <ls_fieldcat>-fix_column = 'X'.

      WHEN 'NUMBER'.
        <ls_fieldcat>-fix_column = 'X'.

      WHEN 'WERKS'.
        <ls_fieldcat>-fix_column = 'X'.

      WHEN 'LFDAT'.
        <ls_fieldcat>-fix_column = 'X'.

      WHEN 'LIFNR'.
        <ls_fieldcat>-fix_column = 'X'.

      WHEN 'BANFN'.
        <ls_fieldcat>-fix_column = 'X'.
        <ls_fieldcat>-hotspot = 'X'.

      WHEN 'BNFPO'.
        <ls_fieldcat>-fix_column = 'X'.

      WHEN 'MESSAGE'.
        <ls_fieldcat>-hotspot = 'X'.

      WHEN 'MESSAGE_V2'.
        <ls_fieldcat>-hotspot = 'X'.

    ENDCASE.

    APPEND <ls_fieldcat> TO gt_fieldcat.

  ENDLOOP.
ENDFORM.                    " CATALOGO
*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM layout .

  "Crea layout para ALV
  gs_layout-cwidth_opt = 'A'.           "Ancho columna justo
  gs_layout-col_opt    = 'X'.
  gs_layout-frontend   = 'X'.
  gs_layout-no_f4      = space.
  gs_layout-no_rowmark = space.
  gs_layout-edit       = space.
  gs_layout-zebra      = 'X'.           "Color de linea alternante
  gs_layout-sel_mode   = 'A'.           "Selección múltiple


ENDFORM.                    " LAYOUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display.

* Creating instances for the event handler.
  CREATE OBJECT go_event_receiver.
  SET HANDLER go_event_receiver->handle_hotspot_click FOR gc_grid.

* Variant for ALV layout
  PERFORM prepare_variant.

  SORT gt_log BY werks lfdat lifnr banfn bnfpo.

  "Muestra ALV con tabla de LOG
  CALL METHOD gc_grid->set_table_for_first_display
    EXPORTING
      i_buffer_active               = space
*     i_bypassing_buffer            =
      i_consistency_check           = space
*     i_structure_name              =
      is_variant                    = gs_variant
      i_save                        = 'A'
      i_default                     = 'X'
      is_layout                     = gs_layout
*     is_print                      =
*     it_special_groups             =
*     it_toolbar_excluding          =
*     it_hyperlink                  =
*     it_alv_graphics               =
*     it_except_qinfo               =
*     ir_salv_adapter               =
    CHANGING
      it_outtab                     = gt_log   "Tabla de log
      it_fieldcatalog               = gt_fieldcat
*     it_sort                       =
*     it_filter                     =
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY

*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_event_receiver
*&---------------------------------------------------------------------*
*        The following events have been implemented
*        - hotspot_click
*----------------------------------------------------------------------*
CLASS cl_event_receiver IMPLEMENTATION.

  METHOD handle_hotspot_click.

    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no.

  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.               "lcl_event_receiver
*&---------------------------------------------------------------------*
*&      Form  handle_hotspot_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM handle_hotspot_click  USING p_e_row     TYPE lvc_s_row
                                 p_e_column  TYPE lvc_s_col
                                 p_es_row_no TYPE lvc_s_roid.

  DATA: lv_object TYPE doku_obj.

  FIELD-SYMBOLS: <ls_log> TYPE zbapiret2.

  READ TABLE gt_log INDEX p_e_row-index ASSIGNING <ls_log>.

  IF <ls_log> IS ASSIGNED.
    IF <ls_log>-message_v2 IS NOT INITIAL AND
       <ls_log>-type EQ 'S' AND
       p_e_column EQ 'MESSAGE_V2'.

      "Tcode para visualizar pedidos de compras
      SET PARAMETER ID 'BES' FIELD <ls_log>-message_v2.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

    ELSEIF <ls_log>-banfn IS NOT INITIAL AND
      p_e_column EQ 'BANFN'.

      "Tcode para visualizar solicitudes de pedidos
      SET PARAMETER ID 'BAN' FIELD <ls_log>-banfn.
      CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.

    ELSEIF <ls_log>-message IS NOT INITIAL AND
           p_e_column EQ 'MESSAGE'.

      "Visualiza text explicativo del mensaje
      CONCATENATE <ls_log>-id <ls_log>-number INTO lv_object.
      CALL FUNCTION 'DOCU_CALL'
        EXPORTING
          id            = 'NA'
          object        = lv_object
          displ         = 'X'
          displ_mode    = '2'
          langu         = sy-langu
          use_sec_langu = 'X'
          typ           = 'E'.

    ENDIF.
  ENDIF.

ENDFORM.                    " handle_hotspot_click

*----------------------------------------------------------------------*
*      -->P_IF_SALV_C_LAYOUT=>RESTRICT_NON  text
*      <--P_P_LAYOUT  text
*----------------------------------------------------------------------*
FORM f4_layouts  USING    p_restrict  TYPE salv_de_layout_restriction
                 CHANGING p_layout    TYPE disvariant-variant.

  DATA: ls_layout TYPE salv_s_layout_info,
        ls_key    TYPE salv_s_layout_key.

  CLEAR: ls_layout,ls_key.

  ls_key-report = sy-repid.
  ls_layout     = cl_salv_layout_service=>f4_layouts(
       s_key    = ls_key  restrict = p_restrict ).

  "Parametro de Seleccion para Layout
  p_layout = ls_layout-layout.

ENDFORM.                    " F4_LAYOUTS

*&---------------------------------------------------------------------*
*&      Form  PREPARE_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_variant .

* Fill variant structure
  CLEAR gs_variant.

  gs_variant-report   = sy-repid.
  gs_variant-variant  = p_layout.
  gs_variant-username = sy-uname.

ENDFORM.                    " PREPARE_VARIANT
*&---------------------------------------------------------------------*
*&      Form  SAP_INDICADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_001  text
*----------------------------------------------------------------------*
FORM sap_indicador  USING  p_text TYPE c
                           p_maxline TYPE sy-dbcnt.

  DATA: lv_text       TYPE string,
        lv_percentage TYPE p.

  lv_percentage = sy-tabix * 100 / p_maxline.
  lv_text = lv_percentage.

  CONCATENATE p_text lv_text '%' INTO lv_text SEPARATED BY space.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = lv_percentage
      text       = lv_text.

ENDFORM.                    " SAP_INDICADOR
* Inicio modificacion T10000198576 LLCB 14/07/2014
*&---------------------------------------------------------------------*
*&      Form  DELETE_SRM
*&---------------------------------------------------------------------*
*       Subrutina to delete PR's with Goods Recipient equal to SRM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_srm .

* Declare local variables
  DATA: lt_items TYPE TABLE OF eban,
        ls_items TYPE eban.

  DATA: BEGIN OF lt_ebkn OCCURS 0,
          banfn TYPE banfn,
          bnfpo TYPE bnfpo,
        END OF lt_ebkn.


  CLEAR: ls_items,
         lt_ebkn.

  REFRESH: lt_items,
           lt_ebkn.

  CONSTANTS: lc_recip TYPE wempf VALUE 'SRM'.

  CHECK NOT gt_items[] IS INITIAL.

* Copy items to local table
  lt_items[] = gt_items[].

* Remove global table
  REFRESH gt_items.
  CLEAR gt_items.

* Select Goods Recipients equal to SRM
  SELECT banfn
         bnfpo
    FROM ebkn
  INTO TABLE lt_ebkn
  FOR ALL ENTRIES IN lt_items
    WHERE banfn  EQ lt_items-banfn
      AND bnfpo  EQ lt_items-bnfpo
      AND wempf  EQ lc_recip.

  IF sy-subrc EQ 0.
* Sort table
    SORT lt_ebkn BY banfn ASCENDING bnfpo ASCENDING.

* Delete duplicates
    DELETE ADJACENT DUPLICATES FROM lt_ebkn.

    CLEAR: lt_ebkn.

* Process items
    LOOP AT lt_items INTO ls_items.

      READ TABLE lt_ebkn WITH KEY banfn = ls_items-banfn
                                  bnfpo = ls_items-bnfpo.

      IF sy-subrc NE 0.
        APPEND ls_items TO gt_items.
      ENDIF.

      CLEAR: ls_items,
             lt_ebkn.

    ENDLOOP.
  ELSE.
    gt_items[] = lt_items[].
  ENDIF.

  IF NOT gt_items[] IS INITIAL.
    SORT gt_items BY banfn ASCENDING bnfpo ASCENDING.
  ENDIF.

  FREE: lt_items,
        lt_ebkn.

ENDFORM.                    " DELETE_SRM
* Fin modificacion T10000198576 LLCB 14/07/2014
* Inicio modificacion T10000203015 LLCB 01/09/2014
*&---------------------------------------------------------------------*
*&      Form  DELETE_SOLPED_ENDS
*&---------------------------------------------------------------------*
*  Subrutina para discriminar solicitudes que tengan Ind. conclusión
*  informado (EBAN-EBAKZ) o bien que la cantidad pedida es mayor o
*  igual que la cantidad de la solicitud (EBAN-BSMNG => EBAN-MENGE)
*----------------------------------------------------------------------*
FORM delete_solped_ends .

* Declare local variables
  DATA: lt_items TYPE TABLE OF eban,
        ls_items TYPE eban.

  DATA: lv_menge TYPE eban-menge.

  REFRESH lt_items.
  CLEAR: ls_items,
         lv_menge.

  CHECK NOT gt_items[] IS INITIAL.

* Copy items to local table
  lt_items[] = gt_items[].

* Remove global table
  REFRESH gt_items.
  CLEAR gt_items.

* Process items
  LOOP AT lt_items INTO ls_items.

    CASE ls_items-ebakz.
      WHEN 'X'.           " Ind. conclusión Informado
        CLEAR ls_items.
      WHEN OTHERS.        " Ind. conclusión No Informado
*    Check quantities
        IF ls_items-bsmng LT ls_items-menge.

*     Calculate ammount difference and update item structure
          lv_menge = ls_items-menge - ls_items-bsmng.
          MOVE lv_menge TO ls_items-menge.

          APPEND ls_items TO gt_items.
          CLEAR: ls_items.

        ENDIF.
    ENDCASE.
    CLEAR: lv_menge.
  ENDLOOP.

  IF NOT gt_items[] IS INITIAL.
    SORT gt_items BY banfn ASCENDING bnfpo ASCENDING.
  ENDIF.

  FREE: lt_items.
ENDFORM.                    " DELETE_SOLPED_ENDS
* Fin modificacion T10000203015 LLCB 01/09/2014
* Inicio modificación T10000252571 EXT90884 04.05.2015
*&---------------------------------------------------------------------*
*&      Form  DELETE_SOLPED_CONDPRECIO
*&---------------------------------------------------------------------*
* Subrutina para eliminar las solicitudes cuyos servicios se encuentren
* fuera del periodo de validez de la condición de precios
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_solped_condprecio.

* Tipos
  TYPES: BEGIN OF ty_solicitud,
           banfn TYPE eban-banfn,
           bnfpo TYPE eban-bnfpo,
           konnr TYPE eban-konnr,
         END OF ty_solicitud.

  DATA: lv_intervalos        TYPE i,
        lv_error             TYPE c,

        ls_return            TYPE bapiret2,
        lw_solic_invalida    TYPE ty_solicitud,
        lw_items             TYPE eban,
        lw_items_solc        TYPE bapieban,
        lw_services_solc     TYPE bapiesll,
        lw_services_solc_aux TYPE bapiesll,
        lw_header_contrato   TYPE bapiekkol,
        lw_address           TYPE bapiaddress,
        lw_services_contrato TYPE bapiesll,

        lt_solic_invalida    TYPE STANDARD TABLE OF ty_solicitud,
        lt_items             TYPE TABLE OF eban,
        lt_items_solc        TYPE TABLE OF bapieban,
        lt_services_solc     TYPE TABLE OF bapiesll,
        lt_items_contrato    TYPE TABLE OF bapiekpo,
        lt_services_contrato TYPE TABLE OF bapiesll,
        lt_a081              TYPE STANDARD TABLE OF a081,
        lt_return            TYPE TABLE OF bapiret2.

  CLEAR: lv_intervalos, lv_error, ls_return, lw_solic_invalida,
         lw_items, lw_items_solc, lw_services_solc, lw_services_solc_aux,
         lw_header_contrato, lw_address, lw_services_contrato .

  REFRESH: lt_solic_invalida, lt_items, lt_items_solc, lt_services_solc,
           lt_items_contrato, lt_services_contrato, lt_a081, lt_return.

  IF gt_items[] IS NOT INITIAL.
    lt_items[] = gt_items[].

    SORT lt_items BY banfn ASCENDING bnfpo ASCENDING.

    LOOP AT lt_items INTO lw_items.
      IF lw_items-konnr IS NOT INITIAL.
        CLEAR: lw_header_contrato, lw_address.
        REFRESH: lt_items_contrato, lt_services_contrato.
* BAPI para obtener los detalles del contrato
        CALL FUNCTION 'BAPI_PO_GETDETAIL'
          EXPORTING
            purchaseorder    = lw_items-konnr
            items            = 'X'
            services         = 'X'
          IMPORTING
            po_header        = lw_header_contrato
            po_address       = lw_address
          TABLES
            po_items         = lt_items_contrato
            po_item_services = lt_services_contrato.


        REFRESH: lt_items_solc, lt_services_solc.
* BAPI para obtener los detalles de la solicitud
        CALL FUNCTION 'BAPI_REQUISITION_GETDETAIL'
          EXPORTING
            number               = lw_items-banfn
            services             = 'X'
          TABLES
            requisition_items    = lt_items_solc
            requisition_services = lt_services_solc.

        IF lt_services_solc[] IS NOT INITIAL.
          CLEAR: lw_items_solc, lw_services_solc_aux.
*         Se obtiene el paquete de la solicitud / posición
          READ TABLE lt_items_solc INTO lw_items_solc
            WITH KEY preq_no = lw_items-banfn
                     preq_item = lw_items-bnfpo.
          IF sy-subrc EQ 0.
*           Se obtiene el paquete que contiene los servicos de la posición
            READ TABLE lt_services_solc INTO lw_services_solc_aux
              WITH KEY pckg_no = lw_items_solc-pckg_no.
          ENDIF.

          CLEAR: lw_services_solc.
*         Se recorren los servicios contenidos en el subpaquete
          LOOP AT lt_services_solc INTO lw_services_solc
            WHERE pckg_no EQ lw_services_solc_aux-subpckg_no
              AND service IS NOT INITIAL.

            CLEAR: lw_services_contrato.
            READ TABLE lt_services_contrato INTO lw_services_contrato
              WITH KEY service = lw_services_solc-service.

            IF sy-subrc EQ 0.
              REFRESH: lt_a081.

              SELECT *
                FROM a081
                INTO TABLE lt_a081
                WHERE kont_pack = lw_services_contrato-pckg_no
                AND   kont_zeile = lw_services_contrato-line_no.

              IF sy-subrc EQ 0.
                CLEAR: lv_intervalos.
                DESCRIBE TABLE lt_a081 LINES lv_intervalos.

                IF lv_intervalos > 1.
                  CLEAR: lv_error.

                  LOOP AT lt_a081 TRANSPORTING NO FIELDS
                    WHERE datbi EQ '99991231'
                    AND   datab LE lw_items-lfdat. "fecha de entrega

                    lv_error = 'X'.
                    EXIT.
                  ENDLOOP.

                  IF lv_error IS NOT INITIAL.
                    CLEAR: lw_solic_invalida.
                    lw_solic_invalida-banfn = lw_items-banfn.
                    lw_solic_invalida-bnfpo = lw_items-bnfpo.
                    lw_solic_invalida-konnr = lw_items-konnr.

                    APPEND lw_solic_invalida TO lt_solic_invalida.

*                 Se informa el log con el error.
*                 Precio no válido. Condición de precios finaliza 31.12.9999.
                    CLEAR: ls_return.
                    ls_return-type = 'E'.
                    ls_return-id = 'ZSGM'.
                    ls_return-number = '111'.
                    MESSAGE e111(zsgm) INTO ls_return-message.
                    APPEND ls_return TO lt_return.

                    PERFORM get_log TABLES lt_return
                                    USING  lw_items 'X'.

                    CLEAR: lv_error.
*                   Ya no se comprueban mas servicios, se pasa
*                   a la siguiente solicitud
                    EXIT.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

            CLEAR: lw_services_solc.
          ENDLOOP.
        ENDIF.
      ENDIF.

      CLEAR: lw_items.
    ENDLOOP.

* Se eliminan aquellas solicitudes que no han pasado la validación.
    CLEAR: lw_solic_invalida.

    LOOP AT lt_solic_invalida INTO lw_solic_invalida.
      DELETE gt_items WHERE banfn EQ lw_solic_invalida-banfn
                      AND   bnfpo EQ lw_solic_invalida-bnfpo.
      CLEAR: lw_solic_invalida.
    ENDLOOP.

    SORT gt_items BY banfn ASCENDING bnfpo ASCENDING.
    FREE: lt_items.
  ENDIF.
ENDFORM.                    " DELETE_SOLPED_CONDPRECIO
* Fin modificación T10000252571 EXT90884 04.05.2015
* Inicio modificación T10000252571 EXTSMA01 16.09.2015
*&---------------------------------------------------------------------*
*&      Form  F_TRATAR_COND_PAGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_POITEM  text
*      <--P_P_LS_POHEADER  text
*      <--P_P_LS_POHEADERX  text
*----------------------------------------------------------------------*
FORM f_tratar_cond_pago  TABLES   p_lt_poitem STRUCTURE bapimepoitem
                         CHANGING p_ls_poheader  TYPE bapimepoheader
                                  p_ls_poheaderx TYPE bapimepoheaderx.

  CONSTANTS: lc_cp     TYPE zdat VALUE 'CP',
             lc_cd     TYPE zdat VALUE 'CD',
             lc_14     TYPE zdat VALUE '14',
             lc_01     TYPE zlevel VALUE '01',
             lc_report TYPE zrepid VALUE 'ZMEPOHEADER_PMNTTRMS'.

  DATA: lr_condcpago    TYPE RANGE OF zu001-zcod,
        lw_condcpago    LIKE LINE OF lr_condcpago,
        lr_doccompras   TYPE RANGE OF zu001-zcod,
        lr_centro       TYPE RANGE OF zu001-zcod,
        lw_poitem       LIKE LINE OF p_lt_poitem,
        lv_pedidopuntal TYPE c,
        lv_condcpago    TYPE bapimepoheader-pmnttrms.

  CLEAR: lw_condcpago, lw_poitem, lv_pedidopuntal, lv_condcpago.

* Recupero la 'Condición de pago' para el report
  CLEAR: lr_condcpago, lr_condcpago[].
  CALL FUNCTION 'Z_HARD_CODES_1'
    EXPORTING
      p_zrepid       = lc_report
      p_zdat         = lc_cp
      p_zlevel       = lc_01
    TABLES
      t_values_range = lr_condcpago
    EXCEPTIONS
      no_data        = 1
      OTHERS         = 2.

* Recupero la 'Clase documento compras' para el report
  CLEAR: lr_doccompras, lr_doccompras[].
  CALL FUNCTION 'Z_HARD_CODES_1'
    EXPORTING
      p_zrepid       = lc_report
      p_zdat         = lc_cd
      p_zlevel       = lc_01
    TABLES
      t_values_range = lr_doccompras
    EXCEPTIONS
      no_data        = 1
      OTHERS         = 2.

* Recupero el 'Centro' para el report
  CLEAR: lr_centro, lr_centro[].
  CALL FUNCTION 'Z_HARD_CODES_1'
    EXPORTING
      p_zrepid       = lc_report
      p_zdat         = lc_14
      p_zlevel       = lc_01
    TABLES
      t_values_range = lr_centro
    EXCEPTIONS
      no_data        = 1
      OTHERS         = 2.

  IF p_lt_poitem[] IS NOT INITIAL.
    CLEAR: lw_poitem.
    READ TABLE p_lt_poitem INTO lw_poitem INDEX 1.
  ENDIF.

  IF p_ls_poheader-doc_type IN lr_doccompras  "Clase documento compras
    AND lw_poitem-plant IN lr_centro. "Centro
    lv_pedidopuntal = 'X'.
  ENDIF.

  IF lv_pedidopuntal IS NOT INITIAL.
    CLEAR: lw_condcpago, lv_condcpago.
    READ TABLE lr_condcpago INTO lw_condcpago INDEX 1.
    IF sy-subrc EQ 0.
      lv_condcpago = lw_condcpago-low.
      p_ls_poheader-pmnttrms = lv_condcpago.  "Clave de condiciones de pago
      p_ls_poheaderx-pmnttrms = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_TRATAR_COND_PAGO
* Fin modificación T10000252571 EXTSMA01 16.09.2015
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_PRESUPUESTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_POITEM  text
*      -->P_LT_POSERVICES  text
*----------------------------------------------------------------------*
FORM validar_presupuesto  TABLES   p_lt_poitem STRUCTURE bapimepoitem
                                     "Insertar nombre correcto para <...>
                                   p_lt_poservices STRUCTURE bapiesllc
                                   p_lt_account    STRUCTURE bapimepoaccount
                          USING  p_ls_poheader   TYPE bapimepoheader
                          CHANGING p_lv_validar_presupuesto TYPE c
                                   p_lw_output TYPE zclmt_consulta_presupuesto
                                   p_lw_input TYPE zclmt_consulta_presupuesto_res
                                   P_FLAG_ERROR_SOAMANAGER TYPE C
                                   p_mensajeerror TYPE String.


  DATA: lwa_output TYPE zclmt_consulta_presupuesto,
        lwa_input  TYPE zclmt_consulta_presupuesto_res,
        lv_asunto  TYPE string,
        lo_ai_sys  TYPE REF TO cx_ai_system_fault.

  DATA  lv_aedat       TYPE ekko-aedat.
  DATA  ls_poservices  TYPE bapiesllc.
  DATA  lv_posserv     TYPE bapimepoitem-po_item.
  DATA: lv_lines      TYPE i.
  DATA  lw_items LIKE LINE OF gt_items.
  DATA: lt_ekko TYPE TABLE OF ekko,
        lw_ekko LIKE LINE  OF lt_ekko.

  DATA: lv_orden     TYPE aufnr,
        lv_solped    TYPE banfn,
        lv_sakto     TYPE sakto,
        lv_sociedad  TYPE bukrs,
        lv_centro    TYPE werks_d,
        lv_ceco      TYPE kostl,
        lv_orden_res TYPE aufnr,
        lv_pep       TYPE c LENGTH 25,
        lv_sakto_res TYPE sakto.

  DATA: lv_ai_system_fault TYPE REF TO cx_ai_system_fault,
        lv_mensajeerror    TYPE string.

  DATA: lt_poservices        TYPE TABLE OF bapiesllc.

  DATA: lo_consulta_presupuesto TYPE REF TO zclco_os_consulta_presupuesto.

  DATA: ls_puerto_logico_wssoamanager TYPE PRX_LOGICAL_PORT_NAME VALUE 'ZCONSULTAPRESUPUESTO'.

  READ TABLE p_lt_poitem INDEX 1 ASSIGNING FIELD-SYMBOL(<fs>).

*  READ TABLE p_lt_account INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_aacount>).
*  CLEAR gv_orderid.
*  IF sy-subrc EQ 0.
*    gv_orderid = <fs_aacount>-orderid.
*  ENDIF.


  TRY.
      CREATE OBJECT lo_consulta_presupuesto
        EXPORTING
          logical_port_name = ls_puerto_logico_wssoamanager.
    CATCH cx_ai_system_fault INTO lv_ai_system_fault.
      CALL METHOD lv_ai_system_fault->if_message~get_text
        RECEIVING
          result = lv_mensajeerror.

      APPEND INITIAL LINE TO lwa_input-mt_consulta_presupuesto_res-mensajes ASSIGNING FIELD-SYMBOL(<fs_mensaje>).
      <fs_mensaje>-descripcion_mensaje = lv_mensajeerror.
      p_mensajeerror = lv_mensajeerror.
      P_FLAG_ERROR_SOAMANAGER = 'X'.
  ENDTRY.



  IF NOT p_lt_poitem[] IS INITIAL.
    SELECT ebeln aedat
      INTO CORRESPONDING FIELDS OF TABLE lt_ekko
      FROM ekko
      FOR ALL ENTRIES IN p_lt_poitem
      WHERE ebeln EQ p_lt_poitem-agreement.
  ENDIF.

  lv_posserv = 0.

  lt_poservices[] = p_lt_poservices[].

  DELETE lt_poservices WHERE service IS INITIAL.
  LOOP AT lt_poservices ASSIGNING FIELD-SYMBOL(<services>).
"EMAT - Enrique Aguirre - LFSENAG-513 - 18.06.2020 - Nuevo
    READ TABLE p_lt_account ASSIGNING FIELD-SYMBOL(<fs_aacount>) WITH KEY PO_ITEM = <services>-outl_no.
    IF sy-subrc EQ 0.
      gv_orderid = <fs_aacount>-orderid.
    ENDIF.
"EMAT - Enrique Aguirre - LFSENAG-513 - 18.06.2020 - Nuevo

    ADD 10 TO lv_posserv.
    APPEND INITIAL LINE TO lwa_output-mt_consulta_presupuesto-datos ASSIGNING FIELD-SYMBOL(<fs_datos>).
    "EMAT - Enrique Aguirre - LFSENAG-473 - 22.05.2020 - Comentar
      "READ TABLE p_lt_poitem  ASSIGNING <fs> INDEX 1.
    "EMAT - Enrique Aguirre - LFSENAG-473 - 22.05.2020 - Comentar
    IF sy-subrc EQ 0.
    "EMAT - Enrique Aguirre - LFSENAG-473 - 22.05.2020 - Replace
      "<fs_datos>-posicion_documento_compras = p_lt_poitem-po_item.
      <fs_datos>-posicion_documento_compras = <services>-OUTL_NO.
    "EMAT - Enrique Aguirre - LFSENAG-473 - 22.05.2020 - Replace
      <fs_datos>-numero_contrato = <fs>-agreement.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_datos>-numero_contrato
        IMPORTING
          output = <fs_datos>-numero_contrato.




      "      READ TABLE lt_ekko INTO lw_ekko WITH KEY ebeln = <fs>-agreement.
      "      IF sy-subrc EQ 0.
      <fs_datos>-ejercicio       = sy-datum+0(4).
      "      ENDIF.

      CLEAR: lv_orden,
             lv_solped,
             lv_sakto,
             lv_sociedad,
             lv_centro,
             lv_ceco,
             lv_orden_res,
             lv_pep,
             lv_sakto_res.


      lv_orden  = <fs_aacount>-orderid.
      lv_solped = <fs>-preq_no.
      lv_sakto  = <fs_aacount>-gl_account.

      <fs_datos>-cuenta_mayor = <fs_aacount>-gl_account.

      CALL FUNCTION 'ZCOGA_MAP_IMPUTACION'
        EXPORTING
          iv_orden    = lv_orden
          iv_solped   = lv_solped
          iv_sakto    = lv_sakto
        IMPORTING
          ev_sociedad = lv_sociedad
          ev_centro   = lv_centro
          ev_ceco     = lv_ceco
          ev_orden    = lv_orden_res
          ev_pep      = lv_pep
          ev_sakto    = lv_sakto_res.

      <fs_datos>-cuenta_mayor = lv_sakto_res. "EMAT - LFSENAGA-473 - 06.07.2020 - Ajusnte en Cta
      <fs_datos>-centro_costos = lv_ceco.
      <fs_datos>-elemento_pep = lv_pep.
      <fs_datos>-numero_orden = lv_orden_res.


*        READ TABLE gt_items INTO lw_items WITH KEY banfn = <fs>-preq_no
*                                                   bnfpo = <fs>-preq_item.
*        IF sy-subrc eq 0.
*          <fs_datos>-numero_contrato = lw_items-agr
*        ENDIF.

    ENDIF.
    "EMAT - Enrique Aguirre - ENAGASLFS-473 - 21.05.2020 - Replace
      "<fs_datos>-sociedad        = p_bukrs.
      <fs_datos>-sociedad        = lv_sociedad.
    "EMAT - Enrique Aguirre - ENAGASLFS-473 - 21.05.2020 - Replace
    <fs_datos>-moneda          = p_ls_poheader-currency.

    <fs_datos>-posicion_documento_presupuesto = lv_posserv.
    <fs_datos>-texto_posicion = <services>-service.
    <fs_datos>-posicion_documento_presupuesto = <services>-ext_line+7(3).
    <fs_datos>-importe_consulta = <services>-gr_price * <services>-quantity.
  ENDLOOP.


IF P_FLAG_ERROR_SOAMANAGER IS INITIAL.
  TRY .
      lo_consulta_presupuesto->os_consulta_presupuesto(
        EXPORTING
          input              =  lwa_output
        IMPORTING
          output             =  lwa_input ).
*      CATCH cx_ai_system_fault.    "
    CATCH cx_ai_system_fault INTO lo_ai_sys.
      CALL METHOD lo_ai_sys->if_message~get_text
        RECEIVING
          result = lv_mensajeerror.

      APPEND INITIAL LINE TO lwa_input-mt_consulta_presupuesto_res-mensajes ASSIGNING FIELD-SYMBOL(<fs_mensaje2>).
      <fs_mensaje2>-descripcion_mensaje = lv_mensajeerror.
      p_mensajeerror = lv_mensajeerror.
      P_FLAG_ERROR_SOAMANAGER = 'X'.
  ENDTRY.
*    IF lv_mensajeerror IS NOT INITIAL.
*      DATA: _lv_asunto TYPE string.
*      CONCATENATE 'Adv: Pedido ' p_ls_poheader-po_number 'Error conexión :'
*                  space sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4)  INTO _lv_asunto RESPECTING BLANKS.
*      PERFORM enviar_correo_errores USING lwa_output _lv_asunto.
*    ENDIF.
ENDIF.

  COMMIT WORK.


  "Actualizar la tabla de logs

  CLEAR gv_codigoejec.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZCOD_EJECC'
      quantity                = '1'
      subobject               = ' '
      toyear                  = '0000'
      ignore_buffer           = ' '
    IMPORTING
      number                  = gv_codigoejec
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF NOT gv_codigoejec IS INITIAL.
    CLEAR: gt_logpresupuesto[],
           gt_menpresupuesto[].

    DATA: lv_posicion TYPE i.

    lv_posicion = 1.
    LOOP AT lwa_output-mt_consulta_presupuesto-datos ASSIGNING FIELD-SYMBOL(<fs_datos_output>).
      CLEAR gw_logpresupuesto.
      gw_logpresupuesto-codejecucion = gv_codigoejec.
      gw_logpresupuesto-pos_ejecucion = lv_posicion.
      gw_logpresupuesto-sociedad = <fs_datos_output>-sociedad.
      gw_logpresupuesto-ejercicio = <fs_datos_output>-ejercicio.
      gw_logpresupuesto-centro_costos = <fs_datos_output>-centro_costos.
      gw_logpresupuesto-elemento_pep = <fs_datos_output>-elemento_pep.
      gw_logpresupuesto-moneda = <fs_datos_output>-moneda.
      gw_logpresupuesto-numero_orden = <fs_datos_output>-numero_orden.
      gw_logpresupuesto-cuenta_mayor = <fs_datos_output>-cuenta_mayor.
      gw_logpresupuesto-numero_contrato = <fs_datos_output>-numero_contrato.
      gw_logpresupuesto-pedido_intermedio = <fs_datos_output>-pedido_intermedio.
      gw_logpresupuesto-posicion_documento_compras = <fs_datos_output>-posicion_documento_compras.
      gw_logpresupuesto-posicion_documento_presupuesto = <fs_datos_output>-posicion_documento_presupuesto.
      gw_logpresupuesto-texto_posicion = <fs_datos_output>-texto_posicion.
      gw_logpresupuesto-fondo = <fs_datos_output>-fondo.
      gw_logpresupuesto-centro_gestor = <fs_datos_output>-centro_gestor.
      gw_logpresupuesto-posicion_presupuestaria = <fs_datos_output>-posicion_presupuestaria.
      gw_logpresupuesto-importe_consulta = <fs_datos_output>-importe_consulta.
      gw_logpresupuesto-importe_disponible = <fs_datos_output>-importe_disponible.
      gw_logpresupuesto-ok = <fs_datos_output>-ok.
      gw_logpresupuesto-direccion = '1'.
      gw_logpresupuesto-fecha = sy-datum.
      gw_logpresupuesto-hora = sy-timlo.
      gw_logpresupuesto-transaccion = 'ZMM002'.
      gw_logpresupuesto-numenvio = '1'.

      APPEND gw_logpresupuesto TO gt_logpresupuesto.
      add 1 to lv_posicion.
    ENDLOOP.

    lv_posicion = 1.
    LOOP AT lwa_input-mt_consulta_presupuesto_res-datos ASSIGNING FIELD-SYMBOL(<fs_datos_input>).
      CLEAR gw_logpresupuesto.
      gw_logpresupuesto-codejecucion = gv_codigoejec.
      gw_logpresupuesto-pos_ejecucion = lv_posicion.
      gw_logpresupuesto-sociedad = <fs_datos_input>-sociedad.
      gw_logpresupuesto-ejercicio = <fs_datos_input>-ejercicio.
      gw_logpresupuesto-centro_costos = <fs_datos_input>-centro_costos.
      gw_logpresupuesto-elemento_pep = <fs_datos_input>-elemento_pep.
      gw_logpresupuesto-moneda = <fs_datos_input>-moneda.
      gw_logpresupuesto-numero_orden = <fs_datos_input>-numero_orden.
      gw_logpresupuesto-cuenta_mayor = <fs_datos_input>-cuenta_mayor.
      gw_logpresupuesto-numero_contrato = <fs_datos_input>-numero_contrato.
      gw_logpresupuesto-pedido_intermedio = <fs_datos_input>-pedido_intermedio.
      gw_logpresupuesto-posicion_documento_compras = <fs_datos_input>-posicion_documento_comercial.
      gw_logpresupuesto-posicion_documento_presupuesto = <fs_datos_input>-posicion_documento_presupuesto.
      gw_logpresupuesto-texto_posicion = <fs_datos_input>-texto_posicion.
      gw_logpresupuesto-fondo = <fs_datos_input>-fondo.
      gw_logpresupuesto-centro_gestor = <fs_datos_input>-centro_gestor.
      gw_logpresupuesto-posicion_presupuestaria = <fs_datos_input>-posicion_presupuestaria.
      gw_logpresupuesto-importe_consulta = <fs_datos_input>-importe_consulta.
      gw_logpresupuesto-importe_disponible = <fs_datos_input>-importe_disponible.
      gw_logpresupuesto-ok = <fs_datos_input>-ok.
      gw_logpresupuesto-direccion = '2'.
      gw_logpresupuesto-fecha = sy-datum.
      gw_logpresupuesto-hora = sy-timlo.
      gw_logpresupuesto-transaccion = 'ZMM002'.
      gw_logpresupuesto-numenvio = '1'.

      APPEND gw_logpresupuesto TO gt_logpresupuesto.
      add 1 to lv_posicion.
    ENDLOOP.

    lv_posicion = 1.
    LOOP AT lwa_input-mt_consulta_presupuesto_res-mensajes ASSIGNING FIELD-SYMBOL(<fs_mensaje_input>).
      CLEAR gw_menpresupuesto.
      gw_menpresupuesto-codejecucion = gv_codigoejec.
      gw_menpresupuesto-pos_ejecucion = lv_posicion.
      gw_menpresupuesto-numenvio = '1'.
      gw_menpresupuesto-sociedad = <fs_mensaje_input>-sociedad.
      gw_menpresupuesto-ejercicio = <fs_mensaje_input>-ejercicio.
      gw_menpresupuesto-documento_compras = <fs_mensaje_input>-documento_compras.
      gw_menpresupuesto-posicion_documento_compras = <fs_mensaje_input>-posicion_documento_compras.
      gw_menpresupuesto-descripcion_mensaje = <fs_mensaje_input>-descripcion_mensaje.

      APPEND gw_menpresupuesto TO gt_menpresupuesto.
      add 1 to lv_posicion.
    ENDLOOP.

    IF NOT gt_logpresupuesto[] IS INITIAL.
      MODIFY ztlogdatospresup FROM TABLE gt_logpresupuesto.
      COMMIT WORK.
    ENDIF.
    IF NOT gt_menpresupuesto[] IS INITIAL.
      MODIFY ztlogmensapresup FROM TABLE gt_menpresupuesto.
      COMMIT WORK.
    ENDIF.


  ENDIF.

  "Validando si la respuesta fue exitosa y si las posiciones tienen presupuesto

  p_lv_validar_presupuesto = ''.

  LOOP AT lwa_input-mt_consulta_presupuesto_res-datos ASSIGNING <fs_datos>.

    IF <fs_datos>-ok EQ 'X'.
      IF <fs_datos>-importe_disponible > 0.
        p_lv_validar_presupuesto = 'X'.
      ELSE.
        p_lv_validar_presupuesto = ''.
        EXIT.
      ENDIF.
    ELSE.
      p_lv_validar_presupuesto = ''.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF lwa_input-mt_consulta_presupuesto_res-datos[] IS INITIAL
    AND lwa_input-mt_consulta_presupuesto_res-mensajes[] IS INITIAL.

    APPEND INITIAL LINE TO lwa_input-mt_consulta_presupuesto_res-mensajes ASSIGNING FIELD-SYMBOL(<fs_mensaje3>).
    <fs_mensaje3>-descripcion_mensaje = 'Error al obtener información de COGA'.
    p_lv_validar_presupuesto = ''.
    PERFORM enviar_correo_errores USING lwa_output <fs_mensaje3>-descripcion_mensaje ''.
  ENDIF.


  IF p_lv_validar_presupuesto EQ 'X'.
    lv_lines = 0.

    DESCRIBE TABLE lwa_input-mt_consulta_presupuesto_res-mensajes LINES lv_lines.

    IF lv_lines EQ 0.
      p_lv_validar_presupuesto = 'X'.
    ENDIF.

  ENDIF.

  p_lw_output = lwa_output.
  p_lw_input = lwa_input.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ENVIAR_CORREO_ERRORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LW_OUTPUT  text
*      -->P_LV_ASUNTO  text
*----------------------------------------------------------------------*
FORM enviar_correo_errores  USING    p_lw_output TYPE zclmt_consulta_presupuesto
                                     p_lv_asunto TYPE string
                                     p_mensajeerror TYPE string.

  DATA: lw_document_data TYPE sodocchgi1,
        lt_packinglist   TYPE TABLE OF sopcklsti1,
        lw_packinglist   LIKE  LINE OF lt_packinglist,
        lt_contents_txt  TYPE TABLE OF solisti1,
        lt_body          TYPE TABLE OF solisti1,
        ls_body          TYPE solisti1,
        lv_lines         TYPE i,
        lt_receivers     TYPE TABLE OF somlreci1,
        lw_receivers     LIKE LINE OF lt_receivers.

  DATA: lw_items LIKE LINE OF gt_items.
  DATA: lv_solicitado     TYPE p DECIMALS 2,
        lv_solicitado_aux TYPE string.


  lw_document_data-obj_name = 'SAPRPT'.
  lw_document_data-obj_descr = p_lv_asunto.
  lw_document_data-sensitivty = 'F'.
  lw_document_data-no_change = 'X'.
  lw_document_data-priority = '1'.
  lw_document_data-obj_prio = '1'.
  lw_document_data-obj_langu = sy-langu.
  lw_document_data-no_change = 'X'.

IF p_mensajeerror IS NOT INITIAL.
  ls_body = '<table style="border: 0px solid #000000;">'.APPEND ls_body TO lt_body.
  ls_body = '<tr>'. APPEND ls_body TO lt_body.
  CONCATENATE '<td style="border: 0px solid #000000;"><strong>' p_mensajeerror '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
  ls_body = '<tr>'. APPEND ls_body TO lt_body.
  ls_body = '</table>'.APPEND ls_body TO lt_body.
ENDIF.

  ls_body = '<table style="border: 1px solid #000000;">'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Sol.Ped.</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Pos.</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Orden</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Ceco</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">AFE</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Orden CO</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Pedido</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Pos.</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Cuenta</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Contrato</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Pos.</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Línea</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Servicio</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Centro gestor</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Posición presupuestaria</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Moneda</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Solicitado</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Disponible</th>'.APPEND ls_body TO lt_body.
  ls_body = '<th style="border: 1px solid #000000;" scope="col">Error</th>'.APPEND ls_body TO lt_body.

  LOOP AT p_lw_output-mt_consulta_presupuesto-datos ASSIGNING FIELD-SYMBOL(<fs>).

    READ TABLE GW_INPUT-MT_CONSULTA_PRESUPUESTO_RES-DATOS INTO DATA(wa_gw_input) WITH KEY texto_posicion = <fs>-texto_posicion.
    <fs>-posicion_presupuestaria = wa_gw_input-posicion_presupuestaria.
    <fs>-centro_gestor = wa_gw_input-centro_gestor.
    <fs>-importe_disponible = wa_gw_input-importe_disponible.

    ls_body = '<tr>'.
    APPEND ls_body TO lt_body.
    CLEAR lw_items.
    READ TABLE gt_items INTO lw_items WITH KEY konnr = <fs>-numero_contrato.
    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' lw_items-banfn '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = <fs>-posicion_documento_compras
      IMPORTING
        output = <fs>-posicion_documento_compras.

    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-posicion_documento_compras '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' gv_orderid '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-centro_costos '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    "EMAT - Enrique Aguirre - ENAGASLFS-473 - 21.05.2020 - Replace
       "CONCATENATE '<td style="border: 1px solid #000000;"><strong>' '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
       CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-elemento_pep '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    "EMAT - Enrique Aguirre - ENAGASLFS-473 - 21.05.2020 - Replace

    "EMAT - Enrique Aguirre - ENAGASLFS-473 - 21.05.2020 - Replace
      "CONCATENATE '<td style="border: 1px solid #000000;"><strong>' '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
      CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-numero_orden '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    "EMAT - Enrique Aguirre - ENAGASLFS-473 - 21.05.2020 - Replace
    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' gv_purchaseorder '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-posicion_documento_compras '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = <fs>-cuenta_mayor
      IMPORTING
        output = <fs>-cuenta_mayor.

    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-cuenta_mayor '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = <fs>-numero_contrato
      IMPORTING
        output = <fs>-numero_contrato.

    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-numero_contrato '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lw_items-ktpnr
      IMPORTING
        output = lw_items-ktpnr.

    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' lw_items-ktpnr '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = <fs>-posicion_documento_presupuesto
      IMPORTING
        output = <fs>-posicion_documento_presupuesto.

    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-posicion_documento_presupuesto '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = <fs>-texto_posicion
      IMPORTING
        output = <fs>-texto_posicion.

    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-texto_posicion '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-centro_gestor '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = <fs>-posicion_presupuestaria
      IMPORTING
        output = <fs>-posicion_presupuestaria.

    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-posicion_presupuestaria '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-moneda '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CLEAR: lv_solicitado, lv_solicitado_aux.
    lv_solicitado =  <fs>-importe_consulta.
    lv_solicitado_aux = lv_solicitado.
    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' lv_solicitado_aux '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-importe_disponible '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    IF <fs>-ok IS INITIAL.
      CONCATENATE '<td style="border: 1px solid #000000;"><strong>' 'X' '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    ELSE.
      CONCATENATE '<td style="border: 1px solid #000000;"><strong>' <fs>-ok '</strong></td>' INTO ls_body.APPEND ls_body TO lt_body.
    ENDIF.
    ls_body = '</tr>'.
    APPEND ls_body TO lt_body.
  ENDLOOP.

  ls_body = '</table>'.APPEND ls_body TO lt_body.

  lt_contents_txt[] = lt_body[].

  lw_packinglist-transf_bin = space.
  lw_packinglist-head_start = 1.
  lw_packinglist-head_num = 0.
  lw_packinglist-body_start = 1.
  lw_packinglist-doc_type = 'HTM'.
  DESCRIBE TABLE lt_contents_txt LINES lv_lines.
  lw_packinglist-body_num = lv_lines.
  APPEND lw_packinglist TO lt_packinglist.

"EMAT - Enrique Aguirre - LFSENAGAS-473 - 21.05.2020 - Replace
  select single line_low into lw_receivers-receiver
  from ztconstantes
  where Zmodulo = 'MM' AND
        zobjetoabap = 'ZMM_CREATE_PEDIDO_HES' AND
        fieldname = 'ZMM_CREATE_PEDIDO_HES' AND
        GRPNAME = 'CORREO' and
        znumcor = 1.

   "lw_receivers-receiver = 'jcochach@everis.com'.
"EMAT - Enrique Aguirre - LFSENAGAS-473 - 21.05.2020 - Replace


  lw_receivers-rec_type = 'U'.
  lw_receivers-com_type = 'INT'.
  APPEND lw_receivers TO  lt_receivers.

  DATA lv_sender TYPE soextreci1-receiver.

  lv_sender = sy-uname.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = lw_document_data
      put_in_outbox              = 'X'
      sender_address             = lv_sender
      commit_work                = 'X'
    TABLES
      packing_list               = lt_packinglist
      contents_txt               = lt_contents_txt
      receivers                  = lt_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
