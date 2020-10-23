FUNCTION z_rfc_p_cola_registro_re.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_MODO) TYPE  CHAR1 OPTIONAL
*"     VALUE(I_VSTEL) TYPE  VSTEL
*"     VALUE(I_STCD1) TYPE  STCD1
*"     VALUE(I_BISMT) TYPE  BISMT
*"     VALUE(I_BISMT2) TYPE  BISMT OPTIONAL
*"     VALUE(I_RUC) TYPE  STCD1 OPTIONAL
*"     VALUE(I_RUTA) TYPE  ZTMMTARIFAS-RUTA OPTIONAL
*"  TABLES
*"      T_MSG STRUCTURE  ZTAB256
*"      T_STATUS STRUCTURE  ZTAB256
*"      T_DATA_CAB STRUCTURE  ZTAB256
*"      T_DATA_DET STRUCTURE  ZTAB256
*"      T_PEDIDO STRUCTURE  ZNSES_PEDIDOREC
*"      T_TRASLADO STRUCTURE  ZST_REGTRASLADO OPTIONAL
*"----------------------------------------------------------------------
  DATA  : l_tracto         TYPE mara-bismt,
          l_carreta        TYPE mara-bismt,
          l_material       TYPE lips-matnr,
          l_werks          TYPE ekpo-werks,
          l_tknum          TYPE vttk-tknum,
          l_vstel_dest     TYPE tvst-vstel,
          l_vstel_orig     TYPE tvst-vstel,
          l_zona_id        TYPE znstb_zona_des-zona_id,
          l_tranq_id       TYPE znstb_tranquera-tranq_id,
          l_cola_id(9)     TYPE c,
          l_ticket(3)      TYPE c,
          l_dti_id         TYPE znstbflujoreccab-tknum_id,
          l_trfid_id       TYPE znstb_trfid-trfid_id,
          l_trfid_nro      TYPE znstb_asig_trfid-trfid_nro,
          l_fecha_act      TYPE sy-datum,
          l_hora_act       TYPE sy-uzeit,
          l_fecha_txt      TYPE char10,
          l_hora_txt       TYPE char8,
          l_chofer_id      TYPE lfa1-lifnr,
          l_chofer_dni     TYPE lfa1-stcd1,
          l_chofer_bvt     TYPE lfa1-stcd2,
          l_chofer_rvl     TYPE lfa1-gbdat,
          l_chofer_fnc     TYPE lfa1-gbort,
          l_chofer_des     TYPE char70,
          l_provee_id      TYPE char10,
          l_provee_des     TYPE char70,
          l_destin_id      TYPE char10,
          l_destin_des     TYPE char70,
          l_transp_id      TYPE vttk-tdlnr,
          l_transp_des     TYPE char70,
          l_transp_ruc     TYPE lfa1-stcd1,
          l_turno_id       TYPE znstb_turnos-turno_id,
          l_turno_zona_ini TYPE znstb_turnos-turno_id,
          l_turno_zona_fin TYPE znstb_turnos-turno_id,
          l_tipo_p         TYPE ekko-bsart,
          l_color          TYPE znstb_zona_des-color,
          l_ebeln          TYPE ekko-ebeln,
          l_vbeln_vl       TYPE likp-vbeln,
          l_material_des   TYPE makt-maktx,
          l_canti_txt      TYPE char15,
          l_um             TYPE lips-meins,
          l_item           TYPE ekpo-ebelp,
          l_evento_id      TYPE znstbflujoreccab-evento_id,
          l_evento_ant     TYPE znstb_evento-evento_id,
          l_evento_pos     TYPE znstb_evento-evento_id,
          l_escenario_id   TYPE znstbflujoreccab-evento_id,
          l_bukrs          TYPE t001k-bukrs,
          l_bukrs_des      TYPE t001-butxt ,                             "NETSOL-CCH05082015+
          l_tipo_t         TYPE c  ,         "Tipo transporte.
*          l_ruta          TYPE vttk-route ,
          l_ruta           TYPE ztmmtarifas-ruta,
          l_cant_gr_tot    TYPE lips-lfimg,
          l_peso_in        TYPE znstbflujoreccab-peso_in,
          l_peso_fi        TYPE znstbflujoreccab-peso_fi,
          l_peso_ne        TYPE znstbflujoreccab-peso_ne,

          l_tarifa_id      TYPE znstbflujoreccab-tarifa_id,
          l_homolog        TYPE ztmmhomolog-material_balanza ,           "NETSOL-CCH18082015+
          l_cant_pedabier  TYPE i  ,                                     "NETSOL-CCH09092015+
          l_dia_ant        TYPE sy-datum  ,                              "NETSOL-CCH09092015+

          lt_flujoreccab   TYPE STANDARD TABLE OF znstbflujoreccab,

          l_error          TYPE c,
          flag_dt          TYPE c,
          flag_turno       TYPE c,
          flag_trfid       TYPE c,
          flag_transp      TYPE c  ,                                     "NETSOL-CCH18082015+
          flag_transp_spec TYPE c  ,                                    "NETSOL-CCCE01032017+
          lw_flujoreccab   TYPE znstbflujorecdet,
          is_traslado      TYPE zst_regtraslado,                         "NETSOL-JPS02122015
          l_matnr_out      TYPE matnr.                                   "NETSOL-JOM02122015

  DATA: lt_msg    TYPE STANDARD TABLE OF tab512 WITH HEADER LINE,       "NETSOL-CCH27082015+
        lt_status TYPE STANDARD TABLE OF tab512 WITH HEADER LINE.       "NETSOL-CCH27082015+

  REFRESH: ti_flujodespcab, ti_flujoreccab, ti_flujorecdet ,            "NETSOL-CCH31072015
           ti_flujodespdet  .                                           "NETSOL-CCH05082015+
  CLEAR:  l_msg , flag_dt, flag_turno .

  READ TABLE t_traslado INTO is_traslado INDEX 1.                       "NETSOL-JPS02122015

* Cargar constantes.
*  PERFORM cargar_constantes_la .
  PERFORM obtener_constantes  .

  flag_trfid  = v_trfid_flag  .                                         "NETSOL-CCH27082015+
*  l_evento_id   = v_evento_id_0100 .                                   "NETSOL-CCH05082015-
  l_fecha_act   = sy-datum .
  l_dia_ant     = l_fecha_act - 1 .                                     "NETSOL-CCH09092015+
  l_hora_act    = sy-uzeit .

* Valida pto.exp.
  SELECT SINGLE vstel INTO l_vstel FROM tvst
    WHERE vstel EQ i_vstel  .
  IF sy-subrc NE 0 .
    PERFORM cargar_log_mensaje  TABLES  t_msg
                                USING   'E' text-m01 v_separador .
  ELSE.
*   Validar datos obligatorios.
    IF i_stcd1  IS INITIAL  .       "Chofer
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING   'E' text-m02 v_separador .
    ENDIF .
    IF i_bismt  IS INITIAL  .   "Tracto
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING   'E' text-m03 v_separador .
    ENDIF.
*    IF i_bismt2  IS INITIAL  .  "Carreta                               "NETSOL-CCCE06032017-
*      PERFORM cargar_log_mensaje  TABLES  t_msg                        "NETSOL-CCCE06032017-
*                                  USING   'E' text-m04 v_separador .   "NETSOL-CCCE06032017-
*    ENDIF.                                                             "NETSOL-CCCE06032017-
*----------------------------------------------------------------------*
*   Corta Proceso en caso de errores.
*----------------------------------------------------------------------*
    PERFORM cargar_log_status  TABLES  t_status t_msg
                               CHANGING l_error .
    IF l_error = 'X' .
      RETURN  .
    ENDIF.
*----------------------------------------------------------------------*
*   Validar datos de chofer.
    PERFORM validar_interlocutor_nif  USING i_stcd1
                                      CHANGING w_lfa1 .
    IF w_lfa1  IS INITIAL  .
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING   'E' text-m05 v_separador .
    ELSE.
      l_chofer_id   = w_lfa1-lifnr  .
      l_chofer_dni  = w_lfa1-stcd1  .
      l_chofer_bvt  = w_lfa1-stcd2  .
      l_chofer_rvl  = w_lfa1-gbdat  .
      CONCATENATE w_lfa1-name1 w_lfa1-name3 w_lfa1-name4
        INTO  l_chofer_des SEPARATED BY space .
      CONCATENATE w_lfa1-gbort+6(4) w_lfa1-gbort+3(2) w_lfa1-gbort(2)
        INTO l_chofer_fnc.
      IF l_chofer_fnc+4(4) = sy-datum+4(4)  .
        PERFORM cargar_log_mensaje  TABLES  t_msg
                                    USING   'I' text-m28  v_separador .
      ENDIF.
    ENDIF .

*   Validar datos de tracto
    PERFORM validar_placa   USING i_bismt
                            CHANGING  w_mara  .
    IF w_mara IS INITIAL   .
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING   'E' text-m06  v_separador .
    ELSE.
      l_tracto  = w_mara-bismt  .
    ENDIF.

*   Validar datos de carreta
    IF i_bismt2 IS NOT INITIAL.                                         "NETSOL-CCCE06032017+
      PERFORM validar_placa   USING i_bismt2
                              CHANGING  w_mara  .
      IF w_mara IS INITIAL   .
        PERFORM cargar_log_mensaje  TABLES  t_msg
                                    USING   'E' text-m07 v_separador .
      ELSE.
        l_carreta  = w_mara-bismt  .
      ENDIF .
    ENDIF.                                                              "NETSOL-CCCE06032017+
*----------------------------------------------------------------------*"BE NETSOL-CCH03092015+
*   Corta Proceso en caso de errores.
*----------------------------------------------------------------------*
    PERFORM validar_datos_enflujo USING l_chofer_dni l_tracto l_carreta
                                  CHANGING l_error l_msg  .
    IF l_error NE space.
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING  'E' l_msg v_separador  .
      PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
      RETURN  .
    ENDIF.
    "EE NETSOL-CCH03092015+

*----------------------------------------------------------------------*
*   Corta Proceso en caso de errores.
*----------------------------------------------------------------------*
    PERFORM cargar_log_status  TABLES  t_status t_msg
                               CHANGING l_error .
    IF l_error = 'X' .
      RETURN  .
    ENDIF.
*----------------------------------------------------------------------*
*   Validar si tiene DT desde el flujo de despacho.
    SELECT * INTO TABLE ti_flujodespcab FROM zsdtflujodespcab
      WHERE stcd2_b1  EQ l_chofer_dni
        AND bismt_p1  EQ l_tracto
        AND idevento  EQ '1400'    "Que tengan GR
*        AND dpreg     EQ sy-datum .                                    "NETSOL-CCH09092015-
        AND dpreg     IN (l_dia_ant, l_fecha_act) .                     "NETSOL-CCH09092015+
    IF sy-subrc NE 0  .
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING   'I' text-m08 v_separador .
      flag_dt = ''  .
    ELSE. "Con DT.
*     Eliminando los que ya esta registrados en el flujo de recepción.
      SELECT * INTO TABLE ti_flujoreccab  FROM znstbflujoreccab "#EC CI_NOFIRST
        FOR ALL ENTRIES IN  ti_flujodespcab
        WHERE tknum EQ ti_flujodespcab-tknum
          AND vstel EQ l_vstel.                                         "NETSOL-CCH05082015+
      IF sy-subrc EQ 0  .
        r_tknum-sign = 'I' .
        r_tknum-option = 'EQ' .
        LOOP AT ti_flujoreccab INTO w_flujoreccab .
          r_tknum-low  = w_flujoreccab-tknum .
          APPEND r_tknum .
        ENDLOOP.

        IF r_tknum[] IS NOT INITIAL .
          DELETE ti_flujodespcab WHERE tknum  IN r_tknum[] .
        ENDIF.
      ENDIF.
      REFRESH:  ti_flujoreccab  .                                       "NETSOL-CCH11092015+
*     Verificar sólo 1 pedido abierto para el conductor y tracto      "Begin NETSOL-CCH09092015+
*     Obtiene información de DT
      REFRESH: r_tknum[]  .
      CLEAR:  l_cant_pedabier.
      SORT  ti_flujodespcab BY dpreg DESCENDING upreg DESCENDING tknum DESCENDING .
      LOOP AT ti_flujodespcab INTO w_flujodespcab .
        PERFORM obtener_info_dt  TABLES ti_vttp
                                 USING  w_flujodespcab-tknum
                                 CHANGING w_vttk   .

*         Obteniendo GR
        PERFORM obtener_info_gr  TABLES ti_likp ti_lips ti_vttp .

*         Obteniendo Pedido
        PERFORM obtener_info_pedido TABLES ti_ekko ti_ekpo ti_ekbe ti_likp  .

*         validar cantidad de ped. abiertos.
        LOOP AT ti_ekpo INTO w_ekpo .
          IF w_ekpo-elikz EQ space  .
            ADD 1 TO l_cant_pedabier  .
            IF l_cant_pedabier > 1 .
              CONCATENATE text-m82 w_ekpo-ebeln text-m89
              INTO l_msg SEPARATED BY space .
              PERFORM cargar_log_mensaje  TABLES  t_msg
                              USING   'E' l_msg v_separador .
            ENDIF.
          ELSE.
            r_tknum-sign = 'I' .
            r_tknum-option = 'EQ' .
            r_tknum-low  = w_flujodespcab-tknum .
            APPEND r_tknum .
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      IF l_cant_pedabier > 1 .
        CONCATENATE text-m85 l_chofer_dni ',' text-m86 l_tracto
                    text-m88
          INTO l_msg SEPARATED BY space .
        PERFORM cargar_log_mensaje  TABLES  t_msg
                          USING   'E' l_msg v_separador .
        RETURN  .
      ENDIF.
      IF r_tknum[] IS NOT INITIAL .
        DELETE ti_flujodespcab WHERE tknum  IN r_tknum .
      ENDIF.
*       Verificar sólo 1 pedido abierto para el conductor y tracto      "End NETSOL-CCH09092015+

      IF ti_flujodespcab IS NOT INITIAL .
        flag_dt = 'X'  .
      ENDIF.
    ENDIF.

*----------------------------------------------------------------------*
*   Proceso con DT
*----------------------------------------------------------------------*
    IF flag_dt EQ 'X'.

      READ TABLE ti_flujodespcab INTO w_flujodespcab INDEX 1 .
*     Obtiene información de DT
      PERFORM obtener_info_dt  TABLES ti_vttp
                               USING  w_flujodespcab-tknum
                               CHANGING w_vttk   .
      IF w_vttk IS INITIAL AND ti_vttp IS INITIAL.
        PERFORM cargar_log_mensaje  TABLES  t_msg
                              USING   'E' text-m14 v_separador .
      ENDIF .

      l_tknum = w_vttk-tknum  .
*      l_ruta  = w_vttk-route  .
*     Obtiene RUC Transportista.
      PERFORM validar_interlocutor_lfr  USING w_vttk-tdlnr
                                        CHANGING w_lfa1 .
      l_transp_id   = w_lfa1-lifnr  .
      l_transp_ruc  = w_lfa1-stcd1  .
      CONCATENATE w_lfa1-name1 w_lfa1-name2 INTO l_transp_des SEPARATED BY space  .

*     Obteniendo GR
      PERFORM obtener_info_gr  TABLES ti_likp ti_lips ti_vttp .

      IF ti_likp IS INITIAL AND ti_lips IS INITIAL .
        PERFORM cargar_log_mensaje  TABLES  t_msg
                              USING   'E' text-m15 v_separador .
      ENDIF.
*                                                                       "BE NETSOL-CCH03092015+
      PERFORM validacion_info_gr  TABLES ti_likp
                                  CHANGING l_error l_msg  .
      IF l_error NE space.
        PERFORM cargar_log_mensaje  TABLES  t_msg
                                    USING  'E' l_msg v_separador  .
        PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
        RETURN  .
      ENDIF.
*                                                                       "BE NETSOL-CCH03092015+
      PERFORM obtener_info_pedido TABLES ti_ekko ti_ekpo ti_ekbe ti_likp  .
      IF ti_ekko IS INITIAL AND ti_ekpo IS INITIAL.
        PERFORM cargar_log_mensaje  TABLES  t_msg
                              USING   'E' text-m16 v_separador .
      ENDIF .
      IF ti_ekbe IS INITIAL  .
        PERFORM cargar_log_mensaje  TABLES  t_msg
                              USING   'E' text-m29 v_separador .
      ENDIF.
*----------------------------------------------------------------------*
*   Textos provedor - cliente
*----------------------------------------------------------------------*
      PERFORM validar_origen_destino TABLES ti_ekko ti_ekpo
                                     CHANGING l_tipo_p
                                              l_provee_id l_provee_des
                                              l_destin_id l_destin_des.

*      Obtiene Material
      READ TABLE ti_lips INTO w_lips  INDEX 1 .
      IF sy-subrc EQ 0  .
        l_material  = w_lips-matnr  .
      ENDIF     .

*----------------------------------------------------------------------*
**        Validar cantidad de material  en transito
*----------------------------------------------------------------------*
      PERFORM validar_cantidad_transito TABLES ti_lips ti_ekpo ti_ekbe
                                        CHANGING l_cant_gr_tot l_um l_error
                                                 l_msg.                 "NETSOL-CCH04092015+
      IF l_error  EQ 'X'.
        PERFORM cargar_log_mensaje  TABLES  t_msg
*                              USING   'E' text-m11 v_separador  .      "NETSOL-CCH04092015-
                              USING   'E' l_msg v_separador  .          "NETSOL-CCH04092015+
        PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error.
        RETURN  .
      ENDIF.

*----------------------------------------------------------------------*
*   PROCESO SIN DT
*----------------------------------------------------------------------*
    ELSE.

      IF is_traslado IS INITIAL.                                          "NETSOL-JPS02112015

*       Validar datos de pedido (Compra / Traslado ) en caso de ingreso .
        IF t_pedido[] IS NOT INITIAL.
          PERFORM validar_pedido  TABLES  t_pedido
                                          ti_ekko
                                          ti_ekpo
                                          ti_ekbe
                            CHANGING l_tipo_p l_material l_provee_id
                                     l_destin_id l_cant_gr_tot l_um
                                     l_msg l_error  .
          IF l_error NE space.
            PERFORM cargar_log_mensaje  TABLES  t_msg
                                        USING  'E' l_msg v_separador  .
            PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
            RETURN  .
          ENDIF.
*           Validar registros duplicados por guía de remisión               "Begin NETSOL-CCH05082015+
          PERFORM validar_gremision TABLES t_pedido
                                    USING i_vstel
                                    CHANGING l_error l_msg  .
          IF l_error NE space.
            PERFORM cargar_log_mensaje  TABLES  t_msg
                                        USING  'E' l_msg v_separador  .
            PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
            RETURN  .
          ENDIF.
*           Validar registros duplicados por guía de remisión               "End NETSOL-CCH05082015+
        ELSE.
          PERFORM cargar_log_mensaje  TABLES  t_msg
                                       USING  'E' text-m27 v_separador  .
          PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
          RETURN.
        ENDIF.
*    *     Validar parámetros.                                              "Begin NETSOL-CCH18082015-
*          IF i_ruta  IS INITIAL  .
*            PERFORM cargar_log_mensaje  TABLES  t_msg
*                                        USING   'E' text-m40 v_separador .
*          ENDIF .
*
*          l_ruta  = i_ruta  .                                              "End NETSOL-CCH18082015-
        "BE NETSOL-CCH31082015-
*          IF i_ruc  IS INITIAL  .
*            PERFORM cargar_log_mensaje  TABLES  t_msg
*                                        USING   'E' text-m41 v_separador .
*          ENDIF .
*    *     Validar datos de transportista.
*          PERFORM validar_interlocutor_nif  USING i_ruc
*                                            CHANGING w_lfa1 .
*
*          IF w_lfa1  IS INITIAL  .
*            PERFORM cargar_log_mensaje  TABLES  t_msg
*                                        USING   'E' text-m42 v_separador .
*          ELSE.
*            l_transp_id = w_lfa1-lifnr  .
*            l_transp_ruc = w_lfa1-stcd1  .
*            CONCATENATE w_lfa1-name1 w_lfa1-name2 INTO l_transp_des
*            SEPARATED BY space  .
*          ENDIF .
        "EE NETSOL-CCH31082015-
*    ----------------------------------------------------------------------*BE NETSOL-CCH03092015+
*     Validar liberación de pedido  .
*    ----------------------------------------------------------------------*
        PERFORM validar_liberacion_pedido TABLES ti_ekko
                                          CHANGING l_error l_msg  .
        IF l_error NE space.
          PERFORM cargar_log_mensaje  TABLES  t_msg
                                      USING  'E' l_msg v_separador  .
          PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
          RETURN  .
        ENDIF.
*                                                                           "EE NETSOL-CCH03092015+
*    ----------------------------------------------------------------------*
*     Textos provedor - cliente
*    ----------------------------------------------------------------------*
        PERFORM validar_origen_destino TABLES ti_ekko ti_ekpo
                                       CHANGING l_tipo_p
                                                l_provee_id l_provee_des
                                                l_destin_id l_destin_des  .
*    ----------------------------------------------------------------------*
*         Corta Proceso en caso de errores.
*    ----------------------------------------------------------------------*
        PERFORM cargar_log_status  TABLES  t_status t_msg
                                   CHANGING l_error .
        IF l_error = 'X' .
          RETURN  .
        ENDIF.

      ENDIF.                                                        "NETSOL-JPS02112015

    ENDIF.

* Begin of NETSOL-JPS02112015
    IF is_traslado IS NOT INITIAL.
      PERFORM validar_interlocutor_nif  USING i_ruc
                                     CHANGING w_lfa1 .
      l_bukrs       = is_traslado-bukrs.
      l_tipo_p      = 'N'.

      CLEAR l_matnr_out.                                            "NETSOL-JOM02122015+
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'                   "NETSOL-JOM02122015+
        EXPORTING
          input            = is_traslado-matnr
        IMPORTING
          output           = l_matnr_out
        EXCEPTIONS
          number_not_found = 1
          length_error     = 2.
      .
      IF sy-subrc = 1.

      ELSEIF sy-subrc = 2.
        DATA: l_tam TYPE i,l_ind TYPE c.
        l_tam = strlen( is_traslado-matnr ).

        SEARCH is_traslado-matnr FOR '-'.
        IF sy-subrc EQ 0.
          l_ind = 'X'.
        ENDIF.

        IF l_tam EQ 18 AND l_ind EQ ''.
          l_matnr_out = is_traslado-matnr.
        ELSEIF l_tam LT 18 AND l_ind EQ ''.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = is_traslado-matnr
            IMPORTING
              output = l_matnr_out.
        ELSE.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
           RAISING length_error.
        ENDIF.
      ENDIF.

*      l_material    = is_traslado-matnr.                           "NETSOL-JOM02122015-
      l_material    =  l_matnr_out.                                 "NETSOL-JOM02122015+
      l_provee_id   = ''.
      l_provee_des  = ''.
      l_destin_id   = is_traslado-cenha.
      l_cant_gr_tot = is_traslado-peso_gr.
      l_um          = 'KG'.
      l_transp_id   = w_lfa1-lifnr.
      l_transp_ruc  = w_lfa1-stcd1.
      CONCATENATE w_lfa1-name1 w_lfa1-name2 INTO l_transp_des
        SEPARATED BY space  .
    ENDIF.
* End of NETSOL-JPS02112015

**----------------------------------------------------------------------*
**     Obteniendo Puesto de expedición.
**----------------------------------------------------------------------*
    IF l_tipo_p EQ 'T'  .
      l_werks = l_destin_id   .
      SELECT SINGLE vstel INTO l_vstel_dest  FROM tvswz
        WHERE werks =   l_werks     .
      l_werks = l_provee_id   .
      SELECT SINGLE vstel INTO l_vstel_orig  FROM tvswz
        WHERE werks =   l_werks     .

      PERFORM obtener_vstel_orig USING ti_ekpo                          "NETSOL-CCCE23022017+
                                 CHANGING l_vstel_orig .                "NETSOL-CCCE23022017+
    ELSEIF l_tipo_p EQ 'C'  .
      l_werks = l_destin_id   .
      SELECT SINGLE vstel INTO l_vstel_dest  FROM tvswz
        WHERE werks =   l_werks     .
*      l_werks = l_provee_id   .
*      SELECT SINGLE vstel INTO l_vstel_orig  FROM tvswz
*        WHERE werks =   l_werks     .
    ELSEIF l_tipo_p EQ 'N'. "Traslado en un solo paso             "NETSOL-JPS02112015
      SELECT SINGLE vstel INTO l_vstel_dest  FROM tvswz           "NETSOL-JPS02112015
        WHERE werks = is_traslado-cenha.                          "NETSOL-JPS02112015
      SELECT SINGLE vstel INTO l_vstel_orig  FROM tvswz           "NETSOL-JPS02112015
        WHERE werks = is_traslado-cende.                          "NETSOL-JPS02112015
    ENDIF.

    l_vstel_dest = l_vstel .                                            "NETSOL-CCCE23022017+
*----------------------------------------------------------------------*"Begin NETSOL-CCH18082015+
* Validar material homologado
*----------------------------------------------------------------------*
    PERFORM validar_matnr_homolog USING l_material l_destin_id
                                        l_tipo_p i_ruc i_ruta           "NETSOL-CCCE01032017+
                                  CHANGING flag_transp l_homolog
                                           flag_transp_spec             "NETSOL-CCCE01032017+
                                           l_error l_msg  .
    IF l_error NE space .
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING   'E' l_msg v_separador .
      PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error.
      RETURN  .
    ENDIF.
*                                                                       "End NETSOL-CCH18082015+
    "BE NETSOL-CCH31082015+
    IF l_tipo_p EQ 'C'  AND flag_transp EQ 'X' .
      IF i_ruc  IS INITIAL
      AND flag_dt EQ 'X'. "EMAT - 13.02.2020 Ticket: 95219  Insertar
        PERFORM cargar_log_mensaje  TABLES  t_msg
                                    USING   'E' text-m41 v_separador .
      ELSEIF i_ruc  IS INITIAL  "EMAT - 13.02.2020 Ticket: 95219  Insertar
      AND flag_dt EQ space.     "EMAT - 13.02.2020 Ticket: 95219  Insertar
        PERFORM cargar_log_mensaje  TABLES  t_msg  "EMAT - 13.02.2020 Ticket: 95219  Insertar
                                    USING   'I' text-m41 v_separador . "EMAT - 13.02.2020 Ticket: 95219  Insertar
      ENDIF .
*     Validar datos de transportista.
      PERFORM validar_interlocutor_nif  USING i_ruc
                                        CHANGING w_lfa1 .

      IF w_lfa1  IS INITIAL  .
        PERFORM cargar_log_mensaje  TABLES  t_msg
                                    USING   'E' text-m42 v_separador .
      ELSE.
        l_transp_id = w_lfa1-lifnr  .
        l_transp_ruc = w_lfa1-stcd1  .
        CONCATENATE w_lfa1-name1 w_lfa1-name2 INTO l_transp_des
        SEPARATED BY space  .
      ENDIF .
    ENDIF .
    "EE NETSOL-CCH31082015+
*----------------------------------------------------------------------*"Begin NETSOL-CCH27082015+
*Datos de sociedad
*----------------------------------------------------------------------*
*     sociedad
    SELECT SINGLE bukrs INTO l_bukrs  FROM t001k
      WHERE bwkey = l_destin_id .
*     desc.sociedad
    SELECT SINGLE butxt INTO l_bukrs_des  FROM t001
      WHERE bukrs = l_bukrs .
*----------------------------------------------------------------------*"End NETSOL-CCH27082015+
* Obtener ruta
*----------------------------------------------------------------------*
    IF flag_transp EQ 'X' .                                             "NETSOL-CCH18082015+
      IF i_ruta IS NOT INITIAL  .
        l_ruta  = i_ruta  .

**     sociedad
*        SELECT SINGLE bukrs INTO l_bukrs  FROM t001k
*          WHERE bwkey = l_destin_id .
**     desc.sociedad                                                     "Begin NETSOL-CCH05082015+
*        SELECT SINGLE butxt INTO l_bukrs_des  FROM t001
*          WHERE bukrs = l_bukrs .                                         "End NETSOL-CCH05082015+

        PERFORM validar_ruta_tarifa TABLES  ti_tarifas
                                    USING l_bukrs l_ruta l_transp_ruc l_material
                                    CHANGING l_tipo_t l_tarifa_id l_error l_msg  .
        IF l_error NE space .

        IF flag_dt EQ 'X'. "EMAT - 13.02.2020 Ticket: 95219  Insertar
          PERFORM cargar_log_mensaje  TABLES  t_msg
                                      USING   'E' l_msg v_separador .
        ELSE.
          PERFORM cargar_log_mensaje  TABLES  t_msg  "EMAT - 13.02.2020 Ticket: 95219  Insertar
                                      USING   'I' l_msg v_separador . "EMAT - 13.02.2020 Ticket: 95219  Insertar
        ENDIF.

        PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error.
        RETURN  .
        ENDIF.
      ELSE.                                                             "Begin NETSOL-CCH05082015+
        PERFORM cargar_log_mensaje  TABLES  t_msg
                                    USING   'E' text-m62 v_separador .
        PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error.
        RETURN  .
*                                                                       "End NETSOL-CCH05082015+
      ENDIF.
    ENDIF.                                                              "NETSOL-CCH18082015+
*----------------------------------------------------------------------*
* Zona de descarga
*----------------------------------------------------------------------*
    PERFORM validar_dispo_zdescarga  USING  l_material
                                            l_vstel_dest                "NETSOL-CCCE26042017+
                                     CHANGING l_zona_id
                                              l_tranq_id
                                              l_turno_id
                                              l_turno_zona_ini
                                              l_turno_zona_fin
                                              l_color
                                              l_error
                                              l_msg.
    IF l_error  EQ 'X' .
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING  'E' l_msg v_separador .
      PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error.
      RETURN  .
    ENDIF.
*----------------------------------------------------------------------*
* Tarjeta RFID
*----------------------------------------------------------------------*
    IF flag_trfid = 'X' .
      PERFORM obtener_trfid USING l_material l_zona_id l_tranq_id
                            CHANGING l_trfid_id
                                   l_trfid_nro
                                   l_error
                                   l_msg  .
      IF l_error  EQ 'X'  .
        PERFORM cargar_log_mensaje  TABLES  t_msg
                                    USING  'E' l_msg v_separador .
      ENDIF.
    ENDIF .
*----------------------------------------------------------------------*
* Documento interno de transporte
*----------------------------------------------------------------------*
    PERFORM obtener_dti_nuevo USING l_vstel_dest
                              CHANGING l_dti_id
                                       l_error
                                       l_msg.
    IF l_error  NE space  .
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING  'E' l_msg v_separador .
      PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error.
      RETURN  .
    ENDIF.
*----------------------------------------------------------------------*
* Escenario
*----------------------------------------------------------------------*
*    PERFORM obtener_escenario USING l_material l_vstel_dest            "NETSOL-CCH31082015-
    PERFORM obtener_escenario USING l_material l_vstel_dest             "NETSOL-CCH31082015+
                                    l_tipo_p  flag_transp flag_dt
                                    flag_transp_spec                    "NETSOL-CCCE01032017+
                              CHANGING l_escenario_id l_error l_msg.
    IF l_error  NE space  .
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING  'E' l_msg v_separador  .
      PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
      RETURN  .
    ENDIF .
**----------------------------------------------------------------------*"BE NETSOL-CCH31082015-
** Validar Escenario
**----------------------------------------------------------------------*
*    PERFORM obtener_validar_escenario USING l_tipo_p flag_transp l_escenario_id
*                              CHANGING l_error l_msg  .
*    IF l_error  NE space  .
*      PERFORM cargar_log_mensaje  TABLES  t_msg
*                                  USING  'E' l_msg v_separador  .
*      PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
*      RETURN  .
*    ENDIF .                                                            "EE NETSOL-CCH31082015-
*----------------------------------------------------------------------*
* Evento
*----------------------------------------------------------------------*
    PERFORM obtener_secuencia_evento  TABLES  ti_flujoreccab
                                      USING l_escenario_id  l_vstel_dest
                                      CHANGING l_evento_id  l_evento_ant l_evento_pos
                                                l_error l_msg .
    IF l_error  NE space  .
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING  'E' l_msg v_separador  .
      PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
      RETURN  .
    ELSE.
*     comparando evento
      IF l_evento_id  NE  v_evento_id_0100.
        CLEAR : l_msg .
        CONCATENATE text-m56 v_evento_id_0100 INTO l_msg  SEPARATED BY space .
        PERFORM cargar_log_mensaje  TABLES  t_msg
                                    USING  'E' l_msg v_separador  .
        PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
        RETURN  .
      ENDIF.
    ENDIF .
*----------------------------------------------------------------------*
* Ticket
*----------------------------------------------------------------------*
    PERFORM obtener_ticket_idcola  USING    l_transp_ruc l_vstel_dest
                                            l_fecha_act  l_hora_act
                                   CHANGING l_ticket l_cola_id l_error l_msg .
    IF l_error  NE space  .
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING  'E' l_msg v_separador  .
      PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
      RETURN  .
    ENDIF .
*----------------------------------------------------------------------*
* Registrar flujo de recepción
*----------------------------------------------------------------------*

*   Begin of NETSOL-JPS04112015
    IF is_traslado IS NOT INITIAL.
      PERFORM guardar_flujo_recepcion_tra TABLES ti_flujoreccab
                                                 ti_flujorecdet
                                                 ti_vttp
                                                 ti_ekko
                                                 ti_ekbe
                                                 ti_likp
                                                 ti_lips
                                                 t_pedido
                                           USING flag_dt
                                                 l_dti_id
                                                 l_tknum
                                                 l_vstel_dest
                                                 l_vstel_orig
                                                 l_transp_ruc
                                                 l_chofer_dni
                                                 l_tracto
                                                 l_carreta
                                                 l_material
                                                 l_ruta
                                                 l_ticket
                                                 l_cola_id
                                                 l_turno_id
                                                 l_trfid_id
                                                 l_tranq_id
                                                 l_evento_id
                                                 l_escenario_id
                                                 l_cant_gr_tot
                                                 l_um
                                                 i_modo
                                                 l_zona_id
                                                 l_peso_in
                                                 l_peso_fi
                                                 l_peso_ne
                                                 l_tarifa_id
                                                 l_homolog              "NETSOL-CCCE07032017+
                                                 is_traslado.
    ELSE.
*   End of NETSOL-JPS04112015
      PERFORM guardar_flujo_recepcion TABLES ti_flujoreccab ti_flujorecdet
                                             ti_vttp ti_ekko ti_ekbe ti_likp ti_lips t_pedido
                                      USING flag_dt l_dti_id l_tknum l_vstel_dest
                                            l_vstel_orig l_transp_ruc l_chofer_dni
                                            l_tracto l_carreta l_material l_ruta
                                            l_ticket l_cola_id l_turno_id l_trfid_id
                                            l_tranq_id l_evento_id l_escenario_id
                                            l_cant_gr_tot l_um i_modo l_zona_id
                                            l_peso_in l_peso_fi l_peso_ne l_tarifa_id
                                            l_homolog.                                  "NETSOL-CCCE07032017+
    ENDIF.                                                                              "NETSOL-JPS04112015

*    conversion de unidades a kilogramos                                "BE NETSOL-CCH03092015+
    PERFORM conversion_um TABLES ti_flujoreccab ti_flujorecdet
                          CHANGING l_msg l_error  .
    IF l_error  NE space  .
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING  'E' l_msg v_separador  .
      PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
      RETURN  .
    ENDIF.
*    conversion de unidades a kilogramos                                "EE NETSOL-CCH03092015+
*   Guarda los datos de flujo en BD  .
    PERFORM actualizar_flujorec_bd  TABLES ti_flujoreccab
                                           ti_flujorecdet
                                    CHANGING l_msg l_error  .
    IF l_error  NE space  .
      PERFORM cargar_log_mensaje  TABLES  t_msg
                                  USING  'E' l_msg v_separador  .
      PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
      RETURN  .
    ENDIF.
*----------------------------------------------------------------------*
* Registrar TRFID en tránsito.
*----------------------------------------------------------------------*
    IF flag_trfid = 'X' .                                               "NETSOL-CCH31072015+
      w_dti_trfid-tknum_id  = l_dti_id  .
      w_dti_trfid-trfid_id  = l_trfid_id  .
      w_dti_trfid-estado    = 'X'  .
      PERFORM actualizar_dti_trfid_bd  USING w_dti_trfid
                                       CHANGING l_msg l_error  .
      IF l_error  NE space  .
        PERFORM cargar_log_mensaje  TABLES  t_msg
                                    USING  'E' l_msg v_separador  .
        PERFORM cargar_log_status  TABLES  t_status t_msg CHANGING l_error  .
        RETURN  .
      ENDIF.
    ENDIF .                                                             "NETSOL-CCH31072015+

*----------------------------------------------------------------------*
* Estructa de datos de salida - cabecera
*----------------------------------------------------------------------*

* Begin of NETSOL-JPS04112015
    IF is_traslado IS NOT INITIAL.
      PERFORM guardar_datos_salida_tra TABLES t_data_cab t_data_det
                                              ti_ekko ti_ekpo ti_ekbe
                                              ti_likp ti_lips t_pedido
                                   USING flag_dt l_fecha_act l_hora_act l_tipo_p
                                         l_provee_id
                                         l_bukrs_des
                                         l_turno_id
                                         l_dti_id
                                         l_tknum
                                         l_material
                                         l_um
                                         l_transp_ruc
                                         l_transp_des
                                         l_chofer_dni
                                         l_chofer_des
                                         l_chofer_bvt
                                         l_tracto
                                         l_carreta
                                         l_ticket
                                         l_trfid_id
                                         l_trfid_nro
                                         l_color
                                         l_destin_id
                                         l_destin_des
                                         l_ruta                         "NETSOL-CCCE13032017+
                                         l_zona_id                      "NETSOL-CCCE15032017+
                                         l_vstel_dest                   "NETSOL-CCCE15032017+
                                         v_separador .
    ELSE.
* End of NETSOL-JPS04112015
      PERFORM guardar_datos_salida TABLES t_data_cab t_data_det
                                          ti_ekko ti_ekpo ti_ekbe
                                          ti_likp ti_lips t_pedido
                                   USING flag_dt l_fecha_act l_hora_act l_tipo_p
                                         l_provee_id
*                                         l_provee_des                    "NETSOL-CCH05082015-
                                         l_bukrs_des                      "NETSOL-CCH05082015+
                                         l_turno_id
                                         l_dti_id
                                         l_tknum
                                         l_material
                                         l_um
                                         l_transp_ruc
                                         l_transp_des
                                         l_chofer_dni
                                         l_chofer_des
                                         l_chofer_bvt
                                         l_tracto
                                         l_carreta
                                         l_ticket
                                         l_trfid_id
                                         l_trfid_nro
                                         l_color
                                         l_destin_id
                                         l_destin_des
                                         l_ruta                         "NETSOL-CCCE13032017+
                                         l_zona_id                      "NETSOL-CCCE15032017+
                                         l_vstel_dest                   "NETSOL-CCCE15032017+
                                         v_separador .
    ENDIF.                                                                "NETSOL-JPS04112015

  ENDIF.

* Mensaje de éxito.                                                     "Begin NETSOL-CCH07082015+
  IF l_error  EQ space  .
    CLEAR: l_msg  .
    CONCATENATE text-m58 l_ticket INTO l_msg SEPARATED BY space .
    PERFORM cargar_log_mensaje  TABLES  t_msg
                                USING  'I' l_msg  v_separador  .
*    RETURN  .                                                          "NETSOL-CCH27082015-
  ENDIF.

* { Adiciona envio de mail a Control de Calidad  NSC-FEV @01

*BI NETSOL-HAJ12042017
*  IF l_error  EQ space  .
*    PERFORM envia_mail  USING l_vstel          "Puesto Expedicion
*                              l_tknum          "DT
*                              l_tracto         "Tracto
*                              l_carreta        "Carreta
*                              l_transp_ruc     "RUC Transportes
*                              l_cant_gr_tot    "Cantidad
*                              l_um             "UM
*                              l_chofer_des     "Chofer
*                              l_material       "Material
*                              l_transp_id
*                              l_transp_des     "Nombre de transportista
*                              l_chofer_dni          "DNI chofer
*                              l_provee_id      "proveedor de mp         "NETSOL-CCCE05042017+
*                              l_provee_des
*                              l_dti_id.        "DT Interno    p         "NETSOL-CCCE05042017+
*  ENDIF.
* } Adiciona envio de mail a Control de Calidad  NSC-FEV @01
*EI NETSOL-HAJ12042017

* Mensaje de exito.                                                     "End NETSOL-CCH07082015+
* llamada automática                                                    "Begin NETSOL-CCH27082015+
  CALL FUNCTION 'Z_RFC_P_LLAMADA_AUTO_RE'
    EXPORTING
      i_vstel    = l_vstel_dest
      i_tknum_id = l_dti_id
    TABLES
      t_msg      = lt_msg
      t_status   = lt_status.
  LOOP AT lt_msg.
    PERFORM cargar_log_mensaje  TABLES  t_msg
                                USING  'I' lt_msg-wa v_separador  .
  ENDLOOP.
*    LOOP AT lt_status.
  PERFORM cargar_log_status  TABLES  lt_status t_msg CHANGING l_error  .
*    ENDLOOP.
* llamada automatica                                                    "End NETSOL-CCH27082015+

ENDFUNCTION.
