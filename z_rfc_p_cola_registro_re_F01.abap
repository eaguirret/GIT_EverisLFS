*----------------------------------------------------------------------*
***INCLUDE LZMMRECEPCIONF01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ADD_CLINE
*&---------------------------------------------------------------------*
*       Adicionar línea
*----------------------------------------------------------------------*
FORM add_cline  TABLES   p_tabla STRUCTURE ztab128
                USING    p_val1
                         p_val2
                         p_sep.
  CONCATENATE p_val1 p_val2 INTO p_tabla-wa SEPARATED BY p_sep  .
  APPEND p_tabla.

ENDFORM.                    " ADD_CLINE
*&---------------------------------------------------------------------*
*&      Form  CARGAR_CONSTANTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cargar_constantes TABLES tb_msg  STRUCTURE tab512 .
*  CLEAR: v_user, v_host, v_clave, v_archivo, v_rfc_destination.
*
** Cargamos el caracter separador
*  SELECT SINGLE valor1
*    FROM zgetdconst
*    INTO v_separador
*    WHERE modulo EQ c_mm
*      AND proyec EQ c_recepcion
*      AND aplica EQ c_constantes
*      AND campo  EQ c_charsep.
*
** Cargamos el Usuario FTP
*  SELECT SINGLE valor1
*    FROM zgetdconst
*    INTO v_user
*    WHERE modulo EQ c_mm
*      AND proyec EQ c_recepcion
*      AND aplica EQ c_dispositivos
*      AND campo  EQ c_user.
*
*  TRANSLATE v_user TO LOWER CASE.
*  IF sy-subrc NE 0.
*    v_status = c_1.
*    CONCATENATE '008' 'E' 'Usuario no encontrado.' INTO v_msg SEPARATED BY v_separador.
*    tb_msg-wa = v_msg.
*    APPEND tb_msg.
*  ENDIF.
*
** Cargamos el Host FTP
*  SELECT SINGLE valor1
*    FROM zgetdconst
*    INTO v_host
*    WHERE modulo EQ c_mm
*      AND proyec EQ c_recepcion
*      AND aplica EQ c_dispositivos
*      AND campo  EQ c_server.
*
*  IF sy-subrc NE 0.
*    v_status = c_1.
*    CONCATENATE '007' 'E' 'Host no encontrado.' INTO v_msg SEPARATED BY v_separador.
*    tb_msg-wa = v_msg.
*    APPEND tb_msg.
*  ENDIF.
*
** Cargamos el Puerto FTP
*  SELECT SINGLE valor1
*    FROM zgetdconst
*    INTO v_puerto
*    WHERE modulo EQ c_mm
*      AND proyec EQ c_recepcion
*      AND aplica EQ c_dispositivos
*      AND campo  EQ c_puerto.
*
*  IF sy-subrc EQ 0.
*    CONCATENATE v_host v_puerto INTO v_host SEPARATED BY space.
*  ELSE.
*    v_status = c_1.
*    CONCATENATE '006' 'E' 'Puerto FTP no encontrado.' INTO v_msg SEPARATED BY v_separador.
*    tb_msg-wa = v_msg.
*    APPEND tb_msg.
*  ENDIF.
*
** Cargamos el Password FTP
*  SELECT SINGLE valor1
*    FROM zgetdconst
*    INTO v_clave
*    WHERE modulo EQ c_mm
*      AND proyec EQ c_recepcion
*      AND aplica EQ c_dispositivos
*      AND campo  EQ c_clave.
*
*  TRANSLATE v_clave TO LOWER CASE.
*  IF sy-subrc NE 0.
*    v_status = c_1.
*    CONCATENATE '005' 'E' 'Password no encontrado.' INTO v_msg SEPARATED BY v_separador.
*    tb_msg-wa = v_msg.
*    APPEND tb_msg.
*  ENDIF.
*
** Cargamos el Destino RFC
*  SELECT SINGLE valor1
*    FROM zgetdconst
*    INTO v_rfc_destination
*    WHERE modulo EQ c_mm
*      AND proyec EQ c_recepcion
*      AND aplica EQ c_dispositivos
*      AND campo  EQ c_rfc_dest.
*
*  IF sy-subrc NE 0.
*    v_status = c_1.
*    CONCATENATE '004' 'E' 'Destino RFC no encontrado.' INTO v_msg SEPARATED BY v_separador.
*    tb_msg-wa = v_msg.
*    APPEND tb_msg.
*  ENDIF.

ENDFORM.                    " CARGAR_CONSTANTES
*&---------------------------------------------------------------------*
*&      Form  SEND_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_server TABLES tb_msg STRUCTURE tab512 CHANGING us_status TYPE c.

  SET EXTENDED CHECK OFF.
  w_longitud = strlen( v_clave ).

  CALL FUNCTION 'HTTP_SCRAMBLE'
    EXPORTING
      source      = v_clave
      sourcelen   = w_longitud
      key         = i_key
    IMPORTING
      destination = v_clave.

  CALL FUNCTION 'FTP_CONNECT'
    EXPORTING
      user            = v_user
      password        = v_clave
      host            = v_host
      rfc_destination = v_rfc_destination
    IMPORTING
      handle          = w_handle
    EXCEPTIONS
      not_connected   = 1
      OTHERS          = 2.

* Si hubo error de conexión, captura la excepción y sale.
  IF sy-subrc NE 0.
    RAISE not_connected.
  ELSE.
*   Se Agrega el Comando a la Ruta de donde se va a dejar el archivo.
    CLEAR commands.
    CONCATENATE 'put' v_archivo INTO commands SEPARATED BY space.

    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        handle        = w_handle
        command       = commands
        compress      = 'N'
      TABLES
        data          = li_result
      EXCEPTIONS
        command_error = 1
        tcpip_error   = 2
        data_error    = 3.

*   Si hubo error al ejecutar los comandos, captura la excepción y sale
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1.
          RAISE command_error.
        WHEN 2.
          RAISE tcpip_error.
        WHEN 3.
          RAISE data_error.
      ENDCASE.
    ELSE.
      CALL FUNCTION 'FTP_DISCONNECT'
        EXPORTING
          handle = w_handle.
      CALL FUNCTION 'RFC_CONNECTION_CLOSE'
        EXPORTING
          destination = v_rfc_destination
        EXCEPTIONS
          OTHERS      = 1.

      us_status = c_0.
      CONCATENATE '003' 'S' 'Proceso realizado correctamente.' INTO tb_msg-wa SEPARATED BY v_separador.
      APPEND tb_msg.
    ENDIF.
  ENDIF.
ENDFORM.                    " SEND_SERVER
*&---------------------------------------------------------------------*
*&      Form  VALIDA_PARAMETRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_RFID  text
*      -->P_P_TRANQUERA  text
*      -->P_P_FECHA  text
*      -->P_P_HORA  text
*      <--P_V_STATUS  text
*----------------------------------------------------------------------*
FORM valida_parametro  TABLES   tb_msg STRUCTURE tab512
                       USING    us_rfid TYPE char10
                                us_tranquera TYPE char10
                       CHANGING us_status TYPE c.
  SELECT SINGLE *
    FROM znstb_trfid
    INTO wa_rfid
    WHERE trfid_id EQ us_rfid.

  IF sy-subrc EQ c_0 AND wa_rfid-estado_rfid EQ c_x.
  ELSE.
    v_status = c_1.
    CONCATENATE '001' 'E' 'El Código RFID no existe o no está activo.' INTO tb_msg-wa SEPARATED BY v_separador.
    APPEND tb_msg.
  ENDIF.


  SELECT SINGLE *
    FROM znstb_tranquera
    INTO wa_tranquera
    WHERE tranq_id EQ us_tranquera.

  IF sy-subrc EQ c_0 AND wa_tranquera-estado EQ c_x.
  ELSE.
    v_status = c_1.
    CONCATENATE '002' 'E' 'El Código de Tranquera no existe o no está activo.' INTO tb_msg-wa SEPARATED BY v_separador.
    APPEND tb_msg.
  ENDIF.

  SELECT SINGLE *
    FROM znstb_asig_trfid
    INTO wa_asignacion
    WHERE tranq_id EQ us_tranquera
      AND trfid_id EQ us_rfid.

  IF sy-subrc EQ c_0.
  ELSE.
    v_status = c_1.
    CONCATENATE '003' 'E' 'No existe una Asignación para los códigos RFID y Tranquera.' INTO tb_msg-wa SEPARATED BY v_separador.
    APPEND tb_msg.
  ENDIF.

ENDFORM.                    " VALIDA_PARAMETRO
*&---------------------------------------------------------------------*
*&      Form  OBTENER_TICKET_IDCOLA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_VTTK_TDLNR  text
*      -->P_L_VSTEL_DEST  text
*      -->P_L_FECHA  text
*      -->P_L_HORA  text
*      <--P_L_TICKET  text
*      <--P_L_IDCOLA  text
*      <--P_FECHA  text
*----------------------------------------------------------------------*
FORM obtener_ticket_idcola  USING    p_transp_ruc  p_vstel_dest
                                     p_fecha  p_hora
                            CHANGING p_ticket p_idcola p_error p_msg
                                      .
  DATA  : l_tiptrn        TYPE c,
          l_numvv         TYPE znstb_correls-numvv,
          l_corre         TYPE znstb_correls-corre,
          l_tipvis        TYPE znstb_correls-tipvis,
          l_corrvis       TYPE znstb_correls-corrvis,
          l_ruc           TYPE zgetdconst-valor1,
          lw_lfa1         TYPE lfa1,
          ltxt_corrvis(2) TYPE c,
          ltxt_corre(5)   TYPE c.


*  IF p_transp_ruc IS NOT INITIAL  .                                    "NETSOL-CCH01092015-
*   Identificar si es propio o tercero
  SELECT SINGLE valor1 INTO l_ruc
    FROM zgetdconst
    WHERE modulo = 'MM'
      AND proyec = 'RECEPCION '
      AND aplica = 'CONSTANTES'
      AND campo  = 'RUC'
      AND correl = '0001'
      AND valor1 = p_transp_ruc  .
  IF sy-subrc EQ 0  .
    l_tiptrn = 'P' .
  ELSE.
    l_tiptrn = 'T' .
  ENDIF.
*   Leo/actualizo tabla
  SELECT SINGLE numvv corre tipvis corrvis
    INTO (l_numvv,l_corre, l_tipvis,l_corrvis)
      FROM znstb_correls
      WHERE vstel  = p_vstel_dest AND
            tiptrn = l_tiptrn  AND
            status = '1'.
  IF sy-subrc NE 0.
    p_error = 'X'.
    p_msg = text-m24  .
    EXIT  .
  ELSE.
*     incremento de campo l_corre
    IF l_corre = 99999.
      l_corre = 00001.
    ELSE.
      l_corre = l_corre + 1.
    ENDIF.
*     incremento de campo l_corrvis
    IF l_corrvis = 99.
      l_corrvis = 01.
*         incremento de campo l_numvv
      IF l_numvv = 99999.
        CLEAR l_numvv.
      ELSE.
        l_numvv = l_numvv + 1.
      ENDIF.
    ELSE.
      l_corrvis = l_corrvis + 1.
    ENDIF.
  ENDIF.

  UPDATE znstb_correls SET numvv    = l_numvv
                           corre    = l_corre
                           corrvis  = l_corrvis
                           luser    = sy-uname
                           ldate    = p_fecha
                           ltime    = p_hora
         WHERE vstel  = p_vstel_dest AND
               tiptrn = l_tiptrn  .
  IF sy-subrc NE 0.
    p_error = 'X'.
    p_msg = text-m23  .
    EXIT  .
  ELSE.
*     Armo el ticket
    WRITE l_corrvis TO ltxt_corrvis.
    CONCATENATE l_tipvis ltxt_corrvis INTO p_ticket.
*     Armo la cola
    WRITE l_corre TO ltxt_corre.
    CONCATENATE l_tiptrn ltxt_corre INTO p_idcola.
  ENDIF.
*  ENDIF.                                                               "NETSOL-CCH01092015-
ENDFORM.                    " OBTENER_TICKET_IDCOLA
*&---------------------------------------------------------------------*
*&      Form  CANT_DISPONIBLE_PEDIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EKPO_EBELN  text
*      <--P_L_ERROR  text
*----------------------------------------------------------------------*
FORM validar_cantidad_transito  TABLES p_ti_lips STRUCTURE lips
                                       p_ti_ekpo STRUCTURE ekpo
                                       p_ti_ekbe STRUCTURE ekbe
                                CHANGING p_cant p_um p_error
                                         p_msg .                        "NETSOL-CCH04092015+

  DATA  : lt_ekbe     TYPE STANDARD TABLE OF ekbe,
          lw_ekbe     TYPE ekbe,
          lv_acumu    TYPE ekbe-menge,
          lv_acumu_em TYPE ekbe-menge  ,                                "NETSOL-CCH04092015+
          lv_pendi    TYPE ekbe-menge,
          lv_permi    TYPE ekbe-menge,
          lv_tottr    TYPE ekbe-menge,
          lv_meins    TYPE lips-meins,
          lv_tottrgr  TYPE lips-lfimg.

  IF p_ti_ekpo[] IS NOT INITIAL .

    SELECT * INTO TABLE lt_ekbe FROM ekbe
      FOR ALL ENTRIES IN p_ti_ekpo
      WHERE ebeln = p_ti_ekpo-ebeln
        AND ebelp = p_ti_ekpo-ebelp .
*        AND vgabe = '6'                                                "NETSOL-CCH04092015-
*        AND bewtp = 'U'.                                               "NETSOL-CCH04092015-
* Total enviado
    SORT  lt_ekbe .

    LOOP AT p_ti_ekpo .
      CLEAR:  lv_acumu, lv_tottr, lv_pendi  .
      LOOP AT lt_ekbe INTO lw_ekbe WHERE ebeln = p_ti_ekpo-ebeln
                                     AND ebelp = p_ti_ekpo-ebelp
                                     AND vgabe = '6'                    "NETSOL-CCH04092015-
                                     AND bewtp = 'U'.                   "NETSOL-CCH04092015-
        IF lw_ekbe-bwart  = '641' .
          lv_acumu  = lv_acumu  + lw_ekbe-menge .
        ELSEIF lw_ekbe-bwart  = '642' .                                 "NETSOL-CCH04092015
          lv_acumu  = lv_acumu  - lw_ekbe-menge .
        ENDIF .
*       Total en transito .
        READ TABLE p_ti_lips WITH KEY vbeln = lw_ekbe-xblnr .
        IF sy-subrc EQ 0.
          lv_tottr  = lv_tottr  + p_ti_lips-lfimg .
        ENDIF.
      ENDLOOP.
*       Acumula EM                                                      "BE NETSOL-CCH04092015+
      LOOP AT lt_ekbe INTO lw_ekbe WHERE ebeln = p_ti_ekpo-ebeln
                                     AND ebelp = p_ti_ekpo-ebelp
                                     AND vgabe = '1'                    "NETSOL-CCH04092015-
                                     AND bewtp = 'E'.                   "NETSOL-CCH04092015-
        IF lw_ekbe-bwart  = '101' .
          lv_acumu_em  = lv_acumu_em  + lw_ekbe-menge .
        ELSEIF lw_ekbe-bwart  = '102' .  .
          lv_acumu_em  = lv_acumu_em  - lw_ekbe-menge .
        ENDIF .
      ENDLOOP.
*     carga final
      IF ( p_ti_ekpo-menge - lv_acumu_em  ) LE 0  .
        p_error = 'X' .
        CONCATENATE text-m82 ':' p_ti_ekpo-ebeln text-m87 '.' INTO p_msg"NETSOL-CCH04092015+
        SEPARATED BY space .                                            "NETSOL-CCH04092015+
        EXIT  .
      ENDIF.
*       Acumula EM                                                      "BE NETSOL-CCH04092015+
      lv_pendi  = p_ti_ekpo-menge - ( lv_acumu - lv_tottr ) .

      IF ( lv_pendi - lv_tottr ) LT 0  .
        p_error = 'X' .
        CONCATENATE text-m82 ':' p_ti_ekpo-ebeln text-m11 '.' INTO p_msg"NETSOL-CCH04092015+
        SEPARATED BY space .                                            "NETSOL-CCH04092015+
        EXIT  .
      ENDIF.

    ENDLOOP.

*   Total acumulado en las guias de remisión  .
    LOOP AT p_ti_lips .
      lv_tottrgr = lv_tottrgr   + p_ti_lips-lfimg  .
      lv_meins  = p_ti_lips-meins .
    ENDLOOP.

    p_cant = lv_tottrgr  .
    p_um  = lv_meins  .

  ENDIF.

ENDFORM.                    " CANT_DISPONIBLE_PEDIDO
*&---------------------------------------------------------------------*
*&      Form  OBTENER_TRFID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*      <--P_L_TRFID  text
*----------------------------------------------------------------------*
FORM obtener_trfid  USING    p_material p_zona_id p_tranq_id
                    CHANGING p_trfid_id p_trfid_nro p_error p_msg .

  DATA: w_zonamat     TYPE znstb_zonamat,
        w_asig_trfid  TYPE znstb_asig_trfid,
        w_dti_trfid   TYPE znstb_dti_trfid,
*        w_asig_tranq    TYPE znstb_asig_tranq ,
        lt_asig_trfid TYPE STANDARD TABLE OF znstb_asig_trfid,
        lt_dti_trfid  TYPE STANDARD TABLE OF znstb_dti_trfid.

  RANGES: lr_trfid      FOR  znstb_trfid-trfid_id .

*
*  SELECT SINGLE * INTO w_asig_tranq FROM znstb_asig_tranq
*    WHERE zona_id   = p_zona_id
*      AND tranq_id  = p_tranq_id .
*  IF sy-subrc EQ 0  .
*   Obtener TRFID(s) por tranquera.
  SELECT * INTO TABLE lt_asig_trfid FROM znstb_asig_trfid
    WHERE tranq_id  = p_tranq_id .
  IF  sy-subrc EQ 0 .
*       Excluir en uso  .
    SELECT * INTO TABLE lt_dti_trfid FROM znstb_dti_trfid
      FOR ALL ENTRIES IN lt_asig_trfid
      WHERE trfid_id  = lt_asig_trfid-trfid_id
        AND estado    = 'X' .
    IF sy-subrc EQ 0  .

      lr_trfid-sign   = 'I' .
      lr_trfid-option = 'EQ' .
      LOOP AT lt_dti_trfid  INTO w_dti_trfid.
        lr_trfid-low =  w_dti_trfid-trfid_id .
        APPEND lr_trfid .
      ENDLOOP.
      DELETE lt_asig_trfid WHERE trfid_id IN lr_trfid[] .

    ENDIF.

    SORT lt_asig_trfid BY trfid_nro .
*       Obtener maestro de trfid
    READ TABLE lt_asig_trfid  INTO w_asig_trfid INDEX 1.
    IF sy-subrc EQ 0 .

      PERFORM conversion_exit_alpha_input USING w_asig_trfid-trfid_id
                                          CHANGING p_trfid_id .

      PERFORM conversion_exit_alpha_input USING w_asig_trfid-trfid_nro
                                          CHANGING p_trfid_nro .

    ELSE.
      p_error = 'X' .
      p_msg = text-m33  .
      EXIT.
    ENDIF.
  ELSE.
    p_error = 'X' .
    p_msg = text-m26  .
    EXIT.
  ENDIF.
*  ELSE.
*    p_error = 'X' .
*    p_msg = text-m25  .
*    EXIT.
*  ENDIF.

ENDFORM.                    " OBTENER_TRFID
*&---------------------------------------------------------------------*
*&      Form  CARGAR_CONSTANTES_LA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cargar_constantes_la .
*  CLEAR: v_separador.
*
*  SELECT *  INTO TABLE  ti_const  FROM zgetdconst
*    WHERE modulo EQ c_mm
*      AND proyec EQ c_recepcion
*      AND aplica EQ c_constantes  .
*
*  LOOP AT ti_const  INTO w_const  .
*
*    CASE w_const-campo  .
*
*      WHEN c_charsep  .
*        v_separador = w_const-valor1 .
*      WHEN c_ev_rcola .
*        v_evento_id = w_const-valor1 .
*      WHEN OTHERS.
*    ENDCASE.
*  ENDLOOP.
*
*  SELECT *  INTO TABLE  ti_const  FROM zgetdconst
*    WHERE modulo EQ c_mm
*      AND proyec EQ c_recepcion
*      AND aplica EQ c_dispositivos  .
*
*  LOOP AT ti_const  INTO w_const  .
*
*    CASE w_const-campo  .
*      WHEN c_file21  .
*        v_file = w_const-valor1 .
*      WHEN c_path .
*        v_ruta = w_const-valor1 .
*      WHEN c_server .
*        v_server = w_const-valor1 .
*      WHEN c_usuario .
*        v_usuario = w_const-valor1 .
*      WHEN OTHERS.
*    ENDCASE.

*  ENDLOOP.

ENDFORM.                    " CARGAR_CONSTANTES_LA
*&---------------------------------------------------------------------*
*&      Form  VALIDA_PARAMETRO_LA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_MSG  text
*      -->P_I_VSTEL  text
*      <--P_V_STATUS  text
*----------------------------------------------------------------------*
FORM valida_parametro_la  TABLES   tb_msg STRUCTURE tab512
                          USING    us_vstel  TYPE vstel
                          CHANGING us_status TYPE c.
  SELECT SINGLE *
    FROM tvst
    INTO wa_tvst
    WHERE vstel EQ us_vstel.

  IF sy-subrc NE c_0.
    v_status = c_1.
    CONCATENATE 'Puesto de Expedición' us_vstel 'no existe' INTO v_msg SEPARATED BY space.
    CONCATENATE '001' 'E' v_msg INTO tb_msg-wa SEPARATED BY v_separador.
    APPEND tb_msg.
  ENDIF.

ENDFORM.                    " VALIDA_PARAMETRO_LA
*&---------------------------------------------------------------------*
*&      Form  OBTENER_DTI_NUEVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_VSTEL_DEST  text
*      <--P_L_DTI_ID  text
*----------------------------------------------------------------------*
FORM obtener_dti_nuevo  USING    p_vstel_dest
                        CHANGING p_dti_id p_error p_msg .

  DATA: lv_tknum_id    TYPE znstbflujoreccab-tknum_id .

  SELECT MAX( tknum_id ) INTO lv_tknum_id
    FROM znstbflujoreccab UP TO 1 ROWS
    WHERE vstel  EQ p_vstel_dest .

  ADD 1 TO  lv_tknum_id .

  PERFORM conversion_exit_alpha_input USING lv_tknum_id
                                      CHANGING p_dti_id .

  IF p_dti_id IS INITIAL .
    p_error = 'X' .
    p_msg   = text-m13  .
    EXIT  .
  ENDIF.

ENDFORM.                    " OBTENER_DTI_NUEVO
*&---------------------------------------------------------------------*
*&      Form  VALIDA_PARAMETRO_CT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_MSG  text
*      -->P_I_VSTEL  text
*      -->P_I_TICKET  text
*      <--P_V_STATUS  text
*----------------------------------------------------------------------*
FORM valida_parametro_ct  TABLES   tb_msg STRUCTURE tab512
                          USING    us_vstel  TYPE vstel
                                   us_ticket TYPE char10
                          CHANGING us_status TYPE c.
  SELECT SINGLE *
    FROM tvst
    INTO wa_tvst
    WHERE vstel EQ us_vstel.

  IF sy-subrc NE c_0.
    v_status = c_1.
    CONCATENATE 'Puesto de Expedición' us_vstel 'no existe' INTO v_msg SEPARATED BY space.
    CONCATENATE '001' 'E' v_msg INTO tb_msg-wa SEPARATED BY v_separador.
    APPEND tb_msg.
  ENDIF.


ENDFORM.                    " VALIDA_PARAMETRO_CT
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_PLACA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_PLACA  text
*      <--P_L_ERROR  text
*----------------------------------------------------------------------*
FORM validar_placa  USING    p_placa
                    CHANGING s_mara .

  CLEAR : s_mara  .

  TRANSLATE p_placa TO UPPER CASE .                                     "NETSOL-CCH03092015+

  SELECT SINGLE * INTO s_mara FROM mara
    WHERE bismt  EQ p_placa .
  IF sy-subrc NE  0 .

  ENDIF.

ENDFORM.                    " VALIDAR_PLACA
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_INTERLOCUTOR_NIF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_STCD1  text
*      <--P_W_LFA1  text
*      <--P_L_ERROR  text
*----------------------------------------------------------------------*
FORM validar_interlocutor_nif  USING    p_nif
                               CHANGING s_lfa1  .

  CLEAR : s_lfa1  .

  SELECT SINGLE * INTO s_lfa1 FROM lfa1
    WHERE stcd1 = p_nif .
  IF sy-subrc NE 0  .

  ENDIF .

ENDFORM.                    " VALIDAR_INTERLOCUTOR_NIF
*&---------------------------------------------------------------------*
*&      Form  CARGAR_LOG_MENSAJE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cargar_log_mensaje  TABLES   p_t_msg STRUCTURE ztab256
                         USING    p_tipo
                                  p_msg
                                  p_sep         .

  DATA  : lv_canti     TYPE i,
          lv_canti_txt TYPE char03,
          lv_tipom     TYPE c,
          lv_msg       TYPE string.

  CLEAR:  lv_canti  , lv_canti_txt, lv_tipom  , lv_msg.

  DESCRIBE TABLE p_t_msg  LINES lv_canti  .

  IF lv_canti  > 0.
    READ TABLE  p_t_msg INDEX lv_canti  .
    SPLIT p_t_msg-wa  AT v_separador INTO lv_canti_txt lv_tipom lv_msg .
  ENDIF.

  ADD 1 TO lv_canti_txt .

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_canti_txt
    IMPORTING
      output = lv_canti_txt.

  CLEAR : lv_msg  .
  CONCATENATE p_tipo p_msg INTO lv_msg
  SEPARATED BY v_separador  .

  PERFORM add_cline TABLES p_t_msg  USING lv_canti_txt lv_msg v_separador .

ENDFORM.                    " CARGAR_LOG_MENSAJE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CARGAR_LOG_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cargar_log_status  TABLES   p_t_status STRUCTURE ztab256
                                 p_t_msg    STRUCTURE ztab256
                        CHANGING p_error  .

  DATA  : lv_canti     TYPE i,
          lv_canti_txt TYPE char03,
          lv_tipom     TYPE c,
          lv_msg       TYPE string.

  CLEAR:  lv_canti  , lv_canti_txt, lv_tipom  , lv_msg.

  DESCRIBE TABLE p_t_msg  LINES lv_canti  .

  IF lv_canti  > 0.
    READ TABLE  p_t_msg INDEX lv_canti  .
    SPLIT p_t_msg-wa  AT '¬' INTO lv_canti_txt lv_tipom lv_msg .
  ENDIF.

  LOOP AT p_t_msg .
    SPLIT p_t_msg-wa  AT '¬' INTO lv_canti_txt lv_tipom lv_msg .
    IF lv_tipom EQ 'E'  .
      EXIT  .
    ENDIF.
  ENDLOOP.

  IF lv_tipom EQ 'E'  .
    p_error = 'X' .
    PERFORM add_cline TABLES p_t_status   USING c_ko text-s01 v_separador.
  ELSE.
    p_error = space .
    PERFORM add_cline TABLES p_t_status   USING c_ok text-s02 v_separador.
  ENDIF.

ENDFORM.                    " CARGAR_LOG_MENSAJE
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_PEDIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_PEDIDO  text
*      <--P_L_ERROR  text
*----------------------------------------------------------------------*
FORM validar_pedido  TABLES   p_t_pedido STRUCTURE znses_pedidorec
                              p_ti_ekko STRUCTURE ekko
                              p_ti_ekpo STRUCTURE ekpo
                              p_ti_ekbe STRUCTURE ekbe
                     CHANGING p_tipo_p
                              p_material
                              p_provee_id
                              p_destin_id
                              p_cant_tot_gr
                              p_um
                              p_msg
                              p_error .
*INSERT {+AGL@76405
  TYPES: BEGIN OF lty_znstbflujorecdet,
           tknum_id  TYPE znstbflujorecdet-tknum_id,
           ebeln     TYPE znstbflujorecdet-ebeln,
           evento_id TYPE znstbflujorecdet-evento_id,
           peso_gr   TYPE znstbflujorecdet-peso_gr,
         END OF lty_znstbflujorecdet.

  DATA: ltd_znstbflujorecdet TYPE STANDARD TABLE OF lty_znstbflujorecdet,
        lwa_znstbflujorecdet TYPE lty_znstbflujorecdet,
        lv_saldo_pc          TYPE znstbflujorecdet-peso_gr,
        lv_saldo_sap         TYPE znstbflujorecdet-peso_gr,
        lv_peso_gr           TYPE znstbflujorecdet-peso_gr.
*INSERT }+AGL@76405

  DATA: lv_canti     TYPE i,
        lv_peson_txt TYPE char20,
        lv_cant_tot  TYPE lips-lfimg,
        lv_cant      TYPE lips-lfimg ,                                 "NETSOL-CCH31072015+
        lv_um        TYPE meins,
        lt_ekko      TYPE STANDARD TABLE OF ekko,
        lt_ekpo      TYPE STANDARD TABLE OF ekpo,
        lv_ingr_tot  TYPE ekbe-menge ,                                 "NETSOL-CCH07082015+
        lt_ekbe      TYPE STANDARD TABLE OF ekbe.


  REFRESH: lt_ekko, lt_ekpo, lt_ekbe, p_ti_ekko, p_ti_ekpo, p_ti_ekbe.

  LOOP AT p_t_pedido  .
*   Validar pedido .
    SELECT * INTO TABLE lt_ekko FROM ekko
      WHERE ebeln = p_t_pedido-ebeln  .
    IF sy-subrc NE 0 .
      p_error = 'X' .
      p_msg   = text-m35  .
      EXIT .
    ENDIF.

    SELECT * INTO TABLE lt_ekpo FROM ekpo
      WHERE ebeln = p_t_pedido-ebeln  .

    DESCRIBE TABLE p_ti_ekpo LINES lv_canti .

    IF lv_canti  > 1 .
      p_error = 'X' .
      p_msg   = text-m34  .
      EXIT .
    ENDIF.

    IF p_t_pedido-gremi IS INITIAL .
      p_error = 'X' .
      p_msg   = text-m37  .
      EXIT .
    ELSEIF p_t_pedido-peson IS INITIAL .
      p_error = 'X' .
      p_msg   = text-m38  .
      EXIT .
    ELSEIF p_t_pedido-meins IS INITIAL .
      p_error = 'X' .
      p_msg   = text-m38  .
      EXIT .
    ENDIF.
* Valido si el peso a registrar tiene solo números                      "Begin NETSOL-CCH07082015+
    CONDENSE p_t_pedido-peson .
    CALL FUNCTION 'MOVE_CHAR_TO_NUM'
      EXPORTING
        chr             = p_t_pedido-peson
      IMPORTING
        num             = lv_cant
      EXCEPTIONS
        convt_no_number = 1
        convt_overflow  = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      p_error = 'X' .
      p_msg   = text-m60  .
      EXIT  .
    ENDIF .
* Valido si el peso a registrar tiene solo números                      "End NETSOL-CCH07082015+
*    CONDENSE p_t_pedido-peson NO-GAPS .                                 "NETSOL-CCH31072015+
*    lv_cant = p_t_pedido-peson  .                                       "NETSOL-CCH31072015+
    lv_cant_tot  = lv_cant_tot  + lv_cant  .                            "NETSOL-CCH31072015+
*    lv_cant_tot  = lv_cant_tot  + p_t_pedido-peson  .                  "NETSOL-CCH31072015-
    lv_um        = p_t_pedido-meins .

    APPEND LINES OF: lt_ekko[] TO p_ti_ekko[] ,
                     lt_ekpo[] TO p_ti_ekpo[] .

  ENDLOOP.

  READ TABLE p_ti_ekko  INDEX 1   .
  IF p_ti_ekko-bsart  = 'ZU'  .
    p_provee_id = p_ti_ekko-reswk  .
    p_tipo_p    =  'T'  .
  ELSE.
    p_provee_id = p_ti_ekko-lifnr  .
    p_tipo_p    =  'C'  .
  ENDIF.

  READ TABLE p_ti_ekpo INDEX 1.
  p_material  = p_ti_ekpo-matnr .
  p_destin_id = p_ti_ekpo-werks .

  p_cant_tot_gr = lv_cant_tot .
  p_um  = lv_um .

* Validar saldos                                                        "Begin NETSOL-CCH07082015+
  IF p_error = 'X' .
    EXIT  .
  ENDIF .

  PERFORM cambiar_medida USING p_ti_ekpo-meins v_meins lv_cant p_cant_tot_gr. "NETSOL-CCCE20171128+

  SELECT * INTO TABLE lt_ekbe FROM ekbe
    FOR ALL ENTRIES IN p_t_pedido
    WHERE ebeln EQ p_t_pedido-ebeln
      AND bewtp EQ 'E'  .
  IF sy-subrc EQ 0  .
    CLEAR : lv_ingr_tot .
    LOOP AT lt_ekbe INTO w_ekbe .
      IF w_ekbe-bwart EQ '101'  .
        lv_ingr_tot = lv_ingr_tot + w_ekbe-menge .
      ELSEIF w_ekbe-bwart EQ '102'  .
        lv_ingr_tot = lv_ingr_tot - w_ekbe-menge .
      ENDIF .
    ENDLOOP.

*    PERFORM cambiar_medida USING p_ti_ekpo-meins v_meins lv_cant p_cant_tot_gr.  "NETSOL-CCCE20171128-

*    IF ( lv_ingr_tot + lv_cant ) > p_ti_ekpo-menge + ( p_ti_ekpo-uebto * p_ti_ekpo-menge ) ."/AGL@76405-
    IF ( lv_ingr_tot + lv_cant ) > p_ti_ekpo-menge + ( p_ti_ekpo-menge * p_ti_ekpo-uebto / 100 ) ."/AGL@76405+
      p_error = 'X' .
      p_msg   = text-m11  .
      EXIT .
    ENDIF.
  ENDIF.
* Validar saldos
*"End NETSOL-CCH07082015+

**INSERT {+AGL@76405
*Validacion de saldos de pedidos de compras
  IF w_flujoreccab-tknum_id IS NOT INITIAL AND
     w_flujoreccab-vstel IS NOT INITIAL.

    SELECT DISTINCT znstbflujoreccab~tknum_id  znstbflujorecdet~ebeln znstbflujoreccab~evento_id znstbflujorecdet~peso_gr
    INTO TABLE ltd_znstbflujorecdet
    FROM znstbflujoreccab
    INNER JOIN znstbflujorecdet
    ON znstbflujoreccab~tknum_id = znstbflujorecdet~tknum_id AND
       znstbflujoreccab~vstel    = znstbflujorecdet~vstel
    FOR ALL ENTRIES IN p_t_pedido
    WHERE NOT ( znstbflujoreccab~tknum_id EQ w_flujoreccab-tknum_id AND
            znstbflujoreccab~vstel EQ w_flujoreccab-vstel ) AND
          znstbflujorecdet~ebeln      EQ p_t_pedido-ebeln AND
          znstbflujorecdet~mblnr      EQ space AND
          znstbflujoreccab~evento_id  NE '0600'.

  ELSE.
    SELECT DISTINCT znstbflujoreccab~tknum_id  znstbflujorecdet~ebeln znstbflujoreccab~evento_id znstbflujorecdet~peso_gr
    INTO TABLE ltd_znstbflujorecdet
    FROM znstbflujoreccab
    INNER JOIN znstbflujorecdet
    ON znstbflujoreccab~tknum_id = znstbflujorecdet~tknum_id AND
       znstbflujoreccab~vstel    = znstbflujorecdet~vstel
    FOR ALL ENTRIES IN p_t_pedido
    WHERE znstbflujorecdet~ebeln      EQ p_t_pedido-ebeln AND
          znstbflujorecdet~mblnr      EQ space AND
          znstbflujoreccab~evento_id  NE '0600'.

  ENDIF.

**Total Peso GR en proceso.
  LOOP AT ltd_znstbflujorecdet INTO lwa_znstbflujorecdet.
    lv_peso_gr = lv_peso_gr + lwa_znstbflujorecdet-peso_gr.
  ENDLOOP.

  lv_saldo_sap  =  p_ti_ekpo-menge - lv_ingr_tot + ( p_ti_ekpo-menge * p_ti_ekpo-uebto / 100 ).
  PERFORM cambiar_medida USING p_ti_ekpo-meins p_um lv_peso_gr lv_peso_gr.
  lv_saldo_pc   =  lv_saldo_sap - lv_peso_gr.

  IF lv_cant > lv_saldo_pc.
    p_error = 'X' .
    p_msg   = text-m11.
    EXIT.
  ENDIF.
*Validación de entrega final.
  IF p_ti_ekpo-elikz EQ 'X' .
    p_error = 'X'.
    CONCATENATE text-m97 p_t_pedido-ebeln INTO p_msg SEPARATED BY space.
  ENDIF.
*INSERT }+AGL@76405

ENDFORM.                    " VALIDAR_PEDIDO
*&---------------------------------------------------------------------*
*&      Form  OBTENER_DT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_VTTP  text
*      -->P_W_FLUJODESPCAB_TKNUM  text
*      <--P_W_VTTK  text
*----------------------------------------------------------------------*
FORM obtener_info_dt  TABLES   p_ti_vttp STRUCTURE vttp
                 USING    p_tknum
                 CHANGING s_vttk  TYPE vttk.

  REFRESH : p_ti_vttp .
  CLEAR:  s_vttk, p_ti_vttp .

  SELECT SINGLE  *  INTO s_vttk  FROM vttk
    WHERE tknum = p_tknum .
  IF sy-subrc EQ 0  .
    SELECT * INTO TABLE p_ti_vttp FROM vttp
      WHERE tknum EQ s_vttk-tknum  .
  ENDIF.

ENDFORM.                    " OBTENER_DT
*&---------------------------------------------------------------------*
*&      Form  OBTENER_INFO_GR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_LIKP  text
*      -->P_TI_LIPS  text
*      -->P_TI_VTTP  text
*----------------------------------------------------------------------*
FORM obtener_info_gr  TABLES   p_ti_likp STRUCTURE likp
                               p_ti_lips STRUCTURE lips
                               p_ti_vttp STRUCTURE vttp .

  REFRESH : p_ti_likp , p_ti_lips .

  SELECT * INTO TABLE p_ti_likp FROM likp
    FOR ALL ENTRIES IN p_ti_vttp
      WHERE vbeln EQ p_ti_vttp-vbeln .

  IF sy-subrc EQ 0 .
    SELECT * INTO TABLE p_ti_lips FROM lips
      FOR ALL ENTRIES IN p_ti_likp
      WHERE vbeln EQ p_ti_likp-vbeln .
  ENDIF .

ENDFORM.                    " OBTENER_INFO_GR
*&---------------------------------------------------------------------*
*&      Form  OBTENER_INFO_PEDIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_EKKO  text
*      -->P_TI_EKPO  text
*      -->P_TI_EKBE  text
*      -->P_TI_LIKP  text
*----------------------------------------------------------------------*
FORM obtener_info_pedido  TABLES   p_ti_ekko STRUCTURE ekko
                                   p_ti_ekpo STRUCTURE ekpo
                                   p_ti_ekbe STRUCTURE ekbe
                                   p_ti_likp STRUCTURE likp.

  RANGES: lr_xblnr  FOR likp-xblnr  .

  REFRESH : p_ti_ekko, p_ti_ekpo, p_ti_ekbe .

  IF p_ti_likp[] IS NOT INITIAL .

    lr_xblnr-sign = 'I' .
    lr_xblnr-option = 'EQ' .
    LOOP AT p_ti_likp .
      lr_xblnr-low =  p_ti_likp-vbeln .
      APPEND lr_xblnr .
    ENDLOOP.

    SELECT * INTO TABLE p_ti_ekbe FROM ekbe
    WHERE xblnr IN lr_xblnr  .
    IF  p_ti_ekbe[]  IS NOT INITIAL .

      SELECT * INTO TABLE p_ti_ekko FROM ekko
        FOR ALL ENTRIES IN p_ti_ekbe
        WHERE ebeln = p_ti_ekbe-ebeln  .

      SELECT * INTO TABLE p_ti_ekpo FROM ekpo
        FOR ALL ENTRIES IN p_ti_ekbe
        WHERE ebeln = p_ti_ekbe-ebeln
          AND ebelp = p_ti_ekbe-ebelp  .

    ENDIF .
  ENDIF.

ENDFORM.                    " OBTENER_INFO_PEDIDO
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_ORIGEN_DESTINO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_EKKO  text
*      -->P_TI_EKPO  text
*      <--P_L_TIPO_P  text
*      <--P_L_PROVEE_ID  text
*      <--P_L_DESTIN_ID  text
*----------------------------------------------------------------------*
FORM validar_origen_destino  TABLES   p_ti_ekko STRUCTURE ekko
                                      p_ti_ekpo STRUCTURE ekpo
                             CHANGING p_tipo_p
                                      p_provee_id p_provee_des
                                      p_destin_id p_destin_des .

  DATA  : lv_tipo_p    TYPE ekko-bsart,
          lv_provee_id TYPE char10,
          lv_destin_id TYPE char10,
          lv_name1     TYPE char35,
          lv_name2     TYPE char35.

  CLEAR:  p_tipo_p, p_provee_id,  p_destin_id .

  LOOP AT p_ti_ekko .

    IF p_ti_ekko-bsart = 'ZU' .
      p_tipo_p  = 'T' .
      p_provee_id = p_ti_ekko-reswk .
    ELSE.
      p_tipo_p  = 'C' .
      p_provee_id = p_ti_ekko-lifnr .
    ENDIF.
    CLEAR : p_ti_ekpo .
    READ TABLE p_ti_ekpo  WITH KEY  ebeln = p_ti_ekko-ebeln .
    IF sy-subrc EQ 0.
      p_destin_id = p_ti_ekpo-werks .
    ENDIF.

    AT FIRST  .
      lv_tipo_p     =     p_tipo_p     .
      lv_provee_id  =     p_provee_id  .
      lv_destin_id  =     p_destin_id  .
    ENDAT .

*   Comparar origen y destino .
    IF lv_tipo_p     NE     p_tipo_p     OR
       lv_provee_id  NE     p_provee_id  OR
       lv_destin_id  NE     p_destin_id   .
      CLEAR : p_tipo_p, p_provee_id, p_destin_id  .
      EXIT  .
    ENDIF.
  ENDLOOP.

* Obtener descripción
  IF  p_tipo_p     IS NOT INITIAL AND
      p_provee_id  IS NOT INITIAL AND
      p_destin_id  IS NOT INITIAL  .

    IF p_tipo_p EQ 'T'  .
      SELECT SINGLE name1 INTO lv_name1 FROM t001w
        WHERE werks EQ  p_provee_id .

      p_provee_des  = lv_name1  .
    ELSE.
      SELECT SINGLE name1 name2 INTO (lv_name1, lv_name2)  FROM lfa1
        WHERE lifnr EQ  p_provee_id .

      CONCATENATE lv_name1 lv_name2 INTO  p_provee_des  SEPARATED BY space  .
    ENDIF.

    SELECT SINGLE name1 INTO lv_name1 FROM t001w
        WHERE werks EQ  p_destin_id .
    IF sy-subrc EQ  0 .
      p_destin_des  = lv_name1  .
    ENDIF.

  ENDIF.

ENDFORM.                    " VALIDAR_ORIGEN_DESTINO
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_DISPO_ZDESCARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM validar_dispo_zdescarga  USING    p_material
                                       p_vstel                          "NETSOL-CCCE26042017+
                              CHANGING p_zona_id  p_tranq_id  p_turno_id
                                       p_turno_zona_ini p_turno_zona_fin
                                       p_color
                                       p_error  p_msg.

  DATA  : lv_flag_turno TYPE c,
          lv_turno_id   TYPE znstb_turnos-turno_id.
* Obteniendo zona por material
  SELECT SINGLE * INTO w_zonamat FROM znstb_zonamat
    WHERE material_id = p_material
      AND vstel       = p_vstel                                         "NETSOL-CCCE26042017+
      AND estado_mat  = 'X'.
  IF sy-subrc EQ 0  .
    SELECT SINGLE * INTO w_zona_des FROM  znstb_zona_des
      WHERE zona_id = w_zonamat-zona_id
        AND vstel   = p_vstel.                                          "NETSOL-CCCE26042017+
    IF sy-subrc NE 0.
      p_error = 'X' .
      p_msg = text-m20  .
      EXIT  .
    ENDIF.

    p_color = w_zona_des-color  .
    p_zona_id = w_zona_des-zona_id  .

    SELECT * INTO TABLE ti_prog_zona FROM znstb_programac
      WHERE zona_id = w_zonamat-zona_id
        AND fecha   = sy-datum
        AND material_id = p_material
        AND vstel       = p_vstel.                                      "NETSOL-CCCE26042017+
    IF sy-subrc EQ 0 .
*     Valida tranquera.
      SELECT SINGLE * INTO w_tranquera  FROM znstb_tranquera
        WHERE tranq_id  = w_zona_des-tranq_id
          AND vstel     = p_vstel.                                      "NETSOL-CCCE26042017+
      IF sy-subrc EQ 0  .
        p_tranq_id =  w_tranquera-tranq_id  .
      ELSE.
        p_error = 'X' .
        p_msg = text-m17  .
        EXIT  .
      ENDIF.

      SELECT  * INTO TABLE ti_turnos FROM znstb_turnos
        FOR ALL ENTRIES IN ti_prog_zona
        WHERE turno_id = ti_prog_zona-turno_id
          AND vstel    = p_vstel.                                       "NETSOL-CCCE26042017+
      IF sy-subrc NE 0  .
        p_error = 'X' .
        p_msg = text-m21  .
        EXIT  .
      ENDIF.

      SORT  : ti_turnos .
      CLEAR : lv_flag_turno .
      LOOP AT ti_turnos INTO w_turnos .
        lv_turno_id   = w_turnos-turno_id .
        AT FIRST .
          p_turno_zona_ini  = lv_turno_id    .
        ENDAT .
        IF sy-uzeit GE w_turnos-hora_ini  AND sy-uzeit LE w_turnos-hora_fin .
          lv_flag_turno  = 'X' .
          p_turno_id = lv_turno_id    .
        ENDIF.
        AT LAST .
          p_turno_zona_fin =  lv_turno_id    .
        ENDAT .
      ENDLOOP.

      IF lv_flag_turno  = 'X' .
      ELSEIF  p_turno_id = space .
        p_error = 'X' .
        p_msg = text-m18  .
        EXIT  .
*      ELSE.
*        p_error = 'X' .
*        p_msg = text-m21  .
*        EXIT  .
      ENDIF.

    ELSE.
      p_error = 'X' .
      p_msg = text-m22  .
      EXIT  .
    ENDIF.
  ELSE.
    p_error = 'X' .
    p_msg = text-m19  .
    EXIT  .
  ENDIF.



ENDFORM.                    " VALIDAR_DISPO_ZDESCARGA
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ASIG_TRFID_TRFID_ID  text
*      <--P_P_TRFID_ID  text
*----------------------------------------------------------------------*
FORM conversion_exit_alpha_input  USING    p_valor1
                                  CHANGING p_valor2 .

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_valor1
    IMPORTING
      output = p_valor2.

ENDFORM.                    " CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_INTERLOCUTOR_LFR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM validar_interlocutor_lfr  USING    p_tdlnr
                               CHANGING s_lfa1  STRUCTURE lfa1  .

  CLEAR : s_lfa1  .

  SELECT SINGLE * INTO s_lfa1 FROM lfa1
    WHERE lifnr = p_tdlnr  .
  IF sy-subrc NE 0  .

  ENDIF .

ENDFORM.                    " VALIDAR_INTERLOCUTOR_LFR
*&---------------------------------------------------------------------*
*&      Form  OBTENER_ESCENARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_LIPS  text
*      -->P_L_MATERIAL  text
*      <--P_L_ESCENARIO_ID  text
*      <--P_L_ERROR  text
*      <--P_L_MSG  text
*----------------------------------------------------------------------*
FORM obtener_escenario  USING    p_material p_vstel
                                 p_tipo_p  p_flag_transp p_flag_dt      "NETSOL-CCH31082015+
                                 p_flag_transp_spec                     "NETSOL-CCCE01032017+
                        CHANGING p_escenario_id
                                 p_error
                                 p_msg.

  DATA  : lv_prodh  TYPE mvke-prodh .
  "BE NETSOL-CCH31082015-
*  SELECT  SINGLE prodh INTO lv_prodh FROM mvke
*    WHERE matnr  = p_material  .
*  IF sy-subrc EQ 0 .
*
*    SELECT SINGLE escenario_id INTO p_escenario_id FROM znstb_escenario
*      WHERE vstel EQ p_vstel
*        AND ( prdha   GE lv_prodh OR
*              prdha_2 LE lv_prodh  ) .
*    IF sy-subrc NE 0 .
*      p_error = 'X' .
*      l_msg = text-m30  .
*    ENDIF.
*
*  ELSE.
*
*    p_error = 'X' .
*    l_msg = text-m39  .
*
*  ENDIF.
  "EE NETSOL-CCH31082015-
  "BE NETSOL-CCH31082015+
  SELECT  SINGLE escenario_id INTO p_escenario_id FROM znstb_material
    WHERE vstel        = p_vstel
      AND material_id  = p_material  .
  IF sy-subrc NE 0 .
    p_error = 'X' .
    CONCATENATE text-m74 p_material INTO l_msg  SEPARATED BY space  .
  ELSE.
*BB NETSOL-CCCE01032017+
*   Escenario pto.Planta&Alm.Prov.
    IF p_tipo_p EQ 'C' AND p_flag_transp_spec EQ abap_true.
      SELECT  SINGLE escenario_id INTO p_escenario_id FROM znstb_escenario
        WHERE vstel         = p_vstel
          AND escenario_id  = p_escenario_id
          AND pedido_tipo   = p_tipo_p
          AND tknum_flag    = p_flag_dt
          AND estado        = 'X'.
      IF sy-subrc NE 0 .
        p_error = 'X' .
        l_msg = text-m30  .
      ENDIF.
    ELSE.
*BE NETSOL-CCCE01032017+
      SELECT  SINGLE escenario_id INTO p_escenario_id FROM znstb_escenario
        WHERE vstel         = p_vstel
          AND escenario_id  = p_escenario_id
          AND pedido_tipo   = p_tipo_p
          AND transporte    = p_flag_transp
          AND tknum_flag    = p_flag_dt
          AND estado        = 'X'.
      IF sy-subrc NE 0 .
        p_error = 'X' .
        l_msg = text-m30  .
      ENDIF.
    ENDIF.                                                              "NETSOL-CCCE01032017+
  ENDIF.
  "EE NETSOL-CCH31082015+
ENDFORM.                    " OBTENER_ESCENARIO
*&---------------------------------------------------------------------*
*&      Form  ACTUALIZAR_FLUJOREC_BD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_FLUJORECCAB  text
*      -->P_TI_FLUJORECDET  text
*----------------------------------------------------------------------*
FORM actualizar_flujorec_bd  TABLES p_ti_flujoreccab STRUCTURE znstbflujoreccab
                                    p_ti_flujorecdet STRUCTURE znstbflujorecdet
                             CHANGING p_msg  p_error .

  MODIFY znstbflujoreccab FROM TABLE p_ti_flujoreccab[] .

  IF sy-subrc EQ 0  .

    MODIFY znstbflujorecdet FROM TABLE p_ti_flujorecdet[] .

    IF sy-subrc EQ 0  .
      COMMIT WORK AND WAIT  .
    ELSE.
      p_msg = text-m31  .
    ENDIF .
  ELSE  .
    p_msg = text-m32  .
  ENDIF .

ENDFORM.                    " ACTUALIZAR_FLUJOREC_BD
*&---------------------------------------------------------------------*
*&      Form  ACTUALIZAR_DTI_TRFID_BD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_DTI_TRFID  text
*      <--P_L_MSG  text
*      <--P_L_ERROR  text
*----------------------------------------------------------------------*
FORM actualizar_dti_trfid_bd  USING    s_dti_trfid STRUCTURE znstb_dti_trfid
                              CHANGING p_msg
                                       p_error.

  IF s_dti_trfid IS NOT INITIAL .

    MODIFY znstb_dti_trfid FROM s_dti_trfid .

    IF sy-subrc EQ 0  .

      COMMIT  WORK AND WAIT .

    ENDIF.

  ENDIF.



ENDFORM.                    " ACTUALIZAR_DTI_TRFID_BD

*&---------------------------------------------------------------------*
*&      Form  GUARDAR_FLUJO_RECEPCION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_FLUJORECCAB  text
*      -->P_TI_FLUJORECDET  text
*      -->P_LV_DDL  text
*----------------------------------------------------------------------*
FORM guardar_flujo_recepcion  TABLES   p_ti_flujoreccab STRUCTURE znstbflujoreccab
                                       p_ti_flujorecdet STRUCTURE znstbflujorecdet
                                       p_ti_vttp        STRUCTURE vttp
                                       p_ti_ekko        STRUCTURE ekko
                                       p_ti_ekbe        STRUCTURE ekbe
                                       p_ti_likp        STRUCTURE likp
                                       p_ti_lips        STRUCTURE lips
                                       p_t_pedido       STRUCTURE znses_pedidorec
                              USING p_flag_dt
                                    p_dti_id
                                    p_tknum
                                    p_vstel_dest
                                    p_vstel_orig
                                    p_transp_ruc
                                    p_chofer_dni
                                    p_tracto
                                    p_carreta
                                    p_material
                                    p_ruta
                                    p_ticket
                                    p_cola_id
                                    p_turno_id
                                    p_trfid_id
                                    p_tranq_id
                                    p_evento_id
                                    p_escenario_id
                                    p_cant_gr_tot
                                    p_um
                                    p_modo
                                    p_zona_id
                                    p_peso_in
                                    p_peso_fi
                                    p_peso_ne
                                    p_tarifa_id
                                    p_homolog.                          "NETSOL-CCCE07032017+

  DATA: lt_flujoreccab TYPE STANDARD TABLE OF znstbflujoreccab,
        lv_cant        TYPE znses_pedidorec-peson ,                     "NETSOL-CCH31072015+
        lv_gr_1        TYPE likp-xblnr  ,                               "NETSOL-CCH05082015+
        lv_gr_2        TYPE likp-xblnr  .                               "NETSOL-CCH05082015+

  REFRESH : p_ti_flujoreccab  , p_ti_flujorecdet  , lt_flujoreccab .
  CLEAR   : w_flujoreccab  , w_flujorecdet .
**      Datos de Log.
*  SELECT * INTO TABLE lt_flujoreccab FROM znstbflujoreccab
  SELECT SINGLE * INTO w_flujoreccab FROM znstbflujoreccab
    WHERE tknum_id = p_dti_id
      AND vstel     = p_vstel_dest
      AND tknum     = p_tknum  .
  IF sy-subrc EQ 0 .
    w_flujoreccab-luser = sy-uname  .
    w_flujoreccab-ldate = sy-datum  .
    w_flujoreccab-ltime = sy-uzeit  .
  ELSE.
    w_flujoreccab-fuser = sy-uname  .
    w_flujoreccab-fdate = sy-datum  .
    w_flujoreccab-ftime = sy-uzeit  .
  ENDIF.
**      Datos de cabecera .
  w_flujoreccab-mandt         = sy-mandt  .
  w_flujoreccab-tknum_id      = p_dti_id  .
  w_flujoreccab-vstel         = p_vstel_dest .
  w_flujoreccab-vstel_orig    = p_vstel_orig .
  w_flujoreccab-tknum         = p_tknum  .
  w_flujoreccab-transp_ruc    = p_transp_ruc  .
  w_flujoreccab-chofer_dni    = p_chofer_dni  .
  w_flujoreccab-tracto_pl     = p_tracto  .
  w_flujoreccab-carret_pl     = p_carreta .
  w_flujoreccab-matnr         = p_material  .
  w_flujoreccab-ruta          = p_ruta  .
  w_flujoreccab-ticket        = p_ticket  .
  w_flujoreccab-cola_id       = p_cola_id .
  w_flujoreccab-turno_id         = p_turno_id  .
  w_flujoreccab-trfid_id      = p_trfid_id  .
  w_flujoreccab-tranq_id      = p_tranq_id  .
  w_flujoreccab-evento_id     = p_evento_id .
  w_flujoreccab-escenario_id  = p_escenario_id  .
  w_flujoreccab-tarifa_id     = p_tarifa_id  .

  w_flujoreccab-peso_gr       = p_cant_gr_tot .
  w_flujoreccab-meins         = p_um .

  w_flujoreccab-zona_id       = p_zona_id .
  w_flujoreccab-peso_in       = p_peso_in .
  w_flujoreccab-peso_fi       = p_peso_fi .
  w_flujoreccab-peso_ne       = p_peso_ne .
  w_flujoreccab-material_balanza = p_homolog .                          "NETSOL-CCCE07032017+

  APPEND w_flujoreccab TO  p_ti_flujoreccab   .

  IF p_flag_dt  EQ 'X' .

    LOOP AT p_ti_vttp INTO  w_vttp   .
      w_flujorecdet-vbeln_vl    = w_vttp-vbeln  .

      READ TABLE p_ti_ekbe  INTO w_ekbe WITH KEY  xblnr = w_vttp-vbeln  .
      IF sy-subrc EQ 0  .
        w_flujorecdet-ebeln       = w_ekbe-ebeln  .
      ENDIF.

      READ TABLE p_ti_likp  INTO w_likp WITH KEY  vbeln = w_vttp-vbeln  .
      IF sy-subrc EQ  0 .
        SPLIT w_likp-xblnr  AT '-'  INTO lv_gr_1 lv_gr_2  .             "NETSOL-CCH05082015+
        w_flujorecdet-gr  =   lv_gr_2+2  .                              "NETSOL-CCH05082015+
*        w_flujorecdet-gr  =   w_likp-xblnr  .                          "NETSOL-CCH05082015-
        READ TABLE p_ti_lips  INTO w_lips WITH KEY  vbeln = w_vttp-vbeln  .
        IF sy-subrc EQ 0  .
          w_flujorecdet-peso_gr = w_lips-lfimg  .
          w_flujorecdet-meins   = w_lips-meins  .
        ENDIF.
      ENDIF.

**      Datos de detalle .
      w_flujorecdet-mandt         = sy-mandt  .
      w_flujorecdet-tknum_id      = p_dti_id  .
      w_flujorecdet-evento_id     = p_evento_id .
      w_flujorecdet-vstel         = p_vstel_dest .
      w_flujorecdet-flag_manual   = p_modo  .

      SELECT SINGLE * INTO w_flujorecdet FROM znstbflujorecdet
        WHERE tknum_id  = p_dti_id
          AND vbeln_vl  = w_vttp-vbeln
          AND evento_id = p_evento_id
          AND vstel     = p_vstel_dest  .
      IF sy-subrc EQ 0 .
        w_flujorecdet-luser = sy-uname  .
        w_flujorecdet-ldate = sy-datum  .
        w_flujorecdet-ltime = sy-uzeit  .
      ELSE.
        w_flujorecdet-fuser = sy-uname  .
        w_flujorecdet-fdate = sy-datum  .
        w_flujorecdet-ftime = sy-uzeit  .
      ENDIF.

      APPEND w_flujorecdet  TO  p_ti_flujorecdet  .

    ENDLOOP.

  ELSE."Guardar sin DT.

    LOOP AT p_ti_ekko  INTO w_ekko .

      w_flujorecdet-ebeln       = w_ekko-ebeln  .

      READ TABLE p_t_pedido WITH KEY ebeln  = w_ekko-ebeln  .
      IF sy-subrc EQ 0  .
        w_flujorecdet-gr      = p_t_pedido-gremi  .
        CONDENSE p_t_pedido-peson NO-GAPS .                             "NETSOL-CCH31072015+
        lv_cant               = p_t_pedido-peson  .                     "NETSOL-CCH31072015+
        w_flujorecdet-peso_gr = lv_cant  .                              "NETSOL-CCH31072015+
*          w_flujorecdet-peso_gr = p_t_pedido-peson  .                    "NETSOL-CCH31072015-
        w_flujorecdet-meins   = p_t_pedido-meins  .
      ENDIF.

*  *      Datos de detalle .
      w_flujorecdet-mandt         = sy-mandt  .
      w_flujorecdet-tknum_id      = p_dti_id  .
      w_flujorecdet-evento_id     = p_evento_id .
      w_flujorecdet-vstel         = p_vstel_dest .
      w_flujorecdet-flag_manual   = p_modo  .

      SELECT SINGLE * INTO w_flujorecdet FROM znstbflujorecdet
        WHERE tknum_id  = p_dti_id
*              AND vbeln_vl  = w_vttp-vbeln
          AND evento_id = p_evento_id
          AND vstel     = p_vstel_dest  .
      IF sy-subrc EQ 0 .
        w_flujorecdet-luser = sy-uname  .
        w_flujorecdet-ldate = sy-datum  .
        w_flujorecdet-ltime = sy-uzeit  .
      ELSE.
        w_flujorecdet-fuser = sy-uname  .
        w_flujorecdet-fdate = sy-datum  .
        w_flujorecdet-ftime = sy-uzeit  .
      ENDIF.

      APPEND w_flujorecdet  TO  p_ti_flujorecdet  .

    ENDLOOP.

  ENDIF.

ENDFORM.                    " GUARDAR_FLUJO_RECEPCION

*&---------------------------------------------------------------------*
*&      Form  GUARDAR_FLUJO_RECEPCION_TRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_FLUJORECCAB  text
*      -->P_TI_FLUJORECDET  text
*      -->P_LV_DDL  text
*----------------------------------------------------------------------*
FORM guardar_flujo_recepcion_tra  TABLES   p_ti_flujoreccab STRUCTURE znstbflujoreccab
                                       p_ti_flujorecdet STRUCTURE znstbflujorecdet
                                       p_ti_vttp        STRUCTURE vttp
                                       p_ti_ekko        STRUCTURE ekko
                                       p_ti_ekbe        STRUCTURE ekbe
                                       p_ti_likp        STRUCTURE likp
                                       p_ti_lips        STRUCTURE lips
                                       p_t_pedido       STRUCTURE znses_pedidorec
                              USING p_flag_dt
                                    p_dti_id
                                    p_tknum
                                    p_vstel_dest
                                    p_vstel_orig
                                    p_transp_ruc
                                    p_chofer_dni
                                    p_tracto
                                    p_carreta
                                    p_material
                                    p_ruta
                                    p_ticket
                                    p_cola_id
                                    p_turno_id
                                    p_trfid_id
                                    p_tranq_id
                                    p_evento_id
                                    p_escenario_id
                                    p_cant_gr_tot
                                    p_um
                                    p_modo
                                    p_zona_id
                                    p_peso_in
                                    p_peso_fi
                                    p_peso_ne
                                    p_tarifa_id
                                    p_homolog                           "NETSOL-CCCE07032017+
                                    p_traslado TYPE zst_regtraslado.

  DATA: lt_flujoreccab TYPE STANDARD TABLE OF znstbflujoreccab,
        lv_cant        TYPE znses_pedidorec-peson ,                     "NETSOL-CCH31072015+
        lv_gr_1        TYPE likp-xblnr  ,                               "NETSOL-CCH05082015+
        lv_gr_2        TYPE likp-xblnr  .                               "NETSOL-CCH05082015+

  REFRESH : p_ti_flujoreccab  , p_ti_flujorecdet  , lt_flujoreccab .
  CLEAR   : w_flujoreccab  , w_flujorecdet .

*******************************************************************
* Cabecera                                                        *
*******************************************************************

* Datos de Log.
  SELECT SINGLE * INTO w_flujoreccab FROM znstbflujoreccab
    WHERE tknum_id = p_dti_id
      AND vstel     = p_vstel_dest
      AND tknum     = p_tknum  .
  IF sy-subrc EQ 0 .
    w_flujoreccab-luser = sy-uname  .
    w_flujoreccab-ldate = sy-datum  .
    w_flujoreccab-ltime = sy-uzeit  .
  ELSE.
    w_flujoreccab-fuser = sy-uname  .
    w_flujoreccab-fdate = sy-datum  .
    w_flujoreccab-ftime = sy-uzeit  .
  ENDIF.

* Datos de cabecera .
  w_flujoreccab-mandt         = sy-mandt  .
  w_flujoreccab-tknum_id      = p_dti_id  .
  w_flujoreccab-vstel         = p_vstel_dest .
  w_flujoreccab-vstel_orig    = p_vstel_orig .
  w_flujoreccab-tknum         = p_tknum  .
  w_flujoreccab-transp_ruc    = p_transp_ruc  .
  w_flujoreccab-chofer_dni    = p_chofer_dni  .
  w_flujoreccab-tracto_pl     = p_tracto  .
  w_flujoreccab-carret_pl     = p_carreta .
  w_flujoreccab-matnr         = p_material  .
  w_flujoreccab-ruta          = p_ruta  .
  w_flujoreccab-ticket        = p_ticket  .
  w_flujoreccab-cola_id       = p_cola_id .
  w_flujoreccab-turno_id      = p_turno_id  .
  w_flujoreccab-trfid_id      = p_trfid_id  .
  w_flujoreccab-tranq_id      = p_tranq_id  .
  w_flujoreccab-evento_id     = p_evento_id .
  w_flujoreccab-escenario_id  = p_escenario_id  .
  w_flujoreccab-tarifa_id     = p_tarifa_id  .
  w_flujoreccab-material_balanza = p_homolog  .                         "NETSOL-CCCE07032017+

* Pesos
  w_flujoreccab-peso_gr       = p_cant_gr_tot .
  w_flujoreccab-meins         = p_um .
  w_flujoreccab-zona_id       = p_zona_id .
  w_flujoreccab-peso_in       = p_peso_in .
  w_flujoreccab-peso_fi       = p_peso_fi .
  w_flujoreccab-peso_ne       = p_peso_ne .

* Campos adicionales Traslado en un paso
  w_flujoreccab-bukrs         = p_traslado-bukrs.
  w_flujoreccab-cende         = p_traslado-cende.
  w_flujoreccab-almde         = p_traslado-almde.
  w_flujoreccab-cenha         = p_traslado-cenha.
  w_flujoreccab-almha         = p_traslado-almha.
  w_flujoreccab-guiar         = p_traslado-guiar.
  w_flujoreccab-budat         = p_traslado-budat.

  APPEND w_flujoreccab TO  p_ti_flujoreccab.

*******************************************************************
* Detalle                                                         *
*******************************************************************

  IF p_flag_dt  EQ 'X' .

    LOOP AT p_ti_vttp INTO  w_vttp   .
      w_flujorecdet-vbeln_vl    = w_vttp-vbeln  .

      READ TABLE p_ti_ekbe  INTO w_ekbe WITH KEY  xblnr = w_vttp-vbeln  .
      IF sy-subrc EQ 0  .
        w_flujorecdet-ebeln       = w_ekbe-ebeln  .
      ENDIF.

      READ TABLE p_ti_likp  INTO w_likp WITH KEY  vbeln = w_vttp-vbeln  .
      IF sy-subrc EQ  0 .
        SPLIT w_likp-xblnr  AT '-'  INTO lv_gr_1 lv_gr_2  .             "NETSOL-CCH05082015+
        w_flujorecdet-gr  =   lv_gr_2+2  .                              "NETSOL-CCH05082015+
*        w_flujorecdet-gr  =   w_likp-xblnr  .                          "NETSOL-CCH05082015-
        READ TABLE p_ti_lips  INTO w_lips WITH KEY  vbeln = w_vttp-vbeln  .
        IF sy-subrc EQ 0  .
          w_flujorecdet-peso_gr = w_lips-lfimg  .
          w_flujorecdet-meins   = w_lips-meins  .
        ENDIF.
      ENDIF.

*     Datos de detalle .
      w_flujorecdet-mandt         = sy-mandt  .
      w_flujorecdet-tknum_id      = p_dti_id  .
      w_flujorecdet-evento_id     = p_evento_id .
      w_flujorecdet-vstel         = p_vstel_dest .
      w_flujorecdet-flag_manual   = p_modo  .

      SELECT SINGLE * INTO w_flujorecdet FROM znstbflujorecdet
        WHERE tknum_id  = p_dti_id
          AND vbeln_vl  = w_vttp-vbeln
          AND evento_id = p_evento_id
          AND vstel     = p_vstel_dest  .
      IF sy-subrc EQ 0 .
        w_flujorecdet-luser = sy-uname  .
        w_flujorecdet-ldate = sy-datum  .
        w_flujorecdet-ltime = sy-uzeit  .
      ELSE.
        w_flujorecdet-fuser = sy-uname  .
        w_flujorecdet-fdate = sy-datum  .
        w_flujorecdet-ftime = sy-uzeit  .
      ENDIF.

      APPEND w_flujorecdet  TO  p_ti_flujorecdet  .

    ENDLOOP.

  ELSE."Guardar sin DT.

    w_flujorecdet-mandt       = sy-mandt.
    w_flujorecdet-ebeln       = ''.
    w_flujorecdet-gr          = p_traslado-guiar.
    w_flujorecdet-peso_gr     = p_cant_gr_tot.
    w_flujorecdet-meins       = p_um.
    w_flujorecdet-tknum_id    = p_dti_id.
    w_flujorecdet-evento_id   = p_evento_id.
    w_flujorecdet-vstel       = p_vstel_dest.
    w_flujorecdet-flag_manual = p_modo.
    SELECT SINGLE * INTO w_flujorecdet FROM znstbflujorecdet
      WHERE tknum_id  = p_dti_id
        AND evento_id = p_evento_id
        AND vstel     = p_vstel_dest.
    IF sy-subrc EQ 0 .
      w_flujorecdet-luser = sy-uname.
      w_flujorecdet-ldate = sy-datum.
      w_flujorecdet-ltime = sy-uzeit.
    ELSE.
      w_flujorecdet-fuser = sy-uname.
      w_flujorecdet-fdate = sy-datum.
      w_flujorecdet-ftime = sy-uzeit.
    ENDIF.
    APPEND w_flujorecdet  TO  p_ti_flujorecdet  .

  ENDIF.

ENDFORM.                    " GUARDAR_FLUJO_RECEPCION_TRA

*&---------------------------------------------------------------------*
*&      Form  GUARDAR_DATOS_SALIDA
*&---------------------------------------------------------------------*
FORM guardar_datos_salida  TABLES   p_t_data_cab STRUCTURE ztab256
                                    p_t_data_det STRUCTURE ztab256
                                    p_ti_ekko STRUCTURE ekko
                                    p_ti_ekpo STRUCTURE ekpo
                                    p_ti_ekbe STRUCTURE ekbe
                                    p_ti_likp STRUCTURE likp
                                    p_ti_lips STRUCTURE lips
                                    p_t_pedido STRUCTURE znses_pedidorec
                           USING    p_flag_dt
                                    p_fecha_act
                                    p_hora_act
                                    p_tipo_p
                                    p_provee_id
                                    p_provee_des
                                    p_turno_id
                                    p_dti_id
                                    p_tknum
                                    p_material
                                    p_um
                                    p_transp_ruc
                                    p_transp_des
                                    p_chofer_dni
                                    p_chofer_des
                                    p_chofer_bvt
                                    p_tracto
                                    p_carreta
                                    p_ticket
                                    p_trfid_id
                                    p_trfid_nro
                                    p_color
                                    p_destin_id
                                    p_destin_des
                                    p_ruta                              "NETSOL-CCCE13032017+
                                    p_zona_id                           "NETSOL-CCCE15032017+
                                    p_vstel_dest                        "NETSOL-CCCE15032017+
                                    p_separador.

  DATA: lv_fecha_txt    TYPE string,
        lv_hora_txt     TYPE string,
        lv_concat       TYPE string,
        lv_ebeln        TYPE ekko-ebeln,
        lv_vbeln_vl     TYPE lips-vbeln,
        lv_material_des TYPE makt-maktx,
        lv_canti_txt    TYPE char20,
        lv_item         TYPE ekpo-ebelp,
        lv_gremi        TYPE likp-xblnr,
        lv_um           TYPE lips-meins,
        lv_cant_total   TYPE lips-lfimg,                                "NETSOL-CCH31072015+
        lv_cant         TYPE lips-lfimg,                                "NETSOL-CCH31072015+
        lv_peso_aux     TYPE lips-lfimg ,                               "NETSOL-CCH04092015+
        lv_material     TYPE mara-matnr,
        lw_zona_des     TYPE znstb_zona_des ,                           "NETSOL-CCH15032017+
        lv_gr_1         TYPE likp-xblnr  ,                              "NETSOL-CCH05082015+
        lv_gr_2         TYPE likp-xblnr  .                              "NETSOL-CCH05082015+

* formato fecha.
  CONCATENATE p_fecha_act+4(2) '/' p_fecha_act+6(2) '/' p_fecha_act+0(4)
    INTO lv_fecha_txt .

  CONCATENATE p_hora_act+0(2) ':' p_hora_act+2(2) ':'  p_hora_act+4(2)
    INTO lv_hora_txt  .

* unidad de medida.
  lv_um = p_um  .

* descripción de material.
  SELECT SINGLE maktx INTO lv_material_des FROM makt
  WHERE matnr = p_material
    AND spras = sy-langu  .

* formato material.
  PERFORM conversion_exit_matn1_output USING p_material
                                       CHANGING p_material .
*BB NETSOL-CCCE15032017+
*  zona de descarga - descripción
  SELECT SINGLE * INTO lw_zona_des FROM znstb_zona_des
    WHERE zona_id  = p_zona_id
     AND  vstel    = p_vstel_dest .
*BE NETSOL-CCCE15032017+
  CONCATENATE p_tipo_p
              p_provee_id
              p_provee_des
              lv_fecha_txt
              lv_hora_txt
              p_turno_id
              p_dti_id
              p_tknum
              p_transp_ruc
              p_transp_des
              p_chofer_dni
              p_chofer_des
              p_chofer_bvt
              p_tracto
              p_carreta
              p_ticket
              p_trfid_id
              p_trfid_nro
              p_color
              p_ruta                              "NETSOL-CCCE13032017+
              lw_zona_des-zona_id                 "NETSOL-CCCE15032017+
              lw_zona_des-descripcion             "NETSOL-CCCE15032017+
  INTO l_concat SEPARATED BY p_separador  .
  APPEND l_concat TO p_t_data_cab .
*----------------------------------------------------------------------*
* Estructa de datos de posición
*----------------------------------------------------------------------*
  CLEAR: lv_cant_total .                                                "NETSOL-CCH31072015+
  LOOP AT p_ti_ekko INTO  w_ekko  .

    lv_ebeln       = w_ekko-ebeln  .

    IF p_flag_dt EQ 'X' .

      LOOP AT p_ti_ekpo INTO w_ekpo WHERE ebeln EQ w_ekko-ebeln .

        READ TABLE p_ti_ekbe INTO w_ekbe  WITH KEY  ebeln = w_ekpo-ebeln
                                                  ebelp = w_ekpo-ebelp  .
        IF sy-subrc EQ 0  .
          READ TABLE p_ti_likp  INTO w_likp WITH KEY  vbeln = w_ekbe-xblnr  .
          READ TABLE p_ti_lips  INTO w_lips WITH KEY  vbeln = w_ekbe-xblnr  .
          IF sy-subrc EQ 0  .
*          lv_ebeln       = w_ekpo-ebeln  .
            lv_vbeln_vl    = w_lips-vbeln  .
            SPLIT w_likp-xblnr  AT '-'  INTO lv_gr_1 lv_gr_2  .             "NETSOL-CCH05082015+
            lv_gremi  =   lv_gr_2  .                                        "NETSOL-CCH05082015+
*            lv_gremi       = w_likp-xblnr .                                "NETSOL-CCH05082015-
            PERFORM cambiar_medida USING 'KG' w_lips-vrkme lv_peso_aux w_lips-lfimg."NETSOL-CCH04092015+
            lv_canti_txt  = lv_peso_aux  .                                  "NETSOL-CCH04092015+
*            lv_canti_txt   = w_lips-lfimg  .                               "NETSOL-CCH04092015-
            lv_item  = w_ekpo-ebelp  .
*            lv_cant_total = lv_cant_total + w_lips-lfimg  .             "NETSOL-CCH31072015+  "NETSOL-CCH04092015-
            lv_cant_total = lv_cant_total + lv_peso_aux  .              "NETSOL-CCH04092015+
            lv_um = 'KG'  .                                             "NETSOL-CCH04092015+
*           quitando espacios en blanco.
            CONDENSE lv_canti_txt NO-GAPS .

            CONCATENATE lv_ebeln
                        lv_vbeln_vl
                        lv_gremi
                        p_material
                        lv_material_des
                        lv_canti_txt
                        lv_um
                        lv_item
                        p_destin_id
                        p_destin_des
            INTO l_concat
            SEPARATED BY p_separador  .

            APPEND l_concat TO p_t_data_det .

          ENDIF.
        ENDIF.

      ENDLOOP.

    ELSE.

      READ TABLE p_t_pedido WITH KEY ebeln  = w_ekko-ebeln  .
      IF sy-subrc EQ 0  .
        lv_gremi      = p_t_pedido-gremi  .
        lv_canti_txt  = p_t_pedido-peson  .
        lv_um         = p_t_pedido-meins  .
      ENDIF.
      CONDENSE p_t_pedido-peson  NO-GAPS .
      lv_cant       = p_t_pedido-peson  .
*     quitando espacios en blanco.
      CONDENSE lv_canti_txt NO-GAPS .                                 "NETSOL-CCH31072015+
      lv_cant_total = lv_cant_total + lv_cant  .                      "NETSOL-CCH31072015+

      READ TABLE p_ti_ekpo INDEX 1 .
      lv_item = p_ti_ekpo-ebelp .

      SELECT SINGLE maktx INTO lv_material_des FROM makt
       WHERE matnr = p_material
         AND spras = sy-langu  .

      lv_item  = w_ekpo-ebelp  .

      CONCATENATE lv_ebeln
                  lv_vbeln_vl
                  lv_gremi
                  p_material
                  lv_material_des
                  lv_canti_txt
                  lv_um
                  lv_item
                  p_destin_id
                  p_destin_des
      INTO l_concat
      SEPARATED BY v_separador  .

      APPEND l_concat TO p_t_data_det .


    ENDIF.
  ENDLOOP.

* total                                                                 "Begin NETSOL-CCH31072015+
  lv_canti_txt = lv_cant_total  .
  CONDENSE lv_canti_txt NO-GAPS .
  CONCATENATE 'T' p_material lv_material_des lv_canti_txt lv_um
  INTO l_concat SEPARATED BY v_separador  .

  APPEND l_concat TO p_t_data_det .
* total                                                                 "End NETSOL-CCH31072015+

ENDFORM.                    " GUARDAR_DATOS_SALIDA

*&---------------------------------------------------------------------*
*&      Form  GUARDAR_DATOS_SALIDA_TRA
*&---------------------------------------------------------------------*
FORM guardar_datos_salida_tra  TABLES   p_t_data_cab STRUCTURE ztab256
                                        p_t_data_det STRUCTURE ztab256
                                        p_ti_ekko    STRUCTURE ekko
                                        p_ti_ekpo    STRUCTURE ekpo
                                        p_ti_ekbe    STRUCTURE ekbe
                                        p_ti_likp    STRUCTURE likp
                                        p_ti_lips    STRUCTURE lips
                                        p_t_pedido   STRUCTURE znses_pedidorec
                                USING   p_flag_dt
                                        p_fecha_act
                                        p_hora_act
                                        p_tipo_p
                                        p_provee_id
                                        p_provee_des
                                        p_turno_id
                                        p_dti_id
                                        p_tknum
                                        p_material
                                        p_um
                                        p_transp_ruc
                                        p_transp_des
                                        p_chofer_dni
                                        p_chofer_des
                                        p_chofer_bvt
                                        p_tracto
                                        p_carreta
                                        p_ticket
                                        p_trfid_id
                                        p_trfid_nro
                                        p_color
                                        p_destin_id
                                        p_destin_des
                                        p_ruta                          "NETSOL-CCCE13032017+
                                        p_zona_id                       "NETSOL-CCCE15032017+
                                        p_vstel_dest                    "NETSOL-CCCE15032017+
                                        p_separador.

  DATA: lv_fecha_txt    TYPE string,
        lv_hora_txt     TYPE string,
        lv_concat       TYPE string,
        lv_ebeln        TYPE ekko-ebeln,
        lv_vbeln_vl     TYPE lips-vbeln,
        lv_material_des TYPE makt-maktx,
        lv_canti_txt    TYPE char20,
        lv_item         TYPE ekpo-ebelp,
        lv_gremi        TYPE likp-xblnr,
        lv_um           TYPE lips-meins,
        lv_cant_total   TYPE lips-lfimg,                                "NETSOL-CCH31072015+
        lv_cant         TYPE lips-lfimg,                                "NETSOL-CCH31072015+
        lv_peso_aux     TYPE lips-lfimg ,                               "NETSOL-CCH04092015+
        lv_material     TYPE mara-matnr,
        lw_zona_des     TYPE znstb_zona_des ,                           "NETSOL-CCH15032017+
        lv_gr_1         TYPE likp-xblnr  ,                              "NETSOL-CCH05082015+
        lv_gr_2         TYPE likp-xblnr  .                              "NETSOL-CCH05082015+

*----------------------------------------------------------------------*
* Estructa de datos de cabecera
*----------------------------------------------------------------------*

* formato fecha.
  CONCATENATE p_fecha_act+4(2) '/' p_fecha_act+6(2) '/' p_fecha_act+0(4)
    INTO lv_fecha_txt .

  CONCATENATE p_hora_act+0(2) ':' p_hora_act+2(2) ':'  p_hora_act+4(2)
    INTO lv_hora_txt  .

* unidad de medida.
  lv_um = p_um  .

* descripción de material.
  SELECT SINGLE maktx INTO lv_material_des FROM makt
  WHERE matnr = p_material
    AND spras = sy-langu  .

* formato material.
  PERFORM conversion_exit_matn1_output USING p_material
                                       CHANGING p_material .
*BB NETSOL-CCCE15032017+
*  zona de descarga - descripción
  SELECT SINGLE * INTO lw_zona_des FROM znstb_zona_des
    WHERE zona_id  = p_zona_id
     AND  vstel    = p_vstel_dest .
*BE NETSOL-CCCE15032017+
  CONCATENATE p_tipo_p
              p_provee_id
              p_provee_des
              lv_fecha_txt
              lv_hora_txt
              p_turno_id
              p_dti_id
              p_tknum
              p_transp_ruc
              p_transp_des
              p_chofer_dni
              p_chofer_des
              p_chofer_bvt
              p_tracto
              p_carreta
              p_ticket
              p_trfid_id
              p_trfid_nro
              p_color
              p_ruta                                                    "NETSOL-CCCE13032017+
              lw_zona_des-zona_id                                       "NETSOL-CCCE15032017+
              lw_zona_des-descripcion                                   "NETSOL-CCCE15032017+
  INTO l_concat SEPARATED BY p_separador  .
  APPEND l_concat TO p_t_data_cab .

*----------------------------------------------------------------------*
* Estructa de datos de posición
*----------------------------------------------------------------------*

* Pendiente (al parecer no es necesario)

ENDFORM.                    " GUARDAR_DATOS_SALIDA_TRA

*&---------------------------------------------------------------------*
*&      Form  OBTENER_CONSTANTES
*&---------------------------------------------------------------------*
FORM obtener_constantes .

  SELECT * INTO TABLE ti_const  FROM zgetdconst
    WHERE modulo  = c_modulo
      AND proyec  = c_proyec  .

  LOOP AT ti_const INTO w_const .

    CASE w_const-aplica .
      WHEN c_constantes .

        CASE w_const-campo  .
          WHEN c_charsep.
            v_separador = w_const-valor1.

          WHEN c_charsep2 .
            l_panel_charsep = w_const-valor1 .

          WHEN c_frecuenc .
            IF w_const-correl = 1.
              l_t_f1 = w_const-valor1 .

            ELSEIF w_const-correl = 2 .
              l_t_f2 = w_const-valor1 .
            ENDIF.

          WHEN c_ev_pinic .
            v_evento_id_0300  = w_const-valor1 .

          WHEN c_ev_rcola  .
            v_evento_id_0100  = w_const-valor1 .

          WHEN c_trfid_flag .
            v_trfid_flag  = w_const-valor1  .
*                                                                       "Begin NETSOL-CCH07082015+
          WHEN c_meins .
            v_meins   = w_const-valor1  .
*                                                                       "End NETSOL-CCH07082015+
"EMAT - Enrique Aguirre - #95219 17.07.2020 - I
          WHEN c_ev_bsart.
            CLEAR wa_rng_bsart.
            wa_rng_bsart-sign = 'I'.
            wa_rng_bsart-option = 'EQ'.
            wa_rng_bsart-low = w_const-valor1.
            APPEND wa_rng_bsart TO gv_rng_bsart.

          WHEN c_ev_bsart_x.
            CLEAR wa_rng_bsart.
            wa_rng_bsart-sign = 'I'.
            wa_rng_bsart-option = 'EQ'.
            wa_rng_bsart-low = w_const-valor1.
            APPEND wa_rng_bsart TO gv_rng_bsart_x.

"EMAT - Enrique Aguirre - #95219 17.07.2020 - I
          WHEN OTHERS.
        ENDCASE.

      WHEN c_dispositivos .

        CASE w_const-campo  .
          WHEN c_file .
            l_filetxt  = w_const-valor1  .

          WHEN c_user .
            l_user  = w_const-valor1  .

          WHEN  c_pwd .
            l_pwd  = w_const-valor1  .

          WHEN  c_host .
            l_host  = w_const-valor1  .

          WHEN  c_path .
            l_path  = w_const-valor1  .

          WHEN OTHERS.

        ENDCASE.


      WHEN OTHERS.

    ENDCASE.

  ENDLOOP.

ENDFORM.                    " OBTENER_CONSTANTES
*&---------------------------------------------------------------------*
*&      Form  FTP_CONNECT_SD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_DESTINO  text
*----------------------------------------------------------------------*
FORM ftp_connect_sd  USING  l_file  .

  DATA: l_line     TYPE i,
        l_dest     LIKE rfcdes-rfcdest VALUE 'SAPFTPA',
        l_hdl      TYPE i,
        l_cmd      TYPE char256,
        l_nro      TYPE char5,
        l_msg1     TYPE char128,
        l_msg2     TYPE char128,
        l_msg3     TYPE char128,
        lt_result  TYPE STANDARD TABLE OF ztab128 WITH HEADER LINE,
*        l_user(30) TYPE c,
*        l_pwd(30)  TYPE c,
*        l_host(30) TYPE c,
*        l_path(50) TYPE c,
*        l_filetxt(30) TYPE c,
        l_cmd1(50) TYPE c.

  DATA:  l_len  TYPE i.
  DATA: BEGIN OF it_result OCCURS 0,
          line(100) TYPE c,
        END OF it_result.

  CONSTANTS: c_modulo(10) TYPE c VALUE 'SD',
             c_proyec(15) TYPE c VALUE 'DESPACHOS',
             c_aplica(15) TYPE c VALUE 'DISPOSITIVOS',
             c_user(15)   TYPE c VALUE 'USER',
             c_pwd(15)    TYPE c VALUE 'PWD',
             c_host(10)   TYPE c VALUE 'SERVER',
             c_path(50)   TYPE c VALUE 'PATH',
             c_compress   TYPE c VALUE  'N',
             c_file(30)   TYPE c VALUE 'FILE'.

  DATA: l_msj(100)  TYPE c VALUE '250 CWD COMMAND SUCCESSFUL.  #',
        l_msj2(100) TYPE c.

* Datos para manejar el FTP
  DATA: l_err(50) TYPE c.
  DATA: l_key  TYPE i VALUE 26101957,
        l_slen TYPE i.

* Obtengo constantes de la tabla ZGETDCONST
* Obtengo usuario
*  SELECT SINGLE valor1 INTO l_user
*   FROM zgetdconst
*   WHERE modulo  EQ c_modulo
*     AND proyec  EQ c_proyec
*     AND aplica  EQ c_aplica
*     AND campo   EQ c_user  .
*     AND correl  EQ c_2.
*  IF sy-subrc NE 0.
  IF l_user IS INITIAL .
    RAISE error_usuario.
  ENDIF.

* Obtengo password
*  SELECT SINGLE valor1 INTO l_pwd
*   FROM zgetdconst
*   WHERE modulo  EQ c_modulo
*     AND proyec  EQ c_proyec
*     AND aplica  EQ c_aplica
*     AND campo   EQ c_pwd
*     AND correl  EQ c_2.
*  IF sy-subrc NE 0.
  IF l_pwd IS INITIAL .
    RAISE error_pws.
  ENDIF.

* Obtengo servidor
*  SELECT SINGLE valor1 INTO l_host
*   FROM zgetdconst
*   WHERE modulo  EQ c_modulo
*     AND proyec  EQ c_proyec
*     AND aplica  EQ c_aplica
*     AND campo   EQ c_host
*     AND correl  EQ c_2.
*  IF sy-subrc NE 0.
  IF l_host IS INITIAL .
    RAISE error_server.
  ENDIF .

* Obtengo ruta
*  SELECT SINGLE valor1 INTO l_path
*   FROM zgetdconst
*   WHERE modulo  EQ c_modulo  AND
*         proyec  EQ c_proyec AND
*         aplica  EQ c_aplica  AND
*         campo   EQ c_path.
*  IF sy-subrc NE 0.
  IF l_path IS INITIAL  .
    RAISE error_path.
  ENDIF.

* Obtengo archivo
*  SELECT SINGLE valor1 INTO l_filetxt
*   FROM zgetdconst
*   WHERE modulo  EQ c_modulo
*     AND proyec  EQ c_proyec
*     AND aplica  EQ c_aplica
*     AND campo   EQ c_file
*     AND correl  EQ c_2.
*  IF sy-subrc NE 0.
  IF l_filetxt IS INITIAL .
    RAISE error_file.
  ENDIF.

  l_len = strlen( l_pwd ).

  CALL FUNCTION 'HTTP_SCRAMBLE'
    EXPORTING
      source      = l_pwd
      sourcelen   = l_len
      key         = l_key
    IMPORTING
      destination = l_pwd.

* open ftp
  CALL FUNCTION 'FTP_CONNECT'
    EXPORTING
      user            = l_user
      password        = l_pwd
      host            = l_host
      rfc_destination = l_dest
    IMPORTING
      handle          = l_hdl
    EXCEPTIONS
      not_connected   = 1
      OTHERS          = 2.

  IF sy-subrc = 0.
* bi netsol-cch26082015
* Para planta piura no se encesita ejecutar el servcio de windows SERVICEPANEL
**   se posiciona en el dir
*   se posiciona en el dir
*    CONCATENATE 'cd' l_path INTO l_cmd SEPARATED BY space.
*
*    CALL FUNCTION 'FTP_COMMAND'
*      EXPORTING
*        handle        = l_hdl
*        command       = l_cmd
*        compress      = c_compress
*      TABLES
*        data          = lt_result
*      EXCEPTIONS
*        command_error = 1
*        tcpip_error   = 2.
* ei netsol-cch26082015
    IF sy-subrc = 0.

      l_line = lines( lt_result ).
      IF l_line > 0.
        READ TABLE lt_result INDEX l_line.
      ENDIF.

      l_cmd = 'ascii'.
      CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
          handle        = l_hdl
          command       = l_cmd
          compress      = c_compress
        TABLES
          data          = lt_result
        EXCEPTIONS
          command_error = 1
          tcpip_error   = 2.

*     guarda el archivo
      REFRESH lt_result.
      CLEAR l_cmd1.
      CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_HOME'
                         ID 'VALUE' FIELD l_cmd1.
      IF l_cmd1 IS INITIAL.
        l_cmd1 = '/usr/sap/CPC/DVEBMGS02/work'.
      ENDIF.
      CONCATENATE 'lcd' l_cmd1 INTO l_cmd SEPARATED BY space.
      CLEAR l_cmd1.
*      l_cmd = 'lcd /usr/sap/CPD/DVEBMGS00/work'.
      CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
          handle        = l_hdl
          command       = l_cmd
          compress      = c_compress
        TABLES
          data          = lt_result
        EXCEPTIONS
          command_error = 1
          tcpip_error   = 2.

      REFRESH lt_result.

*     guarda el archivo l_cmd
      CONCATENATE 'put' l_filetxt INTO l_cmd SEPARATED BY space.
      REFRESH lt_result.
      CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
          handle        = l_hdl
          command       = l_cmd
          compress      = c_compress
        TABLES
          data          = lt_result
        EXCEPTIONS
          command_error = 1
          tcpip_error   = 2.
    ENDIF.

    CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
        handle = l_hdl.

    CALL FUNCTION 'RFC_CONNECTION_CLOSE'
      EXPORTING
        destination = l_dest
      EXCEPTIONS
        OTHERS      = 1.
  ELSE.
    WRITE: / text-m05.
  ENDIF.

ENDFORM.                    " FTP_CONNECT_SD

*&---------------------------------------------------------------------*
*&      Form  REORGANIZE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      <--P_L_FLAG_CHANGE  text
*----------------------------------------------------------------------*
FORM reorganize_table  USING p_fila TYPE n
                       CHANGING p_flag TYPE c.

  DATA: it_panel      TYPE STANDARD TABLE OF znstb_datospanel WITH HEADER LINE,
        it_panel_temp TYPE STANDARD TABLE OF znstb_datospanel WITH HEADER LINE,
        l_correl(2)   TYPE n.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_panel
    FROM znstb_datospanel
    WHERE fila = p_fila.

*  IF LINES( it_panel ) > 0.
  IF NOT it_panel IS INITIAL.
*   BEGIN PCVPDK915128
*   DELETE ZSDTDATOSPANEL FROM TABLE IT_PANEL.
    LOOP AT it_panel.
      DELETE FROM znstb_datospanel WHERE fila = it_panel-fila     AND
                                       correl = it_panel-correl AND
                                       tknum_id = it_panel-tknum_id.
    ENDLOOP.
*  END PCVPDK915128
    SORT it_panel BY fila tknum_id valor.                               "NETSOL-CCCE19052016+
    DELETE ADJACENT DUPLICATES FROM it_panel COMPARING fila tknum_id valor.
    INSERT znstb_datospanel FROM TABLE it_panel.

    LOOP AT it_panel.
      IF sy-tabix EQ 1.
        MOVE-CORRESPONDING it_panel TO it_panel_temp.
        it_panel_temp-correl = '01'.
        APPEND it_panel_temp.
        CONTINUE.
      ENDIF.
      l_correl = it_panel-correl - it_panel_temp-correl.
      IF l_correl GE 1 OR sy-tabix EQ lines( it_panel ).
        p_flag = 'X'.
        l_correl = it_panel_temp-correl + 1.
        MOVE-CORRESPONDING it_panel TO it_panel_temp.
        it_panel_temp-correl = l_correl.
        APPEND it_panel_temp.
      ENDIF.
    ENDLOOP.

    DELETE znstb_datospanel FROM TABLE it_panel.
    INSERT znstb_datospanel FROM TABLE it_panel_temp.

  ENDIF.

ENDFORM.                    " REORGANIZE_TABLE
*&---------------------------------------------------------------------*
*&      Form  OBTENER_DATOS_FLUJO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_FLUJORECCAB  text
*      -->P_TI_FLUJORECDET  text
*      -->P_I_TKNUM_ID  text
*      <--P_L_ERROR  text
*      <--P_L_MSG  text
*----------------------------------------------------------------------*
FORM obtener_datos_flujo  TABLES   p_t_flujoreccab STRUCTURE znstbflujoreccab
                                   p_t_flujorecdet STRUCTURE znstbflujorecdet
*                          USING    p_tknum_id                          "NETSOL-CCH03092015-
                          USING    p_tknum_id p_vstel                   "NETSOL-CCH03092015+
                          CHANGING p_error
                                   p_msg.

  DATA: lv_tknum_id TYPE znstbflujoreccab-tknum_id  .

  REFRESH: p_t_flujoreccab, p_t_flujorecdet .

  PERFORM conversion_exit_alpha_input  USING    p_tknum_id
                                       CHANGING lv_tknum_id .

  SELECT * INTO TABLE p_t_flujoreccab FROM znstbflujoreccab
    WHERE tknum_id  EQ lv_tknum_id
      AND vstel     EQ p_vstel  .                                       "NETSOL-CCH03092015+
  IF sy-subrc NE 0  .
*   Error
  ELSE.
    SELECT * INTO TABLE p_t_flujorecdet FROM znstbflujorecdet
      FOR ALL ENTRIES IN  p_t_flujoreccab
      WHERE tknum_id  EQ  p_t_flujoreccab-tknum_id
        AND vstel     EQ  p_vstel                                       "NETSOL-CCH03092015+
        AND evento_id EQ  p_t_flujoreccab-evento_id .                   "NETSOL-CCH10092015+
    IF sy-subrc NE 0  .
*      Error
    ENDIF.
  ENDIF.

ENDFORM.                    " OBTENER_DATOS_FLUJO
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_RUTA_TARIFA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_TARIFAS  text
*      -->P_L_BUKRS  text
*      -->P_L_RUTA  text
*      -->P_L_TIPO_T  text
*      -->P_L_TRANSP_RUC  text
*      <--P_L_ERROR  text
*      <--P_L_MSG  text
*----------------------------------------------------------------------*
FORM validar_ruta_tarifa  TABLES   p_ti_tarifas STRUCTURE ztmmtarifas
                          USING    p_bukrs
                                   p_ruta
*                                   p_tipo_t
                                   p_transp_ruc
                                   p_material
                          CHANGING p_tipo_t
                                   p_tarifa_id
                                   p_error
                                   p_msg.

  REFRESH: p_ti_tarifas .

  SELECT * INTO TABLE p_ti_tarifas  FROM ztmmtarifas
    WHERE sociedad        = p_bukrs
*      AND ruta            = p_ruta
      AND ruc             = p_transp_ruc
      AND material        = p_material
      AND ( valido_de LE sy-datum AND
            valido_hasta GE sy-datum  )
      AND ind_borrado NE 'X'  .
  IF sy-subrc NE 0  .
    p_msg = text-m57  .
    p_error = 'X' .                                                     "NETSOL-CCH07082015+
  ELSE.
    READ TABLE p_ti_tarifas WITH KEY ruta = p_ruta  .
    IF sy-subrc NE 0  .
      p_msg = text-m50  .
      p_error = 'X' .                                                   "NETSOL-CCH07082015+
    ELSE.
      p_tipo_t  =   p_ti_tarifas-tipo_transporte  .
      p_tarifa_id  =   p_ti_tarifas-idtarifa  .
    ENDIF.
  ENDIF.

ENDFORM.                    " VALIDAR_RUTA_TARIFA
*&---------------------------------------------------------------------*
*&      Form  OBTENER_SECUENCIA_EVENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_FLUJORECCAB  text
*      -->P_L_ESCENARIO_ID  text
*      -->P_L_VSTEL_DEST  text
*      <--P_L_EVENTO_ID  text
*      <--P_L_EVENTO_ANT  text
*      <--P_L_EVENTO_POS  text
*----------------------------------------------------------------------*
FORM obtener_secuencia_evento  TABLES   p_ti_flujoreccab STRUCTURE znstbflujoreccab
                               USING    p_escenario_id
                                        p_vstel_dest
                               CHANGING p_evento_id
                                        p_evento_ant
                                        p_evento_pos
                                        p_error
                                        p_msg.

  DATA: lt_secuencia TYPE STANDARD TABLE OF znstb_secuencia,
        lw_secuencia TYPE znstb_secuencia.
*   Obtiene evento actual de flujo.
  READ TABLE p_ti_flujoreccab INDEX 1 .
  IF sy-subrc EQ 0.
    IF p_ti_flujoreccab-escenario_id NE p_escenario_id.
      p_error = 'X' .
      p_msg = text-m52 .
    ELSE .

      SELECT * INTO TABLE lt_secuencia  FROM znstb_secuencia
        WHERE escenario_id    = p_escenario_id  .
      IF sy-subrc NE 0  .
        p_error = 'X' .
        p_msg = text-m51 .
      ELSE.
        READ TABLE lt_secuencia INTO lw_secuencia  WITH KEY evento_ant = p_ti_flujoreccab-evento_id  .
        IF sy-subrc NE 0.
*        p_msg = text-m53 .
          p_evento_id   = 9999  .                                       "NETSOL-CCH18082015+
          p_evento_ant  = 0600 .                                        "NETSOL-CCH18082015+
          p_evento_pos  = 9999  .                                       "NETSOL-CCH18082015+
        ELSE.
          p_evento_id   = lw_secuencia-evento_id  .
          p_evento_ant  = lw_secuencia-evento_ant .
          p_evento_pos  = lw_secuencia-evento_pos .
        ENDIF.
      ENDIF .
    ENDIF.
  ELSE. "si registro.
    SELECT * INTO TABLE lt_secuencia  FROM znstb_secuencia
        WHERE escenario_id    = p_escenario_id  .
    IF sy-subrc NE 0  .
      p_error = 'X' .
      p_msg = text-m51 .
    ELSE.
      READ TABLE lt_secuencia INTO lw_secuencia  WITH KEY evento_ant = space  .
      IF sy-subrc NE 0.
*        p_msg = text-m53 .
      ELSE.
        p_evento_id   = lw_secuencia-evento_id  .
        p_evento_ant  = lw_secuencia-evento_ant .
        p_evento_pos  = lw_secuencia-evento_pos .
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " OBTENER_SECUENCIA_EVENTO

*&---------------------------------------------------------------------*
*&      Form  traslado_paso_unico
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRASLADO text
*      -->P_MBLNR    text
*      -->P_MJAHR    text
*----------------------------------------------------------------------*
FORM traslado_paso_unico USING p_traslado TYPE zst_regtraslado
                      CHANGING p_mblnr
                               p_mjahr.

  DATA: ls_params TYPE ctu_params,
        li_return TYPE bapiret2_tab,
        lv_neto   TYPE ntgew_15,
        lv_peso   TYPE c LENGTH 15,
        lv_matnr  TYPE matnr,
        lv_fecha  TYPE c LENGTH 10.

  DATA: materialdocument TYPE bapi2017_gm_head_ret-mat_doc,
        matdocumentyear  TYPE bapi2017_gm_head_ret-doc_year.

  REFRESH: ti_bdcdata, ti_messtab.

* Conversiones iniciales
  lv_neto = p_traslado-peso_in - p_traslado-peso_fi.   "NETO = Peso Inicial - Peso Final
  lv_peso = lv_neto.
  CONDENSE lv_peso.
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    EXPORTING
      input  = p_traslado-matnr
    IMPORTING
      output = lv_matnr.
  CONCATENATE p_traslado-budat+6(2) p_traslado-budat+4(2) p_traslado-budat+0(4)
              INTO lv_fecha SEPARATED BY '.'.

* Pantalla de selección
  PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_ING_MATPRIMAS_AGREGADO' '1000' CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'BDC_OKCODE' '=ENTER'                    CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'P_TRASPT'   abap_true                   CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'P_BUKRS'    p_traslado-bukrs            CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'P_CEN_DE'   p_traslado-cende            CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'P_CEN_A'    p_traslado-cenha            CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'P_ALM_DE'   p_traslado-almde            CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'P_ALM_A'    p_traslado-almha            CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'P_PESNET'   lv_peso                     CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'P_PLACA'    p_traslado-tracto_pl        CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'P_GR'       p_traslado-guiar            CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'P_MAT'      lv_matnr                    CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'P_BTN1'     abap_false                  CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'P_BTN2'     abap_true                   CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.

* Ingresar la Fecha
  PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_ING_MATPRIMAS_AGREGADO' '1000' CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'BDC_OKCODE' '=ONLI'                     CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'P_FECHA'    lv_fecha                    CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.

* Segunda pantalla y grabo
  PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_ING_MATPRIMAS_AGREGADO' '0110'   CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'BDC_OKCODE'        '=SAVE'                CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'GWA_DATASAL-STCD1' p_traslado-transp_ruc  CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.
  PERFORM ti_bdcdata_field USING 'GWA_DATASAL-RUTA'  p_traslado-ruta        CHANGING w_bdcdata.
  APPEND w_bdcdata TO ti_bdcdata.

* Invocar transaccion
  ls_params-dismode   = 'N'.
  ls_params-updmode   = 'S'.
  ls_params-racommit  = 'X'   .
  CALL TRANSACTION 'ZMM33' USING ti_bdcdata
                    OPTIONS FROM ls_params
                   MESSAGES INTO ti_messtab.
  COMMIT WORK AND WAIT.

* Identificar mensaje
  READ TABLE ti_messtab INTO w_messtab WITH KEY msgid = 'ZMM01'
                                                msgnr = '035'.
  IF sy-subrc = 0.
    IMPORT materialdocument FROM MEMORY ID 'ZTRASLADO_1'.
    IMPORT matdocumentyear  FROM MEMORY ID 'ZTRASLADO_2'.
    DELETE FROM MEMORY ID 'ZTRASLADO_1'.
    DELETE FROM MEMORY ID 'ZTRASLADO_2'.
    p_mblnr = materialdocument.
    p_mjahr = matdocumentyear.
  ELSE.
*    CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
*      TABLES
*        imt_bdcmsgcoll = ti_messtab[]
*        ext_return     = li_return.
  ENDIF.

ENDFORM.                    "traslado_paso_unico

*&---------------------------------------------------------------------*
*&      Form  INGRESO_MP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_FLUJORECCAB  text
*      -->P_TI_FLUJORECDET  text
*      <--P_L_ERROR  text
*      <--P_L_MSG  text
*----------------------------------------------------------------------*
FORM ingreso_mp  TABLES   p_ti_flujoreccab STRUCTURE znstbflujoreccab
                          p_ti_flujorecdet STRUCTURE znstbflujorecdet
                 USING    p_tipo_p
                          p_flag_transp                                 "NETSOL-CCH18082015+
                 CHANGING p_error
                          p_msg .

  DATA  : lv_ticket_mp   TYPE ztmmtickets-ticket ,         "Ticket balanza.
          lv_ebeln       TYPE ekko-ebeln ,                 "Pedido.
          lv_regman      TYPE c VALUE 'X'  ,               "ind. Registro manual.
          lv_feregu      TYPE c VALUE 'X'  ,               "ind. Fecha regulariz.
          lv_regcon      TYPE c VALUE 'X'  ,               "ind. Config.
          lv_btn1        TYPE c VALUE 'X'  ,               "ind.
          lv_btn2        TYPE c VALUE 'X'  ,               "ind.
          lv_peso_ne     TYPE ztmmtickets-peso_neto  ,     "Peso neto.
          lv_peso_ne_txt TYPE char20  ,     "Peso neto.
          lv_peso_gr     TYPE znstbflujoreccab-peso_gr,    "Peso GR     "NETSOL-CCCE10032017+
          lv_peso_gr_txt TYPE char20  ,                    "Peso GR.    "NETSOL-CCCE10032017+
          lv_tracto_pl   TYPE char6 ,                      "Placa tracto.
          lv_gr          TYPE char16 ,                     "G.Remisión.
          lv_ruta        TYPE ztmmtarifas-ruta ,           "Ruta.
          lv_material_b  TYPE zdedsmaterial  ,             "Material balanza.
          lv_werks       TYPE werks_d  ,                   "Centro.
          lv_transp_ruc  TYPE lfa1-stcd1 ,                 "R.U.C transp.
          lv_i           TYPE i  ,                         "Cantidad.
          lv_fecha       TYPE sydatum  ,                   "Fecha.
          lv_fecha_txt   TYPE char10  .                   "Fecha TXT.
*                                                                       "Begin NETSOL-CCH15092015+
  DATA:   lv_uaten_const_ini TYPE vttk-uaten,
          lv_uaten_const_fin TYPE vttk-uaten,
          lv_hora_txt1       TYPE char08,
          lv_hora_txt2       TYPE char08,
          lv_valor2          TYPE zgetdconst-valor2.
*                                                                       "End NETSOL-CCH15092015+
  DATA:   lw_option  TYPE ctu_params,
          lw_homolog TYPE ztmmhomolog,                                  "NETSOL-CCCE10032017+
          lw_messtab TYPE  bdcmsgcoll.

  CLEAR: lw_option .
  lw_option-dismode = 'N' .
  lw_option-updmode = 'S' .
  lw_option-racommit = 'X'   .

  LOOP AT p_ti_flujoreccab .
*BB NETSOL-CCCE26122016+
**   centro
*    SELECT SINGLE werks INTO lv_werks FROM tvswz
*      WHERE vstel =  p_ti_flujoreccab-vstel  .
*
**   material - homologación
*    SELECT SINGLE material_balanza INTO lv_material_b FROM ztmmhomolog
*      WHERE material_sap  = p_ti_flujoreccab-matnr
*        AND werks = lv_werks  .                                         "NETSOL-CCH18082015+
*BE NETSOL-CCCE26122016+
*   peso neto
    lv_peso_ne  = p_ti_flujoreccab-peso_ne  .
    lv_peso_ne_txt = lv_peso_ne .
    CONDENSE lv_peso_ne_txt NO-GAPS .
*BB NETSOL-CCCE10032017+
*   Peso GR
    lv_peso_gr      = p_ti_flujoreccab-peso_gr / 1000."conver TMS
    lv_peso_gr_txt  = lv_peso_gr .
    CONDENSE lv_peso_gr_txt NO-GAPS .
*BE NETSOL-CCCE10032017+
*   ticket.
*   lv_ticket   = p_ti_flujoreccab-ticket .

*   placa
    lv_tracto_pl = p_ti_flujoreccab-tracto_pl  .

*   ruc transportista.
    lv_transp_ruc = p_ti_flujoreccab-transp_ruc  .

*   ruta.
    lv_ruta = p_ti_flujoreccab-ruta  .

    lv_material_b = p_ti_flujoreccab-material_balanza.                  "NETSOL-CCCE07032017+
*   fecha
*                                                                       "Begin NETSOL-CCH15092015+
    "turno de madrugada
    CLEAR : lv_valor2 .
    SELECT SINGLE valor2 INTO lv_valor2 FROM zgetdconst
      WHERE modulo  EQ  'MM'
        AND proyec  EQ  'RECEPCION'
        AND aplica  EQ  'CONSTANTES'
        AND campo   EQ  'TURNO_MA'
        AND valor1  EQ  p_ti_flujoreccab-vstel  .

    SPLIT lv_valor2 AT v_separador  INTO lv_hora_txt1 lv_hora_txt2  .

    CONCATENATE lv_hora_txt1+0(2) lv_hora_txt1+3(2) lv_hora_txt1+6(2)
      INTO lv_uaten_const_ini .

    CONCATENATE lv_hora_txt2+0(2) lv_hora_txt2+3(2) lv_hora_txt2+6(2)
      INTO lv_uaten_const_fin .

    IF p_ti_flujoreccab-tknum IS NOT INITIAL .
      CLEAR:  w_vttk , lv_fecha .
      SELECT SINGLE * INTO w_vttk FROM vttk
        WHERE tknum EQ  p_ti_flujoreccab-tknum  .
      IF sy-subrc EQ 0  .
        IF lv_uaten_const_ini LE w_vttk-uaten AND w_vttk-uaten LE lv_uaten_const_fin .
          lv_fecha  = w_vttk-daten -  1 .
        ELSE.
          lv_fecha  = w_vttk-daten .
        ENDIF.
      ENDIF.
    ELSE. "Sin DT .
      IF lv_uaten_const_ini LE sy-uzeit AND sy-uzeit LE lv_uaten_const_fin .
        lv_fecha  = sy-datum -  1 .
      ELSE.
        lv_fecha  = sy-datum .
      ENDIF.
    ENDIF.
*                                                                       "End NETSOL-CCH15092015+
*    lv_fecha  = sy-datum  .                                            "NETSOL-CCH15092015-
    CONCATENATE lv_fecha+6(2) '.' lv_fecha+4(2) '.' lv_fecha+0(4) INTO lv_fecha_txt .

    DESCRIBE TABLE p_ti_flujorecdet LINES lv_i  .
    IF lv_i LT 2 .
      READ TABLE p_ti_flujorecdet INDEX 1 .

*     Pedido
      lv_ebeln  = p_ti_flujorecdet-ebeln  .
*BB NETSOL-CCCE26122016+
*     centro
      IF lv_ebeln IS NOT INITIAL.
        SELECT SINGLE werks INTO lv_werks FROM ekpo
          WHERE ebeln = lv_ebeln
            AND matnr = p_ti_flujoreccab-matnr  .
      ELSE.
        lv_werks = p_ti_flujoreccab-cenha.
      ENDIF.
*BB NETSOL-CCCE07032017-
*     material - homologación
*      SELECT SINGLE material_balanza INTO lv_material_b FROM ztmmhomolog
*        WHERE material_sap  = p_ti_flujoreccab-matnr
*          AND werks = lv_werks  .
*BE NETSOL-CCCE07032017-
*BB NETSOL-CCCE10032017-
*     material - homologación
      CLEAR: lw_homolog.
      SELECT SINGLE * INTO lw_homolog FROM ztmmhomolog
        WHERE material_balanza  = lv_material_b.
*BE NETSOL-CCCE10032017-
*BE NETSOL-CCCE26122016+
*     Guía de remisión.
      lv_gr = p_ti_flujorecdet-gr  .

      IF p_tipo_p = 'T'.
** Cargamos el batch-input para actualizar el DT por la Trx. VT02N
*        PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '1000' CHANGING w_bdcdata.
*        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'P_PEDIDO' CHANGING w_bdcdata.
*        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '/00' CHANGING w_bdcdata.
*        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'P_PEDIDO' lv_ebeln CHANGING w_bdcdata.
*        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'P_BTN1' lv_btn1 CHANGING w_bdcdata.
*        APPEND  w_bdcdata TO ti_bdcdata.
      ENDIF.

      PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '1000' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'P_REGCON' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '=ENTER' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_PEDIDO' lv_ebeln CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_REGCON' lv_regcon CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_BTN1' lv_btn1 CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.

      PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '1000' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'P_REGMAN' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '=ENTER' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_PEDIDO' lv_ebeln CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_REGCON' lv_regcon CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_REGMAN' lv_regman CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_FICTIC' ' ' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_BTN1' lv_btn1 CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.

      IF p_tipo_p = 'T'.
*        PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '1000' CHANGING w_bdcdata.
*        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'P_MAT' CHANGING w_bdcdata.
*        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '/00' CHANGING w_bdcdata.
*        APPEND  w_bdcdata TO ti_bdcdata.
**      PERFORM ti_bdcdata_field  USING 'P_TICKET' lv_ticket CHANGING w_bdcdata.
**      APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'P_PEDIDO' lv_ebeln CHANGING w_bdcdata.
*        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'P_REGCON' lv_regcon CHANGING w_bdcdata.
*        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'P_REGMAN' lv_regcon CHANGING w_bdcdata .
*        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'P_PESNET' lv_peso_ne_txt CHANGING w_bdcdata  .
*        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'P_PLACA' lv_tracto_pl CHANGING w_bdcdata .
*        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'P_GR' lv_gr CHANGING w_bdcdata .
*        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'P_MAT' lv_material_b CHANGING w_bdcdata  .
*        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'P_BTN1' lv_btn1 CHANGING w_bdcdata.
*        APPEND  w_bdcdata TO ti_bdcdata.
      ENDIF .

      PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '1000' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'P_BTN2' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '=ENTER' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
*      PERFORM ti_bdcdata_field  USING 'P_TICKET' lv_ticket CHANGING w_bdcdata.
*      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_PEDIDO' lv_ebeln CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_REGCON' lv_regcon CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_REGMAN' lv_regman CHANGING w_bdcdata .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_PESNET' lv_peso_ne_txt CHANGING w_bdcdata  .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_PLACA' lv_tracto_pl CHANGING w_bdcdata .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_GR' lv_gr CHANGING w_bdcdata .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_MAT' lv_material_b CHANGING w_bdcdata  .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_BTN1' ' ' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_BTN2' lv_btn2 CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.

      PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '1000' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'P_FECHA' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '/00' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
*      PERFORM ti_bdcdata_field  USING 'P_TICKET' lv_ticket CHANGING w_bdcdata.
*      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_PEDIDO' lv_ebeln CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_REGCON' lv_regcon CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_REGMAN' lv_regman CHANGING w_bdcdata .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_PESNET' lv_peso_ne_txt CHANGING w_bdcdata  .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_PLACA' lv_tracto_pl CHANGING w_bdcdata .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_GR' lv_gr CHANGING w_bdcdata .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_MAT' lv_material_b CHANGING w_bdcdata  .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_BTN2' lv_btn2 CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_FECHA' lv_fecha_txt CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.

      PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '1000' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'P_FECHA' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '=ONLI' CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
*      PERFORM ti_bdcdata_field  USING 'P_TICKET' lv_ticket CHANGING w_bdcdata.
*      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_PEDIDO' lv_ebeln CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_REGCON' lv_regcon CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_REGMAN' lv_regman CHANGING w_bdcdata .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_PESNET' lv_peso_ne_txt CHANGING w_bdcdata  .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_PLACA' lv_tracto_pl CHANGING w_bdcdata .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_GR' lv_gr CHANGING w_bdcdata .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_MAT' lv_material_b CHANGING w_bdcdata  .
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_BTN2' lv_btn2 CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.
      PERFORM ti_bdcdata_field  USING 'P_FECHA' lv_fecha_txt CHANGING w_bdcdata.
      APPEND  w_bdcdata TO ti_bdcdata.

      IF p_tipo_p = 'T'  .

        PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '0110' CHANGING w_bdcdata.
        APPEND  w_bdcdata TO ti_bdcdata.
        PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '/00' CHANGING w_bdcdata.
        APPEND  w_bdcdata TO ti_bdcdata.
        PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'GWA_DATASAL-RUTA' CHANGING w_bdcdata.
        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'GWA_DATASAL-STCD1' lv_transp_ruc CHANGING w_bdcdata."NETSOL-CCH04092015-
*        APPEND  w_bdcdata TO ti_bdcdata.                                                     "NETSOL-CCH04092015-
        PERFORM ti_bdcdata_field  USING 'GWA_DATASAL-RUTA' lv_ruta CHANGING w_bdcdata.
        APPEND  w_bdcdata TO ti_bdcdata.

        PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '0110' CHANGING w_bdcdata.
        APPEND  w_bdcdata TO ti_bdcdata.
        PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '=SAVE' CHANGING w_bdcdata.
        APPEND  w_bdcdata TO ti_bdcdata.
        PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'GWA_DATASAL-STCD1' CHANGING w_bdcdata.
        APPEND  w_bdcdata TO ti_bdcdata.
*        PERFORM ti_bdcdata_field  USING 'GWA_DATASAL-STCD1' lv_transp_ruc CHANGING w_bdcdata."NETSOL-CCH04092015-
*        APPEND  w_bdcdata TO ti_bdcdata.                                                     "NETSOL-CCH04092015-
        PERFORM ti_bdcdata_field  USING 'GWA_DATASAL-RUTA' lv_ruta CHANGING w_bdcdata.
        APPEND  w_bdcdata TO ti_bdcdata.

        PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '0110' CHANGING w_bdcdata.
        APPEND  w_bdcdata TO ti_bdcdata.
        PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '/EE' CHANGING w_bdcdata.
        APPEND  w_bdcdata TO ti_bdcdata.
        PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'P_TICKET' CHANGING w_bdcdata.
        APPEND  w_bdcdata TO ti_bdcdata.

      ELSEIF p_tipo_p = 'C'  .

        IF p_flag_transp  = 'X' .                                       "NETSOL-CCH18082015+
          PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '0100' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '/00' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'GWA_DATASAL-RUTA' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'GWA_DATASAL-STCD1' lv_transp_ruc CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'GWA_DATASAL-RUTA' lv_ruta CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
*BB NETSOL-CCCE10032017+
          IF lw_homolog-peso_proveedor = 'X'.
            PERFORM ti_bdcdata_field  USING 'GWA_DATASAL-LSMNG' lv_peso_gr_txt CHANGING w_bdcdata.
            APPEND  w_bdcdata TO ti_bdcdata.
          ENDIF.
*BE NETSOL-CCCE10032017+
          PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '0100' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '=SAVE' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'GWA_DATASAL-STCD1' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'GWA_DATASAL-STCD1' lv_transp_ruc CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'GWA_DATASAL-RUTA' lv_ruta CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
*BB NETSOL-CCCE10032017+
          IF lw_homolog-peso_proveedor = 'X'.
            PERFORM ti_bdcdata_field  USING 'GWA_DATASAL-LSMNG' lv_peso_gr_txt CHANGING w_bdcdata.
            APPEND  w_bdcdata TO ti_bdcdata.
          ENDIF.
*BE NETSOL-CCCE10032017+
          PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '0100' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '/EE' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'P_PEDIDO' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
        ELSE.                                                           "Begin NETSOL-CCH18082015+
          PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '0100' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '=SAVE' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'GWA_DATASAL-TICKET' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
*BB NETSOL-CCCE10032017+
          IF lw_homolog-peso_proveedor = 'X'.
            PERFORM ti_bdcdata_field  USING 'GWA_DATASAL-LSMNG' lv_peso_gr_txt CHANGING w_bdcdata.
            APPEND  w_bdcdata TO ti_bdcdata.
          ENDIF.
*BE NETSOL-CCCE10032017+
          PERFORM ti_bdcdata_dynpro USING 'ZMM_RPT_INGR_AUTO_MATPRIMAS' '1000' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'BDC_OKCODE' '/EE' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
          PERFORM ti_bdcdata_field  USING 'BDC_CURSOR' 'P_TICKET' CHANGING w_bdcdata.
          APPEND  w_bdcdata TO ti_bdcdata.
        ENDIF.
*                                                                       "End NETSOL-CCH18082015+
      ENDIF.

      CALL TRANSACTION 'ZMM10'
                   USING    ti_bdcdata
*                   MODE     gv_mode
*                   UPDATE   gv_update
                   OPTIONS FROM lw_option
                   MESSAGES INTO ti_messtab.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'
*       IMPORTING
*         RETURN        =
        .

    ELSE.
      LOOP AT p_ti_flujorecdet  .

      ENDLOOP.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " INGRESO_MP
*&---------------------------------------------------------------------*
*&      Form  TI_BDCDATA_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4007   text
*      -->P_4008   text
*      <--P_W_BDCDATA  text
*----------------------------------------------------------------------*
FORM ti_bdcdata_dynpro  USING program dynpro CHANGING s_bdcdata.

  DATA: w_bdcdata2 TYPE bdcdata.
  CLEAR w_bdcdata2.
  CLEAR s_bdcdata.
  w_bdcdata2-program  = program.
  w_bdcdata2-dynpro   = dynpro.
  w_bdcdata2-dynbegin = 'X'.
  s_bdcdata = w_bdcdata2.

ENDFORM.                    " TI_BDCDATA_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  TI_BDCDATA_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4018   text
*      -->P_4019   text
*      <--P_W_BDCDATA  text
*----------------------------------------------------------------------*
FORM ti_bdcdata_field  USING    fnam fval CHANGING s_bdcdata.

  DATA: w_bdcdata2 TYPE bdcdata.
  CLEAR w_bdcdata2.
  CLEAR s_bdcdata.
  w_bdcdata2-fnam = fnam.
  w_bdcdata2-fval = fval.
  s_bdcdata = w_bdcdata2.

ENDFORM.                    " TI_BDCDATA_FIELD
*&---------------------------------------------------------------------*
*&      Form  REORGANIZE_TABLE2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_VSTEL  text
*      -->P_0994   text
*      <--P_L_FLAG  text
*----------------------------------------------------------------------*
FORM reorganize_table2  USING p_vstel TYPE likp-vstel
                              p_fila TYPE n
                        CHANGING p_flag TYPE c.

  DATA: lt_datospanel      TYPE STANDARD TABLE OF znstb_datospanel WITH HEADER LINE,
        lt_datospanel_temp TYPE STANDARD TABLE OF znstb_datospanel WITH HEADER LINE,
        l_correl(2)        TYPE n.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_datospanel
    FROM znstb_datospanel
    WHERE vstel = p_vstel
    AND fila = p_fila.

  IF NOT lt_datospanel IS INITIAL.

    LOOP AT lt_datospanel.
      DELETE FROM znstb_datospanel WHERE vstel = p_vstel AND
                                         fila = lt_datospanel-fila     AND
                                         correl = lt_datospanel-correl AND
                                         tknum_id = lt_datospanel-tknum_id  .
    ENDLOOP.
*  END PCVPDK915128
    SORT lt_datospanel BY fila tknum_id valor.                          "NETSOL-CCCE19052016+
    DELETE ADJACENT DUPLICATES FROM lt_datospanel COMPARING fila tknum_id valor.
    INSERT znstb_datospanel FROM TABLE lt_datospanel  .

    LOOP AT lt_datospanel.
      IF sy-tabix EQ 1.
        MOVE-CORRESPONDING lt_datospanel TO lt_datospanel_temp.
        lt_datospanel_temp-correl = '01'.
        APPEND lt_datospanel_temp.
        CONTINUE.
      ENDIF.
      l_correl = lt_datospanel-correl - lt_datospanel_temp-correl.
      IF l_correl GE 1 OR sy-tabix EQ lines( lt_datospanel ).
        p_flag = 'X'.
        l_correl = lt_datospanel_temp-correl + 1.
        MOVE-CORRESPONDING lt_datospanel TO lt_datospanel_temp.
        lt_datospanel_temp-correl = l_correl.
        APPEND lt_datospanel_temp.
      ENDIF.
    ENDLOOP.
    DELETE znstb_datospanel FROM TABLE lt_datospanel.
    INSERT znstb_datospanel FROM TABLE lt_datospanel_temp.
  ENDIF.

ENDFORM.                    " REORGANIZE_TABLE2
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_MATN1_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_MATERIAL  text
*      <--P_LV_MATERIAL  text
*----------------------------------------------------------------------*
FORM conversion_exit_matn1_output  USING    p_material
                                   CHANGING p_v_material.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    EXPORTING
      input  = p_material
    IMPORTING
      output = p_v_material.

ENDFORM.                    " CONVERSION_EXIT_MATN1_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  GUARDAR_DATOS_SALIDA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM guardar_datos_salida2  TABLES  p_t_data_cab STRUCTURE tab512"ztab256 "NETSOL-CCH04092015
                                    p_t_data_det STRUCTURE tab512"ztab256 "NETSOL-CCH04092015
                                    p_t_flujoreccab STRUCTURE znstbflujoreccab
                                    p_t_flujorecdet STRUCTURE znstbflujorecdet
                           USING    p_flag_dt
                                    p_fecha_act
                                    p_hora_act
                                    p_tipo_p
                                    p_provee_id
                                    p_provee_des
                                    p_turno_id
                                    p_dti_id
                                    p_tknum
                                    p_material
                                    p_um
                                    p_transp_ruc
                                    p_transp_des
                                    p_chofer_dni
                                    p_chofer_des
                                    p_chofer_bvt
                                    p_tracto
                                    p_carreta
                                    p_ticket
                                    p_trfid_id
                                    p_trfid_nro
                                    p_color
                                    p_destin_id
                                    p_destin_des
                                    p_ruta                              "NETSOL-CCCE13032017+
                                    p_separador.

  DATA: lv_fecha_txt    TYPE string,
        lv_hora_txt     TYPE string,
*        lv_concat       TYPE string ,                                  "NETSOL-CCH04092015-
        lv_ebeln        TYPE ekko-ebeln,
        lv_vbeln_vl     TYPE lips-vbeln,
        lv_material_des TYPE makt-maktx,
        lv_canti_txt    TYPE char20,
        lv_item         TYPE ekpo-ebelp,
        lv_gremi        TYPE likp-xblnr,
        lv_um           TYPE lips-meins,
        lv_cant_total   TYPE lips-lfimg,                                "NETSOL-CCH31072015+
        lv_cant         TYPE lips-lfimg,                                "NETSOL-CCH31072015+
        lv_peso_in_txt  TYPE char20,
        lv_peso_gr_txt  TYPE char20,
        lv_peso_fi_txt  TYPE char20,
        lv_peso_ne_txt  TYPE char20,
        lw_tvswz        TYPE tvswz,
        lw_t001w        TYPE t001w,
        lw_t001k        TYPE t001k  ,                                   "NETSOL-CCH05082015+
        lw_t001         TYPE t001  ,                                    "NETSOL-CCH05082015+
        lw_zona_des     TYPE znstb_zona_des ,                           "NETSOL-CCH07082015+
        lv_material     TYPE mara-matnr.
  DATA: lv_concat(500)  TYPE c  .                                       "NETSOL-CCH04092015+
* formato fecha.
  CONCATENATE p_fecha_act+4(2) '/' p_fecha_act+6(2) '/' p_fecha_act+0(4)
    INTO lv_fecha_txt .

  CONCATENATE p_hora_act+0(2) ':' p_hora_act+2(2) ':'  p_hora_act+4(2)
    INTO lv_hora_txt  .

* unidad de medida.
  lv_um = p_um  .

* descripción de material.
  SELECT SINGLE maktx INTO lv_material_des FROM makt
  WHERE matnr = p_material
    AND spras = sy-langu  .

* formato material.
  PERFORM conversion_exit_matn1_output USING p_material
                                       CHANGING p_material .

* pesos.
  READ TABLE p_t_flujoreccab INDEX 1  .
  lv_peso_gr_txt  = p_t_flujoreccab-peso_gr .
  lv_peso_in_txt  = p_t_flujoreccab-peso_in .
  lv_peso_fi_txt  = p_t_flujoreccab-peso_fi .
  lv_peso_ne_txt  = p_t_flujoreccab-peso_ne .

  CONDENSE: lv_peso_gr_txt  NO-GAPS,
            lv_peso_in_txt  NO-GAPS,
            lv_peso_fi_txt  NO-GAPS,
            lv_peso_ne_txt  NO-GAPS.

* empresa (centro)
  CLEAR:  lw_tvswz        .
*  SELECT SINGLE * INTO lw_tvswz FROM tvswz                             "NETSOL-CCCE12012017-
*    WHERE vstel = p_t_flujoreccab-vstel .                              "NETSOL-CCCE12012017-
*BB NETSOL-CCCE12012017+
  READ TABLE p_t_flujorecdet INDEX 1.
* Determinación de centro.
  CLEAR: lw_tvswz-werks.
  IF p_t_flujorecdet-ebeln IS NOT INITIAL.
    SELECT SINGLE werks INTO lw_tvswz-werks FROM ekpo
      WHERE ebeln EQ p_t_flujorecdet-ebeln
        AND matnr EQ p_t_flujoreccab-matnr.
  ELSE.
    lw_tvswz-werks = p_t_flujoreccab-cenha.
  ENDIF.
*BE NETSOL-CCCE12012017+
*  descripcion de centro
  CLEAR:  lw_t001w  .
  SELECT SINGLE * INTO lw_t001w FROM t001w
    WHERE werks = lw_tvswz-werks  .
*  Sociedad                                                             "Begin NETSOL-CCH05082015+
  CLEAR:  lw_t001k ,  lw_t001 .
  SELECT SINGLE * INTO lw_t001k FROM t001k
    WHERE bwkey = lw_tvswz-werks  .
  SELECT SINGLE * INTO lw_t001 FROM t001
    WHERE bukrs = lw_t001k-bukrs  .
*                                                                       "End NETSOL-CCH05082015+
*  zona de descarga - descripción                                       "Begin NETSOL-CCH07082015+
  SELECT SINGLE * INTO lw_zona_des FROM znstb_zona_des
    WHERE zona_id  = p_t_flujoreccab-zona_id
     AND  vstel    = p_t_flujoreccab-vstel .
*                                                                       "End NETSOL-CCH07082015+
  CONCATENATE p_tipo_p
*              lw_t001w-name1                                           "NETSOL-CCH05082015-
              lw_t001-butxt                                             "NETSOL-CCH05082015+
              p_provee_id
              p_provee_des
              lv_fecha_txt
              lv_hora_txt
              p_turno_id
              p_dti_id
              p_tknum
              lv_peso_gr_txt
              lv_peso_in_txt
              lv_peso_fi_txt
              lv_peso_ne_txt
  INTO lv_concat SEPARATED BY p_separador  .                            "NETSOL-CCH04092015+
  APPEND lv_concat TO p_t_data_cab .                                    "NETSOL-CCH04092015+
  CLEAR lv_concat .                                                     "NETSOL-CCH04092015+
*              p_t_flujoreccab-zona_id                                  "NETSOL-CCH07082015-
  CONCATENATE                                                           "NETSOL-CCH04092015+
              lw_zona_des-descripcion                                   "NETSOL-CCH07082015+
              p_transp_ruc
              p_transp_des
              p_chofer_dni
              p_chofer_des
              p_chofer_bvt
              p_tracto
              p_carreta
              p_ticket
              p_trfid_id
              p_trfid_nro
              p_color
              p_ruta                                                    "NETSOL-CCCE13032017+
              p_t_flujorecdet-ebeln                                     "NETSOL-CCCE13032017+
              p_t_flujorecdet-gr                                        "NETSOL-CCCE13032017+
              p_t_flujorecdet-meins                                     "NETSOL-CCCE13032017+
              p_material                                                "NETSOL-CCCE13032017+
              lv_material_des                                           "NETSOL-CCCE13032017+
              p_destin_id                                               "NETSOL-CCCE13032017+
              p_destin_des                                              "NETSOL-CCCE13032017+
*  INTO l_concat SEPARATED BY p_separador  .                            "NETSOL-CCH04092015-
*  APPEND l_concat TO p_t_data_cab .                                    "NETSOL-CCH04092015-
  INTO lv_concat SEPARATED BY p_separador  .                            "NETSOL-CCH04092015+
  APPEND lv_concat TO p_t_data_cab .                                    "NETSOL-CCH04092015+
**----------------------------------------------------------------------*
** Estructa de datos de posición
**----------------------------------------------------------------------*
*  CLEAR: lv_cant_total .                                                "NETSOL-CCH31072015+
*  LOOP AT p_ti_ekko INTO  w_ekko  .
*
*    lv_ebeln       = w_ekko-ebeln  .
*
*    IF p_flag_dt EQ 'X' .
*
*      LOOP AT p_ti_ekpo INTO w_ekpo WHERE ebeln EQ w_ekko-ebeln .
*
*        READ TABLE p_ti_ekbe INTO w_ekbe  WITH KEY  ebeln = w_ekpo-ebeln
*                                                  ebelp = w_ekpo-ebelp  .
*        IF sy-subrc EQ 0  .
*          READ TABLE p_ti_likp  INTO w_likp WITH KEY  vbeln = w_ekbe-xblnr  .
*          READ TABLE p_ti_lips  INTO w_lips WITH KEY  vbeln = w_ekbe-xblnr  .
*          IF sy-subrc EQ 0  .
**          lv_ebeln       = w_ekpo-ebeln  .
*            lv_vbeln_vl    = w_lips-vbeln  .
*            lv_gremi       = w_likp-xblnr .
*            lv_canti_txt   = w_lips-lfimg  .
*            lv_item  = w_ekpo-ebelp  .
*            lv_cant_total = lv_cant_total + w_lips-lfimg  .             "NETSOL-CCH31072015+
**           quitando espacios en blanco.
*            CONDENSE lv_canti_txt NO-GAPS .
*
*            CONCATENATE lv_ebeln
*                        lv_vbeln_vl
*                        lv_gremi
*                        p_material
*                        lv_material_des
*                        lv_canti_txt
*                        lv_um
*                        lv_item
*                        p_destin_id
*                        p_destin_des
*            INTO l_concat
*            SEPARATED BY p_separador  .
*
*            APPEND l_concat TO p_t_data_det .
*
*          ENDIF.
*        ENDIF.
*
*      ENDLOOP.
*
*    ELSE.
*
*      READ TABLE p_t_pedido WITH KEY ebeln  = w_ekko-ebeln  .
*      IF sy-subrc EQ 0  .
*        lv_gremi      = p_t_pedido-gremi  .
*        lv_canti_txt  = p_t_pedido-peson  .
*        lv_um         = p_t_pedido-meins  .
*      ENDIF.
*      CONDENSE p_t_pedido-peson  NO-GAPS .
*      lv_cant       = p_t_pedido-peson  .
**     quitando espacios en blanco.
*      CONDENSE lv_canti_txt NO-GAPS .                                 "NETSOL-CCH31072015+
*      lv_cant_total = lv_cant_total + lv_cant  .                      "NETSOL-CCH31072015+
*
*      READ TABLE p_ti_ekpo INDEX 1 .
*      lv_item = p_ti_ekpo-ebelp .
*
*      SELECT SINGLE maktx INTO lv_material_des FROM makt
*       WHERE matnr = p_material
*         AND spras = sy-langu  .
*
*      lv_item  = w_ekpo-ebelp  .
*
*      CONCATENATE lv_ebeln
*                  lv_vbeln_vl
*                  lv_gremi
*                  p_material
*                  lv_material_des
*                  lv_canti_txt
*                  lv_um
*                  lv_item
*                  p_destin_id
*                  p_destin_des
*      INTO l_concat
*      SEPARATED BY v_separador  .
*
*      APPEND l_concat TO p_t_data_det .
*
*
*    ENDIF.
*  ENDLOOP.
*
** total                                                                 "Begin NETSOL-CCH31072015+
*  lv_canti_txt = lv_cant_total  .
*  CONDENSE lv_canti_txt NO-GAPS .
*  CONCATENATE 'T' p_material lv_material_des lv_canti_txt lv_um
*  INTO l_concat SEPARATED BY v_separador  .
*
*  APPEND l_concat TO p_t_data_det .
** total                                                                 "End NETSOL-CCH31072015+

ENDFORM.                    " GUARDAR_DATOS_SALIDA2
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_GREMISION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_PEDIDO  text
*      <--P_L_ERROR  text
*      <--P_L_MSG  text
*----------------------------------------------------------------------*
FORM validar_gremision  TABLES   p_t_pedido STRUCTURE znses_pedidorec
                        USING    p_vstel
                        CHANGING p_error
                                 p_msg  .

  DATA: lt_flujorecdet  TYPE STANDARD TABLE OF znstbflujorecdet .

  SELECT * INTO TABLE lt_flujorecdet  FROM znstbflujorecdet
    FOR ALL ENTRIES IN p_t_pedido
    WHERE ebeln = p_t_pedido-ebeln
      AND vstel = p_vstel
      AND evento_id = '0100' .

  LOOP AT lt_flujorecdet INTO w_flujorecdet  .

    READ TABLE p_t_pedido WITH KEY ebeln  = w_flujorecdet-ebeln .
    IF w_flujorecdet-gr  = p_t_pedido-gremi   .
      p_error   = 'X' .
      p_msg = text-m55  .
      EXIT  .
    ENDIF.

  ENDLOOP.

ENDFORM.                    " VALIDAR_GREMISION
*&---------------------------------------------------------------------*
*&      Form  CAMBIAR_MEDIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cambiar_medida  USING    p_un_new
                              p_un_old
                              p_wg_new
                              p_wg_old.

  DATA: l_wg_old TYPE plfh-mgvgw,
        l_wg_new TYPE plfh-mgvgw,
        l_un_old TYPE t006-msehi,
        l_un_new TYPE t006-msehi.

  CLEAR: l_wg_old, l_wg_new, l_un_old, l_un_new.

*  l_wg_old = p_wg_old.
  l_un_old = p_un_old.
  l_un_new = p_un_new.
*BB NETSOL-CCCE20180516+
  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input                = p_wg_old
      unit_in              = l_un_old
      unit_out             = l_un_new
    IMPORTING
      output               = p_wg_new
    EXCEPTIONS
      conversion_not_found = 1
      division_by_zero     = 2
      input_invalid        = 3
      output_invalid       = 4
      overflow             = 5
      type_invalid         = 6
      units_missing        = 7
      unit_in_not_found    = 8
      unit_out_not_found   = 9
      OTHERS               = 10.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.
*BE NETSOL-CCCE20180516+
*BB NETSOL-CCCE20180516-
*  CALL FUNCTION 'CF_UT_UNIT_CONVERSION'
*    EXPORTING
*      unit_new_imp  = l_un_new
*      unit_old_imp  = l_un_old
*      value_old_imp = l_wg_old
*    IMPORTING
*      value_new_exp = l_wg_new
*    EXCEPTIONS
*      overflow      = 1
*      OTHERS        = 2.
*   p_wg_new = l_wg_new.
*BE NETSOL-CCCE20180516-

ENDFORM.                    " CAMBIAR_MEDIDA
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_MATNR_HOMOLOG
*&---------------------------------------------------------------------*
FORM validar_matnr_homolog  USING    p_material
                                     p_destin_id
                                     p_tipo_p                          "NETSOL-CCCE01032017+
                                     p_ruc                             "NETSOL-CCCE01032017+
                                     p_ruta                            "NETSOL-CCCE01032017+
                            CHANGING p_flag_transp
                                     p_homolog
                                     p_flag_transp_spec                 "NETSOL-CCCE01032017+
                                     p_error
                                     p_msg.

  DATA :  lw_homolog TYPE ztmmhomolog,
          lv_werks   TYPE t001w-werks.

  DATA :  lt_homolog TYPE TABLE OF ztmmhomolog,                         "NETSOL-CCCE01032017+
          lv_lines   TYPE i.
  CLEAR: p_flag_transp, p_homolog, p_error, p_msg, lv_werks,
         p_flag_transp_spec.                                            "NETSOL-CCCE01032017+

"EMAT - Enrique Aguirre - #95219 17.07.2020 - I
DATA: l_bsart_flag TYPE C.
DATA: l_bsart_flag_x TYPE C.
CLEAR l_bsart_flag.
CLEAR l_bsart_flag_x.

LOOP AT ti_ekko ASSIGNING FIELD-SYMBOL(<FS_EKKO_BSART>) WHERE BSART IN gv_rng_bsart.
  l_bsart_flag = 'X'.
  EXIT.
ENDLOOP.

LOOP AT ti_ekko ASSIGNING <FS_EKKO_BSART> WHERE BSART IN gv_rng_bsart_x.
  l_bsart_flag_x = 'X'.
  EXIT.
ENDLOOP.

"EMAT - Enrique Aguirre - #95219 17.07.2020 - I

  lv_werks  = p_destin_id .
*BB NETSOL-CCCE01032017-
*  SELECT SINGLE * INTO lw_homolog FROM  ztmmhomolog
*    WHERE material_sap = p_material
*      AND werks        = lv_werks .
*  IF lw_homolog IS INITIAL .
*BE NETSOL-CCCE01032017-
*BB NETSOL-CCCE01032017+
  SELECT * INTO TABLE lt_homolog FROM  ztmmhomolog
    WHERE material_sap = p_material
      AND werks        = lv_werks .
  IF lt_homolog IS INITIAL .
*BE NETSOL-CCCE01032017+
    p_error   = 'X' .
    CONCATENATE text-m61 lv_werks INTO p_msg SEPARATED BY space .
    EXIT  .
  ELSE.
*BB NETSOL-CCCE01032017+
*   Escenario compra pto.Planta&Alm.Proveedor
    DESCRIBE TABLE lt_homolog LINES lv_lines.
    IF lv_lines GT 1.
      IF p_tipo_p EQ 'C'.
          IF p_ruc IS NOT INITIAL OR p_ruta IS NOT INITIAL.
            MOVE: abap_on TO p_flag_transp.
            MOVE: abap_on TO p_flag_transp_spec.
           "EMAT - Enrique Aguirre - #106425 - 29.09.2020 - I
              LOOP AT lt_homolog INTO lw_homolog WHERE transporte = p_flag_transp and
                                                       CENTRO_ORIGEN IS INITIAL.
                  MOVE: lw_homolog-material_balanza TO p_homolog.
              ENDLOOP.
          ELSEIF p_ruc IS INITIAL AND p_ruta IS INITIAL.
              CLEAR p_flag_transp.
              CLEAR p_flag_transp_spec.
              LOOP AT lt_homolog INTO lw_homolog WHERE transporte IS INITIAL and
                                                       CENTRO_ORIGEN IS INITIAL.
                  MOVE: lw_homolog-material_balanza TO p_homolog.
              ENDLOOP.
          ENDIF.
      ELSEIF p_tipo_p EQ 'T'.
          IF p_ruc IS NOT INITIAL OR p_ruta IS NOT INITIAL.
            MOVE: abap_on TO p_flag_transp.
            MOVE: abap_on TO p_flag_transp_spec.
              LOOP AT lt_homolog INTO lw_homolog WHERE transporte = p_flag_transp and
                                                       CENTRO_ORIGEN IS NOT INITIAL.
                  MOVE: lw_homolog-material_balanza TO p_homolog.
              ENDLOOP.
          ELSEIF p_ruc IS INITIAL AND p_ruta IS INITIAL.
              CLEAR p_flag_transp_spec.
              CLEAR p_flag_transp.
              LOOP AT lt_homolog INTO lw_homolog WHERE transporte IS INITIAL and
                                                       CENTRO_ORIGEN IS NOT INITIAL.
                  MOVE: lw_homolog-material_balanza TO p_homolog.
              ENDLOOP.
          ENDIF.
      ENDIF.

       "EMAT - Enrique Aguirre - #106425 - 29.09.2020 - Comentar
           "EMAT - Enrique Aguirre - #95219 17.07.2020 - I
*              IF l_bsart_flag_x EQ 'X'.
*                p_flag_transp = 'X'.
*              ELSEIF l_bsart_flag EQ 'X'.
*                CLEAR p_flag_transp.
*              ENDIF.
           "EMAT - Enrique Aguirre - #95219 17.07.2020 - I
       "EMAT - Enrique Aguirre - #106425 - 29.09.2020 - Comentar

"EMAT - Enrique Aguirre - #106425 - 29.09.2020 - Comentar
    "EMAT - Enrique Aguirre - #105078 21.08.2020 - I
*     IF p_flag_transp IS INITIAL.
*        DELETE lt_homolog WHERE CENTRO_ORIGEN IS NOT INITIAL.
*     ELSEIF p_flag_transp EQ 'X'.
*        DELETE lt_homolog WHERE CENTRO_ORIGEN IS INITIAL.
*     ENDIF.
    "EMAT - Enrique Aguirre - #105078 21.08.2020 - I
"EMAT - Enrique Aguirre - #106425 - 29.09.2020 - Comentar


"EMAT - Enrique Aguirre - #106425 - 29.09.2020 - Comentar
*      READ TABLE lt_homolog INTO lw_homolog WITH KEY transporte = p_flag_transp.
*      MOVE: abap_on                     TO p_flag_transp_spec,
*            lw_homolog-transporte       TO p_flag_transp ,
*            lw_homolog-material_balanza TO p_homolog.
"EMAT - Enrique Aguirre - #106425 - 29.09.2020 - Comentar

    ELSE.
      READ TABLE lt_homolog INTO lw_homolog INDEX 1.
*BE NETSOL-CCCE01032017+
      p_flag_transp = lw_homolog-transporte .
      p_homolog     = lw_homolog-material_balanza .
    ENDIF.                                                              "NETSOL-CCCE01032017+
  ENDIF.

ENDFORM.                    " VALIDAR_MATNR_HOMOLOG
*&---------------------------------------------------------------------*
*&      Form  OBTENER_VALIDAR_ESCENARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM obtener_validar_escenario  USING    p_tipo_p
                                         p_flag_transp
                                         p_escenario_id
                                CHANGING p_error
                                         p_msg.

  CLEAR:  p_error .

  IF p_tipo_p = 'C' AND p_flag_transp = ' ' AND p_escenario_id = 'E001' .
    "Continue.
  ELSEIF p_tipo_p = 'C' AND p_flag_transp = 'X' AND p_escenario_id = 'E002' .
    "Continue.
  ELSEIF p_tipo_p = 'T' AND p_flag_transp = 'X' AND ( p_escenario_id = 'E003' OR
                                                  p_escenario_id = 'E004' OR
*                                                  p_escenario_id = 'E005' OR
                                                  p_escenario_id = 'E006'  ).
    "Continue.
  ELSEIF p_tipo_p = 'T' AND p_flag_transp = 'X' AND p_escenario_id = 'E005' .
    p_error = 'X' .
    CONCATENATE text-m72 p_escenario_id '.' text-m72 '.' INTO p_msg SEPARATED BY space .
  ELSE.
    p_error = 'X' .
    CONCATENATE text-m71 p_escenario_id '.' INTO p_msg SEPARATED BY space .
  ENDIF.

ENDFORM.                    " OBTENER_VALIDAR_ESCENARIO
*&---------------------------------------------------------------------*
*&      Form  GUARDAR_FLUJO_RECEPCION_DM
*&---------------------------------------------------------------------*
FORM guardar_flujo_recepcion_dm  TABLES   p_ti_flujoreccab STRUCTURE znstbflujoreccab
                                          p_ti_flujorecdet STRUCTURE znstbflujorecdet
                                 USING    p_mblnr
                                          p_mjahr
                                          p_error
                                          p_msg.

  LOOP AT p_ti_flujoreccab  .
    LOOP AT p_ti_flujorecdet WHERE  tknum_id EQ p_ti_flujoreccab-tknum_id .
      p_ti_flujorecdet-mblnr  = p_mblnr .
      p_ti_flujorecdet-mjahr  = p_mjahr .
      MODIFY p_ti_flujorecdet .
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " GUARDAR_FLUJO_RECEPCION_DM
*&---------------------------------------------------------------------*
*&      Form  VALIDACION_PESO_ESCENARIO
*&---------------------------------------------------------------------*
FORM validacion_peso_escenario  USING    p_escanrio_id
                                         p_vstel
                                         p_material
                                CHANGING p_peso_ne
                                         p_error
                                         p_msg.

  DATA: lt_flujoreccab TYPE STANDARD TABLE OF znstbflujoreccab  WITH HEADER LINE  .

  REFRESH : lt_flujoreccab  .

  IF p_escanrio_id  = 'E006'  .             "Arena
    SELECT * INTO TABLE lt_flujoreccab  FROM znstbflujoreccab
      WHERE vstel = p_vstel
        AND matnr = p_material
        AND evento_id = '0600'
        AND escenario_id  = p_escanrio_id
        AND fdate = sy-datum  .             "fecha del dia
    IF sy-subrc EQ 0 .
      SORT lt_flujoreccab BY tknum_id ASCENDING .
      READ TABLE lt_flujoreccab INDEX 1.       "Obtiene el primero del día .
      IF sy-subrc EQ 0 .
        p_peso_ne = lt_flujoreccab-peso_ne  .
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " VALIDACION_PESO_ESCENARIO
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_LIBERACION_PEDIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_EKPO  text
*      <--P_L_ERROR  text
*      <--P_L_MSG  text
*----------------------------------------------------------------------*
FORM validar_liberacion_pedido  TABLES   p_ti_ekko STRUCTURE ekko
                                CHANGING p_error
                                         p_msg  .

  DATA: lw_flu_lib_ord TYPE ztpm_flu_lib_ord,
        lv_pendiente   TYPE c.

  LOOP AT p_ti_ekko .

*    SELECT SINGLE * INTO lw_flu_lib_ord FROM ztpm_flu_lib_ord
*     WHERE tipo_doc = 'P'
*      AND  banfn    = p_ti_ekpo-ebeln
*      AND  bnfpo    = p_ti_ekpo-ebelp .
*
*    IF sy-subrc EQ 0  .
*      PERFORM web_service USING lw_flu_lib_ord-banfn lw_flu_lib_ord-bnfpo
*                          lw_flu_lib_ord-tipo_doc lw_flu_lib_ord
*                          CHANGING lv_pendiente p_error p_msg.
*      IF p_error = 'X'  .
*        CONCATENATE p_msg ':' p_ti_ekpo-ebeln INTO p_msg SEPARATED BY space.
*      ENDIF.
*    ELSE.
*      p_error = 'X'  .
*      CONCATENATE text-m76 p_ti_ekpo-ebeln INTO p_msg SEPARATED BY space .
*    ENDIF.
    IF p_ti_ekko-frgke  NE 'L' .
      p_error = 'X'  .
      CONCATENATE text-m82 ':' p_ti_ekko-ebeln text-m83 '.' INTO p_msg
        SEPARATED BY space.
      EXIT  .
    ENDIF.

  ENDLOOP.

ENDFORM.                    " VALIDAR_LIBERACION_PEDIDO
*&---------------------------------------------------------------------*
*&      Form  WEB_SERVICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_FLU_LIB_ORD_BANFN  text
*      -->P_LW_FLU_LIB_ORD_BNFPO  text
*      -->P_LW_FLU_LIB_ORD_TIPO_DOC  text
*      <--P_P_ERROR  text
*      <--P_P_MSG  text
*----------------------------------------------------------------------*
FORM web_service  USING    docume
                           posici
                           us_tipo TYPE c
                           s_flu_lib_ord  STRUCTURE ztpm_flu_lib_ord
                  CHANGING ws_pendiente
                           p_error
                           p_msg.

  DATA: lv_flujo TYPE string,
        lv_liber TYPE string,
        lv_ulibe TYPE string,
        lv_recha TYPE string.

  DATA: lv_libe1(02) TYPE c,
        lv_libe2(02) TYPE c,
        lv_libe3(02) TYPE c,
        lv_libe4(02) TYPE c,
        lv_libe5(02) TYPE c,
        lv_libe6(02) TYPE c,
        lv_libe7(02) TYPE c,
        lv_fluj1(02) TYPE c,
        lv_fluj2(02) TYPE c,
        lv_fluj3(02) TYPE c,
        lv_fluj4(02) TYPE c,
        lv_fluj5(02) TYPE c,
        lv_fluj6(02) TYPE c,
        lv_fluj7(02) TYPE c,
        lv_usrec(02) TYPE c,
        lv_norec     TYPE string.

  DATA:   lv_actual    TYPE string,
          lv_libact(1) TYPE n,
          lv_creador   TYPE string,
          lv_creador1  TYPE string,
          lv_creflu    TYPE string,
          lv_conlib    TYPE i,
          lv_conflu    TYPE i,
          lv_index(2)  TYPE n.

  DATA:   lv_cadena TYPE string.

  DATA: p_resultado TYPE string.

  FIELD-SYMBOLS <fs_campo> TYPE any.

  CLEAR: lv_flujo, lv_liber, lv_ulibe, lv_recha, lv_conlib, lv_conflu.
  lv_flujo = s_flu_lib_ord-flujo.
  lv_liber = s_flu_lib_ord-liber.
  lv_ulibe = s_flu_lib_ord-ulibe.
  lv_recha = s_flu_lib_ord-recha.
* END BACK220615

  CLEAR: lv_creador, lv_creador1, lv_usrec, lv_creflu.
*  SPLIT p_resultado AT '¬' INTO lv_actual lv_creador ws_reporte-libe1 ws_reporte-libe2 ws_reporte-libe3 ws_reporte-libe4
*                                                     ws_reporte-libe5 ws_reporte-libe6 ws_reporte-libe7.

  SPLIT lv_liber AT '¬' INTO lv_creador1 lv_libe1 lv_libe2 lv_libe3 lv_libe4 lv_libe5 lv_libe6 lv_libe7.
*  SPLIT lv_ulibe AT '¬' INTO lv_creador ws_reporte-libe1 ws_reporte-libe2 ws_reporte-libe3 ws_reporte-libe4
*                                        ws_reporte-libe5 ws_reporte-libe6 ws_reporte-libe7.
***  SPLIT lv_ulibe AT '¬' INTO lv_creador ws_reporte-libe1 ws_reporte-libe2 ws_reporte-libe3 ws_reporte-libe4
***                                        ws_reporte-libe5 ws_reporte-libe6 ws_reporte-libe7.
  SPLIT lv_recha AT '¬' INTO lv_usrec lv_norec.
  SPLIT lv_flujo AT '¬' INTO lv_creflu lv_fluj1 lv_fluj2 lv_fluj3 lv_fluj4 lv_fluj5 lv_fluj6 lv_fluj7.


* Liberadores
  DO 7 TIMES.
    lv_index =  sy-index.
    CONCATENATE 'lv_libe'  lv_index+1(1) INTO lv_cadena.
    ASSIGN (lv_cadena) TO <fs_campo>.

    IF <fs_campo>+0(1) = '¬'.
      CLEAR <fs_campo>.
    ELSE.
      IF NOT <fs_campo> IS INITIAL.
        lv_conlib = lv_conlib + 1.
      ENDIF.
    ENDIF.
  ENDDO.

* Flujo
  DO 7 TIMES.
    lv_index =  sy-index.
    CONCATENATE 'lv_fluj'  lv_index+1(1) INTO lv_cadena.
    ASSIGN (lv_cadena) TO <fs_campo>.

    IF <fs_campo>+0(1) = '¬'.
      CLEAR <fs_campo>.
    ELSE.
      IF NOT <fs_campo> IS INITIAL.
        lv_conflu = lv_conflu + 1.
      ENDIF.
    ENDIF.
  ENDDO.

***  DO 7 TIMES.
***    lv_index =  sy-index.
***    CONCATENATE 'ws_reporte-libe'  lv_index+1(1) INTO lv_cadena.
***    ASSIGN (lv_cadena) TO <fs_campo>.
***
***    IF <fs_campo>+0(1) = '¬'.
***      CLEAR <fs_campo>.
***    ENDIF.
***  ENDDO.

*  REPLACE ALL OCCURRENCES OF '¬' IN  p_resultado WITH space.

*  IF sy-subrc EQ 0.
*    CONDENSE p_resultado.
*    IF lv_actual EQ '99'.
  IF lv_usrec IS INITIAL.
    IF lv_conflu = lv_conlib. "lv_flujo EQ lv_liber.
      IF     us_tipo EQ 'S'.
*        ws_reporte-sp_esta = 'Completamente Liberado'.
        p_msg = 'Completamente Liberado'.
      ELSEIF us_tipo EQ 'P'.
*        ws_reporte-ped_est = 'Completamente Liberado'.
        p_msg = 'Completamente Liberado'.
      ENDIF.

***      DO 7 TIMES.
***        lv_index =  sy-index.
***        CONCATENATE 'ws_reporte-libe'  lv_index+1(1) INTO lv_cadena.
***        ASSIGN (lv_cadena) TO <fs_campo>.
***
***        IF <fs_campo>+0(1) IS INITIAL OR <fs_campo>+0(1) = '¬'.
***          CLEAR <fs_campo>.
***        ELSE.
***          CONCATENATE 'LIBE'  lv_index+1(1) INTO lv_cadena.
***          CLEAR gs_cellcolor.
***          gs_cellcolor-fname     = lv_cadena.
***          gs_cellcolor-color-col = '5'. "Verde
***          APPEND gs_cellcolor TO ws_reporte-cellcolor.
***        ENDIF.
***      ENDDO.
    ELSE.
*      IF lv_actual EQ '98'.
      IF lv_libe1 IS INITIAL AND lv_libe2 IS INITIAL AND lv_libe3 IS INITIAL AND lv_libe4 IS INITIAL AND
         lv_libe5 IS INITIAL AND lv_libe6 IS INITIAL AND lv_libe7 IS INITIAL AND lv_creador1 IS INITIAL.

        ws_pendiente = 'X'.
        IF     us_tipo EQ 'S'.
*          ws_reporte-sp_esta = 'Pendiente Liberación Creador'.
          p_error = 'X' .
          p_msg = 'Pendiente Liberación Creador'.
        ELSEIF us_tipo EQ 'P'.
*          ws_reporte-ped_est = 'Pendiente Liberación Creador'.
          p_error = 'X' .
          p_msg = 'Pendiente Liberación Creador'.
        ENDIF.
      ELSE.
        ws_pendiente = 'X'.
        IF     us_tipo EQ 'S'.

*          ws_reporte-sp_esta = 'En proceso de Liberación por'.
          p_error = 'X' .
          p_msg   = 'En proceso de Liberación'  .
****          DO 7 TIMES.
****            lv_index =  sy-index.
****            CONCATENATE 'ws_reporte-libe'  lv_index+1(1) INTO lv_cadena.
****            ASSIGN (lv_cadena) TO <fs_campo>.
****
****            IF NOT <fs_campo> IS INITIAL.
****              CONCATENATE ws_reporte-sp_esta <fs_campo> INTO ws_reporte-sp_esta SEPARATED BY ','.
****            ENDIF.
****          ENDDO.
*          CONCATENATE 'En proceso de Liberación por' ws_reporte-libe1 ws_reporte-libe2
*                      ws_reporte-libe3 ws_reporte-libe4 ws_reporte-libe5 ws_reporte-libe6
*                      ws_reporte-libe7 INTO ws_reporte-sp_esta SEPARATED BY ','."space.
****          CONDENSE ws_reporte-sp_esta.
        ELSEIF us_tipo EQ 'P'.
          p_error = 'X' .
          p_msg   = 'En proceso de Liberación'  .
****          CONCATENATE 'En proceso de Liberación por' ws_reporte-libe1 ws_reporte-libe2
****                      ws_reporte-libe3 ws_reporte-libe4 ws_reporte-libe5 ws_reporte-libe6
****                      ws_reporte-libe7 INTO ws_reporte-ped_est SEPARATED BY ','."space.
****          CONDENSE ws_reporte-ped_est.
        ENDIF.
****        DO 7 TIMES.
****          lv_index =  sy-index.
****          CONCATENATE 'lv_libe'  lv_index+1(1) INTO lv_cadena.
****          ASSIGN (lv_cadena) TO <fs_campo>.
****
****          IF <fs_campo> IS INITIAL.
****            CONCATENATE 'LIBE'  lv_index+1(1) INTO lv_cadena.
****            CLEAR gs_cellcolor.
****            gs_cellcolor-fname     = lv_cadena.
****            gs_cellcolor-color-col = '1'. "Celeste
****            APPEND gs_cellcolor TO ws_reporte-cellcolor.
****            EXIT.
****          ELSE.
****            CONCATENATE 'LIBE'  lv_index+1(1) INTO lv_cadena.
****            CLEAR gs_cellcolor.
****            gs_cellcolor-fname     = lv_cadena.
****            gs_cellcolor-color-col = '5'. "Verde
****            APPEND gs_cellcolor TO ws_reporte-cellcolor.
****          ENDIF.
****        ENDDO.
      ENDIF.
    ENDIF.
  ELSE.
*     Documento rechazado
    "1 Celeste
    "2
    "3 Amarillo
    "4 Celeste claro
    "5 Verde
    "6 Rojo
    "7 Rosado
***    DO 7 TIMES.
***      lv_index =  sy-index.
***      CONCATENATE 'ws_reporte-libe'  lv_index+1(1) INTO lv_cadena.
***      ASSIGN (lv_cadena) TO <fs_campo>.
***
***      IF <fs_campo> CS lv_norec.
***        CONCATENATE 'LIBE'  lv_index+1(1) INTO lv_cadena.
***        CLEAR gs_cellcolor.
***        gs_cellcolor-fname     = lv_cadena.
***        gs_cellcolor-color-col = '6'.
***        APPEND gs_cellcolor TO ws_reporte-cellcolor.
***        EXIT.
***      ELSE.
***        CONCATENATE 'LIBE'  lv_index+1(1) INTO lv_cadena.
***        CLEAR gs_cellcolor.
***        gs_cellcolor-fname     = lv_cadena.
***        gs_cellcolor-color-col = '5'.
***        APPEND gs_cellcolor TO ws_reporte-cellcolor.
***      ENDIF.
***    ENDDO.

    ws_pendiente = 'X'.
    IF     us_tipo EQ 'S'.
*      ws_reporte-sp_esta = 'Rechazado, en bandeja del usuario creador'.
      p_error = 'X'   .
      p_msg   = 'Rechazado, en bandeja del usuario creador'.
    ELSEIF us_tipo EQ 'P'.
*      ws_reporte-ped_est = 'Rechazado, en bandeja del usuario creador'.
      p_error = 'X'   .
      p_msg   = 'Rechazado, en bandeja del usuario creador'.
    ENDIF.
  ENDIF.

ENDFORM.                    " WEB_SERVICE
*&---------------------------------------------------------------------*
*&      Form  OBTENER_DATOS_MAESTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_VSTEL  text
*----------------------------------------------------------------------*
FORM obtener_datos_maestros  USING    s_vstel.
*                                                                       "Begin NETSOL-CCH03092015+
  SELECT  * INTO TABLE  ti_escenario    FROM  znstb_escenario
    WHERE vstel EQ  s_vstel .

  SELECT  * INTO TABLE  ti_evento       FROM  znstb_evento
    WHERE vstel EQ  s_vstel .

  SELECT  * INTO TABLE  ti_material     FROM  znstb_material
    WHERE vstel EQ  s_vstel .

  SELECT  * INTO TABLE  ti_secuencia    FROM  znstb_secuencia
    WHERE vstel EQ  s_vstel .

  SELECT  * INTO TABLE  ti_tranquera    FROM  znstb_tranquera
    WHERE vstel EQ  s_vstel .

  SELECT  * INTO TABLE  ti_trfid        FROM  znstb_trfid
    WHERE vstel EQ  s_vstel .

  SELECT  * INTO TABLE  ti_turnos       FROM  znstb_turnos
    WHERE vstel EQ  s_vstel .

  SELECT  * INTO TABLE  ti_zona_des     FROM  znstb_zona_des
    WHERE vstel EQ  s_vstel .

  SELECT  * INTO TABLE  ti_tvstt        FROM  tvstt
    WHERE spras EQ sy-langu
      AND vstel EQ s_vstel .
*                                                                       "END NETSOL-CCH03092015+
ENDFORM.                    " OBTENER_DATOS_MAESTROS
*&---------------------------------------------------------------------*
*&      Form  OBTENER_DESCRIPCIONES_MAESTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_VSTEL  text
*----------------------------------------------------------------------*
FORM obtener_descripciones_maestros  USING p_escenario_id
                                           p_evento_id
                                           p_material_id
                                           p_tranquera_id
                                           p_turno_id
                                           p_zona_id
                                           p_vstel_id
                                     CHANGING p_escenario_des
                                              p_evento_des
                                              p_material_des
                                              p_tranquera_des
                                              p_hora_ini
                                              p_hora_fin
                                              p_zona_des
                                              p_vstel_des    .


  DATA:   lw_escenario TYPE znstb_escenario,
          lw_evento    TYPE znstb_evento,
          lw_material  TYPE znstb_material,
          lw_secuencia TYPE znstb_secuencia,
          lw_tranquera TYPE znstb_tranquera,
          lw_turnos    TYPE znstb_turnos,
          lw_zona_des  TYPE znstb_zona_des,
          lw_zonamat   TYPE znstb_zonamat,
          lw_trfid     TYPE znstb_trfid,
          lw_tvstt     TYPE tvstt.

  IF p_escenario_id IS NOT INITIAL .
    READ TABLE ti_escenario INTO lw_escenario WITH KEY escenario_id = p_escenario_id  .
    IF sy-subrc EQ 0  .
      p_escenario_des = lw_escenario-descripcion  .
    ENDIF.
  ENDIF.

  IF p_evento_id  IS NOT INITIAL .
    READ TABLE ti_evento  INTO lw_evento WITH KEY evento_id = p_evento_id  .
    IF sy-subrc EQ 0  .
      p_evento_des = lw_evento-descripcion  .
    ENDIF.
  ENDIF.

  IF p_material_id   IS NOT INITIAL .
    READ TABLE ti_material  INTO lw_material WITH KEY material_id = p_material_id  .
    IF sy-subrc EQ 0  .
      p_material_des = lw_material-descrip_mat  .
    ENDIF.
  ENDIF.

  IF p_tranquera_id IS NOT INITIAL .
    READ TABLE ti_tranquera  INTO lw_tranquera WITH KEY tranq_id = p_tranquera_id   .
    IF sy-subrc EQ 0  .
      p_tranquera_des = lw_tranquera-desc_tranq   .
    ENDIF .
  ENDIF.

  IF p_turno_id  IS NOT INITIAL . .
    READ TABLE ti_turnos  INTO lw_turnos WITH KEY turno_id = p_turno_id   .
    IF sy-subrc EQ 0  .
      p_hora_ini = lw_turnos-hora_ini   .
      p_hora_fin = lw_turnos-hora_fin   .
    ENDIF .
  ENDIF.

  IF p_zona_id   IS NOT INITIAL . .
    READ TABLE ti_zona_des  INTO lw_zona_des WITH KEY zona_id = p_zona_id   .
    IF sy-subrc EQ 0  .
      p_zona_des = lw_zona_des-descripcion   .
    ENDIF .
  ENDIF.

  IF p_vstel_id  IS NOT INITIAL .
    READ TABLE ti_tvstt  INTO lw_tvstt WITH KEY vstel = p_vstel_id   .
    IF sy-subrc EQ 0  .
      p_zona_des = lw_tvstt-vtext   .
    ENDIF .
  ENDIF.


ENDFORM.                    " OBTENER_DESCRIPCIONES_MAESTROS
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_DATOS_ENFLUJO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_CHOFER_DNI  text
*      -->P_L_CHOFER_DNI  text
*      -->P_L_CARRETA  text
*      <--P_L_ERROR  text
*      <--P_L_MSG  text
*----------------------------------------------------------------------*
FORM validar_datos_enflujo  USING    p_chofer_dni
                                     p_tracto
                                     p_carreta
                            CHANGING p_error
                                     p_msg.

  DATA  : lw_flujoreccab  TYPE znstbflujoreccab .

  SELECT SINGLE * INTO lw_flujoreccab FROM znstbflujoreccab
    WHERE chofer_dni EQ p_chofer_dni
      AND evento_id  NE '0600'
      AND fdate      EQ sy-datum  .
  IF sy-subrc EQ 0  .
    p_error  = 'X' .
    CONCATENATE text-m85 p_chofer_dni  text-m78 INTO p_msg SEPARATED BY  space .
  ENDIF.

  SELECT SINGLE * INTO lw_flujoreccab FROM znstbflujoreccab
    WHERE tracto_pl EQ p_tracto
      AND evento_id  NE '0600'
      AND fdate      EQ sy-datum  .
  IF sy-subrc EQ 0  .
    p_error  = 'X' .
    CONCATENATE text-m86 p_tracto text-m78 INTO p_msg SEPARATED BY  space .
  ENDIF.
*BB NETSOL-CCCE06032017-
*  SELECT SINGLE * INTO lw_flujoreccab FROM znstbflujoreccab
*    WHERE carret_pl EQ p_carreta
*      AND evento_id  NE '0600'
*      AND fdate      EQ sy-datum  .
*  IF sy-subrc EQ 0  .
*    p_error  = 'X' .
*    CONCATENATE text-m86 p_carreta text-m78 INTO p_msg SEPARATED BY  space .
*  ENDIF.
*BE NETSOL-CCCE06032017-
ENDFORM.                    " VALIDAR_DATOS_ENFLUJO
*&---------------------------------------------------------------------*
*&      Form  VALIDACION_INFO_GR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_LIKP  text
*      <--P_L_ERROR  text
*      <--P_L_MSG  text
*----------------------------------------------------------------------*
FORM validacion_info_gr  TABLES   p_ti_likp STRUCTURE likp
                         CHANGING p_error
                                  p_msg.

  DATA: lv_aux1 TYPE likp-xblnr,
        lv_aux2 TYPE likp-xblnr,
        lv_aux3 TYPE likp-xblnr.

  LOOP AT p_ti_likp .

    IF p_ti_likp-xblnr EQ '0000000000000000' .
      p_error = 'X' .
      CONCATENATE text-m79 ':' p_ti_likp-xblnr  text-m80 INTO p_msg
        SEPARATED BY space   .
      EXIT  .
    ENDIF.

    IF p_ti_likp-xblnr EQ space .
      p_error = 'X' .
      CONCATENATE text-m79 text-m81 INTO p_msg SEPARATED BY space.
      EXIT  .
    ENDIF.

    SPLIT p_ti_likp-xblnr AT  '-' INTO lv_aux1 lv_aux2 lv_aux3  .

    IF lv_aux1 EQ space  .
      p_error = 'X' .
      CONCATENATE text-m79 p_ti_likp-xblnr text-m81 INTO p_msg SEPARATED BY space.
      EXIT  .
    ENDIF.

    IF lv_aux2 EQ space  .
      p_error = 'X' .
      CONCATENATE text-m79 p_ti_likp-xblnr text-m81 INTO p_msg SEPARATED BY space.
      EXIT  .
    ENDIF.

    IF lv_aux3 EQ space  .
      p_error = 'X' .
      CONCATENATE text-m79 p_ti_likp-xblnr text-m81 INTO p_msg SEPARATED BY space.
      EXIT  .
    ENDIF.

  ENDLOOP.

ENDFORM.                    " VALIDACION_INFO_GR
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_UM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_FLUJORECCAB  text
*      -->P_TI_FLUJORECDET  text
*      <--P_L_MSG  text
*      <--P_L_ERROR  text
*----------------------------------------------------------------------*
FORM conversion_um  TABLES   p_ti_flujoreccab STRUCTURE znstbflujoreccab
                             p_ti_flujorecdet STRUCTURE znstbflujorecdet
                    CHANGING p_msg
                             p_error  .

  DATA: l_peso_gr TYPE plfh-mgvgw,
        l_peso_in TYPE plfh-mgvgw,
        l_peso_fi TYPE plfh-mgvgw,
        l_peso_ne TYPE plfh-mgvgw.

*  READ TABLE p_ti_flujoreccab INDEX 1   .
  LOOP AT p_ti_flujoreccab  .
    IF p_ti_flujoreccab-meins NE 'KG'  .
      IF p_ti_flujoreccab-peso_gr IS NOT INITIAL .
        PERFORM cambiar_medida USING 'KG' p_ti_flujoreccab-meins l_peso_gr p_ti_flujoreccab-peso_gr.
        p_ti_flujoreccab-meins    = 'KG'  .
        p_ti_flujoreccab-peso_gr  = l_peso_gr  .
      ENDIF.
      IF p_ti_flujoreccab-peso_in IS NOT INITIAL .
        PERFORM cambiar_medida USING 'KG' p_ti_flujoreccab-meins l_peso_in p_ti_flujoreccab-peso_in.
        p_ti_flujoreccab-meins    = 'KG'  .
        p_ti_flujoreccab-peso_in  = l_peso_in  .
      ENDIF.
      IF p_ti_flujoreccab-peso_fi IS NOT INITIAL .
        PERFORM cambiar_medida USING 'KG' p_ti_flujoreccab-meins l_peso_fi p_ti_flujoreccab-peso_fi.
        p_ti_flujoreccab-meins    = 'KG'  .
        p_ti_flujoreccab-peso_fi  = l_peso_fi  .
      ENDIF.
      IF p_ti_flujoreccab-peso_ne IS NOT INITIAL .
        PERFORM cambiar_medida USING 'KG' p_ti_flujoreccab-meins l_peso_ne p_ti_flujoreccab-peso_ne.
        p_ti_flujoreccab-meins    = 'KG'  .
        p_ti_flujoreccab-peso_ne  = l_peso_ne  .
      ENDIF.

      MODIFY  p_ti_flujoreccab .

      LOOP AT p_ti_flujorecdet WHERE tknum_id EQ p_ti_flujoreccab-tknum_id   .
        p_ti_flujorecdet-peso_gr  = p_ti_flujoreccab-peso_gr  .
        p_ti_flujorecdet-meins    = p_ti_flujoreccab-meins  .
        MODIFY  p_ti_flujorecdet  .                                     "NETSOL-CCH04092015+
      ENDLOOP.
    ENDIF .

  ENDLOOP .

ENDFORM.                    " CONVERSION_UM
*&---------------------------------------------------------------------*
*&      Form  GUARDAR_DATOS_SALIDA3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM guardar_datos_salida3  TABLES  p_t_data_cab STRUCTURE tab512"ztab256 "NETSOL-CCH04092015
                                    p_t_data_det STRUCTURE tab512"ztab256 "NETSOL-CCH04092015
                                    p_t_flujoreccab STRUCTURE znstbflujoreccab
                                    p_t_flujorecdet STRUCTURE znstbflujorecdet
                           USING    p_flag_dt
                                    p_fecha_act
                                    p_hora_act
                                    p_tipo_p
                                    p_provee_id
                                    p_provee_des
                                    p_turno_id
                                    p_dti_id
                                    p_tknum
                                    p_material
                                    p_um
                                    p_transp_ruc
                                    p_transp_des
                                    p_chofer_dni
                                    p_chofer_des
                                    p_chofer_bvt
                                    p_tracto
                                    p_carreta
                                    p_ticket
                                    p_trfid_id
                                    p_trfid_nro
                                    p_color
                                    p_destin_id
                                    p_destin_des
                                    p_ruta                              "NETSOL-CCCE13032017+
                                    p_separador.

  DATA: lv_fecha_txt    TYPE string,
        lv_hora_txt     TYPE string,
*        lv_concat       TYPE string ,                                  "NETSOL-CCH04092015-
        lv_ebeln        TYPE ekko-ebeln,
        lv_vbeln_vl     TYPE lips-vbeln,
        lv_material_des TYPE makt-maktx,
        lv_canti_txt    TYPE char20,
        lv_item         TYPE ekpo-ebelp,
        lv_gremi        TYPE likp-xblnr,
        lv_um           TYPE lips-meins,
        lv_cant_total   TYPE lips-lfimg,                                "NETSOL-CCH31072015+
        lv_cant         TYPE lips-lfimg,                                "NETSOL-CCH31072015+
        lv_peso_in_txt  TYPE char20,
        lv_peso_gr_txt  TYPE char20,
        lv_peso_fi_txt  TYPE char20,
        lv_peso_ne_txt  TYPE char20,
        lv_observacion  TYPE char50,                                    "NETSOL-CCCE17032017+
*        lv_peso_in_txt  TYPE String ,
*        lv_peso_gr_txt  TYPE String ,
*        lv_peso_fi_txt  TYPE String ,
*        lv_peso_ne_txt  TYPE String ,
        lw_homolog      TYPE ztmmhomolog,                                    "NETSOL-CCCE17032017+
        lw_tvswz        TYPE tvswz,
        lw_t001w        TYPE t001w,
        lw_t001k        TYPE t001k  ,                                   "NETSOL-CCH05082015+
        lw_t001         TYPE t001  ,                                    "NETSOL-CCH05082015+
        lw_zona_des     TYPE znstb_zona_des ,                           "NETSOL-CCH07082015+
        lv_material     TYPE mara-matnr.
  DATA: lv_concat(500)  TYPE c  .                                       "NETSOL-CCH04092015+
  DATA: lv_peso              TYPE p DECIMALS 3 ,                             "NETSOL-CCH17092015+
        lv_fecha_hora_in_txt TYPE string ,                             "NETSOL-CCH17092015+
        lv_fecha_hora_fi_txt TYPE string ,                             "NETSOL-CCH17092015+
        lw_turno             TYPE znstb_turnos ,                             "NETSOL-CCH17092015+
        lv_hora_ini_txt      TYPE string ,                                   "NETSOL-CCH17092015+
        lv_hora_fin_txt      TYPE string ,                                   "NETSOL-CCH17092015+
        lv_turno_hora_txt    TYPE string .                                 "NETSOL-CCH17092015+
* formato fecha.
  CONCATENATE p_fecha_act+4(2) '/' p_fecha_act+6(2) '/' p_fecha_act+0(4)
    INTO lv_fecha_txt .

  CONCATENATE p_hora_act+0(2) ':' p_hora_act+2(2) ':'  p_hora_act+4(2)
    INTO lv_hora_txt  .

* unidad de medida.
  lv_um = p_um  .

* descripción de material.
  SELECT SINGLE maktx INTO lv_material_des FROM makt
  WHERE matnr = p_material
    AND spras = sy-langu  .

* formato material.
  PERFORM conversion_exit_matn1_output USING p_material
                                       CHANGING p_material .

* pesos.
  READ TABLE p_t_flujoreccab INDEX 1  .
*  lv_peso_gr_txt  = p_t_flujoreccab-peso_gr .                          "NETSOL-CCH17092015-
*  lv_peso_in_txt  = p_t_flujoreccab-peso_in .                          "NETSOL-CCH17092015-
*  lv_peso_fi_txt  = p_t_flujoreccab-peso_fi .                          "NETSOL-CCH17092015-
*  lv_peso_ne_txt  = p_t_flujoreccab-peso_ne .                          "NETSOL-CCH17092015-
  lv_peso         =         p_t_flujoreccab-peso_gr .                  "NETSOL-CCH17092015+
  WRITE lv_peso  TO lv_peso_gr_txt   .                                 "NETSOL-CCH17092015+
  lv_peso         =         p_t_flujoreccab-peso_in .                  "NETSOL-CCH17092015+
  WRITE lv_peso  TO lv_peso_in_txt   .                                 "NETSOL-CCH17092015+
  lv_peso         =         p_t_flujoreccab-peso_fi .                  "NETSOL-CCH17092015+
  WRITE lv_peso  TO lv_peso_fi_txt   .                                 "NETSOL-CCH17092015+
  lv_peso         =         p_t_flujoreccab-peso_ne .                  "NETSOL-CCH17092015+
  WRITE lv_peso  TO lv_peso_ne_txt   .                                 "NETSOL-CCH17092015+

  CONDENSE: lv_peso_gr_txt  NO-GAPS,
            lv_peso_in_txt  NO-GAPS,
            lv_peso_fi_txt  NO-GAPS,
            lv_peso_ne_txt  NO-GAPS.
*BB NETSOL-CCCE17032017-
* material - homologación - Campo observación
  CLEAR: lw_homolog, lv_observacion.
  SELECT SINGLE * INTO lw_homolog FROM ztmmhomolog
    WHERE material_balanza  = p_t_flujoreccab-material_balanza.
  IF sy-subrc = 0.
    CASE lw_homolog-peso_base_mp.
      WHEN 'M'.
        IF p_t_flujoreccab-peso_ne LE p_t_flujoreccab-peso_gr.
          CONCATENATE text-m95 lv_peso_ne_txt lv_um
            INTO lv_observacion SEPARATED BY space.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
*BE NETSOL-CCCE17032017-
* empresa (centro)
  CLEAR:  lw_tvswz        .
*  SELECT SINGLE * INTO lw_tvswz FROM tvswz                             "NETSOL-CCCE12012017-
*    WHERE vstel = p_t_flujoreccab-vstel .                              "NETSOL-CCCE12012017-
*BB NETSOL-CCCE12012017+
  READ TABLE p_t_flujorecdet INDEX 1.
* Determinación de centro.
  CLEAR: lw_tvswz-werks.
  IF p_t_flujorecdet-ebeln IS NOT INITIAL.
    SELECT SINGLE werks INTO lw_tvswz-werks FROM ekpo
      WHERE ebeln EQ p_t_flujorecdet-ebeln
        AND matnr EQ p_t_flujoreccab-matnr.
  ELSE.
    lw_tvswz-werks = p_t_flujoreccab-cenha.
  ENDIF.
*BE NETSOL-CCCE12012017+
*  descripcion de centro
  CLEAR:  lw_t001w  .
  SELECT SINGLE * INTO lw_t001w FROM t001w
    WHERE werks = lw_tvswz-werks  .
*  Sociedad                                                             "Begin NETSOL-CCH05082015+
  CLEAR:  lw_t001k ,  lw_t001 .
  SELECT SINGLE * INTO lw_t001k FROM t001k
    WHERE bwkey = lw_tvswz-werks  .
  SELECT SINGLE * INTO lw_t001 FROM t001
    WHERE bukrs = lw_t001k-bukrs  .
*                                                                       "End NETSOL-CCH05082015+
*  zona de descarga - descripción                                       "Begin NETSOL-CCH07082015+
  SELECT SINGLE * INTO lw_zona_des FROM znstb_zona_des
    WHERE zona_id  = p_t_flujoreccab-zona_id
     AND  vstel    = p_t_flujoreccab-vstel .
*                                                                       "End NETSOL-CCH07082015+
  CONCATENATE p_tipo_p
*              lw_t001w-name1                                           "NETSOL-CCH05082015-
              lw_t001-butxt                                             "NETSOL-CCH05082015+
              p_provee_id
              p_provee_des
              lv_fecha_txt
              lv_hora_txt
              p_turno_id
              p_dti_id
              p_tknum
              lv_peso_gr_txt
              lv_peso_in_txt
              lv_peso_fi_txt
              lv_peso_ne_txt
  INTO lv_concat SEPARATED BY p_separador  .                            "NETSOL-CCH04092015+
  APPEND lv_concat TO p_t_data_cab .                                    "NETSOL-CCH04092015+
  CLEAR lv_concat .                                                     "NETSOL-CCH04092015+
*              p_t_flujoreccab-zona_id                                  "NETSOL-CCH07082015-
  CONCATENATE                                                           "NETSOL-CCH04092015+
              lw_zona_des-descripcion                                   "NETSOL-CCH07082015+
              p_transp_ruc
              p_transp_des
              p_chofer_dni
              p_chofer_des
              p_chofer_bvt
              p_tracto
              p_carreta
              p_ticket
              p_trfid_id
              p_trfid_nro
              p_color
              p_ruta                                                    "NETSOL-CCCE13032017+
*  INTO l_concat SEPARATED BY p_separador  .                            "NETSOL-CCH04092015-
*  APPEND l_concat TO p_t_data_cab .                                    "NETSOL-CCH04092015-
  INTO lv_concat SEPARATED BY p_separador  .                            "NETSOL-CCH04092015+
  APPEND lv_concat TO p_t_data_cab .                                    "NETSOL-CCH04092015+

**----------------------------------------------------------------------*
** Estructa de datos de posición
**----------------------------------------------------------------------*
  REFRESH : p_t_flujorecdet .
  SELECT * INTO TABLE p_t_flujorecdet FROM znstbflujorecdet
    WHERE tknum_id  EQ  p_t_flujoreccab-tknum_id
      AND vstel     EQ  p_t_flujoreccab-vstel .
  IF sy-subrc EQ 0  .
    LOOP AT p_t_flujorecdet .
*     Fecha hora ingreso
      IF p_t_flujorecdet-evento_id  EQ '0300' .
        IF p_t_flujorecdet-ldate IS NOT INITIAL .
          CONCATENATE p_t_flujorecdet-ldate+6(2) '/' p_t_flujorecdet-ldate+4(2) '/' p_t_flujorecdet-ldate+0(4)
            INTO lv_fecha_txt  .

          CONCATENATE p_t_flujorecdet-ltime+0(2) ':' p_t_flujorecdet-ltime+2(2) ':' p_t_flujorecdet-ltime+4(2)
            INTO lv_hora_txt  .

          CONCATENATE lv_fecha_txt lv_hora_txt INTO lv_fecha_hora_in_txt SEPARATED BY space  .
        ELSEIF  p_t_flujorecdet-fdate IS NOT INITIAL .
          CONCATENATE p_t_flujorecdet-fdate+6(2) '/' p_t_flujorecdet-fdate+4(2) '/' p_t_flujorecdet-fdate+0(4)
            INTO lv_fecha_txt  .

          CONCATENATE p_t_flujorecdet-ftime+0(2) ':' p_t_flujorecdet-ftime+2(2) ':' p_t_flujorecdet-ftime+4(2)
            INTO lv_hora_txt  .

          CONCATENATE lv_fecha_txt lv_hora_txt INTO lv_fecha_hora_in_txt SEPARATED BY space  .
        ENDIF.
      ENDIF.
*     Fecha hora salida
      IF p_t_flujorecdet-evento_id  EQ '0600' .
        IF p_t_flujorecdet-ldate IS NOT INITIAL .
          CONCATENATE p_t_flujorecdet-ldate+6(2) '/' p_t_flujorecdet-ldate+4(2) '/' p_t_flujorecdet-ldate+0(4)
            INTO lv_fecha_txt  .

          CONCATENATE p_t_flujorecdet-ltime+0(2) ':' p_t_flujorecdet-ltime+2(2) ':' p_t_flujorecdet-ltime+4(2)
            INTO lv_hora_txt  .

          CONCATENATE lv_fecha_txt lv_hora_txt INTO lv_fecha_hora_fi_txt SEPARATED BY space  .
        ELSEIF  p_t_flujorecdet-fdate IS NOT INITIAL .
          CONCATENATE p_t_flujorecdet-fdate+6(2) '/' p_t_flujorecdet-fdate+4(2) '/' p_t_flujorecdet-fdate+0(4)
            INTO lv_fecha_txt  .

          CONCATENATE p_t_flujorecdet-ftime+0(2) ':' p_t_flujorecdet-ftime+2(2) ':' p_t_flujorecdet-ftime+4(2)
            INTO lv_hora_txt  .

          CONCATENATE lv_fecha_txt lv_hora_txt INTO lv_fecha_hora_fi_txt SEPARATED BY space  .
        ENDIF.
*       Guia de remisión.
        lv_gremi  = p_t_flujorecdet-gr  .
      ENDIF.

    ENDLOOP.

*   Turno
    SELECT SINGLE * INTO lw_turno  FROM znstb_turnos
      WHERE vstel     EQ  p_t_flujoreccab-vstel
        AND turno_id  EQ  p_t_flujoreccab-turno_id  .
    IF sy-subrc EQ 0.
      CONCATENATE lw_turno-hora_ini+0(2) ':' lw_turno-hora_ini+2(2) ':' lw_turno-hora_ini+4(2)
        INTO lv_hora_ini_txt  .

      CONCATENATE lw_turno-hora_fin+0(2) ':' lw_turno-hora_fin+2(2) ':' lw_turno-hora_fin+4(2)
        INTO lv_hora_fin_txt  .

      CONCATENATE p_t_flujoreccab-turno_id  lv_hora_ini_txt '-' lv_hora_fin_txt INTO lv_turno_hora_txt
        SEPARATED BY space  .
    ENDIF.


    CONCATENATE
              lv_fecha_hora_in_txt
              lv_fecha_hora_fi_txt
              lv_turno_hora_txt
              lv_gremi
              p_material
              lv_material_des
              p_t_flujorecdet-ebeln                                     "NETSOL-CCCE13032017+
              p_t_flujorecdet-meins                                     "NETSOL-CCCE13032017+
              p_destin_id                                               "NETSOL-CCCE13032017+
              p_destin_des                                              "NETSOL-CCCE13032017+
              lv_observacion                                            "NETSOL-CCCE17032017+
    INTO lv_concat SEPARATED BY p_separador  .
    APPEND lv_concat TO p_t_data_cab .

  ENDIF .

ENDFORM.                    " GUARDAR_DATOS_SALIDA3
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_ZDESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_ZONA_ID  text
*      <--P_W_ZONA_DES  text
*----------------------------------------------------------------------*
FORM validar_zdesc  USING    p_zona_id
                    CHANGING s_zona_des.

  CLEAR:  s_zona_des  .

  SELECT SINGLE * INTO w_zona_des FROM znstb_zona_des
    CLIENT SPECIFIED
    WHERE mandt   EQ  sy-mandt
      AND zona_id EQ  p_zona_id
    .

ENDFORM.                    " VALIDAR_ZDESC
*&---------------------------------------------------------------------*
*&      Form  OBTENER_PROGRAMACION_ZDESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_PROGRAMAC  text
*      -->P_I_VSTEL  text
*      -->P_I_ZONA_ID  text
*      -->P_I_FECHA  text
*----------------------------------------------------------------------*
FORM obtener_programacion_zdesc  TABLES   p_t_programac STRUCTURE znstb_programac
                                 USING    p_vstel
                                          p_zona_id
                                          p_fecha.

  REFRESH : p_t_programac .

  IF p_zona_id IS INITIAL .

    SELECT * INTO TABLE  p_t_programac  FROM znstb_programac
      WHERE vstel EQ  p_vstel
        AND fecha EQ  p_fecha .

  ELSE.

    SELECT * INTO TABLE  p_t_programac  FROM znstb_programac
     WHERE vstel     EQ  p_vstel
       AND zona_id   EQ  p_zona_id
       AND fecha     EQ  p_fecha .

  ENDIF.

ENDFORM.                    " OBTENER_PROGRAMACION_ZDESC
*&---------------------------------------------------------------------*
*&      Form  OBTENER_TURNOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_TURNOS  text
*      -->P_I_VSTEL  text
*----------------------------------------------------------------------*
FORM obtener_turnos  TABLES   p_ti_turnos STRUCTURE znstb_turnos
                     USING    p_vstel.

  REFRESH  :  p_ti_turnos  .

  SELECT * INTO TABLE p_ti_turnos  FROM znstb_turnos
    WHERE  vstel  EQ  p_vstel  .

ENDFORM.                    " OBTENER_TURNOS

*&---------------------------------------------------------------------*
*&      Form  ENVIA_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_VSTEL  text
*      -->P_L_TKNUM  text
*      -->P_L_TRACTO  text
*      -->P_L_CARRETA  text
*      -->P_L_TIPO_P  text
*      -->P_L_PROVEE_ID  text
*      -->P_L_DESTIN_ID  text
*      -->P_L_CANT_GR_TOT  text
*      -->P_L_UM  text
*----------------------------------------------------------------------*
FORM envia_mail   USING    p_vstel
                           p_tknum
                           p_tracto
                           p_carreta
                           p_transp_ruc
                           p_cant_gr_tot
                           p_um
                           p_chofer_des
                           p_material
                           p_transp_id
                           p_transp_des
                           p_chofer_dni
                           p_provee_id                                  "NETSOL-CCCE05042017+
                           p_provee_des                                 "NETSOL-CCCE05042017+
                           p_dti_id                                     "NETSOL-CCCE05042017+
                           p_gremi.                                     "NETSOL-CCCE04052017+

  DATA:  gt_mail        TYPE TABLE OF zgetdconst-valor2,
         lv_cant_gr_tot TYPE char20,
         lv_valor1      LIKE zgetdconst-valor1,
         lv_ctrl_qas    TYPE c,
         lv_vstel       LIKE likp-vstel,
         lv_lifnr       LIKE lfa1-lifnr,
         lv_matnr       LIKE lips-matnr,
         gwa_mail       LIKE LINE OF gt_mail.

  DATA: lt_zgetdconst  TYPE TABLE OF zgetdconst,
        lwa_zgetdconst TYPE zgetdconst.

  SELECT campo valor1 valor2
    INTO CORRESPONDING FIELDS OF TABLE lt_zgetdconst
    FROM zgetdconst
   WHERE modulo  EQ c_modulo
     AND proyec  EQ c_proyec
     AND aplica  EQ c_calidad.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
*BB NETSOL-CCCE05042017+
      input  = p_provee_id
    IMPORTING
      output = p_provee_id.
*BE NETSOL-CCCE05042017+
*BB NETSOL-CCCE05042017-
*      input  = p_transp_id
*    IMPORTING
*      output = p_transp_id.
*BE NETSOL-CCCE05042017-
  lv_ctrl_qas = space.

  LOOP AT lt_zgetdconst INTO lwa_zgetdconst.
    CASE lwa_zgetdconst-campo.
      WHEN 'MAIL'.   "Si el puesto de expedicion tiene activo el control de calidad envia mail
        IF p_vstel = lwa_zgetdconst-valor1+0(4).
          APPEND lwa_zgetdconst-valor2 TO gt_mail.
        ENDIF.
      WHEN 'CTRL_DT'. "Si para Psto.Exp. + Proveedor + Material esta activo el control de calidad
        lv_valor1 = lwa_zgetdconst-valor1.
        SPLIT lv_valor1 AT v_separador INTO lv_vstel lv_lifnr lv_matnr.
*        IF lv_vstel = p_vstel AND lv_lifnr = p_transp_id AND lv_matnr = p_material."NETSOL-CCCE05042017-
        IF lv_vstel = p_vstel AND lv_lifnr = p_provee_id AND lv_matnr = p_material. "NETSOL-CCCE05042017+
          lv_ctrl_qas = 'X'.
        ELSEIF lv_vstel = p_vstel AND lv_lifnr = p_provee_id AND lv_matnr = '*' .   "NETSOL-CCCE05042017+
          lv_ctrl_qas = 'X'.                                                        "NETSOL-CCCE05042017+
        ELSEIF lv_vstel = p_vstel AND lv_lifnr = '*' AND lv_matnr = p_material .    "NETSOL-CCCE05042018+
          lv_ctrl_qas = 'X'.                                                        "NETSOL-CCCE05042018+
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  CHECK lv_ctrl_qas = 'X'.

  DATA: it_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        it_contents     LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        it_receivers    LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
        it_attachment   LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        gd_cnt          TYPE i,
        gd_sent_all(1)  TYPE c,
        gd_doc_data     LIKE sodocchgi1,
        gd_error        TYPE sy-subrc.

  DATA: it_message TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
                   WITH HEADER LINE.

  gd_doc_data-obj_langu  = sy-langu.
  gd_doc_data-obj_name   = 'SAPRPT'.
  gd_doc_data-obj_descr  = text-t01.
  gd_doc_data-sensitivty = 'F'.

  FREE it_message.

  it_message-line = 'Estimados: Control de Calidad'.
  APPEND it_message.
  it_message-line = ' '. APPEND it_message.
  CONCATENATE 'El vehículo con placa: ' p_tracto
              ', ha sido seleccionado para pasar por el control de calidad.'
         INTO it_message-line SEPARATED BY space.
  APPEND it_message.
  it_message-line = ' '. APPEND it_message.

  WRITE p_cant_gr_tot TO lv_cant_gr_tot.
  CONDENSE: lv_cant_gr_tot.                                             "NETSOL-CCCE05042017+
*BB NETSOL-CCCE05042017-
*  it_message-line = text-t03.
*  REPLACE '&' WITH p_transp_ruc  INTO it_message-line.
*  REPLACE '&&' WITH p_transp_des  INTO it_message-line.
*  APPEND it_message.
*BE NETSOL-CCCE05042017-
*BB NETSOL-CCCE05042017+
  it_message-line = text-t10.
  REPLACE '&' WITH p_provee_id  INTO it_message-line.
  REPLACE '&&' WITH p_provee_des  INTO it_message-line.
  APPEND it_message.
*BB NETSOL-CCCE04052017+
  it_message-line = text-t11.
  REPLACE '&' WITH p_gremi  INTO it_message-line.
  APPEND it_message.
*BE NETSOL-CCCE04052017+
  it_message-line = text-t09.
  REPLACE '&' WITH p_dti_id  INTO it_message-line.
  APPEND it_message.
*BE NETSOL-CCCE05042017+
  it_message-line = text-t02.
  REPLACE '&' WITH p_tknum INTO it_message-line.
  APPEND it_message.

  it_message-line = text-t07.
  REPLACE '&' WITH p_chofer_dni INTO it_message-line.
  REPLACE '&&' WITH p_chofer_des INTO it_message-line.
  APPEND it_message.

  it_message-line = text-t04.
  TABLES makt.
  CLEAR makt-maktg.
  DATA pout_material LIKE mara-matnr.
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = p_material
    IMPORTING
      output       = pout_material
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

  SELECT SINGLE maktg INTO makt-maktg FROM makt
       WHERE matnr = pout_material
         AND spras = sy-langu.

  REPLACE '&' WITH p_material INTO it_message-line.
  REPLACE '&&' WITH makt-maktg INTO it_message-line.
  APPEND it_message.

  it_message-line = text-t05.
  REPLACE '&' WITH lv_cant_gr_tot INTO it_message-line.
  REPLACE '&&' WITH p_um INTO it_message-line.
  APPEND it_message.

  it_message-line = text-t06.
  REPLACE '&' WITH p_tracto INTO it_message-line.
  APPEND it_message.

  it_message-line = text-t08.
  REPLACE '&' WITH p_carreta INTO it_message-line.
  APPEND it_message.

  it_message-line = ' '. APPEND it_message.
  it_message-line = 'Atte.'. APPEND it_message.
  it_message-line = ' '. APPEND it_message.

* Descripción del cuerpo del mensaje
  CLEAR it_packing_list.
  REFRESH it_packing_list.
  it_packing_list-transf_bin = space.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 0.
  it_packing_list-body_start = 1.
  DESCRIBE TABLE it_message LINES it_packing_list-body_num.
  it_packing_list-doc_type = 'RAW'.
  APPEND it_packing_list.

* Lista de correos
  CLEAR it_receivers.
  REFRESH it_receivers.

  LOOP AT gt_mail INTO gwa_mail.
    it_receivers-receiver = gwa_mail.
    it_receivers-rec_type = 'U'.
    it_receivers-com_type = 'INT'.
    it_receivers-notif_del = 'X'.
    it_receivers-notif_ndel = 'X'.
    APPEND it_receivers.
  ENDLOOP.

  CHECK NOT it_receivers[] IS INITIAL.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = gd_doc_data
      put_in_outbox              = 'X'
      commit_work                = 'X'
    IMPORTING
      sent_to_all                = gd_sent_all
    TABLES
      packing_list               = it_packing_list
      contents_txt               = it_message
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

*  gd_error = sy-subrc.

*  WAIT UP TO 2 SECONDS.


*  IF gd_error EQ 0.
*    SUBMIT rsconn01 WITH mode = 'INT'
*                    WITH output = ' '
*                    AND RETURN.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OBTENER_VSTEL_ORIG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM obtener_vstel_orig  USING    p_ti_ekpo TYPE ekpo_tty
                         CHANGING p_vstel.

  DATA: lw_ekpv TYPE ekpv,
        lw_ekpo TYPE ekpo.

  CLEAR: p_vstel.

  READ TABLE p_ti_ekpo INTO lw_ekpo INDEX 1.

  SELECT SINGLE * INTO lw_ekpv FROM ekpv
    WHERE ebeln EQ lw_ekpo-ebeln
      AND ebelp EQ lw_ekpo-ebelp.
  IF sy-subrc = 0.

    p_vstel = lw_ekpv-vstel.

  ENDIF.

ENDFORM.
