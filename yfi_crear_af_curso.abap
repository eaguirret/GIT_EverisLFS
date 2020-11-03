FUNCTION yfi_crear_af_curso.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_MBLNR) TYPE  MBLNR OPTIONAL
*"     VALUE(I_MJAHR) TYPE  MJAHR OPTIONAL
*"     VALUE(I_ZEILE) TYPE  MBLPO OPTIONAL
*"     VALUE(I_MAIL) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_MIGO) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_RP) TYPE  CHAR01 OPTIONAL
*"----------------------------------------------------------------------
*TIPO
  TYPES: BEGIN OF lty_ekpo,
           ebeln      TYPE ebeln,
           ebelp      TYPE ebelp,
           labnr      TYPE labnr,
           bukrs      TYPE bukrs,
           pstyp      TYPE pstyp,
           zzmarca    TYPE herst,
           zzmodelo   TYPE typbz,
           zzserie    TYPE serge,
           zzremision TYPE char20,
           zzpedido   TYPE ebeln,
           zzposicion TYPE char05,
           matnr      TYPE matnr,
           ean11      TYPE ean11,
           numtp      TYPE numtp,
           anln1      TYPE anln1,
           anln2      TYPE anln2,
           MBLNR     	TYPE MBLNR,
           MJAHR      TYPE MJAHR,
           ZEILE      TYPE MBLPO,
           type       TYPE bapi_mtype,
           message    TYPE bapi_msg,
           sel        TYPE char01,
         END OF lty_ekpo,
         BEGIN OF lty_constantes,
           campo  TYPE zcampo,
           valor1 TYPE zvalor1,
           valor2 TYPE zvalor2,
         END OF lty_constantes,
         BEGIN OF lty_ekkn,
           ebeln TYPE ebeln,
           ebelp TYPE ebelp,
           anln1 TYPE anln1,
           anln2 TYPE anln2,
         END OF lty_ekkn,
         BEGIN OF lty_anlkl,
           anlkl1 TYPE anlkl,
           anlkl2 TYPE anlkl,
         END OF lty_anlkl.
*TABLAS INTERNAS
  DATA: ltd_anlkl        TYPE STANDARD TABLE OF lty_anlkl,
        ltd_ekpo         TYPE STANDARD TABLE OF lty_ekpo,
        ltd_ekpo_af      TYPE STANDARD TABLE OF lty_ekpo,
        ltd_constantes   TYPE STANDARD TABLE OF lty_constantes,
        ltd_ekkn         TYPE STANDARD TABLE OF lty_ekkn,
        lwa_mensaje      TYPE bapiret2,
        ltd_extension_in TYPE STANDARD TABLE OF bapiparex,
        lwa_extension_in TYPE bapiparex,
        lwa_bapi_te_anlu TYPE bapi_te_anlu,
        lv_bukrs         TYPE bukrs,
        ltd_receivers    TYPE TABLE OF somlreci1,
        objtxt           TYPE TABLE OF solisti1,
        gv_hor           TYPE syst_uzeit,
        lv_cadena        TYPE string,
        lwa_data         TYPE sodocchgi1,
        ls_generaldata   TYPE bapi1022_feglg001,
        ls_generaldatax  TYPE bapi1022_feglg001x,
*RANGOS
        lr_bwart         TYPE RANGE OF bwart,
        lr_bwart_sm      TYPE RANGE OF bwart,
        LV_LABNR         TYPE LABNR,
        lv_upd_af        TYPE char01.
  FIELD-SYMBOLS : <ls_constantes> TYPE lty_constantes,
                  <ls_bwart>      LIKE LINE OF lr_bwart,
                  <ls_mseg>       TYPE mseg,
                  <ls_action>     TYPE any,
                  <ls_refdoc>     TYPE any,
                  <ls_ekpo>       TYPE lty_ekpo,
                  <ls_ekpo_af>    TYPE lty_ekpo,
                  <ls_ekkn>       TYPE lty_ekkn,
                  <ls_receivers>  TYPE somlreci1,
                  <ls_objtxt>     LIKE LINE OF objtxt,
                  <ls_anlkl>      TYPE lty_anlkl.

  REFRESH: ltd_anlkl, lr_bwart_sm,ltd_tmp_mseg.
*Verificar que la data este en BD
  DATA : lv_count TYPE i,
         lv_data  TYPE char01.
  WAIT UP TO 3 SECONDS.
  COMMIT WORK AND WAIT .
  WHILE lv_data IS INITIAL.
    SELECT COUNT(*) INTO lv_count FROM mkpf WHERE mblnr EQ i_mblnr
                                            AND   mjahr EQ i_mjahr.
    IF lv_count > 0.
      lv_data = 'X'.
    ENDIF.
  ENDWHILE.
  CHECK lv_data IS NOT INITIAL.
  gv_migo    = i_migo.
  IF i_mblnr IS NOT INITIAL AND i_mjahr IS NOT INITIAL.
    SELECT * FROM mseg INTO TABLE ltd_mseg[] WHERE mblnr EQ i_mblnr
                                             AND   mjahr EQ i_mjahr.
    SELECT SINGLE * FROM mkpf INTO gwa_mkpf  WHERE mblnr EQ i_mblnr
                                              AND  mjahr EQ i_mjahr.
  ENDIF.
  SELECT campo
          valor1 valor2
        FROM zgetdconst
         INTO TABLE ltd_constantes
          WHERE  modulo EQ 'MM'
             AND proyec EQ 'ACTIVO_FIJO'
             AND aplica EQ 'SAPLMIGO'
             AND ( campo EQ 'MAIL'     OR
                   campo EQ 'BWART_EM' OR
                   campo EQ 'BWART_SM' OR
                   campo EQ 'ANLKL' ).
  CHECK ltd_constantes IS NOT INITIAL.
  LOOP AT ltd_constantes ASSIGNING <ls_constantes>.
    CASE <ls_constantes>-campo.
      WHEN 'ANLKL'.
        APPEND INITIAL LINE TO ltd_anlkl ASSIGNING <ls_anlkl>.
        <ls_constantes>-valor1 = <ls_anlkl>-anlkl1.
        <ls_constantes>-valor2 = <ls_anlkl>-anlkl2.
      WHEN 'MAIL'.
      WHEN  'BWART_SM'.
        APPEND INITIAL LINE TO lr_bwart_sm ASSIGNING <ls_bwart>.
        <ls_bwart> = 'IEQ'.
        <ls_bwart>-low = <ls_constantes>-valor1.
      WHEN  'BWART_EM'.
        APPEND INITIAL LINE TO lr_bwart ASSIGNING <ls_bwart>.
        <ls_bwart> = 'IEQ'.
        <ls_bwart>-low = <ls_constantes>-valor1.
    ENDCASE.
  ENDLOOP.
*LOGICA
*(saplmigo)godynpro-detail_take  si la posicion esta seleccionada
  IF lr_bwart IS NOT INITIAL.
  LOOP AT ltd_mseg[] ASSIGNING <ls_mseg> WHERE bwart IN lr_bwart.
    lv_upd_af = 'X'.
    EXIT.
  ENDLOOP.
  CHECK lv_upd_af IS NOT INITIAL.
*Obtenemos la serie del documento que se esta contabilizando
   SELECT DISTINCT ser03~obknr ser03~mblnr ser03~mjahr ser03~zeile objk~sernr
      INTO TABLE ltd_ser03
      FROM ser03
      INNER JOIN objk ON
      objk~obknr = ser03~obknr
      FOR ALL ENTRIES IN ltd_mseg
      WHERE   mblnr EQ ltd_mseg-mblnr
        AND   mjahr EQ ltd_mseg-mjahr
        AND   zeile EQ ltd_mseg-zeile.
*Obtenemos los pedidos de las EM
  SELECT ebeln
         ebelp
         labnr
         bukrs
         pstyp
    FROM ekpo INTO TABLE ltd_ekpo
      FOR ALL ENTRIES IN ltd_mseg
       WHERE ebeln EQ ltd_mseg-ebeln
       AND   ebelp EQ ltd_mseg-ebelp
       AND   loekz EQ space.
*Actualizamos el activo fijo y envio de mail
  LOOP AT ltd_mseg ASSIGNING <ls_mseg>.
    READ TABLE ltd_ekpo ASSIGNING <ls_ekpo>
              WITH KEY ebeln = <ls_mseg>-ebeln
                       ebelp = <ls_mseg>-ebelp.
    IF sy-subrc IS INITIAL.
*Buscamos el pedido original para obtener el af.
         IF <ls_ekpo>-pstyp EQ '7'.
           IF <ls_ekpo>-labnr IS NOT INITIAL AND <ls_ekpo>-bukrs IS NOT INITIAL.
             APPEND INITIAL LINE TO ltd_ekpo_af ASSIGNING <ls_ekpo_af>.
             <ls_ekpo_af>-bukrs  = <ls_ekpo>-bukrs.
             <ls_ekpo_af>-ebeln  = <ls_ekpo>-labnr+0(10).
             <ls_ekpo_af>-ebelp  = <ls_ekpo>-labnr+11(5).

           ELSE.

"EMAT - Enrique Aguirre - 03.11.2020 -  107337

  CHECK ltd_constantes IS NOT INITIAL .
*CREAMOS CUERPO DEL  CORREO
  gv_hor = sy-uzeit.
  IF gv_hor(2) GE 24 AND gv_hor(2) LE 12.
    lv_cadena = 'Buenos días,'.
  ELSEIF gv_hor(2) GE 12 AND gv_hor(2) LE 18.
    lv_cadena = 'Buenas tardes,'.
  ELSEIF gv_hor(2) GE 18 AND gv_hor(2) LE 24.
    lv_cadena = 'Buenas noches,'.
  ENDIF.
*Saludos,
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  <ls_objtxt> = lv_cadena.
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
*Cabecra
  READ TABLE ltd_ekpo_af ASSIGNING <ls_ekpo_af> INDEX 1.
  IF sy-subrc IS INITIAL .
    lv_bukrs = <ls_ekpo_af>-bukrs.
  ENDIF.

  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  CONCATENATE 'No fue posible actualizar el activo que corresponde al pedido de traslado'
  'Nro:' ' ' <ls_mseg>-ebeln ' '
  'Material: ' <ls_mseg>-matnr ' '
  'Periodo: ' <ls_mseg>-gjahr
  INTO <ls_objtxt> SEPARATED BY space.

  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  IF lv_bukrs IS NOT INITIAL.
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  CONCATENATE 'Documento de material: '  <ls_ekpo_af>-MJAHR  ' - ' <ls_ekpo_af>-MBLNR INTO <ls_objtxt> SEPARATED BY space.
  ENDIF.
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
*LISTADO DE ACTIVOS FIJOS
  LOOP AT ltd_ekpo_af ASSIGNING <ls_ekpo_af> WHERE type EQ 'S'.
*AF
    APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
    CONCATENATE <ls_ekpo_af>-anln1 <ls_ekpo_af>-anln2 INTO <ls_objtxt> SEPARATED BY space.
  ENDLOOP.
*FIN
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  <ls_objtxt> = 'Saludos cordiales.'.

* ENVIO DE CORREO
  LOOP AT ltd_constantes ASSIGNING <ls_constantes>.
    APPEND INITIAL LINE TO ltd_receivers ASSIGNING <ls_receivers>.
    <ls_receivers>-receiver = <ls_constantes>-valor1.
    <ls_receivers>-rec_type = 'U'.
  ENDLOOP.
*ASUNTO
  CONCATENATE 'Actualización de Activos Fijos Soc.' lv_bukrs
  INTO lwa_data-obj_descr SEPARATED BY space.

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1' STARTING NEW TASK 'UPDATE' DESTINATION 'NONE'
    EXPORTING
      document_data              = lwa_data
*     document_type              = 'HTM'
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      object_header              = objtxt
      object_content             = objtxt
      receivers                  = ltd_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

 CHECK 1 = 2.
"EMAT - Enrique Aguirre - 03.11.2020 -  107337



           ENDIF.
         ELSE.
           APPEND INITIAL LINE TO ltd_ekpo_af ASSIGNING <ls_ekpo_af>.
             <ls_ekpo_af>-bukrs  = <ls_ekpo>-bukrs.
             <ls_ekpo_af>-ebeln  = <ls_ekpo>-ebeln.
             <ls_ekpo_af>-ebelp  = <ls_ekpo>-ebelp.
         ENDIF.
*  Datos adicionales
          <ls_ekpo_af>-zzmarca     =  <ls_mseg>-zzmarca.
          <ls_ekpo_af>-zzmodelo    =  <ls_mseg>-zzmodelo.
          <ls_ekpo_af>-zzserie     =  <ls_mseg>-zzserie.
          <ls_ekpo_af>-zzremision  =  <ls_mseg>-zzremision.
          <ls_ekpo_af>-zzpedido    =  <ls_mseg>-zzpedido.
          <ls_ekpo_af>-zzposicion  =  <ls_mseg>-zzposicion.
          <ls_ekpo_af>-matnr       =  <ls_mseg>-matnr.

          <ls_ekpo_af>-MBLNR       =  <ls_mseg>-MBLNR.
          <ls_ekpo_af>-MJAHR       =  <ls_mseg>-MJAHR.
          <ls_ekpo_af>-ZEILE       =  <ls_mseg>-ZEILE.
          IF <ls_ekpo_af>-matnr IS NOT INITIAL.
            SELECT SINGLE ean11 numtp
               INTO ( <ls_ekpo_af>-ean11, <ls_ekpo_af>-numtp )
                 FROM mara WHERE matnr EQ <ls_ekpo_af>-matnr.
          ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE ltd_ekpo_af WHERE ebeln EQ space.
  CHECK  ltd_ekpo_af IS NOT INITIAL.
*Imputación en el documento de compras
  SELECT ebeln
         ebelp
         anln1
         anln2
     FROM ekkn
       INTO TABLE ltd_ekkn
        FOR ALL ENTRIES IN ltd_ekpo_af
         WHERE ebeln EQ ltd_ekpo_af-ebeln
           AND ebelp EQ ltd_ekpo_af-ebelp
           AND loekz EQ space.
  IF ltd_ekkn IS NOT INITIAL.
*Si encontramos el pedido orginal y buscamos el activos fijo
    LOOP AT ltd_ekkn ASSIGNING <ls_ekkn>.
      REFRESH ltd_extension_in.
      CLEAR: lwa_extension_in,lwa_bapi_te_anlu,lwa_mensaje,ls_generaldata,ls_generaldatax.
      READ TABLE ltd_ekpo_af ASSIGNING <ls_ekpo_af>
                      WITH KEY ebeln =  <ls_ekkn>-ebeln
                               ebelp =  <ls_ekkn>-ebelp.
      IF sy-subrc IS INITIAL.
        IF <ls_ekkn>-anln1 IS NOT INITIAL.
          <ls_ekpo_af>-anln1 = <ls_ekkn>-anln1.
          <ls_ekpo_af>-anln2 = <ls_ekkn>-anln2.
*
        READ TABLE ltd_ser03 ASSIGNING <ls_ser03> WITH KEY  MBLNR = <ls_ekpo_af>-MBLNR
                                                            MJAHR = <ls_ekpo_af>-MJAHR
                                                            ZEILE = <ls_ekpo_af>-ZEILE.
        IF sy-subrc IS INITIAL.
          ls_generaldata-serial_no = <ls_ser03>-sernr.
        ENDIF.
        IF ls_generaldata-serial_no IS NOT INITIAL.
          ls_generaldatax-serial_no = abap_true.
        ENDIF.
*Actualizar datos adicionales - AF
          lwa_bapi_te_anlu-comp_code   = <ls_ekpo_af>-bukrs.
          lwa_bapi_te_anlu-assetmaino  = <ls_ekkn>-anln1.
          lwa_bapi_te_anlu-assetsubno  = <ls_ekkn>-anln2.
          lwa_bapi_te_anlu-zzmarca     = <ls_ekpo_af>-zzmarca.
          lwa_bapi_te_anlu-zzmodelo    = <ls_ekpo_af>-zzmodelo.
          lwa_bapi_te_anlu-zzserie     = <ls_ekpo_af>-zzserie.
          lwa_bapi_te_anlu-zzremision  = <ls_ekpo_af>-zzremision.
          lwa_bapi_te_anlu-zzpedido    = <ls_ekpo_af>-zzpedido.
          lwa_bapi_te_anlu-zzposicion  = <ls_ekpo_af>-zzposicion.
          lwa_bapi_te_anlu-zz_ean11    = <ls_ekpo_af>-ean11.
          lwa_bapi_te_anlu-zz_numtp    = <ls_ekpo_af>-numtp.
          lwa_bapi_te_anlu-zzmaterial  = <ls_ekpo_af>-matnr.
          <ls_ekpo_af>-sel = 'X'.
          lwa_extension_in-structure = 'BAPI_TE_ANLU'.
          MOVE lwa_bapi_te_anlu TO lwa_extension_in-valuepart1.
          APPEND lwa_extension_in TO ltd_extension_in.
          CLEAR  lwa_extension_in.
          CALL FUNCTION 'BAPI_FIXEDASSET_CHANGE'
            EXPORTING
              companycode = <ls_ekpo_af>-bukrs
              asset       = <ls_ekkn>-anln1
              subnumber   = <ls_ekkn>-anln2
              generaldata         = ls_generaldata
              generaldatax        = ls_generaldatax
            IMPORTING
              return      = lwa_mensaje
            TABLES
              extensionin = ltd_extension_in.
          IF sy-subrc IS INITIAL.
            <ls_ekpo_af>-type    = lwa_mensaje-type.
            <ls_ekpo_af>-message = lwa_mensaje-message.
          ENDIF.
          IF lwa_mensaje-type NE lc_error.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
         ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
*TODOS LOS ACTIVOS FIJOS QUE FUERON ACTUALIZADOS(925) ENVIAR POR CORREO
  DELETE ltd_constantes WHERE campo NE 'MAIL'. "
  DELETE ltd_ekpo_af WHERE sel NE 'X'.
  CHECK ltd_constantes IS NOT INITIAL AND ltd_ekpo_af IS NOT INITIAL.
*CREAMOS CUERPO DEL  CORREO
  gv_hor = sy-uzeit.
  IF gv_hor(2) GE 24 AND gv_hor(2) LE 12.
    lv_cadena = 'Buenos días,'.
  ELSEIF gv_hor(2) GE 12 AND gv_hor(2) LE 18.
    lv_cadena = 'Buenas tardes,'.
  ELSEIF gv_hor(2) GE 18 AND gv_hor(2) LE 24.
    lv_cadena = 'Buenas noches,'.
  ENDIF.
*Saludos,
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  <ls_objtxt> = lv_cadena.
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
*Cabecra
  READ TABLE ltd_ekpo_af ASSIGNING <ls_ekpo_af> INDEX 1.
  IF sy-subrc IS INITIAL .
    lv_bukrs = <ls_ekpo_af>-bukrs.
  ENDIF.

  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  CONCATENATE 'Se actualizaron los datos maestros de los siguientes números de unidad de reemplazo en la sociedad'
   lv_bukrs '(marca, modelo, serie, guía proveedor, cód.nac.unidas, pedido/pos.):'
   INTO <ls_objtxt> SEPARATED BY space.

  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  IF lv_bukrs IS NOT INITIAL.
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  CONCATENATE 'Documento de material: '  <ls_ekpo_af>-MJAHR  ' - ' <ls_ekpo_af>-MBLNR INTO <ls_objtxt> SEPARATED BY space.
  ENDIF.
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
*LISTADO DE ACTIVOS FIJOS
  LOOP AT ltd_ekpo_af ASSIGNING <ls_ekpo_af> WHERE type EQ 'S'.
*AF
    APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
    CONCATENATE <ls_ekpo_af>-anln1 <ls_ekpo_af>-anln2 INTO <ls_objtxt> SEPARATED BY space.
  ENDLOOP.
*FIN
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  APPEND INITIAL LINE TO objtxt ASSIGNING <ls_objtxt>.
  <ls_objtxt> = 'Saludos cordiales.'.

* ENVIO DE CORREO
  LOOP AT ltd_constantes ASSIGNING <ls_constantes>.
    APPEND INITIAL LINE TO ltd_receivers ASSIGNING <ls_receivers>.
    <ls_receivers>-receiver = <ls_constantes>-valor1.
    <ls_receivers>-rec_type = 'U'.
  ENDLOOP.
*ASUNTO
  CONCATENATE 'Actualización de Activos Fijos Soc.' lv_bukrs
  INTO lwa_data-obj_descr SEPARATED BY space.

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1' STARTING NEW TASK 'UPDATE' DESTINATION 'NONE'
    EXPORTING
      document_data              = lwa_data
*     document_type              = 'HTM'
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      object_header              = objtxt
      object_content             = objtxt
      receivers                  = ltd_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
ENDIF.
ENDFUNCTION.