*&---------------------------------------------------------------------*
*& Include Z_MOVE_FIELD_TO_LIPS_PSTYP
*&---------------------------------------------------------------------*
*********************************************************************************
* CAMBIO DEL TIPO DE POSICION PARA ENTREGAS DE UNIDADES COMERCIALES A NO RELEVANTE PARA PICKING
*********************************************************************************
***********************************************************************************
*      H I S T O R I A L    D E    M O D I F I C A C I O N E S                    *
***********************************************************************************
*^ Fecha: 22.05.2019    Autor: Luis Angel Ore Caballero                           *
*^ Marca: @lore22052019                                                           *
*^ Descripción del cambio: Modificación                                           *
*^ Código Requerimiento  :                                                        *
***********************************************************************************


DATA: lt_constantes TYPE STANDARD TABLE OF ztconstantes WITH HEADER LINE.
RANGES: lr_werks FOR lips-werks.
REFRESH: lt_constantes,
         lr_werks.
SELECT *
  FROM ztconstantes
  INTO TABLE lt_constantes
  WHERE modulo     EQ 'SD'
    AND aplicacion EQ 'ENTREGA'
    AND programa   EQ 'Z_MOVE_FIELD_TO_LIPS_PSTYP'.
IF sy-subrc EQ 0.
  lr_werks-sign   = 'E'.
  lr_werks-option = 'EQ'.
  LOOP AT lt_constantes WHERE campo EQ 'WERKS'.
    lr_werks-low    = lt_constantes-valor1.
    APPEND lr_werks.
  ENDLOOP.
  IF lips-werks IN lr_werks AND
     lips-uecha NE space.
    LOOP AT lt_constantes WHERE campo       EQ 'PSTYV'    AND
                                valor2+0(4) EQ likp-lfart AND
                                valor2+5(4) EQ lips-pstyv.
      lips-pstyv = lt_constantes-valor1.
    ENDLOOP.
  ENDIF.
*********************************************************************************
*  ACTUALIZAR TIPO DE POSICION PARA ENTREGAS DE PLANTA - ZNLD NO DETERMINA LOTE
*********************************************************************************
*{+"@lore22052019
  DATA: vl_flag_01 TYPE c LENGTH 1.
  FREE: vl_flag_01.

  IF lips-matnr IS NOT INITIAL.
    SELECT matnr, xchpf, mtart INTO TABLE @DATA(lt_mara)
      FROM mara
      WHERE matnr = @lips-matnr.
    IF sy-subrc = 0.
      DATA(ls_mara) = VALUE #( lt_mara[ matnr = lips-matnr ] DEFAULT space ).
      IF ls_mara <> space.
        IF ls_mara-xchpf EQ space AND ls_mara-mtart EQ 'ZERS'.
          lips-pstyv ='ZNPS'.
          vl_flag_01 = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*}+"@lore22052019

  IF vl_flag_01 IS INITIAL."@lore22052019
    IF lips-werks EQ  'PE03' OR
       lips-werks  EQ  'CO01' OR " TEMPORAL
       lips-werks  EQ  'BR01'.
      IF  likp-lfart = 'ZEI1' OR likp-lfart =  'ZEI2' OR likp-lfart =   'ZEI3' OR likp-lfart =  'ZEI4'.
        IF  lips-uecha EQ space.
          CASE lips-pstyv.
            WHEN 'ZNLC'.
              lips-pstyv = 'ZNLD'.
          ENDCASE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF."@lore22052019

*  Tipo posicion no relevante para picking

  IF  likp-lfart = 'ZENT'.
    SELECT COUNT( * ) FROM marc
                 WHERE matnr = lips-matnr
                  AND  werks IN ('BE01' , 'BE02').
    IF sy-subrc =  0.
      IF  lips-uecha NE space.
        CASE lips-pstyv.
          WHEN 'ZMT2'.
            lips-pstyv = 'ZMT1'.
          WHEN 'ZMO2'.
            lips-pstyv = 'ZMO1'.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDIF.

* From AGP Belgium to AGP Switzerland OR America
  IF  ( lips-werks = 'BE01' OR lips-werks = 'BE02' ) AND likp-kunnr = 'PCH01'.
    IF  likp-lfart = 'ZEI1' OR likp-lfart =  'ZEI2' OR likp-lfart =   'ZEI3' OR likp-lfart =  'ZEI4'.
      IF  lips-pstyv NE  'ZHUP' AND lips-pstyv NE 'ZHUS'.
        lips-bwart = '643'.
      ENDIF.
    ENDIF.
  ENDIF.
  IF  ( lips-werks = 'BE01' OR lips-werks = 'BE02' ) AND likp-kunnr = 'PPA01'.
    IF  likp-lfart = 'ZEI1' OR likp-lfart =  'ZEI2' OR likp-lfart =   'ZEI3' OR likp-lfart =  'ZEI4'.
      IF  lips-pstyv NE  'ZHUP' AND lips-pstyv NE 'ZHUS'.
        lips-bwart = '643'.
      ENDIF.
    ENDIF.
  ENDIF.

* From AGP Switzerland or America to AGP Germany  and ZFER comes from Belgium
  IF ( likp-kunnr = 'PDE01' AND lips-werks = 'CH01')
  OR ( likp-kunnr = 'PDE01' AND lips-werks = 'PA01') .
    IF  likp-lfart = 'ZEI1' OR likp-lfart =  'ZEI2' OR likp-lfart =   'ZEI3' OR likp-lfart =  'ZEI4' AND likp-lfart = 'LF'.
      SELECT COUNT( * ) FROM marc
                      WHERE matnr = lips-matnr
                       AND  werks IN ('BE01' , 'BE02') .
      IF sy-subrc = 0.
        IF  lips-pstyv NE  'ZHUP' AND lips-pstyv NE 'ZHUS'.
          lips-bwart = '643'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* For direct deliveries from Belgium
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - I
DATA: lr_lfart TYPE RANGE OF lfart.
lr_lfart = VALUE #( FOR wa_lfart IN lt_constantes where ( campo = 'LFART' )
                  ( sign = 'I'
                    option = 'EQ'
                    low = CONDENSE( wa_lfart-valor1 )
                  ) ).

DELETE ADJACENT DUPLICATES FROM lr_lfart.

DATA: lr_werks_ TYPE RANGE OF WERKS_D.
lr_werks_ = VALUE #( FOR wa_werks IN lt_constantes where ( campo = 'WERKS' )
                  ( sign = 'I'
                    option = 'EQ'
                    low = CONDENSE( wa_werks-valor1 )
                  ) ).


DELETE ADJACENT DUPLICATES FROM lr_werks_.

DATA: lr_tcode TYPE RANGE OF TCODE.
lr_tcode = VALUE #( FOR wa_tcode IN lt_constantes where ( campo = 'TCODE' )
                  ( sign = 'I'
                    option = 'EQ'
                    low = CONDENSE( wa_tcode-valor1 )
                  ) ).

DELETE ADJACENT DUPLICATES FROM lr_tcode.

DATA: lr_vkorg TYPE RANGE OF VKORG.
lr_vkorg = VALUE #( FOR wa_vkorg IN lt_constantes where ( campo = 'VKORG' )
                  ( sign = 'I'
                    option = 'EQ'
                    low = CONDENSE( wa_vkorg-valor1 )
                  ) ).

DELETE ADJACENT DUPLICATES FROM lr_vkorg.

"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - I

"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace
  "IF  likp-lfart = 'ZENT' AND likp-lfart = 'LF'.
  IF  likp-lfart in lr_lfart.
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace

    SELECT COUNT( * ) FROM marc
                   WHERE matnr = lips-matnr
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace
                    "AND  werks IN ('BE01' , 'BE02') .
                    AND  werks IN lr_werks_.
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace

    IF sy-subrc = 0.
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace
      "IF  sy-tcode = 'VL01N' OR sy-tcode+0(4) = 'VL10'.
      IF  sy-tcode in lr_tcode.
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 c
        likp-kostk = 'A'.
      ENDIF.
    ENDIF.
  ENDIF.


* Determination of the route in case of empty value - Belgium
*============================================================
* All the outbound deliveries in Belgium must have the route 00000 NOT RELEVANT
* Fordirect deliveries and intercompany from Belgium
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace
*  IF likp-lfart = 'LF' OR
*     likp-lfart = 'ZENT' OR
*     likp-lfart = 'ZEI1' OR
*     likp-lfart = 'ZEI2' OR
*     likp-lfart = 'ZEI3' OR
*     likp-lfart = 'ZEI4'.
   IF  likp-lfart in lr_lfart.
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace
    IF likp-route = space AND
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace
       "likp-vkorg = 'BE01'.
       likp-vkorg in lr_vkorg.
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace
      likp-route = '000000'.
    ENDIF.
* Intercompany deliveries Switzerland->Europe
* with a material produced in Belgium must have generic route NOT RELEVANT 00000
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace
*    IF likp-vkorg =  'CH01' OR
*       likp-vkorg =  'UE01' OR
*       likp-vkorg = 'PA01' .
   IF  likp-vkorg in lr_vkorg.
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace
      SELECT COUNT( * )
             FROM marc
             WHERE matnr = lips-matnr
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace
               AND  werks IN lr_werks_.
               "AND werks IN ('BE01','BE02').
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace

      IF sy-subrc = 0.
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace
*        IF likp-vkorg =  'CH01'.
*          likp-route = '000000'.
*        ELSEIF likp-vkorg =  'UE01'.
*          likp-route = '000000'.
*        ELSEIF likp-vkorg =  'PA01'.
*          likp-route = '000000'.
*        ENDIF.
likp-route = '000000'.
"EMAT - Enrique Aguirre - 19.10.2020 - AGP-945 - Replace
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.

"Exit INBOUND DELIVERY Subcontratación
  LOOP AT xlips INTO DATA(lwa_lips) WHERE vgbel IS NOT INITIAL.
    EXIT.
  ENDLOOP.

SELECT SINGLE mtart
  INTO @DATA(lv_mtart)
  FROM mara
  WHERE matnr = @lips-matnr.

SELECT SINGLE bsart
  INTO @DATA(lv_bsart)
  FROM ekko
  WHERE ebeln EQ @lwa_lips-vgbel.

IF lv_bsart     EQ 'ZSC'    AND
   likp-lfart   EQ 'EL'     AND
   ( lips-werks EQ 'BE01' OR
     lips-werks EQ 'BE02' ) AND
   ( lv_mtart   EQ 'ZVER' OR
     lv_mtart   EQ 'ZVEN' ) .
  lips-bwart = '542'.
ENDIF.