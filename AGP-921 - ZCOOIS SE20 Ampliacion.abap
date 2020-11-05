ENHANCEMENT 1  ZCOOIS.    "active version
"EMAT - Enrique Aguirre - 29.10.2020 - AGP`-921
  DATA: ls_id_memory TYPE char50.
  DATA: lt_seltab_tmp TYPE TABLE OF rsparams.
  DATA: wa_seltab_tmp LIKE LINE OF lt_seltab_tmp.
  DATA: lr_filtro TYPE RANGE OF CHAR30.

  DATA: lr_charg TYPE RANGE OF afpo-charg,
        lr_kdauf TYPE RANGE OF afpo-kdauf,
        lr_kdpos TYPE RANGE OF afpo-kdpos,
        lr_ktsch TYPE RANGE OF afru-zzktsch,
        lr_tip_n TYPE RANGE OF afru-zztipo_notif,
        lr_lo_co TYPE RANGE OF afru-zzlote_corte,
        lr_lo_cu TYPE RANGE OF afru-zzlote_curvado,
        lr_cic_t TYPE RANGE OF afru-zzciclo_tq,
        lr_cic_a TYPE RANGE OF afru-zzciclo_auto,
        lr_cod_l TYPE RANGE OF afru-zzcod_lanza,
        lr_lo_ca TYPE RANGE OF afru-zzlote_carga,
        lr_stzhl TYPE RANGE OF afru-stzhl,
        lr_pernr TYPE RANGE OF afru-pernr,
        lr_equip TYPE RANGE OF afru-zzequipo,
        lr_stokz TYPE RANGE OF afru-stokz,

        lr_agp_l TYPE RANGE OF inob-objek,
        lr_balli TYPE RANGE OF inob-objek,
        lr_tip_b TYPE RANGE OF inob-objek,
        lr_piece TYPE RANGE OF inob-objek,
        lr_geome TYPE RANGE OF inob-objek,
        lr_atwrt TYPE RANGE OF ausp-atwrt,

        lr_ers_e TYPE RANGE OF afru-ersda,
        "lr_ers_a TYPE RANGE OF afru-ersda,

        lr_bsark TYPE RANGE OF vbak-bsark,
        lr_mtart TYPE RANGE OF mara-mtart,
        lr_vornr TYPE RANGE OF afru-vornr,

        lr_erzet TYPE RANGE OF afru-erzet.

 DATA: wa_atinn TYPE atinn.
 DATA: rng_atinn TYPE RANGE OF atinn.

IF SY-TCODE EQ 'ZCOOIS'.

 lr_filtro = VALUE #( sign   = 'I'
             option = 'EQ'
           ( low = 'SS_CHARG' ) ( low = 'SS_KDAUF' ) ( low = 'SS_KDPOS' )
           ( low = 'SS_KTSCH' ) ( low = 'SS_TIP_N' ) ( low = 'SS_LO_CO' )
           ( low = 'SS_LO_CU' ) ( low = 'SS_CIC_T' ) ( low = 'SS_CIC_A' )
           ( low = 'SS_COD_L' ) ( low = 'SS_LO_CA' ) ( low = 'SS_STZHL' )
           ( low = 'SS_PERNR' ) ( low = 'SS_EQUIP' ) ( low = 'SS_STOKZ' )
           ( low = 'SS_AGP_L' ) ( low = 'SS_BALLI' ) ( low = 'SS_TIP_B' )
           ( low = 'SS_PIECE' ) ( low = 'SS_GEOME' ) ( low = 'SS_ERS_E' )
           "( low = 'SS_ERS_A' )
           ( low = 'SS_ERZET' ) ).

  CONCATENATE 'ZCOOIS' sy-uname INTO ls_id_memory.
  IMPORT lt_seltab_tmp TO lt_seltab_tmp FROM MEMORY ID ls_id_memory.
  LOOP AT lt_seltab_tmp INTO wa_seltab_tmp
    WHERE SELNAME IN lr_filtro AND LOW IS NOT INITIAL.
    CASE wa_seltab_tmp-SELNAME.
      WHEN 'SS_CHARG'.
            APPEND INITIAL LINE TO lr_charg ASSIGNING FIELD-SYMBOL(<fs_charg>).
            <fs_charg>-sign   = wa_seltab_tmp-sign.
            <fs_charg>-option = wa_seltab_tmp-option.
            <fs_charg>-low    = wa_seltab_tmp-low.
            <fs_charg>-high   = wa_seltab_tmp-high.
      WHEN 'SS_KDAUF'.
            APPEND INITIAL LINE TO lr_kdauf ASSIGNING FIELD-SYMBOL(<fs_lr_kdauf>).
            <fs_lr_kdauf>-sign   = wa_seltab_tmp-sign.
            <fs_lr_kdauf>-option = wa_seltab_tmp-option.
            <fs_lr_kdauf>-low    = wa_seltab_tmp-low.
            <fs_lr_kdauf>-high   = wa_seltab_tmp-high.
      WHEN 'SS_KDPOS'.
            APPEND INITIAL LINE TO lr_kdpos ASSIGNING FIELD-SYMBOL(<fs_lr_kdpos>).
            <fs_lr_kdpos>-sign   = wa_seltab_tmp-sign.
            <fs_lr_kdpos>-option = wa_seltab_tmp-option.
            <fs_lr_kdpos>-low    = wa_seltab_tmp-low.
            <fs_lr_kdpos>-high   = wa_seltab_tmp-high.
      WHEN 'SS_KTSCH'.
            APPEND INITIAL LINE TO lr_ktsch ASSIGNING FIELD-SYMBOL(<fs_lr_ktsch>).
            <fs_lr_ktsch>-sign   = wa_seltab_tmp-sign.
            <fs_lr_ktsch>-option = wa_seltab_tmp-option.
            <fs_lr_ktsch>-low    = wa_seltab_tmp-low.
            <fs_lr_ktsch>-high   = wa_seltab_tmp-high.
      WHEN 'SS_TIP_N'.
            APPEND INITIAL LINE TO lr_tip_n ASSIGNING FIELD-SYMBOL(<fs_lr_tip_n>).
            <fs_lr_tip_n>-sign   = wa_seltab_tmp-sign.
            <fs_lr_tip_n>-option = wa_seltab_tmp-option.
            <fs_lr_tip_n>-low    = wa_seltab_tmp-low.
            <fs_lr_tip_n>-high   = wa_seltab_tmp-high.
      WHEN 'SS_LO_CO'.
            APPEND INITIAL LINE TO lr_lo_co ASSIGNING FIELD-SYMBOL(<fs_lo_co>).
            <fs_lo_co>-sign   = wa_seltab_tmp-sign.
            <fs_lo_co>-option = wa_seltab_tmp-option.
            <fs_lo_co>-low    = wa_seltab_tmp-low.
            <fs_lo_co>-high   = wa_seltab_tmp-high.
      WHEN 'SS_LO_CU'.
            APPEND INITIAL LINE TO lr_lo_cu ASSIGNING FIELD-SYMBOL(<fs_lr_lo_cu>).
            <fs_lr_lo_cu>-sign   = wa_seltab_tmp-sign.
            <fs_lr_lo_cu>-option = wa_seltab_tmp-option.
            <fs_lr_lo_cu>-low    = wa_seltab_tmp-low.
            <fs_lr_lo_cu>-high   = wa_seltab_tmp-high.
      WHEN 'SS_CIC_T'.
            APPEND INITIAL LINE TO lr_cic_t ASSIGNING FIELD-SYMBOL(<fs_lr_cic_t>).
            <fs_lr_cic_t>-sign   = wa_seltab_tmp-sign.
            <fs_lr_cic_t>-option = wa_seltab_tmp-option.
            <fs_lr_cic_t>-low    = wa_seltab_tmp-low.
            <fs_lr_cic_t>-high   = wa_seltab_tmp-high.
      WHEN 'SS_CIC_A'.
            APPEND INITIAL LINE TO lr_cic_a ASSIGNING FIELD-SYMBOL(<fs_lr_cic_a>).
            <fs_lr_cic_a>-sign   = wa_seltab_tmp-sign.
            <fs_lr_cic_a>-option = wa_seltab_tmp-option.
            <fs_lr_cic_a>-low    = wa_seltab_tmp-low.
            <fs_lr_cic_a>-high   = wa_seltab_tmp-high.
      WHEN 'SS_COD_L'.
            APPEND INITIAL LINE TO lr_cod_l ASSIGNING FIELD-SYMBOL(<fs_lr_cod_l>).
            <fs_lr_cod_l>-sign   = wa_seltab_tmp-sign.
            <fs_lr_cod_l>-option = wa_seltab_tmp-option.
            <fs_lr_cod_l>-low    = wa_seltab_tmp-low.
            <fs_lr_cod_l>-high   = wa_seltab_tmp-high.
      WHEN 'SS_LO_CA'.
            APPEND INITIAL LINE TO lr_lo_ca ASSIGNING FIELD-SYMBOL(<fs_lr_lo_ca>).
            <fs_lr_lo_ca>-sign   = wa_seltab_tmp-sign.
            <fs_lr_lo_ca>-option = wa_seltab_tmp-option.
            <fs_lr_lo_ca>-low    = wa_seltab_tmp-low.
            <fs_lr_lo_ca>-high   = wa_seltab_tmp-high.
      WHEN 'SS_STZHL'.
            APPEND INITIAL LINE TO lr_stzhl ASSIGNING FIELD-SYMBOL(<fs_lr_stzhl>).
            <fs_lr_stzhl>-sign   = wa_seltab_tmp-sign.
            <fs_lr_stzhl>-option = wa_seltab_tmp-option.
            <fs_lr_stzhl>-low    = wa_seltab_tmp-low.
            <fs_lr_stzhl>-high   = wa_seltab_tmp-high.
      WHEN 'SS_PERNR'.
            APPEND INITIAL LINE TO lr_pernr ASSIGNING FIELD-SYMBOL(<fs_lr_pernr>).
            <fs_lr_pernr>-sign   = wa_seltab_tmp-sign.
            <fs_lr_pernr>-option = wa_seltab_tmp-option.
            <fs_lr_pernr>-low    = wa_seltab_tmp-low.
            <fs_lr_pernr>-high   = wa_seltab_tmp-high.
      WHEN 'SS_EQUIP'.
            APPEND INITIAL LINE TO lr_equip ASSIGNING FIELD-SYMBOL(<fs_lr_equip>).
            <fs_lr_equip>-sign   = wa_seltab_tmp-sign.
            <fs_lr_equip>-option = wa_seltab_tmp-option.
            <fs_lr_equip>-low    = wa_seltab_tmp-low.
            <fs_lr_equip>-high   = wa_seltab_tmp-high.
      WHEN 'SS_STOKZ'.
            APPEND INITIAL LINE TO lr_stokz ASSIGNING FIELD-SYMBOL(<fs_lr_stokz>).
            <fs_lr_stokz>-sign   = wa_seltab_tmp-sign.
            <fs_lr_stokz>-option = wa_seltab_tmp-option.
            <fs_lr_stokz>-low    = wa_seltab_tmp-low.
            <fs_lr_stokz>-high   = wa_seltab_tmp-high.
      WHEN 'SS_AGP_L'.
            APPEND INITIAL LINE TO lr_agp_l ASSIGNING FIELD-SYMBOL(<fs_lr_agp_l>).
            <fs_lr_agp_l>-sign   = wa_seltab_tmp-sign.
            <fs_lr_agp_l>-option = wa_seltab_tmp-option.

            concatenate '0' wa_seltab_tmp-low INTO wa_seltab_tmp-low.
            data(wlong) = strlen( wa_seltab_tmp-low ) - 2.
            <fs_lr_agp_l>-low    = wa_seltab_tmp-low+wlong(2).

            IF wa_seltab_tmp-high IS NOT INITIAL.
              concatenate '0' wa_seltab_tmp-high INTO wa_seltab_tmp-high.
              wlong = strlen( wa_seltab_tmp-high ) - 2.
              <fs_lr_agp_l>-high  = wa_seltab_tmp-high+wlong(2).
            ENDIF.
      WHEN 'SS_ERS_E'.
            APPEND INITIAL LINE TO lr_ers_e ASSIGNING FIELD-SYMBOL(<fs_lr_ers_e>).
            <fs_lr_ers_e>-sign   = wa_seltab_tmp-sign.
            <fs_lr_ers_e>-option = wa_seltab_tmp-option.
            <fs_lr_ers_e>-low    = wa_seltab_tmp-low.
            <fs_lr_ers_e>-high   = wa_seltab_tmp-high.

      WHEN 'SS_BSARK'.
            APPEND INITIAL LINE TO lr_bsark ASSIGNING FIELD-SYMBOL(<fs_lr_bsark>).
            <fs_lr_bsark>-sign   = wa_seltab_tmp-sign.
            <fs_lr_bsark>-option = wa_seltab_tmp-option.
            <fs_lr_bsark>-low    = wa_seltab_tmp-low.
            <fs_lr_bsark>-high   = wa_seltab_tmp-high.

      WHEN 'SS_MTART'.
            APPEND INITIAL LINE TO lr_mtart ASSIGNING FIELD-SYMBOL(<fs_lr_mtart>).
            <fs_lr_mtart>-sign   = wa_seltab_tmp-sign.
            <fs_lr_mtart>-option = wa_seltab_tmp-option.
            <fs_lr_mtart>-low    = wa_seltab_tmp-low.
            <fs_lr_mtart>-high   = wa_seltab_tmp-high.

      WHEN 'SS_VORNR'.
            APPEND INITIAL LINE TO lr_vornr ASSIGNING FIELD-SYMBOL(<fs_lr_vornr>).
            <fs_lr_vornr>-sign   = wa_seltab_tmp-sign.
            <fs_lr_vornr>-option = wa_seltab_tmp-option.
            <fs_lr_vornr>-low    = wa_seltab_tmp-low.
            <fs_lr_vornr>-high   = wa_seltab_tmp-high.

      WHEN 'SS_BALLI'.
            APPEND INITIAL LINE TO lr_balli ASSIGNING FIELD-SYMBOL(<fs_lr_balli>).
            <fs_lr_balli>-sign   = wa_seltab_tmp-sign.
            <fs_lr_balli>-option = wa_seltab_tmp-option.

            concatenate '0' wa_seltab_tmp-low INTO wa_seltab_tmp-low.
            wlong = strlen( wa_seltab_tmp-low ) - 2.
            <fs_lr_balli>-low    = wa_seltab_tmp-low+wlong(2).

            IF wa_seltab_tmp-high IS NOT INITIAL.
              concatenate '0' wa_seltab_tmp-high INTO wa_seltab_tmp-high.
              wlong = strlen( wa_seltab_tmp-high ) - 2.
              <fs_lr_balli>-high  = wa_seltab_tmp-high+wlong(2).
            ENDIF.
      WHEN 'SS_PIECE'.
            APPEND INITIAL LINE TO lr_piece ASSIGNING FIELD-SYMBOL(<fs_lr_piece>).
            <fs_lr_piece>-sign   = wa_seltab_tmp-sign.
            <fs_lr_piece>-option = wa_seltab_tmp-option.

            concatenate '00' wa_seltab_tmp-low INTO wa_seltab_tmp-low.
            wlong = strlen( wa_seltab_tmp-low ) - 3.
            <fs_lr_piece>-low    = wa_seltab_tmp-low+wlong(3).

            IF wa_seltab_tmp-high IS NOT INITIAL.
              concatenate '00' wa_seltab_tmp-high INTO wa_seltab_tmp-high.
              wlong = strlen( wa_seltab_tmp-high ) - 3.
              <fs_lr_piece>-high   = wa_seltab_tmp-high+wlong(3).
            ENDIF.
      WHEN 'SS_GEOME'.
            APPEND INITIAL LINE TO lr_geome ASSIGNING FIELD-SYMBOL(<fs_lr_geome>).
            <fs_lr_geome>-sign   = wa_seltab_tmp-sign.
            <fs_lr_geome>-option = wa_seltab_tmp-option.

            concatenate '0' wa_seltab_tmp-low INTO wa_seltab_tmp-low.
            wlong = strlen( wa_seltab_tmp-low ) - 2.
            <fs_lr_geome>-low    = wa_seltab_tmp-low+wlong(2).

            IF wa_seltab_tmp-high IS NOT INITIAL.
              concatenate '0' wa_seltab_tmp-high INTO wa_seltab_tmp-high.
              wlong = strlen( wa_seltab_tmp-high ) - 2.
              <fs_lr_geome>-high   = wa_seltab_tmp-high+wlong(2).
            ENDIF.
      WHEN 'SS_TIP_B'.
            APPEND INITIAL LINE TO lr_tip_b ASSIGNING FIELD-SYMBOL(<fs_lr_tip_b>).
            <fs_lr_tip_b>-sign   = wa_seltab_tmp-sign.
            <fs_lr_tip_b>-option = wa_seltab_tmp-option.

            concatenate '0' wa_seltab_tmp-low INTO wa_seltab_tmp-low.
            wlong = strlen( wa_seltab_tmp-low ) - 2.
            <fs_lr_tip_b>-low    = wa_seltab_tmp-low+wlong(2).

            IF wa_seltab_tmp-high IS NOT INITIAL.
              concatenate '0' wa_seltab_tmp-high INTO wa_seltab_tmp-high.
              wlong = strlen( wa_seltab_tmp-high ) - 2.
              <fs_lr_tip_b>-high   = wa_seltab_tmp-high+wlong(2).
            ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

    IF lr_charg[] IS NOT INITIAL.
      EXPORT lr_charg FROM lr_charg TO SHARED BUFFER INDX(XY) ID'#ZCCOISLR_CHARG#'.
    ENDIF.
    IF lr_ktsch[] IS NOT INITIAL.
      EXPORT lr_ktsch FROM lr_ktsch TO SHARED BUFFER INDX(XY) ID'#ZCCOISKTSCH#'.
    ENDIF.
    IF lr_tip_n[] IS NOT INITIAL.
      EXPORT lr_tip_n FROM lr_tip_n TO SHARED BUFFER INDX(XY) ID'#ZCCOISTIP_N#'.
    ENDIF.
    IF lr_lo_co[] IS NOT INITIAL.
      EXPORT lr_lo_co FROM lr_lo_co TO SHARED BUFFER INDX(XY) ID'#ZCCOISLR_LO_CO#'.
    ENDIF.
    IF lr_lo_cu[] IS NOT INITIAL.
      EXPORT lr_lo_cu FROM lr_lo_cu TO SHARED BUFFER INDX(XY) ID'#ZCCOISLR_LO_CU#'.
    ENDIF.
    IF lr_cic_t[] IS NOT INITIAL.
      EXPORT lr_cic_t FROM lr_cic_t TO SHARED BUFFER INDX(XY) ID'#ZCCOISLR_CIC_T#'.
    ENDIF.
    IF lr_cic_a[] IS NOT INITIAL.
      EXPORT lr_cic_a FROM lr_cic_a TO SHARED BUFFER INDX(XY) ID'#ZCCOISLR_CIC_A#'.
    ENDIF.
    IF lr_cod_l[] IS NOT INITIAL.
      EXPORT lr_cod_l FROM lr_cod_l TO SHARED BUFFER INDX(XY) ID'#ZCCOISLR_COD_L#'.
    ENDIF.
    IF lr_lo_ca[] IS NOT INITIAL.
      EXPORT lr_lo_ca FROM lr_lo_ca TO SHARED BUFFER INDX(XY) ID'#ZCCOISLR_LO_CA#'.
    ENDIF.
    IF lr_stzhl[] IS NOT INITIAL.
      EXPORT lr_stzhl FROM lr_stzhl TO SHARED BUFFER INDX(XY) ID'#ZCCOISLR_STZHL#'.
    ENDIF.
    IF lr_pernr[] IS NOT INITIAL.
      EXPORT lr_pernr FROM lr_pernr TO SHARED BUFFER INDX(XY) ID'#ZCCOISLR_PERNR#'.
    ENDIF.
    IF lr_equip[] IS NOT INITIAL.
      EXPORT lr_equip FROM lr_equip TO SHARED BUFFER INDX(XY) ID'#ZCCOISLR_EQUIP#'.
    ENDIF.
    IF lr_stokz[] IS NOT INITIAL.
      EXPORT lr_stokz FROM lr_stokz TO SHARED BUFFER INDX(XY) ID'#ZCCOISLR_STOKZ#'.
    ENDIF.
    IF lr_ers_e[] IS NOT INITIAL.
      EXPORT lr_ers_e FROM lr_ers_e TO SHARED BUFFER INDX(XY) ID'#ZCCOISLR_ERS_E#'.
    ENDIF.

    IF lr_agp_l[] IS NOT INITIAL.
          EXPORT lr_agp_l FROM lr_agp_l TO SHARED BUFFER INDX(XY) ID'#ZCCOIS_LR_AGP_L#'.
    ENDIF.
    IF lr_piece[] IS NOT INITIAL.
          EXPORT lr_piece FROM lr_piece TO SHARED BUFFER INDX(XY) ID'#ZCCOIS_LR_PIECE#'.
    ENDIF.

    IF lr_balli[] IS NOT INITIAL.
          EXPORT lr_balli FROM lr_balli TO SHARED BUFFER INDX(XY) ID'#ZCCOIS_LR_BALLI#'.
    ENDIF.

    IF lr_geome[] IS NOT INITIAL.
          EXPORT lr_geome FROM lr_geome TO SHARED BUFFER INDX(XY) ID'#ZCCOIS_LR_GEOME#'.
    ENDIF.

    IF lr_tip_b[] IS NOT INITIAL.
          EXPORT lr_tip_b FROM lr_tip_b TO SHARED BUFFER INDX(XY) ID'#ZCCOIS_LR_TIP_B#'.
    ENDIF.

    IF lr_charg[] IS NOT INITIAL OR lr_kdauf[] IS NOT INITIAL OR lr_kdpos[] IS NOT INITIAL OR
       lr_ktsch[] IS NOT INITIAL OR lr_tip_n[] IS NOT INITIAL OR lr_lo_co[] IS NOT INITIAL OR
       lr_lo_cu[] IS NOT INITIAL OR lr_cic_t[] IS NOT INITIAL OR lr_cic_a[] IS NOT INITIAL OR
       lr_cod_l[] IS NOT INITIAL OR lr_lo_ca[] IS NOT INITIAL OR lr_stzhl[] IS NOT INITIAL OR
       lr_pernr[] IS NOT INITIAL OR lr_equip[] IS NOT INITIAL OR lr_stokz[] IS NOT INITIAL OR
       lr_balli[] IS NOT INITIAL OR lr_tip_b[] IS NOT INITIAL OR
       lr_piece[] IS NOT INITIAL OR lr_geome[] IS NOT INITIAL OR lr_ers_e[] IS NOT INITIAL OR
       "lr_ers_a[] IS NOT INITIAL OR
       lr_erzet[] IS NOT INITIAL.
      select afru~aufnr as aufnr
      from afru
      left outer join vbak
        on afru~aufnr = vbak~aufnr

      left outer join afpo
        on afru~aufnr = afpo~aufnr

      left outer join mara
        on afpo~matnr = mara~matnr
      where afru~zzlote         in @lr_charg and
            afru~ersda          in @lr_ers_e and
            afru~zzktsch        in @lr_ktsch and
            afru~zztipo_notif   in @lr_tip_n and
            afru~zzlote_corte   in @lr_lo_co and
            afru~zzlote_curvado in @lr_lo_cu and
            afru~zzciclo_tq     in @lr_cic_t and
            afru~zzciclo_auto   in @lr_cic_a and
            afru~zzcod_lanza    in @lr_cod_l and
            afru~zzlote_carga   in @lr_lo_ca and
            afru~stzhl          in @lr_stzhl and
            afru~pernr          in @lr_pernr and
            afru~zzequipo       in @lr_equip and
            afru~stokz          in @lr_stokz and
            afru~werks          in @S_WERKS  and
            afru~aufnr          in @s_aufnr  and
            vbak~bsark          in @lr_bsark and
            mara~mtart          in @lr_mtart and
            afru~vornr          in @lr_vornr
      INTO TABLE @DATA(it_aufnr).

      SORT it_aufnr BY AUFNR.
      DELETE ADJACENT DUPLICATES FROM it_aufnr.
      LOOP AT it_aufnr ASSIGNING FIELD-SYMBOL(<FS_AUFNR>) WHERE AUFNR IS NOT INITIAL.
      APPEND INITIAL LINE TO s_aufnr ASSIGNING FIELD-SYMBOL(<RNG_AUFNR>).
        <RNG_AUFNR>-sign = 'I'.
        <RNG_AUFNR>-option = 'EQ'.
        <RNG_AUFNR>-low = <FS_AUFNR>-aufnr.
      ENDLOOP.
      DELETE s_aufnr[] WHERE low is INITIAL.
      DELETE ADJACENT DUPLICATES FROM s_aufnr[].

      IF lr_agp_l[] IS NOT INITIAL AND
         lr_piece[] IS NOT INITIAL AND
         lr_balli[] IS NOT INITIAL AND
         lr_geome[] IS NOT INITIAL AND
         s_aufnr[] IS INITIAL.

         CHECK 1 = 2. "Se cancela la consulta

      ENDIF.

    ENDIF.

IF lr_agp_l[] IS NOT INITIAL OR
   lr_piece[] IS NOT INITIAL OR
   lr_balli[] IS NOT INITIAL OR
   lr_geome[] IS NOT INITIAL.

IF lr_agp_l[] IS NOT INITIAL.
   MOVE-CORRESPONDING lr_agp_l TO lr_atwrt.

  CLEAR wa_atinn.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'Z_AGP_LEVEL'
    IMPORTING
      output = wa_atinn.

  APPEND INITIAL LINE TO rng_atinn ASSIGNING FIELD-SYMBOL(<fs_atinn>).
  <fs_atinn>-sign   = 'I'.
  <fs_atinn>-option = 'EQ'.
  <fs_atinn>-low    = wa_atinn.

ENDIF.

IF lr_piece[] IS NOT INITIAL.
   MOVE-CORRESPONDING lr_piece TO lr_atwrt.

  CLEAR wa_atinn.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'Z_PIECE_TYPE'
    IMPORTING
      output = wa_atinn.

  APPEND INITIAL LINE TO rng_atinn ASSIGNING <fs_atinn>.
  <fs_atinn>-sign   = 'I'.
  <fs_atinn>-option = 'EQ'.
  <fs_atinn>-low    = wa_atinn.
ENDIF.

IF lr_balli[] IS NOT INITIAL.
   MOVE-CORRESPONDING lr_balli TO lr_atwrt.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'Z_BALLISTIC_STANDARD'
    IMPORTING
      output = wa_atinn.

  APPEND INITIAL LINE TO rng_atinn ASSIGNING <fs_atinn>.
  <fs_atinn>-sign   = 'I'.
  <fs_atinn>-option = 'EQ'.
  <fs_atinn>-low    = wa_atinn.
ENDIF.

IF lr_geome[] IS NOT INITIAL.
   MOVE-CORRESPONDING lr_geome TO lr_atwrt.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'Z_GEOMETRY_TYPE'
    IMPORTING
      output = wa_atinn.

  APPEND INITIAL LINE TO rng_atinn ASSIGNING <fs_atinn>.
  <fs_atinn>-sign   = 'I'.
  <fs_atinn>-option = 'EQ'.
  <fs_atinn>-low    = wa_atinn.

ENDIF.

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

left outer join vbak
on afru~aufnr = vbak~aufnr

left outer join mara
  on afpo~matnr = mara~matnr
 where
       afru~zzlote         in @lr_charg and
       afru~ersda          in @lr_ers_e and
       afru~zzktsch        in @lr_ktsch and
       afru~zztipo_notif   in @lr_tip_n and
       afru~zzlote_corte   in @lr_lo_co and
       afru~zzlote_curvado in @lr_lo_cu and
       afru~zzciclo_tq     in @lr_cic_t and
       afru~zzciclo_auto   in @lr_cic_a and
       afru~zzcod_lanza    in @lr_cod_l and
       afru~zzlote_carga   in @lr_lo_ca and
       afru~stzhl          in @lr_stzhl and
       afru~pernr          in @lr_pernr and
       afru~zzequipo       in @lr_equip and
       afru~stokz          in @lr_stokz and
       afru~werks          in @S_WERKS  and
       afru~aufnr          in @s_aufnr  and

       AUSP~ATINN          in @rng_atinn and
       AUSP~ATWRT          in @lr_atwrt  and
       afru~werks          in @S_WERKS   and
       vbak~bsark          in @lr_bsark  and
       mara~mtart          in @lr_mtart  and
       afru~vornr          in @lr_vornr
 INTO TABLE @it_aufnr.

IF it_aufnr[] IS NOT INITIAL.
      SORT it_aufnr BY AUFNR.
      REFRESH s_aufnr[].
      DELETE ADJACENT DUPLICATES FROM it_aufnr.
      LOOP AT it_aufnr ASSIGNING <FS_AUFNR> WHERE AUFNR IS NOT INITIAL.
      APPEND INITIAL LINE TO s_aufnr ASSIGNING <RNG_AUFNR>.
        <RNG_AUFNR>-sign = 'I'.
        <RNG_AUFNR>-option = 'EQ'.
        <RNG_AUFNR>-low = <FS_AUFNR>-aufnr.
      ENDLOOP.
      DELETE s_aufnr[] WHERE low is INITIAL.
      DELETE ADJACENT DUPLICATES FROM s_aufnr[].
ELSE.
  CHECK 1 = 2. "Se cancela la consulta
ENDIF.

ENDIF.

IF lr_tip_b[] IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'Z_TIPO_BLOQUE'
    IMPORTING
      output = wa_atinn.

  APPEND INITIAL LINE TO rng_atinn ASSIGNING <fs_atinn>.
  <fs_atinn>-sign   = 'I'.
  <fs_atinn>-option = 'EQ'.
  <fs_atinn>-low    = wa_atinn.

select Distinct
       afru~aufnr
from afru
inner join afpo
  on AFRU~aufnr = afpo~aufnr

inner join inob
  on afpo~matnr = INOB~OBJEK

inner join AUSP
    on AUSP~OBJEK = INOB~CUOBJ
       and INOB~KLART = '019'

left outer join vbak
on afru~aufnr = vbak~aufnr

left outer join mara
  on afpo~matnr = mara~matnr
where
       afru~zzlote         in @lr_charg and
       afru~ersda          in @lr_ers_e and
       afru~zzktsch        in @lr_ktsch and
       afru~zztipo_notif   in @lr_tip_n and
       afru~zzlote_corte   in @lr_lo_co and
       afru~zzlote_curvado in @lr_lo_cu and
       afru~zzciclo_tq     in @lr_cic_t and
       afru~zzciclo_auto   in @lr_cic_a and
       afru~zzcod_lanza    in @lr_cod_l and
       afru~zzlote_carga   in @lr_lo_ca and
       afru~stzhl          in @lr_stzhl and
       afru~pernr          in @lr_pernr and
       afru~zzequipo       in @lr_equip and
       afru~stokz          in @lr_stokz and
       afru~werks          in @S_WERKS  and
       afru~aufnr          in @s_aufnr  and

       AUSP~ATINN          in @rng_atinn and
       AUSP~ATWRT          in @lr_atwrt  and
       afru~werks          in @S_WERKS   and
       vbak~bsark          in @lr_bsark  and
       mara~mtart          in @lr_mtart  and
       afru~vornr          in @lr_vornr
 INTO TABLE @it_aufnr.

IF it_aufnr[] IS NOT INITIAL.
      SORT it_aufnr BY AUFNR.
      REFRESH s_aufnr[].
      DELETE ADJACENT DUPLICATES FROM it_aufnr.
      LOOP AT it_aufnr ASSIGNING <FS_AUFNR> WHERE AUFNR IS NOT INITIAL.
      APPEND INITIAL LINE TO s_aufnr ASSIGNING <RNG_AUFNR>.
        <RNG_AUFNR>-sign = 'I'.
        <RNG_AUFNR>-option = 'EQ'.
        <RNG_AUFNR>-low = <FS_AUFNR>-aufnr.
      ENDLOOP.
      DELETE s_aufnr[] WHERE low is INITIAL.
      DELETE ADJACENT DUPLICATES FROM s_aufnr[].
ELSE.
  CHECK 1 = 2. "Se cancela la consulta
ENDIF.
ENDIF.

ENDIF.
"EMAT - Enrique Aguirre - 29.10.2020 - AGP`-921
ENDENHANCEMENT.