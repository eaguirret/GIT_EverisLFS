  METHOD if_ex_workorder_infosystem~detail_list_lay.
    DATA: lt_seltab    TYPE TABLE OF rsparams,
          ls_id_memory TYPE char50,
          lt_data      TYPE REF TO data.
    FIELD-SYMBOLS: <lt_table> TYPE table.

"EMAT - Enrique Aguirre - AGP-921 - Desactivar esta implementacion.
    "CHECK  1 = 2.
"EMAT - Enrique Aguirre - AGP-921 - Desactivar esta implementacion.

    CONCATENATE 'ZCOOIS' sy-uname INTO ls_id_memory.
    IMPORT selname TO lt_seltab FROM MEMORY ID ls_id_memory.

    READ TABLE ct_detail_list WITH KEY object = 'PPIOR000' INTO DATA(wa_ct_detail_list).
    IF sy-subrc EQ 0.
      lt_data = wa_ct_detail_list-table.
      ASSIGN lt_data->* TO <lt_table>.
      DATA: lt_ioconf_tab TYPE ioconf_tab.
      lt_ioconf_tab[] = <lt_table>[].
	  
      LOOP AT lt_ioconf_tab ASSIGNING FIELD-SYMBOL(<fs_table>).
*        AGREGAR VALORES A LOS NUEVOS CAMPOS
*        CONSULTA A LA TABLA AFPO


        SELECT SINGLE kdauf posnr pwerk
          INTO (<fs_table>-zped_clie, <fs_table>-zpos_pedi, <fs_table>-zcentro)
          FROM afpo
          WHERE aufnr EQ <fs_table>-aufnr.
*        CONSULTA A LA TABLA AUFK
        SELECT SINGLE zzcolor zzst_toc auart
          INTO (<fs_table>-zcolor_toc, <fs_table>-zestad_toc, <fs_table>-zcla_orden)
          FROM aufk
          WHERE aufnr EQ <fs_table>-aufnr.
*        CONSULTA A LA TABLA AFKO
        SELECT SINGLE dispo gstrp gltrp cy_seqnr "+@0002-13/11/19
          INTO (<fs_table>-zplani_nec, <fs_table>-zfec_ini_ex, <fs_table>-zfec_fin_ex, <fs_table>-znum_coois) "+@0002-13/11/19
          FROM afko
          WHERE aufnr EQ <fs_table>-aufnr.
		  
		  
		  
		  
        IF <fs_table>-zped_clie IS NOT INITIAL.
*          CONSULTA A LA TABLA VBPA
          SELECT SINGLE parvw
            INTO (<fs_table>-zsolicitante)
            FROM vbpa
            WHERE vbeln EQ <fs_table>-zped_clie
            AND parvw EQ 'AG'.
*          CONSULTA A LA TABLA VBAK
          SELECT SINGLE vkbur submi
            INTO (<fs_table>-zmercado, <fs_table>-zlicitacion)
            FROM vbak
            WHERE vbeln EQ <fs_table>-zped_clie.
*          CONSULTA A LA TABLA VBRP
          SELECT SINGLE vbeln
            INTO @DATA(ls_vbeln)
            FROM vbrp
            WHERE aubel EQ @<fs_table>-zped_clie.
          IF sy-subrc EQ 0 AND ls_vbeln IS NOT INITIAL.
*            CONSULTA A LA TABLA ZTVBRK
            SELECT SINGLE fsalida fllegada awb
              INTO (<fs_table>-zfecha_etd, <fs_table>-zfecha_eta, <fs_table>-ztracking_number)
              FROM ztvbrk
              WHERE factamerica EQ ls_vbeln.
*            CONSULTA A LA TABLA VBRK
            SELECT SINGLE fkdat
              INTO (<fs_table>-zfec_factura)
              FROM vbrk
              WHERE vbeln EQ ls_vbeln.
          ENDIF.
*          CONSULTA A LA TABLA VBKD
          SELECT SINGLE bstdk inco1
            INTO (<fs_table>-zfec_referente, <fs_table>-zincoterms)
            FROM vbkd
            WHERE vbeln EQ <fs_table>-zped_clie.
        ENDIF.
		
		
		
		
*        CONSULTA A LA TABLA CAUFV
        SELECT SINGLE ftrmi
          INTO (<fs_table>-zfecha_lib)
          FROM caufv
          WHERE aufnr EQ <fs_table>-aufnr.
		  
		  
		  
*        CONSULTA A LA TABLA MAKT
        SELECT SINGLE maktx
          INTO (<fs_table>-ztext_mat)
          FROM makt
          WHERE matnr EQ <fs_table>-matnr
          AND spras EQ sy-langu.
*        CONSULTA A LA TABLA MARM
        DATA: ls_matnr_like TYPE mara-matnr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_table>-matnr
          IMPORTING
            output = ls_matnr_like.
        CONCATENATE '%' ls_matnr_like INTO ls_matnr_like.
        SELECT SINGLE umren
          INTO (<fs_table>-zarea_pieza)
          FROM marm
          WHERE matnr EQ ls_matnr_like
          AND meinh EQ 'M2'.
        IF <fs_table>-zarea_pieza IS NOT INITIAL AND <fs_table>-zarea_pieza > 0.
          <fs_table>-zarea_pieza = <fs_table>-zarea_pieza / 100.
        ENDIF.
		
		
*        CONSULTA A LA TABLA INOB 1
        SELECT SINGLE cuobj
          INTO @DATA(ls_cuobj)
          FROM inob
          WHERE objek LIKE @ls_matnr_like
          AND klart EQ '001'.
        IF sy-subrc EQ 0.
          DATA: ls_atinn TYPE ausp-atinn.
          CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
            EXPORTING
              input  = 'Z_AGP_LEVEL'
            IMPORTING
              output = ls_atinn.
*          CONSULTA A LA TABLA AUSP 1
          SELECT SINGLE atwrt
            INTO (<fs_table>-znivel_agp)
            FROM ausp
            WHERE objek EQ ls_cuobj
            AND atinn EQ ls_atinn.

          CLEAR: ls_atinn.
          CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
            EXPORTING
              input  = 'Z_PIECE_TYPE'
            IMPORTING
              output = ls_atinn.
*          CONSULTA A LA TABLA AUSP 2
          SELECT SINGLE atwrt
            INTO @DATA(ls_atwrt)
            FROM ausp
            WHERE objek EQ @ls_cuobj
            AND atinn EQ @ls_atinn.
          IF sy-subrc EQ 0.
*            CONSULTA A LA TABLA CAWN
            SELECT SINGLE atzhl
              INTO @DATA(ls_atzhl)
              FROM cawn
              WHERE atwrt EQ @ls_atwrt
              AND atinn EQ @ls_atinn.
            IF sy-subrc EQ 0.
*              CONSULTA A LA TABLA CAWNT
              SELECT SINGLE atwtb
                INTO (<fs_table>-ztipo_bloque)
                FROM cawnt
                WHERE atzhl EQ ls_atzhl
                AND atinn EQ ls_atinn
                AND spras EQ sy-langu.
            ENDIF.
          ENDIF.

          CLEAR: ls_atinn.
          CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
            EXPORTING
              input  = 'Z_BALLISTIC_STANDARD'
            IMPORTING
              output = ls_atinn.
*          CONSULTA A LA TABLA AUSP 2
          CLEAR: ls_atwrt.
          SELECT SINGLE atwrt
            INTO ls_atwrt
            FROM ausp
            WHERE objek EQ ls_cuobj
            AND atinn EQ ls_atinn.
          IF sy-subrc EQ 0.
*            CONSULTA A LA TABLA CAWN
            CLEAR: ls_atzhl.
            SELECT SINGLE atzhl
              INTO ls_atzhl
              FROM cawn
              WHERE atwrt EQ ls_atwrt
              AND atinn EQ ls_atinn.
            IF sy-subrc EQ 0.
*              CONSULTA A LA TABLA CAWNT
              SELECT SINGLE atwtb
                INTO (<fs_table>-zest_balistico)
                FROM cawnt
                WHERE atzhl EQ ls_atzhl
                AND atinn EQ ls_atinn
                AND spras EQ sy-langu.
            ENDIF.
          ENDIF.

          CLEAR: ls_atinn.
          CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
            EXPORTING
              input  = 'Z_BUSINESS_LINE'
            IMPORTING
              output = ls_atinn.
*          CONSULTA A LA TABLA AUSP 2
          CLEAR: ls_atwrt.
          SELECT SINGLE atwrt
            INTO ls_atwrt
            FROM ausp
            WHERE objek EQ ls_cuobj
            AND atinn EQ ls_atinn.
          IF sy-subrc EQ 0.
*            CONSULTA A LA TABLA CAWN
            CLEAR: ls_atzhl.
            SELECT SINGLE atzhl
              INTO ls_atzhl
              FROM cawn
              WHERE atwrt EQ ls_atwrt
              AND atinn EQ ls_atinn.
            IF sy-subrc EQ 0.
*              CONSULTA A LA TABLA CAWNT
              SELECT SINGLE atwtb
                INTO (<fs_table>-zunid_negocio)
                FROM cawnt
                WHERE atzhl EQ ls_atzhl
                AND atinn EQ ls_atinn
                AND spras EQ sy-langu.
            ENDIF.
          ENDIF.

          CLEAR: ls_atinn.
          CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
            EXPORTING
              input  = 'Z_TRAZABILITY'
            IMPORTING
              output = ls_atinn.
*          CONSULTA A LA TABLA AUSP 2
          CLEAR: ls_atwrt.
          SELECT SINGLE atwrt
            INTO ls_atwrt
            FROM ausp
            WHERE objek EQ ls_cuobj
            AND atinn EQ ls_atinn.
          IF sy-subrc EQ 0.
*            CONSULTA A LA TABLA CAWN
            CLEAR: ls_atzhl.
            SELECT SINGLE atzhl
              INTO ls_atzhl
              FROM cawn
              WHERE atwrt EQ ls_atwrt
              AND atinn EQ ls_atinn.
            IF sy-subrc EQ 0.
*              CONSULTA A LA TABLA CAWNT
              SELECT SINGLE atwtb
                INTO (<fs_table>-ztrazabilidad)
                FROM cawnt
                WHERE atzhl EQ ls_atzhl
                AND atinn EQ ls_atinn
                AND spras EQ sy-langu.
            ENDIF.
          ENDIF.

          CLEAR: ls_atinn.
          CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
            EXPORTING
              input  = 'Z_GEOMETRY_TYPE'
            IMPORTING
              output = ls_atinn.
*          CONSULTA A LA TABLA AUSP 2
          CLEAR: ls_atwrt.
          SELECT SINGLE atwrt
            INTO ls_atwrt
            FROM ausp
            WHERE objek EQ ls_cuobj
            AND atinn EQ ls_atinn.
          IF sy-subrc EQ 0.
*            CONSULTA A LA TABLA CAWN
            CLEAR: ls_atzhl.
            SELECT SINGLE atzhl
              INTO ls_atzhl
              FROM cawn
              WHERE atwrt EQ ls_atwrt
              AND atinn EQ ls_atinn.
            IF sy-subrc EQ 0.
*              CONSULTA A LA TABLA CAWNT
              SELECT SINGLE atwtb
                INTO (<fs_table>-ztipo_geome)
                FROM cawnt
                WHERE atzhl EQ ls_atzhl
                AND atinn EQ ls_atinn
                AND spras EQ sy-langu.
            ENDIF.
          ENDIF.

          CLEAR: ls_atinn.
          CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
            EXPORTING
              input  = 'Z_PRODUCT_TYPE'
            IMPORTING
              output = ls_atinn.
*          CONSULTA A LA TABLA AUSP 2
          CLEAR: ls_atwrt.
          SELECT SINGLE atwrt
            INTO ls_atwrt
            FROM ausp
            WHERE objek EQ ls_cuobj
            AND atinn EQ ls_atinn.
          IF sy-subrc EQ 0.
*            CONSULTA A LA TABLA CAWN
            CLEAR: ls_atzhl.
            SELECT SINGLE atzhl
              INTO ls_atzhl
              FROM cawn
              WHERE atwrt EQ ls_atwrt
              AND atinn EQ ls_atinn.
            IF sy-subrc EQ 0.
*              CONSULTA A LA TABLA CAWNT
              SELECT SINGLE atwtb
                INTO (<fs_table>-ztipo_prod)
                FROM cawnt
                WHERE atzhl EQ ls_atzhl
                AND atinn EQ ls_atinn
                AND spras EQ sy-langu.
            ENDIF.
          ENDIF.
        ENDIF.
        DATA: ls_cen_pt TYPE inob-objek.
        CONCATENATE <fs_table>-zcentro <fs_table>-arbpl INTO ls_cen_pt.
*        CONSULTA A LA TABLA INOB 2
        CLEAR: ls_cuobj.
        SELECT SINGLE cuobj
          INTO ls_cuobj
          FROM inob
          WHERE objek EQ ls_cen_pt
          AND klart EQ '019'.
        IF sy-subrc EQ 0.
          CLEAR: ls_atinn.
          CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
            EXPORTING
              input  = 'Z_TIPO_BLOQUE'
            IMPORTING
              output = ls_atinn.
*          CONSULTA A LA TABLA AUSP
          CLEAR: ls_atwrt.
          SELECT SINGLE atwrt
            INTO ls_atwrt
            FROM ausp
            WHERE objek EQ ls_cuobj
            AND atinn EQ ls_atinn.
          IF sy-subrc EQ 0.
*            CONSULTA A LA TABLA CAWN
            CLEAR: ls_atzhl.
            SELECT SINGLE atzhl
              INTO ls_atzhl
              FROM cawn
              WHERE atwrt EQ ls_atwrt
              AND atinn EQ ls_atinn.
            IF sy-subrc EQ 0.
*              CONSULTA A LA TABLA CAWNT
              SELECT SINGLE atwtb
                INTO (<fs_table>-ztipo_bloque)
                FROM cawnt
                WHERE atzhl EQ ls_atzhl
                AND atinn EQ ls_atinn
                AND spras EQ sy-langu.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

*      RANGOS PARA NUEVO FILTRO
      DATA: lr_charg TYPE RANGE OF afpo-charg,
            wa_charg LIKE LINE OF lr_charg,
            lr_kdauf TYPE RANGE OF afpo-kdauf,
            wa_kdauf LIKE LINE OF lr_kdauf,
            lr_kdpos TYPE RANGE OF afpo-kdpos,
            wa_kdpos LIKE LINE OF lr_kdpos,
            lr_ktsch TYPE RANGE OF afru-zzktsch,
            wa_ktsch LIKE LINE OF lr_ktsch,
            lr_tip_n TYPE RANGE OF afru-zztipo_notif,
            wa_tip_n LIKE LINE OF lr_tip_n,
            lr_lo_co TYPE RANGE OF afru-zzlote_corte,
            wa_lo_co LIKE LINE OF lr_lo_co,
            lr_lo_cu TYPE RANGE OF afru-zzlote_curvado,
            wa_lo_cu LIKE LINE OF lr_lo_cu,
            lr_cic_t TYPE RANGE OF afru-zzciclo_tq,
            wa_cic_t LIKE LINE OF lr_cic_t,
            lr_cic_a TYPE RANGE OF afru-zzciclo_auto,
            wa_cic_a LIKE LINE OF lr_cic_a,
            lr_cod_l TYPE RANGE OF afru-zzcod_lanza,
            wa_cod_l LIKE LINE OF lr_cod_l,
            lr_lo_ca TYPE RANGE OF afru-zzlote_carga,
            wa_lo_ca LIKE LINE OF lr_lo_ca,
            lr_stzhl TYPE RANGE OF afru-stzhl,
            wa_stzhl LIKE LINE OF lr_stzhl,
            lr_pernr TYPE RANGE OF afru-pernr,
            wa_pernr LIKE LINE OF lr_pernr,
            lr_equip TYPE RANGE OF afru-zzequipo,
            wa_equip LIKE LINE OF lr_equip,
            lr_stokz TYPE RANGE OF afru-stokz,
            wa_stokz LIKE LINE OF lr_stokz,
            lr_agp_l TYPE RANGE OF inob-objek,
            wa_agp_l LIKE LINE OF lr_agp_l,
            lr_balli TYPE RANGE OF inob-objek,
            wa_balli LIKE LINE OF lr_balli,
            lr_tip_b TYPE RANGE OF inob-objek,
            wa_tip_b LIKE LINE OF lr_tip_b,
            lr_piece TYPE RANGE OF inob-objek,
            wa_piece LIKE LINE OF lr_piece,
            lr_geome TYPE RANGE OF inob-objek,
            wa_geome LIKE LINE OF lr_geome,
            lr_ers_e TYPE RANGE OF afru-ersda,
            wa_ers_e LIKE LINE OF lr_ers_e,
            lr_ers_a TYPE RANGE OF afru-ersda,
            wa_ers_a LIKE LINE OF lr_ers_a,
            lr_erzet TYPE RANGE OF afru-erzet,
            wa_erzet LIKE LINE OF lr_erzet,
            pp_unoti TYPE checkbox.

      LOOP AT lt_seltab INTO DATA(wa_seltab).
        IF wa_seltab-selname EQ 'PP_UNOTI'.
          pp_unoti = wa_seltab-low.
        ENDIF.
        IF wa_seltab-low IS NOT INITIAL OR wa_seltab-high IS NOT INITIAL.
          CASE wa_seltab-selname.
            WHEN 'SS_CHARG'.
              MOVE-CORRESPONDING wa_seltab TO wa_charg.
              APPEND wa_charg TO lr_charg.
            WHEN 'SS_KDAUF'.
              MOVE-CORRESPONDING wa_seltab TO wa_kdauf.
              APPEND wa_kdauf TO lr_kdauf.
            WHEN 'SS_KDPOS'.
              MOVE-CORRESPONDING wa_seltab TO wa_kdpos.
              APPEND wa_kdpos TO lr_kdpos.
            WHEN 'SS_KTSCH'.
              MOVE-CORRESPONDING wa_seltab TO wa_ktsch.
              APPEND wa_ktsch TO lr_ktsch.
            WHEN 'SS_TIP_N'.
              MOVE-CORRESPONDING wa_seltab TO wa_tip_n.
              APPEND wa_tip_n TO lr_tip_n.
            WHEN 'SS_LO_CO'.
              MOVE-CORRESPONDING wa_seltab TO wa_lo_co.
              APPEND wa_lo_co TO lr_lo_co.
            WHEN 'SS_LO_CU'.
              MOVE-CORRESPONDING wa_seltab TO wa_lo_cu.
              APPEND wa_lo_cu TO lr_lo_cu.
            WHEN 'SS_CIC_T'.
              MOVE-CORRESPONDING wa_seltab TO wa_cic_t.
              APPEND wa_cic_t TO lr_cic_t.
            WHEN 'SS_CIC_A'.
              MOVE-CORRESPONDING wa_seltab TO wa_cic_a.
              APPEND wa_cic_a TO lr_cic_a.
            WHEN 'SS_COD_L'.
              MOVE-CORRESPONDING wa_seltab TO wa_cod_l.
              APPEND wa_cod_l TO lr_cod_l.
            WHEN 'SS_LO_CA'.
              MOVE-CORRESPONDING wa_seltab TO wa_lo_ca.
              APPEND wa_lo_ca TO lr_lo_ca.
            WHEN 'SS_STZHL'.
              MOVE-CORRESPONDING wa_seltab TO wa_stzhl.
              APPEND wa_stzhl TO lr_stzhl.
            WHEN 'SS_PERNR'.
              MOVE-CORRESPONDING wa_seltab TO wa_pernr.
              APPEND wa_pernr TO lr_pernr.
            WHEN 'SS_EQUIP'.
              MOVE-CORRESPONDING wa_seltab TO wa_equip.
              APPEND wa_equip TO lr_equip.
            WHEN 'SS_STOKZ'.
              MOVE-CORRESPONDING wa_seltab TO wa_stokz.
              APPEND wa_stokz TO lr_stokz.
            WHEN 'SS_AGP_L'.
              MOVE-CORRESPONDING wa_seltab TO wa_agp_l.
              APPEND wa_agp_l TO lr_agp_l.
            WHEN 'SS_BALLI'.
              MOVE-CORRESPONDING wa_seltab TO wa_balli.
              APPEND wa_balli TO lr_balli.
            WHEN 'SS_TIP_B'.
              MOVE-CORRESPONDING wa_seltab TO wa_tip_b.
              APPEND wa_tip_b TO lr_tip_b.
            WHEN 'SS_PIECE'.
              MOVE-CORRESPONDING wa_seltab TO wa_piece.
              APPEND wa_piece TO lr_piece.
            WHEN 'SS_GEOME'.
              MOVE-CORRESPONDING wa_seltab TO wa_geome.
              APPEND wa_geome TO lr_geome.
            WHEN 'SS_ERS_E'.
              MOVE-CORRESPONDING wa_seltab TO wa_ers_e.
              APPEND wa_ers_e TO lr_ers_e.
            WHEN 'SS_ERS_A'.
              MOVE-CORRESPONDING wa_seltab TO wa_ers_a.
              APPEND wa_ers_a TO lr_ers_a.
            WHEN 'SS_ERZET'.
              MOVE-CORRESPONDING wa_seltab TO wa_erzet.
              APPEND wa_erzet TO lr_erzet.
          ENDCASE.
        ENDIF.
      ENDLOOP.

      IF lr_charg IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zzlote NOT IN lr_charg.
      ENDIF.
      IF lr_kdauf IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zped_clie NOT IN lr_kdauf.
      ENDIF.
      IF lr_kdpos IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zpos_pedi NOT IN lr_kdpos.
      ENDIF.
      IF lr_ktsch IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zzktsch NOT IN lr_ktsch.
      ENDIF.
      IF lr_tip_n IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zztipo_notif NOT IN lr_tip_n.
      ENDIF.
      IF lr_lo_co IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zzlote_corte NOT IN lr_lo_co.
      ENDIF.
      IF lr_lo_cu IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zzlote_curvado NOT IN lr_lo_cu.
      ENDIF.
      IF lr_cic_t IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zzciclo_tq NOT IN lr_cic_t.
      ENDIF.
      IF lr_cic_a IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zzciclo_auto NOT IN lr_cic_a.
      ENDIF.
      IF lr_cod_l IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zzcod_lanza NOT IN lr_cod_l.
      ENDIF.
      IF lr_lo_ca IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zzlote_carga NOT IN lr_lo_ca.
      ENDIF.
      IF lr_stzhl IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE rmzhl NOT IN lr_stzhl.
      ENDIF.
      IF lr_pernr IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE pernr NOT IN lr_pernr.
      ENDIF.
      IF lr_equip IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zzequipo NOT IN lr_equip.
      ENDIF.
      IF lr_stokz IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE stokz NOT IN lr_stokz.
      ENDIF.
      IF lr_agp_l IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE znivel_agp NOT IN lr_agp_l.
      ENDIF.
      IF lr_balli IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zest_balistico NOT IN lr_balli.
      ENDIF.
      IF lr_tip_b IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE ztipo_bloque NOT IN lr_tip_b.
      ENDIF.
      IF lr_piece IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE ztipo_pieza NOT IN lr_piece.
      ENDIF.
      IF lr_geome IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE ztipo_geome NOT IN lr_geome.
      ENDIF.
      IF lr_ers_e IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zfecha_eta NOT IN lr_ers_e.
      ENDIF.
      IF lr_ers_a IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE zfecha_etd NOT IN lr_ers_a.
      ENDIF.
      IF lr_erzet IS NOT INITIAL.
        DELETE lt_ioconf_tab WHERE erzet NOT IN lr_erzet.
      ENDIF.

      IF pp_unoti EQ 'X'.
        SORT lt_ioconf_tab BY aufnr rueck rmzhl DESCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_ioconf_tab COMPARING aufnr.
        SORT lt_ioconf_tab BY aufnr ASCENDING.
      ENDIF.

      <lt_table>[] = lt_ioconf_tab[].
    ENDIF.
    DELETE FROM MEMORY ID ls_id_memory.
    FREE MEMORY ID ls_id_memory.
  ENDMETHOD.