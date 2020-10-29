*&---------------------------------------------------------------------*
*& Include          ZPPP0002_F01
*&---------------------------------------------------------------------*
***&---------------------------------------------------------------------*
***& Form COMPLETAR_DDL_P_WERKS
***&---------------------------------------------------------------------*
**FORM completar_ddl_p_werks .
**  DATA: lt_t001w TYPE gtyd_t001w WITH HEADER LINE,
**        lt_list  TYPE vrm_values WITH HEADER LINE.
**
**  REFRESH: lt_t001w,
**           lt_list.
**
**
**  SELECT werks
**         name1
**    INTO TABLE lt_t001w
**    FROM t001w
**    WHERE name2 LIKE '%Planta%'.
**
**  LOOP AT lt_t001w.
**    CLEAR: lt_list.
**
**    lt_list-key  = lt_t001w-werks.
**    lt_list-text = lt_t001w-name1.
**
**    APPEND lt_list.
**  ENDLOOP.
**
**  CALL FUNCTION 'VRM_SET_VALUES'
**    EXPORTING
**      id              = 'P_WERKS'
**      values          = lt_list[]
**    EXCEPTIONS
**      id_illegal_name = 1
**      OTHERS          = 2.
**ENDFORM.
***&---------------------------------------------------------------------*
***& Form COMPLETAR_DDL_P_PTRAB
***&---------------------------------------------------------------------*
**FORM completar_ddl_p_ptrab .
**  DATA: lt_crtx TYPE gtyd_crtx  WITH HEADER LINE,
**        lt_list TYPE vrm_values WITH HEADER LINE.
**
**  REFRESH: lt_crtx,
**           lt_list.
**  IF gv_cwerks IS NOT INITIAL.
**    CLEAR: p_ptrab.
**  ENDIF.
**
**  IF p_werks IS NOT INITIAL.
**
**    SELECT crtx~objid
**           crtx~ktext
**      INTO TABLE lt_crtx
**      FROM crhd INNER JOIN crtx ON crhd~objty EQ crtx~objty AND
**                                   crhd~objid EQ crtx~objid
**      WHERE crhd~werks EQ p_werks
**        AND crhd~verwe EQ '0001'
**        AND crhd~lvorm EQ space
**        AND crtx~spras EQ sy-langu.
**
**    LOOP AT lt_crtx.
**      CLEAR: lt_list.
**
**      lt_list-key  = lt_crtx-objid.
**      lt_list-text = lt_crtx-ktext.
**
**      APPEND lt_list.
**    ENDLOOP.
**  ELSE.
**    lt_list-key  = '0000'.
**    lt_list-text = 'Seleccionar un Centro'.
**
**    APPEND lt_list.
**  ENDIF.
**
**  CALL FUNCTION 'VRM_SET_VALUES'
**    EXPORTING
**      id              = 'P_PTRAB'
**      values          = lt_list[]
**    EXCEPTIONS
**      id_illegal_name = 1
**      OTHERS          = 2.
**ENDFORM.
***&---------------------------------------------------------------------*
***& Form ACTUALIZA_P_WERKS
***&---------------------------------------------------------------------*
**FORM actualiza_p_werks .
**  IF gv_werks NE p_werks.
**    gv_cwerks = 'X'.
**  ELSE.
**    CLEAR: gv_cwerks.
**  ENDIF.
**
**  gv_werks = p_werks.
**ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_COMPLETAR_ARBPL_CHAR
*&---------------------------------------------------------------------*
FORM f_completar_arbpl_char .
  DATA: lv_objectkey       TYPE objnum,
        lt_allocvaluesnum  TYPE tt_allocvaluesnum  WITH HEADER LINE,
        lt_allocvalueschar TYPE tt_allocvalueschar WITH HEADER LINE,
        lt_allocvaluescurr TYPE tt_allocvaluescurr WITH HEADER LINE,
        lt_return          TYPE tt_return          WITH HEADER LINE.

  CLEAR:   gv_notif_grupal,
           gv_cod_barra,
           gv_impresora,
           gv_impresora2,
           gv_anul_recha,
           gv_hereda_lote,
           gv_filtro_catag,
           gv_consumo_herra,
           gv_lote_corte,
           gv_deriva_molde,
           gv_codbar_iox,
           gv_status_campo,
           gv_raca_oblig,
           gv_notif_grupal2,
           gv_equipo_oblig,
           gv_codlan_oblig,
           gv_noti_cantidad,
           gv_propuesta_vid,
           gv_lote_dupli,
           gv_hereda_tipnot,
           gv_carac_add_save,
           gv_lote_oblig,
           gv_bloq_notif,
           gv_lote_unico,
           gv_bloq_notif_eg,
           gv_tq_pt_prev,
           gv_valida_eq_mat,
           gv_lcorte_manual,
           gv_gen_ord_nue,
           gv_consumo_lamina,
           gv_rev_mov_herr,
           gv_ver_hora_max,
           gv_add_abierto,
           gv_val_lote_comp,
           gv_val_notif_sec,
           gv_val_uso_herr,
           gv_lote_prov,
           gv_ctrl_peso,
           gv_notif_analisis,                               "+@0006
           gv_vent_prox_ptrab.                              "+@0006
  REFRESH: gt_tipnot,
           gt_print_form,
           gt_lote_auto,
           gt_matkl.

  CONCATENATE p_werks
              p_arbpl
              INTO lv_objectkey.

  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey       = lv_objectkey
      objecttable     = 'CRHD'
      classnum        = 'Z_PUESTO_TRABAJO'
      classtype       = '019'
    TABLES
      allocvaluesnum  = lt_allocvaluesnum[]
      allocvalueschar = lt_allocvalueschar[]
      allocvaluescurr = lt_allocvaluescurr[]
      return          = lt_return[].

  SORT lt_allocvalueschar BY value_neutral ASCENDING.

  LOOP AT lt_allocvalueschar.
    CASE lt_allocvalueschar-charact.
      WHEN 'Z_TIPO_NOTIFICACION'.
        gwa_tipnot-zztipo_notif = lt_allocvalueschar-value_char.

        APPEND gwa_tipnot TO gt_tipnot.
      WHEN 'Z_NOTIF_GRUPAL'.
        gv_notif_grupal = lt_allocvalueschar-value_neutral.
      WHEN 'Z_DEFINE_COD_BARRA'.
        gv_cod_barra = lt_allocvalueschar-value_neutral.
      WHEN 'Z_GENERA_LOTE_AUTO'.
        gwa_lote_auto-value = lt_allocvalueschar-value_neutral.
        APPEND gwa_lote_auto TO gt_lote_auto.
      WHEN 'Z_VISUALIZA_COD_LAN'.
        gv_cod_lan = lt_allocvalueschar-value_neutral.
      WHEN 'Z_IMPRESION_FORMULARIO_AUTO'.
        CLEAR: gwa_print_form.

        gwa_print_form-val  = lt_allocvalueschar-value_neutral.
        gwa_print_form-form = lt_allocvalueschar-value_char.

        APPEND gwa_print_form TO gt_print_form.
      WHEN 'Z_DEFINIR_IMPRESORA'.
        gv_impresora = lt_allocvalueschar-value_char.
      WHEN 'Z_DEFINIR_IMPRESORA2'.
        gv_impresora2 = lt_allocvalueschar-value_char.
      WHEN 'Z_REG_MASIVO_2'.
        gv_reg_masivo = lt_allocvalueschar-value_neutral.
      WHEN 'Z_ANULA_NOTIF_ESP'.
        gv_anul_recha = lt_allocvalueschar-value_neutral.
      WHEN 'Z_HEREDA_LOTE'.
        gv_hereda_lote = lt_allocvalueschar-value_neutral.
      WHEN 'Z_CONTROL_APROBADO'.
        READ TABLE gt_tipnot_oblig ASSIGNING <fs_tipnot_oblig> WITH KEY atzhl = '0001'.
        IF sy-subrc EQ 0.
          <fs_tipnot_oblig>-oblig = lt_allocvalueschar-value_char.
        ENDIF.
      WHEN 'Z_CONTROL_APROBADOPC'.
        READ TABLE gt_tipnot_oblig ASSIGNING <fs_tipnot_oblig> WITH KEY atzhl = '0002'.
        IF sy-subrc EQ 0.
          <fs_tipnot_oblig>-oblig = lt_allocvalueschar-value_char.
        ENDIF.
      WHEN 'Z_CONTROL_APROBADOAM'.
        READ TABLE gt_tipnot_oblig ASSIGNING <fs_tipnot_oblig> WITH KEY atzhl = '0003'.
        IF sy-subrc EQ 0.
          <fs_tipnot_oblig>-oblig = lt_allocvalueschar-value_char.
        ENDIF.
      WHEN 'Z_CONTROL_RECHAZO'.
        READ TABLE gt_tipnot_oblig ASSIGNING <fs_tipnot_oblig> WITH KEY atzhl = '0007'.
        IF sy-subrc EQ 0.
          <fs_tipnot_oblig>-oblig = lt_allocvalueschar-value_char.
        ENDIF.
      WHEN 'Z_CONTROL_REPROCESO'.
        READ TABLE gt_tipnot_oblig ASSIGNING <fs_tipnot_oblig> WITH KEY atzhl = '0005'.
        IF sy-subrc EQ 0.
          <fs_tipnot_oblig>-oblig = lt_allocvalueschar-value_char.
        ENDIF.
      WHEN 'Z_CONTROL_RETRABAJO'.
        READ TABLE gt_tipnot_oblig ASSIGNING <fs_tipnot_oblig> WITH KEY atzhl = '0006'.
        IF sy-subrc EQ 0.
          <fs_tipnot_oblig>-oblig = lt_allocvalueschar-value_char.
        ENDIF.
      WHEN 'Z_CONTROL_ANALISIS'.
        READ TABLE gt_tipnot_oblig ASSIGNING <fs_tipnot_oblig> WITH KEY atzhl = '0004'.
        IF sy-subrc EQ 0.
          <fs_tipnot_oblig>-oblig = lt_allocvalueschar-value_char.
        ENDIF.
      WHEN 'Z_CONTROL_DESGUACE'.
        READ TABLE gt_tipnot_oblig ASSIGNING <fs_tipnot_oblig> WITH KEY atzhl = '0008'.
        IF sy-subrc EQ 0.
          <fs_tipnot_oblig>-oblig = lt_allocvalueschar-value_char.
        ENDIF.
      WHEN 'Z_FILTRO_CATAG_QM'.
        gv_filtro_catag = lt_allocvalueschar-value_neutral.
      WHEN 'Z_CONSUM_HERR'.
        gv_consumo_herra = lt_allocvalueschar-value_neutral.
      WHEN 'Z_LOG_ADIC_LOTE_CORTE'.
        gv_lote_corte = lt_allocvalueschar-value_neutral.
      WHEN 'Z_DERIVA_LOTE_MOLDE'.
        gv_deriva_molde = lt_allocvalueschar-value_neutral.
      WHEN 'Z_COD_BARR_IOX '.
        gv_codbar_iox = lt_allocvalueschar-value_neutral.
      WHEN 'Z_COD_BARR_AVO'.
        gv_codbar_avo = lt_allocvalueschar-value_neutral.
      WHEN 'Z_STATUS_CAMPO_NOTIF'.
        gv_status_campo = lt_allocvalueschar-value_char.
      WHEN 'Z_RACK_CAJA_OBLIG'.
        gv_raca_oblig = lt_allocvalueschar-value_neutral.
      WHEN 'Z_NOTIF_GRUPAL_2'.
        gv_notif_grupal2 = lt_allocvalueschar-value_neutral.
      WHEN 'Z_EQUIPO_OBLIG'.
        gv_equipo_oblig = lt_allocvalueschar-value_neutral.
      WHEN 'Z_COD_LAN_OBLIG'.
        gv_codlan_oblig = lt_allocvalueschar-value_neutral.
      WHEN 'Z_NOTI_CANTIDAD'.
        gv_noti_cantidad = lt_allocvalueschar-value_neutral.
      WHEN 'Z_PROPUESTA_VIDRIOS'.
        gv_propuesta_vid = lt_allocvalueschar-value_neutral.
      WHEN 'Z_LOTE_DUPLID_GRABADO'.
        gv_lote_dupli = lt_allocvalueschar-value_neutral.
      WHEN 'Z_HEREDA_TIPO_NOTIF'.
        gv_hereda_tipnot = lt_allocvalueschar-value_neutral.
      WHEN 'Z_AGREGAR_Y_GRABAR'.
        gv_carac_add_save = lt_allocvalueschar-value_neutral.
      WHEN 'Z_LOTE_OBLIG'.
        gv_lote_oblig = lt_allocvalueschar-value_neutral.
      WHEN 'Z_BLOQ_NOTIF_BRG'.
        gv_bloq_notif = lt_allocvalueschar-value_neutral.
      WHEN 'Z_LOT_UNIC_ORDEN'.
        gv_lote_unico = lt_allocvalueschar-value_neutral.
      WHEN 'Z_BLOQ_NOTIF_EGLASS'.
        gv_bloq_notif_eg = lt_allocvalueschar-value_neutral.
      WHEN 'Z_CICLO_TQ_PT_PREV'.
        gv_tq_pt_prev = lt_allocvalueschar-value_neutral.
      WHEN 'Z_VALIDA_EQ_MAT'.
        gv_valida_eq_mat = lt_allocvalueschar-value_neutral.
      WHEN 'Z_LOT_CORTE_MANUAL'.
        gv_lcorte_manual = lt_allocvalueschar-value_neutral.
      WHEN 'Z_GENERA_ORDEN_NUEVA'.
        gv_gen_ord_nue = lt_allocvalueschar-value_neutral.
      WHEN 'Z_CONSUMO_LAMINA'.
        gv_consumo_lamina = lt_allocvalueschar-value_neutral.
      WHEN 'Z_REVERSA_MOV_HERR'.
        gv_rev_mov_herr = lt_allocvalueschar-value_neutral.
      WHEN 'Z_VERIFICA_HORAS_MAX'.
        gv_ver_hora_max = lt_allocvalueschar-value_neutral.
      WHEN 'Z_AGREGAR_ABIERTO'.
        gv_add_abierto = lt_allocvalueschar-value_neutral.
      WHEN 'Z_BORRAR_LOTE_COMP'.
        gwa_matkl-matkl = lt_allocvalueschar-value_neutral.

        APPEND gwa_matkl TO gt_matkl.
      WHEN 'Z_VALIDA_LOTE_COMP'.
        gv_val_lote_comp = lt_allocvalueschar-value_neutral.
      WHEN 'Z_VALIDA_NOTIF_SEC'.
        gv_val_notif_sec = lt_allocvalueschar-value_neutral.
      WHEN 'Z_VALIDA_USO_HERR'.
        gv_val_uso_herr = lt_allocvalueschar-value_neutral.
      WHEN 'Z_LOTE_PROVEEDOR'.
        gv_lote_prov = lt_allocvalueschar-value_neutral.
      WHEN 'Z_CONTROL_PESO_BAL'.
        gv_ctrl_peso = lt_allocvalueschar-value_neutral.
*{+@0006
      WHEN 'Z_NOTIF_ANALISIS'.
        gv_notif_analisis = lt_allocvalueschar-value_neutral.
      WHEN 'Z_VENTANA_PROX_PTRAB'.
        gv_vent_prox_ptrab = lt_allocvalueschar-value_neutral.
*}+@0006
    ENDCASE.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form ELIMINAR_ORDENES_NOTIFICADAS
*&---------------------------------------------------------------------*
                                                            "{ +@0004
FORM eliminar_ordenes_notificadas.
  " Eliminar ordenes que ya se encuetran notificadas
  DATA: lr_aufnr    TYPE RANGE OF afko-aufnr.
  DATA: lwa_aufnr   LIKE LINE OF lr_aufnr.
  DATA: ls_orden    TYPE string.

  " Eliminar de los componentes
  LOOP AT gt_ordenes_delete INTO ls_orden.
    READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY aufnr = ls_orden.
    DELETE gt_componentes WHERE aufnr = gwa_ordenes-aufnr AND
                                ktsch = gwa_ordenes-ktsch AND
                                vornr = gwa_ordenes-vornr.
  ENDLOOP.

  LOOP AT gt_ordenes_delete INTO ls_orden.
    CLEAR: lwa_aufnr.
    lwa_aufnr-sign = 'I'.
    lwa_aufnr-option = 'EQ'.
    lwa_aufnr-low = ls_orden.
    APPEND lwa_aufnr TO  lr_aufnr.
  ENDLOOP.
  DELETE gt_ordenes WHERE aufnr IN lr_aufnr.

  " Ordenar las ordenes nro
  LOOP AT gt_ordenes ASSIGNING FIELD-SYMBOL(<lfs_ordenes>).
    <lfs_ordenes>-nro = sy-tabix.
    gv_nro = <lfs_ordenes>-nro.
  ENDLOOP.

  CALL METHOD gr_grid01->refresh_table_display
    EXPORTING
      is_stable = gc_stable.
  CALL METHOD gr_grid02->refresh_table_display
    EXPORTING
      is_stable = gc_stable.
ENDFORM.
                                                            "{ +@0004

*&---------------------------------------------------------------------*
*& Form F_COMPLETAR_CABECERA
*&---------------------------------------------------------------------*
FORM f_completar_cabecera .
  DATA: lt_pa0001 TYPE tt_pa0001 WITH HEADER LINE.

  CLEAR: gv_sname.
  REFRESH: lt_pa0001.

  SELECT begda
         ename
         sname
    INTO TABLE lt_pa0001
    FROM pa0001
    WHERE pernr EQ p_pernr.
  CHECK sy-subrc EQ 0.

  SORT lt_pa0001 BY begda DESCENDING.
  READ TABLE lt_pa0001 INDEX 1.

  gv_sname = lt_pa0001-sname.

  gv_add_abierto_new = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_INICIALIZAR
*&---------------------------------------------------------------------*
FORM f_inicializar .
  DATA: lr_tipnot   LIKE LINE OF gr_tipnot_aprob,
        lwa_celltab TYPE lvc_s_styl,
        lv_atinn    TYPE atinn.

  CLEAR:   gv_addreg,
           gv_mod_tipnot,
           lv_atinn,
           gv_nro,
           gv_aprob,
           gv_recha,
           gv_anali,
           gv_repro,
           gv_retra.
  REFRESH: gt_ordenes,
           gt_componentes,
           gt_comp_dele,
           gt_log,
           gt_cawnt,
           gt_ordenes_del,
           gr_tipnot_aprob,
           gr_tipnot_recha,
           gr_tipnot_retra,
           gr_tipnot_anali,
           gt_celltab_aprob,
           gt_celltab_recha,
           gt_celltab_xchpf,
           gt_celltab_molde,
           gt_lot_vidrio,
           gt_celltab_lcorte,
           gt_lot_vidrio_aux,
           gt_lot_vidrio_2,
           gt_lot_vidrio_aux_2.
*           gt_tipnot_oblig.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'Z_TIPO_NOTIFICACION'
    IMPORTING
      output = lv_atinn.

  SELECT *
    INTO TABLE gt_cawnt
    FROM cawnt
    WHERE atinn EQ lv_atinn
      AND spras EQ sy-langu.

  lr_tipnot-sign   = 'I'.
  lr_tipnot-option = 'EQ'.
  LOOP AT gt_cawnt INTO gwa_cawnt.
    lr_tipnot-low = gwa_cawnt-atwtb.
    CASE gwa_cawnt-atzhl.
      WHEN '0001'.
        gv_aprob = lr_tipnot-low.
        APPEND lr_tipnot TO gr_tipnot_aprob.
      WHEN '0002'.
        gv_aprob_pc = lr_tipnot-low.
        APPEND lr_tipnot TO gr_tipnot_aprob.
      WHEN '0003'.
        gv_aprob_am = lr_tipnot-low.
        APPEND lr_tipnot TO gr_tipnot_aprob.
      WHEN '0004'.
        gv_anali = lr_tipnot-low.
        APPEND lr_tipnot TO gr_tipnot_anali.
      WHEN '0005'.
        gv_repro = lr_tipnot-low.
        APPEND lr_tipnot TO gr_tipnot_retra.
      WHEN '0006'.
        gv_retra = lr_tipnot-low.
        APPEND lr_tipnot TO gr_tipnot_retra.
      WHEN '0007'.
        gv_recha = lr_tipnot-low.
        APPEND lr_tipnot TO gr_tipnot_recha.
      WHEN '0008'.
        APPEND lr_tipnot TO gr_tipnot_recha.
    ENDCASE.

*    gwa_tipnot_oblig-atwtb = gwa_cawnt-atwtb.
*    APPEND gwa_tipnot_oblig TO gt_tipnot_oblig.
  ENDLOOP.

  IF gt_tipnot_oblig IS INITIAL.
    LOOP AT gt_cawnt INTO gwa_cawnt.
      gwa_tipnot_oblig-atwtb = gwa_cawnt-atwtb.
      gwa_tipnot_oblig-atzhl = gwa_cawnt-atzhl.
      APPEND gwa_tipnot_oblig TO gt_tipnot_oblig.
    ENDLOOP.
  ENDIF.

  "Columnas editables de tabla ordenes
  lwa_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
  lwa_celltab-fieldname = 'ZZDEFECTO'.
  INSERT lwa_celltab INTO TABLE gt_celltab_recha.
  lwa_celltab-fieldname = 'ZZDEFCOD'.
  INSERT lwa_celltab INTO TABLE gt_celltab_recha.
  lwa_celltab-fieldname = 'ZZORIGEN'.
  INSERT lwa_celltab INTO TABLE gt_celltab_recha.
  lwa_celltab-fieldname = 'ZZORCOD'.
  INSERT lwa_celltab INTO TABLE gt_celltab_recha.
  lwa_celltab-fieldname = 'ZZCAUSA'.
  INSERT lwa_celltab INTO TABLE gt_celltab_recha.
  lwa_celltab-fieldname = 'ZZCAUCOD'.
  INSERT lwa_celltab INTO TABLE gt_celltab_recha.
  lwa_celltab-fieldname = 'ZZDESTINO'.
  INSERT lwa_celltab INTO TABLE gt_celltab_recha.
  lwa_celltab-fieldname = 'GRUND'.
  INSERT lwa_celltab INTO TABLE gt_celltab_recha.
  lwa_celltab-fieldname = 'ZZEQUIPO'.
  INSERT lwa_celltab INTO TABLE gt_celltab_recha.

  lwa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  lwa_celltab-fieldname = 'ZZDEFECTO'.
  INSERT lwa_celltab INTO TABLE gt_celltab_aprob.
  lwa_celltab-fieldname = 'ZZDEFCOD'.
  INSERT lwa_celltab INTO TABLE gt_celltab_aprob.
  lwa_celltab-fieldname = 'ZZORIGEN'.
  INSERT lwa_celltab INTO TABLE gt_celltab_aprob.
  lwa_celltab-fieldname = 'ZZORCOD'.
  INSERT lwa_celltab INTO TABLE gt_celltab_aprob.
  lwa_celltab-fieldname = 'ZZCAUSA'.
  INSERT lwa_celltab INTO TABLE gt_celltab_aprob.
  lwa_celltab-fieldname = 'ZZCAUCOD'.
  INSERT lwa_celltab INTO TABLE gt_celltab_aprob.
  lwa_celltab-fieldname = 'ZZDESTINO'.
  INSERT lwa_celltab INTO TABLE gt_celltab_aprob.
  lwa_celltab-fieldname = 'GRUND'.
  INSERT lwa_celltab INTO TABLE gt_celltab_aprob.
  lwa_celltab-fieldname = 'ZZEQUIPO'.
  INSERT lwa_celltab INTO TABLE gt_celltab_aprob.

  lwa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  lwa_celltab-fieldname = 'BUDAT'.
  INSERT lwa_celltab INTO TABLE gt_celltab_molde.
  lwa_celltab-fieldname = 'ZZEQUIPO'.
  INSERT lwa_celltab INTO TABLE gt_celltab_molde.
  lwa_celltab-fieldname = 'ZZTIPO_NOTIF'.
  INSERT lwa_celltab INTO TABLE gt_celltab_molde.
  lwa_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
  lwa_celltab-fieldname = 'NRO_MOLDE'.
  INSERT lwa_celltab INTO TABLE gt_celltab_molde.

*{-LAF | Definición de lógica de cálculo de lotes para consumo de componentes en órdenes de producción
*  "Edición componente lote
  lwa_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
  lwa_celltab-fieldname = 'LOTE'.
  INSERT lwa_celltab INTO TABLE gt_celltab_xchpf.
  IF gv_lote_prov NE '0'.
    lwa_celltab-fieldname = 'LICHA'.
    INSERT lwa_celltab INTO TABLE gt_celltab_xchpf.
  ENDIF.
*}-LAF | Definición de lógica de cálculo de lotes para consumo de componentes en órdenes de producción

  lwa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  lwa_celltab-fieldname = 'LOTE'.
  INSERT lwa_celltab INTO TABLE gt_celltab_lcorte.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_FCAT_TAB01
*&---------------------------------------------------------------------*
FORM f_fill_fcat_tab01.
  DATA: lt_dropdown TYPE lvc_t_drop WITH HEADER LINE.

  REFRESH: gt_fcat_tab01,
           lt_dropdown.

  LOOP AT gt_tipnot INTO gwa_tipnot.
    CLEAR lt_dropdown.

    lt_dropdown-handle = '1'.
    lt_dropdown-value  = gwa_tipnot-zztipo_notif.

    APPEND lt_dropdown.
  ENDLOOP.

  gr_grid01->set_drop_down_table(
    it_drop_down = lt_dropdown[] ).

  PERFORM f_fill_fcat
    USING:
      gt_fcat_tab01  'NRO'    TEXT-x01  space  space  space   space    space  space  space,
      gt_fcat_tab01  'AUFNR'  TEXT-x02  'X'    space  space   space    space  space  space,
      gt_fcat_tab01  'VORNR'  TEXT-x03  space  space  space   space    space  space  space.
  IF gv_val_notif_sec EQ '1'.
    PERFORM f_fill_fcat
      USING:
        gt_fcat_tab01  'KTSCH'       TEXT-x04  space  space  space   space    space  space  space,
        gt_fcat_tab01  'ARBPL_PROX'  TEXT-x05  space  space  space   space    space  space  space,
        gt_fcat_tab01  'KTSCH_PROX'  TEXT-x06  space  space  space   space    space  space  space.
  ELSE.
    PERFORM f_fill_fcat
      USING:
        gt_fcat_tab01  'KTSCH'  TEXT-x38  space  space  space   space    space  space  space.
  ENDIF.
  PERFORM f_fill_fcat
    USING:
      gt_fcat_tab01  'BUDAT'           TEXT-x07  space  'X'    'AFRU'  'BUDAT'  space  space  space,
      gt_fcat_tab01  'PLNFL'           TEXT-x08  space  space  space   space    space  space  space,
      gt_fcat_tab01  'ZZEQUIPO'        TEXT-x09  'X'    'X'    space   space    space  space  'X',
      gt_fcat_tab01  'ZZLOTE'          TEXT-x10  space  space  space   space    space  space  space,
      gt_fcat_tab01  'PLNBEZ'          TEXT-x11  'X'    space  space   space    space  space  space,
      gt_fcat_tab01  'MAKTX'           TEXT-x12  space  space  space   space    space  space  space,
      gt_fcat_tab01  'GMEIN_TXT'       TEXT-x13  space  space  space   space    space  space  space,
      gt_fcat_tab01  'LMNGA'           TEXT-x14  space  space  space   space    space  space  space,
      gt_fcat_tab01  'ZZTIPO_NOTIF'    TEXT-x15  space  'X'    space   space    space  space  space,
      gt_fcat_tab01  'ZZLOTE_CORTE'    TEXT-x16  space  space  space   space    space  space  space,
      gt_fcat_tab01  'ZZLOTE_CURVADO'  TEXT-x17  space  space  space   space    space  space  space,
      gt_fcat_tab01  'ZZCICLO_TQ'      TEXT-x18  space  space  space   space    space  space  space,
      gt_fcat_tab01  'ZZCICLO_AUTO'    TEXT-x19  space  space  space   space    space  space  space,
      gt_fcat_tab01  'ZZCOD_LANZA'     TEXT-x20  space  space  space   space    space  space  space,
      gt_fcat_tab01  'ZZLOTE_CARGA'    TEXT-x21  space  space  space   space    space  space  space,
      gt_fcat_tab01  'ZZDEFECTO'       TEXT-x22  space  space  space   space    space  space  'X',
      gt_fcat_tab01  'ZZDEFCOD'        TEXT-x23  space  space  space   space    space  space  'X',
      gt_fcat_tab01  'ZZORIGEN'        TEXT-x24  space  space  space   space    space  space  'X',
      gt_fcat_tab01  'ZZORCOD'         TEXT-x25  space  space  space   space    space  space  'X',
      gt_fcat_tab01  'ZZCAUSA'         TEXT-x26  space  space  space   space    space  space  'X',
      gt_fcat_tab01  'ZZCAUCOD'        TEXT-x27  space  space  space   space    space  space  'X',
      gt_fcat_tab01  'ZZDESTINO'       TEXT-x28  space  space  space   space    space  space  'X',
      gt_fcat_tab01  'GRUND'           TEXT-x29  space  space  space   space    space  space  'X',
      gt_fcat_tab01  'CAM_HORNO'       TEXT-x30  space  space  space   space    space  space  space,
      gt_fcat_tab01  'NRO_CAJA'        TEXT-x31  space  space  space   space    space  space  space,
      gt_fcat_tab01  'NRO_RACK'        TEXT-x32  space  space  space   space    space  space  space,
      gt_fcat_tab01  'NRO_MOLDE'       TEXT-x33  space  space  space   space    space  space  space,
      gt_fcat_tab01  'NO_MATERIAL'     TEXT-x34  space  space  space   space    space  space  space,
      gt_fcat_tab01  'NO_ACTIVIDAD'    TEXT-x35  space  space  space   space    space  space  space,
      gt_fcat_tab01  'ANUL_RECHA'      TEXT-x36  space  space  space   space    space  space  space,
      gt_fcat_tab01  'LTXA1'           TEXT-x37  space  space  space   space    space  space  space.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_FCAT_TAB02
*&---------------------------------------------------------------------*
FORM f_fill_fcat_tab02 .
  REFRESH: gt_fcat_tab02.

  PERFORM f_fill_fcat
    USING:
      gt_fcat_tab02  'NRO'          TEXT-x01  space  space  space                      space            4   space  space,
      gt_fcat_tab02  'AUFNR'        TEXT-x02  'X'    space  space                      space            12  space  space,
      gt_fcat_tab02  'VORNR'        TEXT-x03  space  space  space                      space            8   space  space,
      gt_fcat_tab02  'KTSCH'        TEXT-x38  space  space  space                      space            8   space  space,
      gt_fcat_tab02  'POSNR'        TEXT-x39  space  space  space                      space            8   space  space,
      gt_fcat_tab02  'COMPONENTE'   TEXT-x40  'X'    space  space                      space            10  space  space,
      gt_fcat_tab02  'DESCRIPCION'  TEXT-x12  space  space  space                      space            25  space  space,
*{-LAF | Definición de lógica de cálculo de lotes para consumo de componentes en órdenes de producción
*      gt_fcat_tab02  'CANTIDAD'     'Cantidad'       space  'X'    'BAPI2017_GM_ITEM_CREATE'  'ENTRY_QNT'      15  space  space,
*      gt_fcat_tab02  'UM_TXT'       'UM'             space  'X'    space                      space            5   space  'X',
*      gt_fcat_tab02  'ALMACEN'      'Almacén'        space  'X'    space                      space            10  space  'X',
*}-LAF | Definición de lógica de cálculo de lotes para consumo de componentes en órdenes de producción
      gt_fcat_tab02  'CANTIDAD'     TEXT-x14  space  space  'BAPI2017_GM_ITEM_CREATE'  'ENTRY_QNT'      15  space  space,
      gt_fcat_tab02  'UM_TXT'       TEXT-x13  space  space  space                      space            5   space  'X',
      gt_fcat_tab02  'ALMACEN'      TEXT-x41  space  space  space                      space            10  space  'X',
      gt_fcat_tab02  'LICHA'        TEXT-x42  space  space  space                      space            15  space  space,
      gt_fcat_tab02  'LOTE'         TEXT-x10  space  space  space                      space            10  space  'X',
      gt_fcat_tab02  'BWART'        TEXT-x43  space  space  space                      space            10  space  space,
      gt_fcat_tab02  'MOD'          TEXT-x44  space  space  space                      space            8   space  space.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_FCAT_LOG
*&---------------------------------------------------------------------*
FORM f_fill_fcat_log .
  REFRESH: gt_fcat_log.

  PERFORM f_fill_fcat
    USING:
      gt_fcat_log  'NRO'    TEXT-x01  space  space  space  space  space  'C'    space,
      gt_fcat_log  'AUFNR'  TEXT-x02  'X'    space  space  space  space  space  space,
      gt_fcat_log  'VORNR'  TEXT-x03  'X'    space  space  space  space  space  space,
      gt_fcat_log  'NOTIF'  TEXT-x45  space  space  space  space  space  'C'    space,
      gt_fcat_log  'LOTE'   TEXT-x46  space  space  space  space  space  'C'    space,
      gt_fcat_log  'HERRA'  TEXT-x47  space  space  space  space  space  'C'    space,
      gt_fcat_log  'COMP'   TEXT-x48  'X'    space  space  space  space  space  space,
      gt_fcat_log  'MSG'    TEXT-x49  space  space  space  space  space  space  space.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_FCAT_0600
*&---------------------------------------------------------------------*
FORM f_fill_fcat_0600 .
  REFRESH: gt_fcat_0600.

  PERFORM f_fill_fcat
    USING:
      gt_fcat_0600  'LAMINA'  TEXT-x50  space  space  space  space  space  space  space,
      gt_fcat_0600  'CHARG'   TEXT-x51  space  'X'    space  space  space  space  space.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_FCAT_0601
*&---------------------------------------------------------------------*
FORM f_fill_fcat_0601 .
  REFRESH: gt_fcat_0601.

  PERFORM f_fill_fcat
    USING:
      gt_fcat_0601  'MATNR'  TEXT-x11  'X'    space  space  space  10  space  space,
      gt_fcat_0601  'MAKTX'  TEXT-x12  space  space  space  space  30  space  space,
      gt_fcat_0601  'CANT'   TEXT-x52  space  space  space  space  6   'C'    space,
      gt_fcat_0601  'ERFMG'  TEXT-x14  space  space  space  space  8   space  space,
      gt_fcat_0601  'MSEH6'  TEXT-x13  space  space  space   space 5   'C'    space,
      gt_fcat_0601  'CHARG'  TEXT-x10  space  'X'    space  space  12  space  'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_FCAT
*&---------------------------------------------------------------------*
FORM f_fill_fcat USING p_fcat TYPE lvc_t_fcat
                       p_fieldname
                       p_scrtext_m
                       p_no_zero
                       p_edit
                       p_ref_table
                       p_ref_field
                       p_outputlen
                       p_just
                       p_f4availabl.
  DATA: lwa_fcat TYPE lvc_s_fcat.

  CLEAR: lwa_fcat.

  lwa_fcat-fieldname = p_fieldname.
  lwa_fcat-scrtext_m = p_scrtext_m.
  lwa_fcat-no_zero   = p_no_zero.
  lwa_fcat-edit      = p_edit.
  lwa_fcat-ref_table = p_ref_table.
  lwa_fcat-ref_field = p_ref_field.
  lwa_fcat-outputlen = p_outputlen.
  lwa_fcat-just      = p_just.
  lwa_fcat-f4availabl = p_f4availabl.

  "Casuísticas particulares:
  CASE p_fieldname.
    WHEN 'ZZTIPO_NOTIF'.
      lwa_fcat-drdn_hndl = '1'.
    WHEN 'NO_MATERIAL'  OR
         'NO_ACTIVIDAD' OR
         'ANUL_RECHA'   OR
         'MOD'.
      lwa_fcat-checkbox = 'X'.
  ENDCASE.

  APPEND lwa_fcat TO p_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_REG
*&---------------------------------------------------------------------*
FORM f_add_reg .
  CLEAR: zzclavemodelo, gv_check. "@0009 - Cristian Castro - 11.06.2020 - AGP-391 - Insert
  DATA: lt_list     TYPE vrm_values WITH HEADER LINE.

  CLEAR:   zppe_notif_addreg.
  REFRESH: lt_list.

  READ TABLE gt_tipnot INTO gwa_tipnot INDEX 1.
  zppe_notif_addreg-werks        = p_werks.
  zppe_notif_addreg-budat        = sy-datum.
  zppe_notif_addreg-zztipo_notif = gwa_tipnot-zztipo_notif.
  zppe_notif_addreg-lmnga        = 1.

  "Cámara de Horno
  lt_list-key  = TEXT-063."'IZQUIERDO'.
  lt_list-text = TEXT-063."'IZQUIERDO'.
  APPEND lt_list.

  lt_list-key  = TEXT-064."'DERECHO'.
  lt_list-text = TEXT-064."'DERECHO'.
  APPEND lt_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'ZPPE_NOTIF_ADDREG-CAM_HORNO'
      values          = lt_list[]
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  CALL SCREEN 0200 STARTING AT 30  1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB01
*&---------------------------------------------------------------------*
FORM f_add_tab01 .
  DATA: lv_error TYPE c.

  CLEAR: lv_error,
         gv_borrar_cant.

  PERFORM f_validar_flag_obligatorios CHANGING lv_error.
  CHECK lv_error IS INITIAL.

  IF gv_edit IS NOT INITIAL.
    PERFORM f_edit_tab01 CHANGING lv_error.
  ELSEIF zppe_notif_addreg-id IS NOT INITIAL.
    PERFORM f_add_tab01_id CHANGING lv_error.
    "Ingresar lote de corte
  ELSEIF zppe_notif_addreg-usr00 IS NOT INITIAL.
    PERFORM f_add_tab01_usr00 CHANGING lv_error.
    "Ingresar lote de curvado
  ELSEIF zppe_notif_addreg-zzlote_curvado IS NOT INITIAL.
    PERFORM f_add_tab01_zzlote_curvado CHANGING lv_error.
    "Ingresar ciclo TQ
  ELSEIF zppe_notif_addreg-zzciclo_tq IS NOT INITIAL.
    IF gv_tq_pt_prev EQ '1'.
      PERFORM f_add_tab01_zzciclo_tq_pt_prev CHANGING lv_error.
    ELSE.
      PERFORM f_add_tab01_zzciclo_tq CHANGING lv_error.
    ENDIF.
    "Ingresar ciclo Autoclave
  ELSEIF zppe_notif_addreg-zzciclo_auto IS NOT INITIAL.
    PERFORM f_add_tab01_zzciclo_auto CHANGING lv_error.
    "Ingresar orden
  ELSEIF zppe_notif_addreg-aufnr IS NOT INITIAL.
    IF zppe_notif_addreg-ktsch IS NOT INITIAL OR
       zppe_notif_addreg-vornr IS NOT INITIAL.
      PERFORM f_add_tab01_aufnr CHANGING lv_error.
    ELSE.
      MESSAGE TEXT-012 TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF lv_error IS INITIAL.
    IF gv_edit IS NOT INITIAL.
      MESSAGE TEXT-029 TYPE 'S'.

      LEAVE TO SCREEN 0.
    ELSE.
      MESSAGE TEXT-003 TYPE 'S'.

      IF gv_reg_masivo EQ '1'.
        CLEAR: zppe_notif_addreg-charg.



        CALL METHOD gr_grid01->refresh_table_display
          EXPORTING
            is_stable = gc_stable.
        CALL METHOD gr_grid02->refresh_table_display
          EXPORTING
            is_stable = gc_stable.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB01_USR00
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_add_tab01_usr00 CHANGING p_error.
  DATA: lt_data  TYPE tt_data_lcorte WITH HEADER LINE,
        lt_makt  TYPE tt_makt        WITH HEADER LINE,
        lt_t006a TYPE tt_t006a       WITH HEADER LINE,
        lv_nro   TYPE i,
        lv_txt   TYPE string.

  REFRESH: lt_data,
           lt_makt,
           lt_t006a.

  IF p_werks NE 'PE01' AND
     p_werks NE 'BR01' AND
     p_werks NE 'CO01'.
    MESSAGE TEXT-002 TYPE 'S'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  IF zppe_notif_addreg-budat IS INITIAL.
    MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  SELECT  afko~aufnr
          afvc~vornr
          afvc~ktsch
          afvc~plnfl
          afko~plnbez
          afko~gmein
          afpo~charg
          afvc~steus
          afvu~aufpl
          afvu~aplzl
    INTO TABLE lt_data
    FROM afvu INNER JOIN afko ON afvu~aufpl EQ afko~aufpl
              INNER JOIN aufk ON afko~aufnr EQ aufk~aufnr
              INNER JOIN afpo ON afko~aufnr EQ afpo~aufnr
              INNER JOIN afvc ON afvu~aufpl EQ afvc~aufpl AND
                                 afvu~aplzl EQ afvc~aplzl
    WHERE afvu~usr00 EQ zppe_notif_addreg-usr00
      AND afvu~usr01 EQ 'X'
      AND aufk~werks EQ p_werks
    ORDER BY afko~aufnr ASCENDING
             afvc~vornr ASCENDING.

  "Validar orden liberada
  SELECT aufk~aufnr
    INTO TABLE @DATA(lt_aufnr)
    FROM aufk INNER JOIN jest ON aufk~objnr EQ jest~objnr
    FOR ALL ENTRIES IN @lt_data
    WHERE aufk~aufnr EQ @lt_data-aufnr
      AND jest~stat  EQ 'I0002'
      AND jest~inact EQ @space.
  LOOP AT lt_data.
    READ TABLE lt_aufnr TRANSPORTING NO FIELDS WITH KEY aufnr = lt_data-aufnr.
    IF sy-subrc NE 0.
      lv_txt = TEXT-080.
      REPLACE '&' IN lv_txt WITH zppe_notif_addreg-usr00.
      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
  CHECK p_error IS INITIAL.

  IF lt_data[] IS NOT INITIAL.
    SELECT matnr
           maktx
      INTO TABLE lt_makt
      FROM makt
      FOR ALL ENTRIES IN lt_data
      WHERE matnr EQ lt_data-plnbez
        AND spras EQ sy-langu.

    READ TABLE gt_tipnot INTO gwa_tipnot INDEX 1.

    SELECT msehi
           mseh6
      INTO TABLE lt_t006a
      FROM t006a
      WHERE spras EQ sy-langu.
  ENDIF.

  LOOP AT lt_data.
    CLEAR: gwa_ordenes,
           lt_makt,
           lt_t006a.

    lv_nro = gv_nro.
    ADD 1 TO gv_nro.
    READ TABLE lt_makt  WITH KEY matnr = lt_data-plnbez.
    READ TABLE lt_t006a WITH KEY msehi = lt_data-gmein.

    "Nro.
    gwa_ordenes-nro = gv_nro.

    "Orden
    gwa_ordenes-aufnr = lt_data-aufnr.

    "Operación
    gwa_ordenes-vornr = lt_data-vornr.

    "Clave Modelo
    gwa_ordenes-ktsch = lt_data-ktsch.

    "Fecha contabilización
    gwa_ordenes-budat = zppe_notif_addreg-budat.

    "Secuencia
    gwa_ordenes-plnfl = lt_data-plnfl.

    "Equipo
    gwa_ordenes-zzequipo = zppe_notif_addreg-equnr.

    "Lote
    gwa_ordenes-zzlote = lt_data-charg.

    "Material
    gwa_ordenes-plnbez = lt_data-plnbez.

    "Descripción
    gwa_ordenes-maktx = lt_makt-maktx.

    "UM
    gwa_ordenes-gmein     = lt_data-gmein.
    gwa_ordenes-gmein_txt = lt_t006a-mseh6.

    "Cantidad
    gwa_ordenes-lmnga = 1.

    "Tipo de Notificación
    gwa_ordenes-zztipo_notif = gwa_tipnot-zztipo_notif.

    "Lote de Corte
    gwa_ordenes-zzlote_corte = zppe_notif_addreg-usr00.

    "Lote de Curvado
*    gwa_ordenes-ZZLOTE_CURVADO.

    "Ciclo de TQ
*    gwa_ordenes-ZZCICLO_TQ.

    "Ciclo de Autoclave
*    gwa_ordenes-ZZCICLO_AUTO.

    "Código de Lanzada
*    gwa_ordenes-ZZCOD_LANZA.

    "Lote de Carga
*    gwa_ordenes-ZZLOTE_CARGA.

    "Defecto
*    gwa_ordenes-DESC_DEFCOD.

    "Código Defecto
*    gwa_ordenes-ZZDEFCOD.

    "Origen
*    gwa_ordenes-DESC_ORIGEN.

    "Código Origen
*    gwa_ordenes-ZZORIGEN.

    "Causa
*    gwa_ordenes-DESC_AUSA.

    "Código causa
*    gwa_ordenes-ZZCAUSA.

    "Destino
*    gwa_ordenes-ZZDESTINO.

    "Causa de Desviación
*    gwa_ordenes-GRUND.

    "Sin consumo de materiales
    gwa_ordenes-no_material = zppe_notif_addreg-no_material.

    "Sin consumo de actividades
    gwa_ordenes-no_actividad = zppe_notif_addreg-no_actividad.

    "Desperdicio Operario
    gwa_ordenes-zzdesperop   = zppe_notif_addreg-zzdesperop. "Insert @0002
    "Adicionales
    gwa_ordenes-aufpl = lt_data-aufpl.
    gwa_ordenes-aplzl = lt_data-aplzl.

    APPEND gwa_ordenes TO gt_ordenes.
  ENDLOOP.

  IF sy-subrc EQ 0.
    PERFORM f_completar_componente_mas.

    IF gv_consumo_lamina EQ '1'.
      LOOP AT gt_componentes ASSIGNING <fs_componentes>.
        CLEAR: <fs_componentes>-licha,
               <fs_componentes>-lote.

        <fs_componentes>-celltab = gt_celltab_lcorte.
      ENDLOOP.
    ENDIF.

    SORT gt_ordenes BY nro ASCENDING.
*                       aufnr ASCENDING
*                       vornr ASCENDING.

    PERFORM f_asignar_lote_vidrio USING space.

    CALL METHOD gr_grid01->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
    gv_addreg = '1'.
  ELSE.
    p_error = 'X'.

    MESSAGE TEXT-013 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_COMPLETAR_COMPONENTE
*&---------------------------------------------------------------------*
FORM f_completar_componente USING pi_nro
                                  pi_aufnr
                                  pi_vornr
                                  pi_plnfl
                                  pi_ktsch
                                  pi_zztipo_notif
                                  pi_cant
                                  pi_batch.
  DATA: lt_timetickets     TYPE tt_timetickets     WITH HEADER LINE,
        lt_goodsmovements  TYPE tt_goodsmovements  WITH HEADER LINE,
        lt_goodsmaux       TYPE tt_goodsmovements  WITH HEADER LINE,
        lt_makt            TYPE tt_makt            WITH HEADER LINE,
        lt_aux             TYPE tt_makt            WITH HEADER LINE,
        lt_t006a           TYPE tt_t006a           WITH HEADER LINE,
        lt_mara            TYPE tt_mara            WITH HEADER LINE,
        lt_consum_herr     TYPE tt_consum_herr     WITH HEADER LINE,
        lt_allocvaluesnum  TYPE tt_allocvaluesnum  WITH HEADER LINE,
        lt_allocvalueschar TYPE tt_allocvalueschar WITH HEADER LINE,
        lt_allocvaluescurr TYPE tt_allocvaluescurr WITH HEADER LINE,
        lt_return          TYPE tt_return          WITH HEADER LINE,
        lt_mch1            TYPE tt_mch1            WITH HEADER LINE,
        lwa_propose        TYPE bapi_pp_conf_prop,
        lwa_component      TYPE zppe_notif_component,
        lv_lmnga           TYPE lmnga,
        lv_xmnga           TYPE xmnga,
        lv_rmnga           TYPE rmnga,
        lv_class           TYPE klasse_d,
        lv_klart           TYPE klassenart,
        lv_obtab           TYPE tabelle,
        lv_objectkey       TYPE objnum,
        lv_fltp            TYPE cha_class_data-sollwert,
        lv_char            TYPE cha_class_view-sollwert,
        lv_nro             TYPE i,
        lv_cant            TYPE i,
        lv_loop            TYPE i,
        lv_aux             TYPE i,
        lv_tabix           TYPE sytabix.
  FIELD-SYMBOLS: <fs_consum_herr> TYPE ty_consum_herr.

  CLEAR:   lt_timetickets,
           lwa_propose,
           lt_aux,
           lv_lmnga,
           lv_xmnga,
           lv_rmnga.
  REFRESH: lt_timetickets,
           lt_goodsmovements,
           lt_t006a,
           lt_mara,
           lt_goodsmaux,
           lt_consum_herr,
           lt_mch1.

  "Sin componentes
  CHECK zppe_notif_addreg-no_material IS INITIAL.

*{-@0005
*  IF gv_noti_cantidad             EQ '1' AND
*     zppe_notif_addreg-anul_recha NE 'X'.
*}-@0005
  IF gv_noti_cantidad EQ '1'.
    lv_cant = pi_cant.
    lv_loop = 1.
  ELSE.
    lv_cant = 1.
    lv_loop = pi_cant.
  ENDIF.

  IF pi_zztipo_notif IN gr_tipnot_aprob.
    lv_lmnga = lv_cant.
  ELSEIF pi_zztipo_notif IN gr_tipnot_recha.
    lv_xmnga = lv_cant.
  ELSEIF pi_zztipo_notif IN gr_tipnot_retra.
    lv_rmnga = lv_cant.
  ELSEIF pi_zztipo_notif IN gr_tipnot_anali.
  ENDIF.

  lwa_propose = ' XXXX'.

  lt_timetickets-orderid   = pi_aufnr.
  lt_timetickets-operation = pi_vornr.
  lt_timetickets-sequence  = pi_plnfl.
  lt_timetickets-yield     = lv_lmnga.
  lt_timetickets-scrap     = lv_xmnga.
  lt_timetickets-rework    = lv_rmnga.

  APPEND lt_timetickets.

  CALL FUNCTION 'BAPI_PRODORDCONF_GET_TT_PROP'
    EXPORTING
      propose        = lwa_propose
    TABLES
      timetickets    = lt_timetickets
      goodsmovements = lt_goodsmovements.

  CHECK sy-subrc EQ 0.

  LOOP AT lt_goodsmovements.
    lt_aux-matnr = lt_goodsmovements-material.
    APPEND lt_aux.
  ENDLOOP.

  CHECK sy-subrc EQ 0.

  SELECT matnr
         maktx
    INTO TABLE lt_makt
    FROM makt
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr
      AND spras EQ sy-langu.

  SELECT msehi
         mseh6
    INTO TABLE lt_t006a
    FROM t006a
    WHERE spras EQ sy-langu.

  SELECT *
    INTO TABLE lt_mara
    FROM mara
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr.

  SELECT matnr
         werks
         lgort
         charg
    INTO TABLE lt_consum_herr
    FROM mchb
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr
      AND werks EQ p_werks
      AND clabs GT 0.

  SELECT *
    INTO TABLE lt_mch1
    FROM mch1
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr.

  IF gv_consumo_herra EQ '1'.
    lt_goodsmaux[] = lt_goodsmovements[].
    DELETE lt_goodsmaux WHERE batch IS INITIAL.
    SORT lt_goodsmaux BY material ASCENDING
                         batch    ASCENDING.
    LOOP AT lt_goodsmaux.
      READ TABLE lt_mara  WITH KEY matnr = lt_goodsmaux-material.
      IF lt_mara-mtart NE 'ZHER'.
        DELETE lt_goodsmaux WHERE material EQ lt_goodsmaux-material.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_consum_herr ASSIGNING <fs_consum_herr>.
      READ TABLE lt_mara  WITH KEY matnr = <fs_consum_herr>-matnr
                                   mtart = 'ZHER'.
      IF sy-subrc EQ 0.
        CLEAR:   lv_class,
                 lv_klart,
                 lv_obtab,
                 lv_objectkey.
        REFRESH: lt_allocvaluesnum,
                 lt_allocvalueschar,
                 lt_allocvaluescurr,
                 lt_return.
        CALL FUNCTION 'QMSP_MATERIAL_BATCH_CLASS_READ'
          EXPORTING
            i_matnr                = <fs_consum_herr>-matnr
            i_charg                = <fs_consum_herr>-charg
            i_werks                = <fs_consum_herr>-werks
            i_mara_level           = 'X'
          IMPORTING
            e_class                = lv_class
            e_klart                = lv_klart
            e_obtab                = lv_obtab
          EXCEPTIONS
            no_class               = 1
            internal_error_classif = 2
            no_change_service      = 3
            OTHERS                 = 4.

        CONCATENATE <fs_consum_herr>-matnr
                    <fs_consum_herr>-charg
                    INTO lv_objectkey.

        CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
          EXPORTING
            objectkey        = lv_objectkey
            objecttable      = lv_obtab
            classnum         = lv_class
            classtype        = lv_klart
            unvaluated_chars = 'X'
          TABLES
            allocvaluesnum   = lt_allocvaluesnum[]
            allocvalueschar  = lt_allocvalueschar[]
            allocvaluescurr  = lt_allocvaluescurr[]
            return           = lt_return[].

        READ TABLE lt_allocvaluesnum WITH KEY charact = 'LOBM_LWEDT'.
        IF sy-subrc EQ 0.
          lv_fltp = lt_allocvaluesnum-value_from.
          CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
            EXPORTING
              i_number_of_digits       = '0'
              i_fltp_value             = lv_fltp
              i_value_not_initial_flag = 'X'
              i_screen_fieldlength     = 16
            IMPORTING
              e_char_field             = lv_char.
          CONDENSE lv_char.
          <fs_consum_herr>-date = lv_char.
        ENDIF.
      ELSE.
        DELETE lt_consum_herr WHERE matnr EQ <fs_consum_herr>-matnr
                                AND lgort EQ <fs_consum_herr>-lgort.
      ENDIF.
    ENDLOOP.
    DELETE lt_consum_herr WHERE date IS INITIAL.

    SORT lt_consum_herr BY date ASCENDING.
  ENDIF.

  lv_nro = pi_nro.
  DO lv_loop TIMES.
    CLEAR: lv_aux.

    ADD 1 TO lv_nro.
    LOOP AT lt_goodsmovements.
      CLEAR: gwa_componentes,
             lt_makt,
             lt_t006a,
             lt_mara,
             lt_goodsmaux,
             lt_mch1.

      READ TABLE lt_makt      WITH KEY matnr    = lt_goodsmovements-material.
      READ TABLE lt_t006a     WITH KEY msehi    = lt_goodsmovements-entry_uom.
      READ TABLE lt_mara      WITH KEY matnr    = lt_goodsmovements-material.
      READ TABLE lt_goodsmaux WITH KEY material = lt_goodsmovements-material.
      READ TABLE lt_mch1      WITH KEY matnr    = lt_goodsmovements-material
                                       charg    = lt_goodsmovements-batch.

      "Consumo de herramental
      IF gv_consumo_herra   EQ '1'                     AND
         lt_mara-mtart      EQ 'ZHER'                  AND
         lt_goodsmaux-batch NE lt_goodsmovements-batch.
        CONTINUE.
      ENDIF.

      "Nro.
      IF gv_edit IS NOT INITIAL.
        gwa_componentes-nro = gv_edit.
      ELSE.
        gwa_componentes-nro = lv_nro.
      ENDIF.

      "Auxiliar
      ADD 1 TO lv_aux.
      gwa_componentes-aux = lv_aux.

      "Orden
      gwa_componentes-aufnr = pi_aufnr.

      "Operación
      gwa_componentes-vornr = pi_vornr.

      "Clave Modelo
      gwa_componentes-ktsch = pi_ktsch.

      "Posición en lista de materiales
      gwa_componentes-posnr = lt_goodsmovements-deliv_item+2(4).

      "Componente
      gwa_componentes-componente = lt_goodsmovements-material.

      "Descripción
      gwa_componentes-descripcion = lt_makt-maktx.

      "Cantidad
      IF gv_consumo_herra EQ '1' AND
         lt_mara-mtart    EQ 'ZHER'.
        gwa_componentes-cantidad = 1.
      ELSE.
        gwa_componentes-cantidad = lt_goodsmovements-entry_qnt.
      ENDIF.

      "UM
      gwa_componentes-um     = lt_goodsmovements-entry_uom.
      gwa_componentes-um_txt = lt_t006a-mseh6.

      "Almacén
      gwa_componentes-almacen = lt_goodsmovements-stge_loc.

      "Lote
      gwa_componentes-lote = lt_goodsmovements-batch.
      IF gv_consumo_herra        EQ '1'     AND
         lt_mara-mtart           EQ 'ZHER'  AND
         lt_goodsmovements-batch IS INITIAL.
        READ TABLE lt_consum_herr WITH KEY matnr = lt_goodsmovements-material
                                           lgort = lt_goodsmovements-stge_loc.
        IF sy-subrc EQ 0.
          gwa_componentes-lote = lt_consum_herr-charg.
        ENDIF.
      ENDIF.
      IF lt_goodsmovements-move_type EQ '101' AND
         pi_batch                    IS NOT INITIAL.
        gwa_componentes-lote = pi_batch.
      ENDIF.
      IF gv_deriva_molde EQ '1'    AND
         lt_mara-mtart   EQ 'ZHER'.
        gwa_componentes-lote = zppe_notif_addreg-nro_molde.
      ENDIF.

      "Clase de movimiento
      gwa_componentes-bwart = lt_goodsmovements-move_type.

      "Lote Proveedor
      gwa_componentes-licha = lt_mch1-licha.

      "Indicador de movimiento
      gwa_componentes-mvt_ind = lt_goodsmovements-mvt_ind.

      "Indicador de stock especial
      gwa_componentes-spec_stock = lt_goodsmovements-spec_stock.

      "Cliente
      gwa_componentes-customer = lt_goodsmovements-customer.

      "Pedido
      gwa_componentes-sales_ord = lt_goodsmovements-sales_ord.

      "Posición
      gwa_componentes-s_ord_item = lt_goodsmovements-s_ord_item.

      "Num Reserva
      gwa_componentes-reserv_no = lt_goodsmovements-reserv_no.

      "Pos Reserva
      gwa_componentes-res_item = lt_goodsmovements-res_item.

      IF gt_matkl IS NOT INITIAL.
        READ TABLE gt_matkl TRANSPORTING NO FIELDS WITH KEY matkl = lt_mara-matkl.
        IF sy-subrc EQ 0.
          CLEAR: gwa_componentes-lote,
                 gwa_componentes-licha.
          gwa_componentes-mod = 'X'.
          "LAF
          READ TABLE gt_componentes INTO DATA(lwa_componentes_aux) WITH KEY nro        = gwa_componentes-nro
                                                                            componente = gwa_componentes-componente
                                                                            posnr      = gwa_componentes-posnr.
          IF sy-subrc EQ 0.
            lv_tabix = sy-tabix.

            ADD lwa_componentes_aux-cantidad TO gwa_componentes-cantidad.

            DELETE gt_componentes INDEX lv_tabix.
          ENDIF.
          "LAF
        ENDIF.
      ENDIF.

      "Celltab
      CASE lt_mara-xchpf.
        WHEN 'X'.
          gwa_componentes-celltab = gt_celltab_xchpf.
      ENDCASE.

      APPEND gwa_componentes TO gt_componentes.
    ENDLOOP.
  ENDDO.
  SORT gt_componentes BY nro   ASCENDING
                         posnr ASCENDING.
*                         aux ASCENDING.
*                         aufnr      ASCENDING
*                         vornr      ASCENDING
*                         componente ASCENDING.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_COMPLETAR_COMPONENTE_GRUPAL
*&---------------------------------------------------------------------*
FORM f_completar_componente_grupal USING pi_nro
                                         pi_zztipo_notif
                                         pi_batch
                                         pi_data TYPE tt_notif_grupal.
  DATA: lt_timetickets    TYPE tt_timetickets     WITH HEADER LINE,
        lt_goodsmovements TYPE tt_goodsmovements  WITH HEADER LINE,
        lt_link           TYPE tt_link            WITH HEADER LINE,
        lt_makt           TYPE tt_makt            WITH HEADER LINE,
        lt_aux            TYPE tt_makt            WITH HEADER LINE,
        lt_t006a          TYPE tt_t006a           WITH HEADER LINE,
        lt_mara           TYPE tt_mara            WITH HEADER LINE,
        lt_mch1           TYPE tt_mch1            WITH HEADER LINE,
        lwa_propose       TYPE bapi_pp_conf_prop,
        lwa_data          TYPE ty_notif_grupal,
        lv_lmnga          TYPE lmnga,
        lv_xmnga          TYPE xmnga,
        lv_rmnga          TYPE rmnga,
        lv_nro            TYPE i,
        lv_aux            TYPE i,
        lv_tabix          TYPE sytabix.

  CLEAR:   lt_timetickets,
           lwa_propose,
           lt_aux,
           lv_lmnga,
           lv_xmnga,
           lv_rmnga.
  REFRESH: lt_timetickets,
           lt_goodsmovements,
           lt_link,
           lt_t006a,
           lt_mara,
           lt_mch1.

  "Sin componentes
  CHECK zppe_notif_addreg-no_material IS INITIAL.

  IF pi_zztipo_notif IN gr_tipnot_aprob.
    lv_lmnga = 1.
  ELSEIF pi_zztipo_notif IN gr_tipnot_recha.
    lv_xmnga = 1.
  ELSEIF pi_zztipo_notif IN gr_tipnot_retra.
    lv_rmnga = 1.
  ELSEIF pi_zztipo_notif IN gr_tipnot_anali.
    EXIT.
  ENDIF.

  lwa_propose = ' XXXX'.

  LOOP AT pi_data INTO lwa_data.
    lt_timetickets-orderid   = lwa_data-aufnr.
    lt_timetickets-operation = lwa_data-vornr.
    lt_timetickets-sequence  = lwa_data-plnfl.
    lt_timetickets-yield     = lv_lmnga.
    lt_timetickets-scrap     = lv_xmnga.
    lt_timetickets-rework    = lv_rmnga.

    APPEND lt_timetickets.
  ENDLOOP.

  CALL FUNCTION 'BAPI_PRODORDCONF_GET_TT_PROP'
    EXPORTING
      propose            = lwa_propose
    TABLES
      timetickets        = lt_timetickets
      goodsmovements     = lt_goodsmovements
      link_conf_goodsmov = lt_link.
  CHECK sy-subrc EQ 0.

  LOOP AT lt_goodsmovements.
    lt_aux-matnr = lt_goodsmovements-material.
    APPEND lt_aux.
  ENDLOOP.
  CHECK sy-subrc EQ 0.

  SELECT matnr
         maktx
    INTO TABLE lt_makt
    FROM makt
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr
      AND spras EQ sy-langu.

  SELECT msehi
         mseh6
    INTO TABLE lt_t006a
    FROM t006a
    WHERE spras EQ sy-langu.

  SELECT *
    INTO TABLE lt_mara
    FROM mara
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr.

  SELECT *
    INTO TABLE lt_mch1
    FROM mch1
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr.

  "CANT > 1 no va
  lv_nro = pi_nro.

  CLEAR: lv_aux.

  SORT lt_link BY index_confirm  ASCENDING
                  index_goodsmov ASCENDING.
  LOOP AT pi_data INTO lwa_data.
    lv_tabix = sy-tabix.
    ADD 1 TO lv_nro.
    LOOP AT lt_link WHERE index_confirm = lv_tabix.
      READ TABLE lt_goodsmovements INDEX lt_link-index_goodsmov.
      CHECK sy-subrc EQ 0.

      CLEAR: gwa_componentes,
             lt_makt,
             lt_t006a,
             lt_mara,
             lt_mch1.

      READ TABLE lt_makt      WITH KEY matnr    = lt_goodsmovements-material.
      READ TABLE lt_t006a     WITH KEY msehi    = lt_goodsmovements-entry_uom.
      READ TABLE lt_mara      WITH KEY matnr    = lt_goodsmovements-material.
      READ TABLE lt_mch1      WITH KEY matnr    = lt_goodsmovements-material
                                       charg    = lt_goodsmovements-batch.

      "Nro.
      IF gv_edit IS NOT INITIAL.
        gwa_componentes-nro = gv_edit.
      ELSE.
        gwa_componentes-nro = lv_nro.
      ENDIF.

      "Auxiliar
      ADD 1 TO lv_aux.
      gwa_componentes-aux = lv_aux.

      "Orden
      gwa_componentes-aufnr = lwa_data-aufnr.

      "Operación
      gwa_componentes-vornr = lwa_data-vornr.

      "Clave Modelo
      gwa_componentes-ktsch = lwa_data-ktsch.

      "Posición en lista de materiales
      gwa_componentes-posnr = lt_goodsmovements-deliv_item+2(4).

      "Componente
      gwa_componentes-componente = lt_goodsmovements-material.

      "Descripción
      gwa_componentes-descripcion = lt_makt-maktx.

      "Cantidad
      gwa_componentes-cantidad = lt_goodsmovements-entry_qnt.

      "UM
      gwa_componentes-um     = lt_goodsmovements-entry_uom.
      gwa_componentes-um_txt = lt_t006a-mseh6.

      "Almacén
      gwa_componentes-almacen = lt_goodsmovements-stge_loc.

      "Lote
      gwa_componentes-lote = lt_goodsmovements-batch.
      IF lt_goodsmovements-move_type EQ '101' AND
         pi_batch                    IS NOT INITIAL.
        gwa_componentes-lote = pi_batch.
      ENDIF.

      "Clase de movimiento
      gwa_componentes-bwart = lt_goodsmovements-move_type.

      "Lote Proveedor
      gwa_componentes-licha = lt_mch1-licha.

      "Indicador de movimiento
      gwa_componentes-mvt_ind = lt_goodsmovements-mvt_ind.

      "Indicador de stock especial
      gwa_componentes-spec_stock = lt_goodsmovements-spec_stock.

      "Cliente
      gwa_componentes-customer = lt_goodsmovements-customer.

      "Pedido
      gwa_componentes-sales_ord = lt_goodsmovements-sales_ord.

      "Posición
      gwa_componentes-s_ord_item = lt_goodsmovements-s_ord_item.

      "Num Reserva
      gwa_componentes-reserv_no = lt_goodsmovements-reserv_no.

      "Pos Reserva
      gwa_componentes-res_item = lt_goodsmovements-res_item.

      IF gt_matkl IS NOT INITIAL.
        READ TABLE gt_matkl TRANSPORTING NO FIELDS WITH KEY matkl = lt_mara-matkl.
        IF sy-subrc EQ 0.
          CLEAR: gwa_componentes-lote,
                 gwa_componentes-licha.
          gwa_componentes-mod = 'X'.

*LAF
*          READ TABLE gt_componentes INTO DATA(lwa_componentes_aux) WITH KEY nro        = gwa_componentes-nro
*                                                                            componente = gwa_componentes-componente
*                                                                            posnr      = gwa_componentes-posnr.
*          IF sy-subrc EQ 0.
*            lv_tabix = sy-tabix.
*
*            ADD lwa_componentes_aux-cantidad TO gwa_componentes-cantidad.
*
*            DELETE gt_componentes INDEX lv_tabix.
*          ENDIF.
*LAF
        ENDIF.
      ENDIF.

      "Celltab
      CASE lt_mara-xchpf.
        WHEN 'X'.
          gwa_componentes-celltab = gt_celltab_xchpf.
      ENDCASE.

      APPEND gwa_componentes TO gt_componentes.
    ENDLOOP.
  ENDLOOP.
  SORT gt_componentes BY nro   ASCENDING
                         posnr ASCENDING.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_COMPLETAR_COMPONENTE_MAS
*&---------------------------------------------------------------------*
FORM f_completar_componente_mas.
  DATA: lt_timetickets     TYPE tt_timetickets     WITH HEADER LINE,
        lt_goodsmovements  TYPE tt_goodsmovements  WITH HEADER LINE,
        lt_link            TYPE tt_link            WITH HEADER LINE,
        lt_goodsmaux       TYPE tt_goodsmovements  WITH HEADER LINE,
        lt_makt            TYPE tt_makt            WITH HEADER LINE,
        lt_aux             TYPE tt_makt            WITH HEADER LINE,
        lt_t006a           TYPE tt_t006a           WITH HEADER LINE,
        lt_mara            TYPE tt_mara            WITH HEADER LINE,
        lt_consum_herr     TYPE tt_consum_herr     WITH HEADER LINE,
        lt_allocvaluesnum  TYPE tt_allocvaluesnum  WITH HEADER LINE,
        lt_allocvalueschar TYPE tt_allocvalueschar WITH HEADER LINE,
        lt_allocvaluescurr TYPE tt_allocvaluescurr WITH HEADER LINE,
        lt_return          TYPE tt_return          WITH HEADER LINE,
        lt_mch1            TYPE tt_mch1            WITH HEADER LINE,
        lwa_propose        TYPE bapi_pp_conf_prop,
        lwa_component      TYPE zppe_notif_component,
        lv_lmnga           TYPE lmnga,
        lv_xmnga           TYPE xmnga,
        lv_rmnga           TYPE rmnga,
        lv_class           TYPE klasse_d,
        lv_klart           TYPE klassenart,
        lv_obtab           TYPE tabelle,
        lv_objectkey       TYPE objnum,
        lv_fltp            TYPE cha_class_data-sollwert,
        lv_char            TYPE cha_class_view-sollwert,
        lv_nro             TYPE i,
        lv_cant            TYPE i,
        lv_loop            TYPE i,
        lv_tabix           TYPE sytabix,
        lv_tabix2          TYPE sytabix.
  FIELD-SYMBOLS: <fs_consum_herr> TYPE ty_consum_herr.

  CLEAR:   lt_timetickets,
           lwa_propose,
           lt_aux,
           lv_lmnga,
           lv_xmnga,
           lv_rmnga.
  REFRESH: lt_timetickets,
           lt_goodsmovements,
           lt_link,
           lt_t006a,
           lt_mara,
           lt_goodsmaux,
           lt_consum_herr,
           lt_mch1.

  "Sin componentes
  CHECK zppe_notif_addreg-no_material IS INITIAL.

  lwa_propose = ' XXXX'.

  LOOP AT gt_ordenes INTO gwa_ordenes.
    lt_timetickets-orderid   = gwa_ordenes-aufnr.
    lt_timetickets-operation = gwa_ordenes-vornr.
    lt_timetickets-sequence  = gwa_ordenes-plnfl.
    lt_timetickets-yield     = gwa_ordenes-lmnga.

    APPEND lt_timetickets.
  ENDLOOP.

  CALL FUNCTION 'BAPI_PRODORDCONF_GET_TT_PROP'
    EXPORTING
      propose            = lwa_propose
    TABLES
      timetickets        = lt_timetickets
      goodsmovements     = lt_goodsmovements
      link_conf_goodsmov = lt_link.

  CHECK sy-subrc EQ 0.

  LOOP AT lt_goodsmovements.
    lt_aux-matnr = lt_goodsmovements-material.
    APPEND lt_aux.
  ENDLOOP.

  CHECK sy-subrc EQ 0.

  SELECT matnr
         maktx
    INTO TABLE lt_makt
    FROM makt
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr
      AND spras EQ sy-langu.

  SELECT msehi
         mseh6
    INTO TABLE lt_t006a
    FROM t006a
    WHERE spras EQ sy-langu.

  SELECT *
    INTO TABLE lt_mara
    FROM mara
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr.

  SELECT matnr
         werks
         lgort
         charg
    INTO TABLE lt_consum_herr
    FROM mchb
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr
      AND werks EQ p_werks
      AND clabs GT 0.

  SELECT *
    INTO TABLE lt_mch1
    FROM mch1
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr.

  IF gv_consumo_herra EQ '1'.
    lt_goodsmaux[] = lt_goodsmovements[].
    DELETE lt_goodsmaux WHERE batch IS INITIAL.
    SORT lt_goodsmaux BY material ASCENDING
                         batch    ASCENDING.
    LOOP AT lt_goodsmaux.
      READ TABLE lt_mara  WITH KEY matnr = lt_goodsmaux-material.
      IF lt_mara-mtart NE 'ZHER'.
        DELETE lt_goodsmaux WHERE material EQ lt_goodsmaux-material.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_consum_herr ASSIGNING <fs_consum_herr>.
      READ TABLE lt_mara  WITH KEY matnr = <fs_consum_herr>-matnr
                                   mtart = 'ZHER'.
      IF sy-subrc EQ 0.
        CLEAR:   lv_class,
                 lv_klart,
                 lv_obtab,
                 lv_objectkey.
        REFRESH: lt_allocvaluesnum,
                 lt_allocvalueschar,
                 lt_allocvaluescurr,
                 lt_return.
        CALL FUNCTION 'QMSP_MATERIAL_BATCH_CLASS_READ'
          EXPORTING
            i_matnr                = <fs_consum_herr>-matnr
            i_charg                = <fs_consum_herr>-charg
            i_werks                = <fs_consum_herr>-werks
            i_mara_level           = 'X'
          IMPORTING
            e_class                = lv_class
            e_klart                = lv_klart
            e_obtab                = lv_obtab
          EXCEPTIONS
            no_class               = 1
            internal_error_classif = 2
            no_change_service      = 3
            OTHERS                 = 4.

        CONCATENATE <fs_consum_herr>-matnr
                    <fs_consum_herr>-charg
                    INTO lv_objectkey.

        CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
          EXPORTING
            objectkey        = lv_objectkey
            objecttable      = lv_obtab
            classnum         = lv_class
            classtype        = lv_klart
            unvaluated_chars = 'X'
          TABLES
            allocvaluesnum   = lt_allocvaluesnum[]
            allocvalueschar  = lt_allocvalueschar[]
            allocvaluescurr  = lt_allocvaluescurr[]
            return           = lt_return[].

        READ TABLE lt_allocvaluesnum WITH KEY charact = 'LOBM_LWEDT'.
        IF sy-subrc EQ 0.
          lv_fltp = lt_allocvaluesnum-value_from.
          CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
            EXPORTING
              i_number_of_digits       = '0'
              i_fltp_value             = lv_fltp
              i_value_not_initial_flag = 'X'
              i_screen_fieldlength     = 16
            IMPORTING
              e_char_field             = lv_char.
          CONDENSE lv_char.
          <fs_consum_herr>-date = lv_char.
        ENDIF.
      ELSE.
        DELETE lt_consum_herr WHERE matnr EQ <fs_consum_herr>-matnr
                                AND lgort EQ <fs_consum_herr>-lgort.
      ENDIF.
    ENDLOOP.
    DELETE lt_consum_herr WHERE date IS INITIAL.

    SORT lt_consum_herr BY date ASCENDING.
  ENDIF.

  SORT lt_link BY index_confirm  ASCENDING
                  index_goodsmov ASCENDING.
  LOOP AT gt_ordenes INTO gwa_ordenes.
    lv_tabix = sy-tabix.
    LOOP AT lt_link WHERE index_confirm = lv_tabix.
      READ TABLE lt_goodsmovements INDEX lt_link-index_goodsmov.
      CHECK sy-subrc EQ 0.

      CLEAR: gwa_componentes,
             lt_makt,
             lt_t006a,
             lt_mara,
             lt_goodsmaux,
             lt_mch1.

*      AT NEW index_confirm.
*        ADD 1 TO lv_nro.
*      ENDAT.

      READ TABLE lt_makt      WITH KEY matnr    = lt_goodsmovements-material.
      READ TABLE lt_t006a     WITH KEY msehi    = lt_goodsmovements-entry_uom.
      READ TABLE lt_mara      WITH KEY matnr    = lt_goodsmovements-material.
      READ TABLE lt_goodsmaux WITH KEY material = lt_goodsmovements-material.
      READ TABLE lt_mch1      WITH KEY matnr    = lt_goodsmovements-material
                                       charg    = lt_goodsmovements-batch.

      "Consumo de herramental
      IF gv_consumo_herra   EQ '1'                     AND
         lt_mara-mtart      EQ 'ZHER'                  AND
         lt_goodsmaux-batch NE lt_goodsmovements-batch.
        CONTINUE.
      ENDIF.

      "Nro.
      IF gv_edit IS NOT INITIAL.
        gwa_componentes-nro = gv_edit.
      ELSE.
        gwa_componentes-nro = lv_tabix.
      ENDIF.

      "Orden
      gwa_componentes-aufnr = gwa_ordenes-aufnr.

      "Operación
      gwa_componentes-vornr = gwa_ordenes-vornr.

      "Clave Modelo
      gwa_componentes-ktsch = gwa_ordenes-ktsch.

      "Posición en lista de materiales
      gwa_componentes-posnr = lt_goodsmovements-deliv_item+2(4).

      "Componente
      gwa_componentes-componente = lt_goodsmovements-material.

      "Descripción
      gwa_componentes-descripcion = lt_makt-maktx.

      "Cantidad
      IF gv_consumo_herra EQ '1' AND
         lt_mara-mtart    EQ 'ZHER'.
        gwa_componentes-cantidad = 1.
      ELSE.
        gwa_componentes-cantidad = lt_goodsmovements-entry_qnt.
      ENDIF.

      "UM
      gwa_componentes-um     = lt_goodsmovements-entry_uom.
      gwa_componentes-um_txt = lt_t006a-mseh6.

      "Almacén
      gwa_componentes-almacen = lt_goodsmovements-stge_loc.

      "Lote
      gwa_componentes-lote = lt_goodsmovements-batch.
      IF gv_consumo_herra        EQ '1'     AND
         lt_mara-mtart           EQ 'ZHER'  AND
         lt_goodsmovements-batch IS INITIAL.
        READ TABLE lt_consum_herr WITH KEY matnr = lt_goodsmovements-material
                                           lgort = lt_goodsmovements-stge_loc.
        IF sy-subrc EQ 0.
          gwa_componentes-lote = lt_consum_herr-charg.
        ENDIF.
      ENDIF.

      "Clase de movimiento
      gwa_componentes-bwart = lt_goodsmovements-move_type.

      "Lote Proveedor
      gwa_componentes-licha = lt_mch1-licha.

      "Indicador de movimiento
      gwa_componentes-mvt_ind = lt_goodsmovements-mvt_ind.

      "Indicador de stock especial
      gwa_componentes-spec_stock = lt_goodsmovements-spec_stock.

      "Cliente
      gwa_componentes-customer = lt_goodsmovements-customer.

      "Pedido
      gwa_componentes-sales_ord = lt_goodsmovements-sales_ord.

      "Posición
      gwa_componentes-s_ord_item = lt_goodsmovements-s_ord_item.

      "Num Reserva
      gwa_componentes-reserv_no = lt_goodsmovements-reserv_no.

      "Pos Reserva
      gwa_componentes-res_item = lt_goodsmovements-res_item.

      IF gt_matkl IS NOT INITIAL.
        READ TABLE gt_matkl TRANSPORTING NO FIELDS WITH KEY matkl = lt_mara-matkl.
        IF sy-subrc EQ 0.
          CLEAR: gwa_componentes-lote,
                 gwa_componentes-licha.
          gwa_componentes-mod = 'X'.

          "LAF
          READ TABLE gt_componentes INTO DATA(lwa_componentes_aux) WITH KEY nro        = gwa_componentes-nro
                                                                            componente = gwa_componentes-componente
                                                                            posnr      = gwa_componentes-posnr.
          IF sy-subrc EQ 0.
            lv_tabix2 = sy-tabix.

            ADD lwa_componentes_aux-cantidad TO gwa_componentes-cantidad.

            DELETE gt_componentes INDEX lv_tabix2.
          ENDIF.
          "LAF
        ENDIF.
      ENDIF.

      "Celltab
      CASE lt_mara-xchpf.
        WHEN 'X'.
          gwa_componentes-celltab = gt_celltab_xchpf.
      ENDCASE.

      APPEND gwa_componentes TO gt_componentes.
    ENDLOOP.
  ENDLOOP.

  SORT gt_componentes BY nro ASCENDING.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SAVE_0100
*&---------------------------------------------------------------------*
FORM f_save_0100 .
  DATA: lt_comp     TYPE zpptt_notif_component WITH HEADER LINE,
        lt_msg      TYPE zpptt_notif_rpta,
        lv_answer   TYPE c,
        lv_ok_notif TYPE boolean,
        lv_ok_lote  TYPE c,
        lv_lmnga    TYPE ru_lmnga,
        lv_xmnga    TYPE ru_xmnga,
        lv_rmnga    TYPE ru_rmnga,
        lv_error    TYPE boolean,
        lv_nro      TYPE int4.

  CLEAR:   lv_answer,
           lv_error.
  REFRESH: lt_comp,
           lt_msg,
           gt_log.

  PERFORM f_validar_campos CHANGING lv_error.
  CHECK lv_error IS INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = TEXT-065 "'Notificaciones automáticas'
      text_question         = TEXT-066 "'¿Desea notificar los datos?'
      display_cancel_button = space
    IMPORTING
      answer                = lv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  CHECK lv_answer EQ 1.

  PERFORM f_actualiza_lcurvado.
  PERFORM f_actualiza_ciclo_tq.
  PERFORM f_actualiza_ciclo_auto.
*  PERFORM f_actualiza_lote_zhal.
*  PERFORM f_actualiza_falcon3.

  PERFORM f_fill_anular_rechazo.

  LOOP AT gt_ordenes INTO gwa_ordenes.
    CLEAR:   lv_lmnga,
             lv_xmnga,
             lv_nro,
             lv_error.
    REFRESH: lt_comp.

*{-LAF | Definición de lógica de cálculo de lotes para consumo de componentes en órdenes de producción
*    LOOP AT gt_componentes INTO gwa_componentes WHERE nro EQ gwa_ordenes-nro.
**                                                      aufnr EQ gwa_ordenes-aufnr
**                                                  AND vornr EQ gwa_ordenes-vornr.
*      CLEAR: lt_comp.
*
*      lt_comp-matnr      = gwa_componentes-componente.
*      lt_comp-erfmg      = gwa_componentes-cantidad.
*      lt_comp-erfme      = gwa_componentes-um.
*      lt_comp-lgort      = gwa_componentes-almacen.
*      lt_comp-charg      = gwa_componentes-lote.
*      lt_comp-bwart      = gwa_componentes-bwart.
*      lt_comp-mvt_ind    = gwa_componentes-mvt_ind.
*      lt_comp-spec_stock = gwa_componentes-spec_stock.
*      lt_comp-sales_ord  = gwa_componentes-sales_ord.
*      lt_comp-s_ord_item = gwa_componentes-s_ord_item.
*
*      APPEND lt_comp.
*    ENDLOOP.
*}-LAF | Definición de lógica de cálculo de lotes para consumo de componentes en órdenes de producción
    LOOP AT gt_componentes INTO gwa_componentes WHERE nro EQ gwa_ordenes-nro
                                                  AND mod EQ 'X'.
      CLEAR: lt_comp.

      lt_comp-matnr      = gwa_componentes-componente.
      lt_comp-erfmg      = gwa_componentes-cantidad.
      lt_comp-erfme      = gwa_componentes-um.
      lt_comp-lgort      = gwa_componentes-almacen.
      lt_comp-charg      = gwa_componentes-lote.
      lt_comp-bwart      = gwa_componentes-bwart.
      lt_comp-mvt_ind    = gwa_componentes-mvt_ind.
      lt_comp-spec_stock = gwa_componentes-spec_stock.
      lt_comp-sales_ord  = gwa_componentes-sales_ord.
      lt_comp-s_ord_item = gwa_componentes-s_ord_item.
      lt_comp-nuevo      = gwa_componentes-new.

      APPEND lt_comp.
    ENDLOOP.

    IF gwa_ordenes-zztipo_notif IN gr_tipnot_aprob.
*      lv_lmnga = gwa_ordenes-lmnga.
      lv_lmnga = 1.
    ELSEIF gwa_ordenes-zztipo_notif IN gr_tipnot_recha.
*      lv_xmnga = gwa_ordenes-lmnga.
      lv_xmnga = 1.
    ELSEIF gwa_ordenes-zztipo_notif IN gr_tipnot_retra.
*      lv_rmnga = gwa_ordenes-lmnga.
      lv_rmnga = 1.
    ELSEIF gwa_ordenes-zztipo_notif IN gr_tipnot_anali.
    ENDIF.

    PERFORM f_anular_rechazo CHANGING lv_error.

    IF lv_error EQ 'X'.
      CONTINUE.
    ENDIF.

*    DO gwa_ordenes-lmnga TIMES.
*    PERFORM f_genera_lote_auto.

*      ADD 1 TO lv_nro.
    lv_nro = gwa_ordenes-nro.

    CALL FUNCTION 'Z_PP_NOTIFICA_AUTO'
      EXPORTING
        i_dwerk        = p_werks
        i_arbpl        = p_arbpl
        i_aufnr        = gwa_ordenes-aufnr
        i_vornr        = gwa_ordenes-vornr
        i_ktsch        = gwa_ordenes-ktsch
        i_budat        = gwa_ordenes-budat
        i_aplfl        = gwa_ordenes-plnfl
        i_meinh        = gwa_ordenes-gmein
        i_lmnga        = lv_lmnga
        i_xmnga        = lv_xmnga
        i_rmnga        = lv_rmnga
        i_grund        = gwa_ordenes-grund
*       I_TAG_ID       =
        i_lcorte       = gwa_ordenes-zzlote_corte
        i_lcurvado     = gwa_ordenes-zzlote_curvado
        i_ciclotq      = gwa_ordenes-zzciclo_tq
        i_cicloauto    = gwa_ordenes-zzciclo_auto
*       I_TEMPTERM     =
        i_pernr        = p_pernr
        i_defecto      = gwa_ordenes-zzdefecto
        i_defcod       = gwa_ordenes-zzdefcod
        i_origen       = gwa_ordenes-zzorigen
        i_orcod        = gwa_ordenes-zzorcod
        i_causa        = gwa_ordenes-zzcausa
        i_caucod       = gwa_ordenes-zzcaucod
        i_destino      = gwa_ordenes-zzdestino
        i_lote         = gwa_ordenes-zzlote
        i_tipo_notif   = gwa_ordenes-zztipo_notif
        i_codlanza     = gwa_ordenes-zzcod_lanza
        i_lcarga       = gwa_ordenes-zzlote_carga
        i_equipo       = gwa_ordenes-zzequipo
        i_ltxa1        = gwa_ordenes-ltxa1
        i_nro_rack     = gwa_ordenes-nro_rack
        i_nro_caja     = gwa_ordenes-nro_caja
        i_nro_molde    = gwa_ordenes-nro_molde
        i_cam_horno    = gwa_ordenes-cam_horno
        i_no_material  = gwa_ordenes-no_material
        i_no_actividad = gwa_ordenes-no_actividad
        i_interfaz     = space
*{-LAF | Definición de lógica de cálculo de lotes para consumo de componentes en órdenes de producción
        i_component    = lt_comp[]
*{-LAF | Definición de lógica de cálculo de lotes para consumo de componentes en órdenes de producción
      IMPORTING
        e_ok_notif     = lv_ok_notif
        e_ok_lote      = lv_ok_lote
        e_msg          = lt_msg.

    PERFORM f_agregar_log USING gwa_ordenes-aufnr
                                gwa_ordenes-vornr
                                lv_nro
                                lv_ok_notif
                                lv_ok_lote
                                lt_msg.

    IF lv_ok_notif EQ 'X'.
      PERFORM f_impresion_automatica_uni USING gwa_ordenes.
    ENDIF.
*    ENDDO.
*    IF lv_ok_notif IS NOT INITIAL.
*      PERFORM f_corregir_cogi USING gwa_ordenes-aufnr.
*    ENDIF.
  ENDLOOP.

  PERFORM f_actualizar_lcorte.
  PERFORM f_impresion_automatica_mas.
  PERFORM f_mostrar_log.
  PERFORM f_inicializar.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_AGREGAR_LOG
*&---------------------------------------------------------------------*+
FORM f_agregar_log USING p_aufnr
                         p_vornr
                         p_nro
                         p_ok_notif
                         p_ok_lote
                         p_msg TYPE zpptt_notif_rpta.
  DATA: lwa_msg TYPE zppe_notif_rpta.

  CLEAR: gwa_log.

  gwa_log-aufnr = p_aufnr.
  gwa_log-vornr = p_vornr.
  gwa_log-nro   = p_nro.

  CASE p_ok_notif.
    WHEN 'X'.
      gwa_log-notif = TEXT-067. "'Sí'.
    WHEN space.
      gwa_log-notif = TEXT-068. "'No'.
  ENDCASE.
  CASE p_ok_lote.
    WHEN 'X'.
      gwa_log-lote = TEXT-067. "'Sí'.
    WHEN 'N'.
      CLEAR: gwa_log-lote.
    WHEN space.
      gwa_log-lote = TEXT-068. "'No'.
  ENDCASE.
  LOOP AT p_msg INTO lwa_msg.
    IF lwa_msg-componente IS NOT INITIAL.
      gwa_log-comp = lwa_msg-componente.
      CASE lwa_msg-error.
        WHEN 'X'.
          gwa_log-herra = TEXT-068. "'No'.
        WHEN space.
          gwa_log-herra = TEXT-067. "'Sí'.
      ENDCASE.
    ENDIF.

    gwa_log-msg = lwa_msg-msg.

    APPEND gwa_log TO gt_log.
  ENDLOOP.
  IF sy-subrc NE 0.
    CLEAR: gwa_log-comp,
           gwa_log-herra,
           gwa_log-msg.

    APPEND gwa_log TO gt_log.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_AGREGAR_LOG_V2
*&---------------------------------------------------------------------*+
FORM f_agregar_log_v2 USING p_nro
                            p_ok_notif
                            p_ok_lote
                            p_componente
                            p_error
                            p_msg
                            p_rev_herra.
  DATA: lwa_msg TYPE zppe_notif_rpta.

  CLEAR: gwa_log.

  READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY nro = p_nro.

  gwa_log-aufnr = gwa_ordenes-aufnr.
  gwa_log-vornr = gwa_ordenes-vornr.
  gwa_log-nro   = p_nro.

  CASE p_ok_notif.
    WHEN 'X'.
      gwa_log-notif = TEXT-067. "'Sí'.
    WHEN space.
      gwa_log-notif = TEXT-068. "'No'.
  ENDCASE.
  CASE p_ok_lote.
    WHEN 'X'.
      gwa_log-lote = TEXT-067. "'Sí'.
    WHEN 'N'.
      CLEAR: gwa_log-lote.
    WHEN space.
      gwa_log-lote = TEXT-068. "'No'.
  ENDCASE.

  IF p_componente IS NOT INITIAL.
    gwa_log-comp = p_componente.
    CASE p_error.
      WHEN 'X'.
        gwa_log-herra = TEXT-068. "'No'.
      WHEN space.
        gwa_log-herra = TEXT-067. "'Sí'.
    ENDCASE.
  ENDIF.

  gwa_log-msg = p_msg.

  IF p_rev_herra EQ 'W'.
    gwa_log-color = 'C311'.
  ENDIF.

  APPEND gwa_log TO gt_log.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_MOSTRAR_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_mostrar_log .
  DATA: lv_text TYPE string.

  SORT gt_log BY nro ASCENDING.

  IF gv_lote_curvado IS NOT INITIAL.
    lv_text = TEXT-018.
    REPLACE '&' WITH gv_lote_curvado INTO lv_text.
    MESSAGE lv_text TYPE 'S'.
  ELSEIF gv_ciclo_tq IS NOT INITIAL.
    lv_text = TEXT-019.
    REPLACE '&' WITH gv_ciclo_tq INTO lv_text.
    MESSAGE lv_text TYPE 'S'.
  ELSEIF gv_ciclo_auto IS NOT INITIAL.
    lv_text = TEXT-020.
    REPLACE '&' WITH gv_ciclo_auto INTO lv_text.
    MESSAGE lv_text TYPE 'S'.
  ELSE.
    MESSAGE TEXT-006 TYPE 'S'.
  ENDIF.

  CALL SCREEN 0300 STARTING AT 5  1.
ENDFORM.

*{+ @0004
FORM modificar_color_componentes.
  DATA: lv_objectkey       TYPE objnum,
        lt_allocvaluesnum  TYPE tt_allocvaluesnum  WITH HEADER LINE,
        lt_allocvalueschar TYPE tt_allocvalueschar WITH HEADER LINE,
        lt_allocvaluescurr TYPE tt_allocvaluescurr WITH HEADER LINE,
        lt_return          TYPE tt_return          WITH HEADER LINE.

  " Obtener data del dato maestro
  CONCATENATE p_werks
              p_arbpl
              INTO lv_objectkey.
  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey       = lv_objectkey
      objecttable     = 'CRHD'
      classnum        = 'Z_PUESTO_TRABAJO'
      classtype       = '019'
    TABLES
      allocvaluesnum  = lt_allocvaluesnum[]
      allocvalueschar = lt_allocvalueschar[]
      allocvaluescurr = lt_allocvaluescurr[]
      return          = lt_return[].

  " Validación de ventana notif
  DATA: lv_objek      TYPE inob-objek.
  CONCATENATE: p_werks p_arbpl INTO lv_objek.

  SELECT cuobj, klart FROM inob
    INTO @DATA(lwa_inob)
  UP TO 1 ROWS
  WHERE
    objek     EQ @lv_objek     AND
    klart     EQ '019'.
  ENDSELECT.

  CONCATENATE: '%' lwa_inob-cuobj INTO lv_objek.

  DATA: lv_attin      TYPE ausp-atinn.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'Z_VENTANA_NOTIF'
    IMPORTING
      output = lv_attin.
  SELECT objek, atinn, atzhl, mafid, klart, adzhl, atwrt FROM ausp
    INTO @DATA(lwa_ausp)
  UP TO 1 ROWS
  WHERE
    objek     LIKE  @lv_objek       AND
    atinn     EQ    @lv_attin.
  ENDSELECT.
*  CHECK: lwa_ausp-atwrt EQ '1'.

  LOOP AT gt_ordenes INTO gwa_ordenes.
    LOOP AT gt_componentes ASSIGNING FIELD-SYMBOL(<lfs_componentes>) WHERE
        aufnr = gwa_ordenes-aufnr AND
        vornr = gwa_ordenes-vornr AND
        ktsch = gwa_ordenes-ktsch.

*      READ TABLE lt_allocvalueschar[] INTO lt_allocvalueschar WITH KEY charact = 'Z_TIPO_NOTIFICACION'
*                                                                       value_char = gwa_ordenes-zztipo_notif.
*      IF lt_allocvalueschar-value_neutral = '01' OR lt_allocvalueschar-value_neutral = '02' OR lt_allocvalueschar-value_neutral = '03'.
*        IF gwa_ordenes-color EQ gc_amarillo.
      <lfs_componentes>-color = gwa_ordenes-color.
*        ENDIF.

*      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.
*}+ @0004

*&---------------------------------------------------------------------*
*& Form F_ADD_TAB01_AUFNR
*&---------------------------------------------------------------------*
FORM f_add_tab01_aufnr CHANGING p_error.
  DATA: lt_data            TYPE tt_data_lcorte     WITH HEADER LINE,
        lt_makt            TYPE tt_makt            WITH HEADER LINE,
        lt_t006a           TYPE tt_t006a           WITH HEADER LINE,
        lt_afru            TYPE tt_afru            WITH HEADER LINE,
        lt_vornr           TYPE tt_vornr           WITH HEADER LINE,
        lt_allocvaluesnum  TYPE tt_allocvaluesnum  WITH HEADER LINE,
        lt_allocvalueschar TYPE tt_allocvalueschar WITH HEADER LINE,
        lt_allocvaluescurr TYPE tt_allocvaluescurr WITH HEADER LINE,
        lt_return          TYPE tt_return          WITH HEADER LINE,
        lt_aufk            TYPE tt_aufk            WITH HEADER LINE,
        lt_bloq_notif      TYPE tt_bloq_notif      WITH HEADER LINE,
        lt_afru_aux        TYPE tt_afru_full       WITH HEADER LINE,
        lt_embalaux        TYPE tt_caja_rack_full  WITH HEADER LINE,
        lt_notif_grupal    TYPE tt_notif_grupal    WITH HEADER LINE,
        lwa_embalaje       TYPE ztewm_caja_rack,
        lv_txt             TYPE string,
        lv_equnr           TYPE equnr,
        lv_batch           TYPE charg_d,
        lv_comp            TYPE i,
        lv_cant            TYPE i,
        lv_loop            TYPE i,
        lv_lote            TYPE charg_d,
        lv_tipo_notif      TYPE ze_tipo_notif,
        lv_class           TYPE klasse_d,
        lv_klart           TYPE klassenart,
        lv_obtab           TYPE tabelle,
        lv_objectkey       TYPE objnum,
        lv_bloq            TYPE c,
        lv_tabix           TYPE sytabix,
        lv_exidv           TYPE exidv,
        lv_lmnga           TYPE lmnga,
        lv_ok              TYPE c,
        lv_sig_pto         TYPE ze_destino,
        lv_sig_clave       TYPE ktsch.

  REFRESH: lt_data,
           lt_makt,
           lt_t006a,
           lt_afru.

  lv_txt = TEXT-008.

  "Validar Fecha de contabilización
  IF zppe_notif_addreg-budat IS INITIAL.
    MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ELSEIF zppe_notif_addreg-lmnga        IS INITIAL AND
         zppe_notif_addreg-zztipo_notif NOT IN gr_tipnot_anali.
    REPLACE '&' IN lv_txt WITH TEXT-069. "'Cantidad'.
    MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  "Validar causa de desviación
  IF zppe_notif_addreg-grund IS NOT INITIAL.
    SELECT SINGLE COUNT(*)
      FROM trug
      WHERE werks EQ p_werks
        AND grund EQ zppe_notif_addreg-grund.
    IF sy-subrc NE 0.
      REPLACE '&' IN lv_txt WITH TEXT-070. "'Causa de Desviación'.
      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  "Validar equipo
  IF zppe_notif_addreg-equnr IS NOT INITIAL.
    CLEAR: lv_equnr.

    SELECT SINGLE equz~equnr
      INTO lv_equnr
      FROM crhd INNER JOIN iloa ON crhd~objid EQ iloa~ppsid AND
                                   crhd~werks EQ iloa~swerk
                INNER JOIN equz ON iloa~iloan EQ equz~iloan
                INNER JOIN eqkt ON equz~equnr EQ eqkt~equnr
      WHERE crhd~arbpl EQ p_arbpl
        AND crhd~werks EQ p_werks
        AND equz~equnr EQ zppe_notif_addreg-equnr.
    IF sy-subrc NE 0.
      MESSAGE TEXT-017 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  "Validar orden liberada
  SELECT SINGLE COUNT(*)
    FROM aufk INNER JOIN jest ON aufk~objnr EQ jest~objnr
    WHERE aufk~aufnr EQ zppe_notif_addreg-aufnr
      AND jest~stat  EQ 'I0002'
      AND jest~inact EQ space.
  IF sy-subrc NE 0.
    lv_txt = TEXT-021.
    REPLACE '&' IN lv_txt WITH zppe_notif_addreg-aufnr.
    MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  "Validar Lote
  IF zppe_notif_addreg-charg IS NOT INITIAL.
    SELECT SINGLE COUNT(*)
      FROM aufk INNER JOIN afpo ON aufk~aufnr EQ afpo~aufnr
                INNER JOIN mcha ON afpo~matnr EQ mcha~matnr AND
                                   aufk~werks EQ mcha~werks
      WHERE aufk~aufnr EQ zppe_notif_addreg-aufnr
        AND mcha~charg EQ zppe_notif_addreg-charg.
    IF sy-subrc NE 0.
      MESSAGE TEXT-037 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.

    IF zppe_notif_addreg-lmnga GT 1.
      MESSAGE TEXT-088 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  "Validar Lote Obligatorio
  IF gv_lote_oblig           EQ '1'     AND
     zppe_notif_addreg-charg IS INITIAL.
    p_error = 'X'.

    lv_txt = TEXT-025.
    REPLACE '&1' IN lv_txt WITH TEXT-x10."'Lote'.

    MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  "Validar estado de lote
  PERFORM f_validar_estado_lote CHANGING p_error.
  CHECK p_error IS INITIAL.

  IF zppe_notif_addreg-ktsch IS NOT INITIAL.
    SELECT  afko~aufnr
            afvc~vornr
            afvc~ktsch
            afvc~plnfl
            afko~plnbez
            afko~gmein
            afpo~charg
            afvc~steus
      INTO TABLE lt_data
      FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                INNER JOIN afpo ON afko~aufnr EQ afpo~aufnr
                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
      WHERE afko~aufnr EQ zppe_notif_addreg-aufnr
        AND afvc~ktsch EQ zppe_notif_addreg-ktsch
        AND crhd~arbpl EQ p_arbpl
        AND crhd~werks EQ p_werks.
  ELSE.
    SELECT  afko~aufnr
            afvc~vornr
            afvc~ktsch
            afvc~plnfl
            afko~plnbez
            afko~gmein
            afpo~charg
            afvc~steus
      INTO TABLE lt_data
      FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                INNER JOIN afpo ON afko~aufnr EQ afpo~aufnr
                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
      WHERE afko~aufnr EQ zppe_notif_addreg-aufnr
        AND afvc~vornr EQ zppe_notif_addreg-vornr
        AND crhd~arbpl EQ p_arbpl
        AND crhd~werks EQ p_werks.
  ENDIF.

  "Validar duplicados
*  READ TABLE lt_data INDEX 1.
*  PERFORM f_lote_dupli USING    lt_data
*                       CHANGING p_error.
*  CHECK p_error IS INITIAL.

  "Validar rack/caja
  IF sy-subrc                       EQ 0               AND
     gv_raca_oblig                  EQ '1'             AND
     zppe_notif_addreg-zztipo_notif IN gr_tipnot_aprob.
    REFRESH: lt_vornr.

    READ TABLE lt_data INDEX 1.
    SELECT afvc~vornr
      INTO TABLE lt_vornr
      FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
      WHERE aufk~aufnr EQ lt_data-aufnr
        AND crhd~arbpl EQ p_arbpl
        AND crhd~werks EQ p_werks.

    SORT lt_vornr BY vornr ASCENDING.
    READ TABLE lt_vornr INDEX 1.

    IF lt_vornr-vornr NE lt_data-vornr.

      IF zppe_notif_addreg-nro_rack IS INITIAL AND
         zppe_notif_addreg-nro_caja IS INITIAL.
        MESSAGE TEXT-032 TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
        EXIT.
      ELSEIF zppe_notif_addreg-nro_rack IS NOT INITIAL AND
             zppe_notif_addreg-nro_caja IS NOT INITIAL.
        MESSAGE TEXT-033 TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  "Validar equipo
  IF gv_equipo_oblig         IS NOT INITIAL AND
     lt_data[]               IS NOT INITIAL.
    CASE gv_equipo_oblig.
      WHEN '1'.
        REFRESH: lt_vornr.

        READ TABLE lt_data INDEX 1.
        SELECT afvc~vornr
          INTO TABLE lt_vornr
          FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                    INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                    INNER JOIN crhd ON afvc~arbid EQ crhd~objid
          WHERE aufk~aufnr EQ lt_data-aufnr
            AND crhd~arbpl EQ p_arbpl
            AND crhd~werks EQ p_werks.

        SORT lt_vornr BY vornr ASCENDING.
        READ TABLE lt_vornr INDEX 1.

        IF lt_vornr-vornr EQ lt_data-vornr.
          IF zppe_notif_addreg-equnr IS INITIAL.
            lv_txt = TEXT-008.
            REPLACE '&' IN lv_txt WITH TEXT-071. "'Equipo'.
            p_error = 'X'.
            MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        ELSE.
          SORT lt_vornr BY vornr DESCENDING.
          READ TABLE lt_vornr INDEX 1.

          IF lt_vornr-vornr EQ lt_data-vornr.
            SORT lt_vornr BY vornr ASCENDING.
            READ TABLE lt_vornr INDEX 1.

            SELECT ersda,
                   erzet,
                   zzequipo
              INTO TABLE @DATA(lt_equnr)
              FROM afru
              WHERE aufnr EQ @lt_data-aufnr
                AND vornr EQ @lt_vornr-vornr
                AND stokz EQ @space
                AND stzhl EQ @space
              ORDER BY ersda DESCENDING,
                       erzet DESCENDING.
            READ TABLE lt_equnr INTO DATA(lwa_equnr) INDEX 1.

            zppe_notif_addreg-equnr = lwa_equnr-zzequipo.
          ENDIF.
        ENDIF.
      WHEN '2'.
        IF zppe_notif_addreg-equnr IS INITIAL.
          lv_txt = TEXT-008.
          REPLACE '&' IN lv_txt WITH TEXT-071. "'Equipo'.
          p_error = 'X'.
          MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      WHEN '3'.
        REFRESH: lt_vornr.

        READ TABLE lt_data INDEX 1.
        SELECT afvc~vornr
          INTO TABLE lt_vornr
          FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                    INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                    INNER JOIN crhd ON afvc~arbid EQ crhd~objid
          WHERE aufk~aufnr EQ lt_data-aufnr
            AND crhd~arbpl EQ p_arbpl
            AND crhd~werks EQ p_werks.

        SORT lt_vornr BY vornr ASCENDING.
        READ TABLE lt_vornr INDEX 1.

        IF lt_vornr-vornr          EQ lt_data-vornr AND
           zppe_notif_addreg-equnr IS INITIAL.
          lv_txt = TEXT-008.
          REPLACE '&' IN lv_txt WITH TEXT-071. "'Equipo'.
          p_error = 'X'.
          MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ELSEIF lt_vornr-vornr          NE lt_data-vornr AND
               zppe_notif_addreg-equnr IS NOT INITIAL.
          p_error = 'X'.
          MESSAGE TEXT-058 TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
    ENDCASE.
  ENDIF.

  "Validar código lanzada
  IF gv_codlan_oblig               EQ '1'         AND
     lt_data[]                     IS NOT INITIAL AND
     zppe_notif_addreg-zzcod_lanza IS INITIAL.
    REFRESH: lt_vornr.

    READ TABLE lt_data INDEX 1.
    SELECT afvc~vornr
      INTO TABLE lt_vornr
      FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
      WHERE aufk~aufnr EQ lt_data-aufnr
        AND crhd~arbpl EQ p_arbpl
        AND crhd~werks EQ p_werks.

    SORT lt_vornr BY vornr ASCENDING.
    READ TABLE lt_vornr INDEX 1.

    IF lt_vornr-vornr EQ lt_data-vornr.
      lv_txt = TEXT-008.
      REPLACE '&' IN lv_txt WITH TEXT-072."'Código de Lanzada'. "@+0013
      p_error = 'X'.
      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  "Validar registro duplicado
  IF zppe_notif_addreg-charg IS NOT INITIAL AND
     lt_data[]               IS NOT INITIAL.
    READ TABLE lt_data INDEX 1.

    READ TABLE gt_ordenes TRANSPORTING NO FIELDS WITH KEY aufnr  = lt_data-aufnr
                                                          vornr  = lt_data-vornr
                                                          plnfl  = lt_data-plnfl
                                                          zzlote = zppe_notif_addreg-charg.
    IF sy-subrc EQ 0.
      p_error = 'X'.
      MESSAGE TEXT-036 TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ELSEIF lt_data[] IS NOT INITIAL..
    READ TABLE lt_data INDEX 1.
    IF lt_data-charg IS NOT INITIAL.
      READ TABLE gt_ordenes TRANSPORTING NO FIELDS WITH KEY aufnr  = lt_data-aufnr
                                                            vornr  = lt_data-vornr
                                                            plnfl  = lt_data-plnfl
                                                            zzlote = lt_data-charg.
      IF sy-subrc EQ 0.
        p_error = 'X'.
        MESSAGE TEXT-036 TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  "Bloqueo de Notificación BRG
  IF gv_bloq_notif IS NOT INITIAL AND
     lt_data[]     IS NOT INITIAL.
    CLEAR:   lv_bloq.
    REFRESH: lt_bloq_notif,
             lt_afru_aux,
             lt_allocvaluesnum,
             lt_allocvalueschar,
             lt_allocvaluescurr,
             lt_return.

    READ TABLE lt_data INDEX 1.

    SELECT SINGLE matnr
      INTO lv_objectkey
      FROM afpo
      WHERE aufnr EQ lt_data-aufnr.

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_objectkey
        objecttable     = 'MARA'
        classnum        = 'Z_FOR_PIEZAS'
        classtype       = '001'
      TABLES
        allocvaluesnum  = lt_allocvaluesnum[]
        allocvalueschar = lt_allocvalueschar[]
        allocvaluescurr = lt_allocvaluescurr[]
        return          = lt_return[].

    READ TABLE lt_allocvalueschar WITH KEY charact = 'Z_GEOMETRY_TYPE'.

    SELECT crhd~arbpl
           afvc~vornr
      INTO TABLE lt_bloq_notif
      FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
      WHERE aufk~aufnr EQ lt_data-aufnr.

    SELECT *
      INTO TABLE lt_afru_aux
      FROM afru
      WHERE aufnr EQ lt_data-aufnr
        AND stokz EQ space
        AND stzhl EQ space
        AND lmnga GT 0.

    CASE gv_bloq_notif.
      WHEN '1'.
        SORT lt_bloq_notif BY vornr DESCENDING.
        READ TABLE lt_bloq_notif WITH KEY arbpl = p_arbpl.
        IF lt_data-vornr                    EQ lt_bloq_notif-vornr AND
           lt_allocvalueschar-value_neutral EQ '01'.
          LOOP AT lt_bloq_notif WHERE arbpl EQ '01CORTE'.
            READ TABLE lt_afru_aux WITH KEY vornr = lt_bloq_notif-vornr.
            IF sy-subrc NE 0.
              MESSAGE TEXT-046 TYPE 'S' DISPLAY LIKE 'E'.
              lv_bloq = 'X'.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
      WHEN '2'.
        SORT lt_bloq_notif BY vornr ASCENDING.
        READ TABLE lt_bloq_notif WITH KEY arbpl = p_arbpl.
        IF lt_data-vornr                    EQ lt_bloq_notif-vornr AND
           lt_allocvalueschar-value_neutral EQ '02'.
          LOOP AT lt_bloq_notif WHERE arbpl EQ '01CORTE'.
            READ TABLE lt_afru_aux WITH KEY vornr = lt_bloq_notif-vornr.
            IF sy-subrc NE 0.
              MESSAGE TEXT-046 TYPE 'S' DISPLAY LIKE 'E'.
              lv_bloq = 'X'.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
      WHEN '3'.
        SORT lt_bloq_notif BY vornr ASCENDING.
        READ TABLE lt_bloq_notif WITH KEY arbpl = p_arbpl.
        IF lt_data-vornr EQ lt_bloq_notif-vornr.
          LOOP AT lt_bloq_notif WHERE arbpl EQ '10CORTPC'
                                  OR  arbpl EQ '11ZUND'.
            READ TABLE lt_afru_aux WITH KEY vornr = lt_bloq_notif-vornr.
            IF sy-subrc NE 0.
              MESSAGE TEXT-047 TYPE 'S' DISPLAY LIKE 'E'.
              lv_bloq = 'X'.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
    ENDCASE.
    IF lv_bloq EQ 'X'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  "Validar lote único en orden
  IF gv_lote_unico EQ '1'         AND
     lt_data[]     IS NOT INITIAL.
    READ TABLE lt_data INDEX 1.

    SELECT SINGLE COUNT(*)
      FROM afru
      WHERE aufnr NE lt_data-aufnr
        AND stokz EQ space
        AND stzhl EQ space
        AND zzlote EQ zppe_notif_addreg-charg.
    IF sy-subrc EQ 0.
      p_error = 'X'.
      MESSAGE TEXT-048 TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  "Bloqueo de Notificación eGlass
*  IF gv_bloq_notif_eg IS NOT INITIAL AND
  IF gv_bloq_notif_eg EQ '1'         AND
     lt_data[]        IS NOT INITIAL.
    CLEAR:   lv_tabix,
             lv_ok.

    READ TABLE lt_data INDEX 1.

    SELECT crhd~arbpl,
           afvc~vornr,
           afvc~steus
      INTO TABLE @DATA(lt_bloq_notif_eglass)
      FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
      WHERE aufk~aufnr EQ @lt_data-aufnr
      ORDER BY vornr ASCENDING.

    READ TABLE lt_bloq_notif_eglass INTO DATA(lwa_bloq_notif_eglass) WITH KEY vornr = lt_data-vornr.
    IF gv_notif_grupal2 EQ '1'.                             "+@0001
      READ TABLE lt_bloq_notif_eglass INTO lwa_bloq_notif_eglass WITH KEY arbpl = lwa_bloq_notif_eglass-arbpl. "+@0001
    ENDIF.                                                  "+@0001
    lv_tabix = sy-tabix.
    DO.
      SUBTRACT 1 FROM lv_tabix.

      IF lv_tabix EQ 0.
        lv_ok = 'X'.
        EXIT.
      ENDIF.

      READ TABLE lt_bloq_notif_eglass INTO lwa_bloq_notif_eglass INDEX lv_tabix.
*      EXIT.                                                 "+@0001   -@0003
*      {+@0003
      IF gv_notif_grupal2 NE '1' AND lwa_bloq_notif_eglass-steus NE 'ZP04'.
        EXIT.
      ENDIF.
      IF gv_notif_grupal2 EQ '1' AND lwa_bloq_notif_eglass-steus NE 'ZP04'.
        EXIT.
      ENDIF.
*      }+@0003
*      {-@0001
*      IF lwa_bloq_notif_eglass-steus NE 'ZP04'.
*        EXIT.
*      ENDIF.
*      }-@0001
    ENDDO.

    IF lv_ok IS INITIAL.
      SELECT SINGLE COUNT(*)
        FROM afru
        WHERE vornr  EQ lwa_bloq_notif_eglass-vornr
          AND stokz  EQ space
          AND stzhl  EQ space
          AND zzlote EQ zppe_notif_addreg-charg.
      IF sy-subrc NE 0.
        MESSAGE TEXT-049 TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  "Validar Caja/Rack
  IF zppe_notif_addreg-nro_caja IS NOT INITIAL OR
     zppe_notif_addreg-nro_rack IS NOT INITIAL.
    IF zppe_notif_addreg-zztipo_notif NOT IN gr_tipnot_aprob.
      MESSAGE TEXT-052 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.

    CLEAR: lv_exidv.

    IF zppe_notif_addreg-nro_caja IS NOT INITIAL.
      lv_exidv = zppe_notif_addreg-nro_caja.
    ELSE.
      lv_exidv = zppe_notif_addreg-nro_rack.
    ENDIF.

    SELECT SINGLE COUNT(*)
      FROM vekp
      WHERE exidv EQ lv_exidv.

    IF sy-subrc NE 0.
      MESSAGE TEXT-050 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.

    CLEAR: lwa_embalaje,
           lv_lmnga.

    IF zppe_notif_addreg-zztipo_notif EQ gv_aprob_am.
      SELECT SINGLE @abap_true
        INTO @DATA(result)
        FROM afru INNER JOIN afpo ON afru~aufnr EQ afpo~aufnr
        WHERE afru~aufnr        EQ @zppe_notif_addreg-aufnr
          AND afru~stokz        EQ @space
          AND afru~stzhl        EQ @space
          AND afru~zznro_rack   EQ @zppe_notif_addreg-nro_rack
          AND afru~zznro_caja   EQ @zppe_notif_addreg-nro_caja
          AND ( afru~zztipo_notif EQ @gv_aprob OR
                afru~zztipo_notif EQ @gv_aprob_pc ).
      IF sy-subrc NE 0.
        LOOP AT gt_ordenes INTO gwa_ordenes WHERE nro_rack EQ zppe_notif_addreg-nro_rack
                                              AND nro_caja EQ zppe_notif_addreg-nro_caja
                                              AND ( zztipo_notif EQ gv_aprob    OR
                                                    zztipo_notif EQ gv_aprob_pc ).
          EXIT.
        ENDLOOP.
      ENDIF.
    ELSE.
      SELECT SINGLE @abap_true
        INTO @result
        FROM afru INNER JOIN afpo ON afru~aufnr EQ afpo~aufnr
        WHERE afru~aufnr        EQ @zppe_notif_addreg-aufnr
          AND afru~stokz        EQ @space
          AND afru~stzhl        EQ @space
          AND afru~zznro_rack   EQ @zppe_notif_addreg-nro_rack
          AND afru~zznro_caja   EQ @zppe_notif_addreg-nro_caja
          AND afru~zztipo_notif EQ @gv_aprob_am.
      IF sy-subrc NE 0.
        LOOP AT gt_ordenes INTO gwa_ordenes WHERE nro_rack     EQ zppe_notif_addreg-nro_rack
                                              AND nro_caja     EQ zppe_notif_addreg-nro_caja
                                              AND zztipo_notif EQ gv_aprob_am.
          EXIT.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF sy-subrc EQ 0.
      MESSAGE TEXT-053 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.

    SELECT SINGLE ztewm_caja_rack~*
      INTO @lwa_embalaje
      FROM ztewm_caja_rack INNER JOIN afpo ON ztewm_caja_rack~matnr EQ afpo~matnr
      WHERE ztewm_caja_rack~werks EQ @p_werks
        AND afpo~aufnr            EQ @zppe_notif_addreg-aufnr.
    IF sy-subrc NE 0.
      MESSAGE TEXT-054 TYPE 'S' DISPLAY LIKE 'I'.
    ELSE.
      SELECT SUM( afru~lmnga )
        INTO lv_lmnga
        FROM afru INNER JOIN afpo ON afru~aufnr EQ afpo~aufnr
        WHERE afru~aufnr        EQ zppe_notif_addreg-aufnr
          AND afru~stokz        EQ space
          AND afru~stzhl        EQ space
          AND afru~zznro_rack   EQ zppe_notif_addreg-nro_rack
          AND afru~zznro_caja   EQ zppe_notif_addreg-nro_caja
          AND afru~zztipo_notif IN gr_tipnot_aprob.

      LOOP AT gt_ordenes INTO gwa_ordenes WHERE nro_rack EQ zppe_notif_addreg-nro_rack
                                            AND nro_caja EQ zppe_notif_addreg-nro_caja.
        ADD gwa_ordenes-lmnga TO lv_lmnga.
      ENDLOOP.

      ADD zppe_notif_addreg-lmnga TO lv_lmnga.

      IF ( zppe_notif_addreg-nro_rack IS NOT INITIAL           AND
           lv_lmnga                   GT lwa_embalaje-cant_rack ) OR
         ( zppe_notif_addreg-nro_caja IS NOT INITIAL           AND
           lv_lmnga                   GT lwa_embalaje-cant_caja ).
        p_error = 'X'.
        MESSAGE TEXT-051 TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  "Bloqueo de notificación por horno
  IF gv_valida_eq_mat         EQ '1'         AND
     zppe_notif_addreg-equnr IS NOT INITIAL.
    CLEAR:   lv_objectkey,
             lv_ok,
             lv_equnr.
    REFRESH: lt_allocvaluesnum,
             lt_allocvalueschar,
             lt_allocvaluescurr,
             lt_return.

    READ TABLE lt_data INDEX 1.

    SELECT SINGLE matnr
      INTO lv_objectkey
      FROM afpo
      WHERE aufnr EQ lt_data-aufnr.

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_objectkey
        objecttable     = 'MARA'
        classnum        = 'Z_PROCESSOS'
        classtype       = '001'
      TABLES
        allocvaluesnum  = lt_allocvaluesnum[]
        allocvalueschar = lt_allocvalueschar[]
        allocvaluescurr = lt_allocvaluescurr[]
        return          = lt_return[].

    LOOP AT lt_allocvalueschar WHERE charact EQ 'Z_FORNO1'
                                 OR  charact EQ 'Z_FORNO_HAB'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lt_allocvalueschar-value_neutral
        IMPORTING
          output = lv_equnr.

      IF zppe_notif_addreg-equnr EQ lv_equnr.
        lv_ok = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      p_error = 'X'.
      MESSAGE TEXT-055 TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF lv_ok IS INITIAL.
      p_error = 'X'.
      MESSAGE TEXT-056 TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  "Notificación Masiva
  IF gv_notif_grupal                EQ '1'             AND
     zppe_notif_addreg-zztipo_notif IN gr_tipnot_aprob AND
     lt_data[]                      IS NOT INITIAL.
    READ TABLE lt_data INDEX 1.
    IF sy-subrc EQ 0.
      SELECT SINGLE COUNT(*)
        FROM afru
        WHERE aufnr EQ lt_data-aufnr
          AND vornr EQ lt_data-vornr
          AND stokz EQ space
          AND stzhl EQ space.
      IF sy-subrc NE 0.
        SELECT rueck
               rmzhl
          INTO TABLE lt_afru
          FROM crhd INNER JOIN afru ON crhd~objid EQ afru~arbid
          WHERE crhd~werks EQ p_werks
            AND crhd~arbpl EQ p_arbpl
            AND afru~aufnr EQ lt_data-aufnr
            AND afru~stokz EQ space
            AND afru~stzhl EQ space.
        IF sy-subrc NE 0.
          REFRESH: lt_data.

          SELECT  afko~aufnr
                  afvc~vornr
                  afvc~ktsch
                  afvc~plnfl
                  afko~plnbez
                  afko~gmein
                  afpo~charg
                  afvc~steus
            INTO TABLE lt_data
            FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                      INNER JOIN afpo ON afko~aufnr EQ afpo~aufnr
                      INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                      INNER JOIN crhd ON afvc~arbid EQ crhd~objid
            WHERE afko~aufnr EQ zppe_notif_addreg-aufnr
              AND crhd~arbpl EQ p_arbpl
              AND crhd~werks EQ p_werks.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  "Notificación Masiva 2
  IF gv_notif_grupal2               EQ '1'             AND
     zppe_notif_addreg-zztipo_notif IN gr_tipnot_aprob AND
     lt_data[]                      IS NOT INITIAL.
    READ TABLE lt_data INDEX 1.
    IF sy-subrc EQ 0.
      REFRESH: lt_vornr.

      SELECT afvc~vornr
      INTO TABLE lt_vornr
      FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
      WHERE aufk~aufnr EQ lt_data-aufnr
        AND crhd~arbpl EQ p_arbpl
        AND crhd~werks EQ p_werks.
      SORT lt_vornr BY vornr ASCENDING.
      IF lt_data-vornr NE lt_vornr-vornr.
        REFRESH: lt_data.

        SELECT  afko~aufnr
                afvc~vornr
                afvc~ktsch
                afvc~plnfl
                afko~plnbez
                afko~gmein
                afpo~charg
                afvc~steus
          INTO TABLE lt_data
          FROM aufk INNER JOIN afko  ON  aufk~aufnr EQ afko~aufnr
                    INNER JOIN afpo  ON  afko~aufnr EQ afpo~aufnr
                    INNER JOIN afvc  ON  afko~aufpl EQ afvc~aufpl
                    INNER JOIN crhd  ON  afvc~arbid EQ crhd~objid
          WHERE afko~aufnr EQ zppe_notif_addreg-aufnr
            AND crhd~arbpl EQ p_arbpl
            AND crhd~werks EQ p_werks.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gv_borrar_cant EQ 'X'.
    CLEAR: gv_borrar_cant.
    LOOP AT lt_data WHERE ktsch EQ 'SALH'
                       OR ktsch EQ 'TROSAL'.
      EXIT.
    ENDLOOP.
    IF sy-subrc EQ 0.
      CLEAR: zppe_notif_addreg-lmnga.
      REPLACE '&' IN lv_txt WITH TEXT-069. "'Cantidad'.
      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  IF lt_data[] IS NOT INITIAL.
    SELECT matnr
           maktx
      INTO TABLE lt_makt
      FROM makt
      FOR ALL ENTRIES IN lt_data
      WHERE matnr EQ lt_data-plnbez
        AND spras EQ sy-langu.

    SELECT msehi
           mseh6
      INTO TABLE lt_t006a
      FROM t006a
      WHERE spras EQ sy-langu.

    SELECT *
      INTO TABLE lt_aufk
      FROM aufk
      FOR ALL ENTRIES IN lt_data
      WHERE aufnr EQ lt_data-aufnr.
  ENDIF.
*  READ TABLE lt_data INDEX 1.
*  READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY aufnr = lt_data-aufnr
*                                                  vornr = lt_data-vornr.
*  IF sy-subrc EQ 0.
*    p_error = 'X'.
*
*    MESSAGE TEXT-014 TYPE 'S' DISPLAY LIKE 'E'.
*
*    EXIT.
*  ENDIF.

  SORT lt_data BY aufnr ASCENDING
                  vornr ASCENDING.


  IF gv_notif_grupal EQ '1'.
    REFRESH: lt_notif_grupal.
    LOOP AT lt_data.
      CLEAR: lt_notif_grupal.

      lt_notif_grupal-aufnr = lt_data-aufnr.
      lt_notif_grupal-vornr = lt_data-vornr.
      lt_notif_grupal-plnfl = lt_data-plnfl.
      lt_notif_grupal-ktsch = lt_data-ktsch.

      APPEND lt_notif_grupal.
    ENDLOOP.

    PERFORM f_completar_componente_grupal USING gv_nro
                                                zppe_notif_addreg-zztipo_notif
                                                lv_batch
                                                lt_notif_grupal[].
  ENDIF.

  LOOP AT lt_data.
    CLEAR: lt_makt,
           lv_batch,
           lv_lote,
           lv_tipo_notif.

    READ TABLE lt_makt  WITH KEY matnr = lt_data-plnbez.
    READ TABLE lt_t006a WITH KEY msehi = lt_data-gmein.

    IF zppe_notif_addreg-zztipo_notif IN gr_tipnot_anali.
      lv_comp = 1.
      lv_cant = 0.
      lv_loop = 1.
    ELSE.
*{-@0005
*      IF gv_noti_cantidad             EQ '1' AND
*         zppe_notif_addreg-anul_recha NE 'X'.
*}-@0005
      IF gv_noti_cantidad EQ '1'.
        lv_comp = zppe_notif_addreg-lmnga.
        lv_cant = zppe_notif_addreg-lmnga.
        lv_loop = 1.
      ELSE.
        lv_comp = zppe_notif_addreg-lmnga.
        lv_cant = 1.
        lv_loop = zppe_notif_addreg-lmnga.
      ENDIF.
    ENDIF.

    IF zppe_notif_addreg-charg IS INITIAL.
      lv_lote = lt_data-charg.
    ELSE.
      lv_lote = zppe_notif_addreg-charg.
    ENDIF.

    IF gv_hereda_lote EQ '1' AND
       lt_data-steus  EQ 'ZP03'.
      lv_batch = lv_lote.
    ENDIF.
    IF zppe_notif_addreg-zztipo_notif NOT IN gr_tipnot_anali AND
       zppe_notif_addreg-no_material  IS INITIAL             AND
       gv_notif_grupal                NE '1'                 AND
      "LAF
*       gv_notif_grupal2               IS INITIAL             AND
      "LAF
      NOT ( gv_rev_mov_herr                EQ '1'                 AND
            zppe_notif_addreg-zztipo_notif NOT IN gr_tipnot_aprob ).
      PERFORM f_completar_componente USING gv_nro
                                           lt_data-aufnr
                                           lt_data-vornr
                                           lt_data-plnfl
                                           lt_data-ktsch
                                           zppe_notif_addreg-zztipo_notif
                                           lv_comp
                                           lv_batch.
    ENDIF.

    IF gv_hereda_tipnot EQ '1' AND
       lv_lote          IS NOT INITIAL.
      REFRESH: lt_allocvaluesnum,
               lt_allocvalueschar,
               lt_allocvaluescurr,
               lt_return.

      READ TABLE lt_aufk  WITH KEY aufnr = lt_data-aufnr.

      CALL FUNCTION 'QMSP_MATERIAL_BATCH_CLASS_READ'
        EXPORTING
          i_matnr                = lt_data-plnbez
          i_charg                = lv_lote
          i_werks                = lt_aufk-werks
          i_mara_level           = 'X'
        IMPORTING
          e_class                = lv_class
          e_klart                = lv_klart
          e_obtab                = lv_obtab
        EXCEPTIONS
          no_class               = 1
          internal_error_classif = 2
          no_change_service      = 3
          OTHERS                 = 4.

      CONCATENATE lt_data-plnbez
                  lv_lote
                  INTO lv_objectkey.

      CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
        EXPORTING
          objectkey        = lv_objectkey
          objecttable      = lv_obtab
          classnum         = lv_class
          classtype        = lv_klart
          unvaluated_chars = 'X'
        TABLES
          allocvaluesnum   = lt_allocvaluesnum[]
          allocvalueschar  = lt_allocvalueschar[]
          allocvaluescurr  = lt_allocvaluescurr[]
          return           = lt_return[].

      READ TABLE lt_allocvalueschar WITH KEY charact = 'Z_TIPO_NOTIFICACION'.
      IF lt_allocvalueschar-value_char IS NOT INITIAL.
        lv_tipo_notif = lt_allocvalueschar-value_char.
      ELSE.
        lv_tipo_notif = gv_aprob.
      ENDIF.
    ELSE.
      lv_tipo_notif = zppe_notif_addreg-zztipo_notif.
    ENDIF.

    IF gv_val_notif_sec = '1'.
      CLEAR: lv_sig_pto,
             lv_sig_clave.

      CALL FUNCTION 'ZF_PP_ESTADO_ORDEN3'
        EXPORTING
          i_aufnr            = lt_data-aufnr
        IMPORTING
          e_siguiente_puesto = lv_sig_pto
          e_siguiente_clave  = lv_sig_clave.
    ENDIF.

    DO lv_loop TIMES.
      CLEAR: gwa_ordenes.

      ADD 1 TO gv_nro.

      "Nro
      gwa_ordenes-nro = gv_nro.

      "Orden
      gwa_ordenes-aufnr = lt_data-aufnr.

      "Operación
      gwa_ordenes-vornr = lt_data-vornr.

      "Clave Modelo Actual
      gwa_ordenes-ktsch = lt_data-ktsch.

      IF gv_val_notif_sec EQ '1'.
        "Próximo puesto de trabajo
        gwa_ordenes-arbpl_prox = lv_sig_pto.

        "Próxima clave modelo
        gwa_ordenes-ktsch_prox = lv_sig_clave.

        IF gv_notif_grupal EQ '1'.
          READ TABLE lt_data INTO DATA(lwa_aux) WITH KEY ktsch = lv_sig_clave.
          IF sy-subrc   NE 0 OR
             lv_sig_pto NE p_arbpl.
            gwa_ordenes-color = gc_amarillo.
          ENDIF.
        ELSE.
          IF gwa_ordenes-arbpl_prox NE p_arbpl           OR
             gwa_ordenes-ktsch_prox NE gwa_ordenes-ktsch.
            gwa_ordenes-color = gc_amarillo.
          ENDIF.
        ENDIF.
      ENDIF.
      "Fecha contabilización
      gwa_ordenes-budat = zppe_notif_addreg-budat.

      "Secuencia
      IF zppe_notif_addreg-aplfl IS INITIAL.
        gwa_ordenes-plnfl = lt_data-plnfl.
      ELSE.
        gwa_ordenes-plnfl = zppe_notif_addreg-aplfl.
      ENDIF.

      "Equipo
      gwa_ordenes-zzequipo = zppe_notif_addreg-equnr.

      "Lote
      gwa_ordenes-zzlote = lv_lote.

      "Material
      gwa_ordenes-plnbez = lt_data-plnbez.

      "Descripción
      gwa_ordenes-maktx = lt_makt-maktx.

      "UM
      gwa_ordenes-gmein     = lt_data-gmein.
      gwa_ordenes-gmein_txt = lt_t006a-mseh6.

      "Cantidad
      gwa_ordenes-lmnga = lv_cant.
*      IF zppe_notif_addreg-zztipo_notif IN gr_tipnot_anali.
*        gwa_ordenes-lmnga = 0.
*      ELSE.
**        gwa_ordenes-lmnga = zppe_notif_addreg-lmnga.
*        gwa_ordenes-lmnga = 1.
*      ENDIF.

      "Tipo de Notificación
      gwa_ordenes-zztipo_notif = lv_tipo_notif.

      "Lote de Corte
*    gwa_ordenes-zZLOTE_CORTE.

      "Lote de Curvado
*    gwa_ordenes-ZZLOTE_CURVADO.

      "Ciclo de TQ
*    gwa_ordenes-ZZCICLO_TQ.

      "Ciclo de Autoclave
      gwa_ordenes-zzciclo_auto = zppe_notif_addreg-zzciclo_auto.

      "Código de Lanzada
      gwa_ordenes-zzcod_lanza = zppe_notif_addreg-zzcod_lanza.

      "Lote de Carga
      gwa_ordenes-zzlote_carga = zppe_notif_addreg-lote_carga.

      "Defecto
      gwa_ordenes-zzdefecto = zppe_notif_addreg-zzdefecto.

      "Código Defecto
      gwa_ordenes-zzdefcod = zppe_notif_addreg-zzdefcod.

      "Origen
      gwa_ordenes-zzorigen = zppe_notif_addreg-zzorigen.

      "Código Origen
      gwa_ordenes-zzorcod = zppe_notif_addreg-zzorcod.

      "Causa
      gwa_ordenes-zzcausa = zppe_notif_addreg-zzcausa.

      "Código causa
      gwa_ordenes-zzcaucod = zppe_notif_addreg-zzcaucod.

      "Destino
      gwa_ordenes-zzdestino = zppe_notif_addreg-zzdestino.

      "Causa de Desviación
      gwa_ordenes-grund = zppe_notif_addreg-grund.

      "Observaciones
      gwa_ordenes-ltxa1 = zppe_notif_addreg-ltxa1.

      "@001 - EMAT - 13.09.2019 Insert
      DATA: aw_data_aux LIKE LINE OF lt_data.
      READ TABLE lt_data INTO aw_data_aux WITH KEY ktsch = zppe_notif_addreg-ktsch.

      "@001 - EMAT - 13.09.2019 Insert
      IF gv_raca_oblig       EQ '1'                     AND
      aw_data_aux IS NOT INITIAL.  "@001 - EMAT - 13.09.2019 Insert

        "@001 - EMAT - 13.09.2019 Delete
*         ( gwa_ordenes-vornr EQ zppe_notif_addreg-vornr OR
*           gwa_ordenes-ktsch EQ zppe_notif_addreg-ktsch ) .
        "@001 - EMAT - 13.09.2019 Delete

        "Número de Rack
        gwa_ordenes-nro_rack = zppe_notif_addreg-nro_rack.

        "Número de Caja
        gwa_ordenes-nro_caja = zppe_notif_addreg-nro_caja.
      ENDIF.

      "Número molde
      gwa_ordenes-nro_molde = zppe_notif_addreg-nro_molde.

      "Cámara de Horno
      gwa_ordenes-cam_horno = zppe_notif_addreg-cam_horno.

      "Sin consumo de materiales
      gwa_ordenes-no_material = zppe_notif_addreg-no_material.

      "Sin consumo de actividades
      gwa_ordenes-no_actividad = zppe_notif_addreg-no_actividad.

      "Anular con rechazo
      gwa_ordenes-anul_recha = zppe_notif_addreg-anul_recha.

      "Adicionales
*      gwa_ordenes-aufpl = lt_data-aufpl.
*      gwa_ordenes-aplzl = lt_data-aplzl.

      "CellTab
      IF gv_deriva_molde EQ '1'.
        gwa_ordenes-celltab = gt_celltab_molde.
      ENDIF.

      gwa_ordenes-zzdesperop   = zppe_notif_addreg-zzdesperop. "Insert @0002
      APPEND gwa_ordenes TO gt_ordenes.
    ENDDO.
  ENDLOOP.

  IF sy-subrc EQ 0.
    SORT gt_ordenes BY nro   ASCENDING.
*                       aufnr ASCENDING
*                       vornr ASCENDING.

*+{ @0004
    PERFORM: modificar_color_componentes.
*+} @0004

    IF gv_deriva_molde EQ '1'.
      CALL METHOD gr_grid01->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.
    ELSE.
      CALL METHOD gr_grid01->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.
    ENDIF.
    gv_addreg = '2'.
  ELSE.
    p_error = 'X'.

    MESSAGE TEXT-013 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_LAYOUT_LOG
*&---------------------------------------------------------------------*
FORM f_fill_layout_log .
  CLEAR: gwa_layout_log.

  gwa_layout_log-zebra      = 'X'.
  gwa_layout_log-cwidth_opt = 'X'.
  gwa_layout_log-info_fname = 'COLOR'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_LAYOUT_0600
*&---------------------------------------------------------------------*
FORM f_fill_layout_0600 .
  CLEAR: gwa_layout_0600.

*  gwa_layout_0600-zebra      = 'X'.
  gwa_layout_0600-cwidth_opt = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_LAYOUT_0601
*&---------------------------------------------------------------------*
FORM f_fill_layout_0601 .
  DATA: lwa_f4 TYPE lvc_s_f4.

  CLEAR: gwa_layout_0601.

*  gwa_layout_0600-zebra      = 'X'.
*  gwa_layout_0601-cwidth_opt = 'X'.


  lwa_f4-register   = 'X'.
  lwa_f4-getbefore  = space.
  lwa_f4-chngeafter = space.
  lwa_f4-fieldname  = 'CHARG'.
  INSERT lwa_f4 INTO TABLE gt_f4_tab0601.

  CALL METHOD gr_grid_0601->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4_tab0601.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_REGISTER_EVENT_TAB01
*&---------------------------------------------------------------------*
FORM f_register_event_tab01 .
  CALL METHOD gr_grid01->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.
*  CALL METHOD gr_grid01->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*  CALL METHOD gr_grid01->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

  CREATE OBJECT gr_events_tab01.
  SET HANDLER gr_events_tab01->handle_data_changed          FOR gr_grid01.
  SET HANDLER gr_events_tab01->handle_data_changed_finished FOR gr_grid01.
  SET HANDLER gr_events_tab01->handle_on_f4                 FOR gr_grid01.
  SET HANDLER gr_events_tab01->handle_toolbar               FOR gr_grid01.
  SET HANDLER gr_events_tab01->handle_user_command          FOR gr_grid01.

  CALL METHOD gr_grid01->set_toolbar_interactive.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_TRATAR_CAMBIO_TIPNOT
*&---------------------------------------------------------------------*
FORM f_tratar_cambio_tipnot .
  CHECK gv_mod_tipnot EQ 'X'.

  CLEAR: gv_mod_tipnot.

  LOOP AT gt_ordenes ASSIGNING <fs_ordenes>.
*    IF <fs_ordenes>-zztipo_notif IN gr_tipnot_aprob.
    IF <fs_ordenes>-zztipo_notif EQ gv_aprob.
      CLEAR: <fs_ordenes>-zzdefecto,
             <fs_ordenes>-zzdefcod,
             <fs_ordenes>-zzorigen,
             <fs_ordenes>-zzorcod,
             <fs_ordenes>-zzcausa,
             <fs_ordenes>-zzcaucod,
             <fs_ordenes>-zzdestino.
      <fs_ordenes>-celltab = gt_celltab_aprob.
    ELSE.
      <fs_ordenes>-celltab = gt_celltab_recha.
    ENDIF.
  ENDLOOP.
  CALL METHOD gr_grid01->refresh_table_display
    EXPORTING
      is_stable = gc_stable.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_TRATAR_CAMBIO_MOLDE
*&---------------------------------------------------------------------*
FORM f_tratar_cambio_molde USING p_index
                                 p_value.
  DATA: lt_aux  TYPE tt_makt WITH HEADER LINE,
        lt_mara TYPE tt_mara WITH HEADER LINE.

  REFRESH: lt_aux,
           lt_mara.

  READ TABLE gt_ordenes INTO gwa_ordenes INDEX p_index.

  LOOP AT gt_componentes INTO gwa_componentes WHERE nro EQ gwa_ordenes-nro.
    lt_aux-matnr = gwa_componentes-componente.
    APPEND lt_aux.
  ENDLOOP.

  SELECT *
    INTO TABLE lt_mara
    FROM mara
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr.

  LOOP AT gt_componentes ASSIGNING <fs_componentes> WHERE nro EQ gwa_ordenes-nro.
    CLEAR: lt_mara.

    READ TABLE lt_mara WITH KEY matnr = <fs_componentes>-componente.
    IF gv_deriva_molde EQ '1'    AND
       lt_mara-mtart   EQ 'ZHER'.
      <fs_componentes>-lote = p_value.
    ENDIF.
  ENDLOOP.

  CALL METHOD gr_grid02->refresh_table_display
    EXPORTING
      is_stable = gc_stable.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_LAYOUT_TAB01
*&---------------------------------------------------------------------*
FORM f_fill_layout_tab01 .
  DATA: lwa_f4 TYPE lvc_s_f4.
  CLEAR: gwa_layout_tab01.

  gwa_layout_tab01-cwidth_opt = 'X'.
  gwa_layout_tab01-detailinit = 'X'.
  gwa_layout_tab01-sel_mode   = 'A'.
  gwa_layout_tab01-stylefname = 'CELLTAB'.
  gwa_layout_tab01-info_fname = 'COLOR'.
  gwa_layout_tab01-no_rowins  = 'X'.

  lwa_f4-register   = 'X'.
  lwa_f4-getbefore  = space.
  lwa_f4-chngeafter = space.
  lwa_f4-fieldname  = 'ZZDEFECTO'.
  INSERT lwa_f4 INTO TABLE gt_f4_tab01.
  lwa_f4-fieldname  = 'ZZDEFCOD'.
  INSERT lwa_f4 INTO TABLE gt_f4_tab01.
  lwa_f4-fieldname  = 'ZZORIGEN'.
  INSERT lwa_f4 INTO TABLE gt_f4_tab01.
  lwa_f4-fieldname  = 'ZZORCOD'.
  INSERT lwa_f4 INTO TABLE gt_f4_tab01.
  lwa_f4-fieldname  = 'ZZCAUSA'.
  INSERT lwa_f4 INTO TABLE gt_f4_tab01.
  lwa_f4-fieldname  = 'ZZCAUCOD'.
  INSERT lwa_f4 INTO TABLE gt_f4_tab01.
  lwa_f4-fieldname  = 'ZZDESTINO'.
  INSERT lwa_f4 INTO TABLE gt_f4_tab01.
  lwa_f4-fieldname  = 'GRUND'.
  INSERT lwa_f4 INTO TABLE gt_f4_tab01.
  lwa_f4-fieldname  = 'ZZEQUIPO'.
  INSERT lwa_f4 INTO TABLE gt_f4_tab01.

  CALL METHOD gr_grid01->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4_tab01.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_LAYOUT_TAB02
*&---------------------------------------------------------------------*
FORM f_fill_layout_tab02 .
  DATA: lwa_f4 TYPE lvc_s_f4.

  CLEAR: gwa_layout_tab02.

  gwa_layout_tab02-detailinit = 'X'.
  gwa_layout_tab02-sel_mode   = 'A'.
  gwa_layout_tab02-stylefname = 'CELLTAB'.
  gwa_layout_tab02-info_fname = 'COLOR'.
  gwa_layout_tab02-no_rowins  = 'X'.

  lwa_f4-register   = 'X'.
  lwa_f4-getbefore  = space.
  lwa_f4-chngeafter = space.
  lwa_f4-fieldname  = 'UM_TXT'.
  INSERT lwa_f4 INTO TABLE gt_f4_tab02.
  lwa_f4-fieldname  = 'ALMACEN'.
  INSERT lwa_f4 INTO TABLE gt_f4_tab02.
  lwa_f4-fieldname  = 'LOTE'.
  INSERT lwa_f4 INTO TABLE gt_f4_tab02.

  CALL METHOD gr_grid02->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4_tab02.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_TAB01_F4_ZZDESTINO
*&---------------------------------------------------------------------*
FORM f_tab01_f4_zzdestino USING    p_row
                          CHANGING p_modi TYPE lvc_s_modi
                                   p_ok.
  DATA: lt_data TYPE tt_zzdestino      WITH HEADER LINE,
        lt_ret  TYPE tt_ddshretval     WITH HEADER LINE,
        lt_dest TYPE tt_zzdestino_data WITH HEADER LINE.

  CLEAR:   p_modi,
           p_ok.
  REFRESH: lt_data,
           lt_ret,
           lt_dest.

  READ TABLE gt_ordenes INTO gwa_ordenes INDEX p_row.

  SELECT crhd~arbpl
         afvc~ktsch
    INTO TABLE lt_dest
    FROM afko INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN crhd ON afvc~arbid EQ crhd~objid
    WHERE afko~aufnr EQ gwa_ordenes-aufnr
      AND crhd~werks EQ p_werks.

  LOOP AT lt_dest.
    CLEAR: lt_data.

    CONCATENATE lt_dest-arbpl
                lt_dest-ktsch
                INTO lt_data
                SEPARATED BY '-'.

    APPEND lt_data.
  ENDLOOP.

  lt_data = '00DESGÜACE'.
  APPEND lt_data.

  SORT lt_data BY zzdestino ASCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZZDESTINO'
      window_title    = TEXT-x53 "'Lista de valores'
      value_org       = 'S'
    TABLES
      value_tab       = lt_data[]
      return_tab      = lt_ret[]
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

*  CHECK gwa_ordenes-zztipo_notif NOT IN gr_tipnot_aprob
  CHECK gwa_ordenes-zztipo_notif NE gv_aprob
    AND gv_addreg                NE '2'.

  IF sy-subrc = 0.
    READ TABLE lt_ret INDEX 1.

    p_modi-row_id    = p_row.
    p_modi-fieldname = 'ZZDESTINO'.
    p_modi-value     = lt_ret-fieldval.

    p_ok = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_BORRAR_TAB01
*&---------------------------------------------------------------------*
FORM f_borrar_tab01 .
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE.

  CALL METHOD gr_grid01->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[].

  SORT lt_rows BY index DESCENDING.

  LOOP AT lt_rows.
    READ TABLE gt_ordenes INTO gwa_ordenes INDEX lt_rows-index.

    APPEND gwa_ordenes TO gt_ordenes_del.

    DELETE gt_ordenes INDEX lt_rows-index.

    DELETE gt_componentes WHERE nro EQ gwa_ordenes-nro.
*                                aufnr EQ gwa_ordenes-aufnr
*                            AND vornr EQ gwa_ordenes-vornr.
    DELETE gt_comp_dele WHERE nro EQ gwa_ordenes-nro.
  ENDLOOP.
  IF sy-subrc EQ 0.
    IF gt_ordenes IS INITIAL.
      PERFORM f_inicializar.
    ELSE.
      CLEAR: gv_nro.

      LOOP AT gt_ordenes ASSIGNING <fs_ordenes>.
        ADD 1 TO gv_nro.

        LOOP AT gt_componentes ASSIGNING <fs_componentes> WHERE nro EQ <fs_ordenes>-nro.
          <fs_componentes>-nro = gv_nro.
        ENDLOOP.

        <fs_ordenes>-nro = gv_nro.
      ENDLOOP.
    ENDIF.

    CALL METHOD gr_grid01->refresh_table_display
      EXPORTING
        is_stable = gc_stable.

    CALL METHOD gr_grid02->refresh_table_display
      EXPORTING
        is_stable = gc_stable.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = 'DUMMY'.
  ELSE.
    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ACTUALIZAR_LCORTE
*&---------------------------------------------------------------------*
FORM f_actualizar_lcorte .
  DATA: lt_afvu    TYPE tt_afvu    WITH HEADER LINE,
        lt_destino TYPE tt_destino WITH HEADER LINE,
        lt_ordenes TYPE tt_ordenes.

  IF gv_addreg EQ '1'.
    LOOP AT gt_ordenes_del INTO gwa_ordenes.
      UPDATE afvu SET usr00 = 'X'
                      usr01 = ''
                  WHERE aufpl EQ gwa_ordenes-aufpl
                    AND aplzl EQ gwa_ordenes-aplzl.
    ENDLOOP.

    LOOP AT gt_ordenes INTO gwa_ordenes WHERE zztipo_notif IN gr_tipnot_recha
                                          OR  zztipo_notif IN gr_tipnot_retra.
      UPDATE afvu SET usr00 = 'X'
                      usr01 = ''
                  WHERE aufpl EQ gwa_ordenes-aufpl
                    AND aplzl EQ gwa_ordenes-aplzl.
    ENDLOOP.
  ENDIF.

  IF gv_lote_corte IS NOT INITIAL.
    REFRESH: lt_afvu,
             lt_ordenes.

    lt_ordenes = gt_ordenes.

    CASE gv_lote_corte.
      WHEN '1'.
        DELETE lt_ordenes WHERE NOT ( zztipo_notif IN gr_tipnot_recha OR
                                      ( zztipo_notif   IN gr_tipnot_retra AND
                                        zzdestino+0(7) EQ '01CORTE' ) ).
        IF lt_ordenes[] IS NOT INITIAL.
          SELECT afvu~aufpl
                 afvu~aplzl
            INTO TABLE lt_afvu
            FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                      INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                      INNER JOIN crhd ON afvc~arbid EQ crhd~objid
                      INNER JOIN afvu ON afko~aufpl EQ afvu~aufpl AND
                                         afvc~aplzl EQ afvu~aplzl
            FOR ALL ENTRIES IN lt_ordenes
            WHERE afko~aufnr EQ lt_ordenes-aufnr
              AND crhd~arbpl EQ '01CORTE'
              AND crhd~werks EQ p_werks
              AND afvc~ktsch EQ lt_ordenes-ktsch.
        ENDIF.
      WHEN '2'.
        DELETE lt_ordenes WHERE NOT ( zztipo_notif IN gr_tipnot_recha OR
                                      ( zztipo_notif   IN gr_tipnot_retra AND
                                        zzdestino+0(7) EQ '01CORTE' ) ).
        IF lt_ordenes[] IS NOT INITIAL.

          SELECT afvu~aufpl
                 afvu~aplzl
            INTO TABLE lt_afvu
            FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                      INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                      INNER JOIN crhd ON afvc~arbid EQ crhd~objid
                      INNER JOIN afvu ON afko~aufpl EQ afvu~aufpl AND
                                         afvc~aplzl EQ afvu~aplzl
            FOR ALL ENTRIES IN lt_ordenes
            WHERE afko~aufnr EQ lt_ordenes-aufnr
              AND crhd~arbpl EQ '01CORTE'
              AND crhd~werks EQ p_werks.
        ENDIF.
      WHEN '3'.
        DELETE lt_ordenes WHERE NOT ( zztipo_notif IN gr_tipnot_recha OR
                                      ( zztipo_notif   IN gr_tipnot_retra AND
                                        zzdestino+0(7) EQ '01CORTE' ) ).
        IF lt_ordenes[] IS NOT INITIAL.
          SELECT afvu~aufpl
                 afvu~aplzl
            INTO TABLE lt_afvu
            FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                      INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                      INNER JOIN crhd ON afvc~arbid EQ crhd~objid
                      INNER JOIN afvu ON afko~aufpl EQ afvu~aufpl AND
                                         afvc~aplzl EQ afvu~aplzl
            FOR ALL ENTRIES IN lt_ordenes
            WHERE afko~aufnr EQ lt_ordenes-aufnr
              AND crhd~arbpl EQ '01CORTE'
              AND crhd~werks EQ p_werks
              AND afvc~ktsch EQ lt_ordenes-ktsch.
          IF sy-subrc NE 0.
            SELECT afvu~aufpl
                   afvu~aplzl
              INTO TABLE lt_afvu
              FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                        INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                        INNER JOIN crhd ON afvc~arbid EQ crhd~objid
                        INNER JOIN afvu ON afko~aufpl EQ afvu~aufpl AND
                                           afvc~aplzl EQ afvu~aplzl
              FOR ALL ENTRIES IN lt_ordenes
              WHERE afko~aufnr EQ lt_ordenes-aufnr
                AND crhd~arbpl EQ '01CORTE'
                AND crhd~werks EQ p_werks.
          ENDIF.
        ENDIF.
      WHEN '4'.
        DELETE lt_ordenes WHERE zztipo_notif NE gv_repro
                            AND zzdestino    NE space.
        IF lt_ordenes[] IS NOT INITIAL.
          LOOP AT lt_ordenes INTO DATA(lwa_ordenes).
            CLEAR: lt_destino.

            lt_destino-aufnr     = lwa_ordenes-aufnr.
            lt_destino-zzdestino = lwa_ordenes-zzdestino.

            PERFORM f_split_destino USING    lt_destino-zzdestino
                                    CHANGING lt_destino-arbpl
                                             lt_destino-ktsch.

            IF lt_destino-arbpl IS NOT INITIAL.
              APPEND lt_destino.
            ENDIF.
          ENDLOOP.

          IF lt_destino[] IS NOT INITIAL.
            SELECT afvu~aufpl
                   afvu~aplzl
              INTO TABLE lt_afvu
              FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                        INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                        INNER JOIN crhd ON afvc~arbid EQ crhd~objid
                        INNER JOIN afvu ON afko~aufpl EQ afvu~aufpl AND
                                           afvc~aplzl EQ afvu~aplzl
              FOR ALL ENTRIES IN lt_destino
              WHERE afko~aufnr EQ lt_destino-aufnr
                AND afvc~ktsch EQ lt_destino-ktsch
                AND crhd~arbpl EQ lt_destino-arbpl
                AND crhd~werks EQ p_werks.
          ENDIF.
        ENDIF.
    ENDCASE.

    LOOP AT lt_afvu.
      UPDATE afvu SET usr00 = 'X'
                      usr01 = ''
                  WHERE aufpl EQ lt_afvu-aufpl
                    AND aplzl EQ lt_afvu-aplzl.
    ENDLOOP.
  ENDIF.

  IF gv_lcorte_manual EQ '1'.
    REFRESH: lt_afvu,
             lt_ordenes.

    lt_ordenes = gt_ordenes.

    DELETE lt_ordenes WHERE zztipo_notif NE gv_aprob.

    IF lt_ordenes[] IS NOT INITIAL.
      SELECT afvu~aufpl
             afvu~aplzl
        INTO TABLE lt_afvu
        FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                  INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                  INNER JOIN crhd ON afvc~arbid EQ crhd~objid
                  INNER JOIN afvu ON afko~aufpl EQ afvu~aufpl AND
                                     afvc~aplzl EQ afvu~aplzl
        FOR ALL ENTRIES IN lt_ordenes
        WHERE afko~aufnr EQ lt_ordenes-aufnr
          AND afvc~vornr EQ lt_ordenes-vornr
          AND crhd~arbpl EQ p_arbpl
          AND crhd~werks EQ p_werks
          AND afvu~usr00 EQ 'X'.

      LOOP AT lt_afvu.
        UPDATE afvu SET usr00 = 'M'
                    WHERE aufpl EQ lt_afvu-aufpl
                      AND aplzl EQ lt_afvu-aplzl.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_REGISTER_EVENT_TAB02
*&---------------------------------------------------------------------*
FORM f_register_event_tab02 .
  CALL METHOD gr_grid02->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.
*  CALL METHOD gr_grid01->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
  CALL METHOD gr_grid02->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

  CREATE OBJECT gr_events_tab02.
  SET HANDLER gr_events_tab02->handle_data_changed          FOR gr_grid02.
  SET HANDLER gr_events_tab02->handle_data_changed_finished FOR gr_grid02.
  SET HANDLER gr_events_tab02->handle_toolbar               FOR gr_grid02.
  SET HANDLER gr_events_tab02->handle_user_command          FOR gr_grid02.
  SET HANDLER gr_events_tab02->handle_on_f4                 FOR gr_grid02.

  CALL METHOD gr_grid02->set_toolbar_interactive.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_INSERTAR_TAB02
*&---------------------------------------------------------------------*
FORM f_insertar_tab02 .
  DATA: lt_list     TYPE vrm_values WITH HEADER LINE.
  CLEAR:   gwa_componentes.
  REFRESH: lt_list.

  IF gt_ordenes IS INITIAL.
    MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT gt_ordenes INTO gwa_ordenes.
    lt_list-key  = gwa_ordenes-nro.
    lt_list-text = gwa_ordenes-nro.
    APPEND lt_list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'GWA_COMPONENTES-NRO'
      values          = lt_list[]
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  CALL SCREEN 0400 STARTING AT 10 10.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_BORRAR_TAB02
*&---------------------------------------------------------------------*
FORM f_borrar_tab02 .
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE.

  CALL METHOD gr_grid02->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[].

  SORT lt_rows BY index DESCENDING.

  LOOP AT lt_rows.
    READ TABLE gt_componentes INTO gwa_componentes INDEX lt_rows-index.

    LOOP AT gt_componentes ASSIGNING <fs_componentes> WHERE nro        EQ gwa_componentes-nro
                                                        AND componente EQ gwa_componentes-componente.
      <fs_componentes>-mod = 'X'.
    ENDLOOP.

    IF gwa_componentes-new IS INITIAL.
      APPEND gwa_componentes TO gt_comp_dele.
    ENDIF.

    DELETE gt_componentes INDEX lt_rows-index.
  ENDLOOP.

  IF sy-subrc EQ 0.
    CALL METHOD gr_grid02->refresh_table_display
      EXPORTING
        is_stable = gc_stable.
  ELSE.
    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB02
*&---------------------------------------------------------------------*
FORM f_add_tab02 .
  DATA: lv_aufnr TYPE aufnr,
        lv_error TYPE c.

  IF gv_consumo_herra EQ '1'.
    SELECT SINGLE COUNT(*)
      FROM mara
    WHERE matnr EQ gwa_componentes-componente
      AND mtart EQ 'ZHER'.
    IF sy-subrc EQ 0.
      gwa_componentes-cantidad = 1.
    ENDIF.
  ENDIF.

  PERFORM f_add_tab02_valida CHANGING lv_error.
  CHECK lv_error IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gwa_componentes-aufnr
    IMPORTING
      output = lv_aufnr.

  READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY aufnr = lv_aufnr
                                                  vornr = gwa_componentes-vornr.
  IF sy-subrc NE 0.
    MESSAGE TEXT-010 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  gwa_componentes-aufnr = lv_aufnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gwa_componentes-componente
    IMPORTING
      output = gwa_componentes-componente.

  SELECT SINGLE maktx
    INTO gwa_componentes-descripcion
    FROM makt
    WHERE matnr EQ gwa_componentes-componente
      AND spras EQ sy-langu.

  SELECT SINGLE msehi
    INTO gwa_componentes-um
    FROM t006a
    WHERE spras EQ sy-langu
      AND mseh6 EQ gwa_componentes-um_txt.

  SELECT SINGLE COUNT(*)
    FROM mara
    WHERE matnr EQ gwa_componentes-componente
      AND xchpf EQ 'X'.
  IF sy-subrc EQ 0.
    gwa_componentes-celltab = gt_celltab_xchpf.
  ELSE.
    CLEAR: gwa_componentes-lote.
  ENDIF.

  APPEND gwa_componentes TO gt_componentes.

  SORT gt_componentes BY nro        ASCENDING.
*                         aufnr      ASCENDING
*                         vornr      ASCENDING
*                         componente ASCENDING.

  CALL METHOD gr_grid02->refresh_table_display
    EXPORTING
      is_stable = gc_stable.

  MESSAGE TEXT-003 TYPE 'S'.
  LEAVE TO SCREEN 0.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_TAB01_F4_ZZDEFORCAU
*&---------------------------------------------------------------------*
FORM f_tab01_f4_zzdeforcau USING    p_row
                                    p_field
                           CHANGING p_modi TYPE lvc_s_modi
                                    p_ok.
  DATA: lv_val1 TYPE string,
        lv_val2 TYPE string.
  FIELD-SYMBOLS: <fs_val1> TYPE any,
                 <fs_val2> TYPE any.
  CLEAR: p_modi,
         p_ok.

  CASE p_field.
    WHEN 'ZZDEFECTO' OR
         'ZZDEFCOD'.
      lv_val1 = 'ZZDEFECTO'.
      lv_val2 = 'ZZDEFCOD'.
    WHEN 'ZZORIGEN' OR
         'ZZORCOD'.
      lv_val1 = 'ZZORIGEN'.
      lv_val2 = 'ZZORCOD'.
    WHEN 'ZZCAUSA' OR
         'ZZCAUCOD'.
      lv_val1 = 'ZZCAUSA'.
      lv_val2 = 'ZZCAUCOD'.
  ENDCASE.

  READ TABLE gt_ordenes ASSIGNING <fs_ordenes> INDEX p_row.
  ASSIGN COMPONENT lv_val1 OF STRUCTURE <fs_ordenes> TO <fs_val1>.
  ASSIGN COMPONENT lv_val2 OF STRUCTURE <fs_ordenes> TO <fs_val2>.

  PERFORM f_zcode_pf4_i15 USING    p_field
                                   <fs_ordenes>-zzdefcod
                                   <fs_ordenes>-zzorigen
                          CHANGING <fs_val1>
                                   <fs_val2>.

*  CHECK <fs_ordenes>-zztipo_notif NOT IN gr_tipnot_aprob
  CHECK <fs_ordenes>-zztipo_notif NE gv_aprob
    AND gv_addreg                 NE '2'.

  IF sy-subrc = 0.
*    p_modi-row_id    = p_row.
*    p_modi-fieldname = p_field.
*    p_modi-value     = lt_ret-fieldval.

    p_ok = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_TAB02_F4_UM_TXT
*&---------------------------------------------------------------------*
FORM f_tab02_f4_um_txt USING    p_row
                       CHANGING p_modi TYPE lvc_s_modi
                                p_ok.
  DATA: lt_aux  TYPE tt_t006a2         WITH HEADER LINE,
        lt_data TYPE tt_um_txt         WITH HEADER LINE,
        lt_ret  TYPE tt_ddshretval     WITH HEADER LINE,
        lt_dest TYPE tt_zzdestino_data WITH HEADER LINE.

  CLEAR:   p_modi,
           p_ok.
  REFRESH: lt_data,
           lt_ret,
           lt_dest.

  SELECT *
    INTO TABLE lt_aux
    FROM t006a
    WHERE spras EQ sy-langu.

  LOOP AT lt_aux.
    lt_data-um_txt = lt_aux-mseh3.
    lt_data-txt    = lt_aux-msehl.
    APPEND lt_data.
  ENDLOOP.

  SORT lt_data BY um_txt ASCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'UM_TXT'
      window_title    = TEXT-x53 "'Lista de valores'
      value_org       = 'S'
    TABLES
      value_tab       = lt_data[]
      return_tab      = lt_ret[]
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    READ TABLE lt_ret INDEX 1.

    p_modi-row_id    = p_row.
    p_modi-fieldname = 'UM_TXT'.
    p_modi-value     = lt_ret-fieldval.

    p_ok = 'X'.

    READ TABLE gt_componentes ASSIGNING <fs_componentes> INDEX p_row.
    READ TABLE lt_aux WITH KEY mseh3 = lt_ret-fieldval.
    <fs_componentes>-um = lt_aux-msehi.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_TAB02_F4_ALMACEN
*&---------------------------------------------------------------------*
FORM f_tab02_f4_almacen USING    p_row
                        CHANGING p_modi TYPE lvc_s_modi
                                 p_ok.
  DATA: lt_data TYPE tt_t001l          WITH HEADER LINE,
        lt_ret  TYPE tt_ddshretval     WITH HEADER LINE,
        lt_dest TYPE tt_zzdestino_data WITH HEADER LINE.

  CLEAR:   p_modi,
           p_ok.
  REFRESH: lt_data,
           lt_ret,
           lt_dest.

*  SELECT lgort
*         lgobe
*    INTO TABLE lt_data
*    FROM t001l
*    WHERE werks EQ p_werks.
*
*  SORT lt_data BY lgort ASCENDING.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'LGORT'
*      window_title    = 'Lista de valores'
*      value_org       = 'S'
*    TABLES
*      value_tab       = lt_data[]
*      return_tab      = lt_ret[]
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.

  READ TABLE gt_componentes ASSIGNING <fs_componentes> INDEX p_row.
  PERFORM valreq_tab02_almacen USING    <fs_componentes>-componente
                               CHANGING <fs_componentes>-almacen .

*  IF sy-subrc = 0.
*    READ TABLE lt_ret INDEX 1.

  p_modi-row_id    = p_row.
  p_modi-fieldname = 'ALMACEN'.
  p_modi-value     = <fs_componentes>-almacen.

  p_ok = 'X'.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_TAB02_F4_LOTE
*&---------------------------------------------------------------------*
FORM f_tab02_f4_lote USING    p_row
                     CHANGING p_modi TYPE lvc_s_modi
                              p_ok.
  DATA: lt_data TYPE tt_mcha          WITH HEADER LINE,
        lt_ret  TYPE tt_ddshretval     WITH HEADER LINE,
        lt_dest TYPE tt_zzdestino_data WITH HEADER LINE.

  CLEAR:   p_modi,
           p_ok.
  REFRESH: lt_data,
           lt_ret,
           lt_dest.

*  READ TABLE gt_componentes INTO gwa_componentes INDEX p_row.
*
*  SELECT matnr
*         werks
*         charg
*    INTO TABLE lt_data
*    FROM mcha
*    WHERE matnr EQ gwa_componentes-componente
*      AND werks EQ p_werks.
*
*  SORT lt_data BY charg ASCENDING.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'CHARG'
*      window_title    = 'Lista de valores'
*      value_org       = 'S'
*    TABLES
*      value_tab       = lt_data[]
*      return_tab      = lt_ret[]
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*  IF sy-subrc = 0.
*    READ TABLE lt_ret INDEX 1.

  READ TABLE gt_componentes ASSIGNING <fs_componentes> INDEX p_row.
  PERFORM valreq_tab02_lote USING    <fs_componentes>-componente
                                     <fs_componentes>-almacen
                            CHANGING <fs_componentes>-lote
                                     p_ok.
  IF p_ok EQ 'X'.
    p_modi-row_id        = p_row.
    p_modi-fieldname     = 'LOTE'.
    p_modi-value         = <fs_componentes>-lote.
    <fs_componentes>-mod = 'X'.
  ENDIF.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_TAB01_F4_GRUND
*&---------------------------------------------------------------------*
FORM f_tab01_f4_grund USING    p_row
                     CHANGING p_modi TYPE lvc_s_modi
                              p_ok.
  DATA: lt_data TYPE tt_trugt          WITH HEADER LINE,
        lt_ret  TYPE tt_ddshretval     WITH HEADER LINE,
        lt_dest TYPE tt_zzdestino_data WITH HEADER LINE.

  CLEAR:   p_modi,
           p_ok.
  REFRESH: lt_data,
           lt_ret,
           lt_dest.

  READ TABLE gt_componentes INTO gwa_componentes INDEX p_row.

  SELECT grund
         grdtx
    INTO TABLE lt_data
    FROM trugt
    WHERE spras EQ sy-langu
      AND werks EQ p_werks.

  SORT lt_data BY grund ASCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'GRUND'
      window_title    = TEXT-x53 "'Lista de valores'
      value_org       = 'S'
    TABLES
      value_tab       = lt_data[]
      return_tab      = lt_ret[]
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    READ TABLE lt_ret INDEX 1.

    p_modi-row_id    = p_row.
    p_modi-fieldname = 'GRUND'.
    p_modi-value     = lt_ret-fieldval.

    p_ok = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDAR_CAMPOS
*&---------------------------------------------------------------------*
FORM f_validar_campos CHANGING p_error.
  DATA: lt_trug        TYPE tt_trug           WITH HEADER LINE,
        lt_t006a       TYPE tt_t006a2         WITH HEADER LINE,
        lt_t001l       TYPE tt_t001l2         WITH HEADER LINE,
        lt_mcha        TYPE tt_mcha2          WITH HEADER LINE,
        lt_qpgr        TYPE tt_qpgr           WITH HEADER LINE,
        lt_qpcd        TYPE tt_qpcd           WITH HEADER LINE,
        lt_valcant     TYPE tt_valcant        WITH HEADER LINE,
        lt_ordenes     TYPE tt_ordenes        WITH HEADER LINE,
        lt_embalaje    TYPE tt_caja_rack      WITH HEADER LINE,
        lt_embalaux    TYPE tt_caja_rack_full WITH HEADER LINE,
        lt_hrs_parada  TYPE tt_hrs_parada     WITH HEADER LINE,
        lt_mara        TYPE tt_mara           WITH HEADER LINE,
        lt_componentes TYPE tt_componentes    WITH HEADER LINE,
        lt_mcha_aux    TYPE tt_mcha           WITH HEADER LINE,
        lwa_imrg       TYPE imrg,
        lv_txt         TYPE string,
        lv_tabix       TYPE n LENGTH 3,
        lv_material    TYPE matnr18,
        lv_h_paradas   TYPE zhoras_parada,
        lv_sig_pto     TYPE ze_destino,
        lv_sig_clave   TYPE ktsch,
        lv_menge       TYPE bstmg,
        lv_penult      TYPE imrc_cntrr.

  FIELD-SYMBOLS: <fs_ordenes>  TYPE ty_ordenes,
                 <fs_embalaux> TYPE ty_caja_rack.

  CLEAR:   p_error,
           gv_updt_hrs_parada.
  REFRESH: lt_trug,
           lt_t006a,
           lt_t001l,
           lt_mcha,
           lt_qpgr,
           lt_qpcd,
           lt_embalaje,
           lt_embalaux.

  SELECT aufk~aufnr,
         afvc~vornr
    INTO TABLE @DATA(lt_vornr)
    FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN crhd ON afvc~arbid EQ crhd~objid
    FOR ALL ENTRIES IN @gt_ordenes
    WHERE aufk~aufnr EQ @gt_ordenes-aufnr
      AND crhd~arbpl EQ @p_arbpl
      AND crhd~werks EQ @p_werks.

  SELECT *
    INTO TABLE lt_trug
    FROM trug
    WHERE werks EQ p_werks.

  SELECT *
    INTO TABLE lt_t006a
    FROM t006a
    WHERE spras EQ sy-langu.

  SELECT *
    INTO TABLE lt_t001l
    FROM t001l
    WHERE werks EQ p_werks.

  SELECT *
    INTO TABLE lt_mcha
    FROM mcha
    WHERE werks EQ p_werks.

  SELECT *
    INTO TABLE lt_qpgr
    FROM qpgr
    WHERE katalogart IN ('X', 'Z', 'J')."('9', 'E', '5').

  SELECT *
    INTO TABLE lt_qpcd
    FROM qpcd
    WHERE katalogart IN ('X', 'Z', 'J')."('9', 'E', '5').

  LOOP AT gt_ordenes INTO gwa_ordenes.
    lv_tabix = sy-tabix.

    READ TABLE gt_tipnot_oblig INTO gwa_tipnot_oblig WITH KEY atwtb = gwa_ordenes-zztipo_notif.
    READ TABLE gt_cawnt INTO gwa_cawnt WITH KEY atwtb = gwa_ordenes-zztipo_notif.

    "Tipo de notificación
    AUTHORITY-CHECK OBJECT 'ZPP_TIPNOT'
     ID 'ZPP_TIPNOT' FIELD gwa_cawnt-atzhl+2(2).
    IF sy-subrc NE 0.
      p_error = 'X'.

      MESSAGE TEXT-026 TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    "Origen
    IF gwa_tipnot_oblig-oblig+0(1) = '1'.
      IF gwa_ordenes-zzorigen IS INITIAL OR
         gwa_ordenes-zzorcod  IS INITIAL.
        p_error = 'X'.

        lv_txt = TEXT-024.
        REPLACE '&1' IN lv_txt WITH TEXT-073."'Origen'.
        REPLACE '&2' IN lv_txt WITH lv_tabix.

        MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    "Causa
    IF gwa_tipnot_oblig-oblig+1(1) = '1'.
      IF gwa_ordenes-zzcausa IS INITIAL OR
         gwa_ordenes-zzcaucod  IS INITIAL.
        p_error = 'X'.

        lv_txt = TEXT-024.
        REPLACE '&1' IN lv_txt WITH TEXT-x26. "'Causa'.
        REPLACE '&2' IN lv_txt WITH lv_tabix.

        MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    "Defecto
    IF gwa_tipnot_oblig-oblig+2(1) = '1'.
      IF gwa_ordenes-zzdefecto IS INITIAL OR
         gwa_ordenes-zzdefcod  IS INITIAL.
        p_error = 'X'.

        lv_txt = TEXT-024.
        REPLACE '&1' IN lv_txt WITH TEXT-074. "'Defecto'.
        REPLACE '&2' IN lv_txt WITH lv_tabix.

        MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    "Destino
    IF gwa_tipnot_oblig-oblig+3(1) = '1'.
      IF gwa_ordenes-zzdestino IS INITIAL.
        p_error = 'X'.

        lv_txt = TEXT-024.
        REPLACE '&1' IN lv_txt WITH TEXT-x28. "'Destino'.
        REPLACE '&2' IN lv_txt WITH lv_tabix.

        MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    "Causa de desviación
    IF gwa_tipnot_oblig-oblig+4(1) = '1'.
      IF gwa_ordenes-grund IS INITIAL.
        p_error = 'X'.

        lv_txt = TEXT-024.
        REPLACE '&1' IN lv_txt WITH TEXT-070. "'Causa de desviación'.
        REPLACE '&2' IN lv_txt WITH lv_tabix.

        MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    IF gwa_ordenes-grund IS NOT INITIAL.
      READ TABLE lt_trug WITH KEY grund = gwa_ordenes-grund.
      IF sy-subrc NE 0.
        p_error = 'X'.

        lv_txt = TEXT-015.
        REPLACE '&1' IN lv_txt WITH TEXT-075. "'Motivo de Desviación'.
        REPLACE '&2' IN lv_txt WITH lv_tabix.

        MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    IF gwa_ordenes-zzdefecto IS NOT INITIAL.
      READ TABLE lt_qpgr WITH KEY katalogart = 'X'"'9'
                                  codegruppe = gwa_ordenes-zzdefecto.
      IF sy-subrc NE 0.
        p_error = 'X'.

        lv_txt = TEXT-015.
        REPLACE '&1' IN lv_txt WITH TEXT-074. "'Defecto'.
        REPLACE '&2' IN lv_txt WITH lv_tabix.

        MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    IF gwa_ordenes-zzdefcod IS NOT INITIAL.
      IF gwa_ordenes-zzdefecto IS INITIAL.
        READ TABLE lt_qpcd WITH KEY katalogart = 'X'"'9'
                                    code       = gwa_ordenes-zzdefcod.
      ELSE.
        READ TABLE lt_qpcd WITH KEY katalogart = 'X'"'9'
                                    codegruppe = gwa_ordenes-zzdefecto
                                    code       = gwa_ordenes-zzdefcod.
      ENDIF.
      IF sy-subrc NE 0.
        p_error = 'X'.

        lv_txt = TEXT-015.
        REPLACE '&1' IN lv_txt WITH TEXT-076. "'Código Defecto'.
        REPLACE '&2' IN lv_txt WITH lv_tabix.

        MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    IF gwa_ordenes-zzorigen IS NOT INITIAL.
      READ TABLE lt_qpgr WITH KEY katalogart = 'Z'"'E'
                                  codegruppe = gwa_ordenes-zzorigen.
      IF sy-subrc NE 0.
        p_error = 'X'.

        lv_txt = TEXT-015.
        REPLACE '&1' IN lv_txt WITH TEXT-073. "'Origen'.
        REPLACE '&2' IN lv_txt WITH lv_tabix.

        MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    IF gwa_ordenes-zzorcod IS NOT INITIAL.
      IF gwa_ordenes-zzorigen IS INITIAL.
        READ TABLE lt_qpcd WITH KEY katalogart = 'Z'"'E'
                                    code       = gwa_ordenes-zzorcod.
      ELSE.
        READ TABLE lt_qpcd WITH KEY katalogart = 'Z'"'E'
                                    codegruppe = gwa_ordenes-zzorigen
                                    code       = gwa_ordenes-zzorcod.
      ENDIF.
      IF sy-subrc NE 0.
        p_error = 'X'.

        lv_txt = TEXT-015.
        REPLACE '&1' IN lv_txt WITH TEXT-077. "'Código Origen'.
        REPLACE '&2' IN lv_txt WITH lv_tabix.

        MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    IF gwa_ordenes-zzcausa IS NOT INITIAL.
      READ TABLE lt_qpgr WITH KEY katalogart = 'J'"'5'
                                  codegruppe = gwa_ordenes-zzcausa.
      IF sy-subrc NE 0.
        p_error = 'X'.

        lv_txt = TEXT-015.
        REPLACE '&1' IN lv_txt WITH TEXT-x26. "'Causa'.
        REPLACE '&2' IN lv_txt WITH lv_tabix.

        MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    IF gwa_ordenes-zzcaucod IS NOT INITIAL.
      IF gwa_ordenes-zzcausa IS INITIAL.
        READ TABLE lt_qpcd WITH KEY katalogart = 'J'"'5'
                                    code       = gwa_ordenes-zzcaucod.
      ELSE.
        READ TABLE lt_qpcd WITH KEY katalogart = 'J'"'5'
                                    codegruppe = gwa_ordenes-zzcausa
                                    code       = gwa_ordenes-zzcaucod.
      ENDIF.
      IF sy-subrc NE 0.
        p_error = 'X'.

        lv_txt = TEXT-015.
        REPLACE '&1' IN lv_txt WITH TEXT-x27. "'Código Causa'.
        REPLACE '&2' IN lv_txt WITH lv_tabix.

        MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    READ TABLE lt_embalaux ASSIGNING <fs_embalaux> WITH KEY matnr      = gwa_ordenes-plnbez
                                                            zznro_rack = gwa_ordenes-nro_rack
                                                            zznro_caja = gwa_ordenes-nro_caja.
    IF sy-subrc NE 0.
      INSERT INITIAL LINE INTO lt_embalaux ASSIGNING <fs_embalaux> INDEX 1.

      <fs_embalaux>-matnr      = gwa_ordenes-plnbez.
      <fs_embalaux>-zznro_rack = gwa_ordenes-nro_rack.
      <fs_embalaux>-zznro_caja = gwa_ordenes-nro_caja.
    ENDIF.

    ADD gwa_ordenes-lmnga TO <fs_embalaux>-cant.
  ENDLOOP.

  CHECK p_error IS INITIAL.

  IF gv_consumo_lamina EQ '1'         AND
     gt_lot_vidrio     IS NOT INITIAL.
    READ TABLE gt_lot_vidrio TRANSPORTING NO FIELDS WITH KEY charg = ''.
    IF sy-subrc EQ 0.
      MESSAGE TEXT-060 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  "Validar cantidad total vs entregada
  REFRESH: lt_valcant,
           lt_ordenes.

  SELECT  afko~aufnr
          afvc~vornr
          afpo~psmng
          afpo~wemng
    INTO TABLE lt_valcant
    FROM afko INNER JOIN afpo ON afko~aufnr EQ afpo~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN crhd ON afvc~arbid EQ crhd~objid
    FOR ALL ENTRIES IN gt_ordenes
    WHERE afko~aufnr EQ gt_ordenes-aufnr
      AND afvc~vornr EQ gt_ordenes-vornr
      AND crhd~arbpl EQ p_arbpl
      AND crhd~werks EQ p_werks
      AND afvc~steus EQ 'ZP03'.
  IF sy-subrc EQ 0.

    LOOP AT gt_ordenes INTO gwa_ordenes.
      READ TABLE lt_ordenes ASSIGNING <fs_ordenes> WITH KEY aufnr = gwa_ordenes-aufnr
                                                            vornr = gwa_ordenes-vornr.
      IF sy-subrc EQ 0.
        ADD 1 TO <fs_ordenes>-lmnga.
      ELSE.
        lt_ordenes-aufnr = gwa_ordenes-aufnr.
        lt_ordenes-vornr = gwa_ordenes-vornr.
        lt_ordenes-lmnga = 1.
        APPEND lt_ordenes.
      ENDIF.
    ENDLOOP.

    REFRESH: gt_log.
    LOOP AT lt_ordenes.
      READ TABLE lt_valcant WITH KEY aufnr = lt_ordenes-aufnr
                                     vornr = lt_ordenes-vornr.
      IF sy-subrc         EQ 0                                      AND
         lt_ordenes-lmnga GT ( lt_valcant-psmng - lt_valcant-wemng ).

        LOOP AT gt_ordenes INTO gwa_ordenes WHERE aufnr EQ lt_ordenes-aufnr
                                              AND vornr EQ lt_ordenes-vornr.
          CLEAR: gwa_log.

          gwa_log-aufnr = gwa_ordenes-aufnr.
          gwa_log-vornr = gwa_ordenes-vornr.
          gwa_log-nro   = gwa_ordenes-nro.
          gwa_log-notif = 'No'.
          gwa_log-msg   = TEXT-038.

          APPEND gwa_log TO gt_log.
        ENDLOOP.

        p_error = 'X'.
      ENDIF.
    ENDLOOP.

    IF gt_log IS NOT INITIAL.
      PERFORM f_mostrar_log.
    ENDIF.
  ENDIF.

  "Validación de horas paradas
  CHECK p_error IS INITIAL.

  IF gv_ver_hora_max EQ '1'.
    REFRESH: lt_ordenes,
             lt_hrs_parada.

    SELECT *
      INTO TABLE lt_hrs_parada
      FROM ztto_hrs_parada
      WHERE werks   EQ p_werks
        AND zactivo EQ 'X'.

    lt_ordenes[] = gt_ordenes.
    SORT lt_ordenes BY aufnr.
    DELETE ADJACENT DUPLICATES FROM lt_ordenes COMPARING aufnr.

    SELECT aufk~aufnr,
           aufk~zzsiguiente_puesto,
           afko~dispo
      INTO TABLE @DATA(lt_aufk)
      FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
      FOR ALL ENTRIES IN @lt_ordenes
      WHERE aufk~aufnr EQ @lt_ordenes-aufnr.

"EMAT #1- Enrique Aguirre - AGP-476 - Según:
"1) Notificaciones para centro Colombia,
"2) Ordenes segun tabla de ztconstantes
"3)Color
DATA: rbg_ord_zppc0002 TYPE RANGE OF AUFNR.
      SELECT SIGNO, OPCION, valor1 INTO TABLE @DATA(it_zppcc0001_plnbez)
          FROM ztconstantes
          WHERE modulo     EQ 'PP'
            AND aplicacion EQ ''
            AND programa   EQ 'ZPPC0001'
            AND campo      EQ 'PLNBEZ'.

rbg_ord_zppc0002 = VALUE #( FOR wa_zppcc0001_plnbez IN it_zppcc0001_plnbez
                  ( sign = wa_zppcc0001_plnbez-signo
                    option = wa_zppcc0001_plnbez-opcion
                    low = wa_zppcc0001_plnbez-valor1 )
                  ).

  TYPES: BEGIN OF ty_aufnr_data,
             werks TYPE WERKS_D,
             PLNBEZ TYPE caufv-PLNBEZ,
             zzcolor TYPE caufv-zzcolor,
        END OF  ty_aufnr_data.

  DATA: wa_aufnr_data TYPE ty_aufnr_data.
  CLEAR wa_aufnr_data.

  READ TABLE lt_ordenes ASSIGNING FIELD-SYMBOL(<FS_AUFNR>) INDEX 1.
  SELECT SINGLE aufk~werks, caufv~PLNBEZ, caufv~zzcolor INTO @wa_aufnr_data
  FROM aufk inner join caufv
            on     aufk~aufnr = caufv~aufnr
               AND aufk~aufnr = @<FS_AUFNR>-AUFNR.

  SHIFT wa_aufnr_data-PLNBEZ LEFT DELETING LEADING '0'.
"EMAT #1- Enrique Aguirre - AGP-476 - I
 IF wa_aufnr_data-werks NE 'CO01'. "EMAT #1- Enrique Aguirre - AGP-476 - I
"Rutina original ya existente para BRASIL
    LOOP AT lt_ordenes.
      CLEAR: lv_h_paradas.

      READ TABLE lt_aufk INTO DATA(lwa_aufk) WITH KEY aufnr = lt_ordenes-aufnr.

      CALL FUNCTION 'ZF_PP_ESTADO_ORDEN3'
        EXPORTING
          i_aufnr            = lt_ordenes-aufnr
        IMPORTING
          e_siguiente_puesto = lv_sig_pto
          e_horas_parada     = lv_h_paradas.

      READ TABLE lt_hrs_parada WITH KEY arbpl = p_arbpl
                                        dispo = lwa_aufk-dispo.
      CHECK sy-subrc EQ 0 AND
            ( lt_hrs_parada-zbloq_analisis EQ 'X' OR
              lt_hrs_parada-zbloqueado     EQ 'X' ).

      IF p_arbpl NE lv_sig_pto.
        MESSAGE TEXT-062 TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
        EXIT.
      ENDIF.

      IF ( lt_hrs_parada-zbloq_analisis EQ 'X' AND
           lt_hrs_parada-timqty2        GT lv_h_paradas ) OR
         ( lt_hrs_parada-zbloqueado EQ 'X' AND
           lt_hrs_parada-timqty     GT lv_h_paradas ).
        MESSAGE TEXT-062 TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
        EXIT.
      ELSE.
        gv_updt_hrs_parada = 'X'.
      ENDIF.
    ENDLOOP.
else. "Rutina para COLOMBIA - 29.09.2020 AGP-476 - EMAT-Enrique Aguirre
    LOOP AT lt_ordenes.
      CLEAR: lv_h_paradas.

      READ TABLE lt_aufk INTO DATA(lwa_aufk_co) WITH KEY aufnr = lt_ordenes-aufnr.

      CALL FUNCTION 'ZF_PP_ESTADO_ORDEN3'
        EXPORTING
          i_aufnr            = lt_ordenes-aufnr
        IMPORTING
          e_siguiente_puesto = lv_sig_pto
          e_horas_parada     = lv_h_paradas.

      READ TABLE lt_hrs_parada WITH KEY arbpl = p_arbpl
                                        dispo = lwa_aufk_co-dispo.
      CHECK sy-subrc EQ 0 AND
            ( lt_hrs_parada-zbloq_analisis EQ 'X' OR
              lt_hrs_parada-zbloqueado     EQ 'X' ).

      IF p_arbpl NE lv_sig_pto.
        MESSAGE TEXT-062 TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
        EXIT.
      ENDIF.

      IF ( lt_hrs_parada-zbloq_analisis EQ 'X' OR
           lt_hrs_parada-zbloqueado EQ 'X' ) AND
           lt_hrs_parada-timqty2 GT lv_h_paradas AND
           wa_aufnr_data-PLNBEZ in rbg_ord_zppc0002.
              MESSAGE TEXT-062 TYPE 'S' DISPLAY LIKE 'E'.
              p_error = 'X'.
              EXIT.
      ELSEIF ( lt_hrs_parada-zbloq_analisis EQ 'X' OR
               lt_hrs_parada-zbloqueado EQ 'X' ) AND
               lt_hrs_parada-timqty2 GT lv_h_paradas AND
               wa_aufnr_data-PLNBEZ IN rbg_ord_zppc0002 AND
               ( wa_aufnr_data-zzcolor EQ 3 OR wa_aufnr_data-zzcolor EQ 4 ).
                  "Permite notificar.
                  gv_updt_hrs_parada = 'X'.
      ELSEIF ( lt_hrs_parada-zbloq_analisis EQ 'X' OR
               lt_hrs_parada-zbloqueado EQ 'X' ) AND
               lt_hrs_parada-timqty2 GT lv_h_paradas AND
               wa_aufnr_data-PLNBEZ NOT IN rbg_ord_zppc0002.
                  "Permite notificar.
                  gv_updt_hrs_parada = 'X'.
      ELSEIF ( lt_hrs_parada-zbloq_analisis EQ 'X' OR
               lt_hrs_parada-zbloqueado EQ 'X' ) AND
               lt_hrs_parada-timqty2 LT lv_h_paradas AND
               wa_aufnr_data-PLNBEZ IN rbg_ord_zppc0002 AND
               ( wa_aufnr_data-zzcolor NE 3 AND wa_aufnr_data-zzcolor NE 4 ).
                  MESSAGE TEXT-062 TYPE 'S' DISPLAY LIKE 'E'.
                  p_error = 'X'.
                  EXIT.
      ELSEIF ( lt_hrs_parada-zbloq_analisis EQ 'X' OR
               lt_hrs_parada-zbloqueado EQ 'X' ) AND
               lt_hrs_parada-timqty2 LT lv_h_paradas AND
               wa_aufnr_data-PLNBEZ IN rbg_ord_zppc0002 AND
               ( wa_aufnr_data-zzcolor EQ 3 OR wa_aufnr_data-zzcolor EQ 4 ).
                  "Permite notificar.
                  gv_updt_hrs_parada = 'X'.
      ELSEIF ( lt_hrs_parada-zbloq_analisis EQ 'X' OR
               lt_hrs_parada-zbloqueado EQ 'X' ) AND
               lt_hrs_parada-timqty2 LT lv_h_paradas AND
               wa_aufnr_data-PLNBEZ NOT IN rbg_ord_zppc0002.
                  "Siempre notifica.
                  gv_updt_hrs_parada = 'X'.
      ELSE.
        gv_updt_hrs_parada = 'X'.
      ENDIF.
    ENDLOOP.
endif.

  ENDIF.

  "Validación de lotes de componentes
  CHECK p_error IS INITIAL.
  IF gv_val_lote_comp EQ '1'.
    READ TABLE gt_componentes TRANSPORTING NO FIELDS WITH KEY lote = ''.
    IF sy-subrc EQ 0.
      MESSAGE TEXT-081 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.

    REFRESH: lt_mara,
             lt_componentes.

    LOOP AT gt_componentes INTO gwa_componentes.
      lt_mara-matnr = gwa_componentes-componente.
      APPEND lt_mara.
    ENDLOOP.

    LOOP AT gt_componentes INTO gwa_componentes WHERE bwart EQ '261'.
      READ TABLE lt_componentes ASSIGNING FIELD-SYMBOL(<fs_componentes>) WITH KEY componente = gwa_componentes-componente
                                                                                  almacen    = gwa_componentes-almacen
                                                                                  lote       = gwa_componentes-lote.
      IF sy-subrc EQ 0.
        ADD gwa_componentes-cantidad TO <fs_componentes>-cantidad.
      ELSE.
        APPEND gwa_componentes TO lt_componentes.
      ENDIF.
    ENDLOOP.

    SELECT mchb~matnr,
           mchb~lgort,
           mchb~charg,
           mchb~clabs,
           mara~meins
      INTO TABLE @DATA(lt_mchb)
      FROM mchb INNER JOIN mara ON mchb~matnr EQ mara~matnr
      FOR ALL ENTRIES IN @lt_mara
      WHERE mchb~matnr EQ @lt_mara-matnr
        AND mchb~werks EQ @p_werks.
    LOOP AT lt_componentes.
      READ TABLE lt_mchb INTO DATA(lwa_mchb) WITH KEY matnr = lt_componentes-componente
                                                      lgort = lt_componentes-almacen
                                                      charg = lt_componentes-lote.
      IF sy-subrc NE 0.
        MESSAGE TEXT-082 TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
        EXIT.
      ENDIF.

      CLEAR: lv_menge.

      IF lwa_mchb-meins EQ lt_componentes-um.
        lv_menge = lwa_mchb-clabs.
      ELSE.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = lwa_mchb-matnr
            i_in_me              = lwa_mchb-meins
            i_out_me             = lt_componentes-um
            i_menge              = lwa_mchb-clabs
          IMPORTING
            e_menge              = lv_menge
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
      ENDIF.

      IF lt_componentes-cantidad GT lv_menge."lwa_mchb-clabs.
        MESSAGE TEXT-082 TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Validación de notificación de reprocesos en secuencia.
  CHECK p_error IS INITIAL.
  IF gv_val_notif_sec EQ '1' AND
     gv_addreg        NE '1'.
    REFRESH: lt_ordenes.

*    lt_ORDENES[] = GT_ORDENES.
*    sort LT_ORDENES by AUFNR.
*    delete adjaceNT DUPLICATES FROM lt_ordenes COMPARING aufnr.
*
*    LOOP AT lt_ordenes.
*      CLEAR: lv_sig_pto,
*             lv_sig_clave.
*
*      CALL FUNCTION 'ZF_PP_ESTADO_ORDEN3'
*        EXPORTING
*          i_aufnr            = lt_ordenes-aufnr
*        IMPORTING
*          e_siguiente_puesto = lv_sig_pto
*          e_siguiente_clave  = lv_sig_clave.

*      IF lv_sig_pto   NE p_arbpl          OR
*         lv_sig_clave NE lt_ordenes-ktsch.


                                                            "{ +@0004
    " Validamos el número de ordenes que tienen notificación y son aprobadas
    DATA: lv_objectkey       TYPE objnum,
          lt_allocvaluesnum  TYPE tt_allocvaluesnum  WITH HEADER LINE,
          lt_allocvalueschar TYPE tt_allocvalueschar WITH HEADER LINE,
          lt_allocvaluescurr TYPE tt_allocvaluescurr WITH HEADER LINE,
          lt_return          TYPE tt_return          WITH HEADER LINE.
    CONCATENATE p_werks
                p_arbpl
                INTO lv_objectkey.
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_objectkey
        objecttable     = 'CRHD'
        classnum        = 'Z_PUESTO_TRABAJO'
        classtype       = '019'
      TABLES
        allocvaluesnum  = lt_allocvaluesnum[]
        allocvalueschar = lt_allocvalueschar[]
        allocvaluescurr = lt_allocvaluescurr[]
        return          = lt_return[].

    LOOP AT gt_ordenes INTO gwa_ordenes WHERE color = gc_amarillo.
      READ TABLE lt_allocvalueschar[] INTO lt_allocvalueschar WITH KEY charact = 'Z_TIPO_NOTIFICACION'
                                                                       value_char = gwa_ordenes-zztipo_notif.
      IF lt_allocvalueschar-value_neutral = '01' OR lt_allocvalueschar-value_neutral = '02' OR lt_allocvalueschar-value_neutral = '03'.
        APPEND gwa_ordenes-aufnr TO gt_ordenes_delete.
        gv_flag_orden_not = 'X'.
      ENDIF.
    ENDLOOP.
                                                            "} +@0004

*    READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY color = gc_amarillo.  "-@0004
*    IF sy-subrc EQ 0. "-@0004
    IF gt_ordenes_delete IS NOT INITIAL.
      lv_txt = TEXT-083.

      SELECT SINGLE crtx~ktext,
                    afvc~ltxa1
        INTO @DATA(lwa_txt)
        FROM afko INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                  INNER JOIN crhd ON afvc~arbid EQ crhd~objid
                  INNER JOIN crtx ON crhd~objty EQ crtx~objty AND
                                     crhd~objid EQ crtx~objid
        WHERE afko~aufnr EQ @gwa_ordenes-aufnr
          AND afvc~ktsch EQ @gwa_ordenes-ktsch_prox
          AND crhd~arbpl EQ @gwa_ordenes-arbpl_prox
          AND crhd~werks EQ @p_werks
          AND crtx~spras EQ @sy-langu.

      REPLACE '&1' IN lv_txt WITH lwa_txt-ktext.
      REPLACE '&2' IN lv_txt WITH lwa_txt-ltxa1.

      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.

      p_error = 'X'.
      EXIT.
    ENDIF.
*    ENDLOOP.
  ENDIF.
*{-@0012 14.07.2020 re-activación
*{+@0007
  "Notificaciones Obligatoria
  "I-24.09.2020 - EMAT
*  CHECK p_error IS INITIAL.
*  IF gv_val_notif_sec EQ '2'.
*    DATA: ls_097    TYPE string,
*          ls_098    TYPE string,
*          ls_err    TYPE string,
*          ls_msgtmp TYPE string,
*          li_nop    TYPE i,
*          ln_stzhl  TYPE afru-stzhl,
*          lc_exists TYPE c.
*    CLEAR: ls_err,li_nop,lc_exists,gv_popup,ls_msgtmp.
*    ls_097 = TEXT-097.
*    ls_098 = TEXT-098.
*
*    SELECT * INTO TABLE @DATA(lt_clv_cntr)
*    FROM ztconstantes
*    WHERE modulo      = @gc_pp
*      AND aplicacion  = @gc_notif_auto
*      AND programa    = @gc_zppp0005
*      AND campo       = @gc_clv_cntr.
*    IF sy-subrc NE 0.
*      MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
*      p_error = 'X'.
*      EXIT.
*    ELSE.
*
*      TYPES: y_range2  TYPE RANGE OF ztconstantes-valor1.
*      DATA(r_valor1) = VALUE y_range2(
*          FOR ls_cust2 IN lt_clv_cntr
*          LET s = 'I'
*              o = 'EQ'
*          IN sign = s
*           option = o
*      ( low = ls_cust2-valor1 ) ).
*      IF r_valor1[] IS INITIAL.
*        MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
*        p_error = 'X'.
*        EXIT.
*      ENDIF.
  "F-24.09.2020

*      READ TABLE lt_clv_cntr INDEX 1 INTO DATA(lwa_clv_cntr).
*      IF sy-subrc EQ 0.
*        IF lwa_clv_cntr-valor1 IS INITIAL.
*          MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
*          p_error = 'X'.
*          EXIT.
*        ENDIF.
*      ENDIF.
"I-24.09.2020 - EMAT
*    ENDIF.
*
*    SELECT signo AS sign, opcion AS option,
*      CAST( valor1 AS CHAR( 7 ) ) AS low,
*      CAST( valor2 AS CHAR( 7 ) ) AS high
*    INTO TABLE @DATA(lt_lite)
*    FROM ztconstantes
*    WHERE modulo      = @gc_pp
*      AND aplicacion  = @gc_notif_auto
*      AND programa    = @gc_zppp0005
*      AND campo       = @gc_lite.
*    IF sy-subrc NE 0.
*      MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
*      p_error = 'X'.
*      EXIT.
*    ENDIF.
*
*    DATA(lt_orders_x) = gt_ordenes[].
*    SORT lt_orders_x BY aufnr.
*    DELETE ADJACENT DUPLICATES FROM lt_orders_x COMPARING aufnr.
*    DATA(lr_orders) = VALUE range_t_aufnr( FOR lw_order IN lt_orders_x
*    ( sign = 'I' option = 'EQ' low = lw_order-aufnr ) ).
*
***ponemos en orden descendente para buscar los anteriores
*    SELECT afru~rueck,
*           MAX( afru~rmzhl ) AS rmzhl,
*           aufk~aufnr,
*           afvc~vornr,
*           crhd~arbpl,
*           afvc~steus,
*           afvc~ktsch
*      INTO TABLE @DATA(lt_historico)
*      FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
*                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
*                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
*                LEFT  JOIN afru ON aufk~aufnr EQ afru~aufnr
*                               AND afvc~vornr EQ afru~vornr
*                               AND afvc~ktsch EQ afru~zzktsch
*      WHERE aufk~aufnr IN @lr_orders
*      GROUP BY afru~rueck, afru~aufnr,aufk~aufnr, crhd~arbpl,
*               afvc~vornr, afvc~steus, afvc~ktsch
*      ORDER BY aufk~aufnr DESCENDING,
*              afvc~vornr DESCENDING,
*              crhd~arbpl DESCENDING,
*              rmzhl DESCENDING.
**obtenemos ordenes notificadas pero que estan anuladas
**significa que puedo volver a notificarlas
*    SELECT rueck, rmzhl
*    FROM afru
*    INTO TABLE @DATA(lt_anulados)
*    FOR ALL ENTRIES IN @lt_historico
*    WHERE rueck EQ @lt_historico-rueck
*      AND rmzhl EQ @lt_historico-rmzhl
*      AND stzhl NE @ln_stzhl.
*    SORT lt_anulados BY rueck.
*
**validamos la operación sea ZP05 que no estan notificadas
**tambien validamos que dentro de las operaciones a tratar
**existe alguna ZP05 para proceder al otro paso
*    LOOP AT gt_ordenes INTO gwa_ordenes.
**      READ TABLE lt_historico TRANSPORTING NO FIELDS "-LORE09092020
*      READ TABLE lt_historico WITH KEY aufnr = gwa_ordenes-aufnr
*               vornr = gwa_ordenes-vornr
*               ASSIGNING FIELD-SYMBOL(<fs_lore01>). "+LORE09092020
**               steus = lwa_clv_cntr-valor1.        "-LORE09092020
*      IF sy-subrc EQ 0.
*        "+LORE09092020
*        IF <fs_lore01>-steus IN r_valor1.
*          "+LORE09092020
*          lc_exists = 'X'.
*          gwa_ordenes-aufnr = |{ gwa_ordenes-aufnr ALPHA = OUT }|.
*          REPLACE '&' IN ls_097 WITH gwa_ordenes-aufnr.
*          gwa_ordenes-aufnr = |{ gwa_ordenes-aufnr ALPHA = IN }|.
*          LOOP AT lt_historico INTO DATA(lwa_historico)
*                      FROM sy-tabix
*                      WHERE aufnr = gwa_ordenes-aufnr
*                        AND steus IN r_valor1.             "+LORE09092020
**                        AND steus EQ lwa_clv_cntr-valor1. "-LORE09092020
*            "Validamos que no sea uno de los que se va a notificar
*            READ TABLE gt_ordenes WITH KEY aufnr = gwa_ordenes-aufnr
*                                           vornr = lwa_historico-vornr
*            TRANSPORTING NO FIELDS.
*            IF sy-subrc EQ 0.
*              CONTINUE.
*            ENDIF.
*            "validamos que sea de los que estan anulados
*            IF lwa_historico-rmzhl IS NOT INITIAL.
*              READ TABLE lt_anulados WITH KEY rueck = lwa_historico-rueck
*                                              rmzhl = lwa_historico-rmzhl
*              TRANSPORTING NO FIELDS.
*              IF sy-subrc NE 0.
*                CONTINUE.
*              ENDIF.
*            ENDIF.
*            IF li_nop EQ 0.
*              ls_err = |{ ls_097 } { lwa_historico-vornr }|.
*              ADD 1 TO li_nop.
*            ELSE.
*              ls_err = |{ ls_err }, { lwa_historico-vornr }|.
*            ENDIF.
*            p_error = 'X'.
*          ENDLOOP.
*          CLEAR li_nop.
*        ENDIF. "+LORE09092020
*      ENDIF.
*      IF p_error IS NOT INITIAL.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*    IF p_error IS NOT INITIAL.
*      MESSAGE ls_err TYPE 'W' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
**si dentro de las operaciones a tratar hubo alguna ZP05 entonces validamos
**las que no son ZP05
*    IF lc_exists IS NOT INITIAL.
*      DATA(lt_ordaux) = gt_ordenes.
*      DELETE ADJACENT DUPLICATES FROM lt_ordaux COMPARING aufnr.
*      LOOP AT lt_ordaux INTO gwa_ordenes.
*        READ TABLE lt_historico TRANSPORTING NO FIELDS
*        WITH KEY aufnr = gwa_ordenes-aufnr
*                 vornr = gwa_ordenes-vornr.
*        gwa_ordenes-aufnr = |{ gwa_ordenes-aufnr ALPHA = OUT }|.
*        REPLACE '&' IN ls_098 WITH gwa_ordenes-aufnr.
*        gwa_ordenes-aufnr = |{ gwa_ordenes-aufnr ALPHA = IN }|.
*        LOOP AT lt_historico INTO lwa_historico
*            FROM sy-tabix
*            WHERE aufnr = gwa_ordenes-aufnr
*              AND steus NOT IN r_valor1.          "+LORE09092020
**              AND steus NE lwa_clv_cntr-valor1.  "-LORE09092020
*          "Validamos que no sea uno de los que se va a notificar
*          READ TABLE gt_ordenes WITH KEY aufnr = gwa_ordenes-aufnr
*                                         vornr = lwa_historico-vornr
*          TRANSPORTING NO FIELDS.
*          IF sy-subrc EQ 0.
*            CONTINUE.
*          ENDIF.
*          "validamos que sea de los que estan anulados
*          IF lwa_historico-rmzhl IS NOT INITIAL.
*            READ TABLE lt_anulados WITH KEY rueck = lwa_historico-rueck
*                                            rmzhl = lwa_historico-rmzhl
*            TRANSPORTING NO FIELDS.
*            IF sy-subrc NE 0.
*              CONTINUE.
*            ENDIF.
*          ENDIF.
*          "validamos que sea solo alguno de las operaciones establecidas
*          "a validar en la tabla lite
*          READ TABLE lt_lite WITH KEY low = lwa_historico-ktsch
*          TRANSPORTING NO FIELDS.
*          IF sy-subrc NE 0.
*            CONTINUE.
*          ENDIF.
*          IF li_nop EQ 0.
*            ls_msgtmp = |{ ls_098 } { lwa_historico-vornr }|.
*            ADD 1 TO li_nop.
*          ELSE.
*            ls_msgtmp = |{ ls_msgtmp }, { lwa_historico-vornr }|.
*          ENDIF.
*          APPEND INITIAL LINE TO gt_sinnotif ASSIGNING FIELD-SYMBOL(<fs_sinnotif>).
*          <fs_sinnotif>-aufnr = lwa_historico-aufnr.
*          <fs_sinnotif>-arbpl = lwa_historico-arbpl.
*          <fs_sinnotif>-ktsch = lwa_historico-ktsch.
*          <fs_sinnotif>-steus = lwa_historico-steus.
*          <fs_sinnotif>-vornr = lwa_historico-vornr.
*        ENDLOOP.
*        IF gv_popup IS INITIAL.
*          gv_popup = ls_msgtmp.
*        ELSE.
*          gv_popup = |{ gv_popup }. { cl_abap_char_utilities=>cr_lf }{ ls_msgtmp }|.
*        ENDIF.
*        CLEAR li_nop.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.
**}+@0007
**}-@0012 14.07.2020 re-activación
"I-24.09.2020 - EMAT

  "Validación de notificación de reprocesos en secuencia.
  CHECK p_error IS INITIAL.
  IF gv_val_uso_herr = '1'.
    REFRESH: lt_mcha.

    LOOP AT gt_componentes INTO gwa_componentes.
      CLEAR: lt_mcha_aux.

      lt_mcha_aux-matnr = gwa_componentes-componente.
      lt_mcha_aux-charg = gwa_componentes-lote.

      APPEND lt_mcha_aux.
    ENDLOOP.

    SELECT matnr
      INTO TABLE @DATA(lt_mara_zher)
      FROM mara
      FOR ALL ENTRIES IN @lt_mcha_aux
      WHERE matnr EQ @lt_mcha_aux-matnr
        AND mtart EQ 'ZHER'.
    SORT lt_mara_zher.
    DELETE ADJACENT DUPLICATES FROM lt_mara_zher.

    SELECT equi~matnr,
           equi~charge,
           equi~equnr,
           imptt~point,
           mmpt~zykl1
      INTO TABLE @DATA(lt_equi)
      FROM equi INNER JOIN imptt ON equi~objnr  EQ imptt~mpobj
                INNER JOIN mpos  ON equi~equnr  EQ mpos~equnr
                INNER JOIN mmpt  ON mpos~warpl  EQ mmpt~warpl AND
                                    imptt~point EQ mmpt~point
      FOR ALL ENTRIES IN @lt_mcha_aux
      WHERE equi~matnr  EQ @lt_mcha_aux-matnr
        AND equi~charge EQ @lt_mcha_aux-charg
        AND equi~eqtyp  EQ 'P'
        AND imptt~inact EQ @space
        AND imptt~lvorm EQ @space.

    SORT lt_vornr BY aufnr ASCENDING
                     vornr ASCENDING.

    LOOP AT gt_componentes INTO gwa_componentes.
      READ TABLE lt_vornr INTO DATA(lwa_vornr) WITH KEY aufnr = gwa_componentes-aufnr.
      CHECK sy-subrc EQ 0.
      IF lwa_vornr-vornr NE gwa_componentes-vornr.
        CONTINUE.
      ENDIF.

      READ TABLE lt_mara_zher TRANSPORTING NO FIELDS WITH KEY matnr = gwa_componentes-componente.
      CHECK sy-subrc EQ 0.

      READ TABLE lt_equi INTO DATA(lwa_equi) WITH KEY matnr  = gwa_componentes-componente
                                                      charge = gwa_componentes-lote.
      CHECK sy-subrc EQ 0.

      CALL FUNCTION 'MEASUREM_POINT_LAST_VALUE'
        EXPORTING
          i_point           = lwa_equi-point
        IMPORTING
          e_wa_value        = lwa_imrg
        EXCEPTIONS
          pointer_not_found = 1
          OTHERS            = 2.

      lv_penult = lwa_equi-zykl1 - 1.

      IF lwa_imrg-cntrr GE lwa_equi-zykl1.
*{+@0006
        IF p_arbpl EQ '06CURVAD' AND lt_componentes-ktsch EQ 'ENTH'.
          SELECT ersda, iedz, zzlote, aufnr
          UP TO 1 ROWS
          INTO TABLE @DATA(lt_msg)
          FROM crhd
          INNER JOIN afru ON crhd~objid EQ afru~arbid
                         AND crhd~werks EQ afru~werks
          WHERE crhd~arbpl EQ @p_arbpl
            AND crhd~werks EQ @p_werks
            AND afru~zzktsch EQ @lt_componentes-ktsch
            AND afru~manur EQ '1'
          ORDER BY ersda DESCENDING, iedz DESCENDING.
          IF sy-subrc EQ 0.
            READ TABLE lt_msg INDEX 1 INTO DATA(lwa_msg).
          ELSE.
            CLEAR lwa_msg.
          ENDIF.
          DATA: ls_084 TYPE string,
                ls_093 TYPE string.
          ls_093 = TEXT-093.
          REPLACE '&1' IN ls_093 WITH |{ lwa_msg-aufnr ALPHA = OUT }|.
          REPLACE '&2' IN ls_093 WITH lwa_msg-zzlote.

          CONCATENATE TEXT-084 ls_093 INTO ls_084.
          MESSAGE ls_084 TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          MESSAGE TEXT-084 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
*}+@0006
        p_error = 'X'.
        EXIT.
      ELSEIF lwa_imrg-cntrr EQ lv_penult.
        MESSAGE TEXT-085 TYPE 'W'.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Validación de peso en balanza
  CHECK p_error IS INITIAL.
  IF gv_ctrl_peso = '1'.
    SELECT aufnr,
           matnr,
           charg
      INTO TABLE @DATA(lt_peso)
      FROM ztpesobalanza
      WHERE zbrgew GT 0.

    SORT lt_vornr BY aufnr ASCENDING
                     vornr DESCENDING.

    LOOP AT gt_ordenes INTO gwa_ordenes WHERE zztipo_notif EQ gv_aprob.
      READ TABLE gt_componentes INTO gwa_componentes WITH KEY nro   = gwa_ordenes-nro
                                                              bwart = '101'.
      CHECK sy-subrc EQ 0.
      READ TABLE lt_vornr INTO lwa_vornr WITH KEY aufnr = gwa_ordenes-aufnr.
      IF sy-subrc        EQ 0                 AND
         lwa_vornr-vornr EQ gwa_ordenes-vornr.
        READ TABLE lt_peso TRANSPORTING NO FIELDS WITH KEY aufnr = gwa_ordenes-aufnr
                                                           matnr = gwa_componentes-componente
                                                           charg = gwa_componentes-lote.
        IF sy-subrc NE 0.
          MESSAGE TEXT-089 TYPE 'S' DISPLAY LIKE 'E'.
          p_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
*-{@0012 14.07.2020 re-activación
*+{@0010
  "Validación de notificaciones aprobadas
"I-24.09.2020 - EMAT
*  CHECK p_error IS INITIAL.
*  IF gv_vent_prox_ptrab EQ '2' AND
*       gt_ordenes[]        IS NOT INITIAL.
*    IF zppe_notif_addreg-zztipo_notif EQ 'APROBADO'.        "+@0011
**    DATA: lv_tabix TYPE i,lv_ok TYPE c.
**    CLEAR:   lv_tabix,
**             lv_ok.
*      DATA: ls_100     TYPE string,                         "+@0011
*            lc_ind(01) TYPE c,                              "+@0011
*            li_tabix   TYPE i.                              "+@0011
*
*      lt_orders_x = gt_ordenes[].
*      SORT lt_orders_x BY aufnr.
*      DELETE ADJACENT DUPLICATES FROM lt_orders_x COMPARING aufnr.
*      lr_orders = VALUE range_t_aufnr( FOR lw_order IN lt_orders_x
*      ( sign = 'I' option = 'EQ' low = lw_order-aufnr ) ).
*      REFRESH:lt_orders_x.
*
**ordenamos de manera ascendente para poder validar la operacion siguiente.
**obtenemos la lista de objetos a notificar y notificados
*      SELECT afru~rueck,
*             MAX( afru~rmzhl ) AS rmzhl,
*             aufk~aufnr,
*             afvc~vornr,
*             crhd~arbpl,
*             afvc~steus,
*             afvc~ktsch
*        INTO TABLE @lt_historico
*        FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
*                  INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
*                  INNER JOIN crhd ON afvc~arbid EQ crhd~objid
*                  LEFT  JOIN afru ON aufk~aufnr EQ afru~aufnr
*                                 AND afvc~vornr EQ afru~vornr
*                                 AND afvc~ktsch EQ afru~zzktsch
*        WHERE aufk~aufnr IN @lr_orders
*        GROUP BY afru~rueck, afru~aufnr,aufk~aufnr, crhd~arbpl,
*                 afvc~vornr, afvc~steus, afvc~ktsch
*        ORDER BY aufk~aufnr ASCENDING,
*                afvc~vornr ASCENDING,
*                crhd~arbpl ASCENDING,
*                afvc~steus ASCENDING,
*                rmzhl ASCENDING.
*
*      REFRESH: lr_orders.
**obtenemos ordenes notificadas pero que estan anuladas
**significa que puedo volver a notificarlas
*      SELECT rueck, rmzhl
*      FROM afru
*      INTO TABLE @lt_anulados
*      FOR ALL ENTRIES IN @lt_historico
*      WHERE rueck EQ @lt_historico-rueck
*        AND rmzhl EQ @lt_historico-rmzhl
*        AND stzhl NE @ln_stzhl.
*      SORT lt_anulados BY rueck.
*
*      LOOP AT gt_ordenes INTO gwa_ordenes.
**      READ TABLE lt_historico WITH KEY aufnr = gwa_ordenes-aufnr
**                                             vornr = gwa_ordenes-vornr
**                                             arbpl = p_arbpl
**      TRANSPORTING NO FIELDS. "-@0011
*        READ TABLE lt_historico INTO DATA(lwa_hist) WITH KEY aufnr = gwa_ordenes-aufnr
*                                                             vornr = gwa_ordenes-vornr
*                                                             arbpl = p_arbpl. "+@0011
*        IF sy-subrc EQ 0.
*                                                            "+{@0011
*          CLEAR: li_tabix.
*          li_tabix = sy-tabix + 1.
*          IF lwa_hist-rmzhl IS NOT INITIAL.
*            lc_ind = 'X'.
*            READ TABLE lt_anulados WITH KEY rueck = lwa_hist-rueck
*                                            rmzhl = lwa_hist-rmzhl
*            TRANSPORTING NO FIELDS.
*            IF sy-subrc EQ 0.
*              CLEAR: lc_ind.
*            ENDIF.
*          ENDIF.
*                                                            "+}@0011
**        ADD 1 TO sy-tabix. "-@0011
*          LOOP AT lt_historico INTO lwa_historico
**          FROM sy-tabix WHERE aufnr = gwa_ordenes-aufnr. "-@0011
*            FROM li_tabix WHERE aufnr = gwa_ordenes-aufnr.  "+@0011
**                      AND steus EQ gwa_ordenes-.
*            "validamos que sea de los que estan anulados
*            IF lwa_historico-rmzhl IS NOT INITIAL.
*              lc_ind = 'X'.                                 "+@0011
*              READ TABLE lt_anulados WITH KEY rueck = lwa_historico-rueck
*                                              rmzhl = lwa_historico-rmzhl
*              TRANSPORTING NO FIELDS.
*                                                            "-{@0011
**            IF sy-subrc NE 0.
**              CONTINUE.
**            ENDIF.
*                                                            "-}@0011
*                                                            "+{@0011
*              IF sy-subrc EQ 0.
*                CLEAR: ls_100.
*                ls_100 = TEXT-100.
*                REPLACE '&1' IN ls_100 WITH p_arbpl.
*                REPLACE '&2' IN ls_100 WITH gwa_ordenes-vornr.
*                REPLACE '&3' IN ls_100 WITH lwa_historico-vornr.
*                REPLACE '&4' IN ls_100 WITH lwa_historico-arbpl.
*                REPLACE '&5' IN ls_100 WITH lwa_historico-ktsch.
*                MESSAGE ls_100 TYPE 'E' DISPLAY LIKE 'I'.
*                EXIT.
*              ELSE.
*                CONTINUE.
*              ENDIF.
*                                                            "+}@0011
*            ENDIF.
*                                                            "+{@0011
*            IF lwa_historico-rmzhl IS INITIAL AND lc_ind IS NOT INITIAL.
*              CLEAR: ls_100.
*              ls_100 = TEXT-100.
*              REPLACE '&1' IN ls_100 WITH p_arbpl.
*              REPLACE '&2' IN ls_100 WITH gwa_ordenes-vornr.
*              REPLACE '&3' IN ls_100 WITH lwa_historico-vornr.
*              REPLACE '&4' IN ls_100 WITH lwa_historico-arbpl.
*              REPLACE '&5' IN ls_100 WITH lwa_historico-ktsch.
*              MESSAGE ls_100 TYPE 'E' DISPLAY LIKE 'I'.
*              EXIT.
*            ENDIF.
*                                                            "+}@0011
*          ENDLOOP.
*
*
*        ENDIF.
*      ENDLOOP.
*    ENDIF.                                                  "+@0011
*  ENDIF.
"I-24.09.2020 - EMAT
*+}@0010
*-}@0012 14.07.2020 re-activación
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB02_VALIDA
*&---------------------------------------------------------------------*
FORM f_add_tab02_valida CHANGING p_error.
  DATA: lv_text     TYPE string,
        lv_material TYPE matnr18.

  CLEAR: p_error,
         lv_material.

  READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY nro = gwa_componentes-nro.
  IF sy-subrc EQ 0.
    gwa_componentes-aufnr = gwa_ordenes-aufnr.
    gwa_componentes-vornr = gwa_ordenes-vornr.
    gwa_componentes-ktsch = gwa_ordenes-ktsch.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gwa_componentes-componente
    IMPORTING
      output = lv_material.

*  IF gwa_componentes-nro IS INITIAL.
*    lv_text = TEXT-008.
*    REPLACE '&' IN lv_text WITH 'Nro'.
*    p_error = 'X'.
*    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.

*  IF gwa_componentes-aufnr IS INITIAL.
*    lv_text = TEXT-008.
*    REPLACE '&' IN lv_text WITH 'Orden'.
*    p_error = 'X'.
*    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
*  IF gwa_componentes-vornr IS INITIAL.
*    lv_text = TEXT-008.
*    REPLACE '&' IN lv_text WITH 'Operación'.
*    p_error = 'X'.
*    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.

*  IF gwa_componentes-componente IS INITIAL.
*    lv_text = TEXT-008.
*    REPLACE '&' IN lv_text WITH 'Componente'.
*    p_error = 'X'.
*    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
*  IF gwa_componentes-bwart IS INITIAL.
*    lv_text = TEXT-008.
*    REPLACE '&' IN lv_text WITH 'Clase de Movimiento'.
*    p_error = 'X'.
*    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
*  IF gwa_componentes-um_txt IS INITIAL.
*    lv_text = TEXT-008.
*    REPLACE '&' IN lv_text WITH 'UM'.
*    p_error = 'X'.
*    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ELSE.
*    SELECT SINGLE COUNT(*)
*      FROM t006a
*      WHERE spras EQ sy-langu
*        AND msehi EQ gwa_componentes-um_txt.
*    IF sy-subrc NE 0.
*      lv_text = TEXT-016.
*      REPLACE '&' IN lv_text WITH 'UM'.
*      p_error = 'X'.
*      MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*  ENDIF.

*  IF gwa_componentes-almacen IS INITIAL.
*    lv_text = TEXT-008.
*    REPLACE '&' IN lv_text WITH 'Almacén'.
*    p_error = 'X'.
*    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ELSE.
*    SELECT SINGLE COUNT(*)
*      FROM t001l
*      WHERE werks EQ p_werks
*        AND lgort EQ gwa_componentes-almacen.
*    IF sy-subrc NE 0.
*      lv_text = TEXT-016.
*      REPLACE '&' IN lv_text WITH 'Almacén'.
*      p_error = 'X'.
*      MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*  ENDIF.

  SELECT SINGLE COUNT(*)
    FROM mara
    WHERE matnr EQ gwa_componentes-componente
      AND xchpf EQ 'X'.
  IF sy-subrc EQ 0.
    IF gwa_componentes-licha IS NOT INITIAL.
      CLEAR: gwa_componentes-lote.

      SELECT SINGLE charg
        INTO gwa_componentes-lote
        FROM mch1
        WHERE matnr EQ lv_material
          AND licha EQ gwa_componentes-licha.
    ELSE.
      IF gwa_componentes-lote IS INITIAL.
        lv_text = TEXT-008.
        REPLACE '&' IN lv_text WITH 'Lote'.
        p_error = 'X'.
        MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ELSE.
        SELECT SINGLE COUNT(*)
          FROM mcha
          WHERE matnr EQ lv_material
            AND werks EQ p_werks
            AND charg EQ gwa_componentes-lote.
        IF sy-subrc NE 0.
          lv_text = TEXT-016.
          REPLACE '&' IN lv_text WITH TEXT-x10. "'Lote'.
          p_error = 'X'.
          MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ELSE.
          SELECT SINGLE licha
            INTO gwa_componentes-licha
            FROM mch1
            WHERE matnr EQ lv_material
              AND charg EQ gwa_componentes-lote.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB01_ID
*&---------------------------------------------------------------------*
FORM f_add_tab01_id CHANGING p_error.
  DATA: lv_ok    TYPE c,
        lv_aufnr TYPE aufnr,
        lv_txt   TYPE string.

  IF gv_codbar_iox EQ '1'.
    PERFORM f_add_tab01_id_iox CHANGING lv_ok
                                        p_error.
    CHECK lv_ok IS INITIAL.
  ENDIF.

  IF gv_codbar_avo EQ '1'.
    PERFORM f_add_tab01_id_avo CHANGING lv_ok
                                        p_error.
    CHECK p_error IS INITIAL AND
          lv_ok   IS INITIAL.
  ENDIF.

  CASE gv_cod_barra.
    WHEN '01'.
      CLEAR: zppe_notif_addreg-aufnr,
             zppe_notif_addreg-charg.

      SELECT SINGLE COUNT(*)
        FROM aufk
        WHERE aufnr EQ zppe_notif_addreg-id+0(12).
      IF sy-subrc EQ 0.
        zppe_notif_addreg-aufnr = zppe_notif_addreg-id+0(12).
        zppe_notif_addreg-charg = zppe_notif_addreg-id+12(10).
      ELSE.
        CALL FUNCTION 'ZF_BUSCA_ZTB_ETIQUETAS'
          EXPORTING
            i_cod_barra = zppe_notif_addreg-id
          IMPORTING
            e_lote_sap  = zppe_notif_addreg-charg
            e_aufnr     = zppe_notif_addreg-aufnr.

        SELECT SINGLE COUNT(*)
          FROM aufk
          WHERE aufnr EQ zppe_notif_addreg-aufnr.
        IF sy-subrc NE 0.
          MESSAGE TEXT-087 TYPE 'S' DISPLAY LIKE 'E'.
          p_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.
    WHEN '02'.
      CLEAR: zppe_notif_addreg-aufnr,
             zppe_notif_addreg-lote_carga,
             zppe_notif_addreg-lmnga.

      zppe_notif_addreg-aufnr      = zppe_notif_addreg-id+0(12).
      zppe_notif_addreg-lote_carga = zppe_notif_addreg-id+12(10).
      zppe_notif_addreg-lmnga      = zppe_notif_addreg-id+22.

      gv_borrar_cant = 'X'.
    WHEN '03'.
      CLEAR: zppe_notif_addreg-aufnr,
             lv_aufnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = zppe_notif_addreg-id+0(12)
        IMPORTING
          output = lv_aufnr.

      SELECT SINGLE COUNT(*)
        FROM aufk
        WHERE aufnr EQ lv_aufnr."zppe_notif_addreg-id+0(12).
      IF sy-subrc EQ 0.
        zppe_notif_addreg-aufnr = lv_aufnr."zppe_notif_addreg-id+0(12).
      ELSE.
        CALL FUNCTION 'ZF_BUSCA_ZTB_ETIQUETAS'
          EXPORTING
            i_cod_barra = zppe_notif_addreg-id
          IMPORTING
            e_aufnr     = zppe_notif_addreg-aufnr.
      ENDIF.
    WHEN OTHERS.
      MESSAGE TEXT-086 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
  ENDCASE.
  CLEAR: zppe_notif_addreg-id.

  PERFORM f_add_tab01_aufnr CHANGING p_error.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ACTUALIZA_LCURVADO
*&---------------------------------------------------------------------*
FORM f_actualiza_lcurvado .
  DATA: lt_afvc   TYPE tt_data_lcorte WITH HEADER LINE,
        lt_diimpt TYPE tt_diimpt      WITH HEADER LINE,
        lwa_imrg  TYPE imrg,
        lv_error  TYPE c,
        lv_fltp   TYPE cha_class_data-sollwert,
        lv_char   TYPE cha_class_view-sollwert.

  CLEAR:   lv_error,
           gv_lote_curvado.
  REFRESH: lt_afvc,
           lt_diimpt.

  READ TABLE gt_lote_auto INTO gwa_lote_auto WITH KEY value = '01'.
  CHECK sy-subrc  EQ 0   AND
        gv_addreg NE '3'.

  READ TABLE gt_ordenes TRANSPORTING NO FIELDS WITH KEY zztipo_notif = gv_aprob.
  CHECK sy-subrc EQ 0.

  SELECT aufk~aufnr
         afvc~vornr
         afvc~ktsch
    INTO TABLE lt_afvc
    FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN crhd ON afvc~arbid EQ crhd~objid
    FOR ALL ENTRIES IN gt_ordenes
    WHERE aufk~aufnr EQ gt_ordenes-aufnr
      AND crhd~arbpl EQ p_arbpl
      AND crhd~werks EQ p_werks.
  SORT lt_afvc BY aufnr ASCENDING
                  vornr ASCENDING.

  LOOP AT gt_ordenes INTO gwa_ordenes.
    CLEAR: lt_afvc.

    READ TABLE lt_afvc WITH KEY aufnr = gwa_ordenes-aufnr.

    IF gwa_ordenes-ktsch NE lt_afvc-ktsch.
      lv_error = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  CHECK lv_error IS INITIAL.

  CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
    EXPORTING
      i_equnr   = gv_equipo
    TABLES
      et_diimpt = lt_diimpt.

  READ TABLE lt_diimpt WITH KEY psort = 'ZLOTE_CURVADO'.
  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'MEASUREM_DOCUM_READ_LAST'
    EXPORTING
      virtual_reading_ok = 'X'
      point              = lt_diimpt-point
    IMPORTING
      imrg_wa            = lwa_imrg.
  CHECK sy-subrc EQ 0.
  lv_fltp = lwa_imrg-cntrr .

  CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
    EXPORTING
      measurement_point    = lt_diimpt-point
      recorded_value       = '1'
      difference_reading   = 'X'
    EXCEPTIONS
      no_authority         = 1
      point_not_found      = 2
      index_not_unique     = 3
      type_not_found       = 4
      point_locked         = 5
      point_inactive       = 6
      timestamp_in_future  = 7
      timestamp_duprec     = 8
      unit_unfit           = 9
      value_not_fltp       = 10
      value_overflow       = 11
      value_unfit          = 12
      value_missing        = 13
      code_not_found       = 14
      notif_type_not_found = 15
      notif_prio_not_found = 16
      notif_gener_problem  = 17
      update_failed        = 18
      invalid_time         = 19
      invalid_date         = 20
      OTHERS               = 21.
  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
    EXPORTING
      i_number_of_digits       = '0'
      i_fltp_value             = lv_fltp
      i_value_not_initial_flag = 'X'
      i_screen_fieldlength     = 16
    IMPORTING
      e_char_field             = lv_char.

  CONDENSE lv_char NO-GAPS.
  gv_lote_curvado = lv_char .
  ADD 1 TO gv_lote_curvado.
  CONDENSE gv_lote_curvado NO-GAPS.

  LOOP AT gt_ordenes ASSIGNING <fs_ordenes> WHERE zztipo_notif EQ gv_aprob.
    <fs_ordenes>-zzlote_curvado = gv_lote_curvado.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_COMPLETAR_CONSTANTES
*&---------------------------------------------------------------------*
FORM f_completar_constantes .
  DATA: lt_constantes TYPE tt_constantes WITH HEADER LINE.

  CLEAR:   gv_equipo.
  REFRESH: lt_constantes.

  SELECT *
    INTO TABLE lt_constantes
    FROM ztconstantes
    WHERE modulo EQ 'PP'
      AND ( aplicacion EQ 'LOTE_CORTE' OR
            aplicacion EQ 'NOTIF_AUTO' )
      AND ( programa EQ 'ZPPP0004' OR
            programa EQ 'ZPPP0005' ).

  LOOP AT lt_constantes.
    CASE lt_constantes-campo.
      WHEN 'EQUIPO'.
        IF lt_constantes-valor1 EQ p_werks.
          gv_equipo = lt_constantes-valor2.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  IF gv_lote_prov EQ '0'.
    DELETE gt_celltab_xchpf WHERE fieldname EQ 'LICHA'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB01_ZZLOTE_CURVADO
*&---------------------------------------------------------------------*
FORM f_add_tab01_zzlote_curvado CHANGING p_error.
  DATA: lt_afru      TYPE tt_afru        WITH HEADER LINE,
        lt_data      TYPE tt_data_masivo WITH HEADER LINE,
        lt_makt      TYPE tt_makt        WITH HEADER LINE,
        lt_t006a     TYPE tt_t006a       WITH HEADER LINE,
        lv_atinn     TYPE atinn,
        lv_text      TYPE string,
        lv_nro       TYPE i,
        lv_sig_pto   TYPE ze_destino,
        lv_sig_clave TYPE ktsch.

  REFRESH: lt_data,
           lt_afru,
           lt_makt,
           lt_t006a.

  CLEAR: lv_atinn.

  IF zppe_notif_addreg-budat IS INITIAL.
    MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  IF zppe_notif_addreg-ktsch IS INITIAL.
    lv_text = TEXT-008.
    REPLACE '&' WITH TEXT-x38 INTO lv_text. "'Clave Modelo' INTO lv_text.
    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  SELECT rueck
         rmzhl
         vornr
    INTO TABLE lt_afru
    FROM afru
    WHERE werks          EQ p_werks
      AND zzlote_curvado EQ zppe_notif_addreg-zzlote_curvado
      AND stokz          EQ space
      AND stzhl          EQ space
      AND lmnga          NE 0.

  SORT lt_afru BY vornr DESCENDING.
  READ TABLE lt_afru INDEX 1.
  DELETE lt_afru WHERE vornr NE lt_afru-vornr.

  IF lt_afru[] IS NOT INITIAL.
    SELECT  afru~aufnr
            afvc~vornr
            afvc~ktsch
            afvc~plnfl
            afko~plnbez
            afko~gmein
            afru~zzlote
            afvc~steus
            afru~zzequipo
            afru~zzcod_lanza
            afru~rueck
            afru~rmzhl
      INTO TABLE lt_data
      FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
      FOR ALL ENTRIES IN lt_afru
      WHERE afru~rueck EQ lt_afru-rueck
        AND afru~rmzhl EQ lt_afru-rmzhl
        AND crhd~arbpl EQ p_arbpl
        AND crhd~werks EQ p_werks
        AND afvc~ktsch EQ zppe_notif_addreg-ktsch.
  ENDIF.

  IF lt_data[] IS NOT INITIAL.
    SORT lt_data BY aufnr ASCENDING
                    vornr ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM lt_data COMPARING aufnr
*                                                      vornr.

    SELECT matnr
           maktx
      INTO TABLE lt_makt
      FROM makt
      FOR ALL ENTRIES IN lt_data
      WHERE matnr EQ lt_data-plnbez
        AND spras EQ sy-langu.

    READ TABLE gt_tipnot INTO gwa_tipnot INDEX 1.

    SELECT msehi
           mseh6
      INTO TABLE lt_t006a
      FROM t006a
      WHERE spras EQ sy-langu.
  ENDIF.
*  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
*    EXPORTING
*      input  = 'ZLOTE_CURVADO'
*    IMPORTING
*      output = lv_atinn.
*
*  SELECT  afko~aufnr
*          afvc~vornr
*          afvc~ktsch
*          afvc~plnfl
*          afko~plnbez
*          afko~gmein
*          afpo~charg
*          afvc~steus
*    INTO TABLE lt_data
*  FROM ausp INNER JOIN mcha ON ausp~objek EQ mcha~cuobj_bm
*            INNER JOIN afpo ON mcha~matnr EQ afpo~matnr AND
*                               mcha~charg EQ afpo~charg
*            INNER JOIN aufk ON afpo~aufnr EQ aufk~aufnr
*            INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
*            INNER JOIN crhd ON aufk~werks EQ crhd~werks
*            INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl AND
*                               crhd~objid EQ afvc~arbid
*  WHERE ausp~atinn EQ lv_atinn
*    AND ausp~atwrt EQ zppe_notif_addreg-zzlote_curvado
*    AND mcha~werks EQ p_werks
*    AND crhd~arbpl EQ p_arbpl.
*    IF sy-subrc EQ 0.
*      SORT lt_data BY aufnr ASCENDING
*                      vornr ASCENDING.
*      LOOP AT lt_data.
*        AT NEW aufnr.
*          DELETE lt_data INDEX sy-tabix.
*        ENDAT.
*      ENDLOOP.
*
*      SELECT aufnr
*             vornr
*        INTO TABLE lt_afru
*        FROM afru
*        FOR ALL ENTRIES IN lt_data
*        WHERE aufnr EQ lt_data-aufnr
*          AND vornr EQ lt_data-vornr
*          AND stokz EQ space
*          AND stzhl EQ space
*          AND lmnga EQ 1.
*
*      LOOP AT lt_afru.
*        DELETE lt_data WHERE aufnr EQ lt_afru-aufnr
*                         AND vornr EQ lt_afru-vornr.
*      ENDLOOP.
*endif.
  LOOP AT lt_data.
    CLEAR: gwa_ordenes,
           lt_makt,
           lt_t006a.

    lv_nro = gv_nro.
    ADD 1 TO gv_nro.
    READ TABLE lt_makt  WITH KEY matnr = lt_data-plnbez.
    READ TABLE lt_t006a WITH KEY msehi = lt_data-gmein.

    "Nro
    gwa_ordenes-nro = gv_nro.

    "Orden
    gwa_ordenes-aufnr = lt_data-aufnr.

    "Operación
    gwa_ordenes-vornr = lt_data-vornr.

    "Clave Modelo
    gwa_ordenes-ktsch = lt_data-ktsch.

    IF gv_val_notif_sec = '1'.
      CLEAR: lv_sig_pto,
             lv_sig_clave.

      CALL FUNCTION 'ZF_PP_ESTADO_ORDEN3'
        EXPORTING
          i_aufnr            = lt_data-aufnr
        IMPORTING
          e_siguiente_puesto = lv_sig_pto
          e_siguiente_clave  = lv_sig_clave.

      "Próximo puesto de trabajo
      gwa_ordenes-arbpl_prox = lv_sig_pto.

      "Próxima clave modelo
      gwa_ordenes-ktsch_prox = lv_sig_clave.

      IF gwa_ordenes-arbpl_prox NE p_arbpl           OR
         gwa_ordenes-ktsch_prox NE gwa_ordenes-ktsch.
        gwa_ordenes-color = gc_amarillo.
      ENDIF.
    ENDIF.

    "Fecha contabilización
    gwa_ordenes-budat = zppe_notif_addreg-budat.

    "Secuencia
    gwa_ordenes-plnfl = lt_data-plnfl.

    "Equipo
    gwa_ordenes-zzequipo = lt_data-zzequipo.

    "Lote
    gwa_ordenes-zzlote = lt_data-charg.

    "Material
    gwa_ordenes-plnbez = lt_data-plnbez.

    "Descripción
    gwa_ordenes-maktx = lt_makt-maktx.

    "UM
    gwa_ordenes-gmein     = lt_data-gmein.
    gwa_ordenes-gmein_txt = lt_t006a-mseh6.

    "Cantidad
    gwa_ordenes-lmnga = 1.

    "Tipo de Notificación
    gwa_ordenes-zztipo_notif = gwa_tipnot-zztipo_notif.

    "Lote de Corte
*    gwa_ordenes-zzlote_corte =

    "Lote de Curvado
    gwa_ordenes-zzlote_curvado = zppe_notif_addreg-zzlote_curvado.

    "Ciclo de TQ
*    gwa_ordenes-ZZCICLO_TQ.

    "Ciclo de Autoclave
*    gwa_ordenes-ZZCICLO_AUTO.

    "Código de Lanzada
    gwa_ordenes-zzcod_lanza = lt_data-zzcod_lanza.

    "Lote de Carga
*    gwa_ordenes-ZZLOTE_CARGA.

    "Defecto
*    gwa_ordenes-DESC_DEFCOD.

    "Código Defecto
*    gwa_ordenes-ZZDEFCOD.

    "Origen
*    gwa_ordenes-DESC_ORIGEN.

    "Código Origen
*    gwa_ordenes-ZZORIGEN.

    "Causa
*    gwa_ordenes-DESC_AUSA.

    "Código causa
*    gwa_ordenes-ZZCAUSA.

    "Destino
*    gwa_ordenes-ZZDESTINO.

    "Causa de Desviación
*    gwa_ordenes-GRUND.
    gwa_ordenes-zzdesperop   = zppe_notif_addreg-zzdesperop. "Insert @0002
    APPEND gwa_ordenes TO gt_ordenes.

    PERFORM f_completar_componente USING lv_nro
                                         lt_data-aufnr
                                         lt_data-vornr
                                         lt_data-plnfl
                                         lt_data-ktsch
                                         gwa_ordenes-zztipo_notif
                                         1"gwa_ordenes-lmnga
                                         space.
  ENDLOOP.

  IF sy-subrc EQ 0.
    SORT gt_ordenes BY nro   ASCENDING.
*                       aufnr ASCENDING
*                       vornr ASCENDING.

    CALL METHOD gr_grid01->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
    gv_addreg = '3'.
  ELSE.
    p_error = 'X'.

    MESSAGE TEXT-013 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ACTUALIZA_CICLO_TQ
*&---------------------------------------------------------------------*
FORM f_actualiza_ciclo_tq .
  DATA: lt_diimpt   TYPE tt_diimpt      WITH HEADER LINE,
        lt_afvu     TYPE tt_afvu        WITH HEADER LINE,
        lt_afvc     TYPE tt_data_lcorte WITH HEADER LINE,
        lwa_imrg    TYPE imrg,
        lv_error    TYPE c,
        lv_fltp     TYPE cha_class_data-sollwert,
        lv_char     TYPE cha_class_view-sollwert,
        lv_ciclo_tq TYPE afru-zzciclo_tq.

  CLEAR:   gv_ciclo_tq,
           lv_error.
  REFRESH: lt_diimpt,
           lt_afvu,
           lt_afvc.

  READ TABLE gt_lote_auto INTO gwa_lote_auto WITH KEY value = '02'.
  CHECK sy-subrc EQ 0.

  SELECT aufk~aufnr
         afvc~vornr
         afvc~ktsch
    INTO TABLE lt_afvc
    FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN crhd ON afvc~arbid EQ crhd~objid
    FOR ALL ENTRIES IN gt_ordenes
    WHERE aufk~aufnr EQ gt_ordenes-aufnr
      AND crhd~arbpl EQ p_arbpl
      AND crhd~werks EQ p_werks.
  SORT lt_afvc BY aufnr ASCENDING
                  vornr ASCENDING.

  LOOP AT gt_ordenes INTO gwa_ordenes.
    CLEAR: lt_afvc.

    READ TABLE lt_afvc WITH KEY aufnr = gwa_ordenes-aufnr.

    IF gwa_ordenes-ktsch NE lt_afvc-ktsch.
      lv_error = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  CHECK lv_error IS INITIAL.

  IF gv_addreg EQ '4'.
    lv_ciclo_tq = zppe_notif_addreg-zzciclo_tq.
  ELSE.
    CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
      EXPORTING
        i_equnr   = gv_equipo
      TABLES
        et_diimpt = lt_diimpt.

    READ TABLE lt_diimpt WITH KEY psort = 'ZLOTE_CICLO_TQ'.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'MEASUREM_DOCUM_READ_LAST'
      EXPORTING
        virtual_reading_ok = 'X'
        point              = lt_diimpt-point
      IMPORTING
        imrg_wa            = lwa_imrg.
    CHECK sy-subrc EQ 0.
    lv_fltp = lwa_imrg-cntrr .

    CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
      EXPORTING
        measurement_point    = lt_diimpt-point
        recorded_value       = '1'
        difference_reading   = 'X'
      EXCEPTIONS
        no_authority         = 1
        point_not_found      = 2
        index_not_unique     = 3
        type_not_found       = 4
        point_locked         = 5
        point_inactive       = 6
        timestamp_in_future  = 7
        timestamp_duprec     = 8
        unit_unfit           = 9
        value_not_fltp       = 10
        value_overflow       = 11
        value_unfit          = 12
        value_missing        = 13
        code_not_found       = 14
        notif_type_not_found = 15
        notif_prio_not_found = 16
        notif_gener_problem  = 17
        update_failed        = 18
        invalid_time         = 19
        invalid_date         = 20
        OTHERS               = 21.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
      EXPORTING
        i_number_of_digits       = '0'
        i_fltp_value             = lv_fltp
        i_value_not_initial_flag = 'X'
        i_screen_fieldlength     = 16
      IMPORTING
        e_char_field             = lv_char.

    CONDENSE lv_char NO-GAPS.
    gv_ciclo_tq = lv_char .
    ADD 1 TO gv_ciclo_tq.
    CONDENSE gv_ciclo_tq NO-GAPS.

    LOOP AT gt_ordenes ASSIGNING <fs_ordenes>.
      <fs_ordenes>-zzciclo_tq = gv_ciclo_tq.
    ENDLOOP.
    lv_ciclo_tq = gv_ciclo_tq.
  ENDIF.

  SELECT afvu~aufpl
         afvu~aplzl
    INTO TABLE lt_afvu
    FROM afko INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN afvu ON afvc~aufpl EQ afvu~aufpl AND
                                 afvc~aplzl EQ afvu~aplzl
    FOR ALL ENTRIES IN gt_ordenes
    WHERE afko~aufnr EQ gt_ordenes-aufnr
      AND afvc~vornr EQ gt_ordenes-vornr.

  LOOP AT lt_afvu.
    UPDATE afvu SET usr02 = lv_ciclo_tq
                    slwid = '0000001'
                WHERE aufpl EQ lt_afvu-aufpl
                  AND aplzl EQ lt_afvu-aplzl.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ACTUALIZA_CICLO_TQ_CO
*&---------------------------------------------------------------------*
FORM f_actualiza_ciclo_tq_co .
  DATA: lt_diimpt   TYPE tt_diimpt      WITH HEADER LINE,
        lt_afvu     TYPE tt_afvu        WITH HEADER LINE,
        lt_afvc     TYPE tt_data_lcorte WITH HEADER LINE,
        lwa_imrg    TYPE imrg,
        lv_error    TYPE c,
        lv_fltp     TYPE cha_class_data-sollwert,
        lv_char     TYPE cha_class_view-sollwert,
        lv_ciclo_tq TYPE afru-zzciclo_tq.

  READ TABLE gt_lote_auto INTO gwa_lote_auto WITH KEY value = '11'.
  CHECK sy-subrc EQ 0.

  CLEAR:   gv_ciclo_tq,
           lv_error.
  REFRESH: lt_diimpt,
           lt_afvu,
           lt_afvc.

  IF gv_addreg EQ '4'.
    lv_ciclo_tq = zppe_notif_addreg-zzciclo_tq.
  ELSE.
    CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
      EXPORTING
        i_equnr   = gv_equipo
      TABLES
        et_diimpt = lt_diimpt.

    READ TABLE lt_diimpt WITH KEY psort = 'ZLOTE_CICLO_TQ'.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'MEASUREM_DOCUM_READ_LAST'
      EXPORTING
        virtual_reading_ok = 'X'
        point              = lt_diimpt-point
      IMPORTING
        imrg_wa            = lwa_imrg.
    CHECK sy-subrc EQ 0.
    lv_fltp = lwa_imrg-cntrr .

    CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
      EXPORTING
        measurement_point    = lt_diimpt-point
        recorded_value       = '1'
        difference_reading   = 'X'
      EXCEPTIONS
        no_authority         = 1
        point_not_found      = 2
        index_not_unique     = 3
        type_not_found       = 4
        point_locked         = 5
        point_inactive       = 6
        timestamp_in_future  = 7
        timestamp_duprec     = 8
        unit_unfit           = 9
        value_not_fltp       = 10
        value_overflow       = 11
        value_unfit          = 12
        value_missing        = 13
        code_not_found       = 14
        notif_type_not_found = 15
        notif_prio_not_found = 16
        notif_gener_problem  = 17
        update_failed        = 18
        invalid_time         = 19
        invalid_date         = 20
        OTHERS               = 21.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
      EXPORTING
        i_number_of_digits       = '0'
        i_fltp_value             = lv_fltp
        i_value_not_initial_flag = 'X'
        i_screen_fieldlength     = 16
      IMPORTING
        e_char_field             = lv_char.

    CONDENSE lv_char NO-GAPS.
    gv_ciclo_tq = lv_char .
    ADD 1 TO gv_ciclo_tq.
    CONDENSE gv_ciclo_tq NO-GAPS.

    LOOP AT gt_ordenes ASSIGNING <fs_ordenes>.
      <fs_ordenes>-zzciclo_tq = gv_ciclo_tq.
    ENDLOOP.
    lv_ciclo_tq = gv_ciclo_tq.
  ENDIF.

  SELECT afvu~aufpl
         afvu~aplzl
    INTO TABLE lt_afvu
    FROM afko INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN afvu ON afvc~aufpl EQ afvu~aufpl AND
                                 afvc~aplzl EQ afvu~aplzl
    FOR ALL ENTRIES IN gt_ordenes
    WHERE afko~aufnr EQ gt_ordenes-aufnr
      AND afvc~vornr EQ gt_ordenes-vornr.

  LOOP AT lt_afvu.
    UPDATE afvu SET usr02 = lv_ciclo_tq
                    slwid = '0000001'
                WHERE aufpl EQ lt_afvu-aufpl
                  AND aplzl EQ lt_afvu-aplzl.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB01_ZZCICLO_TQ
*&---------------------------------------------------------------------*
FORM f_add_tab01_zzciclo_tq CHANGING p_error.
  DATA: lt_afru      TYPE tt_afru        WITH HEADER LINE,
        lt_data      TYPE tt_data_masivo WITH HEADER LINE,
        lt_makt      TYPE tt_makt        WITH HEADER LINE,
        lt_t006a     TYPE tt_t006a       WITH HEADER LINE,
        lv_atinn     TYPE atinn,
        lv_text      TYPE string,
        lv_nro       TYPE i,
        lv_sig_pto   TYPE ze_destino,
        lv_sig_clave TYPE ktsch.

  REFRESH: lt_data,
           lt_afru,
           lt_makt,
           lt_t006a.

  CLEAR: lv_atinn.

  IF zppe_notif_addreg-budat IS INITIAL.
    MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  IF zppe_notif_addreg-ktsch IS INITIAL.
    lv_text = TEXT-008.
    REPLACE '&' WITH TEXT-079 INTO lv_text. "'Clave Modelo' INTO lv_text.
    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  SELECT rueck
         rmzhl
         vornr
    INTO TABLE lt_afru
    FROM afru
    WHERE werks          EQ p_werks
      AND zzciclo_tq EQ zppe_notif_addreg-zzciclo_tq
      AND stokz          EQ space
      AND stzhl          EQ space
      AND lmnga          NE 0.

  SORT lt_afru BY vornr DESCENDING.
  READ TABLE lt_afru INDEX 1.
  DELETE lt_afru WHERE vornr NE lt_afru-vornr.

  IF lt_afru[] IS NOT INITIAL.
    SELECT  afru~aufnr
            afvc~vornr
            afvc~ktsch
            afvc~plnfl
            afko~plnbez
            afko~gmein
            afru~zzlote
            afvc~steus
            afru~zzequipo
            afru~zzcod_lanza
            afru~rueck
            afru~rmzhl
      INTO TABLE lt_data
      FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
      FOR ALL ENTRIES IN lt_afru
      WHERE afru~rueck EQ lt_afru-rueck
        AND afru~rmzhl EQ lt_afru-rmzhl
        AND crhd~arbpl EQ p_arbpl
        AND crhd~werks EQ p_werks
        AND afvc~ktsch EQ zppe_notif_addreg-ktsch.
  ENDIF.

  IF lt_data[] IS NOT INITIAL.
    SORT lt_data BY aufnr ASCENDING
                    vornr ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM lt_data COMPARING aufnr
*                                                      vornr.

    SELECT matnr
           maktx
      INTO TABLE lt_makt
      FROM makt
      FOR ALL ENTRIES IN lt_data
      WHERE matnr EQ lt_data-plnbez
        AND spras EQ sy-langu.

    READ TABLE gt_tipnot INTO gwa_tipnot INDEX 1.

    SELECT msehi
           mseh6
      INTO TABLE lt_t006a
      FROM t006a
      WHERE spras EQ sy-langu.
  ENDIF.

*  "TQ EGLASS
*  IF gv_lote_auto EQ '02' AND
*     p_werks      EQ 'PE02' .
*
*    IF zppe_notif_addreg-ktsch IS INITIAL.
*      lv_text = TEXT-008.
*      REPLACE '&' WITH 'Clave Modelo' INTO lv_text.
*      MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*      p_error = 'X'.
*      EXIT.
*    ELSEIF zppe_notif_addreg-ktsch NE 'IOXSAL'.
*      lv_text = TEXT-016.
*      REPLACE '&' WITH 'Clave Modelo' INTO lv_text.
*      MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*      p_error = 'X'.
*      EXIT.
*    ENDIF.
*
*    SELECT  afko~aufnr
*            afvc~vornr
*            afvc~ktsch
*            afvc~plnfl
*            afko~plnbez
*            afko~gmein
*            afru~zzlote
*            afvc~steus
*      INTO TABLE lt_data
*      FROM afko INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
*                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
*                INNER JOIN afru ON afko~aufnr EQ afru~aufnr AND
*                                   afvc~vornr EQ afru~vornr
*      WHERE afko~aufnr      EQ zppe_notif_addreg-aufnr
*        AND crhd~arbpl      EQ p_arbpl
*        AND crhd~werks      EQ p_werks
*        AND afvc~ktsch      EQ zppe_notif_addreg-ktsch
*        AND afru~stokz      EQ space
*        AND afru~stzhl      EQ space
*        AND afru~lmnga      GT 0
*        AND afru~zzciclo_tq EQ zppe_notif_addreg-zzciclo_tq.
*  ENDIF.
*
*  SELECT  afko~aufnr
*          afvc~vornr
*          afvc~ktsch
*          afvc~plnfl
*          afko~plnbez
*          afko~gmein
*          afpo~charg
*          afvc~steus
*    INTO TABLE lt_data
*    FROM afvu INNER JOIN afvc ON afvu~aufpl EQ afvc~aufpl AND
*                                 afvu~aplzl EQ afvc~aplzl
*              INNER JOIN crhd ON afvc~werks EQ crhd~werks AND
*                                 afvc~arbid EQ crhd~objid
*              INNER JOIN afko ON afvc~aufpl EQ afko~aufpl
*              INNER JOIN afpo ON afko~aufnr EQ afpo~aufnr
*    WHERE afvu~usr02 EQ zppe_notif_addreg-zzciclo_tq
*      AND crhd~arbpl EQ p_arbpl
*      AND crhd~werks EQ p_werks.
*
*  SELECT aufnr
*         vornr
*    INTO TABLE lt_afru
*    FROM afru
*    FOR ALL ENTRIES IN lt_data
*    WHERE aufnr EQ lt_data-aufnr
*      AND vornr EQ lt_data-vornr
*      AND stokz EQ space
*      AND stzhl EQ space
*      AND lmnga EQ 1.
*
*  LOOP AT lt_afru.
*    DELETE lt_data WHERE aufnr EQ lt_afru-aufnr
*                     AND vornr EQ lt_afru-vornr.
*  ENDLOOP.

  LOOP AT lt_data.
    CLEAR: gwa_ordenes,
           lt_makt,
           lt_t006a.

    lv_nro = gv_nro.
    ADD 1 TO gv_nro.
    READ TABLE lt_makt  WITH KEY matnr = lt_data-plnbez.
    READ TABLE lt_t006a WITH KEY msehi = lt_data-gmein.

    "Nro.
    gwa_ordenes-nro = gv_nro.

    "Orden
    gwa_ordenes-aufnr = lt_data-aufnr.

    "Operación
    gwa_ordenes-vornr = lt_data-vornr.

    "Clave Modelo
    gwa_ordenes-ktsch = lt_data-ktsch.

    IF gv_val_notif_sec = '1'.
      CLEAR: lv_sig_pto,
             lv_sig_clave.

      CALL FUNCTION 'ZF_PP_ESTADO_ORDEN3'
        EXPORTING
          i_aufnr            = lt_data-aufnr
        IMPORTING
          e_siguiente_puesto = lv_sig_pto
          e_siguiente_clave  = lv_sig_clave.

      "Próximo puesto de trabajo
      gwa_ordenes-arbpl_prox = lv_sig_pto.

      "Próxima clave modelo
      gwa_ordenes-ktsch_prox = lv_sig_clave.

      IF gwa_ordenes-arbpl_prox NE p_arbpl           OR
         gwa_ordenes-ktsch_prox NE gwa_ordenes-ktsch.
        gwa_ordenes-color = gc_amarillo.
      ENDIF.
    ENDIF.

    "Fecha contabilización
    gwa_ordenes-budat = zppe_notif_addreg-budat.

    "Secuencia
    gwa_ordenes-plnfl = lt_data-plnfl.

    "Equipo
    gwa_ordenes-zzequipo = lt_data-zzequipo.

    "Lote
    gwa_ordenes-zzlote = lt_data-charg.

    "Material
    gwa_ordenes-plnbez = lt_data-plnbez.

    "Descripción
    gwa_ordenes-maktx = lt_makt-maktx.

    "UM
    gwa_ordenes-gmein     = lt_data-gmein.
    gwa_ordenes-gmein_txt = lt_t006a-mseh6.

    "Cantidad
    gwa_ordenes-lmnga = 1.

    "Tipo de Notificación
    gwa_ordenes-zztipo_notif = gwa_tipnot-zztipo_notif.

    "Lote de Corte
*    gwa_ordenes-zzlote_corte =

    "Lote de Curvado
*    gwa_ordenes-zzlote_curvado =

    "Ciclo de TQ
    gwa_ordenes-zzciclo_tq = zppe_notif_addreg-zzciclo_tq.

    "Ciclo de Autoclave
*    gwa_ordenes-ZZCICLO_AUTO.

    "Código de Lanzada
    gwa_ordenes-zzcod_lanza = lt_data-zzcod_lanza.

    "Lote de Carga
*    gwa_ordenes-ZZLOTE_CARGA.

    "Defecto
*    gwa_ordenes-DESC_DEFCOD.

    "Código Defecto
*    gwa_ordenes-ZZDEFCOD.

    "Origen
*    gwa_ordenes-DESC_ORIGEN.

    "Código Origen
*    gwa_ordenes-ZZORIGEN.

    "Causa
*    gwa_ordenes-DESC_AUSA.

    "Código causa
*    gwa_ordenes-ZZCAUSA.

    "Destino
*    gwa_ordenes-ZZDESTINO.

    "Causa de Desviación
*    gwa_ordenes-GRUND.
    gwa_ordenes-zzdesperop   = zppe_notif_addreg-zzdesperop. "Insert @0002
    APPEND gwa_ordenes TO gt_ordenes.

    PERFORM f_completar_componente USING lv_nro
                                         lt_data-aufnr
                                         lt_data-vornr
                                         lt_data-plnfl
                                         lt_data-ktsch
                                         gwa_ordenes-zztipo_notif
                                         1"gwa_ordenes-lmnga
                                         space.
  ENDLOOP.

  IF sy-subrc EQ 0.
    SORT gt_ordenes BY nro   ASCENDING.
*                       aufnr ASCENDING
*                       vornr ASCENDING.

    CALL METHOD gr_grid01->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
    gv_addreg = '4'.
  ELSE.
    p_error = 'X'.

    MESSAGE TEXT-013 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ACTUALIZA_CICLO_AUTO
*&---------------------------------------------------------------------*
FORM f_actualiza_ciclo_auto .
  DATA: lt_diimpt TYPE tt_diimpt      WITH HEADER LINE,
        lwa_imrg  TYPE imrg,
        lv_fltp   TYPE cha_class_data-sollwert,
        lv_char   TYPE cha_class_view-sollwert,
        lv_ktsch  TYPE ktsch.

  CLEAR:   gv_ciclo_auto.
  REFRESH: lt_diimpt.

  READ TABLE gt_lote_auto INTO gwa_lote_auto WITH KEY value = '03'.
  CHECK sy-subrc  EQ 0   AND
        gv_addreg NE '5'.

  "Validar operacion
  IF p_werks EQ 'PE01' OR
     p_werks EQ 'BR01'.
    lv_ktsch = 'EACV'.
  ELSEIF p_werks EQ 'CO01'.
    lv_ktsch = 'ACV'.
  ENDIF.
  LOOP AT gt_ordenes INTO gwa_ordenes WHERE ktsch NE lv_ktsch.
  ENDLOOP.
  IF sy-subrc EQ 0.
    EXIT.
  ENDIF.

  READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY zztipo_notif = gv_aprob.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
    EXPORTING
      i_equnr   = gv_equipo
    TABLES
      et_diimpt = lt_diimpt.

  READ TABLE lt_diimpt WITH KEY psort = 'ZPP_CACV'.
  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'MEASUREM_DOCUM_READ_LAST'
    EXPORTING
      virtual_reading_ok = 'X'
      point              = lt_diimpt-point
    IMPORTING
      imrg_wa            = lwa_imrg.
  CHECK sy-subrc EQ 0.
  lv_fltp = lwa_imrg-cntrr .

  CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
    EXPORTING
      measurement_point    = lt_diimpt-point
      recorded_value       = '1'
      difference_reading   = 'X'
    EXCEPTIONS
      no_authority         = 1
      point_not_found      = 2
      index_not_unique     = 3
      type_not_found       = 4
      point_locked         = 5
      point_inactive       = 6
      timestamp_in_future  = 7
      timestamp_duprec     = 8
      unit_unfit           = 9
      value_not_fltp       = 10
      value_overflow       = 11
      value_unfit          = 12
      value_missing        = 13
      code_not_found       = 14
      notif_type_not_found = 15
      notif_prio_not_found = 16
      notif_gener_problem  = 17
      update_failed        = 18
      invalid_time         = 19
      invalid_date         = 20
      OTHERS               = 21.
  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
    EXPORTING
      i_number_of_digits       = '0'
      i_fltp_value             = lv_fltp
      i_value_not_initial_flag = 'X'
      i_screen_fieldlength     = 16
    IMPORTING
      e_char_field             = lv_char.

  CONDENSE lv_char NO-GAPS.
  gv_ciclo_auto = lv_char .
  ADD 1 TO gv_ciclo_auto.
  CONDENSE gv_ciclo_auto NO-GAPS.

  LOOP AT gt_ordenes ASSIGNING <fs_ordenes> WHERE zztipo_notif EQ gv_aprob.
    <fs_ordenes>-zzciclo_auto = gv_ciclo_auto.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB01_ZZCICLO_AUTO
*&---------------------------------------------------------------------*
FORM f_add_tab01_zzciclo_auto CHANGING p_error.
  DATA: "lt_afru            TYPE tt_afru            WITH HEADER LINE,
    lt_data            TYPE tt_data_masivo     WITH HEADER LINE,
    lt_makt            TYPE tt_makt            WITH HEADER LINE,
    lt_t006a           TYPE tt_t006a           WITH HEADER LINE,
    lt_vornr           TYPE tt_vornr           WITH HEADER LINE,
    lt_allocvaluesnum  TYPE tt_allocvaluesnum  WITH HEADER LINE,
    lt_allocvalueschar TYPE tt_allocvalueschar WITH HEADER LINE,
    lt_allocvaluescurr TYPE tt_allocvaluescurr WITH HEADER LINE,
    lt_return          TYPE tt_return          WITH HEADER LINE,
    lt_aufk            TYPE tt_aufk            WITH HEADER LINE,
    lv_atinn           TYPE atinn,
    lv_text            TYPE string,
    lv_vornr           TYPE vornr,
    lv_ktsch           TYPE ktsch,
    lv_nro             TYPE i,
    lv_class           TYPE klasse_d,
    lv_klart           TYPE klassenart,
    lv_obtab           TYPE tabelle,
    lv_objectkey       TYPE objnum.

  REFRESH: lt_data,
           lt_makt,
           lt_t006a,
           lt_aufk.

  CLEAR: lv_atinn.

  IF zppe_notif_addreg-budat IS INITIAL.
    MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  IF zppe_notif_addreg-ktsch IS INITIAL.
    lv_text = TEXT-008.
    REPLACE '&' WITH TEXT-079 INTO lv_text. "'Clave Modelo' INTO lv_text.
    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
*  ELSEIF zppe_notif_addreg-ktsch NE 'SACV'.
*    lv_text = TEXT-016.
*    REPLACE '&' WITH 'Clave Modelo' INTO lv_text.
*    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*    p_error = 'X'.
*    EXIT.
  ENDIF.

  READ TABLE gt_lote_auto INTO gwa_lote_auto WITH KEY value = '10'.
  IF sy-subrc                EQ 0           AND
     zppe_notif_addreg-aufnr IS NOT INITIAL.
    REFRESH: lt_vornr.

    IF zppe_notif_addreg-ktsch IS INITIAL AND
       zppe_notif_addreg-vornr IS INITIAL.
      MESSAGE TEXT-012 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.

    IF zppe_notif_addreg-vornr IS NOT INITIAL.
      lv_vornr = zppe_notif_addreg-vornr.

      SELECT SINGLE afvc~ktsch
        INTO lv_ktsch
        FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                  INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                  INNER JOIN crhd ON afvc~arbid EQ crhd~objid
        WHERE aufk~aufnr EQ zppe_notif_addreg-aufnr
          AND afvc~vornr EQ zppe_notif_addreg-vornr
          AND crhd~arbpl EQ p_arbpl
          AND crhd~werks EQ p_werks.
    ELSE.
      lv_ktsch = zppe_notif_addreg-ktsch.

      SELECT SINGLE afvc~vornr
        INTO lv_vornr
        FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                  INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                  INNER JOIN crhd ON afvc~arbid EQ crhd~objid
        WHERE aufk~aufnr EQ zppe_notif_addreg-aufnr
          AND afvc~ktsch EQ zppe_notif_addreg-ktsch
          AND crhd~arbpl EQ p_arbpl
          AND crhd~werks EQ p_werks.
    ENDIF.

    SELECT afvc~vornr
      INTO TABLE lt_vornr
      FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
      WHERE aufk~aufnr EQ zppe_notif_addreg-aufnr
        AND crhd~arbpl EQ p_arbpl
        AND crhd~werks EQ p_werks.
    SORT lt_vornr BY vornr ASCENDING.
    READ TABLE lt_vornr INDEX 1.
    IF lt_vornr-vornr EQ lv_vornr.
      IF gv_propuesta_vid EQ '1'.
        PERFORM f_add_tab01_autoaufnr USING    lv_vornr
                                               lv_ktsch
                                      CHANGING p_error.
      ELSE.
        PERFORM f_add_tab01_aufnr CHANGING p_error.
      ENDIF.

      EXIT.
    ENDIF.
  ENDIF.

  SELECT rueck,
         rmzhl,
         aufnr,
         vornr
    INTO TABLE @DATA(lt_afru)
    FROM afru
    WHERE werks        EQ @p_werks
      AND zzciclo_auto EQ @zppe_notif_addreg-zzciclo_auto
      AND stokz        EQ @space
      AND stzhl        EQ @space
      AND lmnga        NE 0
      AND zzktsch      NE @zppe_notif_addreg-ktsch.

  SORT lt_afru BY aufnr ASCENDING
                  vornr DESCENDING.
  DATA(lt_aux) = lt_afru.
  DELETE ADJACENT DUPLICATES FROM lt_aux COMPARING aufnr.

  LOOP AT lt_aux INTO DATA(lwa_aux).
    READ TABLE lt_afru INTO DATA(lwa_afru) WITH KEY aufnr = lwa_aux-aufnr.
    IF sy-subrc EQ 0.
      DELETE lt_afru WHERE aufnr EQ lwa_aux-aufnr
                       AND vornr NE lwa_afru-vornr.
    ENDIF.
  ENDLOOP.

  IF lt_afru[] IS NOT INITIAL.
    SELECT  afru~aufnr
            afvc~vornr
            afvc~ktsch
            afvc~plnfl
            afko~plnbez
            afko~gmein
            afru~zzlote
            afvc~steus
            afru~zzequipo
            afru~zzcod_lanza
            afru~rueck
            afru~rmzhl
      INTO TABLE lt_data
      FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
      FOR ALL ENTRIES IN lt_afru
      WHERE afru~rueck EQ lt_afru-rueck
        AND afru~rmzhl EQ lt_afru-rmzhl
        AND crhd~arbpl EQ p_arbpl
        AND crhd~werks EQ p_werks
        AND afvc~ktsch EQ zppe_notif_addreg-ktsch.
  ENDIF.

  IF lt_data[] IS NOT INITIAL.
    SORT lt_data BY aufnr ASCENDING
                    vornr ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM lt_data COMPARING aufnr
*                                                      vornr.

    SELECT matnr
           maktx
      INTO TABLE lt_makt
      FROM makt
      FOR ALL ENTRIES IN lt_data
      WHERE matnr EQ lt_data-plnbez
        AND spras EQ sy-langu.

    READ TABLE gt_tipnot INTO gwa_tipnot INDEX 1.

    SELECT msehi
           mseh6
      INTO TABLE lt_t006a
      FROM t006a
      WHERE spras EQ sy-langu.

    SELECT *
      INTO TABLE lt_aufk
      FROM aufk
      FOR ALL ENTRIES IN lt_data
      WHERE aufnr EQ lt_data-aufnr.
  ENDIF.
*  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
*    EXPORTING
*      input  = 'ZLOTE_CICLO_AUTOCLAVE'
*    IMPORTING
*      output = lv_atinn.
*
*  SELECT  afko~aufnr
*          afvc~vornr
*          afvc~ktsch
*          afvc~plnfl
*          afko~plnbez
*          afko~gmein
*          afpo~charg
*          afvc~steus
*    INTO TABLE lt_data
*  FROM ausp INNER JOIN mcha ON ausp~objek EQ mcha~cuobj_bm
*            INNER JOIN afpo ON mcha~matnr EQ afpo~matnr    AND
*                               mcha~charg EQ afpo~charg
*            INNER JOIN aufk ON afpo~aufnr EQ aufk~aufnr
*            INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
*            INNER JOIN crhd ON aufk~werks EQ crhd~werks
*            INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl AND
*                               crhd~objid EQ afvc~arbid
*  WHERE ausp~atinn EQ lv_atinn
*    AND ausp~atwrt EQ zppe_notif_addreg-zzlote_curvado
*    AND mcha~werks EQ p_werks
*    AND crhd~arbpl EQ p_arbpl
*    AND afvc~ktsch EQ zppe_notif_addreg-ktsch.
*
*  SELECT aufnr
*         vornr
*    INTO TABLE lt_afru
*    FROM afru
*    FOR ALL ENTRIES IN lt_data
*    WHERE aufnr EQ lt_data-aufnr
*      AND vornr EQ lt_data-vornr
*      AND stokz EQ space
*      AND stzhl EQ space
*      AND lmnga EQ 1.
*
*  LOOP AT lt_afru.
*    DELETE lt_data WHERE aufnr EQ lt_afru-aufnr
*                     AND vornr EQ lt_afru-vornr.
*  ENDLOOP.

  LOOP AT lt_data.
    CLEAR: gwa_ordenes,
           lt_makt,
           lt_t006a.

    lv_nro = gv_nro.
    ADD 1 TO gv_nro.
    READ TABLE lt_makt  WITH KEY matnr = lt_data-plnbez.
    READ TABLE lt_t006a WITH KEY msehi = lt_data-gmein.
    READ TABLE lt_aufk  WITH KEY aufnr = lt_data-aufnr.

    "Nro
    gwa_ordenes-nro = gv_nro.

    "Orden
    gwa_ordenes-aufnr = lt_data-aufnr.

    "Operación
    gwa_ordenes-vornr = lt_data-vornr.

    "Clave Modelo
    gwa_ordenes-ktsch = lt_data-ktsch.

    "Fecha contabilización
    gwa_ordenes-budat = zppe_notif_addreg-budat.

    "Secuencia
    gwa_ordenes-plnfl = lt_data-plnfl.

    "Equipo
    gwa_ordenes-zzequipo = lt_data-zzequipo.

    "Lote
    gwa_ordenes-zzlote = lt_data-charg.

    "Material
    gwa_ordenes-plnbez = lt_data-plnbez.

    "Descripción
    gwa_ordenes-maktx = lt_makt-maktx.

    "UM
    gwa_ordenes-gmein     = lt_data-gmein.
    gwa_ordenes-gmein_txt = lt_t006a-mseh6.

    "Cantidad
    gwa_ordenes-lmnga = 1.

    "Tipo de Notificación
    IF gv_hereda_tipnot   EQ '1' AND
       gwa_ordenes-zzlote IS NOT INITIAL.
      CALL FUNCTION 'QMSP_MATERIAL_BATCH_CLASS_READ'
        EXPORTING
          i_matnr                = gwa_ordenes-plnbez
          i_charg                = gwa_ordenes-zzlote
          i_werks                = lt_aufk-werks
          i_mara_level           = 'X'
        IMPORTING
          e_class                = lv_class
          e_klart                = lv_klart
          e_obtab                = lv_obtab
        EXCEPTIONS
          no_class               = 1
          internal_error_classif = 2
          no_change_service      = 3
          OTHERS                 = 4.

      CONCATENATE gwa_ordenes-plnbez
                  gwa_ordenes-zzlote
                  INTO lv_objectkey.

      CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
        EXPORTING
          objectkey        = lv_objectkey
          objecttable      = lv_obtab
          classnum         = lv_class
          classtype        = lv_klart
          unvaluated_chars = 'X'
        TABLES
          allocvaluesnum   = lt_allocvaluesnum[]
          allocvalueschar  = lt_allocvalueschar[]
          allocvaluescurr  = lt_allocvaluescurr[]
          return           = lt_return[].

      READ TABLE lt_allocvalueschar WITH KEY charact = 'Z_TIPO_NOTIFICACION'.
      IF lt_allocvalueschar-value_char IS NOT INITIAL.
        gwa_ordenes-zztipo_notif = lt_allocvalueschar-value_char.
      ELSE.
        gwa_ordenes-zztipo_notif = gv_aprob.
      ENDIF.
    ELSE.
      gwa_ordenes-zztipo_notif = zppe_notif_addreg-zztipo_notif.
    ENDIF.

    "Lote de Corte
*    gwa_ordenes-zzlote_corte =

    "Lote de Curvado
*    gwa_ordenes-zzlote_curvado =

    "Ciclo de TQ
*    gwa_ordenes-ZZCICLO_TQ.

    "Ciclo de Autoclave
    gwa_ordenes-zzciclo_auto = zppe_notif_addreg-zzciclo_auto.

    "Código de Lanzada
    gwa_ordenes-zzcod_lanza = lt_data-zzcod_lanza.

    "Lote de Carga
*    gwa_ordenes-ZZLOTE_CARGA.

    "Defecto
*    gwa_ordenes-DESC_DEFCOD.

    "Código Defecto
*    gwa_ordenes-ZZDEFCOD.

    "Origen
*    gwa_ordenes-DESC_ORIGEN.

    "Código Origen
*    gwa_ordenes-ZZORIGEN.

    "Causa
*    gwa_ordenes-DESC_AUSA.

    "Código causa
*    gwa_ordenes-ZZCAUSA.

    "Destino
*    gwa_ordenes-ZZDESTINO.

    "Causa de Desviación
*    gwa_ordenes-GRUND.
    gwa_ordenes-zzdesperop   = zppe_notif_addreg-zzdesperop. "Insert @0002
    APPEND gwa_ordenes TO gt_ordenes.

    PERFORM f_completar_componente USING lv_nro
                                         lt_data-aufnr
                                         lt_data-vornr
                                         lt_data-plnfl
                                         lt_data-ktsch
                                         gwa_ordenes-zztipo_notif
                                         1"gwa_ordenes-lmnga
                                         space.
  ENDLOOP.

  IF sy-subrc EQ 0.
    SORT gt_ordenes BY nro   ASCENDING.
*                       aufnr ASCENDING
*                       vornr ASCENDING.

    CALL METHOD gr_grid01->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
    gv_addreg = '5'.
  ELSE.
    p_error = 'X'.

    MESSAGE TEXT-013 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GENERA_LOTE_AUTO
*&---------------------------------------------------------------------*
FORM f_genera_lote_auto CHANGING p_error
                                 p_val.
  DATA: lt_vornr    TYPE tt_vornr  WITH HEADER LINE,
        lt_return   TYPE tt_return WITH HEADER LINE,
        lv_material TYPE matnr18,
        lv_werks    TYPE werks_d,
        lv_text     TYPE string,
        lv_cont     TYPE i.

  REFRESH: lt_vornr.

  READ TABLE gt_lote_auto INTO gwa_lote_auto WITH KEY value = '07'.
  CHECK sy-subrc                 EQ 0               AND
        <fs_ordenes>-zztipo_notif IN gr_tipnot_aprob AND
        <fs_ordenes>-zzlote       IS INITIAL.

  SELECT afvc~vornr
    INTO TABLE lt_vornr
    FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN crhd ON afvc~arbid EQ crhd~objid
    WHERE aufk~aufnr EQ <fs_ordenes>-aufnr
      AND crhd~arbpl EQ p_arbpl
      AND crhd~werks EQ p_werks.
  SORT lt_vornr BY vornr ASCENDING.
  READ TABLE lt_vornr INDEX 1.
  CHECK lt_vornr-vornr NE <fs_ordenes>-vornr.

  SELECT SINGLE werks
    INTO lv_werks
    FROM aufk
    WHERE aufnr EQ <fs_ordenes>-aufnr.

  lv_material = <fs_ordenes>-plnbez.

  CLEAR: lv_cont.

  IF p_val IS INITIAL.
    p_val = 'X'.

    DO.
      REFRESH: lt_return.

      CALL FUNCTION 'BAPI_BATCH_CREATE'
        EXPORTING
          material = lv_material
          plant    = lv_werks
        IMPORTING
          batch    = <fs_ordenes>-zzlote
        TABLES
          return   = lt_return.

      READ TABLE lt_return WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        ADD 1 TO lv_cont.

        IF lv_cont GT 300.
          lv_text = TEXT-045.
          REPLACE '&' IN lv_text WITH lt_return-message.
          PERFORM f_agregar_log_v2 USING <fs_ordenes>-nro
                                         space
                                         'N'
                                         space
                                         space
                                         lv_text
                                         space.
          p_error = lv_text.
          EXIT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ELSE.
    IF p_error IS INITIAL.
      CALL FUNCTION 'BAPI_BATCH_CREATE'
        EXPORTING
          material = lv_material
          plant    = lv_werks
        IMPORTING
          batch    = <fs_ordenes>-zzlote
        TABLES
          return   = lt_return.
    ELSE.
      PERFORM f_agregar_log_v2 USING <fs_ordenes>-nro
                                     space
                                     'N'
                                     space
                                     space
                                     p_error
                                     space.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDAR_ESTADO_LOTE
*&---------------------------------------------------------------------*
FORM f_validar_estado_lote CHANGING p_error.
  DATA: lt_allocvaluesnum  TYPE tt_allocvaluesnum  WITH HEADER LINE,
        lt_allocvalueschar TYPE tt_allocvalueschar WITH HEADER LINE,
        lt_allocvaluescurr TYPE tt_allocvaluescurr WITH HEADER LINE,
        lt_return          TYPE tt_return          WITH HEADER LINE,
        lwa_afpo           TYPE afpo,
        lv_class           TYPE klasse_d,
        lv_klart           TYPE klassenart,
        lv_obtab           TYPE tabelle,
        lv_objectkey       TYPE objnum,
        lv_txt             TYPE string.

  CLEAR:   lwa_afpo,
           lv_class,
           lv_klart,
           lv_obtab,
           lv_objectkey.
  REFRESH: lt_allocvalueschar.


  CHECK p_werks EQ 'PE01' OR
        p_werks EQ 'PE02' OR
        p_werks EQ 'CO01' OR
        p_werks EQ 'BR01'.
  SELECT SINGLE *
    INTO lwa_afpo
    FROM afpo
    WHERE aufnr EQ zppe_notif_addreg-aufnr.

  IF p_werks EQ 'PE02'.
    lwa_afpo-charg = zppe_notif_addreg-charg.
  ENDIF.

  CHECK lwa_afpo-charg IS NOT INITIAL.

  CALL FUNCTION 'QMSP_MATERIAL_BATCH_CLASS_READ'
    EXPORTING
      i_matnr                = lwa_afpo-matnr
      i_charg                = lwa_afpo-charg
      i_werks                = lwa_afpo-pwerk
      i_mara_level           = 'X'
    IMPORTING
      e_class                = lv_class
      e_klart                = lv_klart
      e_obtab                = lv_obtab
    EXCEPTIONS
      no_class               = 1
      internal_error_classif = 2
      no_change_service      = 3
      OTHERS                 = 4.

  CONCATENATE lwa_afpo-matnr
              lwa_afpo-charg
              INTO lv_objectkey.

  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey        = lv_objectkey
      objecttable      = lv_obtab
      classnum         = lv_class
      classtype        = lv_klart
      unvaluated_chars = 'X'
    TABLES
      allocvaluesnum   = lt_allocvaluesnum[]
      allocvalueschar  = lt_allocvalueschar[]
      allocvaluescurr  = lt_allocvaluescurr[]
      return           = lt_return[].

  READ TABLE lt_allocvalueschar WITH KEY charact = 'Z_TIPO_NOTIFICACION'.
  IF lt_allocvalueschar-value_neutral EQ '04'.
    lv_txt = TEXT-022.
    REPLACE '&' IN lv_txt WITH lwa_afpo-charg.
    MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ANULAR_RECHAZO
*&---------------------------------------------------------------------*
FORM f_anular_rechazo CHANGING p_error.
  DATA: "lt_afru  TYPE tt_afru          WITH HEADER LINE,
    lt_aux     TYPE tt_auxmara       WITH HEADER LINE,
    lt_mara    TYPE tt_mara          WITH HEADER LINE,
    lt_mdocm   TYPE tt_imrg_mdocm    WITH HEADER LINE,
    lt_equi    TYPE tt_equi          WITH HEADER LINE,
    lt_msg     TYPE zpptt_notif_rpta WITH HEADER LINE,
    lt_enq     TYPE tt_enq           WITH HEADER LINE,
    lwa_return TYPE bapiret1,
    lv_index   TYPE i,
    lv_lines   TYPE i,
    lv_garg    TYPE eqegraarg,
    lv_rsnum   TYPE rsnum,
    lv_locked  TYPE ru_locked,
    lv_cant    TYPE i.                                      "+@0005

  CLEAR:   lv_index,
           lv_cant.                                         "+@0005
  REFRESH: "lt_afru,
           lt_aux,
           lt_mara,
           lt_equi,
           lt_msg.

  CHECK gwa_ordenes-anul_recha EQ 'X'.

*  IF gwa_ordenes-zzlote IS INITIAL.
*    SELECT rueck
*           rmzhl
*      INTO TABLE lt_afru
*      FROM afru
*      WHERE aufnr EQ gwa_ordenes-aufnr
*        AND stokz EQ space
*        AND stzhl EQ space
*        AND vornr EQ gwa_ordenes-vornr
*        AND werks EQ p_werks
*        AND lmnga GT 0.
*  ELSE.
*    SELECT rueck
*           rmzhl
*      INTO TABLE lt_afru
*      FROM afru
*      WHERE aufnr EQ gwa_ordenes-aufnr
*        AND stokz EQ space
*        AND stzhl EQ space
*        AND vornr EQ gwa_ordenes-vornr
*        AND werks EQ p_werks
*        AND lmnga GT 0
*        AND zzlote EQ gwa_ordenes-zzlote.
*  ENDIF.

*{-@0005
*  READ TABLE gt_anul INTO gwa_anul WITH KEY aufnr  = gwa_ordenes-aufnr
*                                            vornr  = gwa_ordenes-vornr
*                                            aplfl  = gwa_ordenes-plnfl
*                                            zzlote = gwa_ordenes-zzlote.
*}-@0005
*{+@0005
  LOOP AT gt_anul INTO gwa_anul WHERE aufnr  EQ gwa_ordenes-aufnr
                                  AND vornr  EQ gwa_ordenes-vornr
                                  AND aplfl  EQ gwa_ordenes-plnfl
                                  AND zzlote EQ gwa_ordenes-zzlote.
    ADD gwa_anul-lmnga TO lv_cant.
  ENDLOOP.
*}+@0005

*  IF sy-subrc NE 0.    "-@0005
  IF lv_cant LT gwa_ordenes-lmnga.
    lt_msg-msg = TEXT-030.
    APPEND lt_msg.
    PERFORM f_agregar_log USING gwa_ordenes-aufnr
                                gwa_ordenes-vornr
                                gwa_ordenes-nro
                                space
                                'N'
                                lt_msg[].
    p_error = 'X'.
    EXIT.
  ENDIF.

*  DESCRIBE TABLE lt_afru LINES lv_lines.
*  IF lv_lines LT gwa_ordenes-lmnga.
*    lt_msg-msg = TEXT-031.
*    APPEND lt_msg.
*    PERFORM f_agregar_log USING gwa_ordenes-aufnr
*                                gwa_ordenes-vornr
*                                1
*                                space
*                                'N'
*                                lt_msg[].
*    p_error = 'X'.
*    EXIT.
*  ENDIF.
*
*  SORT lt_afru BY rueck DESCENDING
*                  rmzhl DESCENDING.

  SELECT SINGLE rsnum
    INTO lv_rsnum
    FROM afko
    WHERE aufnr EQ gwa_ordenes-aufnr.

  CONCATENATE sy-mandt
              lv_rsnum
              INTO lv_garg.

*  DO gwa_ordenes-lmnga TIMES.
*    ADD 1 TO lv_index.
*
*    READ TABLE lt_afru INDEX lv_index.
*  IF sy-subrc EQ 0.

*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*    EXPORTING
*      wait = 'X'.
*
*  WAIT UP TO 1 SECONDS.

  SET UPDATE TASK LOCAL.

  CALL FUNCTION 'BAPI_PRODORDCONF_CANCEL'
    EXPORTING
      confirmation        = gwa_anul-rueck
      confirmationcounter = gwa_anul-rmzhl
    IMPORTING
      return              = lwa_return
      locked              = lv_locked.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  WAIT UP TO 1 SECONDS.

  DELETE gt_anul WHERE rueck EQ gwa_anul-rueck
                   AND rmzhl EQ gwa_anul-rmzhl.
  DO.
*        REFRESH: lt_enq.
*
*        CALL FUNCTION 'ENQUEUE_READ'
*          TABLES
*            enq                   = lt_enq
*          EXCEPTIONS
*            communication_failure = 1
*            system_failure        = 2
*            OTHERS                = 3.
*        READ TABLE lt_enq WITH KEY gname = 'RKPF'
*                                   garg  = lv_garg.
*        IF sy-subrc NE 0.
*          EXIT.
*        ENDIF.

    CALL FUNCTION 'ENQUEUE_EMRKPF'
      EXPORTING
        rsnum          = lv_rsnum
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'DEQUEUE_EMRKPF'
        EXPORTING
          rsnum = lv_rsnum.
      EXIT.
    ENDIF.

  ENDDO.
*  ELSE.
*    EXIT.
*  ENDIF.
*  ENDDO.

  LOOP AT gt_componentes INTO gwa_componentes WHERE nro EQ gwa_ordenes-nro.
*                                                    aufnr EQ gwa_ordenes-aufnr
*                                                AND vornr EQ gwa_ordenes-vornr.
    CLEAR: lt_aux.

    lt_aux-matnr  = gwa_componentes-componente.
    lt_aux-charge = gwa_componentes-lote.

    APPEND lt_aux.
  ENDLOOP.

  SELECT *
    INTO TABLE lt_mara
    FROM mara
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr
      AND mtart EQ 'ZHER'.
  CHECK sy-subrc EQ 0.

  SELECT imrg~mdocm
         equi~matnr
         equi~charge
    INTO TABLE lt_equi
    FROM equi INNER JOIN imptt ON imptt~mpobj EQ equi~objnr
              INNER JOIN imrg  ON imrg~point EQ imptt~point
    FOR ALL ENTRIES IN lt_aux
    WHERE equi~matnr   EQ lt_aux-matnr
      AND equi~charge  EQ lt_aux-charge
      AND equi~eqtyp   EQ 'P'
      AND imptt~inact  EQ space
      AND imptt~lvorm  EQ space
      AND imrg~cancl   EQ space.
  SORT lt_equi BY mdocm DESCENDING.

  LOOP AT gt_componentes INTO gwa_componentes WHERE nro EQ gwa_ordenes-nro.
*                                                    aufnr EQ gwa_ordenes-aufnr
*                                                AND vornr EQ gwa_ordenes-vornr.
    READ TABLE lt_mara WITH KEY matnr = gwa_componentes-componente.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

*    DO gwa_ordenes-lmnga TIMES.
    REFRESH: lt_mdocm.
    READ TABLE lt_equi WITH KEY matnr  = gwa_componentes-componente
                                charge = gwa_componentes-lote.
    IF sy-subrc EQ 0.
      lv_index = sy-tabix.

      lt_mdocm-mdocm = lt_equi-mdocm.
      APPEND lt_mdocm.

      CALL FUNCTION 'MEASUREM_DOCUM_RFC_CANCEL'
        TABLES
          it_cancel_requests = lt_mdocm[].

      DELETE lt_equi INDEX lv_index.
    ELSE.
      EXIT.
    ENDIF.
*    ENDDO.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ANULAR_RECHAZO_V2
*&---------------------------------------------------------------------*
FORM f_anular_rechazo_v2 CHANGING p_error.
  DATA: lt_aux     TYPE tt_auxmara       WITH HEADER LINE,
        lt_mara    TYPE tt_mara          WITH HEADER LINE,
        lt_mdocm   TYPE tt_imrg_mdocm    WITH HEADER LINE,
        lt_equi    TYPE tt_equi          WITH HEADER LINE,
        lt_msg     TYPE zpptt_notif_rpta WITH HEADER LINE,
        lt_enq     TYPE tt_enq           WITH HEADER LINE,
        lwa_return TYPE bapiret1,
        lv_index   TYPE i,
        lv_lines   TYPE i,
        lv_garg    TYPE eqegraarg,
        lv_rsnum   TYPE rsnum,
        lv_locked  TYPE ru_locked,
        lv_cant    TYPE i,
        lv_pend    TYPE i.

  CLEAR:   lv_index,
           lv_cant,
           lv_pend.
  REFRESH: lt_aux,
           lt_mara,
           lt_equi,
           lt_msg.

  CHECK <fs_ordenes>-anul_recha EQ 'X'.

*{-@0005
*  READ TABLE gt_anul INTO gwa_anul WITH KEY aufnr  = <fs_ordenes>-aufnr
*                                            vornr  = <fs_ordenes>-vornr
*                                            aplfl  = <fs_ordenes>-plnfl
*                                            zzlote = <fs_ordenes>-zzlote.
*}-@0005
*{+@0005
  LOOP AT gt_anul INTO gwa_anul WHERE aufnr  EQ <fs_ordenes>-aufnr
                                  AND vornr  EQ <fs_ordenes>-vornr
                                  AND aplfl  EQ <fs_ordenes>-plnfl
                                  AND zzlote EQ <fs_ordenes>-zzlote.
    ADD gwa_anul-lmnga TO lv_cant.
  ENDLOOP.
*}+@0005

*  IF sy-subrc NE 0.    "-@0005
  IF lv_cant LT <fs_ordenes>-lmnga.
    PERFORM f_agregar_log_v2 USING <fs_ordenes>-nro
                                   space
                                   'N'
                                   space
                                   space
                                   TEXT-030
                                   space.
    p_error = 'X'.
    EXIT.
  ENDIF.

*  SELECT SINGLE rsnum
*    INTO lv_rsnum
*    FROM afko
*    WHERE aufnr EQ <fs_ordenes>-aufnr.
*
*  CONCATENATE sy-mandt
*              lv_rsnum
*              INTO lv_garg.

*  SET UPDATE TASK LOCAL.


*  DO.
*    CALL FUNCTION 'ENQUEUE_ESORDER'
*      EXPORTING
*        aufnr          = <fs_ordenes>-aufnr
*      EXCEPTIONS
*        foreign_lock   = 1
*        system_failure = 2
*        OTHERS         = 3.
*    IF sy-subrc EQ 0.
*      CALL FUNCTION 'DEQUEUE_ESORDER'
*        EXPORTING
*          aufnr = <fs_ordenes>-aufnr.
*      EXIT.
*    ELSE.
*      WAIT UP TO 1 SECONDS.
**      BREAK-POINT.
*    ENDIF.
*  ENDDO.
*
**  DATA: lv_rsnum   TYPE rsnum.
*  SELECT SINGLE rsnum
*    INTO lv_rsnum
*    FROM afko
*    WHERE aufnr EQ <fs_ordenes>-aufnr.
*  DO.
*    CALL FUNCTION 'ENQUEUE_EMRKPF'
*      EXPORTING
*        rsnum          = lv_rsnum
*      EXCEPTIONS
*        foreign_lock   = 1
*        system_failure = 2
*        OTHERS         = 3.
*    IF sy-subrc EQ 0.
*      CALL FUNCTION 'DEQUEUE_EMRKPF'
*        EXPORTING
*          rsnum = lv_rsnum.
*      EXIT.
*    ELSE.
*      WAIT UP TO 1 SECONDS.
**      BREAK-POINT.
*    ENDIF.
*  ENDDO.
*
*{-@0005
*  DO.
*    CLEAR: lv_locked,
*           lwa_return.
*
*    CALL FUNCTION 'BAPI_PRODORDCONF_CANCEL'
*      EXPORTING
*        confirmation        = gwa_anul-rueck
*        confirmationcounter = gwa_anul-rmzhl
*      IMPORTING
*        return              = lwa_return
*        locked              = lv_locked.
*    IF lv_locked       IS INITIAL AND
*       lwa_return-type NE 'E'.
*      EXIT.
*    ENDIF.
*  ENDDO.
*}-@0005

*{+@0005
  lv_pend = <fs_ordenes>-lmnga.

  LOOP AT gt_anul INTO gwa_anul WHERE aufnr  EQ <fs_ordenes>-aufnr
                                  AND vornr  EQ <fs_ordenes>-vornr
                                  AND aplfl  EQ <fs_ordenes>-plnfl
                                  AND zzlote EQ <fs_ordenes>-zzlote.
    DO.
      CLEAR: lv_locked,
             lwa_return.

      CALL FUNCTION 'BAPI_PRODORDCONF_CANCEL'
        EXPORTING
          confirmation        = gwa_anul-rueck
          confirmationcounter = gwa_anul-rmzhl
        IMPORTING
          return              = lwa_return
          locked              = lv_locked.
      IF lv_locked       IS INITIAL AND
         lwa_return-type NE 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        WAIT UP TO 1 SECONDS.
        gv_anular_rechazo = 'X'.

        EXIT.
      ENDIF.
    ENDDO.

    SUBTRACT gwa_anul-lmnga FROM lv_pend.

    DELETE gt_anul WHERE rueck EQ gwa_anul-rueck
                     AND rmzhl EQ gwa_anul-rmzhl.

    IF lv_pend LE 0.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF lv_pend LT 0.
    PERFORM f_notificar_delta_anul USING <fs_ordenes>-aufnr
                                         <fs_ordenes>-vornr
                                         <fs_ordenes>-ktsch
                                         <fs_ordenes>-budat
                                         <fs_ordenes>-plnfl
                                         <fs_ordenes>-gmein
                                         lv_pend
                                         <fs_ordenes>-zzlote
                                         gv_aprob.
  ENDIF.
*}+@0005

*  DO.
*    CALL FUNCTION 'ENQUEUE_EMRKPF'
*      EXPORTING
*        rsnum          = lv_rsnum
*      EXCEPTIONS
*        foreign_lock   = 1
*        system_failure = 2
*        OTHERS         = 3.
*    IF sy-subrc EQ 0.
*      CALL FUNCTION 'DEQUEUE_EMRKPF'
*        EXPORTING
*          rsnum = lv_rsnum.
*      EXIT.
*    ENDIF.
*
*  ENDDO.

  LOOP AT gt_componentes INTO gwa_componentes WHERE nro EQ <fs_ordenes>-nro.
*                                                    aufnr EQ <fs_ordenes>-aufnr
*                                                AND vornr EQ <fs_ordenes>-vornr.
    CLEAR: lt_aux.

    lt_aux-matnr  = gwa_componentes-componente.
    lt_aux-charge = gwa_componentes-lote.

    APPEND lt_aux.
  ENDLOOP.
  CHECK sy-subrc EQ 0.

  SELECT *
    INTO TABLE lt_mara
    FROM mara
    FOR ALL ENTRIES IN lt_aux
    WHERE matnr EQ lt_aux-matnr
      AND mtart EQ 'ZHER'.
  CHECK sy-subrc EQ 0.

  SELECT imrg~mdocm
         equi~matnr
         equi~charge
    INTO TABLE lt_equi
    FROM equi INNER JOIN imptt ON imptt~mpobj EQ equi~objnr
              INNER JOIN imrg  ON imrg~point EQ imptt~point
    FOR ALL ENTRIES IN lt_aux
    WHERE equi~matnr   EQ lt_aux-matnr
      AND equi~charge  EQ lt_aux-charge
      AND equi~eqtyp   EQ 'P'
      AND imptt~inact  EQ space
      AND imptt~lvorm  EQ space
      AND imrg~cancl   EQ space.
  SORT lt_equi BY mdocm DESCENDING.

  LOOP AT gt_componentes INTO gwa_componentes WHERE nro EQ <fs_ordenes>-nro.
*                                                    aufnr EQ <fs_ordenes>-aufnr
*                                                AND vornr EQ <fs_ordenes>-vornr.
    READ TABLE lt_mara WITH KEY matnr = gwa_componentes-componente.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    REFRESH: lt_mdocm.
    READ TABLE lt_equi WITH KEY matnr  = gwa_componentes-componente
                                charge = gwa_componentes-lote.
    IF sy-subrc EQ 0.
      lv_index = sy-tabix.

      lt_mdocm-mdocm = lt_equi-mdocm.
      APPEND lt_mdocm.

      CALL FUNCTION 'MEASUREM_DOCUM_RFC_CANCEL'
        TABLES
          it_cancel_requests = lt_mdocm[].

      DELETE lt_equi INDEX lv_index.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_TAB01_F4_EQUNR
*&---------------------------------------------------------------------*
FORM f_tab01_f4_equnr USING    p_row
                      CHANGING p_modi TYPE lvc_s_modi
                               p_ok.
  DATA: lt_data TYPE tt_equz       WITH HEADER LINE,
        lt_ret  TYPE tt_ddshretval WITH HEADER LINE.
  CLEAR:   p_modi,
           p_ok.
  REFRESH: lt_data,
           lt_ret.

  READ TABLE gt_ordenes INTO gwa_ordenes INDEX p_row.

  SELECT equz~equnr
         eqkt~eqktx
    INTO TABLE lt_data
    FROM crhd INNER JOIN iloa ON crhd~objid EQ iloa~ppsid AND
                                 crhd~werks EQ iloa~swerk
              INNER JOIN equz ON iloa~iloan EQ equz~iloan
              INNER JOIN eqkt ON equz~equnr EQ eqkt~equnr
    WHERE crhd~arbpl EQ p_arbpl
      AND crhd~werks EQ p_werks
      AND eqkt~spras EQ sy-langu.

  SORT lt_data BY equnr ASCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'EQUNR'
      window_title    = TEXT-x53 "'Lista de valores'
      value_org       = 'S'
    TABLES
      value_tab       = lt_data[]
      return_tab      = lt_ret[]
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK gv_addreg NE '2'.

  IF sy-subrc = 0.
    READ TABLE lt_ret INDEX 1.

    p_modi-row_id    = p_row.
    p_modi-fieldname = 'ZZEQUIPO'.
    p_modi-value     = lt_ret-fieldval.

    p_ok = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDAR_FLAG_OBLIGATORIOS
*&---------------------------------------------------------------------*
FORM f_validar_flag_obligatorios CHANGING p_error.
  DATA: lv_txt TYPE string.

  READ TABLE gt_cawnt INTO gwa_cawnt WITH KEY atwtb = zppe_notif_addreg-zztipo_notif.
  AUTHORITY-CHECK OBJECT 'ZPP_TIPNOT'
   ID 'ZPP_TIPNOT' FIELD gwa_cawnt-atzhl+2(2).
  IF sy-subrc NE 0.
    p_error = 'X'.

    MESSAGE TEXT-026 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  READ TABLE gt_tipnot_oblig INTO gwa_tipnot_oblig WITH KEY atwtb = zppe_notif_addreg-zztipo_notif.

  "Origen
  IF gwa_tipnot_oblig-oblig+0(1) = '1'.
    IF zppe_notif_addreg-zzorigen IS INITIAL OR
       zppe_notif_addreg-zzorcod  IS INITIAL.
      p_error = 'X'.

      lv_txt = TEXT-025.
      REPLACE '&1' IN lv_txt WITH TEXT-073. "'Origen'.

      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  "Causa
  IF gwa_tipnot_oblig-oblig+1(1) = '1'.
    IF zppe_notif_addreg-zzcausa IS INITIAL OR
       zppe_notif_addreg-zzcaucod  IS INITIAL.
      p_error = 'X'.

      lv_txt = TEXT-025.
      REPLACE '&1' IN lv_txt WITH TEXT-x26. "'Causa'.

      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  "Defecto
  IF gwa_tipnot_oblig-oblig+2(1) = '1'.
    IF zppe_notif_addreg-zzdefecto IS INITIAL OR
       zppe_notif_addreg-zzdefcod  IS INITIAL.
      p_error = 'X'.

      lv_txt = TEXT-025.
      REPLACE '&1' IN lv_txt WITH TEXT-074. "'Defecto'.

      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  "Destino
  IF gwa_tipnot_oblig-oblig+3(1) = '1'.
    IF zppe_notif_addreg-zzdestino IS INITIAL.
      p_error = 'X'.

      lv_txt = TEXT-025.
      REPLACE '&1' IN lv_txt WITH TEXT-x28. "'Destino'.

      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  "Causa de desviación
  IF gwa_tipnot_oblig-oblig+4(1) = '1'.
    IF zppe_notif_addreg-grund IS INITIAL.
      p_error = 'X'.

      lv_txt = TEXT-025.
      REPLACE '&1' IN lv_txt WITH TEXT-070. "'Causa de desviación'.

      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB01_ID_IOX
*&---------------------------------------------------------------------*
FORM f_add_tab01_id_iox CHANGING p_ok
                                 p_error.
  DATA: lv_objectkey       TYPE objnum,
        lt_allocvaluesnum  TYPE tt_allocvaluesnum  WITH HEADER LINE,
        lt_allocvalueschar TYPE tt_allocvalueschar WITH HEADER LINE,
        lt_allocvaluescurr TYPE tt_allocvaluescurr WITH HEADER LINE,
        lt_return          TYPE tt_return          WITH HEADER LINE.

  CLEAR:   p_ok.
  REFRESH: lt_allocvalueschar.

  SELECT SINGLE matnr
    INTO lv_objectkey
    FROM afpo
    WHERE aufnr EQ zppe_notif_addreg-id+0(12).

  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey       = lv_objectkey
      objecttable     = 'MARA'
      classnum        = 'Z_FOR_PIEZAS'
      classtype       = '001'
    TABLES
      allocvaluesnum  = lt_allocvaluesnum[]
      allocvalueschar = lt_allocvalueschar[]
      allocvaluescurr = lt_allocvaluescurr[]
      return          = lt_return[].

  READ TABLE lt_allocvalueschar WITH KEY charact = 'Z_GEOMETRY_TYPE'.
  IF lt_allocvalueschar-value_neutral EQ '02'.
    CLEAR: zppe_notif_addreg-aufnr,
           zppe_notif_addreg-charg.

    zppe_notif_addreg-aufnr = zppe_notif_addreg-id+0(12).
    zppe_notif_addreg-charg = zppe_notif_addreg-id+12(10).

    CLEAR: zppe_notif_addreg-id.

    PERFORM f_add_tab01_aufnr CHANGING p_error.

    p_ok = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB01_ID_AVO
*&---------------------------------------------------------------------*
FORM f_add_tab01_id_avo CHANGING p_ok
                                 p_error.
  DATA: lt_afvc    TYPE tt_data_lcorte WITH HEADER LINE,
        lv_bismt   TYPE bismt,
        lv_bismt2  TYPE bismt,
        lv_dummy   TYPE string,
        lv_charg   TYPE charg_d,
        lv_txt     TYPE string,
        lv_matnr   TYPE matnr,
        lv_aufnr   TYPE aufnr,
        lv_entrada TYPE c.

  CLEAR: lv_aufnr,
         lv_entrada.

  SELECT SINGLE aufnr
    INTO lv_aufnr
    FROM aufk
    WHERE aufnr EQ zppe_notif_addreg-id+0(12).
  IF sy-subrc NE 0.
    IF zppe_notif_addreg-aufnr IS INITIAL.
      lv_txt = TEXT-008.
      REPLACE '&' IN lv_txt WITH TEXT-078. "'Orden de Fabricación'.
      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ELSE.
      lv_aufnr = zppe_notif_addreg-aufnr.
    ENDIF.
  ELSE.
    lv_entrada = 'X'.
  ENDIF.

  CLEAR:   p_ok,
           p_error,
           lv_bismt.
  REFRESH: lt_afvc.

  SELECT aufk~aufnr
         afvc~vornr
         afvc~ktsch
  INTO TABLE lt_afvc
  FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
            INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
            INNER JOIN crhd ON afvc~arbid EQ crhd~objid
  WHERE afko~aufnr EQ lv_aufnr
    AND crhd~arbpl EQ p_arbpl
    AND crhd~werks EQ p_werks.

  SORT lt_afvc BY vornr ASCENDING.
  READ TABLE lt_afvc INDEX 1.

  IF lt_afvc-ktsch NE zppe_notif_addreg-ktsch.
    IF lv_entrada EQ 'X'.
      lv_txt = TEXT-042.
      REPLACE '&' IN lv_txt WITH zppe_notif_addreg-ktsch.
      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.

    CLEAR: zppe_notif_addreg-charg.

    SPLIT zppe_notif_addreg-id+1 AT '''' INTO lv_bismt
                                              lv_dummy.
    IF lv_dummy IS INITIAL.
      MESSAGE TEXT-044 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.

    SPLIT lv_dummy AT '''' INTO lv_bismt2
                                lv_dummy.
    IF lv_dummy IS INITIAL.
      MESSAGE TEXT-044 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
    CONCATENATE lv_bismt
                lv_bismt2
                lv_dummy(1)
                INTO lv_bismt
                SEPARATED BY '-'.
    SPLIT lv_dummy+3 AT 'PA' INTO lv_charg
                                  lv_dummy.

    REPLACE ALL OCCURRENCES OF '''' IN lv_bismt WITH '-'.

    SELECT SINGLE COUNT(*)
      FROM mara INNER JOIN afpo ON mara~matnr EQ afpo~matnr
      WHERE afpo~aufnr EQ zppe_notif_addreg-aufnr
        AND mara~bismt EQ lv_bismt.
    IF sy-subrc EQ 0.
      CLEAR: zppe_notif_addreg-id.

      zppe_notif_addreg-charg = lv_charg.

      PERFORM f_add_tab01_aufnr CHANGING p_error.

      p_ok = 'X'.
    ELSE.
      CALL FUNCTION 'ZF_BUSCA_ZTB_ETIQUETAS'
        EXPORTING
          i_cod_barra = zppe_notif_addreg-id
        IMPORTING
          e_matnr     = lv_matnr
          e_lote_sap  = lv_charg.

      SELECT SINGLE COUNT(*)
        FROM aufk INNER JOIN afpo ON aufk~aufnr EQ afpo~aufnr
                  INNER JOIN mchb ON aufk~werks EQ mchb~werks AND
                                     afpo~matnr EQ mchb~matnr
        WHERE aufk~aufnr = zppe_notif_addreg-aufnr
          AND mchb~matnr = lv_matnr
          AND mchb~charg = lv_charg.
      IF sy-subrc EQ 0.
        CLEAR: zppe_notif_addreg-id.

        zppe_notif_addreg-charg = lv_charg.

        PERFORM f_add_tab01_aufnr CHANGING p_error.

        p_ok = 'X'.
      ELSE.
        MESSAGE TEXT-027 TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
      ENDIF.
      MESSAGE TEXT-027 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ENDIF.
  ELSE.
    IF lv_entrada IS INITIAL.
      lv_txt = TEXT-042.
      REPLACE '&' IN lv_txt WITH zppe_notif_addreg-ktsch.
      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.

    CLEAR: zppe_notif_addreg-aufnr,
           zppe_notif_addreg-charg.
    zppe_notif_addreg-aufnr = zppe_notif_addreg-id+0(12).
    zppe_notif_addreg-charg = zppe_notif_addreg-id+12(10).

    p_ok = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EDITAR_TAB01
*&---------------------------------------------------------------------*
FORM f_editar_tab01 .
  DATA: lt_rows  TYPE lvc_t_row WITH HEADER LINE,
        lv_lines TYPE i.

  CLEAR: gv_edit,
         zppe_notif_addreg.

  CALL METHOD gr_grid01->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[].

  SORT lt_rows BY index DESCENDING.
  DESCRIBE TABLE lt_rows LINES lv_lines.

  IF lv_lines NE 1.
    MESSAGE TEXT-028 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  READ TABLE gt_ordenes ASSIGNING <fs_ordenes> INDEX lt_rows-index.

  "Orden
  zppe_notif_addreg-aufnr = <fs_ordenes>-aufnr.

  "Operación
  zppe_notif_addreg-vornr = <fs_ordenes>-vornr.

  "Clave Modelo
  zppe_notif_addreg-ktsch = <fs_ordenes>-ktsch.

  "Fecha contabilización
  zppe_notif_addreg-budat = <fs_ordenes>-budat.

  "Secuencia
  zppe_notif_addreg-aplfl = <fs_ordenes>-plnfl.

  "Equipo
  zppe_notif_addreg-equnr = <fs_ordenes>-zzequipo.

  "Lote
  zppe_notif_addreg-charg = <fs_ordenes>-zzlote.

  "Material
  zppe_notif_addreg-plnbez = <fs_ordenes>-plnbez.

  "Descripción
  zppe_notif_addreg-maktx = <fs_ordenes>-maktx.

  "Cantidad
  zppe_notif_addreg-lmnga = <fs_ordenes>-lmnga.

  "Tipo de Notificación
  zppe_notif_addreg-zztipo_notif = <fs_ordenes>-zztipo_notif.

  "Código de Lanzada
  zppe_notif_addreg-zzcod_lanza = <fs_ordenes>-zzcod_lanza.

  "Lote de Carga
  zppe_notif_addreg-lote_carga = <fs_ordenes>-zzlote_carga.

  "Defecto
  zppe_notif_addreg-zzdefecto = <fs_ordenes>-zzdefecto.

  "Código Defecto
  zppe_notif_addreg-zzdefcod = <fs_ordenes>-zzdefcod.

  "Origen
  zppe_notif_addreg-zzorigen = <fs_ordenes>-zzorigen.

  "Código Origen
  zppe_notif_addreg-zzorcod = <fs_ordenes>-zzorcod.

  "Causa
  zppe_notif_addreg-zzcausa = <fs_ordenes>-zzcausa.

  "Código causa
  zppe_notif_addreg-zzcaucod = <fs_ordenes>-zzcaucod.

  "Destino
  zppe_notif_addreg-zzdestino = <fs_ordenes>-zzdestino.

  "Causa de Desviación
  zppe_notif_addreg-grund = <fs_ordenes>-grund.

  "Observaciones
  zppe_notif_addreg-ltxa1 = <fs_ordenes>-ltxa1.

  "Número de Rack
  zppe_notif_addreg-nro_rack = <fs_ordenes>-nro_rack.

  "Número de Caja
  zppe_notif_addreg-nro_caja = <fs_ordenes>-nro_caja.

  "Número molde
  zppe_notif_addreg-nro_molde = <fs_ordenes>-nro_molde.

  "Cámara de Horno
  zppe_notif_addreg-cam_horno = <fs_ordenes>-cam_horno.

  "Sin consumo de materiales
  zppe_notif_addreg-no_material = <fs_ordenes>-no_material.

  "Sin consumo de actividades
  zppe_notif_addreg-no_actividad = <fs_ordenes>-no_actividad.

  "Anular con rechazo
  zppe_notif_addreg-anul_recha = <fs_ordenes>-anul_recha.

  gv_edit = <fs_ordenes>-nro.
  CALL SCREEN 0200 STARTING AT 5   1.
  CLEAR: gv_edit.

  CALL METHOD gr_grid01->set_frontend_layout
    EXPORTING
      is_layout = gwa_layout_tab01.
  CALL METHOD gr_grid01->refresh_table_display
    EXPORTING
      is_stable = gc_stable.
  CALL METHOD gr_grid02->refresh_table_display
    EXPORTING
      is_stable = gc_stable.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EDIT_TAB01
*&---------------------------------------------------------------------*
FORM f_edit_tab01 CHANGING p_error.
  DATA: lt_vornr           TYPE tt_vornr           WITH HEADER LINE,
        lt_allocvaluesnum  TYPE tt_allocvaluesnum  WITH HEADER LINE,
        lt_allocvalueschar TYPE tt_allocvalueschar WITH HEADER LINE,
        lt_allocvaluescurr TYPE tt_allocvaluescurr WITH HEADER LINE,
        lt_return          TYPE tt_return          WITH HEADER LINE,
        lv_steus           TYPE steus,
        lv_txt             TYPE string,
        lv_batch           TYPE charg_d,
        lv_equnr           TYPE equnr,
        lv_objectkey       TYPE objnum,
        lv_ok              TYPE c.

  CLEAR: lv_steus,
         lv_txt,
         lv_batch.

  lv_txt = TEXT-008.

  IF zppe_notif_addreg-aufnr IS INITIAL.
    REPLACE '&' IN lv_txt WITH TEXT-078."'Orden de Fabricación'.
    MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  IF zppe_notif_addreg-budat IS INITIAL.
    MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ELSEIF zppe_notif_addreg-lmnga        IS INITIAL AND
         zppe_notif_addreg-zztipo_notif NOT IN gr_tipnot_anali.
    REPLACE '&' IN lv_txt WITH TEXT-069. "'Cantidad'.
    MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  IF zppe_notif_addreg-grund IS NOT INITIAL.
    SELECT SINGLE COUNT(*)
      FROM trug
      WHERE werks EQ p_werks
        AND grund EQ zppe_notif_addreg-grund.
    IF sy-subrc NE 0.
      REPLACE '&' IN lv_txt WITH TEXT-070. "'Causa de Desviación'.
      MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  "Validar equipo
  IF zppe_notif_addreg-equnr IS NOT INITIAL.
    SELECT SINGLE COUNT(*)
      FROM crhd INNER JOIN iloa ON crhd~objid EQ iloa~ppsid AND
                                   crhd~werks EQ iloa~swerk
                INNER JOIN equz ON iloa~iloan EQ equz~iloan
                INNER JOIN eqkt ON equz~equnr EQ eqkt~equnr
      WHERE crhd~arbpl EQ p_arbpl
        AND crhd~werks EQ p_werks
        AND equz~equnr EQ zppe_notif_addreg-equnr.
    IF sy-subrc NE 0.
      MESSAGE TEXT-017 TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  "Validar orden liberada
  SELECT SINGLE COUNT(*)
    FROM aufk INNER JOIN jest ON aufk~objnr EQ jest~objnr
    WHERE aufk~aufnr EQ zppe_notif_addreg-aufnr
      AND jest~stat  EQ 'I0002'
      AND jest~inact EQ space.
  IF sy-subrc NE 0.
    lv_txt = TEXT-021.
    REPLACE '&' IN lv_txt WITH zppe_notif_addreg-aufnr.
    MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  "Validar estado de lote
  PERFORM f_validar_estado_lote CHANGING p_error.
  CHECK p_error IS INITIAL.

  SELECT SINGLE afvc~steus
    INTO lv_steus
    FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
              INNER JOIN afpo ON afko~aufnr EQ afpo~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN crhd ON afvc~arbid EQ crhd~objid
    WHERE afko~aufnr EQ zppe_notif_addreg-aufnr
      AND crhd~arbpl EQ p_arbpl
      AND crhd~werks EQ p_werks
      AND afvc~vornr EQ zppe_notif_addreg-vornr.


  "Validar rack/caja
  IF gv_raca_oblig                  EQ '1'             AND
     zppe_notif_addreg-zztipo_notif IN gr_tipnot_aprob.
    REFRESH: lt_vornr.

    SELECT afvc~vornr
      INTO TABLE lt_vornr
      FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
                INNER JOIN crhd ON afvc~arbid EQ crhd~objid
      WHERE aufk~aufnr EQ zppe_notif_addreg-aufnr
        AND crhd~arbpl EQ p_arbpl
        AND crhd~werks EQ p_werks.

    SORT lt_vornr BY vornr ASCENDING.
    READ TABLE lt_vornr INDEX 1.

    IF lt_vornr-vornr NE zppe_notif_addreg-vornr.

      IF zppe_notif_addreg-nro_rack IS INITIAL AND
         zppe_notif_addreg-nro_caja IS INITIAL.
        MESSAGE TEXT-032 TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
        EXIT.
      ELSEIF zppe_notif_addreg-nro_rack IS NOT INITIAL AND
             zppe_notif_addreg-nro_caja IS NOT INITIAL.
        MESSAGE TEXT-033 TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  "Bloqueo de notificación por horno
  IF gv_valida_eq_mat         EQ '1'         AND
     zppe_notif_addreg-equnr IS NOT INITIAL.
    CLEAR:   lv_objectkey,
             lv_ok,
             lv_equnr.
    REFRESH: lt_allocvaluesnum,
             lt_allocvalueschar,
             lt_allocvaluescurr,
             lt_return.

    SELECT SINGLE matnr
      INTO lv_objectkey
      FROM afpo
      WHERE aufnr EQ zppe_notif_addreg-aufnr.

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_objectkey
        objecttable     = 'MARA'
        classnum        = 'Z_PROCESSOS'
        classtype       = '001'
      TABLES
        allocvaluesnum  = lt_allocvaluesnum[]
        allocvalueschar = lt_allocvalueschar[]
        allocvaluescurr = lt_allocvaluescurr[]
        return          = lt_return[].

    LOOP AT lt_allocvalueschar WHERE charact EQ 'Z_FORNO1'
                                 OR  charact EQ 'Z_FORNO_HAB'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lt_allocvalueschar-value_neutral
        IMPORTING
          output = lv_equnr.

      IF zppe_notif_addreg-equnr EQ lv_equnr.
        lv_ok = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      p_error = 'X'.
      MESSAGE TEXT-055 TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF lv_ok IS INITIAL.
      p_error = 'X'.
      MESSAGE TEXT-056 TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  "Orden
  <fs_ordenes>-aufnr = zppe_notif_addreg-aufnr.

  "Operación
  <fs_ordenes>-vornr = zppe_notif_addreg-vornr.

  "Clave Modelo
  <fs_ordenes>-ktsch = zppe_notif_addreg-ktsch.

  "Fecha contabilización
  <fs_ordenes>-budat = zppe_notif_addreg-budat.

  "Secuencia
  <fs_ordenes>-plnfl = zppe_notif_addreg-aplfl.

  "Equipo
  <fs_ordenes>-zzequipo = zppe_notif_addreg-equnr.

  "Lote
  <fs_ordenes>-zzlote = zppe_notif_addreg-charg.
  IF gv_hereda_lote EQ '1' AND
     lv_steus       EQ 'ZP03'.
    lv_batch = gwa_ordenes-zzlote.
  ENDIF.

  "Material
  <fs_ordenes>-plnbez = zppe_notif_addreg-plnbez.

  "Descripción
  <fs_ordenes>-maktx = zppe_notif_addreg-maktx.

  "Cantidad
  IF zppe_notif_addreg-zztipo_notif IN gr_tipnot_anali.
    <fs_ordenes>-lmnga = 0.
  ELSE.
    <fs_ordenes>-lmnga = zppe_notif_addreg-lmnga.
  ENDIF.

  "Tipo de Notificación
  <fs_ordenes>-zztipo_notif = zppe_notif_addreg-zztipo_notif.

  "Código de Lanzada
  <fs_ordenes>-zzcod_lanza = zppe_notif_addreg-zzcod_lanza.

  "Lote de Carga
  <fs_ordenes>-zzlote_carga = zppe_notif_addreg-lote_carga.

  "Defecto
  <fs_ordenes>-zzdefecto = zppe_notif_addreg-zzdefecto.

  "Código Defecto
  <fs_ordenes>-zzdefcod = zppe_notif_addreg-zzdefcod.

  "Origen
  <fs_ordenes>-zzorigen = zppe_notif_addreg-zzorigen.

  "Código Origen
  <fs_ordenes>-zzorcod = zppe_notif_addreg-zzorcod.

  "Causa
  <fs_ordenes>-zzcausa = zppe_notif_addreg-zzcausa.

  "Código causa
  <fs_ordenes>-zzcaucod = zppe_notif_addreg-zzcaucod.

  "Destino
  <fs_ordenes>-zzdestino = zppe_notif_addreg-zzdestino.

  "Causa de Desviación
  <fs_ordenes>-grund = zppe_notif_addreg-grund.

  "Observaciones
  <fs_ordenes>-ltxa1 = zppe_notif_addreg-ltxa1.

  "Número de Rack
  <fs_ordenes>-nro_rack = zppe_notif_addreg-nro_rack.

  "Número de Caja
  <fs_ordenes>-nro_caja = zppe_notif_addreg-nro_caja.

  "Número molde
  <fs_ordenes>-nro_molde = zppe_notif_addreg-nro_molde.

  "Cámara de Horno
  <fs_ordenes>-cam_horno = zppe_notif_addreg-cam_horno.

  "Sin consumo de materiales
  <fs_ordenes>-no_material = zppe_notif_addreg-no_material.

  "Sin consumo de actividades
  <fs_ordenes>-no_actividad = zppe_notif_addreg-no_actividad.

  "Anular con rechazo
  <fs_ordenes>-anul_recha = zppe_notif_addreg-anul_recha.

  DELETE gt_componentes WHERE nro EQ gv_edit.

  IF zppe_notif_addreg-zztipo_notif NOT IN gr_tipnot_anali AND
     <fs_ordenes>-no_material       IS INITIAL.
    PERFORM f_completar_componente USING gv_edit
                                         <fs_ordenes>-aufnr
                                         <fs_ordenes>-vornr
                                         <fs_ordenes>-plnfl
                                         <fs_ordenes>-ktsch
                                         <fs_ordenes>-zztipo_notif
                                         1"gwa_ordenes-lmnga
                                         lv_batch.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CORREGIR_COGI
*&---------------------------------------------------------------------*
FORM f_corregir_cogi USING p_aufnr.
  DATA: lwa_affw  TYPE affw,
        lv_varkey TYPE vim_enqkey,
        lv_opt    TYPE ctu_params.

  REFRESH: gt_bdcdata.

  SELECT SINGLE *
    INTO lwa_affw
    FROM affw
    WHERE aufnr EQ p_aufnr.
  CHECK sy-subrc EQ 0.

*  lv_varkey = lwa_affw-matnr.
*  CALL FUNCTION 'DEQUEUE_E_TABLE'
*    EXPORTING
*      tabname = 'MARA'
*      varkey  = lv_varkey.
*
*  CONCATENATE lwa_affw-matnr
*              lwa_affw-charg
*              INTO lv_varkey.
*  CALL FUNCTION 'DEQUEUE_E_TABLE'
*    EXPORTING
*      tabname = 'MCH1'
*      varkey  = lv_varkey.

  CALL FUNCTION 'VB_DEQUEUE_BATCH'
    EXPORTING
      iv_matnr    = lwa_affw-matnr
      iv_charg    = lwa_affw-charg
      iv_werks    = lwa_affw-werks
    EXCEPTIONS
      no_material = 1
      no_batch    = 2
      no_plant    = 3
      OTHERS      = 4.
  CHECK sy-subrc EQ 0.

  lv_opt-nobinpt = 'X'.
  lv_opt-dismode = 'N'.

  PERFORM f_fill_bi
    USING:
      'X'    'CORUAFFW'     '1000',
      space  'BDC_OKCODE'   '=ONLI',
      space  'S_AUFNR-LOW'  p_aufnr,

      'X'    'SAPLSLVC_FULLSCREEN'  '0500',
      space  'BDC_OKCODE'           '=&ALL',

      'X'    'SAPLSLVC_FULLSCREEN'  '0500',
      space  'BDC_OKCODE'           '=BU'.

  CALL TRANSACTION 'COGI' USING gt_bdcdata
                          OPTIONS FROM lv_opt.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_BI
*&---------------------------------------------------------------------*
FORM f_fill_bi USING p_dynbegin
                     p_field1
                     p_field2.
  DATA: lwa_bdcdata TYPE bdcdata.

  CLEAR: lwa_bdcdata.

  CASE p_dynbegin.
    WHEN 'X'.
      lwa_bdcdata-dynbegin = 'X'.
      lwa_bdcdata-program  = p_field1.
      lwa_bdcdata-dynpro   = p_field2.
    WHEN space.
      lwa_bdcdata-fnam     = p_field1.
      lwa_bdcdata-fval     = p_field2.
  ENDCASE.

  APPEND lwa_bdcdata TO gt_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ACTUALIZA_LOTE_ZHAL
*&---------------------------------------------------------------------*
FORM f_actualiza_lote_zhal CHANGING p_error.
  DATA: lt_allocvaluesnum  TYPE tt_allocvaluesnum  WITH HEADER LINE,
        lt_allocvalueschar TYPE tt_allocvalueschar WITH HEADER LINE,
        lt_allocvaluescurr TYPE tt_allocvaluescurr WITH HEADER LINE,
        lt_return          TYPE tt_return          WITH HEADER LINE,
        lt_steus           TYPE tt_steus           WITH HEADER LINE,
        lv_objectkey       TYPE objnum,
        lv_material        TYPE matnr18,
        lv_batch           TYPE charg_d,
        lv_werks           TYPE werks_d,
        lv_cont            TYPE i,
        lv_text            TYPE string.

  CLEAR: p_error.

  READ TABLE gt_lote_auto INTO gwa_lote_auto WITH KEY value = '08'.
  CHECK sy-subrc EQ 0.

  READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY zzlote = ''.
  CHECK sy-subrc EQ 0.

  CLEAR:   lv_objectkey,
           lv_batch.
  REFRESH: lt_allocvalueschar,
           lt_steus.

  READ TABLE gt_ordenes INTO gwa_ordenes INDEX 1.
  lv_objectkey = gwa_ordenes-plnbez.

  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey       = lv_objectkey
      objecttable     = 'MARA'
      classnum        = 'Z_FOR_PIEZAS'
      classtype       = '001'
    TABLES
      allocvaluesnum  = lt_allocvaluesnum[]
      allocvalueschar = lt_allocvalueschar[]
      allocvaluescurr = lt_allocvaluescurr[]
      return          = lt_return[].

  READ TABLE lt_allocvalueschar WITH KEY charact = 'Z_GEOMETRY_TYPE'.
  CHECK lt_allocvalueschar-value_neutral EQ '01'.

  SELECT SINGLE werks
    INTO lv_werks
    FROM aufk
    WHERE aufnr EQ gwa_ordenes-aufnr.

  lv_material = gwa_ordenes-plnbez.

  CLEAR: lv_cont.

  DO.
    REFRESH: lt_return.

    CALL FUNCTION 'BAPI_BATCH_CREATE'
      EXPORTING
        material = lv_material
        plant    = lv_werks
      IMPORTING
        batch    = lv_batch
      TABLES
        return   = lt_return.

    READ TABLE lt_return WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      ADD 1 TO lv_cont.

      IF lv_cont GT 300.
        lv_text = TEXT-045.
        REPLACE '&' IN lv_text WITH lt_return-message.

        MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.

        p_error = 'X'.
        EXIT.
      ENDIF.
      WAIT UP TO 1 SECONDS.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  CHECK p_error IS INITIAL.

  SELECT  afko~aufnr
          afvc~vornr
          afvc~steus
    INTO TABLE lt_steus
    FROM afko INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN crhd ON afvc~arbid EQ crhd~objid
    FOR ALL ENTRIES IN gt_ordenes
    WHERE afko~aufnr EQ gt_ordenes-aufnr
      AND afvc~vornr EQ gt_ordenes-vornr
      AND crhd~arbpl EQ p_arbpl
      AND crhd~werks EQ p_werks.

  LOOP AT gt_ordenes ASSIGNING <fs_ordenes> WHERE zztipo_notif IN gr_tipnot_aprob
                                              AND zzlote       IS INITIAL.
    <fs_ordenes>-zzlote = lv_batch.

    READ TABLE lt_steus WITH KEY aufnr = <fs_ordenes>-aufnr
                                 vornr = <fs_ordenes>-vornr.
    IF gv_hereda_lote EQ '1' AND
       lt_steus-steus EQ 'ZP03'.
      LOOP AT gt_componentes ASSIGNING <fs_componentes> WHERE nro EQ <fs_ordenes>-nro.
        <fs_componentes>-lote = lv_batch.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_IMPRESION_AUTOMATICA_UNI
*&---------------------------------------------------------------------*
FORM f_impresion_automatica_uni USING p_ordenes TYPE ty_ordenes.
  DATA: lt_vornr    TYPE tt_vornr WITH HEADER LINE,
        lwa_param   TYPE zwpp_param_etiquetas,
        lwa_vidrios TYPE zwpp_vidrios,
        lv_numform  TYPE numc4,
        lv_lines    TYPE i.

  CHECK gt_print_form IS NOT INITIAL.

  REFRESH: lt_vornr.

  SELECT afvc~vornr
    INTO TABLE lt_vornr
    FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN crhd ON afvc~arbid EQ crhd~objid
    WHERE aufk~aufnr EQ p_ordenes-aufnr
      AND crhd~arbpl EQ p_arbpl
      AND crhd~werks EQ p_werks.

  "ZPPSF_CONTROL_CALIDAD
  READ TABLE gt_print_form INTO gwa_print_form WITH KEY val = '0001'.
  IF sy-subrc               EQ 0         AND
     p_ordenes-zztipo_notif EQ gv_aprob.
    CLEAR: lwa_param,
           lv_numform.

    SORT lt_vornr BY vornr DESCENDING.
    READ TABLE lt_vornr INDEX 1.
    IF lt_vornr-vornr EQ p_ordenes-vornr.
      lwa_param-impresora = gv_impresora.
      lwa_param-aufnr     = p_ordenes-aufnr.

      lv_numform = gwa_print_form-val.

      CALL METHOD zcl_pp_formularios=>mostrar_smartform
        EXPORTING
          iw_param_etiqueta = lwa_param
          i_numform         = lv_numform.
    ENDIF.
  ENDIF.

  "ZPPSF_CONTROL_CALIDAD
  READ TABLE gt_print_form INTO gwa_print_form WITH KEY val = '0002'.
  IF sy-subrc               EQ 0        AND
     p_ordenes-zztipo_notif EQ gv_aprob.
    CLEAR: lwa_param,
           lv_numform.

    SORT lt_vornr BY vornr DESCENDING.
    READ TABLE lt_vornr INDEX 1.
    IF lt_vornr-vornr EQ p_ordenes-vornr.
      lwa_param-impresora = gv_impresora.
      lwa_param-aufnr     = p_ordenes-aufnr.

      lv_numform = gwa_print_form-val.

      CALL METHOD zcl_pp_formularios=>mostrar_smartform
        EXPORTING
          iw_param_etiqueta = lwa_param
          i_numform         = lv_numform.
    ENDIF.
  ENDIF.

  "ZPPSF_CONTROL_CALIDAD_BR
  READ TABLE gt_print_form INTO gwa_print_form WITH KEY val = '0006'.
  IF sy-subrc               EQ 0        AND
     p_ordenes-zztipo_notif EQ gv_aprob.
    CLEAR: lwa_param,
           lv_numform.

    SORT lt_vornr BY vornr DESCENDING.
    READ TABLE lt_vornr INDEX 1.
    IF lt_vornr-vornr EQ p_ordenes-vornr.
      lwa_param-impresora = gv_impresora.
      lwa_param-aufnr     = p_ordenes-aufnr.

      lv_numform = gwa_print_form-val.

      CALL METHOD zcl_pp_formularios=>mostrar_smartform
        EXPORTING
          iw_param_etiqueta = lwa_param
          i_numform         = lv_numform.
    ENDIF.
  ENDIF.

  "ZPPSF_HERRAMENTALES
  READ TABLE gt_print_form INTO gwa_print_form WITH KEY val = '0017'.
  IF sy-subrc               EQ 0        AND
     p_ordenes-zztipo_notif EQ gv_aprob.
    CLEAR: lwa_param,
           lv_numform.

    SELECT SINGLE COUNT(*)
      FROM mara
      WHERE matnr EQ p_ordenes-plnbez
        AND mtart EQ 'ZHAL'.
    IF sy-subrc EQ 0.
      lwa_param-impresora = gv_impresora.
      lwa_param-aufnr     = p_ordenes-aufnr.

      lv_numform = gwa_print_form-val.

      CALL METHOD zcl_pp_formularios=>mostrar_smartform
        EXPORTING
          iw_param_etiqueta = lwa_param
          i_numform         = lv_numform.
    ENDIF.
  ENDIF.

  "ZPPSF_PACKING_LIST
  READ TABLE gt_print_form INTO gwa_print_form WITH KEY val = '0004'.
  IF sy-subrc               EQ 0               AND
     p_ordenes-zztipo_notif IN gr_tipnot_aprob.
    CLEAR: lwa_param,
           lv_numform.

    SORT lt_vornr BY vornr DESCENDING.
    READ TABLE lt_vornr INDEX 1.
    IF lt_vornr-vornr EQ p_ordenes-vornr.
      lwa_param-impresora = gv_impresora.
      lwa_param-aufnr     = p_ordenes-aufnr.
      lwa_param-werks     = p_werks.
      lwa_param-arbpl     = p_arbpl.
      lwa_param-molde     = p_ordenes-nro_molde.
      lwa_param-charg     = p_ordenes-zzlote.

      lv_numform = gwa_print_form-val.

      CALL METHOD zcl_pp_formularios=>mostrar_smartform
        EXPORTING
          iw_param_etiqueta = lwa_param
          i_numform         = lv_numform.
    ENDIF.
  ENDIF.

  "ZPPSF_NOTIFICACION_QR
  READ TABLE gt_print_form INTO gwa_print_form WITH KEY val = '0010'.
  IF sy-subrc               EQ 0               AND
     p_ordenes-zztipo_notif IN gr_tipnot_aprob.
    CLEAR: lwa_param,
           lv_numform,
           lv_lines.

    SORT lt_vornr BY vornr ASCENDING.
    READ TABLE lt_vornr INDEX 1.
    IF lt_vornr-vornr EQ p_ordenes-vornr.
      DESCRIBE TABLE gt_ordenes LINES lv_lines.

      lwa_param-impresora   = gv_impresora.
      lwa_param-aufnr       = p_ordenes-aufnr.
      lwa_param-nvidrio     = p_ordenes-zzlote.
      lwa_param-budat       = p_ordenes-budat.

      lv_numform = gwa_print_form-val.

      CALL METHOD zcl_pp_formularios=>mostrar_smartform
        EXPORTING
          iw_param_etiqueta = lwa_param
          i_numform         = lv_numform.
    ENDIF.
  ENDIF.


  "ZPPSF_PACKING_LIST_20
  READ TABLE gt_print_form INTO gwa_print_form WITH KEY val = '0020'.
  IF sy-subrc               EQ 0               AND
     p_ordenes-zztipo_notif IN gr_tipnot_aprob.
    CLEAR: lwa_param,
           lv_numform.

    SORT lt_vornr BY vornr DESCENDING.
    READ TABLE lt_vornr INDEX 1.
    IF lt_vornr-vornr EQ p_ordenes-vornr.
      lwa_param-impresora = gv_impresora.
      lwa_param-aufnr     = p_ordenes-aufnr.
      lwa_param-werks     = p_werks.
      lwa_param-arbpl     = p_arbpl.
      lwa_param-molde     = p_ordenes-nro_molde.
      lwa_param-charg     = p_ordenes-zzlote.

      lv_numform = gwa_print_form-val.

      CALL METHOD zcl_pp_formularios=>mostrar_smartform
        EXPORTING
          iw_param_etiqueta = lwa_param
          i_numform         = lv_numform.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_IMPRESION_AUTOMATICA_MAS
*&---------------------------------------------------------------------*
FORM f_impresion_automatica_mas .
  DATA: lt_vornr    TYPE tt_vornr WITH HEADER LINE,
        lwa_param   TYPE zwpp_param_etiquetas,
        lwa_ordenes TYPE aufnr_s,
        lwa_vidrios TYPE zwpp_vidrios,
        lv_numform  TYPE numc4,
        lv_lines    TYPE i,
        lv_ok       TYPE c.

  CLEAR: lv_ok.

  LOOP AT gt_log INTO gwa_log WHERE notif EQ TEXT-067. "'Sí'.
    READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY nro          = gwa_log-nro
                                                    zztipo_notif = gv_aprob.
    IF sy-subrc EQ 0.
      lv_ok = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  CHECK lv_ok         EQ 'X'         AND
        gt_print_form IS NOT INITIAL.

*  READ TABLE gt_log INTO gwa_log WITH KEY notif = 'Sí'.
*  CHECK sy-subrc      EQ 0           AND
*        gt_print_form IS NOT INITIAL.

  REFRESH: lt_vornr.

  READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY nro = gwa_log-nro.

  SELECT afvc~vornr
    INTO TABLE lt_vornr
    FROM aufk INNER JOIN afko ON aufk~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN crhd ON afvc~arbid EQ crhd~objid
    WHERE aufk~aufnr EQ gwa_ordenes-aufnr
      AND crhd~arbpl EQ p_arbpl
      AND crhd~werks EQ p_werks.

  "ZPPSF_LOF_AUTOCLAVE
  READ TABLE gt_print_form INTO gwa_print_form WITH KEY val = '0014'.
  IF sy-subrc EQ 0.
    CLEAR:   lwa_param,
             lv_numform.

    READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY nro = gwa_log-nro.

    SORT lt_vornr BY vornr ASCENDING.
    READ TABLE lt_vornr INDEX 1.
    IF lt_vornr-vornr EQ gwa_ordenes-vornr.
      lwa_param-impresora = gv_impresora.
      lwa_param-werks     = p_werks.
      lwa_param-equnr     = gwa_ordenes-zzequipo.
      IF gwa_ordenes-zzcod_lanza IS NOT INITIAL.
        lwa_param-charg = gwa_ordenes-zzcod_lanza.
      ELSE.
        lwa_param-charg = gwa_ordenes-zzciclo_auto.
      ENDIF.

      LOOP AT gt_ordenes INTO gwa_ordenes WHERE zztipo_notif EQ gv_aprob.
        READ TABLE gt_log INTO gwa_log WITH KEY nro   = gwa_ordenes-nro
                                                notif = TEXT-067. "'Sí'.
        IF sy-subrc EQ 0.
          CLEAR: lwa_ordenes.
          lwa_ordenes-aufnr = gwa_ordenes-aufnr.
          APPEND lwa_ordenes TO lwa_param-ordenes.
        ENDIF.
      ENDLOOP.
      lv_numform = gwa_print_form-val.

      CALL METHOD zcl_pp_formularios=>mostrar_smartform
        EXPORTING
          iw_param_etiqueta = lwa_param
          i_numform         = lv_numform.
    ENDIF.
  ENDIF.

  "ZPPSF_EMPALME_TQ
  READ TABLE gt_print_form INTO gwa_print_form WITH KEY val = '0005'.
  IF sy-subrc EQ 0.
    CLEAR: lwa_param,
           lv_numform,
           lv_lines.

    READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY nro = gwa_log-nro.

    SORT lt_vornr BY vornr DESCENDING.
    READ TABLE lt_vornr INDEX 1.
    IF lt_vornr-vornr EQ gwa_ordenes-vornr.

      lwa_param-impresora   = gv_impresora.
      lwa_param-werks       = p_werks.
      lwa_param-arbpl       = p_arbpl.

      LOOP AT gt_log INTO gwa_log WHERE notif EQ TEXT-067. "'Sí'.
        READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY nro = gwa_log-nro.
        IF sy-subrc                 EQ 0               AND
           gwa_ordenes-zztipo_notif IN gr_tipnot_aprob.
          IF lwa_param-aufnr IS INITIAL.
            lwa_param-aufnr       = gwa_ordenes-aufnr.
            lwa_param-charg       = gwa_ordenes-zzciclo_tq.
            lwa_param-observacion = gwa_ordenes-ltxa1.
          ENDIF.

          ADD 1 TO lwa_param-cantidad.
        ENDIF.
      ENDLOOP.

      lv_numform = gwa_print_form-val.

      CALL METHOD zcl_pp_formularios=>mostrar_smartform
        EXPORTING
          iw_param_etiqueta = lwa_param
          i_numform         = lv_numform.
    ENDIF.
  ENDIF.

  "ZPPSF_DESPACHO_VIDRIOS_AVO
  READ TABLE gt_print_form INTO gwa_print_form WITH KEY val = '0009'.
  IF sy-subrc EQ 0.
    CLEAR: lwa_param,
           lv_numform,
           lv_lines.

    READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY nro = gwa_log-nro.

    SORT lt_vornr BY vornr DESCENDING.
    READ TABLE lt_vornr INDEX 1.
    IF lt_vornr-vornr EQ gwa_ordenes-vornr.

*      lwa_param-impresora   = gv_impresora.
      lwa_param-impresora   = gv_impresora2.
      lwa_param-werks       = p_werks.
      lwa_param-arbpl       = p_arbpl.

      LOOP AT gt_ordenes INTO gwa_ordenes WHERE zztipo_notif IN gr_tipnot_aprob.
        READ TABLE gt_log INTO gwa_log WITH KEY nro   = gwa_ordenes-nro
                                                notif = TEXT-067. "'Sí'.
        IF sy-subrc EQ 0.
          IF lwa_param-aufnr IS INITIAL.
            lwa_param-aufnr = gwa_ordenes-aufnr.
            IF gwa_ordenes-nro_rack IS NOT INITIAL.
              lwa_param-rack  = gwa_ordenes-nro_rack.
            ELSE.
              lwa_param-rack  = gwa_ordenes-nro_caja.
            ENDIF.
          ENDIF.

          CLEAR: lwa_vidrios.

          ADD 1 TO lwa_param-ttpiezas.

          lwa_vidrios-vidrio = gwa_ordenes-zzlote.

          APPEND lwa_vidrios TO lwa_param-charg_vidrios.
        ENDIF.
      ENDLOOP.
      lv_numform = gwa_print_form-val.

      CALL METHOD zcl_pp_formularios=>mostrar_smartform
        EXPORTING
          iw_param_etiqueta = lwa_param
          i_numform         = lv_numform.
    ENDIF.
  ENDIF.

  "ZPPSF_PACKING_LIST_VIDRIOS
  READ TABLE gt_print_form INTO gwa_print_form WITH KEY val = '0008'.
  IF sy-subrc EQ 0.
    CLEAR: lwa_param,
           lv_numform,
           lv_lines.

    SORT lt_vornr BY vornr DESCENDING.
    READ TABLE lt_vornr INDEX 1.

    lwa_param-impresora   = gv_impresora.

    LOOP AT gt_ordenes INTO gwa_ordenes WHERE zztipo_notif IN gr_tipnot_aprob
                                          AND vornr        EQ lt_vornr-vornr.
      READ TABLE gt_log INTO gwa_log WITH KEY nro   = gwa_ordenes-nro
                                              notif = TEXT-067. "'Sí'.
      IF sy-subrc EQ 0.
        IF lwa_param-aufnr IS INITIAL.
          lwa_param-aufnr = gwa_ordenes-aufnr.
          lwa_param-ncaja = gwa_ordenes-nro_caja.
        ENDIF.

        CLEAR: lwa_vidrios.

*        ADD 1 TO lwa_param-ttpiezas.

        lwa_vidrios-vidrio = gwa_ordenes-zzlote.

        APPEND lwa_vidrios TO lwa_param-charg_vidrios.
      ENDIF.
    ENDLOOP.
    lv_numform = gwa_print_form-val.

    IF lwa_param-charg_vidrios IS NOT INITIAL.
      CALL METHOD zcl_pp_formularios=>mostrar_smartform
        EXPORTING
          iw_param_etiqueta = lwa_param
          i_numform         = lv_numform.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ACTUALIZA_FALCON3
*&---------------------------------------------------------------------*
FORM f_actualiza_falcon3 CHANGING p_error.
  DATA: lt_aufk     TYPE tt_aufk   WITH HEADER LINE,
        lt_return   TYPE tt_return WITH HEADER LINE,
        lv_material TYPE matnr18,
        lv_cont     TYPE i,
        lv_val      TYPE c,
        lv_text     TYPE string.

  CLEAR:   lv_val,
           p_error.
  REFRESH: lt_aufk.

  READ TABLE gt_lote_auto INTO gwa_lote_auto WITH KEY value = '09'.
  CHECK sy-subrc EQ 0.

  SELECT *
    INTO TABLE lt_aufk
    FROM aufk
    FOR ALL ENTRIES IN gt_ordenes
    WHERE aufnr EQ gt_ordenes-aufnr.

  LOOP AT gt_ordenes ASSIGNING <fs_ordenes> WHERE zzlote       IS INITIAL
                                              AND zztipo_notif IN gr_tipnot_aprob.
    CLEAR: lt_aufk.

    READ TABLE lt_aufk WITH KEY aufnr = <fs_ordenes>-aufnr.

    lv_material = <fs_ordenes>-plnbez.
    IF lv_val IS INITIAL.
      lv_val = 'X'.

      DO.
        REFRESH: lt_return.

        CALL FUNCTION 'BAPI_BATCH_CREATE'
          EXPORTING
            material = lv_material
            plant    = lt_aufk-werks
          IMPORTING
            batch    = <fs_ordenes>-zzlote
          TABLES
            return   = lt_return.

        READ TABLE lt_return WITH KEY type = 'E'.
        IF sy-subrc EQ 0.
          ADD 1 TO lv_cont.

          IF lv_cont GT 300.
            lv_text = TEXT-045.
            REPLACE '&' IN lv_text WITH lt_return-message.

            MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.

            p_error = 'X'.
            EXIT.
          ENDIF.
          WAIT UP TO 1 SECONDS.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
      CHECK p_error IS INITIAL.
    ELSE.
      CALL FUNCTION 'BAPI_BATCH_CREATE'
        EXPORTING
          material = lv_material
          plant    = lt_aufk-werks
        IMPORTING
          batch    = <fs_ordenes>-zzlote
        TABLES
          return   = lt_return.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EDITAR_TAB02
*&---------------------------------------------------------------------*
FORM f_editar_tab02 .
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE.

  CLEAR: gwa_componentes.

  CALL METHOD gr_grid02->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[].

  SORT lt_rows BY index DESCENDING.
  IF lt_rows[] IS INITIAL.
    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL SCREEN 0500 STARTING AT 5 5.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EDIT_TAB02
*&---------------------------------------------------------------------*
FORM f_edit_tab02 .
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE.
  FIELD-SYMBOLS: <fs_comp> TYPE ty_componentes.

  CALL METHOD gr_grid02->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[].

*  SELECT SINGLE COUNT(*)
*    FROM mara
*    WHERE matnr EQ gwa_componentes-componente
*      AND xchpf EQ 'X'.
*  IF sy-subrc EQ 0.
*    IF gwa_componentes-lote IS INITIAL.
*      lv_text = TEXT-008.
*      REPLACE '&' IN lv_text WITH 'Lote'.
*      p_error = 'X'.
*      MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*      EXIT.
*    ELSE.
*      SELECT SINGLE COUNT(*)
*        FROM mcha
*        WHERE matnr EQ lv_material
*          AND werks EQ p_werks
*          AND charg EQ gwa_componentes-lote.
*      IF sy-subrc NE 0.
*        lv_text = TEXT-016.
*        REPLACE '&' IN lv_text WITH 'Lote'.
*        p_error = 'X'.
*        MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.
*    ENDIF.
*  ENDIF.

  LOOP AT lt_rows.
    READ TABLE gt_componentes ASSIGNING <fs_comp> INDEX lt_rows-index.
    <fs_comp>-lote = gwa_componentes-lote.
    LOOP AT gt_componentes ASSIGNING <fs_componentes> WHERE nro        EQ <fs_comp>-nro
                                                        AND componente EQ <fs_comp>-componente.
      <fs_componentes>-mod  = 'X'.
    ENDLOOP.
  ENDLOOP.

  IF sy-subrc EQ 0.
    CALL METHOD gr_grid02->refresh_table_display
      EXPORTING
        is_stable = gc_stable.

    MESSAGE TEXT-034 TYPE 'S'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_COPIAR_TAB02
*&---------------------------------------------------------------------*
FORM f_copiar_tab02 .
  DATA: lt_rows  TYPE lvc_t_row WITH HEADER LINE,
        lv_lines TYPE i.

  CALL METHOD gr_grid02->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[].

  DESCRIBE TABLE lt_rows LINES lv_lines.

  IF lv_lines NE 1.
    MESSAGE TEXT-028 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.

  READ TABLE gt_componentes INTO gwa_componentes INDEX lt_rows-index.

  CALL SCREEN 0500 STARTING AT 10 10.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_COPY_TAB02
*&---------------------------------------------------------------------*
FORM f_copy_tab02 .
  DATA: lv_aufnr TYPE aufnr,
        lv_error TYPE c.

*  IF gv_consumo_herra EQ '1'.
*    SELECT SINGLE COUNT(*)
*      FROM mara
*    WHERE matnr EQ gwa_componentes-componente
*      AND mtart EQ 'ZHER'.
*    IF sy-subrc EQ 0.
*      gwa_componentes-cantidad = 1.
*    ENDIF.
*  ENDIF.

  PERFORM f_add_tab02_valida CHANGING lv_error.
  CHECK lv_error IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gwa_componentes-aufnr
    IMPORTING
      output = gwa_componentes-aufnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gwa_componentes-componente
    IMPORTING
      output = gwa_componentes-componente.

  SELECT SINGLE maktx
    INTO gwa_componentes-descripcion
    FROM makt
    WHERE matnr EQ gwa_componentes-componente
      AND spras EQ sy-langu.

  SELECT SINGLE msehi
    INTO gwa_componentes-um
    FROM t006a
    WHERE spras EQ sy-langu
      AND mseh6 EQ gwa_componentes-um_txt.

  SELECT SINGLE COUNT(*)
    FROM mara
    WHERE matnr EQ gwa_componentes-componente
      AND xchpf EQ 'X'.

  gwa_componentes-mod = 'X'.
  gwa_componentes-new = 'X'.

  APPEND gwa_componentes TO gt_componentes.

  SORT gt_componentes BY nro        ASCENDING.

  CALL METHOD gr_grid02->refresh_table_display
    EXPORTING
      is_stable = gc_stable.

  MESSAGE TEXT-035 TYPE 'S'.
  LEAVE TO SCREEN 0.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FILL_ANULAR_RECHAZO
*&---------------------------------------------------------------------*
FORM f_fill_anular_rechazo .
  CLEAR:   gv_anular_rechazo.
  REFRESH: gt_anul.

**  IF gwa_ordenes-zzlote IS INITIAL.
*    SELECT rueck
*           rmzhl
*           aufnr
*           vornr
*           aplfl
*      INTO TABLE gt_anul
*      FROM afru
*      FOR ALL ENTRIES IN gt_ordenes
*      WHERE aufnr EQ gt_ordenes-aufnr
*        AND stokz EQ space
*        AND stzhl EQ space
*        AND vornr EQ gt_ordenes-vornr
*        AND werks EQ p_werks
*        AND lmnga GT 0
*        AND aplfl EQ gt_ordenes-plnfl.
*  ELSE.
  SELECT rueck
         rmzhl
         aufnr
         vornr
         aplfl
         zzlote
         lmnga
    INTO TABLE gt_anul
    FROM afru
    FOR ALL ENTRIES IN gt_ordenes
    WHERE aufnr  EQ gt_ordenes-aufnr
      AND stokz  EQ space
      AND stzhl  EQ space
      AND vornr  EQ gt_ordenes-vornr
      AND werks  EQ p_werks
*      AND lmnga  EQ 1 "GT 0    "-@0005
      AND lmnga  GT 0                                       "+@0005
      AND aplfl  EQ gt_ordenes-plnfl
      AND zzlote EQ gt_ordenes-zzlote.
*  ENDIF.

  SORT gt_anul BY aufnr ASCENDING
                  vornr ASCENDING
                  rueck DESCENDING
                  rmzhl DESCENDING.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SAVE_0100_V2
*&---------------------------------------------------------------------*
FORM f_save_0100_v2 .
  DATA: lt_input      TYPE zpptt_notif_mfinput   WITH HEADER LINE,
        lt_output     TYPE zpptt_notif_mfoutput  WITH HEADER LINE,
        lt_comp       TYPE zpptt_notif_component WITH HEADER LINE,
        lt_msg        TYPE zpptt_notif_rpta,
        lv_answer     TYPE c,
        lv_ok_notif   TYPE boolean,
        lv_ok_lote    TYPE c,
        lv_lmnga      TYPE ru_lmnga,
        lv_xmnga      TYPE ru_xmnga,
        lv_rmnga      TYPE ru_rmnga,
        lv_error      TYPE boolean,
        lv_error_lote TYPE string,
        lv_nro        TYPE int4,
        lv_val        TYPE c.

  CLEAR:   lv_answer,
           lv_error,
           lv_error_lote.
  REFRESH: lt_comp,
           lt_msg,
           gt_log,
           lt_input,
           lt_output.

  REFRESH: gt_ordenes_delete.                               " +@0004
  CLEAR: gv_flag_orden_not.                                 " +@0004
  PERFORM f_validar_campos CHANGING lv_error.

                                                            "{ +@0004
  IF gv_flag_orden_not EQ 'X'.
    DATA: lv_objek      TYPE inob-objek.
    CONCATENATE: p_werks p_arbpl INTO lv_objek.

    SELECT cuobj, klart FROM inob
      INTO @DATA(lwa_inob)
    UP TO 1 ROWS
    WHERE
      objek     EQ @lv_objek     AND
      klart     EQ '019'.
    ENDSELECT.

    CONCATENATE: '%' lwa_inob-cuobj INTO lv_objek.

    DATA: lv_attin      TYPE ausp-atinn.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'Z_VENTANA_NOTIF'
      IMPORTING
        output = lv_attin.
    SELECT objek, atinn, atzhl, mafid, klart, adzhl, atwrt FROM ausp
      INTO @DATA(lwa_ausp)
    UP TO 1 ROWS
    WHERE
      objek     LIKE  @lv_objek       AND
      atinn     EQ    @lv_attin.
    ENDSELECT.

    IF lwa_ausp-atwrt EQ '1'.
      DATA: ls_orden      TYPE string.
      REFRESH: gt_ordenes_notificadas.
      LOOP AT gt_ordenes_delete INTO ls_orden.
        APPEND INITIAL LINE TO gt_ordenes_notificadas ASSIGNING FIELD-SYMBOL(<lfs_notificadas>).
        <lfs_notificadas>-aufnr = ls_orden.
      ENDLOOP.

      " Asignar datos a las variables del POP UP
      txt_titulo1_0202  = TEXT-091.
      CALL SCREEN 0202 STARTING AT 30  1.
    ENDIF.
  ENDIF.
                                                            "} +@0004

  CHECK lv_error IS INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = TEXT-065 "'Notificaciones automáticas'
      text_question         = TEXT-066 "'¿Desea notificar los datos?'
      display_cancel_button = space
    IMPORTING
      answer                = lv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  CHECK lv_answer EQ 1.
*validacion de tipo de notificación Analisis
*{+@0006
  PERFORM f_valida_notif_anali CHANGING lv_error.
  CHECK lv_error IS INITIAL.
*}+@0006
  PERFORM f_actualiza_lcurvado.
  PERFORM f_actualiza_ciclo_tq.
  PERFORM f_actualiza_ciclo_tq_co.
  PERFORM f_actualiza_ciclo_auto.
  PERFORM f_actualiza_lote_zhal CHANGING lv_error.
  CHECK lv_error IS INITIAL.
  PERFORM f_actualiza_falcon3 CHANGING lv_error.
  CHECK lv_error IS INITIAL.

  PERFORM f_fill_anular_rechazo.

  LOOP AT gt_ordenes ASSIGNING <fs_ordenes>.
    CLEAR:   lv_error,
             lv_lmnga,
             lv_xmnga,
             lv_nro,
             lt_input.

    PERFORM f_anular_rechazo_v2 CHANGING lv_error.
    IF lv_error IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    PERFORM f_lote_dupli USING    <fs_ordenes>
                         CHANGING lv_error.
    IF lv_error IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    PERFORM f_genera_lote_auto CHANGING lv_error_lote
                                        lv_val.
    IF lv_error_lote IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    IF <fs_ordenes>-zztipo_notif IN gr_tipnot_aprob.
      lv_lmnga = <fs_ordenes>-lmnga.
    ELSEIF <fs_ordenes>-zztipo_notif IN gr_tipnot_recha.
      lv_xmnga = <fs_ordenes>-lmnga.
    ELSEIF <fs_ordenes>-zztipo_notif IN gr_tipnot_retra.
      lv_rmnga = <fs_ordenes>-lmnga.
    ELSEIF <fs_ordenes>-zztipo_notif IN gr_tipnot_anali.
    ENDIF.

    lt_input-nro          = <fs_ordenes>-nro.
    lt_input-aufnr        = <fs_ordenes>-aufnr.
    lt_input-vornr        = <fs_ordenes>-vornr.
    lt_input-ktsch        = <fs_ordenes>-ktsch.
    lt_input-budat        = <fs_ordenes>-budat.
    lt_input-aplfl        = <fs_ordenes>-plnfl.
    lt_input-meinh        = <fs_ordenes>-gmein.
    lt_input-lmnga        = lv_lmnga.
    lt_input-xmnga        = lv_xmnga.
    lt_input-rmnga        = lv_rmnga.
    lt_input-grund        = <fs_ordenes>-grund.
*    lt_input-TAG_ID       = .
    lt_input-lcorte       = <fs_ordenes>-zzlote_corte.
    lt_input-lcurvado     = <fs_ordenes>-zzlote_curvado.
    lt_input-ciclotq      = <fs_ordenes>-zzciclo_tq.
    lt_input-cicloauto    = <fs_ordenes>-zzciclo_auto.
*    lt_input-TEMPTERM     = .
    lt_input-defecto      = <fs_ordenes>-zzdefecto.
    lt_input-defcod       = <fs_ordenes>-zzdefcod.
    lt_input-origen       = <fs_ordenes>-zzorigen.
    lt_input-orcod        = <fs_ordenes>-zzorcod.
    lt_input-causa        = <fs_ordenes>-zzcausa.
    lt_input-caucod       = <fs_ordenes>-zzcaucod.
    lt_input-destino      = <fs_ordenes>-zzdestino.
    lt_input-lote         = <fs_ordenes>-zzlote.
    lt_input-tipo_notif   = <fs_ordenes>-zztipo_notif.
    lt_input-codlanza     = <fs_ordenes>-zzcod_lanza.
    lt_input-lcarga       = <fs_ordenes>-zzlote_carga.
    lt_input-equipo       = <fs_ordenes>-zzequipo.
    lt_input-ltxa1        = <fs_ordenes>-ltxa1.
    lt_input-nro_rack     = <fs_ordenes>-nro_rack.
    lt_input-nro_caja     = <fs_ordenes>-nro_caja.
    lt_input-nro_molde    = <fs_ordenes>-nro_molde.
    lt_input-cam_horno    = <fs_ordenes>-cam_horno.
    lt_input-no_material  = <fs_ordenes>-no_material.
    lt_input-no_actividad = <fs_ordenes>-no_actividad.
    lt_input-interfaz     = space.
*    lt_input-zzdesperop   = <fs_ordenes>-zzdesperop. "Insert @0002      "-RDGM20191010
    lt_input-zz_rechazo   = <fs_ordenes>-zzdesperop.                    "+RDGM20191010

    LOOP AT gt_componentes INTO gwa_componentes WHERE nro EQ <fs_ordenes>-nro
                                                  AND mod EQ 'X'.
      CLEAR: lt_comp.

      lt_comp-matnr      = gwa_componentes-componente.
      lt_comp-erfmg      = gwa_componentes-cantidad.
      lt_comp-erfme      = gwa_componentes-um.
      lt_comp-lgort      = gwa_componentes-almacen.
      lt_comp-charg      = gwa_componentes-lote.
      lt_comp-bwart      = gwa_componentes-bwart.
      lt_comp-mvt_ind    = gwa_componentes-mvt_ind.
      lt_comp-spec_stock = gwa_componentes-spec_stock.
      lt_comp-sales_ord  = gwa_componentes-sales_ord.
      lt_comp-s_ord_item = gwa_componentes-s_ord_item.
      lt_comp-reserv_no  = gwa_componentes-reserv_no.
      lt_comp-res_item   = gwa_componentes-res_item.
      lt_comp-nuevo      = gwa_componentes-new.

      APPEND lt_comp TO lt_input-component.
    ENDLOOP.

    LOOP AT gt_comp_dele INTO gwa_componentes WHERE nro EQ <fs_ordenes>-nro.
      CLEAR: lt_comp.

      lt_comp-matnr      = gwa_componentes-componente.
      lt_comp-erfmg      = gwa_componentes-cantidad.
      lt_comp-erfme      = gwa_componentes-um.
      lt_comp-lgort      = gwa_componentes-almacen.
      lt_comp-charg      = gwa_componentes-lote.
      lt_comp-bwart      = gwa_componentes-bwart.
      lt_comp-mvt_ind    = gwa_componentes-mvt_ind.
      lt_comp-spec_stock = gwa_componentes-spec_stock.
      lt_comp-sales_ord  = gwa_componentes-sales_ord.
      lt_comp-s_ord_item = gwa_componentes-s_ord_item.
      lt_comp-reserv_no  = gwa_componentes-reserv_no.
      lt_comp-res_item   = gwa_componentes-res_item.
      lt_comp-dele       = 'X'.

      APPEND lt_comp TO lt_input-component.
    ENDLOOP.

    APPEND lt_input.
  ENDLOOP.

*  IF gv_anular_rechazo EQ 'X'.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*  ENDIF.

  IF lt_input[] IS NOT INITIAL.
    CALL FUNCTION 'Z_PP_NOTIFICA_AUTO_V2'
      EXPORTING
        i_werks     = p_werks
        i_arbpl     = p_arbpl
        i_pernr     = p_pernr
        i_ordenes   = lt_input[]
      IMPORTING
        e_resultado = lt_output[].
  ENDIF.

  LOOP AT lt_output.
    PERFORM f_agregar_log_v2 USING lt_output-nro
                                   lt_output-ok_notif
                                   lt_output-ok_lote
                                   lt_output-componente
                                   lt_output-error
                                   lt_output-msg
                                   lt_output-rev_herra.

    IF lt_output-ok_notif EQ 'X'.
      READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY nro = lt_output-nro.

      PERFORM f_impresion_automatica_uni USING gwa_ordenes.
    ENDIF.
  ENDLOOP.

  PERFORM f_actualizar_lcorte.
  PERFORM f_generar_orden_nueva.
  PERFORM f_act_hrs_parada.
  PERFORM f_impresion_automatica_mas.
  PERFORM f_mostrar_log.
*{+@0006
  PERFORM f_alerta_roteiro.
*}+@0006
  PERFORM f_inicializar.
*{-@0012 14.07.2020 re-activación
*{+@0007
"I-24.09.2020 - EMAT
*  PERFORM f_popup.
"I-24.09.2020 - EMAT
*}+@0007
*}-@0012 14.07.2020 re-activación
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB01_AUTOAUFNR
*&---------------------------------------------------------------------*
FORM f_add_tab01_autoaufnr USING    p_vornr
                                    p_ktsch
                           CHANGING p_error.
  DATA: lt_data            TYPE tt_data_masivo     WITH HEADER LINE,
        lt_makt            TYPE tt_makt            WITH HEADER LINE,
        lt_t006a           TYPE tt_t006a           WITH HEADER LINE,
        lt_afru            TYPE tt_afru_full       WITH HEADER LINE,
        lt_lotenotif       TYPE tt_afru_full       WITH HEADER LINE,
        lt_vornr           TYPE tt_vornr           WITH HEADER LINE,
        lt_allocvaluesnum  TYPE tt_allocvaluesnum  WITH HEADER LINE,
        lt_allocvalueschar TYPE tt_allocvalueschar WITH HEADER LINE,
        lt_allocvaluescurr TYPE tt_allocvaluescurr WITH HEADER LINE,
        lt_return          TYPE tt_return          WITH HEADER LINE,
        lv_txt             TYPE string,
        lv_equnr           TYPE equnr,
        lv_batch           TYPE charg_d,
        lv_comp            TYPE i,
        lv_cant            TYPE i,
        lv_loop            TYPE i,
        lv_tabix           TYPE sytabix,
        lv_class           TYPE klasse_d,
        lv_klart           TYPE klassenart,
        lv_obtab           TYPE tabelle,
        lv_objectkey       TYPE objnum,
        lv_aplfl           TYPE n LENGTH 6,
        lv_nro             TYPE i,
        lv_cont            TYPE i.

  REFRESH: lt_data,
           lt_makt,
           lt_t006a,
           lt_afru,
           lt_lotenotif.

  lv_txt = TEXT-008.

  "Validar Fecha de contabilización
  IF zppe_notif_addreg-budat IS INITIAL.
    MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  "Validar cantidad
  IF zppe_notif_addreg-lmnga IS INITIAL.
    REPLACE '&' IN lv_txt WITH TEXT-069. "'Cantidad'.
    MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.
  "Validar orden liberada
  SELECT SINGLE COUNT(*)
    FROM aufk INNER JOIN jest ON aufk~objnr EQ jest~objnr
    WHERE aufk~aufnr EQ zppe_notif_addreg-aufnr
      AND jest~stat  EQ 'I0002'
      AND jest~inact EQ space.
  IF sy-subrc NE 0.
    lv_txt = TEXT-021.
    REPLACE '&' IN lv_txt WITH zppe_notif_addreg-aufnr.
    MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  lv_aplfl = zppe_notif_addreg-aplfl.

  SELECT *
    INTO TABLE lt_lotenotif
    FROM afru
    WHERE werks  EQ p_werks
      AND aufnr  EQ zppe_notif_addreg-aufnr
      AND vornr  EQ p_vornr
      AND aplfl  EQ lv_aplfl
      AND stokz  EQ space
      AND stzhl  EQ space
      AND lmnga  NE 0
      AND zzlote NE space.

  SELECT afvc~vornr
    INTO TABLE lt_vornr
    FROM afko INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
    WHERE afko~aufnr EQ zppe_notif_addreg-aufnr.
  SORT lt_vornr BY vornr ASCENDING.
  READ TABLE lt_vornr WITH KEY vornr = p_vornr.
  lv_tabix = sy-tabix.
  SUBTRACT 1 FROM lv_tabix.
  READ TABLE lt_vornr INDEX lv_tabix.

  SELECT *
    INTO TABLE lt_afru
    FROM afru
    WHERE werks        EQ p_werks
      AND aufnr        EQ zppe_notif_addreg-aufnr
      AND vornr        EQ lt_vornr-vornr
      AND aplfl        EQ lv_aplfl
      AND stokz        EQ space
      AND stzhl        EQ space
      AND lmnga        NE 0
      AND zzlote       NE space
      AND zztipo_notif IN gr_tipnot_aprob.

  LOOP AT lt_afru.
    READ TABLE lt_lotenotif WITH KEY zzlote = lt_afru-zzlote.
    IF sy-subrc EQ 0.
      DELETE lt_afru WHERE zzlote EQ lt_afru-zzlote.
    ENDIF.
  ENDLOOP.

  SORT lt_afru BY zzlote ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_afru COMPARING zzlote.

  SORT lt_afru BY rueck ASCENDING
                  rmzhl ASCENDING.

*  LOOP AT lt_afru.
*    IF sy-tabix GT zppe_notif_addreg-lmnga.
*      DELETE lt_afru.
*    ENDIF.
*  ENDLOOP.

  IF lt_afru[] IS NOT INITIAL.
    SELECT  afru~aufnr
            afvc~vornr
            afvc~ktsch
            afvc~plnfl
            afko~plnbez
            afko~gmein
            afru~zzlote
            afvc~steus
            afru~zzequipo
            afru~zzcod_lanza
            afru~rueck
            afru~rmzhl
      INTO TABLE lt_data
      FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
                INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl AND
                                   afru~arbid EQ afvc~arbid
      FOR ALL ENTRIES IN lt_afru
      WHERE afru~rueck EQ lt_afru-rueck
        AND afru~rmzhl EQ lt_afru-rmzhl
        AND afvc~vornr EQ lt_afru-vornr.
  ENDIF.

  IF lt_data[] IS NOT INITIAL.
    SORT lt_data BY aufnr ASCENDING
                    vornr ASCENDING.

    SELECT matnr
           maktx
      INTO TABLE lt_makt
      FROM makt
      FOR ALL ENTRIES IN lt_data
      WHERE matnr EQ lt_data-plnbez
        AND spras EQ sy-langu.

    READ TABLE gt_tipnot INTO gwa_tipnot INDEX 1.

    SELECT msehi
           mseh6
      INTO TABLE lt_t006a
      FROM t006a
      WHERE spras EQ sy-langu.
  ENDIF.

  LOOP AT lt_data.
    CLEAR:   gwa_ordenes,
             lt_makt,
             lt_t006a,
             lv_class,
             lv_klart,
             lv_obtab,
             lt_allocvalueschar.
    REFRESH: lt_allocvaluesnum,
             lt_allocvalueschar,
             lt_allocvaluescurr,
             lt_return.

    READ TABLE lt_makt  WITH KEY matnr = lt_data-plnbez.
    READ TABLE lt_t006a WITH KEY msehi = lt_data-gmein.

    CALL FUNCTION 'QMSP_MATERIAL_BATCH_CLASS_READ'
      EXPORTING
        i_matnr                = lt_data-plnbez
        i_charg                = lt_data-charg
        i_werks                = p_werks
        i_mara_level           = 'X'
      IMPORTING
        e_class                = lv_class
        e_klart                = lv_klart
        e_obtab                = lv_obtab
      EXCEPTIONS
        no_class               = 1
        internal_error_classif = 2
        no_change_service      = 3
        OTHERS                 = 4.

    CONCATENATE lt_data-plnbez
                lt_data-charg
                INTO lv_objectkey.

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey        = lv_objectkey
        objecttable      = lv_obtab
        classnum         = lv_class
        classtype        = lv_klart
        unvaluated_chars = 'X'
      TABLES
        allocvaluesnum   = lt_allocvaluesnum[]
        allocvalueschar  = lt_allocvalueschar[]
        allocvaluescurr  = lt_allocvaluescurr[]
        return           = lt_return[].

    READ TABLE lt_allocvalueschar WITH KEY charact = 'Z_TIPO_NOTIFICACION'.

    IF lt_allocvalueschar-value_char NOT IN gr_tipnot_aprob.
      CONTINUE.
    ENDIF.

    "Orden
    gwa_ordenes-aufnr = lt_data-aufnr.

    "Operación
    gwa_ordenes-vornr = p_vornr.

    "Clave Modelo
    gwa_ordenes-ktsch = p_ktsch.

    "Fecha contabilización
    gwa_ordenes-budat = zppe_notif_addreg-budat.

    "Secuencia
    gwa_ordenes-plnfl = lt_data-plnfl.

    "Equipo
*    gwa_ordenes-zzequipo = lt_data-zzequipo.

    "Lote
    gwa_ordenes-zzlote = lt_data-charg.

    "Material
    gwa_ordenes-plnbez = lt_data-plnbez.

    "Descripción
    gwa_ordenes-maktx = lt_makt-maktx.

    "UM
    gwa_ordenes-gmein     = lt_data-gmein.
    gwa_ordenes-gmein_txt = lt_t006a-mseh6.

    "Cantidad
    gwa_ordenes-lmnga = 1.

    "Tipo de Notificación
    IF lt_allocvalueschar-value_char IS NOT INITIAL.
      gwa_ordenes-zztipo_notif = lt_allocvalueschar-value_char.
    ELSE.
      gwa_ordenes-zztipo_notif = gwa_tipnot-zztipo_notif.
    ENDIF.

    "Lote de Corte
*    gwa_ordenes-zzlote_corte =

    "Lote de Curvado
*    gwa_ordenes-zzlote_curvado =

    "Ciclo de TQ
*    gwa_ordenes-ZZCICLO_TQ.

    "Ciclo de Autoclave
    gwa_ordenes-zzciclo_auto = zppe_notif_addreg-zzciclo_auto.

    "Código de Lanzada
*    gwa_ordenes-zzcod_lanza = lt_data-zzcod_lanza.

    "Lote de Carga
*    gwa_ordenes-ZZLOTE_CARGA.

    "Defecto
*    gwa_ordenes-DESC_DEFCOD.

    "Código Defecto
*    gwa_ordenes-ZZDEFCOD.

    "Origen
*    gwa_ordenes-DESC_ORIGEN.

    "Código Origen
*    gwa_ordenes-ZZORIGEN.

    "Causa
*    gwa_ordenes-DESC_AUSA.

    "Código causa
*    gwa_ordenes-ZZCAUSA.

    "Destino
*    gwa_ordenes-ZZDESTINO.

    "Causa de Desviación
*    gwa_ordenes-GRUND.

    ADD 1 TO lv_cont.
    lv_nro = gv_nro.
    ADD 1 TO gv_nro.

    "Nro
    gwa_ordenes-nro = gv_nro.

    IF lv_cont GT zppe_notif_addreg-lmnga.
      EXIT.
    ENDIF.
    gwa_ordenes-zzdesperop   = zppe_notif_addreg-zzdesperop. "Insert @0002
    APPEND gwa_ordenes TO gt_ordenes.

    PERFORM f_completar_componente USING lv_nro
                                         lt_data-aufnr
                                         lt_data-vornr
                                         lt_data-plnfl
                                         lt_data-ktsch
                                         gwa_ordenes-zztipo_notif
                                         1"gwa_ordenes-lmnga
                                         space.
  ENDLOOP.

  IF sy-subrc EQ 0.
    SORT gt_ordenes BY nro   ASCENDING.
*                       aufnr ASCENDING
*                       vornr ASCENDING.

    CALL METHOD gr_grid01->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
    gv_addreg = '5'.
  ELSE.
    p_error = 'X'.

    MESSAGE TEXT-013 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LOTE_DUPLI
*&---------------------------------------------------------------------*
FORM f_lote_dupli USING    p_data TYPE ty_ordenes
                  CHANGING p_error.
  DATA: lv_text   TYPE string,
        lv_lote   TYPE charg_d,
        lv_tipnot TYPE ze_tipo_notif.

  CLEAR: p_error,
         lv_tipnot.

  CHECK gv_lote_dupli EQ '1' AND
        p_data-zzlote IS NOT INITIAL.

  "A nivel general
  SELECT SINGLE COUNT(*)
    FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl AND
                                 afru~werks EQ afvc~werks
    WHERE afru~aufnr        EQ p_data-aufnr
      AND afru~stokz        EQ space
      AND afru~stzhl        EQ space
      AND afru~zztipo_notif IN gr_tipnot_recha
      AND afru~zzlote       EQ p_data-zzlote.
  IF sy-subrc EQ 0.
    lv_text = TEXT-040.
    REPLACE '&' IN lv_text WITH gv_recha.
    PERFORM f_agregar_log_v2 USING p_data-nro
                                   space
                                   'N'
                                   space
                                   space
                                   lv_text
                                   space.
    p_error = 'X'.
    EXIT.
  ENDIF.

  "A nivel Puesto de trabajo/Clave Modelo/Operación
  SELECT SINGLE afru~zztipo_notif
    INTO lv_tipnot
    FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl AND
                                 afru~werks EQ afvc~werks
    WHERE afru~werks        EQ p_werks
      AND afru~aufnr        EQ p_data-aufnr
      AND afru~vornr        EQ p_data-vornr
      AND afru~aplfl        EQ p_data-plnfl
      AND afru~stokz        EQ space
      AND afru~stzhl        EQ space
      AND afru~zztipo_notif IN gr_tipnot_aprob
      AND afru~zzlote       EQ p_data-zzlote.
  IF sy-subrc EQ 0.
    IF p_data-zztipo_notif IN gr_tipnot_recha AND
       p_data-anul_recha   IS NOT INITIAL.
    ELSE.
      lv_text = TEXT-040.
      REPLACE '&' IN lv_text WITH lv_tipnot.
      PERFORM f_agregar_log_v2 USING p_data-nro
                                     space
                                     'N'
                                     space
                                     space
                                     lv_text
                                     space.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  "Análisis
  SELECT SINGLE COUNT(*)
    FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl AND
                                 afru~werks EQ afvc~werks
    WHERE afru~werks        EQ p_werks
      AND afru~aufnr        EQ p_data-aufnr
      AND afru~vornr        EQ p_data-vornr
      AND afru~aplfl        EQ p_data-plnfl
      AND afru~stokz        EQ space
      AND afru~stzhl        EQ space
      AND afru~zztipo_notif IN gr_tipnot_anali
      AND afru~zzlote       EQ p_data-zzlote.
  IF sy-subrc            EQ 0                AND
     p_data-zztipo_notif IN gr_tipnot_anali.
    lv_text = TEXT-040.
    REPLACE '&' IN lv_text WITH p_data-zztipo_notif.
    PERFORM f_agregar_log_v2 USING p_data-nro
                                   space
                                   'N'
                                   space
                                   space
                                   lv_text
                                   space.
    p_error = 'X'.
    EXIT.
  ENDIF.

  "Retrabajo
  SELECT SINGLE COUNT(*)
    FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl AND
                                 afru~werks EQ afvc~werks
    WHERE afru~werks        EQ p_werks
      AND afru~aufnr        EQ p_data-aufnr
      AND afru~vornr        EQ p_data-vornr
      AND afru~aplfl        EQ p_data-plnfl
      AND afru~stokz        EQ space
      AND afru~stzhl        EQ space
      AND afru~zztipo_notif EQ gv_retra
      AND afru~zzlote       EQ p_data-zzlote.
  IF sy-subrc            EQ 0        AND
     p_data-zztipo_notif EQ gv_retra.
    lv_text = TEXT-040.
    REPLACE '&' IN lv_text WITH p_data-zztipo_notif.
    PERFORM f_agregar_log_v2 USING p_data-nro
                                   space
                                   'N'
                                   space
                                   space
                                   lv_text
                                   space.
    p_error = 'X'.
    EXIT.
  ENDIF.

  "Reproceso
  SELECT SINGLE COUNT(*)
    FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl AND
                                 afru~werks EQ afvc~werks
    WHERE afru~werks        EQ p_werks
      AND afru~aufnr        EQ p_data-aufnr
      AND afru~vornr        EQ p_data-vornr
      AND afru~aplfl        EQ p_data-plnfl
      AND afru~stokz        EQ space
      AND afru~stzhl        EQ space
      AND afru~zztipo_notif EQ gv_repro
      AND afru~zzlote       EQ p_data-zzlote.
  IF sy-subrc            EQ 0        AND
     p_data-zztipo_notif EQ gv_retra.
    lv_text = TEXT-040.
    REPLACE '&' IN lv_text WITH p_data-zztipo_notif.
    PERFORM f_agregar_log_v2 USING p_data-nro
                                   space
                                   'N'
                                   space
                                   space
                                   lv_text
                                   space.
    p_error = 'X'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ELIMINAR_REGISTROS_EXTRA
*&---------------------------------------------------------------------*
FORM f_eliminar_registros_extra .
  DELETE gt_ordenes WHERE nro IS INITIAL.
  IF sy-subrc EQ 0.
    CALL METHOD gr_grid01->refresh_table_display
      EXPORTING
        is_stable = gc_stable.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_SAVE
*&---------------------------------------------------------------------*
FORM f_add_save .
  CLEAR:   zppe_notif_addreg.

  gv_add_save = 'X'.

  READ TABLE gt_tipnot INTO gwa_tipnot INDEX 1.
  zppe_notif_addreg-werks        = p_werks.
  zppe_notif_addreg-budat        = sy-datum.
  zppe_notif_addreg-zztipo_notif = gwa_tipnot-zztipo_notif.
  zppe_notif_addreg-lmnga        = 1.

  CALL SCREEN 0201 STARTING AT 30  1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_SAVE_0201
*&---------------------------------------------------------------------*
FORM f_add_save_0201 .
  DATA: lt_input    TYPE zpptt_notif_mfinput   WITH HEADER LINE,
        lt_output   TYPE zpptt_notif_mfoutput  WITH HEADER LINE,
        lt_comp     TYPE zpptt_notif_component WITH HEADER LINE,
        lt_msg      TYPE zpptt_notif_rpta,
        lv_answer   TYPE c,
        lv_ok_notif TYPE boolean,
        lv_ok_lote  TYPE c,
        lv_lmnga    TYPE ru_lmnga,
        lv_xmnga    TYPE ru_xmnga,
        lv_rmnga    TYPE ru_rmnga,
        lv_nro      TYPE int4,
        lv_txt      TYPE string,
        lv_error    TYPE c,
        lv_val      TYPE c.

  IF zppe_notif_addreg-id IS NOT INITIAL.
    CLEAR: zppe_notif_addreg-aufnr,
           zppe_notif_addreg-charg.

    zppe_notif_addreg-aufnr = zppe_notif_addreg-id+0(12).
    zppe_notif_addreg-charg = zppe_notif_addreg-id+12(10).

    CLEAR: zppe_notif_addreg-id.
  ENDIF.

  "Clave modelo - Operación
  IF zppe_notif_addreg-ktsch IS INITIAL AND
     zppe_notif_addreg-vornr IS INITIAL.
    MESSAGE TEXT-012 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF zppe_notif_addreg-aufnr IS INITIAL.
    lv_txt = TEXT-008.
    REPLACE '&' IN lv_txt WITH TEXT-078. "'Orden de Fabricación'.
    MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    SELECT SINGLE COUNT(*)
      FROM aufk
      WHERE aufnr EQ zppe_notif_addreg-aufnr.
    IF sy-subrc NE 0.
      MESSAGE TEXT-043 TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  "Lote
  IF gv_lote_oblig           EQ '1'     AND
     zppe_notif_addreg-charg IS INITIAL.
    lv_txt = TEXT-025.
    REPLACE '&1' IN lv_txt WITH TEXT-x10. "'Lote'.

    MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM f_add_tab01_aufnr CHANGING lv_error.
  CHECK lv_error IS INITIAL.

  CLEAR:   lv_answer,
           lv_error.
  REFRESH: lt_comp,
           lt_msg,
           gt_log,
           lt_input,
           lt_output.

  LOOP AT gt_ordenes ASSIGNING <fs_ordenes>.
    CLEAR:   lv_error,
             lv_lmnga,
             lv_xmnga,
             lv_nro,
             lt_input.

    PERFORM f_lote_dupli_0201 USING    <fs_ordenes>
                              CHANGING lv_error.
    IF lv_error IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    PERFORM f_genera_lote_auto CHANGING lv_error
                                        lv_val.
    IF lv_error IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    IF <fs_ordenes>-zztipo_notif IN gr_tipnot_aprob.
      lv_lmnga = <fs_ordenes>-lmnga.
    ELSEIF <fs_ordenes>-zztipo_notif IN gr_tipnot_recha.
      lv_xmnga = <fs_ordenes>-lmnga.
    ELSEIF <fs_ordenes>-zztipo_notif IN gr_tipnot_retra.
      lv_rmnga = <fs_ordenes>-lmnga.
    ELSEIF <fs_ordenes>-zztipo_notif IN gr_tipnot_anali.
    ENDIF.

    lt_input-nro          = <fs_ordenes>-nro.
    lt_input-aufnr        = <fs_ordenes>-aufnr.
    lt_input-vornr        = <fs_ordenes>-vornr.
    lt_input-ktsch        = <fs_ordenes>-ktsch.
    lt_input-budat        = <fs_ordenes>-budat.
    lt_input-aplfl        = <fs_ordenes>-plnfl.
    lt_input-meinh        = <fs_ordenes>-gmein.
    lt_input-lmnga        = lv_lmnga.
    lt_input-xmnga        = lv_xmnga.
    lt_input-rmnga        = lv_rmnga.
    lt_input-grund        = <fs_ordenes>-grund.
*    lt_input-TAG_ID       = .
    lt_input-lcorte       = <fs_ordenes>-zzlote_corte.
    lt_input-lcurvado     = <fs_ordenes>-zzlote_curvado.
    lt_input-ciclotq      = <fs_ordenes>-zzciclo_tq.
    lt_input-cicloauto    = <fs_ordenes>-zzciclo_auto.
*    lt_input-TEMPTERM     = .
    lt_input-defecto      = <fs_ordenes>-zzdefecto.
    lt_input-defcod       = <fs_ordenes>-zzdefcod.
    lt_input-origen       = <fs_ordenes>-zzorigen.
    lt_input-orcod        = <fs_ordenes>-zzorcod.
    lt_input-causa        = <fs_ordenes>-zzcausa.
    lt_input-caucod       = <fs_ordenes>-zzcaucod.
    lt_input-destino      = <fs_ordenes>-zzdestino.
    lt_input-lote         = <fs_ordenes>-zzlote.
    lt_input-tipo_notif   = <fs_ordenes>-zztipo_notif.
    lt_input-codlanza     = <fs_ordenes>-zzcod_lanza.
    lt_input-lcarga       = <fs_ordenes>-zzlote_carga.
    lt_input-equipo       = <fs_ordenes>-zzequipo.
    lt_input-ltxa1        = <fs_ordenes>-ltxa1.
    lt_input-nro_rack     = <fs_ordenes>-nro_rack.
    lt_input-nro_caja     = <fs_ordenes>-nro_caja.
    lt_input-nro_molde    = <fs_ordenes>-nro_molde.
    lt_input-cam_horno    = <fs_ordenes>-cam_horno.
    lt_input-no_material  = <fs_ordenes>-no_material.
    lt_input-no_actividad = <fs_ordenes>-no_actividad.
    lt_input-interfaz     = space.
*    lt_input-zzdesperop   = <fs_ordenes>-zzdesperop. "Insert @0002      "-RDGM20191010
    lt_input-zz_rechazo   = <fs_ordenes>-zzdesperop.                    "+RDGM20191010

    LOOP AT gt_componentes INTO gwa_componentes WHERE nro EQ <fs_ordenes>-nro
                                                  AND mod EQ 'X'.
      CLEAR: lt_comp.

      lt_comp-matnr      = gwa_componentes-componente.
      lt_comp-erfmg      = gwa_componentes-cantidad.
      lt_comp-erfme      = gwa_componentes-um.
      lt_comp-lgort      = gwa_componentes-almacen.
      lt_comp-charg      = gwa_componentes-lote.
      lt_comp-bwart      = gwa_componentes-bwart.
      lt_comp-mvt_ind    = gwa_componentes-mvt_ind.
      lt_comp-spec_stock = gwa_componentes-spec_stock.
      lt_comp-sales_ord  = gwa_componentes-sales_ord.
      lt_comp-s_ord_item = gwa_componentes-s_ord_item.
      lt_comp-nuevo      = gwa_componentes-new.

      APPEND lt_comp TO lt_input-component.
    ENDLOOP.

    LOOP AT gt_comp_dele INTO gwa_componentes WHERE nro EQ <fs_ordenes>-nro.
      CLEAR: lt_comp.

      lt_comp-matnr      = gwa_componentes-componente.
      lt_comp-erfmg      = gwa_componentes-cantidad.
      lt_comp-erfme      = gwa_componentes-um.
      lt_comp-lgort      = gwa_componentes-almacen.
      lt_comp-charg      = gwa_componentes-lote.
      lt_comp-bwart      = gwa_componentes-bwart.
      lt_comp-mvt_ind    = gwa_componentes-mvt_ind.
      lt_comp-spec_stock = gwa_componentes-spec_stock.
      lt_comp-sales_ord  = gwa_componentes-sales_ord.
      lt_comp-s_ord_item = gwa_componentes-s_ord_item.
      lt_comp-dele       = 'X'.

      APPEND lt_comp TO lt_input-component.
    ENDLOOP.

    APPEND lt_input.
  ENDLOOP.

  IF lt_input[] IS NOT INITIAL.
    CALL FUNCTION 'Z_PP_NOTIFICA_AUTO_V2'
      EXPORTING
        i_werks     = p_werks
        i_arbpl     = p_arbpl
        i_pernr     = p_pernr
        i_ordenes   = lt_input[]
      IMPORTING
        e_resultado = lt_output[].
  ENDIF.

  LOOP AT lt_output WHERE ok_notif EQ 'X'.
    READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY nro = lt_output-nro.

    PERFORM f_impresion_automatica_uni USING gwa_ordenes.

    MESSAGE TEXT-041 TYPE 'S'.
  ENDLOOP.
  IF sy-subrc NE 0.
    READ TABLE lt_output INDEX 1.

    MESSAGE lt_output-msg TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

*  PERFORM f_impresion_automatica_mas.

  CLEAR:   zppe_notif_addreg-id,
           zppe_notif_addreg-aufnr,
           zppe_notif_addreg-charg.
  REFRESH: gt_ordenes,
           gt_componentes,
           gt_comp_dele,
           gt_log.

  READ TABLE gt_tipnot INTO gwa_tipnot INDEX 1.
  zppe_notif_addreg-werks        = p_werks.
  zppe_notif_addreg-budat        = sy-datum.
  zppe_notif_addreg-zztipo_notif = gwa_tipnot-zztipo_notif.
  zppe_notif_addreg-lmnga        = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LOTE_DUPLI_0201
*&---------------------------------------------------------------------*
FORM f_lote_dupli_0201 USING    p_data TYPE ty_ordenes
                       CHANGING p_error.
  DATA: lv_text   TYPE string,
        lv_lote   TYPE charg_d,
        lv_tipnot TYPE ze_tipo_notif.

  CLEAR: p_error,
         lv_tipnot.

  CHECK gv_lote_dupli EQ '1' AND
        p_data-zzlote IS NOT INITIAL.

  "A nivel general
  SELECT SINGLE COUNT(*)
    FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl AND
                                 afru~werks EQ afvc~werks
    WHERE afru~aufnr        EQ p_data-aufnr
      AND afru~stokz        EQ space
      AND afru~stzhl        EQ space
      AND afru~zztipo_notif IN gr_tipnot_recha
      AND afru~zzlote       EQ p_data-zzlote.
  IF sy-subrc EQ 0.
    lv_text = TEXT-040.
    REPLACE '&' IN lv_text WITH gv_recha.
    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  "A nivel Puesto de trabajo/Clave Modelo/Operación
  SELECT SINGLE afru~zztipo_notif
    INTO lv_tipnot
    FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl AND
                                 afru~werks EQ afvc~werks
    WHERE afru~werks        EQ p_werks
      AND afru~aufnr        EQ p_data-aufnr
      AND afru~vornr        EQ p_data-vornr
      AND afru~aplfl        EQ p_data-plnfl
      AND afru~stokz        EQ space
      AND afru~stzhl        EQ space
      AND afru~zztipo_notif IN gr_tipnot_aprob
      AND afru~zzlote       EQ p_data-zzlote.
  IF sy-subrc EQ 0.
    IF p_data-zztipo_notif IN gr_tipnot_recha AND
       p_data-anul_recha   IS NOT INITIAL.
    ELSE.
      lv_text = TEXT-040.
      REPLACE '&' IN lv_text WITH lv_tipnot.
      MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  "Análisis
  SELECT SINGLE COUNT(*)
    FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl AND
                                 afru~werks EQ afvc~werks
    WHERE afru~werks        EQ p_werks
      AND afru~aufnr        EQ p_data-aufnr
      AND afru~vornr        EQ p_data-vornr
      AND afru~aplfl        EQ p_data-plnfl
      AND afru~stokz        EQ space
      AND afru~stzhl        EQ space
      AND afru~zztipo_notif IN gr_tipnot_anali
      AND afru~zzlote       EQ p_data-zzlote.
  IF sy-subrc            EQ 0                AND
     p_data-zztipo_notif IN gr_tipnot_anali.
    lv_text = TEXT-040.
    REPLACE '&' IN lv_text WITH p_data-zztipo_notif.
    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  "Retrabajo
  SELECT SINGLE COUNT(*)
    FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl AND
                                 afru~werks EQ afvc~werks
    WHERE afru~werks        EQ p_werks
      AND afru~aufnr        EQ p_data-aufnr
      AND afru~vornr        EQ p_data-vornr
      AND afru~aplfl        EQ p_data-plnfl
      AND afru~stokz        EQ space
      AND afru~stzhl        EQ space
      AND afru~zztipo_notif EQ gv_retra
      AND afru~zzlote       EQ p_data-zzlote.
  IF sy-subrc            EQ 0        AND
     p_data-zztipo_notif EQ gv_retra.
    lv_text = TEXT-040.
    REPLACE '&' IN lv_text WITH p_data-zztipo_notif.
    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  "Reproceso
  SELECT SINGLE COUNT(*)
    FROM afru INNER JOIN afko ON afru~aufnr EQ afko~aufnr
              INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl AND
                                 afru~werks EQ afvc~werks
    WHERE afru~werks        EQ p_werks
      AND afru~aufnr        EQ p_data-aufnr
      AND afru~vornr        EQ p_data-vornr
      AND afru~aplfl        EQ p_data-plnfl
      AND afru~stokz        EQ space
      AND afru~stzhl        EQ space
      AND afru~zztipo_notif EQ gv_repro
      AND afru~zzlote       EQ p_data-zzlote.
  IF sy-subrc            EQ 0        AND
     p_data-zztipo_notif EQ gv_retra.
    lv_text = TEXT-040.
    REPLACE '&' IN lv_text WITH p_data-zztipo_notif.
    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB01_ZZCICLO_TQ_PT_PREV
*&---------------------------------------------------------------------*
FORM f_add_tab01_zzciclo_tq_pt_prev  CHANGING p_error.
  DATA: lt_makt  TYPE tt_makt        WITH HEADER LINE,
        lt_t006a TYPE tt_t006a       WITH HEADER LINE,
        lv_nro   TYPE i.

  REFRESH: lt_makt,
           lt_t006a.

  IF zppe_notif_addreg-budat IS INITIAL.
    MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  SELECT  afko~aufnr,
          afvc2~vornr,
          afvc2~ktsch,
          afvc2~plnfl,
          afko~plnbez,
          afko~gmein,
          afpo~charg,
          afvc2~steus
    INTO TABLE @DATA(lt_data)
    FROM afvu INNER JOIN afko          ON afvu~aufpl EQ afko~aufpl
              INNER JOIN afpo          ON afko~aufnr EQ afpo~aufnr
              INNER JOIN afvc          ON afvu~aufpl EQ afvc~aufpl AND
                                          afvu~aplzl EQ afvc~aplzl
              INNER JOIN afvc AS afvc2 ON afvc~aufpl EQ afvc2~aufpl AND
                                          afvc~ktsch EQ afvc2~ktsch
              INNER JOIN crhd          ON afvc2~arbid EQ crhd~objid
    WHERE afvu~usr02 EQ @zppe_notif_addreg-zzciclo_tq
      AND crhd~arbpl EQ @p_arbpl
      AND crhd~werks EQ @p_werks.

  SELECT matnr
         maktx
    INTO TABLE lt_makt
    FROM makt
    FOR ALL ENTRIES IN lt_data
    WHERE matnr EQ lt_data-plnbez
      AND spras EQ sy-langu.

  READ TABLE gt_tipnot INTO gwa_tipnot INDEX 1.

  SELECT msehi
         mseh6
    INTO TABLE lt_t006a
    FROM t006a
    WHERE spras EQ sy-langu.

  LOOP AT lt_data INTO DATA(lwa_data).
    CLEAR: gwa_ordenes,
           lt_makt,
           lt_t006a.

    lv_nro = gv_nro.
    ADD 1 TO gv_nro.
    READ TABLE lt_makt  WITH KEY matnr = lwa_data-plnbez.
    READ TABLE lt_t006a WITH KEY msehi = lwa_data-gmein.

    "Nro.
    gwa_ordenes-nro = gv_nro.

    "Orden
    gwa_ordenes-aufnr = lwa_data-aufnr.

    "Operación
    gwa_ordenes-vornr = lwa_data-vornr.

    "Clave Modelo
    gwa_ordenes-ktsch = lwa_data-ktsch.

    "Fecha contabilización
    gwa_ordenes-budat = zppe_notif_addreg-budat.

    "Secuencia
    gwa_ordenes-plnfl = lwa_data-plnfl.

    "Equipo
*    gwa_ordenes-zzequipo = .

    "Lote
    gwa_ordenes-zzlote = lwa_data-charg.

    "Material
    gwa_ordenes-plnbez = lwa_data-plnbez.

    "Descripción
    gwa_ordenes-maktx = lt_makt-maktx.

    "UM
    gwa_ordenes-gmein     = lwa_data-gmein.
    gwa_ordenes-gmein_txt = lt_t006a-mseh6.

    "Cantidad
    gwa_ordenes-lmnga = 1.

    "Tipo de Notificación
    gwa_ordenes-zztipo_notif = gwa_tipnot-zztipo_notif.

    "Lote de Corte
*    gwa_ordenes-zzlote_corte = .

    "Lote de Curvado
*    gwa_ordenes-ZZLOTE_CURVADO.

    "Ciclo de TQ
    gwa_ordenes-zzciclo_tq = zppe_notif_addreg-zzciclo_tq.

    "Ciclo de Autoclave
*    gwa_ordenes-ZZCICLO_AUTO.

    "Código de Lanzada
*    gwa_ordenes-zzcod_lanza = .

    "Lote de Carga
*    gwa_ordenes-ZZLOTE_CARGA.

    "Defecto
*    gwa_ordenes-DESC_DEFCOD.

    "Código Defecto
*    gwa_ordenes-ZZDEFCOD.

    "Origen
*    gwa_ordenes-DESC_ORIGEN.

    "Código Origen
*    gwa_ordenes-ZZORIGEN.

    "Causa
*    gwa_ordenes-DESC_AUSA.

    "Código causa
*    gwa_ordenes-ZZCAUSA.

    "Destino
*    gwa_ordenes-ZZDESTINO.

    "Causa de Desviación
*    gwa_ordenes-GRUND.
    gwa_ordenes-zzdesperop   = zppe_notif_addreg-zzdesperop. "Insert @0002
    APPEND gwa_ordenes TO gt_ordenes.

    PERFORM f_completar_componente USING lv_nro
                                         lwa_data-aufnr
                                         lwa_data-vornr
                                         lwa_data-plnfl
                                         lwa_data-ktsch
                                         gwa_ordenes-zztipo_notif
                                         1"gwa_ordenes-lmnga
                                         space.
  ENDLOOP.

  IF sy-subrc EQ 0.
    SORT gt_ordenes BY nro ASCENDING.
*                       aufnr ASCENDING
*                       vornr ASCENDING.

    CALL METHOD gr_grid01->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
    gv_addreg = '1'.
  ELSE.
    p_error = 'X'.

    MESSAGE TEXT-013 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GENERAR_ORDEN_NUEVA
*&---------------------------------------------------------------------*
FORM f_generar_orden_nueva .
  DATA: lt_order TYPE tt_order_key WITH HEADER LINE,
        lwa_mod  TYPE bapi_pp_order_change,
        lwa_modx TYPE bapi_pp_order_changex,
        lwa_new  TYPE bapi_pp_order_create,
        lv_seqnr TYPE cy_seqnr,
        lv_aufnr TYPE aufnr,
        lv_txt   TYPE string,
        lv_loop  TYPE i.
  RANGES: lr_aufnr FOR caufv-aufnr.

  CHECK gv_gen_ord_nue EQ '1'.

  REFRESH: lr_aufnr.

  lr_aufnr-sign   = 'I'.
  lr_aufnr-option = 'EQ'.
  LOOP AT gt_log INTO gwa_log WHERE notif EQ TEXT-067. "'Sí'.
    READ TABLE gt_ordenes INTO gwa_ordenes WITH KEY aufnr        = gwa_log-aufnr
                                                    zztipo_notif = gv_recha.
    IF sy-subrc EQ 0.
      lr_aufnr-low = gwa_ordenes-aufnr.
      APPEND lr_aufnr.
    ENDIF.
  ENDLOOP.

  SORT lr_aufnr BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lr_aufnr COMPARING low.

  CHECK lr_aufnr[] IS NOT INITIAL.

  SELECT caufv~aufnr,
         caufv~cy_seqnr,
         caufv~plnbez,
         caufv~werks,
         afpo~pwerk,
         caufv~auart,
         caufv~gstrp,
         caufv~gltrp,
         caufv~gamng,
         caufv~kdauf,
         caufv~kdpos,
         afpo~verid,
         caufv~zzcolor,
         caufv~zzst_toc
    INTO TABLE @DATA(lt_caufv)
    FROM caufv INNER JOIN afpo ON caufv~aufnr = afpo~aufnr
    WHERE caufv~aufnr IN @lr_aufnr.

  LOOP AT lt_caufv INTO DATA(lwa_caufv).
    CLEAR: lv_seqnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_caufv-aufnr
      IMPORTING
        output = lv_seqnr.

    IF lwa_caufv-cy_seqnr IS INITIAL.
      CLEAR: lwa_mod,
             lwa_modx,
             lv_loop.

      lwa_mod-sequence_number  = lv_seqnr.
      lwa_modx-sequence_number = 'X'.

      WHILE lv_loop LE 300.
        CALL FUNCTION 'ENQUEUE_ESORDER'
          EXPORTING
            aufnr          = lwa_caufv-aufnr
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'DEQUEUE_ESORDER'
            EXPORTING
              aufnr = lwa_caufv-aufnr.
          WAIT UP TO 1 SECONDS.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
          ADD 1 TO lv_loop.
        ENDIF.
      ENDWHILE.

      IF lv_loop GT 300.
        UPDATE afko SET cy_seqnr = lv_seqnr WHERE aufnr = lwa_caufv-aufnr.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        CALL FUNCTION 'BAPI_PRODORD_CHANGE'
          EXPORTING
            number     = lwa_caufv-aufnr
            orderdata  = lwa_mod
            orderdatax = lwa_modx.
      ENDIF.
    ELSEIF lv_seqnr EQ lwa_caufv-cy_seqnr.
      CONTINUE.
    ELSE.
      lv_seqnr = lwa_caufv-cy_seqnr.
    ENDIF.

    "Cerrar tecnicamente
    CLEAR:   lv_loop.
    REFRESH: lt_order.

    WHILE lv_loop LE 300.
      CALL FUNCTION 'ENQUEUE_ESORDER'
        EXPORTING
          aufnr          = lwa_caufv-aufnr
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'DEQUEUE_ESORDER'
          EXPORTING
            aufnr = lwa_caufv-aufnr.
        WAIT UP TO 1 SECONDS.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
        ADD 1 TO lv_loop.
      ENDIF.
    ENDWHILE.

    lt_order-order_number = lwa_caufv-aufnr.
    APPEND lt_order.

    CALL FUNCTION 'DIALOG_SET_NO_DIALOG'.
    CALL FUNCTION 'BAPI_PRODORD_COMPLETE_TECH'
      TABLES
        orders = lt_order.


    CLEAR: lwa_new,
           lwa_mod,
           lwa_modx,
           lv_aufnr,
           lv_txt,
           lv_loop.

    lwa_new-material         = lwa_caufv-plnbez.
    lwa_new-plant            = lwa_caufv-werks.
    lwa_new-planning_plant   = lwa_caufv-pwerk.
    lwa_new-order_type       = lwa_caufv-auart.
    lwa_new-basic_start_date = lwa_caufv-gstrp.
    lwa_new-basic_end_date   = lwa_caufv-gltrp.
    lwa_new-quantity         = lwa_caufv-gamng.
    lwa_new-sales_order      = lwa_caufv-kdauf.
    lwa_new-sales_order_item = lwa_caufv-kdpos.
    lwa_new-prod_version     = lwa_caufv-verid.
    lwa_new-sequence_number  = lv_seqnr.

    CALL FUNCTION 'BAPI_PRODORD_CREATE'
      EXPORTING
        orderdata    = lwa_new
      IMPORTING
        order_number = lv_aufnr.

    lwa_mod-zzcolor          = lwa_caufv-zzcolor.
    lwa_mod-zzst_toc         = lwa_caufv-zzst_toc.
    lwa_mod-basic_start_date = lwa_caufv-gstrp.
    lwa_mod-basic_end_date   = lwa_caufv-gltrp.

    lwa_modx-zzcolor          = 'X'.
    lwa_modx-zzst_toc         = 'X'.
    lwa_modx-basic_start_date = 'X'.
    lwa_modx-basic_end_date   = 'X'.

    WHILE lv_loop LE 300.
      CALL FUNCTION 'ENQUEUE_ESORDER'
        EXPORTING
          aufnr          = lv_aufnr
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'DEQUEUE_ESORDER'
          EXPORTING
            aufnr = lwa_caufv-aufnr.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
        ADD 1 TO lv_loop.
      ENDIF.
    ENDWHILE.

    CALL FUNCTION 'BAPI_PRODORD_CHANGE'
      EXPORTING
        number     = lv_aufnr
        orderdata  = lwa_mod
        orderdatax = lwa_modx.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*      EXPORTING
*        wait = 'X'.

    lv_txt = TEXT-057.
    REPLACE '&' WITH lv_aufnr INTO lv_txt.

    LOOP AT gt_log ASSIGNING <fs_log> WHERE notif EQ TEXT-067 "'Sí'
                                        AND aufnr EQ lwa_caufv-aufnr.
      READ TABLE gt_ordenes TRANSPORTING NO FIELDS WITH KEY nro          = <fs_log>-nro
                                                            zztipo_notif = gv_recha.
      IF sy-subrc EQ 0.
        <fs_log>-msg = lv_txt.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ASIGNAR_LOTE_VIDRIO
*&---------------------------------------------------------------------*
FORM f_asignar_lote_vidrio USING p_show.
  "Sin componentes
  CHECK zppe_notif_addreg-no_material IS INITIAL.

  CASE gv_consumo_lamina.
    WHEN '1'.
      PERFORM f_consumo_lamina_1 USING p_show.
    WHEN '2'.
      PERFORM f_consumo_lamina_2 USING p_show.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB_0600
*&---------------------------------------------------------------------*
FORM f_add_tab_0600 .
  DATA: lv_error TYPE c,
        lv_txt   TYPE string.

  CALL METHOD cl_gui_cfw=>set_new_ok_code
    EXPORTING
      new_code = 'DUMMY'.

  CALL METHOD gr_grid_0600->check_changed_data.

  SELECT afko~aufnr,
         afvc~vornr,
         afvu~usr02
    INTO TABLE @DATA(lt_data)
    FROM afko INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN afvu ON afvc~aufpl EQ afvu~aufpl AND
                                 afvc~aplzl EQ afvu~aplzl
    FOR ALL ENTRIES IN @gt_ordenes
    WHERE afko~aufnr EQ @gt_ordenes-aufnr
      AND afvc~vornr EQ @gt_ordenes-vornr
      AND afvu~slwid EQ '0000001'.

  SELECT mchb~matnr,
         mchb~lgort,
         mchb~charg
    INTO TABLE @DATA(lt_lote)
    FROM mchb
    FOR ALL ENTRIES IN @gt_lot_vidrio
    WHERE werks EQ @p_werks
      AND charg EQ @gt_lot_vidrio-charg.

  CLEAR: lv_error.
  LOOP AT gt_ordenes INTO gwa_ordenes.
    READ TABLE lt_data INTO DATA(lwa_data) WITH KEY aufnr = gwa_ordenes-aufnr
                                                    vornr = gwa_ordenes-vornr.
    CHECK sy-subrc EQ 0.

    READ TABLE gt_lot_vidrio INTO gwa_lot_vidrio WITH KEY lamina = lwa_data-usr02+04(2).
    CHECK sy-subrc             EQ 0           AND
          gwa_lot_vidrio-charg IS NOT INITIAL.

    READ TABLE gt_componentes INTO gwa_componentes WITH KEY nro = gwa_ordenes-nro.
    CHECK sy-subrc EQ 0.

    READ TABLE lt_lote INTO DATA(lwa_lote) WITH KEY matnr = gwa_componentes-componente
                                                    lgort = gwa_componentes-almacen
                                                    charg = gwa_lot_vidrio-charg.
    IF sy-subrc NE 0.
      lv_error = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF lv_error EQ 'X'.
    lv_txt = TEXT-061.
    REPLACE '&' WITH gwa_lot_vidrio-lamina INTO lv_txt.

    MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT gt_ordenes INTO gwa_ordenes.
    READ TABLE lt_data INTO lwa_data WITH KEY aufnr = gwa_ordenes-aufnr
                                              vornr = gwa_ordenes-vornr.
    CHECK sy-subrc EQ 0.

    READ TABLE gt_lot_vidrio INTO gwa_lot_vidrio WITH KEY lamina = lwa_data-usr02+4(2).
    CHECK sy-subrc EQ 0.

    LOOP AT gt_componentes ASSIGNING <fs_componentes> WHERE nro EQ gwa_ordenes-nro.
      <fs_componentes>-lote = gwa_lot_vidrio-charg.
      <fs_componentes>-mod  = 'X'.
    ENDLOOP.
  ENDLOOP.

  gv_mod_lote = 'X'.
  PERFORM f_tratar_cambio_lote.

  CALL METHOD gr_grid02->refresh_table_display
    EXPORTING
      is_stable = gc_stable.

  MESSAGE TEXT-059 TYPE 'S'.
  LEAVE TO SCREEN 0.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_REGISTER_EVENT_0600
*&---------------------------------------------------------------------*
FORM f_register_event_0600 .
  DATA: lv_row    TYPE lvc_s_row,
        lv_col    TYPE lvc_s_col,
        lv_row_no TYPE lvc_s_roid.
  CALL METHOD gr_grid_0600->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  CREATE OBJECT gr_events_0600.
  SET HANDLER gr_events_0600->handle_toolbar FOR gr_grid_0600.

  CALL METHOD gr_grid_0600->set_toolbar_interactive.

  lv_col-fieldname = 'CHARG'.
  lv_row_no-row_id = 1.
  lv_row-index     = 1.

  CALL METHOD gr_grid_0600->set_current_cell_via_id
    EXPORTING
      is_column_id = lv_col
      is_row_no    = lv_row_no.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_REGISTER_EVENT_0601
*&---------------------------------------------------------------------*
FORM f_register_event_0601 .
  DATA: lv_row    TYPE lvc_s_row,
        lv_col    TYPE lvc_s_col,
        lv_row_no TYPE lvc_s_roid.
  CALL METHOD gr_grid_0601->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  CREATE OBJECT gr_events_0601.
  SET HANDLER gr_events_0601->handle_toolbar      FOR gr_grid_0601.
  SET HANDLER gr_events_0601->handle_data_changed FOR gr_grid_0601.
  SET HANDLER gr_events_0601->handle_on_f4        FOR gr_grid_0601.

  CALL METHOD gr_grid_0601->set_toolbar_interactive.

  lv_col-fieldname = 'CHARG'.
  lv_row_no-row_id = 1.
  lv_row-index     = 1.

  CALL METHOD gr_grid_0601->set_current_cell_via_id
    EXPORTING
      is_column_id = lv_col
      is_row_no    = lv_row_no.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ACT_HRS_PARADA
*&---------------------------------------------------------------------*
FORM f_act_hrs_parada .
  CHECK gv_updt_hrs_parada EQ 'X'.

  SUBMIT zppc0001
    WITH p_werks = p_werks
    AND RETURN.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SPLIT_DESTINO
*&---------------------------------------------------------------------*
FORM f_split_destino  USING    p_destino
                      CHANGING p_arbpl
                               p_ktsch.

  DATA: lt_results TYPE match_result_tab WITH HEADER LINE.

  CLEAR:   p_arbpl,
           p_ktsch.
  REFRESH: lt_results.

  FIND ALL OCCURRENCES OF '-' IN p_destino RESULTS lt_results[].

  LOOP AT lt_results.
    SELECT SINGLE COUNT(*)
      FROM crhd
      WHERE arbpl = p_destino(lt_results-offset).
    IF sy-subrc EQ 0.
      p_arbpl = p_destino(lt_results-offset).
      ADD 1 TO lt_results-offset.
      p_ktsch = p_destino+lt_results-offset.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CONSUMO_LAMINA_1
*&---------------------------------------------------------------------*
FORM f_consumo_lamina_1 USING p_show.
  DATA: lt_lot_vidrio TYPE tt_lot_vidrio WITH HEADER LINE.

  lt_lot_vidrio[]   = gt_lot_vidrio.
  gt_lot_vidrio_aux = gt_lot_vidrio.

  REFRESH: gt_lot_vidrio.

  SELECT  afvu~usr02
    INTO TABLE @DATA(lt_data)
    FROM afko INNER JOIN afvc ON afko~aufpl EQ afvc~aufpl
              INNER JOIN afvu ON afvc~aufpl EQ afvu~aufpl AND
                                 afvc~aplzl EQ afvu~aplzl

    FOR ALL ENTRIES IN @gt_ordenes
    WHERE afko~aufnr EQ @gt_ordenes-aufnr
      AND afvc~vornr EQ @gt_ordenes-vornr
      AND afvu~slwid EQ '0000001'.

  LOOP AT lt_data INTO DATA(lwa_data).
    CLEAR: gwa_lot_vidrio.

    READ TABLE lt_lot_vidrio WITH KEY lamina = lwa_data-usr02+4(2).
    IF sy-subrc EQ 0.
      gwa_lot_vidrio = lt_lot_vidrio.
    ELSE.
      gwa_lot_vidrio-lamina = lwa_data-usr02+4(2).
    ENDIF.
    APPEND gwa_lot_vidrio TO gt_lot_vidrio.
  ENDLOOP.

  SORT gt_lot_vidrio BY lamina ASCENDING
                        charg DESCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_lot_vidrio.

  IF p_show EQ 'X'.
    CALL SCREEN 0600 STARTING AT 5  1.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CONSUMO_LAMINA_2
*&---------------------------------------------------------------------*
FORM f_consumo_lamina_2 USING p_show.
  DATA: lt_lot_vidrio_2 TYPE tt_lot_vidrio_2 WITH HEADER LINE,
        lr_matnr        TYPE RANGE OF matnr  WITH HEADER LINE.

  lt_lot_vidrio_2[]   = gt_lot_vidrio_2.
  gt_lot_vidrio_aux_2 = gt_lot_vidrio_2.

  REFRESH: gt_lot_vidrio_2,
           lr_matnr.

  lr_matnr-sign   = 'I'.
  lr_matnr-option = 'EQ'.
  LOOP AT gt_componentes INTO gwa_componentes.
    lr_matnr-low = gwa_componentes-componente.
    APPEND lr_matnr.
  ENDLOOP.
  SORT lr_matnr BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lr_matnr COMPARING low.

  SELECT matnr
    INTO TABLE @DATA(lt_mara)
    FROM mara
    WHERE matnr IN @lr_matnr
      AND xchpf EQ 'X'.

  LOOP AT gt_componentes INTO gwa_componentes.
    READ TABLE lt_mara TRANSPORTING NO FIELDS WITH KEY matnr = gwa_componentes-componente.
    CHECK sy-subrc EQ 0.

    READ TABLE gt_lot_vidrio_2 ASSIGNING <fs_lot_vidrio_2> WITH KEY matnr = gwa_componentes-componente.
    IF sy-subrc NE 0.
      INSERT INITIAL LINE INTO gt_lot_vidrio_2 ASSIGNING <fs_lot_vidrio_2> INDEX 1.
      <fs_lot_vidrio_2>-matnr = gwa_componentes-componente.
      <fs_lot_vidrio_2>-maktx = gwa_componentes-descripcion.
      <fs_lot_vidrio_2>-mseh6 = gwa_componentes-um_txt.
    ENDIF.

    ADD 1                        TO <fs_lot_vidrio_2>-cant.
    ADD gwa_componentes-cantidad TO <fs_lot_vidrio_2>-erfmg.

    READ TABLE lt_lot_vidrio_2 WITH KEY matnr = <fs_lot_vidrio_2>-matnr.
    IF sy-subrc EQ 0.
      <fs_lot_vidrio_2>-charg = lt_lot_vidrio_2-charg.
    ENDIF.
  ENDLOOP.

  SORT gt_lot_vidrio_2 BY matnr ASCENDING.

  IF p_show EQ 'X'.
    CALL SCREEN 0601 STARTING AT 5  1.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TAB_0601
*&---------------------------------------------------------------------*
FORM f_add_tab_0601 .
  DATA: lv_error TYPE c,
        lv_txt   TYPE string,
        lv_val   TYPE c,
        lr_matnr TYPE RANGE OF matnr WITH HEADER LINE.

  CALL METHOD cl_gui_cfw=>set_new_ok_code
    EXPORTING
      new_code = 'DUMMY'.

  CALL METHOD gr_grid_0601->check_changed_data.

  REFRESH: lr_matnr.
  lr_matnr-sign   = 'I'.
  lr_matnr-option = 'EQ'.
  LOOP AT gt_componentes INTO gwa_componentes.
    lr_matnr-low = gwa_componentes-componente.
    APPEND lr_matnr.
  ENDLOOP.
  SORT lr_matnr BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lr_matnr COMPARING low.

  SELECT matnr,
         lgort,
         charg
    INTO TABLE @DATA(lt_mchb)
    FROM mchb
    WHERE matnr IN @lr_matnr
      AND werks EQ @p_werks.

  CLEAR: lv_error.

  LOOP AT gt_lot_vidrio_2 INTO gwa_lot_vidrio_2 WHERE modif EQ 'X'.
    CLEAR: lv_val.

    LOOP AT gt_componentes ASSIGNING <fs_componentes> WHERE componente = gwa_lot_vidrio_2-matnr.
      IF lv_val                 IS INITIAL     AND
         gwa_lot_vidrio_2-charg IS NOT INITIAL.
        lv_val = 'X'.
        READ TABLE lt_mchb TRANSPORTING NO FIELDS WITH KEY matnr = gwa_lot_vidrio_2-matnr
                                                           lgort = <fs_componentes>-almacen
                                                           charg = gwa_lot_vidrio_2-charg.
        IF sy-subrc NE 0.
          lv_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

      <fs_componentes>-lote = gwa_lot_vidrio_2-charg.
      <fs_componentes>-mod  = 'X'.
    ENDLOOP.

    IF lv_error EQ 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF lv_error EQ 'X'.
    MESSAGE ID   'M7'
            TYPE 'E'
            NUMBER 042
            WITH <fs_componentes>-componente
                 p_werks
                 <fs_componentes>-almacen
                 gwa_lot_vidrio_2-charg
            INTO lv_txt.

    MESSAGE lv_txt TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  gv_mod_lote = 'X'.
  PERFORM f_tratar_cambio_lote.

  CALL METHOD gr_grid02->refresh_table_display
    EXPORTING
      is_stable = gc_stable.

  READ TABLE gt_lot_vidrio_2 TRANSPORTING NO FIELDS WITH KEY modif = 'X'.
  IF sy-subrc EQ 0.
    MESSAGE TEXT-059 TYPE 'S'.
  ENDIF.
  LEAVE TO SCREEN 0.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_TAB0601_F4_LOTE
*&---------------------------------------------------------------------*
FORM f_tab0601_f4_lote USING    p_row
                       CHANGING p_modi TYPE lvc_s_modi
                                p_ok.
  DATA: lt_data TYPE tt_mcha          WITH HEADER LINE,
        lt_ret  TYPE tt_ddshretval     WITH HEADER LINE,
        lt_dest TYPE tt_zzdestino_data WITH HEADER LINE.

  CLEAR:   p_modi,
           p_ok.
  REFRESH: lt_data,
           lt_ret,
           lt_dest.

  READ TABLE gt_lot_vidrio_2 ASSIGNING <fs_lot_vidrio_2> INDEX p_row.
  READ TABLE gt_componentes INTO gwa_componentes WITH KEY componente = <fs_lot_vidrio_2>-matnr.
  PERFORM valreq_tab02_lote USING    <fs_lot_vidrio_2>-matnr
                                     gwa_componentes-almacen
                            CHANGING <fs_lot_vidrio_2>-charg
                                     p_ok.
  IF p_ok EQ 'X'.
    p_modi-row_id        = p_row.
    p_modi-fieldname     = 'CHARG'.
    p_modi-value         = <fs_lot_vidrio_2>-charg.
  ENDIF.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_TRATAR_CAMBIO_LOTE
*&---------------------------------------------------------------------*
FORM f_tratar_cambio_lote.
  DATA: lt_mara        TYPE tt_mara        WITH HEADER LINE,
        lt_componentes TYPE tt_componentes WITH HEADER LINE,
        lv_cant        TYPE erfmg,
        lv_menge       TYPE bstmg,
        lv_color       TYPE c LENGTH 4.

  CHECK gv_val_lote_comp EQ '1' AND
        gv_mod_lote      EQ 'X'.

  CLEAR:   gv_mod_lote.
  REFRESH: lt_mara,
           lt_componentes.

  LOOP AT gt_componentes ASSIGNING <fs_componentes>.
    IF <fs_componentes>-lote IS INITIAL.
      CLEAR: <fs_componentes>-color.
    ELSE.
      lt_mara-matnr = <fs_componentes>-componente.
      APPEND lt_mara.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_componentes INTO gwa_componentes WHERE bwart EQ '261'
                                                AND lote  IS NOT INITIAL.
    READ TABLE lt_componentes ASSIGNING FIELD-SYMBOL(<fs_componentes>) WITH KEY componente = gwa_componentes-componente
                                                                                almacen    = gwa_componentes-almacen
                                                                                lote       = gwa_componentes-lote.
    IF sy-subrc EQ 0.
      ADD gwa_componentes-cantidad TO <fs_componentes>-cantidad.
    ELSE.
      APPEND gwa_componentes TO lt_componentes.
    ENDIF.
  ENDLOOP.

  SELECT mchb~matnr,
         mchb~lgort,
         mchb~charg,
         mchb~clabs,
         mara~meins
    INTO TABLE @DATA(lt_mchb)
    FROM mchb INNER JOIN mara ON mchb~matnr EQ mara~matnr
    FOR ALL ENTRIES IN @lt_mara
    WHERE mchb~matnr EQ @lt_mara-matnr
      AND mchb~werks EQ @p_werks.
  LOOP AT lt_componentes.
    CLEAR: lv_color.

    READ TABLE lt_mchb INTO DATA(lwa_mchb) WITH KEY matnr = lt_componentes-componente
                                                    lgort = lt_componentes-almacen
                                                    charg = lt_componentes-lote.
    IF sy-subrc NE 0.
      lv_color = gc_amarillo.
    ENDIF.

    CLEAR: lv_menge.

    IF lwa_mchb-meins EQ lt_componentes-um.
      lv_menge = lwa_mchb-clabs.
    ELSE.
      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = lwa_mchb-matnr
          i_in_me              = lwa_mchb-meins
          i_out_me             = lt_componentes-um
          i_menge              = lwa_mchb-clabs
        IMPORTING
          e_menge              = lv_menge
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.
    ENDIF.

    IF lt_componentes-cantidad GT lv_menge.
      lv_color = gc_amarillo.
    ENDIF.

    LOOP AT gt_componentes ASSIGNING <fs_componentes> WHERE componente EQ lt_componentes-componente
                                                        AND almacen    EQ lt_componentes-almacen
                                                        AND lote       EQ lt_componentes-lote.
      <fs_componentes>-color = lv_color.
    ENDLOOP.
  ENDLOOP.

*  "Valida lote anterior
*  IF p_componentes-lote IS NOT INITIAL.
*    CLEAR:   lv_cant.
*
*    LOOP AT gt_componentes INTO DATA(lwa_comp) WHERE bwart      EQ '261'
*                                                 AND componente EQ p_componentes-componente
*                                                 AND almacen    EQ p_componentes-almacen
*                                                 AND lote       EQ p_componentes-lote.
*      ADD lwa_comp-cantidad TO lv_cant.
*    ENDLOOP.
*    SUBTRACT p_componentes-cantidad FROM lv_cant.
*
*    SELECT SINGLE mchb~clabs,
*                  mara~matnr,
*                  mara~meins
*      INTO @DATA(lwa_mchb)
*      FROM mchb INNER JOIN mara ON mchb~matnr EQ mara~matnr
*      WHERE mchb~matnr EQ @p_componentes-componente
*        AND mchb~werks EQ @p_werks
*        AND mchb~lgort EQ @p_componentes-almacen
*        AND mchb~charg EQ @p_componentes-lote.
*
*    IF sy-subrc NE 0.
*      LOOP AT gt_componentes ASSIGNING FIELD-SYMBOL(<fs_aux_comp>) WHERE bwart      EQ '261'
*                                                                     AND componente EQ p_componentes-componente
*                                                                     AND almacen    EQ p_componentes-almacen
*                                                                     AND lote       EQ p_componentes-lote.
*        <fs_aux_comp>-color = gc_amarillo.
*      ENDLOOP.
*    ELSE.
*      IF lwa_mchb-meins EQ p_componentes-um.
*        lv_menge = lwa_mchb-clabs.
*      ELSE.
*        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*          EXPORTING
*            i_matnr              = lwa_mchb-matnr
*            i_in_me              = lwa_mchb-meins
*            i_out_me             = p_componentes-um
*            i_menge              = lwa_mchb-clabs
*          IMPORTING
*            e_menge              = lv_menge
*          EXCEPTIONS
*            error_in_application = 1
*            error                = 2
*            OTHERS               = 3.
*      ENDIF.
*
*      IF lv_cant GT lv_menge.
*        LOOP AT gt_componentes ASSIGNING <fs_aux_comp> WHERE bwart      EQ '261'
*                                                         AND componente EQ p_componentes-componente
*                                                         AND almacen    EQ p_componentes-almacen
*                                                         AND lote       EQ p_componentes-lote.
*          <fs_aux_comp>-color = gc_amarillo.
*        ENDLOOP.
*      ELSE.
*        LOOP AT gt_componentes ASSIGNING <fs_aux_comp> WHERE bwart      EQ '261'
*                                                         AND componente EQ p_componentes-componente
*                                                         AND almacen    EQ p_componentes-almacen
*                                                         AND lote       EQ p_componentes-lote.
*          CLEAR: <fs_aux_comp>-color.
*        ENDLOOP.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  "Valida nuevo lote
*  IF p_charg IS INITIAL.
*    CLEAR: <fs_componentes>-color.
*  ELSE.
*    CLEAR:   lv_cant.
*
*    LOOP AT gt_componentes INTO lwa_comp WHERE bwart      EQ '261'
*                                           AND componente EQ p_componentes-componente
*                                           AND almacen    EQ p_componentes-almacen
*                                           AND lote       EQ p_charg.
*      CHECK sy-tabix NE p_index.
*
*      ADD lwa_comp-cantidad TO lv_cant.
*    ENDLOOP.
*    ADD p_componentes-cantidad TO lv_cant.
*
*    SELECT SINGLE mchb~clabs,
*                  mara~matnr,
*                  mara~meins
*      INTO @lwa_mchb
*      FROM mchb INNER JOIN mara ON mchb~matnr EQ mara~matnr
*      WHERE mchb~matnr EQ @p_componentes-componente
*        AND mchb~werks EQ @p_werks
*        AND mchb~lgort EQ @p_componentes-almacen
*        AND mchb~charg EQ @p_charg.
*
*    IF sy-subrc NE 0.
*      <fs_componentes>-color = gc_amarillo.
*
*      LOOP AT gt_componentes ASSIGNING <fs_aux_comp> WHERE bwart      EQ '261'
*                                                       AND componente EQ p_componentes-componente
*                                                       AND almacen    EQ p_componentes-almacen
*                                                       AND lote       EQ p_charg.
*        CHECK sy-tabix NE p_index.
*
*        <fs_aux_comp>-color = gc_amarillo.
*      ENDLOOP.
*    ELSE.
*      IF lwa_mchb-meins EQ p_componentes-um.
*        lv_menge = lwa_mchb-clabs.
*      ELSE.
*        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*          EXPORTING
*            i_matnr              = lwa_mchb-matnr
*            i_in_me              = lwa_mchb-meins
*            i_out_me             = p_componentes-um
*            i_menge              = lwa_mchb-clabs
*          IMPORTING
*            e_menge              = lv_menge
*          EXCEPTIONS
*            error_in_application = 1
*            error                = 2
*            OTHERS               = 3.
*      ENDIF.
*
*      IF lv_cant GT lv_menge.
*        <fs_componentes>-color = gc_amarillo.
*
*        LOOP AT gt_componentes ASSIGNING <fs_aux_comp> WHERE bwart      EQ '261'
*                                                         AND componente EQ p_componentes-componente
*                                                         AND almacen    EQ p_componentes-almacen
*                                                         AND lote       EQ p_charg.
*          CHECK sy-tabix NE p_index.
*
*          <fs_aux_comp>-color = gc_amarillo.
*        ENDLOOP.
*      ELSE.
*        CLEAR: <fs_componentes>-color.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
  CALL METHOD gr_grid02->refresh_table_display
    EXPORTING
      is_stable = gc_stable.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_NOTIFICAR_DELTA_ANUL
*&---------------------------------------------------------------------*
FORM f_notificar_delta_anul USING p_aufnr
                                  p_vornr
                                  p_ktsch
                                  p_budat
                                  p_plnfl
                                  p_gmein
                                  p_lmnga
                                  p_lote
                                  p_tipo_notif.
  DATA: lt_tab    TYPE zpptt_notif_mfinput WITH HEADER LINE,
        lt_output TYPE zpptt_notif_mfoutput.

  CLEAR:   lt_tab.
  REFRESH: lt_tab.


  lt_tab-werks      = p_werks.
  lt_tab-arbpl      = p_arbpl.
  lt_tab-pernr      = p_pernr.
  lt_tab-nro        = '1'.
  lt_tab-aufnr      = p_aufnr.
  lt_tab-vornr      = p_vornr.
  lt_tab-ktsch      = p_ktsch.
  lt_tab-budat      = p_budat.
  lt_tab-aplfl      = p_plnfl.
  lt_tab-meinh      = p_gmein.
  lt_tab-lmnga      = abs( p_lmnga ).
  lt_tab-lote       = p_lote.
  lt_tab-tipo_notif = p_tipo_notif.
  APPEND  lt_tab.

  CALL FUNCTION 'Z_PP_NOTIFICA_AUTO_SCADA'
    EXPORTING
      i_ordenes   = lt_tab[]
    IMPORTING
      e_resultado = lt_output.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FIELDCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fieldcat.

* Número de orden
  CLEAR gt_fieldcat.
  gt_fieldcat-fieldname   = 'ICON'.
*  gt_fieldcat-seltext_m   = 'Semaforo'.
*  gt_fieldcat-seltext_l   = 'Semaforo'.
  gt_fieldcat-col_pos     = 0.
  gt_fieldcat-outputlen   = '6'.
  APPEND gt_fieldcat TO gt_fieldcat.

* Número de orden
  CLEAR gt_fieldcat.
  gt_fieldcat-fieldname   = 'AUFNR'.
  gt_fieldcat-key         = 'X'.
  gt_fieldcat-seltext_m   = 'Número orden'.
  gt_fieldcat-seltext_l   = 'Número orden'.
  gt_fieldcat-col_pos     = 1.
  gt_fieldcat-outputlen   = '15'.
  APPEND gt_fieldcat TO gt_fieldcat.

* Número de operación
  CLEAR gt_fieldcat.
  gt_fieldcat-fieldname   = 'VORNR'.
  gt_fieldcat-seltext_m   = 'Número operación'.
  gt_fieldcat-seltext_l   = 'Número operación'.
  gt_fieldcat-col_pos     = 2.
  gt_fieldcat-outputlen   = '10'.
  APPEND gt_fieldcat TO gt_fieldcat.

* Puesto de trabajo
  CLEAR gt_fieldcat.
  gt_fieldcat-fieldname   = 'ARBPL'.
  gt_fieldcat-seltext_m   = 'Puesto trabajo'.
  gt_fieldcat-seltext_l   = 'Puesto trabajo'.
  gt_fieldcat-col_pos     = 3.
  gt_fieldcat-outputlen   = '10'.
  APPEND gt_fieldcat TO gt_fieldcat.

* Clave de control
  CLEAR gt_fieldcat.
  gt_fieldcat-fieldname   = 'STEUS'.
  gt_fieldcat-seltext_m   = 'Clave control'.
  gt_fieldcat-seltext_l   = 'Clave control'.
  gt_fieldcat-col_pos     = 4.
  gt_fieldcat-outputlen   = '10'.
  APPEND gt_fieldcat TO gt_fieldcat.

* Clave de modelo
  CLEAR gt_fieldcat.
  gt_fieldcat-fieldname   = 'STEUS'.
  gt_fieldcat-seltext_m   = 'Clave modelo'.
  gt_fieldcat-seltext_l   = 'Clave modelo'.
  gt_fieldcat-col_pos     = 5.
  gt_fieldcat-outputlen   = '10'.
  APPEND gt_fieldcat TO gt_fieldcat.

* Clave de modelo
  CLEAR gt_fieldcat.
  gt_fieldcat-fieldname   = 'MSG'.
  gt_fieldcat-seltext_m   = 'Mensaje'.
  gt_fieldcat-seltext_l   = 'Mensaje'.
  gt_fieldcat-col_pos     = 6.
  gt_fieldcat-outputlen   = '50'.
  APPEND gt_fieldcat TO gt_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_alv .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_buffer_active    = 'X'
*     i_background_id    = 'ALV_BACKGROUND'
*     i_callback_top_of_page   = 'TOP_OF_PAGE'
      i_callback_program = sy-repid
*     i_callback_pf_status_set = 'PF_STATUS'
*     i_callback_user_command  = 'USER_COMMAND'
*     is_layout          = lf_layout
      it_fieldcat        = gt_fieldcat[]
*     it_special_groups  = lf_sp_group
      i_save             = 'X'
    TABLES
      t_outtab           = gt_outtab[].

ENDFORM.
