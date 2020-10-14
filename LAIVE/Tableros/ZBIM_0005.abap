*----------------------------------------------------------------------------------------------------*
* Tipo de Objeto        : Programa                                                                   *
* Nombre de Programa    : ZBIM_0005                                                                  *
*                                                                                                    *
* Nombre de Proyecto    : Informe de producción                                                      *
* Fecha                 : 28.05.2020                                                                 *
* Compañía              : Everis Perú S.A.C.                                                         *
* Consultor BI          : Carlos Gálvez.                                                              *
*                                                                                                    *
* Descripción General del Proceso: Programa de obtención órdenes de proceso (Real).                  *
*----------------------------------------------------------------------------------------------------*
* Modificaciones:                                                                                    *
*                                                                                                    *
*    Fecha            Autor                    Descripción               Marca          Compañía     *
* dd.mm.aaaa     xxxxxxxxx xxxxxx      xxxxxxxxxxxxxxxxxxxxxxxxxxxx     XXXXXXX     XXXXXXXXXXXXXXXX *
*----------------------------------------------------------------------------------------------------*

REPORT zbim_0005.

*----------------------------------------------------------------------------------------------------*
* NOTAS:                                                                                             *
* - El nivel que se guarda es el nivel de la CDS (z_d_orden_proceso_co_var) + 1, ya que es necesario *
*   que SAP Analytics Cloud reciba desde 1 en adelante (no puede recibir 0).                         *
*----------------------------------------------------------------------------------------------------*

TYPES:
  BEGIN OF lty_jerarquia,
    nivel             TYPE numc2,
    orden_proceso     TYPE aufnr,
    materialprincipal TYPE matnr,
    material1         TYPE matnr,
    material2         TYPE matnr,
    material3         TYPE matnr,
    material4         TYPE matnr,
    material5         TYPE matnr,
    material6         TYPE matnr,
    material7         TYPE matnr,
  END OF lty_jerarquia.

DATA: ltd_conglomerado TYPE STANDARD TABLE OF zbit_ord_proc_co,
      "EMAT - Enrique Aguirre - 21.07.2020 - Comentar
      wa_conglomerado  LIKE LINE OF ltd_conglomerado,
      wa_conglomerado_cero LIKE LINE OF ltd_conglomerado,
      wa_conglomerado_ceroS LIKE LINE OF ltd_conglomerado,
      "EMAT - Enrique Aguirre - 21.07.2020 - Comentar
      lwa_jerarquia    TYPE lty_jerarquia,
      lp_cantidad_conv TYPE bstmg,
      gt_stb           TYPE STANDARD TABLE OF stpox.

IF NOT cl_abap_dbfeatures=>use_features(
     requested_features = VALUE #( ( cl_abap_dbfeatures=>amdp_table_function ) )
   ).
  MESSAGE 'DB Feature no soportado' TYPE 'E'.
  RETURN.
ENDIF.

"EMAT - Enrique Aguirre - 21.07.2020 - Comentar
*SELECT SINGLE valor_low, valor_high
*  FROM zconstantes
*  INTO @DATA(ls_constantes)
*  WHERE zmodulo    EQ 'BI'        AND
*        programm   EQ 'ZBIM_0001' AND
*        fieldname  EQ 'ERDAT'     AND
*        zsigno     EQ 'I'         AND
*        zopcion    EQ 'BT'        AND
*        zsecuencia EQ '01'.
"EMAT - Enrique Aguirre - 21.07.2020 - Comentar

" NUEVA LÓGICA
TABLES: aufk.
SELECT-OPTIONS: s_orden FOR aufk-aufnr.

"----------------------------------------------
  SELECT SINGLE valor_low, valor_high
  FROM zconstantes
  INTO @DATA(ls_constantes)
  WHERE zmodulo    EQ 'BI'        AND
        programm   EQ 'ZBIM_0001' AND
        fieldname  EQ 'ERDAT'     AND
        zsigno     EQ 'I'         AND
        zopcion    EQ 'BT'        AND
        zsecuencia EQ '01'.
  IF sy-subrc NE 0.
    MESSAGE 'No se pudo leer configuración de fechas de ZCONSTANTES - BI - ZBIM_0001 - ERDAT' TYPE 'E'.
    RETURN.
  ENDIF.

  DELETE FROM zbit_ord_proc_co
  WHERE ORDEN_PROCESO IN @s_orden
        and fecha_entrada GE @ls_constantes-valor_low AND fecha_entrada LE @ls_constantes-valor_high.
 COMMIT WORK.

SELECT
    nive0~re_costo_total,
    nive0~re_mo_costo_real,
    nive0~re_ctd_consu_bom,
    nive0~re_um_ctd_consu_bom,
    nive0~re_anio_periodo,
    nive0~pl_costo_total,
    nive0~pl_mo_costo_total,

    nive0~UM_Cantidad_Orden,
    nive0~Cantidad_Orden,

    nive1~nivel                         AS nivel_1,
    nive1~orden_proceso                 AS orden_proceso_1,
    nive1~materialprincipal             AS materialprincipal_1,
    nive1~centro_prod                   AS centro_prod_1,

    nive1~material                      as material_1,
    nive2~material                      as material_2,
    nive3~material                      as material_3,
    nive4~material                      as material_4,
    nive5~material                      as material_5,
    nive6~material                      as material_6,
    nive7~material                      as material_7,

    nive1~componente                    as componente_1,
    nive2~componente                    as componente_2,
    nive3~componente                    as componente_3,
    nive4~componente                    as componente_4,
    nive5~componente                    as componente_5,
    nive6~componente                    as componente_6,
    nive7~componente                    as componente_7,

    nive1~fecha_entrada                 AS fecha_entrada_1,
    nive1~re_anio_periodo               AS re_anio_periodo_1,
    nive1~re_ctd_consu_bom              AS re_ctd_consu_bom_1,
    nive1~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_1,
    nive1~re_porc_solido_consumido      AS re_porc_solido_consumido_1,
    nive1~re_costo_total                AS re_costo_total_1,
    nive1~re_mo_costo_real              AS re_mo_costo_real_1,
    nive1~pl_ctd_consu_bom              AS pl_ctd_consu_bom_1,
    nive1~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_1,
    nive1~pl_costo_total                AS pl_costo_total_1,
    nive1~pl_porc_solido_consumido      AS pl_porc_solido_consumido_1,
    nive1~pl_mo_costo_total             AS pl_mo_costo_total_1,
    nive1~pl_ctd_base_orden             AS pl_ctd_base_orden_1,
    nive1~pl_um_base_orden              AS pl_um_base_orden_1,
    nive1~descripcion_material          AS descripcion_material_1,
    nive1~descripcion_componente        AS descripcion_componente_1,
    nive1~tipo_mat_material             AS tipo_mat_material_1,
    nive1~tipo_mat_componente           AS tipo_mat_componente_1,
    nive1~categoria_material            AS categoria_material_1,
    nive1~marca_material                AS marca_material_1,
    nive1~familia_material              AS familia_material_1,
    nive1~presentacion_material         AS presentacion_material_1,
    nive1~categoria_componente          AS categoria_componente_1,
    nive1~marca_componente              AS marca_componente_1,
    nive1~familia_componente            AS familia_componente_1,
    nive1~presentacion_componente       AS presentacion_componente_1,
    nive1~porc_solido_material          AS porc_solido_material_1,
    nive1~porc_solido_componente        AS porc_solido_componente_1,
    nive1~solido_lacteo                 AS solido_lacteo_1,
    nive1~orden_planta                  AS orden_planta_1,
    nive1~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_1,
    nive1~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_1,
    nive1~orden_porc_solido_real        AS orden_porc_solido_real_1,
    nive1~orden_ctd_total_real          AS orden_ctd_total_real_1,
    nive1~orden_ctd_total_plan          AS orden_ctd_total_plan_1,
    nive1~orden_porc_solido_plan        AS orden_porc_solido_plan_1,
    nive1~orden_um                      AS orden_um_1,
    nive1~orden_estado                  AS orden_estado_1,
    nive1~var_ctd_consu_bom             AS var_ctd_consu_bom_1,
    nive1~var_costo_total               AS var_costo_total_1,
    nive1~var_efecto                    AS var_efecto_1,
    nive1~var_solido_consumido          AS var_solido_consumido_1,
    nive1~var_merma                     AS var_merma_1,
    nive1~orden_variacion_consumo       AS orden_variacion_consumo_1,
    nive1~orden_variacion_solidos       AS orden_variacion_solidos_1,
    nive1~orden_porc_merma              AS orden_porc_merma_1,

    nive2~nivel                         AS nivel_2,
    nive2~orden_proceso                 AS orden_proceso_2,
    nive2~materialprincipal             AS materialprincipal_2,
    nive2~centro_prod                   AS centro_prod_2,
    nive2~fecha_entrada                 AS fecha_entrada_2,
    nive2~re_anio_periodo               AS re_anio_periodo_2,
    nive2~re_ctd_consu_bom              AS re_ctd_consu_bom_2,
    nive2~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_2,
    nive2~re_porc_solido_consumido      AS re_porc_solido_consumido_2,
    nive2~re_costo_total                AS re_costo_total_2,
    nive2~re_mo_costo_real              AS re_mo_costo_real_2,
    nive2~pl_ctd_consu_bom              AS pl_ctd_consu_bom_2,
    nive2~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_2,
    nive2~pl_costo_total                AS pl_costo_total_2,
    nive2~pl_porc_solido_consumido      AS pl_porc_solido_consumido_2,
    nive2~pl_mo_costo_total             AS pl_mo_costo_total_2,
    nive2~pl_ctd_base_orden             AS pl_ctd_base_orden_2,
    nive2~pl_um_base_orden              AS pl_um_base_orden_2,
    nive2~descripcion_material          AS descripcion_material_2,
    nive2~descripcion_componente        AS descripcion_componente_2,
    nive2~tipo_mat_material             AS tipo_mat_material_2,
    nive2~tipo_mat_componente           AS tipo_mat_componente_2,
    nive2~categoria_material            AS categoria_material_2,
    nive2~marca_material                AS marca_material_2,
    nive2~familia_material              AS familia_material_2,
    nive2~presentacion_material         AS presentacion_material_2,
    nive2~categoria_componente          AS categoria_componente_2,
    nive2~marca_componente              AS marca_componente_2,
    nive2~familia_componente            AS familia_componente_2,
    nive2~presentacion_componente       AS presentacion_componente_2,
    nive2~porc_solido_material          AS porc_solido_material_2,
    nive2~porc_solido_componente        AS porc_solido_componente_2,
    nive2~solido_lacteo                 AS solido_lacteo_2,
    nive2~orden_planta                  AS orden_planta_2,
    nive2~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_2,
    nive2~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_2,
    nive2~orden_porc_solido_real        AS orden_porc_solido_real_2,
    nive2~orden_ctd_total_real          AS orden_ctd_total_real_2,
    nive2~orden_ctd_total_plan          AS orden_ctd_total_plan_2,
    nive2~orden_porc_solido_plan        AS orden_porc_solido_plan_2,
    nive2~orden_um                      AS orden_um_2,
    nive2~orden_estado                  AS orden_estado_2,
    nive2~var_ctd_consu_bom             AS var_ctd_consu_bom_2,
    nive2~var_costo_total               AS var_costo_total_2,
    nive2~var_efecto                    AS var_efecto_2,
    nive2~var_solido_consumido          AS var_solido_consumido_2,
    nive2~var_merma                     AS var_merma_2,
    nive2~orden_variacion_consumo       AS orden_variacion_consumo_2,
    nive2~orden_variacion_solidos       AS orden_variacion_solidos_2,
    nive2~orden_porc_merma              AS orden_porc_merma_2,

    nive3~nivel                         AS nivel_3,
    nive3~orden_proceso                 AS orden_proceso_3,
    nive3~materialprincipal             AS materialprincipal_3,
    nive3~centro_prod                   AS centro_prod_3,
    nive3~fecha_entrada                 AS fecha_entrada_3,
    nive3~re_anio_periodo               AS re_anio_periodo_3,
    nive3~re_ctd_consu_bom              AS re_ctd_consu_bom_3,
    nive3~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_3,
    nive3~re_porc_solido_consumido      AS re_porc_solido_consumido_3,
    nive3~re_costo_total                AS re_costo_total_3,
    nive3~re_mo_costo_real              AS re_mo_costo_real_3,
    nive3~pl_ctd_consu_bom              AS pl_ctd_consu_bom_3,
    nive3~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_3,
    nive3~pl_costo_total                AS pl_costo_total_3,
    nive3~pl_porc_solido_consumido      AS pl_porc_solido_consumido_3,
    nive3~pl_mo_costo_total             AS pl_mo_costo_total_3,
    nive3~pl_ctd_base_orden             AS pl_ctd_base_orden_3,
    nive3~pl_um_base_orden              AS pl_um_base_orden_3,
    nive3~descripcion_material          AS descripcion_material_3,
    nive3~descripcion_componente        AS descripcion_componente_3,
    nive3~tipo_mat_material             AS tipo_mat_material_3,
    nive3~tipo_mat_componente           AS tipo_mat_componente_3,
    nive3~categoria_material            AS categoria_material_3,
    nive3~marca_material                AS marca_material_3,
    nive3~familia_material              AS familia_material_3,
    nive3~presentacion_material         AS presentacion_material_3,
    nive3~categoria_componente          AS categoria_componente_3,
    nive3~marca_componente              AS marca_componente_3,
    nive3~familia_componente            AS familia_componente_3,
    nive3~presentacion_componente       AS presentacion_componente_3,
    nive3~porc_solido_material          AS porc_solido_material_3,
    nive3~porc_solido_componente        AS porc_solido_componente_3,
    nive3~solido_lacteo                 AS solido_lacteo_3,
    nive3~orden_planta                  AS orden_planta_3,
    nive3~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_3,
    nive3~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_3,
    nive3~orden_porc_solido_real        AS orden_porc_solido_real_3,
    nive3~orden_ctd_total_real          AS orden_ctd_total_real_3,
    nive3~orden_ctd_total_plan          AS orden_ctd_total_plan_3,
    nive3~orden_porc_solido_plan        AS orden_porc_solido_plan_3,
    nive3~orden_um                      AS orden_um_3,
    nive3~orden_estado                  AS orden_estado_3,
    nive3~var_ctd_consu_bom             AS var_ctd_consu_bom_3,
    nive3~var_costo_total               AS var_costo_total_3,
    nive3~var_efecto                    AS var_efecto_3,
    nive3~var_solido_consumido          AS var_solido_consumido_3,
    nive3~var_merma                     AS var_merma_3,
    nive3~orden_variacion_consumo       AS orden_variacion_consumo_3,
    nive3~orden_variacion_solidos       AS orden_variacion_solidos_3,
    nive3~orden_porc_merma              AS orden_porc_merma_3,

    nive4~nivel                         AS nivel_4,
    nive4~orden_proceso                 AS orden_proceso_4,
    nive4~materialprincipal             AS materialprincipal_4,
    nive4~centro_prod                   AS centro_prod_4,
    nive4~fecha_entrada                 AS fecha_entrada_4,
    nive4~re_anio_periodo               AS re_anio_periodo_4,
    nive4~re_ctd_consu_bom              AS re_ctd_consu_bom_4,
    nive4~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_4,
    nive4~re_porc_solido_consumido      AS re_porc_solido_consumido_4,
    nive4~re_costo_total                AS re_costo_total_4,
    nive4~re_mo_costo_real              AS re_mo_costo_real_4,
    nive4~pl_ctd_consu_bom              AS pl_ctd_consu_bom_4,
    nive4~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_4,
    nive4~pl_costo_total                AS pl_costo_total_4,
    nive4~pl_porc_solido_consumido      AS pl_porc_solido_consumido_4,
    nive4~pl_mo_costo_total             AS pl_mo_costo_total_4,
    nive4~pl_ctd_base_orden             AS pl_ctd_base_orden_4,
    nive4~pl_um_base_orden              AS pl_um_base_orden_4,
    nive4~descripcion_material          AS descripcion_material_4,
    nive4~descripcion_componente        AS descripcion_componente_4,
    nive4~tipo_mat_material             AS tipo_mat_material_4,
    nive4~tipo_mat_componente           AS tipo_mat_componente_4,
    nive4~categoria_material            AS categoria_material_4,
    nive4~marca_material                AS marca_material_4,
    nive4~familia_material              AS familia_material_4,
    nive4~presentacion_material         AS presentacion_material_4,
    nive4~categoria_componente          AS categoria_componente_4,
    nive4~marca_componente              AS marca_componente_4,
    nive4~familia_componente            AS familia_componente_4,
    nive4~presentacion_componente       AS presentacion_componente_4,
    nive4~porc_solido_material          AS porc_solido_material_4,
    nive4~porc_solido_componente        AS porc_solido_componente_4,
    nive4~solido_lacteo                 AS solido_lacteo_4,
    nive4~orden_planta                  AS orden_planta_4,
    nive4~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_4,
    nive4~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_4,
    nive4~orden_porc_solido_real        AS orden_porc_solido_real_4,
    nive4~orden_ctd_total_real          AS orden_ctd_total_real_4,
    nive4~orden_ctd_total_plan          AS orden_ctd_total_plan_4,
    nive4~orden_porc_solido_plan        AS orden_porc_solido_plan_4,
    nive4~orden_um                      AS orden_um_4,
    nive4~orden_estado                  AS orden_estado_4,
    nive4~var_ctd_consu_bom             AS var_ctd_consu_bom_4,
    nive4~var_costo_total               AS var_costo_total_4,
    nive4~var_efecto                    AS var_efecto_4,
    nive4~var_solido_consumido          AS var_solido_consumido_4,
    nive4~var_merma                     AS var_merma_4,
    nive4~orden_variacion_consumo       AS orden_variacion_consumo_4,
    nive4~orden_variacion_solidos       AS orden_variacion_solidos_4,
    nive4~orden_porc_merma              AS orden_porc_merma_4,

    nive5~nivel                         AS nivel_5,
    nive5~orden_proceso                 AS orden_proceso_5,
    nive5~materialprincipal             AS materialprincipal_5,
    nive5~centro_prod                   AS centro_prod_5,
    nive5~fecha_entrada                 AS fecha_entrada_5,
    nive5~re_anio_periodo               AS re_anio_periodo_5,
    nive5~re_ctd_consu_bom              AS re_ctd_consu_bom_5,
    nive5~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_5,
    nive5~re_porc_solido_consumido      AS re_porc_solido_consumido_5,
    nive5~re_costo_total                AS re_costo_total_5,
    nive5~re_mo_costo_real              AS re_mo_costo_real_5,
    nive5~pl_ctd_consu_bom              AS pl_ctd_consu_bom_5,
    nive5~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_5,
    nive5~pl_costo_total                AS pl_costo_total_5,
    nive5~pl_porc_solido_consumido      AS pl_porc_solido_consumido_5,
    nive5~pl_mo_costo_total             AS pl_mo_costo_total_5,
    nive5~pl_ctd_base_orden             AS pl_ctd_base_orden_5,
    nive5~pl_um_base_orden              AS pl_um_base_orden_5,
    nive5~descripcion_material          AS descripcion_material_5,
    nive5~descripcion_componente        AS descripcion_componente_5,
    nive5~tipo_mat_material             AS tipo_mat_material_5,
    nive5~tipo_mat_componente           AS tipo_mat_componente_5,
    nive5~categoria_material            AS categoria_material_5,
    nive5~marca_material                AS marca_material_5,
    nive5~familia_material              AS familia_material_5,
    nive5~presentacion_material         AS presentacion_material_5,
    nive5~categoria_componente          AS categoria_componente_5,
    nive5~marca_componente              AS marca_componente_5,
    nive5~familia_componente            AS familia_componente_5,
    nive5~presentacion_componente       AS presentacion_componente_5,
    nive5~porc_solido_material          AS porc_solido_material_5,
    nive5~porc_solido_componente        AS porc_solido_componente_5,
    nive5~solido_lacteo                 AS solido_lacteo_5,
    nive5~orden_planta                  AS orden_planta_5,
    nive5~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_5,
    nive5~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_5,
    nive5~orden_porc_solido_real        AS orden_porc_solido_real_5,
    nive5~orden_ctd_total_real          AS orden_ctd_total_real_5,
    nive5~orden_ctd_total_plan          AS orden_ctd_total_plan_5,
    nive5~orden_porc_solido_plan        AS orden_porc_solido_plan_5,
    nive5~orden_um                      AS orden_um_5,
    nive5~orden_estado                  AS orden_estado_5,
    nive5~var_ctd_consu_bom             AS var_ctd_consu_bom_5,
    nive5~var_costo_total               AS var_costo_total_5,
    nive5~var_efecto                    AS var_efecto_5,
    nive5~var_solido_consumido          AS var_solido_consumido_5,
    nive5~var_merma                     AS var_merma_5,
    nive5~orden_variacion_consumo       AS orden_variacion_consumo_5,
    nive5~orden_variacion_solidos       AS orden_variacion_solidos_5,
    nive5~orden_porc_merma              AS orden_porc_merma_5,

    nive6~nivel                         AS nivel_6,
    nive6~orden_proceso                 AS orden_proceso_6,
    nive6~materialprincipal             AS materialprincipal_6,
    nive6~centro_prod                   AS centro_prod_6,
    nive6~fecha_entrada                 AS fecha_entrada_6,
    nive6~re_anio_periodo               AS re_anio_periodo_6,
    nive6~re_ctd_consu_bom              AS re_ctd_consu_bom_6,
    nive6~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_6,
    nive6~re_porc_solido_consumido      AS re_porc_solido_consumido_6,
    nive6~re_costo_total                AS re_costo_total_6,
    nive6~re_mo_costo_real              AS re_mo_costo_real_6,
    nive6~pl_ctd_consu_bom              AS pl_ctd_consu_bom_6,
    nive6~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_6,
    nive6~pl_costo_total                AS pl_costo_total_6,
    nive6~pl_porc_solido_consumido      AS pl_porc_solido_consumido_6,
    nive6~pl_mo_costo_total             AS pl_mo_costo_total_6,
    nive6~pl_ctd_base_orden             AS pl_ctd_base_orden_6,
    nive6~pl_um_base_orden              AS pl_um_base_orden_6,
    nive6~descripcion_material          AS descripcion_material_6,
    nive6~descripcion_componente        AS descripcion_componente_6,
    nive6~tipo_mat_material             AS tipo_mat_material_6,
    nive6~tipo_mat_componente           AS tipo_mat_componente_6,
    nive6~categoria_material            AS categoria_material_6,
    nive6~marca_material                AS marca_material_6,
    nive6~familia_material              AS familia_material_6,
    nive6~presentacion_material         AS presentacion_material_6,
    nive6~categoria_componente          AS categoria_componente_6,
    nive6~marca_componente              AS marca_componente_6,
    nive6~familia_componente            AS familia_componente_6,
    nive6~presentacion_componente       AS presentacion_componente_6,
    nive6~porc_solido_material          AS porc_solido_material_6,
    nive6~porc_solido_componente        AS porc_solido_componente_6,
    nive6~solido_lacteo                 AS solido_lacteo_6,
    nive6~orden_planta                  AS orden_planta_6,
    nive6~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_6,
    nive6~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_6,
    nive6~orden_porc_solido_real        AS orden_porc_solido_real_6,
    nive6~orden_ctd_total_real          AS orden_ctd_total_real_6,
    nive6~orden_ctd_total_plan          AS orden_ctd_total_plan_6,
    nive6~orden_porc_solido_plan        AS orden_porc_solido_plan_6,
    nive6~orden_um                      AS orden_um_6,
    nive6~orden_estado                  AS orden_estado_6,
    nive6~var_ctd_consu_bom             AS var_ctd_consu_bom_6,
    nive6~var_costo_total               AS var_costo_total_6,
    nive6~var_efecto                    AS var_efecto_6,
    nive6~var_solido_consumido          AS var_solido_consumido_6,
    nive6~var_merma                     AS var_merma_6,
    nive6~orden_variacion_consumo       AS orden_variacion_consumo_6,
    nive6~orden_variacion_solidos       AS orden_variacion_solidos_6,
    nive6~orden_porc_merma              AS orden_porc_merma_6,

    nive7~nivel                         AS nivel_7,
    nive7~orden_proceso                 AS orden_proceso_7,
    nive7~materialprincipal             AS materialprincipal_7,
    nive7~centro_prod                   AS centro_prod_7,
    nive7~fecha_entrada                 AS fecha_entrada_7,
    nive7~re_anio_periodo               AS re_anio_periodo_7,
    nive7~re_ctd_consu_bom              AS re_ctd_consu_bom_7,
    nive7~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_7,
    nive7~re_porc_solido_consumido      AS re_porc_solido_consumido_7,
    nive7~re_costo_total                AS re_costo_total_7,
    nive7~re_mo_costo_real              AS re_mo_costo_real_7,
    nive7~pl_ctd_consu_bom              AS pl_ctd_consu_bom_7,
    nive7~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_7,
    nive7~pl_costo_total                AS pl_costo_total_7,
    nive7~pl_porc_solido_consumido      AS pl_porc_solido_consumido_7,
    nive7~pl_mo_costo_total             AS pl_mo_costo_total_7,
    nive7~pl_ctd_base_orden             AS pl_ctd_base_orden_7,
    nive7~pl_um_base_orden              AS pl_um_base_orden_7,
    nive7~descripcion_material          AS descripcion_material_7,
    nive7~descripcion_componente        AS descripcion_componente_7,
    nive7~tipo_mat_material             AS tipo_mat_material_7,
    nive7~tipo_mat_componente           AS tipo_mat_componente_7,
    nive7~categoria_material            AS categoria_material_7,
    nive7~marca_material                AS marca_material_7,
    nive7~familia_material              AS familia_material_7,
    nive7~presentacion_material         AS presentacion_material_7,
    nive7~categoria_componente          AS categoria_componente_7,
    nive7~marca_componente              AS marca_componente_7,
    nive7~familia_componente            AS familia_componente_7,
    nive7~presentacion_componente       AS presentacion_componente_7,
    nive7~porc_solido_material          AS porc_solido_material_7,
    nive7~porc_solido_componente        AS porc_solido_componente_7,
    nive7~solido_lacteo                 AS solido_lacteo_7,
    nive7~orden_planta                  AS orden_planta_7,
    nive7~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_7,
    nive7~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_7,
    nive7~orden_porc_solido_real        AS orden_porc_solido_real_7,
    nive7~orden_ctd_total_real          AS orden_ctd_total_real_7,
    nive7~orden_ctd_total_plan          AS orden_ctd_total_plan_7,
    nive7~orden_porc_solido_plan        AS orden_porc_solido_plan_7,
    nive7~orden_um                      AS orden_um_7,
    nive7~orden_estado                  AS orden_estado_7,
    nive7~var_ctd_consu_bom             AS var_ctd_consu_bom_7,
    nive7~var_costo_total               AS var_costo_total_7,
    nive7~var_efecto                    AS var_efecto_7,
    nive7~var_solido_consumido          AS var_solido_consumido_7,
    nive7~var_merma                     AS var_merma_7,
    nive7~orden_variacion_consumo       AS orden_variacion_consumo_7,
    nive7~orden_variacion_solidos       AS orden_variacion_solidos_7,
    nive7~orden_porc_merma              AS orden_porc_merma_7
FROM  z_d_orden_proceso_co_var AS nive1
INNER JOIN zconstantes AS zconst
ON    nive1~fecha_entrada GE zconst~valor_low AND
      nive1~fecha_entrada LE zconst~valor_high AND
      zmodulo    EQ 'BI'        AND
      programm   EQ 'ZBIM_0001' AND
      fieldname  EQ 'ERDAT'     AND
      zsigno     EQ 'I'         AND
      zopcion    EQ 'BT'        AND
      zsecuencia EQ '01'
left outer join z_d_ordpro_info_plan_nv0 as nive0
on nive0~orden_proceso = nive1~orden_proceso and
   nive0~fecha_entrada GE zconst~valor_low AND
   nive0~fecha_entrada LE zconst~valor_high
left outer join z_d_orden_proceso_co_var as nive2
on  nive1~nivel = '01' and
    nive2~nivel = '02' and
    nive1~orden_proceso = nive2~orden_proceso and
    nive1~componente    = nive2~material      and
    nive1~centro_prod   = nive2~centro_prod   and
    nive1~fecha_entrada = nive2~fecha_entrada
left outer join z_d_orden_proceso_co_var as nive3
on  nive3~nivel = '03' and
    nive2~orden_proceso = nive3~orden_proceso and
    nive2~componente    = nive3~material      and
    nive2~centro_prod   = nive3~centro_prod   and
    nive2~fecha_entrada = nive3~fecha_entrada
left outer join z_d_orden_proceso_co_var as nive4
on  nive4~nivel = '04' and
    nive3~orden_proceso = nive4~orden_proceso and
    nive3~componente    = nive4~material      and
    nive3~centro_prod   = nive4~centro_prod   and
    nive3~fecha_entrada = nive4~fecha_entrada
left outer join z_d_orden_proceso_co_var as nive5
on  nive5~nivel = '05' and
    nive4~orden_proceso = nive5~orden_proceso and
    nive4~componente    = nive5~material      and
    nive4~centro_prod   = nive5~centro_prod   and
    nive4~fecha_entrada = nive5~fecha_entrada
left outer join z_d_orden_proceso_co_var as nive6
on  nive5~nivel = '06' and
    nive5~orden_proceso = nive6~orden_proceso and
    nive5~componente    = nive6~material      and
    nive5~centro_prod   = nive6~centro_prod   and
    nive5~fecha_entrada = nive6~fecha_entrada
left outer join z_d_orden_proceso_co_var as nive7
on  nive6~nivel = '07' and
    nive6~orden_proceso = nive7~orden_proceso and
    nive6~componente    = nive7~material      and
    nive6~centro_prod   = nive7~centro_prod   and
    nive6~fecha_entrada = nive7~fecha_entrada
ORDER BY nivel_1, orden_proceso_1, materialprincipal_1, centro_prod_1, material_1, fecha_entrada_1, componente_1, re_anio_periodo_1
INTO TABLE @DATA(it_zbim0005__)
UP TO 1 ROWS.

**********************************************************************
"Identificacion de periodos de todos los niveles
TYPES:BEGIN OF ty_perxniv,
         ORDEN_PROCESO TYPE ZE_BI_ORDEN_PROCESO,
         RE_ANIO_PERIODO TYPE ZE_BI_ANIO_PERIODO,
      END OF ty_perxniv.

DATA: it_ty_perxniv_ TYPE STANDARD TABLE OF ty_perxniv WITH HEADER LINE.

select Distinct
       orden_proceso,
       re_anio_periodo
FROM  z_d_orden_proceso_co_var AS nive1
INNER JOIN zconstantes AS zconst
ON    nive1~fecha_entrada GE zconst~valor_low AND
      nive1~fecha_entrada LE zconst~valor_high AND
      zmodulo    EQ 'BI'        AND
      programm   EQ 'ZBIM_0001' AND
      fieldname  EQ 'ERDAT'     AND
      zsigno     EQ 'I'         AND
      zopcion    EQ 'BT'        AND
      zsecuencia EQ '01'
WHERE  nive1~orden_proceso IN @s_orden
ORDER BY nive1~orden_proceso ASCENDING, re_anio_periodo ASCENDING
INTO CORRESPONDING FIELDS OF TABLE @it_ty_perxniv_.

**********************************************************************
"Sumatoria de los campos RE_PORC_SOLIDO_CONSUMIDO y PL_PORC_SOLIDO_CONSUMIDO
"de todos los niveles de una orden.
TYPES:BEGIN OF ty_sumniv0,
         ORDEN_PROCESO TYPE ZE_BI_ORDEN_PROCESO,
         RE_ANIO_PERIODO TYPE ZE_BI_ANIO_PERIODO,
         RE_PORC_SOLIDO_CONSUMIDO TYPE ZE_BI_PORC_SOLIDOS,
         PL_PORC_SOLIDO_CONSUMIDO TYPE ZE_BI_PORC_SOLIDOS,
      END OF TY_SUMNIV0.

DATA: it_sumniv0 TYPE STANDARD TABLE OF ty_sumniv0 WITH HEADER LINE.

SELECT
    nive1~orden_proceso          AS orden_proceso,
    re_anio_periodo              AS re_anio_periodo,
    sum( Distinct nive1~RE_PORC_SOLIDO_CONSUMIDO ) as RE_PORC_SOLIDO_CONSUMIDO,
    sum( Distinct nive1~PL_PORC_SOLIDO_CONSUMIDO ) as PL_PORC_SOLIDO_CONSUMIDO
FROM  z_d_orden_proceso_co_var AS nive1
INNER JOIN zconstantes AS zconst
ON    nive1~fecha_entrada GE zconst~valor_low AND
      nive1~fecha_entrada LE zconst~valor_high AND
      zmodulo    EQ 'BI'        AND
      programm   EQ 'ZBIM_0001' AND
      fieldname  EQ 'ERDAT'     AND
      zsigno     EQ 'I'         AND
      zopcion    EQ 'BT'        AND
      zsecuencia EQ '01'
where orden_proceso IN @s_orden
group by
      orden_proceso,
      re_anio_periodo
ORDER BY nive1~orden_proceso ASCENDING
INTO CORRESPONDING FIELDS OF TABLE @it_sumniv0.

**********************************************************************
"Sumatoria de RE_COSTO_TOTAL y PL_COSTO_TOTAL para nivel 1
"Esta sumatoria se colocara en el nivel 0 de cada orden de proceso.
TYPES:BEGIN OF ty_sumniv1_total,
         ORDEN_PROCESO TYPE ZE_BI_ORDEN_PROCESO,
         RE_ANIO_PERIODO TYPE ZE_BI_ANIO_PERIODO,
         RE_COSTO_TOTAL TYPE ZE_BI_COSTO_TOTAL_REAL,
         PL_COSTO_TOTAL TYPE ZE_BI_COSTO_TOTAL_PLAN,
      END OF ty_sumniv1_total.

DATA: it_ty_sumniv1_total TYPE STANDARD TABLE OF ty_sumniv1_total WITH HEADER LINE.
REFRESH it_ty_sumniv1_total.
SELECT
    nive1~orden_proceso          AS orden_proceso,
    re_anio_periodo              AS re_anio_periodo,
    sum( Distinct nive1~RE_COSTO_TOTAL )  AS RE_COSTO_TOTAL,
    sum( Distinct nive1~PL_COSTO_TOTAL )  AS PL_COSTO_TOTAL
FROM  z_d_orden_proceso_co_var AS nive1
INNER JOIN zconstantes AS zconst
ON    nive1~fecha_entrada GE zconst~valor_low AND
      nive1~fecha_entrada LE zconst~valor_high AND
      zmodulo    EQ 'BI'        AND
      programm   EQ 'ZBIM_0001' AND
      fieldname  EQ 'ERDAT'     AND
      zsigno     EQ 'I'         AND
      zopcion    EQ 'BT'        AND
      zsecuencia EQ '01'
WHERE  nive1~orden_proceso IN @s_orden and
       nive1~nivel = '01'
group by
      orden_proceso,
      re_anio_periodo
ORDER BY nive1~orden_proceso ASCENDING
INTO TABLE @it_ty_sumniv1_total.
*******************************************************************************
"Identifica valor PL_CTD_CONSU_BOM / RE_CTD_CONSU_BOM por periodo para nivel 0
select Distinct
       a~orden_proceso,
       a~re_anio_periodo,
       a~pl_ctd_consu_bom,
       a~re_ctd_consu_bom
from   z_d_ordpro_info_plan_nv0 as a
WHERE  a~orden_proceso IN @s_orden
ORDER BY a~orden_proceso ASCENDING
INTO TABLE @DATA(it_tdconsumbom_NIVE0).
*******************************************************************************

select a~orden_proceso,
       sum( Distinct a~pl_ctd_consu_bom ) as pl_ctd_consu_bom,
       sum( Distinct a~re_ctd_consu_bom ) as re_ctd_consu_bom
from   z_d_ordpro_info_plan_nv0 as a
WHERE  a~orden_proceso IN @s_orden
group by a~orden_proceso
ORDER BY a~orden_proceso ASCENDING
INTO TABLE @DATA(it_tdconsumbom_SUMANIVE0).

**********************************************************************
"EMAT - Enrique Aguirre - 21.07.2020 - Nuevo
DATA: ld_cursor TYPE cursor.
TRY.
OPEN CURSOR WITH HOLD @DATA(lcs_cursor) FOR
SELECT
    DISTINCT
    nive0~re_costo_total,
    nive0~re_mo_costo_real,
    nive0~re_ctd_consu_bom,
    nive0~re_um_ctd_consu_bom,
    nive0~re_anio_periodo,
    nive0~pl_costo_total,
    nive0~pl_mo_costo_total,
    nive0~UM_Cantidad_Orden,
    nive0~Cantidad_Orden,

    nive1~nivel                         AS nivel_1,
    nive1~orden_proceso                 AS orden_proceso_1,
    nive1~materialprincipal             AS materialprincipal_1,
    nive1~centro_prod                   AS centro_prod_1,

    nive1~material                      as material_1,
    nive2~material                      as material_2,
    nive3~material                      as material_3,
    nive4~material                      as material_4,
    nive5~material                      as material_5,
    nive6~material                      as material_6,
    nive7~material                      as material_7,

    nive1~componente                    as componente_1,
    nive2~componente                    as componente_2,
    nive3~componente                    as componente_3,
    nive4~componente                    as componente_4,
    nive5~componente                    as componente_5,
    nive6~componente                    as componente_6,
    nive7~componente                    as componente_7,

    nive1~fecha_entrada                 AS fecha_entrada_1,
    nive1~re_anio_periodo               AS re_anio_periodo_1,
    nive1~re_ctd_consu_bom              AS re_ctd_consu_bom_1,
    nive1~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_1,
    nive1~re_porc_solido_consumido      AS re_porc_solido_consumido_1,
    nive1~re_costo_total                AS re_costo_total_1,
    nive1~re_mo_costo_real              AS re_mo_costo_real_1,
    nive1~pl_ctd_consu_bom              AS pl_ctd_consu_bom_1,
    nive1~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_1,
    nive1~pl_costo_total                AS pl_costo_total_1,
    nive1~pl_porc_solido_consumido      AS pl_porc_solido_consumido_1,
    nive1~pl_mo_costo_total             AS pl_mo_costo_total_1,
    nive1~pl_ctd_base_orden             AS pl_ctd_base_orden_1,
    nive1~pl_um_base_orden              AS pl_um_base_orden_1,
    nive1~descripcion_material          AS descripcion_material_1,
    nive1~descripcion_componente        AS descripcion_componente_1,
    nive1~tipo_mat_material             AS tipo_mat_material_1,
    nive1~tipo_mat_componente           AS tipo_mat_componente_1,
    nive1~categoria_material            AS categoria_material_1,
    nive1~marca_material                AS marca_material_1,
    nive1~familia_material              AS familia_material_1,
    nive1~presentacion_material         AS presentacion_material_1,
    nive1~categoria_componente          AS categoria_componente_1,
    nive1~marca_componente              AS marca_componente_1,
    nive1~familia_componente            AS familia_componente_1,
    nive1~presentacion_componente       AS presentacion_componente_1,
    nive1~porc_solido_material          AS porc_solido_material_1,
    nive1~porc_solido_componente        AS porc_solido_componente_1,
    nive1~solido_lacteo                 AS solido_lacteo_1,
    nive1~orden_planta                  AS orden_planta_1,
    nive1~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_1,
    nive1~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_1,
    nive1~orden_porc_solido_real        AS orden_porc_solido_real_1,
    nive1~orden_ctd_total_real          AS orden_ctd_total_real_1,
    nive1~orden_ctd_total_plan          AS orden_ctd_total_plan_1,
    nive1~orden_porc_solido_plan        AS orden_porc_solido_plan_1,
    nive1~orden_um                      AS orden_um_1,
    nive1~orden_estado                  AS orden_estado_1,
    nive1~var_ctd_consu_bom             AS var_ctd_consu_bom_1,
    nive1~var_costo_total               AS var_costo_total_1,
    nive1~var_efecto                    AS var_efecto_1,
    nive1~var_solido_consumido          AS var_solido_consumido_1,
    nive1~var_merma                     AS var_merma_1,
    nive1~orden_variacion_consumo       AS orden_variacion_consumo_1,
    nive1~orden_variacion_solidos       AS orden_variacion_solidos_1,
    nive1~orden_porc_merma              AS orden_porc_merma_1,

    nive2~nivel                         AS nivel_2,
    nive2~orden_proceso                 AS orden_proceso_2,
    nive2~materialprincipal             AS materialprincipal_2,
    nive2~centro_prod                   AS centro_prod_2,
    nive2~fecha_entrada                 AS fecha_entrada_2,
    nive2~re_anio_periodo               AS re_anio_periodo_2,
    nive2~re_ctd_consu_bom              AS re_ctd_consu_bom_2,
    nive2~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_2,
    nive2~re_porc_solido_consumido      AS re_porc_solido_consumido_2,
    nive2~re_costo_total                AS re_costo_total_2,
    nive2~re_mo_costo_real              AS re_mo_costo_real_2,
    nive2~pl_ctd_consu_bom              AS pl_ctd_consu_bom_2,
    nive2~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_2,
    nive2~pl_costo_total                AS pl_costo_total_2,
    nive2~pl_porc_solido_consumido      AS pl_porc_solido_consumido_2,
    nive2~pl_mo_costo_total             AS pl_mo_costo_total_2,
    nive2~pl_ctd_base_orden             AS pl_ctd_base_orden_2,
    nive2~pl_um_base_orden              AS pl_um_base_orden_2,
    nive2~descripcion_material          AS descripcion_material_2,
    nive2~descripcion_componente        AS descripcion_componente_2,
    nive2~tipo_mat_material             AS tipo_mat_material_2,
    nive2~tipo_mat_componente           AS tipo_mat_componente_2,
    nive2~categoria_material            AS categoria_material_2,
    nive2~marca_material                AS marca_material_2,
    nive2~familia_material              AS familia_material_2,
    nive2~presentacion_material         AS presentacion_material_2,
    nive2~categoria_componente          AS categoria_componente_2,
    nive2~marca_componente              AS marca_componente_2,
    nive2~familia_componente            AS familia_componente_2,
    nive2~presentacion_componente       AS presentacion_componente_2,
    nive2~porc_solido_material          AS porc_solido_material_2,
    nive2~porc_solido_componente        AS porc_solido_componente_2,
    nive2~solido_lacteo                 AS solido_lacteo_2,
    nive2~orden_planta                  AS orden_planta_2,
    nive2~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_2,
    nive2~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_2,
    nive2~orden_porc_solido_real        AS orden_porc_solido_real_2,
    nive2~orden_ctd_total_real          AS orden_ctd_total_real_2,
    nive2~orden_ctd_total_plan          AS orden_ctd_total_plan_2,
    nive2~orden_porc_solido_plan        AS orden_porc_solido_plan_2,
    nive2~orden_um                      AS orden_um_2,
    nive2~orden_estado                  AS orden_estado_2,
    nive2~var_ctd_consu_bom             AS var_ctd_consu_bom_2,
    nive2~var_costo_total               AS var_costo_total_2,
    nive2~var_efecto                    AS var_efecto_2,
    nive2~var_solido_consumido          AS var_solido_consumido_2,
    nive2~var_merma                     AS var_merma_2,
    nive2~orden_variacion_consumo       AS orden_variacion_consumo_2,
    nive2~orden_variacion_solidos       AS orden_variacion_solidos_2,
    nive2~orden_porc_merma              AS orden_porc_merma_2,

    nive3~nivel                         AS nivel_3,
    nive3~orden_proceso                 AS orden_proceso_3,
    nive3~materialprincipal             AS materialprincipal_3,
    nive3~centro_prod                   AS centro_prod_3,
    nive3~fecha_entrada                 AS fecha_entrada_3,
    nive3~re_anio_periodo               AS re_anio_periodo_3,
    nive3~re_ctd_consu_bom              AS re_ctd_consu_bom_3,
    nive3~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_3,
    nive3~re_porc_solido_consumido      AS re_porc_solido_consumido_3,
    nive3~re_costo_total                AS re_costo_total_3,
    nive3~re_mo_costo_real              AS re_mo_costo_real_3,
    nive3~pl_ctd_consu_bom              AS pl_ctd_consu_bom_3,
    nive3~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_3,
    nive3~pl_costo_total                AS pl_costo_total_3,
    nive3~pl_porc_solido_consumido      AS pl_porc_solido_consumido_3,
    nive3~pl_mo_costo_total             AS pl_mo_costo_total_3,
    nive3~pl_ctd_base_orden             AS pl_ctd_base_orden_3,
    nive3~pl_um_base_orden              AS pl_um_base_orden_3,
    nive3~descripcion_material          AS descripcion_material_3,
    nive3~descripcion_componente        AS descripcion_componente_3,
    nive3~tipo_mat_material             AS tipo_mat_material_3,
    nive3~tipo_mat_componente           AS tipo_mat_componente_3,
    nive3~categoria_material            AS categoria_material_3,
    nive3~marca_material                AS marca_material_3,
    nive3~familia_material              AS familia_material_3,
    nive3~presentacion_material         AS presentacion_material_3,
    nive3~categoria_componente          AS categoria_componente_3,
    nive3~marca_componente              AS marca_componente_3,
    nive3~familia_componente            AS familia_componente_3,
    nive3~presentacion_componente       AS presentacion_componente_3,
    nive3~porc_solido_material          AS porc_solido_material_3,
    nive3~porc_solido_componente        AS porc_solido_componente_3,
    nive3~solido_lacteo                 AS solido_lacteo_3,
    nive3~orden_planta                  AS orden_planta_3,
    nive3~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_3,
    nive3~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_3,
    nive3~orden_porc_solido_real        AS orden_porc_solido_real_3,
    nive3~orden_ctd_total_real          AS orden_ctd_total_real_3,
    nive3~orden_ctd_total_plan          AS orden_ctd_total_plan_3,
    nive3~orden_porc_solido_plan        AS orden_porc_solido_plan_3,
    nive3~orden_um                      AS orden_um_3,
    nive3~orden_estado                  AS orden_estado_3,
    nive3~var_ctd_consu_bom             AS var_ctd_consu_bom_3,
    nive3~var_costo_total               AS var_costo_total_3,
    nive3~var_efecto                    AS var_efecto_3,
    nive3~var_solido_consumido          AS var_solido_consumido_3,
    nive3~var_merma                     AS var_merma_3,
    nive3~orden_variacion_consumo       AS orden_variacion_consumo_3,
    nive3~orden_variacion_solidos       AS orden_variacion_solidos_3,
    nive3~orden_porc_merma              AS orden_porc_merma_3,

    nive4~nivel                         AS nivel_4,
    nive4~orden_proceso                 AS orden_proceso_4,
    nive4~materialprincipal             AS materialprincipal_4,
    nive4~centro_prod                   AS centro_prod_4,
    nive4~fecha_entrada                 AS fecha_entrada_4,
    nive4~re_anio_periodo               AS re_anio_periodo_4,
    nive4~re_ctd_consu_bom              AS re_ctd_consu_bom_4,
    nive4~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_4,
    nive4~re_porc_solido_consumido      AS re_porc_solido_consumido_4,
    nive4~re_costo_total                AS re_costo_total_4,
    nive4~re_mo_costo_real              AS re_mo_costo_real_4,
    nive4~pl_ctd_consu_bom              AS pl_ctd_consu_bom_4,
    nive4~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_4,
    nive4~pl_costo_total                AS pl_costo_total_4,
    nive4~pl_porc_solido_consumido      AS pl_porc_solido_consumido_4,
    nive4~pl_mo_costo_total             AS pl_mo_costo_total_4,
    nive4~pl_ctd_base_orden             AS pl_ctd_base_orden_4,
    nive4~pl_um_base_orden              AS pl_um_base_orden_4,
    nive4~descripcion_material          AS descripcion_material_4,
    nive4~descripcion_componente        AS descripcion_componente_4,
    nive4~tipo_mat_material             AS tipo_mat_material_4,
    nive4~tipo_mat_componente           AS tipo_mat_componente_4,
    nive4~categoria_material            AS categoria_material_4,
    nive4~marca_material                AS marca_material_4,
    nive4~familia_material              AS familia_material_4,
    nive4~presentacion_material         AS presentacion_material_4,
    nive4~categoria_componente          AS categoria_componente_4,
    nive4~marca_componente              AS marca_componente_4,
    nive4~familia_componente            AS familia_componente_4,
    nive4~presentacion_componente       AS presentacion_componente_4,
    nive4~porc_solido_material          AS porc_solido_material_4,
    nive4~porc_solido_componente        AS porc_solido_componente_4,
    nive4~solido_lacteo                 AS solido_lacteo_4,
    nive4~orden_planta                  AS orden_planta_4,
    nive4~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_4,
    nive4~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_4,
    nive4~orden_porc_solido_real        AS orden_porc_solido_real_4,
    nive4~orden_ctd_total_real          AS orden_ctd_total_real_4,
    nive4~orden_ctd_total_plan          AS orden_ctd_total_plan_4,
    nive4~orden_porc_solido_plan        AS orden_porc_solido_plan_4,
    nive4~orden_um                      AS orden_um_4,
    nive4~orden_estado                  AS orden_estado_4,
    nive4~var_ctd_consu_bom             AS var_ctd_consu_bom_4,
    nive4~var_costo_total               AS var_costo_total_4,
    nive4~var_efecto                    AS var_efecto_4,
    nive4~var_solido_consumido          AS var_solido_consumido_4,
    nive4~var_merma                     AS var_merma_4,
    nive4~orden_variacion_consumo       AS orden_variacion_consumo_4,
    nive4~orden_variacion_solidos       AS orden_variacion_solidos_4,
    nive4~orden_porc_merma              AS orden_porc_merma_4,

    nive5~nivel                         AS nivel_5,
    nive5~orden_proceso                 AS orden_proceso_5,
    nive5~materialprincipal             AS materialprincipal_5,
    nive5~centro_prod                   AS centro_prod_5,
    nive5~fecha_entrada                 AS fecha_entrada_5,
    nive5~re_anio_periodo               AS re_anio_periodo_5,
    nive5~re_ctd_consu_bom              AS re_ctd_consu_bom_5,
    nive5~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_5,
    nive5~re_porc_solido_consumido      AS re_porc_solido_consumido_5,
    nive5~re_costo_total                AS re_costo_total_5,
    nive5~re_mo_costo_real              AS re_mo_costo_real_5,
    nive5~pl_ctd_consu_bom              AS pl_ctd_consu_bom_5,
    nive5~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_5,
    nive5~pl_costo_total                AS pl_costo_total_5,
    nive5~pl_porc_solido_consumido      AS pl_porc_solido_consumido_5,
    nive5~pl_mo_costo_total             AS pl_mo_costo_total_5,
    nive5~pl_ctd_base_orden             AS pl_ctd_base_orden_5,
    nive5~pl_um_base_orden              AS pl_um_base_orden_5,
    nive5~descripcion_material          AS descripcion_material_5,
    nive5~descripcion_componente        AS descripcion_componente_5,
    nive5~tipo_mat_material             AS tipo_mat_material_5,
    nive5~tipo_mat_componente           AS tipo_mat_componente_5,
    nive5~categoria_material            AS categoria_material_5,
    nive5~marca_material                AS marca_material_5,
    nive5~familia_material              AS familia_material_5,
    nive5~presentacion_material         AS presentacion_material_5,
    nive5~categoria_componente          AS categoria_componente_5,
    nive5~marca_componente              AS marca_componente_5,
    nive5~familia_componente            AS familia_componente_5,
    nive5~presentacion_componente       AS presentacion_componente_5,
    nive5~porc_solido_material          AS porc_solido_material_5,
    nive5~porc_solido_componente        AS porc_solido_componente_5,
    nive5~solido_lacteo                 AS solido_lacteo_5,
    nive5~orden_planta                  AS orden_planta_5,
    nive5~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_5,
    nive5~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_5,
    nive5~orden_porc_solido_real        AS orden_porc_solido_real_5,
    nive5~orden_ctd_total_real          AS orden_ctd_total_real_5,
    nive5~orden_ctd_total_plan          AS orden_ctd_total_plan_5,
    nive5~orden_porc_solido_plan        AS orden_porc_solido_plan_5,
    nive5~orden_um                      AS orden_um_5,
    nive5~orden_estado                  AS orden_estado_5,
    nive5~var_ctd_consu_bom             AS var_ctd_consu_bom_5,
    nive5~var_costo_total               AS var_costo_total_5,
    nive5~var_efecto                    AS var_efecto_5,
    nive5~var_solido_consumido          AS var_solido_consumido_5,
    nive5~var_merma                     AS var_merma_5,
    nive5~orden_variacion_consumo       AS orden_variacion_consumo_5,
    nive5~orden_variacion_solidos       AS orden_variacion_solidos_5,
    nive5~orden_porc_merma              AS orden_porc_merma_5,

    nive6~nivel                         AS nivel_6,
    nive6~orden_proceso                 AS orden_proceso_6,
    nive6~materialprincipal             AS materialprincipal_6,
    nive6~centro_prod                   AS centro_prod_6,
    nive6~fecha_entrada                 AS fecha_entrada_6,
    nive6~re_anio_periodo               AS re_anio_periodo_6,
    nive6~re_ctd_consu_bom              AS re_ctd_consu_bom_6,
    nive6~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_6,
    nive6~re_porc_solido_consumido      AS re_porc_solido_consumido_6,
    nive6~re_costo_total                AS re_costo_total_6,
    nive6~re_mo_costo_real              AS re_mo_costo_real_6,
    nive6~pl_ctd_consu_bom              AS pl_ctd_consu_bom_6,
    nive6~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_6,
    nive6~pl_costo_total                AS pl_costo_total_6,
    nive6~pl_porc_solido_consumido      AS pl_porc_solido_consumido_6,
    nive6~pl_mo_costo_total             AS pl_mo_costo_total_6,
    nive6~pl_ctd_base_orden             AS pl_ctd_base_orden_6,
    nive6~pl_um_base_orden              AS pl_um_base_orden_6,
    nive6~descripcion_material          AS descripcion_material_6,
    nive6~descripcion_componente        AS descripcion_componente_6,
    nive6~tipo_mat_material             AS tipo_mat_material_6,
    nive6~tipo_mat_componente           AS tipo_mat_componente_6,
    nive6~categoria_material            AS categoria_material_6,
    nive6~marca_material                AS marca_material_6,
    nive6~familia_material              AS familia_material_6,
    nive6~presentacion_material         AS presentacion_material_6,
    nive6~categoria_componente          AS categoria_componente_6,
    nive6~marca_componente              AS marca_componente_6,
    nive6~familia_componente            AS familia_componente_6,
    nive6~presentacion_componente       AS presentacion_componente_6,
    nive6~porc_solido_material          AS porc_solido_material_6,
    nive6~porc_solido_componente        AS porc_solido_componente_6,
    nive6~solido_lacteo                 AS solido_lacteo_6,
    nive6~orden_planta                  AS orden_planta_6,
    nive6~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_6,
    nive6~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_6,
    nive6~orden_porc_solido_real        AS orden_porc_solido_real_6,
    nive6~orden_ctd_total_real          AS orden_ctd_total_real_6,
    nive6~orden_ctd_total_plan          AS orden_ctd_total_plan_6,
    nive6~orden_porc_solido_plan        AS orden_porc_solido_plan_6,
    nive6~orden_um                      AS orden_um_6,
    nive6~orden_estado                  AS orden_estado_6,
    nive6~var_ctd_consu_bom             AS var_ctd_consu_bom_6,
    nive6~var_costo_total               AS var_costo_total_6,
    nive6~var_efecto                    AS var_efecto_6,
    nive6~var_solido_consumido          AS var_solido_consumido_6,
    nive6~var_merma                     AS var_merma_6,
    nive6~orden_variacion_consumo       AS orden_variacion_consumo_6,
    nive6~orden_variacion_solidos       AS orden_variacion_solidos_6,
    nive6~orden_porc_merma              AS orden_porc_merma_6,

    nive7~nivel                         AS nivel_7,
    nive7~orden_proceso                 AS orden_proceso_7,
    nive7~materialprincipal             AS materialprincipal_7,
    nive7~centro_prod                   AS centro_prod_7,
    nive7~fecha_entrada                 AS fecha_entrada_7,
    nive7~re_anio_periodo               AS re_anio_periodo_7,
    nive7~re_ctd_consu_bom              AS re_ctd_consu_bom_7,
    nive7~re_um_ctd_consu_bom           AS re_um_ctd_consu_bom_7,
    nive7~re_porc_solido_consumido      AS re_porc_solido_consumido_7,
    nive7~re_costo_total                AS re_costo_total_7,
    nive7~re_mo_costo_real              AS re_mo_costo_real_7,
    nive7~pl_ctd_consu_bom              AS pl_ctd_consu_bom_7,
    nive7~pl_um_ctd_consu_bom           AS pl_um_ctd_consu_bom_7,
    nive7~pl_costo_total                AS pl_costo_total_7,
    nive7~pl_porc_solido_consumido      AS pl_porc_solido_consumido_7,
    nive7~pl_mo_costo_total             AS pl_mo_costo_total_7,
    nive7~pl_ctd_base_orden             AS pl_ctd_base_orden_7,
    nive7~pl_um_base_orden              AS pl_um_base_orden_7,
    nive7~descripcion_material          AS descripcion_material_7,
    nive7~descripcion_componente        AS descripcion_componente_7,
    nive7~tipo_mat_material             AS tipo_mat_material_7,
    nive7~tipo_mat_componente           AS tipo_mat_componente_7,
    nive7~categoria_material            AS categoria_material_7,
    nive7~marca_material                AS marca_material_7,
    nive7~familia_material              AS familia_material_7,
    nive7~presentacion_material         AS presentacion_material_7,
    nive7~categoria_componente          AS categoria_componente_7,
    nive7~marca_componente              AS marca_componente_7,
    nive7~familia_componente            AS familia_componente_7,
    nive7~presentacion_componente       AS presentacion_componente_7,
    nive7~porc_solido_material          AS porc_solido_material_7,
    nive7~porc_solido_componente        AS porc_solido_componente_7,
    nive7~solido_lacteo                 AS solido_lacteo_7,
    nive7~orden_planta                  AS orden_planta_7,
    nive7~orden_fecha_inicio_extrema    AS orden_fecha_inicio_extrema_7,
    nive7~orden_fecha_fin_extrema       AS orden_fecha_fin_extrema_7,
    nive7~orden_porc_solido_real        AS orden_porc_solido_real_7,
    nive7~orden_ctd_total_real          AS orden_ctd_total_real_7,
    nive7~orden_ctd_total_plan          AS orden_ctd_total_plan_7,
    nive7~orden_porc_solido_plan        AS orden_porc_solido_plan_7,
    nive7~orden_um                      AS orden_um_7,
    nive7~orden_estado                  AS orden_estado_7,
    nive7~var_ctd_consu_bom             AS var_ctd_consu_bom_7,
    nive7~var_costo_total               AS var_costo_total_7,
    nive7~var_efecto                    AS var_efecto_7,
    nive7~var_solido_consumido          AS var_solido_consumido_7,
    nive7~var_merma                     AS var_merma_7,
    nive7~orden_variacion_consumo       AS orden_variacion_consumo_7,
    nive7~orden_variacion_solidos       AS orden_variacion_solidos_7,
    nive7~orden_porc_merma              AS orden_porc_merma_7
FROM  z_d_orden_proceso_co_var AS nive1
INNER JOIN zconstantes AS zconst
ON    nive1~fecha_entrada GE zconst~valor_low AND
      nive1~fecha_entrada LE zconst~valor_high AND
      zmodulo    EQ 'BI'        AND
      programm   EQ 'ZBIM_0001' AND
      fieldname  EQ 'ERDAT'     AND
      zsigno     EQ 'I'         AND
      zopcion    EQ 'BT'        AND
      zsecuencia EQ '01'
left outer join z_d_ordpro_info_plan_nv0 as nive0
on nive0~orden_proceso = nive1~orden_proceso and
   nive0~fecha_entrada GE zconst~valor_low AND
   nive0~fecha_entrada LE zconst~valor_high
left outer join z_d_orden_proceso_co_var as nive2
on  nive1~nivel = '01' and
    nive2~nivel = '02' and
    nive1~orden_proceso = nive2~orden_proceso and
    nive1~componente    = nive2~material      and
    nive1~centro_prod   = nive2~centro_prod   and
    nive1~fecha_entrada = nive2~fecha_entrada
left outer join z_d_orden_proceso_co_var as nive3
on  nive3~nivel = '03' and
    nive2~orden_proceso = nive3~orden_proceso and
    nive2~componente    = nive3~material      and
    nive2~centro_prod   = nive3~centro_prod   and
    nive2~fecha_entrada = nive3~fecha_entrada
left outer join z_d_orden_proceso_co_var as nive4
on  nive4~nivel = '04' and
    nive3~orden_proceso = nive4~orden_proceso and
    nive3~componente    = nive4~material      and
    nive3~centro_prod   = nive4~centro_prod   and
    nive3~fecha_entrada = nive4~fecha_entrada
left outer join z_d_orden_proceso_co_var as nive5
on  nive5~nivel = '05' and
    nive4~orden_proceso = nive5~orden_proceso and
    nive4~componente    = nive5~material      and
    nive4~centro_prod   = nive5~centro_prod   and
    nive4~fecha_entrada = nive5~fecha_entrada
left outer join z_d_orden_proceso_co_var as nive6
on  nive5~nivel = '06' and
    nive5~orden_proceso = nive6~orden_proceso and
    nive5~componente    = nive6~material      and
    nive5~centro_prod   = nive6~centro_prod   and
    nive5~fecha_entrada = nive6~fecha_entrada
left outer join z_d_orden_proceso_co_var as nive7
on  nive6~nivel = '07' and
    nive6~orden_proceso = nive7~orden_proceso and
    nive6~componente    = nive7~material      and
    nive6~centro_prod   = nive7~centro_prod   and
    nive6~fecha_entrada = nive7~fecha_entrada
WHERE  nive1~orden_proceso IN @s_orden and
        nive1~nivel = '01'
ORDER BY nive1~orden_proceso ASCENDING,
         nive1~nivel ASCENDING,
         nive1~materialprincipal ASCENDING,
         nive1~centro_prod ASCENDING,
         nive7~material ASCENDING,
         nive6~material ASCENDING,
         nive5~material ASCENDING,
         nive4~material ASCENDING,
         nive3~material ASCENDING,
         nive2~material ASCENDING,
         nive1~material ASCENDING,
         nive7~componente ASCENDING,
         nive6~componente ASCENDING,
         nive5~componente ASCENDING,
         nive4~componente ASCENDING,
         nive3~componente ASCENDING,
         nive2~componente ASCENDING,
         nive1~componente ASCENDING.

DATA lp_menge_ TYPE ekpo-menge.
DATA: lw_msehi TYPE msehi.

REFRESH it_zbim0005__.
DO.
  REFRESH ltd_conglomerado.
  FETCH NEXT CURSOR @lcs_cursor INTO TABLE @it_zbim0005__ PACKAGE SIZE 10000.

  IF sy-subrc <> 0.
    CLOSE CURSOR @lcs_cursor.
    EXIT.
  ENDIF.
  CLEAR  wa_conglomerado_ceroS.
  LOOP AT it_zbim0005__ ASSIGNING FIELD-SYMBOL(<it_zbim0005>).
    IF ( wa_conglomerado_cero-orden_proceso NE <it_zbim0005>-orden_proceso_1 )
        OR ( wa_conglomerado_cero-orden_proceso EQ <it_zbim0005>-orden_proceso_1 AND
               <it_zbim0005>-RE_ANIO_PERIODO is not INITIAL AND
               wa_conglomerado_cero-re_anio_periodo NE <it_zbim0005>-RE_ANIO_PERIODO ).

      IF  wa_conglomerado_cero-orden_proceso NE <it_zbim0005>-orden_proceso_1.
        CLEAR wa_conglomerado_ceroS.
      ENDIF.

      IF  wa_conglomerado_cero-re_anio_periodo NE <it_zbim0005>-RE_ANIO_PERIODO.
        CLEAR wa_conglomerado_cero.
      ENDIF.


      lwa_jerarquia-orden_proceso = <it_zbim0005>-orden_proceso_1.
      wa_conglomerado_cero-nivel = 0.
      wa_conglomerado_cero-orden_proceso           = <it_zbim0005>-orden_proceso_1.
      wa_conglomerado_cero-centro_prod             = <it_zbim0005>-centro_prod_1.
      wa_conglomerado_cero-fecha_entrada           = <it_zbim0005>-fecha_entrada_1.
      wa_conglomerado_cero-materialprincipal       = <it_zbim0005>-materialprincipal_1.

      wa_conglomerado_cero-presentacion_material   = <it_zbim0005>-presentacion_material_1.
      wa_conglomerado_cero-descripcion_material    = <it_zbim0005>-descripcion_material_1.
      wa_conglomerado_cero-tipo_mat_material       = <it_zbim0005>-tipo_mat_material_1.
      wa_conglomerado_cero-categoria_material      = <it_zbim0005>-categoria_material_1.
      wa_conglomerado_cero-marca_material          = <it_zbim0005>-marca_material_1.
      wa_conglomerado_cero-familia_material        = <it_zbim0005>-familia_material_1.
      wa_conglomerado_cero-porc_solido_material    = <it_zbim0005>-porc_solido_material_1.

      wa_conglomerado_cero-re_anio_periodo         = <it_zbim0005>-re_anio_periodo.
      wa_conglomerado_cero-pl_ctd_consu_bom        = <it_zbim0005>-re_ctd_consu_bom.
      wa_conglomerado_cero-pl_um_ctd_consu_bom     = <it_zbim0005>-UM_Cantidad_Orden.

      wa_conglomerado_cero-re_ctd_consu_bom        = <it_zbim0005>-re_ctd_consu_bom.
      wa_conglomerado_cero-re_um_ctd_consu_bom     = <it_zbim0005>-UM_Cantidad_Orden.

        READ TABLE it_ty_sumniv1_total ASSIGNING FIELD-SYMBOL(<fs_costo_total>)
        WITH KEY orden_proceso   = wa_conglomerado_cero-orden_proceso
                 re_anio_periodo = <it_zbim0005>-re_anio_periodo.
        IF sy-subrc = 0.
            wa_conglomerado_cero-re_costo_total          = <fs_costo_total>-re_costo_total.
            wa_conglomerado_cero-pl_costo_total          = <fs_costo_total>-pl_costo_total.
        ELSE.
            wa_conglomerado_cero-re_costo_total = 0.
            wa_conglomerado_cero-pl_costo_total = 0.
        ENDIF.

        wa_conglomerado_cero-re_mo_costo_real        = <it_zbim0005>-re_mo_costo_real.
        wa_conglomerado_cero-pl_mo_costo_total       = <it_zbim0005>-pl_mo_costo_total.

        wa_conglomerado_cero-material                = <it_zbim0005>-material_1.
        wa_conglomerado_cero-componente              = <it_zbim0005>-material_1.

        wa_conglomerado_cero-orden_planta                = <it_zbim0005>-orden_planta_1.
        wa_conglomerado_cero-orden_fecha_inicio_extrema  =  <it_zbim0005>-orden_fecha_inicio_extrema_1.
        wa_conglomerado_cero-orden_fecha_fin_extrema     =  <it_zbim0005>-orden_fecha_fin_extrema_1.

"Sumatoria de los campos RE_PORC_SOLIDO_CONSUMIDO y PL_PORC_SOLIDO_CONSUMIDO
"de todos los niveles de una orden.
        READ TABLE it_sumniv0 ASSIGNING FIELD-SYMBOL(<fs_sumniv0>)
        WITH KEY orden_proceso = lwa_jerarquia-orden_proceso
                 re_anio_periodo = <it_zbim0005>-re_anio_periodo.
        IF SY-SUBRC EQ 0.
            wa_conglomerado_cero-RE_PORC_SOLIDO_CONSUMIDO = <fs_sumniv0>-RE_PORC_SOLIDO_CONSUMIDO.
            wa_conglomerado_cero-PL_PORC_SOLIDO_CONSUMIDO = <fs_sumniv0>-PL_PORC_SOLIDO_CONSUMIDO.
        ELSE.
            wa_conglomerado_cero-RE_PORC_SOLIDO_CONSUMIDO = 0.
            wa_conglomerado_cero-PL_PORC_SOLIDO_CONSUMIDO = 0.
        ENDIF.

        wa_conglomerado_cero-ORDEN_PORC_SOLIDO_REAL  = <it_zbim0005>-ORDEN_PORC_SOLIDO_REAL_1.
        wa_conglomerado_cero-ORDEN_CTD_TOTAL_REAL    = <it_zbim0005>-ORDEN_CTD_TOTAL_REAL_1.
        wa_conglomerado_cero-ORDEN_CTD_TOTAL_PLAN    = <it_zbim0005>-ORDEN_CTD_TOTAL_PLAN_1.
        wa_conglomerado_cero-ORDEN_PORC_SOLIDO_PLAN  = <it_zbim0005>-ORDEN_PORC_SOLIDO_PLAN_1.
        wa_conglomerado_cero-ORDEN_UM                = <it_zbim0005>-ORDEN_UM_1.
        wa_conglomerado_cero-ORDEN_ESTADO            = <it_zbim0005>-ORDEN_ESTADO_1.

        wa_conglomerado_cero-VAR_CTD_CONSU_BOM       = <it_zbim0005>-VAR_CTD_CONSU_BOM_1.
        wa_conglomerado_cero-VAR_COSTO_TOTAL         = wa_conglomerado_cero-pl_costo_total - wa_conglomerado_cero-re_costo_total.

        IF wa_conglomerado_cero-pl_costo_total NE 0.
            wa_conglomerado_cero-VAR_EFECTO          = wa_conglomerado_cero-VAR_COSTO_TOTAL / wa_conglomerado_cero-pl_costo_total.
        ENDIF.

        wa_conglomerado_cero-VAR_SOLIDO_CONSUMIDO    = wa_conglomerado_cero-PL_PORC_SOLIDO_CONSUMIDO - wa_conglomerado_cero-RE_PORC_SOLIDO_CONSUMIDO.

        IF wa_conglomerado_cero-RE_PORC_SOLIDO_CONSUMIDO NE 0.
            wa_conglomerado_cero-VAR_MERMA           = wa_conglomerado_cero-VAR_SOLIDO_CONSUMIDO / wa_conglomerado_cero-RE_PORC_SOLIDO_CONSUMIDO.
        ENDIF.

        wa_conglomerado_cero-ORDEN_VARIACION_CONSUMO = <it_zbim0005>-ORDEN_VARIACION_CONSUMO_1.
        wa_conglomerado_cero-ORDEN_VARIACION_SOLIDOS = <it_zbim0005>-ORDEN_VARIACION_SOLIDOS_1.
        wa_conglomerado_cero-ORDEN_PORC_MERMA        = <it_zbim0005>-ORDEN_PORC_MERMA_1.

      APPEND wa_conglomerado_cero TO ltd_conglomerado.

        DATA: rng_periodo TYPE RANGE OF CHAR10.
        DATA: wa_periodo LIKE LINE OF rng_periodo.
        CLEAR wa_periodo.
        REFRESH rng_periodo.
        LOOP AT IT_TDCONSUMBOM_NIVE0 ASSIGNING FIELD-SYMBOL(<fs_rng_periodo>)
        WHERE orden_proceso = <it_zbim0005>-orden_proceso_1 and re_CTD_CONSU_BOM NE 0.
           wa_periodo-sign = 'E'.
           wa_periodo-option = 'EQ'.
           wa_periodo-low = <fs_rng_periodo>-re_anio_periodo.
           APPEND wa_periodo TO rng_periodo.
        ENDLOOP.

      "Inserta mas registros de nivel CERO con campos de cantidad y monto en CERO.
      "La cantidad de registros a insertar es tantas como periodos existan en todos los niveles
      IF wa_conglomerado_ceroS IS INITIAL.
        wa_conglomerado_ceroS = wa_conglomerado_cero.
      CLEAR wa_conglomerado_ceroS-porc_solido_material.
      CLEAR wa_conglomerado_ceroS-pl_ctd_consu_bom.
      CLEAR wa_conglomerado_ceroS-re_costo_total.
      CLEAR wa_conglomerado_ceroS-pl_costo_total.
      CLEAR wa_conglomerado_ceroS-re_ctd_consu_bom.
      CLEAR wa_conglomerado_ceroS-RE_PORC_SOLIDO_CONSUMIDO.
      CLEAR wa_conglomerado_ceroS-PL_PORC_SOLIDO_CONSUMIDO.
      CLEAR wa_conglomerado_ceroS-ORDEN_PORC_SOLIDO_REAL.
      CLEAR wa_conglomerado_ceroS-ORDEN_CTD_TOTAL_REAL.
      CLEAR wa_conglomerado_ceroS-ORDEN_CTD_TOTAL_PLAN.
      CLEAR wa_conglomerado_ceroS-ORDEN_PORC_SOLIDO_PLAN.
      CLEAR wa_conglomerado_ceroS-VAR_CTD_CONSU_BOM.
      CLEAR wa_conglomerado_ceroS-VAR_COSTO_TOTAL.
      CLEAR wa_conglomerado_ceroS-VAR_EFECTO.
      CLEAR wa_conglomerado_ceroS-VAR_SOLIDO_CONSUMIDO.
      CLEAR wa_conglomerado_ceroS-VAR_MERMA.
      CLEAR wa_conglomerado_ceroS-ORDEN_VARIACION_CONSUMO.
      CLEAR wa_conglomerado_ceroS-ORDEN_VARIACION_SOLIDOS.
      CLEAR wa_conglomerado_ceroS-ORDEN_PORC_MERMA.

      "Inserta todos los niveles 0 segun periodos existan por orden de proceso.
      LOOP AT it_ty_perxniv_ ASSIGNING FIELD-SYMBOL(<fs_perxniv>)
             WHERE  orden_proceso = wa_conglomerado_cero-orden_proceso and
                    re_anio_periodo IN rng_periodo. "Excluye los periodos de nivel CERO con valores en RE_CTD_CONSU_BOM
        wa_conglomerado_ceroS-re_anio_periodo = <fs_perxniv>-re_anio_periodo.

        IF <fs_perxniv>-re_anio_periodo IS INITIAL.
            READ TABLE it_ty_sumniv1_total ASSIGNING <fs_costo_total>
            WITH KEY orden_proceso   = wa_conglomerado_cero-orden_proceso
                     re_anio_periodo = ''.

            IF SY-SUBRC EQ 0.
                wa_conglomerado_ceroS-pl_costo_total = <fs_costo_total>-pl_costo_total.
                wa_conglomerado_ceroS-re_costo_total = <fs_costo_total>-re_costo_total.
            ENDIF.

            READ TABLE it_sumniv0 ASSIGNING <fs_sumniv0>
            WITH KEY orden_proceso = wa_conglomerado_cero-orden_proceso
                     re_anio_periodo = ''.
            IF SY-SUBRC EQ 0.
                wa_conglomerado_ceroS-RE_PORC_SOLIDO_CONSUMIDO = <fs_sumniv0>-RE_PORC_SOLIDO_CONSUMIDO.
                wa_conglomerado_ceroS-PL_PORC_SOLIDO_CONSUMIDO = <fs_sumniv0>-PL_PORC_SOLIDO_CONSUMIDO.
            ENDIF.
        else.
            wa_conglomerado_ceroS-RE_PORC_SOLIDO_CONSUMIDO = 0.
            wa_conglomerado_ceroS-PL_PORC_SOLIDO_CONSUMIDO = 0.
            wa_conglomerado_ceroS-pl_costo_total = 0.
            wa_conglomerado_ceroS-re_costo_total = 0.
        ENDIF.

        READ TABLE it_ty_sumniv1_total ASSIGNING <fs_costo_total>
        WITH KEY orden_proceso   = wa_conglomerado_cero-orden_proceso
                 re_anio_periodo = <fs_perxniv>-re_anio_periodo.
        IF sy-subrc = 0.
            wa_conglomerado_ceroS-re_costo_total  = <fs_costo_total>-re_costo_total.
            wa_conglomerado_ceroS-pl_costo_total  = <fs_costo_total>-pl_costo_total.

            wa_conglomerado_ceroS-VAR_COSTO_TOTAL  = wa_conglomerado_ceroS-pl_costo_total - wa_conglomerado_ceroS-re_costo_total.

            IF wa_conglomerado_ceroS-pl_costo_total NE 0.
                wa_conglomerado_ceroS-VAR_EFECTO   = wa_conglomerado_ceroS-VAR_COSTO_TOTAL / wa_conglomerado_ceroS-pl_costo_total.
            ENDIF.

        ELSE.
            wa_conglomerado_ceroS-re_costo_total = 0.
            wa_conglomerado_ceroS-pl_costo_total = 0.
        ENDIF.

        READ TABLE it_sumniv0 ASSIGNING <fs_sumniv0>
        WITH KEY orden_proceso   = wa_conglomerado_cero-orden_proceso
                 re_anio_periodo = <fs_perxniv>-re_anio_periodo.
        IF SY-SUBRC EQ 0.
            wa_conglomerado_ceroS-RE_PORC_SOLIDO_CONSUMIDO = <fs_sumniv0>-RE_PORC_SOLIDO_CONSUMIDO.
            wa_conglomerado_ceroS-PL_PORC_SOLIDO_CONSUMIDO = <fs_sumniv0>-PL_PORC_SOLIDO_CONSUMIDO.

            wa_conglomerado_ceroS-VAR_SOLIDO_CONSUMIDO = wa_conglomerado_ceroS-PL_PORC_SOLIDO_CONSUMIDO - wa_conglomerado_ceroS-RE_PORC_SOLIDO_CONSUMIDO.

            IF wa_conglomerado_ceroS-RE_PORC_SOLIDO_CONSUMIDO NE 0.
                wa_conglomerado_ceroS-VAR_MERMA  = wa_conglomerado_ceroS-VAR_SOLIDO_CONSUMIDO / wa_conglomerado_ceroS-RE_PORC_SOLIDO_CONSUMIDO.
            ENDIF.
        ELSE.

            wa_conglomerado_ceroS-RE_PORC_SOLIDO_CONSUMIDO = 0.
            wa_conglomerado_ceroS-PL_PORC_SOLIDO_CONSUMIDO = 0.
            wa_conglomerado_ceroS-VAR_SOLIDO_CONSUMIDO = 0.
            wa_conglomerado_ceroS-VAR_MERMA = 0.
        ENDIF.


        APPEND wa_conglomerado_ceroS TO ltd_conglomerado.
      ENDLOOP.

      "Suma total de todos los niveles del campo re_ctd_consu_bom para la orden en procesamiento.
      READ TABLE it_tdconsumbom_SUMANIVE0 ASSIGNING FIELD-SYMBOL(<FS_SUMANIV0_CTDCONSUBOM>)
        WITH KEY orden_proceso   = wa_conglomerado_cero-orden_proceso.
        wa_conglomerado_ceroS-re_ctd_consu_bom = ABS( <FS_SUMANIV0_CTDCONSUBOM>-re_ctd_consu_bom ).
     ENDIF.
    ENDIF.

    "EMAT - Fill Nivel 1
    CLEAR wa_conglomerado.
    wa_conglomerado-pl_porc_solido_consumido = <it_zbim0005>-pl_porc_solido_consumido_1. "27.07.2020

    wa_conglomerado-pl_ctd_consu_bom    = <it_zbim0005>-pl_ctd_consu_bom_1.
    wa_conglomerado-pl_um_ctd_consu_bom = <it_zbim0005>-pl_um_ctd_consu_bom_1.


    wa_conglomerado-re_costo_total          = <it_zbim0005>-re_costo_total_1.
    wa_conglomerado-re_mo_costo_real        = <it_zbim0005>-re_mo_costo_real_1.
    wa_conglomerado-re_anio_periodo         = <it_zbim0005>-re_anio_periodo_1.
    wa_conglomerado-pl_costo_total          = <it_zbim0005>-pl_costo_total_1.
    wa_conglomerado-pl_mo_costo_total       = <it_zbim0005>-pl_mo_costo_total_1.

    wa_conglomerado-nivel                      = <it_zbim0005>-nivel_1.
    wa_conglomerado-orden_proceso              = <it_zbim0005>-orden_proceso_1.
    wa_conglomerado-centro_prod                = <it_zbim0005>-centro_prod_1.
    wa_conglomerado-material                   = <it_zbim0005>-material_1.
    wa_conglomerado-fecha_entrada              = <it_zbim0005>-fecha_entrada_1.
    wa_conglomerado-componente                 = <it_zbim0005>-componente_1.
    wa_conglomerado-material1                  = <it_zbim0005>-componente_1.
    wa_conglomerado-materialprincipal          = <it_zbim0005>-materialprincipal_1.
    wa_conglomerado-re_ctd_consu_bom           = <it_zbim0005>-re_ctd_consu_bom_1.
    wa_conglomerado-re_um_ctd_consu_bom        = <it_zbim0005>-re_um_ctd_consu_bom_1.

"EMAT - 02.09.2020
IF wa_conglomerado_ceroS-re_ctd_consu_bom > 0.
    wa_conglomerado-PL_COST_KG_FAB   = wa_conglomerado-PL_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
    wa_conglomerado-RE_COST_KG_FAB   = wa_conglomerado-RE_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
ELSE.
    wa_conglomerado-PL_COST_KG_FAB = 0.
    wa_conglomerado-RE_COST_KG_FAB = 0.
ENDIF.
"EMAT - 02.09.2020


    "EMAT - Enrique Aguirre - LAIVELFS-281 - 29.07.2020 - I
     IF wa_conglomerado-re_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-re_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @DATA(wa_mseh) WHERE MSEH3 = @wa_conglomerado-re_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-re_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.

     IF wa_conglomerado-pl_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-pl_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @wa_mseh WHERE MSEH3 = @wa_conglomerado-pl_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-pl_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.

    IF wa_conglomerado-re_um_ctd_consu_bom NE wa_conglomerado-pl_um_ctd_consu_bom and
       wa_conglomerado-re_um_ctd_consu_bom <> space and
       wa_conglomerado-pl_um_ctd_consu_bom <> space and
       wa_conglomerado-re_ctd_consu_bom GT 0.
        CLEAR lp_menge_.
        lp_menge_ = <it_zbim0005>-re_ctd_consu_bom_1.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = <it_zbim0005>-componente_1
            i_in_me              = <it_zbim0005>-re_um_ctd_consu_bom_1
            i_out_me             = <it_zbim0005>-pl_um_ctd_consu_bom_1
            i_menge              = lp_menge_
          IMPORTING
            e_menge              = lp_cantidad_conv
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
        IF sy-subrc = 0.
          wa_conglomerado-re_ctd_consu_bom = lp_cantidad_conv.
          wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
        ENDIF.
    ENDIF.
    "EMAT - Enrique Aguirre - LAIVELFS-281 - 29.07.2020 - I

    IF wa_conglomerado-pl_um_ctd_consu_bom is not initial.
      wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
    ENDIF.

    wa_conglomerado-re_porc_solido_consumido   = <it_zbim0005>-re_porc_solido_consumido_1.
    wa_conglomerado-pl_porc_solido_consumido   = <it_zbim0005>-pl_porc_solido_consumido_1.
    wa_conglomerado-descripcion_material       = <it_zbim0005>-descripcion_material_1.
    wa_conglomerado-descripcion_componente     = <it_zbim0005>-descripcion_componente_1.
    wa_conglomerado-tipo_mat_material          = <it_zbim0005>-tipo_mat_material_1.
    wa_conglomerado-tipo_mat_componente        = <it_zbim0005>-tipo_mat_componente_1.
    wa_conglomerado-categoria_material         = <it_zbim0005>-categoria_material_1.
    wa_conglomerado-marca_material             = <it_zbim0005>-marca_material_1.
    wa_conglomerado-familia_material           = <it_zbim0005>-familia_material_1.
    wa_conglomerado-presentacion_material      = <it_zbim0005>-presentacion_material_1.
    wa_conglomerado-categoria_componente       = <it_zbim0005>-categoria_componente_1.
    wa_conglomerado-marca_componente           = <it_zbim0005>-marca_componente_1.
    wa_conglomerado-familia_componente         = <it_zbim0005>-familia_componente_1.
    wa_conglomerado-presentacion_componente    = <it_zbim0005>-presentacion_componente_1.
    wa_conglomerado-porc_solido_material       = <it_zbim0005>-porc_solido_material_1.
    wa_conglomerado-porc_solido_componente     = <it_zbim0005>-porc_solido_componente_1.
    wa_conglomerado-solido_lacteo              = <it_zbim0005>-solido_lacteo_1.
    wa_conglomerado-orden_planta               = <it_zbim0005>-orden_planta_1.
    wa_conglomerado-orden_fecha_inicio_extrema = <it_zbim0005>-orden_fecha_inicio_extrema_1.
    wa_conglomerado-orden_fecha_fin_extrema    = <it_zbim0005>-orden_fecha_fin_extrema_1.
    wa_conglomerado-orden_porc_solido_real     = <it_zbim0005>-orden_porc_solido_real_1.
    wa_conglomerado-orden_ctd_total_real       = <it_zbim0005>-orden_ctd_total_real_1.
    wa_conglomerado-orden_ctd_total_plan       = <it_zbim0005>-orden_ctd_total_plan_1.
    wa_conglomerado-orden_porc_solido_plan     = <it_zbim0005>-orden_porc_solido_plan_1.
    wa_conglomerado-orden_um                   = <it_zbim0005>-orden_um_1.
    wa_conglomerado-orden_estado               = <it_zbim0005>-orden_estado_1.
    wa_conglomerado-var_ctd_consu_bom          = <it_zbim0005>-var_ctd_consu_bom_1.
    wa_conglomerado-var_costo_total            = <it_zbim0005>-var_costo_total_1.
    wa_conglomerado-var_efecto                 = <it_zbim0005>-var_efecto_1.
    wa_conglomerado-var_solido_consumido       = <it_zbim0005>-var_solido_consumido_1.
    wa_conglomerado-var_merma                  = <it_zbim0005>-var_merma_1.
    wa_conglomerado-orden_variacion_consumo    = <it_zbim0005>-orden_variacion_consumo_1.
    wa_conglomerado-orden_variacion_solidos    = <it_zbim0005>-orden_variacion_solidos_1.
    wa_conglomerado-orden_porc_merma           = <it_zbim0005>-orden_porc_merma_1.
    wa_conglomerado-nivel = 1.
    APPEND wa_conglomerado TO ltd_conglomerado.

    "EMAT - Fill Nivel 1


    "EMAT - Fill Nivel _2
    CLEAR wa_conglomerado.

    wa_conglomerado-pl_ctd_consu_bom = <it_zbim0005>-pl_ctd_consu_bom_2.
    wa_conglomerado-pl_um_ctd_consu_bom = <it_zbim0005>-pl_um_ctd_consu_bom_2.

    wa_conglomerado-pl_porc_solido_consumido = <it_zbim0005>-pl_porc_solido_consumido_2. "27.07.2020

    wa_conglomerado-re_costo_total          = <it_zbim0005>-re_costo_total_2.
    wa_conglomerado-re_mo_costo_real        = <it_zbim0005>-re_mo_costo_real_2.
    wa_conglomerado-re_anio_periodo         = <it_zbim0005>-re_anio_periodo_2.
    wa_conglomerado-pl_costo_total          = <it_zbim0005>-pl_costo_total_2.
    wa_conglomerado-pl_mo_costo_total       = <it_zbim0005>-pl_mo_costo_total_2.


    wa_conglomerado-nivel                      = <it_zbim0005>-nivel_2.
    wa_conglomerado-orden_proceso              = <it_zbim0005>-orden_proceso_2.
    wa_conglomerado-centro_prod                = <it_zbim0005>-centro_prod_2.
    wa_conglomerado-material                   = <it_zbim0005>-material_2.
    wa_conglomerado-fecha_entrada              = <it_zbim0005>-fecha_entrada_2.
    wa_conglomerado-componente                 = <it_zbim0005>-componente_2.
    wa_conglomerado-material2                  = <it_zbim0005>-componente_2.

    wa_conglomerado-material1  = <it_zbim0005>-componente_1.
    wa_conglomerado-material2  = <it_zbim0005>-componente_2.

    wa_conglomerado-materialprincipal          = <it_zbim0005>-materialprincipal_2.
    wa_conglomerado-re_ctd_consu_bom           = <it_zbim0005>-re_ctd_consu_bom_2.
    wa_conglomerado-re_um_ctd_consu_bom        = <it_zbim0005>-re_um_ctd_consu_bom_2.

"EMAT - 02.09.2020
IF wa_conglomerado_ceroS-re_ctd_consu_bom > 0.
    wa_conglomerado-PL_COST_KG_FAB   = wa_conglomerado-PL_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
    wa_conglomerado-RE_COST_KG_FAB   = wa_conglomerado-RE_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
ELSE.
    wa_conglomerado-PL_COST_KG_FAB = 0.
    wa_conglomerado-RE_COST_KG_FAB = 0.
ENDIF.
"EMAT - 02.09.2020


     IF wa_conglomerado-re_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-re_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @wa_mseh WHERE MSEH3 = @wa_conglomerado-re_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-re_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.


     IF wa_conglomerado-pl_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-pl_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @wa_mseh WHERE MSEH3 = @wa_conglomerado-pl_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-pl_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.

    IF wa_conglomerado-re_um_ctd_consu_bom NE wa_conglomerado-pl_um_ctd_consu_bom and
       wa_conglomerado-re_um_ctd_consu_bom <> space and
       wa_conglomerado-pl_um_ctd_consu_bom <> space and
       wa_conglomerado-re_ctd_consu_bom GT 0.
        CLEAR lp_menge_.
        lp_menge_ = <it_zbim0005>-re_ctd_consu_bom_2.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = <it_zbim0005>-componente_2
            i_in_me              = <it_zbim0005>-re_um_ctd_consu_bom_2
            i_out_me             = <it_zbim0005>-pl_um_ctd_consu_bom_2
            i_menge              = lp_menge_
          IMPORTING
            e_menge              = lp_cantidad_conv
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
        IF sy-subrc = 0.
          wa_conglomerado-re_ctd_consu_bom = lp_cantidad_conv.
          wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
        ENDIF.
    ENDIF.
    "EMAT - Enrique Aguirre - LAIVELFS-281 - 29.07.2020 - I


    IF wa_conglomerado-pl_um_ctd_consu_bom is not initial.
      wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
    ENDIF.


    wa_conglomerado-re_porc_solido_consumido   = <it_zbim0005>-re_porc_solido_consumido_2.
    wa_conglomerado-pl_porc_solido_consumido   = <it_zbim0005>-pl_porc_solido_consumido_2.
    wa_conglomerado-descripcion_material       = <it_zbim0005>-descripcion_material_2.
    wa_conglomerado-descripcion_componente     = <it_zbim0005>-descripcion_componente_2.
    wa_conglomerado-tipo_mat_material          = <it_zbim0005>-tipo_mat_material_2.
    wa_conglomerado-tipo_mat_componente        = <it_zbim0005>-tipo_mat_componente_2.
    wa_conglomerado-categoria_material         = <it_zbim0005>-categoria_material_2.
    wa_conglomerado-marca_material             = <it_zbim0005>-marca_material_2.
    wa_conglomerado-familia_material           = <it_zbim0005>-familia_material_2.
    wa_conglomerado-presentacion_material      = <it_zbim0005>-presentacion_material_2.
    wa_conglomerado-categoria_componente       = <it_zbim0005>-categoria_componente_2.
    wa_conglomerado-marca_componente           = <it_zbim0005>-marca_componente_2.
    wa_conglomerado-familia_componente         = <it_zbim0005>-familia_componente_2.
    wa_conglomerado-presentacion_componente    = <it_zbim0005>-presentacion_componente_2.
    wa_conglomerado-porc_solido_material       = <it_zbim0005>-porc_solido_material_2.
    wa_conglomerado-porc_solido_componente     = <it_zbim0005>-porc_solido_componente_2.
    wa_conglomerado-solido_lacteo              = <it_zbim0005>-solido_lacteo_2.
    wa_conglomerado-orden_planta               = <it_zbim0005>-orden_planta_2.
    wa_conglomerado-orden_fecha_inicio_extrema = <it_zbim0005>-orden_fecha_inicio_extrema_2.
    wa_conglomerado-orden_fecha_fin_extrema    = <it_zbim0005>-orden_fecha_fin_extrema_2.
    wa_conglomerado-orden_porc_solido_real     = <it_zbim0005>-orden_porc_solido_real_2.
    wa_conglomerado-orden_ctd_total_real       = <it_zbim0005>-orden_ctd_total_real_2.
    wa_conglomerado-orden_ctd_total_plan       = <it_zbim0005>-orden_ctd_total_plan_2.
    wa_conglomerado-orden_porc_solido_plan     = <it_zbim0005>-orden_porc_solido_plan_2.
    wa_conglomerado-orden_um                   = <it_zbim0005>-orden_um_2.
    wa_conglomerado-orden_estado               = <it_zbim0005>-orden_estado_2.
    wa_conglomerado-var_ctd_consu_bom          = <it_zbim0005>-var_ctd_consu_bom_2.
    wa_conglomerado-var_costo_total            = <it_zbim0005>-var_costo_total_2.
    wa_conglomerado-var_efecto                 = <it_zbim0005>-var_efecto_2.
    wa_conglomerado-var_solido_consumido       = <it_zbim0005>-var_solido_consumido_2.
    wa_conglomerado-var_merma                  = <it_zbim0005>-var_merma_2.
    wa_conglomerado-orden_variacion_consumo    = <it_zbim0005>-orden_variacion_consumo_2.
    wa_conglomerado-orden_variacion_solidos    = <it_zbim0005>-orden_variacion_solidos_2.
    wa_conglomerado-orden_porc_merma           = <it_zbim0005>-orden_porc_merma_2.
    wa_conglomerado-nivel = 2.
    APPEND wa_conglomerado TO ltd_conglomerado.

    "EMAT - Fill Nivel 2


    "EMAT - Fill Nivel _3
    CLEAR wa_conglomerado.

    wa_conglomerado-pl_ctd_consu_bom = <it_zbim0005>-pl_ctd_consu_bom_3.
    wa_conglomerado-pl_um_ctd_consu_bom = <it_zbim0005>-pl_um_ctd_consu_bom_3.

    wa_conglomerado-pl_porc_solido_consumido = <it_zbim0005>-pl_porc_solido_consumido_3. "27.07.2020

    wa_conglomerado-re_costo_total          = <it_zbim0005>-re_costo_total_3.
    wa_conglomerado-re_mo_costo_real        = <it_zbim0005>-re_mo_costo_real_3.
    wa_conglomerado-re_anio_periodo         = <it_zbim0005>-re_anio_periodo_3.
    wa_conglomerado-pl_costo_total          = <it_zbim0005>-pl_costo_total_3.
    wa_conglomerado-pl_mo_costo_total       = <it_zbim0005>-pl_mo_costo_total_3.


    wa_conglomerado-nivel                      = <it_zbim0005>-nivel_3.
    wa_conglomerado-orden_proceso              = <it_zbim0005>-orden_proceso_3.
    wa_conglomerado-centro_prod                = <it_zbim0005>-centro_prod_3.
    wa_conglomerado-material                   = <it_zbim0005>-material_3.
    wa_conglomerado-fecha_entrada              = <it_zbim0005>-fecha_entrada_3.
    wa_conglomerado-componente                 = <it_zbim0005>-componente_3.
    wa_conglomerado-material3                  = <it_zbim0005>-componente_3.


    wa_conglomerado-material1  = <it_zbim0005>-componente_1.
    wa_conglomerado-material2  = <it_zbim0005>-componente_2.
    wa_conglomerado-material3  = <it_zbim0005>-componente_3.

    wa_conglomerado-materialprincipal          = <it_zbim0005>-materialprincipal_3.
    wa_conglomerado-re_ctd_consu_bom           = <it_zbim0005>-re_ctd_consu_bom_3.
    wa_conglomerado-re_um_ctd_consu_bom        = <it_zbim0005>-re_um_ctd_consu_bom_3.

"EMAT - 02.09.2020
IF wa_conglomerado_ceroS-re_ctd_consu_bom > 0.
    wa_conglomerado-PL_COST_KG_FAB   = wa_conglomerado-PL_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
    wa_conglomerado-RE_COST_KG_FAB   = wa_conglomerado-RE_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
ELSE.
    wa_conglomerado-PL_COST_KG_FAB = 0.
    wa_conglomerado-RE_COST_KG_FAB = 0.
ENDIF.
"EMAT - 02.09.2020

     IF wa_conglomerado-re_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-re_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @wa_mseh WHERE MSEH3 = @wa_conglomerado-re_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-re_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.

     IF wa_conglomerado-pl_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-pl_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @wa_mseh WHERE MSEH3 = @wa_conglomerado-pl_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-pl_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.

    IF wa_conglomerado-re_um_ctd_consu_bom NE wa_conglomerado-pl_um_ctd_consu_bom and
       wa_conglomerado-re_um_ctd_consu_bom <> space and
       wa_conglomerado-pl_um_ctd_consu_bom <> space and
       wa_conglomerado-re_ctd_consu_bom GT 0.
        CLEAR lp_menge_.
        lp_menge_ = <it_zbim0005>-re_ctd_consu_bom_3.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = <it_zbim0005>-componente_3
            i_in_me              = <it_zbim0005>-re_um_ctd_consu_bom_3
            i_out_me             = <it_zbim0005>-pl_um_ctd_consu_bom_3
            i_menge              = lp_menge_
          IMPORTING
            e_menge              = lp_cantidad_conv
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
        IF sy-subrc = 0.
          wa_conglomerado-re_ctd_consu_bom = lp_cantidad_conv.
          wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
        ENDIF.
    ENDIF.
    "EMAT - Enrique Aguirre - LAIVELFS-281 - 29.07.2020 - I

    IF wa_conglomerado-pl_um_ctd_consu_bom is not initial.
      wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
    ENDIF.

    wa_conglomerado-re_porc_solido_consumido   = <it_zbim0005>-re_porc_solido_consumido_3.
    wa_conglomerado-pl_porc_solido_consumido   = <it_zbim0005>-pl_porc_solido_consumido_3.
    wa_conglomerado-descripcion_material       = <it_zbim0005>-descripcion_material_3.
    wa_conglomerado-descripcion_componente     = <it_zbim0005>-descripcion_componente_3.
    wa_conglomerado-tipo_mat_material          = <it_zbim0005>-tipo_mat_material_3.
    wa_conglomerado-tipo_mat_componente        = <it_zbim0005>-tipo_mat_componente_3.
    wa_conglomerado-categoria_material         = <it_zbim0005>-categoria_material_3.
    wa_conglomerado-marca_material             = <it_zbim0005>-marca_material_3.
    wa_conglomerado-familia_material           = <it_zbim0005>-familia_material_3.
    wa_conglomerado-presentacion_material      = <it_zbim0005>-presentacion_material_3.
    wa_conglomerado-categoria_componente       = <it_zbim0005>-categoria_componente_3.
    wa_conglomerado-marca_componente           = <it_zbim0005>-marca_componente_3.
    wa_conglomerado-familia_componente         = <it_zbim0005>-familia_componente_3.
    wa_conglomerado-presentacion_componente    = <it_zbim0005>-presentacion_componente_3.
    wa_conglomerado-porc_solido_material       = <it_zbim0005>-porc_solido_material_3.
    wa_conglomerado-porc_solido_componente     = <it_zbim0005>-porc_solido_componente_3.
    wa_conglomerado-solido_lacteo              = <it_zbim0005>-solido_lacteo_3.
    wa_conglomerado-orden_planta               = <it_zbim0005>-orden_planta_3.
    wa_conglomerado-orden_fecha_inicio_extrema = <it_zbim0005>-orden_fecha_inicio_extrema_3.
    wa_conglomerado-orden_fecha_fin_extrema    = <it_zbim0005>-orden_fecha_fin_extrema_3.
    wa_conglomerado-orden_porc_solido_real     = <it_zbim0005>-orden_porc_solido_real_3.
    wa_conglomerado-orden_ctd_total_real       = <it_zbim0005>-orden_ctd_total_real_3.
    wa_conglomerado-orden_ctd_total_plan       = <it_zbim0005>-orden_ctd_total_plan_3.
    wa_conglomerado-orden_porc_solido_plan     = <it_zbim0005>-orden_porc_solido_plan_3.
    wa_conglomerado-orden_um                   = <it_zbim0005>-orden_um_3.
    wa_conglomerado-orden_estado               = <it_zbim0005>-orden_estado_3.
    wa_conglomerado-var_ctd_consu_bom          = <it_zbim0005>-var_ctd_consu_bom_3.
    wa_conglomerado-var_costo_total            = <it_zbim0005>-var_costo_total_3.
    wa_conglomerado-var_efecto                 = <it_zbim0005>-var_efecto_3.
    wa_conglomerado-var_solido_consumido       = <it_zbim0005>-var_solido_consumido_3.
    wa_conglomerado-var_merma                  = <it_zbim0005>-var_merma_3.
    wa_conglomerado-orden_variacion_consumo    = <it_zbim0005>-orden_variacion_consumo_3.
    wa_conglomerado-orden_variacion_solidos    = <it_zbim0005>-orden_variacion_solidos_3.
    wa_conglomerado-orden_porc_merma           = <it_zbim0005>-orden_porc_merma_3.
    wa_conglomerado-nivel = 3.
    APPEND wa_conglomerado TO ltd_conglomerado.

    "EMAT - Fill Nivel _3

    "EMAT - Fill Nivel _4
    CLEAR wa_conglomerado.

    wa_conglomerado-pl_ctd_consu_bom = <it_zbim0005>-pl_ctd_consu_bom_4.
    wa_conglomerado-pl_um_ctd_consu_bom = <it_zbim0005>-pl_um_ctd_consu_bom_4.


    wa_conglomerado-pl_porc_solido_consumido = <it_zbim0005>-pl_porc_solido_consumido_4. "27.07.2020

    wa_conglomerado-re_costo_total          = <it_zbim0005>-re_costo_total_4.
    wa_conglomerado-re_mo_costo_real        = <it_zbim0005>-re_mo_costo_real_4.
    wa_conglomerado-re_anio_periodo         = <it_zbim0005>-re_anio_periodo_4.
    wa_conglomerado-pl_costo_total          = <it_zbim0005>-pl_costo_total_4.
    wa_conglomerado-pl_mo_costo_total       = <it_zbim0005>-pl_mo_costo_total_4.


    wa_conglomerado-nivel                      = <it_zbim0005>-nivel_4.
    wa_conglomerado-orden_proceso              = <it_zbim0005>-orden_proceso_4.
    wa_conglomerado-centro_prod                = <it_zbim0005>-centro_prod_4.
    wa_conglomerado-material                   = <it_zbim0005>-material_4.
    wa_conglomerado-fecha_entrada              = <it_zbim0005>-fecha_entrada_4.
    wa_conglomerado-componente                 = <it_zbim0005>-componente_4.
    wa_conglomerado-material4                  = <it_zbim0005>-componente_4.

    wa_conglomerado-material1  = <it_zbim0005>-componente_1.
    wa_conglomerado-material2  = <it_zbim0005>-componente_2.
    wa_conglomerado-material3  = <it_zbim0005>-componente_3.
    wa_conglomerado-material4  = <it_zbim0005>-componente_4.

    wa_conglomerado-materialprincipal          = <it_zbim0005>-materialprincipal_4.
    wa_conglomerado-re_ctd_consu_bom           = <it_zbim0005>-re_ctd_consu_bom_4.
    wa_conglomerado-re_um_ctd_consu_bom        = <it_zbim0005>-re_um_ctd_consu_bom_4.

"EMAT - 02.09.2020
IF wa_conglomerado_ceroS-re_ctd_consu_bom > 0.
    wa_conglomerado-PL_COST_KG_FAB   = wa_conglomerado-PL_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
    wa_conglomerado-RE_COST_KG_FAB   = wa_conglomerado-RE_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
ELSE.
    wa_conglomerado-PL_COST_KG_FAB = 0.
    wa_conglomerado-RE_COST_KG_FAB = 0.
ENDIF.
"EMAT - 02.09.2020

     IF wa_conglomerado-re_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-re_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @wa_mseh WHERE MSEH3 = @wa_conglomerado-re_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-re_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.

     IF wa_conglomerado-pl_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-pl_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @wa_mseh WHERE MSEH3 = @wa_conglomerado-pl_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-pl_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.

    IF wa_conglomerado-re_um_ctd_consu_bom NE wa_conglomerado-pl_um_ctd_consu_bom and
       wa_conglomerado-re_um_ctd_consu_bom <> space and
       wa_conglomerado-pl_um_ctd_consu_bom <> space and
       wa_conglomerado-re_ctd_consu_bom GT 0.
        CLEAR lp_menge_.
        lp_menge_ = <it_zbim0005>-re_ctd_consu_bom_4.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = <it_zbim0005>-componente_4
            i_in_me              = <it_zbim0005>-re_um_ctd_consu_bom_4
            i_out_me             = <it_zbim0005>-pl_um_ctd_consu_bom_4
            i_menge              = lp_menge_
          IMPORTING
            e_menge              = lp_cantidad_conv
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
        IF sy-subrc = 0.
          wa_conglomerado-re_ctd_consu_bom = lp_cantidad_conv.
          wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
        ENDIF.
    ENDIF.
    "EMAT - Enrique Aguirre - LAIVELFS-281 - 29.07.2020 - I

    IF wa_conglomerado-pl_um_ctd_consu_bom is not initial.
      wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
    ENDIF.

    wa_conglomerado-re_porc_solido_consumido   = <it_zbim0005>-re_porc_solido_consumido_4.
    wa_conglomerado-pl_porc_solido_consumido   = <it_zbim0005>-pl_porc_solido_consumido_4.
    wa_conglomerado-descripcion_material       = <it_zbim0005>-descripcion_material_4.
    wa_conglomerado-descripcion_componente     = <it_zbim0005>-descripcion_componente_4.
    wa_conglomerado-tipo_mat_material          = <it_zbim0005>-tipo_mat_material_4.
    wa_conglomerado-tipo_mat_componente        = <it_zbim0005>-tipo_mat_componente_4.
    wa_conglomerado-categoria_material         = <it_zbim0005>-categoria_material_4.
    wa_conglomerado-marca_material             = <it_zbim0005>-marca_material_4.
    wa_conglomerado-familia_material           = <it_zbim0005>-familia_material_4.
    wa_conglomerado-presentacion_material      = <it_zbim0005>-presentacion_material_4.
    wa_conglomerado-categoria_componente       = <it_zbim0005>-categoria_componente_4.
    wa_conglomerado-marca_componente           = <it_zbim0005>-marca_componente_4.
    wa_conglomerado-familia_componente         = <it_zbim0005>-familia_componente_4.
    wa_conglomerado-presentacion_componente    = <it_zbim0005>-presentacion_componente_4.
    wa_conglomerado-porc_solido_material       = <it_zbim0005>-porc_solido_material_4.
    wa_conglomerado-porc_solido_componente     = <it_zbim0005>-porc_solido_componente_4.
    wa_conglomerado-solido_lacteo              = <it_zbim0005>-solido_lacteo_4.
    wa_conglomerado-orden_planta               = <it_zbim0005>-orden_planta_4.
    wa_conglomerado-orden_fecha_inicio_extrema = <it_zbim0005>-orden_fecha_inicio_extrema_4.
    wa_conglomerado-orden_fecha_fin_extrema    = <it_zbim0005>-orden_fecha_fin_extrema_4.
    wa_conglomerado-orden_porc_solido_real     = <it_zbim0005>-orden_porc_solido_real_4.
    wa_conglomerado-orden_ctd_total_real       = <it_zbim0005>-orden_ctd_total_real_4.
    wa_conglomerado-orden_ctd_total_plan       = <it_zbim0005>-orden_ctd_total_plan_4.
    wa_conglomerado-orden_porc_solido_plan     = <it_zbim0005>-orden_porc_solido_plan_4.
    wa_conglomerado-orden_um                   = <it_zbim0005>-orden_um_4.
    wa_conglomerado-orden_estado               = <it_zbim0005>-orden_estado_4.
    wa_conglomerado-var_ctd_consu_bom          = <it_zbim0005>-var_ctd_consu_bom_4.
    wa_conglomerado-var_costo_total            = <it_zbim0005>-var_costo_total_4.
    wa_conglomerado-var_efecto                 = <it_zbim0005>-var_efecto_4.
    wa_conglomerado-var_solido_consumido       = <it_zbim0005>-var_solido_consumido_4.
    wa_conglomerado-var_merma                  = <it_zbim0005>-var_merma_4.
    wa_conglomerado-orden_variacion_consumo    = <it_zbim0005>-orden_variacion_consumo_4.
    wa_conglomerado-orden_variacion_solidos    = <it_zbim0005>-orden_variacion_solidos_4.
    wa_conglomerado-orden_porc_merma           = <it_zbim0005>-orden_porc_merma_4.
    wa_conglomerado-nivel = 4.
    APPEND wa_conglomerado TO ltd_conglomerado.

    "EMAT - Fill Nivel _4

    "EMAT - Fill Nivel _5
    CLEAR wa_conglomerado.

    wa_conglomerado-pl_ctd_consu_bom = <it_zbim0005>-pl_ctd_consu_bom_5.
    wa_conglomerado-pl_um_ctd_consu_bom = <it_zbim0005>-pl_um_ctd_consu_bom_5.

    wa_conglomerado-pl_porc_solido_consumido = <it_zbim0005>-pl_porc_solido_consumido_5. "27.07.2020

    wa_conglomerado-re_costo_total          = <it_zbim0005>-re_costo_total_5.
    wa_conglomerado-re_mo_costo_real        = <it_zbim0005>-re_mo_costo_real_5.
    wa_conglomerado-re_anio_periodo         = <it_zbim0005>-re_anio_periodo_5.
    wa_conglomerado-pl_costo_total          = <it_zbim0005>-pl_costo_total_5.
    wa_conglomerado-pl_mo_costo_total       = <it_zbim0005>-pl_mo_costo_total_5.

    wa_conglomerado-nivel                      = <it_zbim0005>-nivel_5.
    wa_conglomerado-orden_proceso              = <it_zbim0005>-orden_proceso_5.
    wa_conglomerado-centro_prod                = <it_zbim0005>-centro_prod_5.
    wa_conglomerado-material                   = <it_zbim0005>-material_5.
    wa_conglomerado-fecha_entrada              = <it_zbim0005>-fecha_entrada_5.
    wa_conglomerado-componente                 = <it_zbim0005>-componente_5.
    wa_conglomerado-material5                  = <it_zbim0005>-componente_5.

    wa_conglomerado-material1  = <it_zbim0005>-componente_1.
    wa_conglomerado-material2  = <it_zbim0005>-componente_2.
    wa_conglomerado-material3  = <it_zbim0005>-componente_3.
    wa_conglomerado-material4  = <it_zbim0005>-componente_4.
    wa_conglomerado-material5  = <it_zbim0005>-componente_5.

    wa_conglomerado-materialprincipal          = <it_zbim0005>-materialprincipal_5.
    wa_conglomerado-re_ctd_consu_bom           = <it_zbim0005>-re_ctd_consu_bom_5.
    wa_conglomerado-re_um_ctd_consu_bom        = <it_zbim0005>-re_um_ctd_consu_bom_5.

"EMAT - 02.09.2020
IF wa_conglomerado_ceroS-re_ctd_consu_bom > 0.
    wa_conglomerado-PL_COST_KG_FAB   = wa_conglomerado-PL_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
    wa_conglomerado-RE_COST_KG_FAB   = wa_conglomerado-RE_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
ELSE.
    wa_conglomerado-PL_COST_KG_FAB = 0.
    wa_conglomerado-RE_COST_KG_FAB = 0.
ENDIF.
"EMAT - 02.09.2020

     IF wa_conglomerado-re_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-re_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @wa_mseh WHERE MSEH3 = @wa_conglomerado-re_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-re_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.

     IF wa_conglomerado-pl_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-pl_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @wa_mseh WHERE MSEH3 = @wa_conglomerado-pl_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-pl_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.

    IF wa_conglomerado-re_um_ctd_consu_bom NE wa_conglomerado-pl_um_ctd_consu_bom and
       wa_conglomerado-re_um_ctd_consu_bom <> space and
       wa_conglomerado-pl_um_ctd_consu_bom <> space and
       wa_conglomerado-re_ctd_consu_bom GT 0.
        CLEAR lp_menge_.
        lp_menge_ = <it_zbim0005>-re_ctd_consu_bom_5.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = <it_zbim0005>-componente_5
            i_in_me              = <it_zbim0005>-re_um_ctd_consu_bom_5
            i_out_me             = <it_zbim0005>-pl_um_ctd_consu_bom_5
            i_menge              = lp_menge_
          IMPORTING
            e_menge              = lp_cantidad_conv
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
        IF sy-subrc = 0.
          wa_conglomerado-re_ctd_consu_bom = lp_cantidad_conv.
          wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
        ENDIF.
    ENDIF.
    "EMAT - Enrique Aguirre - LAIVELFS-281 - 29.07.2020 - I

    IF wa_conglomerado-pl_um_ctd_consu_bom is not initial.
      wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
    ENDIF.

    wa_conglomerado-re_porc_solido_consumido   = <it_zbim0005>-re_porc_solido_consumido_5.
    wa_conglomerado-pl_porc_solido_consumido   = <it_zbim0005>-pl_porc_solido_consumido_5.
    wa_conglomerado-descripcion_material       = <it_zbim0005>-descripcion_material_5.
    wa_conglomerado-descripcion_componente     = <it_zbim0005>-descripcion_componente_5.
    wa_conglomerado-tipo_mat_material          = <it_zbim0005>-tipo_mat_material_5.
    wa_conglomerado-tipo_mat_componente        = <it_zbim0005>-tipo_mat_componente_5.
    wa_conglomerado-categoria_material         = <it_zbim0005>-categoria_material_5.
    wa_conglomerado-marca_material             = <it_zbim0005>-marca_material_5.
    wa_conglomerado-familia_material           = <it_zbim0005>-familia_material_5.
    wa_conglomerado-presentacion_material      = <it_zbim0005>-presentacion_material_5.
    wa_conglomerado-categoria_componente       = <it_zbim0005>-categoria_componente_5.
    wa_conglomerado-marca_componente           = <it_zbim0005>-marca_componente_5.
    wa_conglomerado-familia_componente         = <it_zbim0005>-familia_componente_5.
    wa_conglomerado-presentacion_componente    = <it_zbim0005>-presentacion_componente_5.
    wa_conglomerado-porc_solido_material       = <it_zbim0005>-porc_solido_material_5.
    wa_conglomerado-porc_solido_componente     = <it_zbim0005>-porc_solido_componente_5.
    wa_conglomerado-solido_lacteo              = <it_zbim0005>-solido_lacteo_5.
    wa_conglomerado-orden_planta               = <it_zbim0005>-orden_planta_5.
    wa_conglomerado-orden_fecha_inicio_extrema = <it_zbim0005>-orden_fecha_inicio_extrema_5.
    wa_conglomerado-orden_fecha_fin_extrema    = <it_zbim0005>-orden_fecha_fin_extrema_5.
    wa_conglomerado-orden_porc_solido_real     = <it_zbim0005>-orden_porc_solido_real_5.
    wa_conglomerado-orden_ctd_total_real       = <it_zbim0005>-orden_ctd_total_real_5.
    wa_conglomerado-orden_ctd_total_plan       = <it_zbim0005>-orden_ctd_total_plan_5.
    wa_conglomerado-orden_porc_solido_plan     = <it_zbim0005>-orden_porc_solido_plan_5.
    wa_conglomerado-orden_um                   = <it_zbim0005>-orden_um_5.
    wa_conglomerado-orden_estado               = <it_zbim0005>-orden_estado_5.
    wa_conglomerado-var_ctd_consu_bom          = <it_zbim0005>-var_ctd_consu_bom_5.
    wa_conglomerado-var_costo_total            = <it_zbim0005>-var_costo_total_5.
    wa_conglomerado-var_efecto                 = <it_zbim0005>-var_efecto_5.
    wa_conglomerado-var_solido_consumido       = <it_zbim0005>-var_solido_consumido_5.
    wa_conglomerado-var_merma                  = <it_zbim0005>-var_merma_5.
    wa_conglomerado-orden_variacion_consumo    = <it_zbim0005>-orden_variacion_consumo_5.
    wa_conglomerado-orden_variacion_solidos    = <it_zbim0005>-orden_variacion_solidos_5.
    wa_conglomerado-orden_porc_merma           = <it_zbim0005>-orden_porc_merma_5.
    wa_conglomerado-nivel = 5.
    APPEND wa_conglomerado TO ltd_conglomerado.

    "EMAT - Fill Nivel _5


    "EMAT - Fill Nivel _6
    CLEAR wa_conglomerado.

    wa_conglomerado-pl_ctd_consu_bom = <it_zbim0005>-pl_ctd_consu_bom_6.
    wa_conglomerado-pl_um_ctd_consu_bom = <it_zbim0005>-pl_um_ctd_consu_bom_6.

    wa_conglomerado-pl_porc_solido_consumido = <it_zbim0005>-pl_porc_solido_consumido_6. "27.07.2020

    wa_conglomerado-re_costo_total          = <it_zbim0005>-re_costo_total_6.
    wa_conglomerado-re_mo_costo_real        = <it_zbim0005>-re_mo_costo_real_6.
    wa_conglomerado-re_anio_periodo         = <it_zbim0005>-re_anio_periodo_6.
    wa_conglomerado-pl_costo_total          = <it_zbim0005>-pl_costo_total_6.
    wa_conglomerado-pl_mo_costo_total       = <it_zbim0005>-pl_mo_costo_total_6.

    wa_conglomerado-nivel                      = <it_zbim0005>-nivel_6.
    wa_conglomerado-orden_proceso              = <it_zbim0005>-orden_proceso_6.
    wa_conglomerado-centro_prod                = <it_zbim0005>-centro_prod_6.
    wa_conglomerado-material                   = <it_zbim0005>-material_6.
    wa_conglomerado-fecha_entrada              = <it_zbim0005>-fecha_entrada_6.
    wa_conglomerado-componente                 = <it_zbim0005>-componente_6.
    wa_conglomerado-material6                  = <it_zbim0005>-componente_6.

    wa_conglomerado-material1  = <it_zbim0005>-componente_1.
    wa_conglomerado-material2  = <it_zbim0005>-componente_2.
    wa_conglomerado-material3  = <it_zbim0005>-componente_3.
    wa_conglomerado-material4  = <it_zbim0005>-componente_4.
    wa_conglomerado-material5  = <it_zbim0005>-componente_5.
    wa_conglomerado-material6  = <it_zbim0005>-componente_6.

    wa_conglomerado-materialprincipal          = <it_zbim0005>-materialprincipal_6.
    wa_conglomerado-re_ctd_consu_bom           = <it_zbim0005>-re_ctd_consu_bom_6.
    wa_conglomerado-re_um_ctd_consu_bom        = <it_zbim0005>-re_um_ctd_consu_bom_6.

IF wa_conglomerado_ceroS-re_ctd_consu_bom > 0.
    wa_conglomerado-PL_COST_KG_FAB   = wa_conglomerado-PL_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
    wa_conglomerado-RE_COST_KG_FAB   = wa_conglomerado-RE_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
ELSE.
    wa_conglomerado-PL_COST_KG_FAB = 0.
    wa_conglomerado-RE_COST_KG_FAB = 0.
ENDIF.
"EMAT - 02.09.2020

       IF wa_conglomerado-re_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-re_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @wa_mseh WHERE MSEH3 = @wa_conglomerado-re_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-re_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.

     IF wa_conglomerado-pl_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-pl_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @wa_mseh WHERE MSEH3 = @wa_conglomerado-pl_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-pl_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.

       IF wa_conglomerado-re_um_ctd_consu_bom NE wa_conglomerado-pl_um_ctd_consu_bom and
       wa_conglomerado-re_um_ctd_consu_bom <> space and
       wa_conglomerado-pl_um_ctd_consu_bom <> space and
       wa_conglomerado-re_ctd_consu_bom GT 0.
        CLEAR lp_menge_.
        lp_menge_ = <it_zbim0005>-re_ctd_consu_bom_6.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = <it_zbim0005>-componente_6
            i_in_me              = <it_zbim0005>-re_um_ctd_consu_bom_6
            i_out_me             = <it_zbim0005>-pl_um_ctd_consu_bom_6
            i_menge              = lp_menge_
          IMPORTING
            e_menge              = lp_cantidad_conv
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
        IF sy-subrc = 0.
          wa_conglomerado-re_ctd_consu_bom = lp_cantidad_conv.
          wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
        ENDIF.
    ENDIF.
    "EMAT - Enrique Aguirre - LAIVELFS-281 - 29.07.2020 - I

    IF wa_conglomerado-pl_um_ctd_consu_bom is not initial.
      wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
    ENDIF.

    wa_conglomerado-re_porc_solido_consumido   = <it_zbim0005>-re_porc_solido_consumido_6.
    wa_conglomerado-pl_porc_solido_consumido   = <it_zbim0005>-pl_porc_solido_consumido_6.
    wa_conglomerado-descripcion_material       = <it_zbim0005>-descripcion_material_6.
    wa_conglomerado-descripcion_componente     = <it_zbim0005>-descripcion_componente_6.
    wa_conglomerado-tipo_mat_material          = <it_zbim0005>-tipo_mat_material_6.
    wa_conglomerado-tipo_mat_componente        = <it_zbim0005>-tipo_mat_componente_6.
    wa_conglomerado-categoria_material         = <it_zbim0005>-categoria_material_6.
    wa_conglomerado-marca_material             = <it_zbim0005>-marca_material_6.
    wa_conglomerado-familia_material           = <it_zbim0005>-familia_material_6.
    wa_conglomerado-presentacion_material      = <it_zbim0005>-presentacion_material_6.
    wa_conglomerado-categoria_componente       = <it_zbim0005>-categoria_componente_6.
    wa_conglomerado-marca_componente           = <it_zbim0005>-marca_componente_6.
    wa_conglomerado-familia_componente         = <it_zbim0005>-familia_componente_6.
    wa_conglomerado-presentacion_componente    = <it_zbim0005>-presentacion_componente_6.
    wa_conglomerado-porc_solido_material       = <it_zbim0005>-porc_solido_material_6.
    wa_conglomerado-porc_solido_componente     = <it_zbim0005>-porc_solido_componente_6.
    wa_conglomerado-solido_lacteo              = <it_zbim0005>-solido_lacteo_6.
    wa_conglomerado-orden_planta               = <it_zbim0005>-orden_planta_6.
    wa_conglomerado-orden_fecha_inicio_extrema = <it_zbim0005>-orden_fecha_inicio_extrema_6.
    wa_conglomerado-orden_fecha_fin_extrema    = <it_zbim0005>-orden_fecha_fin_extrema_6.
    wa_conglomerado-orden_porc_solido_real     = <it_zbim0005>-orden_porc_solido_real_6.
    wa_conglomerado-orden_ctd_total_real       = <it_zbim0005>-orden_ctd_total_real_6.
    wa_conglomerado-orden_ctd_total_plan       = <it_zbim0005>-orden_ctd_total_plan_6.
    wa_conglomerado-orden_porc_solido_plan     = <it_zbim0005>-orden_porc_solido_plan_6.
    wa_conglomerado-orden_um                   = <it_zbim0005>-orden_um_6.
    wa_conglomerado-orden_estado               = <it_zbim0005>-orden_estado_6.
    wa_conglomerado-var_ctd_consu_bom          = <it_zbim0005>-var_ctd_consu_bom_6.
    wa_conglomerado-var_costo_total            = <it_zbim0005>-var_costo_total_6.
    wa_conglomerado-var_efecto                 = <it_zbim0005>-var_efecto_6.
    wa_conglomerado-var_solido_consumido       = <it_zbim0005>-var_solido_consumido_6.
    wa_conglomerado-var_merma                  = <it_zbim0005>-var_merma_6.
    wa_conglomerado-orden_variacion_consumo    = <it_zbim0005>-orden_variacion_consumo_6.
    wa_conglomerado-orden_variacion_solidos    = <it_zbim0005>-orden_variacion_solidos_6.
    wa_conglomerado-orden_porc_merma           = <it_zbim0005>-orden_porc_merma_6.
    wa_conglomerado-nivel = 6.
    APPEND wa_conglomerado TO ltd_conglomerado.
    "EMAT - Fill Nivel _6

    "EMAT - Fill Nivel _7
    CLEAR wa_conglomerado.

    wa_conglomerado-pl_ctd_consu_bom = <it_zbim0005>-pl_ctd_consu_bom_7.
    wa_conglomerado-pl_um_ctd_consu_bom = <it_zbim0005>-pl_um_ctd_consu_bom_7.

    wa_conglomerado-pl_porc_solido_consumido = <it_zbim0005>-pl_porc_solido_consumido_7. "27.07.2020

    wa_conglomerado-re_costo_total          = <it_zbim0005>-re_costo_total_7.
    wa_conglomerado-re_mo_costo_real        = <it_zbim0005>-re_mo_costo_real_7.
    wa_conglomerado-re_anio_periodo         = <it_zbim0005>-re_anio_periodo_7.
    wa_conglomerado-pl_costo_total          = <it_zbim0005>-pl_costo_total_7.
    wa_conglomerado-pl_mo_costo_total       = <it_zbim0005>-pl_mo_costo_total_7.

    wa_conglomerado-nivel                      = <it_zbim0005>-nivel_7.
    wa_conglomerado-orden_proceso              = <it_zbim0005>-orden_proceso_7.
    wa_conglomerado-centro_prod                = <it_zbim0005>-centro_prod_7.
    wa_conglomerado-material                   = <it_zbim0005>-material_7.
    wa_conglomerado-material7                  = <it_zbim0005>-componente_7.


    wa_conglomerado-material1  = <it_zbim0005>-componente_1.
    wa_conglomerado-material2  = <it_zbim0005>-componente_2.
    wa_conglomerado-material3  = <it_zbim0005>-componente_3.
    wa_conglomerado-material4  = <it_zbim0005>-componente_4.
    wa_conglomerado-material5  = <it_zbim0005>-componente_5.
    wa_conglomerado-material6  = <it_zbim0005>-componente_6.
    wa_conglomerado-material7  = <it_zbim0005>-componente_7.

    wa_conglomerado-fecha_entrada              = <it_zbim0005>-fecha_entrada_7.
    wa_conglomerado-componente                 = <it_zbim0005>-componente_7.
    wa_conglomerado-materialprincipal          = <it_zbim0005>-materialprincipal_7.
    wa_conglomerado-re_ctd_consu_bom           = <it_zbim0005>-re_ctd_consu_bom_7.
    wa_conglomerado-re_um_ctd_consu_bom        = <it_zbim0005>-re_um_ctd_consu_bom_7.

"EMAT - 02.09.2020
*CLEAR wa_conglomerado_cero-re_ctd_consu_bom.
*READ TABLE it_tdconsumbom_NIVE0 ASSIGNING <fs_ctd_consu_bom>
*WITH KEY orden_proceso   = wa_conglomerado-orden_proceso
*         re_anio_periodo = wa_conglomerado-re_anio_periodo.
*IF SY-SUBRC = 0.
*    wa_conglomerado_cero-re_ctd_consu_bom = ABS( <fs_ctd_consu_bom>-re_ctd_consu_bom ).
IF wa_conglomerado_ceroS-re_ctd_consu_bom > 0.
    wa_conglomerado-PL_COST_KG_FAB   = wa_conglomerado-PL_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
    wa_conglomerado-RE_COST_KG_FAB   = wa_conglomerado-RE_COSTO_TOTAL / wa_conglomerado_ceroS-re_ctd_consu_bom.
ELSE.
    wa_conglomerado-PL_COST_KG_FAB = 0.
    wa_conglomerado-RE_COST_KG_FAB = 0.
ENDIF.
"EMAT - 02.09.2020



        IF wa_conglomerado-re_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-re_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @wa_mseh WHERE MSEH3 = @wa_conglomerado-re_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-re_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.

     IF wa_conglomerado-pl_um_ctd_consu_bom IS NOT INITIAL and wa_conglomerado-pl_um_ctd_consu_bom NE ''.
           SELECT SINGLE MSEHI FROM T006A INTO @wa_mseh WHERE MSEH3 = @wa_conglomerado-pl_um_ctd_consu_bom.
            IF sy-subrc EQ 0.
               wa_conglomerado-pl_um_ctd_consu_bom = wa_mseh.
            ENDIF.
     ENDIF.

    IF wa_conglomerado-re_um_ctd_consu_bom NE wa_conglomerado-pl_um_ctd_consu_bom and
       wa_conglomerado-re_um_ctd_consu_bom <> space and
       wa_conglomerado-pl_um_ctd_consu_bom <> space and
       wa_conglomerado-re_ctd_consu_bom GT 0.
        CLEAR lp_menge_.
        lp_menge_ = <it_zbim0005>-re_ctd_consu_bom_7.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = <it_zbim0005>-componente_7
            i_in_me              = <it_zbim0005>-re_um_ctd_consu_bom_7
            i_out_me             = <it_zbim0005>-pl_um_ctd_consu_bom_7
            i_menge              = lp_menge_
          IMPORTING
            e_menge              = lp_cantidad_conv
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
        IF sy-subrc = 0.
          wa_conglomerado-re_ctd_consu_bom = lp_cantidad_conv.
          wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
        ENDIF.
    ENDIF.
    "EMAT - Enrique Aguirre - LAIVELFS-281 - 29.07.2020 - I

    IF wa_conglomerado-pl_um_ctd_consu_bom is not initial.
      wa_conglomerado-re_um_ctd_consu_bom = wa_conglomerado-pl_um_ctd_consu_bom.
    ENDIF.

    wa_conglomerado-re_porc_solido_consumido   = <it_zbim0005>-re_porc_solido_consumido_7.
    wa_conglomerado-pl_porc_solido_consumido   = <it_zbim0005>-pl_porc_solido_consumido_7.
    wa_conglomerado-descripcion_material       = <it_zbim0005>-descripcion_material_7.
    wa_conglomerado-descripcion_componente     = <it_zbim0005>-descripcion_componente_7.
    wa_conglomerado-tipo_mat_material          = <it_zbim0005>-tipo_mat_material_7.
    wa_conglomerado-tipo_mat_componente        = <it_zbim0005>-tipo_mat_componente_7.
    wa_conglomerado-categoria_material         = <it_zbim0005>-categoria_material_7.
    wa_conglomerado-marca_material             = <it_zbim0005>-marca_material_7.
    wa_conglomerado-familia_material           = <it_zbim0005>-familia_material_7.
    wa_conglomerado-presentacion_material      = <it_zbim0005>-presentacion_material_7.
    wa_conglomerado-categoria_componente       = <it_zbim0005>-categoria_componente_7.
    wa_conglomerado-marca_componente           = <it_zbim0005>-marca_componente_7.
    wa_conglomerado-familia_componente         = <it_zbim0005>-familia_componente_7.
    wa_conglomerado-presentacion_componente    = <it_zbim0005>-presentacion_componente_7.
    wa_conglomerado-porc_solido_material       = <it_zbim0005>-porc_solido_material_7.
    wa_conglomerado-porc_solido_componente     = <it_zbim0005>-porc_solido_componente_7.
    wa_conglomerado-solido_lacteo              = <it_zbim0005>-solido_lacteo_7.
    wa_conglomerado-orden_planta               = <it_zbim0005>-orden_planta_7.
    wa_conglomerado-orden_fecha_inicio_extrema = <it_zbim0005>-orden_fecha_inicio_extrema_7.
    wa_conglomerado-orden_fecha_fin_extrema    = <it_zbim0005>-orden_fecha_fin_extrema_7.
    wa_conglomerado-orden_porc_solido_real     = <it_zbim0005>-orden_porc_solido_real_7.
    wa_conglomerado-orden_ctd_total_real       = <it_zbim0005>-orden_ctd_total_real_7.
    wa_conglomerado-orden_ctd_total_plan       = <it_zbim0005>-orden_ctd_total_plan_7.
    wa_conglomerado-orden_porc_solido_plan     = <it_zbim0005>-orden_porc_solido_plan_7.
    wa_conglomerado-orden_um                   = <it_zbim0005>-orden_um_7.
    wa_conglomerado-orden_estado               = <it_zbim0005>-orden_estado_7.
    wa_conglomerado-var_ctd_consu_bom          = <it_zbim0005>-var_ctd_consu_bom_7.
    wa_conglomerado-var_costo_total            = <it_zbim0005>-var_costo_total_7.
    wa_conglomerado-var_efecto                 = <it_zbim0005>-var_efecto_7.
    wa_conglomerado-var_solido_consumido       = <it_zbim0005>-var_solido_consumido_7.
    wa_conglomerado-var_merma                  = <it_zbim0005>-var_merma_7.
    wa_conglomerado-orden_variacion_consumo    = <it_zbim0005>-orden_variacion_consumo_7.
    wa_conglomerado-orden_variacion_solidos    = <it_zbim0005>-orden_variacion_solidos_7.
    wa_conglomerado-orden_porc_merma           = <it_zbim0005>-orden_porc_merma_7.
    wa_conglomerado-nivel = 7.
    APPEND wa_conglomerado TO ltd_conglomerado.

    "EMAT - Fill Nivel _7

    DELETE ltd_conglomerado WHERE orden_proceso IS INITIAL.
    DELETE ltd_conglomerado WHERE RE_CTD_CONSU_BOM = 0 and
                                  PL_CTD_CONSU_BOM = 0 and nivel NE 0.

    SORT ltd_conglomerado BY
            orden_proceso ASCENDING
            nivel ASCENDING
            centro_prod ASCENDING
            material ASCENDING
            fecha_entrada ASCENDING
            componente ASCENDING
            RE_ANIO_PERIODO ASCENDING
            materialprincipal ASCENDING
            material1.

    DELETE ADJACENT DUPLICATES FROM ltd_conglomerado COMPARING ALL FIELDS.

  ENDLOOP.
  MODIFY zbit_ord_proc_co FROM TABLE ltd_conglomerado.
  COMMIT CONNECTION default.
  "EMAT - Enrique Aguirre - 21.07.2020 - Nuevo

**********************************************************************
ENDDO.
CATCH cx_sy_open_sql_db.
   CLOSE CURSOR @lcs_cursor.
ENDTRY.