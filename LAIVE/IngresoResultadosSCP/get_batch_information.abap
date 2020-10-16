    LOOP AT et_output ASSIGNING FIELD-SYMBOL(<fs_output>) WHERE characteristic_quantitative EQ abap_true.
    "EMAT - Enrique Aguirre - LAIVELFS-181 - 12.10.2020 - Insertar

     wa_cad_spec_cantidad = <fs_output>-characteristic_especification.
     REPLACE ALL OCCURRENCES OF REGEX '[A-Z]*[a-z]*[ ]*[%]*[<]*[>]*[=]*[/]*[°]*' IN wa_cad_spec_cantidad WITH ''.
     SPLIT wa_cad_spec_cantidad AT '..' INTO lv_t1 lv_t2.

       wa_cad_text_descrip = <fs_output>-characteristic_text.
       FIND ALL OCCURRENCES OF '(' IN wa_cad_text_descrip RESULTS result_tab.
       READ TABLE result_tab ASSIGNING <match> INDEX 1.
       IF <match> IS ASSIGNED.
           ini_parent = <match>-offset.
           wa_cad_text_descrip = wa_cad_text_descrip(ini_parent).
       ENDIF.

       wa_cad_text_simbolo = <fs_output>-characteristic_text.
       FIND ALL OCCURRENCES OF '(' IN wa_cad_text_simbolo RESULTS result_tab.
       READ TABLE result_tab ASSIGNING <match> INDEX 1.
       IF <match> IS ASSIGNED.
           ini_parent = <match>-offset.
           FIND ALL OCCURRENCES OF ')' IN wa_cad_text_simbolo RESULTS result_tab.
           READ TABLE result_tab ASSIGNING <match> INDEX 1.
           IF <match> IS ASSIGNED.
             fin_parent = <match>-offset + 1.
             DATA(log_simbolo) = fin_parent - ini_parent.
             wa_cad_text_simbolo = wa_cad_text_simbolo+ini_parent(log_simbolo).
           ENDIF.
        REPLACE ALL OCCURRENCES OF REGEX '[(]*[)]*' IN wa_cad_text_simbolo WITH ''.
       ENDIF.

     IF lv_t2 IS NOT INITIAL.
        IF lv_t1 IS NOT INITIAL.
         SPLIT lv_t1 AT '.' INTO lv_t3 lv_t4.
         lv_nro = strlen( lv_t4 ).
         IF lv_nro >= 4.
           CONCATENATE lv_t3 '.' lv_t4(4) INTO lv_t1.
         ELSE.
           lv_t1x = lv_t1.
           lv_t1 = lv_t1x.
         ENDIF.
         CONDENSE lv_t1 NO-GAPS.
        ENDIF.

        IF lv_t2 IS NOT INITIAL.
         SPLIT lv_t2 AT '.' INTO lv_t3 lv_t4.
         lv_nro = strlen( lv_t4 ).
         IF lv_nro >= 4.
           CONCATENATE lv_t3 '.' lv_t4(4) INTO lv_t2.
         ELSE.
           lv_t1x = lv_t2.
           lv_t2 = lv_t1x.
         ENDIF.
         CONDENSE lv_t2 NO-GAPS.
        ENDIF.

       IF wa_cad_text_simbolo EQ <fs_output>-characteristic_text.
          CONCATENATE wa_cad_text_descrip '(' lv_t1 '..' lv_t2 ')' INTO <fs_output>-characteristic_text.
       ELSE.
          CONCATENATE wa_cad_text_descrip '(' lv_t1 '..' lv_t2 ' ' wa_cad_text_simbolo ')' INTO <fs_output>-characteristic_text.
       ENDIF.
     ELSE.
        IF wa_cad_spec_cantidad IS NOT INITIAL.
         SPLIT wa_cad_spec_cantidad AT '.' INTO lv_t3 lv_t4.
         lv_nro = strlen( lv_t4 ).
         IF lv_nro >= 4.
           CONCATENATE lv_t3 '.' lv_t4(4) INTO lv_t1.
         ELSE.
           lv_t1x = lv_t1.
           lv_t1 = lv_t1x.
         ENDIF.
         CONDENSE lv_t1 NO-GAPS.
        ENDIF.

       wa_cad_spec_simbolo = <fs_output>-characteristic_especification.
       REPLACE ALL OCCURRENCES OF REGEX '[A-Z]*[a-z]*[0-9]*[ ]*[%]*[.]*[/]*[°]*' IN wa_cad_spec_simbolo WITH ''.

       IF wa_cad_text_simbolo EQ <fs_output>-characteristic_text.
          CONCATENATE wa_cad_text_descrip '(' wa_cad_spec_simbolo ' ' lv_t1 ')' INTO <fs_output>-characteristic_text.
       ELSE.
         IF lv_t1 IS INITIAL AND wa_cad_spec_simbolo IS INITIAL.
           CONCATENATE wa_cad_text_descrip '(' ' ' wa_cad_text_simbolo ')' INTO <fs_output>-characteristic_text.
         ELSE.
           CONCATENATE wa_cad_text_descrip '(' wa_cad_spec_simbolo ' ' lv_t1 ' ' wa_cad_text_simbolo ')' INTO <fs_output>-characteristic_text.
         ENDIF.
       ENDIF.
     ENDIF.

    "EMAT - Enrique Aguirre - LAIVELFS-181 - 12.10.2020 - Insertar
**********************************************************************

    ENDLOOP.