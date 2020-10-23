"EMAT - Enrique Aguirre - LFSENAG-573 -19.10.2020 - I
  LOOP AT gt_items ASSIGNING FIELD-SYMBOL(<_ls_items>)
    WHERE werks EQ <ls_header>-werks
     AND  lfdat EQ <ls_header>-lfdat
     AND  lifnr EQ <ls_header>-lifnr
     AND  ekgrp EQ <ls_header>-ekgrp
     AND  ekorg EQ <ls_header>-ekorg
     AND  konnr IS NOT INITIAL.

        PERFORM get_log TABLES lt_return
                      USING  <_ls_items> 'X'.
  ENDLOOP.
  EXIT.
"EMAT - Enrique Aguirre - LFSENAG-573 -19.10.2020 - I.
