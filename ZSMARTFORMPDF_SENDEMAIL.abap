https://sapyard.com/how-to-email-smartform-as-pdf-attachment-to-multiple-users/


https://blogs.sap.com/2018/04/22/programmatically-create-and-retrieve-documents-attached-in-gos-generic-object-services-via-stored-business-document./

GOS
https://blogs.sap.com/2018/04/22/programmatically-create-and-retrieve-documents-attached-in-gos-generic-object-services-via-stored-business-document./


* ************************************************************************
*  Report       ZSMARTFORMPDF_SENDEMAIL
*
*  Date                         Company              
* 08/12/2008                    Spartconsulting.com (Jatender Narang)
*
*  DESCRIPTION: Smart form Driver program for operation Ticket Control
* 
*************************************************************************
REPORT  zsmartformpdf_sendemail
        NO STANDARD PAGE HEADING
         LINE-SIZE 132
         LINE-COUNT 65
         MESSAGE-ID zchk.
*-----------------------------------------------------------------------
* TABLES
*-----------------------------------------------------------------------
TABLES : aufk,t352t,t003o,toa_dara,arc_params,nast.
*-----------------------------------------------------------------------
* DATA:  INTERNAL TABLES
*-----------------------------------------------------------------------
DATA : it_aufk TYPE STANDARD TABLE OF aufk.
DATA:  it_lines TYPE STANDARD TABLE OF tline WITH HEADER LINE.

DATA:  BEGIN OF it_header OCCURS 0.      " define a local table header
        INCLUDE STRUCTURE thead.         " using SAPSCRIPT structure
DATA: END   OF it_header.

DATA : wa_aufk LIKE LINE OF it_aufk.
DATA : wa_lines LIKE LINE OF it_lines,
       v_bin_filesize TYPE i,
       it_docs TYPE STANDARD TABLE OF docs,
       it_lines1 TYPE STANDARD TABLE OF tline,
       v_name1 TYPE string,
       v_path TYPE string,
       v_fullpath TYPE string,
       v_filter TYPE string,
       v_uact TYPE i,
       v_guiobj TYPE REF TO cl_gui_frontend_services,
       v_filename TYPE string,
       v_fm_name TYPE rs38l_fnam.

DATA : mstr_print_parms LIKE pri_params,
       mc_valid(1)      TYPE c,
       mi_bytecount     TYPE i,
       mi_length        TYPE i,
       mi_rqident       LIKE tsp01-rqident.

DATA : p_linsz LIKE sy-linsz VALUE 132,         " Line size
       p_paart LIKE sy-paart VALUE 'X_65_132'.  " Paper Format
DATA : var TYPE i VALUE 0 .
DATA : p_repid LIKE sy-repid .

DATA : it_tsp01 TYPE STANDARD TABLE OF tsp01 WITH HEADER LINE .
DATA : it_pdf LIKE tline OCCURS 0 WITH HEADER LINE .

* Spool to PDF conversions
DATA: gd_spool_nr LIKE tsp01-rqident,
      gd_destination LIKE rlgrap-filename,
      gd_bytecount LIKE tst01-dsize,
      gd_buffer TYPE string.

*DATA DECLARATION
DATA: gd_recsize TYPE i.

* Spool IDs
TYPES: BEGIN OF t_tbtcp.
        INCLUDE STRUCTURE tbtcp.
TYPES: END OF t_tbtcp.
DATA: it_tbtcp TYPE STANDARD TABLE OF t_tbtcp INITIAL SIZE 0,
      wa_tbtcp TYPE t_tbtcp.

* Job Runtime Parameters
DATA: gd_eventid LIKE tbtcm-eventid,
      gd_eventparm LIKE tbtcm-eventparm,
      gd_external_program_active LIKE tbtcm-xpgactive,
      gd_jobcount LIKE tbtcm-jobcount,
      gd_jobname LIKE tbtcm-jobname,
      gd_stepcount LIKE tbtcm-stepcount,
      gd_error    TYPE sy-subrc,
      gd_reciever TYPE sy-subrc.

DATA:  w_recsize TYPE i.

DATA: gd_subject   LIKE sodocchgi1-obj_descr,
      it_mess_bod LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      it_mess_att LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      gd_sender_type     LIKE soextreci1-adr_typ,
      gd_attachment_desc TYPE so_obj_nam,
      gd_attachment_name TYPE so_obj_des.

*-----------------------------------------------------------------------
* DATA:  VARIABLES
*-----------------------------------------------------------------------
DATA: fm_sform TYPE rs38l_fnam,
      v_name TYPE thead-tdname.

CONSTANTS: c_id(4) TYPE c VALUE 'KOPF',
           c_langu(1) TYPE c VALUE 'E',
           c_object(10) TYPE c VALUE 'AUFK'.
*--Data Declaration for Printing Layout
DATA: ls_itcpo     TYPE itcpo.
DATA: lf_repid     TYPE sy-repid.
DATA: lf_device    TYPE tddevice.
DATA: cf_retcode   TYPE sy-subrc.
DATA: ls_recipient TYPE swotobjid.
DATA: ls_sender    TYPE swotobjid.
DATA: ls_control_param   TYPE ssfctrlop.
DATA: ls_composer_param  TYPE ssfcompop.
DATA: ls_addr_key        LIKE addr_key.
DATA: w_screen(1) TYPE c.
DATA: xscreen(1) TYPE c.

DATA: da_mess LIKE vbfs OCCURS 0 WITH HEADER LINE.

DATA: ws_fname TYPE rs38l_fnam,
      ws_ctrlp TYPE ssfctrlop,
      ws_optns TYPE ssfcompop,
      w_padest LIKE tsp03l-padest.

DATA: da_message_printed(1) TYPE c,
      da_preview_processed(1) TYPE c,
      repeat(1) TYPE c,
      da_subrc LIKE sy-subrc.

DATA: w_otfdata  TYPE ssfcrescl.
DATA: BEGIN OF it_itcoo OCCURS 0.
        INCLUDE STRUCTURE it coo.
DATA: END OF it_itcoo.
DATA: w_otf  TYPE it coo.

IMPORT var FROM MEMORY ID 'JIT1' .
FREE MEMORY ID 'JIT1'.
IF var = 0.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .
  SELECT
        aufnr
        auart
        zzplat
        objnr
        adrnra
        werks
        ktext
        zzplat
        saknr
        zzmainftln
        zzserviceloc
        zzcounty
        zzpremise
        zzpipe
        ktext
        adrnra
        erdat
        zzcross_street1
        zzcross_street2
        zzcurkcockftln
INTO CORRESPONDING FIELDS OF TABLE it_aufk
        FROM aufk
        WHERE aufk~aufnr = s_aufnr.

  IF it_aufk IS INITIAL.
    MESSAGE i004.
    LEAVE TO LIST-PROCESSING.
  ENDIF.

  READ TABLE it_aufk INDEX 1 INTO wa_aufk.
  CONCATENATE sy-mandt wa_aufk-aufnr INTO v_name.

*& read long text
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = c_id
      language                = c_langu
      name                    = v_name
      object                  = c_object
    IMPORTING
      header                  = it_header
    TABLES
      lines                   = it_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  CALL_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_form .

  DATA lv_msgtype  TYPE sy-msgty.
  IMPORT it_aufk it_lines it_header FROM MEMORY ID 'JIT1' .
  IF var = 0.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = 'ZPM_OPERATION'
      IMPORTING
        fm_name            = fm_sform
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CHECK NOT fm_sform IS INITIAL.
    CLEAR w_otfdata.
    ls_control_param-getotf = 'X'.

    CALL FUNCTION fm_sform
      TABLES
        it_aufk          = it_aufk
        it_lines         = it_lines
        it_header        = it_header
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*-- Setup the Print Parmaters
    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
        authority              = space
        copies                 = '1'
        cover_page             = space
        data_set               = space
        department             = space
        destination            = space
        expiration             = '1'
        immediately            = space
        new_list_id            = 'X'
        no_dialog              = 'X'
        user                   = sy-uname
      IMPORTING
        out_parameters         = mstr_print_parms
        valid                  = mc_valid
      EXCEPTIONS
        archive_info_not_found = 1
        invalid_print_params   = 2
        invalid_archive_params = 3
        OTHERS                 = 4.

*-- Make sure that a printer destination has been set up
*-- If this is not done the PDF function module ABENDS
    IF mstr_print_parms-pdest = space.
      mstr_print_parms-pdest = 'LP01'.
    ENDIF.

*-- Explicitly set line width, and output format so that
*-- the PDF conversion comes out OK
    mstr_print_parms-linsz = p_linsz.
    mstr_print_parms-paart = p_paart.

* for Submit the report to create the abapspool.
    var = var + 1.
    FREE MEMORY ID 'JIT1'.
    EXPORT it_aufk it_lines it_header var TO MEMORY ID 'JIT1' .

    SUBMIT (p_repid) TO SAP-SPOOL
                        SPOOL PARAMETERS mstr_print_parms
                        WITHOUT SPOOL DYNPRO
                        AND RETURN.
*selecting the spool request using the above consructed varibale
    SELECT * FROM tsp01 INTO TABLE it_tsp01
            WHERE rq2name = sy-uname .

    IF sy-subrc = 0.
*sorting the internal table
      SORT  it_tsp01 BY rqcretime DESCENDING .
*reading the first spool request
      READ TABLE it_tsp01 INDEX 1.

      REFRESH it_pdf[].
      CLEAR it_pdf.

*converting the spool request into pdf
      CALL FUNCTION 'CONVERT_OTFSPOOLJOB_2_PDF'
        EXPORTING
          src_spoolid              = it_tsp01-rqident
          no_dialog                = ' '
        TABLES
          pdf                      = it_pdf
        EXCEPTIONS
          err_no_otf_spooljob      = 1
          err_no_spooljob          = 2
          err_no_permission        = 3
          err_conv_not_possible    = 4
          err_bad_dstdevice        = 5
          user_cancelled           = 6
          err_spoolerror           = 7
          err_temseerror           = 8
          err_btcjob_open_failed   = 9
          err_btcjob_submit_failed = 10
          err_btcjob_close_failed  = 11.

      CHECK sy-subrc = 0.

* Transfer the 132-long strings to 255-long strings
      LOOP AT it_pdf.
        TRANSLATE it_pdf USING ' ~'.
        CONCATENATE gd_buffer it_pdf INTO gd_buffer.
      ENDLOOP.

      TRANSLATE gd_buffer USING '~ '.
      DO.
        it_mess_att = gd_buffer.
        APPEND it_mess_att.
        SHIFT gd_buffer LEFT BY 255 PLACES.
        IF gd_buffer IS INITIAL.
          EXIT.
        ENDIF.
      ENDDO.

*RECIPIENT
      DATA : p_email1 TYPE somlreci1-receiver VALUE 'jnarang@spartconsulting.com'.
      DESCRIBE TABLE it_mess_att LINES gd_recsize.
      CHECK gd_recsize > 0.

*& Subroutine to send the email to the RECIPIENT.
      PERFORM send_email USING p_email1.
    ENDIF.
  ENDIF.
ENDFORM.                    " CALL_FORM
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_AUFNR_INPUTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM validate_aufnr_inputs .
  SELECT SINGLE * FROM aufk
      WHERE aufnr = s_aufnr.
*& Error Message for Order Number
  IF sy-subrc <> 0.
    MESSAGE e018 WITH s_aufnr .
  ENDIF.
ENDFORM.                    " VALIDATE_AUFNR_INPUTS
*&---------------------------------------------------------------------*
*&      Form  POPUP
*&---------------------------------------------------------------------*

FORM popup .

  DATA : answer TYPE char1.
  CLEAR answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'CONFIRMATION'
      text_question         = 'Do You Want To Print'
      text_button_1         = 'YES'
      icon_button_1         = 'ICON_CHECKED'
      text_button_2         = 'CANCEL'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF answer = '2'.
    LEAVE SCREEN .
  ENDIF .
ENDFORM.                    " POPUP
*&---------------------------------------------------------------------*
*&      Form  F_SEND_PDF_BY_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TB_PDF255  text
*      -->P_LTB_BODY  text
*      -->P_LTB_RECIPIENTS  text
*      -->P_LW_SUBJECT  text
*      -->P_LW_FILENAME  text
*----------------------------------------------------------------------*
FORM f_send_pdf_by_email  TABLES   p_tb_pdf STRUCTURE tline
                                   p_ltb_body STRUCTURE solisti1
                                   p_ltb_recipients STRUCTURE somlreci1
                          USING    p_lw_subject TYPE so_obj_des
                                   p_lw_filename TYPE char100.

*Variables y tablas internas.
  DATA: BEGIN OF ltb_objbin OCCURS 0.
          INCLUDE STRUCTURE solisti1.
  DATA: END OF ltb_objbin.

  DATA: lwa_doc_chng LIKE sodocchgi1,
  lw_tab_lines LIKE sy-tabix,
  ltb_objtxt LIKE solisti1 OCCURS 10 WITH HEADER LINE,
  ltb_objpack LIKE sopcklsti1 OCCURS 2 WITH HEADER LINE.

*MAIN PROCESSING SECTION------------------------------------------

  CLEAR: ltb_objbin, ltb_objtxt, ltb_objpack, lwa_doc_chng.
  REFRESH: ltb_objbin, ltb_objtxt, ltb_objpack.

*Control Data
  lwa_doc_chng-obj_name = 'URGENT'.
  lwa_doc_chng-sensitivty = 'P'.
  lwa_doc_chng-no_change = 'X'.
  lwa_doc_chng-priority = '1'.
  lwa_doc_chng-obj_prio = '1'.
  lwa_doc_chng-obj_langu = sy-langu.
  lwa_doc_chng-no_change = 'X'.

*Email Subject
  lwa_doc_chng-obj_descr = p_lw_subject.

*Email Body
  LOOP AT p_ltb_body.
    ltb_objtxt-line = p_ltb_body-line.
    APPEND ltb_objtxt.
  ENDLOOP.

  CLEAR ltb_objtxt.
  DESCRIBE TABLE ltb_objtxt LINES lw_tab_lines.
  IF lw_tab_lines GT 0.
    READ TABLE ltb_objtxt INDEX lw_tab_lines.
    lwa_doc_chng-doc_size =
    ( lw_tab_lines - 1 ) * 255 + STRLEN( ltb_objtxt ).
    CLEAR ltb_objpack-transf_bin.
    ltb_objpack-body_start = 1.
    ltb_objpack-body_num = lw_tab_lines.
    ltb_objpack-doc_type = 'RAW'.
    APPEND ltb_objpack.
  ENDIF.

  LOOP AT p_tb_pdf.
    ltb_objbin-line = p_tb_pdf-tdline.
    APPEND ltb_objbin.
  ENDLOOP.

  ltb_objpack-transf_bin = 'X'.
  ltb_objpack-body_start = 1.

  DESCRIBE TABLE ltb_objbin LINES lw_tab_lines.
  ltb_objpack-body_num = lw_tab_lines.
  ltb_objpack-doc_type = 'PDF'.
  ltb_objpack-obj_descr = p_lw_filename.
  ltb_objpack-obj_name = p_lw_filename.
  ltb_objpack-doc_size = lw_tab_lines * 255.
  APPEND ltb_objpack.

*Call the function to send the PDF file by email
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = lwa_doc_chng
      put_in_outbox              = 'X'
    TABLES
      packing_list               = ltb_objpack
      contents_bin               = ltb_objbin
      contents_txt               = ltb_objtxt
      receivers                  = p_ltb_recipients
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  CASE sy-subrc.
    WHEN 0.
    WHEN 1. RAISE too_many_receivers.
    WHEN 2. RAISE document_not_sent .
    WHEN 3. RAISE document_type_not_exist.
    WHEN 4. RAISE operation_no_authorization.
    WHEN 5. RAISE parameter_error.
    WHEN 7. RAISE enqueue_error .
    WHEN OTHERS. RAISE x_error.
  ENDCASE.
  COMMIT WORK.
ENDFORM.                    " F_SEND_PDF_BY_EMAIL
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_EMAIL1  text
*----------------------------------------------------------------------*
FORM send_email  USING p_email.

  CHECK NOT ( p_email IS INITIAL ).

  REFRESH it_mess_bod.

* Default subject matter
  gd_subject         = 'Subject'.
  gd_attachment_desc = 'Attachnament'.
*  CONCATENATE 'attach_name' ' ' INTO gd_attachment_name.
  it_mess_bod        = 'Hi There,'.
  APPEND it_mess_bod.
  it_mess_bod        = 'Please Find the attached Smart Form for Operation  Ticket Control'.
  APPEND it_mess_bod.

  DATA : p_sender TYPE somlreci1-receiver VALUE 'P3D@nwnatural.com'.

* If no sender specified - default blank
  IF p_sender EQ space.
    gd_sender_type  = space.
  ELSE.
    gd_sender_type  = 'INT'.
  ENDIF.

* Send file by email as .xls speadsheet
  PERFORM send_file_as_email_attachment
                               TABLES it_mess_bod
                                      it_mess_att
                                USING p_email
                                      'Example .pdf document attached'
                                      'PDF'
                                      gd_attachment_name
                                      gd_attachment_desc
                                      p_sender
                                      gd_sender_type
                             CHANGING gd_error
                                      gd_reciever.

ENDFORM.                    " SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  SEND_FILE_AS_EMAIL_ATTACHMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MESS_BOD  text
*      -->P_IT_MESS_ATT  text
*      -->P_P_EMAIL  text
*      -->P_1395   text
*      -->P_1396   text
*      -->P_GD_ATTACHMENT_NAME  text
*      -->P_GD_ATTACHMENT_DESC  text
*      -->P_P_SENDER  text
*      -->P_GD_SENDER_TYPE  text
*      <--P_GD_ERROR  text
*      <--P_GD_RECIEVER  text
*----------------------------------------------------------------------*
FORM send_file_as_email_attachment  TABLES   it_message
                                             it_attach
                                    USING    p_email
                                             p_mtitle
                                             p_format
                                             p_filename
                                             p_attdescription
                                             p_sender_address
                                             p_sender_addres_type
                                    CHANGING perror
                                             p_reciever.
  DATA:ld_error    TYPE sy-subrc,
      ld_reciever TYPE sy-subrc,
      ld_mtitle LIKE sodocchgi1-obj_descr,
      ld_email LIKE  somlreci1-receiver,
      ld_format TYPE  so_obj_tp ,
      ld_attdescription TYPE  so_obj_nam ,
      ld_attfilename TYPE  so_obj_des ,
      ld_sender_address LIKE  soextreci1-receiver,
      ld_sender_address_type LIKE  soextreci1-adr_typ,
      ld_receiver LIKE  sy-subrc.

  DATA:   t_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
          t_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          t_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
          t_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          t_object_header LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          w_cnt TYPE i,
          w_sent_all(1) TYPE c,
          w_doc_data LIKE sodocchgi1.

  ld_email  = p_email.
  ld_mtitle = p_mtitle.
  ld_format = p_format.
  ld_attdescription = p_attdescription.
  ld_attfilename    = p_filename.
  ld_sender_address = p_sender_address.
  ld_sender_address_type = p_sender_addres_type.

* Fill the document data.
  w_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
  w_doc_data-obj_langu = sy-langu.
  w_doc_data-obj_name  = 'SAPRPT'.
  w_doc_data-obj_descr = ld_mtitle .
  w_doc_data-sensitivty = 'F'.

* Fill the document data and get size of attachment
  CLEAR w_doc_data.
  READ TABLE it_attach INDEX w_cnt.
  w_doc_data-doc_size =
     ( w_cnt - 1 ) * 255 + STRLEN( it_attach ).
  w_doc_data-obj_langu  = sy-langu.
  w_doc_data-obj_name   = 'SAPRPT'.
  w_doc_data-obj_descr  = ld_mtitle.
  w_doc_data-sensitivty = 'F'.
  CLEAR t_attachment.
  REFRESH t_attachment.
  t_attachment[] = it_attach[].

* Describe the body of the message
  CLEAR t_packing_list.
  REFRESH t_packing_list.
  t_packing_list-transf_bin = space.
  t_packing_list-head_start = 1.
  t_packing_list-head_num = 0.
  t_packing_list-body_start = 1.
  DESCRIBE TABLE it_message LINES t_packing_list-body_num.
  t_packing_list-doc_type = 'RAW'.
  APPEND t_packing_list.

* Create attachment notification
  t_packing_list-transf_bin = 'X'.
  t_packing_list-head_start = 1.
  t_packing_list-head_num   = 1.
  t_packing_list-body_start = 1.

  DESCRIBE TABLE t_attachment LINES t_packing_list-body_num.
  t_packing_list-doc_type   =  ld_format.
  t_packing_list-obj_descr  =  ld_attdescription.
  t_packing_list-obj_name   =  ld_attfilename.
  t_packing_list-doc_size   =  t_packing_list-body_num * 255.
  APPEND t_packing_list.

* Add the recipients email address
  CLEAR t_receivers.
  REFRESH t_receivers.
  t_receivers-receiver = ld_email.
  t_receivers-rec_type = 'U'.
  t_receivers-com_type = 'INT'.
  t_receivers-notif_del = 'X'.
  t_receivers-notif_ndel = 'X'.
  APPEND t_receivers.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = w_doc_data
      put_in_outbox              = 'X'
      sender_address             = ld_sender_address
      sender_address_type        = ld_sender_address_type
      commit_work                = 'X'
    IMPORTING
      sent_to_all                = w_sent_all
    TABLES
      packing_list               = t_packing_list
      contents_bin               = t_attachment
      contents_txt               = it_message
      receivers                  = t_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

* Populate zerror return code
  ld_error = sy-subrc.

* Populate zreceiver return code
  LOOP AT t_receivers.
    ld_receiver = t_receivers-retrn_code.
  ENDLOOP.
ENDFORM.                    " SEND_FILE_AS_EMAIL_ATTACHMENT