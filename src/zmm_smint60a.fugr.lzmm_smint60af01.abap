*----------------------------------------------------------------------*
***INCLUDE LZMM_SMINT60AF01 .
*----------------------------------------------------------------------*

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name: LZMM_SMINT60AF01                                          *
* Title    :  Receive data from TDS and store it to ZMM_FLCM_TDS       *
* Work Unit:  SM-INT-60A-001                                           *
* Created by: Rob West                                                 *
* Created on: December 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose:    Receive data from TDS and store it to ZMM_FLCM_TDS       *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Chari Flor Supino          2017/08/03          DV5K9A0ARX            *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Add delivery date to determine duplicate          *
*----------------------------------------------------------------------*
* Mia Calleen Avila         2013/12/10          DV5K983656             *
*                                                                      *
* Short Description: CR223019 T251976                                  *
*     - Additional fields for ZMM_FLCM_TDS                             *
*     - the validation rules from TDS to SAP                           *
*----------------------------------------------------------------------*
* Rob West                  2012/01/12          DV5K968478             *
*                                                                      *
* Short Description: FLCM Stabilization Defect 678                     *
*                    Check for duplicate BOL on type «502» records     *
*                    when cancel/rebill is blank                       *
*----------------------------------------------------------------------*
* Rob West                  2011/11/14          DV5K967589             *
*                                                                      *
* Short Description: FLCM Stabilization Defect 654                     *
*                    Check for duplicate BOL on type «502» records     *
*----------------------------------------------------------------------*
* Rob West                  2011/09/21          DV5K966585             *
*                                                                      *
* Short Description: FLCM Stabilization Defect 544                     *
*                    Do not send email when only message is "Prior     *
*                    to go-live"                                       *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961203             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_IMPORT_TDS_DATA
*&---------------------------------------------------------------------*
FORM f_import_tds_data USING pi_tds_data TYPE zmm_flcm_tds_interface_table.

  DATA: l_rollback           TYPE xfeld,
        l_send_email         TYPE xfeld,                    "DV5K966585

        li_rebill            TYPE STANDARD TABLE OF REF TO
                                  zcl_smint60a_zmm_flcm_tds,
        li_new_rebill        TYPE t_flcm_ref_table,

        l_flcm               TYPE t_flcm_ref,
        li_flcm              TYPE t_flcm_ref_table,
        li_text              TYPE soli_tab.

  FIELD-SYMBOLS: <tds_data>  TYPE zmm_flcm_tds_interface,
                 <flcm>      TYPE t_flcm_ref,
                 <rebill>    TYPE REF TO zcl_smint60a_zmm_flcm_tds.

  LOOP AT pi_tds_data ASSIGNING <tds_data>.
    l_flcm-ztds_tran_ref_nb = <tds_data>-ztds_tran_ref_nb.
    l_flcm-ztds_tran_type   = <tds_data>-ztds_tran_type.
    l_flcm-ztds_trml_id     = <tds_data>-ztds_trml_id.
    l_flcm-ztds_bol_nbr     = <tds_data>-ztds_bol_nbr.
    l_flcm-ztds_canc_rbil   = <tds_data>-ztds_canc_rbil.
    l_flcm-ztds_tran_dt     = <tds_data>-ztds_tran_dt. "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
    CALL METHOD zcl_smint60a_zmm_flcm_tds=>new_object_ref
      EXPORTING
        im_tran_type    = <tds_data>-ztds_tran_type
        im_tran_ref_nbr = <tds_data>-ztds_tran_ref_nb
        im_tran_dt      = l_flcm-ztds_tran_dt          "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
      CHANGING
        ch_pointer      = l_flcm-flcm_pointer.
    IF l_flcm-flcm_pointer IS BOUND.
      PERFORM f_check_bol_nbr    USING l_flcm.         "I - XT18903 - DV5K983656 - CR223019 T251976

*** Start of Insert TXT22260 - DV5K9A0ARX - CR302137 TK365122
*     If Rebill with different date, treat it as new
*     Need to change 'R' to blank
      IF l_flcm-ztds_canc_rbil EQ 'R'.
        READ TABLE li_flcm WITH KEY ztds_trml_id = l_flcm-ztds_trml_id
                                    ztds_bol_nbr = l_flcm-ztds_bol_nbr
                                    ztds_tran_dt = l_flcm-ztds_tran_dt
                                    TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          CLEAR: l_flcm-ztds_canc_rbil,
                 <tds_data>-ztds_canc_rbil.
        ENDIF.
      ENDIF.
*** End of Insert TXT22260 - DV5K9A0ARX - CR302137 TK365122

      CALL METHOD l_flcm-flcm_pointer->transfer_data
        EXPORTING
          im_tds_data = <tds_data>.
      PERFORM f_check_duplicates USING l_flcm
                                       li_flcm[].
      APPEND l_flcm TO li_flcm.
    ENDIF.
  ENDLOOP.

*** Process all cancellations
  LOOP AT li_flcm ASSIGNING <flcm>
      WHERE ztds_canc_rbil = 'C'.
    IF <flcm>-flcm_pointer->errors_found( ) = abap_false.
      PERFORM f_process_cancellation USING <flcm>
*                                           li_flcm[]
                                  CHANGING li_new_rebill[].
    ENDIF.
  ENDLOOP.

  IF li_new_rebill[] IS NOT INITIAL.
    APPEND LINES OF li_new_rebill TO li_flcm.
  ENDIF.

*** Process rebills in status "00" only
  LOOP AT li_flcm ASSIGNING <flcm>
      WHERE ztds_canc_rbil = 'R'.
    CASE <flcm>-flcm_pointer->get_status( ).
      WHEN '00'.
        PERFORM f_process_rebill USING <flcm>
                              CHANGING li_rebill[].
      WHEN '01'.   "Copy error to Cancellation
        PERFORM f_propagate_error USING <flcm>
                                        li_flcm[]
                               CHANGING li_rebill[].
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
  ENDLOOP.

*** Save data for each object created and report errors
  LOOP AT li_flcm ASSIGNING <flcm>.
    CALL METHOD <flcm>-flcm_pointer->save_data
      EXCEPTIONS
        db_error   = 1
        sbal_error = 2
        OTHERS     = 3.
    IF sy-subrc > 0.
      l_rollback = abap_true.
      EXIT.
    ENDIF.

    CALL METHOD <flcm>-flcm_pointer->report_errors
      CHANGING
        ch_text       = li_text[]
        ch_send_email = l_send_email.                       "DV5K966585
  ENDLOOP.

*** Save data for Rebills
  IF l_rollback = abap_false.
    SORT li_rebill ASCENDING.
    DELETE ADJACENT DUPLICATES FROM li_rebill.
    LOOP AT li_rebill ASSIGNING <rebill>.
      CALL METHOD <rebill>->save_data
        EXCEPTIONS
          db_error   = 1
          sbal_error = 2
          OTHERS     = 3.
      IF sy-subrc > 0.
        l_rollback = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

*** Commit or rollback
  IF l_rollback = abap_true.
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.

  CALL METHOD zcl_smint60a_zmm_flcm_tds=>free_all.

*** Send email notification
*  IF li_text[] IS NOT INITIAL.
  IF li_text[] IS NOT INITIAL AND l_send_email = abap_true. "DV5K966585
    PERFORM f_send_email USING li_text[].
  ENDIF.

ENDFORM.                    " F_IMPORT_TDS_DATA

*&---------------------------------------------------------------------*
*&      Form  F_SEND_EMAIL
*&---------------------------------------------------------------------*
FORM f_send_email  USING    pi_text TYPE soli_tab.

  DATA: l_receiver           TYPE somlreci1,
        l_document_data      TYPE sodocchgi1,
        li_text              TYPE soli_tab,
        li_address           TYPE TABLE OF zvu_email,
        l_packing_list       TYPE sopcklsti1,
        li_packing_list      TYPE TABLE OF sopcklsti1.

  STATICS: sti_receiver      TYPE somlreci1_t.

  FIELD-SYMBOLS: <address>   TYPE zvu_email.

*** Get email address from ZVU_EMAIL
  IF sti_receiver[] IS INITIAL.
    SELECT *
      FROM zvu_email
      INTO TABLE li_address
      WHERE application = 'SI0A01' AND
            parm_name   LIKE 'SMINT60A_%'.
    IF sy-subrc > 0.
      MESSAGE i003(input_rec).   "E-Mail address is missing
      RETURN.
    ENDIF.

    CLEAR l_receiver.
    l_receiver-rec_type = 'E'.
    l_receiver-com_type = 'INT'.
    APPEND l_receiver TO sti_receiver.

    IF sy-sysid = 'PRD'.
      LOOP AT li_address ASSIGNING <address>
          WHERE prod_rec_type = 'U' AND
                prod_rec_name IS NOT INITIAL.
        l_receiver-receiver = <address>-prod_rec_name.
        APPEND l_receiver TO sti_receiver.
      ENDLOOP.
    ELSE.
      LOOP AT li_address ASSIGNING <address>
          WHERE test_rec_type = 'U' AND
                test_rec_name IS NOT INITIAL.
        l_receiver-receiver = <address>-test_rec_name.
        APPEND l_receiver TO sti_receiver.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF sti_receiver[] IS NOT INITIAL.
    APPEND 'The following transactions coming from TDS failed and need to be reviewed:'(m02)
        TO li_text.
    APPEND INITIAL LINE TO li_text.
    APPEND 'Les transactions suivantes venant de TDS sont erronées et doivent être vérifiées :'(m03)
        TO li_text.
    APPEND INITIAL LINE TO li_text.
    APPEND LINES OF pi_text TO li_text.
    l_document_data-obj_descr  = 'TDS interface errors'(m01).
    l_packing_list-head_start = 1.
    l_packing_list-body_start = 1.
    l_packing_list-body_num   = LINES( li_text ).
    l_packing_list-doc_type   = 'RAW'.
    l_packing_list-doc_size   = l_packing_list-body_num * 255.
    APPEND l_packing_list TO li_packing_list.

    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data = l_document_data
        commit_work   = 'X'
      TABLES
        packing_list  = li_packing_list
        contents_txt  = li_text[]
        receivers     = sti_receiver[].
  ELSE.
    MESSAGE i003(input_rec).   "E-Mail address is missing
  ENDIF.

ENDFORM.                    " F_SEND_EMAIL

*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DUPLICATES
*&---------------------------------------------------------------------*
FORM f_check_duplicates USING p_flcm     TYPE t_flcm_ref
                              pi_flcm    TYPE t_flcm_ref_table.

  DATA: li_ztran_stus        TYPE STANDARD TABLE OF zztran_stus.

  FIELD-SYMBOLS: <duplicate> TYPE t_flcm_ref.

*** Check for duplicates in curent batch
  READ TABLE pi_flcm ASSIGNING <duplicate>
      WITH KEY ztds_tran_ref_nb = p_flcm-ztds_tran_ref_nb.
  IF sy-subrc = 0.     "Possible duplicate; check status
    IF <duplicate>-flcm_pointer->get_status( ) <> '01'.
      CALL METHOD p_flcm-flcm_pointer->set_status
        EXPORTING
          im_status = '01'.
      CALL METHOD p_flcm-flcm_pointer->log_error
        EXPORTING
          im_msgid = 'ZZ_FLCM'
          im_msgno = '002'
          im_msgty = 'E'
          im_msgv1 = p_flcm-ztds_tran_ref_nb.
      IF 1 = 2.      "For «Where Used» only
        MESSAGE e002(zz_flcm) WITH space.
      ENDIF.
      CALL METHOD p_flcm-flcm_pointer->clear_update_flag.
    ENDIF.
  ENDIF.

*** Check for duplicates on database
  SELECT ztran_stus
      FROM zmm_flcm_tds
      INTO TABLE li_ztran_stus
      WHERE ztds_tran_ref_nb = p_flcm-ztds_tran_ref_nb.
  IF sy-subrc = 0.
    DELETE li_ztran_stus WHERE table_line = '01'.
    IF li_ztran_stus[] IS NOT INITIAL.
      CALL METHOD p_flcm-flcm_pointer->set_status
        EXPORTING
          im_status = '01'.
      CALL METHOD p_flcm-flcm_pointer->log_error
        EXPORTING
          im_msgid = 'ZZ_FLCM'
          im_msgno = '002'
          im_msgty = 'E'
          im_msgv1 = p_flcm-ztds_tran_ref_nb.
      IF 1 = 2.      "For «Where Used» only
        MESSAGE e002(zz_flcm) WITH space.
      ENDIF.
      CALL METHOD p_flcm-flcm_pointer->clear_update_flag.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
*** Start of new code - FLCM defect 654                     DV5K967589
*** Check for duplicate BOL on «502» transactions only

  IF p_flcm-ztds_tran_type <> '502' OR
     p_flcm-ztds_canc_rbil IS NOT INITIAL.                  "DV5K968478
    RETURN.
  ENDIF.

*** Check for duplicate BOL in curent batch
  READ TABLE pi_flcm ASSIGNING <duplicate>
      WITH KEY ztds_trml_id = p_flcm-ztds_trml_id
               ztds_bol_nbr = p_flcm-ztds_bol_nbr
               ztds_tran_dt = p_flcm-ztds_tran_dt.    "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
  IF sy-subrc = 0.     "Possible duplicate; check status
    IF <duplicate>-flcm_pointer->get_status( ) <> '01'.
      CALL METHOD p_flcm-flcm_pointer->set_status
        EXPORTING
          im_status = '01'.
      CALL METHOD p_flcm-flcm_pointer->log_error
        EXPORTING
          im_msgid = 'ZZ_FLCM'
          im_msgno = '097'
          im_msgty = 'E'
          im_msgv1 = p_flcm-ztds_bol_nbr
          im_msgv2 = p_flcm-ztds_trml_id
          im_msgv3 = p_flcm-ztds_tran_dt.             "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
      IF 1 = 2.      "For «Where Used» only
*        MESSAGE e097(zz_flcm) WITH space space.      "D - TXT22260 - DV5K9A0ARX - CR302137 TK365122
        MESSAGE e097(zz_flcm) WITH space space space. "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
      ENDIF.
    ENDIF.
  ENDIF.

*** Check for duplicate BOL on database
  SELECT ztran_stus
      FROM zmm_flcm_tds
      INTO TABLE li_ztran_stus
      WHERE ztds_trml_id = p_flcm-ztds_trml_id AND
            ztds_bol_nbr = p_flcm-ztds_bol_nbr
        AND ztds_tran_dt = p_flcm-ztds_tran_dt.       "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
  IF sy-subrc = 0.
    DELETE li_ztran_stus WHERE table_line = '01'.
    IF li_ztran_stus[] IS NOT INITIAL.
      CALL METHOD p_flcm-flcm_pointer->set_status
        EXPORTING
          im_status = '01'.
      CALL METHOD p_flcm-flcm_pointer->log_error
        EXPORTING
          im_msgid = 'ZZ_FLCM'
          im_msgno = '097'
          im_msgty = 'E'
          im_msgv1 = p_flcm-ztds_bol_nbr
          im_msgv2 = p_flcm-ztds_trml_id
          im_msgv3 = p_flcm-ztds_tran_dt.             "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
      IF 1 = 2.      "For «Where Used» only
*        MESSAGE e097(zz_flcm) WITH space space.      "D - TXT22260 - DV5K9A0ARX - CR302137 TK365122
        MESSAGE e097(zz_flcm) WITH space space space. "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
      ENDIF.
    ENDIF.
  ENDIF.
*** End of new code - FLCM defect 654                       DV5K967589
*--------------------------------------------------------------------*

ENDFORM.                    " F_CHECK_DUPLICATES

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_CANCELLATION
*&---------------------------------------------------------------------*
FORM f_process_cancellation  USING    p_flcm     TYPE t_flcm_ref
*                                      pi_flcm    TYPE t_flcm_ref_table
                          CHANGING    pi_rebill  TYPE t_flcm_ref_table.

  DATA: lo_flcm              TYPE REF TO zcl_smint60a_zmm_flcm_tds.

  FIELD-SYMBOLS: <rebill>    TYPE t_flcm_ref.


*--------------------------------------------------------------------*
*** Check for original transaction
  CALL METHOD zcl_smint60a_zmm_flcm_tds=>new_object_bol
    EXPORTING
      im_tran_type     = p_flcm-ztds_tran_type
      im_trml_id       = p_flcm-ztds_trml_id
      im_bol_nbr       = p_flcm-ztds_bol_nbr
      im_tran_dt       = p_flcm-ztds_tran_dt    "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
      im_load_data     = abap_true
      im_cancel_rebill = ' '   "look for original
    CHANGING
      ch_pointer       = lo_flcm
    EXCEPTIONS
      not_found        = 1
      OTHERS           = 2.

  IF sy-subrc > 0.     "Original not found
    CALL METHOD p_flcm-flcm_pointer->set_status_substatus
      EXPORTING
        im_status    = '01'
        im_substatus = '030'.
    CALL METHOD p_flcm-flcm_pointer->log_error
      EXPORTING
        im_msgid = 'ZZ_FLCM'
        im_msgno = '008'
        im_msgty = 'E'
        im_msgv1 = p_flcm-ztds_trml_id
        im_msgv2 = p_flcm-ztds_bol_nbr
        im_msgv3 = p_flcm-ztds_tran_dt.       "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
    IF 1 = 2.      "For «Where Used» only
      MESSAGE e008(zz_flcm) WITH space space.
    ENDIF.
    RETURN.
  ELSE.
    IF lo_flcm->get_status( ) = '01'.      "original in error
      CALL METHOD p_flcm-flcm_pointer->set_status_substatus
        EXPORTING
          im_status    = '01'
          im_substatus = '032'.
      CALL METHOD p_flcm-flcm_pointer->log_error
        EXPORTING
          im_msgid = 'ZZ_FLCM'
          im_msgno = '026'
          im_msgty = 'E'
          im_msgv1 = p_flcm-ztds_trml_id
          im_msgv2 = p_flcm-ztds_bol_nbr
          im_msgv3 = p_flcm-ztds_tran_dt.       "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
      IF 1 = 2.      "For «Where Used» only
        MESSAGE e026(zz_flcm) WITH space space.
      ENDIF.
      RETURN.
    ENDIF.
  ENDIF.

*** Check for rebill out of sequence and reprocess if ok
  IF p_flcm-flcm_pointer->get_status( ) = '00'.
    CALL METHOD zcl_smint60a_zmm_flcm_tds=>new_object_bol
      EXPORTING
        im_tran_type          = p_flcm-ztds_tran_type
        im_trml_id            = p_flcm-ztds_trml_id
        im_bol_nbr            = p_flcm-ztds_bol_nbr
        im_load_data          = abap_true
        im_cancel_rebill      = 'R'   "look for rebill
        im_skip_current_batch = abap_true
      CHANGING
        ch_pointer            = lo_flcm
      EXCEPTIONS
        not_found             = 1
        OTHERS                = 2.

    IF sy-subrc = 0 AND lo_flcm->get_status( ) = '01'.
      IF lo_flcm->get_substatus( ) = '000'.   "Rebill has data errors
        CALL METHOD p_flcm-flcm_pointer->set_status_substatus
          EXPORTING
            im_status    = '01'
            im_substatus = '034'.
        CALL METHOD p_flcm-flcm_pointer->log_error
          EXPORTING
            im_msgid = 'ZZ_FLCM'
            im_msgno = '028'
            im_msgty = 'E'
            im_msgv1 = p_flcm-ztds_trml_id
            im_msgv2 = p_flcm-ztds_bol_nbr
            im_msgv3 = p_flcm-ztds_tran_dt.          "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
        IF 1 = 2.      "For «Where Used» only
          MESSAGE e028(zz_flcm) WITH space space.
        ENDIF.
      ELSE.               "Rebill has no data errors
        CALL METHOD lo_flcm->set_status_substatus
          EXPORTING
            im_status    = '00'       "Reset Rebill status to '00'
            im_substatus = '000'.     "Reset Rebill substatus to '000'
        APPEND INITIAL LINE TO pi_rebill ASSIGNING <rebill>.  "To reprocess
        <rebill>-ztds_tran_ref_nb = lo_flcm->get_tran_ref_nbr( ).
        <rebill>-ztds_trml_id = p_flcm-ztds_trml_id.
        <rebill>-ztds_bol_nbr = p_flcm-ztds_bol_nbr.
        <rebill>-ztds_canc_rbil = 'R'.
        <rebill>-flcm_pointer = lo_flcm.
        <rebill>-ztds_tran_dt   = lo_flcm->get_tran_date( ). "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
        <rebill>-ztds_tran_type = p_flcm-ztds_tran_type.     "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_process_cancellation

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_REBILL
*&---------------------------------------------------------------------*
FORM f_process_rebill  USING p_flcm     TYPE t_flcm_ref
                    CHANGING pi_pointer TYPE STANDARD TABLE.

  DATA: lo_flcm              TYPE REF TO zcl_smint60a_zmm_flcm_tds.


*** Get the corresponding Cancellation transaction
  CALL METHOD zcl_smint60a_zmm_flcm_tds=>new_object_bol
    EXPORTING
      im_tran_type     = p_flcm-ztds_tran_type
      im_trml_id       = p_flcm-ztds_trml_id
      im_bol_nbr       = p_flcm-ztds_bol_nbr
      im_tran_dt       = p_flcm-ztds_tran_dt    "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
      im_load_data     = abap_true
      im_cancel_rebill = 'C'      "Cancellation
    CHANGING
      ch_pointer       = lo_flcm
    EXCEPTIONS
      not_found        = 1
      OTHERS           = 2.

  IF sy-subrc > 0.       "No Cancellation found -> Error
    CALL METHOD p_flcm-flcm_pointer->set_status_substatus
      EXPORTING
        im_status    = '01'
        im_substatus = '031'.

    CALL METHOD p_flcm-flcm_pointer->log_error
      EXPORTING
        im_msgid = 'ZZ_FLCM'
        im_msgno = '008'
        im_msgty = 'E'
        im_msgv1 = p_flcm-ztds_trml_id
        im_msgv2 = p_flcm-ztds_bol_nbr
        im_msgv3 = p_flcm-ztds_tran_dt.               "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
    IF 1 = 2.      "For «Where Used» only
      MESSAGE e008(zz_flcm) WITH space space.
    ENDIF.
    RETURN.
*** Start of Insert TXT22260 - DV5K9A0ARX - CR302137 TK365122
** If rebill where date has changed, change field to blank
*  ELSE.
*    CLEAR p_flcm-ztds_canc_rbil.
*** End of Insert TXT22260 - DV5K9A0ARX - CR302137 TK365122
  ENDIF.

*** Cancellation transaction is found
  IF lo_flcm->get_status( ) = '01'.     "Cancellation in error
    IF lo_flcm->get_substatus( ) = '000' OR    "Data errors exist in cancellation
       lo_flcm->get_substatus( ) = '030' OR    "Original transaction not found
       lo_flcm->get_substatus( ) = '032'.      "Data errors exist in original
      CALL METHOD p_flcm-flcm_pointer->set_status_substatus
        EXPORTING
          im_status    = '01'   "Set Rebill status to '01'
          im_substatus = '033'.
      CALL METHOD p_flcm-flcm_pointer->log_error
        EXPORTING
          im_msgid = 'ZZ_FLCM'
          im_msgno = '027'
          im_msgty = 'E'
          im_msgv1 = p_flcm-ztds_trml_id
          im_msgv2 = p_flcm-ztds_bol_nbr
          im_msgv3 = p_flcm-ztds_tran_dt.       "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
      IF 1 = 2.      "For «Where Used» only
        MESSAGE e027(zz_flcm) WITH space space.
      ENDIF.
      APPEND lo_flcm TO pi_pointer.
    ELSE.
      CALL METHOD lo_flcm->set_status_substatus
        EXPORTING
          im_status    = '00'   "Set Cancallation status to '00'
          im_substatus = '00'.  "Set Cancellation substatus to '000
      APPEND lo_flcm TO pi_pointer.
    ENDIF.
  ENDIF.

  IF lo_flcm->get_status( ) = '00' AND
     lo_flcm->data_to_compare( ) =
     p_flcm-flcm_pointer->data_to_compare( ).
    CALL METHOD p_flcm-flcm_pointer->set_status_substatus
      EXPORTING
        im_status    = '02'
        im_substatus = '000'.
    CALL METHOD lo_flcm->set_status_substatus
      EXPORTING
        im_status    = '02'
        im_substatus = '000'.
    APPEND lo_flcm TO pi_pointer.
  ENDIF.

ENDFORM.                    " F_PROCESS_REBILL

*&---------------------------------------------------------------------*
*&      Form  f_propagate_error
*&---------------------------------------------------------------------*
FORM f_propagate_error       USING    p_flcm     TYPE t_flcm_ref
                                      pi_flcm    TYPE t_flcm_ref_table
                          CHANGING    pi_pointer TYPE STANDARD TABLE.

  DATA: lo_flcm              TYPE REF TO zcl_smint60a_zmm_flcm_tds.


  IF p_flcm-ztds_canc_rbil <> 'R'.        "Only for rebill
    RETURN.
  ENDIF.

  CALL METHOD zcl_smint60a_zmm_flcm_tds=>new_object_bol
    EXPORTING
      im_tran_type     = p_flcm-ztds_tran_type
      im_trml_id       = p_flcm-ztds_trml_id
      im_bol_nbr       = p_flcm-ztds_bol_nbr
      im_load_data     = abap_true
      im_cancel_rebill = 'C'
    CHANGING
      ch_pointer       = lo_flcm
    EXCEPTIONS
      not_found        = 1
      OTHERS           = 2.

  IF sy-subrc > 0.        "Cancellation not found; Return
    RETURN.
  ENDIF.

  IF lo_flcm->get_status( ) = '00'.
    CALL METHOD lo_flcm->set_status_substatus
      EXPORTING
        im_status    = '01'
        im_substatus = '034'.
    CALL METHOD lo_flcm->log_error
      EXPORTING
        im_msgid = 'ZZ_FLCM'
        im_msgno = '028'
        im_msgty = 'E'
        im_msgv1 = p_flcm-ztds_trml_id
        im_msgv2 = p_flcm-ztds_bol_nbr
        im_msgv3 = p_flcm-ztds_tran_dt.       "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
    IF 1 = 2.      "For «Where Used» only
      MESSAGE e028(zz_flcm) WITH space space.
    ENDIF.
    APPEND lo_flcm TO pi_pointer.
  ENDIF.

ENDFORM.                    "f_propagate_error

*&---------------------------------------------------------------------*
*&      Form  F_SBAL_BEFORE_UCOMM
*&
*&  >>> Called dynamically from ZCL_SMINT60A_ZMM_FLCM_TDS <<<
*&---------------------------------------------------------------------*
FORM f_sbal_before_ucomm CHANGING p_user_command TYPE bal_s_cbuc. "#EC CALLED

  DATA: l_log_number             TYPE balognr,
        l_log                    TYPE bal_s_log,
        li_tds_data              TYPE zmm_flcm_tds_interface_table,
        l_end_col                TYPE i,
        l_end_row                TYPE i,
        l_layout                 TYPE slis_layout_alv.


  CASE p_user_command-ucomm.
    WHEN '&IC1' OR '%DETAIL'.
      CLEAR p_user_command-ucomm.
** Details button pressed or double-click; provide log handle for selected
      CALL FUNCTION 'BAL_LOG_HDR_READ'
        EXPORTING
          i_log_handle  = p_user_command-list_msgh-log_handle
        IMPORTING
          e_s_log       = l_log
          e_lognumber   = l_log_number
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.
      IF sy-subrc = 0.
        IMPORT tds_data_table = li_tds_data[]
            FROM DATABASE bal_indx(zs) ID l_log_number.
        IF sy-subrc = 0.
          l_layout-zebra = abap_true.
          WRITE l_log-aldate TO l_layout-window_titlebar.
          WRITE l_log-altime TO l_layout-window_titlebar+12.
          l_end_col = sy-scols - 20.
          l_end_row = sy-srows - 15.
          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              is_layout             = l_layout
              i_structure_name      = 'ZMM_FLCM_TDS_INTERFACE'
              i_screen_start_column = 5
              i_screen_start_line   = 5
              i_screen_end_column   = l_end_col
              i_screen_end_line     = l_end_row
            TABLES
              t_outtab              = li_tds_data.
        ENDIF.
      ENDIF.
    WHEN '%EXT_PUSH1'.
      CLEAR p_user_command-ucomm.
      IF zcl_smint60a_zmm_flcm_tds=>log_tds_active( ) = abap_false.
        CALL METHOD zcl_smint60a_zmm_flcm_tds=>log_tds_on_off
          EXPORTING
            im_logging_on = abap_true.
        MESSAGE s083(zz_flcm).
        p_user_command-exit = abap_true.
      ENDIF.
    WHEN '%EXT_PUSH2'.
      CLEAR p_user_command-ucomm.
      IF zcl_smint60a_zmm_flcm_tds=>log_tds_active( ) = abap_true.
        CALL METHOD zcl_smint60a_zmm_flcm_tds=>log_tds_on_off
          EXPORTING
            im_logging_on = abap_false.
        MESSAGE s084(zz_flcm).
        p_user_command-exit = abap_true.
      ENDIF.
  ENDCASE.

ENDFORM.                    " F_SBAL_BEFORE_UCOMM
***Start of Insert - XT18903 - DV5K983656 - CR223019 T251976
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_BOL_NBR
*&---------------------------------------------------------------------*
*       Determine if TDS Bill Of Lading contains character values.
*----------------------------------------------------------------------*
FORM f_check_bol_nbr  USING    pwa_flcm   TYPE t_flcm_ref.

  CONSTANTS: lc_char  TYPE char04 VALUE 'CHAR'.

  DATA: l_data_type TYPE dd01v-datatype.

  IF NOT pwa_flcm-ztds_bol_nbr IS INITIAL.
    CALL FUNCTION 'NUMERIC_CHECK'
      EXPORTING
        string_in = pwa_flcm-ztds_bol_nbr
      IMPORTING
        htype     = l_data_type.

    IF l_data_type EQ lc_char.
      CALL METHOD pwa_flcm-flcm_pointer->set_status
        EXPORTING
          im_status = '01'.

      CALL METHOD pwa_flcm-flcm_pointer->log_error
        EXPORTING
          im_msgid = 'ZZ_FLCM'
          im_msgno = '100'
          im_msgty = 'E'
          im_msgv1 = pwa_flcm-ztds_bol_nbr.

      IF 1 = 2. "For «Where Used» only
        MESSAGE e100(zz_flcm) WITH space.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_CHECK_BOL_NBR
***End of Insert - XT18903 - DV5K983656 - CR223019 T251976
