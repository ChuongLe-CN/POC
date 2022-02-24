*&---------------------------------------------------------------------*
*&  Include           ZMI_SMINT029_CREF01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Canadian National                               *
*                                                                      *
* ABAP Name         : ZMI_SMINT029_TDS_FUEL_TO_EDW                     *
* Created by        : Bob Legault                                      *
* Created on        : 2011-04-08                                       *
* Function Design   : SM-INT-029                                       *
*                                                                      *
* Purpose: Daily extract of TDS fuel records in the ZMM_FLSM_TDS table *
*          which have either been processed or not will be extracted   *
*          and sent to Datacity                                        *
*                                                                      *
* Short Description : Extract of TDS records in the ZMM_FLSCM_TDS table*
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Manuel Gonzaga Jr.      2016-11-02                    DV5K9A052C     *
*                                                                      *
* Short Description : CR310744 TK342077                                *
*  - Add transaction date in the screen. Add radiobutton for option    *
*    to select flagged entries from table ZMM_FLSCM_TDS                *
*  - Change filename format if date range is entered                   *
*----------------------------------------------------------------------*
* Michel Richard      2014-10-14                        DV5K990722     *
*                                                                      *
* Short Description : CR250687 TK271836                                *
*   Changed  fields format from ZMM_FLCM_TDS                           *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Von Kaisser Almeria 2014-08-19                        DV5K989493     *
*                                                                      *
* Short Description : CR250144 TK271028                                *
*   Add new fields from ZMM_FLCM_TDS at the end of the output file     *
*----------------------------------------------------------------------*
* Eva Chan            2012-07-23                        DV5K973152     *
*                                                                      *
* Short Description : CR207792-T216420 SM-INT-029                      *
* to stop short dump of offet too large by passing the equnr           *
*----------------------------------------------------------------------*
* Bob Legault         2011-10-03                        DV5K966824     *
*                                                                      *
* Short Description : Defect 575 s/b aedat,aezeit,aename               *
*                     instead of erdat,erzeit,ername                   *
*----------------------------------------------------------------------*
* Bob Legault         2011-09-26                        DV5K966696     *
*                                                                      *
* Short Description : Defect 563 fill in                               *
*                     on update of zmm_flcm_tds erdat,erzeit,ername    *
*                     add message if file can not be closed correctly  *
*----------------------------------------------------------------------*
* Bob Legault         2011-09-22                        DV5K966636     *
*                                                                      *
* Short Description : Defect 554 Change select to get werks instead of *
*                     trml_id and select only till midnight            *
*----------------------------------------------------------------------*
* Bob Legault         2011-04-08                        DV5K960962     *
*                                                                      *
* Short Description : Initial Development                              *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      SUBROUTINES
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_SEND_EMAIL
*&---------------------------------------------------------------------*
*       Sends the email to COE SM & Business Users
*----------------------------------------------------------------------*
FORM f_send_email USING p_prcss  TYPE so_text255
                        p_faild  TYPE so_text255
                        p_strng1 TYPE so_text255.

* Mail Header / Email Subject
  PERFORM f_build_mail_header.
* Content of the email
  PERFORM f_build_mail_body USING p_prcss
                                  p_faild
                                  p_strng1.
* Main Recepient [TO]
  PERFORM f_get_email_address USING c_smint669_emailto space.
* Copy Recepient [CC]
  PERFORM f_get_email_address USING c_smint669_emailcc c_flag.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = wa_mail_hd
      put_in_outbox              = c_flag
      commit_work                = c_flag
    TABLES
      packing_list               = i_packing_list
      contents_txt               = i_mail_by
      receivers                  = i_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc NE 0.
    "Do Nothing
  ENDIF.

ENDFORM.                    " F_SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_MAIL_BODY
*&---------------------------------------------------------------------*
*       Populates the Emails Content
*----------------------------------------------------------------------*
FORM f_build_mail_body USING p_prcss  TYPE so_text255
                             p_faild  TYPE so_text255
                             p_strng1 TYPE so_text255.

  DATA: lwa_packing_list TYPE soxpl,
        l_text1          TYPE so_text255.

  CLEAR: lwa_packing_list,
         l_text1.

  CONCATENATE: text-006 text-007 INTO l_text1 SEPARATED BY space.

* Populate mail body
  PERFORM f_populate_mail_wa USING: text-004,
                                    space,
                                    text-005,
                                    space,
                                    text-009,
                                    space,
                                    l_text1,
                                    space,
                                    text-008,
                                    space,
                                    p_prcss,
                                    p_faild,
                                    space,
                                    p_strng1,
                                    space,
                                    text-002.

  lwa_packing_list-head_start = 1.
  lwa_packing_list-head_num = 0.
  lwa_packing_list-body_start = 1.
  DESCRIBE TABLE i_mail_by LINES lwa_packing_list-body_num.
  lwa_packing_list-objtp = c_raw.
  APPEND lwa_packing_list TO i_packing_list.
  SORT i_packing_list BY transf_bin ASCENDING.

ENDFORM.                    " F_BUILD_MAIL_BODY

*&---------------------------------------------------------------------*
*&      Form  F_POPULATE_MAIL_WA
*&---------------------------------------------------------------------*
*       Populate Email Body
*----------------------------------------------------------------------*
FORM f_populate_mail_wa USING p_text TYPE so_text255.
  DATA: lwa_mail_by TYPE solisti1.
  CLEAR lwa_mail_by.

  MOVE p_text TO lwa_mail_by.
  APPEND lwa_mail_by TO i_mail_by.
  CLEAR lwa_mail_by.

ENDFORM.                    " F_POPULATE_MAIL_WA

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_MAIL_HEADER
*&---------------------------------------------------------------------*
*      Build Email Header Details
*----------------------------------------------------------------------*
FORM f_build_mail_header.

  DATA: l_lines TYPE i,
        l_text  TYPE string.
  CLEAR: l_lines,
         l_text,
         wa_mail_hd.
*---> Start of Change XT16577 Defect66 2010/02/16 DV5K955634
*  CONCATENATE text-010 sy-datum INTO l_text SEPARATED BY space.
  CONCATENATE text-010 sy-datum INTO l_text SEPARATED BY space.
*---> End of Change XT16577 Defect66 2010/02/16 DV5K955634
  DESCRIBE TABLE i_mail_by LINES l_lines.
  wa_mail_hd-obj_name  = l_text.
  wa_mail_hd-obj_descr = l_text.
  wa_mail_hd-doc_size  = l_lines * 255.

ENDFORM.                    " F_BUILD_MAIL_HEADER

*&---------------------------------------------------------------------*
*&      Form  F_GET_EMAIL_ADDRESS
*&---------------------------------------------------------------------*
*      Get COE SM & Business Users Email Addresses
*----------------------------------------------------------------------*
FORM f_get_email_address USING p_parmname TYPE zvu_email-parm_name
                               p_cc       TYPE c.

  DATA: lwa_receivers   TYPE somlreci1,
        l_rectype       TYPE zvu_email-test_rec_type,
        l_email_address TYPE zvu_email-test_rec_name.

  CLEAR: l_rectype,
         l_email_address,
         lwa_receivers.

  CALL FUNCTION 'ZFI_GET_EMAIL_RECIPIENT'
    EXPORTING
      application    = c_smint669
      parm_name      = p_parmname
    IMPORTING
      recipient_type = l_rectype
      recipient_name = l_email_address
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  IF sy-subrc EQ 0.
    MOVE: l_rectype       TO lwa_receivers-rec_type,
          l_email_address TO lwa_receivers-receiver,
          c_int           TO lwa_receivers-com_type,
          p_cc            TO lwa_receivers-copy.
    APPEND lwa_receivers TO i_receivers.
  ENDIF.
  APPEND lwa_receivers TO i_receivers.

ENDFORM.                    " F_GET_EMAIL_ADDRESS
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_UNIX
*&---------------------------------------------------------------------*
*       Process the data to be loaded to SAP Directories and send email
*       if there are problems encountered in the loading
*----------------------------------------------------------------------*
FORM f_create_unix USING p_flag TYPE c.

  DATA:  l_conqty    TYPE char20,
         l_strng1    TYPE so_text255,
         l_strng2    TYPE so_text255,
         l_lines     TYPE i,
         l_num(9)    TYPE n,
         l_mess      TYPE so_text255,
         l_fail      TYPE i,
         l_prcss     TYPE so_text255,
         l_faild     TYPE so_text255.

  DATA:  lv_finished    TYPE boolean,
         lv_first_alpha TYPE boolean,
         lv_equnr       TYPE equnr,
         lv_counter     TYPE num2,
         lv_position    TYPE num2,
         lv_num(10)     TYPE c           VALUE '0123456789',
* Begin of DV5K973152
         lv_pattern    TYPE string VALUE '([A-Z]+)',
         lv_moff       TYPE i,
         lv_mlen       TYPE i,
         lv_equnr_part1 TYPE string,
         lv_equnr_part2 TYPE string.

* End of DV5K973152

  CLEAR: v_file,
         v_unix,
         l_conqty,
         l_lines,
         l_fail,
         l_strng1,
         l_num,
         l_mess,
         l_prcss,
         l_faild,
         l_strng2.


* begin of DV5K967547 Defect 652
  DATA:
    lo_flcm_exception TYPE REF TO zcx_flcm_error,
    lt_msg_sent       TYPE bapiret2_tab,
    lt_msg            TYPE bapiret2_tab.

  FIELD-SYMBOLS:
    <zmm_flcm_tds>    TYPE zmm_flcm_tds,
    <msg>             TYPE bapiret2.

* end of DV5K967547 Defect 652

* For the Creation of UNIX file for Transfer to FTP Server
***Start of Insert - XT21326 -  DV5K9A052C - CR310744 TK342077
  IF s_trndat[] IS NOT INITIAL.
    IF s_trndat-high IS NOT INITIAL.
      v_file = |{ c_prefix }{ s_trndat-high }|.
    ELSE.
      v_file = |{ c_prefix }{ s_trndat-low }|.
    ENDIF.
  ELSE.
***End of Insert - XT21326 -  DV5K9A052C - CR310744 TK342077
    CONCATENATE c_prefix sy-datum INTO v_file.
  ENDIF.                                                              "I - XT21326 -  DV5K9A052C - CR310744 TK342077

  CONCATENATE c_path1 sy-sysid c_path2 v_file c_flat INTO v_unix.
  CONCATENATE c_path1 sy-sysid c_path2 INTO l_mess.


  DESCRIBE TABLE i_zmm_flcm_tds LINES l_lines.
  l_fail = l_lines.
  OPEN DATASET v_unix FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.

*---> Start of Insert XT16577 2010/02/02 DV5K955251
    CONCATENATE text-003 l_mess INTO l_strng1 SEPARATED BY space.
*---> End of Insert XT16577 2010/02/02 DV5K955251

*   Sends the email to inform error in the creation of file
    PERFORM f_send_email USING l_prcss
                               l_faild
                               l_strng1.
*---> Start of Insert XT16577 2010/02/02 DV5K955251
    MESSAGE text-002 TYPE c_info.
    MESSAGE l_strng1 TYPE c_error.
*---> End of Insert XT16577 2010/02/02 DV5K955251
    RETURN.
  ENDIF.

  CLEAR: l_fail,
         l_lines.
  DESCRIBE TABLE i_zmm_flcm_tds LINES l_lines.
  l_fail = l_lines.
  CLEAR wa_zmm_flcm_tds.

  LOOP AT i_zmm_flcm_tds INTO wa_zmm_flcm_tds.



    CLEAR wa_text.
    l_fail = l_fail - 1.

    WRITE wa_zmm_flcm_tds-ztds_tran_ref_nb      TO wa_text(10).
    WRITE wa_zmm_flcm_tds-ztds_folio_seq        TO wa_text+10(4).
    WRITE wa_zmm_flcm_tds-werks                 TO wa_text+14(4).
    WRITE wa_zmm_flcm_tds-ztds_folio_yr         TO wa_text+18(4).
    WRITE wa_zmm_flcm_tds-ztds_folio_mth        TO wa_text+22(2).
    WRITE wa_zmm_flcm_tds-ztds_folio_nbr        TO wa_text+24(3).
    WRITE wa_zmm_flcm_tds-ztds_tran_type        TO wa_text+27(3).
    WRITE wa_zmm_flcm_tds-ztds_tran_dt          TO wa_text+30(8).
    WRITE wa_zmm_flcm_tds-ztds_tran_tm          TO wa_text+38(6).
    WRITE wa_zmm_flcm_tds-ztds_tran_tz          TO wa_text+44(6).
    WRITE wa_zmm_flcm_tds-ztds_bol_nbr          TO wa_text+50(10).
    WRITE wa_zmm_flcm_tds-lifnr                 TO wa_text+60(10).
    WRITE wa_zmm_flcm_tds-zcarrier              TO wa_text+70(10).
    WRITE wa_zmm_flcm_tds-zprod_owner           TO wa_text+80(10).
    WRITE wa_zmm_flcm_tds-ztds_canc_rbil        TO wa_text+90(1).

* Begin of DV5K973152
*    lv_finished = abap_false.
*    lv_first_alpha = abap_false.
*    lv_counter  = 0.
*    lv_position = 0.
*    clear lv_equnr.
*    while lv_finished eq abap_false.
*
*      if lv_first_alpha = abap_false.
*         if wa_zmm_flcm_tds-equnr+lv_counter(1) ca lv_num.
*            lv_first_alpha = abap_true.
*            lv_position = lv_position + ( 4 - lv_counter ).
*         endif.
*      endif.
*
*      if wa_zmm_flcm_tds-equnr+lv_counter(1) is not initial.
*         move wa_zmm_flcm_tds-equnr+lv_counter(1) to lv_equnr+lv_position(1).
*         add 1 to lv_position.
*      else.
*         lv_finished = abap_true.
*      endif.
*
*      if lv_counter eq 18.
*         lv_finished = abap_true.
*      else.
*         add 1 to lv_counter.
*      endif.
*
*   endwhile.
*
* Split the first 4 initial of equnr (alpha) with the rest of equnr number
* For example, CN1234 would be CN  1234 or GTW5816 would be GTW 5816

    CLEAR: lv_moff, lv_mlen,
           lv_equnr_part1,
           lv_equnr_part2,
           lv_equnr.
    SHIFT wa_zmm_flcm_tds-equnr LEFT DELETING LEADING space.
    TRANSLATE wa_zmm_flcm_tds-equnr TO UPPER CASE.

    FIND REGEX lv_pattern IN wa_zmm_flcm_tds-equnr IGNORING CASE
         MATCH OFFSET lv_moff
         MATCH LENGTH lv_mlen
           SUBMATCHES lv_equnr_part1.
    IF lv_equnr_part1 IS NOT INITIAL
    AND lv_moff = 0.
      lv_equnr = lv_equnr_part1.
      lv_equnr_part2 =  wa_zmm_flcm_tds-equnr+lv_mlen.
      lv_equnr+4 =  lv_equnr_part2.
    ELSE.
      SHIFT wa_zmm_flcm_tds-equnr LEFT DELETING LEADING '0'.
      lv_equnr+4 = wa_zmm_flcm_tds-equnr.
    ENDIF.
* End of DV5K973152
    wa_zmm_flcm_tds-equnr = lv_equnr.
    WRITE wa_zmm_flcm_tds-equnr                 TO wa_text+91(18).
    WRITE wa_zmm_flcm_tds-ztds_mnl_auto         TO wa_text+109(1).
    WRITE wa_zmm_flcm_tds-meins                 TO wa_text+110(3).
    WRITE wa_zmm_flcm_tds-ztds_grs_vol          TO wa_text+113(13).
    WRITE wa_zmm_flcm_tds-ztds_net_vol          TO wa_text+126(13).
    WRITE wa_zmm_flcm_tds-matnr                 TO wa_text+139(18).
    WRITE wa_zmm_flcm_tds-ztds_vol_sign         TO wa_text+157(1).
    WRITE wa_zmm_flcm_tds-ztds_phys_vol         TO wa_text+158(15).
    WRITE wa_zmm_flcm_tds-ztds_pgl              TO wa_text+173(15).
* start mrr
*    WRITE wa_zmm_flcm_tds-ztds_in_trst          TO wa_text+188(15).
*    WRITE wa_zmm_flcm_tds-ztds_in_yd            TO wa_text+203(15).
*    WRITE wa_zmm_flcm_tds-ztds_tot_inv          TO wa_text+218(15).
    data l_f(15)       type c.

    write wa_zmm_flcm_tds-ztds_in_trst LEFT-JUSTIFIED no-grouping no-sign to l_f.
    if wa_zmm_flcm_tds-ztds_in_trst < 0.
      concatenate '-' l_f into l_f .
    endif.
    write l_f RIGHT-JUSTIFIED TO wa_text+188(15).

    Clear l_f.
    write wa_zmm_flcm_tds-ztds_in_yd LEFT-JUSTIFIED no-grouping no-sign to l_f.
    if wa_zmm_flcm_tds-ztds_in_yd < 0.
      concatenate '-' l_f into l_f .
    endif.
    write l_f RIGHT-JUSTIFIED TO wa_text+203(15).

    Clear l_f.
    write wa_zmm_flcm_tds-ztds_tot_inv LEFT-JUSTIFIED no-grouping no-sign to l_f.
    if wa_zmm_flcm_tds-ztds_tot_inv < 0.
      concatenate '-' l_f into l_f .
    endif.
    write l_f RIGHT-JUSTIFIED TO wa_text+218(15).
* end mrr
    WRITE wa_zmm_flcm_tds-zeom_ind              TO wa_text+233(1).
    WRITE wa_zmm_flcm_tds-ztds_folio_frzdt      TO wa_text+234(8).
    WRITE wa_zmm_flcm_tds-werks                 TO wa_text+242(4). "DV5K966636
***BEGIN OF INSERT - XT18912 - CR250144 TK271028 - DV5K989493
    WRITE wa_zmm_flcm_tds-ztds_driver_id        TO wa_text+246(8).
    WRITE wa_zmm_flcm_tds-ztds_ld_start_dt      TO wa_text+254(8).
    WRITE wa_zmm_flcm_tds-ztds_ld_start_tm      TO wa_text+262(4).
    WRITE wa_zmm_flcm_tds-ztds_ld_end_dt        TO wa_text+266(8).
    WRITE wa_zmm_flcm_tds-ztds_ld_end_tm        TO wa_text+274(4).
***BEGIN OF INSERT - XT18912 - CR250144 TK271028 - DV5K989493

    TRANSFER wa_text TO v_unix.
    CLEAR: wa_zmm_flcm_tds.
  ENDLOOP.

* Closes the destination file.
  CLOSE DATASET v_unix.

  IF sy-subrc EQ 0.
* update the transfer flag
    CLEAR wa_zmm_flcm_tds.
    LOOP AT i_zmm_flcm_tds INTO wa_zmm_flcm_tds.

* begin of DV5K967547 Defect 652
      CLEAR lo_flcm_exception.
      REFRESH lt_msg_sent.
      APPEND INITIAL LINE TO lt_msg_sent ASSIGNING <msg>.
      <msg>-type        = c_success.
      <msg>-id          = c_zz_flcm.
      <msg>-number      = '095'.
      <msg>-message_v1  = wa_zmm_flcm_tds-ztds_tran_ref_nb.
      <msg>-message_v2  = c_edw.

* Update transaction status to error with message
      CALL METHOD zcl_smint60a_zmm_flcm_tds=>set_status_and_messages
        EXPORTING
          im_tran_ref_nbr = wa_zmm_flcm_tds-ztds_tran_ref_nb
          im_commit       = abap_true
          im_tran_status  = wa_zmm_flcm_tds-ztran_stus
          im_messages     = lt_msg_sent
        EXCEPTIONS
          error_in_save   = 1
          OTHERS          = 2.

      IF ( sy-subrc <> 0 ).
        IF ( 1 = 2 ). MESSAGE e039(zz_flcm). ENDIF    .
        ADD 1 TO l_fail.

      ELSE.
* update separate for the flag and actual change because the method above will only update if certain
* fields are changed
        IF s_trndat[] IS INITIAL.                                           " I - XT21326 -  DV5K9A052C - CR310744 TK342077
          UPDATE zmm_flcm_tds SET zsmint_029_ind = abap_true
                              aedat          = sy-datum       "DV5K966824
                              aezeit         = sy-uzeit       "DV5K966824
                              aename         = sy-uname       "DV5K966824
              WHERE ztds_tran_ref_nb = wa_zmm_flcm_tds-ztds_tran_ref_nb.

          IF sy-subrc EQ 0.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
            EXIT.
          ENDIF.
        ENDIF.                                                              " I - XT21326 -  DV5K9A052C - CR310744 TK342077
      ENDIF.
    ENDLOOP.
  ELSE.                                                     "DV5K966695
* if file can not be closed correctly                                                                    "DV5K966695
* Display Error Message                                                                                  "DV5K966695
    CONCATENATE text-014 v_unix INTO l_strng1 SEPARATED BY space. "DV5K966695
    MESSAGE l_strng1 TYPE c_error.                          "DV5K966695
    EXIT.                                                   "DV5K966695
  ENDIF.                                                    "DV5K966695




* Report Log
  IF l_lines IS NOT INITIAL.
    l_num = l_lines.
    SHIFT l_num LEFT DELETING LEADING c_zero.
    CONCATENATE text-011 l_num INTO l_strng1 SEPARATED BY space.
  ELSE.
    CONCATENATE text-011 c_zero INTO l_strng1 SEPARATED BY space.
  ENDIF.
  CLEAR l_num.
  IF l_fail > 0.
    l_num = l_fail.
    SHIFT l_num LEFT DELETING LEADING c_zero.
    CONCATENATE text-012 l_num INTO l_strng2 SEPARATED BY space.
  ELSE.
    CONCATENATE text-012 c_zero INTO l_strng2 SEPARATED BY space.
  ENDIF.

* Display Success Message
  MESSAGE l_strng1 TYPE c_success.
  MESSAGE l_strng2 TYPE c_success.

ENDFORM.                    " F_CREATE_UNIX
*&---------------------------------------------------------------------*
*&      Form  F_DATA_RETRIEVAL
*&---------------------------------------------------------------------*
*       Gets the data from main database table zsm_dtl_fuel_evt
*----------------------------------------------------------------------*
*      <--P_SUBRC  Return Code [If not initial then no data retrieved]
*----------------------------------------------------------------------*
FORM f_data_retrieval  CHANGING p_subrc TYPE sy-subrc.

  DATA: lv_datum TYPE sy-datum.                             "DV5K966637

  lv_datum = sy-datum - 1.                                  "DV5K966637

*"Retrieve from ZMM_FLCM_TDS
***Start of Insert - XT21326 -  DV5K9A052C - CR310744 TK342077
  IF s_trndat[] IS NOT INITIAL.
    SELECT *
      FROM zmm_flcm_tds
      INTO TABLE i_zmm_flcm_tds
     WHERE ztds_tran_type   IN s_trnsid
       AND ztran_stus       IN s_trnsts
       AND ztds_tran_dt     IN s_trndat.
    IF sy-subrc IS INITIAL.
      IF rb_flag IS NOT INITIAL.
        DELETE i_zmm_flcm_tds WHERE zsmint_029_ind IS INITIAL.
      ELSEIF rb_unf IS NOT INITIAL.
        DELETE i_zmm_flcm_tds WHERE zsmint_029_ind IS NOT INITIAL.
      ENDIF.
    ENDIF.
  ELSE.
***End of Insert - XT21326 -  DV5K9A052C - CR310744 TK342077
    SELECT *
      FROM zmm_flcm_tds
      INTO TABLE i_zmm_flcm_tds
     WHERE ztds_tran_type   IN s_trnsid
       AND ztran_stus       IN s_trnsts
       AND zsmint_029_ind   EQ abap_false
       AND ztds_tran_dt     LE lv_datum.                      "DV5K966637
  ENDIF.                                                                  "I - XT21326 -  DV5K9A052C - CR310744 TK342077

ENDFORM.                    " F_DATA_RETRIEVAL
*&---------------------------------------------------------------------*
*&      Form  F_INITIALIZE
*&---------------------------------------------------------------------*
*       Refreshes internal tables and clears work areas & variables
*----------------------------------------------------------------------*
FORM f_initialize .

  REFRESH: i_mail_by,
           i_zmm_flcm_tds,
           i_receivers.

  CLEAR:   wa_text,
           wa_mail_hd.

  CLEAR:   v_file,
           v_subrc,                                    "A-XT16577 2010/01/11 DV5K954669
           v_unix.

ENDFORM.                    " F_INITIALIZE
