*----------------------------------------------------------------------*
*                      Canadian National                               *
*                                                                      *
* ABAP Name :      ZME_SMENH8A7_TDS_TDL_TRANSFF01                      *
* Created by:      Chuong Le                                           *
* Created on:      2011-01-31                                          *
* Short Description: Transfer Cartage fuelling events from TDS to DTL. *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by             Date                Tracking number *
* -------------------------        ----------          --------------- *
* Chuong Le                        2011-01-31          DV5K960962      *
*                                                                      *
* Short Description : Initial Creation                                 *
*----------------------------------------------------------------------*
* Chuong Le                        2011-06-02          DV5K964065      *
*                                                                      *
* Short desc: Fix defect 227.                                          *
*----------------------------------------------------------------------*
* Chuong Le                        2011-09-23          DV5K966678      *
*                                                                      *
* Short desc: Fix defect 560.                                          *
*             - Remove transaction status 02 from selection.           *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_INITIAL_DATA
*&---------------------------------------------------------------------*
FORM f_initial_data .

  CLEAR: v_total_cnt, v_success_cnt, v_error_cnt.

  REFRESH: i_flcm_tds, i_tds_success, i_flcm_parms,
           i_dtl_fuel, i_vend_crtg, i_lfa1, i_t001w,
           i_xref, i_plntdef, i_email, i_log,
           i_contents, i_receivers.

ENDFORM.                    " F_INITIAL_DATA

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
FORM f_get_data .

  DATA: l_index TYPE sy-index.

* Get parameter values
  SELECT * INTO TABLE i_flcm_parms
    FROM zmm_flcm_parms
    WHERE progname = c_smenh8a7.

  IF sy-subrc <> 0.
    MESSAGE e044(zz_flcm) WITH c_smenh8a7 c_trans_type.
    EXIT.
  ENDIF.

* Extract Cross reference values
  SELECT * INTO TABLE i_xref
    FROM zmm_flcm_xref
    WHERE zval_qlfy = c_tds2dtl.

  IF sy-subrc <> 0.
    MESSAGE e045(zz_flcm).
    EXIT.
  ENDIF.

  LOOP AT i_flcm_parms INTO wa_flcm_parms.
    CASE wa_flcm_parms-zparm_nm.
*     Select TDS records
      WHEN c_trans_type.
        IF wa_flcm_parms-zval_from+4(4) IS NOT INITIAL.
*         With plant specified
          SELECT * FROM zmm_flcm_tds
            APPENDING TABLE i_flcm_tds
            WHERE ztds_tran_type = wa_flcm_parms-zval_from(3)
              AND werks          = wa_flcm_parms-zval_from+4(4)
              AND ztds_canc_rbil IN (c_blank, c_c, c_r)
*              AND ztran_stus IN (c_00, c_02, c_03 )       "DV5K966678-
              AND ztran_stus IN (c_00, c_03 )              "DV5K966678+
              AND ztran_sub_stus NE c_99.                                                                 "DV5K965071
        ELSE.
*         Without plant specified
          SELECT * FROM zmm_flcm_tds
            APPENDING TABLE i_flcm_tds
            WHERE ztds_tran_type = wa_flcm_parms-zval_from(3)
              AND ztds_canc_rbil IN (c_blank, c_c, c_r)
*              AND ztran_stus IN (c_00, c_02, c_03 )       "DV5K966678-
              AND ztran_stus IN (c_00, c_03 )              "DV5K966678+
              AND ztran_sub_stus NE c_99.                                                                 "DV5K965071
        ENDIF.

*     Select vendors for turning off the cartage indicator
      WHEN c_vend_crtg.
        IF wa_flcm_parms-zval_to IS NOT INITIAL.
          CLEAR wa_vend_crtg.
          wa_vend_crtg-zval_from = wa_flcm_parms-zval_from.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_flcm_parms-zval_to
            IMPORTING
              output = wa_vend_crtg-lifnr.
          APPEND wa_vend_crtg TO i_vend_crtg.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  F_GET_ADDITIONAL_DATA
*&---------------------------------------------------------------------*
FORM f_get_additional_data .

  CHECK NOT i_flcm_tds[] IS INITIAL.

* Extract SPLC code
  SELECT werks zplnt_note
    INTO TABLE i_plntdef
    FROM zmm_flcm_plntdef
    FOR ALL ENTRIES IN i_flcm_tds
    WHERE werks       = i_flcm_tds-werks
      AND zplnt_note <> space.

* Extract vendor data
  SELECT lifnr name1
    INTO TABLE i_lfa1
    FROM lfa1
    FOR ALL ENTRIES IN i_flcm_tds
    WHERE lifnr = i_flcm_tds-zcarrier.                                                                     "DV5K965071

* Extract plant data
  SELECT werks name1 ort01 regio land1
    INTO TABLE i_t001w
    FROM t001w
    FOR ALL ENTRIES IN i_flcm_tds
    WHERE werks = i_flcm_tds-werks.

* Vendor email address
  SELECT a~lifnr a~prsnr b~smtp_addr
    INTO TABLE i_email
    FROM knvk AS a
    INNER JOIN adr6 AS b ON b~persnumber = a~prsnr
    FOR ALL ENTRIES IN i_flcm_tds
    WHERE a~lifnr      = i_flcm_tds-zcarrier
      AND a~pafkt      = c_z7
      AND b~date_from <= sy-datum.

ENDFORM.                    " F_GET_ADDITIONAL_DATA

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_DATA
*&---------------------------------------------------------------------*
FORM f_process_data .

  SORT: i_xref       BY zvar_nm zval_from,
        i_plntdef    BY werks,
        i_lfa1       BY lifnr,
        i_t001w      BY werks,
        i_email      BY lifnr,
        i_flcm_parms BY zparm_nm zval_from,
        i_vend_crtg  BY zval_from lifnr.

* FIRST - process all New records
  PERFORM f_process_new_rec.

* SECOND - process all Cancel record
  PERFORM f_process_update_cancel_rec USING  c_c.

* LAST - process all Rebill records
  PERFORM f_process_update_cancel_rec USING  c_r.

ENDFORM.                    " F_PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_NEW_REC
*&---------------------------------------------------------------------*
FORM f_process_new_rec .

  DATA: l_subrc      TYPE sy-subrc,
        l_id_max     TYPE zzdtl_fuel_event_id,
        li_tds_tab   TYPE TABLE OF zmm_flcm_tds.

* Get the maximum fuel id number
  SELECT MAX( zdtl_fevt_id ) INTO l_id_max
    FROM zmm_dtl_fuel_evt.

  REFRESH: li_tds_tab, i_dtl_fuel, i_tds_success.

  li_tds_tab[] = i_flcm_tds[].
  DELETE li_tds_tab WHERE ztds_canc_rbil <> space.

  IF li_tds_tab[] IS INITIAL.
    EXIT.
  ENDIF.

  LOOP AT li_tds_tab INTO wa_flcm_tds.
    ADD 1 TO v_total_cnt.
    PERFORM f_validate_data CHANGING l_subrc
                                     v_msgtext.
    IF l_subrc <> 0.
      ADD 1 TO v_error_cnt.
*     Insert error record to message log
      PERFORM f_insert_message_log USING wa_flcm_tds
                                         c_e
                                         v_msgtext.
      CONTINUE.
    ELSE.
      ADD 1 TO l_id_max.
      PERFORM f_map_fields USING    wa_flcm_tds
                                    l_id_max
                                    1.

*     Update status after process
      wa_flcm_tds-ztran_sub_stus = c_99.
* begin of DV5K967548 Defect 652
      wa_flcm_tds-aedat  = sy-datum.
      wa_flcm_tds-aezeit = sy-uzeit.
      wa_flcm_tds-aename = sy-uname.
* end of DV5K967548 Defect 652
      APPEND wa_flcm_tds TO i_tds_success.

      ADD 1 TO v_success_cnt.

*     Insert successful record to message log
      MESSAGE i614(67) INTO v_msgtext.
      PERFORM f_insert_message_log USING wa_flcm_tds
                                         c_s
                                         v_msgtext.
    ENDIF.
  ENDLOOP.

  PERFORM f_update_table.

ENDFORM.                    " F_PROCESS_NEW_REC

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_UPDATE_CANCEL_REC
*&---------------------------------------------------------------------*
FORM f_process_update_cancel_rec USING  p_rec_type.

  TYPES: BEGIN OF t_bol_num,
           zcarrier     TYPE zzcarrier_lifnr,
           ztds_bol_nbr TYPE zzbol_ticket_nbr,
           werks        TYPE werks_d,
         END OF t_bol_num.

  DATA: l_status_ind TYPE zzdtl_rec_status,
        l_cancel_ind TYPE zzevt_canc_ind,
        l_subrc      TYPE sy-subrc,
        li_tds_tab   TYPE TABLE OF zmm_flcm_tds,
        li_bol_num   TYPE STANDARD TABLE OF t_bol_num,
        lwa_bol_num  TYPE t_bol_num.

  REFRESH: li_tds_tab, i_fuel_evt, i_dtl_fuel,
           i_tds_success, li_bol_num.

  li_tds_tab[] = i_flcm_tds[].
  DELETE li_tds_tab WHERE ztds_canc_rbil <> p_rec_type.

  IF li_tds_tab[] IS INITIAL.
    EXIT.
  ENDIF.

  IF p_rec_type = c_c.    "Cancel
    l_status_ind = c_a.
    l_cancel_ind = c_n.
  ELSE.                   "Rebill
    l_status_ind = c_i.
    l_cancel_ind = c_y.
  ENDIF.

* Convert BOL number from 10 chars to 12 chars
  LOOP AT li_tds_tab INTO wa_flcm_tds.
    CLEAR lwa_bol_num.
    lwa_bol_num-zcarrier     = wa_flcm_tds-zcarrier.
    lwa_bol_num-ztds_bol_nbr = wa_flcm_tds-ztds_bol_nbr.
    lwa_bol_num-werks        = wa_flcm_tds-werks.
    APPEND lwa_bol_num TO li_bol_num.
  ENDLOOP.

  SELECT * INTO TABLE i_fuel_evt
  FROM zmm_dtl_fuel_evt
  FOR ALL ENTRIES IN li_bol_num
  WHERE lifnr         = li_bol_num-zcarrier
    AND zbol_tckt_nbr = li_bol_num-ztds_bol_nbr
    AND werks         = li_bol_num-werks
    AND zrec_stus     = l_status_ind
    AND zcanc_ind     = l_cancel_ind.

  SORT i_fuel_evt BY lifnr werks zbol_tckt_nbr
                     zdtl_fevt_id zdtl_fevt_ver DESCENDING.

* Keep the record with the highest version
  DELETE ADJACENT DUPLICATES FROM i_fuel_evt
                  COMPARING lifnr werks zbol_tckt_nbr zdtl_fevt_id.

  LOOP AT li_tds_tab INTO wa_flcm_tds.
    ADD 1 TO v_total_cnt.
    PERFORM f_validate_data CHANGING l_subrc
                                     v_msgtext.
    IF l_subrc <> 0.
      ADD 1 TO v_error_cnt.
*     Insert error record to message log
      PERFORM f_insert_message_log USING wa_flcm_tds
                                         c_e
                                         v_msgtext.
      CONTINUE.
    ELSE.
      CLEAR wa_dtl_fuel.
      READ TABLE i_fuel_evt INTO wa_dtl_fuel
                 WITH KEY lifnr         = wa_flcm_tds-zcarrier
                          werks         = wa_flcm_tds-werks
                          zbol_tckt_nbr = wa_flcm_tds-ztds_bol_nbr
                          BINARY SEARCH.
      IF sy-subrc <> 0.
        ADD 1 TO v_error_cnt.
*       Insert error record to message log
        MESSAGE i050(zz_flcm) INTO v_msgtext.
        PERFORM f_insert_message_log USING wa_flcm_tds
                                           c_e
                                           v_msgtext.
      ELSE.
        IF p_rec_type = c_c.      "Cancel
*         Update existing new record
          wa_dtl_fuel-zrec_stus = c_i.
          wa_dtl_fuel-aedat     = sy-datum.
          wa_dtl_fuel-aezeit    = sy-uzeit.
          wa_dtl_fuel-aename    = sy-uname.
          APPEND wa_dtl_fuel TO i_dtl_fuel.

*         Create cancel record with the same id and a new version
          PERFORM f_map_fields USING  wa_flcm_tds
                                      wa_dtl_fuel-zdtl_fevt_id
                                      wa_dtl_fuel-zdtl_fevt_ver.
        ELSE.                     "Rebill
*         Update existing cancel record
          PERFORM f_map_fields USING  wa_flcm_tds
                                      wa_dtl_fuel-zdtl_fevt_id
                                      wa_dtl_fuel-zdtl_fevt_ver.
        ENDIF.

*       Update TDS status after process
        wa_flcm_tds-ztran_sub_stus = c_99.
* begin of DV5K967548 Defect 652
        wa_flcm_tds-aedat  = sy-datum.
        wa_flcm_tds-aezeit = sy-uzeit.
        wa_flcm_tds-aename = sy-uname.
* end of DV5K967548 Defect 652
        APPEND wa_flcm_tds TO i_tds_success.

        ADD 1 TO v_success_cnt.

*       Insert successful record to message log
        MESSAGE i614(67) INTO v_msgtext.
        PERFORM f_insert_message_log USING wa_flcm_tds
                                           c_s
                                           v_msgtext.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM f_update_table.

ENDFORM.                    " F_PROCESS_UPDATE_CANCEL_REC

*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_DATA
*&---------------------------------------------------------------------*
FORM f_validate_data  CHANGING p_subrc
                               p_msgtext.

  DATA: l_parm_val TYPE tvarv_val.

  CLEAR: p_subrc, p_msgtext.

  CLEAR: wa_lfa1, wa_t001w, wa_xref_matkl,
         wa_plntdef, wa_email, wa_flcm_parms, l_parm_val.

  CONCATENATE wa_flcm_tds-ztds_tran_type
              wa_flcm_tds-werks
         INTO l_parm_val SEPARATED BY c_dash.

* Get Transload indicator value
* 1st try - using Trans type and plant
  READ TABLE i_flcm_parms INTO wa_flcm_parms
                          WITH KEY zparm_nm  = c_trans_type
                                   zval_from = l_parm_val
                          BINARY SEARCH.
  IF sy-subrc <> 0.
*   2nd try - using only Trans type
    READ TABLE i_flcm_parms INTO wa_flcm_parms
                            WITH KEY zparm_nm  = c_trans_type
                                     zval_from = wa_flcm_tds-ztds_tran_type
                            BINARY SEARCH.
    IF sy-subrc <> 0.
      p_subrc = sy-subrc.
      MESSAGE i047(zz_flcm) INTO p_msgtext
                            WITH wa_flcm_tds-ztds_tran_type.
      EXIT.
    ENDIF.
  ENDIF.

* Get vendor info
  READ TABLE i_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_flcm_tds-zcarrier                                     "DV5K965071
                                 BINARY SEARCH.
  IF sy-subrc <> 0.
    p_subrc = sy-subrc.
    MESSAGE i170(q3) INTO p_msgtext WITH wa_flcm_tds-zcarrier.                                             "DV5K965071
    EXIT.
  ENDIF.

* Get plant info
  READ TABLE i_t001w INTO wa_t001w WITH KEY werks = wa_flcm_tds-werks
                                   BINARY SEARCH.
  IF sy-subrc <> 0.
    p_subrc = sy-subrc.
    MESSAGE i087(wr) INTO p_msgtext WITH wa_flcm_tds-werks.
    EXIT.
  ENDIF.

* Get cross reference value for MATKL
  READ TABLE i_xref INTO wa_xref_matkl
                    WITH KEY zvar_nm   = c_matkl
                             zval_from = wa_flcm_tds-matnr
                    BINARY SEARCH.
  IF sy-subrc <> 0.
    p_subrc = sy-subrc.
    MESSAGE i407(09) INTO p_msgtext WITH wa_flcm_tds-matnr.
    EXIT.
  ENDIF.

* Get SPLC code value
  READ TABLE i_plntdef INTO wa_plntdef
                       WITH KEY werks = wa_flcm_tds-werks
                       BINARY SEARCH.
  IF sy-subrc <> 0.
    p_subrc = sy-subrc.
    MESSAGE i048(zz_flcm) INTO p_msgtext WITH wa_flcm_tds-werks.
    EXIT.
  ENDIF.

* Get vendor email
  READ TABLE i_email INTO wa_email
                     WITH KEY lifnr = wa_flcm_tds-zcarrier
                     BINARY SEARCH.
  IF sy-subrc <> 0.
    p_subrc = sy-subrc.
    MESSAGE i049(zz_flcm) INTO p_msgtext WITH wa_flcm_tds-zcarrier.
    EXIT.
  ENDIF.

ENDFORM.                    " F_VALIDATE_DATA

*&---------------------------------------------------------------------*
*&      Form  F_MAP_FIELDS
*&---------------------------------------------------------------------*
FORM f_map_fields  USING    ps_tds STRUCTURE zmm_flcm_tds
                            p_id
                            p_version.

  DATA: l_parm_val TYPE tvarv_val,
        l_trans_id TYPE char10.

  CASE ps_tds-ztds_canc_rbil.
    WHEN c_c.     "Cancel record
      wa_dtl_fuel-zdtl_fevt_id  = p_id.
      wa_dtl_fuel-zdtl_fevt_ver = p_version + 1.
      wa_dtl_fuel-zdtl_mod_cd   = c_u.
      wa_dtl_fuel-zrec_stus     = c_i.
      wa_dtl_fuel-zcanc_ind     = c_y.
      wa_dtl_fuel-erdat         = sy-datum.
      wa_dtl_fuel-erzeit        = sy-uzeit.
      wa_dtl_fuel-ername        = sy-uname.
      CLEAR: wa_dtl_fuel-aedat,
             wa_dtl_fuel-aezeit,
             wa_dtl_fuel-aename.
      APPEND wa_dtl_fuel TO i_dtl_fuel.
      EXIT.

    WHEN c_r.     "Rebill record
      wa_dtl_fuel-zdtl_fevt_id  = p_id.
      wa_dtl_fuel-zdtl_fevt_ver = p_version.
      wa_dtl_fuel-zdtl_mod_cd   = c_u.
      wa_dtl_fuel-zrec_stus     = c_a.
      wa_dtl_fuel-zcanc_ind     = c_n.
      wa_dtl_fuel-zuser_stus    = c_op.
      wa_dtl_fuel-aedat         = sy-datum.
      wa_dtl_fuel-aezeit        = sy-uzeit.
      wa_dtl_fuel-aename        = sy-uname.

    WHEN space.   "New record
      CLEAR wa_dtl_fuel.
      wa_dtl_fuel-zdtl_fevt_id  = p_id.
      wa_dtl_fuel-zdtl_fevt_ver = p_version.
      wa_dtl_fuel-zdtl_mod_cd   = c_a.
      wa_dtl_fuel-zrec_stus     = c_a.
      wa_dtl_fuel-zcanc_ind     = c_n.
      wa_dtl_fuel-erdat         = sy-datum.
      wa_dtl_fuel-erzeit        = sy-uzeit.
      wa_dtl_fuel-ername        = sy-uname.
  ENDCASE.

  wa_dtl_fuel-mandt          = sy-mandt.
  wa_dtl_fuel-zdlvr_dt       = ps_tds-ztds_tran_dt.
  wa_dtl_fuel-zdlvr_tm       = ps_tds-ztds_tran_tm.
  wa_dtl_fuel-zbol_tckt_nbr  = ps_tds-ztds_bol_nbr.
  wa_dtl_fuel-lifnr          = ps_tds-zcarrier.
  wa_dtl_fuel-zdlvr_vol_uom  = ps_tds-meins.
  wa_dtl_fuel-matkl          = wa_xref_matkl-zval_to.
  wa_dtl_fuel-zdlvr_fuel_vol = ps_tds-ztds_net_vol.
  wa_dtl_fuel-werks          = ps_tds-werks.
  wa_dtl_fuel-zdtl_splc      = wa_plntdef-zplnt_note.
  wa_dtl_fuel-zxchg_wttm_ind = c_n.
  wa_dtl_fuel-zdtl_mtch_cd   = c_na.
  wa_dtl_fuel-zdtl_load_dt   = sy-datum.
  wa_dtl_fuel-zdtl_yrd_nm    = wa_t001w-name1.
  wa_dtl_fuel-zdtl_city      = wa_t001w-ort01.
  wa_dtl_fuel-zdtl_prst      = wa_t001w-regio.
  wa_dtl_fuel-zdtl_ctry      = wa_t001w-land1.
  wa_dtl_fuel-zdtl_acct_nm   = wa_lfa1-name1.                                                              "DV5K965377
  wa_dtl_fuel-zvdr_nm        = wa_lfa1-name1.
  wa_dtl_fuel-zeml_addr      = wa_email-smtp_addr.

  IF ps_tds-equnr CA c_digits.
    IF sy-fdpos = 0.
      wa_dtl_fuel-zdtl_loco_nbr  = ps_tds-equnr.
    ELSE.
      wa_dtl_fuel-zdtl_loco_init = ps_tds-equnr(sy-fdpos).
      wa_dtl_fuel-zdtl_loco_nbr  = ps_tds-equnr+sy-fdpos.
    ENDIF.
  ELSE.
    wa_dtl_fuel-zdtl_loco_init = ps_tds-equnr.
  ENDIF.

  IF wa_flcm_parms-zval_to = c_transload.
    wa_dtl_fuel-zfuel_tsld_ind = c_y.
  ELSE.
    wa_dtl_fuel-zfuel_tsld_ind = c_n.
  ENDIF.

* Determine cartage indicator value
  CONCATENATE ps_tds-ztds_tran_type
              ps_tds-werks
         INTO l_parm_val SEPARATED BY c_dash.

* 1st try - using Trans type, plant and vendor
  READ TABLE i_vend_crtg INTO wa_vend_crtg
                         WITH KEY zval_from = l_parm_val
                                  lifnr     = ps_tds-zcarrier
                         BINARY SEARCH TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
*   2nd try - using only Trans type and vendor
    READ TABLE i_vend_crtg INTO wa_vend_crtg
                           WITH KEY zval_from = ps_tds-ztds_tran_type
                                    lifnr     = ps_tds-zcarrier
                           BINARY SEARCH TRANSPORTING NO FIELDS.
  ENDIF.

  IF sy-subrc = 0.
    wa_dtl_fuel-zcrtg_ind = c_n.
    wa_dtl_fuel-ztran_stus = 0.
  ELSE.
    wa_dtl_fuel-zcrtg_ind = c_y.
    wa_dtl_fuel-ztran_stus = 2.
  ENDIF.

  CONCATENATE ps_tds-ztds_folio_yr
              ps_tds-ztds_folio_mth
              ps_tds-ztds_folio_nbr
         INTO l_trans_id.
  wa_dtl_fuel-zdtl_trns_id = l_trans_id.

  CONCATENATE c_tds
              ps_tds-ztds_folio_seq
         INTO wa_dtl_fuel-zdtl_dlvr_nbr SEPARATED BY c_dash.

  CONCATENATE c_tds
              ps_tds-werks
         INTO wa_dtl_fuel-zdtl_acct_id SEPARATED BY c_dash.

  APPEND wa_dtl_fuel TO i_dtl_fuel.

ENDFORM.                    " F_MAP_FIELDS

*&---------------------------------------------------------------------*
*&      Form  F_LOCK_TABLE
*&---------------------------------------------------------------------*
FORM f_lock_table  CHANGING p_lock_flag.

  DATA l_user TYPE syuname.

  CLEAR p_lock_flag.

  CALL FUNCTION 'ENQUEUE_EZVU_TABLE_EDIT'
    EXPORTING
      mandt          = sy-mandt
      tabname        = c_fuel_table
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc = 0.
    p_lock_flag = c_x.
  ELSE.
    l_user = sy-msgv1.
    MESSAGE i349(ad) WITH c_fuel_table l_user.
  ENDIF.

ENDFORM.                    " F_LOCK_TABLE

*&---------------------------------------------------------------------*
*&      Form  F_UNLOCK_TABLE
*&---------------------------------------------------------------------*
FORM f_unlock_table .

  CALL FUNCTION 'DEQUEUE_EZVU_TABLE_EDIT'
    EXPORTING
      mandt   = sy-mandt
      tabname = c_fuel_table.

ENDFORM.                    " F_UNLOCK_TABLE

*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_TABLE
*&---------------------------------------------------------------------*
FORM f_update_table .


* begin of DV5K967547 Defect 652
  DATA:
    lo_flcm_exception type ref to zcx_flcm_error,
    lt_msg_sent       TYPE bapiret2_tab,
    lt_msg            TYPE bapiret2_tab.

  FIELD-SYMBOLS:
    <zmm_flcm_tds>    type zmm_flcm_tds,
    <msg>             type bapiret2.

* end of DV5K967547 Defect 652

  CHECK NOT i_dtl_fuel[] IS INITIAL     OR
        NOT i_tds_success[] IS INITIAL.

  IF NOT i_dtl_fuel[] IS INITIAL.
    MODIFY zmm_dtl_fuel_evt FROM TABLE i_dtl_fuel.
  ENDIF.

  IF NOT i_tds_success[] IS INITIAL.
* begin of DV5K967547 Defect 652
*   UPDATE zmm_flcm_tds FROM TABLE i_tds_success.
    LOOP AT i_tds_success assigning <zmm_flcm_tds>.
       CLEAR lo_flcm_exception.
       REFRESH lt_msg_sent.
       APPEND INITIAL LINE TO lt_msg_sent ASSIGNING <msg>.
       <msg>-type        = c_success.
       <msg>-id          = c_zz_flcm.
       <msg>-number      = '095'.
       <msg>-message_v1  = <zmm_flcm_tds>-ztds_tran_ref_nb.
       <msg>-message_v2  = c_dtl.
* Update transaction status to error with message
         CALL METHOD zcl_smint60a_zmm_flcm_tds=>set_status_and_messages
           EXPORTING
             im_tran_ref_nbr   = <zmm_flcm_tds>-ztds_tran_ref_nb
             im_commit         = abap_true
             im_tran_status    = <zmm_flcm_tds>-ztran_stus
             im_tran_substatus = c_status_99
             im_messages       = lt_msg_sent
           EXCEPTIONS
             error_in_save     = 1
             OTHERS            = 2.

         IF ( sy-subrc <> 0 ).
           IF ( 1 = 2 ). MESSAGE e039(zz_flcm). ENDIF    .
              MESSAGE ID c_zz_flcm TYPE 'E' NUMBER 039
                  INTO <msg>-message.

             ADD 1 TO v_error_cnt.
*     Insert error record to message log
             PERFORM f_insert_message_log USING <zmm_flcm_tds>
                                               c_e
                                               <msg>-message.

         ENDIF.


    ENDLOOP.

* begin of DV5K967547 Defect 652
  ENDIF.

* COMMIT WORK.                                                                                             "DV5K967547

ENDFORM.                    " F_UPDATE_TABLE

*&---------------------------------------------------------------------*
*&      Form  F_INSERT_MESSAGE_LOG
*&---------------------------------------------------------------------*
FORM f_insert_message_log  USING ps_tds    STRUCTURE zmm_flcm_tds
                                 p_msgtype
                                 p_msgtext.

  CLEAR wa_log.
  wa_log-msgtype   = p_msgtype.
  wa_log-folio_yr  = ps_tds-ztds_folio_yr.
  wa_log-folio_mth = ps_tds-ztds_folio_mth.
  wa_log-folio_nbr = ps_tds-ztds_folio_nbr.
  wa_log-folio_seq = ps_tds-ztds_folio_seq.
  concatenate p_msgtext ps_tds-ztds_tran_ref_nb into wa_log-msgtext separated by space.                    "DV5K965377
* wa_log-msgtext   = p_msgtext.                                                                            "DV5K965377
  APPEND wa_log TO i_log.

ENDFORM.                    " F_INSERT_MESSAGE_LOG

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_LOG
*&---------------------------------------------------------------------*
FORM f_display_log .

  DATA l_char10 TYPE char10.

  CHECK NOT i_log[] IS INITIAL.

  SORT i_log.

  WRITE:/ 'Total processed records :'(009),
                           v_total_cnt COLOR COL_TOTAL,
        / 'Total transfered records:'(010),
                           v_success_cnt COLOR COL_POSITIVE,
        / 'Total error records     :'(011),
                           v_error_cnt COLOR COL_NEGATIVE.

* Add to email contents
  CLEAR wa_contents.
  l_char10 = v_total_cnt.
  wa_contents-line(30) = 'Total processed records :'.
  wa_contents-line+30  = l_char10.
  APPEND wa_contents TO i_contents.

  CLEAR wa_contents.
  l_char10 = v_success_cnt.
  wa_contents-line(30) = 'Total transfered records:'.
  wa_contents-line+30  = l_char10.
  APPEND wa_contents TO i_contents.

  CLEAR wa_contents.
  l_char10 = v_error_cnt.
  wa_contents-line(30) = 'Total error records     :'.
  wa_contents-line+30  = l_char10.
  APPEND wa_contents TO i_contents.

  SKIP.
  APPEND space TO i_contents.

  WRITE:/   'FOLIO YEAR'(001) COLOR COL_HEADING,
         15 'MONTH'(002) COLOR COL_HEADING,
         23 'NUMBER'(003) COLOR COL_HEADING,
         32 'SEQUENCE'(004) COLOR COL_HEADING,
         45 'MESSAGE'(005) COLOR COL_HEADING.

* Add to email contents
  CLEAR wa_contents.
  wa_contents-line    = text-001.
  wa_contents-line+15 = text-002.
  wa_contents-line+23 = text-003.
  wa_contents-line+32 = text-004.
  wa_contents-line+45 = text-005.
  APPEND wa_contents TO i_contents.

  ULINE.
  CLEAR wa_contents.
  wa_contents-line =
        '------------------------------------------------------------'.
  APPEND wa_contents TO i_contents.

  LOOP AT i_log INTO wa_log.
    WRITE:/   wa_log-folio_yr,
           15 wa_log-folio_mth,
           23 wa_log-folio_nbr,
           32 wa_log-folio_seq,
           45 wa_log-msgtext.

*   Add to email contents
    CLEAR wa_contents.
    wa_contents-line    = wa_log-folio_yr.
    wa_contents-line+15 = wa_log-folio_mth.
    wa_contents-line+23 = wa_log-folio_nbr.
    wa_contents-line+32 = wa_log-folio_seq.
    wa_contents-line+45 = wa_log-msgtext.
    APPEND wa_contents TO i_contents.

    AT END OF msgtype.
      ULINE.
      CLEAR wa_contents.
      wa_contents-line =
        '------------------------------------------------------------'.
      APPEND wa_contents TO i_contents.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " F_DISPLAY_LOG

*&---------------------------------------------------------------------*
*&      Form  F_GET_EMAIL_RECIPIENT
*&---------------------------------------------------------------------*
FORM f_get_email_recipient USING p_parnam.

  DATA: l_recipient_type TYPE zvu_email-test_rec_type,
        l_recipient_name TYPE zvu_email-test_rec_name.

  CLEAR: l_recipient_type, l_recipient_name, wa_receivers.

  CALL FUNCTION 'ZFI_GET_EMAIL_RECIPIENT'
    EXPORTING
      application    = c_appnam
      parm_name      = p_parnam
    IMPORTING
      recipient_type = l_recipient_type
      recipient_name = l_recipient_name
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

  IF sy-subrc = 0.
    wa_receivers = l_recipient_name.
    APPEND wa_receivers TO i_receivers.
  ENDIF.

ENDFORM.                    " F_GET_EMAIL_RECIPIENT

*&---------------------------------------------------------------------*
*&      Form  F_SEND_EMAIL
*&---------------------------------------------------------------------*
FORM f_send_email .

  DATA: l_subjct TYPE sood1-objdes.

  CHECK v_error_cnt > 0.

  PERFORM f_get_email_recipient USING c_parm1.
  PERFORM f_get_email_recipient USING c_parm2.

  l_subjct = 'SMENH8A7 - TDS to DTL Transfer Message Log'(012).

* Send email
  CALL FUNCTION 'Z_SEND_EMAIL'
    EXPORTING
      subject         = l_subjct
    TABLES
      msgtext         = i_contents
      msgdest         = i_receivers
    EXCEPTIONS
      not_sent        = 1
      no_destinations = 2
      no_message      = 3
      OTHERS          = 4.

  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " F_SEND_EMAIL
