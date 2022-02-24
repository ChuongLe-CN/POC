REPORT  zme_smenh824_tds_error_monitor.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZME_SMENH824_TDS_ERROR_MONITOR                           *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI                                               *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Eva Chan                  2012/10/16          DV5K974692             *
*                                                                      *
* Short Description: SM-ENH-824                                        *
* - change the defaults for Transaction Status in selection screen     *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

TYPE-POOLS: slis,
            sscr.

*--------------------------------------------------------------------*
DATA: v_1000_zmm_flcm_tds    TYPE zmm_flcm_tds,
      v_1000_message_shown   TYPE xfeld,
      i_1000_exclude         TYPE STANDARD TABLE OF fcode,
      r_hold_trnref          TYPE RANGE OF zztds_tran_ref_nbr.

SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME.
SELECT-OPTIONS: s_trnid      FOR v_1000_zmm_flcm_tds-ztds_tran_type,
                s_trnref     FOR v_1000_zmm_flcm_tds-ztds_tran_ref_nb
                                 MODIF ID ref.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS      p_endday      AS CHECKBOX USER-COMMAND zendday.
SELECTION-SCREEN COMMENT (60) text-s01 FOR FIELD p_endday.
SELECTION-SCREEN END OF LINE.
PARAMETERS      p_skpchk     TYPE xfeld .
SELECTION-SCREEN END OF BLOCK bk1.
SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME.
SELECT-OPTIONS: s_lifnr      FOR v_1000_zmm_flcm_tds-lifnr,
                s_werks      FOR v_1000_zmm_flcm_tds-werks,
                s_bolnbr     FOR v_1000_zmm_flcm_tds-ztds_bol_nbr,
                s_status     FOR v_1000_zmm_flcm_tds-ztran_stus
                                 MODIF ID coe NO INTERVALS,
                s_trndat     FOR v_1000_zmm_flcm_tds-ztds_tran_dt,
                s_erdat      FOR v_1000_zmm_flcm_tds-erdat MODIF ID coe.
SELECTION-SCREEN END OF BLOCK bk2.
SELECTION-SCREEN BEGIN OF BLOCK bk3 WITH FRAME.
PARAMETERS:     p_alvvar     TYPE slis_vari.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:     p_text       TYPE xfeld.
SELECTION-SCREEN COMMENT (70) text-s02 FOR FIELD p_text.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bk3.
*--------------------------------------------------------------------*

DATA: o_tdserr               TYPE REF TO zcl_smenh824_error_monitor_tds,
      o_flcm_error           TYPE REF TO zcx_flcm_error,

      v_coe_support          TYPE xfeld    VALUE abap_false,
      v_allow_reprocess      TYPE xfeld    VALUE abap_false.

*--------------------------------------------------------------------*
INITIALIZATION.

  AUTHORITY-CHECK OBJECT 'ZTDS_SUPPT'
           ID 'ACTVT' FIELD '36'.
  IF sy-subrc = 0.
    v_coe_support = abap_true.
  ELSE.
    sy-title = 'TDS Error Monitor'(t00).
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZTDS_REPRC'
           ID 'ACTVT' FIELD '02'.    "02=Update, 03=Display
  IF sy-subrc = 0.
    v_allow_reprocess = abap_true.
  ENDIF.

  PERFORM f_restrict_intervals.

  PERFORM f_default_ranges.

  APPEND 'PRIN' TO i_1000_exclude.
  APPEND 'SJOB' TO i_1000_exclude.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF sy-ucomm = 'ONLI' OR
     sy-ucomm = 'PRIN' OR
     sy-ucomm = 'SJOB'.
    IF v_1000_message_shown = abap_false AND
         s_trnid[]  IS INITIAL AND
         s_trnref[] IS INITIAL AND
         s_lifnr[]  IS INITIAL AND
         s_werks[]  IS INITIAL AND
         s_bolnbr[] IS INITIAL AND
         s_trndat[] IS INITIAL AND
         s_erdat[] IS INITIAL.
      v_1000_message_shown = abap_true.
      CLEAR sy-ucomm.
      MESSAGE i689(m7).
      LEAVE TO SCREEN 1000.
    ENDIF.
  ENDIF.

  IF sy-ucomm = 'ZENDDAY'.
    IF p_endday = abap_true.
      r_hold_trnref[] = s_trnref[].
      CLEAR: s_trnref,
             s_trnref[].
    ELSE.
      s_trnref[] = r_hold_trnref[].
      CLEAR r_hold_trnref[].
    ENDIF.
  ELSE.
    IF p_endday = abap_true AND
       s_werks[] IS INITIAL.
      SET CURSOR FIELD 'S_WERKS-LOW'.
      MESSAGE e091(zzrf).      "Please enter a plant
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  IF p_endday = abap_true.
    LOOP AT SCREEN.
      IF screen-group1 = 'REF'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF v_coe_support = abap_false.
    LOOP AT SCREEN.
      IF screen-group1 = 'COE'.
        screen-input = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = '%_00'
    TABLES
      p_exclude = i_1000_exclude.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_alvvar.
  PERFORM f_f4_alv_variants.

*--------------------------------------------------------------------*
START-OF-SELECTION.

  IF v_coe_support = abap_false.
    PERFORM f_default_ranges.     "Ensure default range only
    CLEAR: s_erdat,
           s_erdat[].
  ENDIF.

  CLEAR v_1000_message_shown.

  TRY.
      CREATE OBJECT o_tdserr
        EXPORTING
          im_tran_type    = s_trnid[]
          im_tran_ref_nbr = s_trnref[]
          im_lifnr        = s_lifnr[]
          im_werks        = s_werks[]
          im_bol_nbr      = s_bolnbr[]
          im_tran_stus    = s_status[]
          im_tran_dt      = s_trndat[]
          im_erdat        = s_erdat[]
          im_end_of_day   = p_endday
          im_skip_check   = p_skpchk
          im_with_text    = p_text.

      CALL METHOD o_tdserr->display_alv
        EXPORTING
          im_callback_program   = 'ZME_SMENH824_TDS_ERROR_MONITOR'
          im_callback_form      = 'F_ALV_CALLBACK'
          im_callback_pf_status = 'F_SET_PF_STATUS'
          im_variant            = p_alvvar
          im_default_variant    = v_coe_support.
      CALL METHOD o_tdserr->free.

    CATCH zcx_flcm_error INTO o_flcm_error.
      MESSAGE ID o_flcm_error->msgid
              TYPE o_flcm_error->msgty
              NUMBER o_flcm_error->msgno
              WITH o_flcm_error->msgv1
                   o_flcm_error->msgv2
                   o_flcm_error->msgv3
                   o_flcm_error->msgv4.
  ENDTRY.

  EXIT.

*&---------------------------------------------------------------------*
*&      Form  f_restrict_intervals
*&---------------------------------------------------------------------*
FORM f_restrict_intervals.

*** Define the object to be passed to the RESTRICTION parameter
  DATA: l_restrict           TYPE sscr_restrict.


  FIELD-SYMBOLS: <opt>       TYPE sscr_opt_list,
                 <sscr>      TYPE sscr_ass.

  APPEND INITIAL LINE TO l_restrict-opt_list_tab ASSIGNING <opt>.
  <opt>-name = 'JUST_EQ'.
  <opt>-options-eq = 'X'.

  APPEND INITIAL LINE TO l_restrict-ass_tab ASSIGNING <sscr>.
  <sscr>-kind = 'S'.
  <sscr>-sg_main = 'I'.
  <sscr>-op_main = 'JUST_EQ'.
  <sscr>-name = 'S_STATUS'.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction = l_restrict.

ENDFORM.                    "f_restrict_intervals

*&---------------------------------------------------------------------*
*&      Form  f_default_ranges
*&---------------------------------------------------------------------*
FORM f_default_ranges.

  FIELD-SYMBOLS: <status>    LIKE LINE OF s_status.

  CLEAR: s_status,
         s_status[].

  APPEND INITIAL LINE TO s_status ASSIGNING <status>.
  <status>-sign = 'I'.
  <status>-option = 'EQ'.
* Begin of DV5K974692
*  <status>-low = '3'.
  <status>-low = '0'.
  APPEND <status> TO s_status ASSIGNING <status>.
  <status>-low = '1'.
* End of DV5K974692
  APPEND <status> TO s_status ASSIGNING <status>.
  <status>-low = '4'.


ENDFORM.                    "f_default_ranges

*&---------------------------------------------------------------------*
*&      Form  f_f4_alv_variants
*&---------------------------------------------------------------------*
FORM f_f4_alv_variants.

  DATA: l_is_variant         TYPE disvariant,
        l_es_variant         TYPE disvariant,

        l_dynpfields         TYPE dynpread,
        li_dynpfields        TYPE STANDARD TABLE OF dynpread.

  l_is_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      i_save     = 'A'
      is_variant = l_is_variant
    IMPORTING
      es_variant = l_es_variant.

  l_dynpfields-fieldname = 'P_ALVVAR'.
  l_dynpfields-fieldvalue = l_es_variant-variant.
  APPEND l_dynpfields TO li_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = '1000'
    TABLES
      dynpfields = li_dynpfields.

ENDFORM.                    "f_f4_alv_variants

*&---------------------------------------------------------------------*
*&      Form  f_set_pf_status
*&---------------------------------------------------------------------*
FORM f_set_pf_status USING p_extab TYPE slis_t_extab.       "#EC CALLED

  IF v_allow_reprocess = abap_false.
    PERFORM f_update_extab USING: 'ZREPROCESS' p_extab,
                                  'ZSELEOD'    p_extab,
                                  'ZDESELEOD'  p_extab.
  ELSE.                        "Reprocessing is allowed
    IF p_endday = abap_false.  "End of Day not selected
      PERFORM f_update_extab USING: 'ZSELEOD'   p_extab,
                                    'ZDESELEOD' p_extab.
    ENDIF.
  ENDIF.

  IF o_tdserr IS BOUND AND o_tdserr->is_dock_visible( ) = abap_true.
    DELETE p_extab WHERE fcode = 'ZDETAILOFF'.
  ELSE.
    PERFORM f_update_extab USING 'ZDETAILOFF' p_extab.
  ENDIF.

  SET PF-STATUS 'ALV_CUSTOM' EXCLUDING p_extab.

ENDFORM.                    "f_set_pf_status

*&---------------------------------------------------------------------*
*&      Form  f_update_extab
*&---------------------------------------------------------------------*
FORM f_update_extab USING p_fcode
                          p_extab TYPE slis_t_extab.

  FIELD-SYMBOLS: <extab>     LIKE LINE OF p_extab.

  READ TABLE p_extab ASSIGNING <extab>
      WITH KEY fcode = p_fcode.
  IF sy-subrc > 0.
    APPEND INITIAL LINE TO p_extab ASSIGNING <extab>.
    <extab>-fcode = p_fcode.
  ENDIF.

ENDFORM.                    "f_update_extab

*&---------------------------------------------------------------------*
*&      Form  f_alv_callback
*&---------------------------------------------------------------------*
FORM f_alv_callback USING p_ucomm     TYPE sy-ucomm         "#EC CALLED
                          p_selfield  TYPE slis_selfield.

  IF  o_tdserr IS NOT BOUND.
    RETURN.
  ENDIF.

  CASE p_ucomm.
    WHEN 'ZDETAIL'.
      CASE o_tdserr->check_lines_selected( ).
        WHEN 'S' OR 'N'.   "Single or None
          CALL METHOD o_tdserr->alv_details
            EXPORTING
              im_ucomm    = p_ucomm
            CHANGING
              ch_selfield = p_selfield.
*        WHEN 'N'.
*          MESSAGE i753(jo).
        WHEN 'M'.
          MESSAGE i758(jo).
      ENDCASE.
      CLEAR p_ucomm.
    WHEN 'ZDETAILOFF'.
      CALL METHOD o_tdserr->hide_dock.
      CLEAR p_ucomm.
    WHEN 'ZREPROCESS'.
      CASE o_tdserr->check_lines_selected( ).
        WHEN 'S' OR 'M'.    "Single or Multiple
          CALL METHOD o_tdserr->hide_dock.
          TRY.
              CALL METHOD o_tdserr->reprocess
                EXPORTING
                  im_ucomm    = p_ucomm
                CHANGING
                  ch_selfield = p_selfield.
            CATCH zcx_flcm_error INTO o_flcm_error.
              MESSAGE ID o_flcm_error->msgid
                      TYPE o_flcm_error->msgty
                      NUMBER o_flcm_error->msgno
                      WITH o_flcm_error->msgv1
                           o_flcm_error->msgv2
                           o_flcm_error->msgv3
                           o_flcm_error->msgv4.
          ENDTRY.
        WHEN 'N'.
          MESSAGE i753(jo).
      ENDCASE.
      CLEAR p_ucomm.
    WHEN 'ZSELEOD'.
      CALL METHOD o_tdserr->select_deselect_lines
        EXPORTING
          im_tran_type = zcl_smenh824_error_monitor_tds=>c_end_of_day
          im_sel       = abap_true.
      p_selfield-refresh = p_selfield-row_stable = abap_true.
      CLEAR p_ucomm.
    WHEN 'ZDESELEOD'.
      CALL METHOD o_tdserr->select_deselect_lines
        EXPORTING
          im_tran_type = zcl_smenh824_error_monitor_tds=>c_end_of_day
          im_sel       = abap_false.
      p_selfield-refresh = p_selfield-row_stable = abap_true.
      CLEAR p_ucomm.
  ENDCASE.

ENDFORM.                    "f_alv_callback
