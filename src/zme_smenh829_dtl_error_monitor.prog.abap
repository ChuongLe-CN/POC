REPORT  zme_smenh829_dtl_error_monitor.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZME_SMENH829_DTL_ERROR_MONITOR                           *
* Title    :  DTL Error Monitor                                        *
* Work Unit:  SM-ENH-829-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_DTLMONI                                               *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_DTL_FUEL_EVT and ZMM_DTL_XTRA_CHG tables             *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Joemer Trinidad           2020/06/11          DV5K9A0R5S             *
*                                                                      *
* Short Description: C403761-T456560                                   *
*                    Update the refuel volume                          *
*----------------------------------------------------------------------*
* Rob West                  2011/09/14          DV5K966492             *
*                                                                      *
* Short Description: FLCM Stabilization                                *
*                    Increase size of Extra Charge pop-up              *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961547             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

TYPE-POOLS: slis,
            sscr.

*--------------------------------------------------------------------*
DATA: v_1000_zdtl_fuel_evt   TYPE zmm_dtl_fuel_evt,
      v_1000_zdtl_xtra_chg   TYPE zmm_dtl_xtra_chg,
      v_1000_message_shown   TYPE xfeld,
      i_1000_exclude         TYPE STANDARD TABLE OF fcode.

SELECTION-SCREEN BEGIN OF SCREEN 100 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE text-bk1.
SELECT-OPTIONS: s_dtlevt     FOR v_1000_zdtl_fuel_evt-zdtl_fevt_id,
                s_dtlver     FOR v_1000_zdtl_fuel_evt-zdtl_fevt_ver.
SELECTION-SCREEN END OF BLOCK bk1.
SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME TITLE text-bk2.
SELECT-OPTIONS: s_dtllif     FOR v_1000_zdtl_fuel_evt-lifnr,
                s_dtlwrk     FOR v_1000_zdtl_fuel_evt-werks,
                s_dtlbol     FOR v_1000_zdtl_fuel_evt-zbol_tckt_nbr,
                s_dtlsta     FOR v_1000_zdtl_fuel_evt-ztran_stus
                                 MODIF ID sta NO INTERVALS,
                s_dtldat     FOR v_1000_zdtl_fuel_evt-zdtl_load_dt,
                s_dtlwar     FOR v_1000_zdtl_fuel_evt-zwarn_type.   "I - TXT25668 - C403761 T456560  - DV5K9A0R5S
SELECTION-SCREEN END OF BLOCK bk2.
SELECTION-SCREEN END OF SCREEN 100.

SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK bk3 WITH FRAME TITLE text-bk3.
SELECT-OPTIONS: s_xtrevt     FOR v_1000_zdtl_xtra_chg-zdtl_xchg_id,
                s_xtrver     FOR v_1000_zdtl_xtra_chg-zdtl_xchg_ver.
SELECTION-SCREEN END OF BLOCK bk3.
SELECTION-SCREEN BEGIN OF BLOCK bk4 WITH FRAME TITLE text-bk2.
SELECT-OPTIONS: s_xtrlif     FOR v_1000_zdtl_xtra_chg-lifnr,
                s_xtrwrk     FOR v_1000_zdtl_xtra_chg-werks,
                s_xtrbol     FOR v_1000_zdtl_xtra_chg-zbol_tckt_nbr,
                s_xtrsta     FOR v_1000_zdtl_xtra_chg-ztran_stus
                                 MODIF ID sta NO INTERVALS,
                s_xtrdat     FOR v_1000_zdtl_xtra_chg-zdtl_load_dt.
SELECTION-SCREEN END OF BLOCK bk4.
SELECTION-SCREEN END OF SCREEN 200.

*SELECTION-SCREEN: BEGIN OF TABBED BLOCK bk0 FOR 11 LINES,          "D - TXT25668 - C403761 T456560  - DV5K9A0R5S
SELECTION-SCREEN: BEGIN OF TABBED BLOCK bk0 FOR 12 LINES,           "I - TXT25668 - C403761 T456560  - DV5K9A0R5S
                  TAB (22) bk0_btn1 USER-COMMAND bk0_btn1,
                  TAB (22) bk0_btn2 USER-COMMAND bk0_btn2,
                  END OF BLOCK bk0.

SELECTION-SCREEN BEGIN OF BLOCK bk5 WITH FRAME TITLE text-bk5.
PARAMETERS:     p_alvdtl     TYPE slis_vari MODIF ID dtl,
                p_alvxtr     TYPE slis_vari.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:     p_text       TYPE xfeld.
SELECTION-SCREEN COMMENT (70) text-s02 FOR FIELD p_text.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bk5.
*--------------------------------------------------------------------*

DATA: o_dtlerr               TYPE REF TO zcl_smenh829_error_monitor_dtl,
      o_xtrachg              TYPE REF TO zcl_smenh829_error_monitor_xtr,
      o_flcm_error           TYPE REF TO zcx_flcm_error,

      v_coe_support          TYPE xfeld    VALUE abap_false,
      v_allow_reprocess      TYPE xfeld    VALUE abap_false.

*--------------------------------------------------------------------*
INITIALIZATION.

  AUTHORITY-CHECK OBJECT 'ZDTL_SUPPT'
           ID 'ACTVT' FIELD '36'.
  IF sy-subrc = 0.
    v_coe_support = abap_true.
  ELSE.
    sy-title =  'DTL Error Monitor'(t00).
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZDTL_REPRC'
           ID 'ACTVT' FIELD '02'.    "02=Update, 03=Display
  IF sy-subrc = 0.
    v_allow_reprocess = abap_true.
  ENDIF.

  bk0_btn1 = 'Pure DTL Records'(bk1).
  bk0_btn2 = 'Extra Charge Records'(bk3).
  bk0-prog = sy-repid.

  IMPORT bk0-dynnr     TO bk0-dynnr
         bk0-activetab TO bk0-activetab
      FROM MEMORY ID 'SMENH829_DYNNR'.
  IF sy-subrc = 0.
    DELETE FROM MEMORY ID 'SMENH829_DYNNR'.
  ELSE.
    bk0-dynnr = '0100'.
    bk0-activetab = 'BK1_BTN1'.
  ENDIF.

  PERFORM f_restrict_intervals.

  PERFORM f_default_ranges.

  APPEND 'PRIN' TO i_1000_exclude.
  APPEND 'SJOB' TO i_1000_exclude.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF sy-dynnr = '1000'.
    CASE sy-ucomm.
      WHEN 'BK0_BTN1'.
        bk0-dynnr = '0100'.
      WHEN 'BK0_BTN2'.
        bk0-dynnr = '0200'.
      WHEN 'ONLI' OR 'PRIN' OR 'SJOB'.
        IF v_1000_message_shown = abap_false AND
       ( ( bk0-dynnr = '100' AND
             s_dtlevt[] IS INITIAL AND
             s_dtlver[] IS INITIAL AND
             s_dtllif[] IS INITIAL AND
             s_dtlwrk[] IS INITIAL AND
             s_dtlbol[] IS INITIAL AND
*             s_dtldat[] IS INITIAL ) OR      "D - TXT25668 - C403761 T456560 - DV5K9A0R5S
*** Start of Insert TXT25668 - C403761 T456560 - DV5K9A0R5S
             s_dtldat[] IS INITIAL AND
             s_dtlwar[] IS INITIAL ) OR
*** End of Insert TXT25668 - C403761 T456560 - DV5K9A0R5S
           ( bk0-dynnr = '200' AND
             s_xtrevt[] IS INITIAL AND
             s_xtrver[] IS INITIAL AND
             s_xtrlif[] IS INITIAL AND
             s_xtrwrk[] IS INITIAL AND
             s_xtrbol[] IS INITIAL AND
             s_xtrdat[] IS INITIAL ) ).
          v_1000_message_shown = abap_true.
          CLEAR sy-ucomm.
          MESSAGE i689(m7).
          LEAVE TO SCREEN 1000.
        ENDIF.
    ENDCASE.
  ENDIF.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  IF v_coe_support = abap_false AND
   ( sy-dynnr = '0100' OR sy-dynnr = '0200' ).
    LOOP AT SCREEN.
      IF screen-group1 = 'STA'.
        screen-input = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF sy-dynnr = '1000' AND bk0-dynnr = '0200'.
    LOOP AT SCREEN.
      IF screen-group1 = 'DTL'.
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
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_alvdtl.
  PERFORM f_f4_alv_variants USING 'P_ALVDTL'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_alvxtr.
  PERFORM f_f4_alv_variants USING 'P_ALVXTR'.

*--------------------------------------------------------------------*
START-OF-SELECTION.

  EXPORT bk0-dynnr     FROM bk0-dynnr
         bk0-activetab FROM bk0-activetab
      TO MEMORY ID 'SMENH829_DYNNR'.

  IF v_coe_support = abap_false..
    PERFORM f_default_ranges.     "Ensure default range only
  ENDIF.

  CLEAR v_1000_message_shown.
  CASE bk0-dynnr.
    WHEN '0100'.         "DTL
      PERFORM f_process_dtl_records.
    WHEN '0200'.         "Extra Charges
      PERFORM f_process_xtra_records.
  ENDCASE.

  EXIT.

*&---------------------------------------------------------------------*
*&      Form  f_process_dtl_records
*&---------------------------------------------------------------------*
FORM f_process_dtl_records.

  TRY.
      CREATE OBJECT o_dtlerr
        EXPORTING
          im_fevt_id      = s_dtlevt[]
          im_fevt_ver     = s_dtlver[]
          im_lifnr        = s_dtllif[]
          im_werks        = s_dtlwrk[]
          im_bol_tckt_nbr = s_dtlbol[]
          im_tran_stus    = s_dtlsta[]
          im_load_dt      = s_dtldat[]
          im_warn_typ     = s_dtlwar[]      "I - TXT25668 - C403761 T456560 - DV5K9A0R5S
          im_with_text    = p_text.

      CALL METHOD o_dtlerr->display_alv
        EXPORTING
          im_callback_program   = 'ZME_SMENH829_DTL_ERROR_MONITOR'
          im_callback_form      = 'F_ALV_CALLBACK'
          im_callback_pf_status = 'F_SET_PF_STATUS'
          im_sbal_callback      = 'F_BAL_CALLBACK_READ'
          im_variant            = p_alvdtl
          im_default_variant    = v_coe_support.
      CALL METHOD o_dtlerr->free.

    CATCH zcx_flcm_error INTO o_flcm_error.
      MESSAGE ID o_flcm_error->msgid
              TYPE o_flcm_error->msgty
              NUMBER o_flcm_error->msgno
              WITH o_flcm_error->msgv1
                   o_flcm_error->msgv2
                   o_flcm_error->msgv3
                   o_flcm_error->msgv4.
  ENDTRY.

ENDFORM.                    "f_process_dtl_records

*&---------------------------------------------------------------------*
*&      Form  f_process_xtra_records
*&---------------------------------------------------------------------*
FORM f_process_xtra_records.

  TRY.
      CREATE OBJECT o_xtrachg
        EXPORTING
          im_fevt_id      = s_xtrevt[]
          im_fevt_ver     = s_xtrver[]
          im_lifnr        = s_xtrlif[]
          im_werks        = s_xtrwrk[]
          im_bol_tckt_nbr = s_xtrbol[]
          im_tran_stus    = s_xtrsta[]
          im_load_dt      = s_xtrdat[]
          im_with_text    = p_text.

      CALL METHOD o_xtrachg->display_alv
        EXPORTING
          im_callback_program   = 'ZME_SMENH829_DTL_ERROR_MONITOR'
          im_callback_form      = 'F_ALV_CALLBACK_XTRA'
          im_callback_pf_status = 'F_SET_PF_STATUS_XTRA'
          im_sbal_callback      = 'F_BAL_CALLBACK_READ'
          im_variant            = p_alvxtr
          im_default_variant    = v_coe_support.
      CALL METHOD o_xtrachg->free.

    CATCH zcx_flcm_error INTO o_flcm_error.
      MESSAGE ID o_flcm_error->msgid
              TYPE o_flcm_error->msgty
              NUMBER o_flcm_error->msgno
              WITH o_flcm_error->msgv1
                   o_flcm_error->msgv2
                   o_flcm_error->msgv3
                   o_flcm_error->msgv4.
  ENDTRY.

ENDFORM.                    "f_process_xtra_records

*&---------------------------------------------------------------------*
*&      Form  f_default_ranges
*&---------------------------------------------------------------------*
FORM f_default_ranges.

  FIELD-SYMBOLS: <dtlsta>    LIKE LINE OF s_dtlsta,
                 <xtrsta>    LIKE LINE OF s_xtrsta.

  CLEAR: s_dtlsta,
         s_dtlsta[],
         s_xtrsta,
         s_xtrsta[].

  APPEND INITIAL LINE TO s_dtlsta ASSIGNING <dtlsta>.
  APPEND INITIAL LINE TO s_xtrsta ASSIGNING <xtrsta>.
  <dtlsta>-sign   = <xtrsta>-sign   = 'I'.
  <dtlsta>-option = <xtrsta>-option = 'EQ'.
  <dtlsta>-low    = <xtrsta>-low    = '3'.

  APPEND <dtlsta> TO s_dtlsta ASSIGNING <dtlsta>.
  APPEND <xtrsta> TO s_xtrsta ASSIGNING <xtrsta>.
  <dtlsta>-low    = <xtrsta>-low    = '4'.

ENDFORM.                    "f_default_ranges

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
  <sscr>-name = 'S_DTLSTA'.

  APPEND <sscr> TO l_restrict-ass_tab ASSIGNING <sscr>.
  <sscr>-name = 'S_XTRSTA'.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction = l_restrict.

ENDFORM.                    "f_restrict_intervals

*&---------------------------------------------------------------------*
*&      Form  f_bal_callback_read
*&---------------------------------------------------------------------*
FORM f_bal_callback_read                                    "#EC CALLED
       USING
         p_info              TYPE bal_s_cbrd
       CHANGING
         p_display_data      TYPE bal_s_show
         p_context_header    TYPE bal_s_cont
         p_context_message   TYPE bal_s_cont
         p_field             TYPE any.

  FIELD-SYMBOLS: <key>       TYPE zsm_smint666_appl_log_key.

  IF p_info-ref_table = 'ZSM_SMINT666_APPL_LOG_KEY' AND
     p_info-is_message = abap_true.
    ASSIGN p_display_data-extnumber TO <key> CASTING.
    CASE p_info-ref_field.
      WHEN 'ZDTL_FEVT_ID'.  p_field = <key>-zdtl_fevt_id.
      WHEN 'ZDTL_FEVT_VER'. p_field = <key>-zdtl_fevt_ver.
    ENDCASE.
  ENDIF.

ENDFORM.                    "f_bal_callback_read

*&---------------------------------------------------------------------*
*&      Form  f_f4_alv_variants
*&---------------------------------------------------------------------*
FORM f_f4_alv_variants USING p_fieldname TYPE c.

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

  l_dynpfields-fieldname = p_fieldname.
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
    READ TABLE p_extab TRANSPORTING NO FIELDS
        WITH KEY fcode = 'ZREPROCESS'.
    IF sy-subrc > 0.
      APPEND 'ZREPROCESS' TO p_extab.
    ENDIF.
  ENDIF.

  IF o_dtlerr IS BOUND AND o_dtlerr->is_dock_visible( ) = abap_true.
    DELETE p_extab WHERE table_line = 'ZDETAILOFF'.
  ELSE.
    APPEND 'ZDETAILOFF' TO p_extab.
  ENDIF.

  IF o_dtlerr IS BOUND AND o_dtlerr->displayed_as_popup( ) = abap_true.
    DELETE p_extab WHERE table_line = 'ZCLOSE'.
  ELSE.
    APPEND 'ZCLOSE' TO p_extab.
  ENDIF.

  SET PF-STATUS 'ALV_CUSTOM' EXCLUDING p_extab.

ENDFORM.                    "f_set_pf_status


*&---------------------------------------------------------------------*
*&      Form  f_alv_callback
*&---------------------------------------------------------------------*
FORM f_alv_callback USING p_ucomm     TYPE sy-ucomm         "#EC CALLED
                          p_selfield  TYPE slis_selfield.

  IF o_dtlerr IS NOT BOUND.
    RETURN.
  ENDIF.

  CASE p_ucomm.
    WHEN 'ZDETAIL'.
      CASE o_dtlerr->check_lines_selected( ).
        WHEN 'S' OR 'N'.    "Single or None
          IF p_selfield-value = icon_cost_components.
            PERFORM f_show_extra_costs CHANGING p_selfield.
          ELSE.
            CALL METHOD o_dtlerr->alv_details
              EXPORTING
                im_ucomm    = p_ucomm
              CHANGING
                ch_selfield = p_selfield.
          ENDIF.
*        WHEN 'N'.
*          MESSAGE i753(jo).
        WHEN 'M'.
          MESSAGE i758(jo).
      ENDCASE.
      CLEAR p_ucomm.
    WHEN 'ZDETAILOFF'.
      CALL METHOD o_dtlerr->hide_dock.
      CLEAR p_ucomm.
    WHEN 'ZEXTRA'.
      CASE o_dtlerr->check_lines_selected( ).
        WHEN 'S' OR 'M'.    "Single or Multiple
          PERFORM f_show_extra_costs CHANGING p_selfield.
        WHEN 'N'.
          MESSAGE i753(jo).
*        WHEN 'M'.
*          MESSAGE i758(jo).
      ENDCASE.
      CLEAR p_ucomm.
    WHEN 'ZREPROCESS'.
      CALL METHOD o_dtlerr->hide_dock.
      CASE o_dtlerr->check_lines_selected( ).
        WHEN 'S' OR 'M'.    "Single or many
          TRY.
              CALL METHOD o_dtlerr->reprocess
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
    WHEN 'ZCLOSE'.
      p_selfield-exit = abap_true.
      CLEAR p_ucomm.
  ENDCASE.

ENDFORM.                    "f_alv_callback

*&---------------------------------------------------------------------*
*&      Form  f_show_extra_costs
*&---------------------------------------------------------------------*
FORM f_show_extra_costs CHANGING p_selfield  TYPE slis_selfield.

  DATA: li_bolnbr            TYPE RANGE OF zzbol_ticket_nbr,
        li_bol_selected      TYPE STANDARD TABLE OF zzbol_ticket_nbr,
        l_bolnbr             LIKE LINE OF li_bolnbr,
        l_reprocessed        TYPE xfeld,
        l_end_col            TYPE i,
        l_end_row            TYPE i,
        li_tabix             TYPE re_t_tabix,

        l_bol_message        TYPE c LENGTH 50,

        li_event             TYPE RANGE OF zzdtl_extra_chrg_id,
        li_vers              TYPE RANGE OF zzdtl_extra_chrg_ver,
        li_status            TYPE RANGE OF zztran_stus,
        li_lifnr             TYPE RANGE OF lifnr,
        li_werks             TYPE RANGE OF werks_d,
        li_trndat            TYPE RANGE OF zzdtl_load_dt,

        lo_flcm_error        TYPE REF TO zcx_flcm_error.

  IF p_selfield-value = icon_cost_components.
    CALL METHOD o_dtlerr->get_bol_selected
      EXPORTING
        im_tabix     = p_selfield-tabindex
      CHANGING
        ch_bol_table = li_bol_selected.
    APPEND p_selfield-tabindex TO li_tabix.
  ELSE.
    CALL METHOD o_dtlerr->get_bol_selected
      CHANGING
        ch_bol_table = li_bol_selected
        ch_sel_tabix = li_tabix.
  ENDIF.

  IF li_bol_selected[] IS INITIAL.
    MESSAGE 'No lines selected for Extra Charge details'(t01) TYPE 'I'.
    RETURN.
  ENDIF.

  l_bolnbr-sign = 'I'.
  l_bolnbr-option = 'EQ'.
  LOOP AT li_bol_selected INTO l_bolnbr-low.
    APPEND l_bolnbr TO li_bolnbr.
  ENDLOOP.

  TRY.
      CREATE OBJECT o_xtrachg
        EXPORTING
          im_fevt_id       = li_event[]
          im_fevt_ver      = li_vers[]
          im_lifnr         = li_lifnr[]
*          im_lifnr        = s_dltlif[]
          im_werks         = li_werks[]
*          im_werks        = s_dtlwrk[]
          im_bol_tckt_nbr  = li_bolnbr[]
          im_tran_stus     = li_status[]
          im_load_dt       = li_trndat[]
          im_select_by_bol = abap_true
          im_with_text     = p_text.
*          im_load_dt      = s_dtldat[].

*      l_end_col = sy-scols - 20.
*      l_end_row = sy-srows - 15.
      l_end_col = sy-scols - 10.                             "DV5K966492
      l_end_row = sy-srows - 6.                              "DV5K966492
      CONCATENATE 'Extra charges for BOL'(t02) l_bolnbr-low
          INTO l_bol_message SEPARATED BY space.
      CALL METHOD o_xtrachg->display_alv
        EXPORTING
          im_callback_program   = 'ZME_SMENH829_DTL_ERROR_MONITOR'
          im_callback_form      = 'F_ALV_CALLBACK_XTRA'
          im_callback_pf_status = 'F_SET_PF_STATUS_XTRA'
          im_sbal_callback      = 'F_BAL_CALLBACK_READ'
          im_variant            = p_alvxtr
          im_grid_title         = l_bol_message
*          im_row_start          = 5
          im_row_start          = 1                          "DV5K966492
          im_column_start       = 5
          im_row_end            = l_end_row
          im_column_end         = l_end_col.
      IF o_xtrachg->was_reprocessed( ) = abap_true.
        l_reprocessed = abap_true.
      ENDIF.
      CALL METHOD o_xtrachg->free.
      CLEAR o_xtrachg.

    CATCH zcx_flcm_error INTO lo_flcm_error.
      MESSAGE ID lo_flcm_error->msgid
              TYPE 'I'
              NUMBER lo_flcm_error->msgno
              WITH lo_flcm_error->msgv1
                   lo_flcm_error->msgv2
                   lo_flcm_error->msgv3
                   lo_flcm_error->msgv4.
  ENDTRY.

  IF l_reprocessed = abap_true.
    CALL METHOD o_dtlerr->refresh_data
      EXPORTING
        im_sel_tabix = li_tabix[].
    p_selfield-refresh = p_selfield-row_stable = abap_true.
  ENDIF.

ENDFORM.                    "f_show_extra_costs

*&---------------------------------------------------------------------*
*&      Form  f_alv_callback_xtra
*&---------------------------------------------------------------------*
FORM f_alv_callback_xtra USING p_ucomm     TYPE sy-ucomm    "#EC CALLED
                               p_selfield  TYPE slis_selfield.

  IF o_xtrachg IS NOT BOUND.
    RETURN.
  ENDIF.

  CASE p_ucomm.
    WHEN 'ZDETAIL'.
      CASE o_xtrachg->check_lines_selected( ).
        WHEN 'S' OR 'N'.   "Single or None
          CALL METHOD o_xtrachg->alv_details
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
      CALL METHOD o_xtrachg->hide_dock.
      CLEAR p_ucomm.
    WHEN 'ZREPROCESS'.
      CALL METHOD o_xtrachg->hide_dock.
      CASE o_xtrachg->check_lines_selected( ).
        WHEN 'S' OR 'M'.    "Single or Multiple
          TRY.
              CALL METHOD o_xtrachg->reprocess
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
    WHEN 'ZCLOSE'.
      p_selfield-exit = abap_true.
      CLEAR p_ucomm.
  ENDCASE.

ENDFORM.                    "f_alv_callback_xtra

*&---------------------------------------------------------------------*
*&      Form  f_set_pf_status_xtra
*&---------------------------------------------------------------------*
FORM f_set_pf_status_xtra USING p_extab TYPE slis_t_extab.  "#EC CALLED

  IF v_allow_reprocess = abap_false.
    READ TABLE p_extab TRANSPORTING NO FIELDS
        WITH KEY fcode = 'ZREPROCESS'.
    IF sy-subrc > 0.
      APPEND 'ZREPROCESS' TO p_extab.
    ENDIF.
  ENDIF.

  IF o_xtrachg IS BOUND AND o_xtrachg->is_dock_visible( ) = abap_true.
    DELETE p_extab WHERE table_line = 'ZDETAILOFF'.
  ELSE.
    APPEND 'ZDETAILOFF' TO p_extab.
  ENDIF.

  IF o_xtrachg IS BOUND AND o_xtrachg->displayed_as_popup( ) = abap_true.
    DELETE p_extab WHERE table_line = 'ZCLOSE'.
  ELSE.
    APPEND 'ZCLOSE' TO p_extab.
  ENDIF.

  APPEND 'ZEXTRA' TO p_extab.

  SET PF-STATUS 'ALV_CUSTOM' EXCLUDING p_extab.

ENDFORM.                    "f_set_pf_status_xtra
