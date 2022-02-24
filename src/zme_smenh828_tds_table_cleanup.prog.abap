REPORT  zme_smenh828_tds_table_cleanup.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZME_SMENH828_TDS_TABLE_CLEANUP                           *
* Title    :  TDS table cleanup                                        *
* Work Unit:  SM-ENH-828-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
*                                                                      *
* Purpose:    Remove processed records from ZMM_FLCM_TDS.              *
*             Also remove associated Applicatiom Log entries           *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K962277             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

TYPE-POOLS: slis,
            sscr,
            abap.

TYPES: BEGIN OF t_zmm_flcm_tds.
INCLUDE  TYPE zmm_flcm_tds.
TYPES:   alv_sel               TYPE xfeld,
       END OF t_zmm_flcm_tds,
       t_zmm_flcm_tds_table    TYPE STANDARD TABLE OF t_zmm_flcm_tds.

*--------------------------------------------------------------------*
DATA: v_zmm_flcm_tds_1000     TYPE zmm_flcm_tds.

SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE text-bk1.
SELECT-OPTIONS s_status       FOR v_zmm_flcm_tds_1000-ztran_stus NO INTERVALS.
PARAMETERS: p_period          TYPE numc3 DEFAULT '180',
            p_alvvar          TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK bk1.
SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME TITLE text-bk2.
SELECT-OPTIONS: s_trntyp      FOR v_zmm_flcm_tds_1000-ztds_tran_type MODIF ID tst,
                s_trnref      FOR v_zmm_flcm_tds_1000-ztds_tran_ref_nb MODIF ID tst,
                s_bolnbr      FOR v_zmm_flcm_tds_1000-ztds_bol_nbr MODIF ID tst.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: s_aename      FOR v_zmm_flcm_tds_1000-aename MODIF ID tst,
                s_aedat       FOR v_zmm_flcm_tds_1000-aedat MODIF ID tst.
SELECTION-SCREEN END OF BLOCK bk2.

DATA: i_table                 TYPE t_zmm_flcm_tds_table.

*--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM f_restrict_intervals.

  PERFORM f_default_ranges.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  IF sy-sysid = 'PRD'.
    LOOP AT SCREEN.
      IF screen-group1 = 'TST'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_alvvar.
  PERFORM f_f4_alv_variants.

*--------------------------------------------------------------------*
START-OF-SELECTION.

  IF sy-sysid = 'PRD'.        "Clear any non-PRD selection criteria
    REFRESH: s_trntyp,
             s_trnref,
             s_bolnbr,
             s_aename,
             s_aedat.
    CLEAR:   s_trntyp,
             s_trnref,
             s_bolnbr,
             s_aename,
             s_aedat.
  ENDIF.

  PERFORM f_select_data.

  PERFORM f_alv_output.

  IF sy-batch = abap_true.
    PERFORM f_delete_records.
    SUBMIT sbal_delete WITH p_bef_no = 'X'  "Clean-up any Appl. Log orphans
                       WITH p_bef_ok = space
                       WITH s_obj = 'ZSM'
                       WITH s_sub = 'SM-INT-60A'
                       WITH p_sim = space
                       WITH p_list = space
                       WITH p_direct = 'X'
                       AND RETURN.
  ENDIF.

  EXIT.

*&---------------------------------------------------------------------*
*&      Form  f_select_data
*&---------------------------------------------------------------------*
FORM f_select_data .

  DATA: l_date               TYPE dats,
        l_line               TYPE t_zmm_flcm_tds.

  IF p_period IS INITIAL.
    l_date = '20000101'.
  ELSE.
    l_date = sy-datum - p_period.
  ENDIF.

  SELECT *
    FROM zmm_flcm_tds
    INTO TABLE i_table
    WHERE ztds_tran_type   IN s_trntyp AND
          ztds_tran_ref_nb IN s_trnref AND
          ztds_bol_nbr     IN s_bolnbr AND
          ztds_tran_dt     <  l_date AND
          ztran_stus       IN s_status AND
          aename           IN s_aename AND
          aedat            IN s_aedat.

  SORT i_table BY ztds_tran_type   ASCENDING
                  ztds_tran_ref_nb ASCENDING.

  IF sy-batch = abap_true.     "Set all lines selected in batch
    l_line-alv_sel = abap_true.
    MODIFY i_table FROM l_line TRANSPORTING alv_sel
        WHERE alv_sel = abap_false.
  ENDIF.

ENDFORM.                    " f_read_table

*&---------------------------------------------------------------------*
*&      Form  f_alv_output
*&---------------------------------------------------------------------*
FORM f_alv_output .

  DATA: l_layout                  TYPE slis_layout_alv,
        l_repid                   TYPE syrepid VALUE sy-repid,
        l_variant                 TYPE disvariant.

  l_layout-colwidth_optimize = 'X'.
  l_layout-zebra = 'X'.
  l_layout-box_fieldname = 'ALV_SEL'.
  l_repid = sy-repid.

  l_variant-report = l_repid.
  l_variant-variant = p_alvvar.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = l_repid
      i_callback_pf_status_set = 'F_SET_PF_STATUS'
      i_callback_user_command  = 'F_ALV_CALLBACK'
      i_callback_top_of_page   = 'F_ALV_TOP_OF_PAGE'
      i_default                = ' '
      i_save                   = 'A'
      is_variant               = l_variant
      is_layout                = l_layout
      i_structure_name         = 'ZMM_FLCM_TDS'
    TABLES
      t_outtab                 = i_table
    EXCEPTIONS
      program_error            = 0
      OTHERS                   = 0.

ENDFORM.                    " f_alv_output

*&---------------------------------------------------------------------*
*&      Form  f_delete_records
*&---------------------------------------------------------------------*
FORM f_delete_records.

  DATA: l_tabix                   TYPE i,
        l_count                   TYPE i,
        l_count_x                 TYPE c LENGTH 15,
        li_flcm_tds               TYPE STANDARD TABLE OF zmm_flcm_tds,
        li_refnum                 TYPE zztds_tran_ref_nbr_table.

  FIELD-SYMBOLS: <line>           TYPE t_zmm_flcm_tds.

  LOOP AT i_table ASSIGNING <line> WHERE alv_sel = abap_true.
    l_tabix = sy-tabix.
    APPEND <line> TO li_flcm_tds.
    APPEND <line>-ztds_tran_ref_nb TO li_refnum.
    DELETE i_table INDEX l_tabix.
    ADD 1 TO l_count.
  ENDLOOP.

  IF li_flcm_tds[] IS NOT INITIAL.
    DELETE zmm_flcm_tds FROM TABLE li_flcm_tds.
    COMMIT WORK.
  ENDIF.

  WRITE l_count TO l_count_x LEFT-JUSTIFIED.
  MESSAGE i169(0d) WITH l_count_x 'ZMM_FLCM_TDS'.

  IF li_refnum[] IS NOT INITIAL.
    CALL METHOD zcl_smint60a_zmm_flcm_tds=>delete_messages
      EXPORTING
        im_tran_ref_nbr_table = li_refnum[]
        im_commit             = abap_true
      EXCEPTIONS
        no_logs_specified     = 1
        OTHERS                = 2.
  ENDIF.

ENDFORM.                    "f_delete_records

*&---------------------------------------------------------------------*
*&      Form  f_alv_top_of_page
*&---------------------------------------------------------------------*
FORM f_alv_top_of_page.                                     "#EC CALLED

  DATA: l_count              TYPE i,
        li_header            TYPE slis_t_listheader.

  FIELD-SYMBOLS: <hdr>       TYPE slis_listheader.

  APPEND INITIAL LINE TO li_header ASSIGNING <hdr>.

  <hdr>-typ = 'H'.
  l_count = LINES( i_table ).
  WRITE l_count TO <hdr>-info LEFT-JUSTIFIED.
  CONCATENATE <hdr>-info 'records selected'
      INTO <hdr>-info SEPARATED BY space.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = li_header[].

ENDFORM.                    "f_alv_top_of_page

*&---------------------------------------------------------------------*
*&      Form  f_set_pf_status_
*&---------------------------------------------------------------------*
FORM f_set_pf_status  USING p_extab TYPE slis_t_extab.      "#EC CALLED

  SET PF-STATUS 'ALV_CUSTOM' EXCLUDING p_extab.

ENDFORM.                    "f_set_pf_status

*&---------------------------------------------------------------------*
*&      Form  f_alv_callback
*&---------------------------------------------------------------------*
FORM f_alv_callback  USING p_ucomm     TYPE sy-ucomm        "#EC CALLED
                           p_selfield  TYPE slis_selfield.

  IF p_ucomm = 'ZDELETE'.
    PERFORM f_delete_records.
    p_selfield-refresh = abap_true.
    CLEAR p_ucomm.
  ENDIF.

ENDFORM.                    "f_alv_callback

*&---------------------------------------------------------------------*
*&      Form  f_default_ranges
*&---------------------------------------------------------------------*
FORM f_default_ranges.

  FIELD-SYMBOLS: <status>    LIKE LINE OF s_status.

  APPEND INITIAL LINE TO s_status ASSIGNING <status>.
  <status>-sign   = 'I'.
  <status>-option = 'EQ'.
  <status>-low    = '02'.

  APPEND <status> TO s_status ASSIGNING <status>.
  <status>-low    = '03'.

  APPEND <status> TO s_status ASSIGNING <status>.
  <status>-low    = '05'.

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
  <sscr>-name = 'S_STATUS'.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction = l_restrict.

ENDFORM.                    "f_restrict_intervals

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
