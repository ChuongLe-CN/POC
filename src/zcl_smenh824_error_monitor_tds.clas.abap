class ZCL_SMENH824_ERROR_MONITOR_TDS definition
  public
  inheriting from ZCL_SMENH824_ERROR_MONITOR
  final
  create public .

*"* public components of class ZCL_SMENH824_ERROR_MONITOR_TDS
*"* do not include other source files here!!!
public section.

  constants C_END_OF_DAY type ZZTDS_TRAN_TYPE value '710'. "#EC NOTEXT
  constants C_VENDOR_BOL type ZZTDS_TRAN_TYPE value '502'. "#EC NOTEXT

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR
    importing
      !IM_TRAN_TYPE type ANY TABLE
      !IM_TRAN_REF_NBR type ANY TABLE
      !IM_LIFNR type ANY TABLE
      !IM_WERKS type ANY TABLE
      !IM_BOL_NBR type ANY TABLE
      !IM_TRAN_STUS type ANY TABLE
      !IM_TRAN_DT type ANY TABLE
      !IM_ERDAT type ANY TABLE
      !IM_END_OF_DAY type XFELD optional
      !IM_SKIP_CHECK type XFELD optional
      !IM_WITH_TEXT type XFELD optional
    raising
      ZCX_FLCM_ERROR .
  type-pools ABAP .
  class-methods PROCESS_ZMM_FLCM_TDS
    importing
      !IM_SELECTED type ZMM_FLCM_TDS_T
      !IM_OVERRIDE_OPEN_PERIOD type XFELD default ABAP_FALSE
    raising
      ZCX_FLCM_ERROR .
  methods SELECT_DESELECT_LINES
    importing
      !IM_TRAN_TYPE type ZZTDS_TRAN_TYPE
      !IM_SEL type XFELD .

  methods REFRESH_DATA
    redefinition .
  methods REPROCESS
    redefinition .
protected section.
*"* protected components of class ZCL_SMENH824_ERROR_MONITOR_TDS
*"* do not include other source files here!!!

  constants C_OBJECT type BALOBJ_D value 'ZSM'. "#EC NOTEXT
  constants C_SUBOBJECT type BALSUBOBJ value 'SM-INT-60A'. "#EC NOTEXT

  methods BUILD_EXTNUMBER_PATTERN
    redefinition .
  methods INCLUDE_IN_SELECTION
    redefinition .
  methods MESSAGE_DETAIL_TITLE
    redefinition .
  methods PROCESS_SELECTED_LINES
    redefinition .
  methods GET_LOG_HANDLE
    redefinition .
private section.
*"* private components of class ZCL_SMENH824_ERROR_MONITOR_TDS
*"* do not include other source files here!!!

  types:
    BEGIN OF t_reprocess_sequence,
           zparm_seq         TYPE zmm_flcm_parms-zparm_seq,
           ztds_tran_type    TYPE zztds_tran_type,
           method_name       TYPE progname,
         END OF t_reprocess_sequence .
  types:
    t_reprocess_sequence_table TYPE STANDARD TABLE OF t_reprocess_sequence
                                         WITH DEFAULT KEY .

  data V_END_OF_DAY type XFELD .
  data V_SKIP_CHECK type XFELD .
  class-data I_REPROCESS_SEQ type T_REPROCESS_SEQUENCE_TABLE .
  class-data V_REPROCESS_STEPS type I .

  type-pools ABAP .
  methods APPL_LOG_MESSAGES
    importing
      !IM_REF_NBR type ZZTDS_TRAN_REF_NBR_TABLE
      !IM_IN_REFRESH type XFELD default ABAP_FALSE
    changing
      !CH_TABLE type STANDARD TABLE .
  class-methods SMENH819
    importing
      !IM_TRAN_TYPE type ZZTDS_TRAN_TYPE
      !IM_OVERRIDE_OPEN_PERIOD type XFELD default ABAP_FALSE
    changing
      !CH_FLCM_TDS_TABLE type ZMM_FLCM_TDS_T
    raising
      ZCX_FLCM_ERROR .
  class-methods SMENH820
    importing
      !IM_TRAN_TYPE type ZZTDS_TRAN_TYPE
      !IM_OVERRIDE_OPEN_PERIOD type XFELD default ABAP_FALSE
    changing
      !CH_FLCM_TDS_TABLE type ZMM_FLCM_TDS_T
    raising
      ZCX_FLCM_ERROR .
  class-methods SMENH821
    importing
      !IM_TRAN_TYPE type ZZTDS_TRAN_TYPE
      !IM_OVERRIDE_OPEN_PERIOD type XFELD default ABAP_FALSE
    changing
      !CH_FLCM_TDS_TABLE type ZMM_FLCM_TDS_T
    raising
      ZCX_FLCM_ERROR .
  class-methods SMENH822
    importing
      !IM_TRAN_TYPE type ZZTDS_TRAN_TYPE
      !IM_OVERRIDE_OPEN_PERIOD type XFELD default ABAP_FALSE
    changing
      !CH_FLCM_TDS_TABLE type ZMM_FLCM_TDS_T
    raising
      ZCX_FLCM_ERROR .
ENDCLASS.



CLASS ZCL_SMENH824_ERROR_MONITOR_TDS IMPLEMENTATION.


METHOD appl_log_messages.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS->APPL_LOG_MESSAGES        *
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
* Rob West                  2012/04/09          DV5K971100             *
*                                                                      *
* Short Description: CR199440-T207046 & CR199440-T207145               *
*                    Fix Application Log refresh on reprocess          *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_messages          TYPE zztds_messages_table,
        l_tabix              TYPE sy-tabix VALUE 1.


  FIELD-SYMBOLS: <line>      TYPE ANY,
                 <refnum>    TYPE zztds_tran_ref_nbr,
                 <status>    TYPE zztran_stus,
                 <message>   TYPE zztds_messages,
                 <symsg>     TYPE symsg,
                 <text>      TYPE natxt,
                 <handle>    TYPE balloghndl.

  IF v_with_text = abap_false
          AND im_in_refresh = abap_false.                    "DV5K971100
    RETURN.
  ENDIF.

  CALL METHOD zcl_smint60a_zmm_flcm_tds=>get_all_messages
    EXPORTING
      im_tran_ref_nbr_table = im_ref_nbr[]
      im_free_all_logs      = abap_true
*      im_free_all_logs      = im_free_all_logs
      im_most_recent        = abap_true
    CHANGING
      ch_messages           = li_messages[].

  IF li_messages[] IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT li_messages ASSIGNING <message>.
    LOOP AT ch_table ASSIGNING <line> FROM l_tabix.
      ASSIGN COMPONENT 'ZTDS_TRAN_REF_NB' OF STRUCTURE <line> TO <refnum>.
      IF <refnum> < <message>-ztds_tran_ref_nb.
        CONTINUE.
      ELSE.
        l_tabix = sy-tabix + 1.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF <refnum> IS ASSIGNED AND  <refnum> = <message>-ztds_tran_ref_nb.
      ASSIGN COMPONENT 'ALV_LOG_HANDLE' OF STRUCTURE <line> TO <handle>.
      <handle> = <message>-log_handle.
      ASSIGN COMPONENT 'ALV_TEXT' OF STRUCTURE <line> TO <text>.
      CLEAR: <text>.
      ASSIGN COMPONENT 'ZTRAN_STUS' OF STRUCTURE <line> TO <status>.
      IF ( <status> = '01' OR <status> = '04' ) AND
         ( <message>-msg_cnt_e > 0 OR
           <message>-msg_cnt_a > 0 ).
        LOOP AT <message>-messages ASSIGNING <symsg>
            WHERE msgty = 'E' OR msgty = 'A'.
          MESSAGE ID <symsg>-msgid
                   TYPE <symsg>-msgty
                   NUMBER <symsg>-msgno
                   WITH <symsg>-msgv1
                        <symsg>-msgv2
                        <symsg>-msgv3
                        <symsg>-msgv4
                   INTO <text>.
          EXIT.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CALL METHOD zcl_smint60a_zmm_flcm_tds=>free_appl_log.

ENDMETHOD.


METHOD build_extnumber_pattern.

  CLEAR ch_extnumber+10(6) WITH '_'.

ENDMETHOD.


METHOD class_constructor.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS=>CLASS_CONSTRUCTOR        *
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
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  SELECT zparm_seq
         zval_from
         zval_to
    FROM zmm_flcm_parms
    INTO TABLE i_reprocess_seq
    WHERE progname = 'SMENH824' AND
          zparm_nm = 'ZTDS_TRAN_TYPE_SEQ'.

  v_reprocess_steps = sy-dbcnt.
  IF v_reprocess_steps = 0.
    MESSAGE a056(zz_flcm).
  ENDIF.

  SORT i_reprocess_seq BY zparm_seq ASCENDING.

ENDMETHOD.


METHOD constructor.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS->CONSTRUCTOR              *
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
* Nancy Bergeron            2014/07/03          DV5K987404             *
*                                                                      *
* Short Description: Add edit mask on new number field to remove coma  *
*                    from integer.                                     *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_flcm_tds          TYPE STANDARD TABLE OF zmm_flcm_tds,
        li_flcm_end_of_day   TYPE STANDARD TABLE OF zmm_flcm_tds,
        li_ref_nbr           TYPE zztds_tran_ref_nbr_table,
        lr_tran_type         TYPE RANGE OF zztds_tran_type.

  FIELD-SYMBOLS: <flcm_tds>  TYPE zmm_flcm_tds,
                 <selopt>    LIKE LINE OF lr_tran_type,
                 <icon>      TYPE icon_d,
                 <line>      TYPE ANY,
                 <table>     TYPE STANDARD TABLE,
                 <fcat>      TYPE slis_fieldcat_alv.

  CALL METHOD super->constructor
    EXPORTING
      im_tabname   = 'ZMM_FLCM_TDS'
      im_with_text = im_with_text.

*Begin DV5K987404
  loop at i_fieldcat ASSIGNING <fcat> WHERE fieldname = 'ZINV_DIFF'
                                         or fieldname = 'ZCUMULATE_DIFF'
                                         or fieldname = 'ZSAP_INV'.
    <fcat>-datatype = 'QUAN'.
  endloop.
*End DV5K987404


  v_end_of_day = im_end_of_day.
  v_skip_check = im_skip_check.
  v_with_text  = im_with_text.
  lr_tran_type[] = im_tran_type[].

  ASSIGN o_data_table->* TO <table>.

  IF '00' NOT IN im_tran_stus AND
    ( v_end_of_day = abap_true OR
      v_skip_check = abap_true ).
    SELECT *
      FROM zmm_flcm_tds
      INTO TABLE li_flcm_end_of_day
      WHERE ztran_stus = '00'.
    IF v_skip_check = abap_false.     "End of day only selected
      DELETE li_flcm_end_of_day WHERE ztds_tran_type <> c_end_of_day.
    ENDIF.
  ENDIF.

  IF v_end_of_day = abap_true AND c_end_of_day NOT IN lr_tran_type.   "Add End of Day Tran Type
    APPEND INITIAL LINE TO lr_tran_type ASSIGNING <selopt>.
    <selopt>-sign = 'I'.
    <selopt>-option = 'EQ'.
    <selopt>-low = c_end_of_day.
  ENDIF.

  IF im_tran_stus[] IS INITIAL.
    SELECT *
      FROM zmm_flcm_tds
      INTO TABLE li_flcm_tds
      WHERE ztds_tran_type IN lr_tran_type[] AND
            ztds_tran_ref_nb IN im_tran_ref_nbr[] AND
            werks IN im_werks[] AND
            ztds_bol_nbr IN im_bol_nbr[].
  ELSE.
    SELECT *
      FROM zmm_flcm_tds
      INTO TABLE li_flcm_tds
      WHERE ztran_stus IN im_tran_stus.
  ENDIF.

  APPEND LINES OF li_flcm_end_of_day TO li_flcm_tds.
  SORT li_flcm_tds BY ztds_tran_ref_nb ASCENDING.

  IF im_tran_stus[] IS INITIAL.
    LOOP AT li_flcm_tds ASSIGNING <flcm_tds>
        WHERE ( lifnr IN im_lifnr[] AND
                ztran_stus IN im_tran_stus AND
                ztds_tran_dt IN im_tran_dt[] AND
                erdat IN im_erdat[] ).
      APPEND INITIAL LINE TO <table> ASSIGNING <line>.
      MOVE-CORRESPONDING <flcm_tds> TO <line>.
      ASSIGN COMPONENT 'ALV_ICON' OF STRUCTURE <line> TO <icon>.
      <icon> = convert_status_to_icon( <flcm_tds>-ztran_stus ).
      APPEND <flcm_tds>-ztds_tran_ref_nb TO li_ref_nbr.
    ENDLOOP.
  ELSE.
    LOOP AT li_flcm_tds ASSIGNING <flcm_tds>
        WHERE ( ztds_tran_type IN lr_tran_type[] AND
                ztds_tran_ref_nb IN im_tran_ref_nbr[] AND
                lifnr IN im_lifnr[] AND
                werks IN im_werks[] AND
                ztds_bol_nbr IN im_bol_nbr[] AND
                ztds_tran_dt IN im_tran_dt[] AND
                erdat IN im_erdat[] ).
      APPEND INITIAL LINE TO <table> ASSIGNING <line>.
      MOVE-CORRESPONDING <flcm_tds> TO <line>.
      ASSIGN COMPONENT 'ALV_ICON' OF STRUCTURE <line> TO <icon>.
      <icon> = convert_status_to_icon( <flcm_tds>-ztran_stus ).
      IF <flcm_tds>-ztran_stus = '01' OR <flcm_tds>-ztran_stus = '04'.
        APPEND <flcm_tds>-ztds_tran_ref_nb TO li_ref_nbr.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF <table>[] IS INITIAL.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid  = 'ZZ-CN2'
        msgty  = 'I'
        msgno  = '909'.
  ENDIF.

  CALL METHOD appl_log_messages
    EXPORTING
      im_ref_nbr = li_ref_nbr[]
    CHANGING
      ch_table   = <table>.

  CALL METHOD zcl_smint60a_zmm_flcm_tds=>build_grid_fcat
    EXPORTING
      im_hide_ref_nb = abap_true
    CHANGING
      ch_fcat        = v_sbal_profile-mess_fcat[].

  v_sbal_profile_multi-mess_fcat[] = v_sbal_profile-mess_fcat[].
  CALL METHOD zcl_smint60a_zmm_flcm_tds=>build_tree_fcat
    CHANGING
      ch_fcat = v_sbal_profile_multi-lev1_fcat[]
      ch_sort = v_sbal_profile_multi-lev1_sort[].

ENDMETHOD.


METHOD get_log_handle.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS->GET_LOG_HANDLE           *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2012                                               *
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
* Rob West                  2012/03/01          DV5K970355             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  TYPES: BEGIN OF lt_balhdr,
           object              TYPE balhdr-object,
           subobject           TYPE balhdr-subobject,
           extnumber           TYPE balhdr-extnumber,
           alstate             TYPE balhdr-alstate,
           log_handle          TYPE balhdr-log_handle,
         END OF lt_balhdr.

  DATA: li_balhdr              TYPE STANDARD TABLE OF lt_balhdr,
        l_extnumber            TYPE balnrext.


  FIELD-SYMBOLS: <tranref>     TYPE zmm_flcm_tds-ztds_tran_ref_nb,
                 <balhdr>      TYPE lt_balhdr,
                 <handle>      TYPE balloghndl.

  ASSIGN COMPONENT 'ZTDS_TRAN_REF_NB' OF STRUCTURE im_line TO <tranref>.
  IF sy-subrc = 0.
    l_extnumber+0(10) = <tranref>.
    l_extnumber+10(6) = '______'.
    SELECT object
           subobject
           extnumber
           alstate
           log_handle
      FROM balhdr
      INTO TABLE li_balhdr
      WHERE extnumber LIKE l_extnumber.
    IF sy-subrc = 0.
      DELETE li_balhdr WHERE NOT ( object = c_object AND
                                   subobject = c_subobject AND
                                   alstate IS INITIAL ).
      IF li_balhdr[] IS NOT INITIAL.
        IF LINES( li_balhdr[] ) > 1.
          SORT li_balhdr BY extnumber+0(10) ASCENDING
                            extnumber+10(6) DESCENDING.
        ENDIF.
        READ TABLE li_balhdr ASSIGNING <balhdr> INDEX 1.
        IF sy-subrc = 0.
          re_log_handle = <balhdr>-log_handle.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD include_in_selection.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS->INCLUDE_IN_SELECTION     *
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
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  FIELD-SYMBOLS: <line>      TYPE zmm_flcm_tds.

  re_include = super->include_in_selection( im_selected_line ).

  IF re_include = abap_false.
    ASSIGN im_selected_line TO <line> CASTING.
    IF v_end_of_day = abap_true AND
       <line>-ztds_tran_type = c_end_of_day AND
       <line>-ztran_stus = '00'.
      re_include = abap_true.
    ENDIF.
    IF v_skip_check = abap_true AND
       <line>-ztran_stus = '00'.
      re_include = abap_true.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD message_detail_title.

  FIELD-SYMBOLS: <tds>         TYPE zmm_flcm_tds.

  ASSIGN im_line TO <tds> CASTING.
  IF sy-subrc = 0.
    ch_grid_title = <tds>-ztds_tran_ref_nb.
  ELSE.
    CLEAR ch_grid_title.
  ENDIF.

ENDMETHOD.


METHOD process_selected_lines.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS->PROCESS_SELECTED_LINES   *
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
* Rob West                  2011/10/18          DVSK904456/DV5K967173  *
*                                                                      *
* Short Description: FLCM defect 616                                   *
*                    Add message if '00' status records are selected   *
*                    for reprocessing (other than end-of-day)          *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

*** Start of code added C169709-T195761 defect 616 DVSK904456/DV5K967173
  DATA: l_status_00_found    TYPE zztds_tran_type,
        l_answer             TYPE char01.

  FIELD-SYMBOLS: <tds>       TYPE zmm_flcm_tds.

  LOOP AT im_selected ASSIGNING <tds>.
    IF <tds>-ztran_stus = '00' AND <tds>-ztds_tran_type <> c_end_of_day.
      l_status_00_found = <tds>-ztds_tran_type.
      IF <tds>-ztds_tran_type = c_vendor_bol.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF l_status_00_found IS NOT INITIAL.
    IF l_status_00_found = c_vendor_bol.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Warning'(q00)
          text_question         = 'Fuel PO could be created with wrong price. Validate if contract price is available for transaction date.'(q01)
          text_button_1         = 'Continue'(q03)
          icon_button_1         = 'ICON_OKAY '
          text_button_2         = 'Cancel'(q04)
          icon_button_2         = 'ICON_CANCEL'
          display_cancel_button = ' '
        IMPORTING
          answer                = l_answer.
    ELSE.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Warning'(q00)
          text_question         = 'Stock will be updated. This will have an impact in the physical inventory adjustment.'(q02)
          text_button_1         = 'Continue'(q03)
          icon_button_1         = 'ICON_OKAY '
          text_button_2         = 'Cancel'(q04)
          icon_button_2         = 'ICON_CANCEL'
          display_cancel_button = ' '
        IMPORTING
          answer                = l_answer.
    ENDIF.
    IF l_answer = '2'.
      RETURN.       "Exit if «Cancel» selected
    ENDIF.
  ENDIF.
*** End of code added C169709-T195761 defect 616   DVSK904456/DV5K967173

  CALL METHOD process_zmm_flcm_tds    "Call static method shared by SM-ENH-822
      EXPORTING
        im_selected             = im_selected[]
        im_override_open_period = v_skip_check.

ENDMETHOD.


METHOD process_zmm_flcm_tds.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS=>PROCESS_ZMM_FLCM_TDS     *
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
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_new               TYPE zmm_flcm_tds_t,
        li_cancel            TYPE zmm_flcm_tds_t,
        li_rebill            TYPE zmm_flcm_tds_t,

        l_tabix              TYPE i.

  FIELD-SYMBOLS: <tds>       TYPE zmm_flcm_tds,
                 <sequence>  TYPE t_reprocess_sequence.

  IF im_selected[] IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT im_selected ASSIGNING <tds>.
    CASE <tds>-ztds_canc_rbil.
      WHEN ' '.  APPEND <tds> TO li_new.
      WHEN 'C'.  APPEND <tds> TO li_cancel.
      WHEN 'R'.  APPEND <tds> TO li_rebill.
    ENDCASE.
  ENDLOOP.

*--------------------------------------------------------------------*
  IF li_new[] IS NOT INITIAL.
    SORT li_new BY ztds_tran_ref_nb ASCENDING
                   ztran_stus       DESCENDING.
    LOOP AT i_reprocess_seq ASSIGNING <sequence>.
      CALL METHOD (<sequence>-method_name)
        EXPORTING
          im_tran_type            = <sequence>-ztds_tran_type
          im_override_open_period = im_override_open_period
        CHANGING
          ch_flcm_tds_table       = li_new[].
      IF li_new[] IS INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

*--------------------------------------------------------------------*
  IF li_cancel[] IS NOT INITIAL.
    l_tabix = v_reprocess_steps.
    SORT li_cancel BY ztds_tran_ref_nb DESCENDING
                      ztran_stus       DESCENDING.
    WHILE l_tabix > 0.
      READ TABLE i_reprocess_seq ASSIGNING <sequence> INDEX l_tabix.
      IF sy-subrc = 0.
        CALL METHOD (<sequence>-method_name)
          EXPORTING
            im_tran_type            = <sequence>-ztds_tran_type
            im_override_open_period = im_override_open_period
          CHANGING
            ch_flcm_tds_table       = li_cancel[].
      ENDIF.
      IF li_cancel[] IS INITIAL.
        CLEAR l_tabix.
      ELSE.
        SUBTRACT 1 FROM l_tabix.
      ENDIF.
    ENDWHILE.
  ENDIF.

*--------------------------------------------------------------------*
  IF li_rebill[] IS NOT INITIAL.
    SORT li_rebill BY ztds_tran_ref_nb ASCENDING
                      ztran_stus       DESCENDING.
    LOOP AT i_reprocess_seq ASSIGNING <sequence>.
      CALL METHOD (<sequence>-method_name)
        EXPORTING
          im_tran_type            = <sequence>-ztds_tran_type
          im_override_open_period = im_override_open_period
        CHANGING
          ch_flcm_tds_table       = li_rebill[].
      IF li_rebill[] IS INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD refresh_data.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS->REFRESH_DATA             *
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
* Nancy Bergeron            2014/06/12          DV5K987403             *
* Add new logic to calculate the difference between TDS and SAP        *
* inventory.   SM-ENH-822  Task 261851                                 *
*----------------------------------------------------------------------*
* Rob West                  2012/04/09          DV5K971100             *
*                                                                      *
* Short Description: CR199440-T207046 & CR199440-T207145               *
*                    Fix Application Log refresh on reprocess          *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: lo_line              TYPE REF TO data,
        li_ref_nbr           TYPE zztds_tran_ref_nbr_table.

  FIELD-SYMBOLS: <line>      TYPE ANY,
                 <tabix>     TYPE sy-tabix,
                 <tds>       TYPE zmm_flcm_tds,
                 <icon>      TYPE icon_d,
                 <table>     TYPE STANDARD TABLE.

  CREATE DATA lo_line TYPE HANDLE o_line_type.
  ASSIGN lo_line->* TO <line>.

  ASSIGN <line> TO <tds> CASTING.
  ASSIGN COMPONENT 'ALV_ICON' OF STRUCTURE <line> TO <icon>.

  ASSIGN o_data_table->* TO <table>.

  LOOP AT im_sel_tabix ASSIGNING <tabix>.
    READ TABLE <table> INTO <line> INDEX <tabix>.
    SELECT SINGLE ztran_stus
                  ztran_sub_stus
                  aedat
                  aezeit
                  aename
*NB Begin
                  zsap_inv
                  zinv_diff
*                  zcumulate_diff "DVSK989018
                  zinv_diff_delta "DVSK989018
*NB end

      FROM zmm_flcm_tds
      INTO (<tds>-ztran_stus,
            <tds>-ztran_sub_stus,
            <tds>-aedat,
            <tds>-aezeit,
            <tds>-aename,
            <tds>-zsap_inv,          "I-DV5K987403
            <tds>-zinv_diff,         "I-DV5K987403
*            <tds>-zcumulate_diff )   "I-DV5K987403 d-DVSK989018
            <tds>-zinv_diff_delta )   "I-DVSK989018
    WHERE ztds_tran_type   = <tds>-ztds_tran_type AND
          ztds_tran_ref_nb = <tds>-ztds_tran_ref_nb AND
          werks            = <tds>-werks AND
          ztds_folio_yr    = <tds>-ztds_folio_yr AND
          ztds_folio_mth   = <tds>-ztds_folio_mth AND
          ztds_folio_nbr   = <tds>-ztds_folio_nbr AND
          ztds_folio_seq   = <tds>-ztds_folio_seq AND
          ztds_trml_id     = <tds>-ztds_trml_id AND
          ztds_bol_nbr     = <tds>-ztds_bol_nbr.
    <icon> = convert_status_to_icon( <tds>-ztran_stus ).
    MODIFY <table> FROM <line> INDEX <tabix>
          TRANSPORTING ('ZTRAN_STUS')
                       ('ZTRAN_SUB_STUS')
                       ('AEDAT')
                       ('AEZEIT')
                       ('AENAME')
                       ('ZSAP_INV')         "I-DV5K987403
                       ('ZINV_DIFF')        "I-DV5K987403
*                       ('ZCUMULATE_DIFF')   "I-DV5K987403 d-DVSK989018
                       ('ZINV_DIFF_DELTA')   "I-DVSK989018
                       ('ALV_ICON').
    APPEND <tds>-ztds_tran_ref_nb TO li_ref_nbr.
  ENDLOOP.

  CALL METHOD appl_log_messages
    EXPORTING
      im_ref_nbr = li_ref_nbr[]
      im_in_refresh = abap_true                            "DV5K971100
    CHANGING
      ch_table   = <table>.

  CALL METHOD zcl_smint60a_zmm_flcm_tds=>free_appl_log.

ENDMETHOD.


METHOD reprocess.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS->REPROCESS                *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

  DATA: l_tabix              TYPE i,
        l_unselected         TYPE i.

  FIELD-SYMBOLS: <line>      TYPE ANY,
                 <table>     TYPE STANDARD TABLE,
                 <type>      TYPE zztds_tran_type,
                 <sel>       TYPE xfeld.

*--------------------------------------------------------------------*
*** Do custom processing to identify End of Day selections

  IF v_end_of_day = abap_true.
    ASSIGN o_data_table->* TO <table>.
    LOOP AT <table> ASSIGNING <line>.
      ASSIGN COMPONENT 'ZTDS_TRAN_TYPE' OF STRUCTURE <line> TO <type>.
      IF <type> = c_end_of_day AND include_in_selection( <line> ) = abap_true.
        ASSIGN COMPONENT 'ALV_SEL' OF STRUCTURE <line> TO <sel>.
        IF l_tabix = 0.
          IF <sel> = abap_true.
            l_tabix = sy-tabix.      "Index of first selected End of Day
            IF l_unselected > 0.
              EXIT.                  "No more looping needed
            ENDIF.
          ELSE.
            ADD 1 TO l_unselected.
          ENDIF.
        ELSE.                        "Next End of Day record
          IF <sel> = abap_false.
            ADD 1 TO l_unselected.
            IF l_tabix > 0.
              EXIT.                  "No more looping needed
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF l_tabix > 0 AND         "At least one End of Day selected
       l_unselected > 0.       "One or more End of Day not selected.
      RAISE EXCEPTION TYPE zcx_flcm_error                   "#EC *
        EXPORTING
          msgid  = 'ZZ_FLCM'
          msgty  = 'I'
          msgno  = '077'.
      MESSAGE i077(zz_flcm).      "For «Where-Used» only
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
*** Call the REPROCESS method of the superclass

  CALL METHOD super->reprocess
    EXPORTING
      im_ucomm    = im_ucomm
    CHANGING
      ch_selfield = ch_selfield.

ENDMETHOD.


METHOD select_deselect_lines.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS->SELECT_DESELECT_LINES    *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

  DATA: l_tabix              TYPE sy-tabix.

  FIELD-SYMBOLS: <line>      TYPE ANY,
                 <sel>       TYPE xfeld,
                 <type>      TYPE zztds_tran_type,
                 <table>     TYPE STANDARD TABLE.

  ASSIGN o_data_table->* TO <table>.

  ASSIGN COMPONENT 'ALV_SEL' OF STRUCTURE <line> TO <sel>.

  LOOP AT <table> ASSIGNING <line>.
    l_tabix = sy-tabix.
    ASSIGN COMPONENT 'ZTDS_TRAN_TYPE' OF STRUCTURE <line> TO <type>.
    IF <type> = im_tran_type.
      IF im_sel = abap_true.
        IF include_in_selection( <line> ) = abap_true.
          ASSIGN COMPONENT 'ALV_SEL' OF STRUCTURE <line> TO <sel>.
          <sel> = abap_true.
        ENDIF.
      ELSE.
        ASSIGN COMPONENT 'ALV_SEL' OF STRUCTURE <line> TO <sel>.
        <sel> = abap_false.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD smenh819.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS=>SMENH819                 *
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
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_flcm_tds          TYPE zmm_flcm_tds_t,
        l_tabix              TYPE i,

        lo_smenh819           TYPE REF TO zcl_flcm_trx_proc_bol.

  FIELD-SYMBOLS: <tds>       TYPE zmm_flcm_tds.

  LOOP AT ch_flcm_tds_table ASSIGNING <tds>
      WHERE ztds_tran_type = im_tran_type.
    l_tabix = sy-tabix.
    APPEND <tds> TO li_flcm_tds.
    DELETE ch_flcm_tds_table INDEX l_tabix.
  ENDLOOP.

  IF li_flcm_tds[] IS INITIAL.
    RETURN.
  ENDIF.

* Create instance of the class ZCL_FLCM_TDS_PROC_BOL
  CREATE OBJECT lo_smenh819
    EXPORTING
      it_trans = li_flcm_tds[].

  IF im_override_open_period = abap_true.
    CALL METHOD zcl_flcm_services=>set_override_open_period.
  ENDIF.

* Execute the program logic in class ZCL_FLCM_TDS_PROC_BOL
  CALL METHOD lo_smenh819->execute.

ENDMETHOD.


METHOD smenh820.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS=>SMENH820                 *
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
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_flcm_tds          TYPE zmm_flcm_tds_t,
        l_tabix              TYPE i,

        lo_tds_gr_confirm TYPE REF TO zcl_flcm_tds_gr_confirm.


  FIELD-SYMBOLS: <tds>       TYPE zmm_flcm_tds.

  LOOP AT ch_flcm_tds_table ASSIGNING <tds>
      WHERE ztds_tran_type = im_tran_type.
    l_tabix = sy-tabix.
    APPEND <tds> TO li_flcm_tds.
    DELETE ch_flcm_tds_table INDEX l_tabix.
  ENDLOOP.

  IF li_flcm_tds[] IS INITIAL.
    RETURN.
  ENDIF.

* Create instance of the class ZCL_FLCM_TDS_GR_CONFIRM
  CREATE OBJECT lo_tds_gr_confirm.

  IF im_override_open_period = abap_true.
    CALL METHOD zcl_flcm_services=>set_override_open_period.
  ENDIF.

* Execute the program logic in class ZCL_FLCM_TDS_GR_CONFIRM
  CALL METHOD lo_tds_gr_confirm->process_transactions
    EXPORTING
      it_trans = li_flcm_tds[].

ENDMETHOD.


METHOD SMENH821.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS=>SMENH821                 *
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
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_flcm_tds          TYPE zmm_flcm_tds_t,
        l_tabix              TYPE i,

        lo_smenh821          TYPE REF TO zcl_flcm_trx_proc_gi_fuel.


  FIELD-SYMBOLS: <tds>       TYPE zmm_flcm_tds.

  LOOP AT ch_flcm_tds_table ASSIGNING <tds>
      WHERE ztds_tran_type = im_tran_type.
    l_tabix = sy-tabix.
    APPEND <tds> TO li_flcm_tds.
    DELETE ch_flcm_tds_table INDEX l_tabix.
  ENDLOOP.

  IF li_flcm_tds[] IS INITIAL.
    RETURN.
  ENDIF.

* Create instance of the class ZCL_FLCM_TDS_PROC_BOL
  CREATE OBJECT lo_smenh821
    EXPORTING
      it_trans     = li_flcm_tds.

  IF im_override_open_period = abap_true.
    CALL METHOD zcl_flcm_services=>set_override_open_period.
  ENDIF.

* Execute the program logic in class ZCL_FLCM_TDS_PROC_BOL
  CALL METHOD lo_smenh821->execute.

ENDMETHOD.


METHOD smenh822.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR_TDS=>SMENH822                 *
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
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_flcm_tds          TYPE zmm_flcm_tds_t,
        l_tabix              TYPE i,

        lo_smenh822          TYPE REF TO zcl_flcm_trx_end_of_day.

  FIELD-SYMBOLS: <tds>       TYPE zmm_flcm_tds.

  LOOP AT ch_flcm_tds_table ASSIGNING <tds>
      WHERE ztds_tran_type = im_tran_type.
    l_tabix = sy-tabix.
    APPEND <tds> TO li_flcm_tds.
    DELETE ch_flcm_tds_table INDEX l_tabix.
  ENDLOOP.

  IF li_flcm_tds[] IS INITIAL.
    RETURN.
  ENDIF.

  SORT li_flcm_tds BY ztds_folio_yr  ASCENDING
                      ztds_folio_mth ASCENDING
                      ztds_folio_nbr ASCENDING
                      werks          ASCENDING.

* Create instance of the class ZCL_FLCM_TDS_PROC_BOL
  CREATE OBJECT lo_smenh822
    EXPORTING
      it_trans = li_flcm_tds[].

  IF im_override_open_period = abap_true.
    CALL METHOD zcl_flcm_services=>set_override_open_period.
  ENDIF.

* Execute the program logic in class ZCL_FLCM_TDS_PROC_BOL
  CALL METHOD lo_smenh822->execute.

ENDMETHOD.
ENDCLASS.
