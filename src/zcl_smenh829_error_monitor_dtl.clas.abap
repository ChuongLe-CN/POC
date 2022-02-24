class ZCL_SMENH829_ERROR_MONITOR_DTL definition
  public
  inheriting from ZCL_SMENH824_ERROR_MONITOR
  final
  create public .

*"* public components of class ZCL_SMENH829_ERROR_MONITOR_DTL
*"* do not include other source files here!!!
public section.

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR
    importing
      !IM_FEVT_ID type ANY TABLE
      !IM_FEVT_VER type ANY TABLE
      !IM_LIFNR type ANY TABLE
      !IM_WERKS type ANY TABLE
      !IM_BOL_TCKT_NBR type ANY TABLE
      !IM_TRAN_STUS type ANY TABLE
      !IM_LOAD_DT type ANY TABLE
      !IM_WARN_TYP type ANY TABLE
      !IM_WITH_TEXT type XFELD optional
    raising
      ZCX_FLCM_ERROR .
  methods GET_BOL_SELECTED
    importing
      !IM_TABIX type SY-TABIX default 0
    changing
      !CH_BOL_TABLE type STANDARD TABLE
      !CH_SEL_TABIX type RE_T_TABIX optional .
  class-methods APPL_LOG_MESSAGES
    importing
      !IM_TABNAME type C
      !IM_EVENT_FIELDNAME type C
      !IM_VERSION_FIELDNAME type C
      !IM_EVENT_KEYS type ZSM_SMINT666_EVENT_KEY_TABLE
      !IM_IN_REFRESH type XFELD default ABAP_FALSE
    changing
      !CH_TABLE type STANDARD TABLE .

  methods REFRESH_DATA
    redefinition .
protected section.
*"* protected components of class ZCL_SMENH829_ERROR_MONITOR_DTL
*"* do not include other source files here!!!

  constants C_OBJECT type BALOBJ_D value 'ZSM'. "#EC NOTEXT
  constants C_SUBOBJECT type BALSUBOBJ value 'SM-INT-666'. "#EC NOTEXT

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
*"* private components of class ZCL_SMENH829_ERROR_MONITOR_DTL
*"* do not include other source files here!!!

  types:
    t_zmm_dtl_fuel_evt_table TYPE STANDARD TABLE OF zmm_dtl_fuel_evt
                                      WITH DEFAULT KEY .
  types:
    BEGIN OF t_xtra_chg,
           zdtl_xchg_id      TYPE zmm_dtl_xtra_chg-zdtl_xchg_id,
           zdtl_xchg_ver     TYPE zmm_dtl_xtra_chg-zdtl_xchg_ver,
           lifnr             TYPE zmm_dtl_xtra_chg-lifnr,
           zbol_tckt_nbr     TYPE zmm_dtl_xtra_chg-zbol_tckt_nbr,
           werks             TYPE zmm_dtl_xtra_chg-werks,
           ztran_stus        TYPE zmm_dtl_xtra_chg-ztran_stus,
         END OF t_xtra_chg .
  types:
    t_xtra_chg_table    TYPE STANDARD TABLE OF t_xtra_chg
                                  WITH DEFAULT KEY .

  data I_EXTRA_CHARGE_ERRORS type T_XTRA_CHG_TABLE .
  data V_BINARY_XTRA type XFELD .
  data V_BINARY_AUDIT type XFELD .
  class-data V_COE_SUPPORT type XFELD .

  methods LOAD_EXTRA_CHARGES
    importing
      !IM_FUEL_EVENTS type T_ZMM_DTL_FUEL_EVT_TABLE optional
      !IM_BOL_TCKT_NBR type ZZBOL_TICKET_NBR optional .
  methods CHECK_EXTRA_CHARGES
    importing
      !IM_LIFNR type LIFNR
      !IM_BOL type ZZBOL_TICKET_NBR
      !IM_WERKS type WERKS_D
    changing
      value(CH_ICON) type ICON_D .
  class-methods SUBMIT_JOB
    importing
      !IM_FEVT_ID type ZZDTL_FUEL_EVENT_ID
      !IM_FEVT_VER type ZZDTL_FUEL_EVENT_VER .
ENDCLASS.



CLASS ZCL_SMENH829_ERROR_MONITOR_DTL IMPLEMENTATION.


METHOD appl_log_messages.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH829_ERROR_MONITOR_DTL=>APPL_LOG_MESSAGES        *
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
* Rob West                  2012/04/09          DV5K971100             *
*                                                                      *
* Short Description: CR199440-T207046 & CR199440-T207145               *
*                    Fix Application Log refresh on reprocess          *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961547             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_messages          TYPE zsm_smint666_messages_table.

  FIELD-SYMBOLS: <line>      TYPE ANY,
                 <message>   TYPE zsm_smint666_messages,
                 <status>    TYPE zztran_stus,
                 <handle>    TYPE balloghndl,
                 <evt_id>    TYPE zzdtl_fuel_event_id,
                 <evt_ver>   TYPE zzdtl_fuel_event_ver,
                 <symsg>     TYPE symsg,
                 <text>      TYPE natxt.

  IF v_with_text = abap_false
         AND im_in_refresh = abap_false.                     "DV5K971100
    RETURN.
  ENDIF.

  CALL METHOD zcl_smint666_zmm_dtl_msgs=>get_all_messages
    EXPORTING
      im_tabname       = im_tabname
      im_event_keys    = im_event_keys[]
      im_most_recent   = abap_true
      im_free_all_logs = abap_true
    CHANGING
      ch_messages      = li_messages[].

  IF li_messages[] IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT ch_table ASSIGNING <line>.
    ASSIGN COMPONENT im_event_fieldname OF STRUCTURE <line>
        TO <evt_id>.
    ASSIGN COMPONENT im_version_fieldname OF STRUCTURE <line>
        TO <evt_ver>.
    READ TABLE li_messages ASSIGNING <message>
        WITH KEY zdtl_fevt_id = <evt_id>
                 zdtl_fevt_ver = <evt_ver>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'ALV_LOG_HANDLE' OF STRUCTURE <line> TO <handle>.
      <handle> = <message>-log_handle.
      ASSIGN COMPONENT 'ALV_TEXT' OF STRUCTURE <line> TO <text>.
      CLEAR <text>.
      ASSIGN COMPONENT 'ZTRAN_STUS' OF STRUCTURE <line> TO <status>.
      IF <status> = '04' AND
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

  CALL METHOD zcl_smint666_zmm_dtl_msgs=>free_appl_log.

ENDMETHOD.


METHOD build_extnumber_pattern.

  CLEAR ch_extnumber+45(6) WITH '_'.

ENDMETHOD.


METHOD check_extra_charges.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH829_ERROR_MONITOR_DTL->CHECK_EXTRA_CHARGES      *
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
* Rob West                  2011/03/01          DV5K961547             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  IF ch_icon <> icon_green_light.
    RETURN.
  ENDIF.

  IF v_binary_xtra = abap_true.
    READ TABLE i_extra_charge_errors TRANSPORTING NO FIELDS
        WITH KEY lifnr = im_lifnr
                 zbol_tckt_nbr = im_bol
                 werks = im_werks BINARY SEARCH.
  ELSE.
    READ TABLE i_extra_charge_errors TRANSPORTING NO FIELDS
        WITH KEY lifnr = im_lifnr
                 zbol_tckt_nbr = im_bol
                 werks = im_werks.
  ENDIF.

  IF sy-subrc = 0.
    ch_icon = icon_yellow_light.
  ENDIF.

ENDMETHOD.


METHOD class_constructor.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH829_ERROR_MONITOR_DTL=>CLASS_CONSTRUCTOR        *
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
* Rob West                  2011/03/01          DV5K961547             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  AUTHORITY-CHECK OBJECT 'ZDTL_SUPPT'
           ID 'ACTVT' FIELD '36'.
  IF sy-subrc = 0.
    v_coe_support = abap_true.
  ELSE.
    v_coe_support = abap_false.
  ENDIF.

ENDMETHOD.


METHOD constructor.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH829_ERROR_MONITOR_DTL->CONSTRUCTOR              *
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
* Rob West                  2011/03/01          DV5K961547             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_fuel_evt          TYPE STANDARD TABLE OF zmm_dtl_fuel_evt,
        li_event_keys        TYPE zsm_smint666_event_key_table,
        l_tabix              TYPE sy-tabix.

  FIELD-SYMBOLS: <fuel_evt>  TYPE zmm_dtl_fuel_evt,
                 <icon>      TYPE icon_d,
                 <line>      TYPE ANY,
                 <table>     TYPE STANDARD TABLE,
                 <fcat>      TYPE slis_fieldcat_alv,
                 <fcat2>     TYPE slis_fieldcat_alv,
                 <event_key> TYPE zsm_smint666_event_key.


  CALL METHOD super->constructor
    EXPORTING
      im_tabname   = 'ZMM_DTL_FUEL_EVT'
      im_with_text = im_with_text.

  ASSIGN o_data_table->* TO <table>.

  SELECT *
    FROM zmm_dtl_fuel_evt
    INTO TABLE li_fuel_evt
    WHERE zdtl_fevt_id IN im_fevt_id[] AND
          zdtl_fevt_ver IN im_fevt_ver[].

  IF sy-subrc > 0.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid  = 'ZZ-CN2'
        msgty  = 'I'
        msgno  = '909'.
  ENDIF.

  DELETE li_fuel_evt WHERE NOT ( lifnr IN im_lifnr[] AND
                                 werks IN im_werks[] AND
                                 zbol_tckt_nbr IN im_bol_tckt_nbr[] AND
                                 ztran_stus IN im_tran_stus[] AND
*                                 zdtl_load_dt IN im_load_dt[] ).      "D - TXT25668 - C403761 T456560 - DV5K9A0R5S
*** Start of Insert TXT25668 - C403761 T456560 - DV5K9A0R5S
                                 zdtl_load_dt IN im_load_dt[] AND
                                 zwarn_type IN im_warn_typ[] ).
*** End of Insert TXT25668 - C403761 T456560 - DV5K9A0R5S

  IF li_fuel_evt[] IS INITIAL.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid  = 'ZZ-CN2'
        msgty  = 'I'
        msgno  = '909'.
  ENDIF.

  SORT li_fuel_evt BY zdtl_fevt_id  ASCENDING
                      zdtl_fevt_ver ASCENDING.

  CALL METHOD load_extra_charges
    EXPORTING
      im_fuel_events = li_fuel_evt[].

  LOOP AT li_fuel_evt ASSIGNING <fuel_evt>.
    APPEND INITIAL LINE TO <table> ASSIGNING <line>.
    MOVE-CORRESPONDING <fuel_evt> TO <line>.
    ASSIGN COMPONENT 'ALV_ICON' OF STRUCTURE <line> TO <icon>.
    <icon> = convert_status_to_icon( <fuel_evt>-ztran_stus ).
    IF <icon> = icon_green_light.
      CALL METHOD check_extra_charges
        EXPORTING
          im_lifnr = <fuel_evt>-lifnr
          im_bol   = <fuel_evt>-zbol_tckt_nbr
          im_werks = <fuel_evt>-werks
        CHANGING
          ch_icon  = <icon>.
    ENDIF.
    IF <fuel_evt>-zxchg_wttm_ind = 'Y'.
      ASSIGN COMPONENT 'ALV_ICON_SUB' OF STRUCTURE <line> TO <icon>.
      <icon> = icon_cost_components.
    ENDIF.
    APPEND INITIAL LINE TO li_event_keys ASSIGNING <event_key>.
    <event_key>-zdtl_fevt_id = <fuel_evt>-zdtl_fevt_id.
    <event_key>-zdtl_fevt_ver = <fuel_evt>-zdtl_fevt_ver.
  ENDLOOP.

  IF <table>[] IS INITIAL.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid  = 'ZZ-CN2'
        msgty  = 'I'
        msgno  = '909'.
  ENDIF.

  CALL METHOD zcl_smenh829_error_monitor_dtl=>appl_log_messages
    EXPORTING
      im_tabname           = v_tabname
      im_event_fieldname   = 'ZDTL_FEVT_ID'
      im_version_fieldname = 'ZDTL_FEVT_VER'
      im_event_keys        = li_event_keys[]
    CHANGING
      ch_table             = <table>.

*--------------------------------------------------------------------*
*** Highlight the BOL column
  READ TABLE i_fieldcat ASSIGNING <fcat>
      WITH KEY fieldname = 'ZBOL_TCKT_NBR'.
  IF sy-subrc = 0.
    <fcat>-emphasize = 'C300'.    "highlight in yellow
  ENDIF.

*--------------------------------------------------------------------*
*** Replace Extra charge Y/N with icon
  READ TABLE i_fieldcat ASSIGNING <fcat2>
      WITH KEY fieldname = 'ZXCHG_WTTM_IND'.
  IF sy-subrc = 0.
    l_tabix = sy-tabix.
    READ TABLE i_fieldcat ASSIGNING <fcat>
        WITH KEY fieldname = 'ALV_ICON_SUB'.
    IF sy-subrc = 0.
      CLEAR <fcat>-tech.
      <fcat>-col_pos      = <fcat2>-col_pos.
      <fcat>-seltext_l    = <fcat2>-seltext_l.
      <fcat>-seltext_m    = <fcat2>-seltext_m.
      <fcat>-seltext_s    = <fcat2>-seltext_s.
      <fcat>-reptext_ddic = <fcat2>-reptext_ddic.
      <fcat>-just         = 'C'.
      DELETE i_fieldcat INDEX l_tabix.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
  CALL METHOD zcl_smint666_zmm_dtl_msgs=>build_grid_fcat
    CHANGING
      ch_fcat = v_sbal_profile-mess_fcat[].

  v_sbal_profile_multi-mess_fcat[] = v_sbal_profile-mess_fcat[].
  CALL METHOD zcl_smint666_zmm_dtl_msgs=>build_tree_fcat
    CHANGING
      ch_fcat = v_sbal_profile_multi-lev1_fcat[]
      ch_sort = v_sbal_profile_multi-lev1_sort[].

ENDMETHOD.


METHOD get_bol_selected.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH829_ERROR_MONITOR_DTL->GET_BOL_SELECTED         *
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
* Rob West                  2011/03/01          DV5K961547             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: lo_line                TYPE REF TO data.

  FIELD-SYMBOLS: <line>        TYPE ANY,
                 <sel>         TYPE xfeld,
                 <bol>         TYPE zzbol_ticket_nbr,
                 <table>       TYPE STANDARD TABLE.

  REFRESH ch_bol_table.

  CREATE DATA lo_line TYPE HANDLE o_line_type.
  ASSIGN lo_line->* TO <line>.

  ASSIGN o_data_table->* TO <table>.

  IF im_tabix IS SUPPLIED AND im_tabix > 0.
    IF ch_sel_tabix IS SUPPLIED.
      APPEND im_tabix TO ch_sel_tabix.
    ENDIF.
    READ TABLE <table> INTO <line> INDEX im_tabix.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'ZBOL_TCKT_NBR' OF STRUCTURE <line> TO <bol>.
      APPEND <bol> TO ch_bol_table.
    ENDIF.
  ELSE.
    ASSIGN COMPONENT 'ALV_SEL' OF STRUCTURE <line> TO <sel>.
    ASSIGN COMPONENT 'ZBOL_TCKT_NBR' OF STRUCTURE <line> TO <bol>.
    LOOP AT <table> INTO <line>.
      IF ch_sel_tabix IS SUPPLIED.
        APPEND sy-tabix TO ch_sel_tabix.
      ENDIF.
      IF <sel> = abap_true.
        APPEND <bol> TO ch_bol_table.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD get_log_handle.

  TYPES: BEGIN OF lt_balhdr,
           object              TYPE balhdr-object,
           subobject           TYPE balhdr-subobject,
           extnumber           TYPE balhdr-extnumber,
           alstate             TYPE balhdr-alstate,
           log_handle          TYPE balhdr-log_handle,
         END OF lt_balhdr.

  DATA: li_balhdr            TYPE STANDARD TABLE OF lt_balhdr,
        l_extnumber          TYPE balnrext.


  FIELD-SYMBOLS: <fevt_id>   TYPE zmm_dtl_fuel_evt-zdtl_fevt_id,
                 <fevt_ver>  TYPE zmm_dtl_fuel_evt-zdtl_fevt_ver,
                 <key>       TYPE zsm_smint666_appl_log_key,
                 <balhdr>    TYPE lt_balhdr,
                 <handle>    TYPE balloghndl.

  ASSIGN COMPONENT 'ZDTL_FEVT_ID' OF STRUCTURE im_line TO <fevt_id>.
  IF sy-subrc = 0.
    ASSIGN COMPONENT 'ZDTL_FEVT_VER' OF STRUCTURE im_line TO <fevt_ver>.
    IF sy-subrc = 0.
      ASSIGN l_extnumber TO <key> CASTING.
      CLEAR <key> WITH '_'.
      <key>-tabname = v_tabname.
      <key>-zdtl_fevt_id = <fevt_id>.
      <key>-zdtl_fevt_ver = <fevt_ver>.
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
            SORT li_balhdr BY extnumber+0(45) ASCENDING
                              extnumber+45(6) DESCENDING.
          ENDIF.
          READ TABLE li_balhdr ASSIGNING <balhdr> INDEX 1.
          IF sy-subrc = 0.
            re_log_handle = <balhdr>-log_handle.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD include_in_selection.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH829_ERROR_MONITOR_DTL->INCLUDE_IN_SELECTION     *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-829-001                                           *
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

  FIELD-SYMBOLS: <trnsta>    TYPE zztran_stus.

  re_include = abap_false.

  IF super->include_in_selection( im_selected_line ) = abap_true.
    re_include = abap_true.
  ELSE.
    ASSIGN COMPONENT 'ZTRAN_STUS' OF STRUCTURE im_selected_line TO <trnsta>.
    IF sy-subrc = 0 AND <trnsta> = '03'.
      re_include = abap_true.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD load_extra_charges.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH829_ERROR_MONITOR_DTL->LOAD_EXTRA_CHARGES        *
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
* Rob West                  2011/03/01          DV5K961547             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  IF im_bol_tckt_nbr IS SUPPLIED AND
     im_bol_tckt_nbr IS NOT INITIAL.
    DELETE i_extra_charge_errors WHERE zbol_tckt_nbr = im_bol_tckt_nbr.
    SELECT zdtl_xchg_id
           zdtl_xchg_ver
           lifnr
           zbol_tckt_nbr
           werks
           ztran_stus
      FROM zmm_dtl_xtra_chg
      APPENDING TABLE i_extra_charge_errors
      WHERE zbol_tckt_nbr = im_bol_tckt_nbr AND
            ztran_stus = '04'.
  ELSE.
    IF im_fuel_events[] IS NOT INITIAL.
      SELECT zdtl_xchg_id
             zdtl_xchg_ver
             lifnr
             zbol_tckt_nbr
             werks
             ztran_stus
          FROM zmm_dtl_xtra_chg
          INTO TABLE i_extra_charge_errors
          FOR ALL ENTRIES IN im_fuel_events
          WHERE zbol_tckt_nbr = im_fuel_events-zbol_tckt_nbr AND
                ztran_stus = '04'.
    ENDIF.
  ENDIF.

  IF LINES( i_extra_charge_errors ) < 100.
    v_binary_xtra = abap_false.
  ELSE.
    v_binary_xtra = abap_true.
    SORT i_extra_charge_errors BY lifnr         ASCENDING
                                  zbol_tckt_nbr ASCENDING
                                  werks         ASCENDING.
  ENDIF.

ENDMETHOD.


METHOD message_detail_title.

  DATA: l_event              TYPE c LENGTH 15,
        l_version            TYPE c LENGTH 15.

  FIELD-SYMBOLS: <line>       TYPE zmm_dtl_fuel_evt.

  ASSIGN im_line TO <line> CASTING.
  IF sy-subrc = 0.
    l_event = <line>-zdtl_fevt_id.
    l_version = <line>-zdtl_fevt_ver.
    SHIFT l_event LEFT DELETING LEADING '0'.
    SHIFT l_version LEFT DELETING LEADING '0'.
    CONCATENATE 'Event'(t01) l_event 'Version'(t02) l_version
        INTO ch_grid_title SEPARATED BY space.
  ELSE.
    CLEAR ch_grid_title.
  ENDIF.

ENDMETHOD.


METHOD process_selected_lines.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH829_ERROR_MONITOR_DTL->PROCESS_SELECTED_LINES   *
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
* Rob West                  2011/03/01          DV5K961547             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_reprocess         TYPE STANDARD TABLE OF zmm_dtl_fuel_evt.

  FIELD-SYMBOLS: <dtl>       TYPE zmm_dtl_fuel_evt,
                 <xtra_chg>  TYPE t_xtra_chg.

  li_reprocess[] = im_selected[].
  SORT li_reprocess BY zdtl_fevt_id ASCENDING.

  LOOP AT im_selected ASSIGNING <dtl> CASTING.
    CASE <dtl>-ztran_stus.
      WHEN '03'.
        LOOP AT i_extra_charge_errors ASSIGNING <xtra_chg>
            WHERE zbol_tckt_nbr = <dtl>-zbol_tckt_nbr.
          CALL METHOD zcl_smenh829_error_monitor_xtr=>submit_job
            EXPORTING
              im_xchg_id  = <xtra_chg>-zdtl_xchg_id
              im_xchg_ver = <xtra_chg>-zdtl_xchg_ver
            CHANGING
              ch_joblog   = i_joblog[].
        ENDLOOP.
      WHEN '04'.
        CASE <dtl>-zcanc_ind.
          WHEN 'N'.     "New & Rebill
            IF <dtl>-zdtl_fevt_ver = '1'.     "New
              IF <dtl>-zdtl_mod_cd = 'A' OR <dtl>-zdtl_mod_cd = 'U'.
                CALL METHOD submit_job
                  EXPORTING
                    im_fevt_id  = <dtl>-zdtl_fevt_id
                    im_fevt_ver = <dtl>-zdtl_fevt_ver.
              ENDIF.
            ELSE.                             "rebill
              IF <dtl>-zdtl_mod_cd = 'U'.
                CALL METHOD submit_job
                  EXPORTING
                    im_fevt_id  = <dtl>-zdtl_fevt_id
                    im_fevt_ver = <dtl>-zdtl_fevt_ver.
              ENDIF.
            ENDIF.
          WHEN 'Y'.     "Cancel
            IF <dtl>-zdtl_mod_cd = 'U'.
              CALL METHOD submit_job
                EXPORTING
                  im_fevt_id  = <dtl>-zdtl_fevt_id
                  im_fevt_ver = <dtl>-zdtl_fevt_ver.
            ENDIF.
        ENDCASE.
    ENDCASE.
  ENDLOOP.

ENDMETHOD.


METHOD refresh_data.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH829_ERROR_MONITOR_DTL->REFRESH_DATA             *
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
* Rob West                  2012/04/09          DV5K971100             *
*                                                                      *
* Short Description: CR199440-T207046 & CR199440-T207145               *
*                    Fix Application Log refresh on reprocess          *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961547             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: lo_line              TYPE REF TO data,
        li_event_keys        TYPE zsm_smint666_event_key_table.

  FIELD-SYMBOLS: <line>      TYPE ANY,
                 <tabix>     TYPE sy-tabix,
                 <dtl>       TYPE zmm_dtl_fuel_evt,
                 <icon>      TYPE icon_d,
                 <event_key> TYPE zsm_smint666_event_key,
                 <table>     TYPE STANDARD TABLE.

  CREATE DATA lo_line TYPE HANDLE o_line_type.
  ASSIGN lo_line->* TO <line>.

  ASSIGN <line> TO <dtl> CASTING.
  ASSIGN COMPONENT 'ALV_ICON' OF STRUCTURE <line> TO <icon>.

  ASSIGN o_data_table->* TO <table>.

  LOOP AT im_sel_tabix ASSIGNING <tabix>.
    READ TABLE <table> INTO <line> INDEX <tabix>.
    SELECT SINGLE aedat
                  aezeit
                  aename
                  ztran_stus
      FROM zmm_dtl_fuel_evt
      INTO (<dtl>-aedat,
            <dtl>-aezeit,
            <dtl>-aename,
            <dtl>-ztran_stus)
      WHERE zdtl_fevt_id   = <dtl>-zdtl_fevt_id AND
            zdtl_fevt_ver = <dtl>-zdtl_fevt_ver.
    <icon> = convert_status_to_icon( <dtl>-ztran_stus ).
*    CASE <dtl>-ztran_stus.
*      WHEN '00'.   <icon> = icon_light_out.
*      WHEN '03'.   <icon> = icon_green_light.
*      WHEN '04'.   <icon> = icon_red_light.
*      WHEN OTHERS. <icon> = icon_light_out.
*    ENDCASE.
    CALL METHOD load_extra_charges
      EXPORTING
        im_bol_tckt_nbr = <dtl>-zbol_tckt_nbr.
    IF <icon> = icon_green_light.
      CALL METHOD check_extra_charges
        EXPORTING
          im_lifnr = <dtl>-lifnr
          im_bol   = <dtl>-zbol_tckt_nbr
          im_werks = <dtl>-werks
        CHANGING
          ch_icon  = <icon>.
    ENDIF.
    MODIFY <table> FROM <line> INDEX <tabix>
          TRANSPORTING ('AEDAT')
                       ('AEZEIT')
                       ('AENAME')
                       ('ZTRAN_STUS')
                       ('ALV_ICON').
    APPEND INITIAL LINE TO li_event_keys ASSIGNING <event_key>.
    <event_key>-zdtl_fevt_id = <dtl>-zdtl_fevt_id.
    <event_key>-zdtl_fevt_ver = <dtl>-zdtl_fevt_ver.
  ENDLOOP.

  CALL METHOD zcl_smenh829_error_monitor_dtl=>appl_log_messages
    EXPORTING
      im_tabname           = v_tabname
      im_event_fieldname   = 'ZDTL_FEVT_ID'
      im_version_fieldname = 'ZDTL_FEVT_VER'
      im_event_keys        = li_event_keys[]
      im_in_refresh        = abap_true                     "DV5K971100
    CHANGING
      ch_table             = <table>.
  CALL METHOD zcl_smint666_zmm_dtl_msgs=>free_appl_log.

ENDMETHOD.


METHOD submit_job.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH829_ERROR_MONITOR_DTL=>SUBMIT_JOB               *
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
* Rob West                  2011/03/01          DV5K961547             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_abaplist          TYPE STANDARD TABLE OF abaplist,
        li_joblog            LIKE i_joblog,
        l_joblog             LIKE LINE OF i_joblog.

  IF i_joblog[] IS NOT INITIAL.
    APPEND INITIAL LINE TO i_joblog.
    CLEAR l_joblog WITH '-'.
    APPEND l_joblog TO i_joblog.
  ENDIF.

  CONCATENATE 'Fuel Identifier:'(s01) im_fevt_id
      INTO l_joblog SEPARATED BY space.
  APPEND l_joblog TO i_joblog.
  CONCATENATE 'Fuel Version:'(s02) im_fevt_ver
      INTO l_joblog SEPARATED BY space.
  APPEND l_joblog TO i_joblog.
  APPEND INITIAL LINE TO i_joblog.

  CALL FUNCTION 'LIST_FREE_MEMORY'.

  SUBMIT zme_smenh817_create_po_gr_fuel
    WITH s_fuelid = im_fevt_id
    WITH s_fuelvr = im_fevt_ver
    WITH p_fuelex = abap_true
    EXPORTING LIST TO MEMORY
    AND RETURN.

  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = li_abaplist
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  IF sy-subrc = 0.
    CALL FUNCTION 'LIST_TO_ASCI'
      TABLES
        listasci           = li_joblog
        listobject         = li_abaplist
      EXCEPTIONS
        empty_list         = 1
        list_index_invalid = 2
        OTHERS             = 3.
    IF sy-subrc = 0.
      APPEND LINES OF li_joblog TO i_joblog.
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.
