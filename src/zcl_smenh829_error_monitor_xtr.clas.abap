class ZCL_SMENH829_ERROR_MONITOR_XTR definition
  public
  inheriting from ZCL_SMENH824_ERROR_MONITOR
  final
  create public .

*"* public components of class ZCL_SMENH829_ERROR_MONITOR_XTR
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR
    importing
      !IM_FEVT_ID type ANY TABLE
      !IM_FEVT_VER type ANY TABLE
      !IM_LIFNR type ANY TABLE
      !IM_WERKS type ANY TABLE
      !IM_BOL_TCKT_NBR type ANY TABLE
      !IM_TRAN_STUS type ANY TABLE
      !IM_LOAD_DT type ANY TABLE
      !IM_SELECT_BY_BOL type XFELD optional
      !IM_WITH_TEXT type XFELD optional
    raising
      ZCX_FLCM_ERROR .
  class-methods SUBMIT_JOB
    importing
      !IM_XCHG_ID type ZZDTL_EXTRA_CHRG_ID
      !IM_XCHG_VER type ZZDTL_EXTRA_CHRG_VER
    changing
      !CH_JOBLOG type TCHAR255 optional .
  methods WAS_REPROCESSED
    returning
      value(RE_REPROCESSED) type XFELD .

  methods REFRESH_DATA
    redefinition .
protected section.
*"* protected components of class ZCL_SMENH829_ERROR_MONITOR_XTR
*"* do not include other source files here!!!

  constants C_OBJECT type BALOBJ_D value 'ZSM'. "#EC NOTEXT
  constants C_SUBOBJECT type BALSUBOBJ value 'SM-INT-666'. "#EC NOTEXT

  methods BUILD_EXTNUMBER_PATTERN
    redefinition .
  methods MESSAGE_DETAIL_TITLE
    redefinition .
  methods PROCESS_SELECTED_LINES
    redefinition .
  methods GET_LOG_HANDLE
    redefinition .
private section.
*"* private components of class ZCL_SMENH829_ERROR_MONITOR_XTR
*"* do not include other source files here!!!

  data V_REPROCESSED type XFELD .
ENDCLASS.



CLASS ZCL_SMENH829_ERROR_MONITOR_XTR IMPLEMENTATION.


METHOD build_extnumber_pattern.

  CLEAR ch_extnumber+45(6) WITH '_'.

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
* Rob West                  2011/03/01          DV5K961547             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_xtra_chg          TYPE STANDARD TABLE OF zmm_dtl_xtra_chg,
        li_event_keys        TYPE zsm_smint666_event_key_table.

  FIELD-SYMBOLS: <xtra_chg>  TYPE zmm_dtl_xtra_chg,
                 <icon>      TYPE icon_d,
                 <line>      TYPE ANY,
                 <event_key> TYPE zsm_smint666_event_key,
                 <table>     TYPE STANDARD TABLE,
                 <fcat>      TYPE slis_fieldcat_alv.


  CALL METHOD super->constructor
    EXPORTING
      im_tabname   = 'ZMM_DTL_XTRA_CHG'
      im_with_text = im_with_text.

  ASSIGN o_data_table->* TO <table>.

  IF im_select_by_bol = abap_true.
    SELECT *
      FROM zmm_dtl_xtra_chg
      INTO TABLE li_xtra_chg
      WHERE zbol_tckt_nbr IN im_bol_tckt_nbr[].
  ELSE.
    IF im_fevt_id[] IS INITIAL.
      SELECT *
        FROM zmm_dtl_xtra_chg
        INTO TABLE li_xtra_chg
        WHERE ztran_stus IN im_tran_stus[].
    ELSE.
      SELECT *
        FROM zmm_dtl_xtra_chg
        INTO TABLE li_xtra_chg
        WHERE zdtl_xchg_id IN im_fevt_id[].
    ENDIF.
  ENDIF.

  SORT li_xtra_chg BY zdtl_xchg_id  ASCENDING
                      zdtl_xchg_ver ASCENDING.

  LOOP AT li_xtra_chg ASSIGNING <xtra_chg>
      WHERE ( zdtl_xchg_id IN im_fevt_id[] AND
              zdtl_xchg_ver IN im_fevt_ver[] AND
              lifnr IN im_lifnr[] AND
              werks IN im_werks[] AND
              zbol_tckt_nbr IN im_bol_tckt_nbr[] AND
              zdtl_load_dt IN im_load_dt[] AND
              ztran_stus IN im_tran_stus[] ).
    APPEND INITIAL LINE TO <table> ASSIGNING <line>.
    MOVE-CORRESPONDING <xtra_chg> TO <line>.
    ASSIGN COMPONENT 'ALV_ICON' OF STRUCTURE <line> TO <icon>.
    <icon> = convert_status_to_icon( <xtra_chg>-ztran_stus ).
    APPEND INITIAL LINE TO li_event_keys ASSIGNING <event_key>.
    <event_key>-zdtl_fevt_id = <xtra_chg>-zdtl_xchg_id.
    <event_key>-zdtl_fevt_ver = <xtra_chg>-zdtl_xchg_ver.
  ENDLOOP.

  IF <table>[] IS INITIAL.
    RAISE EXCEPTION TYPE zcx_flcm_error                     "#EC *
      EXPORTING
        msgid  = 'ZZ_FLCM'
        msgty  = 'I'
        msgno  = '053'.
    MESSAGE i053(zz_flcm).        "For «Where Used» only
  ENDIF.

  CALL METHOD zcl_smenh829_error_monitor_dtl=>appl_log_messages
    EXPORTING
      im_tabname           = v_tabname
      im_event_fieldname   = 'ZDTL_XCHG_ID'
      im_version_fieldname = 'ZDTL_XCHG_VER'
      im_event_keys        = li_event_keys[]
    CHANGING
      ch_table             = <table>.

*--------------------------------------------------------------------*
  READ TABLE i_fieldcat ASSIGNING <fcat>
      WITH KEY fieldname = 'ZBOL_TCKT_NBR'.
  IF sy-subrc = 0.
    <fcat>-emphasize = 'C300'.    "highlight in yellow
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


  FIELD-SYMBOLS: <xchg_id>   TYPE zmm_dtl_xtra_chg-zdtl_xchg_id,
                 <xchg_ver>  TYPE zmm_dtl_xtra_chg-zdtl_xchg_ver,
                 <key>       TYPE zsm_smint666_appl_log_key,
                 <balhdr>    TYPE lt_balhdr,
                 <handle>    TYPE balloghndl.

  ASSIGN COMPONENT 'ZDTL_XCHG_ID' OF STRUCTURE im_line TO <xchg_id>.
  IF sy-subrc = 0.
    ASSIGN COMPONENT 'ZDTL_XCHG_VER' OF STRUCTURE im_line TO <xchg_ver>.
    IF sy-subrc = 0.
      ASSIGN l_extnumber TO <key> CASTING.
      CLEAR <key> WITH '_'.
      <key>-tabname = v_tabname.
      <key>-zdtl_fevt_id = <xchg_id>.
      <key>-zdtl_fevt_ver = <xchg_ver>.
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


METHOD message_detail_title.

  DATA: l_event              TYPE c LENGTH 15,
        l_version            TYPE c LENGTH 15.

  FIELD-SYMBOLS: <line>       TYPE zmm_dtl_xtra_chg.

  ASSIGN im_line TO <line> CASTING.
  IF sy-subrc = 0.
    l_event = <line>-zdtl_xchg_id.
    l_version = <line>-zdtl_xchg_ver.
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

  DATA: li_reprocess         TYPE STANDARD TABLE OF zmm_dtl_xtra_chg.

  FIELD-SYMBOLS: <dtl>       TYPE zmm_dtl_xtra_chg.

  li_reprocess[] = im_selected[].
  SORT li_reprocess BY zdtl_xchg_id ASCENDING.

  LOOP AT im_selected ASSIGNING <dtl> CASTING.
    CASE <dtl>-zcanc_ind.
      WHEN 'N'.     "New & Rebill
        IF <dtl>-zdtl_xchg_ver = '1'.     "New
          IF <dtl>-zdtl_mod_cd = 'A' OR <dtl>-zdtl_mod_cd = 'U'.
            v_reprocessed = abap_true.
            CALL METHOD submit_job
              EXPORTING
                im_xchg_id  = <dtl>-zdtl_xchg_id
                im_xchg_ver = <dtl>-zdtl_xchg_ver.
          ENDIF.
        ELSE.                             "rebill
          IF <dtl>-zdtl_mod_cd = 'U'.
            v_reprocessed = abap_true.
            CALL METHOD submit_job
              EXPORTING
                im_xchg_id  = <dtl>-zdtl_xchg_id
                im_xchg_ver = <dtl>-zdtl_xchg_ver.
          ENDIF.
        ENDIF.
      WHEN 'Y'.     "Cancel
        IF <dtl>-zdtl_mod_cd = 'U'.
          v_reprocessed = abap_true.
          CALL METHOD submit_job
            EXPORTING
              im_xchg_id  = <dtl>-zdtl_xchg_id
              im_xchg_ver = <dtl>-zdtl_xchg_ver.
        ENDIF.
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
                 <xtra_chg>  TYPE zmm_dtl_xtra_chg,
                 <icon>      TYPE icon_d,
                 <event_key> TYPE zsm_smint666_event_key,
                 <table>     TYPE STANDARD TABLE.

  CREATE DATA lo_line TYPE HANDLE o_line_type.
  ASSIGN lo_line->* TO <line>.

  ASSIGN <line> TO <xtra_chg> CASTING.
  ASSIGN COMPONENT 'ALV_ICON' OF STRUCTURE <line> TO <icon>.

  ASSIGN o_data_table->* TO <table>.

  LOOP AT im_sel_tabix ASSIGNING <tabix>.
    READ TABLE <table> INTO <line> INDEX <tabix>.
    SELECT SINGLE aedat
                  aezeit
                  aename
                  ztran_stus
      FROM zmm_dtl_xtra_chg
      INTO (<xtra_chg>-aedat,
            <xtra_chg>-aezeit,
            <xtra_chg>-aename,
            <xtra_chg>-ztran_stus)
      WHERE zdtl_xchg_id   = <xtra_chg>-zdtl_xchg_id AND
            zdtl_xchg_ver  = <xtra_chg>-zdtl_xchg_ver.
    <icon> = convert_status_to_icon( <xtra_chg>-ztran_stus ).
*    CASE <xtra_chg>-ztran_stus.
*      WHEN '03'.   <icon> = icon_green_light.
*      WHEN '04'.   <icon> = icon_red_light.
*      WHEN OTHERS. <icon> = icon_light_out.
*    ENDCASE.
    MODIFY <table> FROM <line> INDEX <tabix>
          TRANSPORTING ('AEDAT')
                       ('AEZEIT')
                       ('AENAME')
                       ('ZTRAN_STUS')
                       ('ALV_ICON').
    APPEND INITIAL LINE TO li_event_keys ASSIGNING <event_key>.
    <event_key>-zdtl_fevt_id = <xtra_chg>-zdtl_xchg_id.
    <event_key>-zdtl_fevt_ver = <xtra_chg>-zdtl_xchg_ver.
  ENDLOOP.

  CALL METHOD zcl_smenh829_error_monitor_dtl=>appl_log_messages
    EXPORTING
      im_tabname           = v_tabname
      im_event_fieldname   = 'ZDTL_XCHG_ID'
      im_version_fieldname = 'ZDTL_XCHG_VER'
      im_event_keys        = li_event_keys[]
      im_in_refresh        = abap_true                     "DV5K971100
    CHANGING
      ch_table             = <table>.

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

  FIELD-SYMBOLS: <joblog>    TYPE tchar255.

  IF ch_joblog IS SUPPLIED.
    ASSIGN ch_joblog TO <joblog>.       "use external joblog
  ELSE.
    ASSIGN i_joblog TO <joblog>.        "use internal joblog
  ENDIF.

  IF <joblog>[] IS NOT INITIAL.
    APPEND INITIAL LINE TO <joblog>.
    CLEAR l_joblog WITH '-'.
    APPEND l_joblog TO <joblog>.
  ENDIF.

  CONCATENATE 'Extra Charge Identifier:'(s01) im_xchg_id
      INTO l_joblog SEPARATED BY space.
  APPEND l_joblog TO <joblog>.
  CONCATENATE 'Extra Charge Version:'(s02) im_xchg_ver
      INTO l_joblog SEPARATED BY space.
  APPEND l_joblog TO <joblog>.
  APPEND INITIAL LINE TO <joblog>.

  CALL FUNCTION 'LIST_FREE_MEMORY'.

  SUBMIT zme_smenh817_create_po_gr_fuel
    WITH s_chrgid = im_xchg_id
    WITH s_chrgvr = im_xchg_ver
    WITH p_extra  = abap_true
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
      APPEND LINES OF li_joblog TO <joblog>.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD was_reprocessed.

  re_reprocessed = v_reprocessed.

ENDMETHOD.
ENDCLASS.
