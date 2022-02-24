class ZCL_SMINT666_ZMM_DTL_MSGS definition
  public
  final
  create public .

*"* public components of class ZCL_SMINT666_ZMM_DTL_MSGS
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR
    importing
      !IM_TABNAME type TABNAME
      !IM_EVENT_ID type ZZDTL_FUEL_EVENT_ID
      !IM_VERSION type ZZDTL_FUEL_EVENT_VER
    exceptions
      APPL_LOG_ERROR .
  methods REPORT_ERRORS
    changing
      !CH_TEXT type STANDARD TABLE .
  class-methods LOG_ALL_MESSAGES
    importing
      !IM_TABNAME type TABNAME
      !IM_EVENT_ID type ZZDTL_FUEL_EVENT_ID
      !IM_VERSION type ZZDTL_FUEL_EVENT_VER
      !IM_MESSAGES type BAPIRET2_TAB
    exceptions
      ERROR_IN_SAVE
      APPL_LOG_ERROR .
  class-methods GET_ALL_MESSAGES
    importing
      !IM_TABNAME type TABNAME
      !IM_EVENT_KEYS type ZSM_SMINT666_EVENT_KEY_TABLE
      !IM_MOST_RECENT type XFELD optional
      !IM_FREE_ALL_LOGS type XFELD default 'X'
    changing
      !CH_MESSAGES type ZSM_SMINT666_MESSAGES_TABLE .
  class-methods SHOW_MESSAGES_IN_POPUP
    importing
      !IM_EVENT_ID type ZZDTL_FUEL_EVENT_ID
      !IM_VERSION type ZZDTL_FUEL_EVENT_VER
    exceptions
      NO_MESSAGES .
  class-methods DELETE_MESSAGES
    importing
      !IM_TABNAME type TABNAME
      !IM_EVENT_KEYS type ZSM_SMINT666_EVENT_KEY_TABLE
      !IM_COMMIT type XFELD optional
    exceptions
      NO_LOGS_SPECIFIED .
  class-methods BUILD_GRID_FCAT
    changing
      !CH_FCAT type BAL_T_FCAT .
  class-methods FREE_APPL_LOG .
  class-methods BUILD_TREE_FCAT
    changing
      !CH_FCAT type BAL_T_FCAT
      !CH_SORT type BAL_T_SORT .
protected section.
*"* protected components of class ZCL_SMINT60A_ZMM_FLCM_TDS
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_SMINT666_ZMM_DTL_MSGS
*"* do not include other source files here!!!

  types:
    BEGIN OF t_object_ref,
               appl_log_key  TYPE ZSM_SMINT666_APPL_LOG_KEY,
               object        TYPE REF TO zcl_smint666_zmm_dtl_msgs,
    END OF t_object_ref .
  types:
    t_object_ref_table       TYPE STANDARD TABLE OF t_object_ref
                                  WITH DEFAULT KEY .

  constants C_KEEP_ERRORS type I value 365. "#EC NOTEXT
  constants C_KEEP_WARNINGS type I value 90. "#EC NOTEXT
  data V_APPL_LOG_KEY type ZSM_SMINT666_APPL_LOG_KEY .
  data V_TABNAME type TABNAME .
  data V_UPDATES_MADE type XFELD .
  data V_ERROR_COUNT type I .
  data V_LOG_HANDLE type BALLOGHNDL .
  data O_DTL_RECORD type ref to DATA .
  class-data I_OBJECT_REF type T_OBJECT_REF_TABLE .
  class-data I_LOG_HANDLES type BAL_T_LOGH .
  class-data I_LOG_HEADERS type BALHDR_T .

  methods LOG_ERROR
    importing
      !IM_MSGID type SYMSGID
      !IM_MSGNO type SYMSGNO
      !IM_MSGTY type SYMSGTY optional
      !IM_MSGV1 type C optional
      !IM_MSGV2 type C optional
      !IM_MSGV3 type C optional
      !IM_MSGV4 type C optional .
  methods SAVE_DATA
    importing
      !IM_COMMIT type XFELD optional
    exceptions
      DB_ERROR
      SBAL_ERROR .
ENDCLASS.



CLASS ZCL_SMINT666_ZMM_DTL_MSGS IMPLEMENTATION.


METHOD build_grid_fcat.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT666_ZMM_DTL_MSGS=>BUILD_GRID_FCAT               *
* Title    :  Application Log Messaging                                *
* Work Unit:  SM-INT-666                                               *
* Created by: Rob West                                                 *
* Created on: December 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose  :  Manage Application Log messages related to tables        *
*             ZMM_DTL_FUEL_EVT and ZMM_DTL_FUEL_XTR                    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K963219             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: l_fcat                TYPE bal_s_fcat.

  STATICS: sti_fcat           TYPE bal_t_fcat.

*** Add columns to right-hand panel - displayed as default

  IF sti_fcat[] IS INITIAL.
    CLEAR l_fcat.
    l_fcat-ref_table = 'BAL_S_SHOW'.
    l_fcat-ref_field = 'T_MSG'.
    l_fcat-outputlen = '70'.
    APPEND l_fcat TO sti_fcat.
  ENDIF.

  ch_fcat[] = sti_fcat[].

ENDMETHOD.


METHOD build_tree_fcat.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT666_ZMM_DTL_MSGS=>BUILD_TREE_FCAT               *
* Title    :  Application Log Messaging                                *
* Work Unit:  SM-INT-666                                               *
* Created by: Rob West                                                 *
* Created on: December 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose  :  Manage Application Log messages related to tables        *
*             ZMM_DTL_FUEL_EVT and ZMM_DTL_FUEL_XTR                    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K963219             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: l_fcat               TYPE bal_s_fcat,
        l_sort               TYPE bal_s_sort.

  STATICS: sti_fcat          TYPE bal_t_fcat,
           sti_sort          TYPE bal_t_sort.

**** Build the tree structure
  IF sti_fcat[] IS INITIAL.
    CLEAR l_fcat.
    l_fcat-ref_table = 'BAL_S_SHOW'.
    l_fcat-ref_field = 'ALDATE'.
    APPEND l_fcat TO sti_fcat.

    CLEAR l_fcat.
    l_fcat-ref_table = 'BAL_S_SHOW'.
    l_fcat-ref_field = 'ALTIME'.
    APPEND l_fcat TO sti_fcat.

    CLEAR l_fcat.
    l_fcat-ref_table = 'BAL_S_SHOW'.
    l_fcat-ref_field = 'LOGNUMBER'.
    l_fcat-tech = 'X'.
    APPEND l_fcat TO sti_fcat.

    CLEAR l_fcat.
    l_fcat-ref_table = 'BAL_S_SHOW'.
    l_fcat-ref_field = 'LOG_HANDLE'.
    l_fcat-tech = 'X'.
    APPEND l_fcat TO sti_fcat.
  ENDIF.

*** Set the date sort sequence to DESCENDING
  IF sti_sort[] IS INITIAL.
    l_sort-ref_table = 'BAL_S_SHOW'.
    l_sort-ref_field = 'LOGNUMBER'.
    l_sort-spos = 1.
    l_sort-down = 'X'.
    APPEND l_sort TO sti_sort.
  ENDIF.

  ch_fcat = sti_fcat[].
  ch_sort = sti_sort[].

ENDMETHOD.


METHOD constructor.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT666_ZMM_DTL_MSGS=>CONSTRUCTOR                   *
* Title    :  Application Log Messaging                                *
* Work Unit:  SM-INT-666                                               *
* Created by: Rob West                                                 *
* Created on: December 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose  :  Manage Application Log messages related to tables        *
*             ZMM_DTL_FUEL_EVT and ZMM_DTL_FUEL_XTR                    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K963219             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: l_log                TYPE bal_s_log,
        l_extnumber          TYPE balnrext,
        l_object_ref         TYPE t_object_ref,
        li_headers           TYPE STANDARD TABLE OF balnrext.

  FIELD-SYMBOLS: <key>       TYPE zsm_smint666_appl_log_key.

  v_tabname = im_tabname.
  TRANSLATE v_tabname TO UPPER CASE.

  ASSIGN l_extnumber TO <key> CASTING.
  CLEAR <key> WITH '_'.

  v_appl_log_key-tabname = <key>-tabname = v_tabname.
  v_appl_log_key-zdtl_fevt_id = <key>-zdtl_fevt_id = im_event_id.
  v_appl_log_key-zdtl_fevt_ver = <key>-zdtl_fevt_ver = im_version.

  SELECT extnumber
    FROM balhdr
    INTO TABLE li_headers
    WHERE object    = 'ZSM' AND
          subobject = 'SM-INT-666' AND
          extnumber LIKE l_extnumber.
  IF sy-subrc = 0.
    SORT li_headers DESCENDING.
    READ TABLE li_headers INTO l_extnumber INDEX 1.
  ELSE.
    CLEAR <key>-seqno.
  ENDIF.

  LOOP AT i_object_ref TRANSPORTING NO FIELDS
      WHERE appl_log_key-zdtl_fevt_id = im_event_id AND
            appl_log_key-zdtl_fevt_ver = im_version AND
            object <> me.
    ADD 1 TO <key>-seqno.
  ENDLOOP.

  ADD 1 TO <key>-seqno.

  l_log-object = 'ZSM'.
  l_log-subobject = 'SM-INT-666'.
  l_log-extnumber = l_extnumber.
  l_log-aldate_del = sy-datum + c_keep_errors.
  l_log-aldate = sy-datum.
  l_log-altime = sy-uzeit.
  l_log-alprog = sy-cprog.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = l_log
    IMPORTING
      e_log_handle            = v_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.
  IF sy-subrc > 0.
    RAISE appl_log_error.
  ENDIF.

  APPEND v_log_handle TO i_log_handles.

  l_object_ref-appl_log_key = <key>.
  l_object_ref-object = me.     "self reference
  APPEND l_object_ref TO i_object_ref.

ENDMETHOD.


METHOD delete_messages.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT666_ZMM_DTL_MSGS=>DELETE_MESSAGES               *
* Title    :  Application Log Messaging                                *
* Work Unit:  SM-INT-666                                               *
* Created by: Rob West                                                 *
* Created on: December 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose  :  Manage Application Log messages related to tables        *
*             ZMM_DTL_FUEL_EVT and ZMM_DTL_FUEL_XTR                    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K963219             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: l_object             TYPE bal_s_obj,
        l_subobject          TYPE bal_s_sub,
        l_extnumber          TYPE bal_s_extn,
        l_filter             TYPE bal_s_lfil,

        li_log_headers       TYPE balhdr_t.

  FIELD-SYMBOLS: <ref>       TYPE zsm_smint666_event_key,
                 <key>       TYPE ZSM_SMINT666_APPL_LOG_KEY.

** Set up filters
  l_object-sign = 'I'.
  l_object-option = 'EQ'.
  l_object-low = 'ZSM'.
  APPEND l_object TO l_filter-object.
  l_subobject-sign = 'I'.
  l_subobject-option = 'EQ'.
  l_subobject-low ='SM-INT-666'.
  APPEND l_subobject TO l_filter-subobject.
  l_extnumber-sign = 'I'.
  l_extnumber-option = 'CP'.
  ASSIGN l_extnumber-low TO <key> CASTING.
  CLEAR <key> WITH '+'.
  <key>-tabname = im_tabname.
  TRANSLATE <key>-tabname TO UPPER CASE.
  LOOP AT im_event_keys ASSIGNING <ref>.
    <key>-zdtl_fevt_id = <ref>-zdtl_fevt_id.
    <key>-zdtl_fevt_ver  = <ref>-zdtl_fevt_ver.
    APPEND l_extnumber TO l_filter-extnumber.
  ENDLOOP.

** Retrieve logs from Application Log DB
  CALL FUNCTION 'BAL_DB_SEARCH'
    EXPORTING
      i_s_log_filter = l_filter
    IMPORTING
      e_t_log_header = li_log_headers[]
    EXCEPTIONS
      log_not_found      = 1
      no_filter_criteria = 2
      OTHERS             = 3.

  IF sy-subrc > 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BAL_DB_DELETE'
    EXPORTING
      i_t_logs_to_delete = li_log_headers[]
      i_with_commit_work = im_commit
    EXCEPTIONS
      no_logs_specified  = 1
      OTHERS             = 2.

  IF sy-subrc > 0.
    RAISE no_logs_specified.
  ENDIF.

ENDMETHOD.


METHOD free_appl_log.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT666_ZMM_DTL_MSGS=>FREE_APPL_LOG                 *
* Title    :  Application Log Messaging                                *
* Work Unit:  SM-INT-666                                               *
* Created by: Rob West                                                 *
* Created on: December 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose  :  Manage Application Log messages related to tables        *
*             ZMM_DTL_FUEL_EVT and ZMM_DTL_FUEL_XTR                    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K963219             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  IF i_log_handles[] IS NOT INITIAL.
    CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'
      EXPORTING
        i_t_logs_to_be_refreshed = i_log_handles[]
        i_refresh_all            = abap_false.
    REFRESH: i_log_handles,
             i_log_headers.
  ENDIF.

ENDMETHOD.


METHOD get_all_messages.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT666_ZMM_DTL_MSGS=>GET_ALL_MESSAGES              *
* Title    :  Application Log Messaging                                *
* Work Unit:  SM-INT-666                                               *
* Created by: Rob West                                                 *
* Created on: December 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose  :  Manage Application Log messages related to tables        *
*             ZMM_DTL_FUEL_EVT and ZMM_DTL_FUEL_XTR                    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K963219             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: l_message            TYPE zsm_smint666_messages,
        l_msg                TYPE bal_s_msg,
        l_symsg              TYPE symsg,
        l_msg_hndl           TYPE balmsghndl,
        l_key                TYPE balnrext,
        l_tabix              TYPE sy-tabix,
        li_event_keys        TYPE zsm_smint666_event_key_table,

        l_event              TYPE zzdtl_fuel_event_id,
        l_version            TYPE zzdtl_fuel_event_ver,

        li_log_headers       TYPE balhdr_t,
        li_log_handles       TYPE bal_t_logh,

        li_work_headers      TYPE STANDARD TABLE OF balhdr,
        li_work_handles      TYPE STANDARD TABLE OF balloghndl.

  FIELD-SYMBOLS: <ref>       TYPE zsm_smint666_event_key,
                 <key>       TYPE zsm_smint666_appl_log_key,
                 <hdr>       TYPE balhdr.

  REFRESH ch_messages.

  IF im_free_all_logs = abap_true AND i_log_handles[] IS NOT INITIAL.
    CALL METHOD free_appl_log.
  ENDIF.

*** Retrieve logs from Application Log DB
  ASSIGN l_key TO <key> CASTING.
  CLEAR <key> WITH '_'.
  <key>-tabname = im_tabname.

  SELECT *
    FROM balhdr
    INTO TABLE li_work_headers
    WHERE object = 'ZSM' AND
          subobject = 'SM-INT-666' AND
          extnumber LIKE l_key.

  IF sy-subrc > 0.
    RETURN.
  ENDIF.

  SORT li_work_headers BY extnumber+0(45) ASCENDING    "Table/Event ID/Version
                          extnumber+45(6) DESCENDING.  "Sequence No.

  IF im_most_recent = abap_true.
    DELETE ADJACENT DUPLICATES FROM li_work_headers
           COMPARING extnumber+0(45).                  "Table/Event ID/Version
  ENDIF.

  li_event_keys[] = im_event_keys[].
  SORT li_event_keys ASCENDING.
  SORT li_work_headers BY extnumber ASCENDING.

  LOOP AT li_event_keys ASSIGNING <ref>.
    LOOP AT li_work_headers ASSIGNING <hdr> FROM l_tabix.
      IF <hdr>-extnumber+30(15) = <ref>.  "Event ID/Version
        l_tabix = sy-tabix + 1.
      ELSEIF <hdr>-extnumber+30(15) < <ref>.    "Event ID/Version
        DELETE li_work_headers INDEX sy-tabix.
      ELSE.
        l_tabix = sy-tabix.
        EXIT.
      ENDIF.
*      IF <hdr>-extnumber+30(15) < <ref>.    "Event ID/Version
*        DELETE li_work_headers INDEX sy-tabix.
*      ELSE.
*        l_tabix = sy-tabix.
*        IF <hdr>-extnumber+30(15) = <ref>.  "Event ID/Version
*          ADD 1 TO l_tabix.
*        ENDIF.
*        EXIT.
*      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF l_tabix > 0.
    DELETE li_work_headers FROM l_tabix.
  ENDIF.
  SORT li_work_headers BY mandant   ASCENDING
                         lognumber ASCENDING.
  li_log_headers[] = li_work_headers[].

*** Load log details from Application Log DB
  CALL FUNCTION 'BAL_DB_LOAD'
    EXPORTING
      i_t_log_header     = li_log_headers[]
    IMPORTING
      e_t_log_handle     = li_log_handles[]
    EXCEPTIONS
      no_logs_specified  = 1
      log_not_found      = 2
      log_already_loaded = 3
      OTHERS             = 4.

  LOOP AT li_log_headers ASSIGNING <hdr>.
    CLEAR l_message.
    ASSIGN <hdr>-extnumber TO <key> CASTING.
    l_message-zdtl_fevt_id = <key>-zdtl_fevt_id.
    l_message-zdtl_fevt_ver = <key>-zdtl_fevt_ver.
    l_message-log_handle = <hdr>-log_handle.
    l_message-msg_cnt_al = <hdr>-msg_cnt_al.
    l_message-msg_cnt_a  = <hdr>-msg_cnt_a.
    l_message-msg_cnt_e  = <hdr>-msg_cnt_e.
    l_message-msg_cnt_w  = <hdr>-msg_cnt_w.
    l_message-msg_cnt_i  = <hdr>-msg_cnt_i.
    l_message-msg_cnt_s  = <hdr>-msg_cnt_s.
    CLEAR l_msg_hndl.
    l_msg_hndl-log_handle = <hdr>-log_handle.
    WHILE l_msg_hndl-msgnumber < <hdr>-msg_cnt_al.
      ADD 1 TO l_msg_hndl-msgnumber.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = l_msg_hndl
        IMPORTING
          e_s_msg        = l_msg
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 2
          OTHERS         = 3.
      IF sy-subrc = 0.
        l_symsg-msgty = l_msg-msgty.
        l_symsg-msgid = l_msg-msgid.
        l_symsg-msgno = l_msg-msgno.
        l_symsg-msgv1 = l_msg-msgv1.
        l_symsg-msgv2 = l_msg-msgv2.
        l_symsg-msgv3 = l_msg-msgv3.
        l_symsg-msgv4 = l_msg-msgv4.
        APPEND l_symsg TO l_message-messages.
      ENDIF.
    ENDWHILE.
    APPEND l_message TO ch_messages.
  ENDLOOP.

  SORT ch_messages BY zdtl_fevt_id  ASCENDING
                      zdtl_fevt_ver ASCENDING.

  REFRESH li_work_headers.
  APPEND LINES OF li_log_headers TO li_work_headers.
  APPEND LINES OF i_log_headers TO li_work_headers.
  SORT li_work_headers BY mandant   ASCENDING
                          lognumber ASCENDING.
  DELETE ADJACENT DUPLICATES FROM li_work_headers
                  COMPARING mandant
                            lognumber.
  MOVE li_work_headers[] TO i_log_headers[].

  REFRESH li_work_handles.
  APPEND LINES OF i_log_handles TO li_work_handles.
  APPEND LINES OF li_log_handles TO li_work_handles.
  SORT li_work_handles ASCENDING.
  DELETE ADJACENT DUPLICATES FROM li_work_handles.
  MOVE li_work_handles[] TO i_log_handles[].

ENDMETHOD.


METHOD log_all_messages.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT666_ZMM_DTL_MSGS=>LOG_ALL_MESSAGES              *
* Title    :  Application Log Messaging                                *
* Work Unit:  SM-INT-666                                               *
* Created by: Rob West                                                 *
* Created on: December 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose  :  Manage Application Log messages related to tables        *
*             ZMM_DTL_FUEL_EVT and ZMM_DTL_FUEL_XTR                    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K963219             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: lo_object            TYPE REF TO zcl_smint666_zmm_dtl_msgs.

  FIELD-SYMBOLS: <bapiret2>  TYPE bapiret2.

  CREATE OBJECT lo_object
    EXPORTING
      im_tabname     = im_tabname
      im_event_id    = im_event_id
      im_version     = im_version
    EXCEPTIONS
      appl_log_error = 1
      OTHERS         = 2.

  IF sy-subrc > 0.
    RAISE appl_log_error.
  ENDIF.

  LOOP AT im_messages ASSIGNING <bapiret2>.
    CALL METHOD lo_object->log_error
      EXPORTING
        im_msgid = <bapiret2>-id
        im_msgno = <bapiret2>-number
        im_msgty = <bapiret2>-type
        im_msgv1 = <bapiret2>-message_v1
        im_msgv2 = <bapiret2>-message_v2
        im_msgv3 = <bapiret2>-message_v3
        im_msgv4 = <bapiret2>-message_v4.
  ENDLOOP.

  CALL METHOD lo_object->save_data
    EXPORTING
      im_commit  = 'X'
    EXCEPTIONS
      db_error   = 1
      sbal_error = 2
      OTHERS     = 3.
  IF sy-subrc > 0.
    RAISE error_in_save.
  ENDIF.

ENDMETHOD.


METHOD log_error.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT666_ZMM_DTL_MSGS=>LOG_ERROR                     *
* Title    :  Application Log Messaging                                *
* Work Unit:  SM-INT-666                                               *
* Created by: Rob West                                                 *
* Created on: December 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose  :  Manage Application Log messages related to tables        *
*             ZMM_DTL_FUEL_EVT and ZMM_DTL_FUEL_XTR                    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K963219             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: l_msg              TYPE bal_s_msg.

  l_msg-msgid = im_msgid.
  l_msg-msgno = im_msgno.

  IF im_msgty IS SUPPLIED.
    l_msg-msgty = im_msgty.
  ELSE.
    l_msg-msgty = 'I'.
  ENDIF.

  IF im_msgv1 IS SUPPLIED.
    l_msg-msgv1 = im_msgv1.
  ENDIF.

  IF im_msgv2 IS SUPPLIED.
    l_msg-msgv2 = im_msgv2.
  ENDIF.

  IF im_msgv3 IS SUPPLIED.
    l_msg-msgv3 = im_msgv3.
  ENDIF.

  IF im_msgv4 IS SUPPLIED.
    l_msg-msgv4 = im_msgv4.
  ENDIF.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = v_log_handle
      i_s_msg      = l_msg.

  ADD 1 TO v_error_count.

ENDMETHOD.


METHOD report_errors.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT666_ZMM_DTL_MSGS=>REPORT_ERRORS                 *
* Title    :  Application Log Messaging                                *
* Work Unit:  SM-INT-666                                               *
* Created by: Rob West                                                 *
* Created on: December 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose  :  Manage Application Log messages related to tables        *
*             ZMM_DTL_FUEL_EVT and ZMM_DTL_FUEL_XTR                    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K963219             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: l_text             TYPE tdline,
        l_msg              TYPE bal_s_msg,
        l_msg_handle       TYPE balmsghndl.

  IF v_error_count = 0.
    RETURN.
  ENDIF.

  IF ch_text[] IS NOT INITIAL.
    CLEAR l_text WITH '-'.
    APPEND l_text TO ch_text.
  ENDIF.

  CONCATENATE v_appl_log_key-zdtl_fevt_id
              v_appl_log_key-zdtl_fevt_ver
      INTO l_text SEPARATED BY space.
  APPEND l_text TO ch_text.

  APPEND INITIAL LINE TO ch_text.

  l_msg_handle-log_handle = v_log_handle.
  WHILE l_msg_handle-msgnumber < v_error_count.
    ADD 1 TO l_msg_handle-msgnumber.
    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = l_msg_handle
      IMPORTING
        e_s_msg        = l_msg
      EXCEPTIONS
        log_not_found  = 1
        msg_not_found  = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      MESSAGE
        ID     l_msg-msgid
        TYPE   l_msg-msgty
        NUMBER l_msg-msgno
        WITH   l_msg-msgv1
               l_msg-msgv2
               l_msg-msgv3
               l_msg-msgv4
        INTO l_text.
      APPEND l_text TO ch_text.
    ENDIF.
  ENDWHILE.

ENDMETHOD.


METHOD save_data.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT666_ZMM_DTL_MSGS=>SAVE_DATA                     *
* Title    :  Application Log Messaging                                *
* Work Unit:  SM-INT-666                                               *
* Created by: Rob West                                                 *
* Created on: December 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose  :  Manage Application Log messages related to tables        *
*             ZMM_DTL_FUEL_EVT and ZMM_DTL_FUEL_XTR                    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K963219             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: l_in_update_task     TYPE xfeld,
        l_log                TYPE bal_s_log,
        l_stats              TYPE bal_s_scnt,
        li_log_handles       TYPE bal_t_logh.


  IF v_log_handle IS INITIAL.
    RETURN.             "No errors; no Application Log
  ENDIF.

*** Populate the key data into the Application Log header
  CALL FUNCTION 'BAL_LOG_HDR_READ'
    EXPORTING
      i_log_handle  = v_log_handle
    IMPORTING
      e_s_log       = l_log
      e_statistics  = l_stats
    EXCEPTIONS
      log_not_found = 1
      OTHERS        = 2.
  IF sy-subrc = 0.
    IF l_stats-msg_cnt_a = 0 AND
       l_stats-msg_cnt_e = 0.
*** Adjust expiry date if no errors in batch
      l_log-aldate_del = sy-datum + c_keep_warnings.
    ENDIF.
    CALL FUNCTION 'BAL_LOG_HDR_CHANGE'
      EXPORTING
        i_log_handle = v_log_handle
        i_s_log      = l_log.
  ENDIF.

*** Prepare for DB save of Application Log
  APPEND v_log_handle TO li_log_handles.
  IF im_commit = abap_false.
    l_in_update_task = abap_true.
  ENDIF.

*** Save Application Log data
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_in_update_task = l_in_update_task
      i_t_log_handle   = li_log_handles[]
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.
  IF sy-subrc > 0.
    RAISE sbal_error.
  ELSE.
    COMMIT WORK.
  ENDIF.

  CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'
    EXPORTING
      i_t_logs_to_be_refreshed = li_log_handles[]
      i_refresh_all            = abap_false.

  DELETE i_object_ref
      WHERE appl_log_key-tabname = v_appl_log_key-tabname AND
            appl_log_key-zdtl_fevt_id = v_appl_log_key-zdtl_fevt_id AND
            appl_log_key-zdtl_fevt_ver = v_appl_log_key-zdtl_fevt_ver AND
            appl_log_key-seqno = v_appl_log_key-seqno.

  DELETE i_log_handles WHERE table_line = v_log_handle.

  CLEAR: v_log_handle.

ENDMETHOD.


METHOD show_messages_in_popup.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT666_ZMM_DTL_MSGS=>SHOW_MESSAGES_IN_POPUP        *
* Title    :  Application Log Messaging                                *
* Work Unit:  SM-INT-666                                               *
* Created by: Rob West                                                 *
* Created on: December 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose  :  Manage Application Log messages related to tables        *
*             ZMM_DTL_FUEL_EVT and ZMM_DTL_FUEL_XTR                    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K963219             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: l_profile            TYPE bal_s_prof,
        li_log_handles       TYPE bal_t_logh.

  FIELD-SYMBOLS: <hdr>       TYPE balhdr.

  LOOP AT i_log_headers ASSIGNING <hdr>
      WHERE extnumber+30(10) = im_event_id AND
            extnumber+40(5)  = im_version.
    COLLECT <hdr>-log_handle INTO li_log_handles.
  ENDLOOP.

  IF sy-subrc > 0.
    RAISE no_messages.
  ENDIF.

  l_profile-start_col = 10.
  l_profile-start_row = 5.
  l_profile-end_col = sy-scols - 10.
  l_profile-end_row = 25.
  l_profile-tree_size = 20.
  IF LINES( li_log_handles ) = 1.
    l_profile-show_all = abap_true.  "Expand messages if only one
  ENDIF.

  CALL METHOD zcl_smint666_zmm_dtl_msgs=>build_tree_fcat
    CHANGING
      ch_fcat = l_profile-lev1_fcat[]
      ch_sort = l_profile-lev1_sort[].

  CALL METHOD zcl_smint666_zmm_dtl_msgs=>build_grid_fcat
    CHANGING
      ch_fcat = l_profile-mess_fcat[].

  CONCATENATE 'Fuel Event:'(tr1) im_event_id
              'Version:'(tr2) im_version
      INTO l_profile-title SEPARATED BY space.
  l_profile-use_grid = 'X'.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_t_log_handle      = li_log_handles
      i_s_display_profile = l_profile.

*  CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'
*    EXPORTING
*      i_t_logs_to_be_refreshed = li_log_handles[]
*      i_refresh_all            = abap_false.

ENDMETHOD.
ENDCLASS.
