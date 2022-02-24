class ZCL_SMINT60A_ZMM_FLCM_TDS definition
  public
  final
  create public .

*"* public components of class ZCL_SMINT60A_ZMM_FLCM_TDS
*"* do not include other source files here!!!
public section.

  types:
    BEGIN OF t_object_ref,
                   tran_type     TYPE zztds_tran_type,
                   tran_ref_nbr  TYPE zztds_tran_ref_nbr,
                   trml_id       TYPE zztds_terminal_id,
                   bol_nbr       TYPE zztds_bol_nbr,
                   cancel_rebill TYPE zztds_cancel_rebill,
                   tran_dt       TYPE zztds_transaction_dt, "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
                   object        TYPE REF TO zcl_smint60a_zmm_flcm_tds,
        END OF t_object_ref .
  types:
    t_object_ref_table       TYPE STANDARD TABLE OF t_object_ref
                                      WITH DEFAULT KEY .

  class-methods BUILD_GRID_FCAT
    importing
      !IM_HIDE_REF_NB type XFELD default ABAP_FALSE
    changing
      !CH_FCAT type BAL_T_FCAT .
  class-methods BUILD_TREE_FCAT
    changing
      !CH_FCAT type BAL_T_FCAT
      !CH_SORT type BAL_T_SORT .
  class-methods DELETE_MESSAGES
    importing
      !IM_TRAN_REF_NBR_TABLE type ZZTDS_TRAN_REF_NBR_TABLE
      !IM_COMMIT type XFELD optional
    exceptions
      NO_LOGS_SPECIFIED .
  class-methods FREE_ALL .
  class-methods FREE_APPL_LOG .
  class-methods GET_ALL_MESSAGES
    importing
      !IM_TRAN_REF_NBR_TABLE type ZZTDS_TRAN_REF_NBR_TABLE
      !IM_MOST_RECENT type XFELD optional
      !IM_FREE_ALL_LOGS type XFELD default 'X'
    changing
      !CH_MESSAGES type ZZTDS_MESSAGES_TABLE .
  class-methods LOG_TDS_ACTIVE
    returning
      value(RE_ACTIVE) type XFELD .
  class-methods LOG_TDS_ON_OFF
    importing
      !IM_LOGGING_ON type XFELD .
  class-methods LOG_TDS_START
    importing
      !IM_TDS_DATA_TABLE type ZMM_FLCM_TDS_INTERFACE_TABLE .
  class-methods NEW_OBJECT_BOL
    importing
      !IM_TRAN_TYPE type ZZTDS_TRAN_TYPE
      !IM_TRML_ID type ZZTDS_TERMINAL_ID
      !IM_BOL_NBR type ZZTDS_BOL_NBR
      !IM_TRAN_DT type ZZTDS_TRANSACTION_DT optional
      !IM_LOAD_DATA type XFELD optional
      !IM_CANCEL_REBILL type ZZTDS_CANCEL_REBILL optional
      !IM_SKIP_CURRENT_BATCH type XFELD optional
    changing
      !CH_POINTER type ref to ZCL_SMINT60A_ZMM_FLCM_TDS
    exceptions
      NOT_FOUND .
  class-methods NEW_OBJECT_REF
    importing
      !IM_TRAN_REF_NBR type ZZTDS_TRAN_REF_NBR
      !IM_TRAN_TYPE type ZZTDS_TRAN_TYPE optional
      !IM_LOAD_DATA type XFELD optional
      !IM_CANCEL_REBILL type ZZTDS_CANCEL_REBILL optional
      !IM_TRAN_DT type ZZTDS_TRANSACTION_DT optional
    changing
      !CH_POINTER type ref to ZCL_SMINT60A_ZMM_FLCM_TDS
    exceptions
      NOT_FOUND .
  class-methods PLANT_GO_LIVE_DATE
    importing
      !IM_WERKS type WERKS_D
    returning
      value(RE_GO_LIVE) type DATS .
  class-methods SET_STATUS_AND_MESSAGES
    importing
      !IM_TRAN_REF_NBR type ZZTDS_TRAN_REF_NBR
      !IM_COMMIT type XFELD optional
      !IM_TRAN_STATUS type ZZTRAN_STUS
      !IM_TRAN_SUBSTATUS type NUMC3 optional
      !IM_MESSAGES type BAPIRET2_TAB optional
    exceptions
      ERROR_IN_SAVE .
  class-methods SET_DIFF_INV_AND_MESSAGES
    importing
      !IM_TRAN_REF_NBR type ZZTDS_TRAN_REF_NBR
      !IM_COMMIT type XFELD optional
      !IM_SAP_INV type ZZSAP_INV optional
      !IM_INV_DIFF type ZZINV_DIFF optional
      !IM_CUMULATE_DIFF type ZZCUMULATE_DIFF optional
      !IM_MESSAGES type BAPIRET2_TAB optional
    exceptions
      ERROR_IN_SAVE .
  methods SET_DIFF_INV
    importing
      !IM_SAP_INV type ZZSAP_INV
      !IM_INV_DIFF type ZZINV_DIFF
      !IM_CUMULATE_DIFF type ZZDIFF_DELTA .
  class-methods SHOW_MESSAGES_IN_POPUP
    importing
      !IM_TRAN_REF_NBR type ZZTDS_TRAN_REF_NBR
    exceptions
      NO_MESSAGES .
  methods CLEAR_UPDATE_FLAG .
  methods CONSTRUCTOR
    importing
      !IM_TRAN_REF_NBR type ZZTDS_TRAN_REF_NBR optional
      !IM_TRAN_TYPE type ZZTDS_TRAN_TYPE optional
      !IM_TRML_ID type ZZTDS_TERMINAL_ID optional
      !IM_BOL_NBR type ZZTDS_BOL_NBR optional
      !IM_CANCEL_REBILL type ZZTDS_CANCEL_REBILL optional
      !IM_LOAD_DATA type XFELD optional
      !IM_TRAN_DT type ZZTDS_TRANSACTION_DT optional
    exceptions
      NOT_FOUND .
  methods DATA_TO_COMPARE
    importing
      !IM_FLCM_TDS type ZMM_FLCM_TDS optional
    returning
      value(RE_DATA) type ZMM_FLCM_TDS_INTERFACE .
  methods ERRORS_FOUND
    returning
      value(RE_ERROR_FLAG) type XFELD .
  methods GET_STATUS
    returning
      value(RE_STATUS) type ZZTRAN_STUS .
  methods GET_STATUS_SUBSTATUS
    changing
      !CH_STATUS type ZZTRAN_STUS
      !CH_SUBSTATUS type ZZTRAN_SUB_STUS .
  methods GET_SUBSTATUS
    returning
      value(RE_SUBSTATUS) type ZZTRAN_SUB_STUS .
  methods GET_TRAN_DATE
    returning
      value(RE_TRAN_DATE) type ZZTDS_TRANSACTION_DT .
  methods GET_TRAN_REF_NBR
    returning
      value(RE_TRAN_REF_NBR) type ZZTDS_TRAN_REF_NBR .
  methods LOG_ERROR
    importing
      !IM_MSGID type SYMSGID
      !IM_MSGNO type SYMSGNO
      !IM_MSGTY type SYMSGTY optional
      !IM_MSGV1 type CLIKE optional
      !IM_MSGV2 type CLIKE optional
      !IM_MSGV3 type CLIKE optional
      !IM_MSGV4 type CLIKE optional .
  methods LOG_TDS_DISPLAY .
  methods REPORT_ERRORS
    changing
      !CH_TEXT type STANDARD TABLE
      !CH_SEND_EMAIL type XFELD optional .
  methods SAVE_DATA
    importing
      !IM_COMMIT type XFELD optional
    exceptions
      DB_ERROR
      SBAL_ERROR .
  methods SET_STATUS
    importing
      !IM_STATUS type ZZTRAN_STUS .
  methods SET_STATUS_SUBSTATUS
    importing
      !IM_STATUS type ZZTRAN_STUS
      !IM_SUBSTATUS type ZZTRAN_SUB_STUS .
  methods TRANSFER_DATA
    importing
      !IM_TDS_DATA type ZMM_FLCM_TDS_INTERFACE .
protected section.
*"* protected components of class ZCL_SMINT60A_ZMM_FLCM_TDS
*"* do not include other source files here!!!

  methods FREE_SINGLE .
private section.
*"* private components of class ZCL_SMINT60A_ZMM_FLCM_TDS
*"* do not include other source files here!!!

  types:
    BEGIN OF t_parms,
             zparm_nm        TYPE zmm_flcm_parms-zparm_nm,
             zval_from       TYPE zmm_flcm_parms-zval_from,
           END OF t_parms .
  types:
    t_parms_table     TYPE STANDARD TABLE OF t_parms
                                  WITH DEFAULT KEY .
  types:
    BEGIN OF t_dd03l,
           fieldname            TYPE dd03l-fieldname,
           position             TYPE dd03l-position,
           keyflag              TYPE dd03l-keyflag,
           inttype              TYPE dd03l-inttype,
           scrtext              TYPE dd04t-scrtext_m,
         END OF t_dd03l .
  types:
    t_dd03l_table             TYPE STANDARD TABLE OF t_dd03l
                                  WITH DEFAULT KEY .
  types:
    BEGIN OF t_appl_log_key,
           tran_ref_nbr      TYPE zztds_tran_ref_nbr,
           seqno             TYPE n LENGTH 6,
         END OF t_appl_log_key .
  types:
    t_xref_table         TYPE STANDARD TABLE OF zmm_flcm_xref
                                  WITH DEFAULT KEY .
  types:
    t_fieldname_range TYPE RANGE OF fieldname .

  class-data I_OBJECT_REF type T_OBJECT_REF_TABLE .
  constants C_KEEP_ERRORS type I value 365 ##NO_TEXT.
  constants C_KEEP_WARNINGS type I value 90 ##NO_TEXT.
  constants C_KEEP_LOGS type I value 15 ##NO_TEXT.
  data V_TRAN_DATE type ZZTDS_TRANSACTION_DT .
  data V_TRAN_TYPE type ZZTDS_TRAN_TYPE .
  data V_TRAN_REF_NBR type ZZTDS_TRAN_REF_NBR .
  data V_TRML_ID type ZZTDS_TERMINAL_ID .
  data V_FLCM_TDS type ZMM_FLCM_TDS .
  data V_UPDATES_MADE type XFELD .
  data V_ERROR_COUNT type I .
  data V_WARNING_COUNT type I .
  data V_OTHER_COUNT type I .
  data V_LOG_HANDLE type BALLOGHNDL .
  class-data V_DATA_EXPIRY_DATE type DATS .
  class-data V_DATA_EXPIRY_TIME type TIMS .
  class-data V_TRACKING_INFO type ZMM_SMINT60A_TRACKING_INFO .
  class-data I_PARMS type T_PARMS_TABLE .
  class-data I_FIELD_DESCR type T_DD03L_TABLE .
  class-data I_LOG_HANDLES type BAL_T_LOGH .
  class-data I_LOG_HEADERS type BALHDR_T .
  class-data I_TDS_HANDLES type BAL_T_LOGH .
  class-data I_INTERFACE_FIELDS type FIELDNAME_TAB .
  class-data I_FLCM_XREF type T_XREF_TABLE .
  class-data R_XREF_FIELDS type T_FIELDNAME_RANGE .
  constants C_PROD_OWNER type ZZPRODUCT_OWNER value '0000000346' ##NO_TEXT.
  constants C_LOGGING_ON type CHAR05 value ICON_LED_GREEN ##NO_TEXT.
  constants C_LOGGING_OFF type CHAR05 value ICON_LED_RED ##NO_TEXT.
  constants C_OBJECT type BALOBJ_D value 'ZSM' ##NO_TEXT.
  constants C_SUBOBJECT type BALSUBOBJ value 'SM-INT-60A' ##NO_TEXT.
  constants C_MISC_FUELLING type ZZCARRIER_LIFNR value '0000202945' ##NO_TEXT.
  constants C_520 type ZZTDS_TRAN_TYPE value '520' ##NO_TEXT.
  constants C_530 type ZZTDS_TRAN_TYPE value '530' ##NO_TEXT.
  constants C_NUMERIC type CHAR10 value '0123456789' ##NO_TEXT.

  class-methods CROSS_REFERENCE
    importing
      !IM_FIELDNAME type FIELDNAME
      !IM_TRAN_TYPE type ZZTDS_TRAN_TYPE
      !IM_VALUE type ANY
    returning
      value(RE_VALUE) type ZZVALUE_TO .
  class-methods GET_FIELD_CONFIG
    importing
      !IM_TRAN_TYPE type ZZTDS_TRAN_TYPE
    changing
      !CH_MANDATORY type FIELDNAME_TAB
      !CH_OPTIONAL type FIELDNAME_TAB
    exceptions
      INVALID_TYPE .
  class-methods LOAD_STATIC_DATA .
  class-methods LOG_TDS_NULL_ENTRY .
  class-methods LOG_TDS_VERSION_INFO
    importing
      !IM_OBJECT type TROBJTYPE
      !IM_OBJ_NAME type TROBJ_NAME
    changing
      !CH_TRKORR type TRKORR
      !CH_TRFUNCTION type TRFUNCTION
      !CH_TRSTATUS type TRSTATUS
      !CH_TARSYSTEM type TR_TARGET
      !CH_KORRDEV type TRCATEG
      !CH_AS4USER type TR_AS4USER
      !CH_AS4DATE type AS4DATE
      !CH_AS4TIME type AS4TIME
      !CH_STRKORR type STRKORR .
  methods CREATE_APPL_LOG
    importing
      !IM_TRAN_REF_NBR type ZZTDS_TRAN_REF_NBR
    returning
      value(RE_LOG_HANDLE) type BALLOGHNDL .
  methods EDIT_DATA
    importing
      !IM_TARGET type ANY
      !IM_FIELDNAME type FIELDNAME .
  methods GET_CANCEL_REBILL
    returning
      value(RE_CANCEL_REBILL) type ZZTDS_CANCEL_REBILL .
  methods GET_DATA_TYPE
    importing
      !IM_FIELDNAME type FIELDNAME
    returning
      value(RE_FIELDTYPE) type INTTYPE .
  methods MOVE_AND_EDIT
    importing
      !IM_TDS_DATA type ZMM_FLCM_TDS_INTERFACE
      !IM_FIELDS type FIELDNAME_TAB
      !IM_MANDATORY type XFELD optional .
  methods MOVE_DATA
    importing
      !IM_TYPE_KIND type INTTYPE
      !IM_SOURCE type C
      !IM_FIELDNAME type FIELDNAME
    changing
      !CH_TARGET type ANY .
  methods VALID_DATE
    importing
      !IM_DATE type DATS
    returning
      value(RE_VALID) type XFELD .
  methods VALID_TIME
    importing
      !IM_TIME type TIMS
    returning
      value(RE_VALID) type XFELD .
  class-methods ZERO_IS_ALLOWED
    importing
      !IM_TRAN_TYPE type ZZTDS_TRAN_TYPE
      !IM_WERKS type WERKS_D
      !IM_FIELDNAME type CSEQUENCE
    returning
      value(RE_ZERO_OK) type XFELD .
ENDCLASS.



CLASS ZCL_SMINT60A_ZMM_FLCM_TDS IMPLEMENTATION.


METHOD build_grid_fcat.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>BUILD_GRID_FCAT               *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_fcat                TYPE bal_s_fcat.

  STATICS: sti_fcat           TYPE bal_t_fcat.

*** Add columns to right-hand panel - displayed as default

  IF sti_fcat[] IS INITIAL.
    CLEAR l_fcat.
    l_fcat-ref_table = 'ZMM_FLCM_TDS_KEY'.
    l_fcat-ref_field = 'ZTDS_TRAN_REF_NB'.
    IF im_hide_ref_nb = abap_true.
      l_fcat-no_out = 'X'.
    ENDIF.
    APPEND l_fcat TO sti_fcat.

    CLEAR l_fcat.
    l_fcat-ref_table = 'BAL_S_SHOW'.
    l_fcat-ref_field = 'T_MSG'.
    l_fcat-outputlen = '70'.
    APPEND l_fcat TO sti_fcat.

    CLEAR l_fcat.
    l_fcat-ref_table = 'ZMM_FLCM_TDS_KEY'.
    l_fcat-ref_field = 'ZTDS_TRAN_TYPE'.
    l_fcat-no_out = 'X'.
    APPEND l_fcat TO sti_fcat.

    CLEAR l_fcat.
    l_fcat-ref_table = 'ZMM_FLCM_TDS_KEY'.
    l_fcat-ref_field = 'WERKS'.
    l_fcat-no_out = 'X'.
    APPEND l_fcat TO sti_fcat.

    CLEAR l_fcat.
    l_fcat-ref_table = 'ZMM_FLCM_TDS_KEY'.
    l_fcat-ref_field = 'ZTDS_FOLIO_YR'.
    l_fcat-no_out = 'X'.
    APPEND l_fcat TO sti_fcat.

    CLEAR l_fcat.
    l_fcat-ref_table = 'ZMM_FLCM_TDS_KEY'.
    l_fcat-ref_field = 'ZTDS_FOLIO_MTH'.
    l_fcat-no_out = 'X'.
    APPEND l_fcat TO sti_fcat.

    CLEAR l_fcat.
    l_fcat-ref_table = 'ZMM_FLCM_TDS_KEY'.
    l_fcat-ref_field = 'ZTDS_FOLIO_NBR'.
    l_fcat-no_out = 'X'.
    APPEND l_fcat TO sti_fcat.

    CLEAR l_fcat.
    l_fcat-ref_table = 'ZMM_FLCM_TDS_KEY'.
    l_fcat-ref_field = 'ZTDS_FOLIO_SEQ'.
    l_fcat-no_out = 'X'.
    APPEND l_fcat TO sti_fcat.

    CLEAR l_fcat.
    l_fcat-ref_table = 'ZMM_FLCM_TDS_KEY'.
    l_fcat-ref_field = 'ZTDS_TRML_ID'.
    l_fcat-no_out = 'X'.
    APPEND l_fcat TO sti_fcat.

    CLEAR l_fcat.
    l_fcat-ref_table = 'ZMM_FLCM_TDS_KEY'.
    l_fcat-ref_field = 'ZTDS_BOL_NBR'.
    l_fcat-no_out = 'X'.
    APPEND l_fcat TO sti_fcat.
  ENDIF.

  ch_fcat[] = sti_fcat[].

ENDMETHOD.


METHOD build_tree_fcat.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>BUILD_TREE_FCAT               *
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
* Rob West                  2011/03/01          DV5K961204             *
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


METHOD clear_update_flag.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->CLEAR_UPDATE_FLAG             *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  v_updates_made = abap_false.

ENDMETHOD.


METHOD constructor.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>CONSTRUCTOR                   *
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
* Chari Flor Supino          2019/07/10          DV5K9A0ARX            *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Add delivery date                                 *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_flcm_tds          TYPE STANDARD TABLE OF zmm_flcm_tds,
        l_cancel_rebill      TYPE zztds_cancel_rebill.

  FIELD-SYMBOLS: <objref>    TYPE t_object_ref,
                 <tds>       TYPE zmm_flcm_tds.

*--------------------------------------------------------------------*
*** Skip processing when showing the log from TCODE ZM_TDS_INTERFACE
  IF sy-tcode = 'OS_APPLICATION'.
    RETURN.
  ENDIF.

*--------------------------------------------------------------------*
*** Due to RFC call from Web Methods, CLASS_CONSTRUCTOR not viable

  CALL METHOD load_static_data.

*--------------------------------------------------------------------*

  IF im_load_data = abap_false.
    v_tran_type = im_tran_type.
    v_tran_ref_nbr = im_tran_ref_nbr.
    APPEND INITIAL LINE TO i_object_ref ASSIGNING <objref>.
    <objref>-tran_ref_nbr = v_tran_ref_nbr.
    <objref>-object = me.     "self reference
    <objref>-tran_dt = im_tran_dt. "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
    RETURN.
  ENDIF.

*--------------------------------------------------------------------*

  IF im_cancel_rebill IS SUPPLIED.
    l_cancel_rebill = im_cancel_rebill.
  ELSE.
    l_cancel_rebill = '*'.
  ENDIF.

  IF im_tran_ref_nbr IS SUPPLIED.
    SELECT *
        FROM zmm_flcm_tds
        INTO TABLE li_flcm_tds
        WHERE ztds_tran_ref_nb = im_tran_ref_nbr.
  ELSE.
    IF im_tran_type IS SUPPLIED AND
       im_trml_id IS SUPPLIED AND
       im_bol_nbr IS SUPPLIED.
      SELECT *
          FROM zmm_flcm_tds
          INTO TABLE li_flcm_tds
          WHERE ztds_tran_type = im_tran_type AND
                ztds_trml_id   = im_trml_id AND
                ztds_bol_nbr   = im_bol_nbr.
    ELSE.
      CLEAR v_flcm_tds.
      RAISE not_found.
    ENDIF.
  ENDIF.

  SORT li_flcm_tds BY ztds_tran_dt DESCENDING
                      ztds_tran_tm DESCENDING.
  IF l_cancel_rebill = '*'.
    READ TABLE li_flcm_tds INTO v_flcm_tds INDEX 1.
  ELSE.
    READ TABLE li_flcm_tds INTO v_flcm_tds
        WITH KEY ztds_canc_rbil = l_cancel_rebill.
  ENDIF.

  IF sy-subrc > 0.
    CLEAR v_flcm_tds.
    RAISE not_found.
  ENDIF.

  APPEND INITIAL LINE TO i_object_ref ASSIGNING <objref>.
  <objref>-tran_type     = v_tran_type    = v_flcm_tds-ztds_tran_type.
  <objref>-tran_ref_nbr  = v_tran_ref_nbr = v_flcm_tds-ztds_tran_ref_nb.
  <objref>-trml_id       = v_trml_id      = v_flcm_tds-ztds_trml_id.
  <objref>-bol_nbr       = v_flcm_tds-ztds_bol_nbr.
  <objref>-cancel_rebill = v_flcm_tds-ztds_canc_rbil.
  <objref>-object       = me.     "self reference
  <objref>-tran_dt       = v_tran_date = v_flcm_tds-ztds_tran_dt. "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122

ENDMETHOD.


METHOD create_appl_log.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->CREATE_APPL_LOG               *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_log                TYPE bal_s_log,
        l_extnumber          TYPE balnrext,
        li_headers           TYPE STANDARD TABLE OF balnrext.

  FIELD-SYMBOLS: <key>       TYPE t_appl_log_key.

  ASSIGN l_extnumber TO <key> CASTING.
  CLEAR <key> WITH '_'.
  <key>-tran_ref_nbr = im_tran_ref_nbr.

  SELECT extnumber
    FROM balhdr
    INTO TABLE li_headers
    WHERE object    = c_object AND
          subobject = c_subobject AND
          extnumber LIKE l_extnumber.
  IF sy-subrc = 0.
    SORT li_headers DESCENDING.
    READ TABLE li_headers INTO l_extnumber INDEX 1.
  ELSE.
    CLEAR <key>-seqno.
  ENDIF.

  LOOP AT i_object_ref TRANSPORTING NO FIELDS
      WHERE tran_ref_nbr = im_tran_ref_nbr AND
            object <> me.
    ADD 1 TO <key>-seqno.
  ENDLOOP.

  ADD 1 TO <key>-seqno.

  l_log-object = c_object.
  l_log-subobject = c_subobject.
  l_log-extnumber = l_extnumber.
  l_log-aldate_del = sy-datum + c_keep_errors.
  l_log-aldate = sy-datum.
  l_log-altime = sy-uzeit.
  l_log-alprog = sy-cprog.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = l_log
    IMPORTING
      e_log_handle = re_log_handle.

  COLLECT re_log_handle INTO i_log_handles.

ENDMETHOD.


METHOD cross_reference.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>CROSS_REFERENCE               *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  FIELD-SYMBOLS: <xref>      TYPE zmm_flcm_xref.

  READ TABLE i_flcm_xref ASSIGNING <xref>
      WITH KEY zvar_nm = im_fieldname
               zval_qlfy = im_tran_type
               zval_from = im_value.
  IF sy-subrc = 0.
    re_value = <xref>-zval_to.
  ELSE.
    READ TABLE i_flcm_xref ASSIGNING <xref>
        WITH KEY zvar_nm = im_fieldname
                 zval_qlfy = space
                 zval_from = im_value.
    IF sy-subrc = 0.
      re_value = <xref>-zval_to.
    ELSE.
      CLEAR re_value WITH '*'.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD data_to_compare.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->DATA_TO_COMPARE               *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  CLEAR re_data.

  IF im_flcm_tds IS SUPPLIED AND
     im_flcm_tds IS NOT INITIAL.
    MOVE-CORRESPONDING im_flcm_tds TO re_data.
  ELSE.
    MOVE-CORRESPONDING v_flcm_tds TO re_data.
  ENDIF.

  CLEAR: re_data-ztds_tran_ref_nb,
         re_data-ztds_folio_yr,
         re_data-ztds_folio_mth,
         re_data-ztds_folio_nbr,
         re_data-ztds_folio_seq,
         re_data-ztds_tran_dt,
         re_data-ztds_tran_tm,
         re_data-ztds_tran_tz,
         re_data-ztds_canc_rbil,
         re_data-ztds_mnl_auto,
         re_data-ztds_vol_sign.

ENDMETHOD.


METHOD delete_messages.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>DELETE_MESSAGES               *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_object             TYPE bal_s_obj,
        l_subobject          TYPE bal_s_sub,
        l_extnumber          TYPE bal_s_extn,
        l_filter             TYPE bal_s_lfil,

        li_log_headers       TYPE balhdr_t.

  FIELD-SYMBOLS: <ref>       TYPE zztds_tran_ref_nbr,
                 <key>       TYPE t_appl_log_key.

  IF im_tran_ref_nbr_table[] IS INITIAL.
    RAISE no_logs_specified.
  ENDIF.

** Set up filters
  l_object-sign = 'I'.
  l_object-option = 'EQ'.
  l_object-low = c_object.
  APPEND l_object TO l_filter-object.
  l_subobject-sign = 'I'.
  l_subobject-option = 'EQ'.
  l_subobject-low = c_subobject.
  APPEND l_subobject TO l_filter-subobject.
  l_extnumber-sign = 'I'.
  l_extnumber-option = 'CP'.
  ASSIGN l_extnumber-low TO <key> CASTING.
  CLEAR <key> WITH '+'.
  LOOP AT im_tran_ref_nbr_table ASSIGNING <ref>.
    <key>-tran_ref_nbr = <ref>.
    APPEND l_extnumber TO l_filter-extnumber.
  ENDLOOP.

** Retrieve logs from Application Log DB
  CALL FUNCTION 'BAL_DB_SEARCH'
    EXPORTING
      i_s_log_filter     = l_filter
    IMPORTING
      e_t_log_header     = li_log_headers[]
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


METHOD edit_data.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->EDIT_DATA                     *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_msg              TYPE symsg,
        l_fieldname        TYPE dfies-lfieldname.

  IF im_target IS INITIAL.
    RETURN.
  ENDIF.

  l_fieldname = im_fieldname.

  CALL FUNCTION 'DDUT_INPUT_CHECK'
    EXPORTING
      tabname   = 'ZMM_FLCM_TDS'
      fieldname = l_fieldname
      value     = im_target
    IMPORTING
      msgid     = l_msg-msgid
      msgty     = l_msg-msgty
      msgno     = l_msg-msgno
      msgv1     = l_msg-msgv1
      msgv2     = l_msg-msgv2
      msgv3     = l_msg-msgv3
      msgv4     = l_msg-msgv4.

  IF l_msg-msgty = 'E'.
    CALL METHOD set_status
      EXPORTING
        im_status = '01'.
    CALL METHOD log_error
      EXPORTING
        im_msgid = 'ZZ_FLCM'
        im_msgno = '003'
        im_msgty = 'E'
        im_msgv1 = im_target
        im_msgv2 = im_fieldname.
    IF 1 = 2.      "For «Where Used» only
      MESSAGE e003(zz_flcm) WITH space space.
    ENDIF.
    CALL METHOD log_error
      EXPORTING
        im_msgid = l_msg-msgid
        im_msgno = l_msg-msgno
        im_msgty = l_msg-msgty
        im_msgv1 = l_msg-msgv1
        im_msgv2 = l_msg-msgv2
        im_msgv3 = l_msg-msgv3
        im_msgv4 = l_msg-msgv4.
  ENDIF.

ENDMETHOD.


METHOD errors_found.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->ERRORS_FOUND                  *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  IF v_error_count = 0.
    re_error_flag = abap_false.
  ELSE.
    re_error_flag = abap_true.
  ENDIF.

ENDMETHOD.


METHOD free_all.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>FREE_ALL                      *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  CALL METHOD zcl_smint60a_zmm_flcm_tds=>free_appl_log.

  REFRESH i_object_ref.

ENDMETHOD.


METHOD free_appl_log.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>FREE_APPL_LOG                 *
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
* Rob West                  2011/03/01          DV5K961204             *
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


METHOD free_single.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->FREE_SINGLE                   *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DELETE i_log_handles WHERE table_line = v_log_handle.
  DELETE i_log_headers WHERE log_handle = v_log_handle.

  CALL FUNCTION 'BAL_LOG_REFRESH'
    EXPORTING
      i_log_handle  = v_log_handle
    EXCEPTIONS
      log_not_found = 1
      OTHERS        = 2.

  CLEAR: v_log_handle.

ENDMETHOD.


METHOD get_all_messages.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>GET_ALL_MESSAGES              *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_message            TYPE zztds_messages,
        l_msg                TYPE bal_s_msg,
        l_symsg              TYPE symsg,
        l_msg_hndl           TYPE balmsghndl,
        l_tabix              TYPE sy-tabix   VALUE 1,

        li_ref_num           TYPE zztds_tran_ref_nbr_table,

        li_log_headers       TYPE balhdr_t,
        li_log_handles       TYPE bal_t_logh,

        li_work_headers      TYPE STANDARD TABLE OF balhdr,
        li_work_handles      TYPE STANDARD TABLE OF balloghndl.

  FIELD-SYMBOLS: <ref>       TYPE zztds_tran_ref_nbr,
                 <key>       TYPE t_appl_log_key,
                 <hdr>       TYPE balhdr.

  REFRESH ch_messages.

  IF im_free_all_logs = abap_true AND i_log_handles[] IS NOT INITIAL.
    CALL METHOD zcl_smint60a_zmm_flcm_tds=>free_appl_log.
  ENDIF.

  SELECT *
    FROM balhdr
    INTO TABLE li_work_headers
    WHERE object = c_object AND
          subobject = c_subobject.

  IF sy-subrc > 0.
    RETURN.
  ENDIF.

  DELETE li_work_headers WHERE alstate IS NOT INITIAL.
  IF li_work_headers[] IS INITIAL.
    RETURN.
  ENDIF.

  SORT li_work_headers BY extnumber+0(10) ASCENDING
                          extnumber+10(6) DESCENDING.

  IF im_most_recent = abap_true.
    DELETE ADJACENT DUPLICATES FROM li_work_headers
           COMPARING extnumber+0(10).
  ENDIF.

  li_ref_num[] = im_tran_ref_nbr_table[].
  SORT li_ref_num ASCENDING.

  LOOP AT li_ref_num ASSIGNING <ref>.
    LOOP AT li_work_headers ASSIGNING <hdr> FROM l_tabix.
      IF <hdr>-extnumber+0(10) = <ref>.
        l_tabix = sy-tabix + 1.
      ELSEIF <hdr>-extnumber+0(10) < <ref>.
        DELETE li_work_headers INDEX sy-tabix.
      ELSE.
        l_tabix = sy-tabix.
        EXIT.
      ENDIF.
*      IF <hdr>-extnumber+0(10) < <ref>.
*        DELETE li_work_headers INDEX sy-tabix.
*      ELSE.
*        l_tabix = sy-tabix.
*        IF <hdr>-extnumber+0(10) = <ref>.
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

  IF sy-subrc = 0 OR sy-subrc = 3..
    LOOP AT li_log_headers ASSIGNING <hdr>.
      CLEAR l_message.
      l_message-ztds_tran_ref_nb = <hdr>-extnumber.
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
  ENDIF.

  SORT ch_messages BY ztds_tran_ref_nb ASCENDING.

  REFRESH li_work_headers.
  APPEND LINES OF i_log_headers TO li_work_headers.
  APPEND LINES OF li_log_headers TO li_work_headers.
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


METHOD get_cancel_rebill.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->GET_CANCEL_REBILL             *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  re_cancel_rebill = v_flcm_tds-ztds_canc_rbil.

ENDMETHOD.


METHOD get_data_type.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->GET_DATA_TYPE                *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  FIELD-SYMBOLS: <descr>     TYPE t_dd03l.

  CLEAR re_fieldtype.

  READ TABLE i_field_descr ASSIGNING <descr>
      WITH KEY fieldname = im_fieldname.
  IF sy-subrc = 0.
    re_fieldtype = <descr>-inttype.
  ENDIF.

ENDMETHOD.


METHOD get_field_config.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>GET_FIELD_CONFIG             *
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
* Rob West                  2012/03/14          DV5K970135             *
*                                                                      *
* Short Description: CR199131-T206660                                  *
*                    710 type records will come to SAP with a value of *
*                    zero in Physical Inventory field                  *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  TYPES: BEGIN OF t_config,
           tran_type         TYPE zztds_tran_type,
           mandatory         TYPE fieldname_tab,
           optional          TYPE fieldname_tab,
         END OF t_config.

  STATICS: sti_config        TYPE STANDARD TABLE OF t_config.

  FIELD-SYMBOLS: <parms>     TYPE t_parms,
                 <config>    TYPE t_config.

  REFRESH: ch_mandatory,
           ch_optional.

  READ TABLE sti_config ASSIGNING <config>
      WITH KEY tran_type = im_tran_type.

  IF sy-subrc > 0.
    APPEND INITIAL LINE TO sti_config ASSIGNING <config>.
    <config>-tran_type = im_tran_type.
    <config>-optional[] = i_interface_fields[].
    LOOP AT i_parms ASSIGNING <parms>
        WHERE zparm_nm(3) = im_tran_type AND zparm_nm+3 IS INITIAL. "DV5K970135
*        WHERE zparm_nm = im_tran_type.                             "DV5K970135
      APPEND <parms>-zval_from TO <config>-mandatory.
      DELETE <config>-optional WHERE table_line = <parms>-zval_from.
      IF sy-subrc > 0.
        MESSAGE i052(zz_flcm) WITH <parms>-zval_from im_tran_type.
      ENDIF.
    ENDLOOP.
    IF sy-subrc = 0.     "Parms not needed for this type; delete
      DELETE i_parms WHERE zparm_nm = im_tran_type.
    ELSE.                "Parms not found for im_tran_type
      RAISE invalid_type.
    ENDIF.
  ENDIF.

  ch_mandatory = <config>-mandatory.
  ch_optional  = <config>-optional.

ENDMETHOD.


METHOD get_status.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->GET_STATUS                    *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  re_status = v_flcm_tds-ztran_stus.

ENDMETHOD.


METHOD get_status_substatus.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->GET_STATUS_SUBSTATUS          *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  ch_status = v_flcm_tds-ztran_stus.
  ch_substatus = v_flcm_tds-ztran_sub_stus.

ENDMETHOD.


METHOD get_substatus.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->GET_SUBSTATUS                 *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  re_substatus = v_flcm_tds-ztran_sub_stus.

ENDMETHOD.


METHOD get_tran_date.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->GET_TRAN_DATE                 *
* Title    :  Receive data from TDS and store it to ZMM_FLCM_TDS       *
* Work Unit:  SM-INT-60A-001                                           *
* Created by: Chari Supino                                             *
* Created on: July 24, 2019                                            *
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
* Chari Supino              2019/07/14          DV5K9A0ARX             *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Return the transaction date of rebill             *
*----------------------------------------------------------------------*

  re_tran_date = v_tran_date.

ENDMETHOD.


METHOD get_tran_ref_nbr.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->GET_TRAN_REF_NBR              *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  re_tran_ref_nbr = v_tran_ref_nbr.

ENDMETHOD.


METHOD load_static_data.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>LOAD_STATIC_DATA              *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_dd03l              TYPE t_dd03l.

  FIELD-SYMBOLS: <field>     LIKE LINE OF r_xref_fields,
                 <xref>      TYPE zmm_flcm_xref.

*--------------------------------------------------------------------*
*** Replaces CLASS_CONSTRUCTOR due to RFC function call from Web
*** Methods not releasing the function group and static data

  IF v_data_expiry_date IS INITIAL.
    CALL METHOD zcl_smint60a_zmm_flcm_tds=>log_tds_version_info
      EXPORTING
        im_object     = 'FUGR'
        im_obj_name   = 'ZMM_SMINT60A'
      CHANGING
        ch_trkorr     = v_tracking_info-trkorr_fugr
        ch_trfunction = v_tracking_info-trfunction_fugr
        ch_trstatus   = v_tracking_info-trstatus_fugr
        ch_tarsystem  = v_tracking_info-tarsystem_fugr
        ch_korrdev    = v_tracking_info-korrdev_fugr
        ch_as4user    = v_tracking_info-as4user_fugr
        ch_as4date    = v_tracking_info-as4date_fugr
        ch_as4time    = v_tracking_info-as4time_fugr
        ch_strkorr    = v_tracking_info-strkorr_fugr.
    CALL METHOD zcl_smint60a_zmm_flcm_tds=>log_tds_version_info
      EXPORTING
        im_object     = 'CLAS'
        im_obj_name   = 'ZCL_SMINT60A_ZMM_FLCM_TDS'
      CHANGING
        ch_trkorr     = v_tracking_info-trkorr_clas
        ch_trfunction = v_tracking_info-trfunction_clas
        ch_trstatus   = v_tracking_info-trstatus_clas
        ch_tarsystem  = v_tracking_info-tarsystem_clas
        ch_korrdev    = v_tracking_info-korrdev_clas
        ch_as4user    = v_tracking_info-as4user_clas
        ch_as4date    = v_tracking_info-as4date_clas
        ch_as4time    = v_tracking_info-as4time_clas
        ch_strkorr    = v_tracking_info-strkorr_clas.
  ENDIF.

  GET TIME.      "Refresh sy-datum and sy-uzeit
  IF v_data_expiry_date IS NOT INITIAL AND
    ( v_data_expiry_date > sy-datum OR
    ( v_data_expiry_date = sy-datum AND v_data_expiry_time > sy-uzeit ) ).
    RETURN.
  ENDIF.

  v_data_expiry_date = sy-datum.
  v_data_expiry_time = sy-uzeit.
  ADD 4 TO v_data_expiry_time(2).      "Add 4 hours
  IF v_data_expiry_time(2) > '23'.     "Roll into next day
    SUBTRACT 24 FROM v_data_expiry_time(2).
    ADD 1 TO v_data_expiry_date.
  ENDIF.

  v_tracking_info-parm_expiry_date = v_data_expiry_date.
  v_tracking_info-parm_expiry_time = v_data_expiry_time.

  REFRESH: i_parms,
           i_field_descr,
           i_interface_fields,
           i_flcm_xref,
           r_xref_fields.

*--------------------------------------------------------------------*

*** Get the field configuration for mandatory fields
  SELECT zparm_nm
         zval_from
    FROM zmm_flcm_parms
    INTO TABLE i_parms
    WHERE progname = 'SMINT60A'.

*** Get all components in the structure
  SELECT dd03l~fieldname
         dd03l~position
         dd03l~keyflag
         dd03l~inttype
         dd04t~scrtext_m
    FROM dd03l AS dd03l
    JOIN dd04t AS dd04t
    ON dd04t~rollname = dd03l~rollname
    INTO TABLE i_field_descr
    WHERE dd03l~tabname = 'ZMM_FLCM_TDS' AND
          dd04t~ddlanguage = sy-langu.
  DELETE i_field_descr WHERE fieldname = 'MANDT'.
  SORT i_field_descr BY position ASCENDING.

  l_dd03l-inttype = '>'.    "code to shift right, pad with zeros
  MODIFY i_field_descr FROM l_dd03l TRANSPORTING inttype
      WHERE fieldname = 'ZCARRIER' OR
            fieldname = 'LIFNR'.


  SELECT fieldname
    FROM dd03l
    INTO TABLE i_interface_fields
    WHERE tabname = 'ZMM_FLCM_TDS_INTERFACE'.

  IF sy-subrc = 0.
    SELECT *
      FROM zmm_flcm_xref
      INTO TABLE i_flcm_xref
      FOR ALL ENTRIES IN i_interface_fields
      WHERE zvar_nm = i_interface_fields-table_line.

    IF sy-subrc = 0.
      SORT i_flcm_xref BY zvar_nm   ASCENDING
                          zval_qlfy ASCENDING
                          zvar_seq  ASCENDING.
    ENDIF.
  ENDIF.

  LOOP AT i_flcm_xref ASSIGNING <xref>.
    IF ( <field> IS ASSIGNED AND <field>-low <> <xref>-zvar_nm ) OR
         <field> IS NOT ASSIGNED.
      APPEND INITIAL LINE TO r_xref_fields ASSIGNING <field>.
      <field>-sign = 'I'.
      <field>-option = 'EQ'.
      <field>-low = <xref>-zvar_nm.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD log_error.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->LOG_ERROR                     *
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
* Rob West                  2012/03/14          DV5K970135             *
*                                                                      *
* Short Description: CR199123-T207139                                  *
*                    Move 'MISC' to EQUNR for carrier '202945' with    *
*                    blank EQUNR                                       *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_msg              TYPE bal_s_msg.

  IF v_log_handle IS INITIAL.
    v_log_handle = create_appl_log( v_tran_ref_nbr ).
  ENDIF.

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

  CASE l_msg-msgty.                                          "DV5K970135
    WHEN 'E' OR 'A'. ADD 1 TO v_error_count.                 "DV5K970135
    WHEN 'W'.        ADD 1 TO v_warning_count.               "DV5K970135
    WHEN OTHERS.     ADD 1 TO v_other_count.                 "DV5K970135
  ENDCASE.                                                   "DV5K970135

*  ADD 1 TO v_error_count.                                   "DV5K970135

ENDMETHOD.


METHOD log_tds_active.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>LOG_TDS_ACTIVE                *
* Title    :  Receive data from TDS and store it to ZMM_FLCM_TDS       *
* Work Unit:  SM-INT-60A-001                                           *
* Created by: Rob West                                                 *
* Created on: June 2011                                                *
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
* Rob West                  2011/06/06          DV5K964123             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  TYPES: BEGIN OF lt_balhdr,
           extnumber         TYPE balhdr-extnumber,
           lognumber         TYPE balhdr-lognumber,
           alstate           TYPE balhdr-alstate,
         END OF lt_balhdr.

  DATA: li_balhdr            TYPE STANDARD TABLE OF lt_balhdr.

  FIELD-SYMBOLS: <hdr>       TYPE lt_balhdr.

  re_active = abap_false.

  SELECT extnumber
         lognumber
         alstate
    FROM balhdr
    INTO TABLE li_balhdr
    WHERE object = c_object AND
          subobject = c_subobject AND
          extnumber IN (c_logging_on, c_logging_off).

  IF sy-subrc > 0.
    RETURN.         "No entry found - not active
  ENDIF.

  DELETE li_balhdr WHERE alstate <> '1'.
  IF li_balhdr[] IS INITIAL.
    RETURN.         "No entry found - not active
  ENDIF.

  SORT li_balhdr BY lognumber DESCENDING.    "most recent first
  READ TABLE li_balhdr ASSIGNING <hdr> INDEX 1.
  IF sy-subrc = 0 AND <hdr>-extnumber = c_logging_on.
    re_active = abap_true.
  ENDIF.

ENDMETHOD.


METHOD log_tds_display.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->LOG_TDS_DISPLAY               *
* Title    :  Receive data from TDS and store it to ZMM_FLCM_TDS       *
* Work Unit:  SM-INT-60A-001                                           *
* Created by: Rob West                                                 *
* Created on: June 2011                                                *
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
* Rob West                  2011/06/06          DV5K964123             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: li_log_headers       TYPE balhdr_t,
        li_work_headers      TYPE STANDARD TABLE OF balhdr,
        l_profile            TYPE bal_s_prof,
        l_exit_state         TYPE bal_s_excm.

  FIELD-SYMBOLS: <fcat>      TYPE bal_s_fcat,
                 <sort>      TYPE bal_s_sort.

  SELECT *
  FROM balhdr
  INTO TABLE li_work_headers
  WHERE object = c_object AND
        subobject = c_subobject AND
        extnumber IN (icon_led_green, icon_led_yellow, icon_led_red).

  DELETE li_work_headers WHERE alstate <> '1'.

  IF li_work_headers[] IS NOT INITIAL.
    SORT li_work_headers BY mandant   ASCENDING
                           lognumber ASCENDING.
    li_log_headers[] = li_work_headers[].
*** Load log details from Application Log DB
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header     = li_log_headers[]
      IMPORTING
        e_t_log_handle     = i_tds_handles[]
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4.
  ELSE.
    CALL METHOD log_tds_null_entry.
  ENDIF.

  APPEND INITIAL LINE TO l_profile-lev1_fcat ASSIGNING <fcat>.
  <fcat>-ref_table = 'BAL_S_SHOW'.
  <fcat>-ref_field = 'ALDATE'.

  APPEND INITIAL LINE TO l_profile-lev2_fcat ASSIGNING <fcat>.
  <fcat>-ref_table = 'BAL_S_SHOW'.
  <fcat>-ref_field = 'ALTIME'.
  APPEND INITIAL LINE TO l_profile-lev2_fcat ASSIGNING <fcat>.
  <fcat>-ref_table = 'BAL_S_SHOW'.
  <fcat>-ref_field = 'LOGNUMBER'.
  <fcat>-tech = 'X'.
  APPEND INITIAL LINE TO l_profile-lev2_fcat ASSIGNING <fcat>.
  <fcat>-ref_table = 'BAL_S_SHOW'.
  <fcat>-ref_field = 'LOG_HANDLE'.
  <fcat>-tech = 'X'.

*** Set the date sort sequence to DESCENDING
  APPEND INITIAL LINE TO l_profile-lev1_sort ASSIGNING <sort>.
  <sort>-ref_table = 'BAL_S_SHOW'.
  <sort>-ref_field = 'ALDATE'.
  <sort>-spos = 1.
  <sort>-down = 'X'.

  APPEND INITIAL LINE TO l_profile-lev2_sort ASSIGNING <sort>.
  <sort>-ref_table = 'BAL_S_SHOW'.
  <sort>-ref_field = 'ALTIME'.
  <sort>-spos = 1.
  <sort>-down = 'X'.

  APPEND INITIAL LINE TO l_profile-mess_fcat ASSIGNING <fcat>.
  <fcat>-ref_table = 'BAL_S_SHOW'.
  <fcat>-ref_field = 'T_MSG'.
  <fcat>-outputlen = '50'.
  <fcat>-col_pos = sy-tabix.

  APPEND INITIAL LINE TO l_profile-mess_fcat ASSIGNING <fcat>.
  <fcat>-ref_table = 'BAL_S_SHOW'.
  <fcat>-ref_field = 'ALDATE'.
  <fcat>-no_out = 'X'.

  APPEND INITIAL LINE TO l_profile-mess_fcat ASSIGNING <fcat>.
  <fcat>-ref_table = 'BAL_S_SHOW'.
  <fcat>-ref_field = 'ALTIME'.

  APPEND INITIAL LINE TO l_profile-mess_fcat ASSIGNING <fcat>.
  <fcat>-ref_table = 'BAL_S_SHOW'.
  <fcat>-ref_field = 'ALUSER'.
  <fcat>-no_out = 'X'.

  APPEND INITIAL LINE TO l_profile-mess_sort ASSIGNING <sort>.
  <sort>-ref_table = 'BAL_S_SHOW'.
  <sort>-ref_field = 'ALTIME'.
  <sort>-spos = 1.
  <sort>-down = 'X'.

  l_profile-title = 'SM-INT-60A log'.
  l_profile-use_grid = 'X'.

*** Define callback before user command is executed
  l_profile-clbk_ucbf-userexitt     = space.
  l_profile-clbk_ucbf-userexitp     = 'SAPLZMM_SMINT60A'.
  l_profile-clbk_ucbf-userexitf     = 'F_SBAL_BEFORE_UCOMM'.

  l_profile-ext_push1-active        = abap_false.
  l_profile-ext_push1-def-icon_id   = icon_led_green.
  l_profile-ext_push1-def-icon_text = 'Start Interface Logging'(pb1).
  l_profile-ext_push2-active        = abap_false.
  l_profile-ext_push2-def-icon_id   = icon_led_red.
  l_profile-ext_push2-def-icon_text = 'Stop Interface Logging'(pb2).

  l_profile-exp_level = 2.

  WHILE l_exit_state-back = abap_false AND
        l_exit_state-exit = abap_false AND
        l_exit_state-cancel = abap_false.
    CLEAR: l_exit_state,
           l_profile-ext_push1-active,
           l_profile-ext_push2-active.
    IF log_tds_active( ) = abap_false.
      l_profile-ext_push1-active = abap_true.
    ELSE.
      l_profile-ext_push2-active = abap_true.
    ENDIF.
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle      = i_tds_handles
        i_s_display_profile = l_profile
      IMPORTING
        e_s_exit_command    = l_exit_state.
  ENDWHILE.

  CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'
    EXPORTING
      i_refresh_all            = ' '
      i_t_logs_to_be_refreshed = i_tds_handles[].

  REFRESH i_tds_handles.

ENDMETHOD.


METHOD log_tds_null_entry.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>LOG_TDS_NULL_ENTRY            *
* Title    :  Receive data from TDS and store it to ZMM_FLCM_TDS       *
* Work Unit:  SM-INT-60A-001                                           *
* Created by: Rob West                                                 *
* Created on: June 2011                                                *
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
* Rob West                  2011/06/06          DV5K964123             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: l_log                TYPE bal_s_log,
        l_log_handle         TYPE balloghndl.
*        li_log_handles       TYPE bal_t_logh.


  l_log-object = c_object.
  l_log-subobject = c_subobject.
  l_log-extnumber = icon_led_inactive.
  l_log-alstate = '1'.       "To differentiate from Error Log messages
  l_log-aldate_del = sy-datum + c_keep_logs.
  l_log-aldate = sy-datum.
  l_log-altime = sy-uzeit.
  l_log-alprog = sy-cprog.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = l_log
    IMPORTING
      e_log_handle = l_log_handle.

  COLLECT l_log_handle INTO i_tds_handles.

ENDMETHOD.


METHOD log_tds_on_off.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>LOG_TDS_ON_OFF                *
* Title    :  Receive data from TDS and store it to ZMM_FLCM_TDS       *
* Work Unit:  SM-INT-60A-001                                           *
* Created by: Rob West                                                 *
* Created on: June 2011                                                *
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
* Rob West                  2011/06/06          DV5K964123             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: l_log                TYPE bal_s_log,
        l_msg                TYPE bal_s_msg,
        l_log_handle         TYPE balloghndl,
        li_log_handles       TYPE bal_t_logh.


  l_log-object = c_object.
  l_log-subobject = c_subobject.
  IF im_logging_on = abap_true.
    l_log-extnumber = c_logging_on.    "Set logging ON
  ELSE.                                "Logging is ON
    l_log-extnumber = c_logging_off.   "Set logging OFF
  ENDIF.
  l_log-alstate = '1'.       "To differentiate from Error Log messages
  l_log-aldate_del = sy-datum + c_keep_logs.
  l_log-aldate = sy-datum.
  l_log-altime = sy-uzeit.
  l_log-alprog = sy-cprog.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = l_log
    IMPORTING
      e_log_handle = l_log_handle.

  APPEND l_log_handle TO li_log_handles.
  COLLECT l_log_handle INTO i_tds_handles.

  l_msg-msgid = 'ZZ_FLCM'.
  IF im_logging_on = abap_true.
    l_msg-msgty = 'S'.
    l_msg-msgno = '083'.
    IF 1 = 2.
      MESSAGE s083(zz_flcm).         "For «Where-Used» only
    ENDIF.
  ELSE.
    l_msg-msgty = 'E'.
    l_msg-msgno = '084'.
    IF 1 = 2.
      MESSAGE s084(zz_flcm).         "For «Where-Used» only
    ENDIF.
  ENDIF.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = l_log_handle
      i_s_msg      = l_msg.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle = li_log_handles.

  COMMIT WORK.

ENDMETHOD.


METHOD log_tds_start.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>LOG_TDS_START                 *
* Title    :  Receive data from TDS and store it to ZMM_FLCM_TDS       *
* Work Unit:  SM-INT-60A-001                                           *
* Created by: Rob West                                                 *
* Created on: June 2011                                                *
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
* Rob West                  2011/06/06          DV5K964123             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*


  DATA: l_tds_lines          TYPE i,
        l_log                TYPE bal_s_log,
        l_msg                TYPE bal_s_msg,
        l_log_handle         TYPE balloghndl,
        li_log_handles       TYPE bal_t_logh,
        li_new_lognumbers    TYPE bal_t_lgnm.

  FIELD-SYMBOLS: <lgnm>      TYPE bal_s_lgnm,
                 <track>     TYPE zmm_smint60a_tracking_info.

  IF log_tds_active( ) = abap_false.
    RETURN.
  ENDIF.

  l_tds_lines = LINES( im_tds_data_table ).

  l_log-object = c_object.
  l_log-subobject = c_subobject.
  l_log-extnumber = icon_led_yellow.
  l_log-alstate = '1'.       "To differentiate from Error Log messages
  l_log-aldate_del = sy-datum + c_keep_logs.
  l_log-aldate = sy-datum.
  l_log-altime = sy-uzeit.
  l_log-alprog = sy-cprog.
  l_log-context-tabname = 'ZMM_SMINT60A_TRACKING_INFO'.
  IF v_data_expiry_date IS NOT INITIAL.
    l_log-context-value = v_tracking_info.
  ELSE.
    ASSIGN l_log-context-value TO <track> CASTING.
    CALL METHOD log_tds_version_info
      EXPORTING
        im_object     = 'FUGR'
        im_obj_name   = 'ZMM_SMINT60A'
      CHANGING
        ch_trkorr     = <track>-trkorr_fugr
        ch_trfunction = <track>-trfunction_fugr
        ch_trstatus   = <track>-trstatus_fugr
        ch_tarsystem  = <track>-tarsystem_fugr
        ch_korrdev    = <track>-korrdev_fugr
        ch_as4user    = <track>-as4user_fugr
        ch_as4date    = <track>-as4date_fugr
        ch_as4time    = <track>-as4time_fugr
        ch_strkorr    = <track>-strkorr_fugr.
    CALL METHOD log_tds_version_info
      EXPORTING
        im_object     = 'CLAS'
        im_obj_name   = 'ZCL_SMINT60A_ZMM_FLCM_TDS'
      CHANGING
        ch_trkorr     = <track>-trkorr_clas
        ch_trfunction = <track>-trfunction_clas
        ch_trstatus   = <track>-trstatus_clas
        ch_tarsystem  = <track>-tarsystem_clas
        ch_korrdev    = <track>-korrdev_clas
        ch_as4user    = <track>-as4user_clas
        ch_as4date    = <track>-as4date_clas
        ch_as4time    = <track>-as4time_clas
        ch_strkorr    = <track>-strkorr_clas.
  ENDIF.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = l_log
    IMPORTING
      e_log_handle = l_log_handle.

  APPEND l_log_handle TO li_log_handles.

  l_msg-msgty = 'W'.
  l_msg-msgid = 'ZZ_FLCM'.
  l_msg-msgno = '082'.
  IF 1 = 2.
    MESSAGE s082(zz_flcm).         "For «Where-Used» only
  ENDIF.
  WRITE l_tds_lines TO l_msg-msgv1 LEFT-JUSTIFIED.
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = l_log_handle
      i_s_msg      = l_msg.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle   = li_log_handles
    IMPORTING
      e_new_lognumbers = li_new_lognumbers.

  IF l_tds_lines > 0.
    READ TABLE li_new_lognumbers ASSIGNING <lgnm> INDEX 1.
    IF sy-subrc = 0.
      EXPORT tds_data_table = im_tds_data_table[]
          TO DATABASE bal_indx(zs) ID <lgnm>-lognumber.
    ENDIF.
  ENDIF.

  COMMIT WORK.

  CALL FUNCTION 'BAL_LOG_REFRESH'
    EXPORTING
      i_log_handle = l_log_handle.

ENDMETHOD.


METHOD log_tds_version_info.
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>LOG_TDS_VERSION_INFO          *
* Title    :  Receive data from TDS and store it to ZMM_FLCM_TDS       *
* Work Unit:  SM-INT-60A-001                                           *
* Created by: Rob West                                                 *
* Created on: June 2011                                                *
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
* Rob West                  2011/06/06          DV5K964168             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_trkorr              TYPE STANDARD TABLE OF trkorr,
        li_e070                TYPE STANDARD TABLE OF e070.

  FIELD-SYMBOLS: <e070>        TYPE e070.

  CLEAR: ch_trkorr,
         ch_trfunction,
         ch_trstatus,
         ch_tarsystem,
         ch_korrdev,
         ch_as4user,
         ch_as4date,
         ch_as4time,
         ch_strkorr.

  IF im_object IS INITIAL OR im_obj_name IS INITIAL.
    RETURN.
  ENDIF.

  SELECT trkorr
    FROM e071
    INTO TABLE li_trkorr
    WHERE pgmid = 'R3TR' AND
         object = im_object AND
         obj_name = im_obj_name.

  IF sy-subrc > 0.
    RETURN.
  ENDIF.

  SELECT *
    FROM e070
    INTO TABLE li_e070
    FOR ALL ENTRIES IN li_trkorr
    WHERE trkorr = li_trkorr-table_line.

  IF sy-subrc > 0.
    RETURN.
  ENDIF.

  SORT li_e070 BY as4date DESCENDING
                  as4time DESCENDING.

  LOOP AT li_e070 ASSIGNING <e070>
      WHERE trfunction = 'S' OR trfunction = 'T'.
    ch_trkorr     = <e070>-trkorr.
    ch_trfunction = <e070>-trfunction.
    ch_trstatus   = <e070>-trstatus .
    ch_tarsystem  = <e070>-tarsystem.
    ch_korrdev    = <e070>-korrdev.
    ch_as4user    = <e070>-as4user.
    ch_as4date    = <e070>-as4date.
    ch_as4time    = <e070>-as4time.
    ch_strkorr    = <e070>-strkorr.
    EXIT.
  ENDLOOP.

ENDMETHOD.


METHOD move_and_edit.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->MOVE_AND_EDIT                 *
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
* Rob West                  2012/03/14          DV5K970135             *
*                                                                      *
* Short Description: CR199131-T206660                                  *
*                    710 type records will come to SAP with a value of *
*                    zero in Physical Inventory field                  *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_xref_value         TYPE zmm_flcm_xref-zval_to.


  FIELD-SYMBOLS: <field>     TYPE fieldname,
                 <target>    TYPE ANY,
                 <source>    TYPE ANY.                       "DV5K970135


  LOOP AT im_fields ASSIGNING <field>.
    UNASSIGN <target>.
    ASSIGN COMPONENT <field> OF STRUCTURE v_flcm_tds TO <target>.

    IF <target> IS NOT ASSIGNED.
      CALL METHOD set_status
        EXPORTING
          im_status = '01'.
      CALL METHOD log_error
        EXPORTING
          im_msgid = 'ZZ_FLCM'
          im_msgno = '052'
          im_msgty = 'E'
          im_msgv1 = <field>
          im_msgv2 = im_tds_data-ztds_tran_type.
      CONTINUE.
      IF 1 = 2.      "For «Where Used» only
        MESSAGE e052(zz_flcm) WITH space space.
      ENDIF.
    ENDIF.

    IF <target> IS INITIAL AND
       im_mandatory IS SUPPLIED AND
       im_mandatory = abap_true.
      IF zero_is_allowed( im_tran_type = im_tds_data-ztds_tran_type    "DV5K970135
                          im_werks     = im_tds_data-werks             "DV5K970135
                          im_fieldname = <field> ) = abap_true.        "DV5K970135
        ASSIGN COMPONENT <field> OF STRUCTURE im_tds_data TO <source>. "DV5K970135
        IF sy-subrc = 0 AND <source> IS INITIAL.                       "DV5K970135
          CALL METHOD set_status
            EXPORTING
              im_status = '01'.
          CALL METHOD log_error
            EXPORTING
              im_msgid = 'ZZ_FLCM'
              im_msgno = '004'
              im_msgty = 'E'
              im_msgv1 = <field>
              im_msgv2 = im_tds_data-ztds_tran_type.
          CONTINUE.
          IF 1 = 2.      "For «Where Used» only
            MESSAGE e004(zz_flcm) WITH space space.
          ENDIF.
        ENDIF.
        UNASSIGN <source>.
      ELSE.                                                  "DV5K970135
        CALL METHOD set_status                               "DV5K970135
          EXPORTING                                          "DV5K970135
            im_status = '01'.                                "DV5K970135
        CALL METHOD log_error                                "DV5K970135
          EXPORTING                                          "DV5K970135
            im_msgid = 'ZZ_FLCM'                             "DV5K970135
            im_msgno = '004'                                 "DV5K970135
            im_msgty = 'E'                                   "DV5K970135
            im_msgv1 = <field>                               "DV5K970135
            im_msgv2 = im_tds_data-ztds_tran_type.           "DV5K970135
        CONTINUE.                                            "DV5K970135
        IF 1 = 2.      "For «Where Used» only                "DV5K970135
          MESSAGE e004(zz_flcm) WITH space space.            "DV5K970135
        ENDIF.                                               "DV5K970135
      ENDIF.
    ENDIF.

    IF <field> IN r_xref_fields.
      l_xref_value = zcl_smint60a_zmm_flcm_tds=>cross_reference(
                         im_fieldname = <field>
                         im_tran_type = im_tds_data-ztds_tran_type
                         im_value = <target> ).
      IF l_xref_value(10) = '**********'.
        CALL METHOD set_status
          EXPORTING
            im_status = '01'.
        CALL METHOD log_error
          EXPORTING
            im_msgid = 'ZZ_FLCM'
            im_msgno = '005'
            im_msgty = 'E'
            im_msgv1 = <target>
            im_msgv2 = <field>.
        IF 1 = 2.      "For «Where Used» only
          MESSAGE e005(zz_flcm) WITH space space.
        ENDIF.
      ELSE.
        <target> = l_xref_value.
      ENDIF.
    ENDIF.

    CALL METHOD edit_data
      EXPORTING
        im_target    = <target>
        im_fieldname = <field>.
  ENDLOOP.

ENDMETHOD.


METHOD move_data.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->MOVE_DATA                     *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_work               TYPE c LENGTH 30.

  CLEAR ch_target.

  IF im_source IS INITIAL.
    RETURN.
  ENDIF.

  CASE im_type_kind.
    WHEN 'C'.
      ch_target = im_source.
    WHEN '>'.
      ch_target = im_source.
      SHIFT ch_target RIGHT DELETING TRAILING space.
      TRANSLATE ch_target USING ' 0'.
    WHEN 'D' OR 'T' OR 'N'.
      l_work = im_source.
      CASE im_type_kind.
        WHEN 'D'.
          TRANSLATE l_work USING '/ - . '.
        WHEN 'T'.
          TRANSLATE l_work USING ': '.
        WHEN 'N'.
          TRANSLATE l_work USING '$ , . - '.
      ENDCASE.
      CONDENSE l_work NO-GAPS.
      IF l_work CO '0123456789 '.
        ch_target = l_work.
      ELSE.
        CALL METHOD set_status
          EXPORTING
            im_status = '01'.
        CALL METHOD log_error
          EXPORTING
            im_msgid = 'ZZ_FLCM'
            im_msgno = '003'
            im_msgty = 'E'
            im_msgv1 = im_source
            im_msgv2 = im_fieldname.
        IF 1 = 2.      "For «Where Used» only
          MESSAGE e003(zz_flcm) WITH space space.
        ENDIF.
      ENDIF.
  ENDCASE.

  IF im_type_kind = 'D' AND ch_target IS NOT INITIAL AND valid_date( ch_target ) = abap_false.
    CALL METHOD set_status
      EXPORTING
        im_status = '01'.
    CALL METHOD log_error
      EXPORTING
        im_msgid = 'ZZ_FLCM'
        im_msgno = '003'
        im_msgty = 'E'
        im_msgv1 = im_source
        im_msgv2 = im_fieldname.
    IF 1 = 2.      "For «Where Used» only
      MESSAGE e003(zz_flcm) WITH space space.
    ENDIF.
  ENDIF.

  IF im_type_kind = 'T' AND valid_time( ch_target ) = abap_false.
    CALL METHOD set_status
      EXPORTING
        im_status = '01'.
    CALL METHOD log_error
      EXPORTING
        im_msgid = 'ZZ_FLCM'
        im_msgno = '003'
        im_msgty = 'E'
        im_msgv1 = im_source
        im_msgv2 = im_fieldname.
    IF 1 = 2.      "For «Where Used» only
      MESSAGE e003(zz_flcm) WITH space space.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD new_object_bol.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>NEW_OBJECT_BOL                *
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
* Chari Flor Supino          2019/07/10          DV5K9A0ARX            *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Add delivery date                                 *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_cancel_rebill      TYPE zztds_cancel_rebill.

  FIELD-SYMBOLS: <lookup>    TYPE t_object_ref.

  CLEAR ch_pointer.

  IF im_tran_type IS INITIAL OR
     im_trml_id IS INITIAL OR
     im_bol_nbr IS INITIAL.
    RAISE not_found.
  ENDIF.

  IF im_cancel_rebill IS SUPPLIED.
    l_cancel_rebill = im_cancel_rebill.
  ELSE.
    l_cancel_rebill = '*'.
  ENDIF.

  IF im_skip_current_batch = abap_false.
    LOOP AT i_object_ref ASSIGNING <lookup>
        WHERE tran_type = im_tran_type AND
              trml_id   = im_trml_id AND
              bol_nbr   = im_bol_nbr.
      IF l_cancel_rebill = '*'.
        ch_pointer = <lookup>-object.
        EXIT.
      ELSE.
        IF <lookup>-object->get_cancel_rebill( ) = l_cancel_rebill.
          ch_pointer = <lookup>-object.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF ch_pointer IS INITIAL.
    CREATE OBJECT ch_pointer
      EXPORTING
        im_tran_type     = im_tran_type
        im_trml_id       = im_trml_id
        im_bol_nbr       = im_bol_nbr
        im_load_data     = im_load_data
        im_cancel_rebill = l_cancel_rebill
        im_tran_dt       = im_tran_dt "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
      EXCEPTIONS
        not_found        = 1
        OTHERS           = 2.
    IF sy-subrc > 0.
      RAISE not_found.
    ENDIF.
  ENDIF.

  IF ch_pointer IS INITIAL.
    RAISE not_found.
  ENDIF.

ENDMETHOD.


METHOD new_object_ref.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>NEW_OBJECT_REF                *
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
* Chari Flor Supino          2019/07/10          DV5K9A0ARX            *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Add delivery date                                 *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_cancel_rebill      TYPE zztds_cancel_rebill.

  FIELD-SYMBOLS: <lookup>    TYPE t_object_ref.

  CLEAR ch_pointer.

  IF im_tran_ref_nbr IS INITIAL.
    RETURN.
  ENDIF.

  IF im_cancel_rebill IS SUPPLIED.
    l_cancel_rebill = im_cancel_rebill.
  ELSE.
    l_cancel_rebill = '*'.
  ENDIF.

  CREATE OBJECT ch_pointer
    EXPORTING
      im_tran_ref_nbr  = im_tran_ref_nbr
      im_tran_type     = im_tran_type
      im_load_data     = im_load_data
      im_cancel_rebill = l_cancel_rebill
      im_tran_dt       = im_tran_dt      "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
    EXCEPTIONS
      not_found        = 1
      OTHERS           = 2.

ENDMETHOD.


METHOD plant_go_live_date.

  TYPES: BEGIN OF lt_go_live,
           werks             TYPE werks_d,
           zact_dt           TYPE dats,
         END OF lt_go_live.

  DATA: sti_go_live          TYPE STANDARD TABLE OF lt_go_live.

  FIELD-SYMBOLS: <go_live>   TYPE lt_go_live.

  IF sti_go_live[] IS INITIAL.
    SELECT werks
           zact_dt
      FROM zmm_flcm_plntdef
      INTO TABLE sti_go_live.
  ENDIF.

  READ TABLE sti_go_live ASSIGNING <go_live>
      WITH KEY werks = im_werks.
  IF sy-subrc = 0.
    IF <go_live>-zact_dt = 0.
      re_go_live = '99991231'.
    ELSE.
      re_go_live = <go_live>-zact_dt.
    ENDIF.
  ELSE.
    CLEAR re_go_live.
  ENDIF.

ENDMETHOD.


METHOD report_errors.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->REPORT_ERRORS                 *
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

* Rob West                  2012/03/14          DV5K970135             *
*                                                                      *
* Short Description: CR199123-T207139                                  *
*                    Move 'MISC' to EQUNR for carrier '202945' with    *
*                    blank EQUNR                                       *
*----------------------------------------------------------------------*
* Rob West                  2011/09/21          DV5K966585             *
*                                                                      *
* Short Description: FLCM Stabilization Defect 544                     *
*                    Do not send email when only message is "Prior     *
*                    to go-live"                                       *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_text             TYPE tdline,
        l_msg              TYPE bal_s_msg,
        li_msg             TYPE bal_t_msg,
        l_msg_count        TYPE i,
        l_msg_handle       TYPE balmsghndl.
*        l_send_email       TYPE xfeld     VALUE abap_false. "DV5K966585

  FIELD-SYMBOLS: <descr>   TYPE t_dd03l,
                 <data>    TYPE ANY.

  IF ch_send_email IS SUPPLIED.                             "DV5K966585
    ch_send_email = abap_false.                             "DV5K966585
  ENDIF.                                                    "DV5K966585

*  IF v_error_count = 0.
  IF v_error_count = 0 AND v_warning_count = 0.              "DV5K970135
    RETURN.
  ENDIF.

*** Start of changes CR199123-T207139                         DV5K970135
*  IF ch_text[] IS NOT INITIAL.
*    APPEND INITIAL LINE TO ch_text.
*    CLEAR l_text WITH '-'.
*    APPEND l_text TO ch_text.
*  ENDIF.
*
*  LOOP AT i_field_descr ASSIGNING <descr>
*      WHERE keyflag = abap_true.
*    ASSIGN COMPONENT <descr>-fieldname OF STRUCTURE v_flcm_tds TO <data>.
*    IF sy-subrc = 0.
*      CONCATENATE <descr>-scrtext ':' INTO l_text.
*      l_text+22 = <data>.
*      APPEND l_text TO ch_text.
*    ENDIF.
*  ENDLOOP.

*  APPEND INITIAL LINE TO ch_text.

  l_msg_handle-log_handle = v_log_handle.
  l_msg_count = v_error_count + v_warning_count + v_other_count.
*  WHILE l_msg_handle-msgnumber < v_error_count.
  WHILE l_msg_handle-msgnumber < l_msg_count.
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
      IF l_msg-msgid = 'ZZ_FLCM' AND
         l_msg-msgno = '029'.
        ch_send_email = abap_false.
        RETURN.
      endif.
      APPEND l_msg TO li_msg.
    ENDIF.
  ENDWHILE.

  ch_send_email = abap_true.
*** End of changes CR199123-T207139                           DV5K970135

  IF ch_text[] IS NOT INITIAL.
    APPEND INITIAL LINE TO ch_text.
    CLEAR l_text WITH '-'.
    APPEND l_text TO ch_text.
  ENDIF.

  LOOP AT i_field_descr ASSIGNING <descr>
      WHERE keyflag = abap_true.
    ASSIGN COMPONENT <descr>-fieldname OF STRUCTURE v_flcm_tds TO <data>.
    IF sy-subrc = 0.
      CONCATENATE <descr>-scrtext ':' INTO l_text.
      l_text+22 = <data>.
      APPEND l_text TO ch_text.
    ENDIF.
  ENDLOOP.

*** Start of changes CR199123-T207139                         DV5K970135
*      IF NOT ( l_msg-msgid = 'ZZ_FLCM' AND                  "DV5K966585
*               l_msg-msgno = '029' ).                       "DV5K966585
*        l_send_email = abap_true.                           "DV5K966585
*      ENDIF.

  IF v_warning_count > 0.
    APPEND INITIAL LINE TO ch_text.
    APPEND 'Warnings:'(t01) TO ch_text.
    LOOP AT li_msg INTO l_msg
        WHERE msgty = 'W'.
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
    ENDLOOP.
    APPEND INITIAL LINE TO ch_text.
  ENDIF.

  IF v_error_count > 0.
    APPEND INITIAL LINE TO ch_text.
    APPEND 'Errors:'(t02) TO ch_text.
    LOOP AT li_msg INTO l_msg
        WHERE msgty = 'E' OR msgty = 'A'.
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
    ENDLOOP.
    APPEND INITIAL LINE TO ch_text.
  ENDIF.

  IF v_other_count > 0.
    APPEND INITIAL LINE TO ch_text.
    APPEND 'Other messages:'(t03) TO ch_text.
    LOOP AT li_msg INTO l_msg
        WHERE NOT ( msgty = 'W' OR msgty = 'E' OR msgty = 'A' ).
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
    ENDLOOP.
    APPEND INITIAL LINE TO ch_text.
  ENDIF.

*  ENDWHILE.

*  IF ch_send_email IS SUPPLIED.                             "DV5K966585
*    ch_send_email = l_send_email.                           "DV5K966585
*  ENDIF.                                                    "DV5K966585
*** End of changes CR199123-T207139                           DV5K970135

ENDMETHOD.


METHOD save_data.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->SAVE_DATA                     *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_in_update_task     TYPE xfeld,
        l_log                TYPE bal_s_log,
        l_stats              TYPE bal_s_scnt,
        li_log_handles       TYPE bal_t_logh.

  FIELD-SYMBOLS: <flcm_key>  TYPE zmm_flcm_tds_key.

  IF v_flcm_tds-ztds_tran_ref_nb IS INITIAL.
    v_flcm_tds-ztds_tran_type = v_tran_type.
    v_flcm_tds-ztds_tran_ref_nb = v_tran_ref_nbr.
  ENDIF.

  IF v_updates_made = abap_true.
*** Update audit fields
    v_flcm_tds-aedat  = sy-datum.
    v_flcm_tds-aezeit = sy-uzeit.
    v_flcm_tds-aename = sy-uname.
    IF v_flcm_tds-erdat IS INITIAL.
      v_flcm_tds-erdat  = v_flcm_tds-aedat.
      v_flcm_tds-erzeit = v_flcm_tds-aezeit.
      v_flcm_tds-ername = v_flcm_tds-aename.
    ENDIF.
*** Update ZMM_FLCM_TDS table
    MODIFY zmm_flcm_tds FROM v_flcm_tds.
    IF sy-subrc > 0.
      RAISE db_error.
    ENDIF.

*** Do COMMIT if requested
    IF im_commit = abap_true.
      COMMIT WORK.
    ENDIF.
    CLEAR v_updates_made.
  ENDIF.

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
    l_log-context-tabname = 'ZMM_FLCM_TDS_KEY'.
    ASSIGN l_log-context-value TO <flcm_key> CASTING.
    MOVE-CORRESPONDING v_flcm_tds TO <flcm_key>.
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

ENDMETHOD.


method SET_DIFF_INV.
*----------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* Nancy Bergeron        261851     2014/06/12          DV5K987403      *
* Add new logic to calculate the difference between TDS and SAP        *
* inventory.   SM-ENH-822                                              *
*----------------------------------------------------------------------*

  IF v_flcm_tds-zsap_inv <> im_sap_inv.
    v_flcm_tds-zsap_inv = im_sap_inv.
    v_updates_made = abap_true.
  ENDIF.

  IF v_flcm_tds-zinv_diff <> im_inv_diff.
    v_flcm_tds-zinv_diff = im_inv_diff.
    v_updates_made = abap_true.
  ENDIF.

*  IF v_flcm_tds-zcumulate_diff <> im_cumulate_diff.            DVSK989018
*    v_flcm_tds-zcumulate_diff = im_cumulate_diff.              DVSK989018
  IF v_flcm_tds-zinv_diff_delta <> im_cumulate_diff.            "DVSK989018
    v_flcm_tds-zinv_diff_delta = im_cumulate_diff.              "DVSK989018
    v_updates_made = abap_true.
  ENDIF.

endmethod.


method SET_DIFF_INV_AND_MESSAGES.
*----------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* Nancy Bergeron        261851     2014/06/12          DV5K987403      *
* Add new logic to calculate the difference between TDS and SAP        *
* inventory.   SM-ENH-822                                              *
*----------------------------------------------------------------------*
  data: lo_object            type ref to zcl_smint60a_zmm_flcm_tds.

  field-symbols: <bapiret2>  type bapiret2 .

  create object lo_object
    exporting
      im_tran_ref_nbr = im_tran_ref_nbr
      im_load_data    = abap_true
    exceptions
      not_found       = 1
      others          = 2.

  if sy-subrc > 0.
    raise error_in_save.
  endif.

  if  im_sap_inv       is supplied
  AND im_inv_diff      is supplied
  AND im_cumulate_diff is supplied.
    call method lo_object->set_diff_inv
      exporting
        im_sap_inv       = im_sap_inv
        im_inv_diff      = im_inv_diff
        im_cumulate_diff = im_cumulate_diff.
  endif.

  if im_messages is supplied and
     im_messages[] is not initial.
    loop at im_messages assigning <bapiret2>.
      call method lo_object->log_error
        exporting
          im_msgid = <bapiret2>-id
          im_msgno = <bapiret2>-number
          im_msgty = <bapiret2>-type
          im_msgv1 = <bapiret2>-message_v1
          im_msgv2 = <bapiret2>-message_v2
          im_msgv3 = <bapiret2>-message_v3
          im_msgv4 = <bapiret2>-message_v4.
    endloop.
  endif.

  call method lo_object->save_data
    exporting
      im_commit  = im_commit
    exceptions
      db_error   = 1
      sbal_error = 2
      others     = 3.
  if sy-subrc > 0.
    raise error_in_save.
  endif.

  call method lo_object->free_single.

endmethod.


METHOD set_status.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->SET_STATUS                    *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  IF v_flcm_tds-ztran_stus <> im_status.
    v_flcm_tds-ztran_stus = im_status.
    v_updates_made = abap_true.
  ENDIF.

ENDMETHOD.


METHOD set_status_and_messages.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>SET_STATUS_AND_MESSAGES       *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: lo_object            TYPE REF TO zcl_smint60a_zmm_flcm_tds.

  FIELD-SYMBOLS: <bapiret2>  TYPE bapiret2.

  CREATE OBJECT lo_object
    EXPORTING
      im_tran_ref_nbr = im_tran_ref_nbr
      im_load_data    = abap_true
    EXCEPTIONS
      not_found       = 1
      OTHERS          = 2.

  IF sy-subrc > 0.
    RAISE error_in_save.
  ENDIF.

  IF im_tran_substatus IS SUPPLIED.
    CALL METHOD lo_object->set_status_substatus
      EXPORTING
        im_status    = im_tran_status
        im_substatus = im_tran_substatus.
  ELSE.
    CALL METHOD lo_object->set_status
      EXPORTING
        im_status = im_tran_status.
  ENDIF.

  IF im_messages IS SUPPLIED AND
     im_messages[] IS NOT INITIAL.
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
  ENDIF.

  CALL METHOD lo_object->save_data
    EXPORTING
      im_commit  = im_commit
    EXCEPTIONS
      db_error   = 1
      sbal_error = 2
      OTHERS     = 3.
  IF sy-subrc > 0.
    RAISE error_in_save.
  ENDIF.

  CALL METHOD lo_object->free_single.

ENDMETHOD.


METHOD set_status_substatus.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->SET_STATUS_SUBSTATUS          *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  IF v_flcm_tds-ztran_stus <> im_status.
    v_flcm_tds-ztran_stus = im_status.
    v_updates_made = abap_true.
  ENDIF.

  IF v_flcm_tds-ztran_sub_stus <> im_substatus.
    v_flcm_tds-ztran_sub_stus = im_substatus.
    v_updates_made = abap_true.
  ENDIF.

ENDMETHOD.


METHOD show_messages_in_popup.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>SHOW_MESSAGES_IN_POPUP        *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_profile            TYPE bal_s_prof,
        li_log_handles       TYPE bal_t_logh.

  FIELD-SYMBOLS: <hdr>       TYPE balhdr.

  LOOP AT i_log_headers ASSIGNING <hdr>
      WHERE extnumber(10) = im_tran_ref_nbr.
    COLLECT <hdr>-log_handle INTO li_log_handles.
  ENDLOOP.

  IF li_log_handles[] IS INITIAL.
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

  CALL METHOD zcl_smint60a_zmm_flcm_tds=>build_tree_fcat
    CHANGING
      ch_fcat = l_profile-lev1_fcat[]
      ch_sort = l_profile-lev1_sort[].

  CALL METHOD zcl_smint60a_zmm_flcm_tds=>build_grid_fcat
    CHANGING
      ch_fcat = l_profile-mess_fcat[].

  CONCATENATE 'Transaction Reference:'(tr1) im_tran_ref_nbr
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


METHOD transfer_data.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->TRANSFER_DATA                 *
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
* Chari Flor Supino          2019/07/10          DV5K9A0ARX            *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Add delivery date                                 *
*----------------------------------------------------------------------*
* Rob West                  2012/03/14          DV5K970135             *
*                                               DV5K970866             *
*                                                                      *
* Short Description: CR199123-T207139                                  *
*                    Move 'MISC' to EQUNR for carrier '202945' with    *
*                    blank EQUNR                                       *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K968478             *
*                                                                      *
* Short Description: Defect 678: Add text 'MISC-' to equipment number  *
*                    for fuel events 520 and 530 in account '202945'.  *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: li_mandatory         TYPE fieldname_tab,
        li_optional          TYPE fieldname_tab.

  FIELD-SYMBOLS: <objref>    TYPE t_object_ref,
                 <dd03l>     TYPE t_dd03l,
                 <source>    TYPE c,
                 <target>    TYPE ANY.

*--------------------------------------------------------------------*
*** Move all data from Interface structure to ZMM_FLCM_TDS structure
  MOVE-CORRESPONDING im_tds_data TO v_flcm_tds.

*--------------------------------------------------------------------*

  LOOP AT i_field_descr ASSIGNING <dd03l>   "Move non-character data
      WHERE inttype = '>' OR                "again to apply de-editing
            inttype = 'N' OR
            inttype = 'D' OR
            inttype = 'T'.
    UNASSIGN: <source>,
              <target>.
    ASSIGN COMPONENT <dd03l>-fieldname OF STRUCTURE im_tds_data  TO <source>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT <dd03l>-fieldname OF STRUCTURE v_flcm_tds TO <target>.
      IF sy-subrc = 0.
        CALL METHOD move_data
          EXPORTING
            im_type_kind = <dd03l>-inttype
            im_source    = <source>
            im_fieldname = <dd03l>-fieldname
          CHANGING
            ch_target    = <target>.
      ENDIF.
    ENDIF.
  ENDLOOP.

*--------------------------------------------------------------------*
*** Check for CN relevant transaction
  IF im_tds_data-zprod_owner <> c_prod_owner.
    CALL METHOD set_status
      EXPORTING
        im_status = '01'.
    CALL METHOD log_error
      EXPORTING
        im_msgid = 'ZZ_FLCM'
        im_msgno = '001'
        im_msgty = 'E'
        im_msgv1 = im_tds_data-zprod_owner.
    IF 1 = 2.      "For «Where Used» only
      MESSAGE e001(zz_flcm) WITH space.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
*** Get the configuration for the transaction type
  CALL METHOD zcl_smint60a_zmm_flcm_tds=>get_field_config
    EXPORTING
      im_tran_type = im_tds_data-ztds_tran_type
    CHANGING
      ch_mandatory = li_mandatory[]
      ch_optional  = li_optional[]
    EXCEPTIONS
      invalid_type = 1
      OTHERS       = 2.

*--------------------------------------------------------------------*
*** Check for miscellaneous fuelling event and update equipment field
*** CR199123 Task 207139
  IF v_flcm_tds-ztds_tran_type = c_530  AND
     v_flcm_tds-zcarrier = c_misc_fuelling AND
     v_flcm_tds-equnr IS INITIAL.
    v_flcm_tds-equnr = 'MISC'.
    CALL METHOD log_error
      EXPORTING
        im_msgid = 'ZZ_FLCM'
        im_msgno = '098'
        im_msgty = 'W'.
  ENDIF.

*--------------------------------------------------------------------*
*** Perform data edits
  IF sy-subrc = 0.
    CALL METHOD move_and_edit
      EXPORTING
        im_tds_data  = im_tds_data
        im_fields    = li_mandatory[]
        im_mandatory = abap_true.
    CALL METHOD move_and_edit
      EXPORTING
        im_tds_data = im_tds_data
        im_fields   = li_optional[].
  ELSE.
    CALL METHOD set_status
      EXPORTING
        im_status = '01'.
    CALL METHOD log_error
      EXPORTING
        im_msgid = 'ZZ_FLCM'
        im_msgno = '007'
        im_msgty = 'E'
        im_msgv1 = im_tds_data-ztds_tran_type.
    IF 1 = 2.      "For «Where Used» only
      MESSAGE e007(zz_flcm) WITH space.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
*** Check transaction date against plant go-live date
  IF plant_go_live_date( v_flcm_tds-werks ) > v_flcm_tds-ztds_tran_dt.
    CALL METHOD set_status
      EXPORTING
        im_status = '01'.
    CALL METHOD log_error
      EXPORTING
        im_msgid = 'ZZ_FLCM'
        im_msgno = '029'
        im_msgty = 'E'
        im_msgv1 = v_flcm_tds-werks.
    IF 1 = 2.      "For «Where Used» only
      MESSAGE e029(zz_flcm) WITH space space.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
*** Check for miscellaneous fuelling event and update equipment field
*** FLCM Defect 678
  IF ( v_flcm_tds-ztds_tran_type = c_520 OR
       v_flcm_tds-ztds_tran_type = c_530 ) AND
       v_flcm_tds-zcarrier = c_misc_fuelling.
    IF v_flcm_tds-equnr <> 'MISC'.                          "DV5K970135
      CONCATENATE 'MISC-' v_flcm_tds-equnr INTO v_flcm_tds-equnr.
    ENDIF.                                                  "DV5K970135
  ENDIF.

*--------------------------------------------------------------------*
*** Update Object Reference table with Terminal and BOL
*** transaction type and cancel/rebill
  READ TABLE i_object_ref ASSIGNING <objref>
      WITH KEY object = me.
  IF sy-subrc = 0.
    <objref>-bol_nbr = im_tds_data-ztds_bol_nbr.
    <objref>-trml_id = v_trml_id = im_tds_data-ztds_trml_id.
    <objref>-tran_type = im_tds_data-ztds_tran_type.
    <objref>-cancel_rebill = im_tds_data-ztds_canc_rbil.
    <objref>-tran_dt       = im_tds_data-ztds_tran_dt. "I - TXT22260 - DV5K9A0ARX - CR302137 TK365122
  ENDIF.

  v_updates_made = abap_true.

ENDMETHOD.


METHOD valid_date.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->VALID_DATE                    *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  CONSTANTS: l_hex_three     TYPE x LENGTH 2 VALUE 3.

  DATA: l_max_days           TYPE n LENGTH 2,
        l_year_test          TYPE x LENGTH 2.   "for bit compare

  re_valid = abap_true.

  IF im_date+0(4) <= '2000' OR
     im_date+0(4) > '2100'.
    re_valid = abap_false.
    RETURN.
  ENDIF.

*** Note: Leap Centuries (2000, 2400, 2800.....) are not considrered
***       in this logic
  CASE im_date+4(2).
    WHEN '01' OR '03' OR '05' OR '07' OR '08' OR '10' OR '12'.
      l_max_days = 31.
    WHEN '04' OR '06' OR '09' OR '11'.
      l_max_days = 30.
    WHEN '02'.
      l_year_test = im_date+0(4).
      l_year_test = l_year_test BIT-AND l_hex_three.
      IF l_year_test IS INITIAL.
        l_max_days = 29.         "Set as leap year
      ELSE.
        l_max_days = 28.         "Not a leap year
      ENDIF.
    WHEN OTHERS.
      re_valid = abap_false.
      RETURN.
  ENDCASE.

  IF im_date+6(2) < '01' OR
     im_date+6(2) > l_max_days.
    re_valid = abap_false.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD valid_time.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS->VALID_TIME                    *
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
* Rob West                  2011/03/01          DV5K961204             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  re_valid = abap_true.

  IF im_time+0(2) > '23' OR
     im_time+2(2) > '59' OR
     im_time+4(2) > '59'.
    re_valid = abap_false.
  ENDIF.

ENDMETHOD.


METHOD zero_is_allowed.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMINT60A_ZMM_FLCM_TDS=>ZERO_IS_ALLOWED               *
* Title    :  Receive data from TDS and store it to ZMM_FLCM_TDS       *
* Work Unit:  SM-INT-60A-001                                           *
* Created by: Rob West                                                 *
* Created on: February 2012                                            *
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
* Rob West                  2012/03/14          DV5K970135             *
*                                                                      *
* Short Description: CR199123-T207139                                  *
*                    Move 'MISC' to EQUNR for carrier '202945' with    *
*                    blank EQUNR                                       *
*----------------------------------------------------------------------*

  TYPES: BEGIN OF lt_zero_allowed,
           ztds_tran_type    TYPE zztds_tran_type,
           werks             TYPE werks_d,
           fieldname         TYPE fieldname,
         END OF lt_zero_allowed.

  STATICS: sti_zero_allowed  TYPE STANDARD TABLE OF lt_zero_allowed.

  FIELD-SYMBOLS: <zero>      TYPE lt_zero_allowed,
                 <parms>     TYPE t_parms.

  re_zero_ok = abap_false.

  IF sti_zero_allowed[] IS INITIAL.
    LOOP AT i_parms ASSIGNING <parms>
          WHERE zparm_nm+0(3) CO c_numeric AND zparm_nm+3(1) = '-' AND zparm_nm+4(4) CO c_numeric AND zparm_nm+8 IS INITIAL.
      APPEND INITIAL LINE TO sti_zero_allowed ASSIGNING <zero>.
      <zero>-ztds_tran_type = <parms>-zparm_nm+0(3).
      <zero>-werks          = <parms>-zparm_nm+4(4).
      <zero>-fieldname      = <parms>-zval_from.
    ENDLOOP.
    DELETE i_parms WHERE zparm_nm+0(3) CO c_numeric AND zparm_nm+3(1) = '-' AND zparm_nm+4(4) CO c_numeric AND zparm_nm+8 IS INITIAL.
  ENDIF.

  READ TABLE sti_zero_allowed TRANSPORTING NO FIELDS
      WITH KEY ztds_tran_type = im_tran_type
               werks          = im_werks
               fieldname      = im_fieldname.
  IF sy-subrc = 0.
    re_zero_ok = abap_true.
  ENDIF.

ENDMETHOD.
ENDCLASS.
