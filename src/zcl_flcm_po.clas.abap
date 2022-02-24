class ZCL_FLCM_PO definition
  public
  create public .

*"* public components of class ZCL_FLCM_PO
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  constants MC_ERROR_TYPES type CHAR3 value 'EAX'. "#EC NOTEXT
  constants MC_MSGID_FLCM type SY-MSGID value 'ZZ_FLCM'. "#EC NOTEXT
  constants MC_MSGNO_062 type SY-MSGNO value '062'. "#EC NOTEXT
  constants MC_MSGNO_063 type SY-MSGNO value '063'. "#EC NOTEXT
  constants MC_MSGTY_ERROR type SY-MSGTY value 'E'. "#EC NOTEXT
  constants MC_SUB_STATUS_NONE type ZMM_FLCM_TDS-ZTRAN_SUB_STUS value '000'. "#EC NOTEXT

  methods CONSTRUCTOR
    raising
      ZCX_FLCM_ERROR .
  methods CREATE
    importing
      !IS_POHEADER type BAPIMEPOHEADER
      !IS_POHEADERX type BAPIMEPOHEADERX optional
      !IT_POITEM type BAPIMEPOITEM_TP optional
      !IT_POITEMX type BAPIMEPOITEMX_TP optional
      !IT_POSCHEDULE type BAPIMEPOSCHEDULE_TP optional
      !IT_POSCHEDULEX type BAPIMEPOSCHEDULX_TP optional
    exporting
      !ED_EXPPURCHASEORDER type BAPIMEPOHEADER-PO_NUMBER
      !ET_RETURN type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  methods CHANGE
    importing
      !IV_PURCHASEORDER type BAPIMEPOHEADER-PO_NUMBER
      !IS_POHEADER type BAPIMEPOHEADER optional
      !IS_POHEADERX type BAPIMEPOHEADERX optional
      !IT_POITEM type BAPIMEPOITEM_TP optional
      !IT_POITEMX type BAPIMEPOITEMX_TP optional
      !IT_POSCHEDULE type BAPIMEPOSCHEDULE_TP optional
      !IT_POSCHEDULEX type BAPIMEPOSCHEDULX_TP optional
    exporting
      !ET_RETURN type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
protected section.
*"* protected components of class ZCL_FLCM_PO
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_FLCM_PO
*"* do not include other source files here!!!

  data POHEADER type BAPIMEPOHEADER .
  data POHEADERX type BAPIMEPOHEADERX .
  data POITEM type BAPIMEPOITEM_TP .
  data POITEMX type BAPIMEPOITEMX_TP .
  data POSCHEDULE type BAPIMEPOSCHEDULE_TP .
  data POSCHEDULEX type BAPIMEPOSCHEDULX_TP .
  data PURCHASEORDER type BAPIMEPOHEADER-PO_NUMBER .
ENDCLASS.



CLASS ZCL_FLCM_PO IMPLEMENTATION.


METHOD change.
  DATA: lwa_return TYPE bapiret2,
        lv_committed TYPE abap_bool.
* Initialize local fields
  CLEAR: lwa_return,
         lv_committed.
* BAPI Parameters
  purchaseorder = iv_purchaseorder.
  poheader  = is_poheader.
  poheaderx = is_poheaderx.
  poitem[]  = it_poitem[].
  poitemx[] = it_poitemx[].
  IF it_poschedule IS SUPPLIED.
    poschedule[] = it_poschedule[].
  ENDIF.
  IF it_poschedulex IS SUPPLIED.
    poschedulex[] = it_poschedulex[].
  ENDIF.
* Execute BAPI
  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      purchaseorder = purchaseorder
      poheader      = poheader
      poheaderx     = poheaderx
    TABLES
      return        = et_return
      poitem        = poitem
      poitemx       = poitemx
      poschedule    = poschedule
      poschedulex   = poschedulex.
  IF ( zcl_flcm_services=>find_error( et_return ) EQ abap_false ).
*   Commit changes
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = abap_true
      IMPORTING
        return = lwa_return.
    IF ( NOT lwa_return-type CA mc_error_types ).
*     Success
      lv_committed = abap_true.
    ELSE.
*     Failure
      lv_committed = abap_false.
      APPEND lwa_return TO et_return.
    ENDIF.
  ENDIF.
* Update failure
  IF ( lv_committed = abap_false ).
*   Rollback changes
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = lwa_return.
    IF ( lwa_return-type CA mc_error_types ).
      APPEND lwa_return TO et_return.
    ENDIF.
*   Raise exception
    IF ( 1 = 2 ). MESSAGE e063(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm      "ZZ_FLCM
        msgty    = mc_msgty_error     "E
        msgno    = mc_msgno_063       "Change Po bapi has failed
        sub_stus = mc_sub_status_none "000
        msgtab   = et_return.
  ENDIF.
ENDMETHOD.


METHOD constructor.
  CLEAR:   purchaseorder,
           poheader,
           poheaderx.
  REFRESH: poitem,
           poitemx,
           poschedule,
           poschedulex.
ENDMETHOD.


METHOD create.
  DATA: lwa_return   TYPE bapiret2,
        lv_committed TYPE abap_bool.
* Initialize local fields
  CLEAR: lwa_return,
         lv_committed.
* BAPI Parameters
  poheader  = is_poheader.
  poheaderx = is_poheaderx.
  poitem[]  = it_poitem[].
  poitemx[] = it_poitemx[].
  IF it_poschedule IS SUPPLIED.
    poschedule[] = it_poschedule[].
  ENDIF.
  IF it_poschedulex IS SUPPLIED.
    poschedulex[] = it_poschedulex[].
  ENDIF.
* Execute BAPI
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader         = poheader
      poheaderx        = poheaderx
    IMPORTING
      exppurchaseorder = ed_exppurchaseorder
    TABLES
      return           = et_return
      poitem           = poitem
      poitemx          = poitemx
      poschedule       = poschedule
      poschedulex      = poschedulex.
  IF ( zcl_flcm_services=>find_error( et_return ) EQ abap_false ).
*   Commit changes
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = abap_true
      IMPORTING
        return = lwa_return.
    IF ( NOT lwa_return-type CA mc_error_types ).
*     Success
      lv_committed = abap_true.
    ELSE.
*     Failure
      lv_committed = abap_false.
      APPEND lwa_return TO et_return.
    ENDIF.
  ENDIF.
* Update failure
  IF ( lv_committed = abap_false ).
*   Rollback changes
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = lwa_return.
    IF ( lwa_return-type CA mc_error_types ).
      APPEND lwa_return TO et_return.
    ENDIF.
*   Raise exception
    IF ( 1 = 2 ). MESSAGE e062(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm      "ZZ_FLCM
        msgty    = mc_msgty_error     "E
        msgno    = mc_msgno_062       "Create Po bapi has failed
        sub_stus = mc_sub_status_none "000
        msgtab   = et_return.
  ENDIF.
ENDMETHOD.
ENDCLASS.
