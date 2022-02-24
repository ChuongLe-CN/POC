class ZCL_FLCM_SERVICES definition
  public
  final
  create public .

*"* public components of class ZCL_FLCM_SERVICES
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  constants MC_MOVE_TYPE_101 type BAPI2017_GM_ITEM_CREATE-MOVE_TYPE value '101' ##NO_TEXT.
  constants MC_SUCCESS type BAPIRET2-TYPE value 'S' ##NO_TEXT.
  constants MC_TDS type CHAR03 value 'TDS' ##NO_TEXT.
  class-data M_OVERRIDE_OPEN_PERIOD_FLAG type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  constants MC_502 type SYMSGNO value '502' ##NO_TEXT.
  constants MC_710 type SYMSGNO value '710' ##NO_TEXT.
  constants MC_CAR type CHAR03 value 'CAR' ##NO_TEXT.
  constants MC_F99 type TDID value 'F99' ##NO_TEXT.
  constants MC_GROSS_VOL type CHAR05 value 'GROSS' ##NO_TEXT.
  constants MC_ERROR_TYPES type CHAR3 value 'EAX' ##NO_TEXT.
  constants MC_GMCODE_GOODS_ISSUE type BAPI2017_GM_CODE-GM_CODE value '03' ##NO_TEXT.
  constants MC_GMCODE_TRANSFER_POSTING type BAPI2017_GM_CODE-GM_CODE value '04' ##NO_TEXT.
  constants MC_GR_RCPT type BAPI2017_GM_ITEM_CREATE-GR_RCPT value 'TG/L' ##NO_TEXT.
  constants MC_GR_RCPT_PHG_L type BAPI2017_GM_ITEM_CREATE-GR_RCPT value 'PHG/L' ##NO_TEXT.
  constants MC_M7 type SYMSGID value 'M7' ##NO_TEXT.
  constants MC_MSGID_FLCM type SY-MSGID value 'ZZ_FLCM' ##NO_TEXT.
  constants MC_MSGTY_ERROR type SY-MSGTY value 'E' ##NO_TEXT.
  constants MC_MSGTY_SUCCESS type SY-MSGTY value 'S' ##NO_TEXT.
  constants MC_SLASH type CHAR1 value '/' ##NO_TEXT.
  constants MC_STATUS_ERROR type ZMM_FLCM_TDS-ZTRAN_STUS value '04' ##NO_TEXT.
  constants MC_STATUS_NOT_RELEVANT type ZMM_FLCM_TDS-ZTRAN_STUS value '02' ##NO_TEXT.
  constants MC_STATUS_PROCESSED type ZMM_FLCM_TDS-ZTRAN_STUS value '03' ##NO_TEXT.
  constants MC_STATUS_READY type ZMM_FLCM_TDS-ZTRAN_STUS value '00' ##NO_TEXT.
  constants MC_SUB_STATUS_NONE type ZMM_FLCM_TDS-ZTRAN_SUB_STUS value '000' ##NO_TEXT.
  constants MC_SUB_STATUS_PHYG_FAIL type ZMM_FLCM_TDS-ZTRAN_SUB_STUS value '001' ##NO_TEXT.
  constants MC_SUB_STATUS_PHYI_FAIL type ZMM_FLCM_TDS-ZTRAN_SUB_STUS value '002' ##NO_TEXT.
  constants MC_SUB_STATUS_PHYI_NOTAPPLIED type ZMM_FLCM_TDS-ZTRAN_SUB_STUS value '003' ##NO_TEXT.
  constants MC_XREF_GMCODE type ZMM_FLCM_XREF-ZVAR_NM value 'GMCODE' ##NO_TEXT.
  constants MC_XREF_MOVE_TYPE type ZMM_FLCM_XREF-ZVAR_NM value 'BWART' ##NO_TEXT.
  constants MC_PROGNAME_819 type PROGNAME value 'SMENH819' ##NO_TEXT.
  constants MC_PROGNAME_817 type PROGNAME value 'SMENH817' ##NO_TEXT.
  constants MC_MAX_AMOUNT_PER_LITRE type RVARI_VNAM value 'MAX_AMOUNT_PER_LITRE' ##NO_TEXT.
  constants MC_MAX_AMOUNT_PER_GALLON type RVARI_VNAM value 'MAX_AMOUNT_PER_GALLON' ##NO_TEXT.
  constants MC_COND_CHECK type PROGNAME value 'ACTIVATE_CONDITION_CHECK' ##NO_TEXT.
  constants MC_USD type WAERS value 'USD' ##NO_TEXT.
  constants MC_CAD type WAERS value 'CAD' ##NO_TEXT.
  constants MC_LITRE type MEINS value 'L' ##NO_TEXT.
  constants MC_GALLON type MEINS value 'GLL' ##NO_TEXT.
  constants MC_GALLON_EXTR type MEINS value 'GAL' ##NO_TEXT.

  class-methods M_VALIDATE_PLANT
    importing
      value(IS_TRANS) type ZMM_FLCM_TDS
      value(ID_SUB_STUS) type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    raising
      ZCX_FLCM_ERROR .
  class-methods IS_FOR_VALIDATION
    returning
      value(RD_RESULT) type ABAP_BOOL .
  class-methods CHANGE_PO
    importing
      !ID_EBELN type EKKO-EBELN
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS optional
    exporting
      !ET_RETURN type BAPIRET2TAB
    changing
      !CT_ITEMS type BAPIMEPOITEM_TP
      !CT_ITEMSX type BAPIMEPOITEMX_TP
      !CT_SCHEDULES type BAPIMEPOSCHEDULE_TP
      !CT_SCHEDULESX type BAPIMEPOSCHEDULX_TP
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_GOODS_MVMT
    importing
      !IS_GMCODE type BAPI2017_GM_CODE
      !IS_HEADER type BAPI2017_GM_HEAD_01
      !IT_ITEM type BAPI2017_GM_ITEM_CREATE_T
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
      !IM_OVERRIDE_PROCESSING_DATE type ZZTDS_TRANSACTION_DT default '00000000'
    exporting
      !ED_MAT_DOC type BAPI2017_GM_HEAD_RET-MAT_DOC
      !ED_DOC_YEAR type BAPI2017_GM_HEAD_RET-DOC_YEAR
      !ET_RETURN type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_GOODS_MVMT_GAIN_LOSS
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    returning
      value(RT_RETURN) type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_GOODS_MVMT_END_OF_DAY
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    returning
      value(RT_RETURN) type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_GOODS_MVMT_PLANT2PLANT
    importing
      !ID_MOVE_TYPE type BAPI2017_GM_ITEM_CREATE-MOVE_TYPE
      !IS_TRANS type ZMM_FLCM_TDS
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    returning
      value(RT_RETURN) type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_GOODS_MVMT_PLANT2PLANT2
    importing
      !ID_MOVE_TYPE type BAPI2017_GM_ITEM_CREATE-MOVE_TYPE
      !IS_TRANS type ZMM_FLCM_TDS
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    returning
      value(RT_RETURN) type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_GOODS_MVMT_GI_FUEL
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    returning
      value(RT_RETURN) type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_GOODS_MVMT_TDS_PO
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !IS_UNIQUE_PO type ZMM_FLCM_UNIQUE_PO
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS optional
    returning
      value(RT_RETURN) type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_GOODS_MVMT_TRANSPORT_PO
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !IS_UNIQUE_PO type ZMM_FLCM_UNIQUE_PO
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS optional
    returning
      value(RT_RETURN) type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_NEW_TRANSPORT_PO
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !IS_CONTRACT type ZMM_FLCM_CONTRACT
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS optional
    exporting
      value(ET_RETURN) type BAPIRET2_T
      !ED_EBELN type EKKO-EBELN
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_NEW_TDS_PO
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !IS_CONTRACT type ZMM_FLCM_CONTRACT
      !ID_CONF_CTRL type ZMM_FLCM_PARMS-ZVAL_FROM
    exporting
      value(ET_RETURN) type BAPIRET2_T
      !ED_EBELN type EKKO-EBELN
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_PO
    importing
      !IS_HEADER type BAPIMEPOHEADER
      !IS_HEADERX type BAPIMEPOHEADERX
      !ID_TEST type CHAR1 optional
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS optional
    exporting
      !ET_RETURN type BAPIRET2TAB
      !ED_EBELN type EKKO-EBELN
    changing
      !CT_ITEMS type BAPIMEPOITEM_TP
      !CT_ITEMSX type BAPIMEPOITEMX_TP
      !CT_POSCHEDULE type BAPIMEPOSCHEDULE_TP
      !CT_POSCHEDULEX type BAPIMEPOSCHEDULX_TP
      !CT_POTEXTHEADER type BAPIMEPOTEXTHEADER_TP
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_PHYSINV_ADJUSTMENT
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !IT_MARAMARC type ZMM_FLCM_MARAMARC_T
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    returning
      value(RT_RETURN) type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_PHYSINV_CREATE_MULT
    importing
      !IS_HEADER type BAPI_PHYSINV_CREATE_HEAD
      !IT_ITEM type ZBAPI_PHYSINV_CREATE_ITEMS_T
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    exporting
      !ET_RETURN type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  class-methods CREATE_PHYSINV_COUNT
    importing
      !IS_HEADER type BAPI_PHYSINV_CREATE_HEAD
      !IT_ITEM type ZBAPI_PHYSINV_COUNT_ITEMS_T
      !ID_INV_DOC type BSEG-BELNR
      !ID_POST_DATE type ZMM_FLCM_TDS-ZTDS_FOLIO_FRZDT
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    exporting
      !ET_RETURN type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  class-methods CONFIRM_PO .
  class-methods CREATE_PHYSINV_POSTDIFF
    importing
      !IS_HEADER type BAPI_PHYSINV_CREATE_HEAD
      !IT_ITEM type ZBAPI_PHYSINV_POST_ITEMS_T
      !ID_INV_DOC type BSEG-BELNR
      !ID_POST_DATE type ZMM_FLCM_TDS-ZTDS_FOLIO_FRZDT
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    exporting
      !ET_RETURN type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  class-methods CONVERT_IN_TRANSIT_STOCK
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !IT_MARAMARC type ZMM_FLCM_MARAMARC_T
    returning
      value(RD_INV_TOT_COUNT) type ERFMG
    raising
      ZCX_FLCM_ERROR .
  class-methods DELETE_PHYSINV_RECORD
    importing
      !ID_INV_DOC type BSEG-BELNR
      !ID_POST_DATE type ZMM_FLCM_TDS-ZTDS_FOLIO_FRZDT
    exporting
      value(RT_RETURN) type BAPIRET2_T
    raising
      ZCX_FLCM_ERROR .
  class-methods DB_UPDATE_TRANS_NOT_RELEVANT
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !IO_EXCEPTION type ref to ZCX_FLCM_ERROR
    raising
      ZCX_FLCM_ERROR .
  class-methods DB_UPDATE_TRANS_ERROR
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !IO_EXCEPTION type ref to ZCX_FLCM_ERROR
    changing
      !IT_MSG type BAPIRET2TAB optional
    raising
      ZCX_FLCM_ERROR .
  class-methods DB_UPDATE_TRANS_SUCCESS
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !IT_MSG type BAPIRET2TAB optional
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    raising
      ZCX_FLCM_ERROR .
  class-methods DB_UPDATE_TRANS_DIFF_INV
    importing
      !IT_TRANS type ZMM_FLCM_TDS
      !IT_MSG type BAPIRET2TAB optional
      !ID_SAP_INV type ZMM_FLCM_TDS-ZSAP_INV optional
      !ID_INV_DIFF type ZMM_FLCM_TDS-ZINV_DIFF optional
      !ID_CUMULATE_DIFF type ZMM_FLCM_TDS-ZINV_DIFF_DELTA optional
    raising
      ZCX_FLCM_ERROR .
  class-methods FIND_ERROR
    importing
      !IT_MSG type BAPIRET2TAB
    returning
      value(RD_RESULT) type ABAP_BOOL .
  class-methods FIND_INV_DOC
    importing
      !IT_MSG type BAPIRET2TAB
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    returning
      value(ED_INV_DOC) type BSEG-BELNR
    raising
      ZCX_FLCM_ERROR .
  class-methods FORMAT_VALUE
    importing
      !ID_ANYVALUE type DATA
    returning
      value(RD_OUTPUT) type SY-MSGV1 .
  class-methods GET_GMCODE
    importing
      !ID_MOVE_TYPE type BAPI2017_GM_ITEM_CREATE-MOVE_TYPE
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    returning
      value(RD_GM_CODE) type BAPI2017_GM_CODE
    raising
      ZCX_FLCM_ERROR .
  class-methods GET_MOVEMENT_TYPE
    importing
      !ID_TRANS type ZMM_FLCM_TDS-ZTDS_TRAN_TYPE
      !ID_CANC_RBIL type ZMM_FLCM_TDS-ZTDS_CANC_RBIL optional
      !ID_VOL_SIGN type ZMM_FLCM_TDS-ZTDS_VOL_SIGN optional
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    returning
      value(RD_MOVE_TYPE) type BAPI2017_GM_ITEM_CREATE-MOVE_TYPE
    raising
      ZCX_FLCM_ERROR .
  class-methods GET_PO_DETAIL
    importing
      !ID_EBELN type EKKO-EBELN
    exporting
      !ET_ITEMS type BAPIEKPO_TP
      !ET_SCHEDULES type BAPIEKET_TP
      !ET_RETURN type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
  class-methods GET_STORAGE_LOCATION
    importing
      !ID_WERKS type ZMM_FLCM_PLNTDEF-WERKS
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    returning
      value(RD_LGORT) type ZMM_FLCM_PLNTDEF-LGORT
    raising
      ZCX_FLCM_ERROR .
  class-methods GET_STORAGE_LOCATION_RECEIVING
    importing
      !ID_WERKS type ZMM_FLCM_PLNTDEF-WERKS
    returning
      value(RD_LGORT) type ZMM_FLCM_PLNTDEF-LGORT
    raising
      ZCX_FLCM_ERROR .
  class-methods IS_OPEN_PERIOD
    importing
      !ID_TRAN_REF_NB type ZMM_FLCM_TDS-ZTDS_TRAN_REF_NB
      !ID_WERKS type ZMM_FLCM_TDS-WERKS
      !ID_ZTDS_FOLIO_YR type ZMM_FLCM_TDS-ZTDS_FOLIO_YR
      !ID_ZTDS_FOLIO_MTH type ZMM_FLCM_TDS-ZTDS_FOLIO_MTH
    returning
      value(RD_RESULT) type ABAP_BOOL
    raising
      ZCX_FLCM_ERROR .
  class-methods FI_OPEN_PERIOD_CHECK
    importing
      !IM_DATUM type DATS
    returning
      value(RE_PERIOD_OPEN) type XFELD .
  class-methods UPDATE_FUEL_PO
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !ID_MOVE_TYPE type ZMM_FLCM_TDS-ZTRAN_SUB_STUS optional
      !ID_EBELN type EKKO-EBELN
      !ID_EBELP type EKPO-EBELP
      !ID_EBTYP type ZMM_FLCM_PARMS-ZVAL_FROM
      !ID_BEDNR type EKPO-BEDNR
      !ID_MATNR type EKPO-MATNR
      !ID_MEINS type EKPO-MEINS
      !ID_UPDATE_EBTYP type ABAP_BOOL
    returning
      value(RT_RETURN) type BAPIRET2_T
    raising
      ZCX_FLCM_ERROR .
  class-methods UPDATE_PO
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !ID_EBELN type EKKO-EBELN
      !ID_EBELP type EKPO-EBELP
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS optional
    returning
      value(RT_RETURN) type BAPIRET2_T
    raising
      ZCX_FLCM_ERROR .
  class-methods REVERSE_PO
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !IS_CONTRACT type ZMM_FLCM_CONTRACT
      !IS_UNIQUE_PO type ZMM_FLCM_UNIQUE_PO
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS optional
      !ID_CONF_CTRL type ZMM_FLCM_PARMS-ZVAL_FROM default ' '
    returning
      value(RT_RETURN) type BAPIRET2_T
    raising
      ZCX_FLCM_ERROR .
  class-methods SET_OVERRIDE_OPEN_PERIOD .
  class-methods VALIDATE_NEW_TDS_PO
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !IS_CONTRACT type ZMM_FLCM_CONTRACT
      !ID_CONF_CTRL type ZMM_FLCM_PARMS-ZVAL_FROM
    exporting
      value(ET_RETURN) type BAPIRET2_T
      !ED_EBELN type EKKO-EBELN
    raising
      ZCX_FLCM_ERROR .
protected section.
*"* protected components of class ZCL_FLCM_SERVICES
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_FLCM_SERVICES
*"* do not include other source files here!!!

  types:
    BEGIN OF t_werks_yr_mth,
             werks             TYPE zmm_flcm_tds-werks,
             folio_yr_mth      TYPE n LENGTH 6,                                "DV5K970912
*             folio_yr          TYPE zmm_flcm_tds-ztds_folio_yr,               "DV5K970912
*             folio_mth         TYPE zmm_flcm_tds-ztds_folio_mth,              "DV5K970912
*             ztds_tran_ref_nb  TYPE zmm_flcm_tds-ztds_tran_ref_nb,            "DV5K970912
    END OF t_werks_yr_mth .
  types:
    T_WERKS_YR_MTH_TABLE type SORTED TABLE OF t_werks_yr_mth                   "DV5K970912 "DV5K970912 "DV5K970912 "DV5K970912 "DV5K970912 "DV5K970912 "DV5K970912 "DV5K970912 "DV5K970912 "DV5K970912 "DV5K970912 "DV5K970912 "DV5K970912 "DV5K970912
                              WITH UNIQUE KEY werks .
  types:
    BEGIN OF t_unique_po,
             ebeln           TYPE ekko-ebeln,
             ebelp           TYPE ekpo-ebelp,
             ihrez           TYPE ekko-ihrez,
             lifnr           TYPE ekko-lifnr,
             unsez           TYPE ekko-unsez,
             loekz           TYPE ekpo-loekz,
             matkl           TYPE ekpo-matkl,
             matnr           TYPE ekpo-matnr,
             knttp           TYPE ekpo-knttp,
             retpo           TYPE ekpo-retpo,
             bstae           TYPE ekpo-bstae,
             bednr           TYPE ekpo-bednr,
             menge           TYPE ekpo-menge,
             meins           TYPE ekpo-meins,
             eindt           TYPE eket-eindt,
    END OF t_unique_po .
  types:
    BEGIN OF t_contract,
             ebeln           TYPE eord-ebeln,
             ebelp           TYPE eord-ebelp,
             lifnr           TYPE ekko-lifnr,
             bsart           TYPE ekko-bsart,
             kdatb           TYPE ekko-kdatb,
             kdate           TYPE ekko-kdate,
             matkl           TYPE ekpo-matkl,
             knttp           TYPE ekpo-knttp,
             loekz           TYPE ekpo-loekz,
             werks           TYPE ekpo-werks,
             meins           TYPE ekpo-meins,
             adrnr           TYPE ekpo-adrnr,
             matnr           TYPE ekpo-matnr,
             sort1           TYPE adrc-sort1,
    END OF t_contract .
  types:
    BEGIN OF t_zmm_flcm_parms,
             progname       TYPE zmm_flcm_parms-progname,
             zparm_nm       TYPE zmm_flcm_parms-zparm_nm,
             zparm_opt      TYPE zmm_flcm_parms-zparm_opt,
             zparm_sign     TYPE zmm_flcm_parms-zparm_sign,
             zval_from      TYPE zmm_flcm_parms-zval_from,
             zval_to        TYPE zmm_flcm_parms-zval_to,
   END OF t_zmm_flcm_parms .
  types:
    BEGIN OF t_zmm_flcm_plntdef,
             werks     type zmm_flcm_plntdef-werks,
             bukrs     TYPE zmm_flcm_plntdef-bukrs,
    END OF t_zmm_flcm_plntdef .

  class-data MS_CONTRACT type T_CONTRACT .
  class-data MS_UNIQUE_PO type T_UNIQUE_PO .
  class-data MT_PLANTDEF type ZMM_FLCM_PLNTDEF_TT .
  class-data MT_WERKS_YR_MTH type T_WERKS_YR_MTH_TABLE .
  class-data MT_XREF_GMCODE type ZMM_FLCM_XREF_T .
  class-data MT_XREF_MVTP type ZMM_FLCM_XREF_T .

  class-methods GET_PLANT_DEFINITION
    importing
      !ID_WERKS type ZMM_FLCM_PLNTDEF-WERKS
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS default MC_SUB_STATUS_NONE
    returning
      value(RS_PLNTDEF) type ZMM_FLCM_PLNTDEF .
  class-methods GET_MAX_AMOUNT_PER_VOLUME
    exporting
      !ED_LITRE type BAPICUREXT
      !ED_GALLON type BAPICUREXT .
ENDCLASS.



CLASS ZCL_FLCM_SERVICES IMPLEMENTATION.


method CHANGE_PO.


  DATA:
    ls_return          TYPE bapiret2.

  DATA:
    ld_success         TYPE abap_bool.

  REFRESH:
    et_return.



  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      PURCHASEORDER                = id_ebeln
*     POHEADER                     =
*     POHEADERX                    =
*     POADDRVENDOR                 =
*     TESTRUN                      =
*     MEMORY_UNCOMPLETE            =
*     MEMORY_COMPLETE              =
*     POEXPIMPHEADER               =
*     POEXPIMPHEADERX              =
*     VERSIONS                     =
*     NO_MESSAGING                 =
*     NO_MESSAGE_REQ               =
*     NO_AUTHORITY                 =
*     NO_PRICE_FROM_PO             =
*     PARK_UNCOMPLETE              =
*     PARK_COMPLETE                =
*   IMPORTING
*     EXPHEADER                    =
*     EXPPOEXPIMPHEADER            =
    TABLES
      RETURN                       = et_return
      POITEM                       = ct_items
      POITEMX                      = ct_itemsx
*     POADDRDELIVERY               =
      POSCHEDULE                   = ct_schedules
      POSCHEDULEX                  = ct_schedulesx
*     POACCOUNT                    =
*     POACCOUNTPROFITSEGMENT       =
*     POACCOUNTX                   =
*     POCONDHEADER                 =
*     POCONDHEADERX                =
*     POCOND                       =
*     POCONDX                      =
*     POLIMITS                     =
*     POCONTRACTLIMITS             =
*     POSERVICES                   =
*     POSRVACCESSVALUES            =
*     POSERVICESTEXT               =
*     EXTENSIONIN                  =
*     EXTENSIONOUT                 =
*     POEXPIMPITEM                 =
*     POEXPIMPITEMX                =
*     POTEXTHEADER                 =
*     POTEXTITEM                   =
*     ALLVERSIONS                  =
*     POPARTNER                    =
*     POCOMPONENTS                 =
*     POCOMPONENTSX                =
*     POSHIPPING                   =
*     POSHIPPINGX                  =
*     POSHIPPINGEXP                =
*     POHISTORY                    =
*     POHISTORY_TOTALS             =
*     POCONFIRMATION               =
*     SERIALNUMBER                 =
*     SERIALNUMBERX                =
*     INVPLANHEADER                =
*     INVPLANHEADERX               =
*     INVPLANITEM                  =
*     INVPLANITEMX                 =
*     POHISTORY_MA                 =
            .

* Update success?
  IF ( find_error( et_return ) = abap_false ).
*   Commit changes
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait     = abap_true
      IMPORTING
        return   = ls_return.
    IF ( NOT ls_return-type CA mc_error_types ).
*     Success
      ld_success = abap_true.
    ELSE.
*     Failure
      APPEND ls_return TO et_return.
    ENDIF.
  ENDIF.

* Update failure
  IF ( ld_success = abap_false ).
*   Rollback changes
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = ls_return.
    IF ( ls_return-type CA mc_error_types ).
      APPEND ls_return TO et_return.
    ENDIF.
*   Raise exception
    IF ( 1 = 2 ). MESSAGE e063(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '063'
        sub_stus = id_sub_stus
        msgtab   = et_return.
  ENDIF.



endmethod.


method CONFIRM_PO.



*CALL FUNCTION 'ME_PO_CONFIRM'
*  EXPORTING
*    DOCUMENT_NO            = id_ebeln
**   TESTRUN                = MMPUR_NO
**   HEADER                 =
**   HEADERX                =
*    ITEM                   =
**   ITEMX                  =
**   CONFIRMATION           =
**   CONFIRMATIONX          =
**   ITEM_TEXT              =
**   HEADER_TEXT            =
**   EXTENSIONIN            =
** IMPORTING
**   RETURN                 =
**   EXP_HEADER             =
**   EXP_ITEM               =
**   EXP_CONFIRMATION       =
**   EXTENSIONOUT           =
*          .

endmethod.


method CONVERT_IN_TRANSIT_STOCK.


  DATA:

    ld_output_qty  TYPE EKPO-MENGE,
    ld_input_qty   TYPE EKPO-MENGE.

    FIELD-SYMBOLS:
       <maramarc> type zmm_flcm_maramarc.


    READ TABLE it_maramarc ASSIGNING <maramarc> WITH KEY
                                      matnr = is_trans-matnr
                                      werks = is_trans-werks
                                                   BINARY SEARCH.

    IF sy-subrc EQ 0.

      IF <maramarc>-meins ne is_trans-meins.

        ld_input_qty = <maramarc>-umlmc.

        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = is_trans-matnr
            i_in_me              = <maramarc>-meins
            i_out_me             = is_trans-meins
            i_menge              = ld_input_qty
          IMPORTING
            e_menge              = ld_output_qty
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
        IF SY-SUBRC <> 0.
*   Raise exception
           IF ( 1 = 2 ). MESSAGE e046(zz_flcm). ENDIF.
           RAISE EXCEPTION TYPE zcx_flcm_error
             EXPORTING
               msgid    = mc_msgid_flcm
               msgty    = mc_msgty_error
               msgno    = '046'
               sub_stus = mc_sub_status_phyi_fail.
*              msgtab   = et_return.
        ELSE.
           rd_inv_tot_count = is_trans-ztds_tot_inv - ld_output_qty.
        ENDIF.
      ELSE.
         rd_inv_tot_count = is_trans-ztds_tot_inv - <maramarc>-umlmc.
      ENDIF.
    ELSE.
*   Raise exception
       IF ( 1 = 2 ). MESSAGE e046(zz_flcm). ENDIF.
       RAISE EXCEPTION TYPE zcx_flcm_error
         EXPORTING
           msgid    = mc_msgid_flcm
           msgty    = mc_msgty_error
           msgno    = '046'
           sub_stus = mc_sub_status_phyi_fail.
*          msgtab   = et_return.
    ENDIF.

endmethod.


METHOD create_goods_mvmt.
*----------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* Steven Qiu            243167    2013/08/08           DV5K980921      *
* 1) Add more display logs to know the processing flow of the program. *
* 2) Put fix to reset the flag.                                        *
*----------------------------------------------------------------------*


  DATA:
    ls_return          TYPE bapiret2.

  DATA:
    ld_success         TYPE abap_bool,
    ls_header          LIKE is_header.           "DV5K967528 Defect 638

  REFRESH:
    et_return.

  CLEAR:
    ed_mat_doc,
    ed_doc_year.

*--------------------------------------------------------------------*
*** Start of code added                          DV5K967528 Defect 638
  ls_header = is_header.
  IF im_override_processing_date IS SUPPLIED AND
     im_override_processing_date IS NOT INITIAL.
    ls_header-pstng_date = im_override_processing_date.     "Override the processing date
    READ TABLE et_return TRANSPORTING NO FIELDS
        WITH KEY type   = 'S'
                 id     = 'ZZ_FLCM'
                 number = '096'.
    IF sy-subrc > 0.        "Only write the message once per output set
      ls_return-type = 'S'.
      ls_return-id = 'ZZ_FLCM'.
      ls_return-number = '096'.
      CONCATENATE is_header-pstng_date+0(4) is_header-pstng_date+4(2) is_header-pstng_date+6(2)
          INTO ls_return-message_v1 SEPARATED BY '/'.       "From
      CONCATENATE ls_header-pstng_date+0(4) ls_header-pstng_date+4(2) ls_header-pstng_date+6(2)
          INTO ls_return-message_v2 SEPARATED BY '/'.       "To
      IF 1 = 2.       " For «Where-Used» only
        MESSAGE s096(zz_flcm) WITH space space.
      ENDIF.
    ENDIF.
  ENDIF.
*** End of code added                            DV5K967528 Defect 638
*--------------------------------------------------------------------*

** Begin of Insert  DV5K980921 -->>
  MESSAGE s900(zz-cn2) WITH
     'Calling BAPI_GOODSMVT_CREATE'.
** End of Insert  DV5K980921 --<<

* Perform goods movement
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
*      goodsmvt_header  = is_header             "DV5K967528 Defect 638
      goodsmvt_header  = ls_header              "DV5K967528 Defect 638
      goodsmvt_code    = is_gmcode
    IMPORTING
      materialdocument = ed_mat_doc
      matdocumentyear  = ed_doc_year
    TABLES
      goodsmvt_item    = it_item
      return           = et_return.

  IF ls_return IS NOT INITIAL.                  "DV5K967528 Defect 638
    INSERT ls_return INTO et_return INDEX 1.    "DV5K967528 Defect 638
    CLEAR ls_return.                            "DV5K967528 Defect 638
  ENDIF.                                        "DV5K967528 Defect 638

* Update success?
  IF ( find_error( et_return ) = abap_false ).
** Begin of Insert  DV5K980921 -->>
    MESSAGE s900(zz-cn2) WITH
       'Commit BAPI_GOODSMVT_CREATE'.
** End of Insert  DV5K980921 --<<

*   Commit changes
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait     = abap_true
      IMPORTING
        return   = ls_return.
    IF ( NOT ls_return-type CA mc_error_types ).
*     Success
      ld_success = abap_true.
      clear ls_return.
      ls_return-type       = mc_success.
      ls_return-id         = mc_msgid_flcm.
      ls_return-number     = '069'.
      ls_return-message_v1 = ed_mat_doc.
      ls_return-message_v2 = ed_doc_year.
      append ls_return to et_return.
    ELSE.
*     Failure
      APPEND ls_return TO et_return.
    ENDIF.
  ENDIF.

* Update failure
  IF ( ld_success = abap_false ).
** Begin of Insert  DV5K980921 -->>
    MESSAGE s900(zz-cn2) WITH
       'Rollback BAPI_GOODSMVT_CREATE'.
** End of Insert  DV5K980921 --<<
*   Rollback changes
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = ls_return.
    IF ( ls_return-type CA mc_error_types ).
      APPEND ls_return TO et_return.
    ENDIF.
    IF id_sub_stus = mc_sub_status_phyg_fail.
*   Raise exception
      IF ( 1 = 2 ). MESSAGE e089(zz_flcm). ENDIF.
      RAISE EXCEPTION TYPE zcx_flcm_error
        EXPORTING
          msgid    = mc_msgid_flcm
          msgty    = mc_msgty_error
          msgno    = '089'
          sub_stus = id_sub_stus
          msgtab   = et_return.
    ELSE.
*   Raise exception
      IF ( 1 = 2 ). MESSAGE e038(zz_flcm). ENDIF.
      RAISE EXCEPTION TYPE zcx_flcm_error
        EXPORTING
          msgid    = mc_msgid_flcm
          msgty    = mc_msgty_error
          msgno    = '038'
          sub_stus = id_sub_stus
          msgtab   = et_return.
    ENDIF.
  ENDIF.

ENDMETHOD.


method CREATE_GOODS_MVMT_END_OF_DAY.

  DATA:
    lt_item          TYPE bapi2017_gm_item_create_t.

  DATA:
    ls_plntdef       LIKE LINE OF mt_plantdef,
    ls_gmcode        TYPE bapi2017_gm_code,
    ls_header        TYPE bapi2017_gm_head_01.

  DATA:
    ld_move_type     TYPE bapi2017_gm_item_create-move_type,
    ld_msgv1         TYPE sy-msgv1.

  FIELD-SYMBOLS:
    <item>           LIKE LINE OF lt_item.

  REFRESH:
    rt_return.

* Get movement type
  ld_move_type = get_movement_type( id_trans    = is_trans-ztds_tran_type
                                    id_vol_sign = is_trans-ztds_vol_sign ).

* Goods movement code
  CLEAR ls_gmcode.
  ls_gmcode-gm_code = mc_gmcode_goods_issue.

* Header
  CLEAR ls_header.
  ls_header-pstng_date = is_trans-ztds_folio_frzdt.
  ls_header-doc_date   = is_trans-ztds_tran_dt.
  ls_header-header_txt = is_trans-ztds_tran_ref_nb.

* Line item
  APPEND INITIAL LINE TO lt_item ASSIGNING <item>.
  <item>-material   = is_trans-matnr.
  <item>-plant = is_trans-werks.
  <item>-stge_loc   = get_storage_location( id_werks    = <item>-plant
                                            id_sub_stus = id_sub_stus ).
  <item>-move_type  = ld_move_type.
  <item>-gr_rcpt    = mc_gr_rcpt_phg_l.

  ls_plntdef = get_plant_definition( <item>-plant ).

* G/L Account
  IF ( ls_plntdef-zskato_tgl IS NOT INITIAL ).
    <item>-gl_account = ls_plntdef-zskato_pgl.
  ELSE.
    ld_msgv1 = <item>-plant.
    IF ( 1 = 2 ). MESSAGE e034(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid = mc_msgid_flcm
        msgty = mc_msgty_error
        msgno = '034'
        msgv1 = ld_msgv1
        sub_stus = id_sub_stus.
  ENDIF.

* Cost Center
  IF ( ls_plntdef-kostl IS NOT INITIAL ).
    <item>-costcenter = ls_plntdef-kostl.
  ELSE.
    ld_msgv1 = <item>-plant.
    IF ( 1 = 2 ). MESSAGE e035(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid = mc_msgid_flcm
        msgty = mc_msgty_error
        msgno = '035'
        msgv1 = ld_msgv1
        sub_stus = id_sub_stus.
  ENDIF.

* Business Area
  IF ( ls_plntdef-gsber IS NOT INITIAL ).
    <item>-tr_part_ba = ls_plntdef-gsber.
  ELSE.
    ld_msgv1 = <item>-plant.
    IF ( 1 = 2 ). MESSAGE e036(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid = mc_msgid_flcm
        msgty = mc_msgty_error
        msgno = '036'
        msgv1 = ld_msgv1
        sub_stus = id_sub_stus.
  ENDIF.


  <item>-move_type  = ld_move_type.
  <item>-entry_qnt  = is_trans-ztds_pgl.
  <item>-entry_uom  = is_trans-meins.

* Call goods movement bapi
  create_goods_mvmt( EXPORTING is_gmcode   = ls_gmcode
                               is_header   = ls_header
                               it_item     = lt_item
                               id_sub_stus = id_sub_stus
                     IMPORTING et_return   = rt_return ).


endmethod.


METHOD create_goods_mvmt_gain_loss.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Wayne Jiang               2013/02/22          DV5K977430             *
*                                                                      *
* Short Description: C221006 - T231907                                 *
*     seperate G/L account                                             *
*----------------------------------------------------------------------*
* Jose Luis Banda           2012/04/04          DV5K970912             *
*                                                                      *
* Short Description: C201338 - T209429                                 *
*     Add a value at the recipient field in the goods movement bapi    *
*----------------------------------------------------------------------*


  DATA:
    lt_item          TYPE bapi2017_gm_item_create_t.

  DATA:
    ls_plntdef       LIKE LINE OF mt_plantdef,
    ls_gmcode        TYPE bapi2017_gm_code,
    ls_header        TYPE bapi2017_gm_head_01.

  DATA:
    ld_move_type     TYPE bapi2017_gm_item_create-move_type,
    ls_return        TYPE bapiret2,
    ld_msgv1         TYPE sy-msgv1,
    l_override       TYPE dats.                            "DV5K967528 defect 638

  CONSTANTS:                                                       "I-DV5K977430
    lc_tran_type_700 TYPE ZMM_FLCM_TDS-ztds_tran_type VALUE '700', "I-DV5K977430
    lc_tran_type_701 TYPE ZMM_FLCM_TDS-ztds_tran_type VALUE '701', "I-DV5K977430
    lc_tran_type_702 TYPE ZMM_FLCM_TDS-ztds_tran_type VALUE '702'. "I-DV5K977430

  FIELD-SYMBOLS:
    <item>           LIKE LINE OF lt_item.

  REFRESH:
    rt_return.


  IF ( is_trans-ztds_tgl EQ 0 ).
* then we will not fail but we want to send a message back indicating that the value is zero
    IF ( 1 = 2 ). MESSAGE e080(zz_flcm). ENDIF.
    CLEAR ls_return.
    ls_return-type   = mc_msgty_success.
    ls_return-id     = mc_msgid_flcm.
    ls_return-number = '080'.
    APPEND ls_return TO rt_return.
  ENDIF.
* No gain/loss? then leave method now!!
  CHECK ( is_trans-ztds_tgl <> 0 ).

* Get movement type
  ld_move_type = get_movement_type( id_trans    = is_trans-ztds_tran_type
                                    id_vol_sign = is_trans-ztds_vol_sign
                                    id_sub_stus = id_sub_stus ).

* Goods movement code
  CLEAR ls_gmcode.
  ls_gmcode-gm_code = mc_gmcode_goods_issue.

* Header
  CLEAR ls_header.
  IF is_trans-ztran_stus = '04' AND                               "DV5K967528 defect 638
     fi_open_period_check( is_trans-ztds_tran_dt ) =  abap_false. "DV5K967528 defect 638
    l_override = sy-datum.                                        "DV5K967528 defect 638
  ELSE.                                                           "DV5K967528 defect 638
    CLEAR l_override.                                             "DV5K967528 defect 638
  ENDIF.                                                          "DV5K967528 defect 638
  ls_header-pstng_date = is_trans-ztds_tran_dt.
  ls_header-doc_date   = is_trans-ztds_tran_dt.            "DV5K967528 defect 651
*  ls_header-doc_date   = sy-datum.                        "DV5K967528 defect 651
  ls_header-ref_doc_no = is_trans-ztds_bol_nbr.
  ls_header-pr_uname   = sy-uname.
  ls_header-header_txt = is_trans-ztds_tran_ref_nb.

* Line item
  APPEND INITIAL LINE TO lt_item ASSIGNING <item>.
  <item>-material   = is_trans-matnr.

  IF ( is_trans-ztds_dvrt_ind IS NOT INITIAL ).
    <item>-plant = is_trans-ztds_bol_plant.
  ELSE.
    <item>-plant = is_trans-werks.
  ENDIF.

  <item>-stge_loc   = get_storage_location( id_werks = <item>-plant
                                            id_sub_stus = id_sub_stus ).
  <item>-move_type  = ld_move_type.
*  <item>-gr_rcpt    = mc_gr_rcpt.                                        "DV5K970912
  CONCATENATE mc_gr_rcpt '-' is_trans-ztds_tran_type INTO <item>-gr_rcpt. "DV5K970912

  ls_plntdef = get_plant_definition( <item>-plant ).

* G/L Account
*  IF ( ls_plntdef-zskato_tgl IS NOT INITIAL ).                            "D-DV5K977430
*    <item>-gl_account = ls_plntdef-zskato_tgl.                            "D-DV5K977430
*  ELSE.                                                                   "D-DV5K977430
*    ld_msgv1 = <item>-plant.                                              "D-DV5K977430
*    IF ( 1 = 2 ). MESSAGE e034(zz_flcm). ENDIF.                           "D-DV5K977430
*    RAISE EXCEPTION TYPE zcx_flcm_error                                   "D-DV5K977430
*      EXPORTING                                                           "D-DV5K977430
*        msgid = mc_msgid_flcm                                             "D-DV5K977430
*        msgty = mc_msgty_error                                            "D-DV5K977430
*        msgno = '034'                                                     "D-DV5K977430
*        msgv1 = ld_msgv1                                                  "D-DV5K977430
*        sub_stus = id_sub_stus.                                           "D-DV5K977430
*  ENDIF.                                                                  "D-DV5K977430

  IF is_trans-ztds_tran_type = lc_tran_type_700.                           "I-DV5K977430, tran_type 700
    IF ( ls_plntdef-zskato_tgl IS NOT INITIAL ).                           "I-DV5K977430
      <item>-gl_account = ls_plntdef-zskato_tgl.                           "I-DV5K977430
    ELSE.                                                                  "I-DV5K977430
      ld_msgv1 = <item>-plant.                                             "I-DV5K977430
      IF ( 1 = 2 ). MESSAGE e034(zz_flcm). ENDIF.                          "I-DV5K977430
      RAISE EXCEPTION TYPE zcx_flcm_error                                  "I-DV5K977430
        EXPORTING                                                          "I-DV5K977430
          msgid = mc_msgid_flcm                                            "I-DV5K977430
          msgty = mc_msgty_error                                           "I-DV5K977430
          msgno = '034'                                                    "I-DV5K977430
          msgv1 = ld_msgv1                                                 "I-DV5K977430
          sub_stus = id_sub_stus.                                          "I-DV5K977430
    ENDIF.                                                                 "I-DV5K977430
  ELSE.                                                                    "I-DV5K977430, tran_type 701, 702, others
    IF ( ls_plntdef-zskato_tgl2 IS NOT INITIAL ).                          "I-DV5K977430
      <item>-gl_account = ls_plntdef-zskato_tgl2.                          "I-DV5K977430
    ELSE.                                                                  "I-DV5K977430
      ld_msgv1 = <item>-plant.                                             "I-DV5K977430
      IF ( 1 = 2 ). MESSAGE e034(zz_flcm). ENDIF.                          "I-DV5K977430
      RAISE EXCEPTION TYPE zcx_flcm_error                                  "I-DV5K977430
        EXPORTING                                                          "I-DV5K977430
          msgid = mc_msgid_flcm                                            "I-DV5K977430
          msgty = mc_msgty_error                                           "I-DV5K977430
          msgno = '034'                                                    "I-DV5K977430
          msgv1 = ld_msgv1                                                 "I-DV5K977430
          sub_stus = id_sub_stus.                                          "I-DV5K977430
    ENDIF.                                                                 "I-DV5K977430
  ENDIF.                                                                   "I-DV5K977430

* Cost Center
  IF ( ls_plntdef-kostl IS NOT INITIAL ).
    <item>-costcenter = ls_plntdef-kostl.
  ELSE.
    ld_msgv1 = <item>-plant.
    IF ( 1 = 2 ). MESSAGE e035(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid = mc_msgid_flcm
        msgty = mc_msgty_error
        msgno = '035'
        msgv1 = ld_msgv1
        sub_stus = id_sub_stus.
  ENDIF.

* Business Area
  IF ( ls_plntdef-gsber IS NOT INITIAL ).
    <item>-tr_part_ba = ls_plntdef-gsber.
  ELSE.
    ld_msgv1 = <item>-plant.
    IF ( 1 = 2 ). MESSAGE e036(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid = mc_msgid_flcm
        msgty = mc_msgty_error
        msgno = '036'
        msgv1 = ld_msgv1
        sub_stus = id_sub_stus.
  ENDIF.

  <item>-move_type  = ld_move_type.
  <item>-entry_qnt  = is_trans-ztds_tgl.
  <item>-entry_uom  = is_trans-meins.

* Call goods movement bapi
  create_goods_mvmt( EXPORTING is_gmcode   = ls_gmcode
                               is_header   = ls_header
                               it_item     = lt_item
                               im_override_processing_date = l_override  "DV5K967528 defect 638
                               id_sub_stus = id_sub_stus
                     IMPORTING et_return   = rt_return ).

ENDMETHOD.


METHOD create_goods_mvmt_gi_fuel.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by        Date            Tracking number          *
* ------------------------    ----------      ------------------------ *
* Daryll Recalde              2015/03/23      DV5K994418               *
*                                                                      *
* Short Description: Add a logic for the conversion of g/l accounts    *
*                   and costcenter to its its unconverted form.        *
*----------------------------------------------------------------------*
* Daryll Recalde              2015/02/11      DV5K993167               *
*                                             DV5K994198               *
*                                                                      *
* Short Description : Add logic for checking of locomotive and changing*
*                   its G/L Account and Cost Center                    *
*                                                                      *
************************************************************************

  DATA:
    lt_item          TYPE bapi2017_gm_item_create_t.

  DATA:
    ls_plntdef       LIKE LINE OF mt_plantdef,
    ls_gmcode        TYPE bapi2017_gm_code,
    ls_header        TYPE bapi2017_gm_head_01,
    l_override       TYPE dats.                            "DV5K967528 defect 638

  DATA:
    ld_move_type     TYPE bapi2017_gm_item_create-move_type,
    ld_gm_code       TYPE bapi2017_gm_code,
    ld_msgv1         TYPE sy-msgv1,
***Start of Insert - XT20169 - DV5K993167 - CR257405 TK285613
    ld_msgv2         TYPE sy-msgv2,
    ld_msgv3         TYPE sy-msgv3.

  DATA:
    lt_zmm_flcm_parms     TYPE STANDARD TABLE OF t_zmm_flcm_parms,
    ls_zmm_flcm_parms     TYPE t_zmm_flcm_parms,
    ls_zmm_flcm_parms_gl  TYPE t_zmm_flcm_parms,
    ls_zmm_flcm_parms_cc  TYPE t_zmm_flcm_parms,
    lt_zmm_flcm_plntdef   TYPE STANDARD TABLE OF t_zmm_flcm_plntdef,
    ls_zmm_flcm_plntdef   TYPE t_zmm_flcm_plntdef,
    ld_zval_from          TYPE zmm_flcm_parms-zval_from,
    ld_zval_to            TYPE zmm_flcm_parms-zval_to,
    ld_werks              TYPE zmm_flcm_plntdef-werks,
    ld_zval_from_cc       TYPE zmm_flcm_parms-zval_from.

  CONSTANTS:
    lc_progname   TYPE string VALUE 'SMENH817',
    lc_zparm_nm   TYPE string VALUE 'MISC_RAIL_EQUIP',
    lc_zparm_nm1  TYPE string VALUE 'MISC_RAIL_EQUIP_GL',
    lc_zparm_nm2  TYPE string VALUE 'MISC_RAIL_EQUIP_CC',
    lc_zparm_opt  TYPE string VALUE 'EQ',
    lc_zparm_sign TYPE string VALUE 'I'.
***End of Insert - XT20169 - DV5K993167 - CR257405 TK285613
  FIELD-SYMBOLS:
    <item>           LIKE LINE OF lt_item.

  REFRESH:
    rt_return.

* Get movement type
  ld_move_type = zcl_flcm_services=>get_movement_type(
                             id_trans     = is_trans-ztds_tran_type
                             id_canc_rbil = is_trans-ztds_canc_rbil ).


  ld_gm_code = zcl_flcm_services=>get_gmcode(
                           id_move_type = ld_move_type ).

* Goods movement code
  CLEAR ls_gmcode.
  ls_gmcode-gm_code = ld_gm_code.
* Header
  CLEAR ls_header.
  IF is_trans-ztran_stus = '04' AND                               "DV5K967528 defect 638
     fi_open_period_check( is_trans-ztds_tran_dt ) =  abap_false. "DV5K967528 defect 638
    l_override = sy-datum.                                        "DV5K967528 defect 638
  ELSE.                                                           "DV5K967528 defect 638
    CLEAR l_override.                                             "DV5K967528 defect 638
  ENDIF.                                                          "DV5K967528 defect 638
  ls_header-pstng_date = is_trans-ztds_tran_dt.
   ls_header-doc_date   = is_trans-ztds_tran_dt.           "DV5K967528 defect 651
*  ls_header-doc_date   = sy-datum.                        "DV5K967528 defect 651
  ls_header-ref_doc_no = is_trans-ztds_bol_nbr.
  ls_header-pr_uname   = sy-uname.
  ls_header-header_txt = is_trans-ztds_tran_ref_nb.

* Line item
  APPEND INITIAL LINE TO lt_item ASSIGNING <item>.
  <item>-material   = is_trans-matnr.
  <item>-plant      = is_trans-ztds_trml_id.
  <item>-stge_loc   = get_storage_location( <item>-plant ).
  <item>-gr_rcpt    = is_trans-equnr.
* only map plant and receiving storage location if gm code EQ 04

  IF ld_gm_code = mc_gmcode_transfer_posting.
     <item>-move_plant = is_trans-werks.
     <item>-move_stloc = get_storage_location_receiving( <item>-move_plant ).
  ENDIF.

  ls_plntdef = get_plant_definition( <item>-plant ).

* only map gl account, cost center and business area if gm code is 03

  IF ld_gm_code = mc_gmcode_goods_issue.
***Start of Insert - XT20169 - DV5K993167 - CR257405 TK285613
    SELECT progname
           zparm_nm
           zparm_opt
           zparm_sign
           zval_from
           zval_to
      INTO TABLE lt_zmm_flcm_parms
      FROM zmm_flcm_parms
      WHERE progname   EQ lc_progname
        AND zparm_opt  EQ lc_zparm_opt
        AND zparm_sign EQ lc_zparm_sign.
    IF sy-subrc = 0.
      SORT lt_zmm_flcm_parms BY zval_from
                                zparm_nm.
    ENDIF.

    ld_werks = is_trans-werks.
    SELECT werks
           bukrs
      INTO TABLE lt_zmm_flcm_plntdef
      FROM zmm_flcm_plntdef
      WHERE werks EQ ld_werks.
    IF sy-subrc = 0.
      SORT lt_zmm_flcm_plntdef BY werks.
    ENDIF.

    LOOP AT lt_item ASSIGNING <item>.
      READ TABLE lt_zmm_flcm_parms INTO ls_zmm_flcm_parms WITH KEY zval_from = is_trans-equnr
                                                                   zparm_nm  = lc_zparm_nm
                                                          BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE lt_zmm_flcm_plntdef INTO ls_zmm_flcm_plntdef WITH KEY werks = ld_werks
                                                                BINARY SEARCH.
        IF sy-subrc = 0.
*         G/L Account
          ld_zval_to = ls_zmm_flcm_parms-zval_to.
          CONCATENATE ld_zval_to ls_zmm_flcm_plntdef-bukrs INTO ld_zval_from.
          READ TABLE lt_zmm_flcm_parms INTO ls_zmm_flcm_parms_gl WITH KEY zval_from = ld_zval_from
                                                                          zparm_nm  = lc_zparm_nm1
                                                                 BINARY SEARCH.
          IF ls_zmm_flcm_parms_gl-zval_to IS NOT INITIAL.
            <item>-gl_account = ls_zmm_flcm_parms_gl-zval_to.
          ELSE.
            ld_msgv1 = ld_zval_from.
            ld_msgv2 = is_trans-ztds_tran_type.
            ld_msgv3 = ls_zmm_flcm_plntdef-bukrs.
            IF ( 1 = 2 ). MESSAGE e103(zz_flcm). ENDIF.
            RAISE EXCEPTION TYPE zcx_flcm_error
              EXPORTING
                msgid = mc_msgid_flcm
                msgty = mc_msgty_error
                msgno = '103'
                msgv1 = ld_msgv1
                msgv2 = ld_msgv2
                msgv3 = ld_msgv3.
          ENDIF.
*         Cost Center
          CONCATENATE ld_zval_from ld_werks INTO ld_zval_from_cc.
          READ TABLE lt_zmm_flcm_parms INTO ls_zmm_flcm_parms WITH KEY zval_from = ld_zval_from_cc
                                                                       zparm_nm  = lc_zparm_nm2
                                                              BINARY SEARCH.
          IF sy-subrc = 0.
            <item>-costcenter = ls_zmm_flcm_parms-zval_to.
          ELSE.
            READ TABLE lt_zmm_flcm_parms INTO ls_zmm_flcm_parms_cc WITH KEY zval_from = ld_zval_from
                                                                            zparm_nm  = lc_zparm_nm2
                                                                                       BINARY SEARCH.
            IF sy-subrc = 0.
              <item>-costcenter = ls_zmm_flcm_parms_cc-zval_to.
            ELSE.
              ld_msgv1 = ld_zval_from.
              ld_msgv2 = is_trans-ztds_tran_type.
              ld_msgv3 = ls_zmm_flcm_plntdef-bukrs.
              IF ( 1 = 2 ). MESSAGE e104(zz_flcm). ENDIF.
              RAISE EXCEPTION TYPE zcx_flcm_error
                EXPORTING
                  msgid = mc_msgid_flcm
                  msgty = mc_msgty_error
                  msgno = '104'
                  msgv1 = ld_msgv1
                  msgv2 = ld_msgv2
                  msgv3 = ld_msgv3.
            ENDIF.
          ENDIF.
        ELSE.
          ld_msgv1 = ld_zval_from.
          ld_msgv2 = is_trans-ztds_tran_type.
          ld_msgv3 = text-001.
          IF ( 1 = 2 ). MESSAGE e103(zz_flcm). ENDIF.
          RAISE EXCEPTION TYPE zcx_flcm_error
            EXPORTING
              msgid = mc_msgid_flcm
              msgty = mc_msgty_error
              msgno = '103'
              msgv1 = ld_msgv1
              msgv2 = ld_msgv2
              msgv3 = ld_msgv3.
        ENDIF.
      ELSE.
*       G/L Account
        IF ( ls_plntdef-zskato_tgl IS NOT INITIAL ).
          <item>-gl_account = ls_plntdef-zskato_fuel.
        ELSE.
          ld_msgv1 = <item>-plant.
          IF ( 1 = 2 ). MESSAGE e034(zz_flcm). ENDIF.
          RAISE EXCEPTION TYPE zcx_flcm_error
            EXPORTING
              msgid = mc_msgid_flcm
              msgty = mc_msgty_error
              msgno = '034'
              msgv1 = ld_msgv1.
        ENDIF.
*       Cost Center
        IF ( ls_plntdef-kostl IS NOT INITIAL ).
          <item>-costcenter = ls_plntdef-kostl.
        ELSE.
          ld_msgv1 = <item>-plant.
          IF ( 1 = 2 ). MESSAGE e035(zz_flcm). ENDIF.
          RAISE EXCEPTION TYPE zcx_flcm_error
            EXPORTING
              msgid = mc_msgid_flcm
              msgty = mc_msgty_error
              msgno = '035'
              msgv1 = ld_msgv1.
        ENDIF.
      ENDIF.
***Start of Insert - XT20169 -  DV5K994418 - CR257405 TK285613
      IF <item>-gl_account IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <item>-gl_account
          IMPORTING
            output = <item>-gl_account.
      ENDIF.

      IF <item>-costcenter IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <item>-costcenter
          IMPORTING
            output = <item>-costcenter.
      ENDIF.
***End of Insert - XT20169 - DV5K994418 - CR257405 TK285613
      CLEAR:
      ls_zmm_flcm_plntdef,
      ls_zmm_flcm_parms_gl,
      ls_zmm_flcm_parms_cc.
    ENDLOOP.

    FREE: lt_zmm_flcm_parms,
          lt_zmm_flcm_plntdef.
***End of Insert - XT20169 - DV5K993167 - CR257405 TK285613
***Start of Delete - XT20169 - DV5K993167 - CR257405 TK285613
** G/L Account
*     IF ( ls_plntdef-zskato_tgl IS NOT INITIAL ).
*       <item>-gl_account = ls_plntdef-zskato_fuel.
*     ELSE.
*       ld_msgv1 = <item>-plant.
*       IF ( 1 = 2 ). MESSAGE e034(zz_flcm). ENDIF.
*       RAISE EXCEPTION TYPE zcx_flcm_error
*         EXPORTING
*           msgid = mc_msgid_flcm
*           msgty = mc_msgty_error
*           msgno = '034'
*           msgv1 = ld_msgv1.
*     ENDIF.
*
** Cost Center
*     IF ( ls_plntdef-kostl IS NOT INITIAL ).
*       <item>-costcenter = ls_plntdef-kostl.
*     ELSE.
*       ld_msgv1 = <item>-plant.
*       IF ( 1 = 2 ). MESSAGE e035(zz_flcm). ENDIF.
*       RAISE EXCEPTION TYPE zcx_flcm_error
*         EXPORTING
*           msgid = mc_msgid_flcm
*           msgty = mc_msgty_error
*           msgno = '035'
*           msgv1 = ld_msgv1.
*     ENDIF.
***End of Delete - XT20169 - DV5K993167 - CR257405 TK285613
* Business Area
     IF ( ls_plntdef-gsber IS NOT INITIAL ).
       <item>-tr_part_ba = ls_plntdef-gsber.
     ELSE.
       ld_msgv1 = <item>-plant.
       IF ( 1 = 2 ). MESSAGE e036(zz_flcm). ENDIF.
       RAISE EXCEPTION TYPE zcx_flcm_error
         EXPORTING
           msgid = mc_msgid_flcm
           msgty = mc_msgty_error
           msgno = '036'
           msgv1 = ld_msgv1.
     ENDIF.
  ENDIF.

  <item>-move_type  = ld_move_type.
  <item>-entry_qnt  = is_trans-ztds_net_vol.
  <item>-entry_uom  = is_trans-meins.

* Call goods movement bapi
  create_goods_mvmt( EXPORTING is_gmcode   = ls_gmcode
                               is_header   = ls_header
                               it_item     = lt_item
                               im_override_processing_date = l_override  "DV5K967528 defect 638
                     IMPORTING et_return   = rt_return ).


ENDMETHOD.


METHOD create_goods_mvmt_plant2plant.

  DATA:
    lt_item          TYPE bapi2017_gm_item_create_t.

  DATA:
    ls_gmcode        TYPE bapi2017_gm_code,
    ls_header        TYPE bapi2017_gm_head_01,
    l_override       TYPE dats.                            "DV5K967528 defect 638


  FIELD-SYMBOLS:
    <item>           LIKE LINE OF lt_item.

  REFRESH:
    rt_return.

* Goods movement code
  CLEAR ls_gmcode.
  ls_gmcode-gm_code = mc_gmcode_transfer_posting.

* Header
  CLEAR ls_header.
  IF is_trans-ztran_stus = '04' AND                               "DV5K967528 defect 638
     fi_open_period_check( is_trans-ztds_tran_dt ) =  abap_false. "DV5K967528 defect 638
    l_override = sy-datum.                                        "DV5K967528 defect 638
  ELSE.                                                           "DV5K967528 defect 638
    CLEAR l_override.                                             "DV5K967528 defect 638
  ENDIF.                                                          "DV5K967528 defect 638
  ls_header-pstng_date = is_trans-ztds_tran_dt.
  ls_header-doc_date   = is_trans-ztds_tran_dt.            "DV5K967528 defect 651
*  ls_header-doc_date   = sy-datum.                        "DV5K967528 defect 651
  ls_header-ref_doc_no = is_trans-ztds_bol_nbr.
  ls_header-pr_uname   = sy-uname.
  ls_header-header_txt = is_trans-ztds_tran_ref_nb.

* Line item
  APPEND INITIAL LINE TO lt_item ASSIGNING <item>.
  <item>-material   = is_trans-matnr.
  <item>-plant      = is_trans-ztds_bol_plant.
  <item>-stge_loc   = get_storage_location( <item>-plant ).
  <item>-move_type  = id_move_type.
  <item>-entry_qnt  = is_trans-ztds_net_vol.
  <item>-entry_uom  = is_trans-meins.
  <item>-move_plant = is_trans-werks.
  <item>-move_stloc = get_storage_location_receiving( <item>-move_plant ).

* Call goods movement bapi
  create_goods_mvmt( EXPORTING is_gmcode   = ls_gmcode
                               is_header   = ls_header
                               it_item     = lt_item
                               im_override_processing_date = l_override   "DV5K967528 defect 638
                               id_sub_stus = id_sub_stus
                     IMPORTING et_return   = rt_return ).

ENDMETHOD.


METHOD create_goods_mvmt_plant2plant2.

  DATA:
    lt_item          TYPE bapi2017_gm_item_create_t.

  DATA:
    ls_gmcode        TYPE bapi2017_gm_code,
    ls_header        TYPE bapi2017_gm_head_01,
    l_override       TYPE dats.                            "DV5K967528 defect 638


  FIELD-SYMBOLS:
    <item>           LIKE LINE OF lt_item.

  REFRESH:
    rt_return.

* Goods movement code
  CLEAR ls_gmcode.
  ls_gmcode-gm_code = mc_gmcode_transfer_posting.

* Header
  CLEAR ls_header.
  IF is_trans-ztran_stus = '04' AND                               "DV5K967528 defect 638
     fi_open_period_check( is_trans-ztds_tran_dt ) =  abap_false. "DV5K967528 defect 638
    l_override = sy-datum.                                        "DV5K967528 defect 638
  ELSE.                                                           "DV5K967528 defect 638
    CLEAR l_override.                                             "DV5K967528 defect 638
  ENDIF.                                                          "DV5K967528 defect 638
  ls_header-pstng_date = is_trans-ztds_tran_dt.
  ls_header-doc_date   = is_trans-ztds_tran_dt.            "DV5K967528 defect 651
*  ls_header-doc_date   = sy-datum.                        "DV5K967528 defect 651
  ls_header-ref_doc_no = is_trans-ztds_bol_nbr.
  ls_header-pr_uname   = sy-uname.
  ls_header-header_txt = is_trans-ztds_tran_ref_nb.

* Line item
  APPEND INITIAL LINE TO lt_item ASSIGNING <item>.
  <item>-material   = is_trans-matnr.
  <item>-plant      = is_trans-werks.
  <item>-stge_loc   = get_storage_location( <item>-plant ).
  <item>-move_type  = id_move_type.
  <item>-entry_qnt  = is_trans-ztds_bol_net.
  <item>-entry_uom  = is_trans-meins.
  <item>-move_plant = is_trans-ztds_bol_plant.
  <item>-move_stloc = get_storage_location_receiving( <item>-move_plant ).

* Call goods movement bapi
  create_goods_mvmt( EXPORTING is_gmcode   = ls_gmcode
                               is_header   = ls_header
                               it_item     = lt_item
                               im_override_processing_date = l_override   "DV5K967528 defect 638
                               id_sub_stus = id_sub_stus
                     IMPORTING et_return   = rt_return ).


ENDMETHOD.


method CREATE_GOODS_MVMT_TDS_PO.

  DATA:
    lt_item          TYPE bapi2017_gm_item_create_t.

  DATA:
    ls_plntdef       LIKE LINE OF mt_plantdef,
    ls_gmcode        TYPE bapi2017_gm_code,
    ls_header        TYPE bapi2017_gm_head_01,
    l_override       TYPE dats.                            "DV5K967528 defect 638

  DATA:
    ld_move_type     TYPE bapi2017_gm_item_create-move_type,
    ld_msgv1         TYPE sy-msgv1.

  FIELD-SYMBOLS:
    <item>           LIKE LINE OF lt_item.

  REFRESH:
    rt_return.


*     Get goods movement type
  ld_move_type = mc_move_type_101.

  ls_gmcode = zcl_flcm_services=>get_gmcode(
                           id_move_type = ld_move_type ).

* Goods movement code
  ls_gmcode-gm_code = ls_gmcode.
*
** Header
  CLEAR ls_header.
  IF is_trans-ztran_stus = '04' AND                               "DV5K967528 defect 638
     fi_open_period_check( is_trans-ztds_tran_dt ) =  abap_false. "DV5K967528 defect 638
    l_override = sy-datum.                                        "DV5K967528 defect 638
  ELSE.                                                           "DV5K967528 defect 638
    CLEAR l_override.                                             "DV5K967528 defect 638
  ENDIF.                                                          "DV5K967528 defect 638
  ls_header-pstng_date = is_trans-ztds_tran_dt.
  ls_header-doc_date   = is_trans-ztds_tran_dt.            "DV5K967528 defect 651
*  ls_header-doc_date   = sy-datum.                        "DV5K967528 defect 651
  concatenate mc_tds is_trans-ztds_tran_ref_nb into
  ls_header-ref_doc_no separated by '-'.
  ls_header-bill_of_lading = is_unique_po-ihrez.
*
** Line item
  APPEND INITIAL LINE TO lt_item ASSIGNING <item>.
*
  <item>-move_type         = ld_move_type.
  <item>-plant             = is_trans-werks.
* <item>-entry_qnt  = is_unique_po-menge.
* <item>-entry_uom  = is_unique_po-meins.
  <item>-ind_propose_quanx = abap_true.
  <item>-po_number         = is_unique_po-ebeln.
  <item>-po_item           = is_unique_po-ebelp.
  <item>-mvt_ind           = 'B'.
*
* Call goods movement bapi
  create_goods_mvmt( EXPORTING is_gmcode   = ls_gmcode
                               is_header   = ls_header
                               it_item     = lt_item
                               im_override_processing_date = l_override   "DV5K967528 defect 638
                               id_sub_stus = '000'
                     IMPORTING et_return   = rt_return ).


endmethod.


METHOD create_goods_mvmt_transport_po.

  DATA:
    lt_item          TYPE bapi2017_gm_item_create_t.

  DATA:
    ls_plntdef       LIKE LINE OF mt_plantdef,
    ls_gmcode        TYPE bapi2017_gm_code,
    ls_header        TYPE bapi2017_gm_head_01,
    l_override       TYPE dats.                            "DV5K967528 defect 638

  DATA:
    ld_move_type     TYPE bapi2017_gm_item_create-move_type,
    ld_msgv1         TYPE sy-msgv1.

  FIELD-SYMBOLS:
    <item>           LIKE LINE OF lt_item.

  REFRESH:
    rt_return.


*     Get goods movement type
  ld_move_type = zcl_flcm_services=>get_movement_type(
                     id_trans     = is_trans-ztds_tran_type
                     id_canc_rbil = is_trans-ztds_canc_rbil ).

  ls_gmcode = zcl_flcm_services=>get_gmcode(
                           id_move_type = ld_move_type ).

* Goods movement code
  ls_gmcode-gm_code = ls_gmcode.
*
** Header
  CLEAR ls_header.
  IF is_trans-ztran_stus = '04' AND                               "DV5K967528 defect 638
     fi_open_period_check( is_trans-ztds_tran_dt ) =  abap_false. "DV5K967528 defect 638
    l_override = sy-datum.                                        "DV5K967528 defect 638
  ELSE.                                                           "DV5K967528 defect 638
    CLEAR l_override.                                             "DV5K967528 defect 638
  ENDIF.                                                          "DV5K967528 defect 638
  ls_header-pstng_date = is_trans-ztds_tran_dt.            "DV5K967528 defect 668
*  ls_header-pstng_date = is_trans-ztds_bol_dt.            "DV5K967528 defect 668
  ls_header-doc_date   = is_trans-ztds_bol_dt.             "DV5K967528 defect 651
*  ls_header-doc_date   = sy-datum.                        "DV5K967528 defect 651
  CONCATENATE mc_car is_trans-ztds_tran_ref_nb INTO
  ls_header-ref_doc_no SEPARATED BY '-'.
  ls_header-bill_of_lading = is_unique_po-ihrez.
*
** Line item
  APPEND INITIAL LINE TO lt_item ASSIGNING <item>.
*
  <item>-move_type         = ld_move_type.
* <item>-entry_qnt  = is_unique_po-menge.
* <item>-entry_uom  = is_unique_po-meins.
  <item>-ind_propose_quanx = abap_true.
  <item>-po_number         = is_unique_po-ebeln.
  <item>-po_item           = is_unique_po-ebelp.
  <item>-mvt_ind           = 'B'.
*
* Call goods movement bapi
  create_goods_mvmt( EXPORTING is_gmcode   = ls_gmcode
                               is_header   = ls_header
                               it_item     = lt_item
                               im_override_processing_date = l_override   "DV5K967528 defect 638
                               id_sub_stus = id_sub_stus
                     IMPORTING et_return   = rt_return ).


ENDMETHOD.


method CREATE_NEW_TDS_PO.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by             Date                Tracking number *
* -------------------------        ----------          --------------- *
* Chuong Le                        2017/01/04          DV5K9A05WW      *
*                                                                      *
* Short Description: CR306321-T347799                                  *
*    - Save Our Reference in PO header text instead of EKKO-UNSEZ.     *
*----------------------------------------------------------------------*
* Von Kaisser Almeria              2016-07-28          DV5K9A03CO      *
*                                                                      *
* Short Description : CR269546 TK334189                                *
*                     Add controls to avoid creating POs for           *
*                     unreasonable amounts                             *
*----------------------------------------------------------------------*

  CONSTANTS:
    c_10           TYPE bapimepoitem-po_item value '00010'.

  DATA:

    ld_output_qty  TYPE EKPO-MENGE,
    ld_unit_tds    TYPE zmm_flcm_tds-meins,
    ld_input_qty   TYPE EKPO-MENGE,
    ld_ebeln       TYPE ebeln.

  DATA:
    ls_header      TYPE BAPIMEPOHEADER,
    ls_headerx     TYPE BAPIMEPOHEADERX,
    lt_item        TYPE BAPIMEPOITEM_TP,
    ls_item        TYPE BAPIMEPOITEM,
    lt_poschedule  TYPE BAPIMEPOSCHEDULE_TP,
    ls_poschedule  TYPE BAPIMEPOSCHEDULE,
    lt_poschedulex TYPE BAPIMEPOSCHEDULX_TP,
    ls_poschedulex TYPE BAPIMEPOSCHEDULX,
    lt_itemx       TYPE BAPIMEPOITEMX_TP,
    ls_itemx       TYPE BAPIMEPOITEMX,
    lt_potextheader TYPE bapimepotextheader_tp.            "DV5K9A05WW+

  FIELD-SYMBOLS:
    <item>           LIKE LINE OF lt_item,
    <itemx>          LIKE LINE OF lt_itemx.

* first see if we need to convert the quantity to the quantity of the contract
  refresh lt_item.
  refresh lt_itemx.
  refresh lt_poschedule.
  refresh lt_poschedulex.

  IF is_contract-bednr = 'GROSS'.
     ld_input_qty = is_trans-ztds_grs_vol.
  ELSE.
     ld_input_qty = is_trans-ztds_net_vol.
  ENDIF.

***BEGIN OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO
  call method zcl_flcm_services=>validate_new_tds_po(
    EXPORTING
      is_trans     = is_trans
      is_contract  = is_contract
      id_conf_ctrl = id_conf_ctrl
    IMPORTING
      et_return    = et_return
      ed_ebeln     = ed_ebeln
      ).
***END OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO

  IF is_trans-meins ne is_contract-meins.

*       ld_input_qty = is_trans-ztds_grs_vol.

        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = is_trans-matnr
            i_in_me              = is_trans-meins
            i_out_me             = is_contract-meins
            i_menge              = ld_input_qty
          IMPORTING
            e_menge              = ld_output_qty
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
        IF SY-SUBRC <> 0.
*   Raise exception
           IF ( 1 = 2 ). MESSAGE e046(zz_flcm). ENDIF.
           RAISE EXCEPTION TYPE zcx_flcm_error
             EXPORTING
               msgid    = mc_msgid_flcm
               msgty    = mc_msgty_error
               msgno    = '046'
               sub_stus = mc_sub_status_none.
*              msgtab   = et_return.
        ENDIF.
   ELSE.
      ld_output_qty = ld_input_qty.
   ENDIF.

* header
  clear ls_header.
  ls_header-doc_type   = 'FB  '.
  ls_header-doc_date   = is_trans-ztds_tran_dt.
  ls_header-ref_1      = is_trans-ztds_bol_nbr.
*  concatenate mc_tds is_trans-ztds_trml_id                "DV5K9A05WW-
*  into ls_header-our_ref separated by '-'.                "DV5K9A05WW-
  ls_header-vendor     = is_trans-lifnr.
  ls_header-comp_code  = is_contract-bukrs.
  ls_header-purch_org  = is_contract-ekorg.
  ls_header-pur_group  = is_contract-ekgrp.
  ls_header-pmnttrms   = is_contract-zterm.

  clear ls_headerx.
  ls_headerx-doc_type  = abap_true.
  ls_headerx-doc_date  = abap_true.
  ls_headerx-ref_1     = abap_true.
*  ls_headerx-our_ref   = abap_true.                       "DV5K9A05WW-
  ls_headerx-vendor    = abap_true.
  ls_headerx-purch_org = abap_true.
  ls_headerx-pur_group = abap_true.
  ls_headerx-comp_code = abap_true.
  ls_headerx-pmnttrms  = abap_true.


* line item
  clear ls_item.
  ls_item-po_item       = c_10.
  ls_item-stge_loc      = get_storage_location( is_contract-werks ).
  ls_item-trackingno    = is_contract-bednr.
  ls_item-quantity      = ld_output_qty.
  ls_item-po_unit       = is_contract-meins.
  ls_item-agreement     = is_contract-ebeln.
  ls_item-agmt_item     = is_contract-ebelp.
  ls_item-conf_ctrl     = id_conf_ctrl.
  ls_item-no_rounding   = abap_true.
  append ls_item to lt_item.

*
  clear ls_itemx.
  ls_itemx-po_item      = c_10.
  ls_itemx-stge_loc     = abap_true.
  ls_itemx-trackingno   = abap_true.
  ls_itemx-quantity     = abap_true.
  ls_itemx-po_unit      = abap_true.
  ls_itemx-agreement    = abap_true.
  ls_itemx-agmt_item    = abap_true.
  ls_itemx-conf_ctrl    = abap_true.
  ls_itemx-no_rounding  = abap_true.
  append ls_itemx to lt_itemx.

  clear ls_poschedule.
  ls_poschedule-po_item        = c_10.
  ls_poschedule-sched_line     = 1.
  ls_poschedule-delivery_date  = is_trans-ztds_tran_dt.
  ls_poschedule-quantity       = ld_output_qty.
  append ls_poschedule to lt_poschedule.

  clear ls_poschedulex.
  ls_poschedulex-po_item       = c_10.
  ls_poschedulex-sched_line    = 1.
  ls_poschedulex-delivery_date = abap_true.
  ls_poschedulex-quantity      = abap_true.
  append ls_poschedulex to lt_poschedulex.

*--> Start of Insert DV5K9A05WW - Chuong Le - 2017/01/04
  APPEND INITIAL LINE TO lt_potextheader ASSIGNING FIELD-SYMBOL(<text>).
  <text>-text_id   = mc_f99.
  <text>-text_form = '* '.
  <text>-text_line = |{ mc_tds }-{ is_trans-ztds_trml_id }|.
*<-- End of Insert DV5K9A05WW - Chuong Le - 2017/01/04

** Call create PO bapi
  create_po( EXPORTING is_header       = ls_header
                       is_headerx      = ls_headerx
                       id_sub_stus     = '000'
             IMPORTING et_return       = et_return
                       ed_ebeln        = ed_ebeln
             CHANGING  ct_items        = lt_item
                       ct_itemsx       = lt_itemx
                       ct_poschedule   = lt_poschedule
                       ct_poschedulex  = lt_poschedulex
                       ct_potextheader = lt_potextheader ). "DV5K9A05WW+



endmethod.


METHOD create_new_transport_po.
************************************************************************
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------    ----------          ---------------       *
* Chuong Le                  2017/01/04          DV5K9A05WW            *
*                                                                      *
* Short Description: CR306321-T347799                                  *
*    - Save Our Reference in PO header text instead of EKKO-UNSEZ.     *
*----------------------------------------------------------------------*
* Line Parent                2015-02-02          DV5K992970            *
*                                                                      *
* Short Description: C248034-T268218 SM-ENH-820                        *
*                    Change the field used for gross volume.           *
*----------------------------------------------------------------------*
* Jennifer Alexander         2014-01-10          DV5K984000            *
*                                                                      *
* Short Description: C220940-T254182 Determine if the fuel carrier is  *
*                    compensated in gross volume or paid volume.       *
*----------------------------------------------------------------------*

  CONSTANTS:
    c_10           TYPE bapimepoitem-po_item VALUE '00010'.

  DATA:

    ld_output_qty  TYPE p DECIMALS 2,
    ld_unit_tds    TYPE zmm_flcm_tds-meins,
    ld_input_qty   TYPE p DECIMALS 2.

  DATA:
    ls_header      TYPE bapimepoheader,
    ls_headerx     TYPE bapimepoheaderx,
    lt_item        TYPE bapimepoitem_tp,
    ls_item        TYPE bapimepoitem,
    lt_poschedule  TYPE bapimeposchedule_tp,
    ls_poschedule  TYPE bapimeposchedule,
    lt_poschedulex TYPE bapimeposchedulx_tp,
    ls_poschedulex TYPE bapimeposchedulx,
    lt_itemx       TYPE bapimepoitemx_tp,
    ls_itemx       TYPE bapimepoitemx,
    lt_potextheader TYPE bapimepotextheader_tp,            "DV5K9A05WW+
    diff           TYPE i.                             "+DV5K984000

  FIELD-SYMBOLS:
    <item>           LIKE LINE OF lt_item,
    <itemx>          LIKE LINE OF lt_itemx.

* first see if we need to convert the quantity to the quantity of the contract
  REFRESH lt_item.
  REFRESH lt_itemx.
  REFRESH lt_poschedule.
  REFRESH lt_poschedulex.

* Begin of C220940-T254182 2014-01-10 DV5K984000
*  ld_input_qty = is_trans-ztds_paid_vol.

  diff = is_trans-ztds_bol_net - is_trans-ztds_paid_vol.

  IF diff NE 0.
    ld_input_qty = is_trans-ztds_paid_vol.
  ELSE.
    IF is_contract-bednr = mc_gross_vol.
*      ld_input_qty = is_trans-ztds_grs_vol.    "D-DV5K992970
       ld_input_qty = is_trans-ztds_bol_grs.    "I-DV5K992970
    ELSE.
      ld_input_qty = is_trans-ztds_paid_vol.
    ENDIF.
  ENDIF.
* End of C220940-T254182 2014-01-10 DV5K984000

  IF is_trans-meins NE is_contract-meins.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                      = ld_input_qty
*           NO_TYPE_CHECK              = 'X'
*           ROUND_SIGN                 = ' '
        unit_in                    = is_trans-meins
        unit_out                   = is_contract-meins
      IMPORTING
*           ADD_CONST                  =
*           DECIMALS                   =
*           DENOMINATOR                =
*           NUMERATOR                  =
        output                     = ld_output_qty
      EXCEPTIONS
        conversion_not_found       = 1
        division_by_zero           = 2
        input_invalid              = 3
        output_invalid             = 4
        overflow                   = 5
        type_invalid               = 6
        units_missing              = 7
        unit_in_not_found          = 8
        unit_out_not_found         = 9
        OTHERS                     = 10
                 .

    IF sy-subrc <> 0.
*  Raise exception
      IF ( 1 = 2 ). MESSAGE e046(zz_flcm). ENDIF.
      RAISE EXCEPTION TYPE zcx_flcm_error
        EXPORTING
          msgid    = mc_msgid_flcm
          msgty    = mc_msgty_error
          msgno    = '046'
          sub_stus = id_sub_stus.
*           msgtab   = rt_return.
    ENDIF.
  ELSE.
    ld_output_qty = ld_input_qty.
  ENDIF.

* header
  CLEAR ls_header.
  ls_header-doc_type   = 'FB  '.
  ls_header-doc_date   = is_trans-ztds_bol_dt.
  ls_header-ref_1      = is_trans-ztds_bol_nbr.
*  CONCATENATE mc_car is_trans-ztds_trml_id                "DV5K9A05WW-
*  INTO ls_header-our_ref SEPARATED BY '-'.                "DV5K9A05WW-
  ls_header-vendor     = is_trans-zcarrier.
  ls_header-purch_org  = is_contract-ekorg.
  ls_header-pur_group  = is_contract-ekgrp.

  CLEAR ls_headerx.
  ls_headerx-doc_type  = abap_true.
  ls_headerx-doc_date  = abap_true.
  ls_headerx-ref_1     = abap_true.
*  ls_headerx-our_ref   = abap_true.                       "DV5K9A05WW-
  ls_headerx-vendor    = abap_true.
  ls_headerx-purch_org = abap_true.
  ls_headerx-pur_group = abap_true.


* line item
  CLEAR ls_item.
  ls_item-po_item       = c_10.
  ls_item-quantity      = ld_output_qty.
  ls_item-po_unit       = is_contract-meins.
  ls_item-agreement     = is_contract-ebeln.
  ls_item-agmt_item     = is_contract-ebelp.
  ls_item-stge_loc      = get_storage_location( is_contract-werks ).
  ls_item-trackingno    = is_contract-bednr.
  ls_item-no_rounding   = abap_true.
  APPEND ls_item TO lt_item.

*
  CLEAR ls_itemx.
  ls_itemx-po_item      = c_10.
  ls_itemx-quantity     = abap_true.
  ls_itemx-po_unit      = abap_true.
  ls_itemx-agreement    = abap_true.
  ls_itemx-agmt_item    = abap_true.
  ls_itemx-stge_loc     = abap_true.
  ls_itemx-trackingno   = abap_true.
  ls_itemx-no_rounding  = abap_true.
  APPEND ls_itemx TO lt_itemx.

  CLEAR ls_poschedule.
  ls_poschedule-po_item        = c_10.
  ls_poschedule-sched_line     = 1.
  ls_poschedule-delivery_date  = is_trans-ztds_bol_dt.
  ls_poschedule-quantity       = ld_output_qty.
  APPEND ls_poschedule TO lt_poschedule.

  CLEAR ls_poschedulex.
  ls_poschedulex-po_item       = c_10.
  ls_poschedulex-sched_line    = 1.
  ls_poschedulex-delivery_date = abap_true.
  ls_poschedulex-quantity      = abap_true.
  APPEND ls_poschedulex TO lt_poschedulex.

*--> Start of Insert DV5K9A05WW - Chuong Le - 2017/01/04
  APPEND INITIAL LINE TO lt_potextheader ASSIGNING FIELD-SYMBOL(<text>).
  <text>-text_id   = mc_f99.
  <text>-text_form = '* '.
  <text>-text_line = |{ mc_car }-{ is_trans-ztds_trml_id }|.
*<-- End of Insert DV5K9A05WW - Chuong Le - 2017/01/04

** Call create PO bapi
  create_po( EXPORTING is_header       = ls_header
                       is_headerx      = ls_headerx
                       id_sub_stus     = id_sub_stus
             IMPORTING et_return       = et_return
                       ed_ebeln        = ed_ebeln
             CHANGING  ct_items        = lt_item
                       ct_itemsx       = lt_itemx
                       ct_poschedule   = lt_poschedule
                       ct_poschedulex  = lt_poschedulex
                       ct_potextheader = lt_potextheader ). "DV5K9A05WW+



ENDMETHOD.


method CREATE_PHYSINV_ADJUSTMENT.

  DATA:
    lo_flcm_exception   TYPE REF TO zcx_flcm_error.

  DATA:
    lt_create_items  TYPE ZBAPI_PHYSINV_CREATE_ITEMS_T,
    lt_post_items    TYPE ZBAPI_PHYSINV_POST_ITEMS_T,
    lt_count_items   TYPE ZBAPI_PHYSINV_COUNT_ITEMS_T.


  DATA:
    ls_header        TYPE bapi_physinv_create_head,
    ld_inv_doc       TYPE bseg-belnr,
    ld_inv_tot_count type BAPI_PHYSINV_COUNT_ITEMS-entry_qnt,
    lt_return        TYPE bapiret2tab.



  FIELD-SYMBOLS:
    <create_items>         LIKE LINE OF lt_create_items,
    <post_items>           LIKE LINE OF lt_post_items,
    <count_items>          LIKE LINE OF lt_count_items.

  REFRESH:
    rt_return.
  REFRESH:
    lt_return.


* Create Physical Inventory Document
** Header
  CLEAR ls_header.
  ls_header-plant      = is_trans-werks.
  ls_header-stge_loc   = get_storage_location( id_werks    = is_trans-werks
                                               id_sub_stus = mc_sub_status_phyi_fail ).
  ls_header-doc_date   = is_trans-ztds_tran_dt.
  ls_header-plan_date  = is_trans-ztds_folio_frzdt.

* Line item
  APPEND INITIAL LINE TO lt_create_items ASSIGNING <create_items>.
  <create_items>-material   = is_trans-matnr.
  <create_items>-stock_type = '1'.
  <create_items>-alt_unit   = abap_true.


* Call create multi bapi
  create_physinv_create_mult(  EXPORTING is_header   = ls_header
                                         it_item     = lt_create_items
                                         id_sub_stus = mc_sub_status_phyi_fail
                               IMPORTING et_return   = rt_return ).

  delete adjacent duplicates from rt_return
      comparing type id number.

  append lines of rt_return to lt_return.

  ld_inv_doc = find_inv_doc( it_msg = rt_return ).


* do conversion
  ld_inv_tot_count = convert_in_transit_stock( is_trans    = is_trans
                                               it_maramarc = it_maramarc ).



* set data for inventory count
* Line item
  APPEND INITIAL LINE TO lt_count_items ASSIGNING <count_items>.
  <count_items>-item       = '001'.
  <count_items>-material   = is_trans-matnr.
  <count_items>-entry_qnt  = ld_inv_tot_count.
  <count_items>-entry_uom  = is_trans-meins.

* Call create count
  create_physinv_count(  EXPORTING is_header    = ls_header
                                   it_item      = lt_count_items
                                   id_inv_doc   = ld_inv_doc
                                   id_post_date = is_trans-ztds_folio_frzdt
                                   id_sub_stus  = mc_sub_status_phyi_fail
                         IMPORTING et_return    = rt_return ).

  delete adjacent duplicates from rt_return
      comparing type id number.

  append lines of rt_return to lt_return.

* set data for inventory difference
* Line item
  APPEND INITIAL LINE TO lt_post_items ASSIGNING <post_items>.
  <post_items>-item       = '001'.
  <post_items>-material   = is_trans-matnr.


* call inventory difference

  create_physinv_postdiff(  EXPORTING is_header    = ls_header
                                      it_item      = lt_post_items
                                      id_inv_doc   = ld_inv_doc
                                      id_post_date = is_trans-ztds_folio_frzdt
                                      id_sub_stus  = mc_sub_status_phyi_fail
                            IMPORTING et_return    = rt_return ).

  delete adjacent duplicates from rt_return
      comparing type id number.

  append lines of rt_return to lt_return.
  rt_return[] = lt_return[].



endmethod.


METHOD CREATE_PHYSINV_COUNT.
***********************************************************************
*----------------------------------------------------------------------*
* Changed by                   Date          Tracking number   SIR/ENH *
* -------------------------    ----------    ---------------   -----   *
* Steven Qiu                  2013/08/08     DV5K980921        243167  *
* 1) Add more display logs to know the processing flow of the program. *
* 2) Put fix to reset the flag.                                        *
*----------------------------------------------------------------------*
* Charles Darby                2013-01-11    DV5K976372                *
*                                                                      *
* Short Description: CR217330-T228106 SM-ENH-822                       *
* Use the fiscal year of the document and not the current year         *
*----------------------------------------------------------------------*

  DATA:
    ls_return          TYPE bapiret2,
    lt_return          TYPE bapiret2_t.

  DATA:
    ld_success         TYPE abap_bool.

** Begin of Insert  DV5K980921 -->>
    MESSAGE s900(zz-cn2) WITH
       'Calling BAPI_MATPHYSINV_COUNT'.
** End of Insert  DV5K980921 --<<

* Perform update the inventory count
  CALL FUNCTION 'BAPI_MATPHYSINV_COUNT'
    EXPORTING
      PHYSINVENTORY             = id_inv_doc
      FISCALYEAR                = id_post_date+0(4) "U-DV5K976372
*     PERCENTAGE_VARIANCE       =
      COUNT_DATE                = id_post_date
    TABLES
      ITEMS                     = it_item
      RETURN                    = et_return
*     SERIALNUMBERS             =
*     EXTENSIONIN               =
            .
*
* Update success?
  IF ( find_error( et_return ) = abap_false ).
** Begin of Insert  DV5K980921 -->>
    MESSAGE s900(zz-cn2) WITH
       'Commit BAPI_MATPHYSINV_COUNT'.
** End of Insert  DV5K980921 --<<
*   Commit changes
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait     = abap_true
      IMPORTING
        return   = ls_return.
    IF ( NOT ls_return-type CA mc_error_types ).
*     Success
      ld_success = abap_true.
    ELSE.
*     Failure
      APPEND ls_return TO et_return.
    ENDIF.
  ENDIF.

* Update failure
  IF ( ld_success = abap_false ).
** Begin of Insert  DV5K980921 -->>
    MESSAGE s900(zz-cn2) WITH
       'Rollback BAPI_MATPHYSINV_COUNT'.
** End of Insert  DV5K980921 --<<
*   Rollback changes
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = ls_return.
    IF ( ls_return-type CA mc_error_types ).
      APPEND ls_return TO et_return.
    ENDIF.
    lt_return[] = et_return[].
    delete_physinv_record( EXPORTING id_inv_doc = id_inv_doc      "U-DV5K976372
                                     id_post_date = id_post_date  "U-DV5K976372
                           IMPORTING rt_return = et_return ).     "U-DV5K976372

    append lines of et_return to lt_return.
    et_return[] = lt_return[].
*   Raise exception
    IF ( 1 = 2 ). MESSAGE e042(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '042'
        sub_stus = id_sub_stus
        msgtab   = et_return.
  ENDIF.

ENDMETHOD.


METHOD CREATE_PHYSINV_CREATE_MULT.
*----------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* Steven Qiu            243167    2013/08/08           DV5K980921      *
* 1) Add more display logs to know the processing flow of the program. *
* 2) Put fix to reset the flag.                                        *
*----------------------------------------------------------------------*


  DATA:
    ls_return          TYPE bapiret2.

  DATA:
    ld_success         TYPE abap_bool.



  REFRESH:
    et_return.

** Begin of Insert  DV5K980921 -->>
    MESSAGE s900(zz-cn2) WITH
       'Calling BAPI_MATPHYSINV_CREATE_MULT'.
** End of Insert  DV5K980921 --<<

* Perform physical inventory document
  CALL FUNCTION 'BAPI_MATPHYSINV_CREATE_MULT'
    EXPORTING
      HEAD           = is_header
*     MAXITEMS       =
    TABLES
      ITEMS          = it_item
      RETURN         = et_return
           .

* it seems that the messages that sap return are duplicated and there is no oss so we will parse



*
* Update success?
  IF ( find_error( et_return ) = abap_false ).
** Begin of Insert  DV5K980921 -->>
    MESSAGE s900(zz-cn2) WITH
       'Commit BAPI_MATPHYSINV_CREATE_MULT'.
** End of Insert  DV5K980921 --<<

*   Commit changes
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait     = abap_true
      IMPORTING
        return   = ls_return.
    IF ( NOT ls_return-type CA mc_error_types ).
*     Success
      ld_success = abap_true.
    ELSE.
*     Failure
      APPEND ls_return TO et_return.
    ENDIF.
  ENDIF.

* Update failure
  IF ( ld_success = abap_false ).
** Begin of Insert  DV5K980921 -->>
    MESSAGE s900(zz-cn2) WITH
       'Rollback BAPI_MATPHYSINV_CREATE_MULT'.
** End of Insert  DV5K980921 --<<
*   Rollback changes
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = ls_return.
    IF ( ls_return-type CA mc_error_types ).
      APPEND ls_return TO et_return.
    ENDIF.
*   Raise exception
    IF ( 1 = 2 ). MESSAGE e042(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '042'
        sub_stus = id_sub_stus
        msgtab   = et_return.
  ENDIF.

ENDMETHOD.


METHOD create_physinv_postdiff.
***********************************************************************
*----------------------------------------------------------------------*
* Changed / Created by         Date          Tracking number   SIR/ENH *
* -------------------------    ----------    ---------------   -----   *
* Steven Qiu                   2013/08/08    DV5K980921        243167  *
* 1) Add more display logs to know the processing flow of the program. *
* 2) Put fix to reset the flag.                                        *
*----------------------------------------------------------------------*
* Charles Darby                2013-01-11    DV5K976372                *
*                                                                      *
* Short Description: CR217330-T228106 SM-ENH-822                       *
* Use the fiscal year of the document and not the current year         *
*----------------------------------------------------------------------*
* Eva Chan                     2012-10-17    DV5K974718                *
*                                            DV5K975485                *
*                                            DV5K975531                *
* Short Description: CR211196-T220773 SM-ENH-822                       *
* Replace msg zz_flcm042 by a new message                              *
* "Physical inventory adjustment failed, volume was 999999"            *
*----------------------------------------------------------------------*

  DATA:
    ls_return          TYPE bapiret2,
    lt_return          TYPE bapiret2_t,
    lv_physinv_diff    TYPE sy-msgv1,                       "DV5K974718
    lt_return2         TYPE bapiret2_t,                     "DV5K974718
    lt_physinv_items   TYPE TABLE OF bapi_physinv_item.     "DV5K974718

  DATA:
    ld_success         TYPE abap_bool.

  FIELD-SYMBOLS:  <ls_physinv_item> TYPE bapi_physinv_item. "DV5K974718

  REFRESH:
    et_return.

** Begin of Insert  DV5K980921 -->>
    MESSAGE s900(zz-cn2) WITH
       'Calling BAPI_MATPHYSINV_POSTDIFF'.
** End of Insert  DV5K980921 --<<

* Perform post the inventory difference
  CALL FUNCTION 'BAPI_MATPHYSINV_POSTDIFF'
    EXPORTING
      physinventory         = id_inv_doc
      fiscalyear            = id_post_date+0(4) "U-DV5K976372
      pstng_date            = id_post_date
*     THRESHOLD_VALUE       =
    TABLES
*     ITEMS                 =
      return                = et_return.

*
* Update success?
  IF ( find_error( et_return ) = abap_false ).
** Begin of Insert  DV5K980921 -->>
    MESSAGE s900(zz-cn2) WITH
       'Commit BAPI_MATPHYSINV_POSTDIFF'.
** End of Insert  DV5K980921 --<<
*   Commit changes
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = abap_true
      IMPORTING
        return = ls_return.
    IF ( NOT ls_return-type CA mc_error_types ).
*     Success
      ld_success = abap_true.
    ELSE.
*     Failure
      APPEND ls_return TO et_return.
    ENDIF.
  ENDIF.

* Update failure
  IF ( ld_success = abap_false ).
** Begin of Insert  DV5K980921 -->>
    MESSAGE s900(zz-cn2) WITH
       'Rollback BAPI_MATPHYSINV_POSTDIFF'.
** End of Insert  DV5K980921 --<<
*   Rollback changes
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = ls_return.
    IF ( ls_return-type CA mc_error_types ).
      APPEND ls_return TO et_return.
    ENDIF.
    lt_return[] = et_return[].
* Begin of DV5K974718
** Begin of Insert  DV5K980921 -->>
    MESSAGE s900(zz-cn2) WITH
       'Calling BAPI_MATPHYSINV_GETDETAIL'.
** End of Insert  DV5K980921 --<<
    CALL FUNCTION 'BAPI_MATPHYSINV_GETDETAIL'
      EXPORTING
        physinventory = id_inv_doc
        fiscalyear    = id_post_date+0(4) "U-DV5K976372
      TABLES
        items         = lt_physinv_items
        return        = lt_return2.
    IF lt_physinv_items[] IS NOT INITIAL.
      READ TABLE lt_physinv_items ASSIGNING <ls_physinv_item> INDEX 1.
      IF sy-subrc = 0.
        CLEAR: lv_physinv_diff.
        lv_physinv_diff =  <ls_physinv_item>-quantity - <ls_physinv_item>-book_qty.
        CONDENSE lv_physinv_diff.                           "DV5K975485 DV5K975531
        CONCATENATE lv_physinv_diff
                    <ls_physinv_item>-base_uom
               INTO lv_physinv_diff SEPARATED BY space.

      ENDIF.
    ENDIF.
* End of DV5K974718

    delete_physinv_record( EXPORTING id_inv_doc = id_inv_doc       "U-DV5K976372
                                     id_post_date = id_post_date   "U-DV5K976372
                           IMPORTING rt_return = et_return ).      "U-DV5K976372
    APPEND LINES OF et_return TO lt_return.
    et_return[] = lt_return[].
*   Raise exception
    IF ( 1 = 2 ). MESSAGE e042(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
* Begin of DV5K974718
*        msgno    = '042'
        msgno    = '099'
        msgv1    = lv_physinv_diff
* End of DV5K974718
        sub_stus = id_sub_stus
        msgtab   = et_return.
  ENDIF.

ENDMETHOD.


method CREATE_PO.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by             Date                Tracking number *
* -------------------------        ----------          --------------- *
* Chuong Le  CR306321-T347799      2017/01/04          DV5K9A05WW      *
*                                                                      *
*    - Save Our Reference in PO header text instead of EKKO-UNSEZ.     *
*----------------------------------------------------------------------*
* Von Kaisser Almeria              2016-07-28          DV5K9A03CO      *
*                                                      DV5K9A03QW      *
* Short Description : CR269546 TK334189                                *
*                     Add controls to avoid creating POs for           *
*                     unreasonable amounts                             *
*----------------------------------------------------------------------*


  DATA:
    ls_return          TYPE bapiret2.

  DATA:
    ld_success          TYPE abap_bool,
    lv_exppurchaseorder TYPE bapimepoheader-po_number,
    lv_expheader        TYPE BAPIMEPOHEADER,
    lv_exppoexpimheader TYPE bapieikp.

  REFRESH:
    et_return.

***BEGIN OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO
  DATA: lv_test TYPE char1.

  IF id_test IS NOT INITIAL.
    lv_test = abap_true.
  ELSE.
    lv_test = abap_false.
  ENDIF.
***END OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO

   CALL FUNCTION 'BAPI_PO_CREATE1'
     EXPORTING
       POHEADER                     = is_header
       POHEADERX                    = is_headerx
      TESTRUN                      = lv_test "I - XT18912 - CR269546 TK334189 - DV5K9A03CO
     IMPORTING
       EXPPURCHASEORDER             = lv_exppurchaseorder
       EXPHEADER                    = lv_expheader
       EXPPOEXPIMPHEADER            = lv_exppoexpimheader
     TABLES
       RETURN                       = et_return
       POITEM                       = ct_items
       POITEMX                      = ct_itemsx
       POSCHEDULE                   = ct_poschedule
       POSCHEDULEX                  = ct_poschedulex
       potextheader                 = ct_potextheader.     "DV5K9A05WW+

***BEGIN OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO
  IF lv_test EQ abap_true.
*   Update success?
    IF ( find_error( et_return ) = abap_true ).
      CLEAR ct_items.
    ENDIF.
***BEGIN OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03QW
*   Rollback changes
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = ls_return.

    REFRESH: et_return.
***END OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03QW
  ELSE.
***END OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO
*
*   Update success?
    IF ( find_error( et_return ) = abap_false ).
*     Commit changes
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait     = abap_true
        IMPORTING
          return   = ls_return.
      IF ( NOT ls_return-type CA mc_error_types ).
*       Success
        ld_success = abap_true.
        ed_ebeln = lv_exppurchaseorder.
      ELSE.
*       Failure
        APPEND ls_return TO et_return.
      ENDIF.
    ENDIF.

*   Update failure
    IF ( ld_success = abap_false ).
*     Rollback changes
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
        IMPORTING
          return = ls_return.
      IF ( ls_return-type CA mc_error_types ).
        APPEND ls_return TO et_return.
      ENDIF.
*     Raise exception
      IF ( 1 = 2 ). MESSAGE e062(zz_flcm). ENDIF.
      RAISE EXCEPTION TYPE zcx_flcm_error
        EXPORTING
          msgid    = mc_msgid_flcm
          msgty    = mc_msgty_error
          msgno    = '062'
          sub_stus = id_sub_stus
          msgtab   = et_return.
    ENDIF.
  ENDIF. "I - XT18912 - CR269546 TK334189 - DV5K9A03CO

endmethod.


METHOD DB_UPDATE_TRANS_DIFF_INV.
*----------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* Michel Richard        274769     2014/10/14          DV5K990720      *
* Renamed field                                                        *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* Nancy Bergeron        261851     2014/06/12          DV5K987403      *
* Add new logic to calculate the difference between TDS and SAP        *
* inventory.   SM-ENH-822                                              *
*----------------------------------------------------------------------*


  DATA: ld_sap_inv       TYPE zmm_flcm_tds-zsap_inv,
        ld_inv_diff      TYPE zmm_flcm_tds-zinv_diff,
        ld_cumulate_diff TYPE zmm_flcm_tds-zinv_diff_delta.

  IF id_sap_inv IS SUPPLIED.
     ld_sap_inv = id_sap_inv.
  ELSE.
     ld_sap_inv = 0.
  ENDIF.

  IF id_inv_diff IS SUPPLIED.
     ld_inv_diff = id_inv_diff.
  ELSE.
     ld_inv_diff = 0.
  ENDIF.

  IF id_cumulate_diff IS SUPPLIED.
     ld_cumulate_diff = id_cumulate_diff.
  ELSE.
     ld_cumulate_diff = 0.
  ENDIF.

* Update transaction status to error with message
  CALL METHOD zcl_smint60a_zmm_flcm_tds=>set_diff_inv_and_messages
    EXPORTING
      im_tran_ref_nbr   = it_trans-ztds_tran_ref_nb
      im_commit         = abap_true
      im_sap_inv        = ld_sap_inv
      im_inv_diff       = ld_inv_diff
      im_cumulate_diff  = ld_cumulate_diff
      im_messages       = it_msg
    EXCEPTIONS
      error_in_save     = 1
      OTHERS            = 2.

  IF ( sy-subrc <> 0 ).
    IF ( 1 = 2 ). MESSAGE e039(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '039'
        msgtab   = it_msg.
  ENDIF.

ENDMETHOD.


METHOD db_update_trans_error.

  DATA:
    lt_msg         TYPE bapiret2_tab.

  FIELD-SYMBOLS:
    <lt_msg>       TYPE bapiret2.

  lt_msg = io_exception->get_bapimsg_table( ).


* if the process that has triggered the error may have had other bapi calls that were successful
* therefre we want all of them written to the app log so we will put them all in the tabel
  if it_msg[] is SUPPLIED.
     if it_msg[] is not initial.
         loop at lt_msg assigning <lt_msg>.
            append <lt_msg> to it_msg.
         endloop.
         lt_msg[] = it_msg[].
     endif.
  endif.
* Update transaction status to error with message
  CALL METHOD zcl_smint60a_zmm_flcm_tds=>set_status_and_messages
    EXPORTING
      im_tran_ref_nbr   = is_trans-ztds_tran_ref_nb
      im_commit         = abap_true
      im_tran_status    = mc_status_error
      im_tran_substatus = io_exception->sub_stus
      im_messages       = lt_msg
    EXCEPTIONS
      error_in_save     = 1
      OTHERS            = 2.

  IF ( sy-subrc <> 0 ).
    IF ( 1 = 2 ). MESSAGE e040(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '040'
        msgtab   = lt_msg.
  ENDIF.

ENDMETHOD.


METHOD db_update_trans_not_relevant.

  DATA:
    lt_msg         TYPE bapiret2_tab.

  lt_msg = io_exception->get_bapimsg_table( ).

* Update transaction status to error with message
  CALL METHOD zcl_smint60a_zmm_flcm_tds=>set_status_and_messages
    EXPORTING
      im_tran_ref_nbr   = is_trans-ztds_tran_ref_nb
      im_commit         = abap_true
      im_tran_status    = mc_status_not_relevant
      im_tran_substatus = io_exception->sub_stus
      im_messages       = lt_msg
    EXCEPTIONS
      error_in_save     = 1
      OTHERS            = 2.

  IF ( sy-subrc <> 0 ).
    IF ( 1 = 2 ). MESSAGE e065(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '065'
        msgtab   = lt_msg.
  ENDIF.

ENDMETHOD.


METHOD db_update_trans_success.

  DATA:
    ld_sub_stus type zmm_flcm_tds-ztran_sub_stus.


  IF id_sub_stus IS SUPPLIED.
     ld_sub_stus = id_sub_stus.
  ELSE.
     ld_sub_stus = mc_sub_status_none.
  ENDIF.

* Update transaction status to error with message
  CALL METHOD zcl_smint60a_zmm_flcm_tds=>set_status_and_messages
    EXPORTING
      im_tran_ref_nbr   = is_trans-ztds_tran_ref_nb
      im_commit         = abap_true
      im_tran_status    = mc_status_processed
      im_tran_substatus = ld_sub_stus
      im_messages       = it_msg
    EXCEPTIONS
      error_in_save     = 1
      OTHERS            = 2.

  IF ( sy-subrc <> 0 ).
    IF ( 1 = 2 ). MESSAGE e039(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '039'
        msgtab   = it_msg.
  ENDIF.

ENDMETHOD.


method DELETE_PHYSINV_RECORD.
***********************************************************************
*----------------------------------------------------------------------*
* Changed / Created by         Date          Tracking number   SIR/ENH *
* -------------------------    ----------    ---------------   -----   *
* Steven Qiu                   2013/08/08    DV5K980921        243167  *
* 1) Add more display logs to know the processing flow of the program. *
* 2) Put fix to reset the flag.                                        *
*----------------------------------------------------------------------*
* Charles Darby                2013-01-11    DV5K976372                *
*                                                                      *
* Short Description: CR217330-T228106 SM-ENH-822                       *
* Added IMPORTING parameter ID_POST_DATE                               *
* Use the fiscal year of the document and not the current year         *
*----------------------------------------------------------------------*

DATA:
  lt_bdcdata      TYPE standard table of bdcdata,
  lt_bdc_message  TYPE standard table of bdcmsgcoll,
  ls_bdcdata      TYPE bdcdata,
  ls_bdc_message  TYPE bdcmsgcoll,
  ls_bapiret2     TYPE bapiret2,
  ld_output_qty   TYPE ekes-menge,
  ld_output_qtyn(13)  TYPE n,
  ld_input_qty    TYPE p decimals 2,
  ld_unit_tds     TYPE zmm_flcm_tds-meins,
  ld_bdc_mode.

DATA:
  ld_success      TYPE abap_bool,
  ld_msgv1        TYPE sy-msgv1,
  ld_msgv2        TYPE sy-msgv2.


  REFRESH:
    lt_bdcdata,
    lt_bdc_message.

  CLEAR ls_bdcdata.
  ls_bdcdata-program  = 'SAPMM07I'.
  ls_bdcdata-dynpro   = '0701'.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata to lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'RM07I-IBLNR'.
  ls_bdcdata-fval = id_inv_doc.
  APPEND ls_bdcdata to lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'RM07I-GJAHR'.
  ls_bdcdata-fval = id_post_date+0(4).       "U-DV5K976372
  APPEND ls_bdcdata to lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'BDC_OKCODE'.
  ls_bdcdata-fval = '=DL'.
  APPEND ls_bdcdata to lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-program  = 'SAPLSPO1'.
  ls_bdcdata-dynpro   = '0100'.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata to lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'BDC_OKCODE'.
  ls_bdcdata-fval = '=YES'.
  APPEND ls_bdcdata to lt_bdcdata.

** Begin of Insert  DV5K980921 -->>
  MESSAGE s900(zz-cn2) WITH
     'Calling transaction MI02 using BDC'.
** End of Insert  DV5K980921 --<<

  ld_bdc_mode = 'N'.
* Invoke Call Transaction
  CALL TRANSACTION 'MI02' USING lt_bdcdata
                          MODE ld_bdc_mode
                          UPDATE 'S'
                          MESSAGES INTO lt_bdc_message.

  ld_success = abap_true.

  loop at lt_bdc_message into ls_bdc_message.

    ls_bapiret2-type       = ls_bdc_message-msgtyp.
    ls_bapiret2-id          = ls_bdc_message-msgid.
    ls_bapiret2-number      = ls_bdc_message-msgnr.
    ls_bapiret2-message_v1  = ls_bdc_message-msgv1.
    ls_bapiret2-message_v2  = ls_bdc_message-msgv2.
    ls_bapiret2-message_v3  = ls_bdc_message-msgv3.
    ls_bapiret2-message_v4  = ls_bdc_message-msgv4.
    append ls_bapiret2 to rt_return.
    if ls_bdc_message-msgtyp = 'E' or
       ls_bdc_message-msgtyp = 'A'.
       ld_success = abap_false.
    endif.

  endloop.

* Update failure
  IF ( ld_success = abap_false ).
** Begin of Insert  DV5K980921 -->>
    MESSAGE s900(zz-cn2) WITH
       'Transaction MI02 failed'.
** End of Insert  DV5K980921 --<<

*   Raise exception
    IF ( 1 = 2 ). MESSAGE e073(zz_flcm). ENDIF.
    ld_msgv1 = id_inv_doc.
    ld_msgv2 = id_post_date+0(4).                 "U-DV5K976372
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgv1    = ld_msgv1
        msgv2    = ld_msgv2
        msgno    = '073'
        sub_stus = '14'
        msgtab   = rt_return.
  ENDIF.




endmethod.


METHOD FIND_ERROR.

  FIELD-SYMBOLS:
    <msg>            LIKE LINE OF it_msg.

* Default as 'no error'
  rd_result = abap_false.

* Go through all messages
  LOOP AT it_msg ASSIGNING <msg>.
*   This is an error?  set as 'yes' then exit loop
    IF ( <msg>-type CA mc_error_types ).
      rd_result = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD FIND_INV_DOC.

  FIELD-SYMBOLS:
    <msg>            LIKE LINE OF it_msg.

* Default as 'no error'
  clear ed_inv_doc.

* Go through all messages to find inventory document number
  LOOP AT it_msg ASSIGNING <msg>.
*   if this is the inventory document number then set to yes
    IF ( <msg>-id EQ mc_m7 and <msg>-number EQ mc_710 ).
       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
           INPUT         = <msg>-message_v1
         IMPORTING
          OUTPUT         = ed_inv_doc
                 .

       EXIT.
     ENDIF.
  ENDLOOP.

  IF ( ed_inv_doc is initial ).
*   Raise exception
    IF ( 1 = 2 ). MESSAGE e042(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '042'
        sub_stus = id_sub_stus.
*       msgtab   = et_return.
  ENDIF.


ENDMETHOD.


METHOD fi_open_period_check.
*** Start of code added defect 638                          DV5K967528

  TYPES: BEGIN OF lt_fi_period,
           gjahr             TYPE t001b-frye1,
           monat             TYPE t001b-frpe1,
           period_open       TYPE xfeld,
        END OF lt_fi_period.

  DATA: BEGIN OF l_marv,
          lfgja              TYPE marv-lfgja,
          lfmon              TYPE marv-lfmon,
          vmgja              TYPE marv-vmgja,
          vmmon              TYPE marv-vmmon,
          xruem              TYPE marv-xruem,
        END OF l_marv.

  STATICS: st_mm_date        TYPE dats,
           sti_fi_periods    TYPE STANDARD TABLE OF lt_fi_period.

  FIELD-SYMBOLS: <fi_prd>    TYPE lt_fi_period.

  re_period_open = abap_false.

  IF st_mm_date IS INITIAL.
    SELECT SINGLE lfgja
                  lfmon
                  vmgja
                  vmmon
                  xruem
      FROM marv
      INTO l_marv
      WHERE bukrs = '1000'.
    IF sy-subrc = 0.
      IF l_marv-xruem = abap_true.
        st_mm_date+0(4) = l_marv-vmgja.
        st_mm_date+4(2) = l_marv-vmmon.
      ELSE.
        st_mm_date+0(4) = l_marv-lfgja.
        st_mm_date+4(2) = l_marv-lfmon.
      ENDIF.
      st_mm_date+6(2) = '01'.
    ENDIF.
  ENDIF.

  IF im_datum < st_mm_date.
    RETURN.
  ENDIF.

  READ TABLE sti_fi_periods ASSIGNING <fi_prd>
      WITH KEY gjahr = im_datum+0(4)
               monat = im_datum+4(2).
  IF sy-subrc > 0.
    APPEND INITIAL LINE TO sti_fi_periods ASSIGNING <fi_prd>.
    <fi_prd>-gjahr = im_datum+0(4).
    <fi_prd>-monat = im_datum+4(2).
    CALL FUNCTION 'FI_PERIOD_CHECK'
      EXPORTING
        i_bukrs          = '1000'
        i_gjahr          = <fi_prd>-gjahr
*        i_koart          = 'S'                  d- DV5K985836 mrr
        i_koart          = 'M'                  "i- DV5K985836 mrr
        i_monat          = <fi_prd>-monat
      EXCEPTIONS
        error_period     = 1
        error_period_acc = 2
        invalid_input    = 3
        OTHERS           = 4.
    IF sy-subrc = 0.
      <fi_prd>-period_open = abap_true.
    ELSE.
      <fi_prd>-period_open = abap_false.
    ENDIF.
  ENDIF.

  re_period_open = <fi_prd>-period_open.

ENDMETHOD.
*** End of code added defect 638                            DV5K967528


METHOD FORMAT_VALUE.

* Local constants
  CONSTANTS:
    lc_character          TYPE c        VALUE 'C'.

* Local variables
  DATA:
    lv_type               TYPE c.

  CLEAR rd_output.

  CHECK ( id_anyvalue IS NOT INITIAL ).

* Get field type
  DESCRIBE FIELD id_anyvalue TYPE lv_type.

  IF ( lv_type = lc_character ).

*   Character field

    WRITE id_anyvalue TO rd_output.
    SHIFT rd_output LEFT DELETING LEADING space.

  ELSE.

*   Other type

    WRITE id_anyvalue TO rd_output.

    IF ( rd_output IS INITIAL ).
      WRITE id_anyvalue TO rd_output USING NO EDIT MASK.
    ENDIF.

    SHIFT rd_output LEFT DELETING LEADING space.
    SHIFT rd_output LEFT DELETING LEADING '0'.

  ENDIF.

ENDMETHOD.


METHOD GET_GMCODE.

  DATA:
    ls_xref        TYPE zmm_flcm_xref.

  DATA:
    ld_valfrom     TYPE zmm_flcm_xref-zval_from,
    ld_msgv1       TYPE sy-msgv1,
    ld_msgv2       TYPE sy-msgv2.

  FIELD-SYMBOLS:
    <xref>         LIKE LINE OF mt_xref_gmcode.

  CLEAR rd_gm_code.


* Get value from buffer
  READ TABLE mt_xref_gmcode ASSIGNING <xref>
        WITH KEY zvar_nm        = mc_xref_gmcode
                 zval_from      = id_move_type BINARY SEARCH.

  IF ( sy-subrc = 0 ).
*   Found
    rd_gm_code = <xref>-zval_to.
  ELSE.
*   Get value from database table
    SELECT SINGLE *
             INTO ls_xref
             FROM zmm_flcm_xref
            WHERE ( zvar_nm        = mc_xref_gmcode    )
              AND ( zval_from      = id_move_type      ).
    IF ( sy-subrc = 0 ).
*     Return value and put it in buffer
      rd_gm_code   = ls_xref-zval_to.
      APPEND ls_xref TO mt_xref_gmcode.
      SORT mt_xref_gmcode BY zvar_nm zval_qlfy zval_from.
    ELSE.
*     Not found in db, error!
      ld_msgv1 = id_move_type.
      ld_msgv2 = mc_xref_gmcode.
      IF ( 1 = 2 ). MESSAGE e005(zz_flcm). ENDIF.
      RAISE EXCEPTION TYPE zcx_flcm_error
        EXPORTING
          msgid    = mc_msgid_flcm
          msgty    = mc_msgty_error
          msgno    = '005'
          msgv1    = ld_msgv1
          msgv2    = ld_msgv2
          sub_stus = id_sub_stus.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD get_max_amount_per_volume.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by             Date                Tracking number *
* -------------------------        ----------          --------------- *
* Von Kaisser Almeria              2016-07-28          DV5K9A03CO      *
*                                                                      *
* Short Description : CR269546 TK334189                                *
*                     Add controls to avoid creating POs for           *
*                     unreasonable amounts                             *
*----------------------------------------------------------------------*
  STATICS:
    ld_max_litre  TYPE bapicurext,
    ld_max_gallon TYPE bapicurext.

  DATA:
    ld_zval       TYPE zmm_flcm_parms-zval_to.

  IF ld_max_litre IS INITIAL.
    SELECT SINGLE zval_from
      FROM zmm_flcm_parms
      INTO ld_zval
     WHERE progname EQ mc_progname_817
       AND zparm_nm EQ mc_max_amount_per_litre.
    IF sy-subrc IS INITIAL.
      ld_max_litre = ld_zval.
    ELSE.
      CLEAR ld_max_litre.
    ENDIF.
  ENDIF.

  IF ld_max_gallon IS INITIAL.
    SELECT SINGLE zval_from
      FROM zmm_flcm_parms
      INTO ld_zval
     WHERE progname EQ mc_progname_817
       AND zparm_nm EQ mc_max_amount_per_gallon.
    IF sy-subrc IS INITIAL.
      ld_max_gallon = ld_zval.
    ELSE.
      CLEAR ld_max_gallon.
    ENDIF.
  ENDIF.

  IF ed_litre IS SUPPLIED.
    ed_litre = ld_max_litre.
  ENDIF.

  IF ed_gallon IS SUPPLIED.
    ed_gallon = ld_max_gallon.
  ENDIF.
ENDMETHOD.


METHOD get_movement_type.

  DATA:
    ls_xref        TYPE zmm_flcm_xref.

  DATA:
    ld_valfrom     TYPE zmm_flcm_xref-zval_from,
    ld_msgv1       TYPE sy-msgv1,
    ld_msgv2       TYPE sy-msgv2.

  FIELD-SYMBOLS:
    <xref>         LIKE LINE OF mt_xref_mvtp.

  CLEAR rd_move_type.

  IF ( id_canc_rbil IS SUPPLIED ).
    ld_valfrom = id_canc_rbil.
  ELSE.
    IF ( id_vol_sign IS SUPPLIED ).
      ld_valfrom = id_vol_sign.
    ELSE.
      IF ( 1 = 2 ). MESSAGE e041(zz_flcm). ENDIF.
      RAISE EXCEPTION TYPE zcx_flcm_error
        EXPORTING
          msgid = mc_msgid_flcm
          msgty = mc_msgty_error
          msgno = '041'
          sub_stus = id_sub_stus.
    ENDIF.
  ENDIF.

* Get value from buffer
  READ TABLE mt_xref_mvtp ASSIGNING <xref>
        WITH KEY zvar_nm        = mc_xref_move_type
                 zval_qlfy      = id_trans
                 zval_from      = ld_valfrom BINARY SEARCH.

  IF ( sy-subrc = 0 ).
*   Found
    rd_move_type = <xref>-zval_to.
  ELSE.
*   Get value from database table
    SELECT SINGLE *
             INTO ls_xref
             FROM zmm_flcm_xref
            WHERE ( zvar_nm        = mc_xref_move_type )
              AND ( zval_qlfy      = id_trans          )
              AND ( zval_from      = ld_valfrom        ).
    IF ( sy-subrc = 0 ).
*     Return value and put it in buffer
      rd_move_type = ls_xref-zval_to.
      APPEND ls_xref TO mt_xref_mvtp.
      SORT mt_xref_mvtp BY zvar_nm zval_qlfy zval_from.
    ELSE.
*     Not found in db, error!
      CONCATENATE id_trans ld_valfrom INTO ld_msgv1 SEPARATED BY mc_slash.
      ld_msgv2 = mc_xref_move_type.
      IF ( 1 = 2 ). MESSAGE e005(zz_flcm). ENDIF.
      RAISE EXCEPTION TYPE zcx_flcm_error
        EXPORTING
          msgid    = mc_msgid_flcm
          msgty    = mc_msgty_error
          msgno    = '005'
          msgv1    = ld_msgv1
          msgv2    = ld_msgv2
          sub_stus = id_sub_stus.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD get_plant_definition.

  CLEAR rs_plntdef.

* Get value from buffer
  READ TABLE mt_plantdef INTO rs_plntdef
        WITH KEY werks = id_werks BINARY SEARCH.

  IF ( sy-subrc = 0 ).
*   Found - nothing to do
  ELSE.
*   Get value from database table
    SELECT SINGLE *
             INTO rs_plntdef
             FROM zmm_flcm_plntdef
            WHERE ( werks = id_werks ).
    IF ( sy-subrc = 0 ).
*     Return value and put it in buffer
      APPEND rs_plntdef TO mt_plantdef.
      SORT mt_plantdef BY werks.
    ENDIF.
  ENDIF.

ENDMETHOD.


method GET_PO_DETAIL.

  DATA:
    ls_return          TYPE bapiret2.

  DATA:
    ld_success         TYPE abap_bool.

  DATA:
    ld_msgv1         TYPE sy-msgv1.


  REFRESH:
    et_items,
    et_return.



CALL FUNCTION 'BAPI_PO_GETDETAIL'
  EXPORTING
    PURCHASEORDER                    = ID_EBELN
    ITEMS                            = 'X'
*   ACCOUNT_ASSIGNMENT               = ' '
    SCHEDULES                        = 'X'
*   HISTORY                          = ' '
*   ITEM_TEXTS                       = ' '
*   HEADER_TEXTS                     = ' '
*   SERVICES                         = ' '
*   CONFIRMATIONS                    = ' '
*   SERVICE_TEXTS                    = ' '
*   EXTENSIONS                       = ' '
* IMPORTING
*   PO_HEADER                        =
*   PO_ADDRESS                       =
  TABLES
*   PO_HEADER_TEXTS                  =
    PO_ITEMS                         = et_items
*   PO_ITEM_ACCOUNT_ASSIGNMENT       =
    PO_ITEM_SCHEDULES                = et_schedules
*   PO_ITEM_CONFIRMATIONS            =
*   PO_ITEM_TEXTS                    =
*   PO_ITEM_HISTORY                  =
*   PO_ITEM_HISTORY_TOTALS           =
*   PO_ITEM_LIMITS                   =
*   PO_ITEM_CONTRACT_LIMITS          =
*   PO_ITEM_SERVICES                 =
*   PO_ITEM_SRV_ACCASS_VALUES        =
    RETURN                           = et_return
*   PO_SERVICES_TEXTS                =
*   EXTENSIONOUT                     =
          .


* Update success?
  IF ( find_error( et_return ) = abap_false ).
    IF ( NOT ls_return-type CA mc_error_types ).
*     Success
      ld_success = abap_true.
    ELSE.
*     Failure
      APPEND ls_return TO et_return.
    ENDIF.
  ENDIF.

* Update failure
  IF ( ld_success = abap_false ).
*   Raise exception
    IF ( 1 = 2 ). MESSAGE e061(zz_flcm). ENDIF.
*   ld_msgv1 = <item>-plant.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '061'
        sub_stus = mc_sub_status_none
        msgv1    = ld_msgv1
        msgtab   = et_return.
  ENDIF.


endmethod.


METHOD get_storage_location.

  DATA:
    ls_plntdef     LIKE LINE OF mt_plantdef.

  DATA:
    ld_msgv1       TYPE sy-msgv1.

  ls_plntdef = get_plant_definition( id_werks    = id_werks
                                     id_sub_stus = id_sub_stus ).

  IF ( ls_plntdef-lgort IS NOT INITIAL ).
    rd_lgort = ls_plntdef-lgort.
  ELSE.
    ld_msgv1 = id_werks.
    IF ( 1 = 2 ). MESSAGE e033(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid = mc_msgid_flcm
        msgty = mc_msgty_error
        msgno = '033'
        msgv1 = ld_msgv1
        sub_stus = id_sub_stus.
  ENDIF.

ENDMETHOD.


METHOD GET_STORAGE_LOCATION_RECEIVING.

  DATA:
    ls_plntdef     LIKE LINE OF mt_plantdef.

  DATA:
    ld_msgv1       TYPE sy-msgv1.

  ls_plntdef = get_plant_definition( id_werks ).

  IF ( ls_plntdef-lgort IS NOT INITIAL ).
    rd_lgort = ls_plntdef-lgort.
  ELSE.
    ld_msgv1 = id_werks.
    IF ( 1 = 2 ). MESSAGE e037(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid = mc_msgid_flcm
        msgty = mc_msgty_error
        msgno = '037'
        msgv1 = ld_msgv1.
  ENDIF.

ENDMETHOD.


  METHOD is_for_validation.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by             Date                Tracking number *
* -------------------------        ----------          --------------- *
* Von Kaisser Almeria              2016-07-28          DV5K9A03CO      *
*                                                                      *
* Short Description : CR269546 TK334189                                *
*                     Add controls to avoid creating POs for           *
*                     unreasonable amounts                             *
*----------------------------------------------------------------------*
    STATICS:
      l_check      TYPE c.

    DATA:
      l_zval      TYPE zmm_flcm_parms-zval_to.

    IF l_check IS INITIAL.
      SELECT SINGLE zval_from
        FROM zmm_flcm_parms
        INTO l_zval
       WHERE progname EQ mc_progname_819
         AND zparm_nm EQ mc_cond_check.
      IF sy-subrc IS INITIAL
        AND l_zval EQ abap_true.
        l_check = abap_true.
      ENDIF.
    ENDIF.

    rd_result = l_check.
  ENDMETHOD.


method IS_OPEN_PERIOD.

  DATA:
    ls_zmm_flcm_plntdef  TYPE zmm_flcm_plntdef,
    ls_zmm_flcm_tds      TYPE zmm_flcm_tds,
    ls_werks_yr_mth      LIKE LINE OF mt_werks_yr_mth,
    lt_zmm_flcm_tds      TYPE standard table of zmm_flcm_tds.

  DATA:
    ld_open_period_date TYPE num6,
    ld_werks_yr_mth     TYPE num6,
    ld_in_yr_mth        TYPE num6,
    ld_valfrom          TYPE zmm_flcm_xref-zval_from,
    ld_ztds_folio_yr    TYPE zmm_flcm_tds-ztds_folio_yr,
    ld_ztds_folio_mth   TYPE zmm_flcm_tds-ztds_folio_mth,
    ld_msgv1            TYPE sy-msgv1,
    ld_msgv2            TYPE sy-msgv2.

  FIELD-SYMBOLS:
    <werks_yr_mth>        LIKE LINE OF mt_werks_yr_mth,
    <zmm_flcm_tds>        TYPE zmm_flcm_tds.

  rd_result = abap_false.
  clear ld_werks_yr_mth.

  if ( m_override_open_period_flag eq abap_true ).
      m_override_open_period_flag = abap_false.
      rd_result = abap_true.
      exit.
  endif.

  concatenate id_ztds_folio_yr id_ztds_folio_mth into ld_in_yr_mth.
* Get value from buffer
  read table mt_werks_yr_mth ASSIGNING <werks_yr_mth>
      WITH TABLE KEY werks = id_werks.                                                   "DV5K970912
*    with key  werks             = id_werks.                                             "DV5K970912
*              folio_yr          = id_ztds_folio_yr                                      "DV5K970912
*              folio_mth         = id_ztds_folio_mth BINARY SEARCH.                      "DV5K970912

  IF ( sy-subrc = 0 ).
*   Found
    ld_werks_yr_mth = <werks_yr_mth>-folio_yr_mth.                                       "DV5K970912
*    concatenate <werks_yr_mth>-folio_yr <werks_yr_mth>-folio_mth into ld_werks_yr_mth.  "DV5K970912
  ELSE.
*   Get value from database table
    SELECT  *
             INTO TABLE lt_zmm_flcm_tds
             FROM zmm_flcm_tds
            WHERE ( werks           = id_werks            )
              AND ( ztds_tran_type  = mc_710              )
              AND ( ztran_stus      = mc_status_processed )
              AND ( zeom_ind        = abap_true           ).
    IF ( sy-subrc ne 0 ).
       select *
          from zmm_flcm_tds
          into table lt_zmm_flcm_tds
         where werks = id_werks
         and  ( ztran_stus = mc_status_processed
         or     ztran_stus = mc_status_error ).
    ENDIF.
*     we need to verify which period is the valid one
*     Return value and put it in buffer
    sort lt_zmm_flcm_tds by ztds_folio_yr descending
                            ztds_folio_mth descending
                            ztds_folio_nbr descending
                            ztds_folio_seq descending.
    loop at lt_zmm_flcm_tds assigning <zmm_flcm_tds>
      where ztds_tran_ref_nb NE id_tran_ref_nb.
      if ( <zmm_flcm_tds>-ztran_stus = mc_status_processed and
           <zmm_flcm_tds>-zeom_ind = abap_true ).
         if ( <zmm_flcm_tds>-ztds_folio_mth lt 12 ).
            ld_ztds_folio_mth = <zmm_flcm_tds>-ztds_folio_mth + 1.
            ld_ztds_folio_yr  = <zmm_flcm_tds>-ztds_folio_yr.
         else.
            ld_ztds_folio_mth = 1.
            ld_ztds_folio_yr  = <zmm_flcm_tds>-ztds_folio_yr + 1.
         endif.
         concatenate ld_ztds_folio_yr ld_ztds_folio_mth into ld_werks_yr_mth.
      else.
         concatenate <zmm_flcm_tds>-ztds_folio_yr <zmm_flcm_tds>-ztds_folio_mth into ld_werks_yr_mth.
      endif.
      exit.
    endloop.

    IF ( ld_werks_yr_mth is initial ).
*     Not found then we must find the date from zmm_flcm_plntdef this would be the default and would only occcur at
*     the beginning of the project go live once we have started processign this should not occur
       SELECT  SINGLE *
              INTO ls_zmm_flcm_plntdef
              FROM zmm_flcm_plntdef
             WHERE ( werks          = id_werks          ).
       IF ( sy-subrc EQ 0 ).
          ld_werks_yr_mth = ls_zmm_flcm_plntdef-zact_dt+0(6).
       ELSE.
         ld_msgv1 = id_werks.
         IF ( 1 = 2 ). MESSAGE e054(zz_flcm). ENDIF.
           RAISE EXCEPTION TYPE zcx_flcm_error
             EXPORTING
               msgid    = mc_msgid_flcm
               msgty    = mc_msgty_error
               msgno    = '054'
               msgv1    = ld_msgv1
               sub_stus = mc_sub_status_none.
       ENDIF.
    ENDIF.
  ENDIF.

* see if we have found a year month for the plant and then compare and also add to mt_werks to improve on performance
  check ld_werks_yr_mth is not initial.

  if ( ld_in_yr_mth le ld_werks_yr_mth ).
     rd_result = abap_true.
  endif.

*  READ TABLE mt_werks_yr_mth ASSIGNING <werks_yr_mth>                         "DV5K970912
*        WITH KEY werks          = id_werks BINARY SEARCH.                     "DV5K970912

*  IF ( sy-subrc ne 0 ).                                                       "DV5K970912
  IF <werks_yr_mth> IS NOT ASSIGNED.     "Buffer value not previously found    "DV5K970912
*   Not found
*     Return value and put it in buffer
      move id_werks                        to ls_werks_yr_mth-werks.
      MOVE ld_werks_yr_mth to ls_werks_yr_mth-folio_yr_mth.
      INSERT ls_werks_yr_mth INTO TABLE mt_werks_yr_mth.   "Sorted table       "DV5K970912
*      move id_ztds_folio_yr                to ls_werks_yr_mth-folio_yr.       "DV5K970912
*      move id_ztds_folio_mth               to ls_werks_yr_mth-folio_mth.      "DV5K970912
*      APPEND ls_werks_yr_mth     to mt_werks_yr_mth.                          "DV5K970912
*      SORT mt_werks_yr_mth by werks.                                          "DV5K970912
  ENDIF.

endmethod.


METHOD m_validate_plant.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by             Date                Tracking number *
* -------------------------        ----------          --------------- *
* Von Kaisser Almeria              2017-04-27           DV5K9A07RX     *
*                                                       DV5K9A07ZV     *
* Short Description : CR338898 TK357207                                *
*                     Add validation for plant to plant transfer       *
*                                                                      *
*----------------------------------------------------------------------*
  IF is_trans-werks = is_trans-ztds_bol_plant.
    IF ( 1 = 2 ). MESSAGE e109(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '109'
        sub_stus = id_sub_stus.
  ENDIF.
ENDMETHOD.


method reverse_po.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by           Date         Tracking number   SIR    *
* --------------------        ----------      ----------------------   *
* Nadine Crossgill             2013/12/20      DV5K983777
*
* Short Description: CR 227248 TASK 254180 SM-ENH-820
* Only for 502 TD Trans should the transaction date should be moved to
* Delievery date. Otherwise if the BOL date exist use BOL date otherwise
* transaction date.
*----------------------------------------------------------------------*
* Eva Chan                     2013/01/25      DV5K976670              *
*                                                                      *
* Short Description : CR215058-229919 SM-ENH-820                       *
* Fix the issue of 2012 Fall SPs defect, it's doesn't work to leave the*
* internal schedule line tables blank for reversing PO.                *
*----------------------------------------------------------------------*
* Eva Chan                     2012/11/20      DV5K975362              *
*                                              DV5K975505              *
* Short Description : SM-ENH-820                                       *
* As of 2012 Fall SPs, SAP changed the way they handle Schedule        *
* Lines internally, so leave the internal schedule line tables blank   *
* They are enforcing we use 0001, 0002, ...                            *
*----------------------------------------------------------------------*

    data:

      ld_output_qty  type ekpo-menge,
      ld_unit_tds    type zmm_flcm_tds-meins,
      ld_input_qty   type ekpo-menge,
      ld_msgv1       type sy-msgv1,
      ld_msgv2       type sy-msgv2.

  data:

    ls_items_hold         type bapiekpo,
    lt_items              type bapiekpo_tp,
    ls_items              type bapiekpo,
    lt_schedules          type bapieket_tp,
    ls_schedules          type bapieket,
    lt_items_chg          type bapimepoitem_tp,
    ls_items_chg          type bapimepoitem,
    lt_schedules_chg      type bapimeposchedule_tp,
    ls_schedules_chg      type bapimeposchedule,
    lt_itemsx             type bapimepoitemx_tp,
    ls_itemsx             type bapimepoitemx,
    ls_schedulex          type bapimeposchedulx,
    lt_schedulex          type bapimeposchedulx_tp.


  data:
    lv_lines       type i,
    lv_index       type sy-index,
    lv_poitem_original type ebelp.                          "DV5K975505

* Call po_getdetail  bapi
  get_po_detail( exporting id_ebeln       = is_unique_po-ebeln
                 importing et_items       = lt_items
                           et_schedules   = lt_schedules
                           et_return      = rt_return ).


  if ( is_trans-ztds_tran_type eq mc_502 ).

    if ( is_contract-bednr = 'GROSS' ).
      ld_input_qty = is_trans-ztds_grs_vol.
    else.
      ld_input_qty = is_trans-ztds_net_vol.
    endif.


    if ( is_trans-meins ne is_contract-meins ).

*          ld_input_qty = is_trans-ztds_grs_vol.

      call function 'MD_CONVERT_MATERIAL_UNIT'
        exporting
          i_matnr              = is_trans-matnr
          i_in_me              = is_trans-meins
          i_out_me             = is_contract-meins
          i_menge              = ld_input_qty
        importing
          e_menge              = ld_output_qty
        exceptions
          error_in_application = 1
          error                = 2
          others               = 3.
      if sy-subrc <> 0.
*   Raise exception
        if ( 1 = 2 ). message e046(zz_flcm). endif.
        raise exception type zcx_flcm_error
          exporting
            msgid    = mc_msgid_flcm
            msgty    = mc_msgty_error
            msgno    = '046'
            sub_stus = id_sub_stus.
*              msgtab   = et_return.
      endif.
    else.
      ld_output_qty = ld_input_qty.
    endif.
  else.

    ld_input_qty = is_trans-ztds_paid_vol.


    if ( is_trans-meins ne is_contract-meins ).

      call function 'UNIT_CONVERSION_SIMPLE'
        exporting
          input                      = ld_input_qty
*           NO_TYPE_CHECK              = 'X'
*           ROUND_SIGN                 = ' '
          unit_in                    = is_trans-meins
          unit_out                   = is_contract-meins
        importing
*           ADD_CONST                  =
*           DECIMALS                   =
*           DENOMINATOR                =
*           NUMERATOR                  =
       output                        = ld_output_qty
        exceptions
          conversion_not_found       = 1
          division_by_zero           = 2
          input_invalid              = 3
          output_invalid             = 4
          overflow                   = 5
          type_invalid               = 6
          units_missing              = 7
          unit_in_not_found          = 8
          unit_out_not_found         = 9
          others                     = 10
                .

      if sy-subrc <> 0.
*  Raise exception
        if ( 1 = 2 ). message e046(zz_flcm). endif.
        raise exception type zcx_flcm_error
          exporting
            msgid    = mc_msgid_flcm
            msgty    = mc_msgty_error
            msgno    = '046'
            sub_stus = id_sub_stus.
*              msgtab   = rt_return.
      endif.
    else.
      ld_output_qty = ld_input_qty.
    endif.
  endif.

  clear ls_items.
  loop at lt_items into ls_items
    where po_item  = is_unique_po-ebelp.
    ls_items_hold = ls_items.
  endloop.


*   Raise exception
  if ls_items is initial.
    ld_msgv1 = is_trans-matnr.
    ld_msgv2 = is_trans-werks.
    if ( 1 = 2 ). message e072(zz_flcm). endif.
    raise exception type zcx_flcm_error
      exporting
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '072'
        sub_stus = id_sub_stus
        msgv1    = ld_msgv1
        msgv2    = ld_msgv2.
  endif.

  check ls_items is not initial.
* now make sure what the last record is may not be the cancel there may have been records added manually by user
  describe table lt_items lines lv_lines.
  lv_index = lv_lines.
  clear ls_items.
  read table lt_items into ls_items
    index lv_index.

  if sy-subrc eq 0.
* Begin of DV5K975505
    clear lv_poitem_original.
    lv_poitem_original = ls_items-po_item.
* End of DV5K975505
    ls_items-po_item = ls_items-po_item + 10.
  endif.


  refresh lt_items_chg.
  clear ls_items_chg.
  move-corresponding ls_items_hold to ls_items_chg.
* line item
  ls_items_chg-po_item        = ls_items-po_item.
  ls_items_chg-stge_loc       = get_storage_location( is_contract-werks ).
  ls_items_chg-trackingno     = is_contract-bednr.
  ls_items_chg-quantity       = ld_output_qty.
  ls_items_chg-po_unit        = is_contract-meins.
  ls_items_chg-agreement      = is_contract-ebeln.
  ls_items_chg-agmt_item      = is_contract-ebelp.
  ls_items_chg-ret_item       = abap_false.
  ls_items_chg-conf_ctrl      = id_conf_ctrl.
  ls_items_chg-no_rounding    = abap_true.
* Begin of DV5K976670
** Begin of DV5K975505
*  ls_items_chg-ref_doc   = ls_items-po_number.
*  ls_items_chg-ref_item  = lv_poitem_original.  "Reference to original item
** End of DV5K975505
* End of DV5K976670
  append ls_items_chg to lt_items_chg.

*
  clear ls_itemsx.
  ls_itemsx-po_item            = ls_items-po_item.
  ls_itemsx-stge_loc           = abap_true.
  ls_itemsx-trackingno         = abap_true.
  ls_itemsx-quantity           = abap_true.
  ls_itemsx-po_unit            = abap_true.
  ls_itemsx-agreement          = abap_true.
  ls_itemsx-agmt_item          = abap_true.
  ls_itemsx-ret_item           = abap_true.
  ls_itemsx-conf_ctrl          = abap_true.
  ls_itemsx-no_rounding        = abap_true.
* Begin of DV5K976670
** Begin of DV5K975505
*  ls_itemsx-ref_doc            = abap_true.
*  ls_itemsx-ref_item           = abap_true.
** End of DV5K975505
* End of DV5K976670
  append ls_itemsx to lt_itemsx.

* Begin of DV5K976670
*  LOOP AT lt_schedules INTO ls_schedules
*    WHERE po_item = is_contract-ebelp.
*    EXIT.
*  ENDLOOP.
  clear ls_schedules.
  read table lt_schedules  into ls_schedules
    with key po_item =  lv_poitem_original.
  if sy-subrc = 0.
* End of DV5K976670

    refresh: lt_schedules_chg,
             lt_schedulex.                                  "DV5K975505
    clear: ls_schedules_chg,
           ls_schedulex.                                    "DV5K975505

    move-corresponding ls_schedules to ls_schedules_chg.
    ls_schedules_chg-po_item         = ls_items-po_item.
*    ls_schedules_chg-sched_line      = ls_schedules-serial_no.
    ls_schedules_chg-quantity        = ld_output_qty.

    if ( is_trans-ztds_tran_type eq mc_502 ) or ( is_trans-ztds_bol_dt is initial ).  "I- DV5K983777
*  ls_schedules_chg-delivery_date   = is_trans-ztds_bol_dt.  "D-DV5K973716
      ls_schedules_chg-delivery_date   = is_trans-ztds_tran_dt.  "I-DV5K973716
    else.                                                                             "I-DV5K983777
      ls_schedules_chg-delivery_date   = is_trans-ztds_bol_dt.                        "I-DV5K983777
    endif.                                                                            "I-DV5K983777
    append ls_schedules_chg to lt_schedules_chg.

    ls_schedulex-po_item       = ls_items-po_item.
*    ls_schedulex-sched_line    = ls_schedules_chg-sched_line.
    ls_schedulex-delivery_date = abap_true.
    ls_schedulex-quantity      = abap_true.
    append ls_schedulex to lt_schedulex.

  endif.
* End of DV5K976670

* Call change PO bapi
  change_po( exporting id_ebeln        = is_unique_po-ebeln
                       id_sub_stus     = id_sub_stus
             importing et_return       = rt_return
             changing  ct_items        = lt_items_chg
                       ct_itemsx       = lt_itemsx
                       ct_schedules    = lt_schedules_chg
                       ct_schedulesx   = lt_schedulex ).


endmethod.


method SET_OVERRIDE_OPEN_PERIOD.

   m_override_open_period_flag = abap_true.

endmethod.


METHOD update_fuel_po.


  DATA:
    lt_bdcdata      TYPE STANDARD TABLE OF bdcdata,
    lt_bdc_message  TYPE STANDARD TABLE OF bdcmsgcoll,
    ls_bdcdata      TYPE bdcdata,
    ls_bdc_message  TYPE bdcmsgcoll,
    ls_bapiret2     TYPE bapiret2,
    ld_output_qty   TYPE ekes-menge,
    ld_output_qtyc(17)  TYPE c,
    ld_input_qty    TYPE p DECIMALS 2,
    ld_unit_tds     TYPE zmm_flcm_tds-meins,
    ld_bdc_mode.

  DATA:
    ld_success      TYPE abap_bool.

  REFRESH:
    lt_bdcdata,
    lt_bdc_message.


  IF id_bednr = 'GROSS'.
     ld_input_qty = is_trans-ztds_grs_vol.
  ELSE.
     ld_input_qty = is_trans-ztds_net_vol.
  ENDIF.

  IF is_trans-meins NE id_meins.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                      = ld_input_qty
*           NO_TYPE_CHECK              = 'X'
*           ROUND_SIGN                 = ' '
        unit_in                    = is_trans-meins
        unit_out                   = id_meins
      IMPORTING
*           ADD_CONST                  =
*           DECIMALS                   =
*           DENOMINATOR                =
*           NUMERATOR                  =
        output                     = ld_output_qty
      EXCEPTIONS
        conversion_not_found       = 1
        division_by_zero           = 2
        input_invalid              = 3
        output_invalid             = 4
        overflow                   = 5
        type_invalid               = 6
        units_missing              = 7
        unit_in_not_found          = 8
        unit_out_not_found         = 9
         OTHERS                     = 10
                 .
    IF sy-subrc <> 0.
*   Raise exception
      IF ( 1 = 2 ). MESSAGE e046(zz_flcm). ENDIF.
      RAISE EXCEPTION TYPE zcx_flcm_error
        EXPORTING
          msgid    = mc_msgid_flcm
          msgty    = mc_msgty_error
          msgno    = '046'
          sub_stus = mc_sub_status_phyi_fail.
*              msgtab   = et_return.
    ENDIF.
  ELSE.
    ld_output_qty = ld_input_qty.
  ENDIF.

  MOVE ld_output_qty TO ld_output_qtyc.

  CLEAR ls_bdcdata.
  ls_bdcdata-program  = 'SAPMM06E'.
  ls_bdcdata-dynpro   = '0105'.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lt_bdcdata.


  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'RM06E-BSTNR'.
  ls_bdcdata-fval = id_ebeln.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'BDC_OKCODE'.
  ls_bdcdata-fval = '/00'.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-program  = 'SAPMM06E'.
  ls_bdcdata-dynpro   = '0120'.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'RM06E-EBELP'.
  ls_bdcdata-fval = id_ebelp.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'BDC_OKCODE'.
  ls_bdcdata-fval = '/00'.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-program  = 'SAPMM06E'.
  ls_bdcdata-dynpro   = '0120'.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'RM06E-TCSELFLAG(01)'.
  ls_bdcdata-fval = abap_true.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'BDC_OKCODE'.
  ls_bdcdata-fval = '=BSTA'.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-program  = 'SAPLEINB'.
  ls_bdcdata-dynpro   = '0200'.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lt_bdcdata.

  IF ( id_update_ebtyp = abap_true ).

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'EKES-EBTYP(01)'.
    ls_bdcdata-fval = id_ebtyp.
    APPEND ls_bdcdata TO lt_bdcdata.

  ENDIF.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'RM06E-LPEIN(01)'.
  ls_bdcdata-fval = 'D'.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'RM06E-EEIND(01)'.
  ls_bdcdata-fval = is_trans-ztds_tran_dt.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'EKES-UZEIT(01)'.
  ls_bdcdata-fval = is_trans-ztds_tran_tm.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'EKES-MENGE(01)'.
  ls_bdcdata-fval = ld_output_qtyc.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'EKES-XBLNR(01)'.
  ls_bdcdata-fval = is_trans-ztds_tran_ref_nb.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'EKES-ERDAT(01)'.
  ls_bdcdata-fval = sy-datum.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'BDC_OKCODE'.
  ls_bdcdata-fval = '/00'.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-program  = 'SAPLEINB'.
  ls_bdcdata-dynpro   = '0200'.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'BDC_OKCODE'.
  ls_bdcdata-fval = '=BU'.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-program  = 'SAPLSPO1'.
  ls_bdcdata-dynpro   = '0300'.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lt_bdcdata.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = 'BDC_OKCODE'.
  ls_bdcdata-fval = '=YES'.
  APPEND ls_bdcdata TO lt_bdcdata.


  ld_bdc_mode = 'N'.
* Invoke Call Transaction
  CALL TRANSACTION 'ME22' USING lt_bdcdata
                          MODE ld_bdc_mode
                          UPDATE 'S'
                          MESSAGES INTO lt_bdc_message.

  ld_success = abap_true.

  LOOP AT lt_bdc_message INTO ls_bdc_message.

    ls_bapiret2-type       = ls_bdc_message-msgtyp.
    ls_bapiret2-id          = ls_bdc_message-msgid.
    ls_bapiret2-number      = ls_bdc_message-msgnr.
    ls_bapiret2-message_v1  = ls_bdc_message-msgv1.
    ls_bapiret2-message_v2  = ls_bdc_message-msgv2.
    ls_bapiret2-message_v3  = ls_bdc_message-msgv3.
    ls_bapiret2-message_v4  = ls_bdc_message-msgv4.
    APPEND ls_bapiret2 TO rt_return.
    IF ls_bdc_message-msgtyp = 'E' OR
       ls_bdc_message-msgtyp = 'A'.
      ld_success = abap_false.
    ENDIF.
  ENDLOOP.


* Update failure
  IF ( ld_success = abap_false ).
*   Raise exception
    IF ( 1 = 2 ). MESSAGE e067(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '067'
        sub_stus = '14'
        msgtab   = rt_return.
  ENDIF.



ENDMETHOD.


METHOD update_po.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by           Date         Tracking number   SIR    *
* --------------------        ----------      ----------------------   *
* Eva Chan                     2012/11/20      DV5K975362              *
*                                              DV5K975505              *
* Short Description :                                                  *
* As of 2012 Fall SPs, SAP changed the way they handle Schedule        *
* Lines internally, so leave the internal schedule line tables blank   *
* They are enforcing we use 0001, 0002, ...                            *
*----------------------------------------------------------------------*

  DATA:

    ls_items_hold         TYPE bapiekpo,
    lt_items              TYPE bapiekpo_tp,
    ls_items              TYPE bapiekpo,
    lt_schedules          TYPE bapieket_tp,
    ls_schedules          TYPE bapieket,
    lt_items_chg          TYPE bapimepoitem_tp,
    ls_items_chg          TYPE bapimepoitem,
    lt_schedules_chg      TYPE bapimeposchedule_tp,
    ls_schedules_chg      TYPE bapimeposchedule,
    lt_itemsx             TYPE bapimepoitemx_tp,
    ls_itemsx             TYPE bapimepoitemx,
    ls_schedulex          TYPE bapimeposchedulx,
    lt_schedulex          TYPE bapimeposchedulx_tp.


  DATA:

    ld_output_qty  TYPE p DECIMALS 2,
    ld_unit_tds    TYPE zmm_flcm_tds-meins,
    ld_input_qty   TYPE p DECIMALS 2,
    lv_lines       TYPE i,
    lv_index       TYPE sy-index,
    ld_msgv1       TYPE sy-msgv1,
    ld_msgv2       TYPE sy-msgv2,
    lv_poitem_original TYPE ebelp.                          "DV5K975505

* Call po_getdetail  bapi
  get_po_detail( EXPORTING id_ebeln       = id_ebeln
                 IMPORTING et_items       = lt_items
                           et_schedules   = lt_schedules
                           et_return      = rt_return ).

  LOOP AT lt_items INTO ls_items
    WHERE po_item = id_ebelp.
    ls_items_hold = ls_items.
  ENDLOOP.

*   Raise exception
  IF ls_items IS INITIAL.
    ld_msgv1 = is_trans-matnr.
    ld_msgv2 = is_trans-werks.
    IF ( 1 = 2 ). MESSAGE e072(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '072'
        sub_stus = mc_sub_status_none
        msgv1    = ld_msgv1
        msgv2    = ld_msgv2.
  ENDIF.

* now make sure what the last record is may not be the cancel there may have been records added manually by user
  DESCRIBE TABLE lt_items LINES lv_lines.
  lv_index = lv_lines.
  CLEAR ls_items.
  READ TABLE lt_items INTO ls_items
    INDEX lv_index.

  IF sy-subrc EQ 0.
* Begin of DV5K975505
    CLEAR lv_poitem_original.
    lv_poitem_original = ls_items-po_item.
* End of DV5K975505
    ls_items-po_item = ls_items-po_item + 10.
  ENDIF.

  REFRESH lt_items_chg.
  MOVE-CORRESPONDING ls_items_hold TO ls_items_chg.
  ls_items_chg-po_item      = ls_items-po_item.
  ls_items_chg-ret_item     = abap_true.
  ls_items_chg-stge_loc     = ls_items_hold-store_loc.
  ls_items_chg-no_rounding  = abap_true.
* Begin of DV5K975505
  ls_items_chg-ref_doc   = ls_items-po_number.
  ls_items_chg-ref_item  = lv_poitem_original.  "Reference to original item
* End of DV5K975505
  APPEND ls_items_chg TO lt_items_chg.

  CLEAR ls_itemsx.
  ls_itemsx-po_item      = ls_items-po_item.
  ls_itemsx-quantity     = abap_true.
  ls_itemsx-po_unit      = abap_true.
  ls_itemsx-agreement    = abap_true.
  ls_itemsx-agmt_item    = abap_true.
  ls_itemsx-ret_item     = abap_true.
  ls_itemsx-stge_loc     = abap_true.
  ls_itemsx-trackingno   = abap_true.
  ls_itemsx-no_rounding  = abap_true.
* Begin of DV5K975505
  ls_itemsx-ref_doc   = abap_true.
  ls_itemsx-ref_item  = abap_true.
* End of DV5K975505
  APPEND ls_itemsx TO lt_itemsx.

* Begin of DV5K975362
*  loop at lt_schedules into ls_schedules
*    where po_item = id_ebelp.
*    exit.
*  endloop.
*
*  move-corresponding ls_schedules to ls_schedules_chg.
*  ls_schedules_chg-po_item         = ls_items-po_item.
*  ls_schedules_chg-sched_line      = ls_schedules-serial_no.
*  ls_schedules_chg-delivery_date   = ls_schedules-deliv_date.
*  append ls_schedules_chg to lt_schedules_chg.
*
*  ls_schedulex-po_item       = ls_items-po_item.
*  ls_schedulex-sched_line    = ls_schedules_chg-sched_line.
*  ls_schedulex-delivery_date = abap_true.
*  ls_schedulex-quantity      = abap_true.
*  append ls_schedulex to lt_schedulex.

  REFRESH: lt_schedules_chg,  lt_schedulex.
* End of DV5K975362

* Call change PO bapi
  change_po( EXPORTING id_ebeln        = id_ebeln
                       id_sub_stus     = id_sub_stus
             IMPORTING et_return       = rt_return
             CHANGING  ct_items        = lt_items_chg
                       ct_itemsx       = lt_itemsx
                       ct_schedules    = lt_schedules_chg
                       ct_schedulesx   = lt_schedulex ).


ENDMETHOD.


METHOD validate_new_tds_po.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by             Date                Tracking number *
* -------------------------        ----------          --------------- *
* Chuong Le  CR306321-T347799      2017/01/04          DV5K9A05WW      *
*                                                                      *
*    - Save Our Reference in PO header text instead of EKKO-UNSEZ.     *
*----------------------------------------------------------------------*
* Von Kaisser Almeria              2016-07-28          DV5K9A03CO      *
*                                  2016-08-11          DV5K9A03N4      *
*                                  2016-09-13          DV5K9A046R      *
*                                                                      *
* Short Description : CR269546 TK334189                                *
*                     Add controls to avoid creating POs for           *
*                     unreasonable amounts                             *
*                     Change control #1 to validate contract UOM       *
*                     against contract UOM                             *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
  CONSTANTS:
    c_10           TYPE bapimepoitem-po_item VALUE '00010'.

  DATA:
    ld_output_qty  TYPE ekpo-menge,
    ld_unit_tds    TYPE zmm_flcm_tds-meins,
    ld_input_qty   TYPE ekpo-menge,
    ld_ebeln       TYPE ebeln,
    ls_header      TYPE bapimepoheader,
    ls_headerx     TYPE bapimepoheaderx,
    lt_item        TYPE bapimepoitem_tp,
    ls_item        TYPE bapimepoitem,
    lt_poschedule  TYPE bapimeposchedule_tp,
    ls_poschedule  TYPE bapimeposchedule,
    lt_poschedulex TYPE bapimeposchedulx_tp,
    ls_poschedulex TYPE bapimeposchedulx,
    lt_itemx       TYPE bapimepoitemx_tp,
    ls_itemx       TYPE bapimepoitemx,
    lt_potextheader TYPE bapimepotextheader_tp.            "DV5K9A05WW+

  DATA:
    ld_equi       TYPE tvarv_val,
    ld_invalid    TYPE c,
    ld_net_price  TYPE bapicurext,
    ld_zval       TYPE zmm_flcm_parms-zval_to,
    ld_capacity   TYPE brgew_ap,
    ld_quantity   TYPE brgew_ap,
    ld_cuobj      TYPE inob-cuobj,
    ld_atwrt      TYPE ausp-atwrt,
    ld_max_litre  TYPE bapicurext,
    ld_max_gallon TYPE bapicurext,
    ld_msg1       TYPE syst_msgv,
    ld_msg2       TYPE syst_msgv.

  FIELD-SYMBOLS:
    <item>  LIKE LINE OF lt_item,
    <itemx> LIKE LINE OF lt_itemx.

  "Check if validation is needed
  IF ( zcl_flcm_services=>is_for_validation( ) EQ abap_false ).
    RETURN.
  ENDIF.

  "Get Max amount per volume type
  CALL METHOD zcl_flcm_services=>get_max_amount_per_volume
    IMPORTING
      ed_litre  = ld_max_litre
      ed_gallon = ld_max_gallon.

***BEGIN OF DELETE - XT18912 - CR269546 TK334189 - DV5K9A046R
*  "1.  Validate that the currency and the order unit (OUn) in the PO matches
*  IF ( is_contract-waers EQ mc_cad AND is_trans-meins NE mc_litre )
*    OR ( is_contract-waers EQ mc_usd AND is_trans-meins NE mc_gallon ).
*    ld_msg1 = is_contract-waers.
**** Start of Change - DV5K9A03N4 ----------------->>>
**    ld_msg2 = is_trans-meins.
*    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*      EXPORTING
*        input                = is_trans-meins
*        language             = sy-langu
*      IMPORTING
*        output               = ld_msg2
*      EXCEPTIONS
*        unit_not_found       = 1
*        OTHERS               = 2.
*    IF sy-subrc <> 0.
*      ld_msg2 = '<Unknown>'.
*    ENDIF.
**** End   of Change - DV5K9A03N4 -----------------<<<
*    IF ( 1 = 2 ). MESSAGE e107(zz_flcm). ENDIF.
*    RAISE EXCEPTION TYPE zcx_flcm_error
*      EXPORTING
*        msgid    = mc_msgid_flcm
*        msgty    = mc_msgty_error
*        msgno    = '107'
*        sub_stus = '000'
*        msgv1    = ld_msg1
*        msgv2    = ld_msg2
*        msgtab   = et_return.
*  ENDIF.
***END OF DELETE - XT18912 - CR269546 TK334189 - DV5K9A046R
***BEGIN OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A046R
  IF is_trans-meins NE is_contract-meins.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input                = is_contract-meins
        language             = sy-langu
      IMPORTING
        output               = ld_msg1
      EXCEPTIONS
        unit_not_found       = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      ld_msg1 = '<Unknown>'.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input                = is_trans-meins
        language             = sy-langu
      IMPORTING
        output               = ld_msg2
      EXCEPTIONS
        unit_not_found       = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      ld_msg2 = '<Unknown>'.
    ENDIF.

    IF ( 1 = 2 ). MESSAGE e108(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '108'
        sub_stus = '000'
        msgv1    = ld_msg2
        msgv2    = ld_msg1
        msgtab   = et_return.
  ENDIF.
***END OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A046R

* first see if we need to convert the quantity to the quantity of the contract
  REFRESH lt_item.
  REFRESH lt_itemx.
  REFRESH lt_poschedule.
  REFRESH lt_poschedulex.

  IF is_contract-bednr = 'GROSS'.
    ld_input_qty = is_trans-ztds_grs_vol.
  ELSE.
    ld_input_qty = is_trans-ztds_net_vol.
  ENDIF.

  IF is_trans-meins NE is_contract-meins.
    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = is_trans-matnr
        i_in_me              = is_trans-meins
        i_out_me             = is_contract-meins
        i_menge              = ld_input_qty
      IMPORTING
        e_menge              = ld_output_qty
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
*   Raise exception
      IF ( 1 = 2 ). MESSAGE e046(zz_flcm). ENDIF.
      RAISE EXCEPTION TYPE zcx_flcm_error
        EXPORTING
          msgid    = mc_msgid_flcm
          msgty    = mc_msgty_error
          msgno    = '046'
          sub_stus = mc_sub_status_none.
    ENDIF.
  ELSE.
    ld_output_qty = ld_input_qty.
  ENDIF.

* header
  CLEAR ls_header.
  ls_header-doc_type   = 'FB  '.
  ls_header-doc_date   = is_trans-ztds_tran_dt.
  ls_header-ref_1      = is_trans-ztds_bol_nbr.
*  CONCATENATE mc_tds is_trans-ztds_trml_id                "DV5K9A05WW-
*  INTO ls_header-our_ref SEPARATED BY '-'.                "DV5K9A05WW-
  ls_header-vendor     = is_trans-lifnr.
  ls_header-comp_code  = is_contract-bukrs.
  ls_header-purch_org  = is_contract-ekorg.
  ls_header-pur_group  = is_contract-ekgrp.
  ls_header-pmnttrms   = is_contract-zterm.

  CLEAR ls_headerx.
  ls_headerx-doc_type  = abap_true.
  ls_headerx-doc_date  = abap_true.
  ls_headerx-ref_1     = abap_true.
*  ls_headerx-our_ref   = abap_true.                       "DV5K9A05WW-
  ls_headerx-vendor    = abap_true.
  ls_headerx-purch_org = abap_true.
  ls_headerx-pur_group = abap_true.
  ls_headerx-comp_code = abap_true.
  ls_headerx-pmnttrms  = abap_true.

* line item
  CLEAR ls_item.
  ls_item-po_item       = c_10.
  ls_item-stge_loc      = get_storage_location( is_contract-werks ).
  ls_item-trackingno    = is_contract-bednr.
  ls_item-quantity      = ld_output_qty.
  ls_item-po_unit       = is_contract-meins.
  ls_item-agreement     = is_contract-ebeln.
  ls_item-agmt_item     = is_contract-ebelp.
  ls_item-conf_ctrl     = id_conf_ctrl.
  ls_item-no_rounding   = abap_true.
  APPEND ls_item TO lt_item.

  CLEAR ls_itemx.
  ls_itemx-po_item      = c_10.
  ls_itemx-stge_loc     = abap_true.
  ls_itemx-trackingno   = abap_true.
  ls_itemx-quantity     = abap_true.
  ls_itemx-po_unit      = abap_true.
  ls_itemx-agreement    = abap_true.
  ls_itemx-agmt_item    = abap_true.
  ls_itemx-conf_ctrl    = abap_true.
  ls_itemx-no_rounding  = abap_true.
  APPEND ls_itemx TO lt_itemx.

  CLEAR ls_poschedule.
  ls_poschedule-po_item        = c_10.
  ls_poschedule-sched_line     = 1.
  ls_poschedule-delivery_date  = is_trans-ztds_tran_dt.
  ls_poschedule-quantity       = ld_output_qty.
  APPEND ls_poschedule TO lt_poschedule.

  CLEAR ls_poschedulex.
  ls_poschedulex-po_item       = c_10.
  ls_poschedulex-sched_line    = 1.
  ls_poschedulex-delivery_date = abap_true.
  ls_poschedulex-quantity      = abap_true.
  APPEND ls_poschedulex TO lt_poschedulex.

*--> Start of Insert DV5K9A05WW - Chuong Le - 2017/01/04
  APPEND INITIAL LINE TO lt_potextheader ASSIGNING FIELD-SYMBOL(<text>).
  <text>-text_id   = mc_f99.
  <text>-text_form = '* '.
  <text>-text_line = |{ mc_tds }-{ is_trans-ztds_trml_id }|.
*<-- End of Insert DV5K9A05WW - Chuong Le - 2017/01/04

*** Call create PO bapi
  create_po( EXPORTING is_header       = ls_header
                       is_headerx      = ls_headerx
                       id_test         = abap_true
                       id_sub_stus     = '000'
             IMPORTING et_return       = et_return
                       ed_ebeln        = ed_ebeln
             CHANGING  ct_items        = lt_item
                       ct_itemsx       = lt_itemx
                       ct_poschedule   = lt_poschedule
                       ct_poschedulex  = lt_poschedulex
                       ct_potextheader = lt_potextheader ). "DV5K9A05WW+

  READ TABLE lt_item INTO ls_item
    INDEX 1.
  IF sy-subrc IS INITIAL.
    IF ls_item-price_unit IS NOT INITIAL.
      ld_net_price = ls_item-net_price / ls_item-price_unit.
    ELSE.
      CLEAR ld_net_price.
    ENDIF.

    CASE is_trans-meins.
      WHEN mc_litre.
        IF ld_net_price GT ld_max_litre.
          ld_invalid = abap_true.
        ENDIF.
      WHEN mc_gallon.
        IF ld_net_price GT ld_max_gallon.
          ld_invalid = abap_true.
        ENDIF.
    ENDCASE.
  ENDIF.

  IF ld_invalid EQ abap_true.
*** Start of Change - DV5K9A03N4 ----------------->>>
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input                = is_trans-meins
        language             = sy-langu
      IMPORTING
        output               = ld_msg1
      EXCEPTIONS
        unit_not_found       = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      ld_msg2 = '<Unknown>'.
    ENDIF.
*** End   of Change - DV5K9A03N4 -----------------<<<
    IF ( 1 = 2 ). MESSAGE i106(zz_flcm). ENDIF. "for where-used only
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '106'
        sub_stus = '000'
        msgv1    = ld_msg1                       "I-DV5K9A03N4
        msgtab   = et_return.
  ENDIF.
ENDMETHOD.
ENDCLASS.
