class ZCL_FLCM_TRX definition
  public
  create public .

*"* public components of class ZCL_FLCM_TRX
*"* do not include other source files here!!!
public section.

  constants MC_IND_BLANK type ZMM_FLCM_TDS-ZTDS_CANC_RBIL value ' '. "#EC NOTEXT
  constants MC_IND_CANCEL type ZMM_FLCM_TDS-ZTDS_CANC_RBIL value 'C'. "#EC NOTEXT
  constants MC_IND_REBILL type ZMM_FLCM_TDS-ZTDS_CANC_RBIL value 'R'. "#EC NOTEXT

  methods CONSTRUCTOR
    importing
      !IT_TRANS type ZMM_FLCM_TDS_T .
  methods EXECUTE
    returning
      value(RT_RETURN) type BAPIRET2TAB .
  type-pools ABAP .
  methods IS_ERROR
    returning
      value(RD_RESULT) type ABAP_BOOL .
protected section.
*"* protected components of class ZCL_FLCM_TRX
*"* do not include other source files here!!!

  data MT_RESULT type BAPIRET2TAB .
  data MT_TRANS type ZMM_FLCM_TDS_T .
  data MT_TRANS_PROCESSED type ZMM_FLCM_TDS_T .
  data MT_SAP_INV type MARD_TT .
private section.
*"* private components of class ZCL_FLCM_TRX
*"* do not include other source files here!!!

  class-methods FORMAT_VALUE
    importing
      !ID_ANYVALUE type DATA
    exporting
      !ED_OUTPUT type CSEQUENCE .
ENDCLASS.



CLASS ZCL_FLCM_TRX IMPLEMENTATION.


METHOD constructor.

  REFRESH mt_result.
  mt_trans = it_trans.

ENDMETHOD.


METHOD execute.

  REFRESH mt_result.

ENDMETHOD.


METHOD format_value.

* Local constants
  CONSTANTS:
    lc_character          TYPE c        VALUE 'C'.

* Local variables
  DATA:
    lv_type               TYPE c.

  CLEAR ed_output.

  CHECK ( id_anyvalue IS NOT INITIAL ).

* Get field type
  DESCRIBE FIELD id_anyvalue TYPE lv_type.

  IF ( lv_type = lc_character ).

*   Character field

    WRITE id_anyvalue TO ed_output.
    SHIFT ed_output LEFT DELETING LEADING space.

  ELSE.

*   Other type

    WRITE id_anyvalue TO ed_output.

    IF ( ed_output IS INITIAL ).
      WRITE id_anyvalue TO ed_output USING NO EDIT MASK.
    ENDIF.

    SHIFT ed_output LEFT DELETING LEADING space.
    SHIFT ed_output LEFT DELETING LEADING '0'.

  ENDIF.

ENDMETHOD.


METHOD is_error.

  rd_result = zcl_flcm_services=>find_error( mt_result ).

ENDMETHOD.
ENDCLASS.
