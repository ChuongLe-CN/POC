class ZCL_FLCM_TDS_GR_CONFIRM definition
  public
  final
  create public .

*"* public components of class ZCL_FLCM_TDS_GR_CONFIRM
*"* do not include other source files here!!!
public section.

  constants MC_TRX_DIVERSION_RECEIPT type ZMM_FLCM_TDS-ZTDS_TRAN_TYPE value '505'. "#EC NOTEXT
  constants MC_TRX_VENDOR_BOL type ZMM_FLCM_TDS-ZTDS_TRAN_TYPE value '700'. "#EC NOTEXT
  constants MC_TRX_INYARD_TRANSFER type ZMM_FLCM_TDS-ZTDS_TRAN_TYPE value '701'. "#EC NOTEXT
  constants MC_TRX_SITE2SITE_TRANSFER type ZMM_FLCM_TDS-ZTDS_TRAN_TYPE value '702'. "#EC NOTEXT

  methods PROCESS_TRANSACTIONS
    importing
      !IT_TRANS type ZMM_FLCM_TDS_T
    returning
      value(RT_RETURN) type BAPIRET2TAB
    raising
      ZCX_FLCM_ERROR .
protected section.
*"* protected components of class ZCL_FLCM_TDS_GR_CONFIRM
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_FLCM_TDS_GR_CONFIRM
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_FLCM_TDS_GR_CONFIRM IMPLEMENTATION.


METHOD process_transactions.

  DATA:
    lo_flcm_exception   TYPE REF TO zcx_flcm_error.

  DATA:
    lt_msg              TYPE bapiret2_tab.

  DATA:
    lo_diversion_receipt   TYPE REF TO zcl_flcm_trx_diversion_receipt,
    lo_vendor_bol          TYPE REF TO zcl_flcm_trx_vendor_bol,
    lo_recon_for_in_yard   TYPE REF TO zcl_flcm_trx_recon_for_in_yard,
    lo_recon_for_site      TYPE REF TO zcl_flcm_trx_recon_for_site.

  DATA:
    lt_parm               TYPE zmm_flcm_tds_t,
    lt_trans              TYPE zmm_flcm_tds_t,
    lt_trx_result         TYPE bapiret2tab,
    lt_result             TYPE bapiret2tab.

  DATA:
    ls_next               LIKE LINE OF lt_trans.


  DATA:
    ld_index              TYPE i.

  FIELD-SYMBOLS:
    <trans>               LIKE LINE OF lt_trans.

* Move records to local copy
  lt_trans[] = it_trans[].

* Sort transaction by type, older records first
  SORT lt_trans BY ztds_tran_type ztds_tran_ref_nb.

* Go through each transaction and process
  LOOP AT lt_trans ASSIGNING <trans>.
*   Get next record data
*   Verify if it is in the open period
*  ======
    TRY.
*  ======
      ld_index = sy-tabix + 1.
      CLEAR ls_next.
      READ TABLE lt_trans INTO ls_next INDEX ld_index.


*   Append transaction to parameter table
      APPEND <trans> TO lt_parm.
*   End of transactions for particular type?  then process
      IF ( <trans>-ztds_tran_type <> ls_next-ztds_tran_type ).
        REFRESH lt_result.

        CASE <trans>-ztds_tran_type.
*       Transaction ID 505 # Diversion Receipt
          WHEN mc_trx_diversion_receipt.
            CREATE OBJECT lo_diversion_receipt
              EXPORTING
                it_trans = lt_parm.

            lt_result = lo_diversion_receipt->execute( ).

            FREE lo_diversion_receipt.

*       Transaction ID 700 # Reconciliation for Vendor BOLs
          WHEN mc_trx_vendor_bol.

             CREATE OBJECT lo_vendor_bol
               EXPORTING
                 it_trans = lt_parm.

             lt_result = lo_vendor_bol->execute( ).

             FREE lo_vendor_bol.
*
*       Transaction ID 701 # Reconciliation for in-Yard transfers
          WHEN mc_trx_inyard_transfer.

             CREATE OBJECT lo_recon_for_in_yard
               EXPORTING
                 it_trans = lt_parm.

             lt_result = lo_recon_for_in_yard->execute( ).

*       Transaction ID 702 # Reconciliation for Site to Site transfers
          WHEN mc_trx_site2site_transfer.

             CREATE OBJECT lo_recon_for_site
               EXPORTING
                 it_trans = lt_parm.

             lt_result = lo_recon_for_site->execute( ).


        ENDCASE.

*     Keep processing results
        APPEND LINES OF lt_result TO lt_trx_result.

*     Reset parameter
        REFRESH lt_parm.
      ENDIF.

*  ======
    CATCH zcx_flcm_error INTO lo_flcm_exception.
*  ======

*       Status update failed, return the error message
        lt_msg = lo_flcm_exception->get_bapimsg_table( ).
        APPEND LINES OF lt_msg TO rt_return.

*  ======
    ENDTRY.
*  ======

  ENDLOOP.

* Return messages
  rt_return[] = lt_trx_result[].

* Release allocated memory
  FREE:
    lt_trans,
    lt_trx_result.

ENDMETHOD.
ENDCLASS.
