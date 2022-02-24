class ZCL_FLCM_TRX_RECON_FOR_IN_YARD definition
  public
  inheriting from ZCL_FLCM_TRX
  final
  create public .

*"* public components of class ZCL_FLCM_TRX_RECON_FOR_IN_YARD
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR
    importing
      !IT_TRANS type ZMM_FLCM_TDS_T .

  methods EXECUTE
    redefinition .
protected section.
*"* protected components of class ZCL_FLCM_TRX_DIVERSION_RECEIPT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_FLCM_TRX_DIVERSION_RECEIPT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_FLCM_TRX_RECON_FOR_IN_YARD IMPLEMENTATION.


METHOD CONSTRUCTOR.

* Call superclass constructor
  super->constructor( it_trans ).

* Remove irrelevant transactions
  DELETE mt_trans WHERE ( ztds_tran_type <> zcl_flcm_tds_gr_confirm=>mc_trx_inyard_transfer ).

* Make sure transactions are in order of creation
  SORT mt_trans BY ztds_tran_ref_nb.

ENDMETHOD.


METHOD EXECUTE.
*---------------------------------------------------------------------------
* -Diversion Receipt-
* The 701 transaction represents a reconciliation for In Yard transfers
*---------------------------------------------------------------------------

  DATA:
    lo_flcm_exception   TYPE REF TO zcx_flcm_error.

  DATA:
    lt_msg              TYPE bapiret2_tab.

  DATA:
    ld_move_type        TYPE bapi2017_gm_item_create-move_type.

  FIELD-SYMBOLS:
    <trans>             LIKE LINE OF mt_trans.

  REFRESH:
    rt_return.

* Call super class
  super->execute( ).

* Error in super call? exit now!
  IF ( is_error( ) = abap_true ).
    RETURN.
  ENDIF.

* Go through transactions
  LOOP AT mt_trans ASSIGNING <trans>.

*   Only process transaction ready for process or processed in error
    CHECK ( <trans>-ztran_stus = zcl_flcm_services=>mc_status_ready )
       OR ( <trans>-ztran_stus = zcl_flcm_services=>mc_status_error ).

*  ======
    TRY.
*  ======

      if ( zcl_flcm_services=>is_open_period(
                               id_tran_ref_nb    = <trans>-ztds_tran_ref_nb
                               id_werks          = <trans>-werks
                               id_ztds_folio_yr  = <trans>-ztds_folio_yr
                               id_ztds_folio_mth = <trans>-ztds_folio_mth ) eq abap_false ).
         continue.
      endif.

*     Create goods movement
      rt_return = zcl_flcm_services=>create_goods_mvmt_gain_loss(
                      is_trans     = <trans> ).

*     Set transaction status as processed successfully
      zcl_flcm_services=>db_update_trans_success( EXPORTING is_trans = <trans>
                                                            it_msg   = rt_return ).

*  ======
    CATCH zcx_flcm_error INTO lo_flcm_exception.
*  ======

      TRY.

*       Update transaction as in error in database
        zcl_flcm_services=>db_update_trans_error( EXPORTING is_trans     = <trans>
                                                            io_exception = lo_flcm_exception ).

      CATCH zcx_flcm_error INTO lo_flcm_exception.

*       Status update failed, return the error message
        lt_msg = lo_flcm_exception->get_bapimsg_table( ).
        APPEND LINES OF lt_msg TO rt_return.

      ENDTRY.

*  ======
    ENDTRY.
*  ======

  ENDLOOP.

* Remove all processed records
  FREE mt_trans.

ENDMETHOD.
ENDCLASS.
