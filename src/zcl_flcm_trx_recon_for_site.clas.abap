class ZCL_FLCM_TRX_RECON_FOR_SITE definition
  public
  inheriting from ZCL_FLCM_TRX
  final
  create public .

*"* public components of class ZCL_FLCM_TRX_RECON_FOR_SITE
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



CLASS ZCL_FLCM_TRX_RECON_FOR_SITE IMPLEMENTATION.


METHOD CONSTRUCTOR.

* Call superclass constructor
  super->constructor( it_trans ).

* Remove irrelevant transactions
  DELETE mt_trans WHERE ( ztds_tran_type <> zcl_flcm_tds_gr_confirm=>mc_trx_site2site_transfer ).

* Make sure transactions are in order of creation
  SORT mt_trans BY ztds_tran_ref_nb.

ENDMETHOD.


METHOD EXECUTE.
*---------------------------------------------------------------------------
* -Diversion Receipt-
* The 702 transaction represents a reconciliation for Site to Site Transfers
*---------------------------------------------------------------------------

  DATA:
    lo_flcm_exception   TYPE REF TO zcx_flcm_error.

  DATA:
    lt_msg              TYPE bapiret2_tab.

  DATA:
    ld_move_type        TYPE bapi2017_gm_item_create-move_type,
    ld_sub_stus         TYPE zmm_flcm_tds-ztran_sub_stus,
    lt_return           TYPE bapiret2tab.


  FIELD-SYMBOLS:
    <trans>             LIKE LINE OF mt_trans.

  REFRESH:
    rt_return.
  REFRESH:
    lt_return.

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
*   Set sub status for error reprocessing
    CLEAR ld_sub_stus.
    IF ( <trans>-ztran_stus = zcl_flcm_services=>mc_status_error ).
      ld_sub_stus = <trans>-ztran_sub_stus.
    ELSE.
      ld_sub_stus = '15'.
    ENDIF.

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


      CASE ld_sub_stus.


       WHEN '15'.
* Get movement type
        ld_move_type = zcl_flcm_services=>get_movement_type(
                                   id_trans     = <trans>-ztds_tran_type
                                   id_canc_rbil = <trans>-ztds_canc_rbil ).


*     Create goods movement
         rt_return = zcl_flcm_services=>create_goods_mvmt_plant2plant2(
                                   id_move_type = ld_move_type
                                   is_trans     = <trans>
                                   id_sub_stus  = ld_sub_stus ).
         append lines of rt_return to lt_return.
*     Create goods movement
         rt_return = zcl_flcm_services=>create_goods_mvmt_gain_loss(
                                   is_trans     = <trans>
                                   id_sub_stus  = '016' ).
         append lines of rt_return to lt_return.
         rt_return[] = lt_return[].


       WHEN '16'.

*     Create goods movement
         rt_return = zcl_flcm_services=>create_goods_mvmt_gain_loss(
                                   is_trans     = <trans>
                                   id_sub_stus  = '016' ).


      ENDCASE.

*     Set transaction status as processed successfully
      zcl_flcm_services=>db_update_trans_success( EXPORTING is_trans = <trans>
                                                            it_msg   = rt_return ).

*  ======
    CATCH zcx_flcm_error INTO lo_flcm_exception.
*  ======

      TRY.

*       Update transaction as in error in database
        zcl_flcm_services=>db_update_trans_error( EXPORTING is_trans     = <trans>
                                                            io_exception = lo_flcm_exception
                                                  CHANGING  it_msg       = lt_return ).

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
