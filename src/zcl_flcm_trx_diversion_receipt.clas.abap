class ZCL_FLCM_TRX_DIVERSION_RECEIPT definition
  public
  inheriting from ZCL_FLCM_TRX
  final
  create public .

*"* public components of class ZCL_FLCM_TRX_DIVERSION_RECEIPT
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



CLASS ZCL_FLCM_TRX_DIVERSION_RECEIPT IMPLEMENTATION.


METHOD constructor.

* Call superclass constructor
  super->constructor( it_trans ).

* Remove irrelevant transactions
  DELETE mt_trans WHERE ( ztds_tran_type <> zcl_flcm_tds_gr_confirm=>mc_trx_diversion_receipt ).

* Make sure transactions are in order of creation
  SORT mt_trans BY ztds_tran_ref_nb.

ENDMETHOD.


METHOD execute.
*---------------------------------------------------------------------------
* -Diversion Receipt-
* The 505 transaction represents a receipt of material in TDS but in
* an un-expecting Plant, hence the term diversion is used. We need
* to process this record in SAP because at the moment of PO creation
* when the BOL is generated we also process the GR for the full PO
* amount in the original plant where the stock was due to be delivered.
* The 505 transaction gives us the indication that a certain amount of
* stock was diverted from the original plant to a new destination plant
* and hence we will need to transfer that amount from plant A to plant B.
*---------------------------------------------------------------------------
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by             Date                Tracking number *
* -------------------------        ----------          --------------- *
* Von Kaisser Almeria              2017-04-27           DV5K9A07RX     *
*                                                                      *
* Short Description : CR338898 TK357207                                *
*                     Add validation for plant to plant transfer       *
*                                                                      *
*----------------------------------------------------------------------*

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
*     Validate plant to plant transfer.
      zcl_flcm_services=>m_validate_plant( EXPORTING is_trans = <trans> ). "I- XT21610 - DV5K9A07RX - CR338898 TK357207

*     Get goods movement type
      ld_move_type = zcl_flcm_services=>get_movement_type(
                         id_trans     = <trans>-ztds_tran_type
                         id_canc_rbil = <trans>-ztds_canc_rbil ).

*     Create goods movement
      rt_return = zcl_flcm_services=>create_goods_mvmt_plant2plant(
                      id_move_type = ld_move_type
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
