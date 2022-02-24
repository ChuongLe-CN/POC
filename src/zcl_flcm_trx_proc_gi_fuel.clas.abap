class ZCL_FLCM_TRX_PROC_GI_FUEL definition
  public
  inheriting from ZCL_FLCM_TRX
  final
  create public .

*"* public components of class ZCL_FLCM_TRX_PROC_GI_FUEL
*"* do not include other source files here!!!
public section.

  constants MC_BWART type ZMM_FLCM_XREF-ZVAR_NM value 'BWART'. "#EC NOTEXT
  constants MC_MSGID_FLCM type SY-MSGID value 'ZZ_FLCM'. "#EC NOTEXT
  constants MC_MSGTY_ERROR type SY-MSGTY value 'E'. "#EC NOTEXT
  constants MC_PROGNAME type ZMM_FLCM_PARMS-PROGNAME value 'SMENH821'. "#EC NOTEXT
  constants MC_ZPARM_NM1 type ZMM_FLCM_PARMS-ZPARM_NM value 'ZTDS_TRAN_TYPE'. "#EC NOTEXT
  data MT_ZMM_FLCM_TDS type ZMM_FLCM_TDS_T .
  constants MC_ZPARM_NM type ZMM_FLCM_PARMS-ZPARM_NM value 'MISC_RAIL_EQUIP'. "#EC NOTEXT
  constants MC_ZPARM_SIGN type ZMM_FLCM_PARMS-ZPARM_SIGN value 'I'. "#EC NOTEXT

  methods CONSTRUCTOR
    importing
      !IT_TRANS type ZMM_FLCM_TDS_T
    raising
      ZCX_FLCM_ERROR .
  type-pools ABAP .
  methods IS_GVMT_CRITERIA_MET
    importing
      !IS_TRANS type ZMM_FLCM_TDS
    returning
      value(RD_RESULT) type ABAP_BOOL
    raising
      ZCX_FLCM_ERROR .

  methods EXECUTE
    redefinition .
protected section.
*"* protected components of class ZCL_FLCM_TRX_DIVERSION_RECEIPT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_FLCM_TRX_PROC_GI_FUEL
*"* do not include other source files here!!!

  types:
    BEGIN OF t_maramarc,
             mara            TYPE mara-matnr,
             meins           TYPE mara-meins,
             werks           TYPE marc-werks,
             umlmc           TYPE marc-umlmc,
    END OF t_maramarc .
  types:
    t_maramarc_tbl    TYPE STANDARD TABLE OF t_maramarc .
  types:
    BEGIN OF t_mkpf,
             mblnr           TYPE mkpf-mblnr,
             mjahr           TYPE mkpf-mjahr,
             bktxt           TYPE mkpf-bktxt,
    END OF t_mkpf .
  types:
    t_mkpf_tbl        TYPE STANDARD TABLE OF t_mkpf .
  types:
    BEGIN OF t_zmm_flcm_tds,
    werks     TYPE zmm_flcm_tds-werks,
    equnr     TYPE zmm_flcm_tds-equnr,
    zvalfrom  TYPE zmm_flcm_parms-zval_from,
  END OF t_zmm_flcm_tds .
  types:
    BEGIN OF t_zmm_flcm_parms,
      progname   TYPE zmm_flcm_parms-progname,
      zparm_nm   TYPE zmm_flcm_parms-zparm_nm,
      zparm_sign TYPE zmm_flcm_parms-zparm_sign,
      zval_from  TYPE zmm_flcm_parms-zval_from,
      zval_to    TYPE zmm_flcm_parms-zval_to,
    END OF t_zmm_flcm_parms .

  data MS_ZMM_FLCM_PARMS type ZMM_FLCM_PARMS .
  data MT_MKPF type T_MKPF_TBL .
  data MT_ZMM_FLCM_PARMS type ZMM_FLCM_PARMS_T .
  data MT_ZMM_FLCM_XREF type ZMM_FLCM_XREF_T .
ENDCLASS.



CLASS ZCL_FLCM_TRX_PROC_GI_FUEL IMPLEMENTATION.


METHOD CONSTRUCTOR.


TYPES: BEGIN OF t_zmm_flcm_tds_821,
        bktxt               TYPE mkpf-bktxt,
        werks               TYPE mseg-werks,
      END OF t_zmm_flcm_tds_821.



  DATA: r_trans_type TYPE RANGE OF zmm_flcm_tds-ztds_tran_type,
        r_bwart      TYPE RANGE OF zmm_flcm_parms-zval_from.


  DATA: lwa_range             LIKE LINE OF r_bwart,
        lv_progname           type char50,
        lv_zparm_nm1          type char50,
        lt_zmm_flcm_tds_821   type standard table of t_zmm_flcm_tds_821,
        ls_zmm_flcm_tds_821   type t_zmm_flcm_tds_821.


  FIELD-SYMBOLS:
    <trans>             LIKE LINE OF mt_trans.

* Call superclass constructor
  super->constructor( it_trans ).

* Make sure transactions are in order of creation
  SORT mt_trans BY werks
                   ztds_folio_yr
                   ztds_folio_mth
                   ztds_folio_nbr.

  refresh lt_zmm_flcm_tds_821.
  clear ls_zmm_flcm_tds_821.

* loop at mt_trans assigning <trans>.
*   ls_zmm_flcm_tds_821-bktxt = <trans>-ztds_tran_ref_nb.
*   ls_zmm_flcm_tds_821-werks = <trans>-ztds_trml_id.
*   append ls_zmm_flcm_tds_821 to lt_zmm_flcm_tds_821.

* endloop.

  IF lt_zmm_flcm_tds_821[] is not initial.

     SELECT mblnr
            mjahr
            bktxt
       FROM mkpf
       INTO TABLE mt_mkpf
       FOR ALL ENTRIES IN mt_trans
       WHERE budat EQ mt_trans-ztds_bol_dt.
     IF sy-subrc NE 0.
        refresh mt_mkpf.
     ELSE.
       SORT mt_mkpf BY bktxt.
     ENDIF.

  ENDIF.
* Retrieve data valid transaction types and movement types from zmm_flcm_parms
  SELECT * FROM zmm_flcm_parms
           INTO TABLE mt_zmm_flcm_parms
           WHERE progname EQ mc_progname
           AND zparm_nm EQ mc_zparm_nm1.
  IF sy-subrc EQ 0.
    LOOP AT mt_zmm_flcm_parms  INTO ms_zmm_flcm_parms.
      IF ms_zmm_flcm_parms-zparm_nm EQ mc_zparm_nm1.
        lwa_range-sign   = ms_zmm_flcm_parms-zparm_sign.
        lwa_range-option = ms_zmm_flcm_parms-zparm_opt.
        lwa_range-low    = ms_zmm_flcm_parms-zval_from.
        APPEND lwa_range TO r_trans_type.
      ENDIF.
      CLEAR: lwa_range,
             ms_zmm_flcm_parms.
    ENDLOOP.
  ELSE.
      IF ( 1 = 2 ). MESSAGE e044(zz_flcm). ENDIF.
      lv_progname = mc_progname.
      lv_zparm_nm1 = mc_zparm_nm1.
      RAISE EXCEPTION TYPE zcx_flcm_error
        EXPORTING
          msgid = mc_msgid_flcm
          msgty = mc_msgty_error
          msgv1 = lv_progname
          msgv2 = lv_zparm_nm1
          msgno = '044'.
  ENDIF.

  SELECT * FROM zmm_flcm_xref
     INTO TABLE mt_zmm_flcm_xref
           WHERE zvar_nm EQ mc_bwart
             AND ZVAL_QLFY IN r_trans_type.
  IF sy-subrc NE 0.
      IF ( 1 = 2 ). MESSAGE e045(zz_flcm). ENDIF.
      RAISE EXCEPTION TYPE zcx_flcm_error
        EXPORTING
          msgid = mc_msgid_flcm
          msgty = mc_msgty_error
          msgno = '045'.
  ENDIF.

ENDMETHOD.


METHOD execute.
*---------------------------------------------------------------------------
* The 502 transaction represents a Dispensing / Issueing of Fuel
*---------------------------------------------------------------------------

  DATA:
    lo_flcm_exception   TYPE REF TO zcx_flcm_error,
    lo_tds_proc_bol     TYPE REF TO zcl_flcm_trx_proc_bol,
    lo_tds_gr_confirm   TYPE REF TO zcl_flcm_tds_gr_confirm.

  DATA:
    lt_msg              TYPE bapiret2_tab,
    lt_trans            TYPE zmm_flcm_tds_t.


  DATA:
    ld_move_type        TYPE bapi2017_gm_item_create-move_type.

  FIELD-SYMBOLS:
    <trans>             LIKE LINE OF mt_trans,
    <trans2>            LIKE LINE OF mt_trans.

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

*  ======
    TRY.
*  ======

      IF ( zcl_flcm_services=>is_open_period( id_tran_ref_nb    = <trans>-ztds_tran_ref_nb
                                              id_werks          = <trans>-werks
                                              id_ztds_folio_yr  = <trans>-ztds_folio_yr
                                              id_ztds_folio_mth = <trans>-ztds_folio_mth ) EQ abap_true ).


        IF ( is_gvmt_criteria_met( <trans> ) = abap_true ).
*         Create goods movement
          rt_return = zcl_flcm_services=>create_goods_mvmt_gi_fuel( is_trans = <trans> ).
        ENDIF.
*       Set transaction status as processed successfully
        zcl_flcm_services=>db_update_trans_success( EXPORTING is_trans = <trans>
                                                              it_msg   = rt_return
                                                              id_sub_stus = <trans>-ztran_sub_stus ).
      ENDIF.
*  ======
      CATCH zcx_flcm_error INTO lo_flcm_exception.
*  ======

      TRY.

*       Update transaction as in error in database
        lo_flcm_exception->sub_stus = <trans>-ztran_sub_stus.
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


method IS_GVMT_CRITERIA_MET.



   READ TABLE mt_mkpf WITH KEY bktxt = is_trans-ztds_tran_ref_nb
                                   TRANSPORTING NO FIELDS BINARY SEARCH.

   IF sy-subrc EQ 0.

      DELETE mt_mkpf WHERE bktxt EQ is_trans-ztds_tran_ref_nb.

      READ TABLE mt_mkpf WITH KEY bktxt = is_trans-ztds_tran_ref_nb
                                      TRANSPORTING NO FIELDS BINARY SEARCH.


      IF sy-subrc EQ 0.
         DELETE mt_mkpf WHERE bktxt EQ is_trans-ztds_tran_ref_nb.
         IF ( 1 = 2 ). MESSAGE e031(zz_flcm). ENDIF.
         RAISE EXCEPTION TYPE zcx_flcm_error
            EXPORTING
             msgid = mc_msgid_flcm
             msgty = mc_msgty_error
             msgno = '031'.
      ENDIF.
   ELSE.
      rd_result = abap_true.
   ENDIF.
endmethod.
ENDCLASS.
