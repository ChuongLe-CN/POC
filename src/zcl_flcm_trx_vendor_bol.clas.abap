class ZCL_FLCM_TRX_VENDOR_BOL definition
  public
  inheriting from ZCL_FLCM_TRX
  final
  create public .

*"* public components of class ZCL_FLCM_TRX_VENDOR_BOL
*"* do not include other source files here!!!
public section.

  constants MC_LITRES type EKPO-MEINS value 'L' ##NO_TEXT.
  constants MC_GALLONS type EKPO-MEINS value 'GLL' ##NO_TEXT.
  constants MC_TDS type CHAR03 value 'TDS' ##NO_TEXT.
  constants MC_CAR type CHAR03 value 'CAR' ##NO_TEXT.
  constants MC_PROGNAME type ZMM_FLCM_PARMS-PROGNAME value 'SMENH820' ##NO_TEXT.
  constants MC_PROGNAME_819 type ZMM_FLCM_PARMS-PROGNAME value 'SMENH819' ##NO_TEXT.
  constants MC_MSGID_FLCM type SY-MSGID value 'ZZ_FLCM' ##NO_TEXT.
  constants MC_MSGTY_ERROR type SY-MSGTY value 'E' ##NO_TEXT.
  constants MC_PROGNAME_820 type ZMM_FLCM_PARMS-PROGNAME value 'SMENH820' ##NO_TEXT.
  constants MC_ZPARM_CONF_CNTRL type ZMM_FLCM_PARMS-ZPARM_NM value 'CONF_CTRL' ##NO_TEXT.
  constants MC_ZPARM_EBTYP type ZMM_FLCM_PARMS-ZPARM_NM value 'EBTYP' ##NO_TEXT.
  constants MC_ZPARM_MATKL type ZMM_FLCM_PARMS-ZPARM_NM value 'MATKL' ##NO_TEXT.
  constants MC_ZPARM_ZCARRIER type ZMM_FLCM_PARMS-ZPARM_NM value 'ZCARRIER_NP' ##NO_TEXT.
  data MT_TRANS_CANCEL type ZMM_FLCM_TDS_T .

  methods CONSTRUCTOR
    importing
      !IT_TRANS type ZMM_FLCM_TDS_T
    raising
      ZCX_FLCM_ERROR .
  methods INVALID_SUB_STATUS
    importing
      !ID_SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS
    raising
      ZCX_FLCM_ERROR .
  methods IS_CONFIRMATION_FOUND
    importing
      !ID_EBELN type EKKO-EBELN
      !ID_EBELP type EKPO-EBELP
    returning
      value(RD_RESULT) type ABAP_BOOL .
  methods IS_FUEL_PO_FOUND
    importing
      !IS_TRANS type ZMM_FLCM_TDS
    returning
      value(RD_RESULT) type ABAP_BOOL
    raising
      ZCX_FLCM_ERROR .
  methods IS_SUPPLIER_CARRIER_NOT_EXCL
    importing
      !IS_TRANS type ZMM_FLCM_TDS
    returning
      value(RD_RESULT) type ABAP_BOOL
    raising
      ZCX_FLCM_ERROR .
  methods IS_GR_ALREADY_DONE
    returning
      value(RD_RESULT) type ABAP_BOOL
    raising
      ZCX_FLCM_ERROR .
  methods IS_GR_TRANSPORT_PO_FOUND
    importing
      !IS_TRANS type ZMM_FLCM_TDS
    returning
      value(RD_RESULT) type ABAP_BOOL
    raising
      ZCX_FLCM_ERROR .
  methods IS_GR_TRANSPORT_PO_RET_FOUND
    importing
      !IS_TRANS type ZMM_FLCM_TDS
    returning
      value(RD_RESULT) type ABAP_BOOL
    raising
      ZCX_FLCM_ERROR .
  methods IS_TRANSPORT_PO_FOUND
    importing
      !IS_TRANS type ZMM_FLCM_TDS
    returning
      value(RD_RESULT) type ABAP_BOOL
    raising
      ZCX_FLCM_ERROR .
  methods RETREIVE_NEW_PO
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !ID_EBELN type EKKO-EBELN .
  methods RETREIVE_NEW_PO_LINE
    raising
      ZCX_FLCM_ERROR .
  methods GET_HEADER_TEXT
    importing
      !ID_EBELN type EBELN
    returning
      value(RD_RESULT) type STRING .

  methods EXECUTE
    redefinition .
protected section.
*"* protected components of class ZCL_FLCM_TRX_VENDOR_BOL
*"* do not include other source files here!!!
PRIVATE SECTION.
*"* private components of class ZCL_FLCM_TRX_VENDOR_BOL
*"* do not include other source files here!!!

  TYPES:
    BEGIN OF t_unique_po,
      ebeln TYPE ekko-ebeln,
      ebelp TYPE ekpo-ebelp,
      ihrez TYPE ekko-ihrez,
      lifnr TYPE ekko-lifnr,
      unsez TYPE ekko-unsez,
      loekz TYPE ekpo-loekz,
      matkl TYPE ekpo-matkl,
      matnr TYPE ekpo-matnr,
      knttp TYPE ekpo-knttp,
      retpo TYPE ekpo-retpo,
      bstae TYPE ekpo-bstae,
      bednr TYPE ekpo-bednr,
      menge TYPE ekpo-menge,
      meins TYPE ekpo-meins,
      eindt TYPE eket-eindt,
    END OF t_unique_po .
  TYPES:
    t_unique_po_tbl   TYPE STANDARD TABLE OF t_unique_po .
  TYPES:
    BEGIN OF t_contract,
      ebeln TYPE eord-ebeln,
      ebelp TYPE eord-ebelp,
      ekorg TYPE ekko-ekorg,
      ekgrp TYPE ekko-ekgrp,
      lifnr TYPE ekko-lifnr,
      bsart TYPE ekko-bsart,
      kdatb TYPE ekko-kdatb,
      kdate TYPE ekko-kdate,
      matkl TYPE ekpo-matkl,
      knttp TYPE ekpo-knttp,
      loekz TYPE ekpo-loekz,
      werks TYPE ekpo-werks,
      meins TYPE ekpo-meins,
      adrnr TYPE ekpo-adrnr,
      matnr TYPE ekpo-matnr,
      sort1 TYPE adrc-sort1,
    END OF t_contract .
  TYPES:
    t_contract_tbl TYPE STANDARD TABLE OF t_contract .
  TYPES:
    BEGIN OF t_ekes,
      ebeln TYPE eord-ebeln,
      ebelp TYPE eord-ebelp,
    END OF t_ekes .
  TYPES:
    t_ekes_tbl TYPE STANDARD TABLE OF t_ekes .

*--> Start of insert DV5K9A05WW - Chuong Le - 2017/01/04
  TYPES:
    BEGIN OF text_type,
      ebeln  TYPE ebeln,
      tdline TYPE tdline,
    END OF text_type .
  DATA:
    mt_texttab TYPE SORTED TABLE OF text_type WITH UNIQUE KEY ebeln .
*<-- End of insert DV5K9A05WW - Chuong Le - 2017/01/04

  CONSTANTS mc_msgty_success TYPE sy-msgty VALUE 'S' ##NO_TEXT.
  DATA ms_contract TYPE zmm_flcm_contract .
  DATA ms_unique_po TYPE zmm_flcm_unique_po .
  DATA ms_conf_cntrl TYPE zmm_flcm_parms-zval_from .
  DATA ms_ebtyp TYPE zmm_flcm_parms-zval_from .
  DATA ms_matkl TYPE zmm_flcm_parms-zval_from .
  DATA mrt_zcarrier_np_range TYPE zmm_flcm_smenh820_range_t .
  DATA mt_a016 TYPE mmpur_t_a016 .
  DATA mt_contract TYPE zmm_flcm_contract_tp .
  DATA mt_ekes TYPE t_ekes_tbl .
  DATA mt_mseg TYPE ty_mseg .
  DATA mt_unique_po TYPE zmm_flcm_unique_po_tp .
  DATA mt_unique_po_car TYPE zmm_flcm_unique_po_tp .
  DATA mt_unique_po_tds TYPE zmm_flcm_unique_po_tp .
  DATA mrt_matkl_range TYPE zmm_flcm_smenh820_range_t .
ENDCLASS.



CLASS ZCL_FLCM_TRX_VENDOR_BOL IMPLEMENTATION.


METHOD constructor.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Chari Flor Supino         2017/08/03          DV5K9A0ARX             *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Add delivery date for unique PO combination       *
*----------------------------------------------------------------------*
* Chuong Le                  2017/01/04          DV5K9A05WW            *
*                                                                      *
* Short Description: CR306321-T347799                                  *
*    - Read Our Reference from PO header text instead of EKKO-UNSEZ.   *
*----------------------------------------------------------------------*
* Marie Kris Mendoza (XT18775) 2013-07-30        DV5K980540            *
*                                                                      *
* Short Description: CR227428 TK242984                                 *
*                    Make sure all accesses using ZMM_FLCM_PARMS-ZVAL  *
*                    = "SMENH820" and ZMM_FLCM_PARMS-ZPARM_NM = "MATKL"*
*                    can process multiple values returned by this      *
*                    selection.  A carrier contract can now be created *
*                    with either MATKL = TRNS9070 or TRNS9079          *
*----------------------------------------------------------------------*
  TYPES: begin of t_adrc,
          addrnumber   TYPE adrc-addrnumber,
          sort1        TYPE adrc-sort1,
         end of t_adrc.

  TYPES: begin of t_eord,
          ebeln        TYPE eord-ebeln,
          ebelp        TYPE eord-ebelp,
          matnr        TYPE eord-matnr,
         end of t_eord.

  DATA:
     ls_zmm_flcm_parms TYPE zmm_flcm_parms,
     lt_zmm_flcm_parms TYPE STANDARD TABLE OF zmm_flcm_parms,
     lv_progname       TYPE char50,
     lv_zparm_nm1      TYPE char50,
     r_zval_from       TYPE RANGE OF zmm_flcm_parms-zval_from,
     ld_index          TYPE sy-index.


  DATA:
     lwa_range             LIKE LINE OF r_zval_from,
     lt_trans_car          TYPE ZMM_FLCM_TDS_T,
     lt_trans_tds          TYPE ZMM_FLCM_TDS_T,
     lt_adrc               TYPE STANDARD TABLE OF t_adrc,
     lt_eord               TYPE STANDARD TABLE OF t_eord,
     ls_contract           TYPE zmm_flcm_contract.

  FIELD-SYMBOLS:
     <trans>               TYPE zmm_flcm_tds,
     <ms_unique_po>        TYPE zmm_flcm_unique_po,
     <adrc>                TYPE t_adrc,
     <eord>                TYPE t_eord,
     <contract>            TYPE zmm_flcm_contract.




* Call superclass constructor
  super->constructor( it_trans ).

* Remove irrelevant transactions
  DELETE mt_trans WHERE ( ztds_tran_type <> zcl_flcm_tds_gr_confirm=>mc_trx_vendor_bol ).

* Make sure transactions are in order of creation
  SORT mt_trans BY ztds_tran_ref_nb.

* Retrieve valid data for matkl , ebtyp and zcarrier_np
  SELECT * FROM zmm_flcm_parms
           INTO TABLE lt_zmm_flcm_parms
           WHERE progname EQ mc_progname.
  IF sy-subrc EQ 0.
    LOOP AT lt_zmm_flcm_parms  INTO ls_zmm_flcm_parms.
      IF ls_zmm_flcm_parms-zparm_nm EQ mc_zparm_zcarrier.
        lwa_range-sign   = ls_zmm_flcm_parms-zparm_sign.
        lwa_range-option = ls_zmm_flcm_parms-zparm_opt.
        lwa_range-low    = ls_zmm_flcm_parms-zval_from.
        APPEND lwa_range TO mrt_zcarrier_np_range.
      ELSEIF ls_zmm_flcm_parms-zparm_nm EQ mc_zparm_ebtyp.
             ms_ebtyp = ls_zmm_flcm_parms-zval_from.
      ELSEIF ls_zmm_flcm_parms-zparm_nm EQ mc_zparm_matkl.
*             ms_matkl = ls_zmm_flcm_parms-zval_from.             "D-XT18775 - DV5K980540- CR227428 TK242984
***Start of Insert - XT18775 - DV5K980540- CR227428 TK242984
        "Handle multiple values of MAKTL
        lwa_range-sign   = ls_zmm_flcm_parms-zparm_sign.
        lwa_range-option = ls_zmm_flcm_parms-zparm_opt.
        lwa_range-low    = ls_zmm_flcm_parms-zval_from.
        APPEND lwa_range TO mrt_matkl_range.
***End of Insert - XT18775 - DV5K980540- CR227428 TK242984
      ENDIF.
      CLEAR: lwa_range,
             ls_zmm_flcm_parms.
    ENDLOOP.
  ELSE.
      IF ( 1 = 2 ). MESSAGE e044(zz_flcm). ENDIF.
      lv_progname = mc_progname.
      concatenate mc_zparm_zcarrier mc_zparm_ebtyp mc_zparm_matkl into
      lv_zparm_nm1 separated by ' '.
      RAISE EXCEPTION TYPE zcx_flcm_error
        EXPORTING
          msgid = mc_msgid_flcm
          msgty = mc_msgty_error
          msgv1 = lv_progname
          msgv2 = lv_zparm_nm1
          msgno = '044'.
  ENDIF.

  SELECT SINGLE zval_from
           FROM zmm_flcm_parms
           INTO ms_conf_cntrl
           WHERE progname EQ mc_progname_819
           AND   zparm_nm EQ mc_zparm_conf_cntrl.
  IF sy-subrc NE 0.
      IF ( 1 = 2 ). MESSAGE e044(zz_flcm). ENDIF.
      lv_progname = mc_progname.
      lv_zparm_nm1 = mc_zparm_conf_cntrl.
      RAISE EXCEPTION TYPE zcx_flcm_error
        EXPORTING
          msgid = mc_msgid_flcm
          msgty = mc_msgty_error
          msgv1 = lv_progname
          msgv2 = lv_zparm_nm1
          msgno = '044'.
  ENDIF.

* find transport po's for CAR & TDS

  IF mt_trans[] is not initial.

    SELECT a~ebeln
           b~ebelp
           a~ihrez
           a~lifnr
*           a~unsez                                        "DV5K9A05WW-
           b~loekz
           b~matkl
           b~matnr
           b~knttp
           b~retpo
           b~bstae
           b~bednr
           b~menge
           b~meins
           b~elikz
***Start of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
           c~eindt
***End of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
     FROM ekko AS a
     INNER JOIN ekpo AS b ON
     b~ebeln = a~ebeln
***Start of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
   INNER JOIN eket AS c ON
   c~ebeln = b~ebeln AND
   c~ebelp = b~ebelp
***End of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
     INTO CORRESPONDING FIELDS OF TABLE  mt_unique_po      "DV5K9A05WW+
     FOR ALL ENTRIES IN mt_trans
     WHERE a~bsart EQ 'FB'
     AND   ( a~lifnr EQ mt_trans-lifnr ).
*    AND     b~matnr EQ mt_trans-matnr
*    AND     b~knttp EQ ' '     )
*     OR      a~lifnr EQ mt_trans-zcarrier ).
*    AND     b~matkl EQ ms_matkl
*    AND   b~knttp EQ 'K') )
*    AND   b~loekz EQ ' '.

    SELECT a~ebeln
           b~ebelp
           a~ihrez
           a~lifnr
*           a~unsez                                        "DV5K9A05WW-
           b~loekz
           b~matkl
           b~matnr
           b~knttp
           b~retpo
           b~bstae
           b~bednr
           b~menge
           b~meins
           b~elikz
***Start of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
           c~eindt
***End of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
     FROM ekko AS a
     INNER JOIN ekpo AS b ON
     b~ebeln = a~ebeln
***Start of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
   INNER JOIN eket AS c ON
   c~ebeln = b~ebeln AND
   c~ebelp = b~ebelp
***End of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
     APPENDING CORRESPONDING FIELDS OF TABLE  mt_unique_po "DV5K9A05WW+
     FOR ALL ENTRIES IN mt_trans
     WHERE a~bsart EQ 'FB'
*    AND   ( a~lifnr EQ mt_trans-lifnr
*    AND     b~matnr EQ mt_trans-matnr
*    AND     b~knttp EQ ' '     )
     AND   ( a~lifnr EQ mt_trans-zcarrier ).
*    AND     b~matkl EQ ms_matkl
*    AND   b~knttp EQ 'K') )
*    AND   b~loekz EQ ' '.

    sort mt_trans by ztds_bol_nbr.

    delete mt_unique_po where loekz ne ' '.

*--> Start of Insert DV5K9A05WW - Chuong Le - 2017/01/04
    loop at mt_unique_po assigning <ms_unique_po>.
      <ms_unique_po>-unsez = get_header_text( <ms_unique_po>-ebeln ).
    endloop.
*<-- End of Insert DV5K9A05WW - Chuong Le - 2017/01/04

    loop at mt_unique_po assigning <ms_unique_po>.
* need to remove the entries that do not satisfy the knttp and matnr matkl since performance is improved
* if it is done this way

      read table mt_trans assigning <trans>
        with key ztds_bol_nbr = <ms_unique_po>-ihrez BINARY SEARCH.

      check sy-subrc EQ 0.
* if its for CAR then append it to the mt_unique_po_car
* otherwise append it to the mt_unique_po_tds
      if ( <ms_unique_po>-lifnr eq <trans>-lifnr
      AND <ms_unique_po>-matnr eq <trans>-matnr
      AND <ms_unique_po>-knttp is initial )
***Start of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
      AND <ms_unique_po>-eindt EQ <trans>-ztds_bol_dt.
***End of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
      " nothing this is ok
      elseif ( <ms_unique_po>-lifnr eq <trans>-zcarrier
*           AND <ms_unique_po>-matkl eq ms_matkl                  "D-XT18775 - DV5K980540- CR227428 TK242984
           AND <ms_unique_po>-matkl IN mrt_matkl_range            "I-XT18775 - DV5K980540- CR227428 TK242984
           AND <ms_unique_po>-knttp eq 'K' )
***Start of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
           AND <ms_unique_po>-eindt EQ <trans>-ztds_bol_dt.
***End of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
      " nothing this is ok
      else.
         continue.
      endif.

      if <ms_unique_po>-unsez+0(3) = mc_car.
         append <ms_unique_po> to mt_unique_po_car.
      elseif <ms_unique_po>-unsez+0(3) = mc_tds.
         append <ms_unique_po> to mt_unique_po_tds.
      endif.

    endloop.


* Make sure transactions are in order of creation
    SORT mt_trans BY ztds_tran_ref_nb.
    REFRESH mt_contract.

* find contracts

    SELECT b~ebeln
           b~ebelp
           a~ekorg
           a~ekgrp
           a~lifnr
           a~bsart
           a~kdatb
           a~kdate
           b~matkl
           b~knttp
           b~loekz
           b~werks
           b~meins
           b~adrn2
           b~matnr
           a~bukrs
           a~zterm
           b~bednr
*          d~sort1
     FROM ekko AS a
     INNER JOIN ekpo AS b ON
     b~ebeln = a~ebeln
     INTO TABLE  mt_contract
     FOR ALL ENTRIES IN mt_trans
     WHERE a~lifnr EQ mt_trans-zcarrier
     AND   a~bsart EQ 'FK'
     AND   a~kdatb LE mt_trans-ztds_bol_dt
     AND   a~kdate GE mt_trans-ztds_bol_dt
*     AND   b~matkl eq ms_matkl                                   "D-XT18775 - DV5K980540- CR227428 TK242984
     AND   b~matkl IN mrt_matkl_range                             "I-XT18775 - DV5K980540- CR227428 TK242984
     AND   b~knttp EQ 'K'
     AND   b~loekz EQ ' '
     AND   b~werks EQ mt_trans-werks.

     delete mt_contract where meins ne mc_litres
                        and   meins ne mc_gallons.


     if mt_contract[] is not initial.
* get the address number for the sort1 field seperate they do not all have adrnr
       SELECT addrnumber
              sort1
       FROM adrc
       INTO TABLE lt_adrc
       FOR ALL ENTRIES IN mt_contract
       WHERE addrnumber EQ mt_contract-adrnr.

       SORT lt_adrc BY addrnumber.

       LOOP AT mt_contract assigning <contract>.
         read table lt_adrc assigning <adrc>
           with key addrnumber = <contract>-adrnr BINARY SEARCH.
         IF sy-subrc EQ 0.
            <contract>-sort1 = <adrc>-sort1.
         ENDIF.

       ENDLOOP.

* get the purchasing source list seperate since they do not all have source lists
       SELECT ebeln
              ebelp
              matnr
       FROM eord
       INTO TABLE lt_eord
       FOR ALL ENTRIES IN mt_contract
       WHERE ebeln      EQ mt_contract-ebeln
       AND   ebelp      EQ mt_contract-ebelp.

       SORT lt_eord BY ebeln ebelp.

       LOOP AT lt_eord assigning <eord>.
          clear ld_index.
          read table mt_contract assigning <contract>
             with key ebeln = <eord>-ebeln
                      ebelp = <eord>-ebelp   BINARY SEARCH.
          ld_index = sy-tabix.
          IF sy-subrc EQ 0.
             <contract>-matnr = <eord>-matnr.
             modify mt_contract index ld_index from <contract>.
          ENDIF.

       ENDLOOP.
* add pricing data information for the contract this will be used when searching the contract to ensure the pricing
* info is valid otherwise you get an error on the Po Bapi Create
* now retreive the entries for pricing
       refresh mt_a016.
       SELECT *
       FROM A016
       INTO TABLE mt_a016
       FOR ALL ENTRIES IN mt_contract
       WHERE kappl      EQ 'M'
       AND   kschl      EQ 'PB00'
       AND   evrtn      EQ mt_contract-ebeln
       AND   evrtp      EQ mt_contract-ebelp.

     ENDIF.

* get confirmation info
     IF mt_unique_po_tds[] is not initial.
        SELECT ebeln
               ebelp
        FROM ekes
        INTO TABLE mt_ekes
        FOR ALL ENTRIES IN mt_unique_po_tds
        WHERE ebeln      EQ mt_unique_po_tds-ebeln
        AND   ebelp      EQ mt_unique_po_tds-ebelp
        AND   ebtyp      EQ ms_ebtyp
        AND   estkz      EQ '1'.

        SORT mt_ekes BY ebeln ebelp.
     ENDIF.
  ENDIF.

  REFRESH mt_trans_cancel.

  SELECT *
    FROM zmm_flcm_tds
    APPENDING TABLE mt_trans_cancel
    FOR ALL ENTRIES IN mt_trans
   WHERE ztds_tran_type   EQ mt_trans-ztds_tran_type
   AND ztds_bol_nbr       EQ mt_trans-ztds_bol_nbr
   AND ztds_canc_rbil     EQ mc_ind_cancel.

ENDMETHOD.


METHOD execute.
*---------------------------------------------------------------------------
* -Reconciliation for Vendor BOLs-
* The 700 transaction is sent to SAP once a vendor BOL has been
* reconciled, hence confirmed as completely delivered.  This means
* that at this point the T G/L quantity can be calculated by TDS
* and sent to SAP for inventory adjustment.
*---------------------------------------------------------------------------

  DATA:
    lo_flcm_exception   TYPE REF TO zcx_flcm_error.

  DATA:
    lt_msg              TYPE bapiret2_tab,
    ld_msgv1            TYPE symsgv,
    ls_return           TYPE bapiret2,
    lt_return           TYPE bapiret2tab.


  DATA:
    ld_sub_stus         TYPE zmm_flcm_tds-ztran_sub_stus,
    ld_ebeln            TYPE ekko-ebeln,
    ld_move_type        TYPE bapi2017_gm_item_create-move_type,
    ld_processed        TYPE abap_bool,
    ld_update_ebtyp     TYPE abap_bool.

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

*   Set sub status for error reprocessing
    CLEAR ld_sub_stus.
    IF ( <trans>-ztran_stus = zcl_flcm_services=>mc_status_error ).
      ld_sub_stus = <trans>-ztran_sub_stus.
    ENDIF.
    REFRESH lt_return.
    CLEAR   ls_return.
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

* set the sub status to process we want it to fall into if the record is of type status ready
* otherwise the sub status is already set because the record we are processing is of type
* error and it would already have a sub status
      if ( ld_sub_stus is initial ).

          if ( <trans>-ztds_canc_rbil NE mc_ind_cancel ).

              if ( is_supplier_carrier_not_excl( <trans> ) = abap_true ).
*           STEP 13
                 ld_sub_stus = '10'.
              else.
*           STEP 17
                 ld_msgv1 = <trans>-zcarrier.
                 IF ( 1 = 2 ). MESSAGE s085(zz_flcm). ENDIF.
                 ls_return-type   = mc_msgty_success.
                 ls_return-id     = mc_msgid_flcm.
                 ls_return-number = '085'.
                 ls_return-message_v1 = ld_msgv1.
                 append ls_return to lt_return.
                 ld_sub_stus = '13'.

              endif.
          else.
*           STEP 23
                 ld_sub_stus = '10'.
          endif.
      elseif ( is_supplier_carrier_not_excl( <trans> ) = abap_true ).
             ld_sub_stus = <trans>-ztran_sub_stus.
      endif.

      ld_processed       = abap_false.
      while ld_processed = abap_false.

        CASE ld_sub_stus.

           WHEN '10'.

              if ( is_transport_po_found( <trans> ) = abap_true ).
*             STEP 23 find transport po

                 if ( <trans>-ztds_canc_rbil = mc_ind_cancel ).
*             STEP 24 Update transport po return line
                    rt_return =
                    zcl_flcm_services=>update_po( is_trans     = <trans>
                                                   id_ebeln     = ms_unique_po-ebeln
                                                   id_ebelp     = ms_unique_po-ebelp
                                                   id_sub_stus  = ld_sub_stus ).
                    append lines of rt_return to lt_return.

                 elseif ( <trans>-ztds_canc_rbil = mc_ind_rebill ).
*             STEP 14 Update transport po new line
                     rt_return =
                     zcl_flcm_services=>reverse_po( is_trans     = <trans>
                                                    is_contract  = ms_contract
                                                    is_unique_po = ms_unique_po
                                                    id_sub_stus  = ld_sub_stus ).

                     append lines of rt_return to lt_return.

                 endif.
                 ld_sub_stus = '12'.
* we need to add the po to the attribute mt_unique_po
                 TRY.
                   me->RETREIVE_NEW_PO_LINE( ).
                   CATCH zcx_flcm_error INTO lo_flcm_exception.
                 ENDTRY.
              else.

                 if ( <trans>-ztds_canc_rbil ne mc_ind_cancel ).
*                   STEP 15
                    call method zcl_flcm_services=>create_new_transport_po(
                                       EXPORTING is_trans        = <trans>
                                                 is_contract     = ms_contract
                                                 id_sub_stus     = ld_sub_stus
                                       IMPORTING et_return       = rt_return
                                                 ed_ebeln        = ld_ebeln               ).

                    append lines of rt_return to lt_return.
* we need to add the po to the attribute mt_unique_po_car
                    CALL METHOD me->RETREIVE_NEW_PO( is_trans = <trans>
                                                     id_ebeln = ld_ebeln                  ).
                       .
                    ld_sub_stus = '12'.


                 else.
*           STEP 13
                    ld_sub_stus = '13'.

                 endif.

              endif.

           WHEN '12'.

              if ( <trans>-ztds_canc_rbil = mc_ind_cancel ).
*                STEP 25 GR transport PO return line
                 if ( is_gr_transport_po_ret_found( <trans> ) = abap_true ).
                    rt_return =
                    zcl_flcm_services=>create_goods_mvmt_transport_po( is_trans     = <trans>
                                                                       is_unique_po = ms_unique_po
                                                                       id_sub_stus  = ld_sub_stus ).
                    append lines of rt_return to lt_return.
                    ld_sub_stus = '13'.
                 endif.
              else.
                  if ( is_gr_transport_po_found( <trans> ) = abap_true ).
*                STEP 16 Process GR for Transport PO
                       rt_return =
                       zcl_flcm_services=>create_goods_mvmt_transport_po( is_trans     = <trans>
                                                                      is_unique_po = ms_unique_po
                                                                      id_sub_stus  = ld_sub_stus ).
                       append lines of rt_return to lt_return.
                       ld_sub_stus = '13'.
                   else.
                       ld_sub_stus = '13'.
                  endif.
              endif.

           WHEN '13'.
              rt_return =
              zcl_flcm_services=>create_goods_mvmt_gain_loss( is_trans     = <trans>
                                                              id_sub_stus  = ld_sub_stus ).

              append lines of rt_return to lt_return.
*             STEP 26 Call Cance T G/L
              if ( <trans>-ztds_canc_rbil = mc_ind_cancel ).
                   ld_processed = abap_true.
              else.
*             STEP 17 Start Apply T Gain or Loss
                   ld_sub_stus = '14'.
              endif.

           WHEN '14'.

*             STEP 18 Find Fuel PO
              if ( is_fuel_po_found( <trans> ) = abap_true ).
*                step 19 Update Fuel Po with confirmation
                 ld_update_ebtyp = abap_false.
                 if ( is_confirmation_found( id_ebeln = ms_unique_po-ebeln
                                             id_ebelp = ms_unique_po-ebelp ) = abap_false ).
                     ld_update_ebtyp = abap_true.
                 endif.
                 rt_return =
                 zcl_flcm_services=>update_fuel_po( is_trans        =   <trans>
                                                    id_ebeln        = ms_unique_po-ebeln
                                                    id_ebelp        = ms_unique_po-ebelp
                                                    id_ebtyp        = ms_ebtyp
                                                    id_bednr        = ms_unique_po-bednr
                                                    id_matnr        = ms_unique_po-matnr
                                                    id_meins        = ms_unique_po-meins
                                                    id_update_ebtyp = ld_update_ebtyp ).
                 append lines of rt_return to lt_return.
                 ld_processed = abap_true.
              endif.

           WHEN others.
               CALL METHOD me->INVALID_SUB_STATUS( id_sub_stus = ld_sub_stus ).


         ENDCASE.
      endwhile.

      rt_return[] = lt_return[].

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
        ld_processed = abap_true.

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


  METHOD get_header_text.
*---------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)             *
*---------------------------------------------------------------------*
* Changed / Created by              Date              Tracking number *
* ---------------------------      ----------         --------------- *
* Chuong Le  CR306321-T347799       2017/01/04        DV5K9A05WW      *
*                                                                     *
*    - Read PO header text.                                           *
*---------------------------------------------------------------------*

    DATA:
      lt_lines TYPE tline_tab.

    CLEAR rd_result.

    IF line_exists( mt_texttab[ ebeln = id_ebeln ] ).
      rd_result = mt_texttab[ ebeln = id_ebeln ]-tdline.
    ELSE.
      CLEAR lt_lines[].
      "Read English text
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = 'F99'
          language                = 'E'
          name                    = CONV tdobname( id_ebeln )
          object                  = 'EKKO'
        TABLES
          lines                   = lt_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF sy-subrc <> 0.
        "Read French text
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = 'F99'
            language                = 'F'
            name                    = CONV tdobname( id_ebeln )
            object                  = 'EKKO'
          TABLES
            lines                   = lt_lines
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.
      ENDIF.

      IF lt_lines[] IS NOT INITIAL.
        rd_result = lt_lines[ 1 ]-tdline.     "1st line only
      ENDIF.

      "Add to PO text table
      mt_texttab = VALUE #( BASE mt_texttab ( ebeln  = id_ebeln
                                              tdline = rd_result ) ).

    ENDIF.

  ENDMETHOD.


method INVALID_SUB_STATUS.

*   Raise exception
    IF ( 1 = 2 ). MESSAGE e071(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '071'
        sub_stus = id_sub_stus.

endmethod.


method IS_CONFIRMATION_FOUND.

  rd_result = abap_false.

  FIELD-SYMBOLS:
     <ekes>  TYPE t_ekes.

  read table mt_ekes assigning <ekes>
    with key ebeln = id_ebeln
             ebelp = id_ebelp.

  IF sy-subrc EQ 0.
     rd_result = abap_true.
  ENDIF.


endmethod.


method IS_FUEL_PO_FOUND.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Chari Flor Supino         2017/08/03          DV5K9A0ARX             *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Add delivery date for unique PO combination       *
*----------------------------------------------------------------------*

  FIELD-SYMBOLS:
    <ms_unique_po> LIKE LINE OF mt_unique_po_tds.

  rd_result = abap_false.


  loop at mt_unique_po_tds assigning <ms_unique_po>
     where ihrez      = is_trans-ztds_bol_nbr
     and   lifnr      = is_trans-lifnr
     and   unsez+4(7) = is_trans-ztds_vdr_trml_id
     and   loekz      is initial
     and   matnr      = is_trans-matnr
     and   knttp      is initial
*    and   retpo      is initial
     and   bstae      EQ ms_conf_cntrl
***Start of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
     AND   eindt      EQ is_trans-ztds_bol_dt.
***End of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122

  endloop.

  if sy-subrc EQ 0.
     if <ms_unique_po>-retpo eq abap_false.
        ms_unique_po = <ms_unique_po>.
        rd_result = abap_true.
     endif.
  endif.

  if rd_result = abap_false.
*   Raise exception
    IF ( 1 = 2 ). MESSAGE e074(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '074'
        sub_stus = '14'.
*       msgtab   = et_return.
  endif.


endmethod.


method IS_GR_ALREADY_DONE.


  DATA:
    ls_mseg        TYPE mseg,
    ld_msgv1       TYPE sy-msgv1.

  FIELD-SYMBOLS:
    <mseg>         TYPE mseg.

  rd_result = abap_false.
  clear ls_mseg.

* now we need to check the mseg table for the GR
  if ms_unique_po is not initial.
     read table mt_mseg assigning <mseg>
        with key ebeln = ms_unique_po-ebeln
                 ebelp = ms_unique_po-ebelp BINARY SEARCH.
     if sy-subrc EQ 0.
        if <mseg>-kzbew = 'B'.
           rd_result = abap_true.
        endif.
     else.
*   Get value from database table
       SELECT SINGLE *
                INTO ls_mseg
                FROM mseg
               WHERE ( ebeln          = ms_unique_po-ebeln )
                 AND ( ebelp          = ms_unique_po-ebelp ). "#EC CI_NOFIELD
       IF ( sy-subrc = 0 ).
*     Return value and put it in buffer
         APPEND ls_mseg TO mt_mseg.
         IF ls_mseg-kzbew = 'B'.
            rd_result = abap_true.
         ENDIF.
         SORT mt_mseg BY ebeln ebelp.
     endif.
    ENDIF.
  ENDIF.




endmethod.


method IS_GR_TRANSPORT_PO_FOUND.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Chari Flor Supino         2017/08/03          DV5K9A0ARX             *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Add delivery date for unique PO combination       *
*----------------------------------------------------------------------*
* Marie Kris Mendoza (XT18775) 2013-07-30        DV5K980540            *
*                                                                      *
* Short Description: CR227428 TK242984                                 *
*                    Make sure all accesses using ZMM_FLCM_PARMS-ZVAL  *
*                    = "SMENH820" and ZMM_FLCM_PARMS-ZPARM_NM = "MATKL"*
*                    can process multiple values returned by this      *
*                    selection.  A carrier contract can now be created *
*                    with either MATKL = TRNS9070 or TRNS9079          *
*----------------------------------------------------------------------*

  DATA:
    ls_mseg        TYPE mseg,
    ld_msgv1       TYPE sy-msgv1,
    lv_tabix       TYPE sy-tabix,
    lv_ponumber    TYPE ekko-ebeln.

  FIELD-SYMBOLS:
    <ms_unique_po_start> LIKE LINE OF mt_unique_po,
    <ms_unique_po>       LIKE LINE OF mt_unique_po,
    <mseg>         TYPE mseg.

  rd_result = abap_false.
  clear ms_unique_po.
  clear ls_mseg.
  clear lv_ponumber.

  sort mt_unique_po_car by ebeln descending
                           ebelp descending.

  loop at mt_unique_po_car assigning <ms_unique_po_start>
     where ihrez      EQ is_trans-ztds_bol_nbr
     and   lifnr      EQ is_trans-zcarrier
     and   unsez+4(7) EQ is_trans-ztds_trml_id
     and   loekz      is initial
*     and   matkl      EQ ms_matkl                                "D-XT18775 - DV5K980540- CR227428 TK242984
     AND   matkl      IN mrt_matkl_range                          "I-XT18775 - DV5K980540- CR227428 TK242984
     and   knttp      EQ 'K'
*    and   retpo      IS initial
     and   elikz      IS initial
***Start of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
     AND   eindt      EQ is_trans-ztds_bol_dt.
***End of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
     if lv_ponumber ne <ms_unique_po_start>-ebeln.
        assign <ms_unique_po_start> to <ms_unique_po>.
        lv_ponumber = <ms_unique_po>-ebeln.
        ms_unique_po = <ms_unique_po>.
        lv_tabix = lv_tabix + 1.
     endif.
  endloop.

  if sy-subrc eq 0.
* unique Po
     if <ms_unique_po>-retpo eq abap_false.
        ms_unique_po = <ms_unique_po>.
        rd_result = abap_true.
     endif.
  endif.


  if rd_result = abap_false.
*   Raise exception
    IF ( 1 = 2 ). MESSAGE e008(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '008'
        sub_stus = '12'.
*       msgtab   = et_return.
  endif.




endmethod.


method IS_GR_TRANSPORT_PO_RET_FOUND.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Chari Flor Supino         2017/08/03          DV5K9A0ARX             *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Add delivery date for unique PO combination       *
*----------------------------------------------------------------------*
* Marie Kris Mendoza (XT18775) 2013-07-30        DV5K980540            *
*                                                                      *
* Short Description: CR227428 TK242984                                 *
*                    Make sure all accesses using ZMM_FLCM_PARMS-ZVAL  *
*                    = "SMENH820" and ZMM_FLCM_PARMS-ZPARM_NM = "MATKL"*
*                    can process multiple values returned by this      *
*                    selection.  A carrier contract can now be created *
*                    with either MATKL = TRNS9070 or TRNS9079          *
*----------------------------------------------------------------------*
  DATA:
    ls_mseg        TYPE mseg,
    ld_msgv1       TYPE sy-msgv1,
    lv_tabix       TYPE sy-tabix,
    lv_ponumber    TYPE ekko-ebeln.

  FIELD-SYMBOLS:
    <ms_unique_po_start> LIKE LINE OF mt_unique_po,
    <ms_unique_po>       LIKE LINE OF mt_unique_po,
    <mseg>         TYPE mseg.

  rd_result = abap_false.
  clear ms_unique_po.
  clear ls_mseg.
  clear lv_tabix.
  clear lv_ponumber.

  clear lv_tabix.
  sort mt_unique_po_car by ebeln descending
                           ebelp descending.

  loop at mt_unique_po_car assigning <ms_unique_po_start>
     where ihrez     = is_trans-ztds_bol_nbr
     and   lifnr     = is_trans-zcarrier
     and   unsez+4(7) EQ is_trans-ztds_trml_id
     and   loekz     is initial
*     and   matkl     EQ ms_matkl                                 "D-XT18775 - DV5K980540- CR227428 TK242984
     AND   matkl     IN mrt_matkl_range                           "I-XT18775 - DV5K980540- CR227428 TK242984
     and   knttp     EQ 'K'
     and   elikz     is initial
***Start of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
     AND   eindt      EQ is_trans-ztds_bol_dt.
***End of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
     if lv_ponumber ne <ms_unique_po_start>-ebeln.
        assign <ms_unique_po_start> to <ms_unique_po>.
        lv_ponumber = <ms_unique_po>-ebeln.
        ms_unique_po = <ms_unique_po>.
        lv_tabix = lv_tabix + 1.
     endif.
  endloop.

  if ( sy-subrc ne 0 )  and
* no PO found
     ( is_trans-zcarrier = is_trans-lifnr or
       is_trans-zcarrier IN mrt_zcarrier_np_range ).
        ms_unique_po = <ms_unique_po>.
        rd_result    = abap_true.
  endif.

  if lv_tabix EQ 1.
     rd_result        = abap_true.
  endif.

  if rd_result = abap_false.
*   Raise exception
    IF ( 1 = 2 ). MESSAGE e025(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msgid_flcm
        msgty    = mc_msgty_error
        msgno    = '025'
        sub_stus = '12'.
*       msgtab   = et_return.
  endif.





endmethod.


method IS_SUPPLIER_CARRIER_NOT_EXCL.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Marie Kris Mendoza (XT18775) 2013-07-30        DV5K980540            *
*                                                                      *
* Short Description: CR227428 TK242984                                 *
*                    Make sure all accesses using ZMM_FLCM_PARMS-ZVAL  *
*                    = "SMENH820" and ZMM_FLCM_PARMS-ZPARM_NM = "MATKL"*
*                    can process multiple values returned by this      *
*                    selection.  A carrier contract can now be created *
*                    with either MATKL = TRNS9070 or TRNS9079          *
*----------------------------------------------------------------------*
  DATA:
    lv_tabix            TYPE sy-tabix,
    lv_ponumber         TYPE ekko-ebeln,
    ld_msgv1            TYPE sy-msgv1,
    ld_msgv2            TYPE sy-msgv2.

  FIELD-SYMBOLS:
    <ms_contract>       LIKE LINE OF mt_contract,
    <a016>              LIKE LINE OF mt_a016,
    <zcarrier_np_range> LIKE LINE OF mrt_zcarrier_np_range.

  rd_result = abap_false.
  clear lv_tabix.

  check is_trans-zcarrier not in mrt_zcarrier_np_range.

* see how many contracts exist they must be of type litres or gallons otherwise we exlude them and then we would
* know that we have to create a po if there are many then we have to see if there is a delivery address if there is
* none then we still have to create a po

  clear lv_ponumber.
  clear ms_contract.

  loop at mt_contract assigning <ms_contract>
    where lifnr EQ is_trans-zcarrier
    and   bsart EQ 'FK'
    and   kdatb LE is_trans-ztds_bol_dt
    and   kdate GE is_trans-ztds_bol_dt
*    and   matkl EQ ms_matkl                                      "D-XT18775 - DV5K980540- CR227428 TK242984
    AND   matkl IN mrt_matkl_range                                "I-XT18775 - DV5K980540- CR227428 TK242984
    and   knttp EQ 'K'
    and   loekz is initial
    and   werks EQ is_trans-werks.
    lv_tabix = lv_tabix + 1.
  endloop.

* unique contract then po needs to be created
  if sy-subrc EQ 0.
     if lv_tabix GT 1.
        clear lv_ponumber.
        clear lv_tabix.
        loop at mt_contract assigning <ms_contract>
          where lifnr EQ is_trans-zcarrier
          and   bsart EQ 'FK'
          and   kdatb LE is_trans-ztds_bol_dt
          and   kdate GE is_trans-ztds_bol_dt
*          and   matkl EQ ms_matkl                                "D-XT18775 - DV5K980540- CR227428 TK242984
          AND   matkl IN mrt_matkl_range                          "I-XT18775 - DV5K980540- CR227428 TK242984
          and   knttp EQ 'K'
          and   loekz is initial
          and   werks EQ is_trans-werks
          and   sort1 EQ is_trans-ztds_trml_id.
          lv_tabix = lv_tabix + 1.
        endloop.

        if lv_tabix EQ 1.
           ms_contract = <ms_contract>.
           rd_result   = abap_true.
        endif.
     else.
       ms_contract = <ms_contract>.
       rd_result   = abap_true.
     endif.
  endif.
* see if contract pricing is valid.
  IF ( rd_result = abap_true ).
     loop at mt_a016 assigning <a016>
        where datbi ge is_trans-ztds_bol_dt
        and   datab le is_trans-ztds_bol_dt.
        exit.
     endloop.
     IF sy-subrc NE 0.
        rd_result = abap_false.
     ENDIF.
  ENDIF.

  IF ( rd_result = abap_false ).
    IF lv_tabix GT 1.
*   Raise exception
       IF ( 1 = 2 ). MESSAGE e051(zz_flcm). ENDIF.
       RAISE EXCEPTION TYPE zcx_flcm_error
         EXPORTING
           msgid    = mc_msgid_flcm
           msgty    = mc_msgty_error
           msgno    = '051'
           sub_stus = '10'.
*             msgtab   = et_return.
    ELSEIF ms_contract is not initial.
*   Raise exception
       IF ( 1 = 2 ). MESSAGE e081(zz_flcm). ENDIF.
       ld_msgv1 = ms_contract-ebeln.
       ld_msgv2 = ms_contract-ebelp.
       RAISE EXCEPTION TYPE zcx_flcm_error
         EXPORTING
           msgid    = mc_msgid_flcm
           msgty    = mc_msgty_error
           msgv1    = ld_msgv1
           msgv2    = ld_msgv2
           msgno    = '81'
           sub_stus = '10'.
*             msgtab   = et_return.
    ELSE.
*   Raise exception
       IF ( 1 = 2 ). MESSAGE e088(zz_flcm). ENDIF.
       RAISE EXCEPTION TYPE zcx_flcm_error
         EXPORTING
           msgid    = mc_msgid_flcm
           msgty    = mc_msgty_error
           msgno    = '088'
           sub_stus = '10'.
*             msgtab   = et_return.
    ENDIF.


  ENDIF.


endmethod.


method IS_TRANSPORT_PO_FOUND.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Chari Flor Supino         2017/08/03          DV5K9A0ARX             *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Add delivery date for unique PO combination       *
*----------------------------------------------------------------------*
* Marie Kris Mendoza (XT18775) 2013-07-30        DV5K980540            *
*                                                                      *
* Short Description: CR227428 TK242984                                 *
*                    Make sure all accesses using ZMM_FLCM_PARMS-ZVAL  *
*                    = "SMENH820" and ZMM_FLCM_PARMS-ZPARM_NM = "MATKL"*
*                    can process multiple values returned by this      *
*                    selection.  A carrier contract can now be created *
*                    with either MATKL = TRNS9070 or TRNS9079          *
*----------------------------------------------------------------------*
  DATA:
    lv_tabix       TYPE sy-tabix,
    lv_ponumber    TYPE ekko-ebeln,
    ls_cancel      TYPE zmm_flcm_tds,
    ld_msgv1       TYPE sy-msgv1.


  FIELD-SYMBOLS:
    <ms_unique_po_start> LIKE LINE OF mt_unique_po,
    <ms_unique_po>       LIKE LINE OF mt_unique_po.

  rd_result = abap_false.
  clear ms_unique_po.
  clear lv_tabix.
  clear lv_ponumber.


  sort mt_trans_cancel by ztds_tran_ref_nb descending
                          werks
                          ztds_trml_id
                          ztds_bol_nbr
                          ztds_canc_rbil.

  clear ls_cancel.
* Get the line with CANCEL
  loop at mt_trans_cancel INTO ls_cancel
       WHERE ztds_tran_ref_nb  lt is_trans-ztds_tran_ref_nb
       and   ztds_trml_id       = is_trans-ztds_trml_id
       and   ztds_bol_nbr      = is_trans-ztds_bol_nbr
       and   ztds_canc_rbil    = mc_ind_cancel.
    exit.
  endloop.

  sort mt_unique_po_car by ebeln descending
                           ebelp descending.


  loop at mt_unique_po_car assigning <ms_unique_po_start>
     where ihrez      EQ is_trans-ztds_bol_nbr
     and   lifnr      EQ is_trans-zcarrier
     and   unsez+4(7) EQ is_trans-ztds_trml_id
     and   loekz      is initial
*     and   matkl      EQ ms_matkl                                "D-XT18775 - DV5K980540- CR227428 TK242984
     AND   matkl      IN mrt_matkl_range                          "I-XT18775 - DV5K980540- CR227428 TK242984
     and   knttp      EQ 'K'
***Start of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
     AND   eindt      EQ is_trans-ztds_bol_dt.
***End of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
     if lv_ponumber ne <ms_unique_po_start>-ebeln.
        assign <ms_unique_po_start> to <ms_unique_po>.
        lv_ponumber = <ms_unique_po>-ebeln.
        lv_tabix = lv_tabix + 1.
     endif.
  endloop.


  IF lv_tabix GT 1.
*   Error: Multiple POs found
    IF ( 1 = 2 ). MESSAGE e018(zz_flcm). ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error
     EXPORTING
        msgid    = mc_msgid_flcm    "ZZ_FLCM
        msgty    = mc_msgty_error "E
        msgno    = '018'          "018:Multiple Po found
        sub_stus = '10'.
*       msgtab   = et_return.
  endif.

* PO found or not
 if lv_tabix EQ 1.
* unique po
    ms_unique_po = <ms_unique_po>.
    rd_result = abap_true.
 endif.

 CASE is_trans-ztds_canc_rbil.

    WHEN mc_ind_blank.

     if rd_result = abap_true.
* when the status is blank there there must be no po found otherwise this is an error
        ld_msgv1 = ms_unique_po-ebeln.
*   Raise exception
        IF ( 1 = 2 ). MESSAGE e002(zz_flcm). ENDIF.
        RAISE EXCEPTION TYPE zcx_flcm_error
          EXPORTING
            msgid    = mc_msgid_flcm
            msgty    = mc_msgty_error
            msgv1 = ld_msgv1
            msgno    = '002'
            sub_stus = '10'.
*           msgtab   = et_return.
     endif.

    WHEN mc_ind_rebill.

     if rd_result = abap_true.
        if ms_unique_po-retpo = abap_false.
* since there is a PO found
           if ( ls_cancel-zcarrier EQ is_trans-zcarrier or
                ls_cancel-zcarrier IS initial ).
*   Raise exception
              IF ( 1 = 2 ). MESSAGE e070(zz_flcm). ENDIF.
               RAISE EXCEPTION TYPE zcx_flcm_error
                 EXPORTING
                   msgid    = mc_msgid_flcm
                   msgty    = mc_msgty_error
                   msgno    = '070'
                   sub_stus = '10'.
*                  msgtab   = et_return.
           ELSE.
              clear ms_unique_po.
           ENDIF.
        ENDIF.
     else.
       if ( is_trans-zcarrier NOT IN mrt_zcarrier_np_range ).
          if ( ls_cancel-zcarrier EQ is_trans-zcarrier or
               ls_cancel-zcarrier IS initial ).
*   Raise exception
               IF ( 1 = 2 ). MESSAGE e070(zz_flcm). ENDIF.
               RAISE EXCEPTION TYPE zcx_flcm_error
                 EXPORTING
                   msgid    = mc_msgid_flcm
                   msgty    = mc_msgty_error
                   msgno    = '070'
                   sub_stus = '10'.
*                  msgtab   = et_return
          endif.
       endif.
     endif.

    WHEN mc_ind_cancel.

* cancel
     if rd_result = abap_true.
        if ms_unique_po-retpo = abap_true.
*   Raise exception
           IF ( 1 = 2 ). MESSAGE e086(zz_flcm). ENDIF.
           RAISE EXCEPTION TYPE zcx_flcm_error
             EXPORTING
               msgid    = mc_msgid_flcm
               msgty    = mc_msgty_error
               msgno    = '086'
               sub_stus = '10'.
*              msgtab   = et_return
        else.
           if ( is_trans-zcarrier IN mrt_zcarrier_np_range ).
*   Raise exception
             IF ( 1 = 2 ). MESSAGE e025(zz_flcm). ENDIF.
             RAISE EXCEPTION TYPE zcx_flcm_error
               EXPORTING
                 msgid    = mc_msgid_flcm
                 msgty    = mc_msgty_error
                 msgno    = '025'
                 sub_stus = '10'.
*                msgtab   = et_return.
           endif.
        endif.
     else.
       if ( is_trans-zcarrier NOT IN mrt_zcarrier_np_range ).
          IF ( 1 = 2 ). MESSAGE e025(zz_flcm). ENDIF.
          RAISE EXCEPTION TYPE zcx_flcm_error
            EXPORTING
              msgid    = mc_msgid_flcm
              msgty    = mc_msgty_error
              msgno    = '025'
              sub_stus = '10'.
*             msgtab   = et_return.
       endif.
     endif.

    WHEN others.

 ENDCASE.



endmethod.


method RETREIVE_NEW_PO.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Chari Flor Supino         2019/08/30          DV5K9A0ARX             *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Uncomment select from EKET to get the EINDT       *
*----------------------------------------------------------------------*

  DATA:
    lt_trans               TYPE standard table of zmm_flcm_tds.

* find transport po's for CAR & TDS
  FIELD-SYMBOLS:
     <trans>               TYPE zmm_flcm_tds,
     <ms_unique_po>        TYPE zmm_flcm_unique_po.

  refresh lt_trans.
  append is_trans to lt_trans.


  SELECT a~ebeln
         b~ebelp
         a~ihrez
         a~lifnr
*         a~unsez                                          "DV5K9A05WW-
         b~loekz
         b~matkl
         b~matnr
         b~knttp
         b~retpo
         b~bstae
         b~bednr
         b~menge
         b~meins
         c~eindt
   FROM ekko AS a
   INNER JOIN ekpo AS b ON
   b~ebeln = a~ebeln
   INNER JOIN eket AS c ON
   c~ebeln = b~ebeln AND
   c~ebelp = b~ebelp
   APPENDING CORRESPONDING FIELDS OF TABLE  mt_unique_po   "DV5K9A05WW+
    WHERE a~ebeln = id_ebeln.
*   WHERE a~lifnr EQ lt_trans-zcarrier
*   AND   b~loekz EQ ' '
*   AND   ( ( b~matnr EQ lt_trans-matnr
*   AND     b~knttp EQ ' '     )
*   OR    ( b~matkl EQ ms_matkl
*   AND   b~knttp EQ 'K') ).


  loop at mt_unique_po assigning <ms_unique_po>
    where ebeln = id_ebeln.
    <ms_unique_po>-unsez = get_header_text( id_ebeln ).    "DV5K9A05WW+
* if its for CAR then append it to the mt_unique_po_car
* otherwise append it to the mt_unique_po_tds
    if <ms_unique_po>-unsez+0(3) = mc_car.
       append <ms_unique_po> to mt_unique_po_car.
    elseif <ms_unique_po>-unsez+0(3) = mc_tds.
       append <ms_unique_po> to mt_unique_po_tds.
    endif.

  endloop.


endmethod.


method RETREIVE_NEW_PO_LINE.


  DATA:
    lt_trans               TYPE standard table of zmm_flcm_tds,
    lt_items               TYPE BAPIEKPO_TP,
    ls_items               TYPE BAPIEKPO,
    lt_schedules           TYPE BAPIEKET_TP.

  DATA:
    rt_return              TYPE BAPIRET2TAB.

* find transport po's for CAR & TDS
  FIELD-SYMBOLS:
     <trans>               TYPE zmm_flcm_tds,
     <ms_unique_po>        TYPE zmm_flcm_unique_po.

  refresh lt_items.

* Call po_getdetail  bapi
  TRY.
    CALL METHOD ZCL_FLCM_SERVICES=>GET_PO_DETAIL
      EXPORTING
        ID_EBELN     = ms_unique_po-ebeln
       IMPORTING
         ET_ITEMS     = lt_items
         ET_SCHEDULES = lt_schedules
         ET_RETURN    = rt_return.
    CATCH zcx_flcm_error INTO DATA(lo_flcm_exception).
  ENDTRY.

  sort lt_items by po_item descending.

  loop at lt_items into ls_items.
    exit.
  endloop.

  clear ms_unique_po-elikz.
  move ls_items-po_item  to ms_unique_po-ebelp.
  move ls_items-ret_item to ms_unique_po-retpo.
  append ms_unique_po to mt_unique_po.

  if ms_unique_po-unsez+0(3) = mc_car.
     append ms_unique_po to mt_unique_po_car.
  elseif ms_unique_po-unsez+0(3) = mc_tds.
     append ms_unique_po to mt_unique_po_tds.
  endif.


  if sy-subrc ne 0.
     IF ( 1 = 2 ). MESSAGE e025(zz_flcm). ENDIF.
     RAISE EXCEPTION TYPE zcx_flcm_error
       EXPORTING
         msgid    = mc_msgid_flcm "ZZ_FLCM
         msgty    = mc_msgty_error         "E
         msgno    = '025'          "025:Original Po not found
         sub_stus = '000'.         "000: &1 &2 &3 &4
*                   msgtab   = et_return.
  endif.

endmethod.
ENDCLASS.
