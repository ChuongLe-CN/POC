class ZCL_FLCM_TRX_PROC_BOL definition
  public
  inheriting from ZCL_FLCM_TRX
  final
  create public .

*"* public components of class ZCL_FLCM_TRX_PROC_BOL
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  constants MC_LITRES type EKPO-MEINS value 'L' ##NO_TEXT.
  constants MC_GALLONS type EKPO-MEINS value 'GLL' ##NO_TEXT.
  constants MC_PROGNAME_819 type ZMM_FLCM_PARMS-PROGNAME value 'SMENH819' ##NO_TEXT.
  constants MC_ZPARM_CONF_CNTRL type ZMM_FLCM_PARMS-ZPARM_NM value 'CONF_CTRL' ##NO_TEXT.
  constants MC_BOL type ZMM_FLCM_TDS-ZTDS_TRAN_TYPE value '502' ##NO_TEXT.
  constants MC_EQUAL type ACE_GENERIC_RANGE-OPTION value 'EQ' ##NO_TEXT.
  constants MC_ERR type BAPIRET2-TYPE value 'E' ##NO_TEXT.
  constants MC_GMCODE type BAPI2017_GM_CODE value '01' ##NO_TEXT.
  constants MC_FOR_DELETION type EKPO-LOEKZ value 'L' ##NO_TEXT.
  constants MC_FUEL_CONTRACT type EKKO-BSART value 'FK' ##NO_TEXT.
  constants MC_FUEL_PO type EKKO-BSART value 'FB' ##NO_TEXT.
  constants MC_INCLUDE type ACE_GENERIC_RANGE-SIGN value 'I' ##NO_TEXT.
  constants MC_KIND_STRUCT type ABAP_TYPECATEGORY value 'S' ##NO_TEXT.
  constants MC_KIND_TABLE type ABAP_TYPECATEGORY value 'T' ##NO_TEXT.
  constants MC_MSG_CL_FLCM type BAPIRET2-ID value 'ZZ_FLCM' ##NO_TEXT.
  constants MC_MSG_NO_000 type BAPIRET2-NUMBER value '000' ##NO_TEXT.
  constants MC_MSG_NO_017 type BAPIRET2-NUMBER value '017' ##NO_TEXT.
  constants MC_MSG_NO_018 type BAPIRET2-NUMBER value '018' ##NO_TEXT.
  constants MC_MSG_NO_024 type BAPIRET2-NUMBER value '024' ##NO_TEXT.
  constants MC_MSG_NO_051 type BAPIRET2-NUMBER value '051' ##NO_TEXT.
  constants MC_MVT_IND_PO type MSEG-KZBEW value 'B' ##NO_TEXT.
  constants MC_PREFIX type CHAR04 value 'TDS-' ##NO_TEXT.
  constants MC_PROGNAME type ZMM_FLCM_PARMS-PROGNAME value 'SMENH819' ##NO_TEXT.
  constants MC_TDS_TABLE_NAME type STRING value 'ZMM_FLCM_TDS' ##NO_TEXT.
  constants MC_TRACKINGNO_GROSS type EKPO-BEDNR value 'GROSS' ##NO_TEXT.
  constants MC_ZPARM_NM type ZMM_FLCM_PARMS-ZPARM_NM value 'CONF_CTRL' ##NO_TEXT.
  constants MC_10 type I value 10 ##NO_TEXT.
  data MS_CONF_CNTRL type ZMM_FLCM_PARMS-ZVAL_FROM .
  data MS_CONTRACT type ZMM_FLCM_CONTRACT .
  data MS_UNIQUE_PO type ZMM_FLCM_UNIQUE_PO .
  data MT_CONTRACT type ZMM_FLCM_CONTRACT_TP .
  data MT_TRANS_CANCEL type ZMM_FLCM_TDS_T .
  data MT_UNIQUE_PO type ZMM_FLCM_UNIQUE_PO_TP .
  data MT_GR_NOT_COMPLETED type ZMM_FLCM_GR_NOT_COMPLETED_TP .

  methods CONSTRUCTOR
    importing
      !IT_TRANS type ZMM_FLCM_TDS_T
    raising
      ZCX_FLCM_ERROR .
  methods RETREIVE_NEW_PO
    importing
      !IS_TRANS type ZMM_FLCM_TDS
      !ID_EBELN type EKKO-EBELN
    raising
      ZCX_FLCM_ERROR .
  methods RETREIVE_NEW_PO_LINE
    raising
      ZCX_FLCM_ERROR .

  methods EXECUTE
    redefinition .
protected section.
*"* protected components of class ZCL_FLCM_TRX_PROC_BOL
*"* do not include other source files here!!!
PRIVATE SECTION.
*"* private components of class ZCL_FLCM_TRX_PROC_BOL
*"* do not include other source files here!!!

  TYPES:
*--> Start of insert DV5K9A05WW - Chuong Le - 2017/01/04
    BEGIN OF text_type,
      ebeln  TYPE ebeln,
      tdline TYPE tdline,
    END OF text_type .
  DATA:
    mt_texttab TYPE SORTED TABLE OF text_type WITH UNIQUE KEY ebeln .
*<-- End of insert DV5K9A05WW - Chuong Le - 2017/01/04

  DATA:
    mt_ekko TYPE STANDARD TABLE OF t_ekko .
  DATA:
    mt_ekpo TYPE STANDARD TABLE OF t_ekpo .
  DATA mt_a016 TYPE mmpur_t_a016 .


  METHODS search_po
    IMPORTING
      !is_trans          TYPE zmm_flcm_tds
    EXPORTING
      VALUE(ev_ponumber) TYPE ekko-ebeln
      VALUE(ev_poitem)   TYPE ekpo-ebelp
      !ev_elikz          TYPE ekpo-elikz
      VALUE(ev_pocount)  TYPE i
      VALUE(et_return)   TYPE bapiret2tab
    RAISING
      zcx_flcm_error .
  METHODS check_cancel_rebill
    IMPORTING
      !is_trans        TYPE zmm_flcm_tds
    EXPORTING
      VALUE(es_cancel) TYPE zmm_flcm_tds
      VALUE(es_rebill) TYPE zmm_flcm_tds .
  METHODS search_contract
    IMPORTING
      !is_trans               TYPE zmm_flcm_tds
    EXPORTING
      VALUE(ev_contract)      TYPE konnr
      VALUE(ev_contract_item) TYPE ktpnr
      VALUE(et_return)        TYPE bapiret2tab
    RAISING
      zcx_flcm_error .
  METHODS update_gr_elikz_flag
    IMPORTING
      !is_unique_po TYPE zmm_flcm_unique_po .
  METHODS get_header_text
    IMPORTING
      !id_ebeln        TYPE ebeln
    RETURNING
      VALUE(rd_result) TYPE string .
ENDCLASS.



CLASS ZCL_FLCM_TRX_PROC_BOL IMPLEMENTATION.


METHOD check_cancel_rebill.
  CLEAR: es_cancel,
         es_rebill.
* Get the line with CANCEL
  sort mt_trans_cancel by ztds_tran_ref_nb descending
                          werks
                          ztds_trml_id
                          ztds_bol_nbr
                          ztds_canc_rbil.


  loop at mt_trans_cancel INTO es_cancel
       WHERE ztds_tran_ref_nb  lt is_trans-ztds_tran_ref_nb
       AND   werks             = is_trans-werks
       AND   ztds_trml_id      = is_trans-ztds_trml_id
       AND   ztds_bol_nbr      = is_trans-ztds_bol_nbr
       AND   ztds_canc_rbil    = mc_ind_cancel.
   exit.
  endloop.
  IF sy-subrc NE 0.
    SELECT *
      FROM zmm_flcm_tds
      APPENDING TABLE mt_trans_cancel
      FOR ALL ENTRIES IN mt_trans
     WHERE ztds_tran_type   EQ mt_trans-ztds_tran_type
     AND werks              EQ mt_trans-werks
     AND ztds_canc_rbil     EQ mc_ind_cancel.
    sort mt_trans_cancel by ztds_tran_ref_nb descending
                            werks
                            ztds_trml_id
                            ztds_bol_nbr
                            ztds_canc_rbil.
     loop at mt_trans_cancel INTO es_cancel
       WHERE ztds_tran_ref_nb  lt is_trans-ztds_tran_ref_nb
        and   werks          = is_trans-werks
        and   ztds_trml_id   = is_trans-ztds_trml_id
        and   ztds_bol_nbr   = is_trans-ztds_bol_nbr
        and   ztds_canc_rbil = mc_ind_cancel.
        exit.
     endloop.
    IF sy-subrc NE 0.
       CLEAR: es_cancel.
    ENDIF.
  ENDIF.
* Get the line with REBILL
  READ TABLE mt_trans INTO es_rebill
       WITH KEY werks          = is_trans-werks
                ztds_trml_id   = is_trans-ztds_trml_id
                ztds_bol_nbr   = is_trans-ztds_bol_nbr
                ztds_canc_rbil = mc_ind_rebill.
  IF sy-subrc NE 0.
    CLEAR: es_rebill.
  ENDIF.
ENDMETHOD.


METHOD constructor.
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by             Date                Tracking number *
* -------------------------        ----------          --------------- *
* Chari Flor Supino                2017/08/03          DV5K9A0ARX      *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Add delivery date for unique PO combination       *
*----------------------------------------------------------------------*
* Chuong Le  CR306321-T347799      2017/01/04          DV5K9A05WW      *
*                                                                      *
*    - Read Our Reference from PO header text instead of EKKO-UNSEZ.   *
*----------------------------------------------------------------------*
* Von Kaisser Almeria              2016-07-28          DV5K9A03CO      *
*                                                                      *
* Short Description : CR269546 TK334189                                *
*                     Add controls to avoid creating POs for           *
*                     unreasonable amounts                             *
*----------------------------------------------------------------------*


  TYPES: begin of t_adrc,
          addrnumber   TYPE adrc-addrnumber,
          sort1        TYPE adrc-sort1,
         end of t_adrc.


  DATA:
     lv_progname       TYPE char50,
     lv_zparm_nm1      TYPE char50.



  DATA: lr_ihrez       TYPE RANGE OF ekko-ihrez,
        lwa_ihrez      LIKE LINE OF lr_ihrez,
        lr_unsez       TYPE RANGE OF ekko-unsez,
        lt_adrc        TYPE STANDARD TABLE OF t_adrc,
        lwa_unsez      LIKE LINE OF lr_unsez,
        ls_contract    TYPE zmm_flcm_contract,
        ld_index       TYPE sy-index.


  FIELD-SYMBOLS:
        <trans> like LINE OF mt_trans,
        <adrc>             TYPE t_adrc,
        <contract>         TYPE zmm_flcm_contract,
        <ms_unique_po>     TYPE zmm_flcm_unique_po.



* Get data from ZMM_FLCM_TDS
  super->constructor( it_trans ).
* Build selection criteria for purchase order data
  REFRESH: lr_ihrez,
           lr_unsez,
           mt_trans_cancel.
  CLEAR:   lwa_ihrez,
           lwa_unsez.
  UNASSIGN: <trans>.

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
          msgid = mc_msg_cl_flcm
          msgty = mc_err
          msgv1 = lv_progname
          msgv2 = lv_zparm_nm1
          msgno = '044'.
  ENDIF.


  lwa_ihrez-sign   = lwa_unsez-sign   = mc_include. "I
  lwa_ihrez-option = lwa_unsez-option = mc_equal.   "EQ
  LOOP AT mt_trans ASSIGNING <trans>.
    MOVE <trans>-ztds_bol_nbr TO lwa_ihrez-low.
    CONCATENATE mc_prefix                            "TDS-
                <trans>-ztds_trml_id
           INTO lwa_unsez-low.
    APPEND: lwa_ihrez TO lr_ihrez,
            lwa_unsez TO lr_unsez.
*   NOTE: ONLY CLEAR THE -LOW FOR IHREZ AND UNSEZ AS SIGN AND
*         OPTION WILL REMAIN TO BE 'I' AND 'EQ' RESPECTIVELY
    CLEAR:  lwa_ihrez-low,
            lwa_unsez-low.
  ENDLOOP. "of LOOP AT mt_trans ASSIGNING <trans>

* find transport po's for TDS

  IF mt_trans[] is not initial.

    SELECT b~ebeln
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
     b~ebeln = c~ebeln AND
     b~ebelp = c~ebelp
***End of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
     INTO CORRESPONDING FIELDS OF TABLE  mt_unique_po      "DV5K9A05WW+
     FOR ALL ENTRIES IN mt_trans
     WHERE  a~bsart EQ mc_fuel_po
     AND    a~lifnr EQ mt_trans-lifnr.
*    AND    b~matnr EQ mt_trans-matnr
*    AND    b~knttp EQ ' ' )
*    AND    b~loekz EQ ' '.

* Make sure transactions are in order of creation
    SORT mt_trans BY ztds_tran_ref_nb.

*    delete mt_unique_po where unsez not in lr_unsez.      "DV5K9A05WW-

    delete mt_unique_po where ihrez not in lr_ihrez.

    delete mt_unique_po where knttp NE ' '.

    delete mt_unique_po where loekz NE ' '.

    loop at mt_unique_po assigning <ms_unique_po>.
      ld_index = sy-tabix.
      read table mt_trans assigning <trans>
         with key lifnr        = <ms_unique_po>-lifnr
                  ztds_bol_nbr = <ms_unique_po>-ihrez.

      if sy-subrc NE 0.
         delete mt_unique_po index ld_index.
      elseif <trans>-matnr ne <ms_unique_po>-matnr.
         delete mt_unique_po index ld_index.
***Start of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
      ELSEIF <trans>-ztds_tran_dt NE <ms_unique_po>-eindt.
         DELETE mt_unique_po INDEX ld_index.
***End of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
      endif.

    endloop.

*--> Start of Insert DV5K9A05WW - Chuong Le - 2017/01/04
    loop at mt_unique_po assigning <ms_unique_po>.
      <ms_unique_po>-unsez = get_header_text( <ms_unique_po>-ebeln ).
    endloop.
    delete mt_unique_po where unsez not in lr_unsez.
*<-- End of Insert DV5K9A05WW - Chuong Le - 2017/01/04

*Begin DV5K966159NB 2011-08-30
*Extract all PO number where the Good Receipt in not completed.
    IF mt_unique_po[] IS NOT INITIAL.
      SELECT ebeln
             ebelp
             vgabe
      FROM ekbe
      INTO TABLE  mt_gr_not_completed    "mt_unique_po
      FOR ALL ENTRIES IN mt_unique_po
      WHERE  ebeln EQ mt_unique_po-ebeln
      AND    ebelp EQ mt_unique_po-ebelp
      AND    vgabe NE '1'.        "not completed.
    ENDIF.
*End DV5K966159NB 2011-08-30

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
           a~waers "I - XT18912 - CR269546 TK334189 - DV5K9A03CO
*          d~sort1
     FROM ekko AS a
     INNER JOIN ekpo AS b ON
     b~ebeln = a~ebeln
     INTO TABLE  mt_contract
     FOR ALL ENTRIES IN mt_trans
     WHERE a~lifnr EQ mt_trans-lifnr
     AND   a~bsart EQ mc_fuel_contract
     AND   a~kdatb LE mt_trans-ztds_tran_dt
     AND   a~kdate GE mt_trans-ztds_tran_dt
     AND   b~matnr eq mt_trans-matnr
     AND   b~knttp EQ ' '
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


     endif.
  endif.
ENDMETHOD.


METHOD execute.

  DATA:
    lo_flcm_exception   TYPE REF TO zcx_flcm_error.

  DATA:
    lt_msg              TYPE bapiret2_tab.


  DATA:
*       Internal tables
        li_return  TYPE bapiret2tab,
        lt_return  TYPE bapiret2tab,
*       Work areas
        lwa_ekko      LIKE LINE OF mt_ekko,
        lwa_ekpo      LIKE LINE OF mt_ekpo,
        lwa_return    TYPE bapiret2,
*       Variables
        lv_ponumber       TYPE ekko-ebeln,
        lv_poitem         TYPE ekpo-ebelp,
        lv_elikz          TYPE ekpo-elikz,
        lv_pocount        TYPE i,
        lv_po_change_type TYPE i,
        lv_contract       TYPE ekko-konnr,
        lv_contract_item  TYPE ekpo-ktpnr,
        lv_mblnr          TYPE mseg-mblnr,
        lv_msgv1          TYPE sy-msgv1.
*       Used for Purchase Order related BAPIs
  DATA: lwa_poheader      TYPE bapimepoheader,
        lwa_poheaderx     TYPE bapimepoheaderx,
        li_poitem         TYPE bapimepoitem_tp,
        li_poitemx        TYPE bapimepoitemx_tp,
        li_poschedule     TYPE bapimeposchedule_tp,
        li_poschedulex    TYPE bapimeposchedulx_tp,
        lo_purchase_order TYPE REF TO zcl_flcm_po.
*       Used for Goods Movement related BAPIs
  DATA: lwa_goodsmvt_header TYPE bapi2017_gm_head_01,
        li_goodsmvt_item    TYPE STANDARD TABLE OF bapi2017_gm_item_create,
        lv_mat_doc          TYPE bapi2017_gm_head_ret-mat_doc,
        lv_doc_year         TYPE bapi2017_gm_head_ret-doc_year,
        lv_ebeln            TYPE ekko-ebeln.
*       Used for update ZMM_FLCM_TDS table related logic

  DATA: lo_exception TYPE REF TO zcx_flcm_error.
  FIELD-SYMBOLS: <trans> LIKE LINE OF mt_trans.
* Call super class
  super->execute( ).
* Error in super call? exit now!
  IF ( is_error( ) = abap_true ).
    RETURN.
  ENDIF.

  UNASSIGN <trans>.
  SORT mt_trans BY ztds_tran_ref_nb.

  LOOP AT mt_trans ASSIGNING <trans>.

*   Only process transaction ready for process or processed in error
    CHECK ( <trans>-ztran_stus = zcl_flcm_services=>mc_status_ready )
       OR ( <trans>-ztran_stus = zcl_flcm_services=>mc_status_error ).

*   Check if the period is open
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
*   Search PO
      CLEAR ms_unique_po.
      REFRESH li_return.
      REFRESH lt_return.
* STEP 5.2
      me->search_po(
        EXPORTING
          is_trans    = <trans>
        IMPORTING
          ev_ponumber = lv_ponumber
          ev_poitem   = lv_poitem
          ev_elikz    = lv_elikz          ev_pocount  = lv_pocount
          et_return   = li_return
      ).

* STEP 5.5
      me->search_contract(
        EXPORTING
          is_trans         = <trans>
        IMPORTING
          ev_contract      = lv_contract
          ev_contract_item = lv_contract_item
          et_return        = li_return
      ).
* depending on the type of record we are processing we will search for the po and decide after what it is we want to do
* if we are searching it is because there is not po and we need to create one
      IF ( ms_unique_po is initial ).
* STEP 5.6
         call method zcl_flcm_services=>create_new_tds_po(
                            EXPORTING is_trans        = <trans>
                                      is_contract     = ms_contract
                                      id_conf_ctrl    = ms_conf_cntrl
                            IMPORTING et_return       = rt_return
                                      ed_ebeln        = lv_ebeln               ).


         lt_return[] = rt_return[].
* we need to add the po to the attribute mt_unique_po
         CALL METHOD me->RETREIVE_NEW_PO( is_trans = <trans>
                                          id_ebeln = lv_ebeln ).

         rt_return =
         zcl_flcm_services=>create_goods_mvmt_tds_po( is_trans     = <trans>
                                                      is_unique_po = ms_unique_po ).
         append lines of rt_return to lt_return.
* we need to update the GR flag in the attribute for further processing
         me->update_gr_elikz_flag( ms_unique_po ).
      ELSE.
* Cancel
         IF ( <trans>-ztds_canc_rbil EQ mc_ind_cancel and
              ms_unique_po-retpo = abap_false ).
* STEP 5.7
              rt_return =
              zcl_flcm_services=>update_po( is_trans     = <trans>
                                            id_ebeln     = ms_unique_po-ebeln
                                            id_ebelp     = ms_unique_po-ebelp
                                            id_sub_stus  = '00' ).
              lt_return[] = rt_return[].
* we need to add the po to the attribute mt_unique_po
              me->RETREIVE_NEW_PO_LINE( ).

* rebill
         ELSEIF ( <trans>-ztds_canc_rbil EQ mc_ind_rebill and
                  ms_unique_po-retpo = abap_true ).

* STEP 5.7
                  rt_return =
                  zcl_flcm_services=>reverse_po( is_trans     = <trans>
                                                 is_contract  = ms_contract
                                                 is_unique_po = ms_unique_po
                                                 id_sub_stus  = '00'
                                                 id_conf_ctrl = ms_conf_cntrl ).
                  lt_return[] = rt_return[].
* we need to add the po to the attribute mt_unique_po
                  me->RETREIVE_NEW_PO_LINE( ).

         ENDIF.
* there may be times where we only want to do a GR since when the original one was done the GR failed in the
* search po this will be determined
         rt_return =
         zcl_flcm_services=>create_goods_mvmt_tds_po( is_trans     = <trans>
                                                      is_unique_po = ms_unique_po ).
         append lines of rt_return to lt_return.
* we need to update the GR flag in the attribute for further processing
         me->update_gr_elikz_flag( ms_unique_po ).

      ENDIF.

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
      CATCH zcx_flcm_error INTO lo_flcm_exception.

*       Status update failed, return the error message
        lt_msg = lo_flcm_exception->get_bapimsg_table( ).
        APPEND LINES OF lt_msg TO rt_return.

      ENDTRY.

*  ======
    ENDTRY.
*  ======

  ENDLOOP.
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


method RETREIVE_NEW_PO.
*---------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)             *
*---------------------------------------------------------------------*
* Changed / Created by              Date              Tracking number *
* ---------------------------      ----------         --------------- *
* Chari Flor Supino                2017/08/03         DV5K9A0ARX      *
*                                                                     *
* Short Description: CR302137 TK365122                                *
*                    Add delivery date for unique PO combination      *
*---------------------------------------------------------------------*
* Chuong Le  CR306321-T347799       2017/01/04        DV5K9A05WW      *
*                                                                     *
*    - Read Our Reference from PO header text instead of EKKO-UNSEZ.  *
*---------------------------------------------------------------------*

  DATA:
    lt_trans               TYPE standard table of zmm_flcm_tds,
    lt_unique_po           TYPE standard table of ZMM_FLCM_UNIQUE_PO.


  DATA: lr_ihrez       TYPE RANGE OF ekko-ihrez,
        lwa_ihrez      LIKE LINE OF lr_ihrez,
        lr_unsez       TYPE RANGE OF ekko-unsez,
        lwa_unsez      LIKE LINE OF lr_unsez,
        ld_index       TYPE sy-index,
        lv_ponumber    TYPE ekko-ebeln.


* find transport po's for CAR & TDS
  FIELD-SYMBOLS:
     <trans>               TYPE zmm_flcm_tds,
     <ms_unique_po>        TYPE zmm_flcm_unique_po.


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
   APPENDING CORRESPONDING FIELDS OF TABLE  mt_unique_po   "DV5K9A05WW+
   WHERE  a~ebeln EQ id_ebeln.

*--> Start of Insert DV5K9A05WW - Chuong Le - 2017/01/04
  loop at mt_unique_po assigning <ms_unique_po> where ebeln = id_ebeln.
    <ms_unique_po>-unsez = get_header_text( <ms_unique_po>-ebeln ).
  endloop.
*<-- End of Insert DV5K9A05WW - Chuong Le - 2017/01/04

  clear lv_ponumber.

  loop at mt_unique_po assigning <ms_unique_po>
     where ebeln = id_ebeln.
     ms_unique_po = <ms_unique_po>.
     lv_ponumber = <ms_unique_po>-ebeln.
     exit.
  endloop.

  if sy-subrc ne 0.
     IF ( 1 = 2 ). MESSAGE e025(zz_flcm). ENDIF.
     RAISE EXCEPTION TYPE zcx_flcm_error
       EXPORTING
         msgid    = mc_msg_cl_flcm "ZZ_FLCM
         msgty    = mc_err         "E
         msgno    = '025'          "025:Original Po not found
         sub_stus = mc_msg_no_000. "000: &1 &2 &3 &4
*                   msgtab   = et_return.
  endif.
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

  CALL METHOD ZCL_FLCM_SERVICES=>GET_PO_DETAIL
    EXPORTING
      ID_EBELN     = ms_unique_po-ebeln
     IMPORTING
       ET_ITEMS     = lt_items
       ET_SCHEDULES = lt_schedules
       ET_RETURN    = rt_return.
    .
  sort lt_items by po_item descending.

  loop at lt_items into ls_items.
   exit.
  endloop.

  clear ms_unique_po-elikz.
  move ls_items-po_item  to ms_unique_po-ebelp.
  move ls_items-ret_item to ms_unique_po-retpo.
  append ms_unique_po to mt_unique_po.


  if sy-subrc ne 0.
     IF ( 1 = 2 ). MESSAGE e025(zz_flcm). ENDIF.
     RAISE EXCEPTION TYPE zcx_flcm_error
       EXPORTING
         msgid    = mc_msg_cl_flcm "ZZ_FLCM
         msgty    = mc_err         "E
         msgno    = '025'          "025:Original Po not found
         sub_stus = mc_msg_no_000. "000: &1 &2 &3 &4
*                   msgtab   = et_return.
  endif.
endmethod.


METHOD search_contract.

   DATA:
      lv_tabix     TYPE sy-tabix,
      lv_ponumber  TYPE ekko-ebeln,
      ld_msgv1     TYPE sy-msgv1,
      ld_msgv2     TYPE sy-msgv2.

   FIELD-SYMBOLS:

     <ms_contract> TYPE zmm_flcm_contract,
     <a016>        LIKE LINE OF mt_a016.

   clear lv_ponumber.
   clear ms_contract.
   loop at mt_contract assigning <ms_contract>
    where lifnr EQ is_trans-lifnr
    and   bsart EQ 'FK'
    and   kdatb LE is_trans-ztds_tran_dt
    and   kdate GE is_trans-ztds_tran_dt
    and   matnr EQ is_trans-matnr
    and   knttp EQ ' '
    and   loekz is initial
    and   werks EQ is_trans-werks.
    lv_tabix = lv_tabix + 1.
   endloop.

* unique contract then po needs to be created
   if lv_tabix GT 1.
     clear lv_tabix.
     loop at mt_contract assigning <ms_contract>
       where lifnr EQ is_trans-lifnr
       and   bsart EQ 'FK'
       and   kdatb LE is_trans-ztds_tran_dt
       and   kdate GE is_trans-ztds_tran_dt
       and   matnr EQ is_trans-matnr
       and   knttp EQ ' '
       and   loekz is initial
       and   werks EQ is_trans-werks
       and   sort1 EQ is_trans-ztds_trml_id.
       lv_tabix = lv_tabix + 1.
     endloop.

     if lv_tabix EQ 1.
        ms_contract = <ms_contract>.
     else.
*   Error: No contract found
        IF ( 1 = 2 ). MESSAGE e051(zz_flcm). ENDIF.
        RAISE EXCEPTION TYPE zcx_flcm_error
          EXPORTING
            msgid    = mc_msg_cl_flcm
            msgty    = mc_err
            msgno    = '051'.
*           msgtab   = et_return.
     endif.
   elseif lv_tabix EQ 1.
     ms_contract = <ms_contract>.
   endif.

* see if contract pricing is valid.
   if lv_tabix eq 1.
      loop at mt_a016 assigning <a016>
        where datbi ge is_trans-ztds_tran_dt
        and   datab le is_trans-ztds_tran_dt.
        exit.
     endloop.
     IF sy-subrc EQ 0.
        exit.
     ENDIF.
  ENDIF.

  IF ms_contract is not initial.
*   Raise exception
    IF ( 1 = 2 ). MESSAGE e081(zz_flcm). ENDIF.
    ld_msgv1 = ms_contract-ebeln.
    ld_msgv2 = ms_contract-ebelp.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid    = mc_msg_cl_flcm
        msgty    = mc_err
        msgv1    = ld_msgv1
        msgv2    = ld_msgv2
        msgno    = '81'
        sub_stus = '10'.
*             msgtab   = et_return.
  ELSE.
*   Error: No contract found
     IF ( 1 = 2 ). MESSAGE e088(zz_flcm). ENDIF.
     RAISE EXCEPTION TYPE zcx_flcm_error
       EXPORTING
         msgid    = mc_msg_cl_flcm
         msgty    = mc_err
         msgno    = '088'.
*           msgtab   = et_return.
  ENDIF.

ENDMETHOD.


METHOD search_po.
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
  DATA: lwa_return TYPE bapiret2.
  DATA: li_ekko  TYPE STANDARD TABLE OF t_ekko,
        lwa_ekko LIKE LINE OF li_ekko,
        li_ekpo  TYPE STANDARD TABLE OF t_ekpo,
        lwa_ekpo LIKE LINE OF li_ekpo,
        lwa_cancel    TYPE zmm_flcm_tds,
        lwa_rebill    TYPE zmm_flcm_tds.
  DATA: lv_ihrez TYPE ekko-ihrez,
        lv_unsez TYPE ekko-unsez.
  DATA: lv_lines TYPE i,
        lv_error TYPE c,
        lv_tabix TYPE sy-tabix,
        lv_ponumber TYPE ekko-ebeln,
***Start of Change - XT22260 - DV5K9A0ARX - CR302137 TK365122
        lv_msgv1 TYPE sy-msgv1,
        lv_msgv2 TYPE sy-msgv2.
***End of Change - XT22260 - DV5K9A0ARX - CR302137 TK365122

  FIELD-SYMBOLS:
    <ms_unique_po_start>  TYPE zmm_flcm_unique_po,
    <ms_unique_po>        TYPE zmm_flcm_unique_po,
    <ms_gr_not_completed> TYPE zmm_flcm_gr_not_completed.

  REFRESH: li_ekko,
           li_ekpo.
  CLEAR: lwa_ekko,
         lwa_ekpo,
         lv_tabix,
         ev_ponumber,
         ev_poitem,
         ev_pocount.

*   Check Cancel Rebill flag
  CLEAR: lwa_cancel,
         lwa_rebill,
         ms_unique_po.

  IF ( is_trans-ztds_canc_rbil EQ mc_ind_rebill ).
    me->check_cancel_rebill(
      EXPORTING
        is_trans  = is_trans
      IMPORTING
        es_cancel = lwa_cancel
        es_rebill = lwa_rebill
    ).
  ENDIF.

  clear lv_ponumber.
  clear lv_tabix.
  sort mt_unique_po by ebeln descending
                       ebelp descending.

  loop at mt_unique_po assigning <ms_unique_po_start>
     where ihrez      EQ is_trans-ztds_bol_nbr
     and   lifnr      EQ is_trans-lifnr
     and   unsez+4(7) EQ is_trans-ztds_trml_id
     and   loekz      is initial
     and   matnr      EQ is_trans-matnr
     and   knttp      EQ abap_false
***Start of Insert - XT22260 - DV5K9A0ARX - CR302137 TK365122
     AND   eindt      EQ is_trans-ztds_tran_dt.
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
        msgid    = mc_msg_cl_flcm "ZZ_FLCM
        msgty    = mc_err         "E
        msgno    = '018'          "025:Original Po not found
        sub_stus = mc_msg_no_000. "000: &1 &2 &3 &4
*                   msgtab   = et_return.

  endif.


  CASE is_trans-ztds_canc_rbil.

    WHEN mc_ind_blank.

* found PO now we must check to see what scenario we are processing and
* verify if this is ok for the type or record we are processing
* checking to see if the GR is done if it is then this could be an error

      if ( <ms_unique_po> is assigned ).

         IF ( <ms_unique_po>-elikz = abap_true ).
***Start of Change - XT22260 - DV5K9A0ARX - CR302137 TK365122
*           IF ( 1 = 2 ). MESSAGE e086(zz_flcm). ENDIF.
           IF ( 1 = 2 ). MESSAGE e111(zz_flcm). ENDIF.
           lv_msgv1 = is_trans-ztds_bol_nbr.
           lv_msgv2 = is_trans-ztds_tran_dt.
***End of Change - XT22260 - DV5K9A0ARX - CR302137 TK365122
           RAISE EXCEPTION TYPE zcx_flcm_error
             EXPORTING
               msgid    = mc_msg_cl_flcm "ZZ_FLCM
               msgty    = mc_err         "E
***Start of Change - XT22260 - DV5K9A0ARX - CR302137 TK365122
*               msgno    = '086'          "086:Original Po not found or already cancelled
*               sub_stus = mc_msg_no_000. "000: &1 &2 &3 &4
**              msgtab   = et_return.
               msgno     = '111'
               msgv1     = lv_msgv1
               msgv2     = lv_msgv2.
***End of Change - XT22260 - DV5K9A0ARX - CR302137 TK365122
         ELSE.
             ms_unique_po = <ms_unique_po>.
         ENDIF.
      ENDIF.

    WHEN mc_ind_cancel.

* found PO now we must check to see what scenario we are processing and
* verify if this is ok for the type or record we are processing
* checking to see if the GR is done if it is then this could be an error

      if ( <ms_unique_po> is assigned ).
* see if the newest record we are looking at a cancel
         IF ( <ms_unique_po>-retpo EQ abap_true ).
* see if the GR was completed
            IF ( <ms_unique_po>-elikz = abap_true ).
              IF ( 1 = 2 ). MESSAGE e086(zz_flcm). ENDIF.
              RAISE EXCEPTION TYPE zcx_flcm_error
                EXPORTING
                  msgid    = mc_msg_cl_flcm "ZZ_FLCM
                  msgty    = mc_err         "E
                  msgno    = '086'          "086:Original Po not found or already cancelled
                  sub_stus = mc_msg_no_000. "000: &1 &2 &3 &4
*                 msgtab   = et_return.
            ELSE.
                ms_unique_po = <ms_unique_po>.
            ENDIF.
* there may be an instance that if the previous action the goods receipt did not get processed then
* we do no want to process the cancel we want the GR for the previous record to get created first
         ELSEIF ( <ms_unique_po>-elikz = abap_false ).
                IF ( 1 = 2 ). MESSAGE e092(zz_flcm). ENDIF.
                RAISE EXCEPTION TYPE zcx_flcm_error
                  EXPORTING
                    msgid    = mc_msg_cl_flcm "ZZ_FLCM
                    msgty    = mc_err         "E
                    msgno    = '092'          "092:Previous record not completed
                    sub_stus = mc_msg_no_000. "000: &1 &2 &3 &4
*                   msgtab   = et_return.
         ELSE.
             ms_unique_po = <ms_unique_po>.
         ENDIF.
      ELSE.
* no po found
         IF ( 1 = 2 ). MESSAGE e086(zz_flcm). ENDIF.
         RAISE EXCEPTION TYPE zcx_flcm_error
           EXPORTING
             msgid    = mc_msg_cl_flcm "ZZ_FLCM
             msgty    = mc_err         "E
             msgno    = '086'          "086:Original Po not found or already cancelled
             sub_stus = mc_msg_no_000. "000: &1 &2 &3 &4
*            msgtab   = et_return.
      ENDIF.



    WHEN mc_ind_rebill.

      IF ( <ms_unique_po> is assigned ).
* see if the newest record we are looking at is a rebill
         IF ( <ms_unique_po>-retpo EQ abap_false ).
* see if the GR was completed
            IF ( <ms_unique_po>-elikz = abap_true ).
               IF ( lwa_cancel-lifnr is not initial ).
                  IF ( lwa_rebill-lifnr is not initial ).
                     IF ( lwa_cancel-lifnr EQ lwa_rebill-lifnr ).
                        IF ( 1 = 2 ). MESSAGE e070(zz_flcm). ENDIF.
                        RAISE EXCEPTION TYPE zcx_flcm_error
                          EXPORTING
                            msgid    = mc_msg_cl_flcm "ZZ_FLCM
                            msgty    = mc_err         "E
                            msgno    = '070'          "070: Vendors are not different and no po found
                            sub_stus = mc_msg_no_000. "000: &1 &2 &3 &4
*                           msgtab   = et_return.
                     ENDIF.
                   ELSE.
                     IF ( 1 = 2 ). MESSAGE e087(zz_flcm). ENDIF.
                     RAISE EXCEPTION TYPE zcx_flcm_error
                       EXPORTING
                         msgid    = mc_msg_cl_flcm "ZZ_FLCM
                         msgty    = mc_err         "E
                         msgno    = '087'          "086:Original Po not found or already rebilled
                         sub_stus = mc_msg_no_000. "000: &1 &2 &3 &4
*                        msgtab   = et_return.
                   ENDIF.
               ELSE.
                 IF ( 1 = 2 ). MESSAGE e087(zz_flcm). ENDIF.
                 RAISE EXCEPTION TYPE zcx_flcm_error
                   EXPORTING
                     msgid    = mc_msg_cl_flcm "ZZ_FLCM
                     msgty    = mc_err         "E
                     msgno    = '087'          "086:Original Po not found or already rebilled
                     sub_stus = mc_msg_no_000. "000: &1 &2 &3 &4
*                    msgtab   = et_return.
               ENDIF.
            ELSE.
*add test on cancel record for rebuild, cancel must be successfull (03) .                          i-DV5K9A07TI
               if ( lwa_cancel-ZTRAN_STUS <> '03' ).             "i-DV5K9A07TI
                  IF ( 1 = 2 ). MESSAGE e092(zz_flcm). ENDIF.   "i-DV5K9A07TI
                  RAISE EXCEPTION TYPE zcx_flcm_error           "i-DV5K9A07TI
                     EXPORTING                                  "i-DV5K9A07TI
                      msgid    = mc_msg_cl_flcm                 "i-DV5K9A07TI
                      msgty    = mc_err                         "i-DV5K9A07TI
                      msgno    = '092'                          "i-DV5K9A07TI
                      sub_stus = mc_msg_no_000.                 "i-DV5K9A07TI
*                     msgtab   = et_return.                     "i-DV5K9A07TI
               else.                                            "i-DV5K9A07TI
                 ms_unique_po = <ms_unique_po>.
               endif.                                           "i-DV5K9A07TI
            ENDIF.
* there may be an instance where the goods receipt of the previous record did not get processed and we
* want to process it before any other records
         ELSEIF ( <ms_unique_po>-elikz = abap_false ).
               IF ( 1 = 2 ). MESSAGE e092(zz_flcm). ENDIF.
               RAISE EXCEPTION TYPE zcx_flcm_error
                 EXPORTING
                   msgid    = mc_msg_cl_flcm "ZZ_FLCM
                   msgty    = mc_err         "E
                   msgno    = '092'          "092:Previous record not completed
                   sub_stus = mc_msg_no_000. "000: &1 &2 &3 &4
*                  msgtab   = et_return.
         ELSE.
* check to see if the vendor is different and if so then its a rebill different vendor
* and we want to create a new po
            IF ( lwa_cancel-lifnr is not initial ).
               IF ( lwa_rebill-lifnr is not initial ).
                  IF ( lwa_cancel-lifnr EQ lwa_rebill-lifnr ).
                     ms_unique_po = <ms_unique_po>.
                  ENDIF.
               ENDIF.
            ENDIF.
         ENDIF.
      ELSE.
* check to see if the vendor is different and if so then its a rebill different vendor
* and we want to create a new po
         IF ( lwa_cancel-lifnr is not initial ).
            IF ( lwa_rebill-lifnr is not initial ).
               IF ( lwa_cancel-lifnr EQ lwa_rebill-lifnr ).
                  IF ( 1 = 2 ). MESSAGE e070(zz_flcm). ENDIF.
                  RAISE EXCEPTION TYPE zcx_flcm_error
                    EXPORTING
                      msgid    = mc_msg_cl_flcm "ZZ_FLCM
                      msgty    = mc_err         "E
                      msgno    = '070'          "070: Vendors are not different and no po found
                      sub_stus = mc_msg_no_000. "000: &1 &2 &3 &4
*                        msgtab   = et_return.
               ENDIF.
            ENDIF.
         ENDIF.
      ENDIF.

    WHEN others.

  ENDCASE.


  ENDMETHOD.


method UPDATE_GR_ELIKZ_FLAG.


  FIELD-SYMBOLS:
    <ms_unique_po> type zmm_flcm_unique_po.

  LOOP at mt_unique_po assigning <ms_unique_po>
    WHERE ebeln = is_unique_po-ebeln
    AND   ebelp = is_unique_po-ebelp.
    <ms_unique_po>-elikz = abap_true.
    exit.
  ENDLOOP.

endmethod.
ENDCLASS.
