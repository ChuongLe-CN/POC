class ZCL_FLCM_TRX_END_OF_DAY definition
  public
  inheriting from ZCL_FLCM_TRX
  final
  create public .

*"* public components of class ZCL_FLCM_TRX_END_OF_DAY
*"* do not include other source files here!!!
public section.

  constants MC_ZZFLCM type BAPIRET2-ID value 'ZZ_FLCM'. "#EC NOTEXT
  constants MC_SUCCESS type BAPIRET2-TYPE value 'S'. "#EC NOTEXT
  constants MC_ZPARM_NM1 type ZMM_FLCM_PARMS-ZPARM_NM value 'ZTDS_TRAN_TYPE'. "#EC NOTEXT
  constants MC_RECON_RECEIPT_SITE type ZMM_FLCM_TDS-ZTDS_TRAN_TYPE value '702'. "#EC NOTEXT
  constants MC_RECON_RECEIPT_DIV type ZMM_FLCM_TDS-ZTDS_TRAN_TYPE value '700'. "#EC NOTEXT
  constants MC_PARTIAL_RECEIPT type ZMM_FLCM_TDS-ZTDS_TRAN_TYPE value '505'. "#EC NOTEXT
  constants MC_IN_YARD_INV type ZMM_FLCM_TDS-ZTDS_TRAN_TYPE value '701'. "#EC NOTEXT
  data MT_ZMM_FLCM_PARMS type ZMM_FLCM_PARMS_T .
  data MT_MARAMARC type ZMM_FLCM_MARAMARC_T .
  constants MC_EOD type ZMM_FLCM_TDS-ZTDS_TRAN_TYPE value '710'. "#EC NOTEXT
  constants MC_PROGNAME_819 type ZMM_FLCM_PARMS-PROGNAME value 'SMENH819'. "#EC NOTEXT
  constants MC_PROGNAME_820 type ZMM_FLCM_PARMS-PROGNAME value 'SMENH820'. "#EC NOTEXT
  constants MC_PROGNAME_821 type ZMM_FLCM_PARMS-PROGNAME value 'SMENH821'. "#EC NOTEXT
  data MD_EXECUTION_DATE type SY-DATUM .
  data MT_MARM type ZMM_FLCM_MARM_T .

  methods CALCUL_DIFF_INVENTORY
    changing
      !IS_TRANS type ZMM_FLCM_TDS .
  methods CONSTRUCTOR
    importing
      !IT_TRANS type ZMM_FLCM_TDS_T optional
      !ID_DATE type SY-DATUM optional
    preferred parameter IT_TRANS .
  type-pools ABAP .
  methods IS_GVMT_CRITERIA_MET
    importing
      !IS_TRANS type ZMM_FLCM_TDS
    returning
      value(RD_RESULT) type ABAP_BOOL .

  methods EXECUTE
    redefinition .
protected section.
*"* protected components of class ZCL_FLCM_TRX_DIVERSION_RECEIPT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_FLCM_TRX_END_OF_DAY
*"* do not include other source files here!!!

  data MC_PHY_GAIN_OR_LOSS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS value '001'. "#EC NOTEXT .
  data MC_PHY_INV_ADJ type ZMM_FLCM_TDS-ZTRAN_SUB_STUS value '002'. "#EC NOTEXT .
ENDCLASS.



CLASS ZCL_FLCM_TRX_END_OF_DAY IMPLEMENTATION.


METHOD calcul_diff_inventory.
*----------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* Nancy Bergeron        261851     2014/06/12          DV5K987403      *
* Add new logic to calculate the difference between TDS and SAP        *
* inventory.   SM-ENH-822                                              *
*----------------------------------------------------------------------*
  DATA: ls_maramarc        TYPE zmm_flcm_maramarc,
        ls_marm            TYPE zmm_flcm_marm,      "I-DV5K989581
        ls_sap_inv         TYPE mard,
        lv_sap_inventory   TYPE ZZSAP_INV, "i,
        lv_tds_inventory   TYPE ZZSAP_INV,  "i, " umlme,
        li_tds_previous    TYPE TABLE OF zmm_flcm_tds,
        ls_tds_previous    TYPE zmm_flcm_tds,
        ls_zmm_flcm_tds    TYPE zmm_flcm_tds,
        lt_return          TYPE bapiret2tab,
        ld_sub_stus        type zmm_flcm_tds-ztran_sub_stus,
        lo_flcm_exception  type ref to zcx_flcm_error,
        lt_msg             type bapiret2_tab,
        lt_plntdef         TYPE TABLE OF zmm_flcm_plntdef,  "I-DV5K989581
        lt_trans_temp      TYPE ZMM_FLCM_TDS_T.             "I-DV5K989581

*********************
* Get TDS inventory *
  lv_tds_inventory = 0.
  READ TABLE mt_maramarc INTO ls_maramarc WITH KEY matnr = is_trans-matnr
                                                   werks = is_trans-werks.

*Begin DV5K989581
  if mt_trans IS NOT INITIAL.

    lt_trans_temp = mt_trans.
    sort lt_trans_temp by werks.
    DELETE ADJACENT DUPLICATES FROM lt_trans_temp COMPARING werks.


*    "Get the werks and lgort
    SELECT *
      from zmm_flcm_plntdef
      INTO TABLE lt_plntdef
      FOR ALL ENTRIES IN lt_trans_temp
      WHERE werks = lt_trans_temp-werks.

    IF lt_plntdef IS NOT INITIAL.
      SELECT *
        FROM mard
        INTO TABLE mt_sap_inv
        FOR ALL ENTRIES IN lt_plntdef
        WHERE werks = lt_plntdef-werks
        AND   lgort = lt_plntdef-lgort.
    ENDIF.

    SELECT matnr
           meinh
           umrez
           umren
      FROM marm
      INTO TABLE mt_marm
      FOR ALL ENTRIES IN lt_trans_temp
      WHERE matnr = lt_trans_temp-matnr.

  ENDIF.
*End DV5K989581

*********************
* Get SAP inventory *
  "mt_sap_inv is mard type
  lv_sap_inventory = 0.
  READ TABLE mt_sap_inv INTO ls_sap_inv WITH KEY matnr = is_trans-matnr
                                                 werks = is_trans-werks.

************************************                                 "I-DV5K989581
* Get Unit of Measure for Material *                                 "I-DV5K989581
  READ TABLE mt_marm INTO ls_marm WITH KEY matnr = is_trans-matnr    "I-DV5K989581
                                           meinh = is_trans-meins.   "I-DV5K989581

  IF  ls_maramarc IS NOT INITIAL
  AND ls_sap_inv  IS NOT INITIAL
  AND ls_marm     IS NOT INITIAL.                                    "I-DV5K989581


    IF  ls_maramarc-meins <> is_trans-meins
*Begin DV5K989581
    and ls_marm-umrez <> 0.

      ls_maramarc-umlmc = ls_maramarc-umlmc * ( ls_marm-umren / ls_marm-umrez ).
      ls_sap_inv-labst  = ls_sap_inv-labst  * ( ls_marm-umren / ls_marm-umrez ).

*      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
*        EXPORTING
*          input                      = ls_maramarc-umlmc
*          unit_in                    = ls_maramarc-meins
*          unit_out                   = is_trans-meins
*       IMPORTING
*         output                      = ls_maramarc-umlmc.
*    ENDIF.
*
*    IF ls_maramarc-meins <> is_trans-meins.
*      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
*        EXPORTING
*          input                      = ls_sap_inv-labst
*          unit_in                    = ls_maramarc-meins
*          unit_out                   = is_trans-meins
*       IMPORTING
*         output                      = ls_sap_inv-labst.
*End DV5K989581
    ENDIF.

    lv_tds_inventory = is_trans-ztds_tot_inv - ls_maramarc-umlmc.
    lv_sap_inventory = ls_sap_inv-labst.
  ENDIF.


**********************************
* Get the 710 for previous folio *
  SELECT *
    FROM zmm_flcm_tds
    INTO TABLE li_tds_previous
    WHERE ztds_tran_type   = MC_EOD
      AND werks            = is_trans-werks
      and ztds_folio_frzdt <= is_trans-ztds_folio_frzdt
      AND ( zsap_inv       <> 0
         or zinv_diff      <> 0
*         or zcumulate_diff <> 0 ). "DVSK989018
         or zinv_diff_delta <> 0 ). "DVSK989018

  SORT li_tds_previous by ztds_folio_frzdt DESCENDING.

  read TABLE li_tds_previous into ls_tds_previous INDEX 1.

  is_trans-zsap_inv       = lv_sap_inventory.
  is_trans-zinv_diff      = lv_sap_inventory - lv_tds_inventory.
*  is_trans-zcumulate_diff = ls_tds_previous-zcumulate_diff + ( lv_sap_inventory - lv_tds_inventory ). DVSK989018
  is_trans-zinv_diff_delta = is_trans-zinv_diff - ls_tds_previous-zinv_diff . "DVSK989018

*  IF is_trans-zinv_diff = 0. "DVSK989018
  IF is_trans-zinv_diff = 0. "DVSK989018
    MESSAGE s102(zz_flcm).
  ELSE.
*    MESSAGE s101(zz_flcm) WITH is_trans-zcumulate_diff. "DVSK989018
    MESSAGE s101(zz_flcm) WITH is_trans-zinv_diff_delta. "DVSK989018
  ENDIF.

  TRY.
*  Set transaction status as processed successfully
   CALL METHOD zcl_flcm_services=>db_update_trans_diff_inv
     EXPORTING
       it_trans         = is_trans
       it_msg           = lt_return
       id_sap_inv       = is_trans-zsap_inv
       id_inv_diff      = is_trans-zinv_diff
*       id_cumulate_diff = is_trans-zcumulate_diff. "DVSK989018
       id_cumulate_diff = is_trans-zinv_diff_delta. "DVSK989018

*  ======
    catch zcx_flcm_error into lo_flcm_exception.
*  ======

      try.

*       Update transaction as in error in database
        zcl_flcm_services=>db_update_trans_error( exporting is_trans     = is_trans
                                                            io_exception = lo_flcm_exception
                                                  changing  it_msg       = lt_return ).

      catch zcx_flcm_error into lo_flcm_exception.

*       Status update failed, return the error message
        lt_msg = lo_flcm_exception->get_bapimsg_table( ).
        append lines of lt_msg to lt_return.

        message s900(zz-cn2) with
          'Exception zcx_flcm_error'
          'was caught'.

      endtry.

*  ======
    endtry.
*  ======

ENDMETHOD.


METHOD CONSTRUCTOR.

*----------------------------------------------------------------------*
* Changed by            Prj/Tsk           Date        Tracking number  *
*---------------------  ----------------  ----------  -----------------*
* Nancy Bergeron        CR214969-T261854  2014/06/13  DV5K987403       *
*                                                                      *
* Short Description: Add new extraction for the SAP inventory.         *
*----------------------------------------------------------------------*

DATA:
  lt_trans_head    TYPE zmm_flcm_tds_t,
  lt_maramarc      TYPE zmm_flcm_maramarc_t,
  ld_select_date   TYPE sy-datum.

* Call superclass constructor
  super->constructor( it_trans ).


  REFRESH mt_zmm_flcm_parms.
  REFRESH lt_trans_head.
  REFRESH: lt_maramarc.

  if id_date is not initial.
     md_execution_date = id_date.
  else.
    md_execution_date = sy-datum.
  endif.
* if no end of day we need not retreive the maramarc records as we will not need them

  CHECK mt_trans[] is not initial.

  SELECT a~matnr
         a~meins
         b~werks
         b~umlmc
   FROM mara AS a
   INNER JOIN marc AS b ON
   b~matnr = a~matnr
   INTO TABLE  lt_maramarc
   FOR ALL ENTRIES IN mt_trans
  WHERE a~matnr EQ mt_trans-matnr.

  IF sy-subrc EQ 0.
    SORT lt_maramarc BY matnr werks.
  ENDIF.

* Retrieve all of the records that would be of the same folio as the
* end of day records that were previously extracted
  SELECT *
    FROM zmm_flcm_tds
    INTO TABLE mt_trans_processed
    FOR ALL ENTRIES           IN mt_trans
    WHERE ztds_tran_type      NE mc_eod    "710
      AND   werks             EQ mt_trans-werks

      AND ( (  ztds_folio_yr  EQ mt_trans-ztds_folio_yr
      AND   ztds_folio_mth    EQ mt_trans-ztds_folio_mth
      AND   ztds_folio_nbr    EQ mt_trans-ztds_folio_nbr
      AND   ztds_folio_seq    EQ mt_trans-ztds_folio_seq
      AND ( ztran_stus        EQ zcl_flcm_services=>mc_status_processed
      OR    ztran_stus        EQ zcl_flcm_services=>mc_status_error ) )


      OR  ( ( ( ztran_stus      EQ zcl_flcm_services=>mc_status_processed
      OR      ztran_stus      EQ zcl_flcm_services=>mc_status_error )
      AND ( ztds_folio_yr     EQ mt_trans-ztds_folio_yr
      AND   ztds_folio_mth    GT mt_trans-ztds_folio_mth )
      OR  ( ztds_folio_yr     GT mt_trans-ztds_folio_yr )
      OR  ( ztds_folio_yr     EQ mt_trans-ztds_folio_yr
      AND   ztds_folio_mth    EQ mt_trans-ztds_folio_mth
      AND   ztds_folio_nbr    GT mt_trans-ztds_folio_nbr ) ) ) ).

* Make sure transactions are in order of creation
  SORT mt_trans BY werks
                   ztds_folio_yr
                   ztds_folio_mth
                   ztds_folio_nbr
                   ztran_stus.

  SORT mt_trans_processed  BY werks
                              ztds_folio_yr descending
                              ztds_folio_mth descending
                              ztds_folio_nbr descending.

  sort mt_sap_inv by matnr werks lgort.   "I-DV5K987403

  mt_maramarc = lt_maramarc.

ENDMETHOD.


METHOD execute.
*---------------------------------------------------------------------------
* -Diversion Receipt-
* The 710 transaction represents an end of day in TDS.
*---------------------------------------------------------------------------

*----------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* Nancy Bergeron        261851     2014/06/12          DV5K987403      *
* Add new logic to calculate the difference between TDS and SAP.       *
*----------------------------------------------------------------------*
* Jose Luis Banda       243167     2014/01/22          DV5K983344      *
*                                                      DV5K984457      *
* For EOM process, check if the EOM processed flag is active when      *
* current WERKS changed to prevent the false record skip.              *
*                                                                      *
*----------------------------------------------------------------------*
* Steven Qiu            243167    2013/08/08           DV5K980921      *
* 1) Add more display logs to know the processing flow of the program. *
* 2) Put fix to reset the flag.                                        *
*                                                                      *
* Steven Qiu            230796    2013/06/30           DV5K979339      *
* - Add display log to know the processing flow of the program.        *
*----------------------------------------------------------------------*
* 23/9/2011  Bob Legault (XT11105)                       DV5K966662    *
* Defect 556 Consider the freeze date as if it was one day earlier     *
************************************************************************


  DATA:
    lo_flcm_exception    TYPE REF TO zcx_flcm_error,
    lo_tds_proc_bol      TYPE REF TO zcl_flcm_trx_proc_bol,
    lo_tds_gr_confirm    TYPE REF TO zcl_flcm_tds_gr_confirm,
    lo_tds_proc_gi_fuel  TYPE REF TO zcl_flcm_trx_proc_gi_fuel.

  DATA:
    lt_msg               TYPE bapiret2_tab,
    lt_trans             TYPE zmm_flcm_tds_t,
    ls_return            TYPE bapiret2,
    lt_return            TYPE bapiret2tab,
    ld_bypass            TYPE abap_bool.

  DATA:
    ld_eom_ind_processed  TYPE abap_bool,
    ld_eom_ind_in_process TYPE abap_bool,
    lr_werks_eom_done     TYPE RANGE OF zmm_flcm_tds-werks,
    ld_eom_folio_yr       TYPE zmm_flcm_tds-ztds_folio_yr,
    ld_eom_folio_mth      TYPE zmm_flcm_tds-ztds_folio_mth,
    ld_eom_folio_nbr      TYPE zmm_flcm_tds-ztds_folio_nbr,
    ld_date_less_one      TYPE sy-datum,
    ld_skip               TYPE abap_bool.

  DATA:
    ls_next               LIKE LINE OF lt_trans,
    ls_prev               LIKE LINE OF lt_trans.            "DV5K983344

  DATA:
    ld_index              TYPE i,
    ld_indx1              TYPE i.                           "DV5K983344

  DATA:
    ld_move_type        TYPE bapi2017_gm_item_create-move_type,
    ld_sub_stus         TYPE zmm_flcm_tds-ztran_sub_stus.

  DATA:
    ld_msgv1       TYPE sy-msgv1.

  DATA:
    lv_flag_next_rec_read,                   "I-DV5K980921
    lv_count       TYPE i,                       "I-DV5K979339
    lv_count_char(20),                           "I-DV5K979339
    lv_text(60).                                 "I-DV5K979339

  FIELD-SYMBOLS:
    <trans>             LIKE LINE OF mt_trans,
    <trans2>            LIKE LINE OF mt_trans,
    <werks>             LIKE LINE OF lr_werks_eom_done.

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

  ld_date_less_one = md_execution_date - 1.
  ld_eom_ind_processed  = abap_false.
  ld_eom_ind_in_process = abap_false.

** Begin of Insert  DV5K979339 -->>
  MESSAGE s900(zz-cn2) WITH
     'List of records to be processed'
     'Transaction Ref Number / Plant:'.
  CLEAR: lv_count, lv_count_char.
  LOOP AT mt_trans ASSIGNING <trans>
    WHERE ( ztds_tran_type = mc_eod ).
    ADD 1 TO lv_count.
    MESSAGE s900(zz-cn2) WITH
        <trans>-ztds_tran_ref_nb
        '/'
        <trans>-werks.
  ENDLOOP.
  lv_count_char = lv_count.
  CONDENSE lv_count_char.
  MESSAGE s900(zz-cn2) WITH
     'Total Number of records to be processed:'
      lv_count_char.
** End of Insert  DV5K979339 --<<

* Go through transactions for end of day process
  LOOP AT mt_trans ASSIGNING <trans>
    WHERE ( ztds_tran_type = mc_eod ).

    CLEAR lv_flag_next_rec_read.                   "I-DV5K980921

** Begin of Insert  DV5K979339 -->>
    MESSAGE s900(zz-cn2) WITH
       '=== Processing Transaction Ref'
        <trans>-ztds_tran_ref_nb
       'Plant'
        <trans>-werks.
** End of Insert  DV5K979339 --<<

    IF lr_werks_eom_done[] IS NOT INITIAL AND
       <trans>-werks IN lr_werks_eom_done.

** Begin of Insert  DV5K979339 -->>
      MESSAGE s900(zz-cn2) WITH
         'Record skipped because'
         'plant has been done for End of Month'.
** End of Insert  DV5K979339 --<<

      CONTINUE.
    ENDIF.

    ld_bypass = abap_false.
    CLEAR ld_sub_stus.

    REFRESH lt_return.
    ld_index = sy-tabix + 1.
    ld_indx1 = sy-tabix - 1.                                "DV5K983344

** Begin of Insert  DV5K979339 -->>
    IF ( <trans>-ztran_stus = zcl_flcm_services=>mc_status_ready )
    OR ( <trans>-ztran_stus = zcl_flcm_services=>mc_status_error ).
    ELSE.
      MESSAGE s900(zz-cn2) WITH
       'Record skipped because'
       'transaction status is not Ready or Error'.
    ENDIF.
** End of Insert  DV5K979339 --<<


*   Only process transaction ready for process or processed in error
    CHECK ( <trans>-ztran_stus = zcl_flcm_services=>mc_status_ready )
       OR ( <trans>-ztran_stus = zcl_flcm_services=>mc_status_error ).

    <trans>-ztds_folio_frzdt = <trans>-ztds_folio_frzdt - 1. "DV5K966662

*  ======
    TRY.
*  ======

*->Start of insert      DV5K983344, DV5K984457
** Clean up both variables of EOM if current WERKS changed,
** just if at least one of the EOM flags is active.
** ld_eom_ind_processed   >>  False record skip.
** ld_eom_ind_in_process  >>  False EOM active at werks change.

        IF ( ld_eom_ind_processed  = abap_true OR
             ld_eom_ind_in_process = abap_true )
          AND ld_indx1 GT 0.
          CLEAR ls_prev.
          READ TABLE mt_trans INTO ls_prev INDEX ld_indx1.
          IF <trans>-werks NE ls_prev-werks.
            ld_eom_ind_processed  = abap_false.
            ld_eom_ind_in_process = abap_false.
          ENDIF.
        ENDIF.
*<-End of insert      DV5K983344, DV5K984457

        IF ld_eom_ind_in_process = abap_true.
** Begin of Insert  DV5K979339 -->>
          MESSAGE s900(zz-cn2) WITH
            'EOM (eom_ind_in_process) is ON'.
** End of Insert  DV5K979339 --<<
          LOOP AT mt_trans_processed ASSIGNING <trans2>
            WHERE werks = <trans>-werks
            AND  ( ztds_folio_yr GT ld_eom_folio_yr
            OR   ( ztds_folio_yr EQ ld_eom_folio_yr
            AND  ztds_folio_mth GT ld_eom_folio_mth ) ).
            EXIT.
          ENDLOOP.
          IF sy-subrc EQ 0.
            IF ( is_gvmt_criteria_met( <trans> ) = abap_true ).
** Begin of Insert  DV5K979339 -->>
              MESSAGE s900(zz-cn2) WITH
                 'EOM is ON, criteria met, Do:'
                 'create_goods_mvmt_end_of_day'.
** End of Insert  DV5K979339 --<<

*     Create goods movement
              rt_return = zcl_flcm_services=>create_goods_mvmt_end_of_day(
                              is_trans     = <trans>
                              id_sub_stus  = mc_phy_gain_or_loss ).

              APPEND LINES OF rt_return TO lt_return.
            ELSEIF ( <trans>-ztds_pgl EQ 0 ).
** Begin of Insert  DV5K979339 -->>
              MESSAGE s900(zz-cn2) WITH
                 'Record not processed. Condition:'
                 '<trans>-ztds_pgl EQ 0'.
** End of Insert  DV5K979339 --<<
              IF ( 1 = 2 ). MESSAGE s075(zz_flcm). ENDIF.
              ls_return-type   = mc_success.
              ls_return-id     = mc_zzflcm.
              ls_return-number = '075'.
              APPEND ls_return TO lt_return.
            ENDIF.
          ELSE.
** Begin of Insert  DV5K979339 -->>
            MESSAGE s900(zz-cn2) WITH
               'Record skipped. See Point 1'
               'in the code'.
** End of Insert  DV5K979339 --<<
            CONTINUE.
          ENDIF.

        ELSE.
** Begin of Insert  DV5K979339 -->>
          MESSAGE s900(zz-cn2) WITH
            'EOM (eom_ind_in_process) is OFF'.
** End of Insert  DV5K979339 --<<

          IF ld_eom_ind_processed = abap_false.
            IF ( is_gvmt_criteria_met( <trans> ) = abap_true ).
** Begin of Insert  DV5K979339 -->>
              MESSAGE s900(zz-cn2) WITH
                 'EOM is OFF, criteria met, Do:'
                 'create_goods_mvmt_end_of_day'.
** End of Insert  DV5K979339 --<<

*     Create goods movement
              rt_return = zcl_flcm_services=>create_goods_mvmt_end_of_day(
                              is_trans     = <trans>
                              id_sub_stus  = mc_phy_gain_or_loss ).

              APPEND LINES OF rt_return TO lt_return.
            ELSEIF ( <trans>-ztds_pgl EQ 0 ).
** Begin of Insert  DV5K979339 -->>
              MESSAGE s900(zz-cn2) WITH
                 'Record not processed. Condition:'
                 '<trans>-ztds_pgl EQ 0'.
** End of Insert  DV5K979339 --<<
              IF ( 1 = 2 ). MESSAGE s075(zz_flcm). ENDIF.
              ls_return-type   = mc_success.
              ls_return-id     = mc_zzflcm.
              ls_return-number = '075'.
              APPEND ls_return TO lt_return.
            ENDIF.
          ELSE.
** Begin of Insert  DV5K979339 -->>
            MESSAGE s900(zz-cn2) WITH
               'Record skipped. See Point 2'
               'in the code'.
** End of Insert  DV5K979339 --<<
            CONTINUE.
          ENDIF.
        ENDIF.

** Begin of Insert  DV5K979339 -->>
        MESSAGE s900(zz-cn2) WITH
          'Now do Process physical inventory'.
** End of Insert  DV5K979339 --<<

*   process physical inventory document which is made up of three BAPI's Create Physical Inventory
*   document Update the inventory count & Post the Inventory difference only if there are no records
*   that are processed or processed in error

* if the record is flagged as end of month then do the physical inventory adjustment
* this will be irregardless of whether or not there are entries in the future
* that have been processed

*-> Start of comment      DV5K984457
** Next code avoid the right determination of the end of the month record.
*        LOOP AT mt_trans_processed ASSIGNING <trans2>
*          WHERE werks = <trans>-werks
*          AND  ztds_tran_type <> zcl_flcm_services=>mc_710
*          AND  ( ztran_stus EQ zcl_flcm_services=>mc_status_processed OR
*                 ztran_stus EQ zcl_flcm_services=>mc_status_error )
*          AND  ( ztds_folio_yr GT <trans>-ztds_folio_yr
*          OR   ( ztds_folio_yr EQ <trans>-ztds_folio_yr
*          AND  ztds_folio_mth GT <trans>-ztds_folio_mth ) ).
*          EXIT.
*        ENDLOOP.

*        IF sy-subrc NE 0.
        IF ( <trans>-zeom_ind IS NOT INITIAL ).
*<- End of comment        DV5K984457

* Begin of Insert  DV5K979339 -->>
          MESSAGE s900(zz-cn2) WITH
            'Subrc NE 0. See Point 3'
            'in the code'
            '(Process physical inventory)'.
* End of Insert  DV5K979339 --<<

*   IF ( <trans>-zeom_ind IS NOT INITIAL ).                 "DV5K984457
          APPEND INITIAL LINE TO lr_werks_eom_done ASSIGNING <werks>.
          <werks>-option = 'EQ'.
          <werks>-sign   = 'I'.
          <werks>-low    = <trans>-werks.
          ld_eom_ind_in_process = abap_true.
          ld_eom_folio_yr       = <trans>-ztds_folio_yr.
          ld_eom_folio_mth      = <trans>-ztds_folio_mth.
          ld_eom_folio_nbr      = <trans>-ztds_folio_nbr.
** Begin of Insert  DV5K979339 -->>
          MESSAGE s900(zz-cn2) WITH
             'zeom_ind is ON. Do:'
             'create_physinv_adjustment 1'.
** End of Insert  DV5K979339 --<<

*Begin DV5K987403
* ADD LOGIC FOR CALCULATIONS INV DIFFENCE

          CALL METHOD me->calcul_diff_inventory
            CHANGING is_trans = <trans>.

*          rt_return = zcl_flcm_services=>create_physinv_adjustment(
*                          is_trans     = <trans>
*                          it_maramarc  = mt_maramarc
*                          id_sub_stus  = mc_phy_inv_adj ).
*
*** Begin of Insert  DV5K980921 -->>
*          MESSAGE s900(zz-cn2) WITH
*             'After doing create_physinv_adjustment 1,'
*             'reset ld_eom_ind_in_process to false'.
*** End of Insert  DV5K980921 --<<
*
*          ld_eom_ind_processed = abap_true.
*          APPEND LINES OF rt_return TO lt_return.
*          ld_eom_ind_in_process = abap_false.
**        ENDIF.                                             "DV5K984457

          ld_eom_ind_processed = abap_true.
          ld_eom_ind_in_process = abap_false.
*End DV5K987403
        ENDIF.

        CLEAR ls_next.
        READ TABLE mt_trans INTO ls_next INDEX ld_index.
        lv_flag_next_rec_read = abap_true.           "I-DV5K980921
** Begin of Insert  DV5K979339 -->>
        MESSAGE s900(zz-cn2) WITH
            'Next trans rec is:'
            ls_next-ztds_tran_ref_nb.
** End of Insert  DV5K979339 --<<

* it this is the last transaction for the plant and the last entry is the end of day
* otherwise there could be records for the plant for another day and they have not
* sent the end of day yet but they have sent other records and they have been processed
* if that is the case we do not want to do the physical inventory adjustment

* in the case that we may have received two eods with the same folio freeze date we only
* want to process the last one
        ld_skip = abap_false.
        IF ( <trans>-werks EQ ls_next-werks ) AND
           ( <trans>-zeom_ind IS INITIAL )    AND
           ( ls_next-zeom_ind IS INITIAL )    AND
           ( <trans>-ztds_folio_yr EQ ls_next-ztds_folio_yr ) AND
           ( <trans>-ztds_folio_mth EQ ls_next-ztds_folio_mth ) AND
           ( <trans>-ztds_folio_frzdt EQ ls_next-ztds_folio_frzdt ).
          ld_sub_stus = zcl_flcm_services=>mc_sub_status_phyi_notapplied.
          ld_msgv1    = <trans>-ztds_folio_frzdt.
          IF ( 1 = 2 ). MESSAGE s091(zz_flcm). ENDIF.
          ls_return-type   = mc_success.
          ls_return-id     = mc_zzflcm.
          ls_return-number = '091'.
          ls_return-message_v1 = ld_msgv1.
          APPEND ls_return TO lt_return.
          ld_skip = abap_true.
        ENDIF.

** Begin of Insert  DV5K979339 -->>
        IF ld_skip = abap_true.
          MESSAGE s900(zz-cn2) WITH
            'Two eods with same folio freeze date,'
            'we only want to process last one.'
            'Skip indicator(ld_skip) is ON'.
        ENDIF.
** End of Insert  DV5K979339 --<<

        IF ld_skip = abap_false.
          IF ld_eom_ind_in_process = abap_true.
            LOOP AT mt_trans_processed ASSIGNING <trans2>
              WHERE werks = <trans>-werks
              AND  ( ztds_folio_yr GT ld_eom_folio_yr
              OR   ( ztds_folio_yr EQ ld_eom_folio_yr
              AND  ztds_folio_mth GT ld_eom_folio_mth ) ).
              EXIT.
            ENDLOOP.

            IF sy-subrc EQ 0.
"              If there are more EOD transactions (in the new month) to be processed for the corresponding plant, do not process them at all.
*Begin DV5K987403
              CALL METHOD me->calcul_diff_inventory
                CHANGING is_trans = <trans>.

*** Begin of Insert  DV5K979339 -->>
*              MESSAGE s900(zz-cn2) WITH
*                 'zeom_ind is ON. Do:'
*                 'create_physinv_adjustment 2'.
*** End of Insert  DV5K979339 --<<
*              rt_return = zcl_flcm_services=>create_physinv_adjustment(
*                              is_trans     = <trans>
*                              it_maramarc = mt_maramarc
*                              id_sub_stus  = mc_phy_inv_adj ).
*
*              APPEND LINES OF rt_return TO lt_return.
*End DV5K987403
            ENDIF.

          ELSEIF ld_eom_ind_processed = abap_false.

            IF   ( <trans>-ztds_folio_frzdt EQ md_execution_date OR
                   <trans>-ztds_folio_frzdt EQ ld_date_less_one ).


              IF ( <trans>-zeom_ind IS INITIAL ).
*Begin DV5K987403
                CALL METHOD me->calcul_diff_inventory
                  CHANGING is_trans = <trans>.

*** Begin of Insert  DV5K979339 -->>
*                MESSAGE s900(zz-cn2) WITH
*                  'zeom_ind is ON. Do:'
*                  'create_physinv_adjustment 3'.
*** End of Insert  DV5K979339 --<<
*
*                rt_return = zcl_flcm_services=>create_physinv_adjustment(
*                               is_trans     = <trans>
*                               it_maramarc = mt_maramarc
*                               id_sub_stus  = mc_phy_inv_adj ).
*
*                APPEND LINES OF rt_return TO lt_return.
*End DV5K987403
              ENDIF.
            ELSE.
* if this record is not being processed because records in the future we want to set the substatus to 003 and update
              ld_sub_stus = zcl_flcm_services=>mc_sub_status_phyi_notapplied.
              IF ( 1 = 2 ). MESSAGE s079(zz_flcm). ENDIF.
              ls_return-type   = mc_success.
              ls_return-id     = mc_zzflcm.
              ls_return-number = '079'.
              APPEND ls_return TO lt_return.

            ENDIF.

            IF <trans>-werks NE ls_next-werks.
              ld_eom_ind_processed  = abap_false.
              ld_eom_ind_in_process = abap_false.
            ENDIF.
          ENDIF.
        ENDIF.

        rt_return[] = lt_return[].

*     Set transaction status as processed successfully
        zcl_flcm_services=>db_update_trans_success( EXPORTING is_trans    = <trans>
                                                              it_msg      = rt_return
                                                              id_sub_stus = ld_sub_stus ).

*  ======
      CATCH zcx_flcm_error INTO lo_flcm_exception.
*  ======

        TRY.
* we will check to see if the plant is already in the table which indicates that the eom is being done for the plant
* there are two scenarios that exist if we are processing and eom indicated record and it failed it may have failed in
* the create_goods_mvmt_end_of_day if this happens we do not want to process any other records until that one is fixed
* if there was an error in the create_pysinv_adjustment then the record is already in the table and we need not add it
            IF ( <trans>-zeom_ind IS NOT INITIAL ).
              READ TABLE lr_werks_eom_done TRANSPORTING NO FIELDS
                 WITH KEY low = <trans>-werks.
              IF sy-subrc NE 0.
                APPEND INITIAL LINE TO lr_werks_eom_done ASSIGNING <werks>.
                <werks>-option = 'EQ'.
                <werks>-sign   = 'I'.
                <werks>-low    = <trans>-werks.
              ENDIF.
            ENDIF.

*       Update transaction as in error in database
            zcl_flcm_services=>db_update_trans_error( EXPORTING is_trans     = <trans>
                                                                io_exception = lo_flcm_exception
                                                      CHANGING  it_msg       = lt_return ).

          CATCH zcx_flcm_error INTO lo_flcm_exception.

*       Status update failed, return the error message
            lt_msg = lo_flcm_exception->get_bapimsg_table( ).
            APPEND LINES OF lt_msg TO rt_return.

** Begin of Insert  DV5K980921 -->>
            MESSAGE s900(zz-cn2) WITH
              'Exception zcx_flcm_error'
              'was caught'.
** End of Insert  DV5K980921 --<<

        ENDTRY.

*  ======
    ENDTRY.
*  ======

*-> Start of insert      DV5K984457
** If MI02 trigger an exception for the EOM process,
** the flags are not well updated.
    IF <trans>-zeom_ind IS NOT INITIAL
    AND ld_eom_ind_in_process = abap_true.
      MESSAGE s900(zz-cn2) WITH
         'MI02 exception create_physinv_adjustment 1,'
         'reset ld_eom_ind_in_process to false'.

      ld_eom_ind_processed = abap_true.
      ld_eom_ind_in_process = abap_false.

    ENDIF.
*<- End of insert      DV5K984457

    IF lv_flag_next_rec_read = abap_false.               "I-DV5K980921
      CLEAR ls_next.                                     "I-DV5K980921
      READ TABLE mt_trans INTO ls_next INDEX ld_index.   "I-DV5K980921
      MESSAGE s900(zz-cn2) WITH                          "I-DV5K980921
        'End of code,'                                   "I-DV5K980921
        'Next record not read, read again'.              "I-DV5K980921
      MESSAGE s900(zz-cn2) WITH
        'Next trans rec is:'
        ls_next-ztds_tran_ref_nb.
    ENDIF.                                               "I-DV5K980921

    IF <trans>-werks NE ls_next-werks.
** Begin of Insert  DV5K980921 -->>
      MESSAGE s900(zz-cn2) WITH
       'End of code,'
       'Plant changed, flag was reset'.
** End of Insert  DV5K980921 --<<
      ld_eom_ind_in_process = abap_false.
      ld_eom_ind_processed = abap_false.
    ENDIF.

  ENDLOOP.

* Remove all processed records
  FREE mt_trans.

ENDMETHOD.


method IS_GVMT_CRITERIA_MET.


* here we want to verify if the record is in error and it was on the physical inventory adjustment error
* if this is the case then we do not need to create a goods movement since it would already have been done
    if ( is_trans-ztran_stus = zcl_flcm_services=>mc_status_error ).
* if the physical gain or loss was already applied we must not create another goods movement
      if ( is_trans-ztran_sub_stus eq mc_phy_gain_or_loss ).
         rd_result = abap_true.
      endif.
    elseif ( is_trans-ztran_stus = zcl_flcm_services=>mc_status_ready ).
           if ( is_trans-ztds_pgl NE 0 ).
              rd_result = abap_true.
           endif.
    endif.

endmethod.
ENDCLASS.
