REPORT  zme_smenh9qe01_revaluation LINE-SIZE 132.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZME_SMENH9QE01_REVALUATION                               *
* Title    :  Create an ERS transaction to process ERS invoice per BOL.*
* Work Unit:  SM-ENH-9QE-001                                           *
* Created by: Rob West                                                 *
* Created on: November 2010                                            *
* Version:    1.0                                                      *
* T-Code:     ZM_MASSERSREVAL                                          *
*                                                                      *
* Purpose:    Extract all changes in the past 7 days and initiate a    *
*             run of MRNB to revaluate the documents                   *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Chuong Le                  2017-01-04         DV5K9A05WW             *
*                                                                      *
* Short Description: CR306321-T347799                                  *
*    - Read Our Reference from PO header text instead of EKKO-UNSEZ.   *
*----------------------------------------------------------------------*
* Jose Luis Banda           2016/05/03          DV5K9A01KC             *
*                           DVS SYNC            DVSK9A022Y,DVSK9A0259  *
* Short Description: CR269551-T325965 [FI-ENH-9QE]                     *
*  - Display different ALV structure for Test and Update mode execution*
*  - For Update mode the standard transaction MRNB executed as BDC,    *
*    finish before to reach the implicit enhancement to save the       *
*    execution results into the memory variable.                       *
*----------------------------------------------------------------------*
* Aldrien Baugbog           2013/09/24          DV5K982273             *
*                                                                      *
* Short Description: Added GR/Price Determination date ranges and      *
*                    radio buttons similar to what exists in           *
*                    transaction MRNB                                  *
*                    (Date for GR Selection/Price Determination)       *
*                                                                      *
*----------------------------------------------------------------------*
* Rob West                  2011/11/22          DV5K967528             *
*                                                                      *
* Short Description: FLCM defect 651                                   *
*                    Seelct correct radio button for MRNB processing   *
*                                                                      *
*----------------------------------------------------------------------*
* Rob West                  2010/11/29          DV5K961206             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

TYPE-POOLS: abap,
            icon.
***Start of Insert - XT18910 - DV5K982273 - CR224502 TK247533
TABLES: rap_para.
***End of Insert - XT18910 - DV5K982273 - CR224502 TK247533
*--------------------------------------------------------------------*
*TYPES: BEGIN OF t_ekko,
*         ebeln               TYPE ekko-ebeln,
*         loekz               TYPE ekko-loekz,
*         zterm               TYPE ekko-zterm,
*         bedat               TYPE ekko-bedat,
*         unsez               TYPE ekko-unsez,
*       END OF t_ekko,
TYPES:
       t_ekko_table          TYPE STANDARD TABLE OF zcl_smenh9qe01_revaluation=>t_ekko
                                  WITH DEFAULT KEY.

*TYPES: BEGIN OF t_ekpo,
*         ebelp               TYPE ekpo-ebelp,
*         loekz               TYPE ekpo-loekz,
*         matnr               TYPE ekpo-matnr,
*         werks               TYPE ekpo-werks,
*         konnr               TYPE ekpo-konnr,
*         xersy               TYPE ekpo-xersy,
*         txz01               TYPE ekpo-txz01,
*       END OF t_ekpo,
TYPES:
       t_ekpo_table          TYPE STANDARD TABLE OF zcl_smenh9qe01_revaluation=>t_ekpo
                                  WITH EMPTY KEY.

*       t_ebelp_table         TYPE STANDARD TABLE OF ebelp
*                                  WITH DEFAULT KEY.

*--------------------------------------------------------------------*
DATA: v_ekko_1000  TYPE ekko            ##NEEDED,
      v_ekpo_1000  TYPE ekpo            ##NEEDED,
      v_ucomm_1000 TYPE sy-ucomm        ##NEEDED,
      v_msg_shown  TYPE xfeld           ##NEEDED.
***Start of Insert - XT18910 - DV5K982273 - CR224502 TK247533
DATA: v_start TYPE sy-datum                       ##NEEDED.
***End of Insert - XT18910 - DV5K982273 - CR224502 TK247533
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_date1        AS CHECKBOX USER-COMMAND dt1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 4(20) text-dt1.  " FOR FIELD s_bedat.
SELECTION-SCREEN POSITION 32.
SELECT-OPTIONS: s_bedat      FOR v_ekko_1000-bedat MODIF ID dt1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_date2        AS CHECKBOX USER-COMMAND dt2.
SELECTION-SCREEN COMMENT 4(20) text-dt2 FOR FIELD p_numday.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: p_numday         TYPE numc3 MODIF ID dt2.
SELECTION-SCREEN POSITION 44.
PARAMETERS: p_today      AS CHECKBOX MODIF ID dt2.
SELECTION-SCREEN COMMENT (25) text-dt3 FOR FIELD p_today.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: s_lifnr      FOR v_ekko_1000-lifnr,
                s_ebeln      FOR v_ekko_1000-ebeln,
                s_konnr      FOR v_ekko_1000-konnr,
                s_matnr      FOR v_ekpo_1000-matnr,
                s_werks      FOR v_ekpo_1000-werks,
                s_unsez      FOR v_ekko_1000-unsez.
SELECTION-SCREEN SKIP.
***Start of Insert - XT18910 - DV5K982273 - CR224502 TK247533
"Added 'Date for GR Selection/Price Determination' parameters as seen in MRNB
SELECTION-SCREEN BEGIN OF BLOCK datum WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_wedat FOR rap_para-datum NO-EXTENSION.
PARAMETERS:
  rb_budat TYPE rap_para-budat RADIOBUTTON GROUP bez,
  rb_cpudt TYPE rap_para-cpudt RADIOBUTTON GROUP bez,
  rb_bldat TYPE rap_para-bldat RADIOBUTTON GROUP bez  DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK datum.
***End of Insert - XT18910 - DV5K982273 - CR224502 TK247533
PARAMETERS: p_test           AS CHECKBOX DEFAULT 'X' MODIF ID tst.
*PARAMETERS: p_test           AS CHECKBOX DEFAULT 'X'.

*--------------------------------------------------------------------*
DATA: i_ekko                 TYPE t_ekko_table               ##NEEDED.
DATA: v_update_available     TYPE xfeld VALUE abap_false     ##NEEDED.

*--------------------------------------------------------------------*
INITIALIZATION.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = 'MRNB'
    EXCEPTIONS
      ok     = 1
      not_ok = 2
      OTHERS = 3.

  IF sy-subrc <> 1.
    MESSAGE e172(pg) WITH 'MRNB'.
  ENDIF.

  PERFORM f_default_ranges.
***Start of Insert - XT18910 - DV5K982273 - CR224502 TK247533
  "Defaulted 'Date of past period' values to be identical to what is defaulted in MRNB
  PERFORM f_past_period_date.
***End of Insert - XT18910 - DV5K982273 - CR224502 TK247533

  AUTHORITY-CHECK OBJECT 'ZM_9QE_UPD'
             ID 'ZRUNMODE' FIELD '02'.    "check for update enabled
  IF sy-subrc = 0.
    v_update_available = abap_true.
  ELSE.
    AUTHORITY-CHECK OBJECT 'ZM_9QE_UPD'
             ID 'ZRUNMODE' FIELD '01'.    "check for test mode enabled
    IF sy-subrc = 0.
      p_test = abap_true.
    ELSE.
      MESSAGE e034(aut).                  "not authorized.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'DT1'.
      v_ucomm_1000 = sy-ucomm.
      IF p_date1 = abap_false.
        p_date1 = abap_true.
      ELSE.
        CLEAR: p_date2.
      ENDIF.
      RETURN.
    WHEN 'DT2'.
      v_ucomm_1000 = sy-ucomm.
      IF p_date2 = abap_false.
        p_date2 = abap_true.
      ELSE.
        IF p_numday IS INITIAL.
          p_numday = '10'.
        ENDIF.
        CLEAR: p_date1.
      ENDIF.
      RETURN.
  ENDCASE.

  IF sy-ucomm = 'ONLI' OR
     sy-ucomm = 'PRIN' OR
     sy-ucomm = 'SJOB'.
    IF s_bedat[] IS INITIAL AND p_numday IS INITIAL.
      MESSAGE e057(zz_flcm).
    ENDIF.

    IF s_lifnr[] IS INITIAL AND v_msg_shown = abap_false.
      v_msg_shown = abap_true.
      MESSAGE i058(zz_flcm).
      LEAVE TO SCREEN 1000.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  IF p_date1 = abap_true.
    LOOP AT SCREEN.
      IF screen-group1 = 'DT1'.
        IF screen-name = 'S_BEDAT-LOW' OR
           screen-name = 'S_BEDAT-HIGH'.
          screen-input = '1'.
        ELSEIF screen-name = '%_S_BEDAT_%_APP_%-OPTI_PUSH' OR
               screen-name = '%_S_BEDAT_%_APP_%-VALU_PUSH'.
          screen-invisible = '0'.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'DT2'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p_date2 = abap_true.
    LOOP AT SCREEN.
      IF screen-group1 = 'DT1'.
        IF screen-name = 'S_BEDAT-LOW' OR
           screen-name = 'S_BEDAT-HIGH'.
          screen-input = '0'.
        ELSEIF screen-name = '%_S_BEDAT_%_APP_%-OPTI_PUSH' OR
               screen-name = '%_S_BEDAT_%_APP_%-VALU_PUSH'.
          screen-invisible = '1'.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'DT2'.
        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CASE v_ucomm_1000.
    WHEN 'DT1'.
      SET CURSOR FIELD 'S_BEDAT-LOW'.
      CLEAR v_ucomm_1000.
    WHEN 'DT2'.
      SET CURSOR FIELD 'P_NUMDAY'.
      CLEAR v_ucomm_1000.
  ENDCASE.

  IF v_update_available = abap_false.
    LOOP AT SCREEN INTO DATA(l_screen).
      IF l_screen-group1 = 'TST'.
        l_screen-input = '0'.
        MODIFY screen FROM l_screen.
        p_test = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

*--------------------------------------------------------------------*

START-OF-SELECTION.

  IF v_update_available = abap_false.
    p_test = abap_true.
  ENDIF.

*-> Start of comment  CR269551-T325965   DV5K9A01KC
*  IF p_test = abap_true.
*    WRITE: /, / '*** MRBN called in test mode ***'(t01), /.
*    ULINE.
*  ENDIF.
*<- End of comment    CR269551-T325965    DV5K9A01KC

  PERFORM f_load_ekko CHANGING i_ekko[].

  PERFORM f_process_ekko USING i_ekko[].

  IF sy-batch = abap_false.
    zcl_smenh9qe01_revaluation=>display_alv( im_test = p_test ).    "U-DV5K9A01KC
  ENDIF.

  EXIT.

*&---------------------------------------------------------------------*
*&      Form  f_load_ekko
*&---------------------------------------------------------------------*
FORM f_load_ekko CHANGING pi_ekko TYPE t_ekko_table.

  DATA: ls_bedat           TYPE RANGE OF ekko-bedat.

  FIELD-SYMBOLS: <bedat>   LIKE LINE OF ls_bedat.

*** Determine date range
  IF p_date1 = abap_true.
    ls_bedat[] = s_bedat[].
  ELSE.
    APPEND INITIAL LINE TO ls_bedat ASSIGNING <bedat>.
    <bedat>-sign = 'I'.
    <bedat>-option = 'BT'.
    <bedat>-high = sy-datum.
    IF p_today = abap_false.
      SUBTRACT 1 FROM <bedat>-high.
    ENDIF.
    <bedat>-low = <bedat>-high - p_numday.
  ENDIF.

*** Select relevant EKKO records based on date range
  SELECT ebeln
         loekz
         lifnr
         zterm
         bedat
*         unsez                                            "DV5K9A05WW-
    FROM ekko
    INTO CORRESPONDING FIELDS OF TABLE pi_ekko             "DV5K9A05WW+
    WHERE lifnr IN s_lifnr AND
          bedat IN ls_bedat.

  IF sy-subrc = 0.
*--> Start of insert DV5K9A05WW - Chuong Le - 2017/01/04
    LOOP AT pi_ekko ASSIGNING FIELD-SYMBOL(<fs_ekko>).
      PERFORM f_get_po_header_text USING    <fs_ekko>-ebeln
                                   CHANGING <fs_ekko>-unsez.
    ENDLOOP.
*<-- End of insert DV5K9A05WW - Chuong Le - 2017/01/04

*** Remove EKKO entreis that are not relevant to this process
    DELETE pi_ekko WHERE NOT ( loekz = space AND
                               ebeln IN s_ebeln AND
                               unsez IN s_unsez ).
  ENDIF.

  IF pi_ekko[] IS INITIAL.
    MESSAGE 'No EKKO data to process'(e01) TYPE 'I'.
  ELSE.
    SORT pi_ekko BY ebeln ASCENDING.
  ENDIF.

ENDFORM.                    "f_load_ekko

*&---------------------------------------------------------------------*
*&      Form  f_process_ekko
*&---------------------------------------------------------------------*
FORM f_process_ekko USING pi_ekko TYPE t_ekko_table.

*  TYPES: BEGIN OF lt_ekpo,
*           ebelp            TYPE ekpo-ebelp,
*           loekz            TYPE ekpo-loekz,
*           matnr            TYPE ekpo-matnr,
*           werks            TYPE ekpo-werks,
*           konnr            TYPE ekpo-konnr,
*           xersy            TYPE ekpo-xersy,
*         END OF lt_ekpo.

  DATA: "l_tabix            TYPE sy-tabix,
        li_ekpo            TYPE t_ekpo_table.
*        li_ebelp           TYPE t_ebelp_table,

*        li_ekpo            TYPE STANDARD TABLE OF lt_ekpo.

  FIELD-SYMBOLS: <ekko>      TYPE zcl_smenh9qe01_revaluation=>t_ekko.
*                 <ekpo>      TYPE lt_ekpo.

  LOOP AT pi_ekko ASSIGNING <ekko>.

*** Get line items from EKPO
    SELECT ebelp
           loekz
           matnr
           werks
           konnr
           xersy
           txz01
      FROM ekpo
      INTO TABLE li_ekpo
      WHERE ebeln = <ekko>-ebeln.

*** Keep a list of non-deleted line items
    IF sy-subrc = 0.
      DELETE li_ekpo WHERE NOT ( loekz IS INITIAL AND
                                 konnr IN s_konnr AND
                                 matnr IN s_matnr AND
                                 werks IN s_werks AND
                                 xersy = abap_true ).
      IF li_ekpo[] IS NOT INITIAL.
        PERFORM f_submit_mrnb USING <ekko>
                                    li_ekpo[].
        CLEAR li_ekpo[].
      ENDIF.
    ENDIF.

  ENDLOOP.
*    LOOP AT li_ekpo ASSIGNING <ekpo>
*        WHERE loekz IS INITIAL AND
*              konnr IN s_konnr AND
*              matnr IN s_matnr AND
*              werks IN s_werks AND
*              xersy = abap_true.
*      APPEND <ekpo>-ebelp TO li_ebelp.
*    ENDLOOP.

*    PERFORM f_submit_mrnb USING <ekko>
*                                li_ebelp[].
*    REFRESH li_ebelp.
*  ENDLOOP.

ENDFORM.                    "f_process_ekko

*&---------------------------------------------------------------------*
*&      Form  f_submit_mrnb
*&---------------------------------------------------------------------*
FORM f_submit_mrnb USING p_ekko   TYPE zcl_smenh9qe01_revaluation=>t_ekko
                         pi_ekpo  TYPE t_ekpo_table.
*                         pi_ebelp TYPE t_ebelp_table.

  DATA: li_bdcdata       TYPE STANDARD TABLE OF bdcdata,
        li_messages      TYPE tab_bdcmsgcoll,
        li_mrnb_messages TYPE zcl_smenh9qe01_revaluation=>t_mrnb_extract_table,
*        li_mrnb_messages   TYPE bkk_tab_mesg,
        l_tabix          TYPE sy-tabix,

        l_mode           TYPE c VALUE 'P',
        l_update         TYPE c VALUE 'S'.

  FIELD-SYMBOLS: <ebelp>   TYPE ekpo-ebelp,
                 <ekpo>    TYPE zcl_smenh9qe01_revaluation=>t_ekpo,
                 <bdcdata> TYPE bdcdata.

*  IF pi_ebelp[] IS INITIAL.
  IF pi_ekpo[] IS INITIAL.
    RETURN.           "No line items to process - return
  ENDIF.

  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  <bdcdata>-program = 'RMMR1MRB'.
  <bdcdata>-dynpro = '1000'.
  <bdcdata>-dynbegin = 'X'.
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  <bdcdata>-fnam = 'BDC_OKCODE'.
  <bdcdata>-fval = '=ONLI'.
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  <bdcdata>-fnam = 'PA_EBELN'.
  <bdcdata>-fval = p_ekko-ebeln.
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  l_tabix = sy-tabix.
  <bdcdata>-fnam = 'PA_EBELP'.                             "DV5K967528 defect 651
  <bdcdata>-fval = space.                                  "DV5K967528 defect 651
***Start of Insert - XT18910 - DV5K982273 - CR224502 TK247533
  "Added BDC fields for 'Date of past period' in MRNB
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  <bdcdata>-fnam = 'SO_WEDAT-LOW'.
  <bdcdata>-fval = s_wedat-low.
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  <bdcdata>-fnam = 'SO_WEDAT-HIGH'.
  <bdcdata>-fval = s_wedat-high.
***End of Insert - XT18910 - DV5K982273 - CR224502 TK247533
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.   "DV5K967528 defect 651
  <bdcdata>-fnam = 'PA_BUDAT'.                             "DV5K967528 defect 651
*  <bdcdata>-fval = abap_false.                            "DV5K967528 defect 651 "D - XT18910 - DV5K982273 - CR224502 TK247533
  <bdcdata>-fval = rb_budat.                                "I - XT18910 - DV5K982273 - CR224502 TK247533
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.   "DV5K967528 defect 651
  <bdcdata>-fnam = 'PA_CPUDT'.                             "DV5K967528 defect 651
*  <bdcdata>-fval = abap_false.                            "DV5K967528 defect 651 "D - XT18910 - DV5K982273 - CR224502 TK247533
  <bdcdata>-fval = rb_cpudt.                                "I - XT18910 - DV5K982273 - CR224502 TK247533
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.   "DV5K967528 defect 651
  <bdcdata>-fnam = 'PA_BLDAT'.                             "DV5K967528 defect 651
*  <bdcdata>-fval = abap_true.                             "DV5K967528 defect 651 "D - XT18910 - DV5K982273 - CR224502 TK247533
  <bdcdata>-fval = rb_bldat.                                "I - XT18910 - DV5K982273 - CR224502 TK247533
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  <bdcdata>-fnam = 'PA_ZFBDT'.
  <bdcdata>-fval = p_ekko-bedat.
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  <bdcdata>-fnam = 'PA_ZTERM'.
  <bdcdata>-fval = p_ekko-zterm.
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  <bdcdata>-fnam = 'PA_GUZTE'.
  <bdcdata>-fval = p_ekko-zterm.
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  <bdcdata>-fnam = 'PA_XTEST'.
  <bdcdata>-fval = p_test.

  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  <bdcdata>-program = 'SAPMSSY0'.
  <bdcdata>-dynpro = '0120'.
  <bdcdata>-dynbegin = 'X'.
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  <bdcdata>-fnam = 'BDC_OKCODE'.
  <bdcdata>-fval = '=EF3'.

  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  <bdcdata>-program = 'RMMR1MRB'.
  <bdcdata>-dynpro = '1000'.
  <bdcdata>-dynbegin = 'X'.
  APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
  <bdcdata>-fnam = 'BDC_OKCODE'.
  <bdcdata>-fval = '/EE'.

  READ TABLE li_bdcdata ASSIGNING <bdcdata> INDEX l_tabix.
  ASSIGN <bdcdata>-fval TO <ebelp> CASTING.

*  LOOP AT pi_ebelp INTO <ebelp>.
  LOOP AT pi_ekpo ASSIGNING <ekpo>.
    <ebelp> = <ekpo>-ebelp.
    CALL METHOD zcl_smenh9qe01_revaluation=>set_message_flag.
    CALL TRANSACTION 'MRNB' USING  li_bdcdata            "#EC CI_CALLTA
                            MODE   l_mode
                            UPDATE l_update
                            MESSAGES INTO li_messages.
    DELETE li_messages WHERE ( msgtyp = 'S' AND
                               msgid = '00' AND
                               msgnr = '344' AND
                               msgv1 = 'RMMR1MRB' AND
                               msgv2 = '1000' ).

    IF sy-batch = abap_true.
      CALL METHOD zcl_smenh9qe01_revaluation=>get_messages
        CHANGING
          ch_messages = li_mrnb_messages[].

      PERFORM f_write_joblog USING p_ekko
                                   <ebelp>
                                   li_messages[]
                                   li_mrnb_messages[].
    ELSE.
      zcl_smenh9qe01_revaluation=>accumulate_alv_data( im_ekko         = p_ekko
                                                       im_ekpo         = <ekpo>
                                                       im_bdc_messages = li_messages[]
                                                       im_test = p_test ).
    ENDIF.
    REFRESH: li_messages,
             li_mrnb_messages.
  ENDLOOP.

ENDFORM.                    "f_submit_mrnb

*&---------------------------------------------------------------------*
*&      Form  f_write_joblog
*&---------------------------------------------------------------------*
FORM f_write_joblog USING p_ekko      TYPE zcl_smenh9qe01_revaluation=>t_ekko
                          p_ebelp     TYPE ebelp
                          pi_messages TYPE tab_bdcmsgcoll
                          pi_mrnb_msg TYPE zcl_smenh9qe01_revaluation=>t_mrnb_extract_table.
*                          pi_mrnb_msg TYPE bkk_tab_mesg.

  DATA: l_message TYPE text100,
        l_icon    TYPE c LENGTH 4.
*        li_messages          TYPE tab_bdcmsgcoll.

  FIELD-SYMBOLS: <msg>      TYPE bdcmsgcoll,
                 <mrnb_msg> TYPE zcl_smenh9qe01_revaluation=>t_mrnb_extract.
*                 <mrnb_msg>  TYPE mesg.

*** Write the header data
  SKIP 1.
  WRITE: / 'Purchase order:'(jl2), AT 22 p_ekko-ebeln,
         / 'PO line:'(jl3),        AT 22 p_ebelp,
         / 'Baseline date:'(jl4),  AT 22 p_ekko-bedat,
         / 'Payment terms:'(jl5),  AT 22 p_ekko-zterm.
  SKIP 1.

  IF pi_messages[] IS NOT INITIAL.
    LOOP AT pi_messages ASSIGNING <msg>.
*        WHERE NOT ( msgtyp = 'S' AND
*                    msgid = '00' AND
*                    msgnr = '344' AND
*                    msgv1 = 'RMMR1MRB' AND
*                    msgv2 = '1000' ).
      MESSAGE ID <msg>-msgid
              TYPE <msg>-msgtyp
              NUMBER <msg>-msgnr
              WITH <msg>-msgv1
                   <msg>-msgv2
                   <msg>-msgv3
                   <msg>-msgv4
              INTO l_message.
      CASE <msg>-msgtyp.
        WHEN 'S' OR 'I'. l_icon = icon_green_light.
        WHEN 'W'.        l_icon = icon_yellow_light.
        WHEN 'E'.        l_icon = icon_red_light.
        WHEN OTHERS.     l_icon = icon_light_out.
      ENDCASE.
      WRITE: / l_icon AS ICON, l_message.
    ENDLOOP.
    SKIP 1.
  ENDIF.

  IF pi_mrnb_msg[] IS NOT INITIAL.
    LOOP AT pi_mrnb_msg ASSIGNING <mrnb_msg>.
      CASE <mrnb_msg>-msgty.
        WHEN 'S' OR 'I'. l_icon = icon_green_light.
        WHEN 'W'.        l_icon = icon_yellow_light.
        WHEN 'E'.        l_icon = icon_red_light.
        WHEN OTHERS.     l_icon = icon_light_out.
      ENDCASE.
      WRITE: / l_icon AS ICON, <mrnb_msg>-text.
    ENDLOOP.
    SKIP 1.
  ENDIF.

*** Mark end of job with separator
  ULINE.

ENDFORM.                    "f_write_joblog

*&---------------------------------------------------------------------*
*&      Form  f_default_ranges
*&---------------------------------------------------------------------*
FORM f_default_ranges.

  FIELD-SYMBOLS: <unsez>     LIKE LINE OF s_unsez.

  CLEAR: s_unsez,
         s_unsez[].

  APPEND INITIAL LINE TO s_unsez ASSIGNING <unsez>.
  <unsez>-sign = 'I'.
  <unsez>-option = 'CP'.
  <unsez>-low = 'TDS*'.

  APPEND <unsez> TO s_unsez ASSIGNING <unsez>.
  <unsez>-low = 'DTL*'.

  APPEND <unsez> TO s_unsez ASSIGNING <unsez>.
  <unsez>-low = 'CAR*'.

ENDFORM.                    "f_default_ranges
***Start of Insert - XT18910 - DV5K982273 - CR224502 TK247533
*&---------------------------------------------------------------------*
*&      Form  F_PAST_PERIOD_DATE
*&---------------------------------------------------------------------*
* Defaulted 'Date of past period' values to be identical to what is
* defaulted in MRNB
*----------------------------------------------------------------------*
FORM f_past_period_date.
  DATA: l_year      TYPE i,
        l_month     TYPE i,
        l_year2(4)  TYPE n,
        l_month2(2) TYPE n,
        l_date2(2)  TYPE n VALUE '01'.

  l_year  = sy-datum(04).
  l_month = sy-datum+04(02).
  l_month = l_month - 02.
  IF l_month LE 0.
    l_month = l_month + 12.
    l_year  = l_year  - 1.
  ENDIF.
  l_year2 = l_year.
  l_month2 = l_month.
  WRITE l_year2  TO v_start.
  WRITE l_month2 TO v_start+04(02).
  WRITE l_date2  TO v_start+06(02).

  s_wedat[] = VALUE #( ( sign   = 'I'
                         option = 'BT'
                         low    = v_start
                         high   = sy-datum ) ).
*  s_wedat-sign = 'I'.
*  s_wedat-option = 'BT'.
*  s_wedat-low = v_start.
*  s_wedat-high = sy-datum.
*  APPEND s_wedat.
ENDFORM.                    " F_PAST_PERIOD_DATE
***End of Insert - XT18910 - DV5K982273 - CR224502 TK247533

*--> Start of insert DV5K9A05WW - Chuong Le - 2017/01/04
*&---------------------------------------------------------------------*
*&      Form  F_GET_PO_HEADER_TEXT
*&---------------------------------------------------------------------*
FORM f_get_po_header_text  USING    p_ebeln
                           CHANGING p_text.

    TYPES:
      BEGIN OF text_type,
        ebeln  TYPE ebeln,
        tdline TYPE tdline,
      END OF text_type.

    STATICS:
      lt_texttab TYPE SORTED TABLE OF text_type WITH UNIQUE KEY ebeln.

    DATA:
      lt_lines   TYPE tline_tab.

    CLEAR p_text.

    IF line_exists( lt_texttab[ ebeln = p_ebeln ] ).
      p_text = lt_texttab[ ebeln = p_ebeln ]-tdline.
    ELSE.
      CLEAR lt_lines[].
      "Read English text
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = 'F99'
          language                = 'E'
          name                    = CONV tdobname( p_ebeln )
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
            name                    = CONV tdobname( p_ebeln )
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
        p_text = lt_lines[ 1 ]-tdline.     "1st line only
      ENDIF.

      "Add to PO text table
      lt_texttab = VALUE #( BASE lt_texttab ( ebeln  = p_ebeln
                                              tdline = p_text ) ).
    ENDIF.

ENDFORM.
*<-- End of insert DV5K9A05WW - Chuong Le - 2017/01/04
