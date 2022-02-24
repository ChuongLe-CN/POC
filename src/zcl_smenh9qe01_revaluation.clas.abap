CLASS zcl_smenh9qe01_revaluation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_SMENH9QE01_REVALUATION
*"* do not include other source files here!!!
public section.

  types:
    BEGIN OF t_mrnb_data,
        remng TYPE rap_data-remng,
        meins TYPE rap_data-meins,
        werta TYPE rap_data-werta,
        wertn TYPE rap_data-wertn,
        wertd TYPE rap_data-wertd,
        waers TYPE rap_data-waers,
      END OF t_mrnb_data .
  types:
    BEGIN OF t_mrnb_extract.
            INCLUDE TYPE t_mrnb_data.
            INCLUDE TYPE mesg.
    TYPES:
      END OF t_mrnb_extract .
  types:
    t_mrnb_extract_table TYPE STANDARD TABLE OF t_mrnb_extract WITH EMPTY KEY .
  types:
    BEGIN OF t_ekko,
        ebeln TYPE ekko-ebeln,
        loekz TYPE ekko-loekz,
        lifnr TYPE ekko-lifnr,
        zterm TYPE ekko-zterm,
        bedat TYPE ekko-bedat,
        unsez TYPE ekko-unsez,
      END OF t_ekko .
  types:
    BEGIN OF t_ekpo,
        ebelp TYPE ekpo-ebelp,
        loekz TYPE ekpo-loekz,
        matnr TYPE ekpo-matnr,
        werks TYPE ekpo-werks,
        konnr TYPE ekpo-konnr,
        xersy TYPE ekpo-xersy,
        txz01 TYPE ekpo-txz01,
      END OF t_ekpo .

  class-methods SET_MESSAGE_FLAG .
  class-methods STORE_MESSAGES
    importing
      !IM_SEGTAB type MMRAP_TSEGTAB
    returning
      value(RE_EXIT) type XFELD .
  class-methods GET_MESSAGES
    changing
      !CH_MESSAGES type T_MRNB_EXTRACT_TABLE .
  class-methods ACCUMULATE_ALV_DATA
    importing
      !IM_EKKO type T_EKKO
      !IM_EKPO type T_EKPO
      !IM_BDC_MESSAGES type TAB_BDCMSGCOLL
      !IM_TEST type BOOLEAN_FLG .
  class-methods DISPLAY_ALV
    importing
      !IM_TEST type BOOLEAN_FLG .
protected section.
*"* protected components of class ZCL_SMENH9QE01_REVALUATION
*"* do not include other source files here!!!
PRIVATE SECTION.
*"* private components of class ZCL_SMENH9QE01_REVALUATION
*"* do not include other source files here!!!

*-> Start of change     CR269551-T325965    DV5K9A01KC
  TYPES:
    BEGIN OF t_alv_test,
      ebeln TYPE ekko-ebeln,
      ebelp TYPE ekpo-ebelp,
      bedat TYPE ekko-bedat,
      zterm TYPE ekko-zterm,
      lifnr TYPE ekko-lifnr,
      werks TYPE ekpo-werks,
      txz01 TYPE ekpo-txz01,
      eindt TYPE eket-eindt.
          INCLUDE TYPE t_mrnb_data.
  TYPES:
    text TYPE mesg-text,
    END OF t_alv_test .

  TYPES:
    BEGIN OF t_alv_data,
      ebeln TYPE ekko-ebeln,
      ebelp TYPE ekpo-ebelp,
      lifnr TYPE ekko-lifnr,
      werks TYPE ekpo-werks,
      eindt TYPE eket-eindt,
      text  TYPE mesg-text,
    END OF t_alv_data .

  CLASS-DATA:
    i_alv_data TYPE STANDARD TABLE OF t_alv_data WITH EMPTY KEY,
    i_alv_test TYPE STANDARD TABLE OF t_alv_test WITH EMPTY KEY.
*<- End of change     CR269551-T325965    DV5K9A01KC
  CONSTANTS c_memory_id TYPE char10 VALUE 'SMENH9QE' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_SMENH9QE01_REVALUATION IMPLEMENTATION.


  METHOD accumulate_alv_data.

    DATA: l_alv_data       TYPE t_alv_data,
          l_alv_test       TYPE t_alv_test,
          li_mrnb_messages TYPE t_mrnb_extract_table.

    l_alv_test-ebeln = im_ekko-ebeln.
    l_alv_test-ebelp = im_ekpo-ebelp.
    l_alv_test-lifnr = im_ekko-lifnr.
    l_alv_test-werks = im_ekpo-werks.
    l_alv_test-bedat = im_ekko-bedat.
    l_alv_test-zterm = im_ekko-zterm.
    l_alv_test-txz01 = im_ekpo-txz01.

    SELECT eindt
      FROM eket
      INTO TABLE @DATA(li_eket)
      UP TO 1 ROWS
      WHERE ebeln = @im_ekko-ebeln AND
            ebelp = @im_ekpo-ebelp.
    IF sy-subrc = 0.
      l_alv_test-eindt = li_eket[ 1 ]-eindt.
    ENDIF.

    get_messages( CHANGING ch_messages = li_mrnb_messages[] ).

    IF im_bdc_messages[] IS INITIAL AND li_mrnb_messages[] IS INITIAL.
      IF im_test IS INITIAL.
** Success process in update mode
        MOVE-CORRESPONDING l_alv_test TO l_alv_data.
        l_alv_data-text = text-s01.
        APPEND l_alv_data TO i_alv_data[].
      ENDIF.
    ELSE.
      LOOP AT im_bdc_messages[] ASSIGNING FIELD-SYMBOL(<bdc>).
*-> Start of comment     CR269551-T325965    DV5K9A01KC
*        CASE <bdc>-msgtyp.
*          WHEN 'S' OR 'I'. l_alv_data-icon = icon_green_light.
*          WHEN 'W'.        l_alv_data-icon = icon_yellow_light.
*          WHEN 'E' OR 'A'. l_alv_data-icon = icon_red_light.
*          WHEN OTHERS.     l_alv_data-icon = icon_light_out.
*        ENDCASE.
*<- End of comment     CR269551-T325965    DV5K9A01KC
        MESSAGE ID     <bdc>-msgid
                TYPE   <bdc>-msgtyp
                NUMBER <bdc>-msgnr
                WITH   <bdc>-msgv1
                       <bdc>-msgv2
                       <bdc>-msgv3
                       <bdc>-msgv4
                INTO   l_alv_test-text.
*-> Start of insert     CR269551-T325965    DV5K9A01KC
        IF ( im_test IS NOT INITIAL AND ( <bdc>-msgid EQ 'MRNB' AND
           ( <bdc>-msgnr EQ '018' OR <bdc>-msgnr EQ '025' OR <bdc>-msgnr EQ '026' ) ) ).
** Test mode execution
** Keep in the report the MRNB messages:
***  Credit memo item can be generated (026)
***  Invoice item can be generated     (025)
***  There is no difference amount for this goods receipt (delivery note .. (018)
          APPEND l_alv_test TO i_alv_test[].
        ENDIF.

        IF ( im_test IS INITIAL AND ( <bdc>-msgid EQ 'MRNB' AND <bdc>-msgnr EQ '018' ) ).
** Update mode execution
** Keep in the report the MRNB message:
***  There is no difference amount for this goods receipt (delivery note .. (018)
          MOVE-CORRESPONDING l_alv_test TO l_alv_data.
          APPEND l_alv_data TO i_alv_data[].
        ENDIF.
*<- End of insert     CR269551-T325965    DV5K9A01KC
      ENDLOOP.


      LOOP AT li_mrnb_messages[] ASSIGNING FIELD-SYMBOL(<mrnb>).
        l_alv_test-remng = <mrnb>-remng.
        l_alv_test-meins = <mrnb>-meins.
        l_alv_test-werta = <mrnb>-werta.
        l_alv_test-wertn = <mrnb>-wertn.
        l_alv_test-wertd = <mrnb>-wertd.
        l_alv_test-waers = <mrnb>-waers.
*-> Start of comment     CR269551-T325965    DV5K9A01KC
*        CASE <mrnb>-msgty.
*          WHEN 'S' OR 'I'. l_alv_data-icon = icon_green_light.
*          WHEN 'W'.        l_alv_data-icon = icon_yellow_light.
*          WHEN 'E' OR 'A'. l_alv_data-icon = icon_red_light.
*          WHEN OTHERS.     l_alv_data-icon = icon_light_out.
*        ENDCASE.
*<- End of comment     CR269551-T325965    DV5K9A01KC
*-> Start of insert     CR269551-T325965    DV5K9A01KC
        IF ( im_test IS NOT INITIAL AND ( <mrnb>-arbgb EQ 'MRNB' AND
             ( <mrnb>-txtnr EQ '18' OR <mrnb>-txtnr EQ '25' OR <mrnb>-txtnr EQ '26' ) ) ).
          l_alv_test-text = <mrnb>-text.
          APPEND l_alv_test TO i_alv_test[].
        ENDIF.

        IF ( im_test IS INITIAL AND ( <mrnb>-arbgb EQ 'MRNB' AND <mrnb>-txtnr EQ '18' ) ).
** Update mode execution
** Keep in the report the MRNB message:
***  There is no difference amount for this goods receipt (delivery note .. (018)
          MOVE-CORRESPONDING l_alv_test TO l_alv_data.
          l_alv_data-text = <mrnb>-text.
          APPEND l_alv_data TO i_alv_data[].
        ENDIF.
*<- End of insert     CR269551-T325965    DV5K9A01KC
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD display_alv.

    DATA: lo_alv           TYPE REF TO cl_salv_table,
          lo_alv_functions TYPE REF TO cl_salv_functions_list,
          lo_alv_columns   TYPE REF TO cl_salv_columns_table,
*          lo_column        TYPE REF TO cl_salv_column_table,
*          lo_events        TYPE REF TO cl_salv_events_table,
          lo_display       TYPE REF TO cl_salv_display_settings.

*-> Start of comment     CR269551-T325965    DV5K9A01KC
*    IF i_alv_data[] IS INITIAL.
*      RETURN.
*    ENDIF.
*<- End of comment     CR269551-T325965    DV5K9A01KC

    TRY.
*-> Start of insert     CR269551-T325965    DV5K9A01KC
        IF im_test IS INITIAL.
          cl_salv_table=>factory( IMPORTING r_salv_table   = lo_alv
                                  CHANGING  t_table        = i_alv_data[] ).
        ELSE.
          cl_salv_table=>factory( IMPORTING r_salv_table   = lo_alv
                                  CHANGING  t_table        = i_alv_test[] ).
        ENDIF.
*<- End of insert     CR269551-T325965    DV5K9A01KC
        lo_alv->set_screen_status( pfstatus      =  'STANDARD_FULLSCREEN'
                                   report        =  'SAPLSLVC_FULLSCREEN'
                                   set_functions = cl_salv_table=>c_functions_all ).

        lo_alv_functions = lo_alv->get_functions( ).
        lo_alv_functions->set_abc_analysis( abap_false ).
        lo_alv_functions->set_print_preview( abap_false ).
        lo_alv_functions->set_graphics( abap_false ).
        lo_alv_functions->set_export_send( abap_false ).
        lo_alv_functions->set_export_wordprocessor( abap_false ).
        lo_display = lo_alv->get_display_settings( ).
*-> Start of insert     CR269551-T325965    DV5K9A01KC
        IF im_test IS INITIAL.
          lo_display->set_list_header( text-h01 ).
        ELSE.
          lo_display->set_list_header( text-h02 ).
        ENDIF.
*<- End of insert     CR269551-T325965    DV5K9A01KC
        lo_display->set_striped_pattern( abap_true ).
        lo_alv_columns = lo_alv->get_columns( ).
        lo_alv_columns->set_optimize( abap_true ).

        CALL METHOD lo_alv->display.
      CATCH cx_salv_msg.
        RETURN.
    ENDTRY.

  ENDMETHOD.


METHOD get_messages.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH9QE01_REVALUATION=>GET_MESSAGES                 *
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
* Rob West                  2010/11/29          DV5K961206             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  REFRESH ch_messages.

  IMPORT i_msgtab TO ch_messages[] FROM MEMORY ID c_memory_id.
  DELETE FROM MEMORY ID c_memory_id.

ENDMETHOD.


METHOD set_message_flag.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH9QE01_REVALUATION=>SET_MESSAGE_FLAG             *
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
* Rob West                  2010/11/29          DV5K961206             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DELETE FROM MEMORY ID c_memory_id.

  EXPORT v_get_messages FROM abap_true TO MEMORY ID c_memory_id.

ENDMETHOD.


METHOD store_messages.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH9QE01_REVALUATION=>STORE_MESSAGES               *
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
* Rob West                  2010/11/29          DV5K961206             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  DATA: l_get_messages       TYPE xfeld,
        li_messages          TYPE t_mrnb_extract_table.
*        li_messages          TYPE bkk_tab_mesg.

*  FIELD-SYMBOLS: <mesg>      TYPE mesg,
*                 <seg>       LIKE LINE OF im_segtab.

  CLEAR re_exit.

  IMPORT v_get_messages TO l_get_messages FROM MEMORY ID c_memory_id.

  IF sy-subrc = 0 AND l_get_messages = 'X'.
    DELETE FROM MEMORY ID c_memory_id.
    li_messages[] = VALUE #( FOR <seg> IN im_segtab[]
                           ( remng = <seg>-remng
                             meins = <seg>-meins
                             werta = <seg>-werta
                             wertn = <seg>-wertn
                             wertd = <seg>-wertd
                             waers = <seg>-waers
                             zeile = <seg>-zeile
                             msgty = <seg>-msgty
                             text  = <seg>-text
                             arbgb = <seg>-arbgb
                             txtnr = <seg>-txtnr
                             msgv1 = <seg>-msgv1
                             msgv2 = <seg>-msgv2
                             msgv3 = <seg>-msgv3
                             msgv4 = <seg>-msgv4 ) ).
*    LOOP AT im_segtab ASSIGNING <seg>.
*      APPEND INITIAL LINE TO li_messages ASSIGNING <mesg>.
*      <mesg>-zeile = <seg>-zeile.
*      <mesg>-msgty = <seg>-msgty.
*      <mesg>-text  = <seg>-text.
*      <mesg>-arbgb = <seg>-arbgb.
*      <mesg>-txtnr = <seg>-txtnr.
*      <mesg>-msgv1 = <seg>-msgv1.
*      <mesg>-msgv2 = <seg>-msgv2.
*      <mesg>-msgv3 = <seg>-msgv3.
*      <mesg>-msgv4 = <seg>-msgv4.
*    ENDLOOP.
    EXPORT i_msgtab FROM li_messages[] TO MEMORY ID c_memory_id.
    re_exit = 'X'.
  ENDIF.

ENDMETHOD.
ENDCLASS.
