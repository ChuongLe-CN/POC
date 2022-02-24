REPORT  zfr_firpt7h6_ers_invoices.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZFR_FIRPT7H6_ERS_INVOICES                                *
* Title    :  Create Invoice Summary Report to PDF form                *
* Work Unit:  FI-RPT-7H6-001                                           *
* Created by: Rob West                                                 *
* Created on: January 2011                                             *
* Version:    1.0                                                      *
* T-Code:     ZSM_RPT7H6_WVIS                                          *
*                                                                      *
* Purpose:    Remove processed records from ZMM_FLCM_TDS.              *
*             Also remove associated Applicatiom Log entries           *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Justin Apurado(TXT21609)  2021/03/10           DV5K9A0TR9            *
*                                                                      *
* Short Description: RTSK0013736 DFCT0011202                           *
*   - Add logic to include India vendors                               *
*   - Add PO line item column after PO number                          *
*   - List each PIN associated to the CN Invoice No / PO Line Item     *
*   - Add new column 'Amount before tax' after 'Unit Price' column     *
*----------------------------------------------------------------------*
* GuoTai Chen(T182535)      2018/11/01           DV5K9A0GNZ            *
*                                                                      *
* Short Description: C372902-T403637                                   *
*   - Remove column "Network" from consultant version of report        *
*     and replace by consultant "PIN".                                 *
*----------------------------------------------------------------------*
* Justin Apurado(XT21609)   2017/04/18          DV5K9A07FB/DV5K9A07FB  *
*                                               DV5K9A08B2             *
*                                                                      *
* Short Description: CR333910 TK356363                                 *
*   - Added IT Consultant checkbox                                     *
*----------------------------------------------------------------------*
* Rob West                  2011/12/13          DV5K968325             *
*                                                                      *
* Short Description: Defect 674 FLCM Stabilization                     *
*                    Add Locomotive ID to ALV and csv from MSEG-WEMPF  *
*----------------------------------------------------------------------*
* Rob West                  2011/11/03          DVSK904592/DV5K967412  *
*                                                                      *
* Short Description: Defect 612 FLCM Stabilization                     *
*                    Set FCAT attribute to NO_OUT for new fields       *
*                         - SHKZG                                      *
*                         - XNEGP                                      *
*----------------------------------------------------------------------*
* Rob West                  2011/10/06          DV5K966969             *
*                                                                      *
* Short Description: Defect 584 FLCM Stabilization                     *
*                    1 - Remove MANDATORY for S_BLDAT                  *
*                    2 - Remove "Just Between" for S_CPUDT             *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K960962             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

TYPE-POOLS: abap,
            sscr,
            slis.

*--------------------------------------------------------------------*
*** Selection screen
DATA: v_bsik_1000     TYPE bsik,
      v_ekpo_1000     TYPE ekpo,
      v_ekko_1000     TYPE ekko,
      v_answer_1000   TYPE c,
      v_email_ok_1000 TYPE xfeld.

SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE text-bk1.
SELECT-OPTIONS: s_lifnr    FOR v_bsik_1000-lifnr OBLIGATORY,
                s_bukrs    FOR v_bsik_1000-bukrs OBLIGATORY,
                s_gjahr    FOR v_bsik_1000-gjahr OBLIGATORY,
                s_unsez    FOR v_ekko_1000-unsez,
*                s_bldat    FOR v_bsik_1000-bldat OBLIGATORY,   "DV5K966969
                s_bldat    FOR v_bsik_1000-bldat,           "DV5K966969
                s_matnr    FOR v_ekpo_1000-matnr,
                s_werks    FOR v_ekpo_1000-werks.
SELECTION-SCREEN END OF BLOCK bk1.
SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME TITLE text-bk2.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(65) text-s01 MODIF ID bk2.
SELECTION-SCREEN END OF LINE.
PARAMETERS: p_range        TYPE num03.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(60) text-s02  MODIF ID bk2.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS s_cpudt     FOR v_bsik_1000-cpudt.
SELECTION-SCREEN END OF BLOCK bk2.
SELECTION-SCREEN BEGIN OF BLOCK bk3 WITH FRAME TITLE text-bk3.
PARAMETERS: p_tds    TYPE xfeld AS CHECKBOX,
            p_dtl    TYPE xfeld AS CHECKBOX,
            p_car    TYPE xfeld AS CHECKBOX,
            p_email  TYPE xfeld AS CHECKBOX MODIF ID eml,
            p_consul TYPE xfeld AS CHECKBOX.          "I - XT21609 - DV5K9A07FB - CR333910 TK356363
SELECTION-SCREEN END OF BLOCK bk3.
*--------------------------------------------------------------------*

INCLUDE zfr_firpt7h6_ers_invoices_k01.     "Local Class definitions

DATA: o_extract TYPE REF TO lcl_extract,
      v_bsik    TYPE lcl_extract=>t_bsik,
      v_lifnr   TYPE lifnr,                                 "#EC NEEDED

      v_canada  TYPE xfeld,
      v_usa     TYPE xfeld,

      i_alv     TYPE zfi_firpt7h6_table_to_alv.
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
CONSTANTS: c_email TYPE adcp-deflt_comm VALUE 'INT',
           c_fax   TYPE adcp-deflt_comm VALUE 'FAX'.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
*--------------------------------------------------------------------*
INITIALIZATION.

  PERFORM f_restrict_intervals.

  AUTHORITY-CHECK OBJECT 'ZM_7H6MAIL'
           ID 'ACTVT' FIELD 'A9'.    "Send
  IF sy-subrc = 0.
    v_email_ok_1000 = abap_true.
  ELSE.
    v_email_ok_1000 = abap_false.
  ENDIF.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_matnr-low.
  PERFORM f_f4_matnr CHANGING s_matnr-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_matnr-high.
  PERFORM f_f4_matnr CHANGING s_matnr-high.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK bk2.
  IF p_range IS NOT INITIAL AND
     s_cpudt[] IS NOT INITIAL.
    MESSAGE e010(zz_flcm).  "Specify either number of days or date range
  ENDIF.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK bk3.

  IF sy-sysid = 'PRD' AND
     sy-uname(7) <> 'ESPOPER' AND
     p_email = abap_true AND
   ( sy-ucomm = 'ONLI' OR
     sy-ucomm = 'PRIN' OR
     sy-ucomm = 'SJOB' ).
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Send Email?'(c00)
        text_question         = 'Are you sure that you want to send email?'(c01)
        text_button_1         = 'Yes'(c02)
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'No'(c03)
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = abap_false
      IMPORTING
        answer                = v_answer_1000.

    IF v_answer_1000 = '2'.
      LEAVE TO SCREEN 1000.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'BK2'.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.
    IF v_email_ok_1000 = abap_false.
      IF screen-group1 = 'EML'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

*--------------------------------------------------------------------*
START-OF-SELECTION.

  IF s_cpudt[] IS INITIAL.
    IF p_range IS NOT INITIAL.
      s_cpudt-high = sy-datum - 1.
      s_cpudt-low = sy-datum - 1 - p_range.
      s_cpudt-sign = 'I'.
      s_cpudt-option = 'BT'.
      APPEND s_cpudt.                          "#EC
      CLEAR s_cpudt.
    ENDIF.
  ENDIF.

  IF s_cpudt[] IS NOT INITIAL AND sy-batch = abap_true.
    READ TABLE s_cpudt INDEX 1.                "#EC
    MESSAGE i012(zz_flcm) WITH s_cpudt-low s_cpudt-high.
    CLEAR s_cpudt.
  ENDIF.

  IF v_email_ok_1000 = abap_false.
    CLEAR p_email.
  ENDIF.
*--------------------------------------------------------------------*
*** Collect all BSIK data by vendor
  DO.
    v_bsik = lcl_extract=>get_next( ).
    IF v_bsik IS INITIAL.      "end of input
      EXIT.
    ENDIF.
    o_extract = lcl_extract=>locate_vendor( im_lifnr  = v_bsik-lifnr
                                            im_create = abap_true ).
    IF o_extract IS BOUND.
      CALL METHOD o_extract->collect_vendor_data
        EXPORTING
          im_bsik = v_bsik.
    ENDIF.
  ENDDO.

*--------------------------------------------------------------------*
*** Process report for each vendor object created
  DO.
    o_extract = lcl_extract=>vendor_by_index( sy-index ).
    IF o_extract IS NOT BOUND.
      EXIT.
    ENDIF.
    IF o_extract->data_to_report( ) = abap_true.
      IF v_canada = abap_false AND ( o_extract->vendor_country( ) = 'CA'
       OR o_extract->vendor_country( ) = 'IN' ).                              "I - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
        v_canada = abap_true.
      ENDIF.
      IF v_usa = abap_false AND o_extract->vendor_country( ) = 'US'.
        v_usa = abap_true.
      ENDIF.
      IF p_email = abap_true.
        IF o_extract->email_preferred( ) = abap_true.
          PERFORM f_send_email.
*        ELSE.            "D - XT21609 - DV5K9A07FB - CR333910 TK356363
        ENDIF.            "I - XT21609 - DV5K9A07FB - CR333910 TK356363
        IF o_extract->fax_preferred( ) = abap_true.
          PERFORM f_send_fax.
        ENDIF.
*        ENDIF.           "D - XT21609 - DV5K9A07FB - CR333910 TK356363
      ENDIF.
      IF sy-batch = abap_false.
        CALL METHOD o_extract->format_alv
          CHANGING
            ch_alv = i_alv[].
      ENDIF.
    ENDIF.
  ENDDO.

*--------------------------------------------------------------------*
*** Produce ALV report in online mode
  IF sy-batch = abap_false.
    IF i_alv[] IS NOT INITIAL.
      PERFORM f_display_alv.
    ELSE.
      MESSAGE i011(zz_flcm).  "No data to display
    ENDIF.
  ENDIF.

  EXIT.

*&---------------------------------------------------------------------*
*&      Form  f_restrict_intervals
*&---------------------------------------------------------------------*
FORM f_restrict_intervals.

*** Define the object to be passed to the RESTRICTION parameter
  DATA: l_restrict           TYPE sscr_restrict.

*** Auxiliary objects for filling RESTRICT
  DATA: l_opt_list TYPE sscr_opt_list,
        l_sscr_ass TYPE sscr_ass.

*** Restrict selection for selections to BETWEEN only
*  l_opt_list-name = 'JUST_BT'.                       "Removed DV5K966969
*  l_opt_list-options-bt = 'X'.                       "Removed DV5K966969
*  APPEND l_opt_list TO l_restrict-opt_list_tab.      "Removed DV5K966969
*** Restrict selection for selections to NO PATTERN only
  l_opt_list-name = 'NO_PATTERN'.
  CLEAR l_opt_list-options WITH 'X'.
  CLEAR: l_opt_list-options-cp,
         l_opt_list-options-np.
  APPEND l_opt_list TO l_restrict-opt_list_tab.

*  l_sscr_ass-kind = 'S'.                             "Removed DV5K966969
*  l_sscr_ass-sg_main = 'I'.                          "Removed DV5K966969
*  l_sscr_ass-sg_addy = 'N'.                          "Removed DV5K966969
*  l_sscr_ass-op_main = 'JUST_BT'.                    "Removed DV5K966969
*  l_sscr_ass-name = 'S_CPUDT'.                       "Removed DV5K966969
*  APPEND l_sscr_ass TO l_restrict-ass_tab.           "Removed DV5K966969

  l_sscr_ass-kind = 'S'.
  l_sscr_ass-sg_main = '*'.
  l_sscr_ass-sg_addy = '*'.
  l_sscr_ass-op_main = 'NO_PATTERN'.
  l_sscr_ass-name = 'S_BUKRS'.
  APPEND l_sscr_ass TO l_restrict-ass_tab.
  l_sscr_ass-name = 'S_GJAHR'.
  APPEND l_sscr_ass TO l_restrict-ass_tab.
  l_sscr_ass-name = 'S_WERKS'.
  APPEND l_sscr_ass TO l_restrict-ass_tab.
  l_sscr_ass-name = 'S_BLDAT'.
  APPEND l_sscr_ass TO l_restrict-ass_tab.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction = l_restrict.

ENDFORM.                    "f_restrict_intervals

*&---------------------------------------------------------------------*
*&      Form  F_SEND_EMAIL
*&---------------------------------------------------------------------*
FORM f_send_email .

  CONSTANTS: lc_crlf         TYPE c LENGTH 2 VALUE %_cr_lf.

  DATA: l_pdf            TYPE fpcontent,
        l_csv            TYPE string,
        l_text           TYPE soli,
        l_lifnr          TYPE lifnr,
        l_size           TYPE i,
        l_sent           TYPE xfeld,
        l_begda          TYPE c LENGTH 10,
        l_endda          TYPE c LENGTH 10,

        li_csv           TYPE soli_tab,
        li_text          TYPE soli_tab,
        li_address       TYPE soli_tab,
        li_pdf           TYPE solix_tab,
        li_recipients    TYPE lcl_extract=>t_recipient_table,
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
        li_recipients_it TYPE lcl_extract=>t_recipient_table,
        ls_recipients_it TYPE lcl_extract=>t_recipient,
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
        lo_send_request  TYPE REF TO cl_bcs,
        lo_document      TYPE REF TO cl_document_bcs,
*        lo_sender            TYPE REF TO cl_sapuser_bcs,
        lo_recipient     TYPE REF TO if_recipient_bcs,
        lo_bcs_exception TYPE REF TO cx_bcs.                "#EC NEEDED

  FIELD-SYMBOLS: <email>     LIKE LINE OF li_recipients.

  li_recipients[] = o_extract->get_recipients( ).
  IF li_recipients[] IS INITIAL.
    l_lifnr = o_extract->vendor_number( abap_true ).
    MESSAGE i013(zz_flcm) WITH l_lifnr.   "No recipients found
    RETURN.
  ENDIF.
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
  IF p_consul = abap_true.
    CLEAR ls_recipients_it.
    LOOP AT li_recipients ASSIGNING FIELD-SYMBOL(<lfs_recipients>)
        WHERE comm_method = c_email.
      ls_recipients_it-name    = <lfs_recipients>-name.
      ls_recipients_it-address = <lfs_recipients>-address.
      APPEND ls_recipients_it TO li_recipients_it.
      CLEAR ls_recipients_it.
    ENDLOOP.
    UNASSIGN <lfs_recipients>.
    DELETE li_recipients WHERE comm_method = c_email.

    IF li_recipients_it IS NOT INITIAL.
      PERFORM f_send_email_it TABLES li_recipients_it.
    ENDIF.
  ENDIF.

  READ TABLE li_recipients TRANSPORTING NO FIELDS
    WITH KEY comm_method = c_email.
  IF sy-subrc = 0.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    APPEND 'CN' TO li_text.
    APPEND INITIAL LINE TO li_text.

    li_address[] = o_extract->vendor_address( ).
    APPEND LINES OF li_address TO li_text.
    APPEND INITIAL LINE TO li_text.
    WRITE sy-datum TO l_text LEFT-JUSTIFIED.
    APPEND l_text TO li_text.
    APPEND INITIAL LINE TO li_text.
    l_text = o_extract->vendor_contact( ).
    APPEND l_text TO li_text.
    APPEND INITIAL LINE TO li_text.

    l_begda = o_extract->period_start_date( ).
    l_endda = o_extract->period_end_date( ).
    CONCATENATE 'Please find attached a summary of the invoices'(e01)
                'created during the following period:'(e02)
                l_begda 'to'(e03) l_endda INTO l_text SEPARATED BY space.
    l_size = strlen( l_text ).
    l_text+l_size(1) = '.'.
    APPEND l_text TO li_text.
    APPEND INITIAL LINE TO li_text.

    APPEND 'Please feel free to contact us if you have any questions.'(e04)
        TO li_text.
    APPEND INITIAL LINE TO li_text.
    APPEND 'Thank you,'(e05) TO li_text.
    APPEND INITIAL LINE TO li_text.

    APPEND 'CN Supply Management'(e06) TO li_text.
    APPEND 'P.O. Box 8103'(e07) TO li_text.
    APPEND 'Montreal, Quebec'(e08) TO li_text.
    APPEND 'H3C 3N3'(e09) TO li_text.
    APPEND INITIAL LINE TO li_text.
    APPEND 'sm_fuel@cn.ca'(e10) TO li_text.

    IF o_extract->vendor_country( ) = 'CA'.
      APPEND INITIAL LINE TO li_text.
      APPEND INITIAL LINE TO li_text.
      CLEAR: l_text,
             l_text(100) WITH '-'.
      APPEND l_text TO li_text.
      APPEND INITIAL LINE TO li_text.
      APPEND INITIAL LINE TO li_text.
      APPEND 'CN' TO li_text.
      APPEND INITIAL LINE TO li_text.

      APPEND LINES OF li_address TO li_text.
      APPEND INITIAL LINE TO li_text.
      WRITE sy-datum TO l_text LEFT-JUSTIFIED.
      APPEND l_text TO li_text.
      APPEND INITIAL LINE TO li_text.
      l_text = o_extract->vendor_contact( ).
      APPEND l_text TO li_text.
      APPEND INITIAL LINE TO li_text.

      l_begda = o_extract->period_start_date( ).
      l_endda = o_extract->period_end_date( ).
      CONCATENATE 'Veuillez trouver ci-joint un sommaire des factures'(e11)
                  'créées durant la période suivante :'(e12)
                  l_begda 'à'(e13) l_endda INTO l_text SEPARATED BY space.
      l_size = strlen( l_text ).
      l_text+l_size(1) = '.'.
      APPEND l_text TO li_text.
      APPEND INITIAL LINE TO li_text.

      APPEND 'N''hésitez pas à nous contacter si vous avez des questions.'(e14)
          TO li_text.
      APPEND INITIAL LINE TO li_text.
      APPEND 'Merci,'(e15) TO li_text.
      APPEND INITIAL LINE TO li_text.

      APPEND 'CN - Gestion des approvisionnements'(e16) TO li_text.
      APPEND 'CP 8103'(e17) TO li_text.
      APPEND 'Montréal, Québec'(e18) TO li_text.
      APPEND 'H3C 3N3'(e09) TO li_text.
      APPEND INITIAL LINE TO li_text.
      APPEND 'sm_fuel@cn.ca'(e10) TO li_text.
    ENDIF.

*--------------------------------------------------------------------*

    CALL METHOD o_extract->format_pdf
*    EXPORTING
*      im_preview = abap_true
      CHANGING
        ch_pdf = l_pdf.

    IF l_pdf IS NOT INITIAL.
      CALL METHOD cl_document_bcs=>xstring_to_solix
        EXPORTING
          ip_xstring = l_pdf
        RECEIVING
          rt_solix   = li_pdf.
    ENDIF.

*--------------------------------------------------------------------*

    CALL METHOD o_extract->format_csv
      CHANGING
        ch_csv = li_csv[].

    IF li_csv[] IS NOT INITIAL.
      CONCATENATE LINES OF li_csv INTO l_csv SEPARATED BY lc_crlf.
      CALL METHOD cl_document_bcs=>string_to_soli
        EXPORTING
          ip_string = l_csv
        RECEIVING
          rt_soli   = li_csv[].
    ENDIF.

*--------------------------------------------------------------------*

    TRY.
*     -------- create persistent send request ------------------------
        lo_send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document with attachment ---------------
*     create document from internal table with text
        CALL METHOD cl_document_bcs=>create_document
          EXPORTING
            i_type    = 'RAW'
            i_subject = 'Vendor Invoice Summary'(e00)
            i_text    = li_text[]
          RECEIVING
            result    = lo_document.

*     add attachment to document
*     BCS expects document content here e.g. from document upload
*     binary_content = ...
        CALL METHOD lo_document->add_attachment
          EXPORTING
            i_attachment_type    = 'PDF'
            i_attachment_subject = text-e00
            i_att_content_hex    = li_pdf[].

        CALL METHOD lo_document->add_attachment
          EXPORTING
            i_attachment_type    = 'CSV'
            i_attachment_subject = text-e00
            i_att_content_text   = li_csv[].

*     add document to send request
        CALL METHOD lo_send_request->set_document
          EXPORTING
            i_document = lo_document.

*     --------- set sender -------------------------------------------
*     note: this is necessary only if you want to set the sender
*           different from actual user (SY-UNAME). Otherwise sender is
*           set automatically with actual user.

*      sender = cl_sapuser_bcs=>create( sy-uname ).
*      CALL METHOD send_request->set_sender
*        EXPORTING i_sender = sender.

*     --------- add recipient (e-mail address) -----------------------
*     create recipient - please replace e-mail address !!!
        LOOP AT li_recipients ASSIGNING <email>
          WHERE comm_method = c_email.           "I - XT21609 - DV5K9A07FB - CR333910 TK356363
          CALL METHOD cl_cam_address_bcs=>create_internet_address
            EXPORTING
              i_address_string = <email>-address
            RECEIVING
              result           = lo_recipient.
*     add recipient with its respective attributes to send request
          CALL METHOD lo_send_request->add_recipient
            EXPORTING
              i_recipient = lo_recipient
              i_express   = 'X'.
        ENDLOOP.

*   ---------- send document ---------------------------------------
        CALL METHOD lo_send_request->send
          RECEIVING
            result = l_sent.

        IF l_sent = abap_false.
          MESSAGE i014(zz_flcm) WITH l_lifnr.   "Email error.
        ENDIF.

        COMMIT WORK.

* -----------------------------------------------------------
* *                     exception handling
* -----------------------------------------------------------
* * replace this very rudimentary exception handling
* * with your own one !!!
* -----------------------------------------------------------
      CATCH cx_bcs INTO lo_bcs_exception.
        MESSAGE i014(zz_flcm) WITH l_lifnr.   "Email error.
        EXIT.

    ENDTRY.
  ENDIF.                    "I - XT21609 - DV5K9A07FB - CR333910 TK356363
ENDFORM.                    "f_send_email
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
*&---------------------------------------------------------------------*
*&      Form  F_SEND_EMAIL_IT
*&---------------------------------------------------------------------*
FORM f_send_email_it TABLES li_recipients_it TYPE lcl_extract=>t_recipient_table.

  CONSTANTS: lc_crlf_it         TYPE c LENGTH 2 VALUE %_cr_lf.

  DATA: l_pdf_it            TYPE fpcontent,
        l_csv_it            TYPE string,
        l_text_it           TYPE soli,
        l_lifnr_it          TYPE lifnr,
        l_size_it           TYPE i,
        l_sent_it           TYPE xfeld,
        l_begda_it          TYPE c LENGTH 10,
        l_endda_it          TYPE c LENGTH 10,

        li_csv_it           TYPE soli_tab,
        li_text_it          TYPE soli_tab,
        li_address_it       TYPE soli_tab,
        li_pdf_it           TYPE solix_tab,
        lo_send_request_it  TYPE REF TO cl_bcs,
        lo_document_it      TYPE REF TO cl_document_bcs,
        lo_recipient_it     TYPE REF TO if_recipient_bcs,
        lo_bcs_exception_it TYPE REF TO cx_bcs.             "#EC NEEDED

  APPEND 'CN' TO li_text_it.
  APPEND INITIAL LINE TO li_text_it.

  li_address_it[] = o_extract->vendor_address( ).
  APPEND LINES OF li_address_it TO li_text_it.
  APPEND INITIAL LINE TO li_text_it.
  WRITE sy-datum TO l_text_it LEFT-JUSTIFIED.
  APPEND l_text_it TO li_text_it.
  APPEND INITIAL LINE TO li_text_it.
  l_text_it = o_extract->vendor_contact( ).
  APPEND l_text_it TO li_text_it.
  APPEND INITIAL LINE TO li_text_it.

  l_begda_it = o_extract->period_start_date( ).
  l_endda_it = o_extract->period_end_date( ).
  CONCATENATE text-e01 text-e02 l_begda_it text-e03 l_endda_it
    INTO l_text_it SEPARATED BY space.
  l_size_it = strlen( l_text_it ).
  l_text_it+l_size_it(1) = '.'.
  APPEND l_text_it TO li_text_it.
  APPEND INITIAL LINE TO li_text_it.

  APPEND 'Please feel free to contact us if you have any questions.'(e04)
      TO li_text_it.
  APPEND INITIAL LINE TO li_text_it.
  APPEND 'Thank you,'(e05) TO li_text_it.
  APPEND INITIAL LINE TO li_text_it.
*  APPEND 'CN IT People Services'(e19) TO li_text_it.                 "D - XT21609 - DV5K9A08B2 - CR333910 TK356363
  APPEND 'CN I&T Workforce Management'(e19) TO li_text_it.            "I - XT21609 - DV5K9A08B2 - CR333910 TK356363

  IF o_extract->vendor_country( ) = 'US'.
    APPEND '17641 SOUTH ASHLAND AVENUE, '(e20) TO li_text_it.
    APPEND 'Homewood, Illinois'(e21) TO li_text_it.
    APPEND 'United states of America'(e22) TO li_text_it.
    APPEND '60430'(e23) TO li_text_it.

  ELSEIF o_extract->vendor_country( ) = 'CA'.
    APPEND INITIAL LINE TO li_text_it.
    APPEND INITIAL LINE TO li_text_it.
    CLEAR: l_text_it,
           l_text_it(100) WITH '-'.
    APPEND l_text_it TO li_text_it.
    APPEND INITIAL LINE TO li_text_it.
    APPEND INITIAL LINE TO li_text_it.
    APPEND 'CN' TO li_text_it.
    APPEND INITIAL LINE TO li_text_it.

    APPEND LINES OF li_address_it TO li_text_it.
    APPEND INITIAL LINE TO li_text_it.
    WRITE sy-datum TO l_text_it LEFT-JUSTIFIED.
    APPEND l_text_it TO li_text_it.
    APPEND INITIAL LINE TO li_text_it.
    l_text_it = o_extract->vendor_contact( ).
    APPEND l_text_it TO li_text_it.
    APPEND INITIAL LINE TO li_text_it.

    l_begda_it = o_extract->period_start_date( ).
    l_endda_it = o_extract->period_end_date( ).
    CONCATENATE text-e11 text-e12 l_begda_it text-e13 l_endda_it
      INTO l_text_it SEPARATED BY space.
    l_size_it = strlen( l_text_it ).
    l_text_it+l_size_it(1) = '.'.
    APPEND l_text_it TO li_text_it.
    APPEND INITIAL LINE TO li_text_it.

    APPEND 'N''hésitez pas à nous contacter si vous avez des questions.'(e14)
        TO li_text_it.
    APPEND INITIAL LINE TO li_text_it.
    APPEND 'Merci,'(e15) TO li_text_it.
    APPEND INITIAL LINE TO li_text_it.

*    APPEND 'I&T Workforce Management'(e19) TO li_text_it.                    "D - XT21609 - DV5K9A08B2 - CR333910 TK356363
    APPEND text-e28 TO li_text_it.                                            "I - XT21609 - DV5K9A08B2 - CR333910 TK356363
    APPEND '935 de la Gauchetière'(e24) TO li_text_it.
    APPEND 'Montreal, Quebec'(e25) TO li_text_it.
    APPEND 'H3B 2M9'(e26) TO li_text_it.
  ENDIF.

  APPEND INITIAL LINE TO li_text_it.
  APPEND 'it-consultant-administration@cn.ca'(e27) TO li_text_it.

  CALL METHOD o_extract->format_pdf_it
    CHANGING
      ch_pdf_it = l_pdf_it.

  IF l_pdf_it IS NOT INITIAL.
    CALL METHOD cl_document_bcs=>xstring_to_solix
      EXPORTING
        ip_xstring = l_pdf_it
      RECEIVING
        rt_solix   = li_pdf_it.
  ENDIF.

  CALL METHOD o_extract->format_csv_it
    CHANGING
      ch_csv_it = li_csv_it[].

  IF li_csv_it[] IS NOT INITIAL.
    CONCATENATE LINES OF li_csv_it INTO l_csv_it SEPARATED BY lc_crlf_it.
    CALL METHOD cl_document_bcs=>string_to_soli
      EXPORTING
        ip_string = l_csv_it
      RECEIVING
        rt_soli   = li_csv_it[].
  ENDIF.

  TRY.
*     -------- create persistent send request ------------------------
      lo_send_request_it = cl_bcs=>create_persistent( ).

*     -------- create and set document with attachment ---------------
*     create document from internal table with text
      CALL METHOD cl_document_bcs=>create_document
        EXPORTING
          i_type    = 'RAW'
          i_subject = 'Vendor Invoice Summary'(e00)
          i_text    = li_text_it[]
        RECEIVING
          result    = lo_document_it.

*     add attachment to document
*     BCS expects document content here e.g. from document upload
*     binary_content = ...
      CALL METHOD lo_document_it->add_attachment
        EXPORTING
          i_attachment_type    = 'PDF'
          i_attachment_subject = text-e00
          i_att_content_hex    = li_pdf_it[].

      CALL METHOD lo_document_it->add_attachment
        EXPORTING
          i_attachment_type    = 'CSV'
          i_attachment_subject = text-e00
          i_att_content_text   = li_csv_it[].

*     add document to send request
      CALL METHOD lo_send_request_it->set_document
        EXPORTING
          i_document = lo_document_it.

*     --------- add recipient (e-mail address) -----------------------
      LOOP AT li_recipients_it ASSIGNING FIELD-SYMBOL(<lfs_email_it>).
        CALL METHOD cl_cam_address_bcs=>create_internet_address
          EXPORTING
            i_address_string = <lfs_email_it>-address
          RECEIVING
            result           = lo_recipient_it.
*     add recipient with its respective attributes to send request
        CALL METHOD lo_send_request_it->add_recipient
          EXPORTING
            i_recipient = lo_recipient_it
            i_express   = 'X'.
      ENDLOOP.

*   ---------- send document ---------------------------------------
      CALL METHOD lo_send_request_it->send
        RECEIVING
          result = l_sent_it.

      IF l_sent_it = abap_false.
        MESSAGE i014(zz_flcm) WITH l_lifnr_it.   "Email error.
      ENDIF.

      COMMIT WORK.

    CATCH cx_bcs INTO lo_bcs_exception_it.
      MESSAGE i014(zz_flcm) WITH l_lifnr_it.   "Email error.
      EXIT.

  ENDTRY.

ENDFORM.                    "f_send_email_it
*&---------------------------------------------------------------------*
*&      Form  F_SEND_FAX_IT
*&---------------------------------------------------------------------*
FORM f_send_fax_it TABLES li_recipients_it TYPE lcl_extract=>t_recipient_table.

  DATA: l_pdf            TYPE fpcontent,
        l_lifnr          TYPE lifnr,
        l_sent           TYPE xfeld,
        l_land1          TYPE lfa1-land1,
        l_name           TYPE so_obj_des,

        li_pdf           TYPE solix_tab,
        lo_send_request  TYPE REF TO cl_bcs,
        lo_document      TYPE REF TO cl_document_bcs,       "#EC NEEDED
        lo_recipient     TYPE REF TO if_recipient_bcs,
        lo_bcs_exception TYPE REF TO cx_bcs.                "#EC NEEDED

  FIELD-SYMBOLS: <number>    TYPE ad_fxnmbr.

  l_land1 = o_extract->vendor_country( ).

  CALL METHOD o_extract->format_pdf
    CHANGING
      ch_pdf = l_pdf.

  IF l_pdf IS NOT INITIAL.
    CALL METHOD cl_document_bcs=>xstring_to_solix
      EXPORTING
        ip_xstring = l_pdf
      RECEIVING
        rt_solix   = li_pdf.
  ENDIF.

  LOOP AT li_recipients_it ASSIGNING FIELD-SYMBOL(<lfs_fax>).
    ASSIGN <lfs_fax>-address TO <number> CASTING.
    CONCATENATE '>>> ATTN:' <lfs_fax>-name INTO l_name SEPARATED BY space.
    TRY.
*     -------- create persistent send request ------------------------
        lo_send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document with attachment ---------------
*     create document from internal table with text
        CALL METHOD cl_document_bcs=>create_document
          EXPORTING
            i_type    = 'PDF'
            i_subject = l_name
            i_hex     = li_pdf[]
          RECEIVING
            result    = lo_document.

*     --------- add recipient (e-mail address) -----------------------
*     create recipient - please replace e-mail address !!!
        CALL METHOD cl_cam_address_bcs=>create_fax_address
          EXPORTING
            i_country = l_land1
            i_number  = <number>
          RECEIVING
            result    = lo_recipient.
**     add recipient with its respective attributes to send request
        CALL METHOD lo_send_request->add_recipient
          EXPORTING
            i_recipient = lo_recipient
            i_express   = 'X'.

*   ---------- send document ---------------------------------------
        CALL METHOD lo_send_request->send
          RECEIVING
            result = l_sent.

        IF l_sent = abap_false.
          MESSAGE i016(zz_flcm) WITH l_lifnr.   "Fax error.
        ENDIF.

        COMMIT WORK.

      CATCH cx_bcs INTO lo_bcs_exception.
        MESSAGE i016(zz_flcm) WITH l_lifnr.   "Fax error.
        EXIT.
    ENDTRY.
  ENDLOOP.
  UNASSIGN <lfs_fax>.
ENDFORM.                    "f_send_fax_it
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
*&---------------------------------------------------------------------*
*&      Form  f_send_fax
*&---------------------------------------------------------------------*
FORM f_send_fax.

  DATA: l_pdf            TYPE fpcontent,
        l_lifnr          TYPE lifnr,
        l_sent           TYPE xfeld,
        l_land1          TYPE lfa1-land1,
        l_name           TYPE so_obj_des,

        li_pdf           TYPE solix_tab,
        li_recipients    TYPE lcl_extract=>t_recipient_table,
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
        li_recipients_it TYPE lcl_extract=>t_recipient_table,
        ls_recipients_it TYPE lcl_extract=>t_recipient,
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
        lo_send_request  TYPE REF TO cl_bcs,
        lo_document      TYPE REF TO cl_document_bcs,       "#EC NEEDED
*        lo_sender            TYPE REF TO cl_sapuser_bcs,
        lo_recipient     TYPE REF TO if_recipient_bcs,
        lo_bcs_exception TYPE REF TO cx_bcs.                "#EC NEEDED

  FIELD-SYMBOLS: <fax>    LIKE LINE OF li_recipients,
                 <number> TYPE ad_fxnmbr.

  li_recipients[] = o_extract->get_recipients( ).
  IF li_recipients[] IS INITIAL.
    l_lifnr = o_extract->vendor_number( abap_true ).
    MESSAGE i015(zz_flcm) WITH l_lifnr.   "No fax recipients found
    RETURN.
  ENDIF.
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
  IF p_consul = abap_true.
    CLEAR ls_recipients_it.
    LOOP AT li_recipients ASSIGNING FIELD-SYMBOL(<lfs_recipients>)
        WHERE comm_method = c_fax.           "I - XT21609 - DV5K9A07FB - CR333910 TK356363
      ls_recipients_it-name    = <lfs_recipients>-name.
      ls_recipients_it-address = <lfs_recipients>-address.
      APPEND ls_recipients_it TO li_recipients_it.
      CLEAR ls_recipients_it.
    ENDLOOP.
    UNASSIGN <lfs_recipients>.
    DELETE li_recipients WHERE comm_method = c_fax.

    IF li_recipients_it IS NOT INITIAL.
      PERFORM f_send_fax_it TABLES li_recipients_it.
    ENDIF.
  ENDIF.

  READ TABLE li_recipients TRANSPORTING NO FIELDS
    WITH KEY comm_method = c_fax.
  IF sy-subrc = 0.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    l_land1 = o_extract->vendor_country( ).

*--------------------------------------------------------------------*

    CALL METHOD o_extract->format_pdf
      CHANGING
        ch_pdf = l_pdf.

    IF l_pdf IS NOT INITIAL.
      CALL METHOD cl_document_bcs=>xstring_to_solix
        EXPORTING
          ip_xstring = l_pdf
        RECEIVING
          rt_solix   = li_pdf.
    ENDIF.

*--------------------------------------------------------------------*

    LOOP AT li_recipients ASSIGNING <fax>
      WHERE comm_method = c_fax.                  "I - XT21609 - DV5K9A07FB - CR333910 TK356363
      ASSIGN <fax>-address TO <number> CASTING.
      CONCATENATE '>>> ATTN:' <fax>-name INTO l_name SEPARATED BY space.
      TRY.
*     -------- create persistent send request ------------------------
          lo_send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document with attachment ---------------
*     create document from internal table with text
          CALL METHOD cl_document_bcs=>create_document
            EXPORTING
              i_type    = 'PDF'
              i_subject = l_name
              i_hex     = li_pdf[]
            RECEIVING
              result    = lo_document.

*     --------- add recipient (e-mail address) -----------------------
*     create recipient - please replace e-mail address !!!
          CALL METHOD cl_cam_address_bcs=>create_fax_address
            EXPORTING
              i_country = l_land1
              i_number  = <number>
            RECEIVING
              result    = lo_recipient.
**     add recipient with its respective attributes to send request
          CALL METHOD lo_send_request->add_recipient
            EXPORTING
              i_recipient = lo_recipient
              i_express   = 'X'.

*   ---------- send document ---------------------------------------
          CALL METHOD lo_send_request->send
            RECEIVING
              result = l_sent.

          IF l_sent = abap_false.
            MESSAGE i016(zz_flcm) WITH l_lifnr.   "Fax error.
          ENDIF.

          COMMIT WORK.

        CATCH cx_bcs INTO lo_bcs_exception.
          MESSAGE i016(zz_flcm) WITH l_lifnr.   "Fax error.
          EXIT.
      ENDTRY.
    ENDLOOP.
  ENDIF.                            "I - XT21609 - DV5K9A07FB - CR333910 TK356363
ENDFORM.                    "f_send_fax

*&---------------------------------------------------------------------*
*&      Form  f_display_alv
*&---------------------------------------------------------------------*
FORM f_display_alv.

  DATA: l_layout    TYPE slis_layout_alv,
        l_sort      TYPE slis_sortinfo_alv,
        li_sort     TYPE slis_t_sortinfo_alv,
        li_fieldcat TYPE slis_t_fieldcat_alv.

  PERFORM f_build_fieldcat CHANGING li_fieldcat[].

  l_layout-colwidth_optimize = abap_true.
  l_layout-zebra = abap_true.
  l_layout-no_sumchoice = abap_true.
  l_layout-f2code = 'DBL_CLICK'.

  l_sort-spos = 1.
  l_sort-fieldname = 'LIFNR'.
  l_sort-up = abap_true.
  l_sort-subtot = abap_true.
  APPEND l_sort TO li_sort.

  l_sort-spos = 2.
  l_sort-fieldname = 'CURRENCY'.
  l_sort-up = abap_true.
  l_sort-subtot = abap_true.
  APPEND l_sort TO li_sort.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = 'ZFR_FIRPT7H6_ERS_INVOICES'
      i_callback_user_command = 'F_ALV_CALLBACK'
      is_layout               = l_layout
      it_fieldcat             = li_fieldcat[]
      it_sort                 = li_sort[]
    TABLES
      t_outtab                = i_alv[].

ENDFORM.                    "f_display_alv

*&---------------------------------------------------------------------*
*&      Form  f_build_fieldcat
*&---------------------------------------------------------------------*
FORM f_build_fieldcat CHANGING pi_fcat TYPE slis_t_fieldcat_alv.

***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
  CONSTANTS:
    lc_amount_before_tax TYPE string VALUE 'AMOUNT_BEFORE_TAX',
    lc_po_item_line      TYPE string VALUE 'EBELP'.
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9

  FIELD-SYMBOLS: <fcat>      TYPE slis_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZFI_FIRPT7H6_EXTRACT_TO_ALV'
    CHANGING
      ct_fieldcat      = pi_fcat[].

  LOOP AT pi_fcat ASSIGNING <fcat>
      WHERE fieldname = 'BUKRS' OR
            fieldname = 'GJAHR' OR
            fieldname = 'EBELP' OR
            fieldname = lc_amount_before_tax OR             "I - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
            fieldname = 'FUEL_FEES' OR
            fieldname = 'ERS_INDICATOR' OR
            fieldname = 'SHKZG' OR                          "DVSK904592
            fieldname = 'XNEGP' OR                          "DVSK904592
            fieldname = 'KNUMV'.
    <fcat>-no_out = abap_true.

***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
    IF ( <fcat>-fieldname = lc_po_item_line OR
         <fcat>-fieldname = lc_amount_before_tax ) AND
     p_consul = abap_true.

      <fcat>-no_out = abap_false.

    ENDIF. " IF ( <fcat>-fieldname = lc_po_item_line OR
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9

  ENDLOOP.

  IF v_canada = abap_false.  "Hide if no Canadian data
    LOOP AT pi_fcat ASSIGNING <fcat>
        WHERE fieldname = 'GST_HST' OR
              fieldname = 'PST_QST' OR
              fieldname = 'EXCISE_TAX'.
      <fcat>-no_out = abap_true.
    ENDLOOP.
  ENDIF.

  IF v_usa = abap_false.     "Hide if no US data
    LOOP AT pi_fcat ASSIGNING <fcat>
        WHERE fieldname = 'FEDERAL_SPILL' OR
              fieldname = 'FEDERAL_LUST' OR
              fieldname = 'US_TAX'.
      <fcat>-no_out = abap_true.
    ENDLOOP.
  ENDIF.

***start of insert T182535 / DV5K9A0GNZ  C372902-T403637***
  IF p_consul = abap_true.
    IF line_exists( pi_fcat[ fieldname = 'PERNR' ] ).
      pi_fcat[ fieldname = 'PERNR' ]-no_out = abap_false.
    ENDIF.
    IF line_exists( pi_fcat[ fieldname = 'NETWORK' ] ).
      pi_fcat[ fieldname = 'NETWORK' ]-no_out = abap_true.
    ENDIF.
  ELSE.
    IF line_exists( pi_fcat[ fieldname = 'PERNR' ] ).
      pi_fcat[ fieldname = 'PERNR' ]-no_out = abap_true.
    ENDIF.
    IF line_exists( pi_fcat[ fieldname = 'NETWORK' ] ).
      pi_fcat[ fieldname = 'NETWORK' ]-no_out = abap_false.
    ENDIF.
  ENDIF.
***end of insert T182535 / DV5K9A0GNZ  C372902-T403637***

  LOOP AT pi_fcat ASSIGNING <fcat>.
    CASE <fcat>-fieldname.
      WHEN 'GST_HST'.
        <fcat>-seltext_l = <fcat>-seltext_m = <fcat>-seltext_s =
                           <fcat>-reptext_ddic = text-t02.
        <fcat>-do_sum = abap_true.
      WHEN 'PST_QST'.
        <fcat>-seltext_l = <fcat>-seltext_m = <fcat>-seltext_s =
                           <fcat>-reptext_ddic = text-t03.
        <fcat>-do_sum = abap_true.
      WHEN 'EXCISE_TAX'.
        <fcat>-seltext_l = <fcat>-seltext_m = <fcat>-reptext_ddic = text-m04.
        <fcat>-seltext_s = text-t04.
        <fcat>-do_sum = abap_true.
      WHEN 'FEDERAL_SPILL'.
        <fcat>-seltext_l = <fcat>-seltext_m = <fcat>-seltext_s =
                           <fcat>-reptext_ddic = text-t05.
        <fcat>-do_sum = abap_true.
      WHEN 'FEDERAL_LUST'.
        <fcat>-seltext_l = <fcat>-seltext_m = <fcat>-seltext_s =
                           <fcat>-reptext_ddic = text-t06.
        <fcat>-do_sum = abap_true.
      WHEN 'US_TAX'.
        <fcat>-seltext_l = <fcat>-seltext_m = <fcat>-seltext_s =
                           <fcat>-reptext_ddic = text-t07.
        <fcat>-do_sum = abap_true.
      WHEN 'LOCAL_FUEL_FEES'.
        <fcat>-seltext_l = <fcat>-reptext_ddic = text-l08.
        <fcat>-seltext_m = text-m08.
        <fcat>-seltext_s = text-t08.
        <fcat>-do_sum = abap_true.
      WHEN 'STCD1'.
        <fcat>-seltext_l = <fcat>-seltext_m = <fcat>-seltext_s =
                           <fcat>-reptext_ddic = text-t09.
      WHEN 'STCD2'.
        <fcat>-seltext_l = <fcat>-seltext_m = <fcat>-seltext_s =
                           <fcat>-reptext_ddic = text-t10.
      WHEN 'DELIVERY_LOCN'.
        <fcat>-seltext_l = <fcat>-reptext_ddic = text-l11.
        <fcat>-seltext_m = text-m11.
        <fcat>-seltext_s = text-t11.
      WHEN 'ZTDS_BOL_NBR'.
        <fcat>-seltext_l = <fcat>-reptext_ddic = text-l12.
        <fcat>-seltext_m = text-m12.
        <fcat>-seltext_s = text-t12.
      WHEN 'EBELN'.
        <fcat>-seltext_l = <fcat>-reptext_ddic = text-l13.
        <fcat>-seltext_m = text-m13.
        <fcat>-seltext_s = text-t13.
      WHEN 'WEMPF'.
        <fcat>-seltext_l = <fcat>-reptext_ddic = text-l14.  "DV5K968325
        <fcat>-seltext_m = text-m14.                        "DV5K968325
        <fcat>-seltext_s = text-t14.                        "DV5K968325
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      WHEN 'PERIOD_FROM'.
        <fcat>-seltext_l = <fcat>-reptext_ddic = text-l15.
        <fcat>-seltext_m = text-l15.
        <fcat>-seltext_s = text-t15.
      WHEN 'PERIOD_TO'.
        <fcat>-seltext_l = <fcat>-reptext_ddic = text-l16.
        <fcat>-seltext_m = text-l16.
        <fcat>-seltext_s = text-t16.
      WHEN 'PERNR'. " DV5K9A0GNZ +
        <fcat>-seltext_l = <fcat>-seltext_m = <fcat>-seltext_s =
                           <fcat>-reptext_ddic = text-t17.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
      WHEN 'AMOUNT_BEFORE_TAX'.
        <fcat>-seltext_l = <fcat>-seltext_m = <fcat>-seltext_s =
                           <fcat>-reptext_ddic = text-t18.
        <fcat>-do_sum = abap_true.
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
    ENDCASE.
  ENDLOOP.

ENDFORM.                    "f_build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  f_alv_callback
*&---------------------------------------------------------------------*
FORM f_alv_callback USING p_ucomm    TYPE syucomm           "#EC CALLED
                          p_selfield TYPE slis_selfield.

  FIELD-SYMBOLS: <data>      TYPE zfi_firpt7h6_extract_to_alv.

  IF p_ucomm = 'DBL_CLICK' AND
     p_selfield-tabindex > 0 AND
     p_selfield-fieldname = 'LIFNR' OR
   ( p_selfield-fieldname = 'BLDAT' OR
     p_selfield-fieldname = 'ZTDS_BOL_NBR' OR
     p_selfield-fieldname = 'BELNR' OR
     p_selfield-fieldname = 'EBELN' OR
     p_selfield-fieldname = 'KONNR' ).
    "valid callback - keep on going
  ELSE.
    RETURN.    "otherwise get out
  ENDIF.

  READ TABLE i_alv ASSIGNING <data> INDEX p_selfield-tabindex.
  IF sy-subrc > 0.
    RETURN.
  ENDIF.

  CASE p_selfield-fieldname.
    WHEN 'LIFNR'.
      PERFORM f_display_bdms USING <data>-lifnr.
    WHEN 'BLDAT'.
      PERFORM f_call_fb03  USING <data>.
    WHEN 'ZTDS_BOL_NBR'.
      PERFORM f_call_me23n USING <data>.
    WHEN 'BELNR'.
      PERFORM f_call_fb03  USING <data>.
    WHEN 'EBELN'.
      PERFORM f_call_me23n USING <data>.
    WHEN 'KONNR'.
      PERFORM f_call_me33k USING <data>.
  ENDCASE.

ENDFORM.                    "f_alv_callback

*&---------------------------------------------------------------------*
*&      Form  f_display_bdms
*&---------------------------------------------------------------------*
FORM f_display_bdms USING p_lifnr TYPE lifnr.

  DATA: lo_vendor TYPE REF TO lcl_extract,
        l_objkey  TYPE bapibds01-objkey,
        l_docid   TYPE bapibds01-doc_id,
        li_conn   TYPE STANDARD TABLE OF bds_conn00.

  FIELD-SYMBOLS: <conn>      TYPE bds_conn00.

  lo_vendor = lcl_extract=>locate_vendor( p_lifnr ).
  IF lo_vendor IS BOUND.
    l_objkey = lo_vendor->get_bdms_key( ).
    IF l_objkey IS INITIAL.
      CALL METHOD lo_vendor->format_pdf( im_preview = abap_true ).
    ELSE.
      SELECT *
        FROM bds_conn00
        INTO TABLE li_conn
        WHERE object_key = l_objkey.
      IF sy-dbcnt > 0.
        READ TABLE li_conn ASSIGNING <conn> INDEX sy-dbcnt.
        IF <conn> IS ASSIGNED.
          l_docid = <conn>-loio_class.
          l_docid+10 = <conn>-loio_id.
          CALL FUNCTION 'BDS_DOCUMENT_DISPLAY'
            EXPORTING
              doc_id = l_docid.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "f_display_bdms

*&---------------------------------------------------------------------*
*&      Form  f_call_fb03
*&---------------------------------------------------------------------*
FORM f_call_fb03 USING p_data TYPE zfi_firpt7h6_extract_to_alv.

  STATICS: st_authorized     TYPE sy-subrc.

  IF st_authorized = 0.
    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'FB03'
      EXCEPTIONS
        ok     = 1
        not_ok = 2
        OTHERS = 3.
    st_authorized = sy-subrc.
  ENDIF.

  IF st_authorized = 1.
    SET PARAMETER ID: 'BLN' FIELD p_data-belnr,
                      'BUK' FIELD p_data-bukrs,
                      'GJR' FIELD p_data-gjahr.

    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    "f_call_fb03

*&---------------------------------------------------------------------*
*&      Form  f_call_me23n
*&---------------------------------------------------------------------*
FORM f_call_me23n USING p_data TYPE zfi_firpt7h6_extract_to_alv.

  STATICS: st_authorized     TYPE sy-subrc.

  IF st_authorized = 0.
    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'ME23N'
      EXCEPTIONS
        ok     = 1
        not_ok = 2
        OTHERS = 3.
    st_authorized = sy-subrc.
  ENDIF.

  IF st_authorized = 1.
    SET PARAMETER ID 'BES' FIELD p_data-ebeln.
    CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    "f_call_me23n

*&---------------------------------------------------------------------*
*&      Form  f_call_me33k
*&---------------------------------------------------------------------*
FORM f_call_me33k USING p_data TYPE zfi_firpt7h6_extract_to_alv.

  DATA: li_bdcdata           TYPE STANDARD TABLE OF bdcdata.

  STATICS: st_authorized     TYPE sy-subrc.

  FIELD-SYMBOLS: <bdcdata>   TYPE bdcdata.

  IF st_authorized = 0.
    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'ME33K'
      EXCEPTIONS
        ok     = 1
        not_ok = 2
        OTHERS = 3.
    st_authorized = sy-subrc.
  ENDIF.

  IF st_authorized = 1.
    APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
    <bdcdata>-program = 'SAPMM06E'.
    <bdcdata>-dynpro = '0205'.
    <bdcdata>-dynbegin = 'X'.
    APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
    <bdcdata>-fnam = 'BDC_OKCODE'.
    <bdcdata>-fval = '=AB'.
    APPEND INITIAL LINE TO li_bdcdata ASSIGNING <bdcdata>.
    <bdcdata>-fnam = 'RM06E-EVRTN'.
    <bdcdata>-fval = p_data-konnr.
    CALL TRANSACTION 'ME33K' USING li_bdcdata
                             MODE 'E'.
  ENDIF.

ENDFORM.                    "f_call_me33k

*&---------------------------------------------------------------------*
*&      Form  F_F4_MATNR
*&---------------------------------------------------------------------*
FORM f_f4_matnr CHANGING p_matnr.                           "#EC *

  TYPES: BEGIN OF lt_matnr_parm,
           matnr TYPE matnr,
           maktx TYPE maktx,
         END OF lt_matnr_parm.

  DATA: li_return    TYPE STANDARD TABLE OF ddshretval,
        li_parms     TYPE STANDARD TABLE OF zmm_flcm_parms,
        l_return     TYPE ddshretval,
        l_matnr_parm TYPE lt_matnr_parm.

  STATICS: sti_matnr_parms   TYPE STANDARD TABLE OF lt_matnr_parm.

  FIELD-SYMBOLS: <parm>      TYPE zmm_flcm_parms.

*--------------------------------------------------------------------*
  IF sti_matnr_parms[] IS INITIAL.
    SELECT *
      FROM zmm_flcm_parms
      INTO TABLE li_parms
      WHERE progname = sy-repid AND
            zparm_nm = 'S_MATNR'.
    IF sy-subrc = 0.
      LOOP AT li_parms ASSIGNING <parm>.
        l_matnr_parm-matnr = <parm>-zval_from.
        TRANSLATE l_matnr_parm-matnr USING '- '.
        CONDENSE l_matnr_parm-matnr NO-GAPS.
        SHIFT l_matnr_parm-matnr RIGHT DELETING TRAILING space.
        TRANSLATE l_matnr_parm-matnr USING ' 0'.
        SELECT SINGLE maktx
          FROM makt
          INTO l_matnr_parm-maktx
          WHERE matnr = l_matnr_parm-matnr AND
                spras = sy-langu.
        IF sy-subrc > 0.
          l_matnr_parm-maktx = '*** Unknown ***'(t01).
        ENDIF.
        APPEND l_matnr_parm TO sti_matnr_parms.
      ENDLOOP.
    ENDIF.
  ENDIF.
*--------------------------------------------------------------------*

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'MATNR'
      value_org       = 'S'
    TABLES
      value_tab       = sti_matnr_parms[]
      return_tab      = li_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    READ TABLE li_return INTO l_return INDEX 1.
    IF sy-subrc = 0.
      p_matnr = l_return-fieldval.
      TRANSLATE p_matnr USING '- '.
      CONDENSE p_matnr NO-GAPS.
      SHIFT p_matnr RIGHT DELETING TRAILING space.
      TRANSLATE p_matnr USING ' 0'.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_f4_matnr
