************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZMR_SMRPT6HE_CONTRACT_PRICE                              *
* Title    :  Contract Price Report                                    *
* Work Unit:  SM-RPT-6HE                                               *
* Created by: Rob West                                                 *
* Created on: June 2011                                                *
* Version:    1.0                                                      *
* T-Code:     ZSM_RPT6HE_CONTRACTS                                     *
*                                                                      *
* Purpose:    Report to vendors to provide the contract fuel price     *
*             per plant that details the price formula calculation.    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/05/06          DV5K964489             *
*                                                                      *
* Short Description: New program                                       *
*                                                                      *
*----------------------------------------------------------------------*
REPORT  zmr_smrpt6he_contract_price.

TYPE-POOLS: abap,
            sscr,
            slis.

CONSTANTS: c_prod            TYPE sy-sysid   VALUE 'PRD',
           c_espoper         TYPE sy-uname   VALUE 'ESPOPER'.

*--------------------------------------------------------------------*
*** The following data declarations are for the selection screen only
DATA: v_1000_ekko            TYPE ekko,
      v_1000_ekpo            TYPE ekpo,
      v_1000_konp            TYPE konp,
      v_1000_a016            TYPE a016,
      v_answer_1000          TYPE c,
      v_email_allowed        TYPE xfeld.

SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE text-bk1.
SELECT-OPTIONS: s_lifnr      FOR v_1000_ekko-lifnr,
                s_ebeln      FOR v_1000_ekko-ebeln,
                s_ebelp      FOR v_1000_ekpo-ebelp,
                s_werks      FOR v_1000_ekpo-werks,
                s_matkl      FOR v_1000_ekpo-matkl,
                s_kschl      FOR v_1000_konp-kschl.
SELECTION-SCREEN END OF BLOCK bk1.
SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME TITLE text-bk2.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_days1 RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND rb1.
SELECTION-SCREEN COMMENT (20) text-sl1 FOR FIELD p_days.
SELECTION-SCREEN POSITION 33.
PARAMETERS p_days            TYPE numc3  MODIF ID ln1 DEFAULT '7'.
SELECTION-SCREEN POSITION 45.
PARAMETERS p_incl            TYPE xfeld MODIF ID ln1.
SELECTION-SCREEN COMMENT (20) text-sl2 FOR FIELD p_incl.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_days2 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT (20) text-sl3 FOR FIELD s_dayrg.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS: s_dayrg      FOR v_1000_a016-datab MODIF ID ln2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bk2.
SELECTION-SCREEN BEGIN OF BLOCK bk3 WITH FRAME TITLE text-bk3.
PARAMETERS: p_email       TYPE xfeld AS CHECKBOX MODIF ID em1 USER-COMMAND em1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) text-sl4 FOR FIELD p_sender MODIF ID em2.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: p_sender      TYPE ad_smtpadr  MODIF ID em2.
SELECTION-SCREEN COMMENT (35) text-sl5 MODIF ID em2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_alvvar      TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK bk3.

*--------------------------------------------------------------------*
INCLUDE zmr_smrpt6he_contract_price_k0.
*--------------------------------------------------------------------*

*** Global Data
DATA: i_ekko                 TYPE lcl_vendor=>tt_ekko,
      i_object_catalog       TYPE lcl_vendor=>tt_object_catalog.

*--------------------------------------------------------------------*
INITIALIZATION.

  AUTHORITY-CHECK OBJECT 'ZM_6HEMAIL'
           ID 'ACTVT' FIELD 'A9'.    "Send
  IF sy-subrc = 0.
    v_email_allowed = abap_true.
  ELSE.
    v_email_allowed = abap_false.
  ENDIF.

  PERFORM f_restrict_intervals.
  PERFORM f_no_input USING 'LN2'.

  IF v_email_allowed = abap_false.
    PERFORM f_invisible USING 'EM+'.
  ENDIF.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF s_lifnr[] IS INITIAL AND
   ( sy-ucomm = '    ' OR
     sy-ucomm = 'ONLI' OR
     sy-ucomm = 'PRIN' OR
     sy-ucomm = 'SJOB' ).
    CLEAR sy-ucomm.
    MESSAGE e027(zz_vp).
  ENDIF.

  IF sy-ucomm = 'ZMAIL' AND sy-sysid <> c_prod.  "Show email in test systems only
    SUBMIT rssosoststat
        WITH g_sender = sy-uname
        WITH g_adrtp  = 'INT'
        AND RETURN.
  ENDIF.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_alvvar.
  PERFORM f_f4_alv_variants.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  IF p_days1 = 'X'.
    PERFORM f_no_input USING 'LN2'.
  ELSE.
    PERFORM f_no_input USING 'LN1'.
  ENDIF.

  IF v_email_allowed = abap_true AND p_email = abap_false.
    PERFORM f_no_input USING 'EM2'.
  ENDIF.
*--------------------------------------------------------------------*
START-OF-SELECTION.

  IF p_email = abap_true AND sy-uname(7) <> c_espoper.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Send Email?'(c00)
        text_question         = 'Are you sure that you want to send the contract price report to the vendor contact'(c01)
        text_button_1         = 'Yes'(c02)
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'No'(c03)
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = abap_false
      IMPORTING
        answer                = v_answer_1000.

    IF v_answer_1000 = '2'.
      RETURN.
    ENDIF.
  ENDIF.

  PERFORM f_set_date_range.

  PERFORM f_select_ekko CHANGING i_ekko[].        "Returns sorted by EBELN

  PERFORM f_create_objects USING i_ekko[].

  IF p_email = abap_true.
    PERFORM f_email_by_vendor USING i_object_catalog[].
  ENDIF.

  PERFORM f_alv_grid USING i_object_catalog[].

  EXIT.

*&---------------------------------------------------------------------*
*&      Form  f_set_date_range
*&---------------------------------------------------------------------*
FORM f_set_date_range.

  DATA: l_datab              TYPE a016-datab,    "Start date
        l_datbi              TYPE a016-datbi.    "End date

  FIELD-SYMBOLS: <dayrg>     LIKE LINE OF s_dayrg.

*** Set the date range for data selection
  IF p_days2 = abap_true.
    READ TABLE s_dayrg ASSIGNING <dayrg> INDEX 1.
    IF sy-subrc = 0.
      l_datab = <dayrg>-low.
      l_datbi = <dayrg>-high.
    ENDIF.
  ELSE.
    l_datbi = sy-datum.
    IF p_incl = abap_false.
      SUBTRACT 1 FROM l_datbi.
    ENDIF.
    l_datab = l_datbi - p_days.
  ENDIF.
  CALL METHOD lcl_vendor=>set_date_range
    EXPORTING
      im_datab = l_datab
      im_datbi = l_datbi.

  IF sy-batch = abap_true OR
     p_days1  = abap_true.
    MESSAGE i030(zz_flcm) WITH l_datab l_datbi.
  ENDIF.

ENDFORM.                    "f_set_date_range

*&---------------------------------------------------------------------*
*&      Form  f_create_objects
*&---------------------------------------------------------------------*
FORM f_create_objects USING pi_ekko TYPE lcl_vendor=>tt_ekko.

  FIELD-SYMBOLS: <ekko>      TYPE lcl_vendor=>t_ekko,
                 <object>    TYPE lcl_vendor=>t_object_catalog.

*** Create a new object per vendor and append each Purchasing Document
  LOOP AT pi_ekko ASSIGNING <ekko>.
    IF <object> IS NOT ASSIGNED OR
     ( <object> IS ASSIGNED AND
       <object>-lifnr <> <ekko>-lifnr ).
      APPEND INITIAL LINE TO i_object_catalog ASSIGNING <object>.
      <object>-lifnr = <ekko>-lifnr.
      CREATE OBJECT <object>-objref
        EXPORTING
          im_lifnr = <ekko>-lifnr.
    ENDIF.
    CALL METHOD <object>-objref->append_ebeln
      EXPORTING
        im_ebeln = <ekko>-ebeln.
  ENDLOOP.

ENDFORM.                    "f_create_objects

*&---------------------------------------------------------------------*
*&      Form  f_select_ekko
*&---------------------------------------------------------------------*
FORM f_select_ekko CHANGING pi_ekko  TYPE lcl_vendor=>tt_ekko.

  IF s_ebeln[] IS INITIAL.      "Select by LIFNR if EBELN not provided
    SELECT ebeln
           bstyp
           lifnr
           kdate
      FROM ekko
      INTO TABLE pi_ekko
      WHERE lifnr IN s_lifnr.
  ELSE.                         "Select by primary key EBELN, if provided
    SELECT ebeln
           bstyp
           lifnr
           kdate
      FROM ekko
      INTO TABLE pi_ekko
      WHERE ebeln IN s_ebeln.
    IF sy-subrc = 0.
      DELETE pi_ekko WHERE NOT ( lifnr IN s_lifnr ).   "Apply LIFNR selection
    ENDIF.
  ENDIF.

  IF pi_ekko[] IS NOT INITIAL.
    DELETE pi_ekko WHERE NOT ( bstyp = 'K' AND         "Apply BSTYP selection
                               kdate >= sy-datum ).    "Apply KDATE selection
    SORT pi_ekko BY lifnr ASCENDING                    "Sort by LIFNR and EBELN
                    ebeln ASCENDING.
  ENDIF.

ENDFORM.                    "f_select_ekko

*&---------------------------------------------------------------------*
*&      Form  f_alv_grid
*&---------------------------------------------------------------------*
FORM f_alv_grid USING pi_catalog  TYPE lcl_vendor=>tt_object_catalog.

  DATA: li_extract           TYPE lcl_vendor=>tt_zmr_smrpt6he_extract,
        li_sort              TYPE slis_t_sortinfo_alv,
        l_variant            TYPE disvariant,
        l_layout             TYPE slis_layout_alv.

  FIELD-SYMBOLS: <object>    TYPE lcl_vendor=>t_object_catalog,
                 <sort>      TYPE slis_sortinfo_alv.

*** Get all data to be displayed from each object (vendor)
  LOOP AT pi_catalog ASSIGNING <object>.
    CALL METHOD <object>-objref->get_alv_data
      CHANGING
        ch_alv = li_extract[].
  ENDLOOP.

*** Message if no data to display
  IF li_extract[] IS INITIAL.
    MESSAGE i011(zz_flcm).
    RETURN.
  ENDIF.

*** Set layout
  l_layout-colwidth_optimize = abap_true.
  l_layout-zebra = abap_true.

  IF p_alvvar IS INITIAL.       "Set default sort sequences
    APPEND INITIAL LINE TO li_sort ASSIGNING <sort>.
    <sort>-spos = sy-tabix.
    <sort>-fieldname = 'LIFNR'.
    <sort>-up = abap_true.
    APPEND INITIAL LINE TO li_sort ASSIGNING <sort>.
    <sort>-spos = sy-tabix.
    <sort>-fieldname = 'NAME1'.
    <sort>-up = abap_true.
    APPEND INITIAL LINE TO li_sort ASSIGNING <sort>.
    <sort>-spos = sy-tabix.
    <sort>-fieldname = 'WERKS'.
    <sort>-up = abap_true.
    APPEND INITIAL LINE TO li_sort ASSIGNING <sort>.
    <sort>-spos = sy-tabix.
    <sort>-fieldname = 'DATBI'.
    <sort>-down = abap_true.
  ELSE.                         "Or, use the specfied variant
    l_variant-variant = p_alvvar.
  ENDIF.
  l_variant-report = sy-repid.

*** Display the results in an ALV grid
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_structure_name = 'ZMR_SMRPT6HE_EXTRACT'
      is_layout        = l_layout
      is_variant       = l_variant
      it_sort          = li_sort[]
      i_save           = 'A'       "user and global variants
      i_default        = ' '       "default layouts not allowed
    TABLES
      t_outtab         = li_extract[].

ENDFORM.                    "f_alv_grid

*&---------------------------------------------------------------------*
*&      Form  f_email_by_vendor
*&---------------------------------------------------------------------*
FORM f_email_by_vendor USING pi_catalog  TYPE lcl_vendor=>tt_object_catalog.

  FIELD-SYMBOLS: <object>    TYPE lcl_vendor=>t_object_catalog.

*** Prepare an email for each object (Vendor)
  LOOP AT pi_catalog ASSIGNING <object>.
    CALL METHOD <object>-objref->send_email.
  ENDLOOP.

ENDFORM.                    "f_email_by_vendor

*&---------------------------------------------------------------------*
*&      Form  f_no_input
*&---------------------------------------------------------------------*
FORM f_no_input  USING p_group1.

  IF p_group1 CA '+'.
    LOOP AT SCREEN.
      IF screen-group1 CP p_group1.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = p_group1.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.


ENDFORM.                    "f_no_input

*&---------------------------------------------------------------------*
*&      Form  f_invisible
*&---------------------------------------------------------------------*
FORM f_invisible  USING p_group1.

  IF p_group1 CA '+'.
    LOOP AT SCREEN.
      IF screen-group1 CP p_group1.
        screen-input = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = p_group1.
        screen-input = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "f_invisible

*&---------------------------------------------------------------------*
*&      Form  f_restrict_intervals
*&---------------------------------------------------------------------*
FORM f_restrict_intervals.

*** Define the object to be passed to the RESTRICTION parameter
  DATA: l_restrict           TYPE sscr_restrict.

*** Auxiliary objects for filling RESTRICT
  FIELD-SYMBOLS: <opt_list>  TYPE sscr_opt_list,
                 <sscr_ass>  TYPE sscr_ass.

*** Restrict selection for selections to BETWEEN only
  APPEND INITIAL LINE TO l_restrict-opt_list_tab ASSIGNING <opt_list>.
  <opt_list>-name = 'JUST_BT'.
  <opt_list>-options-bt = 'X'.

  APPEND INITIAL LINE TO l_restrict-ass_tab ASSIGNING <sscr_ass>.
  <sscr_ass>-kind = 'S'.
  <sscr_ass>-sg_main = 'I'.
  <sscr_ass>-sg_addy = 'N'.
  <sscr_ass>-op_main = 'JUST_BT'.
  <sscr_ass>-name = 'S_DAYRG'.

*** Restrict selection for selections to EQUAL only
  APPEND INITIAL LINE TO l_restrict-opt_list_tab ASSIGNING <opt_list>.
  <opt_list>-name = 'EQUAL'.
  <opt_list>-options-eq = 'X'.

  APPEND INITIAL LINE TO l_restrict-ass_tab ASSIGNING <sscr_ass>.
  <sscr_ass>-kind = 'S'.
  <sscr_ass>-sg_main = 'I'.
  <sscr_ass>-op_main = 'EQUAL'.
  <sscr_ass>-name = 'S_LIFNR'.


  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction = l_restrict.

ENDFORM.                    "f_restrict_intervals

*&---------------------------------------------------------------------*
*&      Form  f_f4_alv_variants
*&---------------------------------------------------------------------*
FORM f_f4_alv_variants.

  DATA: l_is_variant         TYPE disvariant,
        l_es_variant         TYPE disvariant.

  l_is_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      i_save     = 'A'
      is_variant = l_is_variant
    IMPORTING
      es_variant = l_es_variant.

  IF l_es_variant-variant IS NOT INITIAL.
    PERFORM f_update_dynpro USING 'P_ALVVAR' l_es_variant-variant.
  ENDIF.

ENDFORM.                    "f_f4_alv_variants

*&---------------------------------------------------------------------*
*&      Form  f_update_dynpro
*&---------------------------------------------------------------------*
FORM f_update_dynpro USING p_fieldname p_value.

  DATA: li_dynpfields        TYPE STANDARD TABLE OF dynpread.

  FIELD-SYMBOLS: <field>     TYPE dynpread.


  APPEND INITIAL LINE TO li_dynpfields ASSIGNING <field>.
  <field>-fieldname = p_fieldname.
  <field>-fieldvalue = p_value.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = '1000'
    TABLES
      dynpfields = li_dynpfields.

ENDFORM.                    "f_update_dynpro
