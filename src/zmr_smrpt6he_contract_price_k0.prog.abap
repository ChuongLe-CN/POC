************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZMR_SMRPT6HE_CONTRACT_PRICE_K0                           *
* Title    :  Contract Price Report                                    *
* Work Unit:  SM-RPT-6HE                                               *
* Created by: Rob West                                                 *
* Created on: June 2011                                                *
* Version:    1.0                                                      *
* T-Code:     ZSM_RPT6HE_CONTRACTS                                     *
*                                                                      *
* Purpose:    Local class definition and implementation for use by     *
*             ZMR_SMRPT6HE_CONTRACT_PRICE                              *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/09/14          DV5K966489             *
*                                                                      *
* Short Description: FLCM Stabilization                                *
*                    Fix display of percentages                        *
*                                                                      *
*----------------------------------------------------------------------*
* Rob West                  2011/05/06          DV5K964489             *
*                                               DV5K965159             *
*                                                                      *
* Short Description: New program                                       *
*                                                                      *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&       Class LCL_VENDOR
*&---------------------------------------------------------------------*
CLASS lcl_vendor DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_ekko,
             ebeln               TYPE ekko-ebeln,
             bstyp               TYPE ekko-bstyp,
             lifnr               TYPE ekko-lifnr,
             kdate               TYPE ekko-kdate,
           END OF t_ekko,
           tt_ekko               TYPE STANDARD TABLE OF t_ekko WITH DEFAULT KEY,

           BEGIN OF t_ekpo,
             ebeln               TYPE ekpo-ebeln,
             ebelp               TYPE ekpo-ebelp,
             loekz               TYPE ekpo-loekz,
             txz01               TYPE ekpo-txz01,
             werks               TYPE ekpo-werks,
             matkl               TYPE ekpo-matkl,
             knttp               TYPE ekpo-knttp,
           END OF t_ekpo,
           tt_ekpo               TYPE STANDARD TABLE OF t_ekpo WITH DEFAULT KEY,

           BEGIN OF t_a016,
             kappl               TYPE a016-kappl,
             evrtn               TYPE a016-evrtn,
             evrtp               TYPE a016-evrtp,
             datbi               TYPE a016-datbi,
             datab               TYPE a016-datab,
             knumh               TYPE a016-knumh,
           END OF t_a016,
           tt_a016               TYPE STANDARD TABLE OF t_a016 WITH DEFAULT KEY,

           BEGIN OF t_konp,
             knumh               TYPE konp-knumh,
             kopos               TYPE konp-kopos,
             kappl               TYPE konp-kappl,
             kschl               TYPE konp-kschl,
             kbetr               TYPE konp-kbetr,
             konwa               TYPE konp-konwa,
             kpein               TYPE konp-kpein,
             kmein               TYPE konp-kmein,
             loevm_ko            TYPE konp-loevm_ko,
           END OF t_konp,
           tt_konp               TYPE STANDARD TABLE OF t_konp WITH DEFAULT KEY,

           BEGIN OF t_t685t,
             kschl               TYPE t685t-kschl,
             spras               TYPE t685t-spras,
             vtext               TYPE t685t-vtext,
           END OF t_t685t,
           tt_t685t              TYPE STANDARD TABLE OF t_t685t WITH DEFAULT KEY,

           BEGIN OF t_knvk,
             namev               TYPE knvk-namev,
             name1               TYPE knvk-name1,
             lifnr               TYPE knvk-lifnr,
             prsnr               TYPE knvk-prsnr,
             smtp_addr           TYPE adr6-smtp_addr,
           END OF t_knvk,
           tt_knvk               TYPE STANDARD TABLE OF t_knvk WITH DEFAULT KEY,

           tt_lifnr              TYPE STANDARD TABLE OF lifnr WITH DEFAULT KEY,
           tt_dd03l              TYPE STANDARD TABLE OF dd03l WITH DEFAULT KEY,
           tt_ebeln              TYPE STANDARD TABLE OF ebeln WITH DEFAULT KEY,
           tt_zmr_smrpt6he_extract TYPE STANDARD TABLE OF zmr_smrpt6he_extract WITH DEFAULT KEY,

           BEGIN OF t_object_catalog,
             lifnr           TYPE lifnr,
             objref          TYPE REF TO lcl_vendor,
           END OF t_object_catalog,
           tt_object_catalog TYPE STANDARD TABLE OF t_object_catalog WITH DEFAULT KEY.


    METHODS:
      constructor
        IMPORTING
          im_lifnr           TYPE lifnr,

      append_ebeln
        IMPORTING
          im_ebeln           TYPE ebeln,

      get_alv_data
        CHANGING
          ch_alv             TYPE tt_zmr_smrpt6he_extract,

      send_email.

    CLASS-METHODS:
      class_constructor,

      set_date_range
        IMPORTING
          im_datab           TYPE a016-datab     "Start date
          im_datbi           TYPE a016-datbi.    "End date



  PRIVATE SECTION.

    CONSTANTS: c_binary_min     TYPE i          VALUE 100,
               c_email_contact  TYPE knvk-pafkt VALUE 'Z6'.

    DATA: v_lifnr            TYPE lifnr,
          v_name1            TYPE lfa1-name1,
          v_name2            TYPE lfa1-name2,
          v_ort01            TYPE lfa1-ort01,
          v_regio            TYPE lfa1-regio,
          v_pstlz            TYPE lfa1-pstlz,
          v_land1            TYPE lfa1-land1,
          v_data_retrieved   TYPE xfeld,

          i_ebeln            TYPE tt_ebeln,
          i_ekpo             TYPE tt_ekpo,
          i_a016             TYPE tt_a016,
          i_konp             TYPE tt_konp,
          i_knvk             TYPE tt_knvk,
          i_merged           TYPE tt_zmr_smrpt6he_extract,

          v_ekpo_count       TYPE i,
          v_a016_count       TYPE i,
          v_konp_count       TYPE i.


    CLASS-DATA: v_datab      TYPE a016-datab,    "Start date
                v_datbi      TYPE a016-datbi,    "End date
                i_t685t      TYPE tt_t685t,
                i_dd03l      TYPE tt_dd03l.

    METHODS:
      fill_a016_data
        IMPORTING
          im_ekpo            TYPE t_ekpo
        CHANGING
          ch_data_lines      TYPE tt_zmr_smrpt6he_extract,

      fill_konp_data
        IMPORTING
          im_ekpo            TYPE t_ekpo
          im_a016            TYPE t_a016
        CHANGING
          ch_data_lines      TYPE tt_zmr_smrpt6he_extract,

      csv_line
        IMPORTING
          im_data_line       TYPE zmr_smrpt6he_extract
        CHANGING
          ch_text            TYPE soli.

    CLASS-METHODS:
      fill_tables
        IMPORTING
          im_ebeln           TYPE tt_ebeln
        CHANGING
          ch_ekpo            TYPE tt_ekpo
          ch_a016            TYPE tt_a016
          ch_konp            TYPE tt_konp
          ch_ekpo_count      TYPE i
          ch_a016_count      TYPE i
          ch_konp_count      TYPE i,

      add_t685t_text
        IMPORTING
          im_kschl           TYPE t685t-kschl
        RETURNING
          value(re_vtext)    TYPE t685t-vtext.

ENDCLASS.               "LCL_VENDOR

*----------------------------------------------------------------------*
*       CLASS lcl_vendor IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_vendor IMPLEMENTATION.
  METHOD constructor.

*** Set instance global data for the vendor
    v_lifnr = im_lifnr.

*** Add in the vendor name and address
    SELECT SINGLE land1
                  name1
                  name2
                  ort01
                  regio
                  pstlz
      FROM lfa1
      INTO (v_land1,
            v_name1,
            v_name2,
            v_ort01,
            v_regio,
            v_pstlz)
      WHERE lifnr = v_lifnr.

*** If email, add the recipients for the vendor
    IF p_email = abap_true.
      SELECT namev
             name1
             lifnr
             prsnr
             smtp_addr
        FROM knvk AS knvk
        JOIN adr6 AS adr6
        ON adr6~persnumber = knvk~prsnr
        INTO TABLE i_knvk
        WHERE knvk~lifnr = v_lifnr AND
              knvk~pafkt = c_email_contact.
    ENDIF.

  ENDMETHOD.                    "constructor

  METHOD class_constructor.

*** Set global class data for the ZMR_SMRPT6HE_EXTRACT metadata
    SELECT *
      FROM dd03l
      INTO TABLE i_dd03l
      WHERE tabname = 'ZMR_SMRPT6HE_EXTRACT'.

    IF sy-subrc = 0.
      DELETE i_dd03l WHERE fieldname = 'KNTTP' OR
                           fieldname = 'KNUMH' OR
                           fieldname = 'MATKL'.
      SORT i_dd03l BY position ASCENDING.
    ENDIF.

    v_datab = v_datbi = sy-datum.    "Default start & end dates to current date

  ENDMETHOD.                    "class_constructor

  METHOD append_ebeln.

*** Append the Purchasing Document number
    IF im_ebeln IS NOT INITIAL.
      APPEND im_ebeln TO i_ebeln.
    ENDIF.

  ENDMETHOD.                    "append_ebeln

  METHOD get_alv_data.

    FIELD-SYMBOLS: <ekpo>    TYPE t_ekpo.

*** Get the data for all Purchasing Documents - one time only per vendor
    IF v_data_retrieved = abap_false.
      v_data_retrieved = abap_true.
      CALL METHOD fill_tables
        EXPORTING
          im_ebeln      = i_ebeln[]
        CHANGING
          ch_ekpo       = i_ekpo[]
          ch_a016       = i_a016[]
          ch_konp       = i_konp[]
          ch_ekpo_count = v_ekpo_count
          ch_a016_count = v_a016_count
          ch_konp_count = v_konp_count.

*** Merge the data into a standard formst
      LOOP AT i_ekpo ASSIGNING <ekpo>.
        CALL METHOD fill_a016_data
          EXPORTING
            im_ekpo       = <ekpo>
          CHANGING
            ch_data_lines = i_merged[].
      ENDLOOP.
    ENDIF.

*** Append the vendor data to the parameter table
    APPEND LINES OF i_merged TO ch_alv.

  ENDMETHOD.                    "get_alv_data

  METHOD send_email.

    CONSTANTS: lc_crlf         TYPE c LENGTH 2 VALUE %_cr_lf.

    DATA: li_extract         TYPE tt_zmr_smrpt6he_extract,
          li_text            TYPE soli_tab,
          li_csv             TYPE soli_tab,
          l_csv              TYPE string,

          l_sent             TYPE xfeld,

          lo_send_request    TYPE REF TO cl_bcs,
          lo_document        TYPE REF TO cl_document_bcs,
          lo_sender          TYPE REF TO if_sender_bcs,
          lo_recipient       TYPE REF TO if_recipient_bcs,
          lo_bcs_exception   TYPE REF TO cx_bcs.            "#EC NEEDED


    FIELD-SYMBOLS: <extract> TYPE zmr_smrpt6he_extract,
                   <text>    TYPE soli,
                   <knvk>    TYPE t_knvk.

*** Return if no recipients for the vendor with a message
    IF i_knvk[] IS INITIAL.
      MESSAGE i013(zz_flcm) WITH v_lifnr.
      RETURN.
    ENDIF.

*** Call method to get vendor info in standard format
    CALL METHOD get_alv_data
      CHANGING
        ch_alv = li_extract[].

*** Return if no data to send
    IF li_extract[] IS INITIAL.
      RETURN.
    ENDIF.

*** Sort data by date and plant
    SORT li_extract BY datbi DESCENDING
                       werks ASCENDING.

*** Prepare English text - Canada and US
    APPEND 'CN'(e01) TO li_text.
    APPEND INITIAL LINE TO li_text.
    APPEND v_name1 TO li_text.
    IF v_name2 IS NOT INITIAL.
      APPEND v_name2 TO li_text.
    ENDIF.
    IF v_ort01 IS NOT INITIAL.
      APPEND v_ort01 TO li_text.
    ENDIF.
    IF v_regio IS NOT INITIAL.
      APPEND v_regio TO li_text.
    ENDIF.
    IF v_pstlz IS NOT INITIAL.
      APPEND v_pstlz TO li_text.
    ENDIF.
    APPEND INITIAL LINE TO li_text.
    APPEND INITIAL LINE TO li_text ASSIGNING <text>.
    WRITE sy-datum TO <text> LEFT-JUSTIFIED.
    APPEND INITIAL LINE TO li_text.
    LOOP AT i_knvk ASSIGNING <knvk>.
      APPEND INITIAL LINE TO li_text ASSIGNING <text>.
      CONCATENATE <knvk>-namev <knvk>-name1 INTO <text> SEPARATED BY space.
      SHIFT <text> LEFT DELETING LEADING space.
    ENDLOOP.
    APPEND INITIAL LINE TO li_text.
    APPEND 'Please find attached the price list for your CN contracts.'(e02) TO li_text.
    APPEND INITIAL LINE TO li_text.
    APPEND 'Please feel free to contact us if you have any questions.'(e03) TO li_text.
    APPEND INITIAL LINE TO li_text.
    APPEND 'Thank you,'(e04) TO li_text.
    APPEND INITIAL LINE TO li_text.
    APPEND 'CN - Supply Management'(e05) TO li_text.
    APPEND INITIAL LINE TO li_text.
    APPEND 'P.O. Box 8103'(e06) TO li_text.
    APPEND 'Montreal, Quebec'(e07) TO li_text.
    APPEND 'H3C 3N3'(e08) TO li_text.

    APPEND 'Vendor,Vendor Name,Contract,Item,Delivery Type,Description,Plant,Validity From, Validity To,' & "#EC NOTEXT
           'Condition Type,Condition Description,Amount,Currency,Per, Unit of measure' TO li_csv.

*** Prepare French text - Canada only
    IF v_land1 = 'CA'.
      APPEND INITIAL LINE TO li_text.
      APPEND INITIAL LINE TO li_text ASSIGNING <text>.
      CLEAR <text>(100) WITH '-'.
      APPEND INITIAL LINE TO li_text.
      APPEND 'Veuillez trouver ci-joint la liste des prix pour les contrats du CN'(e12) TO li_text.
      APPEND INITIAL LINE TO li_text.
      APPEND 'N''hésitez pas à nous contacter si vous avez des questions.'(e13) TO li_text.
      APPEND INITIAL LINE TO li_text.
      APPEND 'Merci,'(e14) TO li_text.
      APPEND INITIAL LINE TO li_text.
      APPEND 'CN - Gestion des approvisionnements'(e15) TO li_text.
      APPEND INITIAL LINE TO li_text.
      APPEND 'CP 8103'(e16) TO li_text.
      APPEND 'Montréal, Québec'(e17) TO li_text.
      APPEND 'H3C 3N3'(e08) TO li_text.

      APPEND 'Fournisseur,Nom du fournisseur,Contrat,Poste,Type livraison,Description,Division,Validité début,' & "#EC NOTEXT
             'Validité fin,Type de condition,Description condition,Montant,Devise,Par,Unité de mesure' TO li_csv.
    ENDIF.

*** For each line of data, convert to CSV format
    LOOP AT li_extract ASSIGNING <extract>.
      APPEND INITIAL LINE TO li_csv ASSIGNING <text>.
      CALL METHOD csv_line
        EXPORTING
          im_data_line = <extract>
        CHANGING
          ch_text      = <text>.
    ENDLOOP.

*** Compress csv data into an attachment friendy format for SAP Office
    IF li_csv[] IS NOT INITIAL.
      CONCATENATE LINES OF li_csv INTO l_csv SEPARATED BY lc_crlf.
      CALL METHOD cl_document_bcs=>string_to_soli
        EXPORTING
          ip_string = l_csv
        RECEIVING
          rt_soli   = li_csv[].
    ENDIF.

    TRY.
*** Create persistent send request
        lo_send_request = cl_bcs=>create_persistent( ).

*** Create document from internal table with text
        CALL METHOD cl_document_bcs=>create_document
          EXPORTING
            i_type    = 'RAW'
            i_subject = 'Contract Price Report'(e00)
            i_text    = li_text[]
          RECEIVING
            result    = lo_document.

*** Add attachment to document
        CALL METHOD lo_document->add_attachment
          EXPORTING
            i_attachment_type    = 'CSV'
            i_attachment_subject = text-e00
            i_att_content_text   = li_csv[].

*** Add document to send request
        CALL METHOD lo_send_request->set_document
          EXPORTING
            i_document = lo_document.

*** Set sender
*     Note: This is necessary only if you want to set the sender
*           different from actual user (SY-UNAME). Otherwise sender is
*           set automatically with actual user.
        IF p_sender IS NOT INITIAL.
          CALL METHOD cl_cam_address_bcs=>create_internet_address
            EXPORTING
              i_address_string = p_sender
            RECEIVING
              result           = lo_sender.

          CALL METHOD lo_send_request->set_sender
            EXPORTING
              i_sender = lo_sender.
        ENDIF.

*** Add recipients (e-mail address)
        LOOP AT i_knvk ASSIGNING <knvk>.
          CALL METHOD cl_cam_address_bcs=>create_internet_address
            EXPORTING
              i_address_string = <knvk>-smtp_addr
            RECEIVING
              result           = lo_recipient.
*** Add recipient with its respective attributes to send request
          CALL METHOD lo_send_request->add_recipient
            EXPORTING
              i_recipient = lo_recipient
              i_express   = 'X'.
        ENDLOOP.

*** Send document ---------------------------------------
        CALL METHOD lo_send_request->send
          RECEIVING
            result = l_sent.

        IF l_sent = abap_false.
          MESSAGE i014(zz_flcm) WITH v_lifnr.   "Email error.
        ENDIF.

        COMMIT WORK.

      CATCH cx_bcs INTO lo_bcs_exception.
        MESSAGE i014(zz_flcm) WITH v_lifnr.   "Message in case of Email error.
        EXIT.

    ENDTRY.

  ENDMETHOD.                    "send_email

  METHOD fill_a016_data.

    DATA: l_tabix            TYPE sytabix.

    FIELD-SYMBOLS: <a016>    TYPE t_a016.

*** Find first occurrence of EBELN/EBELP in the A016 table
    IF v_a016_count > c_binary_min.      "Use binary search for big tables
      READ TABLE i_a016 TRANSPORTING NO FIELDS
          WITH KEY evrtn = im_ekpo-ebeln
                   evrtp = im_ekpo-ebelp
          BINARY SEARCH.
    ELSE.
      READ TABLE i_a016 TRANSPORTING NO FIELDS
          WITH KEY evrtn = im_ekpo-ebeln
                   evrtp = im_ekpo-ebelp.
    ENDIF.

    IF sy-subrc = 0.
      l_tabix = sy-tabix.
    ELSE.
      RETURN.
    ENDIF.

*** Loop through all matching A016 data
    LOOP AT i_a016 ASSIGNING <a016> FROM l_tabix.
      IF <a016>-evrtn <> im_ekpo-ebeln OR
         <a016>-evrtp <> im_ekpo-ebelp.
        EXIT.
      ENDIF.
      CALL METHOD fill_konp_data
        EXPORTING
          im_ekpo       = im_ekpo
          im_a016       = <a016>
        CHANGING
          ch_data_lines = ch_data_lines[].
    ENDLOOP.

  ENDMETHOD.                    "fill_a016_data

  METHOD fill_konp_data.

    DATA: l_tabix            TYPE sytabix.

    FIELD-SYMBOLS: <line>    TYPE zmr_smrpt6he_extract,
                   <konp>    TYPE t_konp.

    IF v_konp_count > c_binary_min.     "Use binary search for big tables
      READ TABLE i_konp TRANSPORTING NO FIELDS
          WITH KEY knumh = im_a016-knumh
          BINARY SEARCH.
    ELSE.
      READ TABLE i_konp TRANSPORTING NO FIELDS
          WITH KEY knumh = im_a016-knumh.
    ENDIF.

    IF sy-subrc = 0.
      l_tabix = sy-tabix.
    ELSE.
      RETURN.
    ENDIF.

*** Loop through all matching KONP data
    LOOP AT i_konp ASSIGNING <konp> FROM l_tabix.
      IF <konp>-knumh <> im_a016-knumh.
        EXIT.
      ENDIF.
      APPEND INITIAL LINE TO ch_data_lines ASSIGNING <line>.
*** Add in Vendor and line item details
      <line>-lifnr = v_lifnr.
      <line>-name1 = v_name1.
      <line>-ebeln = im_ekpo-ebeln.
      <line>-ebelp = im_ekpo-ebelp.
      <line>-knttp = im_ekpo-knttp.
      IF im_ekpo-knttp = 'K'.
        <line>-zzdelivery_type = 'DTL'(kn1).
      ELSE.
        <line>-zzdelivery_type = 'Storage'(kn2).
      ENDIF.
      <line>-txz01 = im_ekpo-txz01.
      <line>-werks = im_ekpo-werks.
      <line>-matkl = im_ekpo-matkl.
      <line>-datab = im_a016-datab.
      <line>-datbi = im_a016-datbi.
      <line>-knumh = <konp>-knumh.
      <line>-kschl = <konp>-kschl.
      <line>-kbetr = <konp>-kbetr.
      <line>-konwa = <konp>-konwa.
      <line>-kpein = <konp>-kpein.
      <line>-kmein = <konp>-kmein.
      <line>-vtext = add_t685t_text( <konp>-kschl ).
      IF <line>-konwa = '%'.                         "DV5K966489
        DIVIDE <line>-kbetr BY 10.                   "DV5K966489
      ENDIF.                                         "DV5K966489
    ENDLOOP.

  ENDMETHOD.                    "fill_konp_data

  METHOD csv_line.

    DATA: l_text               TYPE c LENGTH 80,
          l_length             TYPE i,
          l_negative           TYPE xfeld.

    FIELD-SYMBOLS: <field>     TYPE ANY,
                   <dd03l>     TYPE dd03l.

*** Convert all fields of the record into a CSV accepable format
    LOOP AT i_dd03l ASSIGNING <dd03l>.
      ASSIGN COMPONENT <dd03l>-position OF STRUCTURE im_data_line TO <field>.
      IF sy-subrc = 0.
        l_negative = abap_false.
        CASE <dd03l>-inttype.
          WHEN 'C'.        "Direct move for character data
            l_text = <field>.
          WHEN 'D' OR 'T'. "Formatted date and time
            WRITE <field> TO l_text LEFT-JUSTIFIED.
          WHEN 'N'.        "Formatted numeric data with no sign
            WRITE <field> TO l_text LEFT-JUSTIFIED
                                    NO-GROUPING
                                    NO-SIGN.
            IF <field> < 0.
              l_negative = abap_true.    "Set flag for negative
            ENDIF.
          WHEN 'P'.        "Formatted packed data with no sign, decimals as defined
            WRITE <field> TO l_text LEFT-JUSTIFIED
                                    NO-GROUPING
                                    NO-SIGN
                                    DECIMALS <dd03l>-decimals.
            IF <field> < 0.
              l_negative = abap_true.    "Set flag for negative
            ENDIF.
        ENDCASE.
        TRANSLATE l_text USING '"'',.'.  "Convert double-quote to single-quote and comma to decimal
        IF l_text CA ',''+-/*'.          "Put quotes around data with symbols.
          l_length = STRLEN( l_text ).
          l_text+l_length(1) = '"'.
          IF l_negative = abap_true.
            SHIFT l_text RIGHT BY 1 PLACES.
            l_text+0(1) = '-'.
          ENDIF.
          SHIFT l_text RIGHT BY 1 PLACES.
          l_text+0(1) = '"'.
        ELSE.
          IF l_negative = abap_true.      "Add in leading negative sign
            SHIFT l_text RIGHT BY 1 PLACES.
            l_text+0(1) = '-'.
          ENDIF.
        ENDIF.
        IF ch_text IS INITIAL.
          ch_text = l_text.
        ELSE.
          CONCATENATE ch_text l_text INTO ch_text SEPARATED BY ','.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "csv_line


  METHOD fill_tables.

*** Return if no Purchasing Documents
    IF im_ebeln[] IS INITIAL.
      RETURN.
    ENDIF.

*** Get the line item data from EKPO
    SELECT ebeln
           ebelp
           loekz
           txz01
           werks
           matkl
           knttp
      FROM ekpo
      INTO TABLE ch_ekpo
      FOR ALL ENTRIES IN im_ebeln
      WHERE ebeln = im_ebeln-table_line AND
            ebelp IN s_ebelp.

    IF sy-subrc = 0.
      DELETE ch_ekpo WHERE NOT ( loekz = space AND
                                 werks IN s_werks AND  "Apply WERKS selection
                                 matkl IN s_matkl ).   "Apply MATKL selection
      SORT ch_ekpo BY ebeln ASCENDING
                      ebelp ASCENDING.
    ENDIF.

*--------------------------------------------------------------------*
*** Get the Contract Item data from A016
    IF ch_ekpo[] IS NOT INITIAL.
      SELECT kappl
             evrtn
             evrtp
             datbi
             datab
             knumh
        FROM a016
        INTO TABLE ch_a016
        FOR ALL ENTRIES IN ch_ekpo
        WHERE kappl = 'M' AND
              evrtn = ch_ekpo-ebeln AND
              evrtp IN s_ebelp AND
              datbi >= v_datab.

      IF sy-subrc = 0.
        DELETE ch_a016 WHERE datab > v_datbi.
        SORT ch_a016 BY evrtn ASCENDING
                        evrtp ASCENDING.
        IF ch_a016[] IS NOT INITIAL.
*** Get the Conditions data from KONP
          SELECT knumh
                 kopos
                 kappl
                 kschl
                 kbetr
                 konwa
                 kpein
                 kmein
                 loevm_ko
            FROM konp
            INTO TABLE ch_konp
            FOR ALL ENTRIES IN ch_a016
            WHERE knumh = ch_a016-knumh.
          IF sy-subrc = 0.
            DELETE ch_konp WHERE NOT ( kappl = 'M' AND
                                       kschl IN s_kschl AND  "Apply KSCHL selection criteria
                                       loevm_ko = space ).
            SORT ch_konp BY knumh ASCENDING
                            kopos ASCENDING.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*** Determine the number of lines in each raw data table
    ch_ekpo_count = LINES( ch_ekpo ).
    ch_a016_count = LINES( ch_a016 ).
    ch_konp_count = LINES( ch_konp ).

  ENDMETHOD.                    "fill_tables

  METHOD set_date_range.

*** Set the global date range
    IF im_datab IS NOT INITIAL.
      v_datab = im_datab.
    ENDIF.

    IF im_datbi IS NOT INITIAL.
      v_datbi = im_datbi.
    ENDIF.

  ENDMETHOD.                    "set_date_range

  METHOD add_t685t_text.

    FIELD-SYMBOLS: <t685t>   TYPE t_t685t.

*** Read buffer table of T685T texts
    READ TABLE i_t685t ASSIGNING <t685t>
        WITH KEY kschl = im_kschl.
*** If not found, append new line with data from DB table
    IF sy-subrc > 0.
      APPEND INITIAL LINE TO i_t685t ASSIGNING <t685t>.
      <t685t>-kschl = im_kschl.
      SELECT SINGLE vtext
        FROM t685t
        INTO <t685t>-vtext
        WHERE spras = sy-langu AND
              kvewe = 'A' AND
              kappl = 'M' AND
              kschl = im_kschl.
      IF sy-subrc > 0.
        <t685t>-vtext = '** Unknown **'(te1).
      ENDIF.
    ENDIF.

    re_vtext = <t685t>-vtext.

  ENDMETHOD.                    "add_t685t_text

ENDCLASS.                    "lcl_vendor IMPLEMENTATION
