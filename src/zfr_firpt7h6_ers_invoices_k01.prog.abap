************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZFR_FIRPT7H6_ERS_INVOICES_K01                            *
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
* Justin Apurado(TXT21609)  2021/04/16           DV5K9A0U6O            *
*                                                                      *
* Short Description: RTSK0014488 DFCT0011652                           *
*   - Fix IT consultants data sorting                                  *
*----------------------------------------------------------------------*
* Justin Apurado(TXT21609)  2021/03/08           DV5K9A0TR9            *
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
*   - Consolidated payment notice, send monthly invoice summary        *
*     report to IT Consultant vendors.                                 *
*----------------------------------------------------------------------*
* Chuong Le                 2017-01-04          DV5K9A05WW             *
*                                                                      *
* Short Description: CR306321-T347799                                  *
*    - Read Our Reference from PO header text instead of EKKO-UNSEZ.   *
*----------------------------------------------------------------------*
*  Denis Gadoua             2012-07-20          DV5K973130             *
* Short Description:                                                   *
* - The date range in the headings                                     *
*   "invoices issued for the period" is now different for each vendor. *
*   The from-date is the earliest date and the to-date is the latest   *
*   date on the report, per vendor.                                    *
*----------------------------------------------------------------------*
* Rob West                  2011/12/13          DV5K968325             *
*                                                                      *
* Short Description: Defect 674 FLCM Stabilization                     *
*                  - Add Locomotive ID to ALV and csv from MSEG-WEMPF  *
*                  - Change sort order on pdf and csv files            *
*                      1 - Terminal ID                                 *
*                      2 - BOL                                         *
*                      3 - Delivery Date                               *
*                  - Correct to and from dates in pdf header when no   *
*                    «Document Date» selection is given                *
*----------------------------------------------------------------------*
* Rob West                  2011/10/19          DVSK904466/DV5K967207  *
*                                               DVSK904577/DV5K967347  *
*                                               DVSK904592/DV5K967412  *
*                                                                      *
* Short Description: FLCM Stabilization defect 612                     *
*                    Add BSIK-SHKZG to determine credit/debit          *
*                    Add BSEG-XNEGP to determine credit/debit          *
*----------------------------------------------------------------------*
* Rob West                  2011/09/27          DV5K966726             *
*                                                                      *
* Short Description: FLCM Stabilization defect 566                     *
*                    Add new KONV subtype ZGR1                         *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K960962             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&       Class LCL_EXTRACT
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_extract DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_bsik,
        lifnr TYPE bsik-lifnr,
        bukrs TYPE bsik-bukrs,
        gjahr TYPE bsik-gjahr,
        belnr TYPE bsik-belnr,
        buzei TYPE bsik-buzei,
        bldat TYPE bsik-bldat,
        cpudt TYPE bsik-cpudt,
        waers TYPE bsik-waers,
        blart TYPE bsik-blart,
        bschl TYPE bsik-bschl,
        shkzg TYPE bsik-shkzg,                              "DVSK904466
        wrbtr TYPE bsik-wrbtr,
        zfbdt TYPE bsik-zfbdt,
        zbd1t TYPE bsik-zbd1t,
      END OF t_bsik,
      t_bsik_table TYPE STANDARD TABLE OF t_bsik
                                  WITH DEFAULT KEY,

      BEGIN OF t_recipient,
        name        TYPE c LENGTH 50,
        address     TYPE ad_smtpadr,
        comm_method TYPE adcp-deflt_comm,    "I - XT21609 - DV5K9A07FB - CR333910 TK356363
      END OF t_recipient,
      t_recipient_table TYPE STANDARD TABLE OF t_recipient
                                  WITH DEFAULT KEY,
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      BEGIN OF t_recipient_s3,
        namev      TYPE knvk-namev,
        name1      TYPE knvk-name1,
        prsnr      TYPE knvk-prsnr,
        pafkt      TYPE knvk-pafkt,
        persnumber TYPE adcp-persnumber,
        addrnumber TYPE adcp-addrnumber,
        deflt_comm TYPE adcp-deflt_comm,
      END OF t_recipient_s3.

    DATA: i_recipient_s3 TYPE STANDARD TABLE OF t_recipient_s3.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    METHODS:
      constructor
        IMPORTING
          im_lifnr TYPE lifnr,

      collect_vendor_data
        IMPORTING
          im_bsik TYPE t_bsik,

      data_to_report
        RETURNING
          VALUE(re_data) TYPE xfeld,

      vendor_number
        IMPORTING
          im_shift_left   TYPE xfeld OPTIONAL
        RETURNING
          VALUE(re_lifnr) TYPE lifnr,

      vendor_address
        RETURNING
          VALUE(re_address) TYPE soli_tab,

      vendor_country
        RETURNING
          VALUE(re_land1) TYPE lfa1-land1,

      vendor_contact
        RETURNING
          VALUE(re_contact) TYPE soli,

      email_preferred
        RETURNING
          VALUE(re_email) TYPE xfeld,

      fax_preferred
        RETURNING
          VALUE(re_fax) TYPE xfeld,

      get_recipients
        RETURNING
          VALUE(re_cipients) TYPE t_recipient_table,

      period_start_date
        RETURNING
          VALUE(re_begda) TYPE char10,

      period_end_date
        RETURNING
          VALUE(re_endda) TYPE char10,

      format_pdf
        IMPORTING
          im_preview TYPE xfeld OPTIONAL
        CHANGING
          ch_pdf     TYPE fpcontent OPTIONAL,
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      format_pdf_it
        IMPORTING
          im_preview TYPE xfeld OPTIONAL
        CHANGING
          ch_pdf_it  TYPE fpcontent OPTIONAL,

      format_csv_it
        CHANGING
          ch_csv_it TYPE soli_tab,
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      format_csv
        CHANGING
          ch_csv TYPE soli_tab,

      format_alv
        CHANGING
          ch_alv TYPE zfi_firpt7h6_table_to_alv,

      get_bdms_key
        RETURNING
          VALUE(re_bdms) TYPE bapibds01-objkey.

    CLASS-METHODS:
      class_constructor,

      get_next
        RETURNING
          VALUE(re_bsik) TYPE t_bsik,

      locate_vendor
        IMPORTING
          im_lifnr          TYPE lifnr
          im_create         TYPE xfeld OPTIONAL
        RETURNING
          VALUE(re_pointer) TYPE REF TO lcl_extract,

      vendor_by_index
        IMPORTING
          im_index          TYPE i
        RETURNING
          VALUE(re_pointer) TYPE REF TO lcl_extract.

  PRIVATE SECTION.

    CONSTANTS:
***Start of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
*      c_email_preferred      TYPE lfa1-esrnr VALUE 'FUELEMAIL',
*      c_fax_preferred        TYPE lfa1-esrnr VALUE 'FUELFAX',
***End of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
      c_classname  TYPE bds_clsnam VALUE 'ZSMFUELINVSUM',
      c_classtype  TYPE bds_clstyp VALUE 'OT',
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      c_email      TYPE adcp-deflt_comm VALUE 'INT',
      c_fax        TYPE adcp-deflt_comm VALUE 'FAX',
      c_form_ca_it TYPE fpwbformname
                             VALUE 'ZFI_RPT_7H6_FLCM_SMRY_CA_IT',
      c_form_us_it TYPE fpwbformname
                             VALUE 'ZFI_RPT_7H6_FLCM_SMRY_US_IT',
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      c_form_ca    TYPE fpwbformname
                             VALUE 'ZFI_RPT_7H6_FLCM_WKLY_SMRY_CA',
      c_form_us    TYPE fpwbformname
                             VALUE 'ZFI_RPT_7H6_FLCM_WKLY_SMRY_US'.

    TYPES:
      BEGIN OF t_object_index,
        lifnr   TYPE lifnr,
        pointer TYPE REF TO lcl_extract,
      END OF t_object_index,
      t_object_index_table TYPE STANDARD TABLE OF t_object_index
                                  WITH DEFAULT KEY,

      BEGIN OF t_extra_data,
        bukrs TYPE bsik-bukrs,
        gjahr TYPE bsik-gjahr,
        blart TYPE bsik-blart,
        konnr TYPE ekko-konnr,
        ktpnr TYPE ekpo-ktpnr,
      END OF t_extra_data,
      t_extra_data_table TYPE STANDARD TABLE OF t_extra_data
                                  WITH DEFAULT KEY,

      BEGIN OF t_ekpo,
        loekz TYPE ekpo-loekz,
        txz01 TYPE ekpo-txz01,
        matnr TYPE ekpo-matnr,
        bednr TYPE ekpo-bednr,
        matkl TYPE ekpo-matkl,
        menge TYPE ekpo-menge,
        meins TYPE ekpo-meins,
        konnr TYPE ekpo-konnr,
        ktpnr TYPE ekpo-ktpnr,
      END OF t_ekpo,
      t_ekpo_table TYPE STANDARD TABLE OF t_ekpo
                                  WITH DEFAULT KEY,
      BEGIN OF t_ekko,
        ebeln  TYPE ekko-ebeln,
        unsez  TYPE ekko-unsez,
        ihrez  TYPE ekko-ihrez,
        knumv  TYPE ekko-knumv,
        bypass TYPE xfeld,
      END OF t_ekko,
      t_ekko_table TYPE STANDARD TABLE OF t_ekko
                                  WITH DEFAULT KEY,

      BEGIN OF t_konv,
        kposn TYPE konv-kposn,
        kschl TYPE konv-kschl,
        kwert TYPE konv-kwert,
      END OF t_konv,
      t_konv_table TYPE STANDARD TABLE OF t_konv
                                  WITH DEFAULT KEY,

      BEGIN OF t_mseg,
        ebeln TYPE ebeln,                                   "DV5K968325
        ebelp TYPE ebelp,                                   "DV5K968325
        wempf TYPE wempf,                                   "DV5K968325
      END OF t_mseg,                                        "DV5K968325
      t_mseg_table TYPE SORTED TABLE OF t_mseg              "DV5K968325
                                  WITH UNIQUE KEY ebeln ebelp, "DV5K968325

      BEGIN OF text_type,                                  "DV5K9A05WW+
        ebeln  TYPE ebeln,                                 "DV5K9A05WW+
        tdline TYPE tdline,                                "DV5K9A05WW+
      END OF text_type,                                    "DV5K9A05WW+

      BEGIN OF t_knvk,
        namev      TYPE knvk-namev,
        name1      TYPE knvk-name1,
        anred      TYPE knvk-anred,
        pafkt      TYPE knvk-pafkt,
        prsnr      TYPE knvk-prsnr,
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
        persnumber TYPE adcp-persnumber,
        addrnumber TYPE adcp-addrnumber,
        deflt_comm TYPE adcp-deflt_comm,
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      END OF t_knvk,
      t_knvk_table TYPE STANDARD TABLE OF t_knvk
                                  WITH DEFAULT KEY.

    DATA:
      v_lifnr_flag_empty TYPE char1,                    "I - XT21609 - DV5K9A07FB - CR333910 TK356363
      v_lifnr            TYPE bsik-lifnr,
      v_land1            TYPE lfa1-land1,
      v_adrnr            TYPE lfa1-adrnr,
*      v_esrnr                TYPE lfa1-esrnr,              "D - XT21609 - DV5K9A07FB - CR333910 TK356363

      v_pdf_data         TYPE zfi_firpt7h6_extract_to_pdf,

      v_bds_object       TYPE bapibds01-objkey,
      i_recipients       TYPE t_recipient_table,
      i_texttab          TYPE SORTED TABLE OF text_type "DV5K9A05WW+
                                  WITH UNIQUE KEY ebeln,    "DV5K9A05WW+
      i_extra_data_cad   TYPE t_extra_data_table,
      i_extra_data_usd   TYPE t_extra_data_table,

      v_knumv            TYPE ekko-knumv,
      i_konv_data        TYPE t_konv_table,
      i_mseg_data        TYPE t_mseg_table.                 "DV5K968325

    CLASS-DATA:
      i_bsik               TYPE t_bsik_table,
      i_object_index       TYPE t_object_index_table,
      i_ekko               TYPE t_ekko_table,
      o_base_descr         TYPE REF TO cl_abap_structdescr,
      o_line_descr         TYPE REF TO cl_abap_structdescr,
      v_index              TYPE i,
      v_datum              TYPE dats,
      v_uzeit              TYPE tims,
*      v_cpudt_min            TYPE dats,
*      v_cpudt_max            TYPE dats,
*      v_bldat_min            TYPE dats,                    "D - XT21609 - DV5K9A07FB - CR333910 TK356363
*      v_bldat_max            TYPE dats,                    "D - XT21609 - DV5K9A07FB - CR333910 TK356363
      v_form_ca            TYPE funcname,
      v_form_us            TYPE funcname,
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      v_form_ca_it         TYPE funcname,
      v_form_us_it         TYPE funcname,
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      v_bds_mimetype       TYPE bds_mimetp,
      v_bds_logical_system TYPE tbdls-logsys.

    METHODS:
      vendor_recipients
        IMPORTING
          im_lifnr      TYPE bsik-lifnr
*          im_esrnr           TYPE lfa1-esrnr           "D - XT21609 - DV5K9A07FB - CR333910 TK356363
        CHANGING
          ch_recipients TYPE t_recipient_table,

      add_ekko_data
        IMPORTING
          im_ebeln  TYPE ekko-ebeln
        CHANGING
          ch_unsez  TYPE c
          ch_ihrez  TYPE c
          ch_knumv  TYPE c
          ch_bypass TYPE xfeld,

*--> Start of insert DV5K9A05WW - Chuong Le - 2017/01/04
      get_po_header_text
        IMPORTING
          im_ebeln       TYPE ekko-ebeln
        RETURNING
          VALUE(re_text) TYPE string,
*<-- End of insert DV5K9A05WW - Chuong Le - 2017/01/04

      add_ekpo_data
        IMPORTING
          im_data_line   TYPE zfi_firpt7h6_line_data
          im_bukrs       TYPE bsik-bukrs
          im_gjahr       TYPE bsik-gjahr
          im_blart       TYPE bsik-blart
        CHANGING
          ch_data_table  TYPE zfi_firpt7h6_table_data
          ch_extra_table TYPE t_extra_data_table
          ch_total_line  TYPE zfi_firpt7h6_line_data,

      add_konv_data
        IMPORTING
          im_knumv           TYPE knumv
          im_ebelp           TYPE ebelp
          im_shkzg           TYPE shkzg                     "DVSK904466
          im_xnegp           TYPE xnegp                     "DVSK904592
        CHANGING
          ch_unit_price      TYPE kwert
          ch_federal_spill   TYPE fwste
          ch_federal_lust    TYPE fwste
          ch_excise_tax      TYPE fwste
          ch_local_fuel_fees TYPE fwste,

      add_mseg_data                                         "DV5K968325
        IMPORTING                                           "DV5K968325
          im_ebeln TYPE ebeln                               "DV5K968325
          im_ebelp TYPE ebelp                               "DV5K968325
          im_gjahr TYPE gjahr                               "DV5K968325
        CHANGING                                            "DV5K968325
          ch_wempf TYPE wempf,                              "DV5K968325

      lookup_taxes
        IMPORTING
          im_bukrs   TYPE bsik-bukrs
          im_belnr   TYPE bsik-belnr
          im_gjahr   TYPE bsik-gjahr
        CHANGING
          ch_gst_hst TYPE fwste
          ch_pst_qst TYPE fwste
          ch_us_tax  TYPE fwste,

      add_to_total
        IMPORTING
          im_data_line TYPE zfi_firpt7h6_line_data
        CHANGING
          ch_total     TYPE zfi_firpt7h6_line_data,

      csv_data_line
        IMPORTING
          im_data_line TYPE zfi_firpt7h6_line_data
        CHANGING
          ch_result    TYPE c,
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      csv_data_line_it
        IMPORTING
          im_data_line TYPE zfi_firpt7h6_line_data
        CHANGING
          ch_result    TYPE c,
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      csv_field
        IMPORTING
          im_value       TYPE any
          im_type_kind   TYPE inttype OPTIONAL
          im_descr       TYPE REF TO cl_abap_structdescr OPTIONAL
          im_fieldname   TYPE c OPTIONAL
          im_decimals    TYPE i OPTIONAL
          im_concatenate TYPE xfeld OPTIONAL
        CHANGING
          ch_result      TYPE c,

      store_in_bdms
        IMPORTING
          im_pdf    TYPE fpcontent
        EXPORTING
          ex_object TYPE bapibds01-objkey.

    CLASS-METHODS:
      material_group
        IMPORTING
          im_matkl        TYPE matkl
        RETURNING
          VALUE(re_group) TYPE wgbez,

      vendor_email
        IMPORTING
          im_prsnr       TYPE ad_persnum
        RETURNING
          VALUE(re_addr) TYPE ad_smtpadr,

      vendor_fax
        IMPORTING
          im_prsnr         TYPE ad_persnum
        RETURNING
          VALUE(re_number) TYPE ad_fxnmbr.

ENDCLASS.               "LCL_EXTRACT

*----------------------------------------------------------------------*
*       CLASS LCL_EXTRACT IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_extract IMPLEMENTATION.
  METHOD constructor.
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    DATA: l_loevm TYPE lfa1-loevm,
          l_sperr TYPE lfa1-sperr,
          l_sperm TYPE lfa1-sperm.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    FIELD-SYMBOLS: <contact> TYPE t_recipient.

    CLEAR v_lifnr_flag_empty.                               "I - XT21609 - DV5K9A07FB - CR333910 TK356363

    v_lifnr = v_pdf_data-lifnr = im_lifnr.

    v_pdf_data-cdate = v_datum.
*   v_pdf_data-begda = v_bldat_min.                          DV5K973130
*   v_pdf_data-endda = v_bldat_max.                          DV5K973130
    v_pdf_data-begda = '99990101'.                          "DV5K973130
    v_pdf_data-endda = '11110101'.                          "DV5K973130

    SELECT SINGLE land1
                  name1
                  ort01
                  pstlz
                  regio
                  stras
                  adrnr
*                  esrnr                  "D - XT21609 - DV5K9A07FB - CR333910 TK356363
                  stcd1
                  stcd2
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
                  loevm
                  sperr
                  sperm
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      FROM lfa1
      INTO (v_land1,
            v_pdf_data-name1,
            v_pdf_data-ort01,
            v_pdf_data-pstlz,
            v_pdf_data-regio,
            v_pdf_data-stras,
            v_adrnr,
*            v_esrnr,                     "D - XT21609 - DV5K9A07FB - CR333910 TK356363
            v_pdf_data-stcd1,
            v_pdf_data-stcd2,
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
            l_loevm,
            l_sperr,
            l_sperm)
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      WHERE lifnr = im_lifnr.

    IF sy-subrc > 0.
      RETURN.
    ENDIF.
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    IF p_consul = abap_true.
      IF l_loevm = abap_true
      OR l_sperr = abap_true
      OR l_sperm = abap_true.
        v_lifnr_flag_empty = abap_true.
      ENDIF.
    ENDIF.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
***Start of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
*    CONDENSE v_esrnr NO-GAPS.
*    TRANSLATE v_esrnr TO UPPER CASE.                      "#EC SYNTCHAR
*    IF NOT ( v_esrnr = c_email_preferred OR
*             v_esrnr = c_fax_preferred ).
*      MESSAGE i023(zz_flcm) WITH v_lifnr.
*    ENDIF.
***End of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
    IF p_email = abap_true.
      CALL METHOD vendor_recipients
        EXPORTING
          im_lifnr      = im_lifnr
*         im_esrnr      = v_esrnr        "D - XT21609 - DV5K9A07FB - CR333910 TK356363
        CHANGING
          ch_recipients = i_recipients[].
      READ TABLE i_recipients ASSIGNING <contact> INDEX 1.
      IF sy-subrc = 0.
        v_pdf_data-contact_name = <contact>-name.
***Start of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
*        CASE v_esrnr.
*          WHEN c_email_preferred.
*            v_pdf_data-contact_email = <contact>-address.
*          WHEN c_fax_preferred.
*            v_pdf_data-fax = <contact>-address.
*        ENDCASE.
***End of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      ELSE.
        "Vendor contact not indicated
        MESSAGE i110(zz_flcm) WITH im_lifnr.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      ENDIF.
    ENDIF.

    IF sy-sysid = 'PRD'.
      CLEAR: v_pdf_data-reference.
    ELSE.        "Non-Prod systems only
      CONCATENATE '*** TEST' sy-sysid sy-uname
          INTO v_pdf_data-reference SEPARATED BY '-'.
      CONCATENATE v_pdf_data-reference '***'
          INTO v_pdf_data-reference SEPARATED BY space.
    ENDIF.

  ENDMETHOD.                    "constructor

  METHOD vendor_recipients.
    CONSTANTS: lc_pafkts3 TYPE knvk-pafkt VALUE 'S3'.           "I - XT21609 - DV5K9A07FB - CR333910 TK356363

    DATA: li_knvk     TYPE t_knvk_table,
          l_recipient TYPE t_recipient.

    FIELD-SYMBOLS: <knvk>    TYPE t_knvk.
***Start of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
*    SELECT namev
*           name1
*           anred
*           pafkt
*           prsnr
*      FROM knvk
*      INTO TABLE li_knvk
*      WHERE lifnr = im_lifnr.
*    LOOP AT li_knvk ASSIGNING <knvk>.
*        WHERE pafkt = 'Z1'.
*      CASE im_esrnr.
*        WHEN c_email_preferred.
*          CONCATENATE <knvk>-namev <knvk>-name1
*              INTO l_recipient-name SEPARATED BY space.
*          l_recipient-address = lcl_extract=>vendor_email( <knvk>-prsnr ).
*          IF l_recipient-address = '*** Unknown ***'(t01).
*            MESSAGE i021(zz_flcm) WITH <knvk>-prsnr im_lifnr.
*          ELSE.
*            APPEND l_recipient TO ch_recipients.
*          ENDIF.
*        WHEN c_fax_preferred.
*          CONCATENATE <knvk>-namev <knvk>-name1
*              INTO l_recipient-name SEPARATED BY space.
*          l_recipient-address = lcl_extract=>vendor_fax( <knvk>-prsnr ).
*          IF l_recipient-address = '*** Unknown ***'(t01).
*            MESSAGE i022(zz_flcm) WITH <knvk>-prsnr im_lifnr.
*          ELSE.
*            APPEND l_recipient TO ch_recipients.
*          ENDIF.
*      ENDCASE.
*    ENDLOOP.
***End of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
*    Person Number and Communication Method
    SELECT knvk~namev,
           knvk~name1,
           knvk~prsnr,
           knvk~pafkt,
           adcp~persnumber,
           adcp~addrnumber,
           adcp~deflt_comm
      INTO TABLE @i_recipient_s3
      FROM knvk AS knvk
      INNER JOIN adcp AS adcp
      ON adcp~persnumber    = knvk~prsnr
      WHERE knvk~lifnr      = @im_lifnr
      AND   knvk~pafkt      = @lc_pafkts3
      AND   adcp~addrnumber = @v_adrnr.
    IF sy-subrc = 0.
      SORT i_recipient_s3 BY prsnr.
      DATA(li_temp_recipient) = i_recipient_s3[].
      DELETE ADJACENT DUPLICATES FROM li_temp_recipient COMPARING prsnr.
*      For Email
      SELECT persnumber,
             addrnumber,
             smtp_addr
        INTO TABLE @DATA(li_comm_email)
        FROM adr6
        FOR ALL ENTRIES IN @li_temp_recipient
        WHERE addrnumber = @v_adrnr
        AND   persnumber = @li_temp_recipient-prsnr
        AND   flgdefault = @abap_true.
      IF sy-subrc <> 0.
        CLEAR li_comm_email.
      ENDIF.
*      For FAX
      SELECT persnumber,
             addrnumber,
             fax_number,
             fax_extens
        INTO TABLE @DATA(li_comm_fax)
        FROM adr3
        FOR ALL ENTRIES IN @li_temp_recipient
        WHERE addrnumber = @v_adrnr
        AND   persnumber = @li_temp_recipient-prsnr
        AND   flgdefault = @abap_true.
      IF sy-subrc <> 0.
        CLEAR li_comm_fax.
      ENDIF.

      LOOP AT i_recipient_s3 ASSIGNING FIELD-SYMBOL(<lfs_recipient>).
        CASE <lfs_recipient>-deflt_comm.
          WHEN c_email.
            READ TABLE li_comm_email ASSIGNING FIELD-SYMBOL(<lfs_comm_email>)
                WITH KEY persnumber = <lfs_recipient>-persnumber
                         addrnumber = <lfs_recipient>-addrnumber.
            IF sy-subrc = 0.
              l_recipient-name    = |{ <lfs_recipient>-namev } { <lfs_recipient>-name1 }|.
              l_recipient-address = <lfs_comm_email>-smtp_addr.
              l_recipient-comm_method = c_email.
              APPEND l_recipient TO ch_recipients.
            ELSE.
              "Email address not maintained in vendor contact for im_lifnr message
              MESSAGE i021(zz_flcm) WITH <lfs_recipient>-persnumber im_lifnr.
            ENDIF.
            UNASSIGN <lfs_comm_email>.
          WHEN c_fax.
            READ TABLE li_comm_fax ASSIGNING FIELD-SYMBOL(<lfs_comm_fax>)
                WITH KEY persnumber = <lfs_recipient>-persnumber
                         addrnumber = <lfs_recipient>-addrnumber.
            IF sy-subrc = 0.
              l_recipient-name    = |{ <lfs_recipient>-namev } { <lfs_recipient>-name1 }|.
              l_recipient-address = |{ <lfs_comm_fax>-fax_number } { <lfs_comm_fax>-fax_extens }|.
              l_recipient-comm_method = c_fax.
              APPEND l_recipient TO ch_recipients.
            ELSE.
              "Fax not maintained in vendor contact for im_lifnr message
              MESSAGE i022(zz_flcm) WITH <lfs_recipient>-persnumber im_lifnr.
            ENDIF.
            UNASSIGN <lfs_comm_fax>.
          WHEN OTHERS.

        ENDCASE.
      ENDLOOP.
      UNASSIGN <lfs_recipient>.
    ENDIF.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    SORT i_recipients BY address.
    DELETE ADJACENT DUPLICATES FROM ch_recipients
        COMPARING address.

  ENDMETHOD.                    "vendor_recipients

  METHOD collect_vendor_data.

    TYPES: BEGIN OF lt_bseg,
             ebeln TYPE bseg-ebeln,
             ebelp TYPE bseg-ebelp,
             buzid TYPE bseg-buzid,
             xnegp TYPE bseg-xnegp,                         "DVSK904592
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
             nplnr TYPE bseg-nplnr,
             wrbtr TYPE bseg-wrbtr,
             menge TYPE bseg-menge,
             meins TYPE bseg-meins,
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
           END OF lt_bseg.
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    CONSTANTS: lc_ltri  TYPE c     VALUE 'I',
               lc_equal TYPE char2 VALUE 'EQ',
               lc_ltrw  TYPE c     VALUE 'W',
               lc_ltrs  TYPE c     VALUE 'S',
***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
               lc_cad   TYPE char3 VALUE 'CAD',
               lc_usd   TYPE char3 VALUE 'USD',
               lc_inv_total TYPE string VALUE 'INV TOTAL'.
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***

    DATA: li_bseg      TYPE STANDARD TABLE OF lt_bseg,
          l_data_line  TYPE zfi_firpt7h6_line_data,
          l_bypass     TYPE xfeld,
*          l_extra_data         TYPE t_extra_data,
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
          li_bseg_temp TYPE STANDARD TABLE OF lt_bseg,
          l_wrbtr      TYPE bseg-wrbtr,
          l_menge      TYPE bseg-menge,
          lr_gjahr     TYPE RANGE OF bkpf-gjahr,
          lwa_gjahr    LIKE LINE OF lr_gjahr,
          l_num        TYPE i.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    FIELD-SYMBOLS: <bseg>    TYPE lt_bseg.

* Set date range for heading
    IF im_bsik-bldat LT v_pdf_data-begda.                   "DV5K973130
      MOVE im_bsik-bldat TO v_pdf_data-begda.               "DV5K973130
    ENDIF.                                                  "DV5K973130
    IF im_bsik-bldat GT v_pdf_data-endda.                   "DV5K973130
      MOVE im_bsik-bldat TO v_pdf_data-endda.               "DV5K973130
    ENDIF.                                                  "DV5K973130
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    IF p_consul = abap_true.
*      SELECT awkey,                                        "D - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
      SELECT belnr,
             bukrs,
             gjahr,
             awkey
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
        INTO TABLE @DATA(li_bkpf)
        FROM bkpf
        WHERE belnr = @im_bsik-belnr
        AND   bukrs = @im_bsik-bukrs                        "I - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
        AND   gjahr = @im_bsik-gjahr.
      IF sy-subrc = 0.
        SORT li_bkpf BY awkey.
        LOOP AT li_bkpf ASSIGNING FIELD-SYMBOL(<lfs_bkpf>).
          lwa_gjahr-sign   = lc_ltri.
          lwa_gjahr-option = lc_equal.
          lwa_gjahr-low    = <lfs_bkpf>-awkey+10(4).
          APPEND lwa_gjahr TO lr_gjahr.
          CLEAR lwa_gjahr.
        ENDLOOP.
        UNASSIGN <lfs_bkpf>.

*        SELECT packno,                                     "D - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
        SELECT belnr,
               gjahr,
               buzei,
               lfbnr,
               packno
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
          INTO TABLE @DATA(li_rseg)
          FROM rseg
          FOR ALL ENTRIES IN @li_bkpf
          WHERE belnr = @li_bkpf-awkey(10)
          AND   gjahr IN @lr_gjahr.
        IF sy-subrc = 0.
          SORT li_rseg BY packno.

*          SELECT sdate,                                                          "D - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
          SELECT packno,
                 introw,
                 sdate,
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
                 tbtwr
            INTO TABLE @DATA(li_esll)
            FROM esll
            FOR ALL ENTRIES IN @li_rseg
            WHERE packno = @li_rseg-packno.
          IF sy-subrc = 0.
            SORT li_esll BY sdate.
            DATA(l_dates) = lines( li_esll ).
            LOOP AT li_esll ASSIGNING FIELD-SYMBOL(<lfs_esll>).
              IF sy-tabix = 1.
                l_data_line-period_from = <lfs_esll>-sdate.
                l_data_line-unit_price  = <lfs_esll>-tbtwr.
              ELSEIF sy-tabix = l_dates.
                l_data_line-period_to = <lfs_esll>-sdate.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDLOOP.
            UNASSIGN <lfs_esll>.
          ENDIF.
***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
          "Get each PIN associated to the CN Invoice No / PO Line Item
          DATA(ls_bkpf) = VALUE #( li_bkpf[ belnr = im_bsik-belnr
                                            bukrs = im_bsik-bukrs
                                            gjahr = im_bsik-gjahr ] OPTIONAL ).

          DATA(lt_rseg) = li_rseg[].
          DELETE lt_rseg WHERE ( belnr <> ls_bkpf-awkey(10) AND
                                 gjahr <> im_bsik-gjahr ).
          lt_rseg = VALUE #( FOR <ls_rseg> IN li_rseg WHERE ( belnr = ls_bkpf-awkey(10) AND
                                                              gjahr = im_bsik-gjahr )
                                ( CORRESPONDING #( <ls_rseg> ) ) ).

          "Get Package numbers from ESSR table
          SELECT  lblni,                      "#EC CI_SEL_NESTED
                  packno
            FROM  essr
            INTO TABLE @DATA(lt_essr)
            FOR ALL ENTRIES IN @lt_rseg
            WHERE lblni = @lt_rseg-lfbnr.
          IF sy-subrc = 0.

            "Get the list of PIN numbers and Qty (number of hours) from ML_ESLL
            SELECT  packno,                   "#EC CI_SEL_NESTED
                    introw,
                    pernr,
                    menge
              FROM  ml_esll
              INTO TABLE @DATA(lt_ml_esll)
              FOR ALL ENTRIES IN @lt_essr
              WHERE fpackno = @lt_essr-packno
                AND packno <> @lt_essr-packno.
            IF sy-subrc = 0.

              "Add all number of hours data for each unique pin
              DATA(lt_ml_esll_tmp) = lt_ml_esll[].

              SORT lt_ml_esll_tmp BY pernr.
              DELETE ADJACENT DUPLICATES FROM lt_ml_esll_tmp COMPARING pernr.

              LOOP AT lt_ml_esll ASSIGNING FIELD-SYMBOL(<ls_ml_esll>)
               GROUP BY ( pernr = <ls_ml_esll>-pernr )
               REFERENCE INTO DATA(lt_group_pernr).

                DATA(lv_menge_total_pin) = VALUE mengev( ).
                LOOP AT GROUP lt_group_pernr ASSIGNING FIELD-SYMBOL(<ls_group_pernr>).

                  lv_menge_total_pin = lv_menge_total_pin + <ls_group_pernr>-menge.

                ENDLOOP. " LOOP AT GROUP lt_group_pernr ASSIGNING FIELD-SYMBOL(<ls_group_pernr>).

                "Replace menge with the total menge value
                READ TABLE lt_ml_esll_tmp ASSIGNING FIELD-SYMBOL(<ls_ml_esll_tmp>)
                 WITH KEY pernr = lt_group_pernr->pernr BINARY SEARCH.
                IF sy-subrc = 0.

                  <ls_ml_esll_tmp>-menge = lv_menge_total_pin.

                ENDIF. " IF sy-subrc = 0.

                CLEAR:
                  lv_menge_total_pin.

              ENDLOOP. " LOOP AT lt_ml_esll ASSIGNING FIELD-SYMBOL(<ls_ml_esll>)

            ENDIF. " IF sy-subrc = 0.

          ENDIF. " IF sy-subrc = 0.
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
        ENDIF.
      ENDIF.
    ENDIF.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    l_data_line-bldat = im_bsik-bldat.
    l_data_line-belnr = im_bsik-belnr.
    l_data_line-currency = im_bsik-waers.
    l_data_line-due_date = im_bsik-zfbdt + im_bsik-zbd1t.

    l_data_line-shkzg = im_bsik-shkzg.                      "DVSK904466
    IF im_bsik-shkzg = 'S'.                                 "DVSK904466
      l_data_line-invoice_amount = 0 - im_bsik-wrbtr.       "DVSK904466
    ELSE.                                                   "DVSK904466
      l_data_line-invoice_amount = im_bsik-wrbtr.           "DVSK904466
    ENDIF.                                                  "DVSK904466
*    IF im_bsik-blart = 'K0'.                               "DVSK904466
*      IF im_bsik-bschl = '21' OR                           "DVSK904466
*         im_bsik-bschl = '27'.                             "DVSK904466
*        l_data_line-invoice_amount = 0 - im_bsik-wrbtr.    "DVSK904466
*      ELSE.                                                "DVSK904466
*        l_data_line-invoice_amount = im_bsik-wrbtr.        "DVSK904466
*      ENDIF.                                               "DVSK904466
*    ENDIF.                                                 "DVSK904466

    IF im_bsik-blart = 'KB' OR
       im_bsik-blart = 'K0'.
      l_data_line-ers_indicator = 'ERS'.
    ENDIF.

    SELECT ebeln
           ebelp
           buzid
           xnegp                                            "DVSK904592
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
           nplnr
           wrbtr
           menge
           meins
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      FROM bseg
      INTO TABLE li_bseg
      WHERE bukrs = im_bsik-bukrs AND
            belnr = im_bsik-belnr AND
            gjahr = im_bsik-gjahr AND
            buzid IN ('S', 'W').
    IF sy-subrc > 0.
      RETURN.       "BSEG not found; exit
    ENDIF.

    READ TABLE li_bseg ASSIGNING <bseg>
        WITH KEY buzid = 'W'.
*    IF sy-subrc > 0.                         "D - XT21609 - DV5K9A07FB - CR333910 TK356363
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    IF sy-subrc = 0.
*      If there are more than 1 data, add quantity and amount
      REFRESH li_bseg_temp.
      CLEAR: l_wrbtr,
             l_menge,
             l_num.
      li_bseg_temp = li_bseg[].
      DELETE li_bseg_temp WHERE buzid <> lc_ltrw.
      l_num = lines( li_bseg_temp ).
      IF l_num > 1.
        LOOP AT li_bseg_temp ASSIGNING FIELD-SYMBOL(<lfs_bseg_temp_w>).
*          l_wrbtr = l_wrbtr + <lfs_bseg_temp_w>-wrbtr.
          l_menge = l_menge + <lfs_bseg_temp_w>-menge.
        ENDLOOP.
        UNASSIGN <lfs_bseg_temp_w>.
*        l_data_line-invoice_amount = l_wrbtr.
        l_data_line-volume         = l_menge.
      ELSE.
*        l_data_line-invoice_amount = <bseg>-wrbtr.
        l_data_line-volume         = <bseg>-menge.
      ENDIF.
      l_data_line-unit_of_measure = <bseg>-meins.
    ELSE.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      READ TABLE li_bseg ASSIGNING <bseg>
          WITH KEY buzid = 'S'.
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      IF sy-subrc = 0.
*        If there are more than 1 data, add quantity and amount
        REFRESH li_bseg_temp.
        CLEAR: l_wrbtr,
               l_menge,
               l_num.
        li_bseg_temp = li_bseg[].
        DELETE li_bseg_temp WHERE buzid <> lc_ltrs.
        l_num = lines( li_bseg_temp ).
        IF l_num > 1.
          LOOP AT li_bseg_temp ASSIGNING FIELD-SYMBOL(<lfs_bseg_temp_s>).
*            l_wrbtr = l_wrbtr + <lfs_bseg_temp_s>-wrbtr.
            l_menge = l_menge + <lfs_bseg_temp_s>-menge.
          ENDLOOP.
          UNASSIGN <lfs_bseg_temp_s>.
          l_data_line-volume         = l_menge.
*          l_data_line-invoice_amount = l_wrbtr.
        ELSE.
          l_data_line-volume         = <bseg>-menge.
*          l_data_line-invoice_amount = <bseg>-wrbtr.
        ENDIF.
        l_data_line-unit_of_measure = <bseg>-meins.
      ENDIF.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    ENDIF.
    l_data_line-amount_before_tax = l_data_line-unit_price * l_data_line-volume.          "I - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
    l_data_line-ebeln = <bseg>-ebeln.
    l_data_line-ebelp = <bseg>-ebelp.
    l_data_line-xnegp = <bseg>-xnegp.                       "DVSK904592
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    IF p_consul = abap_true.
      l_data_line-network        = <bseg>-nplnr.
***Start of Delete - TXT21609 - RTSK0014488 DFCT0011652 - DV5K9A0U6O
****start of insert T182535 / DV5K9A0GNZ  C372902-T403637***
*      SELECT SINGLE pernr INTO l_data_line-pernr
*      FROM catsekko
*      WHERE sebeln = l_data_line-ebeln.
****end of insert T182535 / DV5K9A0GNZ  C372902-T403637***
***End of Delete - TXT21609 - RTSK0014488 DFCT0011652 - DV5K9A0U6O
    ENDIF.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    CALL METHOD add_ekko_data
      EXPORTING
        im_ebeln  = l_data_line-ebeln
      CHANGING
        ch_unsez  = l_data_line-ztds_trml_id
        ch_ihrez  = l_data_line-ztds_bol_nbr
        ch_knumv  = l_data_line-knumv
        ch_bypass = l_bypass.
    IF l_bypass = abap_false.
      CALL METHOD add_mseg_data                             "DV5K968325
        EXPORTING                                           "DV5K968325
          im_ebeln = l_data_line-ebeln                      "DV5K968325
          im_ebelp = l_data_line-ebelp                      "DV5K968325
          im_gjahr = im_bsik-gjahr                          "DV5K968325
        CHANGING                                            "DV5K968325
          ch_wempf = l_data_line-wempf.                     "DV5K968325

***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
      IF p_consul = abap_true.                                                              "I - TXT21609 - RTSK0014488 DFCT0011652 - DV5K9A0U6O
      l_data_line-pernr = COND #( WHEN LINES( lt_ml_esll_tmp ) > 1
                                    THEN lc_inv_total
                                    ELSE VALUE #( lt_ml_esll_tmp[ 1 ]-pernr OPTIONAL ) ).   "I - TXT21609 - RTSK0014488 DFCT0011652 - DV5K9A0U6O
*                                    ELSE l_data_line-pernr ).                              "D - TXT21609 - RTSK0014488 DFCT0011652 - DV5K9A0U6O

      SHIFT l_data_line-pernr LEFT DELETING LEADING '0'.
      ENDIF. " IF p_consul = abap_true.                                                     "I - TXT21609 - RTSK0014488 DFCT0011652 - DV5K9A0U6O
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9

      CASE im_bsik-waers.
        WHEN 'CAD'.
          CALL METHOD add_ekpo_data
            EXPORTING
              im_data_line   = l_data_line
              im_bukrs       = im_bsik-bukrs
              im_gjahr       = im_bsik-gjahr
              im_blart       = im_bsik-blart
            CHANGING
              ch_data_table  = v_pdf_data-data_table_cad[]
              ch_extra_table = i_extra_data_cad[]
              ch_total_line  = v_pdf_data-total_line_cad.
        WHEN 'USD'.
          CALL METHOD add_ekpo_data
            EXPORTING
              im_data_line   = l_data_line
              im_bukrs       = im_bsik-bukrs
              im_gjahr       = im_bsik-gjahr
              im_blart       = im_bsik-blart
            CHANGING
              ch_data_table  = v_pdf_data-data_table_usd[]
              ch_extra_table = i_extra_data_usd[]
              ch_total_line  = v_pdf_data-total_line_usd.
      ENDCASE.
***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
      IF LINES( lt_ml_esll_tmp ) > 1.

        LOOP AT lt_ml_esll_tmp ASSIGNING <ls_ml_esll_tmp>.

          CLEAR:
            l_data_line-invoice_amount.

          l_data_line-volume            = <ls_ml_esll_tmp>-menge.
          l_data_line-amount_before_tax = l_data_line-unit_price * l_data_line-volume.
          l_data_line-pernr             = <ls_ml_esll_tmp>-pernr.

          CASE im_bsik-waers.
            WHEN lc_cad.

              l_data_line-delivery_locn = VALUE #( v_pdf_data-data_table_cad[
                                                      belnr = l_data_line-belnr ]-delivery_locn OPTIONAL ).
              l_data_line-product       = VALUE #( v_pdf_data-data_table_cad[
                                                      belnr = l_data_line-belnr ]-product OPTIONAL ).
              l_data_line-bednr       = VALUE #( v_pdf_data-data_table_cad[
                                                      belnr = l_data_line-belnr ]-bednr OPTIONAL ).
              APPEND l_data_line TO v_pdf_data-data_table_cad[].

            WHEN lc_usd.

              l_data_line-delivery_locn = VALUE #( v_pdf_data-data_table_usd[
                                                      belnr = l_data_line-belnr ]-delivery_locn OPTIONAL ).
              l_data_line-product       = VALUE #( v_pdf_data-data_table_usd[
                                                      belnr = l_data_line-belnr ]-product OPTIONAL ).
              l_data_line-bednr       = VALUE #( v_pdf_data-data_table_usd[
                                                      belnr = l_data_line-belnr ]-bednr OPTIONAL ).
              APPEND l_data_line TO v_pdf_data-data_table_usd[].

            WHEN OTHERS.
          ENDCASE. " CASE im_bsik-waers.

        ENDLOOP. " LOOP AT lt_ml_esll ASSIGNING <ls_ml_esll>.

      ENDIF.
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
    ENDIF.

  ENDMETHOD.                    "collect_vendor_data

  METHOD add_ekko_data.

    DATA: l_prefix           TYPE c LENGTH 3.

    FIELD-SYMBOLS: <ekko>    TYPE t_ekko.

    ch_bypass = abap_true.

    CLEAR: ch_unsez,
           ch_ihrez,
           ch_knumv.

    READ TABLE i_ekko ASSIGNING <ekko>
        WITH KEY ebeln = im_ebeln.

    IF sy-subrc > 0.
      APPEND INITIAL LINE TO i_ekko ASSIGNING <ekko>.
      <ekko>-ebeln = im_ebeln.
      <ekko>-bypass = abap_true.
*      SELECT SINGLE unsez                                 "DV5K9A05WW-
      SELECT SINGLE ihrez
                    knumv
        FROM ekko
*        INTO (<ekko>-unsez,                               "DV5K9A05WW-
        INTO (<ekko>-ihrez,
              <ekko>-knumv)
        WHERE ebeln = im_ebeln.

      IF sy-subrc > 0.
        RETURN.
      ENDIF.

      <ekko>-unsez = get_po_header_text( im_ebeln ).       "DV5K9A05WW+

      l_prefix = <ekko>-unsez(3).
      IF p_tds = abap_true OR
         p_dtl = abap_true OR
         p_car = abap_true.
        IF ( p_tds = abap_true AND l_prefix = 'TDS' ) OR
           ( p_dtl = abap_true AND l_prefix = 'DTL' ) OR
           ( p_car = abap_true AND l_prefix = 'CAR' ).
          <ekko>-bypass = abap_false.
        ENDIF.
      ELSE.
        <ekko>-bypass = abap_false.
      ENDIF.

      IF l_prefix = 'TDS' OR
         l_prefix = 'DTL' OR
         l_prefix = 'CAR'.
        SHIFT <ekko>-unsez LEFT BY 4 PLACES.
      ENDIF.

    ENDIF.

    IF <ekko>-unsez IN s_unsez.
      ch_unsez  = <ekko>-unsez.
      ch_ihrez  = <ekko>-ihrez.
      ch_knumv  = <ekko>-knumv.
      ch_bypass = <ekko>-bypass.
    ENDIF.

  ENDMETHOD.                    "add_ekko_data

*--> Start of insert DV5K9A05WW - Chuong Le - 2017/01/04
  METHOD get_po_header_text.

    DATA:
      lt_lines TYPE tline_tab.

    CLEAR re_text.

    IF line_exists( i_texttab[ ebeln = im_ebeln ] ).
      re_text = i_texttab[ ebeln = im_ebeln ]-tdline.
    ELSE.
      CLEAR lt_lines[].
      "Read English text
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = 'F99'
          language                = 'E'
          name                    = CONV tdobname( im_ebeln )
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
            name                    = CONV tdobname( im_ebeln )
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
        re_text = lt_lines[ 1 ]-tdline.     "1st line only
      ENDIF.

      "Add to PO text table
      i_texttab = VALUE #( BASE i_texttab ( ebeln  = im_ebeln
                                            tdline = re_text ) ).
    ENDIF.

  ENDMETHOD.                    "get_po_header_text
*<-- End of insert DV5K9A05WW - Chuong Le - 2017/01/04

  METHOD add_ekpo_data.

    DATA: l_data_line  TYPE zfi_firpt7h6_line_data,
          l_extra_data TYPE t_extra_data,
          li_ekpo      TYPE t_ekpo_table,
          l_price      TYPE kwert.

    FIELD-SYMBOLS: <ekpo>      TYPE t_ekpo.

    l_data_line = im_data_line.

    l_extra_data-bukrs = im_bukrs.
    l_extra_data-gjahr = im_gjahr.
    l_extra_data-blart = im_blart.

    CALL METHOD lookup_taxes
      EXPORTING
        im_bukrs   = im_bukrs
        im_belnr   = im_data_line-belnr
        im_gjahr   = im_gjahr
      CHANGING
        ch_gst_hst = l_data_line-gst_hst
        ch_pst_qst = l_data_line-pst_qst
        ch_us_tax  = l_data_line-us_tax.

    SELECT loekz
           txz01
           matnr
           bednr
           matkl
           menge
           meins
           konnr
           ktpnr
      FROM ekpo
      INTO TABLE li_ekpo
      WHERE ebeln = im_data_line-ebeln AND
            ebelp = im_data_line-ebelp.
    IF sy-subrc = 0.
      IF s_matnr IS NOT INITIAL.
        DELETE li_ekpo WHERE matnr NOT IN s_matnr.
      ENDIF.
    ENDIF.

    IF li_ekpo[] IS INITIAL.
      RETURN.       "nothing to process
    ENDIF.

    LOOP AT li_ekpo ASSIGNING <ekpo>
        WHERE loekz IS INITIAL.
      l_data_line-delivery_locn = <ekpo>-txz01.
      l_data_line-product = lcl_extract=>material_group( <ekpo>-matkl ).
      l_data_line-bednr = <ekpo>-bednr.
      IF im_blart <> 'K0'.   "Ignore this data for K0
        IF p_consul = abap_false.                             "I - XT21609 - DV5K9A07FB - CR333910 TK356363
          l_data_line-volume = <ekpo>-menge.
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
        ENDIF.
        IF p_consul = abap_false.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
          l_data_line-unit_of_measure = <ekpo>-meins.
          CALL METHOD add_konv_data
            EXPORTING
              im_knumv           = l_data_line-knumv
              im_ebelp           = l_data_line-ebelp
              im_shkzg           = l_data_line-shkzg          "DVSK904466
              im_xnegp           = l_data_line-xnegp          "DVSK904592
            CHANGING
              ch_unit_price      = l_price
              ch_federal_spill   = l_data_line-federal_spill
              ch_federal_lust    = l_data_line-federal_lust
              ch_excise_tax      = l_data_line-excise_tax
              ch_local_fuel_fees = l_data_line-local_fuel_fees.
          l_data_line-fuel_fees = l_data_line-federal_spill +
                                  l_data_line-federal_lust +
                                  l_data_line-excise_tax +
                                  l_data_line-local_fuel_fees.
          l_data_line-invoice_amount = l_price +
                                       l_data_line-fuel_fees +
                                       l_data_line-gst_hst +
                                       l_data_line-pst_qst +
                                       l_data_line-us_tax.
          l_data_line-unit_price = abs( l_price / <ekpo>-menge ). "DVSK904592
        ENDIF.                                                "I - XT21609 - DV5K9A07FB - CR333910 TK356363
*        l_data_line-unit_price = l_price / <ekpo>-menge.
      ENDIF.
      l_extra_data-konnr = <ekpo>-konnr.
      l_extra_data-ktpnr = <ekpo>-ktpnr.
*      IF l_data_line-unit_price < 0 AND l_data_line-volume > 0.
*      IF l_price < 0 AND l_data_line-volume > 0.            "DVSK904592      "D - XT21609 - DV5K9A07FB - CR333910 TK356363
      IF l_price < 0 AND l_data_line-volume > 0 AND p_consul = abap_false.    "I - XT21609 - DV5K9A07FB - CR333910 TK356363
        l_data_line-volume = 0 - l_data_line-volume.
      ENDIF.
      APPEND l_data_line TO ch_data_table.
      APPEND l_extra_data TO ch_extra_table.
      CALL METHOD add_to_total
        EXPORTING
          im_data_line = l_data_line
        CHANGING
          ch_total     = ch_total_line.
    ENDLOOP.

  ENDMETHOD.                    "add_ekpo_data

  METHOD add_konv_data.

    DATA: l_kwert            TYPE konv-kwert.               "DVSK904577

    FIELD-SYMBOLS: <konv>    TYPE t_konv.

    CLEAR: ch_unit_price,
           ch_federal_spill,
           ch_federal_lust,
           ch_excise_tax,
           ch_local_fuel_fees.

    IF v_knumv <> im_knumv.
      v_knumv = im_knumv.
      SELECT kposn
             kschl
             kwert
        FROM konv
        INTO TABLE i_konv_data
        WHERE knumv = v_knumv.
      IF sy-subrc > 0.                                      "DVSK904577
        REFRESH i_konv_data.                                "DVSK904577
      ENDIF.                                                "DVSK904577
    ENDIF.

    LOOP AT i_konv_data ASSIGNING <konv>
        WHERE kposn = im_ebelp AND
            ( kschl = 'PB00' OR                             "DVSK904466
              kschl = 'PBXX' OR                             "DVSK904466
              kschl = 'ZFOS' OR                             "DVSK904466
              kschl = 'ZFLT' OR                             "DVSK904466
              kschl = 'ZFET' OR                             "DVSK904466
              kschl = 'ZFST' OR                             "DVSK904466
              kschl = 'ZGR1' ).                             "DVSK904466
*      IF im_shkzg = 'S'.                                    "DVSK904466
      IF im_shkzg = 'S' AND im_xnegp = abap_true.           "DVSK904592
*        <konv>-kwert = 0 - <konv>-kwert.                   "DVSK904466
        l_kwert = 0 - <konv>-kwert.                         "DVSK904577
      ELSE.
        l_kwert = <konv>-kwert.
      ENDIF.                                                "DVSK904466
      CASE <konv>-kschl.                                    "DVSK904466
        WHEN 'PB00' OR 'PBXX'.
          ADD l_kwert TO ch_unit_price.                     "DVSK904577
*          ADD <konv>-kwert TO ch_unit_price.
        WHEN 'ZFOS'.
          ADD l_kwert TO ch_federal_spill.                  "DVSK904577
*          ADD <konv>-kwert TO ch_federal_spill.
        WHEN 'ZFLT'.
          ADD l_kwert TO ch_federal_lust.                   "DVSK904577
*          ADD <konv>-kwert TO ch_federal_lust.
        WHEN 'ZFET'.
          ADD l_kwert TO ch_excise_tax.                     "DVSK904577
*          ADD <konv>-kwert TO ch_excise_tax.
*        WHEN 'ZFST'.
        WHEN 'ZFST' OR 'ZGR1'.                              "DV5K966726
          ADD l_kwert TO ch_local_fuel_fees.                "DVSK904577
*          ADD <konv>-kwert TO ch_local_fuel_fees.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.                    "konv_data

  METHOD add_mseg_data.                                     "DV5K968325
                                                            "DV5K968325
    DATA: l_mseg             TYPE t_mseg.                   "DV5K968325
                                                            "DV5K968325
    FIELD-SYMBOLS: <mseg>    TYPE t_mseg.                   "DV5K968325
                                                            "DV5K968325
    CLEAR ch_wempf.                                         "DV5K968325
                                                            "DV5K968325
    READ TABLE i_mseg_data ASSIGNING <mseg>                 "DV5K968325
        WITH TABLE KEY ebeln = im_ebeln                     "DV5K968325
                       ebelp = im_ebelp.                    "DV5K968325
    IF sy-subrc = 0.                                        "DV5K968325
      ch_wempf = <mseg>-wempf.                              "DV5K968325
      RETURN.                                               "DV5K968325
    ENDIF.                                                  "DV5K968325
                                                            "DV5K968325
    l_mseg-ebeln = im_ebeln.                                "DV5K968325
    l_mseg-ebelp = im_ebelp.                                "DV5K968325
    SELECT SINGLE mseg~wempf                                "DV5K968325
      INTO l_mseg-wempf                                     "DV5K968325
      FROM ekbe                                             "DV5K968325
      JOIN mseg ON mseg~mblnr = ekbe~belnr AND              "DV5K968325
                   mseg~mjahr = ekbe~gjahr AND              "DV5K968325
                   mseg~zeile = ekbe~buzei                  "DV5K968325
      WHERE ekbe~ebeln = im_ebeln AND                       "DV5K968325
            ekbe~ebelp = im_ebelp AND                       "DV5K968325
            ekbe~vgabe = '1' AND      "Goods receipt        "DV5K968325
            ekbe~gjahr = im_gjahr.                          "DV5K968325
    INSERT l_mseg INTO TABLE i_mseg_data.                   "DV5K968325
                                                            "DV5K968325
    ch_wempf = l_mseg-wempf.                                "DV5K968325
                                                            "DV5K968325
  ENDMETHOD.                    "add_mseg_data              "DV5K968325

  METHOD email_preferred.
***Start of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
*    IF v_esrnr = c_email_preferred.
*      re_email = abap_true.
*    ELSE.
*      re_email = abap_false.
*    ENDIF.
***End of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    READ TABLE i_recipients TRANSPORTING NO FIELDS
      WITH KEY comm_method = c_email.
    IF sy-subrc = 0.
      re_email = abap_true.
    ELSE.
      re_email = abap_false.
    ENDIF.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
  ENDMETHOD.                    "email_preferred

  METHOD fax_preferred.
***Start of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
*    IF v_esrnr = c_fax_preferred.
*      re_fax = abap_true.
*    ELSE.
*      re_fax = abap_false.
*    ENDIF.
***End of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    READ TABLE i_recipients TRANSPORTING NO FIELDS
      WITH KEY comm_method = c_fax.
    IF sy-subrc = 0.
      re_fax = abap_true.
    ELSE.
      re_fax = abap_false.
    ENDIF.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
  ENDMETHOD.                    "email_addresses

  METHOD get_recipients.

    IF email_preferred( ) = abap_true OR
       fax_preferred( ) = abap_true.
      re_cipients[] = i_recipients[].
    ELSE.
      REFRESH re_cipients.
    ENDIF.

  ENDMETHOD.                    "email_addresses

  METHOD lookup_taxes.

    DATA: l_taxes TYPE zfi_fienh7p7_invoice_tax_calc,
          li_bset TYPE STANDARD TABLE OF zfi_fienh7p7_tax_table.

    SELECT   mwskz
             shkzg
             fwste
             ktosl
             txjcd
             kschl
        INTO TABLE li_bset
        FROM bset
        WHERE bukrs = im_bukrs
        AND belnr = im_belnr
        AND gjahr = im_gjahr.
    IF sy-subrc = 0.
      SORT li_bset BY mwskz.    "sort by tax code

      CALL FUNCTION 'ZF_INVOICE_TAXAMOUNTCALC_7P7'
        EXPORTING
          country    = v_land1
        IMPORTING
          tax_amount = l_taxes
        TABLES
          tax_table  = li_bset[].

      ch_gst_hst = l_taxes-gst + l_taxes-hst.
      ch_pst_qst = l_taxes-pst + l_taxes-qst.
      ch_us_tax =  l_taxes-us_sales_tax.
    ENDIF.

  ENDMETHOD.                    "lookup_taxes

  METHOD add_to_total.

    IF ch_total IS INITIAL.
      CLEAR ch_total-ztds_trml_id WITH '>'.
      ch_total-currency = im_data_line-currency.
    ENDIF.

    ADD im_data_line-volume          TO ch_total-volume.
    ADD im_data_line-gst_hst         TO ch_total-gst_hst.
    ADD im_data_line-pst_qst         TO ch_total-pst_qst.
    IF p_consul = abap_false.                                     "I - XT21609 - DV5K9A07FB - CR333910 TK356363
      ADD im_data_line-fuel_fees       TO ch_total-fuel_fees.
      ADD im_data_line-excise_tax      TO ch_total-excise_tax.
      ADD im_data_line-local_fuel_fees TO ch_total-local_fuel_fees.
    ENDIF.                                                        "I - XT21609 - DV5K9A07FB - CR333910 TK356363
    ADD im_data_line-us_tax          TO ch_total-us_tax.
    IF p_consul = abap_false.                                     "I - XT21609 - DV5K9A07FB - CR333910 TK356363
      ADD im_data_line-federal_spill   TO ch_total-federal_spill.
      ADD im_data_line-federal_lust    TO ch_total-federal_lust.
    ENDIF.                                                        "I - XT21609 - DV5K9A07FB - CR333910 TK356363
    ADD im_data_line-invoice_amount  TO ch_total-invoice_amount.
    ADD im_data_line-amount_before_tax TO ch_total-amount_before_tax.         "I - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9

  ENDMETHOD.                    "add_to_total

  METHOD class_constructor.
***Start of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
*    FIELD-SYMBOLS: <bsik>      TYPE t_bsik,
*                   <bldat>     LIKE LINE OF s_bldat.
***End of Delete - XT21609 - DV5K9A07FB - CR333910 TK356363***
    v_datum = sy-datum.
    v_uzeit = sy-uzeit.

    SELECT lifnr
           bukrs
           gjahr
           belnr
           buzei
           bldat
           cpudt
           waers
           blart
           bschl
           shkzg                                            "DVSK904466
           wrbtr
           zfbdt
           zbd1t
      FROM bsik
      INTO TABLE i_bsik
      WHERE bukrs IN s_bukrs AND
            lifnr IN s_lifnr AND
            gjahr IN s_gjahr.

    IF sy-subrc = 0.
      DELETE i_bsik WHERE NOT ( cpudt IN s_cpudt AND
                                bldat IN s_bldat AND
                              ( blart = 'KB' OR blart = 'K0' ) ). "Defect 271
    ENDIF.

    IF i_bsik[] IS INITIAL.
      RETURN.     "No data to process; bypass remaining steps
    ENDIF.

    SORT i_bsik BY lifnr ASCENDING.

*BEGIN DELETE DV5K973130
*    v_bldat_min = '99991231'.
*    IF s_bldat[] IS INITIAL.                                 "DV5K968325
*      LOOP AT i_bsik ASSIGNING <bsik>.                       "DV5K968325
*        IF <bsik>-bldat < v_bldat_min.                       "DV5K968325
*          v_bldat_min = <bsik>-bldat.                        "DV5K968325
*        ENDIF.                                               "DV5K968325
*        IF <bsik>-bldat > v_bldat_max.                       "DV5K968325
*          v_bldat_max = <bsik>-bldat.                        "DV5K968325
*        ENDIF.                                               "DV5K968325
*      ENDLOOP.                                               "DV5K968325
*    ELSE.                                                    "DV5K968325
*      LOOP AT s_bldat ASSIGNING <bldat>.
*        IF <bldat>-low < v_bldat_min.
*          v_bldat_min = <bldat>-low.
*        ENDIF.
*        IF <bldat>-high > v_bldat_max.
*          v_bldat_max = <bldat>-high.
*        ENDIF.
*      ENDLOOP.
*      IF v_bldat_max IS INITIAL.
*        IF v_bldat_min <= sy-datum.
*          v_bldat_max = sy-datum.
*        ELSE.
*          v_bldat_max = v_bldat_min.
*        ENDIF.                                               "DV5K968325
*      ENDIF.
*    ENDIF.
*END DELETE DV5K973130

    IF p_email = abap_true.
      TRY.
          CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
            EXPORTING
              i_name     = c_form_ca
            IMPORTING
              e_funcname = v_form_ca.
        CATCH cx_fp_api_repository.
          CLEAR v_form_ca.
        CATCH cx_fp_api_usage.
          CLEAR v_form_ca.
        CATCH cx_fp_api_internal.
          CLEAR v_form_ca.
      ENDTRY.

      TRY.
          CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
            EXPORTING
              i_name     = c_form_us
            IMPORTING
              e_funcname = v_form_us.
        CATCH cx_fp_api_repository.
          CLEAR v_form_us.
        CATCH cx_fp_api_usage.
          CLEAR v_form_us.
        CATCH cx_fp_api_internal.
          CLEAR v_form_us.
      ENDTRY.
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      IF p_consul = abap_true.
        TRY.
            CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
              EXPORTING
                i_name     = c_form_ca_it
              IMPORTING
                e_funcname = v_form_ca_it.
          CATCH cx_fp_api_repository.
            CLEAR v_form_ca_it.
          CATCH cx_fp_api_usage.
            CLEAR v_form_ca_it.
          CATCH cx_fp_api_internal.
            CLEAR v_form_ca_it.
        ENDTRY.

        TRY.
            CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
              EXPORTING
                i_name     = c_form_us_it
              IMPORTING
                e_funcname = v_form_us_it.
          CATCH cx_fp_api_repository.
            CLEAR v_form_us_it.
          CATCH cx_fp_api_usage.
            CLEAR v_form_us_it.
          CATCH cx_fp_api_internal.
            CLEAR v_form_us_it.
        ENDTRY.
      ENDIF.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
    ENDIF.

*** Get all components in the structures
    o_base_descr ?= cl_abap_tabledescr=>describe_by_name( 'ZFI_FIRPT7H6_EXTRACT_TO_PDF' ).
    o_line_descr ?= cl_abap_tabledescr=>describe_by_name( 'ZFI_FIRPT7H6_LINE_DATA' ).

    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'                  "#EC *
      IMPORTING
        own_logical_system             = v_bds_logical_system
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2.

*** Get MIME type for BDS
    SELECT SINGLE mimetype
      FROM toadd
      INTO v_bds_mimetype
      WHERE doc_type EQ 'PDF'.

  ENDMETHOD.                    "class_constructor

  METHOD material_group.

    TYPES: BEGIN OF lt_t023t,
             matkl TYPE t023t-matkl,
             wgbez TYPE t023t-wgbez,
           END OF lt_t023t.

    STATICS: sti_t023t       TYPE STANDARD TABLE OF lt_t023t.

    FIELD-SYMBOLS: <t023t>   TYPE lt_t023t.

    READ TABLE sti_t023t ASSIGNING <t023t>
        WITH KEY matkl = im_matkl.
    IF sy-subrc > 0.
      APPEND INITIAL LINE TO sti_t023t ASSIGNING <t023t>.
      SELECT SINGLE matkl
                    wgbez
        FROM t023t
        INTO (<t023t>-matkl,
              <t023t>-wgbez)
        WHERE spras = sy-langu AND
              matkl = im_matkl.
      IF sy-subrc > 0.
        <t023t>-matkl = im_matkl.
        <t023t>-wgbez = '*** Unknown ***'(t01).
      ENDIF.
    ENDIF.

    re_group = <t023t>-wgbez.

  ENDMETHOD.                    "material_group

  METHOD vendor_number.

    re_lifnr = v_lifnr.

    IF im_shift_left IS SUPPLIED AND
       im_shift_left = abap_true.
      SHIFT re_lifnr LEFT DELETING LEADING '0'.
    ENDIF.

  ENDMETHOD.                    "vendor_number

  METHOD vendor_email.

    TYPES: BEGIN OF lt_adr6,
             persnumber TYPE adr6-persnumber,
             smtp_addr  TYPE adr6-smtp_addr,
           END OF lt_adr6.

    STATICS: sti_adr6        TYPE STANDARD TABLE OF lt_adr6.

    FIELD-SYMBOLS: <adr6>    TYPE lt_adr6.

    READ TABLE sti_adr6 ASSIGNING <adr6>
        WITH KEY persnumber = im_prsnr.
    IF sy-subrc > 0.
      APPEND INITIAL LINE TO sti_adr6 ASSIGNING <adr6>.
      SELECT SINGLE persnumber                              "#EC *
                    smtp_addr
        FROM adr6
        INTO (<adr6>-persnumber,
              <adr6>-smtp_addr)
        WHERE persnumber = im_prsnr.
      IF sy-subrc > 0.
        <adr6>-persnumber = im_prsnr.
        <adr6>-smtp_addr = '*** Unknown ***'(t01).
      ENDIF.
    ENDIF.

    re_addr = <adr6>-smtp_addr.

  ENDMETHOD.                    "vendor_email

  METHOD vendor_fax.

    TYPES: BEGIN OF lt_adr3,
             persnumber TYPE adr3-persnumber,
             fax_number TYPE adr3-fax_number,
           END OF lt_adr3.

    STATICS: sti_adr3        TYPE STANDARD TABLE OF lt_adr3.

    FIELD-SYMBOLS: <adr3>    TYPE lt_adr3.

    READ TABLE sti_adr3 ASSIGNING <adr3>
        WITH KEY persnumber = im_prsnr.
    IF sy-subrc > 0.
      APPEND INITIAL LINE TO sti_adr3 ASSIGNING <adr3>.
      SELECT SINGLE persnumber                              "#EC *
                    fax_number
        FROM adr3
        INTO (<adr3>-persnumber,
              <adr3>-fax_number)
        WHERE persnumber = im_prsnr.
      IF sy-subrc > 0.
        <adr3>-persnumber = im_prsnr.
        <adr3>-fax_number = '*** Unknown ***'(t01).
      ENDIF.
    ENDIF.

    re_number = <adr3>-fax_number.

  ENDMETHOD.                    "vendor_email

  METHOD get_next.

    READ TABLE i_bsik INTO re_bsik INDEX 1.
    IF sy-subrc = 0.
      DELETE i_bsik INDEX 1.      "Remove processed entry
    ELSE.
      CLEAR re_bsik.
    ENDIF.

  ENDMETHOD.                    "get_next

  METHOD locate_vendor.

    DATA: l_object_index     TYPE t_object_index.

    CLEAR re_pointer.

*--------------------------------------------------------------------*
*** Read the current index; because of the sort by LIFNR, this should
*** catch almost all accesses unless processing for a new vendor
    READ TABLE i_object_index INTO l_object_index INDEX v_index.
    IF sy-subrc = 0 AND l_object_index-lifnr = im_lifnr.
      re_pointer = l_object_index-pointer.
      RETURN.
    ENDIF.

*--------------------------------------------------------------------*
*** Not current, try to read by LIFNR
    READ TABLE i_object_index INTO l_object_index
        WITH KEY lifnr = im_lifnr.
    IF sy-subrc = 0.
      re_pointer = l_object_index-pointer.
      RETURN.
    ENDIF.

*--------------------------------------------------------------------*
*** Still not found, create a new instance if IM_CREATE is true
    IF im_create = abap_true.
      l_object_index-lifnr = im_lifnr.
      CREATE OBJECT l_object_index-pointer
        EXPORTING
          im_lifnr = im_lifnr.
      re_pointer = l_object_index-pointer.
      APPEND l_object_index TO i_object_index.
      ADD 1 TO v_index.
    ENDIF.

  ENDMETHOD.                    "locate_vendor

  METHOD vendor_by_index.

    FIELD-SYMBOLS: <object>  TYPE t_object_index.

    CLEAR re_pointer.

    READ TABLE i_object_index ASSIGNING <object>
         INDEX im_index.
    IF sy-subrc = 0.
      re_pointer = <object>-pointer.
    ENDIF.

  ENDMETHOD.                    "vendor_by_index

  METHOD data_to_report.

    IF v_pdf_data-data_table_cad[] IS INITIAL AND
       v_pdf_data-data_table_usd[] IS INITIAL.
      re_data = abap_false.
    ELSE.
      re_data = abap_true.
    ENDIF.

  ENDMETHOD.                    "data_to_report
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
  METHOD format_pdf_it.

    CONSTANTS: lc_pdfversion(3)    TYPE c            VALUE '1.6'.

    DATA: l_output_parms TYPE sfpoutputparams,
          l_docparms     TYPE sfpdocparams,
          l_form_output  TYPE fpformoutput,
          l_result       TYPE sfpjoboutput.                 "#EC NEEDED

    FIELD-SYMBOLS: <form>          TYPE funcname.

    CLEAR ch_pdf_it.

    IF v_pdf_data-data_table_cad[] IS INITIAL AND
       v_pdf_data-data_table_usd[] IS INITIAL.
      RETURN.
    ENDIF.

***Start of Delete - TXT21609 - RTSK0014488 DFCT0011652 - DV5K9A0U6O
*    SORT v_pdf_data-data_table_cad BY ztds_trml_id ASCENDING
*                                      ztds_bol_nbr ASCENDING
*                                      bldat        ASCENDING.
*    SORT v_pdf_data-data_table_usd BY ztds_trml_id ASCENDING
*                                      ztds_bol_nbr ASCENDING
*                                      bldat        ASCENDING.
***End of Delete - TXT21609 - RTSK0014488 DFCT0011652 - DV5K9A0U6O

    IF v_land1 = 'US'.
      l_docparms-country = 'US'.
      IF v_form_us_it IS NOT INITIAL.
        ASSIGN v_form_us_it TO <form>.
      ELSE.
        RETURN.      "No form available
      ENDIF.
    ELSE.
      l_docparms-country = 'CA'.
      IF v_form_ca_it IS NOT INITIAL.
        ASSIGN v_form_ca_it TO <form>.
      ELSE.
        RETURN.      "No form available
      ENDIF.
    ENDIF.

*** Set up the output parameters based on the user command
    l_output_parms-nodialog = 'X'.
    l_output_parms-pdfversion = lc_pdfversion.

    IF im_preview = abap_true.
      l_output_parms-getpdf = ' '.
      l_output_parms-preview = 'X'.
    ELSE.
      l_output_parms-getpdf = 'X'.
      l_output_parms-preview = ' '.
    ENDIF.

*** Open the print job
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = l_output_parms
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.
    IF sy-subrc > 0.
      RETURN.
    ENDIF.

    l_docparms-langu = 'E'.
    l_docparms-fillable = 'N'.

*** Call the form function
    CALL FUNCTION <form>
      EXPORTING
        /1bcdwb/docparams  = l_docparms
        fi_rpt_7h6_data    = v_pdf_data
      IMPORTING
        /1bcdwb/formoutput = l_form_output
      EXCEPTIONS
        OTHERS             = 1.

    IF sy-subrc > 0.
      RETURN.
    ENDIF.

*** Close the form
    CALL FUNCTION 'FP_JOB_CLOSE'
      IMPORTING
        e_result       = l_result
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.

    IF sy-subrc > 0.
      RETURN.
    ENDIF.

    IF im_preview = abap_false.
      IF ch_pdf_it IS SUPPLIED.
        ch_pdf_it = l_form_output-pdf.
      ENDIF.
      CALL METHOD store_in_bdms
        EXPORTING
          im_pdf    = l_form_output-pdf
        IMPORTING
          ex_object = v_bds_object.
    ENDIF.

  ENDMETHOD.                    "format_pdf_it

  METHOD format_csv_it.

***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
    CONSTANTS:
      lc_india        TYPE land1_gp VALUE 'IN'.
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9

    DATA: l_csv_line         TYPE soli-line.

    FIELD-SYMBOLS: <data>    TYPE zfi_firpt7h6_line_data.

    IF v_pdf_data-data_table_cad[] IS INITIAL AND
       v_pdf_data-data_table_usd[] IS INITIAL.
      RETURN.
    ENDIF.

    CALL METHOD csv_field
      EXPORTING
        im_value  = 'CN - Transaction Summary Report'       "#EC NOTEXT
      CHANGING
        ch_result = l_csv_line.
    APPEND l_csv_line TO ch_csv_it.

    CALL METHOD csv_field
      EXPORTING
        im_value  = 'Date issued'                           "#EC NOTEXT
      CHANGING
        ch_result = l_csv_line.
    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-cdate
        im_fieldname   = 'CDATE'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.
    APPEND l_csv_line TO ch_csv_it.
    APPEND INITIAL LINE TO ch_csv_it.

    CALL METHOD csv_field
      EXPORTING
        im_value     = v_pdf_data-name1
        im_fieldname = 'NAME1'
        im_descr     = o_base_descr
      CHANGING
        ch_result    = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-stras
        im_fieldname   = 'STRAS'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-ort01
        im_fieldname   = 'ORT01'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-regio
        im_fieldname   = 'REGIO'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-pstlz
        im_fieldname   = 'PSTLZ'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    APPEND l_csv_line TO ch_csv_it.

    IF v_land1 = 'CA'
     OR v_land1 = lc_india.                                       "I - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
      CALL METHOD csv_field
        EXPORTING
          im_value  = 'GST/HST No'                          "#EC NOTEXT
        CHANGING
          ch_result = l_csv_line.
      CALL METHOD csv_field
        EXPORTING
          im_value       = v_pdf_data-stcd1
          im_fieldname   = 'STCD1'
          im_descr       = o_base_descr
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.
      APPEND l_csv_line TO ch_csv_it.

      CALL METHOD csv_field
        EXPORTING
          im_value  = 'PST/QST No'                          "#EC NOTEXT
        CHANGING
          ch_result = l_csv_line.
      CALL METHOD csv_field
        EXPORTING
          im_value       = v_pdf_data-stcd2
          im_fieldname   = 'STCD2'
          im_descr       = o_base_descr
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.
      APPEND l_csv_line TO ch_csv_it.
    ENDIF.

    CALL METHOD csv_field
      EXPORTING
        im_value  = 'Vendor Ref No'                         "#EC NOTEXT
      CHANGING
        ch_result = l_csv_line.
    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-lifnr
        im_fieldname   = 'LIFNR'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.
    APPEND l_csv_line TO ch_csv_it.

    CALL METHOD csv_field
      EXPORTING
        im_value  = 'Invoices issued for the period'        "#EC NOTEXT
      CHANGING
        ch_result = l_csv_line.
    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-begda
        im_fieldname   = 'BEGDA'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.
    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-endda
        im_fieldname   = 'ENDDA'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.
    APPEND l_csv_line TO ch_csv_it.

    APPEND INITIAL LINE TO ch_csv_it.

    CALL METHOD csv_field
      EXPORTING
        im_value  = 'Period from'                    "#EC NOTEXT
      CHANGING
        ch_result = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Period to'                      "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'CN Invoice No.'                 "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

***start of insert T182535 / DV5K9A0GNZ  C372902-T403637***
    IF p_consul = abap_false.
      CALL METHOD csv_field
        EXPORTING
          im_value       = 'Network'                        "#EC NOTEXT
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.
    ELSE.
      CALL METHOD csv_field
        EXPORTING
          im_value       = 'PIN'                        "#EC NOTEXT
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.
    ENDIF.
***end of insert T182535 / DV5K9A0GNZ  C372902-T403637***

    CALL METHOD csv_field
      EXPORTING
*       im_value       = 'Delivery Location'              "#EC NOTEXT          "D - XT21609 - DV5K9A08B2 - CR333910 TK356363
        im_value       = 'Consultant Name' "#EC NOTEXT           "I - XT21609 - DV5K9A08B2 - CR333910 TK356363
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Product-Service'                "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Qty'                            "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'UOM'                            "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Unit Price'                     "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
    "Add Amount before tax
    CALL METHOD csv_field
      EXPORTING
        im_concatenate = abap_true
        im_value       = text-t18
      CHANGING
        ch_result      = l_csv_line.
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9

    IF v_land1 = 'CA'
     OR v_land1 = lc_india.                                       "I - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
      CALL METHOD csv_field
        EXPORTING
          im_value       = 'GST/HST'                      "#EC NOTEXT
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.

      CALL METHOD csv_field
        EXPORTING
          im_value       = 'PST/QST'                      "#EC NOTEXT
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.
    ENDIF.

    IF v_land1 = 'US'.
      CALL METHOD csv_field
        EXPORTING
          im_value       = 'US Tax'                       "#EC NOTEXT
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.
    ENDIF.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Invoice Amount'                 "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Currency'                       "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_concatenate = abap_true
        im_value       = 'Due Date'                       "#EC NOTEXT
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_concatenate = abap_true
        im_value       = 'PO Number'                      "#EC NOTEXT
      CHANGING
        ch_result      = l_csv_line.

***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
    "Add PO line item
    CALL METHOD csv_field
      EXPORTING
        im_concatenate = abap_true
        im_value       = text-t19
      CHANGING
        ch_result      = l_csv_line.
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9

    APPEND l_csv_line TO ch_csv_it.

    IF v_land1 = 'CA'
     OR v_land1 = lc_india.                                       "I - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
      IF v_pdf_data-data_table_cad[] IS NOT INITIAL.
        LOOP AT v_pdf_data-data_table_cad ASSIGNING <data>.
          CALL METHOD csv_data_line_it
            EXPORTING
              im_data_line = <data>
            CHANGING
              ch_result    = l_csv_line.
          APPEND l_csv_line TO ch_csv_it.
        ENDLOOP.
      ENDIF.
      IF v_pdf_data-data_table_usd[] IS NOT INITIAL.
        APPEND INITIAL LINE TO ch_csv_it.
        LOOP AT v_pdf_data-data_table_usd ASSIGNING <data>.
          CALL METHOD csv_data_line_it
            EXPORTING
              im_data_line = <data>
            CHANGING
              ch_result    = l_csv_line.
          APPEND l_csv_line TO ch_csv_it.
        ENDLOOP.
        CLEAR l_csv_line.
      ENDIF.
    ENDIF.

    IF v_land1 = 'US'.
      IF v_pdf_data-data_table_usd[] IS NOT INITIAL.
        LOOP AT v_pdf_data-data_table_usd ASSIGNING <data>.
          CALL METHOD csv_data_line_it
            EXPORTING
              im_data_line = <data>
            CHANGING
              ch_result    = l_csv_line.
          APPEND l_csv_line TO ch_csv_it.
        ENDLOOP.
      ENDIF.
      IF v_pdf_data-data_table_cad[] IS NOT INITIAL.
        APPEND INITIAL LINE TO ch_csv_it.
        LOOP AT v_pdf_data-data_table_cad ASSIGNING <data>.
          CALL METHOD csv_data_line_it
            EXPORTING
              im_data_line = <data>
            CHANGING
              ch_result    = l_csv_line.
          APPEND l_csv_line TO ch_csv_it.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "format_csv_it

  METHOD csv_data_line_it.

    DATA: l_concatenate TYPE xfeld,
          lwa_value     TYPE bseg-meins.         "I - XT21609 - DV5K9A07FB - CR333910 TK356363

***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
    CONSTANTS:
      lc_india  TYPE land1_gp VALUE 'IN',
      lc_canada TYPE land1_gp VALUE 'CA'.
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9

    FIELD-SYMBOLS: <value> TYPE any,
                   <descr> TYPE abap_compdescr.

    l_concatenate = abap_false.

    LOOP AT o_line_descr->components ASSIGNING <descr>
          WHERE NOT ( name = 'ZTDS_TRML_ID'    OR
                      name = 'BLDAT'           OR
                      name = 'ZTDS_BOL_NBR'    OR
                      name = 'BEDNR'           OR
                      name = 'ERS_INDICATOR'   OR
                      name = 'FUEL_FEES'       OR
                      name = 'EXCISE_TAX'      OR
                      name = 'LOCAL_FUEL_FEES' OR
                      name = 'FEDERAL_SPILL'   OR
                      name = 'FEDERAL_LUST'    OR
*                      name = 'EBELP'           OR            "D - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
                      name = 'KNUMV'           OR
                      name = 'SHKZG'           OR
                      name = 'XNEGP'           OR
                      name = 'WEMPF' ).
*      IF v_land1 = 'CA' AND                                  "D - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
      IF ( v_land1 = lc_canada OR v_land1 = lc_india ) AND    "I - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
       ( <descr>-name = 'US_TAX').
        CONTINUE.       "skip this field
      ENDIF.
      IF v_land1 = 'US' AND
       ( <descr>-name = 'GST_HST' OR
         <descr>-name = 'PST_QST').
        CONTINUE.       "skip this field
      ENDIF.

***start of insert T182535 / DV5K9A0GNZ  C372902-T403637***
      IF ( p_consul = abap_true AND <descr>-name = 'NETWORK' ) OR
         (  p_consul = abap_false AND <descr>-name = 'PERNR' ).
        CONTINUE.
      ENDIF.
***end of insert T182535 / DV5K9A0GNZ  C372902-T403637***

      ASSIGN COMPONENT <descr>-name OF STRUCTURE im_data_line TO <value>.
      IF sy-subrc > 0.
        EXIT.
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      ELSEIF <descr>-name = 'UNIT_OF_MEASURE'.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = <value>
          IMPORTING
            output         = lwa_value
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
        IF sy-subrc = 0.
          ASSIGN lwa_value TO <value>.
        ENDIF.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      ENDIF.
      CALL METHOD csv_field
        EXPORTING
          im_value       = <value>
          im_type_kind   = <descr>-type_kind
          im_decimals    = <descr>-decimals
          im_concatenate = l_concatenate
        CHANGING
          ch_result      = ch_result.
      l_concatenate = abap_true.
    ENDLOOP.

  ENDMETHOD.                    "csv_data_line_it
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
  METHOD format_pdf.

    CONSTANTS: lc_pdfversion(3)    TYPE c            VALUE '1.6'.

    DATA: l_output_parms TYPE sfpoutputparams,
          l_docparms     TYPE sfpdocparams,
          l_form_output  TYPE fpformoutput,
          l_result       TYPE sfpjoboutput.                 "#EC NEEDED

    FIELD-SYMBOLS: <form>          TYPE funcname.

    CLEAR ch_pdf.

    IF v_pdf_data-data_table_cad[] IS INITIAL AND
       v_pdf_data-data_table_usd[] IS INITIAL.
      RETURN.
    ENDIF.

*    SORT v_pdf_data-data_table_cad BY delivery_locn ASCENDING. "DV5K968325
*    SORT v_pdf_data-data_table_usd BY delivery_locn ASCENDING. "DV5K968325
    SORT v_pdf_data-data_table_cad BY ztds_trml_id ASCENDING "DV5K968325
                                      ztds_bol_nbr ASCENDING "DV5K968325
                                      bldat        ASCENDING. "DV5K968325
    SORT v_pdf_data-data_table_usd BY ztds_trml_id ASCENDING "DV5K968325
                                      ztds_bol_nbr ASCENDING "DV5K968325
                                      bldat        ASCENDING. "DV5K968325

    IF v_land1 = 'US'.
      l_docparms-country = 'US'.
      IF v_form_us IS NOT INITIAL.
        ASSIGN v_form_us TO <form>.
      ELSE.
        RETURN.      "No form available
      ENDIF.
    ELSE.
      l_docparms-country = 'CA'.
      IF v_form_ca IS NOT INITIAL.
        ASSIGN v_form_ca TO <form>.
      ELSE.
        RETURN.      "No form available
      ENDIF.
    ENDIF.

*** Set up the output parameters based on the user command
    l_output_parms-nodialog = 'X'.
    l_output_parms-pdfversion = lc_pdfversion.
*  l_output_parms-connection = v_ads_connection.

    IF im_preview = abap_true.
      l_output_parms-getpdf = ' '.
      l_output_parms-preview = 'X'.
    ELSE.
      l_output_parms-getpdf = 'X'.
      l_output_parms-preview = ' '.
    ENDIF.

*** Open the print job
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = l_output_parms
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.
    IF sy-subrc > 0.
      RETURN.
    ENDIF.

    l_docparms-langu = 'E'.
    l_docparms-fillable = 'N'.

*** Call the form function
    CALL FUNCTION <form>
      EXPORTING
        /1bcdwb/docparams  = l_docparms
        fi_rpt_7h6_data    = v_pdf_data
      IMPORTING
        /1bcdwb/formoutput = l_form_output
      EXCEPTIONS
        OTHERS             = 1.

    IF sy-subrc > 0.
      RETURN.
    ENDIF.

*** Close the form
    CALL FUNCTION 'FP_JOB_CLOSE'
      IMPORTING
        e_result       = l_result
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.

    IF sy-subrc > 0.
      RETURN.
    ENDIF.

    IF im_preview = abap_false.
      IF ch_pdf IS SUPPLIED.
        ch_pdf = l_form_output-pdf.
      ENDIF.
      CALL METHOD store_in_bdms
        EXPORTING
          im_pdf    = l_form_output-pdf
        IMPORTING
          ex_object = v_bds_object.
    ENDIF.

  ENDMETHOD.                    "format_pdf

  METHOD format_csv.

    DATA: l_csv_line         TYPE soli-line.

    FIELD-SYMBOLS: <data>    TYPE zfi_firpt7h6_line_data.

    IF v_pdf_data-data_table_cad[] IS INITIAL AND
       v_pdf_data-data_table_usd[] IS INITIAL.
      RETURN.
    ENDIF.

    CALL METHOD csv_field
      EXPORTING
        im_value  = 'CN - Transaction Summary Report'       "#EC NOTEXT
      CHANGING
        ch_result = l_csv_line.
    APPEND l_csv_line TO ch_csv.

    CALL METHOD csv_field
      EXPORTING
        im_value  = 'Date issued'                           "#EC NOTEXT
      CHANGING
        ch_result = l_csv_line.
    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-cdate
        im_fieldname   = 'CDATE'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.
    APPEND l_csv_line TO ch_csv.
    APPEND INITIAL LINE TO ch_csv.

    CALL METHOD csv_field
      EXPORTING
        im_value     = v_pdf_data-name1
        im_fieldname = 'NAME1'
        im_descr     = o_base_descr
      CHANGING
        ch_result    = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-stras
        im_fieldname   = 'STRAS'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-ort01
        im_fieldname   = 'ORT01'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-regio
        im_fieldname   = 'REGIO'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-pstlz
        im_fieldname   = 'PSTLZ'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    APPEND l_csv_line TO ch_csv.

    IF v_land1 = 'CA'.
      CALL METHOD csv_field
        EXPORTING
          im_value  = 'GST/HST No'                          "#EC NOTEXT
        CHANGING
          ch_result = l_csv_line.
      CALL METHOD csv_field
        EXPORTING
          im_value       = v_pdf_data-stcd1
          im_fieldname   = 'STCD1'
          im_descr       = o_base_descr
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.
      APPEND l_csv_line TO ch_csv.

      CALL METHOD csv_field
        EXPORTING
          im_value  = 'PST/QST No'                          "#EC NOTEXT
        CHANGING
          ch_result = l_csv_line.
      CALL METHOD csv_field
        EXPORTING
          im_value       = v_pdf_data-stcd2
          im_fieldname   = 'STCD2'
          im_descr       = o_base_descr
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.
      APPEND l_csv_line TO ch_csv.
    ENDIF.

    CALL METHOD csv_field
      EXPORTING
        im_value  = 'Vendor Ref No'                         "#EC NOTEXT
      CHANGING
        ch_result = l_csv_line.
    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-lifnr
        im_fieldname   = 'LIFNR'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.
    APPEND l_csv_line TO ch_csv.

    CALL METHOD csv_field
      EXPORTING
        im_value  = 'Invoices issued for the period'        "#EC NOTEXT
      CHANGING
        ch_result = l_csv_line.
    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-begda
        im_fieldname   = 'BEGDA'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.
    CALL METHOD csv_field
      EXPORTING
        im_value       = v_pdf_data-endda
        im_fieldname   = 'ENDDA'
        im_descr       = o_base_descr
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.
    APPEND l_csv_line TO ch_csv.

    APPEND INITIAL LINE TO ch_csv.

    CALL METHOD csv_field
      EXPORTING
        im_value  = 'Terminal ID'                           "#EC NOTEXT
      CHANGING
        ch_result = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Invoice Date'                     "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'BOL #'                            "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Tracking No.'                     "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'CN Invoice No.'                   "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
*       im_value       = 'Delivery Location'                "#EC NOTEXT      "D - XT21609 - DV5K9A08B2 - CR333910 TK356363
        im_value       = 'Consultant Name'                  "#EC NOTEXT       "I - XT21609 - DV5K9A08B2 - CR333910 TK356363
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Product/Service'                  "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Volume'                           "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'UOM'                              "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Unit Price'                       "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    IF v_land1 = 'CA'.
      CALL METHOD csv_field
        EXPORTING
          im_value       = 'GST/HST'                        "#EC NOTEXT
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.

      CALL METHOD csv_field
        EXPORTING
          im_value       = 'PST/QST'                        "#EC NOTEXT
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.

      CALL METHOD csv_field
        EXPORTING
          im_value       = 'Excise Tax'                     "#EC NOTEXT
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.
    ENDIF.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Local Fuel Fees'                  "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    IF v_land1 = 'US'.
      CALL METHOD csv_field
        EXPORTING
          im_value       = 'US Tax'                         "#EC NOTEXT
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.

      CALL METHOD csv_field
        EXPORTING
          im_value       = 'Federal Oil Spill'              "#EC NOTEXT
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.

      CALL METHOD csv_field
        EXPORTING
          im_value       = 'Federal L.U.S.T.'               "#EC NOTEXT
          im_concatenate = abap_true
        CHANGING
          ch_result      = l_csv_line.
    ENDIF.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Invoice Amount'                   "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_value       = 'Currency'                         "#EC NOTEXT
        im_concatenate = abap_true
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_concatenate = abap_true
        im_value       = 'Due Date'                         "#EC NOTEXT
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field
      EXPORTING
        im_concatenate = abap_true
        im_value       = 'PO Number'                        "#EC NOTEXT
      CHANGING
        ch_result      = l_csv_line.

    CALL METHOD csv_field                                   "DV5K968325
      EXPORTING                                             "DV5K968325
        im_concatenate = abap_true                          "DV5K968325
        im_value       = 'Locomotive ID'(m14)               "DV5K968325
      CHANGING                                              "DV5K968325
        ch_result      = l_csv_line.                        "DV5K968325

    APPEND l_csv_line TO ch_csv.

    IF v_land1 = 'CA'.
      IF v_pdf_data-data_table_cad[] IS NOT INITIAL.
        LOOP AT v_pdf_data-data_table_cad ASSIGNING <data>.
          CALL METHOD csv_data_line
            EXPORTING
              im_data_line = <data>
            CHANGING
              ch_result    = l_csv_line.
          APPEND l_csv_line TO ch_csv.
        ENDLOOP.
      ENDIF.
      IF v_pdf_data-data_table_usd[] IS NOT INITIAL.
        APPEND INITIAL LINE TO ch_csv.
        LOOP AT v_pdf_data-data_table_usd ASSIGNING <data>.
          CALL METHOD csv_data_line
            EXPORTING
              im_data_line = <data>
            CHANGING
              ch_result    = l_csv_line.
          APPEND l_csv_line TO ch_csv.
        ENDLOOP.
        CLEAR l_csv_line.
      ENDIF.
    ENDIF.

    IF v_land1 = 'US'.
      IF v_pdf_data-data_table_usd[] IS NOT INITIAL.
        LOOP AT v_pdf_data-data_table_usd ASSIGNING <data>.
          CALL METHOD csv_data_line
            EXPORTING
              im_data_line = <data>
            CHANGING
              ch_result    = l_csv_line.
          APPEND l_csv_line TO ch_csv.
        ENDLOOP.
      ENDIF.
      IF v_pdf_data-data_table_cad[] IS NOT INITIAL.
        APPEND INITIAL LINE TO ch_csv.
        LOOP AT v_pdf_data-data_table_cad ASSIGNING <data>.
          CALL METHOD csv_data_line
            EXPORTING
              im_data_line = <data>
            CHANGING
              ch_result    = l_csv_line.
          APPEND l_csv_line TO ch_csv.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "format_csv

  METHOD csv_data_line.

***Start of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
    CONSTANTS:
      lc_amount_before_tax TYPE string VALUE 'AMOUNT_BEFORE_TAX'.
***End of Insert - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9

    DATA: l_concatenate TYPE xfeld,
          l_tabix       TYPE sy-tabix.

    FIELD-SYMBOLS: <value> TYPE any,
                   <descr> TYPE abap_compdescr.

    l_concatenate = abap_false.

    LOOP AT o_line_descr->components ASSIGNING <descr>
          WHERE NOT ( name = 'ERS_INDICATOR' OR
                      name = 'EBELP' OR
                      name = 'KNUMV' OR
                      name = 'SHKZG' OR                     "DV5K968325
                      name = 'XNEGP' OR                     "DV5K968325
                      name = lc_amount_before_tax OR        "I - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
                      name = 'PERNR' OR  " DV5K9A0GNZ +
                      name = 'PERIOD_FROM' OR
                      name = 'PERIOD_TO'   OR
                      name = 'NETWORK' ).
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      l_tabix = sy-tabix.
      IF v_land1 = 'CA' AND
       ( <descr>-name = 'US_TAX' OR
         <descr>-name = 'FEDERAL_SPILL' OR
         <descr>-name = 'FUEL_FEES' OR
         <descr>-name = 'FEDERAL_LUST').
        CONTINUE.       "skip this field
      ENDIF.
      IF v_land1 = 'US' AND
       ( <descr>-name = 'GST_HST' OR
         <descr>-name = 'PST_QST' OR
         <descr>-name = 'FUEL_FEES' OR
         <descr>-name = 'EXCISE_TAX').
        CONTINUE.       "skip this field
      ENDIF.
      ASSIGN COMPONENT l_tabix OF STRUCTURE im_data_line TO <value>.
      IF sy-subrc > 0.
        EXIT.
      ENDIF.
      CALL METHOD csv_field
        EXPORTING
          im_value       = <value>
          im_type_kind   = <descr>-type_kind
          im_decimals    = <descr>-decimals
          im_concatenate = l_concatenate
        CHANGING
          ch_result      = ch_result.
      l_concatenate = abap_true.
    ENDLOOP.

  ENDMETHOD.                    "csv_data_line

  METHOD csv_field.

    DATA: l_value     TYPE soli-line,
          l_offset    TYPE i     VALUE 0,
          l_length    TYPE i,
          l_type_kind TYPE c.

    FIELD-SYMBOLS: <descr>   TYPE abap_compdescr.

    IF im_concatenate = abap_false.
      CLEAR ch_result.
    ENDIF.
    IF im_value IS INITIAL.
      IF im_concatenate = abap_true.
        l_length = strlen( ch_result ).
        ch_result+l_length(1) = ','.
      ENDIF.
      RETURN.
    ENDIF.

    IF im_type_kind IS SUPPLIED AND
       im_type_kind IS NOT INITIAL.
      l_type_kind = im_type_kind.
    ELSE.
      l_type_kind = 'C'.
    ENDIF.

    IF im_descr IS SUPPLIED AND
       im_descr IS BOUND AND
       im_fieldname IS SUPPLIED AND
       im_fieldname IS NOT INITIAL.
      READ TABLE im_descr->components ASSIGNING <descr>
           WITH KEY name = im_fieldname.
      IF sy-subrc = 0.
        l_type_kind = <descr>-type_kind.
      ELSE.
        l_type_kind = 'C'.
      ENDIF.
    ENDIF.

    CASE l_type_kind.
      WHEN 'C'.
        l_value = im_value.
        SHIFT l_value LEFT DELETING LEADING space.
        TRANSLATE l_value USING '"'''.   "change double quote to single
      WHEN 'D' OR 'T'.
        WRITE im_value TO l_value LEFT-JUSTIFIED.
      WHEN 'N' OR 'P'.
        IF im_decimals IS SUPPLIED AND im_decimals > 0.
          WRITE im_value TO l_value LEFT-JUSTIFIED
                                    DECIMALS im_decimals
                                    NO-SIGN
                                    NO-GROUPING.
        ELSE.
          WRITE im_value TO l_value LEFT-JUSTIFIED
                                    NO-SIGN
                                    NO-GROUPING.
        ENDIF.
        TRANSLATE l_value USING ',.'.    "change comma to decimal
        IF im_value < 0.       "Put minus sign leading
          SHIFT l_value RIGHT BY 1 PLACES.
          l_value+0(1) = '-'.
          l_offset = 1.     "To test the portion after the minus sign below
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    IF l_value+l_offset CA ',''+-/*'.   "Put quotes around data with symbols
      l_length = strlen( l_value ).
      l_value+l_length(1) = '"'.
      SHIFT l_value RIGHT BY 1 PLACES.
      l_value+0(1) = '"'.
    ENDIF.

    IF im_concatenate = abap_true.
      CONCATENATE ch_result l_value INTO ch_result
          SEPARATED BY ','.
    ELSE.
      ch_result = l_value.
    ENDIF.

  ENDMETHOD.                    "csv_field

  METHOD vendor_address.

    DATA: l_text             TYPE soli.

    REFRESH re_address.

    IF v_pdf_data-name1 IS NOT INITIAL.
      APPEND v_pdf_data-name1 TO re_address.
    ENDIF.

    IF v_pdf_data-stras IS NOT INITIAL.
      APPEND v_pdf_data-stras TO re_address.
    ENDIF.

    CONCATENATE v_pdf_data-ort01
                v_pdf_data-regio
                v_pdf_data-pstlz INTO l_text SEPARATED BY space.
    IF l_text IS NOT INITIAL.
      APPEND l_text TO re_address.
    ENDIF.
  ENDMETHOD.                    "vendor_address

  METHOD vendor_contact.

    CLEAR re_contact.
    re_contact = v_pdf_data-contact_name.

  ENDMETHOD.                    "vendor_contact

  METHOD vendor_country.

    re_land1 = v_land1.

  ENDMETHOD.                    "vendor_country

  METHOD period_start_date.

*   CLEAR re_begda.                                          DV5K973130
*   IF v_bldat_min IS NOT INITIAL.                           DV5K973130
*     WRITE v_bldat_min TO re_begda LEFT-JUSTIFIED.
    WRITE v_pdf_data-begda TO re_begda LEFT-JUSTIFIED.      "DV5K973130
*   ENDIF.                                                   DV5K973130

  ENDMETHOD.                    "period_start_date

  METHOD period_end_date.

*   CLEAR re_endda.                                          DV5K973130
*   IF v_bldat_max IS NOT INITIAL.                           DV5K973130
*     WRITE v_bldat_max TO re_endda LEFT-JUSTIFIED.          DV5K973130
    WRITE v_pdf_data-endda TO re_endda LEFT-JUSTIFIED.      "DV5K973130
*   ENDIF.                                                   DV5K973130

  ENDMETHOD.                    "period_end_date


  METHOD format_alv.

    DATA: l_alv              TYPE zfi_firpt7h6_extract_to_alv.

    FIELD-SYMBOLS: <data>  TYPE zfi_firpt7h6_line_data,
                   <extra> TYPE t_extra_data.

    IF v_pdf_data-data_table_cad[] IS INITIAL AND
       v_pdf_data-data_table_usd[] IS INITIAL.
      RETURN.
    ENDIF.

    l_alv-lifnr = v_lifnr.
    l_alv-stcd1 = v_pdf_data-stcd1.
    l_alv-stcd2 = v_pdf_data-stcd2.

    LOOP AT v_pdf_data-data_table_cad ASSIGNING <data>.
      l_alv-ztds_trml_id    = <data>-ztds_trml_id.
      l_alv-bldat           = <data>-bldat.
      l_alv-ztds_bol_nbr    = <data>-ztds_bol_nbr.
      l_alv-bednr           = <data>-bednr.
      l_alv-belnr           = <data>-belnr.
      l_alv-ers_indicator   = <data>-ers_indicator.
      l_alv-delivery_locn   = <data>-delivery_locn.
      l_alv-product         = <data>-product.
      l_alv-volume          = <data>-volume.
      l_alv-unit_of_measure = <data>-unit_of_measure.
      l_alv-unit_price      = <data>-unit_price.
      l_alv-amount_before_tax = <data>-amount_before_tax.             "I - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
      l_alv-gst_hst         = <data>-gst_hst.
      l_alv-pst_qst         = <data>-pst_qst.
      l_alv-fuel_fees       = <data>-fuel_fees.
      l_alv-excise_tax      = <data>-excise_tax.
      l_alv-local_fuel_fees = <data>-local_fuel_fees.
      l_alv-us_tax          = <data>-us_tax.
      l_alv-federal_spill   = <data>-federal_spill.
      l_alv-federal_lust    = <data>-federal_lust.
      l_alv-invoice_amount  = <data>-invoice_amount.
      l_alv-currency        = <data>-currency.
      l_alv-due_date        = <data>-due_date.
      l_alv-ebeln           = <data>-ebeln.
      l_alv-ebelp           = <data>-ebelp.
      l_alv-knumv           = <data>-knumv.
      l_alv-wempf           = <data>-wempf.                 "DV5K968325
***Start of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      l_alv-network         = <data>-network.
      l_alv-pernr           = <data>-pernr.   "DV5K9A0GNZ +
      l_alv-period_from     = <data>-period_from.
      l_alv-period_to       = <data>-period_to.
***End of Insert - XT21609 - DV5K9A07FB - CR333910 TK356363***
      READ TABLE i_extra_data_cad ASSIGNING <extra> INDEX sy-tabix.
      IF sy-subrc = 0.
        l_alv-konnr         = <extra>-konnr.
        l_alv-ktpnr         = <extra>-ktpnr.
        l_alv-bukrs         = <extra>-bukrs.
        l_alv-gjahr         = <extra>-gjahr.
      ELSE.
        CLEAR: l_alv-konnr,
               l_alv-ktpnr,
               l_alv-bukrs,
               l_alv-gjahr.
      ENDIF.
      APPEND l_alv TO ch_alv.
    ENDLOOP.

    LOOP AT v_pdf_data-data_table_usd ASSIGNING <data>.
      l_alv-ztds_trml_id    = <data>-ztds_trml_id.
      l_alv-bldat           = <data>-bldat.
      l_alv-ztds_bol_nbr    = <data>-ztds_bol_nbr.
      l_alv-belnr           = <data>-belnr.
      l_alv-ers_indicator   = <data>-ers_indicator.
      l_alv-delivery_locn   = <data>-delivery_locn.
      l_alv-product         = <data>-product.
      l_alv-volume          = <data>-volume.
      l_alv-unit_of_measure = <data>-unit_of_measure.
      l_alv-unit_price      = <data>-unit_price.
      l_alv-amount_before_tax = <data>-amount_before_tax.             "I - TXT21609 - RTSK0013736 DFCT0011202 - DV5K9A0TR9
      l_alv-gst_hst         = <data>-gst_hst.
      l_alv-pst_qst         = <data>-pst_qst.
      l_alv-fuel_fees       = <data>-fuel_fees.
      l_alv-excise_tax      = <data>-excise_tax.
      l_alv-local_fuel_fees = <data>-local_fuel_fees.
      l_alv-us_tax          = <data>-us_tax.
      l_alv-federal_spill   = <data>-federal_spill.
      l_alv-federal_lust    = <data>-federal_lust.
      l_alv-invoice_amount  = <data>-invoice_amount.
      l_alv-currency        = <data>-currency.
      l_alv-due_date        = <data>-due_date.
      l_alv-ebeln           = <data>-ebeln.
      l_alv-ebelp           = <data>-ebelp.
      l_alv-knumv           = <data>-knumv.
      l_alv-wempf           = <data>-wempf.                 "DV5K968325
      READ TABLE i_extra_data_usd ASSIGNING <extra> INDEX sy-tabix.
      IF sy-subrc = 0.
        l_alv-konnr         = <extra>-konnr.
        l_alv-ktpnr         = <extra>-ktpnr.
        l_alv-bukrs         = <extra>-bukrs.
        l_alv-gjahr         = <extra>-gjahr.
      ELSE.
        CLEAR: l_alv-konnr,
               l_alv-ktpnr,
               l_alv-bukrs,
               l_alv-gjahr.
      ENDIF.
      APPEND l_alv TO ch_alv.
    ENDLOOP.

  ENDMETHOD.                    "format_alv

  METHOD store_in_bdms.

* Local internal tables
    DATA:li_comp     TYPE STANDARD TABLE OF bapicompon,
         l_comp      TYPE bapicompon,
         li_sign     TYPE STANDARD TABLE OF bapisignat,
         l_sign      TYPE bapisignat,
         li_cont_bin TYPE STANDARD TABLE OF bapiconten,
         l_cont_bin  TYPE bapiconten.

* Local variables
    DATA: l_binary   TYPE char1,
          l_count    TYPE i,
          l_xstring  TYPE xstring,
          l_vendor   TYPE lifnr,
          l_bdms_key TYPE bapibds01-objkey.

*--------------------------------------------------------------------*

    l_vendor = vendor_number( abap_true ).
    CONCATENATE l_vendor v_datum v_uzeit INTO l_bdms_key
        SEPARATED BY '_'.

    l_xstring = im_pdf.

* File component
    CLEAR l_comp.
    l_comp-doc_count  = 1.
    l_comp-comp_count = 1.

    CONCATENATE l_bdms_key '.pdf' INTO l_comp-comp_id.  "filename
    l_comp-mimetype  = v_bds_mimetype.
    l_comp-comp_size = xstrlen( l_xstring ).
    APPEND l_comp TO li_comp.

* Prepare file signature properties

* Document type
    CLEAR l_sign.
    l_sign-doc_count = 1.
    l_sign-comp_count = 1.
    l_sign-prop_name = 'BDS_DOCUMENTTYPE'.
    l_sign-prop_value = 'ZINVSUM'.
    APPEND l_sign TO li_sign.

* Document class
    CLEAR l_sign.
    l_sign-doc_count = 1.
    l_sign-comp_count = 1.
    l_sign-prop_name = 'BDS_DOCUMENTCLASS'.
    l_sign-prop_value = 'PDF'.
    APPEND l_sign TO li_sign.

* Description
    CLEAR l_sign.
    l_sign-doc_count = 1.
    l_sign-comp_count = 1.
    l_sign-prop_name = 'DESCRIPTION'.
    l_sign-prop_value = l_comp-comp_id.    "filename
    APPEND l_sign TO li_sign.

* Language
    CLEAR l_sign.
    l_sign-doc_count = 1.
    l_sign-comp_count = 1.
    l_sign-prop_name = 'LANGUAGE'.
    l_sign-prop_value = 'E'.
    APPEND l_sign TO li_sign.

* Storage Category
    CLEAR l_sign.
    l_sign-doc_count = 1.
    l_sign-comp_count = 1.
    l_sign-prop_name = 'STORAGE_CATEGORY'.
    l_sign-prop_value = 'BDS_DB'.
    APPEND l_sign TO li_sign.

* Keyword 1
    CLEAR l_sign.
    l_sign-doc_count = 1.
    l_sign-comp_count = 1.
    l_sign-prop_name = 'BDS_KEYWORD'.
    CONCATENATE l_vendor 'Vendor Invoice Summary'(e00) 'FI-RPT-7H6'
    INTO l_sign-prop_value SEPARATED BY ', '.
    APPEND l_sign TO li_sign.

** Re-format binary data in BDMS compliant content
    DO.
      l_count = xstrlen( l_xstring ).
      IF l_count > 1022.
        l_count = 1022.
      ENDIF.
      l_cont_bin-line = l_xstring(l_count).
      APPEND l_cont_bin TO li_cont_bin.
      SHIFT l_xstring LEFT BY l_count PLACES IN BYTE MODE.
      IF l_xstring IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
    l_binary = abap_true.

* Upload file contents in BDMS for the specified Business Object
    CALL FUNCTION 'BDS_BUSINESSDOCUMENT_CREA_TAB'
      EXPORTING
        logical_system  = v_bds_logical_system
        classname       = c_classname
        classtype       = c_classtype
        object_key      = l_bdms_key
        binary_flag     = l_binary
      IMPORTING
        object_key      = ex_object
      TABLES
        signature       = li_sign
        components      = li_comp
        content         = li_cont_bin
      EXCEPTIONS
        nothing_found   = 1
        parameter_error = 2
        not_allowed     = 3
        error_kpro      = 4
        internal_error  = 5
        not_authorized  = 6
        OTHERS          = 7.

    IF sy-subrc = 0.
*   Update mode, commit upload to database
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ENDIF.

  ENDMETHOD.                    "store_in_bdms

  METHOD get_bdms_key.

    re_bdms = v_bds_object.

  ENDMETHOD.                    "get_bdms_key

ENDCLASS.                    "LCL_EXTRACT IMPLEMENTATION
