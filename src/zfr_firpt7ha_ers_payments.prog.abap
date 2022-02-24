*----------------------------------------------------------------------*
*                      Canadian National                               *
*                                                                      *
* ABAP Name :  ZFR_FIRPT7HA_ERS_PAYMENTS                               *
* Created by:  Chuong Le                                               *
* Created on:  2011-03-10                                              *
* Short Description: This program will create payment advice in CSV    *
*                    file format and email it to ERS vendors. It is a  *
*                    batch report but can also be executed online via  *
*                    transaction ZFI_CSVPAYADVICE.                     *
* Transaction:  ZFI_CSVPAYADVICE                                       *
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by             Date                Tracking number *
* -------------------------        ----------          --------------- *
* Charles Darby                    2014-11-13          DV5K991322      *
*                                                                      *
* Short Description : C255559-T277979                                  *
*                     the program doesn't take in consideration that   *
*                     the cie 1000 is paying for 1236 for instance     *
*                     Added zall_parms for US companies
*                                                                      *
*----------------------------------------------------------------------*
* Chuong Le                        2011-03-10          DV5K960962      *
*                                                                      *
* Short Description : Initial Creation                                 *
*----------------------------------------------------------------------*
* Chuong Le                        2011-05-30          DV5K963687      *
*                                                                      *
* Short desc: Fix defects 147, 162 and 207.                            *
*----------------------------------------------------------------------*
REPORT  zfr_firpt7ha_ers_payments NO STANDARD PAGE HEADING
                                  LINE-SIZE 255.

TYPE-POOLS abap.

*----------------------------------------------------------------------*
*        TYPES                                                         *
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF t_bsak,
    lifnr TYPE bsak-lifnr,
    bukrs TYPE bsak-bukrs,
    augbl TYPE bsak-augbl,
    augdt TYPE bsak-augdt,
    gjahr TYPE bsak-gjahr,
    belnr TYPE bsak-belnr,
    buzei TYPE bsak-buzei,
    blart TYPE bsak-blart,
    xblnr TYPE bsak-xblnr,
    bldat TYPE bsak-bldat,
    bschl TYPE bsak-bschl,
    wrbtr TYPE bsak-wrbtr,
    waers TYPE bsak-waers,
    sknto TYPE bsak-sknto,
    wskto TYPE bsak-wskto,
  END OF t_bsak,

  BEGIN OF t_bkpf,
    bukrs  TYPE bkpf-bukrs,
    belnr  TYPE bkpf-belnr,
    gjahr  TYPE bkpf-gjahr,
    blart  TYPE bkpf-blart,
    bldat  TYPE bkpf-bldat,
    waers  TYPE bkpf-waers,
  END OF t_bkpf,

  BEGIN OF t_bseg,
    bukrs  TYPE bseg-bukrs,
    belnr  TYPE bseg-belnr,
    gjahr  TYPE bseg-gjahr,
    buzei  TYPE bseg-buzei,
    bschl  TYPE bseg-bschl,
    wrbtr  TYPE bseg-wrbtr,
    valut  TYPE bseg-valut,
  END OF t_bseg,

  BEGIN OF t_bseg_po,
    bukrs  TYPE bseg-bukrs,
    belnr  TYPE bseg-belnr,
    gjahr  TYPE bseg-gjahr,
    buzei  TYPE bseg-buzei,
    buzid  TYPE bseg-buzid,
    ebeln  TYPE bseg-ebeln,
    ebelp  TYPE bseg-ebelp,
  END OF t_bseg_po,

  BEGIN OF t_ekpo,
    ebeln  TYPE ekpo-ebeln,
    ebelp  TYPE ekpo-ebelp,
    txz01  TYPE ekpo-txz01,
    bednr  TYPE ekpo-bednr,
  END OF t_ekpo,

  BEGIN OF t_bvor,
    bukrs  TYPE bkpf-bukrs,
    belnr  TYPE bkpf-belnr,
    gjahr  TYPE bkpf-gjahr,
    blart  TYPE bkpf-blart,
    bvorg  TYPE bkpf-bvorg,
    bukrs2 TYPE bvor-bukrs,
    belnr2 TYPE bvor-belnr,
    gjahr2 TYPE bvor-gjahr,
  END OF t_bvor,

  BEGIN OF t_lfa1,
    lifnr TYPE lfa1-lifnr,
    land1 TYPE lfa1-land1,
    name1 TYPE lfa1-name1,
    pstlz TYPE lfa1-pstlz,
    regio TYPE lfa1-regio,
    stras TYPE lfa1-stras,
    mcod3 TYPE lfa1-mcod3,
  END OF t_lfa1,

  BEGIN OF t_t001,
    bukrs TYPE t001-bukrs,
    butxt TYPE t001-butxt,
  END OF t_t001,

  BEGIN OF t_vend_comp,
    lifnr TYPE bsak-lifnr,
    bukrs TYPE bsak-bukrs,
  END OF t_vend_comp,

  BEGIN OF t_cross_no,
    bvorg  TYPE bkpf-bvorg,
  END OF t_cross_no,

  BEGIN OF t_email,
    lifnr TYPE lifnr,
    bukrs TYPE bukrs,
    intad TYPE intad,
  END OF t_email,

  BEGIN OF t_kz_sum,
    lifnr TYPE bsak-lifnr,
    bukrs TYPE bsak-bukrs,
    augbl TYPE bsak-augbl,
    augdt TYPE bsak-augdt,
    gjahr TYPE bsak-gjahr,
    belnr TYPE bsak-belnr,
    wrbtr TYPE bsak-wrbtr,
  END OF t_kz_sum,

  BEGIN OF t_zall_parms,
    name TYPE zall_parms-name,
    type TYPE zall_parms-type,
    numb TYPE zall_parms-numb,
    sign TYPE zall_parms-sign,
    opti TYPE zall_parms-opti,
    low  TYPE zall_parms-low,
    high TYPE zall_parms-high,
  END OF t_zall_parms.

*----------------------------------------------------------------------*
*        DATA/VARIABLE DECLARATION                                     *
*----------------------------------------------------------------------*
DATA:
  i_bsak_hdr      TYPE STANDARD TABLE OF t_bsak,
  i_bsak_kx       TYPE STANDARD TABLE OF t_bsak,
  i_bsak_kz       TYPE STANDARD TABLE OF t_bsak,
  i_bsak_itm      TYPE STANDARD TABLE OF t_bsak,
  wa_bsak         TYPE t_bsak,
  wa_bsak_itm     TYPE t_bsak,
  i_bkpf          TYPE STANDARD TABLE OF t_bkpf,
  i_bseg          TYPE STANDARD TABLE OF t_bseg,
  i_bseg_po       TYPE STANDARD TABLE OF t_bseg_po,
  i_ekpo          TYPE STANDARD TABLE OF t_ekpo,
  i_bvor          TYPE STANDARD TABLE OF t_bvor,
  i_lfa1          TYPE STANDARD TABLE OF t_lfa1,
  wa_lfa1         TYPE t_lfa1,
  i_t001          TYPE STANDARD TABLE OF t_t001,
  i_email         TYPE STANDARD TABLE OF t_email,
  i_attachment    TYPE STANDARD TABLE OF soli,
  wa_attachment   TYPE soli,
  i_kz_sum        TYPE TABLE OF t_kz_sum,
  wa_kz_sum       TYPE t_kz_sum,
  i_cross_no      TYPE SORTED TABLE OF t_cross_no
                       WITH UNIQUE KEY bvorg,
  r_compcode_us   TYPE TABLE OF vdbukrs_range,
  wa_compcode_us  TYPE vdbukrs_range,
  i_vend_comp     TYPE SORTED TABLE OF t_vend_comp
                       WITH UNIQUE KEY lifnr bukrs,
  wa_vend_comp    TYPE t_vend_comp,
  i_zallparm      TYPE STANDARD TABLE OF zall_parms.  "I-DV5K991322

DATA:
  v_email_autho TYPE abap_bool VALUE abap_false,
  v_exit        TYPE abap_bool VALUE abap_false,
  v_answer      TYPE c.

*----------------------------------------------------------------------*
*        CONSTANTS DECLARATION                                         *
*----------------------------------------------------------------------*
CONSTANTS:
  c_a       TYPE c        VALUE 'A',
  c_b       TYPE c        VALUE 'B',
  c_c       TYPE c        VALUE 'C',
  c_i       TYPE c        VALUE 'I',
  c_u       TYPE c        VALUE 'U',
  c_x       TYPE c        VALUE 'X',
  c_h       TYPE shkzg    VALUE 'H',
  c_21      TYPE bschl    VALUE '21',
  c_27      TYPE bschl    VALUE '27',
  c_35      TYPE bschl    VALUE '35',
  c_37      TYPE bschl    VALUE '37',
  c_38      TYPE bschl    VALUE '38',
  c_40      TYPE bschl    VALUE '40',
  c_ca      TYPE land1_gp VALUE 'CA',
  c_cn      TYPE char2    VALUE 'CN',
  c_bt      TYPE char2    VALUE 'BT',
  c_eq      TYPE char2    VALUE 'EQ',
  c_kx      TYPE blart    VALUE 'KX',
  c_kz      TYPE blart    VALUE 'KZ',
  c_z1      TYPE pafkt    VALUE 'Z1',
  c_csv     TYPE char3    VALUE 'CSV',
  c_ext     TYPE char3    VALUE 'EXT',
  c_int     TYPE char3    VALUE 'INT',
  c_raw     TYPE char3    VALUE 'RAW',
  c_prd     TYPE sysysid  VALUE 'PRD',
  c_dash    TYPE c        VALUE '-',
  c_stars   TYPE char4    VALUE '****',
  c_comma   TYPE c        VALUE ',',
  c_quote   TYPE c        VALUE '"',
  c_espoper TYPE syuname  VALUE 'ESPOPER',
  c_2010    TYPE bukrs    VALUE '2010',
  c_1000    TYPE bukrs    VALUE '1000', "I-DV5K991322
  c_2070    TYPE bukrs    VALUE '2070',
  c_2160    TYPE bukrs    VALUE '2160',
  c_2355    TYPE bukrs    VALUE '2355',
  c_1210    TYPE bukrs    VALUE '1210', "I-DV5K991322
  c_zallparm TYPE rvari_vnam VALUE 'Z_FIRPT7HA_ERS_PAYMENTS'.


*----------------------------------------------------------------------*
*        SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_augbl FOR wa_bsak-augbl,
                s_lifnr FOR wa_bsak-lifnr OBLIGATORY NO INTERVALS,
                s_gjahr FOR wa_bsak-gjahr,
                s_bukrs FOR wa_bsak-bukrs OBLIGATORY NO INTERVALS,
                s_augdt FOR wa_bsak-augdt OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS:     p_email TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK blk.

*----------------------------------------------------------------------*
*        INITIALIZATION                                                *
*----------------------------------------------------------------------*
INITIALIZATION.
  CLEAR s_augdt.
  s_augdt-sign   = c_i.
  s_augdt-option = c_bt.
  s_augdt-low    = sy-datum - 6.
  s_augdt-high   = sy-datum.
  APPEND s_augdt.
*---> Start of DV5K991322
  PERFORM f_get_compcodes.
*  CLEAR wa_compcode_us.
*  wa_compcode_us-sign   = c_i.
*  wa_compcode_us-option = c_eq.
*
*  wa_compcode_us-low    = c_2070.
*  APPEND wa_compcode_us TO r_compcode_us.
*  wa_compcode_us-low    = c_2160.
*  APPEND wa_compcode_us TO r_compcode_us.
*  wa_compcode_us-low    = c_2355.
*  APPEND wa_compcode_us TO r_compcode_us.
*  wa_compcode_us-low    = c_1210.         "I-DV5K991322
*  APPEND wa_compcode_us TO r_compcode_us. "I-DV5K991322
*<--- End of DV5K991322
*----------------------------------------------------------------------*
*        AT SELECTION-SCREEN                                           *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*        START-OF-SELECTION                                            *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF p_email IS NOT INITIAL.
    PERFORM f_check_email_authorization CHANGING v_email_autho
                                                 v_exit.
    IF v_exit IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDIF.

  PERFORM f_get_vendor_data.

  PERFORM f_get_payment_data.

*----------------------------------------------------------------------*
*        END-OF-SELECTION                                            *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF i_bsak_hdr[] IS INITIAL.
    MESSAGE s133(n5).
  ELSE.
    PERFORM f_process_data.
  ENDIF.

*----------------------------------------------------------------------*
*        INCLUDES                                                      *
*----------------------------------------------------------------------*
  INCLUDE zfr_firpt7ha_ers_payments_f01.
