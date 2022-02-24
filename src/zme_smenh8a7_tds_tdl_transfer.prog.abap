*----------------------------------------------------------------------*
*                      Canadian National                               *
*                                                                      *
* ABAP Name :      ZME_SMENH8A7_TDS_TDL_TRANSFER                       *
* Created by:      Chuong Le                                           *
* Created on:      2011-01-31                                          *
* Short Description: Transfer Cartage fuelling events from TDS to DTL. *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by             Date                Tracking number *
* -------------------------        ----------          --------------- *
* Chuong Le                        2011-01-31          DV5K960962      *
*                                                                      *
* Short Description : Initial Creation                                 *
*----------------------------------------------------------------------*

REPORT  zme_smenh8a7_tds_tdl_transfer NO STANDARD PAGE HEADING
                                      LINE-SIZE 132.

*----------------------------------------------------------------------*
*       TYPES OR TYPE-POOLS                                            *
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF t_vend_crtg,
    zval_from TYPE tvarv_val,
    lifnr     TYPE lifnr,
  END OF t_vend_crtg,

  BEGIN OF t_lfa1,
    lifnr TYPE lifnr,
    name1 TYPE name1_gp,
  END OF t_lfa1,

  BEGIN OF t_t001w,
    werks TYPE werks_d,
    name1 TYPE name1,
    ort01 TYPE ort01,
    regio TYPE regio,
    land1 TYPE land1,
  END OF t_t001w,

  BEGIN OF t_plntdef,
    werks      TYPE werks_d,
    zplnt_note TYPE zzplant_note,
  END OF t_plntdef,

  BEGIN OF t_email,
    lifnr     TYPE lifnr,
    prsnr     TYPE ad_persnum,
    smtp_addr TYPE ad_smtpadr,
  END OF t_email,

  BEGIN OF t_log,
    msgtype   TYPE c,
    folio_yr  TYPE zztds_folio_year,
    folio_mth TYPE zztds_folio_month,
    folio_nbr TYPE zztds_folio_number,
    folio_seq TYPE zztds_folio_seq_number,
    msgtext   TYPE text80,                                                                                 "DV5K965377
  END OF t_log.

*----------------------------------------------------------------------*
*        DATA/VARIABLE DECLARATION                                     *
*----------------------------------------------------------------------*
DATA:
  i_flcm_tds      TYPE STANDARD TABLE OF zmm_flcm_tds,
  i_tds_success   TYPE STANDARD TABLE OF zmm_flcm_tds,
  i_flcm_parms    TYPE STANDARD TABLE OF zmm_flcm_parms,
  i_dtl_fuel      TYPE STANDARD TABLE OF zmm_dtl_fuel_evt,
  i_vend_crtg     TYPE STANDARD TABLE OF t_vend_crtg,
  i_lfa1          TYPE STANDARD TABLE OF t_lfa1,
  i_t001w         TYPE STANDARD TABLE OF t_t001w,
  i_xref          TYPE STANDARD TABLE OF zmm_flcm_xref,
  i_plntdef       TYPE STANDARD TABLE OF t_plntdef,
  i_email         TYPE STANDARD TABLE OF t_email,
  i_fuel_evt      TYPE STANDARD TABLE OF zmm_dtl_fuel_evt,
  i_log           TYPE STANDARD TABLE OF t_log,
  i_contents      TYPE STANDARD TABLE OF soli,
  i_receivers     TYPE STANDARD TABLE OF soli.

DATA:
  wa_flcm_tds    TYPE zmm_flcm_tds,
  wa_flcm_parms  TYPE zmm_flcm_parms,
  wa_dtl_fuel    TYPE zmm_dtl_fuel_evt,
  wa_vend_crtg   TYPE t_vend_crtg,
  wa_lfa1        TYPE t_lfa1,
  wa_t001w       TYPE t_t001w,
  wa_xref_matkl  TYPE zmm_flcm_xref,
  wa_plntdef     TYPE t_plntdef,
  wa_email       TYPE t_email,
  wa_log         TYPE t_log,
  wa_contents    TYPE soli,
  wa_receivers   TYPE soli.

DATA:
  v_lock_flag   TYPE c,
  v_total_cnt   TYPE i,
  v_success_cnt TYPE i,
  v_error_cnt   TYPE i,
  v_msgtext     TYPE text70.

*----------------------------------------------------------------------*
*        CONSTANTS DECLARATION                                         *
*----------------------------------------------------------------------*
CONSTANTS:
  c_blank         TYPE zztds_cancel_rebill VALUE ' ',   "New
  c_a             TYPE c                   VALUE 'A',   "Active
  c_c             TYPE zztds_cancel_rebill VALUE 'C',   "Cancel
  c_r             TYPE zztds_cancel_rebill VALUE 'R',   "Rebill
  c_i             TYPE c                   VALUE 'I',   "Inactive
  c_n             TYPE zzxtra_chrg_waittm  VALUE 'N',
  c_e             TYPE c                   VALUE 'E',   "Error
  c_s             TYPE c                   VALUE 'S',   "Completed
  c_u             TYPE c                   VALUE 'U',   "Update
  c_x             TYPE c                   VALUE 'X',
  c_y             TYPE zzcartage_ind       VALUE 'Y',
  c_99            TYPE zztran_sub_stus     VALUE 99,    "Successful
* begin of DV5K965071
  c_00            TYPE zztran_stus         VALUE '00',
  c_02            TYPE zztran_stus         VALUE '02',
  c_03            TYPE zztran_stus         VALUE '03',
* end of DV5K965071
  c_eq            TYPE char02              VALUE 'EQ',  "Equal
  c_na            TYPE zzdtl_match_cd      VALUE 'NA',
  c_op            TYPE zzuser_status       VALUE 'OP',
  c_z7            TYPE pafkt               VALUE 'Z7',
  c_tds           TYPE char04              VALUE 'TDS',
  c_tds2dtl       TYPE ltrs_qaval          VALUE 'TDS2DTL',
  c_dash          TYPE c                   VALUE '-',
  c_matkl         TYPE fieldname           VALUE 'MATKL',
  c_digits        TYPE char10              VALUE '0123456789',
  c_smenh8a7      TYPE progname            VALUE 'SMENH8A7',
  c_transload     TYPE tvarv_val           VALUE 'TRANSLOAD',
  c_trans_type    TYPE rvari_vnam          VALUE 'ZTDS_TRAN_TYPE',
  c_vend_crtg     TYPE rvari_vnam          VALUE 'ZTDS_VEND_CRTG',
  c_fuel_table    TYPE tabname             VALUE 'ZMM_DTL_FUEL_EVT',
  c_appnam        TYPE zzapplication  VALUE 'SMFAUD',
  c_parm1         TYPE zzparm_name    VALUE 'Z_SMINT666_COE_EMAIL',
  c_parm2         TYPE zzparm_name    VALUE 'Z_SMINT666_DTL_EMAIL'.

* begin of DV5K967547 Defect 652
CONSTANTS:
  c_success       TYPE bapiret2-type       VALUE 'S',
  c_zz_flcm       TYPE bapiret2-id         VALUE 'ZZ_FLCM',
  c_dtl           TYPE bapiret2-message_v2 VALUE 'DTL',
  c_status_99     TYPE ZMM_FLCM_TDS-ZTRAN_SUB_STUS VALUE '99'.




* end of DV5K967547 Defect 652

*----------------------------------------------------------------------*
*        INITIALIZATION                                                *
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------*
*        AT SELECTION-SCREEN                                           *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
*        START-OF-SELECTION                                            *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM f_initial_data.
  PERFORM f_get_data.

  IF i_flcm_tds[] IS INITIAL.
    MESSAGE i133(n5).
    EXIT.
  ELSE.
    PERFORM f_lock_table CHANGING v_lock_flag.

    IF v_lock_flag = c_x.
      PERFORM f_get_additional_data.
      PERFORM f_process_data.
      PERFORM f_unlock_table.
      PERFORM f_display_log.
      PERFORM f_send_email.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
*        END-OF-SELECTION                                              *
*----------------------------------------------------------------------*
END-OF-SELECTION.


*----------------------------------------------------------------------*
*        INCLUDES                                                      *
*----------------------------------------------------------------------*
  INCLUDE zme_smenh8a7_tds_tdl_transff01.
