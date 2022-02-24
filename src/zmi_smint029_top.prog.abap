*&---------------------------------------------------------------------*
*&  Include           ZMI_SMINT029_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Canadian National                               *
*                                                                      *
* ABAP Name         : ZMI_SMINT029_TDS_FUEL_TO_EDW                     *
* Created by        : Bob Legault                                      *
* Created on        : 2011-04-08                                       *
* Function Design   : SM-INT-029                                       *
*                                                                      *
* Purpose: Daily extract of TDS fuel records in the ZMM_FLSM_TDS table *
*          which have either been processed or not will be extracted   *
*          and sent to Datacity                                        *
*                                                                      *
* Short Description : Extract of TDS records in the ZMM_FLSCM_TDS table*
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
* Bob Legault         2011-04-08                        DV5K960962     *
*                                                                      *
* Short Description : Initial Development                              *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*


TYPES: BEGIN OF t_text,
        entry(1000) TYPE c,
       END OF t_text.


DATA: i_packing_list  TYPE STANDARD TABLE OF soxpl,
      i_mail_by       TYPE STANDARD TABLE OF solisti1,
      i_zmm_flcm_tds  TYPE STANDARD TABLE OF zmm_flcm_tds,
      wa_zmm_flcm_tds TYPE zmm_flcm_tds,
      i_receivers     TYPE STANDARD TABLE OF somlreci1,
      wa_text         TYPE t_text,
      wa_mail_hd      TYPE sodocchgi1.

DATA: v_file  TYPE rlgrap-filename,
      v_unix  TYPE rlgrap-filename,
      v_subrc TYPE sy-subrc.                            "A-XT16577 2010/01/11 DV5K954669

CONSTANTS: c_path1            TYPE string VALUE '/iface/',
           c_path2            TYPE string VALUE '/out/com/smdata/',
           c_path3            TYPE string VALUE '/out/ia/arc/smdata/',
           c_int              TYPE string VALUE 'INT',
           c_raw              TYPE string VALUE 'RAW',
           c_prefix(8)        TYPE c      VALUE 'SMINT029',
           c_flag             TYPE c      VALUE 'X',
           c_flat(4)          TYPE c      VALUE '.txt',
           c_success          TYPE c      VALUE 'S',
           c_info             TYPE c      VALUE 'I',
           c_error            TYPE c      VALUE 'E',    "A-XT16577 2010/02/02 DV5K955251
           c_zero             TYPE c      VALUE '0',
           c_comma            TYPE c      VALUE ',',
           c_dash             TYPE c      VALUE '-',
           c_add              TYPE zzdtl_mod_cd   VALUE 'A',
           c_update           TYPE zzdtl_mod_cd   VALUE 'U',
           c_delete           TYPE zzevt_canc_ind VALUE 'Y',
           c_notdel           TYPE zzevt_canc_ind VALUE 'N',
           c_gallons          TYPE zzdtl_fuel_dlvr_uom   VALUE 'GAL',
           c_convgal          TYPE zzdtl_fuel_dlvr_uom   VALUE 'GLL',
           c_smint669         TYPE zvu_email-application VALUE 'SMFAUD',
           c_smint669_emailto TYPE zvu_email-parm_name   VALUE 'Z_SMINT666_COE_EMAIL',
           c_smint669_emailcc TYPE zvu_email-parm_name   VALUE 'Z_SMINT666_DTL_EMAIL',
           c_midnight         TYPE zmm_flcm_tds-ztds_tran_tm VALUE '240000'.                               "DV5K966636

* begin of DV5K967547 Defect 652
CONSTANTS:
  c_zz_flcm       TYPE bapiret2-id         VALUE 'ZZ_FLCM',
  c_edw           TYPE bapiret2-message_v2 VALUE 'EDW',
  c_status_99     TYPE ZMM_FLCM_TDS-ZTRAN_SUB_STUS VALUE '99'.



* end of DV5K967547 Defect 652
