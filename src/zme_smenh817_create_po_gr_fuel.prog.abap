*----------------------------------------------------------------------*
*                      Canadian National                               *
*                                                                      *
* ABAP Name :      ZME_SMENH817_CREATE_PO_GR_FUEL                      *
* Created by:      Chuong Le                                           *
* Created on:      2010-12-20                                          *
* Short Description: Create PO and GR for DTL Events                   *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by             Date                Tracking number *
* -------------------------        ----------          --------------- *
* Joemer Trinidad                  2020/05/22          DV5K9A0R6Z      *
*                                                                      *
* Short Description: C403761 T456863                                   *
*    - Fix the the error in updating DTL record with extra charges     *
*----------------------------------------------------------------------*
* Joemer Trinidad                  2020/04/03          DV5K9A0QBF      *
*                                                                      *
* Short Description: C403761 T453881                                   *
*    - Fix the the error in updating DTL record with different         *
*       delivery date                                                  *
*----------------------------------------------------------------------*
* Von Kaisser Almeria              2016-07-22          DV5K9A03CO      *
*                                                                      *
* Short Description : Add controls to avoid creating POs for           *
*   unreasonable amounts.                                              *
*----------------------------------------------------------------------*
* Chuong Le                        2010-12-20          DV5K960962      *
*                                                                      *
* Short Description : Initial Creation                                 *
*----------------------------------------------------------------------*
* Chuong Le                        2011-06-02          DV5K964049      *
*                                                                      *
* Short desc: Fix defects 218 and 226.                                 *
*----------------------------------------------------------------------*
* Chuong Le                        2011-09-01          DV5K966206      *
*                                                                      *
* Short desc: Fix defects 500.                                         *
*       - Add DTL transaction date to selection screen.                *
*----------------------------------------------------------------------*
* Chuong Le                        2011-09-07          DV5K960923      *
*                                                                      *
* Short desc: Fix defect 517.                                          *
*       - Add vendor to selection screen.                              *
*----------------------------------------------------------------------*
* Chuong Le                        2011-09-22          DV5K966646      *
*                                                                      *
* Short desc: Fix defect 555.                                          *
*       - Add DTL match code to selection screen.                      *
*----------------------------------------------------------------------*
* Chuong Le                        2011-09-30          DV5K966805      *
*                                                                      *
* Short desc: Fix defects 570 & 585.                                   *
*       - Updating application log only if record status is '00' for   *
*         the following error messages:                                *
*         1. ZZ_FLCM(017) (no contract found)                          *
*         2. ZZ_FLCM(051) (multiple contracts found)                   *
*       - Validate fuel delivery date within start and end validity    *
*         dates of the contract.                                       *
*----------------------------------------------------------------------*
* Yanick Plante                    2015-01-19          DV5K992637      *
*                                                      DV5K993052      *
*                                                      DV5K993102      *
*                                                                      *
* Short desc: CR257405 TK280378 SM-ENH-817                             *
*                                                                      *
*       - override contract G/L account and Cost Center for            *
*         non-locomotive equipments.                                   *
*       - change cancel indicator condition                            *
*                                                                      *
*----------------------------------------------------------------------*

REPORT  zme_smenh817_create_po_gr_fuel NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
*       TYPES OR TYPE-POOLS                                            *
*----------------------------------------------------------------------*
TYPE-POOLS:  "DV5K992637 CR 257405
  abap.      "DV5K992637 CR 257405
TYPES:
  BEGIN OF t_audit,
    zdtl_fevt_id     TYPE zmm_dtl_fuel_adt-zdtl_fevt_id,
    zdtl_fevt_ver    TYPE zmm_dtl_fuel_adt-zdtl_fevt_ver,
    zdtl_fuel_adt_id TYPE zmm_dtl_fuel_adt-zdtl_fuel_adt_id,
    zdtl_audt_stus   TYPE zmm_dtl_fuel_adt-zdtl_audt_stus,
    zmsg_id          TYPE zmm_dtl_fuel_adt-zmsg_id,
  END OF t_audit,

  BEGIN OF t_po,
    ebeln TYPE ebeln,
    ebelp TYPE ebelp,
    lifnr TYPE elifn,
    ihrez TYPE ihrez,
    matkl TYPE matkl,
    werks TYPE ewerk,
    bednr TYPE bednr,
    afnam TYPE afnam,
    retpo TYPE retpo,
    elikz TYPE elikz,
    eindt TYPE eindt,                             "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
  END OF t_po,

  BEGIN OF t_contract,
    ebeln TYPE ebeln,
    ebelp TYPE ebelp,
    lifnr TYPE elifn,
    matkl TYPE matkl,
    werks TYPE ewerk,
    idnlf TYPE idnlf,
    meins TYPE meins,
    matnr TYPE matnr,
    xersy TYPE xersy,
    kdatb TYPE kdatb,                              "DV5K966805+
    kdate TYPE kdate,                              "DV5K966805+
    bukrs type bukrs,     "DV5K992637 CR 257405
    waers TYPE waers,     "I - XT18912 - CR269546 TK334189 - DV5K9A03CO
  END OF t_contract,

***** start of code added DV5K992637 CR 257405
  t_bapi_account    type bapimepoaccount,
  t_bapi_account_t  type table of bapimepoaccount,
  t_bapi_accountx   type bapimepoaccountx,
  t_bapi_accountx_t type table of bapimepoaccountx,
***** end   of code added DV5K992637 CR 257405

  BEGIN OF t_common,
    id             TYPE zzdtl_fuel_event_id,
    version        TYPE zzdtl_fuel_event_ver,
    lifnr          TYPE elifn,
    matkl          TYPE matkl,
    werks          TYPE ewerk,
    zbol_tckt_nbr  TYPE zzbol_ticket_nbr,
    zdlvr_dt       TYPE zzdtl_fuel_dlvr_dt,
    quantity       TYPE zzdtl_fuel_dlvr_vol,
    uom            TYPE zzdtl_fuel_dlvr_uom,
    zdtl_load_dt   TYPE zzdtl_load_dt,
    zdtl_loco_nbr  TYPE zzdtl_loco_nbr,
    zdtl_loco_init TYPE zzloco_init,
    zcanc_ind      TYPE zzevt_canc_ind,
  END OF t_common,

  BEGIN OF t_goods_recp,
    item TYPE ebelp,
  END OF t_goods_recp.

*----------------------------------------------------------------------*
*        DATA/VARIABLE DECLARATION                                     *
*----------------------------------------------------------------------*
DATA:
  i_fuel          TYPE TABLE OF zmm_dtl_fuel_evt,
  i_fuel_new      TYPE TABLE OF zmm_dtl_fuel_evt,
  i_fuel_update   TYPE TABLE OF zmm_dtl_fuel_evt,
  i_fuel_status   TYPE TABLE OF zmm_dtl_fuel_evt,
  i_charge        TYPE TABLE OF zmm_dtl_xtra_chg,
  i_charge_new    TYPE TABLE OF zmm_dtl_xtra_chg,
  i_charge_update TYPE TABLE OF zmm_dtl_xtra_chg,
  i_charge_status TYPE TABLE OF zmm_dtl_xtra_chg,
  i_charge_fuel   TYPE TABLE OF zmm_dtl_fuel_evt,
  i_audit         TYPE TABLE OF t_audit,
  i_po_list       TYPE SORTED TABLE OF t_po
                  WITH NON-UNIQUE KEY lifnr ihrez matkl
*                                      werks ebeln ebelp,      "D - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
                                      werks eindt ebeln ebelp, "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
  gt_po_list_upd  TYPE STANDARD TABLE OF t_po,                 "I - TXT25668 - C403761 T453881 - DV5K9A0QBF
  i_contract_list TYPE TABLE OF t_contract,
  i_parms         TYPE TABLE OF zmm_flcm_parms,
  i_return        TYPE bapiret2_tab,
  i_po_item       TYPE TABLE OF bapiekpo,
  i_po_schedule   TYPE TABLE OF bapieket,
  i_goods_recp    TYPE TABLE OF t_goods_recp.

DATA:
  wa_fuel        TYPE zmm_dtl_fuel_evt,
  wa_charge      TYPE zmm_dtl_xtra_chg,
  wa_audit       TYPE t_audit,
  wa_po_list     TYPE t_po,
  wa_contract    TYPE t_contract,
  wa_parms       TYPE zmm_flcm_parms,
  wa_return      TYPE bapiret2,
  wa_po_header   TYPE bapiekkol,
  wa_po_item     TYPE bapiekpo,
  wa_po_schedule TYPE bapieket,
  wa_common      TYPE t_common,
  wa_goods_recp  TYPE t_goods_recp.

DATA:   "used for BAPIs
   wa_poheader    TYPE bapimepoheader,
   wa_poheaderx   TYPE bapimepoheaderx,
   i_poitem       TYPE TABLE OF bapimepoitem,
   wa_poitem      TYPE bapimepoitem,
   i_poitemx      TYPE TABLE OF bapimepoitemx,
   wa_poitemx     TYPE bapimepoitemx,
***** start of code added DV5K992637 CR 257405
   i_misc_rail    type table of zmm_flcm_parms,
   i_poaccount    type t_bapi_account_t,
   wa_poaccount   type t_bapi_account,
   i_poaccountx   type t_bapi_accountx_t,
   wa_poaccountx  type t_bapi_accountx,
***** end   of code added DV5K992637 CR 257405
   i_poschedule   TYPE TABLE OF bapimeposchedule,
   wa_poschedule  TYPE bapimeposchedule,
   i_poschedulex  TYPE TABLE OF bapimeposchedulx,
   wa_poschedulex TYPE bapimeposchedulx.

DATA:
  v_rec_count   TYPE i,
  v_duplicate_po TYPE c,
  v_id          TYPE char10,
  v_version     TYPE char05,
  v_index       TYPE i.
*  lv_upd        TYPE abap_bool.                               "I - TXT25668 - C403761 T453881 - DV5K9A0QBF "D - TXT25668 - C403761 T456863 - DV5K9A0R6Z

*----------------------------------------------------------------------*
*        CONSTANTS DECLARATION                                         *
*----------------------------------------------------------------------*
CONSTANTS:
  c_00       TYPE zztran_stus    VALUE '00',    "Ready to be processed
  c_03       TYPE zztran_stus    VALUE '03',    "Successfully Processed
  c_04       TYPE zztran_stus    VALUE '04',    "Processed with error
  c_10       TYPE ebelp          VALUE 10,      "PO item
  c_00001    TYPE num5           VALUE 1,       "Version
  c_a        TYPE zzdtl_mod_cd   VALUE 'A',     "Add
  c_u        TYPE zzdtl_mod_cd   VALUE 'U',     "Update
  c_f        TYPE ebstyp         VALUE 'F',     "PO category
  c_k        TYPE knttp          VALUE 'K',     "Acct. assgmnt category
  c_n        TYPE zzevt_canc_ind VALUE 'N',     "Yes
  c_y        TYPE zzevt_canc_ind VALUE 'Y',     "No
  c_x        TYPE c              VALUE 'X',
  c_fb       TYPE esart          VALUE 'FB',    "PO type FB
  c_fk       TYPE esart          VALUE 'FK',    "PO type FK
  c_dtl      TYPE unsez          VALUE 'DTL',   "Our reference
  c_dtlpo    TYPE idnlf          VALUE 'DTLPO%', "Pattern
  c_abort    TYPE bapi_mtype     VALUE 'A',     "Abort
  c_error    TYPE bapi_mtype     VALUE 'E',     "Error
  c_success  TYPE bapi_mtype     VALUE 'S',     "Successful
  c_code_01  TYPE bapi2017_gm_code VALUE '01',  "GR with PO
  c_type_101 TYPE bwart          VALUE '101',   "Movement type
  c_ind_b    TYPE kzbew          VALUE 'B',     "Movement indicator
  c_msgid    TYPE symsgid        VALUE 'ZZ_FLCM',
  c_smenh817 TYPE progname       VALUE 'SMENH817',
  c_audit    TYPE rvari_vnam     VALUE 'AUDIT',
  c_fuel1010 TYPE matkl          VALUE 'FUEL1010',
  c_fuel_table   TYPE tabname    VALUE 'ZMM_DTL_FUEL_EVT',
  c_charge_table TYPE tabname    VALUE 'ZMM_DTL_XTRA_CHG'.

***** start of code added DV5K992637 CR 257405
data:
  c_matgrp_extra_chrg type char4                    value 'TRNS',                 "DV5K993102
  c_equip_pattern type zmm_flcm_parms-zparm_nm      value 'MISC_RAIL_EQUIP%',
  c_equip         type zmm_flcm_parms-zparm_nm      value 'MISC_RAIL_EQUIP',
  c_gl_account    type zmm_flcm_parms-zparm_nm      value 'MISC_RAIL_EQUIP_GL',
  c_cost_center   type zmm_flcm_parms-zparm_nm      value 'MISC_RAIL_EQUIP_CC',
***BEGIN OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO
  c_cond_check   TYPE zmm_flcm_parms-zparm_nm      VALUE 'ACTIVATE_CONDITION_CHECK',
  c_cap_gallon   TYPE zmm_flcm_parms-zparm_nm      VALUE 'MISC_EQUIP_TANK_CAP_GALLONS',
  c_max_litre    TYPE zmm_flcm_parms-zparm_nm      VALUE 'MAX_AMOUNT_PER_LITRE',
  c_max_gallon   TYPE zmm_flcm_parms-zparm_nm      VALUE 'MAX_AMOUNT_PER_GALLON',
  c_gallon       TYPE bstme                        VALUE 'GAL',
  c_gallon_2     TYPE bstme                        VALUE 'GLL',
  c_liter        TYPE bstme                        VALUE 'L',
  c_cad          TYPE waers                        VALUE 'CAD',
  c_usd          TYPE waers                        VALUE 'USD',
  c_z02          TYPE inob-klart                   VALUE 'Z02',
  c_fuelqty      TYPE char10                       VALUE 'FUELQTY',
  c_fuelqty_int  TYPE atinn,
***END OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO
  c_equal         type zmm_flcm_parms-zparm_opt     value 'EQ',
  c_include       type zmm_flcm_parms-zparm_sign    value 'I'.
***** end   of code added DV5K992637 CR 257405

*----------------------------------------------------------------------*
*        SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*
*Fuelling event
SELECTION-SCREEN BEGIN OF BLOCK selfuel WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_fuelid FOR wa_fuel-zdtl_fevt_id NO INTERVALS,
                s_fuelvr FOR wa_fuel-zdtl_fevt_ver NO INTERVALS.
PARAMETERS:     p_fuelex TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK selfuel.

*Extra charge
SELECTION-SCREEN BEGIN OF BLOCK selcharge WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_chrgid FOR wa_charge-zdtl_xchg_id NO INTERVALS,
                s_chrgvr FOR wa_charge-zdtl_xchg_ver NO INTERVALS.
PARAMETERS:     p_extra TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK selcharge.

*Common fields
SELECTION-SCREEN BEGIN OF BLOCK selcommon WITH FRAME.
SELECT-OPTIONS: s_lifnr  FOR wa_fuel-lifnr,               "DV5K960923+
                s_deldat FOR wa_fuel-zdlvr_dt,
                s_trsdat FOR wa_fuel-ztrid_date,          "DV5K966206+
                s_werks  FOR wa_fuel-werks,
                s_ticket FOR wa_fuel-zbol_tckt_nbr,
                s_locoid FOR wa_fuel-zdtl_loco_nbr
                             NO INTERVALS NO-EXTENSION,
                s_locoin FOR wa_fuel-zdtl_loco_init
                             NO INTERVALS NO-EXTENSION,
                s_matkl  FOR wa_charge-matkl,
                s_recsta FOR wa_fuel-zrec_stus
                             NO INTERVALS NO-EXTENSION,
                s_mtchcd FOR wa_fuel-zdtl_mtch_cd,        "DV5K966646+
                s_modcod FOR wa_fuel-zdtl_mod_cd
                             NO INTERVALS NO-EXTENSION,
                s_canind FOR wa_fuel-zcanc_ind
                             NO INTERVALS NO-EXTENSION,
                s_status FOR wa_fuel-ztran_stus
                             NO INTERVALS NO-EXTENSION,
                s_aedat  FOR wa_fuel-aedat,
                s_aename FOR wa_fuel-aename.
SELECTION-SCREEN END OF BLOCK selcommon.

*----------------------------------------------------------------------*
*        INCLUDES                                                      *
*----------------------------------------------------------------------*
INCLUDE zme_smenh817_create_po_gr_form.

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

  PERFORM f_initialize_data.
  PERFORM f_get_data.
  PERFORM f_generate_data.

*----------------------------------------------------------------------*
*        END-OF-SELECTION                                              *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF i_fuel[] IS INITIAL AND i_charge[] IS INITIAL.
    MESSAGE s133(n5).
    EXIT.
  ENDIF.

  PERFORM f_process_data.
  PERFORM f_update_error_status.

  WRITE:/2 'Program completed successfully!'(003).
