*----------------------------------------------------------------------*
*                      Canadian National                               *
*                                                                      *
* ABAP Name :      ZME_SMENH817_CREATE_PO_GR_FORM                      *
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
* Chari Supino                     2019/09/12          DV5K9A0MSD      *
*                                                                      *
* Short Description: CR368488 TK437632                                 *
*    - Add delivery date as condition for PO duplicates                *
*----------------------------------------------------------------------*
* Chuong Le                        2017/01/04          DV5K9A05WW      *
*                                                                      *
* Short Description: CR306321-T347799                                  *
*    - Save Our Reference in PO header text instead of EKKO-UNSEZ.     *
*----------------------------------------------------------------------*
* Von Kaisser Almeria              2016-07-28          DV5K9A03CO      *
*                                                      DV5K9A03XO      *
*                                                                      *
* Short Description : CR269546 TK334189                                *
*                     Add controls to avoid creating POs for           *
*                     unreasonable amounts                             *
*                     Change control #1 to validate contract UOM       *
*                     against contract UOM                             *
*----------------------------------------------------------------------*
* Chuong Le                        2010-12-20          DV5K960962      *
*                                                                      *
* Short Description : Initial Creation                                 *
*----------------------------------------------------------------------*
* Chuong Le                        2011-06-02          DV5K964049      *
*                                                                      *
* Short desc: Fix defects 218 and 226.                                 *
*----------------------------------------------------------------------*
* Chuong Le                        2011-06-20          DV5K964212      *
*                                                                      *
* Short desc: Fix defects 247 and 268.                                 *
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
* Chuong Le                        2011-09-19          DV5K960991      *
*                                                                      *
* Short desc: Fix defect 540.                                          *
*----------------------------------------------------------------------*
* Chuong Le                        2011-09-22          DV5K966646      *
*                                                                      *
* Short desc: Fix defect 555.                                          *
*       - Add DTL match code to selection screen.                      *
*----------------------------------------------------------------------*
* Chuong Le                        2011-09-23          DV5K966672      *
*                                                                      *
* Short desc: Fix defect 559.                                          *
*       - Add logic to update error log for associated extra charges.  *
*----------------------------------------------------------------------*
* Chuong Le                        2011-09-30          DV5K966805      *
*                                                                      *
* Short desc: Fix defects 570, 581 & 585.                              *
*       - Updating application log only if record status is '00' for   *
*         the following error messages:                                *
*         1. ZZ_FLCM(017) (no contract found)                          *
*         2. ZZ_FLCM(051) (multiple contracts found)                   *
*       - Change GR posting date to system's date - 1.                 *
*       - Validate fuel delivery date within start and end validity    *
*         dates of the contract.                                       *
*----------------------------------------------------------------------*
* Rob West                         2011/11/24           DV5K967793     *
*                                                                      *
* Short desc: Fix defect 659.                                          *
*       - Add logic to create new PO when previous version failed      *
*         with 0001 or 0012 error code.                                *
*                                                                      *
* Short desc: Fix defect 651.                                          *
*       - DOC_DATE changed to ZDLVR_DT instead of system date          *
*                                                                      *
*----------------------------------------------------------------------*
* Rob West                         2012/01/03           DV5K968544     *
*                                                                      *
* Short desc: Fix defect 683.                                          *
*       - Perform new code added for defect 659 only when              *
*         zcanc_ind = "N".  Bypass in all other cases.                 *
*                                                                      *
*----------------------------------------------------------------------*
* Yanick Plante                    2015-01-19           DV5K992637     *
*                                                       DV5K993052     *
*                                                       DV5K993102     *
*                                                                      *
* Short desc: CR257405 TK280378 SM-ENH-817                             *
*                                                                      *
*       - override contract G/L account and Cost Center for            *
*         non-locomotive equipments.                                   *
*       - change cancel indicator condition                            *
*                                                                      *
*----------------------------------------------------------------------*
* Rob West                         2016/05              DV5K9A022C     *
*                                  2016/07              DV5K9A039Y     *
*                                                                      *
* Short desc: CR292257-T329648                                         *
*       - Bypass ZCRTG_IND = 'Y' in "Search C"                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_INITIALIZE_DATA
*&---------------------------------------------------------------------*
form f_initialize_data.

  refresh: i_parms, i_audit, i_fuel, i_charge,
           i_fuel_new, i_fuel_update, i_charge_new, i_charge_update,
           i_fuel_status, i_charge_status, i_charge_fuel,
           i_po_list, i_contract_list.

endform.                    " F_INITIALIZE_DATA

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
form f_get_data.

  data: li_fuel_04   type table of zmm_dtl_fuel_evt,
        li_charge_04 type table of zmm_dtl_xtra_chg,
        l_delete_flg type c.

***BEGIN OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input         = c_fuelqty
    IMPORTING
      output        = c_fuelqty_int.
***END OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO

* Get parameter values
  select * into table i_parms
    from zmm_flcm_parms
    where progname = c_smenh817
      and zparm_nm = c_audit.

  if not i_parms[] is initial.
*   Get list of erroneous fuelling events
    select zdtl_fevt_id zdtl_fevt_ver zdtl_fuel_adt_id  "#EC CI_NOFIELD  DV5K9A022C
           zdtl_audt_stus zmsg_id
      into table i_audit
      from zmm_dtl_fuel_adt
      for all entries in i_parms
      where zdtl_audt_stus = i_parms-zval_from(1)
        and zmsg_id        = i_parms-zval_to(10).
  endif.

  if p_extra is initial.            "Process fuelling events
*   Get list of fuelling events
    select * into table i_fuel
      from zmm_dtl_fuel_evt
      where zdtl_fevt_id   in s_fuelid
        and zdtl_fevt_ver  in s_fuelvr
        and lifnr          in s_lifnr              "DV5K960923+
        and zdlvr_dt       in s_deldat
        and werks          in s_werks
        and zbol_tckt_nbr  in s_ticket
        and zdtl_loco_nbr  in s_locoid
        and zdtl_loco_init in s_locoin
        and matkl          in s_matkl
        and zrec_stus      in s_recsta
        and zdtl_mod_cd    in s_modcod
        and zcanc_ind      in s_canind
        and ztran_stus     in s_status and ztran_stus in (c_00,c_04)
        and aedat          in s_aedat
        and aename         in s_aename
        and ztrid_date     in s_trsdat             "DV5K966206+
        and zdtl_mtch_cd   in s_mtchcd.            "DV5K966646+
  endif.

  if p_fuelex is not initial.
    if i_fuel[] is not initial.
*     Get list of all extra charges (only with fuelling events)
      select * into table i_charge
        from zmm_dtl_xtra_chg
        for all entries in i_fuel
        where zdtl_xchg_id   in s_chrgid
          and zdtl_xchg_ver  in s_chrgvr
          and zdtl_trns_id    = i_fuel-zdtl_trns_id        "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
          and lifnr           = i_fuel-lifnr
          and zdlvr_dt       in s_deldat
          and werks           = i_fuel-werks
          and zbol_tckt_nbr   = i_fuel-zbol_tckt_nbr
          and matkl          in s_matkl
          and zrec_stus      in s_recsta
          and zdtl_mod_cd    in s_modcod
          and zcanc_ind      in s_canind
          and ztran_stus     in s_status and ztran_stus in (c_00,c_04)
          and aedat          in s_aedat
          and aename         in s_aename.
    endif.
  else.
*   Get list of all extra charges (with and without fuelling events)
    select * into table i_charge
      from zmm_dtl_xtra_chg
      where zdtl_xchg_id   in s_chrgid
        and zdtl_xchg_ver  in s_chrgvr
        and lifnr          in s_lifnr              "DV5K960923+
        and zdlvr_dt       in s_deldat
        and werks          in s_werks
        and zbol_tckt_nbr  in s_ticket
        and matkl          in s_matkl
        and zrec_stus      in s_recsta
        and zdtl_mod_cd    in s_modcod
        and zcanc_ind      in s_canind
        and ztran_stus     in s_status and ztran_stus in (c_00,c_04)
        and aedat          in s_aedat
        and aename         in s_aename.
  endif.

* Split error records (ztran_stus = '04') into temporary tables
  li_fuel_04[] = i_fuel[].
  delete li_fuel_04 where ztran_stus <> c_04.

  li_charge_04[] = i_charge[].
  delete li_charge_04 where ztran_stus <> c_04.

* For Ready to be processed records (ztran_stus = '00'), we keep
* only the record with the lastest version
  delete: i_fuel   where ztran_stus <> c_00,
          i_charge where ztran_stus <> c_00.

  sort: i_fuel   by zdtl_fevt_id zdtl_fevt_ver descending,
        i_charge by zdtl_xchg_id zdtl_xchg_ver descending.

  delete: adjacent duplicates from i_fuel   comparing zdtl_fevt_id,
          adjacent duplicates from i_charge comparing zdtl_xchg_id.

* Insert error records (ztran_stus = '04') back to main table
  append: lines of li_fuel_04[]   to i_fuel[],
          lines of li_charge_04[] to i_charge[].

  sort: i_fuel   by zdtl_fevt_id zdtl_fevt_ver,
        i_charge by zdtl_xchg_id zdtl_xchg_ver,
        i_audit  by zdtl_fevt_id zdtl_fevt_ver.

**> Begin excluding records that are audited as erroneous.
  loop at i_fuel into wa_fuel where zaudt_stus = 'F'
                                 or zaudt_stus = space.
    clear l_delete_flg.
    case wa_fuel-zaudt_stus.
      when 'F'.      "Failed
*       Check for erroneous fuelling event
        read table i_audit into wa_audit
                           with key zdtl_fevt_id  = wa_fuel-zdtl_fevt_id
                                    zdtl_fevt_ver = wa_fuel-zdtl_fevt_ver
                                    binary search.
        if sy-subrc = 0.
          l_delete_flg = c_x.
        endif.

      when space.
*        if wa_fuel-zcanc_ind <> c_y.    "DV5K992637 CR 257405
        if wa_fuel-zcanc_ind = c_n.      "DV5K992637 CR 257405
          l_delete_flg = c_x.
        endif.
    endcase.

    if l_delete_flg = c_x.
      delete i_fuel.
      delete i_charge where lifnr         = wa_fuel-lifnr
                        and zbol_tckt_nbr = wa_fuel-zbol_tckt_nbr
                        and werks         = wa_fuel-werks.
    endif.
  endloop.
**< End excluding records that are audited as erroneous.

***** start of code added DV5K992637 CR 257405
  refresh i_misc_rail.
  select *
    into table i_misc_rail
    from zmm_flcm_parms
   where ( progname      = c_smenh817 )
     and ( zparm_nm   like c_equip_pattern )
     and ( zparm_sign    = c_include )
     and ( zparm_opt     = c_equal ).
  sort i_misc_rail by zparm_nm zval_from.
***** end   of code added DV5K992637 CR 257405

endform.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  F_GENERATE_DATA
*&---------------------------------------------------------------------*
form f_generate_data.

  if not i_fuel[] is initial.
*   Extract list of existing POs for fuel
    select a~ebeln b~ebelp a~lifnr a~ihrez
           b~matkl b~werks b~bednr b~afnam b~retpo b~elikz
           c~eindt                                         "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
      into table i_po_list
      from ekko as a
      inner join ekpo as b on a~ebeln = b~ebeln
      INNER JOIN eket AS c                                 "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
      ON  b~ebeln EQ c~ebeln                               "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
      AND b~ebelp EQ c~ebelp                               "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
      for all entries in i_fuel
    where a~lifnr = i_fuel-lifnr
      and a~bsart = c_fb
      and a~bstyp = c_f
      and a~ihrez = i_fuel-zbol_tckt_nbr
      and b~werks = i_fuel-werks
      and b~loekz = space
      and b~matkl = i_fuel-matkl.

*   Extract list of existing Contracts for fuel
    select a~ebeln b~ebelp a~lifnr b~matkl b~werks b~idnlf b~meins
           b~matnr b~xersy a~kdatb a~kdate                "DV5K966805+
           a~bukrs                          "DV5K992637 CR 257405
           a~waers                          "I - XT18912 - CR269546 TK334189 - DV5K9A03CO
      into table i_contract_list
      from ekko as a
      inner join ekpo as b on a~ebeln = b~ebeln
      for all entries in i_fuel
    where a~lifnr  = i_fuel-lifnr
      and a~bsart  = c_fk
      and a~bstyp  = c_k
      and a~kdatb <= i_fuel-zdlvr_dt
      and a~kdate => i_fuel-zdlvr_dt
      and b~werks  = i_fuel-werks
      and b~matkl  = i_fuel-matkl
      and b~loekz  = space
      and b~knttp  = c_k
      and ( b~idnlf like c_dtlpo or b~xersy = c_x ).
  endif.

  if not i_charge[] is initial.
*   Extract list of existing POs for extra charges
    select a~ebeln b~ebelp a~lifnr a~ihrez
           b~matkl b~werks b~bednr b~afnam b~retpo b~elikz
           c~eindt                                         "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
      appending table i_po_list
      from ekko as a
      inner join ekpo as b on a~ebeln = b~ebeln
      INNER JOIN eket AS c                                 "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
      ON  b~ebeln EQ c~ebeln                               "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
      AND b~ebelp EQ c~ebelp                               "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
      for all entries in i_charge
    where a~lifnr = i_charge-lifnr
      and a~bsart = c_fb
      and a~bstyp = c_f
      and a~ihrez = i_charge-zbol_tckt_nbr
      and b~werks = i_charge-werks
      and b~loekz = space
      and ( b~matkl = i_charge-matkl or b~matkl = c_fuel1010 ).

*   Extract list of existing Contracts for extra charges
    select a~ebeln b~ebelp a~lifnr b~matkl b~werks b~idnlf b~meins
           b~matnr b~xersy a~kdatb a~kdate                "DV5K966805+
           a~bukrs                          "DV5K992637 CR 257405
           a~waers                          "I - XT18912 - CR269546 TK334189 - DV5K9A03CO
      appending table i_contract_list
      from ekko as a
      inner join ekpo as b on a~ebeln = b~ebeln
      for all entries in i_charge
    where a~lifnr  = i_charge-lifnr
      and a~bsart  = c_fk
      and a~bstyp  = c_k
      and a~kdatb <= i_charge-zdlvr_dt
      and a~kdate => i_charge-zdlvr_dt
      and b~werks  = i_charge-werks
      and b~matkl  = i_charge-matkl
      and b~loekz  = space
      and b~knttp  = c_k
      and ( b~idnlf like c_dtlpo or b~xersy = c_x ).

*   Extract list of associated fuelling event records
*   for extra charges
    select * into table i_charge_fuel    "#EC CI_NOFIELD   DV5K9A022C
      from zmm_dtl_fuel_evt
      for all entries in i_charge
      where lifnr         = i_charge-lifnr
        and werks         = i_charge-werks
        and zbol_tckt_nbr = i_charge-zbol_tckt_nbr
        and matkl         = c_fuel1010.
    IF sy-subrc = 0.                                        "DV5K9A039Y
     DELETE i_charge_fuel[] WHERE zcrtg_ind = c_y.          "DV5K9A039Y
    ENDIF.                                                  "DV5K9A039Y
  endif.

  sort: i_contract_list by lifnr matkl werks,
*        i_charge_fuel   by lifnr zbol_tckt_nbr werks.                  "D - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
        i_charge_fuel   BY lifnr zbol_tckt_nbr werks zdlvr_dt.          "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD

  delete adjacent duplicates from i_po_list
*                  comparing lifnr ihrez matkl werks ebeln ebelp.       "D - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
                  COMPARING lifnr ihrez matkl werks eindt ebeln ebelp.  "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD

*** Start of Insert TXT25668 - C403761 T453881 - DV5K9A0QBF
  gt_po_list_upd = i_po_list.
  LOOP AT gt_po_list_upd ASSIGNING FIELD-SYMBOL(<ls_po_list_upd>).
    <ls_po_list_upd>-afnam = <ls_po_list_upd>-afnam(10).
  ENDLOOP.
*** End of Insert TXT25668 - C403761 T453881 - DV5K9A0QBF

  loop at i_fuel into wa_fuel.
    if wa_fuel-zcanc_ind = c_n.
***   New fuel record
      if wa_fuel-zdtl_mod_cd = c_a or
       ( wa_fuel-zdtl_mod_cd = c_u and
         wa_fuel-zdtl_fevt_ver = c_00001 ).
        append wa_fuel to i_fuel_new.
***   Update fuel record
      elseif wa_fuel-zdtl_mod_cd = c_u and
             wa_fuel-zdtl_fevt_ver > c_00001.
        append wa_fuel to i_fuel_update.
      endif.
    elseif wa_fuel-zcanc_ind = c_y.
***   Cancel fuel record
      if ( wa_fuel-zdtl_mod_cd = c_a and
           wa_fuel-zdtl_fevt_ver > c_00001 ) or          "DV5K960991+
           wa_fuel-zdtl_mod_cd = c_u.
        append wa_fuel to i_fuel_update.
      endif.
    endif.
  endloop.

  loop at i_charge into wa_charge.
    if wa_charge-zcanc_ind = c_n.
***   New extra charge record
      if wa_charge-zdtl_mod_cd = c_a or
       ( wa_charge-zdtl_mod_cd = c_u and
         wa_charge-zdtl_xchg_ver = c_00001 ).
        append wa_charge to i_charge_new.
***   Update extra charge record
      elseif wa_charge-zdtl_mod_cd = c_u and
             wa_charge-zdtl_xchg_ver > c_00001.
        append wa_charge to i_charge_update.
      endif.
    elseif wa_charge-zcanc_ind = c_y.
***   Cancel extra charge record
      if ( wa_charge-zdtl_mod_cd = c_a and
           wa_charge-zdtl_xchg_ver > c_00001 ) or        "DV5K960991+
           wa_charge-zdtl_mod_cd = c_u.
        append wa_charge to i_charge_update.
      endif.
    endif.
  endloop.

endform.                    " F_GENERATE_DATA

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_DATA
*&---------------------------------------------------------------------*
form f_process_data .

* Process Fuel new records
  loop at i_fuel_new into wa_fuel.
    perform f_move_fields using    c_x
                          changing wa_common.

    perform f_process_new_recs using c_fuel_table
                                     c_x
                                     wa_common
                            changing wa_fuel-ztran_stus.
    wa_fuel-aedat  = sy-datum.
    wa_fuel-aezeit = sy-uzeit.
    wa_fuel-aename = sy-uname.
    append wa_fuel to i_fuel_status.
  endloop.

* Process Fuel update/cancel records
  loop at i_fuel_update into wa_fuel.
    perform f_move_fields using    c_x
                          changing wa_common.

    perform f_process_update_cancel_recs using c_fuel_table
                                               wa_common
                                      changing wa_fuel-ztran_stus.
    wa_fuel-aedat  = sy-datum.
    wa_fuel-aezeit = sy-uzeit.
    wa_fuel-aename = sy-uname.
    append wa_fuel to i_fuel_status.
  endloop.

* Process Extra charge new records
  loop at i_charge_new into wa_charge.
    perform f_move_fields using    space
                          changing wa_common.

*   Check if extra charge has fuelling event associated with?
    read table i_charge_fuel into wa_fuel
               with key lifnr         = wa_charge-lifnr
                        zbol_tckt_nbr = wa_charge-zbol_tckt_nbr
                        werks         = wa_charge-werks
                        zdlvr_dt      = wa_charge-zdlvr_dt    "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
                        binary search.
    if sy-subrc = 0.
      perform f_process_ext_charge_with_fuel
                               using   c_charge_table
                                       wa_common
                              changing wa_charge-ztran_stus.
    else.
      perform f_process_new_recs using c_charge_table
                                       space
                                       wa_common
                              changing wa_charge-ztran_stus.
    endif.
    wa_charge-aedat  = sy-datum.
    wa_charge-aezeit = sy-uzeit.
    wa_charge-aename = sy-uname.
    append wa_charge to i_charge_status.
  endloop.

* Process Extra charge update/cancel records
  loop at i_charge_update into wa_charge.
    perform f_move_fields using    space
                          changing wa_common.

    perform f_process_update_cancel_recs using c_charge_table
                                               wa_common
                                      changing wa_charge-ztran_stus.
    wa_charge-aedat  = sy-datum.
    wa_charge-aezeit = sy-uzeit.
    wa_charge-aename = sy-uname.
    append wa_charge to i_charge_status.
  endloop.

endform.                    " F_PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  F_MOVE_FIELDS
*&---------------------------------------------------------------------*
form f_move_fields  using    p_fuel_flag
                    changing ps_common structure wa_common.

  clear ps_common.

  if p_fuel_flag = c_x.
    move-corresponding wa_fuel to ps_common.
    ps_common-id             = wa_fuel-zdtl_fevt_id.
    ps_common-version        = wa_fuel-zdtl_fevt_ver.
    ps_common-quantity       = wa_fuel-zdlvr_fuel_vol.
    ps_common-uom            = wa_fuel-zdlvr_vol_uom.
  else.
    move-corresponding wa_charge to ps_common.
    ps_common-id       = wa_charge-zdtl_xchg_id.
    ps_common-version  = wa_charge-zdtl_xchg_ver.
    ps_common-quantity = wa_charge-zxchg_qty.
    ps_common-uom      = wa_charge-meins.
  endif.

endform.                    " F_MOVE_FIELDS

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_NEW_RECS
*&---------------------------------------------------------------------*
form f_process_new_recs  using    p_table
                                  p_fuel_flag
                                  ps_common structure wa_common
                         changing p_status.

  data: l_po_num        type ebeln,
        l_po_item       type ebelp,
        l_contract_num  type konnr,
        l_contract_item type ktpnr,
        l_quantity      type bstmg,
        l_uom           type bstme,
        l_subrc         type sy-subrc,
        l_elikz         type elikz,
        lwa_contract_item type t_contract,        "DV5K992637 CR 257405
        lwa_common_tmp  type t_common.

  clear: l_po_num, l_po_item,
         l_contract_num, l_contract_item,
         l_quantity, l_uom, l_subrc, v_rec_count.

  refresh i_return.

* Check record has been processed?
  perform f_check_record_processed using    ps_common
                                            space
                                            space                "I - TXT25668 - C403761 T453881 - DV5K9A0QBF
                                   changing l_po_num
                                            l_po_item
                                            l_elikz
                                            v_rec_count
                                            v_duplicate_po.

  if v_rec_count > 1 or v_duplicate_po is not initial.    "Error
    l_subrc = 4.
*   Generate the message log
    clear wa_return.
    wa_return-type   = c_error.
    wa_return-id     = c_msgid.
    wa_return-number = 18.
    append wa_return to i_return.
    if ( 1 = 2 ). message i018(zz_flcm). endif.

  elseif v_rec_count = 1.    "Create Goods Receipt
    if l_elikz is initial.
      refresh i_goods_recp.
*     Map Goods receipt data
      clear wa_goods_recp.
      wa_goods_recp-item = l_po_item.
      append wa_goods_recp to i_goods_recp.

      perform f_create_goods_receipt tables i_goods_recp
                                     using l_po_num
                                           ps_common-id
                                           ps_common-version
                                           ps_common-zbol_tckt_nbr
                                           ps_common-zdlvr_dt
                                           ps_common-zdtl_loco_nbr
                                           ps_common-zdtl_loco_init
                                  changing l_subrc
                                           i_return.
    endif.
  else.   "Create PO and Goods Receipt
    perform f_contract_search using    ps_common
                                       p_status           "DV5K966805+
                              changing l_contract_num
                                       l_contract_item
                                       l_quantity
                                       l_uom
                                       l_subrc
                                       lwa_contract_item    "DV5K992637 CR 257405
                                       i_return.
    if l_subrc is initial.
      l_po_item = c_10.

***BEGIN OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO
      PERFORM f_validate_po_creation  USING    ps_common
                                               l_contract_num
                                               l_contract_item
                                               l_quantity
                                               l_uom
                                               lwa_contract_item
                                               l_po_num
                                      CHANGING i_return
                                               p_status
                                               l_subrc.
      IF l_subrc IS INITIAL.
***END OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO
        perform f_create_po using l_po_item
                                  l_quantity
                                  l_uom
                                  l_contract_num
                                  l_contract_item
                                  ps_common
                                  lwa_contract_item           "DV5K992637 CR 257405
                            changing l_po_num
                                     l_subrc
                                     i_return.
      ENDIF. "I - XT18912 - CR269546 TK334189 - DV5K9A03CO
    endif.
  endif.

* Update message log
  perform f_update_log using p_table
                               ps_common-id
                               ps_common-version
                               i_return.

  if l_subrc is not initial.
*   Set fuel record status to '04' (Error)
    p_status = c_04.
    if p_fuel_flag = c_x.
*--> Begin of insert DV5K966672 - Chuong Le   Defect 559  2011/09/23
*     Generate the message log
      refresh i_return.
      clear wa_return.
      wa_return-type   = c_error.
      wa_return-id     = c_msgid.
      wa_return-number = 93.
      append wa_return to i_return.
      if ( 1 = 2 ). message i093(zz_flcm). endif.
*<-- End of insert DV5K966672 - Chuong Le    Defect 559  2011/09/23
*     Also set all relevant extra charges to '04' (Error)
      loop at i_charge_new into wa_charge
                    where lifnr         = ps_common-lifnr
                      and zbol_tckt_nbr = ps_common-zbol_tckt_nbr
                      and werks         = ps_common-werks.
        wa_charge-ztran_stus = c_04.
        wa_charge-aedat  = sy-datum.
        wa_charge-aezeit = sy-uzeit.
        wa_charge-aename = sy-uname.
        append wa_charge to i_charge_status.
        delete i_charge_new.
*--> Begin of insert DV5K966672 - Chuong Le    Defect 559  2011/09/23
*       Update message log
        perform f_update_log using c_charge_table
                                   wa_charge-zdtl_xchg_id
                                   wa_charge-zdtl_xchg_ver
                                   i_return.
*<-- End of insert DV5K966672 - Chuong Le    Defect 559  2011/09/23
      endloop.
    endif.
  else.
*   Set fuel record status to '03' (Success)
    p_status = c_03.
  endif.

endform.                    " F_PROCESS_NEW_RECS

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_EXT_CHARGE_WITH_FUEL
*&---------------------------------------------------------------------*
form f_process_ext_charge_with_fuel
                       using  p_table
                              ps_common structure wa_common
                    changing  p_status.

  types: begin of t_ekpo,
          ebelp type ebelp,
          bednr type bednr,
         end of t_ekpo.

  data: li_ekpo  type table of t_ekpo,
        lwa_ekpo type t_ekpo.

  data: l_po_num        type ebeln,
        l_po_item       type ebelp,
        l_contract_num  type konnr,
        l_contract_item type ktpnr,
        l_quantity      type bstmg,
        l_uom           type bstme,
        l_subrc         type sy-subrc,
        l_line_count    type i,
        lwa_contract_item type t_contract,        "DV5K992637 CR 257405
        l_elikz         type elikz.

  clear: l_po_num, l_po_item,
         l_contract_num, l_contract_item,
         l_quantity, l_uom, l_subrc, v_rec_count.

  refresh i_return.

* Check record has been processed?
  perform f_check_record_processed using    ps_common
                                            space
                                            space                "I - TXT25668 - C403761 T453881 - DV5K9A0QBF
                                   changing l_po_num
                                            l_po_item
                                            l_elikz
                                            v_rec_count
                                            v_duplicate_po.

  if v_rec_count > 1.    "Error
    l_subrc = 4.
*   Generate the message log
    clear wa_return.
    wa_return-type   = c_error.
    wa_return-id     = c_msgid.
    wa_return-number = 18.
    append wa_return to i_return.
    if ( 1 = 2 ). message i018(zz_flcm). endif.

  elseif v_rec_count = 1.    "Create Goods Receipt
    if l_elikz is initial.
      refresh i_goods_recp.
*     Map Goods receipt data
      clear wa_goods_recp.
      wa_goods_recp-item = l_po_item.
      append wa_goods_recp to i_goods_recp.

      perform f_create_goods_receipt tables i_goods_recp
                                     using l_po_num
                                           ps_common-id
                                           ps_common-version
                                           ps_common-zbol_tckt_nbr
                                           ps_common-zdlvr_dt
                                           ps_common-zdtl_loco_nbr
                                           ps_common-zdtl_loco_init
                                  changing l_subrc
                                           i_return.
    endif.

  else.   "Modify existing PO
*   Search for fuelling event line item
    perform f_search_po_line_item using ps_common
                                        c_fuel1010
                                        space
                               changing l_po_num
                                        l_po_item
                                        v_rec_count.

    if v_rec_count > 1.   "Error
      l_subrc = 4.
*     Generate the message log
      clear wa_return.
      wa_return-type   = c_error.
      wa_return-id     = c_msgid.
      wa_return-number = 20.
      append wa_return to i_return.
      if ( 1 = 2 ). message i020(zz_flcm). endif.

    elseif v_rec_count = 1.   "Add extra charge to the same PO
      perform f_contract_search using    ps_common
                                         p_status         "DV5K966805+
                                changing l_contract_num
                                         l_contract_item
                                         l_quantity
                                         l_uom
                                         l_subrc
                                         lwa_contract_item       "DV5K992637 CR 257405
                                         i_return.
      if l_subrc is initial.
*       Get PO items
        refresh li_ekpo.
        select ebelp bednr into table li_ekpo
        from ekpo
        where ebeln = l_po_num.

        if sy-subrc = 0.
          sort li_ekpo by ebelp descending.
*         Generate new PO item number
          clear lwa_ekpo.
          read table li_ekpo into lwa_ekpo index 1.
          l_po_item = lwa_ekpo-ebelp + c_10.
        else.
          l_po_item = c_10.
        endif.

        perform f_add_items using l_po_num
                                  l_po_item
                                  l_quantity
                                  l_uom
                                  l_contract_num
                                  l_contract_item
                                  ps_common
                                  lwa_contract_item       "DV5K993102
                         changing l_subrc
                                  i_return.
      endif.
    else.     "Error
      l_subrc = 4.
*     Generate the message log
      clear wa_return.
      wa_return-type   = c_error.
      wa_return-id     = c_msgid.
      wa_return-number = 19.
      append wa_return to i_return.
      if ( 1 = 2 ). message i019(zz_flcm). endif.
    endif.
  endif.

  if l_subrc is not initial.
*   Set fuel record status to '04' (Error)
    p_status = c_04.
  else.
*   Set fuel record status to '03' (Success)
    p_status = c_03.
  endif.

* Update message log
  perform f_update_log using p_table
                             ps_common-id
                             ps_common-version
                             i_return.

endform.                    " F_PROCESS_EXT_CHARGE_WITH_FUEL

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_UPDATE_CANCEL_RECS
*&---------------------------------------------------------------------*
form f_process_update_cancel_recs using p_table
                                        ps_common structure wa_common
                               changing p_status.

  data: l_po_num        type ebeln,
        l_po_item       type ebelp,
        l_contract_num  type konnr,
        l_contract_item type ktpnr,
        l_quantity      type bstmg,
        l_uom           type bstme,
        l_subrc         type sy-subrc,
        l_po_save       type ebeln,
        l_elikz         type elikz,
        l_return_po     type xfeld,
        lwa_contract_item type t_contract,        "DV5K992637 CR 257405
        l_do_update     type char01,              "DV5K967793
        lv_upd          TYPE abap_bool.                          "I - TXT25668 - C403761 T456863 - DV5K9A0R6Z

  clear: wa_po_list, l_po_num, l_po_item, l_subrc, l_po_save.

  refresh: i_return.

  if ps_common-zcanc_ind = c_y.    "Cancel
    l_return_po = c_x.
    CLEAR lv_upd.                                                "I - TXT25668 - C403761 T453881 - DV5K9A0QBF
  else.                            "Update
    clear l_return_po.
    lv_upd = c_x.                                                "I - TXT25668 - C403761 T453881 - DV5K9A0QBF
  endif.

* Check record has been processed?
  perform f_check_record_processed using    ps_common
                                            l_return_po
                                            lv_upd               "I - TXT25668 - C403761 T453881 - DV5K9A0QBF
                                   changing l_po_num
                                            l_po_item
                                            l_elikz
                                            v_rec_count
                                            v_duplicate_po.

  if v_rec_count > 1.    "Error
    l_subrc = 4.
*   Generate the message log
    clear wa_return.
    wa_return-type   = c_error.
    wa_return-id     = c_msgid.
    wa_return-number = 18.
    append wa_return to i_return.
    if ( 1 = 2 ). message i018(zz_flcm). endif.

  elseif v_rec_count = 1.    "Create Goods Receipt
    if l_elikz is initial.
      refresh i_goods_recp.
*     Map Goods receipt data
      clear wa_goods_recp.
      wa_goods_recp-item = l_po_item.
      append wa_goods_recp to i_goods_recp.

      perform f_create_goods_receipt tables i_goods_recp
                                     using l_po_num
                                           ps_common-id
                                           ps_common-version
                                           ps_common-zbol_tckt_nbr
                                           ps_common-zdlvr_dt
                                           ps_common-zdtl_loco_nbr
                                           ps_common-zdtl_loco_init
                                  changing l_subrc
                                           i_return.
    endif.
  else.   "Perform Update or Cancel
*   Search for original PO item to copy from
    perform f_search_po_line_item using ps_common
                                        ps_common-matkl
                                        c_x
                               changing l_po_num
                                        l_po_item
                                        v_rec_count.
    if v_rec_count <> 1.    "Error
      if ps_common-zcanc_ind = c_n.                         "DV5K968544
        perform f_check_prior_version using ps_common-id      "DV5K967793
                                            ps_common-version "DV5K967793
                                   changing l_do_update.      "DV5K967793
      endif.                                                "DV5K968544
     if l_do_update is initial.                             "DV5K967793
      l_subrc = 4.
*     Generate the message log
      clear wa_return.
      wa_return-type   = c_error.
      wa_return-id     = c_msgid.
      wa_return-number = 25.
      append wa_return to i_return.
      if ( 1 = 2 ). message i025(zz_flcm). endif.
     endif.                                                "DV5K967793
    else.
      l_do_update = 'U'.                                   "DV5K967793
    endif.                                                 "DV5K967793

    if l_do_update is not initial.                         "DV5K967793
***   Update record
      if ps_common-zcanc_ind = c_n.
        perform f_contract_search using    ps_common
                                           p_status       "DV5K966805+
                                  changing l_contract_num
                                           l_contract_item
                                           l_quantity
                                           l_uom
                                           l_subrc
                                           lwa_contract_item           "DV5K992637 CR 257405
                                           i_return.

        if l_subrc is initial.
         if l_do_update = 'I'.     "Insert new PO          "DV5K967793
          l_po_item = c_10.                                "DV5K967793
          perform f_create_po using    l_po_item           "DV5K967793
                                       l_quantity          "DV5K967793
                                       l_uom               "DV5K967793
                                       l_contract_num      "DV5K967793
                                       l_contract_item     "DV5K967793
                                       ps_common           "DV5K967793
                                       lwa_contract_item           "DV5K992637 CR 257405
                              changing l_po_num            "DV5K967793
                                       l_subrc             "DV5K967793
                                       i_return.           "DV5K967793
         else.                                             "DV5K967793
          perform f_update_po using    l_po_num
                                       l_po_item
                                       l_quantity
                                       l_uom
                                       l_contract_num
                                       l_contract_item
                                       ps_common
                                       lwa_contract_item   "DV5K993102
                              changing l_subrc
                                       i_return.
         endif.
        endif.                                             "DV5K967793
***   Cancel fuel record
      elseif ps_common-zcanc_ind = c_y.
        perform f_cancel_po using    l_po_num
                                     l_po_item
                                     ps_common
                            changing l_subrc
                                     i_return.

      endif.
    endif.
  endif.

  if l_subrc is not initial.
*   Set fuel record status to '04' (Error)
    p_status = c_04.
  else.
*   Set fuel record status to '03' (Success)
    p_status = c_03.
  endif.

* Update message log
  perform f_update_log using p_table
                             ps_common-id
                             ps_common-version
                             i_return.

endform.                    " F_PROCESS_UPDATE_CANCEL_RECS

*** Start of changes defect 659                               DV5K967793
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_PRIOR_VERSION
*&---------------------------------------------------------------------*
form f_check_prior_version  using p_fevt_id   type zzdtl_fuel_event_id
                                  p_fevt_ver  type zzdtl_fuel_event_ver
                         changing p_do_update type char01.

  data: l_fevt_ver           type zmm_dtl_fuel_evt-zdtl_fevt_ver,
        l_audt_stus          type zmm_dtl_fuel_evt-zaudt_stus,
        l_tran_stus          type zmm_dtl_fuel_evt-ztran_stus,
        li_msg_id            type standard table of zmm_dtl_fuel_adt-zmsg_id.

  clear p_do_update.

  if p_fevt_ver < 2.
    return.
  endif.

  l_fevt_ver = p_fevt_ver - 1.

  select single zaudt_stus
                ztran_stus
    from zmm_dtl_fuel_evt
    into (l_audt_stus,
          l_tran_stus)
    where zdtl_fevt_id = p_fevt_id and
          zdtl_fevt_ver = l_fevt_ver.
  if sy-subrc = 0 and l_audt_stus = 'F' and l_tran_stus = '00'.
    select zmsg_id
      from zmm_dtl_fuel_adt
      into table li_msg_id
      where zdtl_fevt_id = p_fevt_id and
            zdtl_fevt_ver = l_fevt_ver.
    loop at li_msg_id transporting no fields
        where table_line = '0001' or table_line = '0012'.
      p_do_update = 'I'.
      exit.
    endloop.
  endif.

endform.
*** End of changes defect 659                                 DV5K967793
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_RECORD_PROCESSED
*&---------------------------------------------------------------------*
form f_check_record_processed  using    ps_common structure wa_common
                                        p_return_flag
                                        uv_upd                "I - TXT25668 - C403761 T453881 - DV5K9A0QBF
                               changing p_po_num
                                        p_po_item
                                        p_elikz
                                        p_rec_count
                                        p_duplicate_po.

  clear: p_po_num, p_po_item, p_elikz, p_rec_count, p_duplicate_po.

*** Start of Insert TXT25668 - C403761 T453881 - DV5K9A0QBF
  IF uv_upd IS NOT INITIAL.
*    READ TABLE lt_po_list_upd INTO wa_po_list                "D - TXT25668 - C403761 T456863 - DV5K9A0R6Z
    READ TABLE gt_po_list_upd INTO wa_po_list                 "I - TXT25668 - C403761 T456863 - DV5K9A0R6Z
                              WITH KEY lifnr = ps_common-lifnr
                                       ihrez = ps_common-zbol_tckt_nbr
                                       matkl = ps_common-matkl
                                       werks = ps_common-werks
                                       afnam = ps_common-id.
  ELSE.
*** End of Insert TXT25668 - C403761 T453881 - DV5K9A0QBF
    read table i_po_list into wa_po_list
                         with key lifnr = ps_common-lifnr
                                  ihrez = ps_common-zbol_tckt_nbr
                                  matkl = ps_common-matkl
                                  werks = ps_common-werks
                                  eindt = ps_common-zdlvr_dt     "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
                                  binary search transporting no fields.
  ENDIF.                                                         "I - TXT25668 - C403761 T453881 - DV5K9A0QBF

  if sy-subrc = 0.
    v_index   = sy-tabix.
    v_id      = ps_common-id.
    v_version = ps_common-version+3(2).
    loop at i_po_list into wa_po_list from v_index.
      IF uv_upd IS NOT INITIAL.                                 "I - TXT25668 - C403761 T456863 - DV5K9A0R6Z
        if wa_po_list-lifnr <> ps_common-lifnr         or
         wa_po_list-ihrez <> ps_common-zbol_tckt_nbr or
         wa_po_list-matkl <> ps_common-matkl         or
         wa_po_list-werks <> ps_common-werks         OR
*           wa_po_list-eindt <> ps_common-zdlvr_dt.               "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD  "D - TXT25668 - C403761 T453881 - DV5K9A0QBF
         wa_po_list-afnam(10) <> ps_common-id.                  "I - TXT25668 - C403761 T453881 - DV5K9A0QBF
        exit.
      else.
        if wa_po_list-retpo     = p_return_flag and
           wa_po_list-afnam(10) = v_id          and
           wa_po_list-afnam+10  = v_version.
          add 1 to p_rec_count.
          p_po_num     = wa_po_list-ebeln.
          p_po_item    = wa_po_list-ebelp.
          p_elikz      = wa_po_list-elikz.
        endif.
*         Check duplicate PO for different fuel id or version
        if wa_po_list-afnam(10) <> v_id or
           wa_po_list-afnam+10  <> v_version.
          p_duplicate_po = c_x.
        endif.
      endif.
***Start of Insert TXT25668 - C403761 T456863 - DV5K9A0R6Z
      ELSE.
        IF wa_po_list-lifnr NE ps_common-lifnr         OR
           wa_po_list-ihrez NE ps_common-zbol_tckt_nbr OR
           wa_po_list-matkl NE ps_common-matkl         OR
           wa_po_list-werks NE ps_common-werks         OR
           wa_po_list-eindt NE ps_common-zdlvr_dt.
          EXIT.
        ELSE.
          IF wa_po_list-retpo     EQ p_return_flag AND
             wa_po_list-afnam(10) EQ v_id          AND
             wa_po_list-afnam+10  EQ v_version.
            ADD 1 TO p_rec_count.
            p_po_num     = wa_po_list-ebeln.
            p_po_item    = wa_po_list-ebelp.
            p_elikz      = wa_po_list-elikz.
          ENDIF.
*         Check duplicate PO for different fuel id or version
          IF wa_po_list-afnam(10) NE v_id OR
             wa_po_list-afnam+10  NE v_version.
            p_duplicate_po = c_x.
          ENDIF.
        ENDIF.
      ENDIF.
***End of Insert TXT25668 - C403761 T456863 - DV5K9A0R6Z
    endloop.
  endif.

endform.                    " F_CHECK_RECORD_PROCESSED

*&---------------------------------------------------------------------*
*&      Form  F_SEARCH_PO_LINE_ITEM
*&---------------------------------------------------------------------*
form f_search_po_line_item  using    ps_common structure wa_common
                                     p_matkl
                                     p_update_flag
                            changing p_po_num
                                     p_po_item
                                     p_rec_count.

  data: l_common_id type char10.

  clear: p_po_num, p_po_item, p_rec_count.

* Search for existing PO line item
*** Start of Insert TXT25668 - C403761 T453881 - DV5K9A0QBF
  IF p_update_flag IS NOT INITIAL.
*    READ TABLE lt_po_list_upd INTO wa_po_list                "D - TXT25668 - C403761 T456863 - DV5K9A0R6Z
    READ TABLE gt_po_list_upd INTO wa_po_list                 "I - TXT25668 - C403761 T456863 - DV5K9A0R6Z
                              WITH KEY lifnr = ps_common-lifnr
                                       ihrez = ps_common-zbol_tckt_nbr
                                       matkl = ps_common-matkl
                                       werks = ps_common-werks
                                       afnam = ps_common-id.
  ELSE.
*** End of Insert TXT25668 - C403761 T453881 - DV5K9A0QBF
    read table i_po_list into wa_po_list
                         with key lifnr = ps_common-lifnr
                                  ihrez = ps_common-zbol_tckt_nbr
                                  matkl = p_matkl
                                  werks = ps_common-werks
                                  eindt = ps_common-zdlvr_dt     "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
                                  binary search.
  ENDIF.                                                         "I - TXT25668 - C403761 T453881 - DV5K9A0QBF

  if sy-subrc = 0.
    v_index = sy-tabix.
    loop at i_po_list into wa_po_list from v_index.
      IF p_update_flag IS NOT INITIAL.                          "I - TXT25668 - C403761 T456863 - DV5K9A0R6Z
        if wa_po_list-lifnr <> ps_common-lifnr         or
         wa_po_list-ihrez <> ps_common-zbol_tckt_nbr or
         wa_po_list-matkl <> p_matkl                 or
         wa_po_list-werks <> ps_common-werks         OR
*           wa_po_list-eindt <> ps_common-zdlvr_dt.               "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD  "D - TXT25668 - C403761 T453881 - DV5K9A0QBF
         wa_po_list-afnam(10) <> ps_common-id.                  "I - TXT25668 - C403761 T453881 - DV5K9A0QBF
        exit.
      endif.

*       Check for update/cancel process
      if p_update_flag = c_x.
        l_common_id = ps_common-id.
        if l_common_id <> wa_po_list-afnam(10).
          continue.
        endif.
      endif.

      if wa_po_list-retpo = space.
        if sy-tabix = v_index.
          add 1 to p_rec_count.
          p_po_num  = wa_po_list-ebeln.
          p_po_item = wa_po_list-ebelp.
        else.
          if wa_po_list-ebeln <> p_po_num.    "Different PO
            add 1 to p_rec_count.
            p_po_num  = wa_po_list-ebeln.
            p_po_item = wa_po_list-ebelp.
          else.
            p_po_item = wa_po_list-ebelp.     "highest line item
          endif.
        endif.
      endif.
***Start of Insert TXT25668 - C403761 T456863 - DV5K9A0R6Z
      ELSE.
        IF wa_po_list-lifnr NE ps_common-lifnr         OR
           wa_po_list-ihrez NE ps_common-zbol_tckt_nbr OR
           wa_po_list-matkl NE p_matkl                 OR
           wa_po_list-werks NE ps_common-werks         OR
           wa_po_list-eindt NE ps_common-zdlvr_dt.
          EXIT.
        ENDIF.

*       Check for update/cancel process
        IF p_update_flag EQ c_x.
          l_common_id = ps_common-id.
          IF l_common_id <> wa_po_list-afnam(10).
            CONTINUE.
          ENDIF.
        ENDIF.

        IF wa_po_list-retpo = space.
          IF sy-tabix = v_index.
            ADD 1 TO p_rec_count.
            p_po_num  = wa_po_list-ebeln.
            p_po_item = wa_po_list-ebelp.
          ELSE.
            IF wa_po_list-ebeln NE p_po_num.    "Different PO
              ADD 1 TO p_rec_count.
              p_po_num  = wa_po_list-ebeln.
              p_po_item = wa_po_list-ebelp.
            ELSE.
              p_po_item = wa_po_list-ebelp.     "highest line item
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
***End of Insert TXT25668 - C403761 T456863 - DV5K9A0R6Z
    endloop.
  endif.

endform.                    " F_SEARCH_PO_LINE_ITEM

*&---------------------------------------------------------------------*
*&      Form  F_CONTRACT_SEARCH
*&---------------------------------------------------------------------*
form f_contract_search  using    ps_common structure wa_common
                                 p_status                 "DV5K966805+
                        changing p_contract_num
                                 p_contract_item
                                 p_qty_out
                                 p_uom_out
                                 p_subrc
                                 lwa_contract_item           "DV5K992637 CR 257405
                                 pi_return type bapiret2_tab.

  data: lwa_contract type t_contract,
        lwa_temp     type t_contract,
        l_quantity   type bstmg,
        l_counter    type i.

  clear: p_contract_num, p_contract_item,
         p_qty_out, p_uom_out, p_subrc,
         lwa_contract_item,        "DV5K992637 CR 257405
         lwa_contract, lwa_temp.

  read table i_contract_list into lwa_contract
                             with key lifnr = ps_common-lifnr
                                      matkl = ps_common-matkl
                                      werks = ps_common-werks
                             binary search transporting no fields.
  if sy-subrc <> 0.
    p_subrc = 4.
    if p_status <> c_04.                                  "DV5K966805+
*     Generate the message log
      clear wa_return.
      wa_return-type   = c_error.
      wa_return-id     = c_msgid.
      wa_return-number = 17.
      append wa_return to pi_return.
      if ( 1 = 2 ). message i017(zz_flcm). endif.
    endif.                                                "DV5K966805+

  else.
    clear l_counter.
    v_index = sy-tabix.
    loop at i_contract_list into lwa_contract from v_index.
      if lwa_contract-lifnr <> ps_common-lifnr or
         lwa_contract-matkl <> ps_common-matkl or
         lwa_contract-werks <> ps_common-werks.
        exit.
      endif.

      check ps_common-zdlvr_dt >= lwa_contract-kdatb and  "DV5K966805+
            ps_common-zdlvr_dt <= lwa_contract-kdate.     "DV5K966805+

      if ps_common-zdlvr_dt >= lwa_contract-idnlf+5(8) or
         lwa_contract-xersy = c_x.
        add 1 to l_counter.
        lwa_temp = lwa_contract.
      endif.
    endloop.

    if l_counter > 1.
      p_subrc = 4.
      if p_status <> c_04.                                "DV5K966805+
*       Generate the message log
        clear wa_return.
        wa_return-type   = c_error.
        wa_return-id     = c_msgid.
        wa_return-number = 51.
        append wa_return to pi_return.
        if ( 1 = 2 ). message i051(zz_flcm). endif.
      endif.                                              "DV5K966805+

    elseif l_counter = 1.
      lwa_contract_item = lwa_temp.          "DV5K992637 CR 257405
      p_contract_num  = lwa_temp-ebeln.
      p_contract_item = lwa_temp-ebelp.
      if ps_common-uom <> lwa_temp-meins.
        p_uom_out = lwa_temp-meins. "I - XT18912 - CR269546 TK334189 - DV5K9A03CO
        if lwa_temp-matnr is not initial.      "material exists
          l_quantity = ps_common-quantity.
          call function 'MD_CONVERT_MATERIAL_UNIT'
            exporting
              i_matnr                    = lwa_temp-matnr
              i_in_me                    = ps_common-uom
              i_out_me                   = lwa_temp-meins
              i_menge                    = l_quantity
            importing
              e_menge                    = p_qty_out
            exceptions
              error_in_application       = 1
              error                      = 2
              others                     = 3.

        else.                                  "no material
          call function 'UNIT_CONVERSION_SIMPLE'
            exporting
              input                = ps_common-quantity
              unit_in              = ps_common-uom
              unit_out             = lwa_temp-meins
            importing
              output               = p_qty_out
            exceptions
              conversion_not_found = 1
              division_by_zero     = 2
              input_invalid        = 3
              output_invalid       = 4
              overflow             = 5
              type_invalid         = 6
              units_missing        = 7
              unit_in_not_found    = 8
              unit_out_not_found   = 9
              others               = 10.
        endif.
      else.
        p_qty_out = ps_common-quantity.
        p_uom_out = ps_common-uom.
      endif.

    else.
      p_subrc = 4.
      if p_status <> c_04.                                "DV5K966805+
*       Generate the message log
        clear wa_return.
        wa_return-type   = c_error.
        wa_return-id     = c_msgid.
        wa_return-number = 17.
        append wa_return to pi_return.
        if ( 1 = 2 ). message i017(zz_flcm). endif.
      endif.                                              "DV5K966805+
    endif.
  endif.

endform.                    " F_CONTRACT_SEARCH

*&---------------------------------------------------------------------*
*&      Form  F_CREATE_PO
*&---------------------------------------------------------------------*
form f_create_po using p_item_num
                       p_quantity
                       p_meins
                       p_contract_num
                       p_contract_item
                       ps_common structure wa_common
                       ps_contract_item type t_contract      "DV5K992637 CR 257405
              changing p_po_num
                       p_subrc
                       pi_return type bapiret2_tab.

  data: li_bapireturn type bapiret2_tab,
        l_version     type char05.

  clear: wa_poheader, wa_poheaderx,
         wa_poitem, wa_poitemx,
         wa_poschedule, wa_poschedulex,
         p_po_num, p_subrc.

  refresh: i_poitem, i_poitemx,
           i_poaccount, i_poaccountx,          "DV5K992637 CR 257405
           i_poschedule, i_poschedulex,
           i_goods_recp, li_bapireturn.

* Map PO header data
  wa_poheader-doc_type = c_fb.
  wa_poheader-doc_date = ps_common-zdlvr_dt.
  wa_poheader-ref_1    = ps_common-zbol_tckt_nbr.
  wa_poheader-vendor   = ps_common-lifnr.
*  wa_poheader-our_ref  = c_dtl.                           "DV5K9A05WW-

  wa_poheaderx-doc_type = c_x.
  wa_poheaderx-doc_date = c_x.
  wa_poheaderx-ref_1    = c_x.
  wa_poheaderx-vendor   = c_x.
*  wa_poheaderx-our_ref  = c_x.                            "DV5K9A05WW-

* Map PO item data
  wa_poitem-po_item   = p_item_num.
  wa_poitem-quantity  = p_quantity.
  wa_poitem-po_unit   = p_meins.
  wa_poitem-agreement = p_contract_num.
  wa_poitem-agmt_item = p_contract_item.
  wa_poitem-no_rounding = c_x.
  l_version = ps_common-version.
  concatenate ps_common-id
              l_version+3(2)
         into wa_poitem-preq_name.
  append wa_poitem to i_poitem.

  wa_poitemx-po_item    = p_item_num.
  wa_poitemx-quantity   = c_x.
  wa_poitemx-po_unit    = c_x.
  wa_poitemx-agreement  = c_x.
  wa_poitemx-agmt_item  = c_x.
  wa_poitemx-no_rounding = c_x.
  wa_poitemx-preq_name  = c_x.
  append wa_poitemx to i_poitemx.

* Map PO schedule line data
  wa_poschedule-po_item       = p_item_num.
  wa_poschedule-sched_line    = 1.
  wa_poschedule-delivery_date = ps_common-zdlvr_dt.
  wa_poschedule-quantity      = p_quantity.
  append wa_poschedule to i_poschedule.

  wa_poschedulex-po_item       = p_item_num.
  wa_poschedulex-sched_line    = 1.
  wa_poschedulex-po_itemx      = c_x.
  wa_poschedulex-sched_linex   = c_x.
  wa_poschedulex-delivery_date = c_x.
  wa_poschedulex-quantity      = c_x.
  append wa_poschedulex to i_poschedulex.

***** start of code added DV5K992637 CR 257405
* If the equipment, which was fueled by vendor, is not a locomotive, then cost center and
* G/L should not come from the contract.
  perform f_misc_equi_account using p_po_num p_item_num ps_common ps_contract_item changing i_poaccount i_poaccountx pi_return p_subrc. "DV5K993102
  check ( p_subrc = 0 ).
***** end   of code added DV5K992637 CR 257405

*--> Start of Insert DV5K9A05WW - Chuong Le - 2017/01/04
  DATA(lt_potextheader) = VALUE bapimepotextheader_tp( ( text_id   = 'F99'
                                                         text_form = '* '
                                                         text_line = c_dtl ) ).
*<-- End of Insert DV5K9A05WW - Chuong Le - 2017/01/04

  call function 'BAPI_PO_CREATE1'
    exporting
      poheader         = wa_poheader
      poheaderx        = wa_poheaderx
    importing
      exppurchaseorder = p_po_num
    tables
      return           = li_bapireturn
      poitem           = i_poitem
      poitemx          = i_poitemx
      poaccount        = i_poaccount         "DV5K992637 CR 257405
      poaccountx       = i_poaccountx        "DV5K992637 CR 257405
      poschedule       = i_poschedule
      poschedulex      = i_poschedulex
      potextheader     = lt_potextheader.                  "DV5K9A05WW+

  append lines of li_bapireturn to pi_return.

  if p_po_num is not initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.

*   Map Goods receipt data
    clear wa_goods_recp.
    wa_goods_recp-item    = p_item_num.
    append wa_goods_recp to i_goods_recp.

    perform f_create_goods_receipt tables i_goods_recp
                                   using p_po_num
                                         ps_common-id
                                         ps_common-version
                                         ps_common-zbol_tckt_nbr
                                         ps_common-zdlvr_dt
                                         ps_common-zdtl_loco_nbr
                                         ps_common-zdtl_loco_init
                                changing p_subrc
                                         pi_return.

    perform f_add_to_po_list tables i_goods_recp
                             using  p_po_num
                                    ps_common-lifnr
                                    ps_common-zbol_tckt_nbr
                                    ps_common-zdlvr_dt.      "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
  else.
    p_subrc = 4.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
  endif.

endform.                    " F_CREATE_PO

*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_PO
*&---------------------------------------------------------------------*
form f_update_po  using    p_po_num
                           p_item_original
                           p_quantity
                           p_meins
                           p_contract_num
                           p_contract_item
                           ps_common structure wa_common
                           ps_contract_item type t_contract      "DV5K993052
                  changing p_subrc
                           pi_return type bapiret2_tab.

  data: l_item_new   type ebelp,
        l_version    type char05,
        l_row_cnt    type i.

  clear: wa_po_header, p_subrc, l_item_new.

* Get PO details
  perform f_get_po_detail tables   i_po_item
                                   i_po_schedule
                          using    p_po_num
                          changing wa_po_header
                                   p_subrc.

  if p_subrc is not initial.
    exit.
  endif.

  clear: wa_poitem, wa_poitemx,
         wa_poschedule, wa_poschedulex, l_item_new.

  refresh: i_poitem, i_poitemx,
           i_poaccount, i_poaccountx,          "DV5K993052
           i_poschedule, i_poschedulex, i_goods_recp.

  sort: i_po_item     by po_item,
        i_po_schedule by po_item.

* Get the latest item number
  describe table i_po_item lines l_row_cnt.
  read table i_po_item into wa_po_item
                       index l_row_cnt transporting po_item.
  if sy-subrc = 0.
    l_item_new = wa_po_item-po_item + c_10.
  endif.

* Return item data
  clear wa_poitem.
  wa_poitem-po_item   = l_item_new.
  wa_poitem-ref_doc   = p_po_num.
  wa_poitem-ref_item  = p_item_original.  "Reference to original item
  wa_poitem-ret_item  = c_x.
  wa_poitem-calctype  = 'C'.
  wa_poitem-no_rounding = c_x.
  l_version = ps_common-version.
  concatenate ps_common-id
              l_version+3(2)
         into wa_poitem-preq_name.
  append wa_poitem to i_poitem.

  clear wa_poitemx.
  wa_poitemx-po_item   = l_item_new.
  wa_poitemx-ref_doc   = c_x.
  wa_poitemx-ref_item  = c_x.
  wa_poitemx-ret_item  = c_x.
  wa_poitemx-calctype  = c_x.
  wa_poitemx-no_rounding = c_x.
  wa_poitemx-preq_name = c_x.
  append wa_poitemx to i_poitemx.

* Map Goods receipt data
  clear wa_goods_recp.
  wa_goods_recp-item    = l_item_new.
  append wa_goods_recp to i_goods_recp.

* New item data
  add c_10 to l_item_new.
  clear wa_poitem.
  wa_poitem-po_item   = l_item_new.
  wa_poitem-quantity  = p_quantity.
  wa_poitem-po_unit   = p_meins.
  wa_poitem-agreement = p_contract_num.
  wa_poitem-agmt_item = p_contract_item.
  wa_poitem-no_rounding = c_x.
  l_version = ps_common-version.
  concatenate ps_common-id
              l_version+3(2)
         into wa_poitem-preq_name.
  append wa_poitem to i_poitem.

  clear wa_poitemx.
  wa_poitemx-po_item    = l_item_new.
  wa_poitemx-quantity   = c_x.
  wa_poitemx-po_unit    = c_x.
  wa_poitemx-agreement  = c_x.
  wa_poitemx-agmt_item  = c_x.
  wa_poitemx-no_rounding = c_x.
  wa_poitemx-preq_name  = c_x.
  append wa_poitemx to i_poitemx.

* Map schedule line data for the new item
  clear wa_poschedule.
  wa_poschedule-po_item       = l_item_new.
  wa_poschedule-sched_line    = 1.
  wa_poschedule-delivery_date = ps_common-zdlvr_dt.
  wa_poschedule-quantity      = p_quantity.
  append wa_poschedule to i_poschedule.

  clear wa_poschedulex.
  wa_poschedulex-po_item       = l_item_new.
  wa_poschedulex-sched_line    = 1.
  wa_poschedulex-po_itemx      = c_x.
  wa_poschedulex-sched_linex   = c_x.
  wa_poschedulex-delivery_date = c_x.
  wa_poschedulex-quantity      = c_x.
  append wa_poschedulex to i_poschedulex.

* Map Goods receipt data
  clear wa_goods_recp.
  wa_goods_recp-item    = l_item_new.
  append wa_goods_recp to i_goods_recp.

***** start of code added DV5K993102
* If the equipment, which was fueled by vendor, is not a locomotive, then cost center and
* G/L should not come from the contract.
  perform f_misc_equi_account using p_po_num l_item_new ps_common ps_contract_item changing i_poaccount i_poaccountx pi_return p_subrc.
  check ( p_subrc = 0 ).
***** end   of code added DV5K993102

  perform f_call_bapi_po_change tables   i_poitem
                                         i_poitemx
                                         i_poschedule
                                         i_poschedulex
                                         i_poaccount       "DV5K993102
                                         i_poaccountx      "DV5K993102
                                using    p_po_num
                                changing p_subrc
                                         pi_return.

  if p_subrc is initial.
    perform f_create_goods_receipt tables i_goods_recp
                                   using  p_po_num
                                          ps_common-id
                                          ps_common-version
                                          wa_po_header-ref_1
                                          ps_common-zdlvr_dt
                                          ps_common-zdtl_loco_nbr
                                          ps_common-zdtl_loco_init
                                 changing p_subrc
                                          pi_return.

    perform f_add_to_po_list tables i_goods_recp
                             using  p_po_num
                                    wa_po_header-vendor
                                    wa_po_header-ref_1
                                    ps_common-zdlvr_dt.      "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
  endif.

endform.                    " F_UPDATE_PO

*&---------------------------------------------------------------------*
*&      Form  F_CANCEL_PO
*&---------------------------------------------------------------------*
form f_cancel_po  using    p_po_num
                           p_item_original
                           ps_common structure wa_common
                  changing p_subrc
                           pi_return type bapiret2_tab.

  data: l_item_new type ebelp,
        l_version  type char05.

  clear: wa_po_header, p_subrc.

* Get PO details
  perform f_get_po_detail tables   i_po_item
                                   i_po_schedule
                          using    p_po_num
                          changing wa_po_header
                                   p_subrc.

  if p_subrc is not initial.
    exit.
  endif.

  clear: wa_poitem, wa_poitemx,
         wa_poschedule, wa_poschedulex, l_item_new.

  refresh: i_poitem, i_poitemx,
           i_poaccount, i_poaccountx,                       "DV5K993102
           i_poschedule, i_poschedulex, i_goods_recp.

  sort: i_po_item     by po_item descending,
        i_po_schedule by po_item.

* Get the latest item number
  read table i_po_item into wa_po_item index 1.

  if sy-subrc = 0.
    l_item_new = wa_po_item-po_item + c_10.
  endif.

* Map PO item data
  wa_poitem-po_item   = l_item_new.
  wa_poitem-ref_doc   = p_po_num.
  wa_poitem-ref_item  = p_item_original.  "Reference to original item
  wa_poitem-ret_item  = c_x.
  wa_poitem-calctype  = 'C'.
  wa_poitem-no_rounding = c_x.
  l_version = ps_common-version.
  concatenate ps_common-id
              l_version+3(2)
         into wa_poitem-preq_name.
  append wa_poitem to i_poitem.

  wa_poitemx-po_item   = l_item_new.
  wa_poitemx-ref_doc   = c_x.
  wa_poitemx-ref_item  = c_x.
  wa_poitemx-ret_item  = c_x.
  wa_poitemx-calctype  = c_x.
  wa_poitemx-no_rounding = c_x.
  wa_poitemx-preq_name = c_x.
  append wa_poitemx to i_poitemx.

  perform f_call_bapi_po_change tables   i_poitem
                                         i_poitemx
                                         i_poschedule
                                         i_poschedulex
                                         i_poaccount       "DV5K993102
                                         i_poaccountx      "DV5K993102
                                using    p_po_num
                                changing p_subrc
                                         pi_return.

  if p_subrc is initial.
*   Map Goods receipt data
    clear wa_goods_recp.
    wa_goods_recp-item    = l_item_new.
    append wa_goods_recp to i_goods_recp.

    perform f_create_goods_receipt tables i_goods_recp
                                   using p_po_num
                                         ps_common-id
                                         ps_common-version
                                         wa_po_header-ref_1
                                         ps_common-zdlvr_dt
                                         ps_common-zdtl_loco_nbr
                                         ps_common-zdtl_loco_init
                                changing p_subrc
                                         pi_return.

    perform f_add_to_po_list tables i_goods_recp
                             using  p_po_num
                                    wa_po_header-vendor
                                    wa_po_header-ref_1
                                    ps_common-zdlvr_dt.      "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
  endif.

endform.                    " F_CANCEL_PO

*&---------------------------------------------------------------------*
*&      Form  F_ADD_ITEMS
*&---------------------------------------------------------------------*
form f_add_items  using p_po_num
                        p_item_num
                        p_quantity
                        p_meins
                        p_contract_num
                        p_contract_item
                        ps_common structure wa_common
                        value(ps_contract_item) type t_contract    "DV5K993102
               changing p_subrc
                        pi_return type bapiret2_tab.

  data: l_version     type char05.

  clear: wa_poitem, wa_poitemx,
         wa_poschedule, wa_poschedulex,
         p_subrc.

  refresh: i_poitem, i_poitemx,
           i_poaccount, i_poaccountx,                              "DV5K993102
           i_poschedule, i_poschedulex, i_goods_recp.

* Map PO item data
  wa_poitem-po_item    = p_item_num.
  wa_poitem-quantity   = p_quantity.
  wa_poitem-po_unit    = p_meins.
  wa_poitem-agreement  = p_contract_num.
  wa_poitem-agmt_item  = p_contract_item.
  l_version = ps_common-version.
  concatenate ps_common-id
              l_version+3(2)
         into wa_poitem-preq_name.
  append wa_poitem to i_poitem.

  wa_poitemx-po_item    = p_item_num.
  wa_poitemx-quantity   = c_x.
  wa_poitemx-po_unit    = c_x.
  wa_poitemx-agreement  = c_x.
  wa_poitemx-agmt_item  = c_x.
  wa_poitemx-preq_name  = c_x.
  append wa_poitemx to i_poitemx.

* Map PO schedule line data
  wa_poschedule-po_item       = p_item_num.
  wa_poschedule-sched_line    = 1.
  wa_poschedule-delivery_date = ps_common-zdlvr_dt.
  wa_poschedule-quantity      = p_quantity.
  append wa_poschedule to i_poschedule.

  wa_poschedulex-po_item       = p_item_num.
  wa_poschedulex-sched_line    = 1.
  wa_poschedulex-po_itemx      = c_x.
  wa_poschedulex-sched_linex   = c_x.
  wa_poschedulex-delivery_date = c_x.
  wa_poschedulex-quantity      = c_x.
  append wa_poschedulex to i_poschedulex.

***** start of code added DV5K993102 CR 257405
* If the equipment, which was fueled by vendor, is not a locomotive, then cost center and
* G/L should not come from the contract.
  perform f_misc_equi_account using p_po_num p_item_num ps_common ps_contract_item changing i_poaccount i_poaccountx pi_return p_subrc.
  check ( p_subrc = 0 ).
***** end   of code added DV5K993102 CR 257405

  perform f_call_bapi_po_change tables   i_poitem
                                         i_poitemx
                                         i_poschedule
                                         i_poschedulex
                                         i_poaccount       "DV5K993102
                                         i_poaccountx      "DV5K993102
                                using    p_po_num
                                changing p_subrc
                                         pi_return.

  if p_subrc is initial.
*   Map Goods receipt data
    clear wa_goods_recp.
    wa_goods_recp-item    = p_item_num.
    append wa_goods_recp to i_goods_recp.

    perform f_create_goods_receipt tables i_goods_recp
                                   using p_po_num
                                         ps_common-id
                                         ps_common-version
                                         ps_common-zbol_tckt_nbr
                                         ps_common-zdlvr_dt
                                         ps_common-zdtl_loco_nbr
                                         ps_common-zdtl_loco_init
                                changing p_subrc
                                         pi_return.

    perform f_add_to_po_list tables i_goods_recp
                             using  p_po_num
                                    ps_common-lifnr
                                    ps_common-zbol_tckt_nbr
                                    ps_common-zdlvr_dt.      "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
  endif.

endform.                    " F_ADD_ITEMS

*&---------------------------------------------------------------------*
*&      Form  F_CALL_BAPI_PO_CHANGE
*&---------------------------------------------------------------------*
form f_call_bapi_po_change
               tables   pi_poitem      structure bapimepoitem
                        pi_poitemx     structure bapimepoitemx
                        pi_poschedule  structure bapimeposchedule
                        pi_poschedulex structure bapimeposchedulx
                        pi_poaccount   structure bapimepoaccount         "DV5K993102
                        pi_poaccountx  structure bapimepoaccountx        "DV5K993102
               using    p_po_num
               changing p_subrc
                        pi_return type bapiret2_tab.

  data li_bapireturn   type table of bapiret2.

  clear p_subrc.

  refresh li_bapireturn.

  call function 'BAPI_PO_CHANGE'
    exporting
      purchaseorder = p_po_num
    tables
      return        = li_bapireturn
      poitem        = pi_poitem
      poitemx       = pi_poitemx
      poaccount     = pi_poaccount                    "DV5K993102
      poaccountx    = pi_poaccountx                   "DV5K993102
      poschedule    = pi_poschedule
      poschedulex   = pi_poschedulex.

  append lines of li_bapireturn to pi_return.

  loop at li_bapireturn into wa_return
                        where type = c_abort
                           or type = c_error.
    p_subrc = 4.
    exit.
  endloop.

  if p_subrc is initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
  endif.

endform.                    " F_CALL_BAPI_PO_CHANGE

*&---------------------------------------------------------------------*
*&      Form  F_GET_PO_DETAIL
*&---------------------------------------------------------------------*
form f_get_po_detail tables   pi_po_item     structure bapiekpo
                              pi_po_schedule structure bapieket
                     using    p_po_num
                     changing ps_po_header   structure bapiekkol
                              p_subrc.

  data: li_bapireturn  type table of bapireturn,
        lwa_bapireturn type bapireturn.

  clear: ps_po_header, p_subrc.

  refresh: pi_po_item, pi_po_schedule, li_bapireturn.

  call function 'BAPI_PO_GETDETAIL'
    exporting
      purchaseorder     = p_po_num
      items             = c_x
      schedules         = c_x
    importing
      po_header         = ps_po_header
    tables
      po_items          = pi_po_item
      po_item_schedules = pi_po_schedule
      return            = li_bapireturn.

  loop at li_bapireturn into lwa_bapireturn
                        where type = c_abort
                           or type = c_error.
    p_subrc = 4.
    exit.
  endloop.

endform.                    " F_GET_PO_DETAIL

*&---------------------------------------------------------------------*
*&      Form  F_CREATE_GOODS_RECEIPT
*&---------------------------------------------------------------------*
form f_create_goods_receipt tables  pi_gr structure wa_goods_recp
                            using   p_po_num
                                    p_id
                                    p_version
                                    p_ref_1
                                    p_date
                                    p_zdtl_loco_nbr
                                    p_zdtl_loco_init
                           changing p_subrc
                                    pi_return type bapiret2_tab.

  data: lwa_goodsmvt_header  type bapi2017_gm_head_01,
        l_materialdocument   type bapi2017_gm_head_ret-mat_doc,
        li_goodsmvt_item     type table of bapi2017_gm_item_create,
        lwa_goodsmvt_item    type bapi2017_gm_item_create,
        li_bapireturn        type table of bapiret2,
        l_version            type char05.

  clear: l_materialdocument, p_subrc.

  refresh: li_goodsmvt_item, li_bapireturn.

* Map GR header data
  clear lwa_goodsmvt_header.
  lwa_goodsmvt_header-doc_date       = p_date.             "DV5K967793 defect 651
*  lwa_goodsmvt_header-doc_date       = sy-datum.          "DV5K967793 defect 651
  lwa_goodsmvt_header-bill_of_lading = p_ref_1.
*  lwa_goodsmvt_header-pstng_date     = p_date.            "DV5K966805-
  lwa_goodsmvt_header-pstng_date     = sy-datum - 1.      "DV5K966805+
  l_version = p_version.
  concatenate p_id
              l_version+3(2)
         into lwa_goodsmvt_header-ref_doc_no.

* Map GR item data
  clear lwa_goodsmvt_item.
  lwa_goodsmvt_item-move_type         = c_type_101.
  lwa_goodsmvt_item-mvt_ind           = c_ind_b.
  lwa_goodsmvt_item-po_number         = p_po_num.
  lwa_goodsmvt_item-ind_propose_quanx = c_x.
  concatenate p_zdtl_loco_init
              p_zdtl_loco_nbr
         into lwa_goodsmvt_item-gr_rcpt.
  loop at pi_gr.
    lwa_goodsmvt_item-po_item = pi_gr-item.
    append lwa_goodsmvt_item to li_goodsmvt_item.
  endloop.

  call function 'BAPI_GOODSMVT_CREATE'
    exporting
      goodsmvt_header  = lwa_goodsmvt_header
      goodsmvt_code    = c_code_01              "GR with PO
    importing
      materialdocument = l_materialdocument
    tables
      goodsmvt_item    = li_goodsmvt_item
      return           = li_bapireturn.

  append lines of li_bapireturn to pi_return.

  if l_materialdocument is not initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
*   Generate the message log
    clear wa_return.
    wa_return-type       = c_success.
    wa_return-id         = 'MIGO'.
    wa_return-number     = 12.
    wa_return-message_v1 = l_materialdocument.
    append wa_return to pi_return.
    if ( 1 = 2 ). message i012(migo). endif.
  else.
    p_subrc = 4.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
  endif.

endform.                    " F_CREATE_GOODS_RECEIPT

*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_LOG
*&---------------------------------------------------------------------*
form f_update_log  using p_tabname
                         p_id
                         p_version
                         pi_return type bapiret2_tab.

  check not pi_return[] is initial.

  call method zcl_smint666_zmm_dtl_msgs=>log_all_messages
    exporting
      im_tabname     = p_tabname
      im_event_id    = p_id
      im_version     = p_version
      im_messages    = pi_return
    exceptions
      error_in_save  = 1
      appl_log_error = 2
      others         = 3.

endform.                    " F_UPDATE_LOG

*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_ERROR_STATUS
*&---------------------------------------------------------------------*
form f_update_error_status .

  if not i_fuel_status[] is initial.
    update zmm_dtl_fuel_evt from table i_fuel_status.
  endif.

  if not i_charge_status[] is initial.
    update zmm_dtl_xtra_chg from table i_charge_status.
  endif.

  commit work.

endform.                    " F_UPDATE_ERROR_STATUS

*&---------------------------------------------------------------------*
*&      Form  F_ADD_TO_PO_LIST
*&---------------------------------------------------------------------*
form f_add_to_po_list  tables   pi_gr structure wa_goods_recp
                       using    p_po_num
                                p_lifnr
                                p_zbol_tckt_nbr
                                uv_zdlvr_dt TYPE dats.      "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
  loop at pi_gr.
    clear wa_po_list.
    wa_po_list-lifnr = p_lifnr.
    wa_po_list-ihrez = p_zbol_tckt_nbr.
    wa_po_list-eindt  = uv_zdlvr_dt.                        "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD

    select single ebeln ebelp matkl werks bednr afnam retpo elikz
    into corresponding fields of wa_po_list
    from ekpo
    where ebeln = p_po_num
      and ebelp = pi_gr-item.

    if sy-subrc = 0.
      insert wa_po_list into table i_po_list.
    endif.
  endloop.

*** Start of Insert TXT25668 - C403761 T453881 - DV5K9A0QBF
  gt_po_list_upd = i_po_list.
  LOOP AT gt_po_list_upd ASSIGNING FIELD-SYMBOL(<ls_po_list_upd>).
    <ls_po_list_upd>-afnam = <ls_po_list_upd>-afnam(10).
  ENDLOOP.
*** Start of Insert TXT25668 - C403761 T453881 - DV5K9A0QBF
endform.                    " F_ADD_TO_PO_LIST

***** start of code added DV5K992637 CR 257405
*&---------------------------------------------------------------------*
*&      Form  F_MISC_EQUI_ACCOUNT
*&---------------------------------------------------------------------*
*       If equipment is not a locomotive, return G/L and cost center
*       to override the ones from the contract.
*----------------------------------------------------------------------*
*  -->  Pa_COMMON
*  <--  Pa_POACCOUNT
*  <--  Pa_POACCOUNTX
*----------------------------------------------------------------------*
form f_misc_equi_account using    value(pa_ebeln)         type ebeln                  "DV5K993102
                                  value(pa_ebelp)         type ebelp
                                  value(pa_common)        type t_common
                                  value(pa_contract_item) type t_contract
                         changing pa_poaccount            type t_bapi_account_t
                                  pa_poaccountx           type t_bapi_accountx_t
                                  pa_return               type bapiret2_tab
                                  pa_subrc                type i.

  data:
    ld_gl            type t_bapi_account-gl_account,             "DV5K993102
    ld_cc            type t_bapi_account-costcenter,             "DV5K993102
    ld_zekkn         type ekkn-zekkn,                            "DV5K993102
    ld_loco          type equi-equnr,
    ld_group         type zmm_flcm_parms-zval_to,
    ld_key           type zmm_flcm_parms-zval_from.

  field-symbols:
    <misc_rail>      like line of i_misc_rail,
    <gl>             like line of i_misc_rail,
    <cc>             like line of i_misc_rail,
    <return>         like line of pa_return.

  clear:
    pa_subrc.

  refresh:
    pa_poaccount,
    pa_poaccountx.

* Get item account assignment sequence number                                          "DV5K993102
  perform f_get_accsgn_seqno using pa_ebeln pa_ebelp changing ld_zekkn.                "DV5K993102

* No equipment number?                                                                 "DV5K993102
  if ( pa_common-zdtl_loco_init is initial ) or                                        "DV5K993102
     ( pa_common-zdtl_loco_nbr  is initial ).                                          "DV5K993102
*   Extra charge event?                                                                "DV5K993102
    if ( pa_common-matkl(4) = c_matgrp_extra_chrg ).                                   "DV5K993102
*     Get equipment from matching fuel event                                           "DV5K993102
      perform f_fuel_evt_equip changing pa_common ld_gl ld_cc.                         "DV5K993102

*     We got GL and CC from matching fuel PO?                                          "DV5K993102
      if ( ld_gl is not initial ) and ( ld_cc is not initial ).                        "DV5K993102
*        Use those for the extra charge PO as well                                     "DV5K993102
         perform f_prepare_bapi_account using pa_ebelp ld_zekkn ld_gl ld_cc            "DV5K993102
                                        changing pa_poaccount pa_poaccountx.           "DV5K993102
*        we're done, leave routine now!!                                               "DV5K993102
         exit.                                                                         "DV5K993102
      endif.                                                                           "DV5K993102
    endif.                                                                             "DV5K993102
  endif.                                                                               "DV5K993102

* No equipment number? leave routine now!
  check ( pa_common-zdtl_loco_init is not initial )
    and ( pa_common-zdtl_loco_nbr  is not initial ).

* Build equipment number for lookup
  concatenate pa_common-zdtl_loco_init pa_common-zdtl_loco_nbr into ld_loco.

* Get misc equipment record from fuel params table
  read table i_misc_rail assigning <misc_rail>
        with key zparm_nm  = c_equip
                 zval_from = ld_loco binary search.

* Found? this equipment is not a locomotive, proceed
  check ( sy-subrc = 0 ).

* Get equipment group
  ld_group = <misc_rail>-zval_to.

* ---- Find G/L for Rail Grinder Equipment -----------------

* Build key
  concatenate ld_group pa_contract_item-bukrs into ld_key.

* Get G/L account from fuel params table
  read table i_misc_rail assigning <gl>
        with key zparm_nm  = c_gl_account
                 zval_from = ld_key binary search.

  if ( sy-subrc <> 0 ).
*   No found? return error
    pa_subrc = 4.
    if ( 1 = 2 ). message e103(zz_flcm). endif.
    append initial line to pa_return assigning <return>.
    <return>-id = 'ZZ_FLCM'.
    <return>-number = '103'.
    <return>-type = 'E'.
    <return>-message_v1 = ld_loco.
    <return>-message_v2 = pa_common-id.
    shift <return>-message_v2 left deleting leading '0'.
    <return>-message_v3 = pa_contract_item-bukrs.
*   Leave routine now!
    exit.
  endif.

* ---- Find Cost Center for Rail Grinder Equipment -----------------

* Build key
  concatenate ld_group pa_contract_item-bukrs pa_contract_item-werks into ld_key.

* Get cost center from fuel params table
  read table i_misc_rail assigning <cc>
        with key zparm_nm  = c_cost_center
                 zval_from = ld_key binary search.

  if ( sy-subrc <> 0 ).
*   Not found? check again without the plant                           "DV5K993052
    concatenate ld_group pa_contract_item-bukrs into ld_key.           "DV5K993052

*   Get cost center from fuel params table                             "DV5K993052
    read table i_misc_rail assigning <cc>                              "DV5K993052
          with key zparm_nm  = c_cost_center                           "DV5K993052
                   zval_from = ld_key binary search.                   "DV5K993052

    if ( sy-subrc <> 0 ).                                              "DV5K993052
*     Not found? return error
      pa_subrc = 4.
      if ( 1 = 2 ). message e104(zz_flcm). endif.
      append initial line to pa_return assigning <return>.
      <return>-id = 'ZZ_FLCM'.
      <return>-number = '104'.
      <return>-type = 'E'.
      <return>-message_v1 = ld_loco.
      <return>-message_v2 = pa_common-id.
      shift <return>-message_v2 left deleting leading '0'.
      <return>-message_v3 = pa_contract_item-bukrs.
      <return>-message_v4 = pa_contract_item-werks.
*     Leave routine now!
      exit.
    endif.                                                             "DV5K993052
  endif.

* ---- build po account data

  perform f_prepare_bapi_account using pa_ebelp ld_zekkn <gl>-zval_to <cc>-zval_to  "DV5K993102
                                 changing pa_poaccount pa_poaccountx.      "DV5K993102

endform.                    " F_MISC_EQUI_ACCOUNT
***** end   of code added DV5K992637 CR 257405
***** start of code added DV5K993102 CR 257405
*&---------------------------------------------------------------------*
*&      Form  F_FUEL_EVT_EQUIP
*&---------------------------------------------------------------------*
*       Get equipment from matching fuel event
*----------------------------------------------------------------------*
*  <--  pa_common
*----------------------------------------------------------------------*
form f_fuel_evt_equip changing pa_common type t_common
                               pa_gl     type ekkn-sakto
                               pa_cc     type ekkn-kostl.

  field-symbols:
    <fuel>          like line of i_charge_fuel,
    <po>            like line of i_po_list.

  data:
    lt_ekkn         type table of bapiekkn.

  clear:
    pa_gl,
    pa_cc.

* Get matching fuel event
  read table i_charge_fuel assigning <fuel>
        with key lifnr         = pa_common-lifnr
                 zbol_tckt_nbr = pa_common-zbol_tckt_nbr
*                 werks         = pa_common-werks binary search.    "D - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
                 werks         = pa_common-werks                    "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
                 zdlvr_dt      = pa_common-zdlvr_dt BINARY SEARCH.  "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD

  check ( sy-subrc = 0 ).

* Overwrite equipment from fuel event
  pa_common-zdtl_loco_nbr = <fuel>-zdtl_loco_nbr.
  pa_common-zdtl_loco_init = <fuel>-zdtl_loco_init.

*---- from this point on we check if a PO exists and return GL/CC if present

* Get fuel event PO item
  read table i_po_list assigning <po>
        with key lifnr = pa_common-lifnr
                 ihrez = pa_common-zbol_tckt_nbr
                 matkl = c_fuel1010
*                 werks = pa_common-werks binary search.    "D - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
                 werks = pa_common-werks                    "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD
                 eindt = pa_common-zdlvr_dt BINARY SEARCH.  "I - TXT22260 - CR368488 TK437632 - DV5K9A0MSD

  check ( sy-subrc = 0 ).

* Get PO item account assignment
  select single sakto
                kostl
           into (pa_gl,
                 pa_cc)
           from ekkn
          where ( ebeln = <po>-ebeln )
            and ( ebelp = <po>-ebelp ).

endform.                    " F_FUEL_EVT_EQUIP

*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_BAPI_ACCOUNT
*&---------------------------------------------------------------------*
*       Prepare GL and costcenter for Bapi update
*----------------------------------------------------------------------*
*  -->  pa_gl
*  -->  pa_cc
*  <--  pa_poaccount
*  <--  pa_poaccountx
*----------------------------------------------------------------------*
form f_prepare_bapi_account using    value(pa_ebelp) type ebelp
                                     value(pa_zekkn) type ekkn-zekkn            "DV5K993102
                                     value(pa_gl)    type c
                                     value(pa_cc)    type c
                            changing pa_poaccount    type t_bapi_account_t
                                     pa_poaccountx   type t_bapi_accountx_t.

  field-symbols:
    <poaccount>      like line of pa_poaccount,
    <poaccountx>     like line of pa_poaccountx.

* Account fields
  append initial line to pa_poaccount assigning <poaccount>.
  <poaccount>-po_item = pa_ebelp.
  <poaccount>-serial_no = pa_zekkn.                                             "DV5K993102
  <poaccount>-gl_account = pa_gl.
  <poaccount>-costcenter = pa_cc.

  condense <poaccount>-gl_account no-gaps.
  condense <poaccount>-costcenter no-gaps.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting input  = <poaccount>-gl_account
    importing output = <poaccount>-gl_account.
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting input  = <poaccount>-costcenter
    importing output = <poaccount>-costcenter.

* Account change indicators
  append initial line to pa_poaccountx assigning <poaccountx>.
  <poaccountx>-po_item = pa_ebelp.
  <poaccountx>-serial_no = pa_zekkn.                                            "DV5K993102
  <poaccountx>-gl_account = abap_true.
  <poaccountx>-costcenter = abap_true.

endform.                    " F_PREPARE_BAPI_ACCOUNT

*&---------------------------------------------------------------------*
*&      Form  F_GET_ACCSGN_SEQNO
*&---------------------------------------------------------------------*
*       Get PO item account assignment sequence number
*----------------------------------------------------------------------*
*  -->  pa_ebeln
*  -->  pa_ebelp
*  <--  pa_zekkn
*----------------------------------------------------------------------*
form f_get_accsgn_seqno using    value(pa_ebeln) type ebeln
                                 value(pa_ebelp) type ebelp
                        changing pa_zekkn        type ekkn-zekkn.

  clear pa_zekkn.

  if ( pa_ebeln is not initial ).
    select single zekkn
             into pa_zekkn
             from ekkn
            where ( ebeln = pa_ebeln )
              and ( ebelp = pa_ebelp ).
  endif.

  if ( pa_zekkn is initial ).
    pa_zekkn = 1.
  endif.

endform.                    " F_GET_ACCSGN_SEQNO
***** end   of code added DV5K993102 CR 257405
***BEGIN OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_PO_CREATION
*&---------------------------------------------------------------------*
*       Validate PO creation for unreasonable amount
*----------------------------------------------------------------------*
FORM f_validate_po_creation  USING    p_common  structure wa_common
                                      p_contract_num
                                      p_contract_item
                                      p_quantity
                                      p_uom
                                      pwa_contract_item TYPE t_contract
                                      p_po_num
                             CHANGING c_return type bapiret2_tab
                                      c_status
                                      c_subrc.
  DATA:
    l_check     TYPE c,
    l_equi      TYPE tvarv_val,
    l_invalid   TYPE c,
    l_net_price TYPE bapicurext,
    l_zval      TYPE zmm_flcm_parms-zval_to,
    l_capacity  TYPE brgew_ap,
    l_quantity  TYPE brgew_ap,
    l_cuobj     TYPE inob-cuobj,
    l_atwrt     TYPE ausp-atwrt,
    l_max_litre   TYPE bapicurext,
    l_max_gallon  TYPE bapicurext,
    li_bapireturn TYPE bapiret2_tab,
    l_version     TYPE char05,
    ld_msg1       TYPE syst_msgv,
    ld_msg2       TYPE syst_msgv.

  CLEAR: wa_poheader, wa_poheaderx,
         wa_poitem, wa_poitemx,
         wa_poschedule, wa_poschedulex,
         p_po_num, c_subrc.

  REFRESH: i_poitem, i_poitemx,
           i_poaccount, i_poaccountx,
           i_poschedule, i_poschedulex,
           i_goods_recp, li_bapireturn.

  "Check if validation is needed
  PERFORM f_check_validation CHANGING l_check.

  CHECK l_check EQ abap_true.

***BEGIN OF DELETE - XT18912 - CR269546 TK334189 - DV5K9A03XO
*  "1.  Validate that the currency and the order unit (OUn) in the PO matches
*  IF ( pwa_contract_item-waers EQ c_cad AND p_common-uom NE c_liter )
*    OR ( pwa_contract_item-waers EQ c_usd AND ( p_common-uom NE c_gallon AND p_common-uom NE c_gallon_2 ) ).
*    c_status = c_04.
*    c_subrc  = c_04.
*
*    "Generate the message log
*    CLEAR wa_return.
*    wa_return-type       = c_error.
*    wa_return-id         = c_msgid.
*    wa_return-number     = 107.
*    wa_return-message_v1 = pwa_contract_item-waers.
*    IF p_common-uom EQ c_gallon_2.
*      wa_return-message_v2 = c_gallon.
*    ELSE.
*      wa_return-message_v2 = p_common-uom.
*    ENDIF.
*
*    APPEND wa_return to i_return.
*    IF ( 1 = 2 ). message i107(zz_flcm). endif.
*    EXIT.
*  ENDIF.
***END OF DELETE - XT18912 - CR269546 TK334189 - DV5K9A03XO
***BEGIN OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03XO
  "1.  Validate that the contract UOM and the delivery UOM matches
  IF p_common-uom NE p_uom.
    c_status = c_04.
    c_subrc  = c_04.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input                = p_common-uom
        language             = sy-langu
      IMPORTING
        output               = ld_msg1
      EXCEPTIONS
        unit_not_found       = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      ld_msg1 = '<Unknown>'.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input                = p_uom
        language             = sy-langu
      IMPORTING
        output               = ld_msg2
      EXCEPTIONS
        unit_not_found       = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      ld_msg2 = '<Unknown>'.
    ENDIF.

    "Generate the message log
    CLEAR wa_return.
    wa_return-type       = c_error.
    wa_return-id         = c_msgid.
    wa_return-number     = 108.
    wa_return-message_v1 = ld_msg1.
    wa_return-message_v2 = ld_msg2.

    APPEND wa_return to i_return.
    IF ( 1 = 2 ). message i108(zz_flcm). endif.
    EXIT.
  ENDIF.
***END OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03XO

  "2.  The PO quantity must not exceed the fuel tank capacity of the equipment.
  CONCATENATE p_common-zdtl_loco_init
              p_common-zdtl_loco_nbr
         INTO l_equi.

  SELECT SINGLE zval_to
    FROM zmm_flcm_parms
    INTO l_zval
  WHERE progname = c_smenh817
    AND zparm_nm = c_cap_gallon
    AND zval_from = l_equi.
  IF sy-subrc IS INITIAL.
    l_capacity = l_zval.
  ELSE.
    SELECT SINGLE cuobj
      FROM inob
      INTO l_cuobj
     WHERE objek = l_equi
       AND klart = c_z02.                             "#EC CI_NOFIRST
    IF sy-subrc IS INITIAL.
      SELECT SINGLE atwrt
        FROM ausp
        INTO l_atwrt
       WHERE objek = l_cuobj
         AND klart = c_z02
         AND atinn = c_fuelqty_int.
      IF sy-subrc IS INITIAL.
        l_capacity = l_atwrt.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_uom EQ c_liter.
    l_quantity = p_quantity / '3.78541'.
  ELSE.
    l_quantity = p_quantity.
  ENDIF.

  IF l_quantity GT l_capacity
    AND l_capacity IS NOT INITIAL.
    c_status = c_04.
    c_subrc  = c_04.

*   Generate the message log
    CLEAR wa_return.
    wa_return-type   = c_error.
    wa_return-id     = c_msgid.
    wa_return-number = 105.
    APPEND wa_return to i_return.
    IF ( 1 = 2 ). message i105(zz_flcm). endif.
    EXIT.
  ENDIF.

*  "3-  The net price must not exceed the maximum price allowed
* Map PO header data
  wa_poheader-doc_type = c_fb.
  wa_poheader-doc_date = p_common-zdlvr_dt.
  wa_poheader-ref_1    = p_common-zbol_tckt_nbr.
  wa_poheader-vendor   = p_common-lifnr.
*  wa_poheader-our_ref  = c_dtl.                           "DV5K9A05WW-

  wa_poheaderx-doc_type = c_x.
  wa_poheaderx-doc_date = c_x.
  wa_poheaderx-ref_1    = c_x.
  wa_poheaderx-vendor   = c_x.
*  wa_poheaderx-our_ref  = c_x.                            "DV5K9A05WW-

* Map PO item data
  wa_poitem-po_item   = p_contract_item.
  wa_poitem-quantity  = p_quantity.
  wa_poitem-po_unit   = p_uom.
  wa_poitem-agreement = p_contract_num.
  wa_poitem-agmt_item = p_contract_item.
  wa_poitem-no_rounding = c_x.
  l_version = p_common-version.
  CONCATENATE p_common-id
              l_version+3(2)
         INTO wa_poitem-preq_name.
  APPEND wa_poitem to i_poitem.

  wa_poitemx-po_item    = p_contract_item.
  wa_poitemx-quantity   = c_x.
  wa_poitemx-po_unit    = c_x.
  wa_poitemx-agreement  = c_x.
  wa_poitemx-agmt_item  = c_x.
  wa_poitemx-no_rounding = c_x.
  wa_poitemx-preq_name  = c_x.
  APPEND wa_poitemx TO i_poitemx.

* Map PO schedule line data
  wa_poschedule-po_item       = p_contract_item.
  wa_poschedule-sched_line    = 1.
  wa_poschedule-delivery_date = p_common-zdlvr_dt.
  wa_poschedule-quantity      = p_quantity.
  APPEND wa_poschedule TO i_poschedule.

  wa_poschedulex-po_item       = p_contract_item.
  wa_poschedulex-sched_line    = 1.
  wa_poschedulex-po_itemx      = c_x.
  wa_poschedulex-sched_linex   = c_x.
  wa_poschedulex-delivery_date = c_x.
  wa_poschedulex-quantity      = c_x.
  APPEND wa_poschedulex TO i_poschedulex.

* If the equipment, which was fueled by vendor, is not a locomotive, then cost center and
* G/L should not come from the contract.
  PERFORM f_misc_equi_account USING p_po_num
                                    p_contract_item
                                    p_common
                                    pwa_contract_item
                           CHANGING i_poaccount
                                    i_poaccountx
                                    c_return
                                    c_subrc.

  CHECK ( c_subrc = 0 ).

*--> Start of Insert DV5K9A05WW - Chuong Le - 2017/01/04
  DATA(lt_potextheader) = VALUE bapimepotextheader_tp( ( text_id   = 'F99'
                                                         text_form = '* '
                                                         text_line = c_dtl ) ).
*<-- End of Insert DV5K9A05WW - Chuong Le - 2017/01/04

  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader         = wa_poheader
      poheaderx        = wa_poheaderx
      testrun          = abap_true
    IMPORTING
      exppurchaseorder = p_po_num
    TABLES
      return           = li_bapireturn
      poitem           = i_poitem
      poitemx          = i_poitemx
      poaccount        = i_poaccount
      poaccountx       = i_poaccountx
      poschedule       = i_poschedule
      poschedulex      = i_poschedulex
      potextheader     = lt_potextheader.                  "DV5K9A05WW+

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  PERFORM f_retrieve_max_amount CHANGING l_max_litre
                                         l_max_gallon.

  READ TABLE i_poitem INTO wa_poitem
    INDEX 1.
  IF sy-subrc IS INITIAL.
    IF wa_poitem-price_unit IS NOT INITIAL.
      l_net_price = wa_poitem-net_price / wa_poitem-price_unit.
    ELSE.
      CLEAR: l_net_price.
    ENDIF.

    CASE p_uom.
      WHEN c_liter.
        IF l_net_price GT l_max_litre.
          l_invalid = abap_true.
        ENDIF.
      WHEN c_gallon.
        IF l_net_price GT l_max_gallon.
          l_invalid = abap_true.
        ENDIF.
      WHEN c_gallon_2.
        IF l_net_price GT l_max_gallon.
          l_invalid = abap_true.
        ENDIF.
    ENDCASE.
  ENDIF.

  IF l_invalid EQ abap_true.
    c_status = c_04.
    c_subrc  = c_04.

*   Generate the message log
    CLEAR wa_return.
    wa_return-type   = c_error.
    wa_return-id     = c_msgid.
    wa_return-number = 106.
    IF p_uom EQ c_gallon_2.
      wa_return-message_v1 = c_gallon.
    ELSE.
      wa_return-message_v1 = p_uom.
    ENDIF.

    APPEND wa_return to i_return.
    IF ( 1 = 2 ). message i106(zz_flcm). ENDIF.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_VALIDATION
*&---------------------------------------------------------------------*
*       Check if validation is to be executed
*----------------------------------------------------------------------*
FORM f_check_validation  CHANGING c_check.
  CONSTANTS: lc_false TYPE c VALUE 'F'.

  STATICS:
    s_check      TYPE c.

  DATA:
    l_zval      TYPE zmm_flcm_parms-zval_to.

  CLEAR c_check.

  IF s_check IS INITIAL.
    SELECT SINGLE zval_from
      FROM zmm_flcm_parms
      INTO l_zval
     WHERE progname EQ c_smenh817
       AND zparm_nm EQ c_cond_check.
    IF sy-subrc IS INITIAL
      AND l_zval EQ abap_true.
      s_check = abap_true.
    ELSE.
      s_check = lc_false.
    ENDIF.
  ENDIF.

  IF s_check EQ abap_true.
    c_check = abap_true.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_RETRIEVE_MAX_AMOUNT
*&---------------------------------------------------------------------*
*       Retrieve Max Amount per Volume Type
*----------------------------------------------------------------------*
FORM f_retrieve_max_amount  CHANGING c_max_litre_x
                                     c_max_gallon_x.
  STATICS:
    l_max_litre  TYPE bapicurext,
    l_max_gallon TYPE bapicurext.

  DATA:
    l_zval      TYPE zmm_flcm_parms-zval_to.

  CLEAR:
    c_max_litre_x,
    c_max_gallon_x.

  IF l_max_litre IS INITIAL.
    SELECT SINGLE zval_from
      FROM zmm_flcm_parms
      INTO l_zval
     WHERE progname EQ c_smenh817
       AND zparm_nm EQ c_max_litre.
    IF sy-subrc IS INITIAL.
      l_max_litre = l_zval.
    ENDIF.
  ENDIF.

  IF l_max_gallon IS INITIAL.
    SELECT SINGLE zval_from
      FROM zmm_flcm_parms
      INTO l_zval
     WHERE progname EQ c_smenh817
       AND zparm_nm EQ c_max_gallon.
    IF sy-subrc IS INITIAL.
      l_max_gallon = l_zval.
    ENDIF.
  ENDIF.

  c_max_litre_x   = l_max_litre.
  c_max_gallon_x  = l_max_gallon.
ENDFORM.
***END OF INSERT - XT18912 - CR269546 TK334189 - DV5K9A03CO
