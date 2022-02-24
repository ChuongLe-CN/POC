*----------------------------------------------------------------------*
*                      Canadian National                               *
*                                                                      *
* ABAP Name:       ZME_SMENH827_CANC_PO_TDS_DTL                        *
* Created by:      Nancy Bergeron                                      *
* Created on:      2011-02-01                                          *
* Function Design: SM-ENH-827                                          *
*                                                                      *
* Purpose:    RTM requirement for the FLCM project will set the #flag  *
*             for deletion# on both the original Purchase Order (PO)   *
*             line item(s) and the return PO line(s) of a fuel PO if   *
*             the corresponding Bill Of Lading (BOL) record was        *
*             cancelled in either the TDS system (Toptech Data         *
*             Services) or on the UCON portal.                         *
*                                                                      *
*             The deletion flag on the PO lines will prevent the       *
*             Revaluation program (see transaction MRNB) (which is part*
*             of the ERS invoice process) to create a debit or credit  *
*             memo for #deleted# PO lines in case the fuel price was   *
*             changed on the fuel contracts for which a PO was already *
*             created. Even if the PO lines are not ERS relevant but   *
*             EDI relevant, the PO lines that represent cancelled      *
*             records in their respective systems should be deleted in *
*             SAP. This will facilitate the search of valid PO lines to*
*             post the EDI invoice. However, in order to set the       *
*             deletion flag for a PO line item in the SAP system that  *
*             already contains a Goods Receipt (GR) but no invoice     *
*             posting, then the GR transaction must be reversed prior  *
*             to set the deletion flag for that PO item.               *
*                                                                      *
* Short Description : Cancel PO for TDS/DTL event.                     *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
*  Jean Seto (154136)  2011-08-16                   Task: DV5K965782   *
*  FLCM defect #430.                                                   *
*----------------------------------------------------------------------*
*  Nancy Bergeron   2011-0627                       Task:DV5K964640    *
*  Short Description : CR169709-T173617 FLCM SM-ENH-827 DEFECT 272     *
*    If "No Purchase Order Found", message should be sent as           *
*        information not as error.                                     *
*----------------------------------------------------------------------*
*  Nancy Bergeron   2011-02-01                       Task:DV5K960962   *
*  Short Description : CR169709-T173617 FLCM SM-ENH-827 (Creation).    *
*----------------------------------------------------------------------*
REPORT zme_smenh827_canc_po_tds_dtl MESSAGE-ID zz_flcm.   "?? message-id

*&---------------------------------------------------------------------*
*&      INCLUDES
*&---------------------------------------------------------------------*
INCLUDE zme_smenh827_canc_po_top.  "GLOBAL DATA

INCLUDE zme_smenh827_canc_po_f1.   "SUBROUTINES

*&---------------------------------------------------------------------*
*&      AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

  DATA: lv_condition TYPE ekko-unsez.

  v_valid_process  = 'Y'.
  v_online_process = 'Y'.
* ONLINE PROCESS
  IF sy-ucomm = 'ONLI'.
    v_start_date = sy-datum - p_nbdays.
*    IF s_unsez[] IS NOT INITIAL.
*      IF ( s_unsez-low+0(3) = c_car or s_unsez-high+0(3) = c_car )
*      OR ( s_unsez-low+0(3) = c_dtl or s_unsez-high+0(3) = c_dtl )
*      OR ( s_unsez-low+0(3) = c_tds or s_unsez-high+0(3) = c_tds ).
**      *Continue
*      ELSE.
*        message e000(zz_flcm) with text-001 text-002.
*        v_valid_process = 'N'.
*      ENDIF.
*    ENDIF.

    IF s_bedat-low  IS INITIAL.
      MESSAGE i000(zz_flcm) WITH text-003.
      v_valid_process = 'N'.
    ELSEIF s_bedat-high IS INITIAL.
      MESSAGE i000(zz_flcm) WITH text-004.
      v_valid_process = 'N'.
    ENDIF.
  ELSE.
    v_online_process = 'N'.
  ENDIF.

  IF s_unsez[] IS NOT INITIAL.
    IF ( s_unsez-low+0(3) = c_car OR s_unsez-high+0(3) = c_car )
    OR ( s_unsez-low+0(3) = c_dtl OR s_unsez-high+0(3) = c_dtl )
    OR ( s_unsez-low+0(3) = c_tds OR s_unsez-high+0(3) = c_tds ).
*      *Continue
    ELSE.
*     MESSAGE e000(zz_flcm) WITH text-001 text-002.  "DV5K965782-
      MESSAGE i000(zz_flcm) WITH text-001 text-002.  "DV5K965782+
      v_valid_process = 'N'.
    ENDIF.
  ELSE.
    s_unsez-sign   = 'I'.
    s_unsez-option = 'CP'.
    CONCATENATE c_car '*' INTO lv_condition.
    s_unsez-low = lv_condition.
    APPEND s_unsez.   " to r_fe_audit_stus.

    CONCATENATE c_dtl '*' INTO lv_condition.
    s_unsez-low = lv_condition.
    APPEND s_unsez.   " to r_fe_audit_stus.

    CONCATENATE c_tds '*' INTO lv_condition.
    s_unsez-low = lv_condition.
    APPEND s_unsez.   " to r_fe_audit_stus.

  ENDIF.

*&---------------------------------------------------------------------*
*&      START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  IF v_valid_process = 'N'.
    EXIT.
  ENDIF.
*5.1 Select valid undeleted/unblocked fuel TDS/DTL PO's
  PERFORM f_valid_fuel_tds_dtl_po.

  IF i_po_header[] IS INITIAL
  OR i_po_items[]  IS INITIAL.
*    MESSAGE e000(zz_flcm) WITH text-005.  "D-DV5K964640
    MESSAGE i000(zz_flcm) WITH text-005.   "I-DV5K964640
    EXIT.
  ENDIF.

*5.2 Select valid undeleted/unblocked ORIGINAL fuel PO
  PERFORM f_search_valid_po.

*obs  IF i_po_header_orig[] IS INITIAL
  IF i_po_items_valid[] IS INITIAL.
*   MESSAGE e000(zz_flcm) WITH text-006.   "DV5K965782-
    MESSAGE i000(zz_flcm) WITH text-006.   "DV5K965782+
    EXIT.
  ENDIF.

*5.3 Search for credit memo on return PO line and ERS flag or Not
  PERFORM f_ers_return_po.

  IF  i_po_items_ers[]     IS INITIAL
  AND i_po_items_non_ers[] IS INITIAL.
    MESSAGE i000(zz_flcm) WITH text-007.
    EXIT.
  ENDIF.

  IF i_po_items_ers[] IS NOT INITIAL.
*5.4 Search for credit memo posted with on return PO line and ERS flag
    PERFORM f_invoice_posted_orig_po.

*5.5 Set Flag for Deletion on original PO line and return PO line.
    PERFORM f_deletion_items TABLES i_po_items_ers.
  ENDIF.

  IF i_po_items_non_ers[] IS NOT INITIAL.
*5.6 If credit memo was posted for return PO line with ERS, Check if invoice
*    was posted for the original PO line.
    PERFORM f_no_invoice_posted_orig_po.

    IF i_po_items_gr[] IS NOT INITIAL.
*5.7 Reverse GR for original PO line and return PO line.
      PERFORM f_reverse_good_receipt.   " TABLES i_po_items_gr.

*5.5 Set Flag for Deletion on original PO line and return PO line.
      PERFORM f_deletion_items TABLES i_po_items_non_ers.
    ENDIF.
  ENDIF.

  IF i_po_delete[] IS NOT INITIAL.
    IF v_online_process = 'Y'.
      PERFORM f_alv_delete_result.

    ENDIF.

  ENDIF.
END-OF-SELECTION.
