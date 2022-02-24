*----------------------------------------------------------------------*
*                      Canadian National                               *
*                                                                      *
* ABAP Name:       ZME_SMENH827_CANC_PO_F1                             *
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
* Changed By                 Date        SIR   ENH     Tracking number *
*----------------------------------------------------------------------*
* Charles Darby              2017-02-17                DV5K9A06LM      *
*                                                                      *
* Short Description: C334599-T351582                                   *
*    - Make sure that the order is at the same "stage" before          *
*      it can be cancelled                                             *
*----------------------------------------------------------------------*
* Chuong Le                  2017-01-04                DV5K9A05WW      *
*                                                                      *
* Short Description: CR306321-T347799                                  *
*    - Read Our Reference from PO header text instead of EKKO-UNSEZ.   *
*----------------------------------------------------------------------*
*  Nancy Bergeron   2011-02-01                       Task:DV5K960962   *
*  Short Description : CR169709-T173617 FLCM SM-ENH-827 (Creation).    *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form f_valid_fuel_tds_dtl_po                                   *
*&---------------------------------------------------------------------*
* 5.1: Select all (non-deleted and non-blocked) TDS and DTL Fuel PO    *
*      line  with a return line & GR.                                  *
*----------------------------------------------------------------------*
FORM f_valid_fuel_tds_dtl_po.

  DATA: lv_ekko_index TYPE sy-tabix.

  CLEAR: lv_ekko_index,
         wa_po_header,
         wa_po_items.

  REFRESH: i_po_header,
           i_po_items.

* Purchase Order Header
  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE i_po_header
  FROM ekko
  WHERE ebeln IN s_ebeln      "Purchasing Document Number
    AND lifnr IN s_lifnr      "Vendor Account Number
    AND bedat IN s_bedat      "Purchasing Document Date
    AND ihrez IN s_ihrez      "Your Reference
*    AND unsez IN s_unsez      "Our Reference              "DV5K9A05WW-
    AND bsart =  'FB'         "Purchasing Document Type
    AND aedat >= v_start_date "Date on Which Record Was Created
    AND bstyp = 'F'.        "Purchasing Document Category

  IF i_po_header[] IS NOT INITIAL.
*--> Start of insert DV5K9A05WW - Chuong Le - 2017/01/04
    LOOP AT i_po_header ASSIGNING FIELD-SYMBOL(<fs_ekko>).
      PERFORM f_get_po_header_text USING    <fs_ekko>-ebeln
                                   CHANGING <fs_ekko>-unsez.
    ENDLOOP.
    DELETE i_po_header WHERE unsez NOT IN s_unsez.
*<-- End of insert DV5K9A05WW - Chuong Le - 2017/01/04
* Keep only record with OUR REFERENCE (unsez) = CAR, DTL or TDS (Batch process only)
    IF v_online_process = 'N'.
      LOOP AT i_po_header INTO wa_po_header.
        IF  wa_po_header-unsez+0(3) <> c_car
        AND wa_po_header-unsez+0(3) <> c_dtl
        AND wa_po_header-unsez+0(3) <> c_tds.
          DELETE i_po_header INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

*   Purchase Order Items
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE i_po_items
    FROM ekpo
    FOR ALL ENTRIES IN i_po_header
    WHERE ebeln =  i_po_header-ebeln "Purchasing Document Number
      AND werks IN s_werks      "Plant
      AND matkl IN s_matkl      "Material Group
      AND loekz =  ' '          "Deletion Indicator in Purchasing Document
      AND elikz =  'X'.         " "Delivery Completed" Indicator

    IF i_po_items[] IS NOT INITIAL.
*     Sort Purchase Order Header
      SORT i_po_header[] by ebeln.
      SORT i_po_items[]  by ebeln ebelp.

*     Keep only record who are candidate.
*     Means 1 EKKO who have at least 1 EKPO with at least 1 return items.
      LOOP AT i_po_header INTO wa_po_header.     "PO header
        lv_ekko_index = sy-tabix.
        READ TABLE i_po_items INTO wa_po_items WITH KEY ebeln = wa_po_header-ebeln
                                                        retpo = 'X'.
        IF sy-subrc <> 0.
          DELETE i_po_items  WHERE ebeln = wa_po_header-ebeln.
          DELETE i_po_header INDEX lv_ekko_index.
        ENDIF.
      ENDLOOP.

*     Keep a copy of i_po_items into i_po_delete to show the into the report
*     List of items process.
      i_po_delete[] = i_po_items.
    ENDIF.
  ENDIF.


ENDFORM. "f_valid_fuel_tds_dtl_po

*&---------------------------------------------------------------------*
*&      Form f_search_valid_po                                *
*&---------------------------------------------------------------------*
* 5.2: Search valid (non-deleted and non-blocked) original fuel PO     *
*      line & GR.                                                      *
*----------------------------------------------------------------------*
FORM f_search_valid_po.
  DATA: li_po_items_temp   TYPE STANDARD TABLE OF t_ekpo,
        lwa_po_return      TYPE t_ekpo,
        lwa_po_orig        TYPE t_ekpo.

  CLEAR: lwa_po_return,
         lwa_po_orig.

  REFRESH: li_po_items_temp,
           i_po_items_valid.


  li_po_items_temp[] = i_po_items[].

* Found all return PO
*  LOOP AT i_po_items INTO lwa_po_return WHERE ebeln = wa_po_header-ebeln
*                                            AND retpo = 'X'.  "Return PO
  LOOP AT i_po_items INTO lwa_po_return WHERE retpo = 'X'.  "Return PO


* Found all the original PO for this return.
    LOOP AT li_po_items_temp INTO lwa_po_orig WHERE ebeln = lwa_po_return-ebeln
                                                AND ebelp < lwa_po_return-ebelp
                                                AND matkl = lwa_po_return-matkl
                                                AND matnr = lwa_po_return-matnr
                                                AND menge = lwa_po_return-menge
                                                AND meins = lwa_po_return-meins
                                                AND netpr = lwa_po_return-netpr
                                                AND werks = lwa_po_return-werks
                                                AND lgort = lwa_po_return-lgort
                                                AND retpo = ' '.  "Original PO
      APPEND lwa_po_orig   TO i_po_items_valid.
      APPEND lwa_po_return TO i_po_items_valid.
    ENDLOOP.
  ENDLOOP.

  IF i_po_items_valid[] IS NOT INITIAL.
    SORT i_po_items_valid by ebeln
                        ebelp
                        retpo.
  ENDIF.
ENDFORM. "f_search_valid_po

*&---------------------------------------------------------------------*
*&      Form f_credit_memo_ers_return_po                               *
*&---------------------------------------------------------------------*
* 5.3 Search for credit memo on return PO line and ERS flag            *
*                                                                      *
* i_po_item_ers => Credit memo was posted and the ERS flag is Active   *
*                                                                      *
* i_po_item_non_ers => No Credit memo was posted OR the PO line is not *
*                   ERS relevant.                                      *
*----------------------------------------------------------------------*
FORM f_ers_return_po.

  CLEAR: wa_ekbe,
         wa_po_return.

  REFRESH: i_ekbe,
           i_po_items_ers,
           i_po_items_non_ers.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE i_ekbe
  FROM ekbe
  FOR ALL ENTRIES IN i_po_items_valid
  WHERE ebeln = i_po_items_valid-ebeln
    AND ebelp = i_po_items_valid-ebelp.
*    AND bewtp = 'Q'.          "Q = ERS invoice is posted for the full PO & GR quantity.

  LOOP AT i_po_items_valid INTO wa_po_return.

    READ TABLE i_ekbe INTO wa_ekbe WITH KEY ebeln = wa_po_return-ebeln
                                            ebelp = wa_po_return-ebelp
                                            bewtp = 'Q'. "Q = ERS invoice is posted for the full PO & GR quantity.
    IF sy-subrc = 0
    AND wa_po_return-xersy IS NOT INITIAL.
      APPEND wa_po_return TO i_po_items_ers.      "An invoice was posted for the return PO line (include original)

    ELSE.            " process 5.6
      APPEND wa_po_return TO i_po_items_non_ers.  "No invoice was posted for the return PO line (include original)
    ENDIF.
  ENDLOOP.
ENDFORM. "f_ers_return_po

*&---------------------------------------------------------------------*
*&      Form f_invoice_posted_orig_po                                  *
*&---------------------------------------------------------------------*
* 5.4 Search for invoices posted on the original PO line.              *
* Remove items who don't have Original invoices posted.                *
*----------------------------------------------------------------------*
FORM f_invoice_posted_orig_po.

  DATA: li_po_items_temp  TYPE STANDARD TABLE OF t_ekpo,
        lwa_po_items_temp TYPE t_ekpo,
        lv_prev_ebeln     TYPE t_ekpo-ebeln.

  CLEAR: lwa_po_items_temp.
  REFRESH: li_po_items_temp.

  li_po_items_temp[] =  i_po_items_ers.

  LOOP AT i_po_items_ers INTO wa_po_return.
    IF wa_po_return-ebeln <> lv_prev_ebeln.
      READ TABLE li_po_items_temp INTO lwa_po_items_temp
      WITH KEY ebeln = wa_po_return-ebeln
               retpo = ' '.

      IF sy-subrc <> 0.
*       *Original is not invoices and posted
        DELETE i_po_items_ers   WHERE ebeln = wa_po_return-ebeln.

        "Also do not CANCEL the ret PO if original can not be cancelled   "I-DV5K9A06LM
        DELETE i_po_items_non_ers   WHERE ebeln = wa_po_return-ebeln.     "I-DV5K9A06LM
      ENDIF.
    ENDIF.

    lv_prev_ebeln = wa_po_return-ebeln.
  ENDLOOP.

*--> start of Insert DV5K9A06LM
  "check if its return PO is at the same @stage@
  LOOP AT i_po_items_ers INTO wa_po_return WHERE retpo = ' '.
    READ TABLE i_po_items_non_ers WITH KEY ebeln = wa_po_return-ebeln
                                           retpo = 'X'
                                  TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      "because we found something, we need to remove it from the i_po_items_ers, since its
      "return PO is not at the same stage.
      DELETE i_po_items_ers where ebeln = wa_po_return-ebeln.
      DELETE i_po_items_non_ers where ebeln = wa_po_return-ebeln.
    ENDIF.
  ENDLOOP.
*<--- end of DV5K9A06LM
ENDFORM.  "f_invoice_posted_orig_po.

*&---------------------------------------------------------------------*
*&      Form f_deletion_items                                      *
*&---------------------------------------------------------------------*
* 5.5 Set Flag for Deletion on original PO line and return PO line.    *
*----------------------------------------------------------------------*
FORM f_deletion_items TABLES p_po_items.

  DATA: li_po_distinct  TYPE STANDARD TABLE OF t_ekpo,
        li_po_delete    TYPE STANDARD TABLE OF t_ekpo,
        li_items        TYPE TABLE OF bapiekpo,
        li_poitem       TYPE TABLE OF bapimepoitem,
        li_poitemx      TYPE TABLE OF bapimepoitemx,
        li_bapireturn   TYPE TABLE OF bapiret2,

        lwa_po_delete   TYPE t_ekpo,
        lwa_po_distinct TYPE t_ekpo,
        lwa_items       TYPE bapiekpo,
        lwa_poitem      TYPE bapimepoitem,
        lwa_poitemx     TYPE bapimepoitemx,
        lwa_bapireturn  TYPE bapiret2.

  CLEAR: lwa_po_delete,
         lwa_items,
         lwa_poitem,
         lwa_poitemx,
         lwa_bapireturn.

  REFRESH: li_po_distinct,
           li_po_delete.

  li_po_delete[] = p_po_items[].

* *Create an internal table with distinct PO Number.
  li_po_distinct[] = p_po_items[].
  DELETE ADJACENT DUPLICATES FROM li_po_distinct COMPARING ebeln.

  LOOP AT li_po_distinct INTO lwa_po_distinct.

* Get detail for each different PO Number.
    REFRESH: li_items[],
             li_bapireturn[],
             li_poitem[],
             li_poitemx[].

    CALL FUNCTION 'BAPI_PO_GETDETAIL'
      EXPORTING
        purchaseorder   = lwa_po_distinct-ebeln
        items           = 'X'
     TABLES
       PO_ITEMS         = li_items
       RETURN           = li_bapireturn.

    IF li_bapireturn[] IS NOT INITIAL.
      LOOP AT li_bapireturn INTO lwa_bapireturn.
        CASE lwa_bapireturn-type.
          WHEN c_abort.
            sy-subrc = 4.
            EXIT.
          WHEN c_error.
            sy-subrc = 4.
            EXIT.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDIF.

* Set to delete each item found in i_po_delete and li_items.
    IF li_items[] IS NOT INITIAL.
      LOOP AT li_po_delete INTO lwa_po_delete WHERE ebeln = lwa_po_distinct-ebeln.

        READ TABLE li_items INTO lwa_items WITH KEY po_number = lwa_po_delete-ebeln
                                                    po_item   = lwa_po_delete-ebelp.
        IF sy-subrc = 0.
          CLEAR lwa_poitem.
          lwa_poitem-po_item    = lwa_items-po_item.
          lwa_poitem-delete_ind = 'L'.
          APPEND lwa_poitem to li_poitem[].

          CLEAR lwa_poitemx.
          lwa_poitemx-po_item = lwa_items-po_item.
          lwa_poitemx-delete_ind = 'X'.
          APPEND lwa_poitemx to li_poitemx[].
        ENDIF.
      ENDLOOP.

      REFRESH: li_bapireturn.

      CALL FUNCTION 'BAPI_PO_CHANGE'
        EXPORTING
          purchaseorder   = lwa_po_distinct-ebeln
        TABLES
          RETURN          = li_bapireturn
          POITEM          = li_poitem
          POITEMX         = li_poitemx.

      IF li_bapireturn[] IS NOT INITIAL.
        LOOP AT li_bapireturn INTO lwa_bapireturn.
          IF  lwa_bapireturn-type <> c_success
          AND lwa_bapireturn-type <> c_warning.
            PERFORM f_create_log TABLES li_bapireturn[]
                                  USING lwa_items-po_number
                                        lwa_items-po_item
                                        lwa_items-ret_item
                               CHANGING v_subrc.
          ENDIF.
        ENDLOOP.
      ENDIF.
*      ELSE.
      if v_subrc <> 4.
        LOOP AT li_poitemx INTO lwa_poitemx.
          READ TABLE i_po_delete INTO wa_po_delete
            WITH KEY ebeln = lwa_po_distinct-ebeln
                     ebelp = lwa_poitemx-po_item.
          IF sy-subrc = 0.
            wa_po_delete-loekz = 'X'.
            MODIFY i_po_delete FROM wa_po_delete INDEX sy-tabix.
*            WHERE ebeln = lwa_po_distinct-ebeln
*            AND   ebelp = lwa_poitemx-po_item
*
*            TRANSPORTING loekz.
**              WHERE ebeln = wa_po_delete-ebeln
**              AND   ebelp = wa_po_delete-ebelp
**              TRANSPORTING loekz.
          ENDIF.
        ENDLOOP.
*        APPEND lwa_po_distinct to i_po_delete.

        sy-subrc = v_subrc.

        PERFORM f_commit_rollback.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.   "f_deletion_items.

*&---------------------------------------------------------------------*
*&      Form f_no_invoice_posted_orig_po                               *
*&---------------------------------------------------------------------*
*5.6 If credit memo was posted for return PO line with ERS, Check if   *
*    invoice was posted for the original PO line.                      *
*----------------------------------------------------------------------*
FORM f_no_invoice_posted_orig_po.

  REFRESH: i_po_items_gr[].

* Read all RETURN non ERS.
  LOOP AT i_po_items_non_ers INTO wa_po_return.
* *Check for this "return non ers" if their is a "original ers".
    READ TABLE i_po_items_ers INTO wa_po_items WITH KEY ebeln = wa_po_return-ebeln
                                                        retpo = ' '.

    IF sy-subrc = 0.
      "This should not happen. Do nothing in if ever this happens.
*       Yes an invoice was posted for the original PO line
    "??? error message.  NO PO HEADER FOUND
**** ERROR STOP PROCESS
*      EXIT.
    ELSE.
*       No invoice was posted for the original PO line
      APPEND wa_po_return to i_po_items_gr.
    ENDIF.
  ENDLOOP.
ENDFORM.   "f_no_invoice_posted_orig_po

*&---------------------------------------------------------------------*
*&      Form f_deletion_items                                          *
*&---------------------------------------------------------------------*
* 5.7 Reverse GR for original PO line and return PO line.              *
*----------------------------------------------------------------------*
FORM f_reverse_good_receipt.

  CONSTANTS: lc_gr_with_po TYPE GM_CODE    VALUE '01',
             lc_101        TYPE ekbe-bwart VALUE '101',
             lc_102        TYPE ekbe-bwart VALUE '102',
             lc_161        TYPE ekbe-bwart VALUE '161',
             lc_b(1)       TYPE c          VALUE 'B',
             lc_ref_doc_no TYPE XBLNR VALUE space.

  DATA: lv_prev_ebeln  TYPE ekko-ebeln,
        lv_count_items TYPE i,
        lv_items_index TYPE sy-tabix.

  DATA: lo_flcm_exception TYPE REF TO zcx_flcm_error,
        li_item        TYPE TABLE OF BAPI2017_GM_ITEM_CREATE,
        li_return      TYPE TABLE OF bapiret2,

        lwa_gm_header  TYPE BAPI2017_GM_HEAD_01,
        lwa_gm_code    TYPE BAPI2017_GM_CODE,
        lwa_item       TYPE BAPI2017_GM_ITEM_CREATE,
        lwa_po_number  TYPE t_ekpo,

        lv_mat_doc     TYPE BAPI2017_GM_HEAD_RET-MAT_DOC,
        lv_doc_year    TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR.

  CLEAR: lwa_gm_header,
         lwa_gm_code,
         lwa_po_number,
         lv_mat_doc,
         lv_doc_year,
         lv_prev_ebeln.

  REFRESH: li_return[].

  DESCRIBE TABLE i_po_items_gr LINES lv_count_items.
  SORT i_po_items_gr by ebeln
                        retpo.  "to have orig in first.

  LOOP AT i_po_items_gr INTO lwa_po_number.
    lv_items_index = sy-tabix.

    IF lwa_po_number-ebeln <> lv_prev_ebeln
    OR sy-tabix = 1.
      CLEAR lwa_gm_header.

      READ TABLE i_po_header INTO wa_po_header WITH key ebeln = lwa_po_number-ebeln.

      IF sy-subrc = 0.
        lwa_gm_code-gm_code          = lc_gr_with_po.  "01

        lwa_gm_header-PSTNG_DATE     = sy-datum.
        lwa_gm_header-DOC_DATE       = sy-datum.
        lwa_gm_header-REF_DOC_NO     = lc_ref_doc_no.
        lwa_gm_header-BILL_OF_LADING = wa_po_header-ihrez.
      ENDIF.
    ENDIF.

    REFRESH li_item[].

    IF lwa_gm_header IS NOT INITIAL.
      CLEAR: lwa_item.
      READ TABLE i_ekbe INTO wa_ekbe WITH KEY ebeln = lwa_po_number-ebeln
                                              ebelp = lwa_po_number-ebelp.
      IF sy-subrc = 0
      AND ( wa_ekbe-bwart = lc_101
         OR wa_ekbe-bwart = lc_161 ).
        lwa_item-MATERIAL  = lwa_po_number-ematn.
        lwa_item-PLANT     = lwa_po_number-werks.
        lwa_item-STGE_LOC  = lwa_po_number-lgort.
        lwa_item-MOVE_TYPE = lc_102.
        lwa_item-ENTRY_QNT = lwa_po_number-menge.
        lwa_item-ENTRY_UOM = lwa_po_number-meins.
        lwa_item-PO_NUMBER = lwa_po_number-ebeln.
        lwa_item-PO_ITEM   = lwa_po_number-ebelp.
        lwa_item-MVT_IND   = lc_b.

        APPEND lwa_item to li_item.
      ENDIF.
    ENDIF.

    IF  lwa_gm_code   IS NOT INITIAL
    AND lwa_gm_header IS NOT INITIAL
    AND li_item[] IS NOT INITIAL.
*     *COMMIT AND ROLLBACK ARE INCLUDED INTO THE METHOD
      TRY.
      CALL METHOD zcl_flcm_services=>create_goods_mvmt
        EXPORTING
          is_gmcode   = lwa_gm_code
          is_header   = lwa_gm_header
          it_item     = li_item
        IMPORTING
          ed_mat_doc  = lv_mat_doc
          ed_doc_year = lv_doc_year
          et_return   = li_return.
      CATCH zcx_flcm_error INTO lo_flcm_exception.
      ENDTRY.

      IF lo_flcm_exception IS NOT INITIAL.
        CALL METHOD lo_flcm_exception->get_bapimsg_table
          receiving
            rt_return = li_return.
      ENDIF.

      IF li_return[] IS NOT INITIAL.
        perform f_create_log tables li_return[]
                              USING lwa_po_number-ebeln
                                    lwa_po_number-ebelp
                                    lwa_po_number-retpo
                           changing v_subrc.
      ENDIF.
    ENDIF.
    lv_prev_ebeln = lwa_po_number-ebeln.
  ENDLOOP.
ENDFORM.    "f_reverse_good_receipt

*&---------------------------------------------------------------------*
*&      Form f_commit_rollback d_orig_po                               *
*&---------------------------------------------------------------------*
FORM f_commit_rollback.

  IF sy-subrc = 0.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting wait = 'X'.
  ELSE.
    sy-subrc = 4.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
ENDFORM.  "f_commit_rollback

*&---------------------------------------------------------------------*
*&      Form f_create_log                                              *
*&---------------------------------------------------------------------*
* 5.8 Keep log of error messages                                       *
* a = abort                                                            *
* e = error                                                            *
* i = info                                                             *
* w = warning                                                          *
** Sucess is not process                                               *
* s = success                                                          *
*&---------------------------------------------------------------------*
FORM f_create_log TABLES p_bapireturn
                   USING p_items-po_number
                         p_items-po_item
                         p_items-ret_item
                CHANGING p_subrc.

  DATA: li_bapireturn  TYPE TABLE OF BAPIRET2,
        lwa_bapireturn TYPE BAPIRET2.

  CONSTANTS: lc_space(1) TYPE c VALUE ' '.

  CLEAR: lwa_bapireturn,
         wa_log,
         p_subrc.

  REFRESH: li_bapireturn.

  li_bapireturn[] = p_bapireturn[].

  LOOP AT li_bapireturn INTO lwa_bapireturn.
    IF  lwa_bapireturn-type <> c_success
    AND lwa_bapireturn-type <> c_warning.

      READ TABLE i_po_header INTO wa_po_header WITH KEY ebeln = p_items-PO_NUMBER.
      IF sy-subrc = 0.
        CONCATENATE lwa_bapireturn-type
                    sy-datum
                    p_items-PO_NUMBER
                    p_items-PO_ITEM
                    p_items-RET_ITEM
                    wa_po_header-lifnr
                    wa_po_header-ihrez
                    wa_po_header-unsez
                    lwa_bapireturn-message
               INTO wa_log-message SEPARATED BY lc_space.
        APPEND wa_log to i_log.
      ENDIF.

      IF lwa_bapireturn-type = c_error
      OR lwa_bapireturn-type = c_abort.
        p_subrc = 4.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF i_log[] IS NOT INITIAL.
    SORT i_log.   "to have first Abort and Error..
  ENDIF.
ENDFORM.  "f_create_log.


FORM f_alv_delete_result.

  DATA: lr_alv      TYPE REF TO cl_salv_table,
        lr_header   TYPE REF TO cl_salv_form_header_info,
        lr_columns  TYPE REF TO cl_salv_columns_table,  "Reference on all Columns
        lo_salv_msg TYPE REF TO cx_salv_msg,
        lv_title_header(80) TYPE c.

*Create the ALV reference to po_delete table
  TRY.
  CALL METHOD cl_salv_table=>factory
*    EXPORTING
*      list_display   =
*      r_container    = ref_alv
*      container_name =
    IMPORTING
      r_salv_table   = lr_alv
    CHANGING
      t_table        = i_po_delete[].
   CATCH cx_salv_msg INTO lo_salv_msg.
  ENDTRY.

  IF lo_salv_msg IS NOT INITIAL.
    sy-subrc = v_subrc.
    perform f_commit_rollback.
  ENDIF.

* Create an object of cl_salv_form_header_info with the Header Literal
  lv_title_header = text-t01.

  create object lr_header
    exporting
      text = lv_title_header.

  lr_alv->set_top_of_list( lr_header ).

* For Optimize the width of all columnS.
  lr_columns = lr_alv->get_columns( ).   "Put a reference on all columns
  IF lr_columns IS NOT INITIAL.
    lr_columns->set_optimize( 'X').
  ENDIF.

*Show ALV GRID
  lr_alv->display( ).

ENDFORM.       "f_alv_delete_result

*--> Start of insert DV5K9A05WW - Chuong Le - 2017/01/04
*&---------------------------------------------------------------------*
*&      Form  F_GET_PO_HEADER_TEXT
*&---------------------------------------------------------------------*
FORM f_get_po_header_text USING    p_ebeln
                          CHANGING p_text.

    TYPES:
      BEGIN OF text_type,
        ebeln  TYPE ebeln,
        tdline TYPE tdline,
      END OF text_type.

    STATICS:
      lt_texttab TYPE SORTED TABLE OF text_type WITH UNIQUE KEY ebeln.

    DATA:
      lt_lines   TYPE tline_tab.

    CLEAR p_text.

    IF line_exists( lt_texttab[ ebeln = p_ebeln ] ).
      p_text = lt_texttab[ ebeln = p_ebeln ]-tdline.
    ELSE.
      CLEAR lt_lines[].
      "Read English text
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = 'F99'
          language                = 'E'
          name                    = CONV tdobname( p_ebeln )
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
            name                    = CONV tdobname( p_ebeln )
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
        p_text = lt_lines[ 1 ]-tdline.     "1st line only
      ENDIF.

      "Add to PO text table
      lt_texttab = VALUE #( BASE lt_texttab ( ebeln  = p_ebeln
                                              tdline = p_text ) ).
    ENDIF.

ENDFORM.
*<-- End of insert DV5K9A05WW - Chuong Le - 2017/01/04
