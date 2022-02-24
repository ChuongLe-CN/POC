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
* Jose Luis Banda                  2016-03-31          DV5K9A01FD      *
*                                                                      *
* Short Description : C292276-T325274                                  *
*      PRD issue with the discount amount.                             *
*----------------------------------------------------------------------*
* Charles Darby                    2014-11-13          DV5K991322      *
*                                                                      *
* Short Description : C255559-T277979                                  *
*                     the program doesn't take in consideration that   *
*                     the cie 1000 is paying for 1236 for instance     *
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
* Chuong Le                        2011-08-19          DV5K965748      *
*                                                                      *
* Short desc: Defect 456 - Fix duplicate data for cross-company        *
*                          payments.                                   *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_GET_VENDOR_DATA
*&---------------------------------------------------------------------*
FORM f_get_vendor_data .

* Get vendor master data
  SELECT lifnr
         land1
         name1
         pstlz
         regio
         stras
         mcod3
    INTO TABLE i_lfa1
    FROM lfa1
    WHERE lifnr IN s_lifnr.

ENDFORM.                    " F_GET_VENDOR_DATA

*&---------------------------------------------------------------------*
*&      Form  F_GET_PAYMENT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_get_payment_data .

* Get all payments
  SELECT lifnr bukrs augbl augdt gjahr belnr buzei blart
         xblnr bldat bschl wrbtr waers sknto wskto
    INTO TABLE i_bsak_hdr
    FROM bsak
    WHERE bukrs IN s_bukrs
      AND lifnr IN s_lifnr
      AND augdt IN s_augdt
      AND augbl IN s_augbl
      AND gjahr IN s_gjahr.

  DELETE:
    i_bsak_hdr WHERE blart <> c_kx AND bukrs IN r_compcode_us,
    i_bsak_hdr WHERE blart <> c_kz AND bukrs NOT IN r_compcode_us.

  IF i_bsak_hdr[] IS INITIAL.
    RETURN.
  ENDIF.

* Get invoices for cross-company payments
  i_bsak_kx[] = i_bsak_hdr[].
  DELETE i_bsak_kx WHERE blart <> c_kx.
  IF NOT i_bsak_kx[] IS INITIAL.
    PERFORM f_get_cross_payment_data TABLES i_bsak_kx.
  ENDIF.

* Get invoices for non cross-company payments
  i_bsak_kz[] = i_bsak_hdr[].
  DELETE i_bsak_kz WHERE blart <> c_kz.
  IF NOT i_bsak_kz[] IS INITIAL.
    PERFORM f_get_regular_payment_data TABLES i_bsak_kz.
  ENDIF.

* Get vendor's email address
  SELECT lifnr bukrs intad
    INTO TABLE i_email
    FROM lfb1
    FOR ALL ENTRIES IN i_bsak_hdr
    WHERE lifnr  = i_bsak_hdr-lifnr
      AND bukrs  = i_bsak_hdr-bukrs
      AND intad <> space.

  SELECT lifnr bukrs intad
    APPENDING TABLE i_email
    FROM lfb1
    WHERE lifnr IN s_lifnr
      AND bukrs  = c_2010
      AND intad <> space.

* Extract PO data
  IF i_bsak_itm[] IS NOT INITIAL.
    SELECT bukrs belnr gjahr buzei buzid ebeln ebelp
    INTO TABLE i_bseg_po
    FROM bseg
    FOR ALL ENTRIES IN i_bsak_itm
    WHERE bukrs = i_bsak_itm-bukrs
      AND belnr = i_bsak_itm-belnr
      AND gjahr = i_bsak_itm-gjahr
      AND buzid = 'W'.

    IF sy-subrc = 0.
      SELECT ebeln ebelp txz01 bednr
      INTO TABLE i_ekpo
      FROM ekpo
      FOR ALL ENTRIES IN i_bseg_po
      WHERE ebeln = i_bseg_po-ebeln
        AND ebelp = i_bseg_po-ebelp.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GET_PAYMENT_DATA

*&---------------------------------------------------------------------*
*&      Form  F_GET_CROSS_PAYMENT_DATA
*&---------------------------------------------------------------------*
FORM f_get_cross_payment_data TABLES pi_bsak_kx STRUCTURE wa_bsak.

  DATA: li_bvor_kx TYPE STANDARD TABLE OF t_bvor,
        li_bvor_kz TYPE STANDARD TABLE OF t_bvor.

* Get the Cross-company codes for all KX payment records
  SELECT a~bukrs a~belnr a~gjahr a~blart a~bvorg
         b~bukrs b~belnr b~gjahr
  INTO TABLE i_bvor
  FROM bkpf AS a INNER JOIN bvor AS b ON a~bvorg = b~bvorg
  FOR ALL ENTRIES IN pi_bsak_kx
  WHERE a~bukrs  = pi_bsak_kx-bukrs
    AND a~belnr  = pi_bsak_kx-belnr
    AND a~gjahr  = pi_bsak_kx-gjahr
    AND a~budat IN s_augdt.

*---> Start of DV5K965748 - Insert - Chuong Le  2011/08/19
  SORT i_bvor BY bvorg bukrs2 belnr2 gjahr2.
  DELETE ADJACENT DUPLICATES FROM i_bvor
                  COMPARING bvorg bukrs2 belnr2 gjahr2.
*<--- End of DV5K965748 - Insert - Chuong Le  2011/08/19

* Every set of documents that relate to the same Cross-comp code no.
* is a set of KZ type payment created in company code 2010 and
* related KX type interco-payments in one or all company codes
* (2070, 2160 and 2355).

* Get required info for cross-co KZ payments
  li_bvor_kz[] = i_bvor[].
  DELETE li_bvor_kz WHERE ( bukrs2 <> c_2010 AND bukrs2 <> c_1000 ). "U-DV5K991322
  IF NOT li_bvor_kz[] IS INITIAL.
    SELECT bukrs belnr gjahr blart bldat waers
    INTO TABLE i_bkpf
    FROM bkpf
    FOR ALL ENTRIES IN li_bvor_kz
    WHERE bukrs = li_bvor_kz-bukrs2
      AND belnr = li_bvor_kz-belnr2
      AND gjahr = li_bvor_kz-gjahr2.

    IF NOT i_bkpf[] IS INITIAL.
      SELECT bukrs belnr gjahr buzei bschl wrbtr valut
      INTO TABLE i_bseg
      FROM bseg
      FOR ALL ENTRIES IN i_bkpf
      WHERE bukrs = i_bkpf-bukrs
        AND belnr = i_bkpf-belnr
        AND gjahr = i_bkpf-gjahr
        AND shkzg = c_h.
    ENDIF.
  ENDIF.

* Get required info for cross-co non-KZ payments
  li_bvor_kx[] = i_bvor[].
  DELETE li_bvor_kx WHERE ( bukrs2 = c_2010 OR bukrs2 = c_1000 ). "U-DV5K991322
  IF NOT li_bvor_kx[] IS INITIAL.
    SELECT lifnr bukrs augbl augdt gjahr belnr buzei blart
           xblnr bldat bschl wrbtr waers sknto wskto
    APPENDING TABLE i_bsak_itm
    FROM bsak
    FOR ALL ENTRIES IN li_bvor_kx
    WHERE lifnr IN s_lifnr
      AND bukrs  = li_bvor_kx-bukrs2
      AND augbl  = li_bvor_kx-belnr2
      AND augdt IN s_augdt
      AND blart <> c_kx.

*   Get company name
    SELECT bukrs butxt INTO TABLE i_t001
    FROM t001
    FOR ALL ENTRIES IN li_bvor_kx
    WHERE bukrs = li_bvor_kx-bukrs2.
  ENDIF.

ENDFORM.                    " F_GET_CROSS_PAYMENT_DATA

*&---------------------------------------------------------------------*
*&      Form  F_GET_REGULAR_PAYMENT_DATA
*&---------------------------------------------------------------------*
FORM f_get_regular_payment_data TABLES pi_bsak_kz STRUCTURE wa_bsak.

* Get required info for regular non-KZ payments
  SELECT lifnr bukrs augbl augdt gjahr belnr buzei blart
         xblnr bldat bschl wrbtr waers sknto wskto
    APPENDING TABLE i_bsak_itm
    FROM bsak
    FOR ALL ENTRIES IN pi_bsak_kz
    WHERE lifnr  = pi_bsak_kz-lifnr
      AND bukrs  = pi_bsak_kz-bukrs
      AND augbl  = pi_bsak_kz-belnr
      AND augdt  = pi_bsak_kz-augdt
      AND blart <> c_kz.

  LOOP AT pi_bsak_kz INTO wa_bsak.
*   Generate vendor and company code table
    READ TABLE i_vend_comp INTO wa_vend_comp
                           WITH KEY lifnr = wa_bsak-lifnr
                                    bukrs = wa_bsak-bukrs
                                    BINARY SEARCH.
    IF sy-subrc <> 0.
      CLEAR wa_vend_comp.
      wa_vend_comp-lifnr = wa_bsak-lifnr.
      wa_vend_comp-bukrs = wa_bsak-bukrs.
      INSERT wa_vend_comp INTO TABLE i_vend_comp.
    ENDIF.

*   As per defect 70, sum up all lines in BSAK for the same KZ number.
    CLEAR wa_kz_sum.
    MOVE-CORRESPONDING wa_bsak TO wa_kz_sum.
    COLLECT wa_kz_sum INTO i_kz_sum.
  ENDLOOP.

ENDFORM.                    " F_GET_REGULAR_PAYMENT_DATA

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_DATA
*&---------------------------------------------------------------------*
FORM f_process_data .

  DATA: l_error_msg   TYPE text60,
        l_cross_pmt   TYPE c VALUE space,
        l_regular_pmt TYPE c VALUE space.

  SORT: i_lfa1      BY lifnr,
        i_t001      BY bukrs,
        i_email     BY lifnr bukrs,
        i_bkpf      BY bukrs belnr gjahr,
        i_bseg      BY bukrs belnr gjahr,
        i_bseg_po   BY bukrs belnr gjahr,
        i_ekpo      BY ebeln ebelp,
        i_bvor      BY bukrs belnr gjahr bukrs2,
        i_kz_sum    BY lifnr bukrs augbl augdt belnr gjahr,
        i_bsak_kz   BY lifnr bukrs augbl augdt belnr gjahr buzei,
        i_bsak_itm  BY lifnr bukrs augbl augdt belnr gjahr buzei.

* Keep only the first line for every KZ record.
  DELETE ADJACENT DUPLICATES FROM i_bsak_kz
         COMPARING lifnr bukrs augbl augdt belnr gjahr.

  LOOP AT s_bukrs.
    IF s_bukrs-low IN r_compcode_us.
      l_cross_pmt = c_x.
    ELSE.
      l_regular_pmt = c_x.
    ENDIF.
  ENDLOOP.

  LOOP AT s_lifnr.
    CLEAR wa_lfa1.
    READ TABLE i_lfa1 INTO wa_lfa1 WITH KEY lifnr = s_lifnr-low
                                   BINARY SEARCH.
    IF sy-subrc = 0.
      IF l_cross_pmt = c_x.
        PERFORM f_process_cross_payments USING  wa_lfa1.
        SKIP.
      ENDIF.

      IF l_regular_pmt = c_x.
        PERFORM f_process_regular_payments USING i_vend_comp
                                                 wa_lfa1.
        SKIP.
      ENDIF.
    ELSE.
*     Vendor does not exist
      MESSAGE i054(vk) WITH s_lifnr-low.
      MESSAGE i054(vk) INTO l_error_msg WITH s_lifnr-low.
      CONCATENATE c_stars l_error_msg c_stars INTO l_error_msg
                                              SEPARATED BY space.
      WRITE:/ 'Vendor:'(002), s_lifnr-low.
      WRITE:/ l_error_msg COLOR COL_NEGATIVE.
    ENDIF.
    ULINE.
  ENDLOOP.

ENDFORM.                    " F_PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_CROSS_PAYMENTS
*&---------------------------------------------------------------------*
*& Note: report and email will be generated for every Vendor.
*&---------------------------------------------------------------------*
FORM f_process_cross_payments  USING  ps_lfa1 STRUCTURE wa_lfa1.

  DATA: lwa_bvor       TYPE t_bvor,
        lwa_bvor_kx    TYPE t_bvor,
        lwa_bsak_tmp   TYPE t_bsak,
        l_error_msg    TYPE text60,
        l_vendor_email TYPE text70,
        l_header_found TYPE c,
        lwa_cross_no   TYPE t_cross_no,
        lwa_bkpf       TYPE t_bkpf,
        lwa_bseg       TYPE t_bseg,
        i_bvor_tmp     TYPE STANDARD TABLE OF t_bvor.      "I-DV5K991322

  CLEAR l_header_found.

  REFRESH i_attachment.

* Display vendor data
  PERFORM f_display_vendor_data USING ps_lfa1
                                      c_2010
                             CHANGING l_vendor_email.

  LOOP AT i_bsak_kx INTO wa_bsak WHERE lifnr = ps_lfa1-lifnr.
    CLEAR lwa_bvor.
    i_bvor_tmp[] = i_bvor[].                       "I-DV5K991322
    DELETE i_bvor_tmp WHERE ( bukrs2 <> c_2010 ) AND ( bukrs2 <> c_1000 ). "I-DV5K991322
    READ TABLE i_bvor_tmp INTO lwa_bvor WITH KEY bukrs  = wa_bsak-bukrs
                                             belnr  = wa_bsak-belnr
                                             gjahr  = wa_bsak-gjahr
                                             "bukrs2 = c_2010
                                             BINARY SEARCH.
    IF sy-subrc = 0.
*     Check if the cross-company code has already been processed?
      READ TABLE i_cross_no INTO lwa_cross_no
                            WITH KEY bvorg = lwa_bvor-bvorg
                                     BINARY SEARCH.
      IF sy-subrc = 0.
        CONTINUE.
      ELSE.
        CLEAR lwa_cross_no.
        lwa_cross_no-bvorg = lwa_bvor-bvorg.
        INSERT lwa_cross_no INTO TABLE i_cross_no.
      ENDIF.

      l_header_found = c_x.

      CLEAR lwa_bkpf.
      READ TABLE i_bkpf INTO lwa_bkpf WITH KEY bukrs  = lwa_bvor-bukrs2
                                               belnr  = lwa_bvor-belnr2
                                               gjahr  = lwa_bvor-gjahr2
                                               BINARY SEARCH.

      CLEAR lwa_bseg.
      READ TABLE i_bseg INTO lwa_bseg WITH KEY bukrs  = lwa_bkpf-bukrs
                                               belnr  = lwa_bkpf-belnr
                                               gjahr  = lwa_bkpf-gjahr
                                               BINARY SEARCH.
*     Display payment header data
      CLEAR lwa_bsak_tmp.
      lwa_bsak_tmp-belnr = lwa_bvor-belnr2.
      lwa_bsak_tmp-augdt = lwa_bkpf-bldat.
      lwa_bsak_tmp-waers = lwa_bkpf-waers.
      lwa_bsak_tmp-wrbtr = lwa_bseg-wrbtr.
      lwa_bsak_tmp-bschl = lwa_bseg-bschl.
      PERFORM f_display_payment_header_data USING lwa_bsak_tmp
                                                  l_vendor_email
                                                  lwa_bseg-valut
                                                  abap_true.

      LOOP AT i_bvor INTO lwa_bvor_kx WHERE bvorg   = lwa_bvor-bvorg
                                        AND bukrs2 <> lwa_bvor-bukrs2.
        LOOP AT i_bsak_itm INTO wa_bsak_itm
                           WHERE lifnr = ps_lfa1-lifnr
                             AND bukrs = lwa_bvor_kx-bukrs2
                             AND augbl = lwa_bvor_kx-belnr2.
*         Display payment item data
          PERFORM f_display_payment_item_data USING wa_bsak_itm
                                                    lwa_bvor-belnr2
                                                    l_vendor_email
                                                    abap_true.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  IF l_header_found IS INITIAL.
*   No payment was found for vendor &
    MESSAGE i066(zz_flcm) WITH ps_lfa1-lifnr.
    MESSAGE i066(zz_flcm) INTO l_error_msg WITH ps_lfa1-lifnr.
    CONCATENATE c_stars l_error_msg c_stars INTO l_error_msg
                                            SEPARATED BY space.
    WRITE:/ l_error_msg COLOR COL_NEGATIVE.
  ELSE.
*   Send email if the email option is selected and
*   the email address is valid.
    IF v_email_autho = abap_true AND l_vendor_email IS NOT INITIAL.
      PERFORM f_send_email USING ps_lfa1
                                 l_vendor_email.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_PROCESS_CROSS_PAYMENTS

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_REGULAR_PAYMENTS
*&---------------------------------------------------------------------*
*& Note: report and email will be generated for every Vendor and
*&       Company code combination.
*&---------------------------------------------------------------------*
FORM f_process_regular_payments
                     USING pi_vend_comp LIKE i_vend_comp
                           ps_lfa1      STRUCTURE wa_lfa1.

  DATA: l_error_msg    TYPE text60,
        l_vendor_email TYPE text70,
        l_header_found TYPE c.

  LOOP AT pi_vend_comp INTO wa_vend_comp WHERE lifnr = ps_lfa1-lifnr.
    REFRESH i_attachment.
*   Display vendor data
    PERFORM f_display_vendor_data USING ps_lfa1
                                        wa_vend_comp-bukrs
                               CHANGING l_vendor_email.

    CLEAR l_header_found.
    LOOP AT i_bsak_kz INTO wa_bsak
                      WHERE lifnr = ps_lfa1-lifnr
                        AND bukrs = wa_vend_comp-bukrs.
      l_header_found = c_x.
*     Display payment header data
      PERFORM f_display_payment_header_data USING wa_bsak
                                                  l_vendor_email
                                                  space
                                                  abap_false.

      LOOP AT i_bsak_itm INTO wa_bsak_itm
                         WHERE lifnr = ps_lfa1-lifnr
                           AND bukrs = wa_vend_comp-bukrs
                           AND augbl = wa_bsak-belnr.
*       Display payment item data
        PERFORM f_display_payment_item_data USING wa_bsak_itm
                                                  wa_bsak-belnr
                                                  l_vendor_email
                                                  abap_false.
      ENDLOOP.
    ENDLOOP.

    IF l_header_found IS INITIAL.
*     No payment was found for vendor &
      MESSAGE i066(zz_flcm) WITH ps_lfa1-lifnr.
      MESSAGE i066(zz_flcm) INTO l_error_msg WITH ps_lfa1-lifnr.
      CONCATENATE c_stars l_error_msg c_stars INTO l_error_msg
                                              SEPARATED BY space.
      WRITE:/ l_error_msg COLOR COL_NEGATIVE.
    ELSE.
*     Send email if the email option is selected and
*     the email address is valid.
      IF v_email_autho = abap_true AND l_vendor_email IS NOT INITIAL.
        PERFORM f_send_email USING ps_lfa1
                                   l_vendor_email.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_PROCESS_REGULAR_PAYMENTS

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_VENDOR_DATA
*&---------------------------------------------------------------------*
FORM f_display_vendor_data  USING    ps_lfa1 STRUCTURE wa_lfa1
                                     p_compcode
                            CHANGING p_vendor_email.

  DATA: l_error_msg   TYPE text60,
        l_vendor_addr TYPE text70,
        lwa_email     TYPE t_email.

  CLEAR p_vendor_email.

  CONCATENATE ps_lfa1-stras
              ps_lfa1-mcod3
              ps_lfa1-regio
              ps_lfa1-pstlz
         INTO l_vendor_addr SEPARATED BY space.

  WRITE:/ 'Vendor:'(002), ps_lfa1-lifnr, ps_lfa1-name1.
  WRITE:/ 'Vendor Address:'(003), l_vendor_addr.
  WRITE:/ 'Vendor Email:'(004).

  CLEAR lwa_email.
  READ TABLE i_email INTO lwa_email WITH KEY lifnr = ps_lfa1-lifnr
                                             bukrs = p_compcode
                                             BINARY SEARCH.
  IF sy-subrc = 0.
    p_vendor_email = lwa_email-intad.
    WRITE p_vendor_email.
  ELSE.
*   No email address was found for vendor &
    MESSAGE i049(zz_flcm) WITH ps_lfa1-lifnr.
    MESSAGE i049(zz_flcm) INTO l_error_msg WITH ps_lfa1-lifnr.
    CONCATENATE c_stars l_error_msg c_stars INTO l_error_msg
                                            SEPARATED BY space.
    WRITE:/ l_error_msg COLOR COL_NEGATIVE.
    RETURN.
  ENDIF.

* Generate email attachment
  IF v_email_autho = abap_true AND p_vendor_email IS NOT INITIAL.
    CONCATENATE c_a
                'EFT Remittance Advice'(024)
           INTO wa_attachment SEPARATED BY c_comma.
    APPEND wa_attachment TO i_attachment.

    CONCATENATE c_a
                'CN - Accounts Payable'(019)
           INTO wa_attachment SEPARATED BY c_comma.
    APPEND wa_attachment TO i_attachment.

    CONCATENATE c_a
                'P.O. Box 8103 Montreal Quebec H3C 3N3'(026)
           INTO wa_attachment SEPARATED BY c_comma.
    APPEND wa_attachment TO i_attachment.

    CONCATENATE c_a
                '(514) 399-4700'(023)
           INTO wa_attachment SEPARATED BY c_comma.
    APPEND wa_attachment TO i_attachment.

    CONCATENATE c_a
                ps_lfa1-lifnr
                ps_lfa1-name1
           INTO wa_attachment SEPARATED BY c_comma.
    APPEND wa_attachment TO i_attachment.

    PERFORM f_check_comma CHANGING l_vendor_addr.
    CONCATENATE c_a
                l_vendor_addr
           INTO wa_attachment SEPARATED BY c_comma.
    APPEND wa_attachment TO i_attachment.

    CONCATENATE c_a
                p_vendor_email
           INTO wa_attachment SEPARATED BY c_comma.
    APPEND wa_attachment TO i_attachment.
  ENDIF.

ENDFORM.                    " F_DISPLAY_VENDOR_DATA

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_PAYMENT_HEADER_DATA
*&---------------------------------------------------------------------*
FORM f_display_payment_header_data
                         USING  ps_header STRUCTURE wa_bsak
                                p_vendor_email
                                p_value_date
                                p_cross_comp.

  DATA: l_value_date TYPE sy-datum,
        l_wrbtr      TYPE wrbtr,
        l_char_amt   TYPE char17,
        l_date       TYPE char10.

  IF p_cross_comp = abap_true.     "Cross-company payment
    l_value_date = p_value_date.
    l_wrbtr      = ps_header-wrbtr.
  ELSE.
    l_value_date = ps_header-augdt + 2.
    CLEAR wa_kz_sum.
    READ TABLE i_kz_sum INTO wa_kz_sum WITH KEY lifnr = ps_header-lifnr
                                                bukrs = ps_header-bukrs
                                                augbl = ps_header-augbl
                                                augdt = ps_header-augdt
                                                belnr = ps_header-belnr
                                                gjahr = ps_header-gjahr
                                                BINARY SEARCH.
    l_wrbtr = wa_kz_sum-wrbtr.
  ENDIF.

* Display as negative values
  IF ps_header-bschl = c_35 OR ps_header-bschl = c_37 OR
     ps_header-bschl = c_38 OR ps_header-bschl = c_40.
    l_wrbtr = - l_wrbtr.
  ENDIF.

  SKIP.
  WRITE:/ 'Processing Date:'(005), ps_header-augdt,
        / 'Value Date:'(006), l_value_date,
        / 'Reference:'(007), ps_header-belnr,
        / 'Payment Amount:'(008), l_wrbtr, ps_header-waers.

  SKIP.
  WRITE:/4 'Document No'(009),
        20 'Invoice No'(010),
        40 'Invoice Date'(011),
        58 'Gross Amount'(012),
        80 'Discount'(013),
        95 'Net Amount'(014),
       113 'Payment Reference'(015),
       136 'Tracking No'(035),
       153 'Product/Service'(036),
       193 'Paid on behalf'(029).

* Generate email attachment
  IF v_email_autho = abap_true AND p_vendor_email IS NOT INITIAL.
    WRITE ps_header-augdt TO l_date USING EDIT MASK '____/__/__'.
    CONCATENATE c_b
                'Processing Date:'(005)
                l_date
           INTO wa_attachment SEPARATED BY c_comma.
    APPEND wa_attachment TO i_attachment.

    WRITE l_value_date TO l_date USING EDIT MASK '____/__/__'.
    CONCATENATE c_b
                'Value Date:'(006)
                l_date
           INTO wa_attachment SEPARATED BY c_comma.
    APPEND wa_attachment TO i_attachment.

    CONCATENATE c_b
                'Reference:'(007)
                ps_header-belnr
           INTO wa_attachment SEPARATED BY c_comma.
    APPEND wa_attachment TO i_attachment.

    PERFORM f_format_amount_field USING    l_wrbtr
                                  CHANGING l_char_amt.
    CONCATENATE c_b
                'Payment Amount:'(008)
                l_char_amt
                ps_header-waers
           INTO wa_attachment SEPARATED BY c_comma.
    APPEND wa_attachment TO i_attachment.

    CONCATENATE c_c
                'Document No'(009)
                'Invoice No'(010)
                'Invoice Date'(011)
                'Gross Amount'(012)
                'Discount'(013)
                'Net Amount'(014)
                'Payment Reference'(015)
                'Tracking No'(035)
                'Product/Service'(036)
                'Paid on Behalf'(029)
           INTO wa_attachment SEPARATED BY c_comma.
    APPEND wa_attachment TO i_attachment.
  ENDIF.

ENDFORM.                    " F_DISPLAY_PAYMENT_HEADER_DATA

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_PAYMENT_ITEM_DATA
*&---------------------------------------------------------------------*
FORM f_display_payment_item_data
                         USING  ps_item STRUCTURE wa_bsak_itm
                                p_reference
                                p_vendor_email
                                p_cross_comp.

  DATA: l_discount    TYPE wrbtr,
        l_net_amt     TYPE wrbtr,
        l_char_gross  TYPE char17,
        l_char_disc   TYPE char17,
        l_char_net    TYPE char17,
        l_butxt       TYPE butxt,
        l_date        TYPE char10,
        l_prod_serv   TYPE txz01,
        l_tracking_no TYPE bednr,
        lwa_t001      TYPE t_t001.

  l_discount = ps_item-wskto.   "+ ps_item-sknto    "DV5K9A01FD
  l_net_amt  = ps_item-wrbtr - l_discount.

* Display as negative values
  IF ps_item-bschl = c_21 OR ps_item-bschl = c_27.
    ps_item-wrbtr = - ps_item-wrbtr.
    l_net_amt     = - l_net_amt.
  ELSE.
    l_discount = - l_discount.
  ENDIF.

* Get company name
  IF p_cross_comp = abap_true.
    CLEAR lwa_t001.
    READ TABLE i_t001 INTO lwa_t001 WITH KEY bukrs = ps_item-bukrs
                                    BINARY SEARCH.
    l_butxt = lwa_t001-butxt.
  ELSE.
    CLEAR l_butxt.
  ENDIF.

* Get product service and tracking number
  PERFORM f_get_po_data USING    ps_item-bukrs
                                 ps_item-belnr
                                 ps_item-gjahr
                        CHANGING l_prod_serv
                                 l_tracking_no.

  WRITE:/ ps_item-belnr UNDER text-009,
          ps_item-xblnr UNDER text-010,
          ps_item-bldat UNDER text-011,
       55 ps_item-wrbtr,
       73 l_discount,
       90 l_net_amt,
          p_reference   UNDER text-015,
          l_tracking_no UNDER text-035,
          l_prod_serv   UNDER text-036,
          l_butxt       UNDER text-029.

* Generate email attachment
  IF v_email_autho = abap_true AND p_vendor_email IS NOT INITIAL.
    PERFORM: f_format_amount_field USING    ps_item-wrbtr
                                   CHANGING l_char_gross,
             f_format_amount_field USING    l_discount
                                   CHANGING l_char_disc,
             f_format_amount_field USING    l_net_amt
                                   CHANGING l_char_net.
    PERFORM: f_check_comma CHANGING l_butxt,
             f_check_comma CHANGING l_prod_serv.

    WRITE ps_item-bldat TO l_date USING EDIT MASK '____/__/__'.
    CONCATENATE c_c
                ps_item-belnr
                ps_item-xblnr
                l_date
                l_char_gross
                l_char_disc
                l_char_net
                p_reference
                l_tracking_no
                l_prod_serv
                l_butxt
           INTO wa_attachment SEPARATED BY c_comma.
    APPEND wa_attachment TO i_attachment.
  ENDIF.

ENDFORM.                    " F_DISPLAY_PAYMENT_ITEM_DATA

*&---------------------------------------------------------------------*
*&      Form  F_CHECK_COMMA
*&---------------------------------------------------------------------*
FORM f_check_comma  CHANGING p_value.

  CHECK p_value IS NOT INITIAL.

  IF p_value CA c_comma.
    CONCATENATE c_quote p_value c_quote INTO p_value.
  ENDIF.

ENDFORM.                    " F_CHECK_COMMA

*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_AMOUNT_FIELD
*&---------------------------------------------------------------------*
FORM f_format_amount_field  USING    p_amount
                            CHANGING p_value_char.

  CLEAR p_value_char.

* Display negative sign in front of a number
  WRITE p_amount TO p_value_char LEFT-JUSTIFIED NO-SIGN.
  IF p_amount < 0.
    CONCATENATE c_dash p_value_char INTO p_value_char.
  ENDIF.

  PERFORM f_check_comma CHANGING p_value_char.

ENDFORM.                    " F_FORMAT_AMOUNT_FIELD

*&---------------------------------------------------------------------*
*&      Form  F_CHECK_EMAIL_AUTHORIZATION
*&---------------------------------------------------------------------*
FORM f_check_email_authorization  CHANGING p_email_autho
                                           p_exit.

  DATA l_question TYPE text100.

* Check authorization?
  AUTHORITY-CHECK OBJECT 'ZF_7HAMAIL'
                  ID     'ACTVT'
                  FIELD  'A9'.         "Send email

  IF sy-subrc = 0.
    IF sy-sysid = c_prd AND sy-uname(7) <> c_espoper.
      l_question = 'Are you sure that you want to send email?'(c01).
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Send Email?'(c00)
          text_question         = l_question
          text_button_1         = 'Yes'(c02)
          icon_button_1         = 'ICON_OKAY'
          text_button_2         = 'No'(c03)
          icon_button_2         = 'ICON_CANCEL'
          display_cancel_button = abap_false
        IMPORTING
          answer                = v_answer.

      IF v_answer = '1'.                 "Yes
        p_email_autho = abap_true.
      ELSE.                              "No
        p_exit = abap_true.
      ENDIF.
    ELSE.
      p_email_autho = abap_true.
    ENDIF.

  ELSE.
    IF sy-sysid = c_prd AND sy-uname(7) = c_espoper.
*     No authorization to send emails
      MESSAGE i064(zz_flcm).
    ELSE.
      CONCATENATE 'You do not have the authorization to send email.'(c04)
                  'Only report can be displayed, continue?'(c05)
             INTO l_question SEPARATED BY space.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Send Email?'(c00)
          text_question         = l_question
          text_button_1         = 'Yes'(c02)
          icon_button_1         = 'ICON_OKAY'
          text_button_2         = 'No'(c03)
          icon_button_2         = 'ICON_CANCEL'
          display_cancel_button = abap_false
        IMPORTING
          answer                = v_answer.

      IF v_answer = '2'.        "No
        p_exit = abap_true.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_CHECK_EMAIL_AUTHORIZATION

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_EMAIL_CONTENTS
*&---------------------------------------------------------------------*
FORM f_build_email_contents  TABLES   pi_contents STRUCTURE soli
                             USING    p_vendor STRUCTURE wa_lfa1.

  DATA: l_date        TYPE char10,
        l_vendor_addr TYPE text70.

  APPEND: c_cn TO pi_contents,
          INITIAL LINE TO pi_contents.

  APPEND: p_vendor-name1 TO pi_contents,
          INITIAL LINE TO pi_contents.

  CONCATENATE p_vendor-stras
              p_vendor-mcod3
              p_vendor-regio
              p_vendor-pstlz
         INTO l_vendor_addr SEPARATED BY space.

  APPEND: l_vendor_addr TO pi_contents,
          INITIAL LINE TO pi_contents.

  WRITE sy-datum TO l_date USING EDIT MASK '____/__/__'.
  APPEND: l_date TO pi_contents,
          INITIAL LINE TO pi_contents,
          INITIAL LINE TO pi_contents.

  APPEND: 'Please find attached a payment remittance advice to update your records.'(016)
           TO pi_contents,
          'Feel free to contact us if you have any questions.'(017)
           TO pi_contents,
           INITIAL LINE TO pi_contents.

  APPEND: 'Thank you,'(018) TO pi_contents,
           INITIAL LINE TO pi_contents.

  APPEND: 'CN - Accounts Payable' TO pi_contents,
           INITIAL LINE TO pi_contents.

  IF p_vendor-land1 = c_ca.
    APPEND: '______________________________________________________________________'
            TO pi_contents,
            INITIAL LINE TO pi_contents.

*   French version
    APPEND: 'Veuillez trouver ci-joint votre avis de paiement.'(033)
             TO pi_contents,
            'N''hésitez pas à nous contacter pour toute question.'(034)
             TO pi_contents,
             INITIAL LINE TO pi_contents.

    APPEND: 'Merci,'(031) TO pi_contents,
             INITIAL LINE TO pi_contents.

    APPEND: 'CN - Comptes Fournisseurs'(032) TO pi_contents,
             INITIAL LINE TO pi_contents.
  ENDIF.

  APPEND: 'P.O. Box 8103'(020) TO pi_contents,
          'Montreal, Quebec'(021) TO pi_contents,
          'H3C 3N3'(022) TO pi_contents,
          '(514) 399-4700'(023) TO pi_contents.

ENDFORM.                    " F_BUILD_EMAIL_CONTENTS

*&---------------------------------------------------------------------*
*&      Form  F_SEND_EMAIL
*&---------------------------------------------------------------------*
FORM f_send_email  USING    p_vendor STRUCTURE wa_lfa1
                            p_vendor_email.

  DATA: li_contents      TYPE TABLE OF soli,
        li_receivers     TYPE TABLE OF soos1,
        lwa_receivers    TYPE soos1,
        lwa_object       TYPE sood1,
        li_packing_list  TYPE TABLE OF soxpl,
        lwa_packing_list TYPE soxpl.

  REFRESH: li_contents,
           li_receivers,
           li_packing_list.

* Build the subject line
  IF sy-sysid = c_prd.
    CONCATENATE 'CSV Payment Advice for Vendor'(027)
                 p_vendor-lifnr
            INTO lwa_object-objdes SEPARATED BY space.
  ELSE.
    CONCATENATE '***TEST***CSV Payment Advice for Vendor'(028)
                 p_vendor-lifnr
            INTO lwa_object-objdes SEPARATED BY space.
  ENDIF.

* Build email contents
  PERFORM f_build_email_contents TABLES li_contents
                                 USING  p_vendor.

* Build the receivers
  CLEAR lwa_receivers.
  lwa_receivers-recextnam = p_vendor_email.
  lwa_receivers-recesc    = c_u.
  lwa_receivers-rectp     = c_int.
  APPEND lwa_receivers TO li_receivers.

  CALL FUNCTION 'SO_RAW_TO_RTF'
    TABLES
      objcont_old = i_attachment
      objcont_new = i_attachment.

* Set packing list information for attachment
  CLEAR lwa_packing_list.
  DESCRIBE TABLE i_attachment.
  READ TABLE i_attachment INTO wa_attachment INDEX sy-tfill.
  IF sy-subrc = 0.
    lwa_packing_list-objlen
               = ( sy-tfill - 1 ) * 255 + strlen( wa_attachment ).
  ENDIF.
*  lwa_packing_list-transf_bin = 'X'.
  lwa_packing_list-head_start = 1.
  lwa_packing_list-head_num   = sy-tfill.
  lwa_packing_list-body_start = 1.
  lwa_packing_list-body_num   = sy-tfill.
  lwa_packing_list-objtp      = c_ext.
  lwa_packing_list-file_ext   = c_csv.
  lwa_packing_list-objnam     = 'ATTACHMENT'.
  lwa_packing_list-objdes     = 'CN Payment Remittance Advice'(025).
  APPEND lwa_packing_list TO li_packing_list.

  CALL FUNCTION 'SO_OBJECT_SEND'
    EXPORTING
      object_hd_change           = lwa_object
      object_type                = c_raw
    TABLES
      objcont                    = li_contents
      receivers                  = li_receivers
      packing_list               = li_packing_list
      att_cont                   = i_attachment
    EXCEPTIONS
      active_user_not_exist      = 1
      communication_failure      = 2
      component_not_available    = 3
      folder_not_exist           = 4
      folder_no_authorization    = 5
      forwarder_not_exist        = 6
      note_not_exist             = 7
      object_not_exist           = 8
      object_not_sent            = 9
      object_no_authorization    = 10
      object_type_not_exist      = 11
      operation_no_authorization = 12
      owner_not_exist            = 13
      parameter_error            = 14
      substitute_not_active      = 15
      substitute_not_defined     = 16
      system_failure             = 17
      too_much_receivers         = 18
      user_not_exist             = 19
      originator_not_exist       = 20
      x_error                    = 21
      OTHERS                     = 22.

  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " F_SEND_EMAIL

*&---------------------------------------------------------------------*
*&      Form  F_GET_PO_DATA
*&---------------------------------------------------------------------*
FORM f_get_po_data  USING    p_bukrs
                             p_belnr
                             p_gjahr
                    CHANGING p_prod_serv
                             p_tracking_no.

  DATA: lwa_bseg_po TYPE t_bseg_po,
        lwa_ekpo    TYPE t_ekpo.

  CLEAR: p_prod_serv,
         p_tracking_no.

  READ TABLE i_bseg_po INTO lwa_bseg_po WITH KEY bukrs = p_bukrs
                                                 belnr = p_belnr
                                                 gjahr = p_gjahr
                                                 BINARY SEARCH.
  IF sy-subrc = 0.
    READ TABLE i_ekpo INTO lwa_ekpo WITH KEY ebeln = lwa_bseg_po-ebeln
                                             ebelp = lwa_bseg_po-ebelp
                                             BINARY SEARCH.
    IF sy-subrc = 0.
      p_prod_serv   = lwa_ekpo-txz01.
      p_tracking_no = lwa_ekpo-bednr.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GET_PO_DATA
*---> Start of DV5K991322
*&---------------------------------------------------------------------*
*&      Form  F_GET_COMPCODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_compcodes .

  DATA wa_zall_parms   TYPE t_zall_parms.

  SELECT *
  INTO TABLE i_zallparm
  FROM zall_parms
  WHERE name LIKE c_zallparm.

  IF sy-subrc EQ 0.
    wa_compcode_us-sign = c_i.
    wa_compcode_us-option = c_eq.
    LOOP AT i_zallparm INTO wa_zall_parms.
      wa_compcode_us-low = wa_zall_parms-low.
      APPEND wa_compcode_us TO r_compcode_us.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_GET_COMPCODES
*<--- End of DV5K991322
