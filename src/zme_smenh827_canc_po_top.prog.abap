*----------------------------------------------------------------------*
*                      Canadian National                               *
*                                                                      *
* ABAP Name:       ZME_SMENH827_CANC_PO_TOP                            *
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
*  Nancy Bergeron   2011-02-01                       Task:DV5K960962   *
*  Short Description : CR169709-T173617 FLCM SM-ENH-827 (Creation).    *
*----------------------------------------------------------------------*

TABLES: EKKO,  "Purchasing Document Header
        EKPO.  "Purchasing Document Item

*&---------------------------------------------------------------------*
*&      TYPE DECLARATION
*&---------------------------------------------------------------------*
TYPES: BEGIN OF t_ekko,
         ebeln TYPE ekko-ebeln,  "Purchasing Document Number
         lifnr TYPE ekko-lifnr,  "Vendor Account Number
         ihrez TYPE ekko-ihrez,  "Your Reference
         unsez TYPE ekko-unsez,  "Our Reference
       END OF t_ekko,

       BEGIN OF t_ekpo,
         ebeln TYPE ekpo-ebeln,  "Purchasing Document Number
         ebelp TYPE ekpo-ebelp,  "Item Number of Purchasing Document
         werks TYPE ekpo-werks,  "Plant
         matkl TYPE ekpo-matkl,  "Material Group
         matnr TYPE ekpo-matnr,  "Material Number
         ematn TYPE ekpo-ematn,  "Material Number
         menge TYPE ekpo-menge,  "Purchase Order Quantity
         meins TYPE ekpo-meins,  "Purchase Order Unit of Measure
         netpr TYPE ekpo-netpr,  "Net Price in Purchasing Document (in Document Currency)
         lgort TYPE ekpo-lgort,  "Storage Location
         loekz TYPE ekpo-loekz,  "Deletion Indicator in Purchasing Document
         retpo TYPE ekpo-retpo,  "Returns Item
         xersy TYPE ekpo-xersy,  "Evaluated Receipt Settlement (ERS)
       END OF t_ekpo,

       BEGIN OF t_ekbe,
         ebeln TYPE ekbe-ebeln,  "Purchasing Document Number
         ebelp TYPE ekbe-ebelp,  "Item Number of Purchasing Document
         bwart TYPE ekbe-bwart,  "Movement Type (Inventory Management)
         bewtp TYPE ekbe-bewtp,  "Purchase Order History Category
       END OF t_ekbe,

       BEGIN OF t_log,
         MESSAGE TYPE BAPIRET2-MESSAGE,
       END OF t_log.

*&---------------------------------------------------------------------*
*&      DATA DECLARATION
*&---------------------------------------------------------------------*
* Internal Tables Declaration
DATA: i_po_header        TYPE STANDARD TABLE OF t_ekko,
      i_po_items         TYPE STANDARD TABLE OF t_ekpo,
      i_po_items_valid   TYPE STANDARD TABLE OF t_ekpo,
      i_po_items_ers     TYPE STANDARD TABLE OF t_ekpo,
      i_po_items_non_ers TYPE STANDARD TABLE OF t_ekpo,
      i_po_items_gr      TYPE STANDARD TABLE OF t_ekpo,
      i_po_delete        TYPE STANDARD TABLE OF t_ekpo,
      i_ekbe             TYPE STANDARD TABLE OF t_ekbe,  "History per Purchasing Document
      i_log              TYPE STANDARD TABLE OF t_log.

* Working Area
DATA: wa_po_header TYPE t_ekko,
      wa_po_items  TYPE t_ekpo,
      wa_po_return TYPE t_ekpo,
      wa_po_delete TYPE t_ekpo,
      wa_ekbe      TYPE t_ekbe,
      wa_log       TYPE t_log.

* Variables
DATA: v_start_date        TYPE sy-datum,
      v_valid_process(1)  TYPE c,
      v_online_process(1) TYPE c,
      v_subrc             TYPE sy-subrc.

* Range Definition
DATA: r_unsez        TYPE RANGE OF ekko-unsez.
*      s_unsez_range  LIKE LINE OF r_unsez.

* Constants
CONSTANTS: c_car(3)  TYPE c VALUE 'CAR',
           c_dtl(3)  TYPE c VALUE 'DTL',
           c_tds(3)  TYPE c VALUE 'TDS',
           c_abort   TYPE bapi_mtype VALUE 'A',     "Abort
           c_error   TYPE bapi_mtype VALUE 'E',     "Error
           c_success TYPE bapi_mtype VALUE 'S',     "Success
           c_warning TYPE bapi_mtype VALUE 'W'.     "Warning

*&---------------------------------------------------------------------*
*&      START OF SELECTION SCREEN
*&---------------------------------------------------------------------*

SELECT-OPTIONS s_ebeln FOR ekko-ebeln.
SELECT-OPTIONS s_lifnr FOR ekko-lifnr.
SELECT-OPTIONS s_bedat FOR ekko-bedat. "OBLIGATORY (validate in at_selection_screen)
SELECT-OPTIONS s_ihrez FOR ekko-ihrez.
SELECT-OPTIONS s_unsez FOR ekko-unsez.
SELECT-OPTIONS s_werks FOR ekpo-werks.
SELECT-OPTIONS s_matkl FOR ekpo-matkl.
PARAMETER      p_nbdays(3) TYPE c.

*&---------------------------------------------------------------------*
*&      END OF SELECTION SCREEN
*&---------------------------------------------------------------------*
