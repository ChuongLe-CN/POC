*&---------------------------------------------------------------------*
*&  Include           ZXM08U20
*&---------------------------------------------------------------------*
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZXM08U20                                                 *
* Title    :  Create an ERS transaction to process ERS invoice per BOL.*
* Work Unit:  SM-ENH-9QE-002                                           *
* Created by: Rob West                                                 *
* Created on: November 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose:    Pick up BOL# and Document date from the PO and populate  *
*             in the Baseline date and Reference fields in the FI      *
*             Vendor invoice document.                                 *
*             User exit MRMH0001                                       *
*                                                                      *
* Input:      I_RBKPV              TYPE       MRM_RBKPV                *
* Output:     E_RBKPV_ERS_CHANGE   TYPE       MRM_RBKPV_ERS_CHANGE     *
*             E_CHANGE             TYPE       C                        *
* Tables:     T_SELWENR            STRUCTURE  EK08ERSWE                *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2010/11/14          DV5K967593             *
*                                               DV5K967981             *
*                                                                      *
* Short Description: FLCM defect 656                                   *
*                    Do 9QE logic only for fuel POs - BLART = 'FB'.    *
*----------------------------------------------------------------------*
* Rob West                  2010/11/29          DV5K961205             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
*  IF i_rbkpv-blart = 'FB'.                                  "DV5K967593
  PERFORM f_smenh9qe_mrrl USING i_rbkpv
                                t_selwenr[]
                       CHANGING e_rbkpv_ers_change
                                e_change.
*  ENDIF.                                                    "DV5K967593
