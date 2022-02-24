*&---------------------------------------------------------------------*
*&  Include           ZXM08U27
*&---------------------------------------------------------------------*
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZXM08U27                                                 *
* Title    :  Create an ERS transaction to process ERS invoice per BOL.*
* Work Unit:  SM-ENH-9QE-002                                           *
* Created by: Rob West                                                 *
* Created on: February 2011                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose:    Pick up BOL# and Document date from the PO and populate  *
*             in the Baseline date and Reference fields in the FI      *
*             Vendor invoice document.                                 *
*             User exit MRMH0003                                       *
*                                                                      *
* Input:      IS_RBKPV             TYPE       MRM_RBKPV                *
* Output:     ES_RBKPV_ERS_CHANGE  TYPE       MMRM_RBKPV               *
*             EF_CHANGE            TYPE       C                        *
* Tables:     IT_SEGTAB            STRUCTURE  MMRAP_TSEGTAB            *
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
* Rob West                  2011/02/18          DV5K961205             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

*  IF is_rbkpv-blart = 'FB'.                                 "DV5K967593
  PERFORM f_smenh9qe_mrnb USING is_rbkpv
                                it_segtab[]
                       CHANGING es_rbkpv_change
                                ef_change.
*  ENDIF.                                                    "DV5K967593
