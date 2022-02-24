*----------------------------------------------------------------------*
*                 Canadian National                                    *
*                                                                      *
* Program   : ZME_SMENH821_PROC_GI_FUEL                                *
* Created by: Maria Cristina C. Niniel                                 *
* Created on: December 1, 2010                                         *
* Report Description     : This enhancement is covering only dispensing/
*                          issue of Fueling events from CN storage     *
*                          facilities. Pure DTL fueling events are not *
*                          part of this enhancement; they are covered  *
*                          by enhancement SM-ENH-817.                  *
*----------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* 12/1/2010  Maria Cristina C. Niniel (XT16578)          DV5K961212    *
* Initial Creation                                                     *
************************************************************************
REPORT  zme_smenh821_proc_gi_fuel.
*----------------------------------------------------------------------*
*           T A B L E S
*----------------------------------------------------------------------*
TABLES: zmm_flcm_tds.

*----------------------------------------------------------------------*
*        INCLUDES                                                      *
*----------------------------------------------------------------------*
INCLUDE: zme_smenh821_proc_gi_fuel_f01, "include for data declaration
         zme_smenh821_proc_gi_fuel_f02. "include for subroutines

*----------------------------------------------------------------------*
*        INITIALIZATION                                                *
*----------------------------------------------------------------------*
INITIALIZATION.
PERFORM f_initialization.

*----------------------------------------------------------------------*
*  Start-of-Selection / End-of-Selection                               *
*----------------------------------------------------------------------*
START-OF-SELECTION.
PERFORM f_get_data.
PERFORM f_data_processing.
