class ZCL_SMENH824_ERROR_MONITOR definition
  public
  abstract
  create public .

*"* public components of class ZCL_SMENH824_ERROR_MONITOR
*"* do not include other source files here!!!
public section.

  type-pools SLIS .
  methods ALV_DETAILS
    importing
      !IM_UCOMM type SYUCOMM
    changing
      !CH_SELFIELD type SLIS_SELFIELD .
  type-pools ABAP .
  methods DISPLAY_ALV
    importing
      !IM_CALLBACK_PROGRAM type SYREPID
      !IM_CALLBACK_FORM type SLIS_FORMNAME
      !IM_CALLBACK_PF_STATUS type SLIS_FORMNAME optional
      !IM_SBAL_CALLBACK type SLIS_FORMNAME optional
      !IM_VARIANT type SLIS_VARI optional
      !IM_ROW_START type ANY optional
      !IM_COLUMN_START type ANY optional
      !IM_ROW_END type ANY optional
      !IM_COLUMN_END type ANY optional
      !IM_GRID_TITLE type C optional
      !IM_DEFAULT_VARIANT type XFELD default ABAP_FALSE .
  methods CONSTRUCTOR
    importing
      !IM_TABNAME type TABNAME
      !IM_WITH_TEXT type XFELD optional
    raising
      ZCX_FLCM_ERROR .
  methods REPROCESS
    importing
      !IM_UCOMM type SYUCOMM
    changing
      !CH_SELFIELD type SLIS_SELFIELD
    raising
      ZCX_FLCM_ERROR .
  methods HIDE_DOCK .
  methods IS_DOCK_VISIBLE
    returning
      value(RE_STATUS) type XFELD .
  methods FREE .
  methods REFRESH_DATA
    importing
      !IM_SEL_TABIX type RE_T_TABIX .
  methods DISPLAYED_AS_POPUP
    returning
      value(RE_POPUP) type XFELD .
  methods CHECK_LINES_SELECTED
    returning
      value(RE_LINES) type CHAR01 .
protected section.
*"* protected components of class ZCL_SMENH824_ERROR_MONITOR
*"* do not include other source files here!!!

  data O_ALV_DOCK type ref to CL_GUI_DOCKING_CONTAINER .
  data V_SBAL_CONTROL_HANDLE type BALCNTHNDL .
  data V_SBAL_PROFILE type BAL_S_PROF .
  data V_SBAL_PROFILE_MULTI type BAL_S_PROF .
  data V_TABNAME type TABNAME .
  data V_ALV_DYNNR type SYDYNNR .
  data V_DOCK_VISIBLE type XFELD .
  data O_DATA_TABLE type ref to DATA .
  data O_LINE_TYPE type ref to CL_ABAP_STRUCTDESCR .
  data O_TABLE_TYPE type ref to CL_ABAP_TABLEDESCR .
  data O_BASE_TABLE_TYPE type ref to CL_ABAP_TABLEDESCR .
  data I_FIELDCAT type SLIS_T_FIELDCAT_ALV .
  class-data I_JOBLOG type TCHAR255 .
  data I_LOG_HANDLES type BAL_T_LOGH .
  data V_LOG_HANDLE type BALLOGHNDL .
  class-data V_WITH_TEXT type XFELD .

  methods INCLUDE_IN_SELECTION
    importing
      !IM_SELECTED_LINE type ANY
    returning
      value(RE_INCLUDE) type XFELD .
  methods PROCESS_SELECTED_LINES
    importing
      !IM_SELECTED type STANDARD TABLE
    raising
      ZCX_FLCM_ERROR .
  class-methods CONVERT_STATUS_TO_ICON
    importing
      !IM_STATUS type ZZTRAN_STUS
    returning
      value(RE_ICON) type ICON_D .
  methods LOAD_MESSAGES_FROM_LOG
    importing
      !IM_LOG_HANDLE type BALLOGHNDL
    changing
      !CH_LOG_HANDLES type BAL_T_LOGH .
  methods BUILD_EXTNUMBER_PATTERN
    changing
      !CH_EXTNUMBER type BALNREXT .
  methods MESSAGE_DETAIL_TITLE
    importing
      !IM_LINE type ANY
    changing
      !CH_GRID_TITLE type C .
  methods GET_LOG_HANDLE
    importing
      !IM_LINE type ANY
    returning
      value(RE_LOG_HANDLE) type BALLOGHNDL .
private section.
*"* private components of class ZCL_SMENH824_ERROR_MONITOR
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_SMENH824_ERROR_MONITOR IMPLEMENTATION.


METHOD alv_details.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->ALV_DETAILS                  *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2012/04/09          DV5K971100             *
*                                                                      *
* Short Description: CR199440-T207046 & CR199440-T207145               *
*                    Fix ALV abend when no line selected               *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961502             *
*                                               DV5K970355             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

  DATA: l_grid_title           TYPE bal_gridtitle,
        l_ratio                TYPE i.

  FIELD-SYMBOLS: <line>        TYPE ANY,
                 <profile>     TYPE bal_s_prof,
                 <handle>      TYPE balloghndl,
                 <table>       TYPE STANDARD TABLE.

  IF ch_selfield-tabindex = 0.                               "DV5K971100
    RETURN.                                                  "DV5K971100
  ENDIF.                                                     "DV5K971100

  ASSIGN o_data_table->* TO <table>.
  READ TABLE <table> ASSIGNING <line> INDEX ch_selfield-tabindex.
  IF sy-subrc = 0.
    ASSIGN COMPONENT 'ALV_LOG_HANDLE' OF STRUCTURE <line>
        TO <handle>.
    IF <handle> IS INITIAL.                                  "DV5K970355
      <handle> = get_log_handle( <line> ).                   "DV5K970355
    ENDIF.                                                   "DV5K970355
    IF <handle> IS INITIAL.
      MESSAGE 'No message details available'(t01) TYPE 'I'.
      v_dock_visible = abap_false.
      IF o_alv_dock IS BOUND.
        CALL METHOD o_alv_dock->set_visible
          EXPORTING
            visible = abap_false.
      ENDIF.
      RETURN.
    ENDIF.
    CALL METHOD load_messages_from_log
      EXPORTING
        im_log_handle  = <handle>
      CHANGING
        ch_log_handles = i_log_handles.
  ENDIF.

  IF v_alv_dynnr = '0700'.        "displayed in popup
    l_ratio = 50.
  ELSE.                           "fullscreen
    l_ratio = 30.
  ENDIF.

  IF o_alv_dock IS INITIAL.
    CREATE OBJECT o_alv_dock
      EXPORTING
        repid = 'SAPLSLVC_FULLSCREEN'
        dynnr = v_alv_dynnr
        ratio = l_ratio
        side  = cl_gui_docking_container=>dock_at_bottom
        name  = 'ALV_DOCK'.
  ENDIF.

  IF v_sbal_control_handle IS NOT INITIAL.
    CALL FUNCTION 'BAL_CNTL_FREE'
      CHANGING
        c_control_handle = v_sbal_control_handle.
    CLEAR v_sbal_control_handle.
  ENDIF.

  IF LINES( i_log_handles ) < 2.
    ASSIGN v_sbal_profile TO <profile>.
  ELSE.
    ASSIGN v_sbal_profile_multi TO <profile>.
  ENDIF.

  CALL METHOD message_detail_title
    EXPORTING
      im_line       = <line>
    CHANGING
      ch_grid_title = l_grid_title.

  IF l_grid_title IS INITIAL.
    CLEAR <profile>-grid_title.
  ELSE.
    <profile>-grid_title-no_gridtitle = abap_true.
    CONCATENATE 'Message details for:'(t02) l_grid_title INTO
        <profile>-grid_title-gridtitle SEPARATED BY space.
  ENDIF.

  CALL FUNCTION 'BAL_CNTL_CREATE'
    EXPORTING
      i_container         = o_alv_dock   "'ALV_DOCK'
      i_s_display_profile = <profile>
      i_t_log_handle      = i_log_handles[]
    IMPORTING
      e_control_handle    = v_sbal_control_handle
    EXCEPTIONS
      OTHERS              = 0.

  v_dock_visible = abap_true.
  CALL METHOD o_alv_dock->set_visible
    EXPORTING
      visible = v_dock_visible.

ENDMETHOD.


METHOD build_extnumber_pattern.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->BUILT_EXTNUMBER_PATTERN      *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

*** This method is blank to be implemented by the child class
ENDMETHOD.


METHOD check_lines_selected.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->CHECK_LINES_SELECTED         *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

  DATA: lo_line              TYPE REF TO data.

  FIELD-SYMBOLS: <line>      TYPE ANY,
                 <sel>       TYPE xfeld,
                 <table>     TYPE STANDARD TABLE.

  CREATE DATA lo_line TYPE HANDLE o_line_type.
  ASSIGN lo_line->* TO <line>.

  ASSIGN o_data_table->* TO <table>.

  ASSIGN COMPONENT 'ALV_SEL' OF STRUCTURE <line> TO <sel>.

  re_lines = 'N'.        "None

  LOOP AT <table> INTO <line>.
    IF <sel> = abap_true.
      IF re_lines = 'N'. "None
        re_lines = 'S'.  "Single
      ELSE.
        re_lines = 'M'.  "Multiple
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD constructor.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->CONSTRUCTOR                  *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

  DATA: l_tabclass           TYPE dd02l-tabclass,
        li_comp_tab          TYPE abap_component_tab,
        l_comp               LIKE LINE OF li_comp_tab,
        li_fieldcat          TYPE slis_t_fieldcat_alv,
        l_count              TYPE i,
        l_msgv1              TYPE symsgv,

        lo_base_line_type    TYPE REF TO cl_abap_structdescr.

  FIELD-SYMBOLS: <fcat>      TYPE slis_fieldcat_alv.

  v_tabname = im_tabname.
  TRANSLATE v_tabname TO UPPER CASE.                      "#EC SYNTCHAR
  IF v_with_text = abap_false.
    v_with_text = im_with_text.
  ENDIF.

  SELECT SINGLE tabclass
    FROM dd02l
    INTO l_tabclass
    WHERE tabname  = v_tabname AND
          as4local = 'A' AND
          as4vers  = '0000'.

  IF sy-subrc > 0 OR l_tabclass <> 'TRANSP'.
    l_msgv1 = v_tabname.
    RAISE EXCEPTION TYPE zcx_flcm_error
      EXPORTING
        msgid  = 'HRPIQ00ARCH'
        msgty  = 'E'
        msgno  = '100'
        msgv1  = l_msgv1.
  ENDIF.

  l_comp-as_include = abap_true.
  l_comp-type ?= cl_abap_datadescr=>describe_by_name( v_tabname ).
  APPEND l_comp TO li_comp_tab.
  lo_base_line_type = cl_abap_structdescr=>create( li_comp_tab ).

  l_comp-type ?= cl_abap_datadescr=>describe_by_name( 'ZMM_SMENH824_ALV_STRUCTURE' ).
  APPEND l_comp TO li_comp_tab.

  o_line_type = cl_abap_structdescr=>create( li_comp_tab ).

  CALL METHOD cl_abap_tabledescr=>create
    EXPORTING
      p_line_type = o_line_type
    RECEIVING
      p_result    = o_table_type.

  CREATE DATA o_data_table TYPE HANDLE o_table_type.

  CALL METHOD cl_abap_tabledescr=>create
    EXPORTING
      p_line_type = lo_base_line_type
    RECEIVING
      p_result    = o_base_table_type.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = v_tabname
    CHANGING
      ct_fieldcat      = i_fieldcat[].

  DELETE i_fieldcat INDEX 1.    "remove MANDT
  l_count = LINES( i_fieldcat ).

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZMM_SMENH824_ALV_STRUCTURE'
    CHANGING
      ct_fieldcat      = li_fieldcat[].

  LOOP AT li_fieldcat ASSIGNING <fcat>.
    CASE <fcat>-fieldname.
      WHEN 'ALV_ICON'.
        <fcat>-col_pos = 1.
      WHEN 'ALV_SEL' OR 'ALV_LOG_HANDLE' OR 'ALV_ICON_SUB'.
*                     OR 'ALV_TEXT'.
        <fcat>-tech = abap_true.
        ADD l_count TO <fcat>-col_pos.
      WHEN 'ALV_TEXT'.
        IF v_with_text = abap_false.
          <fcat>-tech = abap_true.
        ENDIF.
        ADD l_count TO <fcat>-col_pos.
      WHEN OTHERS.
        ADD l_count TO <fcat>-col_pos.
    ENDCASE.
    APPEND <fcat> TO i_fieldcat.
  ENDLOOP.

ENDMETHOD.


METHOD convert_status_to_icon.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->CONVERT_STATUS_TO_ICON       *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

  CASE im_status.
    WHEN '00'.   re_icon = icon_light_out.
    WHEN '01'.   re_icon = icon_red_light.
    WHEN '03'.   re_icon = icon_green_light.
    WHEN '04'.   re_icon = icon_red_light.
    WHEN OTHERS. re_icon = icon_led_inactive.
  ENDCASE.

ENDMETHOD.


METHOD displayed_as_popup.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->DISPLAYED_AS_POPUP           *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

  IF v_alv_dynnr = '0700'.
    re_popup = abap_true.
  ELSE.
    re_popup = abap_false.
  ENDIF.

ENDMETHOD.


METHOD display_alv.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->DISPLAY_ALV                  *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

  DATA: l_layout             TYPE slis_layout_alv,
        l_variant            TYPE disvariant,

        l_lines              TYPE i,

        l_row_start          TYPE i,
        l_row_end            TYPE i,
        l_column_start       TYPE i,
        l_column_end         TYPE i,
        l_grid_title         TYPE lvc_title.

  FIELD-SYMBOLS: <table>       TYPE STANDARD TABLE.

  l_row_start = im_row_start.
  l_row_end = im_row_end.
  l_column_start = im_column_start.
  l_column_end = im_column_end.

  IF l_row_start = 0 AND
     l_row_end = 0 AND
     l_column_start = 0 AND
     l_column_end = 0.
    v_alv_dynnr = '0500'.
  ELSE.
    v_alv_dynnr = '0700'.
  ENDIF.

  l_layout-colwidth_optimize = abap_true.
  l_layout-zebra = abap_true.
  l_layout-box_fieldname = 'ALV_SEL'.
  l_layout-f2code = 'ZDETAIL'.

  ASSIGN o_data_table->* TO <table>.

  l_variant-report = im_callback_program.
  l_variant-variant = im_variant.

  IF im_grid_title IS INITIAL.
    l_lines = LINES( <table> ).
    IF l_lines = 1.
      l_grid_title = '1 record displayed'(t04).
    ELSE.
      WRITE l_lines  TO l_grid_title LEFT-JUSTIFIED.
      CONCATENATE l_grid_title 'records displayed'(t03)
          INTO l_grid_title SEPARATED BY space.
    ENDIF.
  ELSE.
    l_grid_title = im_grid_title.
  ENDIF.

  IF im_sbal_callback IS SUPPLIED AND
     im_sbal_callback IS NOT INITIAL.
*** Define callback to add text for run status
    v_sbal_profile-clbk_read-userexitt     = space.
    v_sbal_profile-clbk_read-userexitp     = im_callback_program.
    v_sbal_profile-clbk_read-userexitf     = im_sbal_callback.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
        i_callback_program                = im_callback_program
        i_callback_pf_status_set          = im_callback_pf_status
        i_callback_user_command           = im_callback_form
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*        i_structure_name                  = v_tabname
*     I_BACKGROUND_ID                   = ' '
     i_grid_title                      = l_grid_title
*     I_GRID_SETTINGS                   =
     is_layout                         = l_layout
     it_fieldcat                       = i_fieldcat[]
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
*     IT_SORT                           =
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
     i_default                         = im_default_variant
     i_save                            = 'A'
     is_variant                        = l_variant
*     IT_EVENTS                         =
*     IT_EVENT_EXIT                     =
*     IS_PRINT                          =
*     IS_REPREP_ID                      =
     i_screen_start_column             = l_column_start
     i_screen_start_line               = l_row_start
     i_screen_end_column               = l_column_end
     i_screen_end_line                 = l_row_end
*     I_HTML_HEIGHT_TOP                 = 0
*     I_HTML_HEIGHT_END                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_HYPERLINK                      =
*     IT_ADD_FIELDCAT                   =
*     IT_EXCEPT_QINFO                   =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = <table>.
*   EXCEPTIONS
*     PROGRAM_ERROR                     = 1
*     OTHERS                            = 2

ENDMETHOD.


METHOD free.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->FREE                         *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

*  DATA: o_line               TYPE REF TO data,
*        li_handles           TYPE bal_t_logh.
*
*  FIELD-SYMBOLS: <table>     TYPE STANDARD TABLE,
*                 <line>      TYPE ANY,
*                 <handle>    TYPE balloghndl.

*  CREATE DATA o_line TYPE HANDLE o_line_type.
*  ASSIGN o_line->* TO <line>.
*  ASSIGN o_data_table->* TO <table>.
*
*  ASSIGN COMPONENT 'ALV_LOG_HANDLE' OF STRUCTURE <line>
*      TO <handle>.
*
*  LOOP AT <table> INTO <line>.
*    IF <handle> IS NOT INITIAL.
*      COLLECT <handle> INTO li_handles.
*    ENDIF.
*  ENDLOOP.

  IF v_sbal_control_handle IS NOT INITIAL.
    CALL FUNCTION 'BAL_CNTL_FREE'
      CHANGING
        c_control_handle = v_sbal_control_handle.
    CLEAR v_sbal_control_handle.
  ENDIF.

  IF o_alv_dock IS BOUND.
    CALL METHOD o_alv_dock->free.
    CLEAR o_alv_dock.
  ENDIF.

  IF i_log_handles[] IS NOT INITIAL.
    CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'
      EXPORTING
        i_refresh_all            = space
        i_t_logs_to_be_refreshed = i_log_handles[].
    CLEAR: v_log_handle,
           i_log_handles[].
  ENDIF.

ENDMETHOD.


method GET_LOG_HANDLE.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->GET_LOG_HANDLE               *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2012                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2012/03/09          DV5K970355             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

*--------------------------------------------------------------------*
*  This is a blank method to be overridden by the inheriting class
*--------------------------------------------------------------------*
endmethod.


METHOD hide_dock.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->HIDE_DOCK                    *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

  v_dock_visible = abap_false.
  IF o_alv_dock IS BOUND..
    CALL METHOD o_alv_dock->set_visible
      EXPORTING
        visible = v_dock_visible.
  ENDIF.

ENDMETHOD.


METHOD include_in_selection.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->INCLUDE_IN_SELECTION         *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

  FIELD-SYMBOLS: <trnsta>    TYPE zztran_stus.

  ASSIGN COMPONENT 'ZTRAN_STUS' OF STRUCTURE im_selected_line TO <trnsta>.

  IF sy-subrc = 0 AND <trnsta> = '04'.
    re_include = abap_true.
  ELSE.
    re_include = abap_false.
  ENDIF.

ENDMETHOD.


METHOD is_dock_visible.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->IS_DOCK_VISIBLE              *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

  re_status = v_dock_visible.

ENDMETHOD.


METHOD load_messages_from_log.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->LOAD_MESSAGES_FROM_LOG       *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Shankar Gomare           2012/07/03            DV5K972674            *
* Short Description: Select from balhdr is changed due to preformance  *
*                     issue in production                              *
************************************************************************
  TYPES: BEGIN OF lt_balhdr,
           object              TYPE balhdr-object,
           subobject           TYPE balhdr-subobject,
           extnumber           TYPE balhdr-extnumber,
           alstate             TYPE balhdr-alstate,
           log_handle          TYPE balhdr-log_handle,
         END OF lt_balhdr.

  DATA: li_balhdr            TYPE STANDARD TABLE OF lt_balhdr,
        lw_balhdr            TYPE lt_balhdr.

  DATA: l_extnumber          TYPE balhdr-extnumber,
        l_object             TYPE balhdr-object,
        l_subobject          TYPE balhdr-subobject,
        li_log_handles       TYPE STANDARD TABLE OF balloghndl,
        lw_log_handles       TYPE balloghndl.

  IF v_log_handle = im_log_handle.
    RETURN.        "Appliaction Log data already loaded
  ENDIF.

  v_log_handle = im_log_handle.

  IF ch_log_handles[] IS NOT INITIAL.
    CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'
      EXPORTING
        i_refresh_all            = ' '
        i_t_logs_to_be_refreshed = ch_log_handles.
    REFRESH ch_log_handles.
  ENDIF.

  IF im_log_handle IS INITIAL.
    RETURN.
  ENDIF.

*** Get the EXTNUMBER associated with the Log Handle
  SELECT SINGLE object
                subobject
                extnumber
    INTO (l_object,
          l_subobject,
          l_extnumber)
    FROM balhdr
    WHERE log_handle = im_log_handle.

  IF sy-subrc > 0.
    RETURN.
  ENDIF.

*** Select the Log Handles of all similar messages based on EXTNUMBER
  CALL METHOD build_extnumber_pattern
    CHANGING
      ch_extnumber = l_extnumber.

  SELECT object
          subobject
          extnumber
          alstate
          log_handle
     FROM balhdr
     INTO TABLE li_balhdr
     WHERE extnumber LIKE l_extnumber.
*<Code change for performance C206924-T215320>
*  SELECT log_handle
*    FROM balhdr
*    INTO TABLE li_log_handles
*    WHERE extnumber LIKE l_extnumber AND
*          object = l_object AND
*          subobject = l_subobject.
*    WHERE object = l_object AND
*          subobject = l_subobject AND
*          extnumber LIKE l_extnumber.
  DELETE li_balhdr WHERE NOT (     object = l_object
                               AND subobject = l_subobject
                               AND alstate IS INITIAL ).

  LOOP AT li_balhdr INTO lw_balhdr.
    MOVE lw_balhdr-log_handle TO lw_log_handles.
    APPEND lw_log_handles TO li_log_handles.
  ENDLOOP.
* <end of code change C206924-T215320>
  SORT li_log_handles ASCENDING.
  ch_log_handles[] = li_log_handles[].

*  CALL METHOD select_log_headers
*    EXPORTING
*      im_extnumber   = l_extnumber
*    CHANGING
*      ch_grid_title  = ch_grid_title
*      ch_log_handles = ch_log_handles[].

*** Load the Application Logs into memory for display
  IF ch_log_handles[] IS NOT INITIAL.
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_handle = ch_log_handles[].
  ENDIF.

ENDMETHOD.


METHOD message_detail_title.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->MESSAGE_DETAIL_TITLE         *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

*** This method is blank to be implemented by the child class

ENDMETHOD.


METHOD process_selected_lines.                    "#EC NEEDED

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->PROCESS_SELECTED_LINES       *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

*** This method is blank to be implemented by the child class

ENDMETHOD.


METHOD refresh_data.                              "#EC NEEDED

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->REFRESH_DATA                 *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

*** This method is blank to be implemented by the child class

ENDMETHOD.


METHOD reprocess.

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZCL_SMENH824_ERROR_MONITOR->REPROCESS                    *
* Title    :  TDS Error Monitor                                        *
* Work Unit:  SM-ENH-824-001                                           *
* Created by: Rob West                                                 *
* Created on: March 2011                                               *
* Version:    1.0                                                      *
* T-Code:     ZM_TDSMONI /ZM_DTLMONI                                   *
*                                                                      *
* Purpose:    Allow for error reprocessing of records in the           *
*             ZMM_FLCM_TDS table                                       *
*                                                                      *
* Note   :    This is an abstract class only.                          *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2012/04/09          DV5K971100             *
*                                                                      *
* Short Description: CR199440-T207046 & CR199440-T207145               *
*                    No changes made                                   *
*----------------------------------------------------------------------*
* Rob West                  2011/10/18          DVSK904456/DV5K967173  *
*                                                                      *
* Short Description: FLCM defect 616                                   *
*                    Change message when no lines selected for         *
*                    reprocessing                                      *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961502             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*
************************************************************************

  DATA: lo_line              TYPE REF TO data,
        lo_selected          TYPE REF TO data,

        l_tabix              TYPE sy-tabix,
        li_sel_tabix         TYPE re_t_tabix.

  FIELD-SYMBOLS: <line>      TYPE ANY,
                 <sel>       TYPE xfeld,
                 <selected>  TYPE STANDARD TABLE,
                 <table>     TYPE STANDARD TABLE.

  CREATE DATA lo_line TYPE HANDLE o_line_type.
  ASSIGN lo_line->* TO <line>.

  ASSIGN o_data_table->* TO <table>.

  CREATE DATA lo_selected TYPE HANDLE o_base_table_type.
  ASSIGN lo_selected->* TO <selected>.

  ASSIGN COMPONENT 'ALV_SEL' OF STRUCTURE <line> TO <sel>.

  LOOP AT <table> INTO <line>.
    l_tabix = sy-tabix.
    IF <sel> = abap_true.
      IF include_in_selection( <line> ) = abap_true.
        APPEND l_tabix TO li_sel_tabix.
        APPEND <line> TO <selected>.
      ELSE.
        CLEAR <sel>.       "Reset line status; cannot process
        MODIFY <table> FROM <line> INDEX l_tabix TRANSPORTING ('ALV_SEL').
      ENDIF.
      ch_selfield-refresh = ch_selfield-row_stable = abap_true.
    ENDIF.
  ENDLOOP.

  IF <selected>[] IS NOT INITIAL.
    CALL METHOD process_selected_lines
      EXPORTING
        im_selected = <selected>.
    CALL METHOD refresh_data
      EXPORTING
        im_sel_tabix = li_sel_tabix[].
  ELSE.
*    IF li_sel_tabix[] IS NOT INITIAL.
*      CALL METHOD refresh_data
*        EXPORTING
*          im_sel_tabix = li_sel_tabix[].
*    ENDIF.
    RAISE EXCEPTION TYPE zcx_flcm_error     "#EC *
      EXPORTING
        msgid  = 'ZZ_FLCM'
        msgty  = 'I'
*        msgno  = '055'.                                   "DVSK904456/DV5K967173
        msgno  = '094'.                                    "DVSK904456/DV5K967173
    MESSAGE i094(zz_flcm).      "For «Where-Used» only     "DVSK904456/DV5K967173
*    MESSAGE i055(zz_flcm).      "For «Where-Used» only    "DVSK904456/DV5K967173
  ENDIF.

*  CALL METHOD process_selected_lines
*    EXPORTING
*      im_selected = <selected>.

*  CALL METHOD refresh_data
*    EXPORTING
*      im_sel_tabix = li_sel_tabix[].

ENDMETHOD.
ENDCLASS.
