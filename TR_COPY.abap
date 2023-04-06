TYPE-POOLS: trwbo, rs, trsel, abap, icon, stms, ctslg.

CLASS: gcl_alv DEFINITION DEFERRED.

DATA: go_alv TYPE REF TO gcl_alv.




CLASS gcl_alv DEFINITION.


  PUBLIC SECTION.


    CONSTANTS:
      BEGIN OF mcs_usr_cmd,
        cop TYPE string VALUE 'COP',
        cpr TYPE string VALUE 'CPR',
        cri TYPE string VALUE 'CRI',
      END OF   mcs_usr_cmd.

    DATA: mo_grid      TYPE REF TO cl_gui_alv_grid,
          mo_container TYPE REF TO cl_gui_custom_container.

    METHODS:
      constructor
        IMPORTING
          iv_trkorr TYPE trkorr
          iv_test   TYPE boolean,
      show_alv,
      run,
      handle_hotspot FOR EVENT hotspot_click
                          OF cl_gui_alv_grid
                          IMPORTING e_row_id
                                    e_column_id
                                    es_row_no,
      popup_to_confirm
        IMPORTING
          iv_fname TYPE lvc_fname
          iv_rowid TYPE int4
        EXPORTING
          ev_denied TYPE boolean.

  PRIVATE SECTION.

    TYPES BEGIN OF mts_req.
            INCLUDE TYPE trwbo_request_header.
    TYPES request TYPE trwbo_request.
    TYPES END OF  mts_req.

    TYPES mtt_req TYPE STANDARD TABLE OF mts_req WITH DEFAULT KEY.

    TYPES BEGIN OF mts_data.
            INCLUDE TYPE trwbo_request_header.
    TYPES cpr          TYPE icon_d.
    TYPES cop          TYPE icon_d.
    TYPES cri          TYPE icon_d.
    TYPES iconrc       TYPE zed_avap_iconrc.
    TYPES t_new_trkorr TYPE trkorrs.
    TYPES END OF mts_data.

    TYPES mtt_data TYPE STANDARD TABLE OF mts_data WITH DEFAULT KEY.

    DATA: mt_data     TYPE mtt_data,
          mv_trkorr   TYPE trkorr,
          mt_requests TYPE trwbo_request_headers,
          mv_target   TYPE tr_target.

    DATA: mo_timer TYPE REF TO cl_gui_timer.

    METHODS:
      popup_to_select
        CHANGING
          ct_trkorr TYPE mtt_req,
      check_trkorr
        IMPORTING
          it_trkorr TYPE mtt_req
        EXPORTING
          et_messages TYPE ctsgerrmsgs,
      get_target_system
        RETURNING value(rv_target) TYPE tr_target,
      copy_and_release
        IMPORTING
          iv_rowid TYPE int4,
      copy
        IMPORTING
          iv_rowid TYPE int4
        EXPORTING
          ev_new_trkorr TYPE trkorr,
      release
        IMPORTING
          iv_trkorr TYPE trkorr,
      copy_release_import
        IMPORTING
          iv_rowid TYPE int4,
      get_trkorrs,
      create_fcat
       IMPORTING
         is_component   TYPE any
       RETURNING value(rt_fcat) TYPE lvc_t_fcat,
       on_double_click FOR EVENT double_click
                OF cl_gui_alv_grid
                IMPORTING e_row
                          e_column
                          sender,
       handle_timer FOR EVENT finished OF cl_gui_timer.


ENDCLASS.                    "gcl_alv DEFINITION


*----------------------------------------------------------------------*
*       CLASS gcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_alv IMPLEMENTATION.

  METHOD constructor.
    mv_trkorr = iv_trkorr. 

    mv_target = me->get_target_system( ).

    CREATE OBJECT mo_timer.

    SET HANDLER me->handle_timer FOR mo_timer.

  ENDMETHOD.                    "constructor

  METHOD handle_timer.
* Check import queue status every 5 seconds
* Update screen

    DATA: ls_settings   TYPE ctslg_settings,
          ls_cofiles    TYPE ctslg_cofile,
          lv_rc         TYPE i,
          lv_count      TYPE i,
          lv_unchecked  TYPE boolean VALUE 'X'.

    FIELD-SYMBOLS: <ls_data>      TYPE mts_data,
                   <ls_trkorr>    TYPE trkorr,
                   <ls_syname>    TYPE trsysname,
                   <ls_system_rc> TYPE ctslg_system.

    APPEND INITIAL LINE TO ls_settings-systems ASSIGNING <ls_syname>.
    <ls_syname>-name = mv_target.

    LOOP AT mt_data ASSIGNING <ls_data> WHERE t_new_trkorr IS NOT INITIAL.

      CLEAR lv_rc.

      LOOP AT <ls_data>-t_new_trkorr ASSIGNING <ls_trkorr>.
        CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
          EXPORTING
            iv_trkorr   = <ls_trkorr>
            is_settings = ls_settings
          IMPORTING
            es_cofile   = ls_cofiles.

        IF ls_cofiles-imported IS INITIAL.
          CLEAR lv_unchecked.
        ENDIF.

        READ TABLE ls_cofiles-systems ASSIGNING <ls_system_rc> INDEX 1.
        CHECK sy-subrc = 0.

        lv_rc = lv_rc + <ls_system_rc>-rc.
      ENDLOOP.

      lv_count = LINES( <ls_data>-t_new_trkorr ) * 4.

      IF lv_count = lv_rc.
        <ls_data>-iconrc = icon_led_yellow.
      ELSEIF lv_count < lv_rc.
        <ls_data>-iconrc = icon_led_red.
      ELSE.
        <ls_data>-iconrc = icon_led_green.
      ENDIF.

    ENDLOOP.

    IF lv_unchecked IS INITIAL.
      mo_timer->cancel( ).
      mo_timer->run( ).
    ENDIF.

    mo_grid->refresh_table_display( i_soft_refresh = abap_true ).

  ENDMETHOD.                    "handle_timer

  METHOD on_double_click.

    DATA: lt_request   TYPE cts_trkorrs.


    FIELD-SYMBOLS: <ls_data>    TYPE mts_data,
                   <lv_value>   TYPE ANY,
                   <ls_request> TYPE cts_trkorr.

    READ TABLE mt_data ASSIGNING <ls_data> INDEX e_row-index.
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT e_column-fieldname OF STRUCTURE <ls_data> TO <lv_value>.
    CHECK sy-subrc = 0.

    IF e_column-fieldname = 'TRKORR'.

      APPEND INITIAL LINE TO lt_request ASSIGNING <ls_request>.
      <ls_request>-trkorr = <lv_value>.

      CALL FUNCTION 'TR_DISPLAY_REQUESTS'
        EXPORTING
          it_request_numbers = lt_request.

    ENDIF.


  ENDMETHOD.                    "on_double_click

  METHOD handle_hotspot.

    DATA lv_denied TYPE boolean.

    me->popup_to_confirm(
      EXPORTING
        iv_fname = e_column_id-fieldname
        iv_rowid = es_row_no-row_id
      IMPORTING
        ev_denied = lv_denied
     ).

    CHECK lv_denied IS INITIAL.

    CASE e_column_id-fieldname.
      WHEN mcs_usr_cmd-cop.
        me->copy( es_row_no-row_id ).
      WHEN  mcs_usr_cmd-cpr.
        me->copy_and_release( es_row_no-row_id ).
      WHEN  mcs_usr_cmd-cri.
        me->copy_release_import( es_row_no-row_id ).
    ENDCASE.

  ENDMETHOD.                    "handle_hotspot


  METHOD copy_and_release.

    DATA: lv_trkorr TYPE trkorr.

    me->copy(
      EXPORTING
        iv_rowid = iv_rowid
      IMPORTING
        ev_new_trkorr = lv_trkorr
     ).
 
    CHECK lv_trkorr IS NOT INITIAL.

    me->release( lv_trkorr ).

  ENDMETHOD.                    "copy_and_release

  METHOD check_trkorr.

    DATA lt_messages_tmp TYPE ctsgerrmsgs.

    FIELD-SYMBOLS: <ls_req> TYPE mts_req.

    LOOP AT it_trkorr ASSIGNING <ls_req>.

      CALL FUNCTION 'TR_CHECK_REQUEST'
        EXPORTING
          is_request       = <ls_req>-request
        IMPORTING
          et_messages      = lt_messages_tmp
        EXCEPTIONS
          header_error     = 1
          attribute_error  = 2
          obj_or_key_error = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      APPEND LINES OF lt_messages_tmp TO et_messages.

    ENDLOOP.

  ENDMETHOD.                    "check_trkorr

  METHOD popup_to_select.

    DATA: lt_return TYPE bkk_tab_retval,
          lv_index  TYPE sy-tabix,
          lt_trkorr TYPE TABLE OF e070.

    FIELD-SYMBOLS: <ls_trkorr> TYPE e070,
                   <ls_req>    TYPE mts_req.

    LOOP AT ct_trkorr ASSIGNING <ls_req>.
      APPEND INITIAL LINE TO lt_trkorr ASSIGNING <ls_trkorr>.
      MOVE-CORRESPONDING <ls_req> TO <ls_trkorr>.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'TRKORR'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        window_title    = 'Choose tasks which you want add'(007)
        value_org       = 'S'
        multiple_choice = 'X'
      TABLES
        value_tab       = lt_trkorr
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF lt_return IS INITIAL.
      CLEAR ct_trkorr.
      RETURN.
    ENDIF.

    LOOP AT ct_trkorr ASSIGNING <ls_req>.
      lv_index = sy-tabix.
      READ TABLE lt_return TRANSPORTING NO FIELDS
        WITH KEY fieldval = <ls_req>-trkorr.
      IF sy-subrc <> 0.
        DELETE ct_trkorr INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "popup_to_select

  METHOD get_target_system.

    DATA: lv_layer TYPE devlayer.

    "Define layer and target system

    CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'
      EXPORTING
        iv_use_default             = abap_true
        iv_get_layer_only          = abap_true
      IMPORTING
        ev_layer                   = lv_layer
      EXCEPTIONS
        wrong_call                 = 1
        invalid_input              = 2
        cts_initialization_failure = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'
      EXPORTING
        iv_transport_layer         = lv_layer
      IMPORTING
        ev_target                  = rv_target
      EXCEPTIONS
        wrong_call                 = 1
        invalid_input              = 2
        cts_initialization_failure = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.                    "get_target_system

  METHOD popup_to_confirm.

    DATA: lv_titlebar TYPE string,
          lv_question TYPE string,
          lv_icon     TYPE iconname,
          lv_answer.

    FIELD-SYMBOLS: <ls_data> TYPE mts_data.

    READ TABLE mt_data ASSIGNING <ls_data> INDEX iv_rowid.
    CHECK sy-subrc = 0.

    CASE iv_fname.
      WHEN mcs_usr_cmd-cop.
        "Create transport copy of request &1 ?
        MESSAGE s000 WITH <ls_data>-trkorr INTO lv_question.
        lv_icon = icon_create_copy.
      WHEN mcs_usr_cmd-cpr.
        "Create transport copy of request &1 and release it ?
        MESSAGE s001 WITH <ls_data>-trkorr INTO lv_question.
        lv_icon = icon_transport.
      WHEN mcs_usr_cmd-cri.
        "Create transport copy of request &1, release and import it to &2 ?
        MESSAGE s002 WITH <ls_data>-trkorr mv_target INTO lv_question.
        lv_icon = icon_import_all_requests.
    ENDCASE.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = 'The following action will be performed. Continue?'(011)
        text_question  = lv_question
        text_button_1  = 'Yes'(009)
        icon_button_1  = lv_icon
        text_button_2  = 'No'(010)
      IMPORTING
        answer         = lv_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lv_answer = '2' OR lv_answer = 'A'.
      ev_denied = abap_true.
    ENDIF.


  ENDMETHOD.                    "popup_to_confirm

  METHOD copy_release_import.

    DATA: lv_trkorr  TYPE trkorr,
          lt_clinets TYPE stms_clients.


    me->copy(
      EXPORTING
        iv_rowid = iv_rowid
      IMPORTING
        ev_new_trkorr = lv_trkorr
     ).
 
    CHECK lv_trkorr IS NOT INITIAL.

    me->release( lv_trkorr ).

    CALL FUNCTION 'TMS_UI_IMPORT_TR_REQUEST'
      EXPORTING
        iv_system             = mv_target
        iv_request            = lv_trkorr
      EXCEPTIONS
        cancelled_by_user     = 1
        import_request_denied = 2
        import_request_failed = 3
        OTHERS                = 4.
    IF sy-subrc <> 0 AND sy-subrc <> 1.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "Set timer to check import queue status every 5 seconds
    mo_timer->interval = 5.
    mo_timer->run( ).

  ENDMETHOD.                    "copy_release_import

  METHOD release.

    CALL FUNCTION 'TRINT_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                   = iv_trkorr
        iv_dialog                   = space
        iv_without_objects_check    = 'X'
        iv_without_locking          = 'X'
        iv_ignore_warnings          = 'X'
      EXCEPTIONS
        cts_initialization_failure  = 1
        enqueue_failed              = 2
        no_authorization            = 3
        invalid_request             = 4
        request_already_released    = 5
        repeat_too_early            = 6
        object_lock_error           = 7
        object_check_error          = 8
        docu_missing                = 9
        db_access_error             = 10
        action_aborted_by_user      = 11
        export_failed               = 12
        execute_objects_check       = 13
        release_in_bg_mode          = 14
        release_in_bg_mode_w_objchk = 15
        error_in_export_methods     = 16
        object_lang_error           = 17
        OTHERS                      = 18.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "release

  METHOD copy.

    DATA: lt_trkorr     TYPE mtt_req,
          ls_requests   TYPE trwbo_request,
          ls_new_trkorr TYPE trwbo_request_header,
          lt_check_msg  TYPE ctsgerrmsgs.

    FIELD-SYMBOLS: <ls_data>    TYPE mts_data,
                   <ls_request> TYPE trwbo_request_header,
                   <ls_req>     TYPE mts_req.

* 1. Create copy request
* 2. Copy task to new copies tasks
* 3. After each copy release selected tasks

    READ TABLE mt_data ASSIGNING <ls_data> INDEX iv_rowid.
    CHECK sy-subrc = 0.

    "Get tasks for process
    LOOP AT mt_requests ASSIGNING <ls_request> WHERE strkorr = <ls_data>-trkorr AND trstatus = 'D'.

      CALL FUNCTION 'TR_READ_REQUEST'
        EXPORTING
          iv_read_e070       = abap_true
          iv_read_e07t       = abap_true
          iv_read_e070c      = abap_true
          iv_read_e070m      = abap_true
          iv_read_objs_keys  = abap_true
          iv_read_attributes = abap_true
          iv_trkorr          = <ls_request>-trkorr
        CHANGING
          cs_request         = ls_requests
        EXCEPTIONS
          error_occured      = 1
          no_authorization   = 2
          OTHERS             = 3.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CHECK ls_requests-objects IS NOT INITIAL.

      APPEND INITIAL LINE TO lt_trkorr ASSIGNING <ls_req>.
      MOVE-CORRESPONDING <ls_request> TO <ls_req>.
      <ls_req>-request = ls_requests.

    ENDLOOP.


    IF lt_trkorr IS INITIAL.
      MESSAGE 'Request does not containt  objects'(003) TYPE rs_c_success DISPLAY LIKE rs_c_error.
      RETURN.
    ENDIF.

    "Ask user to select tasks to include to transport copies
    me->popup_to_select( CHANGING ct_trkorr = lt_trkorr ).

    IF lt_trkorr IS INITIAL.
      MESSAGE 'Canceled'(004) TYPE rs_c_success DISPLAY LIKE rs_c_error.
      RETURN.
    ENDIF.

    me->check_trkorr(
      EXPORTING
        it_trkorr = lt_trkorr
      IMPORTING
        et_messages = lt_check_msg
     ).

    CALL FUNCTION 'TR_DISPLAY_REQ_CHECK_MESSAGES'
      EXPORTING
        it_messages  = lt_check_msg
        iv_title     = 'Tasks have error'(005)
      EXCEPTIONS
        cancel       = 1
        display_only = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CHECK lt_check_msg IS INITIAL.
	
	"FM ZABAP_TR_REQUEST_MODIFY copy of TR_REQUEST_MODIFY where deleted perform "send_popup"

    "Create transport copies
    CALL FUNCTION 'ZABAP_TR_REQUEST_MODIFY'
      EXPORTING
        iv_action            = 'CREA'
        iv_new_task_type     = 'X'
        iv_new_request_type  = 'T'
        iv_new_tarsystem     = mv_target
        iv_new_as4text       = <ls_data>-as4text
      IMPORTING
        es_new_request       = ls_new_trkorr
      EXCEPTIONS
        cancelled_by_user    = 1
        no_authorization     = 2
        invalid_action       = 3
        invalid_request      = 4
        invalid_request_type = 5
        request_not_created  = 6
        request_not_deleted  = 7
        enqueue_failed       = 8
        db_access_error      = 9
        OTHERS               = 10.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    ev_new_trkorr = ls_new_trkorr-trkorr.

    APPEND ls_new_trkorr-trkorr TO <ls_data>-t_new_trkorr.

    "Put tasks to transpot copies and release selected tasks
    LOOP AT lt_trkorr ASSIGNING <ls_req>.

      CALL FUNCTION 'TR_COPY_COMM'
        EXPORTING
          wi_dialog                = space
          wi_trkorr_from           = <ls_req>-trkorr
          wi_trkorr_to             = ls_new_trkorr-trkorr
          wi_without_documentation = 'X'
        EXCEPTIONS
          db_access_error          = 1
          trkorr_from_not_exist    = 2
          trkorr_to_is_repair      = 3
          trkorr_to_locked         = 4
          trkorr_to_not_exist      = 5
          trkorr_to_released       = 6
          user_not_owner           = 7
          no_authorization         = 8
          wrong_client             = 9
          wrong_category           = 10
          object_not_patchable     = 11
          OTHERS                   = 12.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      me->release( <ls_req>-trkorr ).

    ENDLOOP.

  ENDMETHOD.                    "copy

  METHOD run.

    me->get_trkorrs( ).

    CALL SCREEN 0100.

  ENDMETHOD.                    "run

  METHOD get_trkorrs.

    DATA: ls_new_trkorr TYPE trwbo_request_header,
          lt_requests   TYPE trwbo_request_headers,
          ls_ranges     TYPE trsel_ts_ranges,
          ls_requests   TYPE trwbo_request.

    FIELD-SYMBOLS: <ls_s4user>  TYPE trsel_trs_as4user,
                   <ls_trkorr>  TYPE trsel_trs_trkorr,
                   <ls_reqstt>  TYPE trsel_trs_status,
                   <ls_reqfun>  TYPE trsel_trs_function,
                   <ls_request> TYPE trwbo_request_header,
                   <ls_objects> TYPE trwbo_s_e071,
                   <ls_data>    TYPE mts_data.

    APPEND INITIAL LINE TO ls_ranges-as4user ASSIGNING <ls_s4user>.
    APPEND INITIAL LINE TO ls_ranges-request_status ASSIGNING <ls_reqstt>.
    APPEND INITIAL LINE TO ls_ranges-request_funcs ASSIGNING <ls_reqfun>.

    IF mv_trkorr IS NOT INITIAL.
      APPEND INITIAL LINE TO ls_ranges-trkorr ASSIGNING <ls_trkorr>.
      <ls_trkorr>-sign   = rs_c_range_sign-including.
      <ls_trkorr>-option = rs_c_range_opt-equal.
      <ls_trkorr>-low    = mv_trkorr.
    ENDIF.

    <ls_s4user>-sign   = <ls_reqstt>-sign   = <ls_reqfun>-sign   = rs_c_range_sign-including.
    <ls_s4user>-option = <ls_reqstt>-option = <ls_reqfun>-option = rs_c_range_opt-equal.

    <ls_s4user>-low = sy-uname.
    <ls_reqstt>-low = 'D'.
    <ls_reqfun>-low = 'K'.


    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern    = sy-uname
      IMPORTING
        et_requests            = mt_requests
      CHANGING
        cs_ranges              = ls_ranges
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT mt_requests ASSIGNING <ls_request> WHERE trfunction = 'K'.
      APPEND INITIAL LINE TO mt_data ASSIGNING <ls_data>.
      MOVE-CORRESPONDING <ls_request> TO <ls_data>.
      <ls_data>-cop = icon_create_copy.
      <ls_data>-cpr = icon_transport.
      <ls_data>-cri = icon_import_all_requests.
      <ls_data>-iconrc = icon_led_inactive.
    ENDLOOP.

    SORT mt_data BY trkorr DESCENDING.

  ENDMETHOD.                    "get_trkorrs

  METHOD show_alv.

    DATA: lt_fcat    TYPE lvc_t_fcat,
          ls_alv     TYPE zst_abap_alv_trcopy,
          ls_layout  TYPE lvc_s_layo,
          ls_variant TYPE disvariant.
	
	"zst_abap_alv_trcopy
	"TRKORR	    TRKORR	        CHAR	20
    "AS4USER	TR_AS4USER	    CHAR	12
    "AS4TEXT	AS4TEXT	        CHAR	60
    "COP	    ZED_ABAP_COP	CHAR	4
    "CPR	    ZED_ABAP_CPR	CHAR	4
    "CRI	    ZED_ABAP_CRI	CHAR	4
    "ICONRC	    ZED_AVAP_ICONRC	CHAR	4
	
    lt_fcat = create_fcat( ls_alv ).

    ls_variant-report = sy-repid.
    ls_variant-handle = 1.
    ls_layout-sel_mode = 'A'.
    ls_layout-smalltitle = abap_true.

    IF mo_container IS NOT BOUND.

      CREATE OBJECT mo_container
        EXPORTING
          container_name = 'CONTAINER'.

      CREATE OBJECT mo_grid
        EXPORTING
          i_parent = mo_container.

      SET HANDLER me->on_double_click FOR mo_grid.
      SET HANDLER me->handle_hotspot FOR mo_grid.

      mo_grid->set_table_for_first_display(
        EXPORTING
          i_save     = 'A'
          is_layout  = ls_layout
          is_variant = ls_variant
        CHANGING
          it_fieldcatalog = lt_fcat
          it_outtab       = mt_data ).


    ENDIF.


  ENDMETHOD.                    "show_alv

  METHOD create_fcat.

    DATA:
      lo_descr      TYPE REF TO cl_abap_structdescr,
      lt_fieldlist  TYPE ddfields,
      lo_type_descr TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS:
      <ls_component> TYPE abap_compdescr,
      <ls_fcat>      TYPE lvc_s_fcat,
      <ls_flist>     TYPE dfies.

    lo_type_descr = cl_abap_typedescr=>describe_by_data( is_component ).

    lo_descr ?= lo_type_descr.

    lt_fieldlist = lo_descr->get_ddic_field_list( ).

    LOOP AT lo_descr->components ASSIGNING <ls_component>.
      APPEND INITIAL LINE TO rt_fcat ASSIGNING <ls_fcat>.
      READ TABLE lt_fieldlist ASSIGNING <ls_flist>
        WITH KEY fieldname = <ls_component>-name.
      CHECK sy-subrc = 0.
      MOVE-CORRESPONDING <ls_flist> TO <ls_fcat>.

      <ls_fcat>-col_opt = abap_true.

      IF <ls_fcat>-fieldname = 'TRKORR'.
        <ls_fcat>-key = abap_true.
      ENDIF.

      CASE <ls_fcat>-fieldname .
        WHEN mcs_usr_cmd-cop.
          <ls_fcat>-icon = abap_true.
          <ls_fcat>-hotspot = abap_true.
        WHEN mcs_usr_cmd-cpr.
          <ls_fcat>-icon = abap_true.
          <ls_fcat>-hotspot = abap_true.
        WHEN mcs_usr_cmd-cri.
          <ls_fcat>-icon = abap_true.
          <ls_fcat>-hotspot = abap_true.
        WHEN 'ICONRC'.
          <ls_fcat>-icon = abap_true.
      ENDCASE.

      <ls_fcat>-ref_field = <ls_fcat>-fieldname.
      <ls_fcat>-ref_table = <ls_fcat>-tabname.
      "Clear for enable F1 using on field
      CLEAR <ls_fcat>-rollname.
      "Hide table type before set fcat
      IF <ls_fcat>-datatype = 'TTYP'.
        <ls_fcat>-tech = abap_true.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.                    "create_fcat
ENDCLASS.                    "gcl_alv IMPLEMENTATION
 
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_trkorr TYPE trkorr.

SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  CREATE OBJECT go_alv
    EXPORTING
      iv_trkorr = p_trkorr.

  go_alv->run( ).
  
  


MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'T0100'.
  go_alv->show_alv( ).
ENDMODULE.                 " STATUS_0100  OUTPUT

MODULE exit_command_0100 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT_COMMAND_0100  INPUT  