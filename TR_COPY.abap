TYPE-POOLS: trwbo, rs, trsel, abap, icon.

CLASS: gcl_alv DEFINITION DEFERRED.

DATA: go_alv TYPE REF TO gcl_alv.




CLASS gcl_alv DEFINITION.


  PUBLIC SECTION.


    CONSTANTS:
      BEGIN OF mcs_usr_cmd,
        cop TYPE string VALUE 'COP',
        cpr TYPE string VALUE 'CPR',
      END OF   mcs_usr_cmd.

    DATA: mo_grid      TYPE REF TO cl_gui_alv_grid,
          mo_container TYPE REF TO cl_gui_custom_container.

    METHODS:
      constructor
        IMPORTING
          iv_trkorr TYPE trkorr,
      run,
      handle_user_command FOR EVENT user_command
                          OF cl_gui_alv_grid
                          IMPORTING e_ucomm.

  PRIVATE SECTION.

    DATA: mt_data     TYPE trwbo_request_headers,
          mv_trkorr   TYPE trkorr,
          mt_requests TYPE trwbo_request_headers.

    METHODS:
      copy_and_release
        IMPORTING
          it_rows TYPE lvc_t_roid,
      copy
        IMPORTING
          it_rows TYPE lvc_t_roid
        EXPORTING
          ev_new_trkorr TYPE trkorr,
      get_trkorrs,
      create_fcat
       IMPORTING
         is_component   TYPE any
       RETURNING value(rt_fcat) TYPE lvc_t_fcat,
       handle_toolbar FOR EVENT toolbar
                          OF cl_gui_alv_grid
                          IMPORTING e_object,
      show_alv.


ENDCLASS.                    "gcl_alv DEFINITION


*----------------------------------------------------------------------*
*       CLASS gcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_alv IMPLEMENTATION.

  METHOD constructor.
    mv_trkorr = iv_trkorr.
  ENDMETHOD.                    "constructor

  METHOD handle_toolbar.

    DATA: ls_toolbar TYPE stb_button.

    ls_toolbar-text = text-001.
    ls_toolbar-function = mcs_usr_cmd-cpr.
    ls_toolbar-icon = icon_transport.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-text = text-002.
    ls_toolbar-function = mcs_usr_cmd-cop.
    ls_toolbar-icon = icon_create_copy.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.

    DATA: lt_rowid   TYPE lvc_t_roid.

    CALL METHOD mo_grid->get_selected_rows
      IMPORTING
        et_row_no = lt_rowid.

    CHECK lt_rowid IS NOT INITIAL.

    CASE e_ucomm.
      WHEN mcs_usr_cmd-cpr.
        me->copy_and_release( lt_rowid ).
      WHEN mcs_usr_cmd-cop.
        me->copy( lt_rowid ).
    ENDCASE.
  ENDMETHOD.                    "handle_user_command

  METHOD copy_and_release.

    DATA: lv_trkorr TYPE trkorr.

    me->copy(
      EXPORTING
        it_rows = it_rows
      IMPORTING
        ev_new_trkorr = lv_trkorr
     ).

    CALL FUNCTION 'TRINT_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                   = lv_trkorr
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

  ENDMETHOD.                    "copy_and_release

  METHOD copy.

    DATA: lt_trkorr     TYPE trwbo_request_headers,
          ls_requests   TYPE trwbo_request,
          ls_new_trkorr TYPE trwbo_request_header.

    FIELD-SYMBOLS: <ls_rowid>   TYPE lvc_s_roid,
                   <ls_data>    TYPE trwbo_request_header,
                   <ls_request> TYPE trwbo_request_header.


    LOOP AT it_rows ASSIGNING <ls_rowid> .
      READ TABLE mt_data ASSIGNING <ls_data> INDEX <ls_rowid>-row_id.
      CHECK sy-subrc = 0.

      LOOP AT mt_requests ASSIGNING <ls_request> WHERE ( trkorr = <ls_data>-trkorr ) OR ( strkorr = <ls_data>-trkorr ).

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

        READ TABLE ls_requests-objects TRANSPORTING NO FIELDS
          WITH KEY pgmid = 'R3TR'.
        CHECK sy-subrc = 0.

        APPEND <ls_request> TO lt_trkorr.

      ENDLOOP.
    ENDLOOP.

    CHECK lt_trkorr IS NOT INITIAL.

    CALL FUNCTION 'ZABAP_TR_REQUEST_MODIFY'
      EXPORTING
        iv_action            = 'CREA'
        iv_new_task_type     = 'X'
        iv_new_request_type  = 'T'
        iv_new_tarsystem     = 'EQR'
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

    LOOP AT lt_trkorr ASSIGNING <ls_data>.

      CALL FUNCTION 'TR_COPY_COMM'
        EXPORTING
          wi_dialog                = space
          wi_trkorr_from           = <ls_data>-trkorr
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

    ENDLOOP.

  ENDMETHOD.                    "copy

  METHOD run.

    me->get_trkorrs( ).
    me->show_alv( ).

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
                   <ls_objects> TYPE trwbo_s_e071.

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
      APPEND <ls_request> TO mt_data.
    ENDLOOP.

    SORT mt_data BY trkorr DESCENDING.

  ENDMETHOD.                    "get_trkorrs

  METHOD show_alv.

    DATA: lt_fcat    TYPE lvc_t_fcat,
          ls_alv     TYPE zst_abap_alv_trcopy,
          ls_layout  TYPE lvc_s_layo,
          ls_variant TYPE disvariant.

    lt_fcat = create_fcat( ls_alv ).

    ls_variant-report = sy-repid.
    ls_variant-handle = 1.
    ls_layout-sel_mode = 'A'.
    ls_layout-smalltitle = abap_true.

    IF mo_grid IS NOT BOUND.

      CREATE OBJECT mo_grid
        EXPORTING
          i_parent = cl_gui_container=>default_screen.

      SET HANDLER me->handle_user_command FOR mo_grid.
      SET HANDLER me->handle_toolbar FOR mo_grid.

      mo_grid->set_table_for_first_display(
        EXPORTING
          i_save     = 'A'
          is_layout  = ls_layout
          is_variant = ls_variant
        CHANGING
          it_fieldcatalog = lt_fcat
          it_outtab       = mt_data ).

      WRITE space.

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

      IF <ls_fcat>-fieldname = 'TRKORR'.
        <ls_fcat>-key = abap_true.
      ENDIF.

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