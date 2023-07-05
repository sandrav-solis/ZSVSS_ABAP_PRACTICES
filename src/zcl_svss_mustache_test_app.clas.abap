CLASS zcl_svss_mustache_test_app DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    TYPES:
      BEGIN OF ty_shop_item,
        name  TYPE string,
        price TYPE string,
      END OF ty_shop_item,

      ty_shop_item_tt TYPE STANDARD TABLE OF ty_shop_item WITH DEFAULT KEY,

      BEGIN OF ty_shop,
        shop_name TYPE string,
        items     TYPE ty_shop_item_tt,
      END OF ty_shop.

    CLASS-DATA c_nl LIKE cl_abap_char_utilities=>newline VALUE cl_abap_char_utilities=>newline READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_svss_mustache_test_app IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA lo_mustache TYPE REF TO zcl_mustache.
    DATA: lv_text    TYPE string,
          ls_my_data TYPE ty_shop.

    "Enter the stor name
    ls_my_data-shop_name = 'Sandra Solís Shop'.
    "Fill all the produc items
    ls_my_data-items[] = VALUE ty_shop_item_tt( ( name = 'ITEM 123' price = '678.99')
                                                ( name = 'ITEM 345' price = '2345.99')
                                                ( name = 'ITEM 789' price = '467.99') ).
    TRY.
        lo_mustache = zcl_mustache=>create(
          'Welcome to {{shop_name}}!' && c_nl &&
          '{{>items_template}}' ). " << CALLING A PARTIAL !

        DATA(lo_partial) = zcl_mustache=>create(
          '<table>' && c_nl &&
          ' {{#items}}' && c_nl &&
          ' <tr><td>{{name}}</td><td>${{price}}</td>' && c_nl &&
          ' {{/items}}' && c_nl &&
          '</table>' ).

        lo_mustache->add_partial( " Register the partial
          iv_name = 'items_template'
          io_obj = lo_partial ).

        lv_text = lo_mustache->render( ls_my_data ). " ls_my_data type ty_shop
        out->write( lv_text ).
      CATCH zcx_mustache_error INTO DATA(error).
        out->write( error->get_text(  ) ).
        out->write( error->msg ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

