CLASS zcl_svss_type_ref_to_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_svss_type_ref_to_data IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    DATA: bopf_monster_header_records TYPE STANDARD TABLE OF zdlc_travel_001,
          header_record_reference     TYPE REF TO data.

    SELECT * FROM zdlc_travel_001
    INTO TABLE @bopf_monster_header_records.

    LOOP AT bopf_monster_header_records INTO DATA(bopf_monster_header_record).
      "After 7.4
      header_record_reference = REF #( bopf_monster_header_record ).

      IF sy-subrc = 0.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
